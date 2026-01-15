/**************************************************************************************/
/*                                                                                    */
/*  Visualization Library                                                             */
/*  http://www.visualizationlibrary.org                                               */
/*                                                                                    */
/*  Copyright (c) 2005-2010, Michele Bosi                                             */
/*  All rights reserved.                                                              */
/*                                                                                    */
/*  Redistribution and use in source and binary forms, with or without modification,  */
/*  are permitted provided that the following conditions are met:                     */
/*                                                                                    */
/*  - Redistributions of source code must retain the above copyright notice, this     */
/*  list of conditions and the following disclaimer.                                  */
/*                                                                                    */
/*  - Redistributions in binary form must reproduce the above copyright notice, this  */
/*  list of conditions and the following disclaimer in the documentation and/or       */
/*  other materials provided with the distribution.                                   */
/*                                                                                    */
/*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND   */
/*  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED     */
/*  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE            */
/*  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR  */
/*  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES    */
/*  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;      */
/*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON    */
/*  ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT           */
/*  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS     */
/*  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                      */
/*                                                                                    */
/**************************************************************************************/

#include "ioJPG.hpp"
#include <vlCore/VisualizationLibrary.hpp>
#include <vlCore/FileSystem.hpp>
#include <vlCore/VirtualFile.hpp>
#include <vlCore/Image.hpp>
#include <vlCore/LoadWriterManager.hpp>

extern "C"
{
  #include <jpeglib.h>
  #include <jconfig.h>
  #include <jerror.h>
  #include <jmorecfg.h>
}
#include <setjmp.h>
#include <cstdio>

using namespace vl;

namespace
{
  /*
   * Some of the code below has been adapted to VL from the IJG jpeg library.
   */

  /* Expanded data source object for stdio input */

  typedef struct 
  {
    struct jpeg_source_mgr pub;
    VirtualFile * infile;  /* source stream */
    JOCTET * buffer;          /* start of buffer */
    boolean start_of_file;    /* have we gotten any data yet? */
  } my_source_mgr;

  typedef my_source_mgr * my_src_ptr;

  #define INPUT_BUF_SIZE  4096 /* choose an efficiently fread'able size */

  /*
   * Initialize source --- called by jpeg_read_header
   * before any data is actually read.
   */

  METHODDEF(void)
  init_source (j_decompress_ptr cinfo)
  {
    my_src_ptr src = (my_src_ptr) cinfo->src;

    /* We reset the empty-input-file flag for each image,
     * but we don't clear the input buffer.
     * This is correct behavior for reading a series of images from one source.
     */
    src->start_of_file = TRUE;
  }

  /*
   * Fill the input buffer --- called whenever buffer is emptied.
   *
   * In typical applications, this should read fresh data into the buffer
   * (ignoring the current state of next_input_byte & bytes_in_buffer),
   * reset the pointer & count to the start of the buffer, and return TRUE
   * indicating that the buffer has been reloaded.  It is not necessary to
   * fill the buffer entirely, only to obtain at least one more byte.
   *
   * There is no such thing as an EOF return.  If the end of the file has been
   * reached, the routine has a choice of ERREXIT() or inserting fake data into
   * the buffer.  In most cases, generating a warning message and inserting a
   * fake EOI marker is the best course of action --- this will allow the
   * decompressor to output however much of the image is there.  However,
   * the resulting error message is misleading if the real problem is an empty
   * input file, so we handle that case specially.
   *
   * In applications that need to be able to suspend compression due to input
   * not being available yet, a FALSE return indicates that no more data can be
   * obtained right now, but more may be forthcoming later.  In this situation,
   * the decompressor will return to its caller (with an indication of the
   * number of scanlines it has read, if any).  The application should resume
   * decompression after it has loaded more data into the input buffer.  Note
   * that there are substantial restrictions on the use of suspension --- see
   * the documentation.
   *
   * When suspending, the decompressor will back up to a convenient restart point
   * (typically the start of the current MCU). next_input_byte & bytes_in_buffer
   * indicate where the restart point will be if the current call returns FALSE.
   * Data beyond this point must be rescanned after resumption, so move it to
   * the front of the buffer rather than discarding it.
   */

  METHODDEF(boolean)
  fill_input_buffer (j_decompress_ptr cinfo)
  {
    my_src_ptr src = (my_src_ptr) cinfo->src;
    size_t nbytes;

    nbytes = (size_t)src->infile->read(src->buffer, INPUT_BUF_SIZE);

    if (nbytes <= 0) {
      if (src->start_of_file)  /* Treat empty input file as fatal error */
        ERREXIT(cinfo, JERR_INPUT_EMPTY);
      WARNMS(cinfo, JWRN_JPEG_EOF);
      /* Insert a fake EOI marker */
      src->buffer[0] = (JOCTET) 0xFF;
      src->buffer[1] = (JOCTET) JPEG_EOI;
      nbytes = 2;
    }

    src->pub.next_input_byte = src->buffer;
    src->pub.bytes_in_buffer = nbytes;
    src->start_of_file = FALSE;

    return TRUE;
  }

  /*
   * Skip data --- used to skip over a potentially large amount of
   * uninteresting data (such as an APPn marker).
   *
   * Writers of suspendable-input applications must note that skip_input_data
   * is not granted the right to give a suspension return.  If the skip extends
   * beyond the data currently in the buffer, the buffer can be marked empty so
   * that the next read will cause a fill_input_buffer call that can suspend.
   * Arranging for additional bytes to be discarded before reloading the input
   * buffer is the application writer's problem.
   */

  METHODDEF(void)
  skip_input_data (j_decompress_ptr cinfo, long num_bytes)
  {
    my_src_ptr src = (my_src_ptr) cinfo->src;

    /* Just a dumb implementation for now.  Could use fseek() except
     * it doesn't work on pipes.  Not clear that being smart is worth
     * any trouble anyway --- large skips are infrequent.
     */
    if (num_bytes > 0) {
      while (num_bytes > (long) src->pub.bytes_in_buffer) {
        num_bytes -= (long) src->pub.bytes_in_buffer;
        fill_input_buffer(cinfo);
        /* note we assume that fill_input_buffer will never return FALSE,
         * so suspension need not be handled.
         */
      }
      src->pub.next_input_byte += (size_t) num_bytes;
      src->pub.bytes_in_buffer -= (size_t) num_bytes;
    }
  }

  /*
   * An additional method that can be provided by data source modules is the
   * resync_to_restart method for error recovery in the presence of RST markers.
   * For the moment, this source module just uses the default resync method
   * provided by the JPEG library.  That method assumes that no backtracking
   * is possible.
   */

  /*
   * Terminate source --- called by jpeg_finish_decompress
   * after all data has been read.  Often a no-op.
   *
   * NB: *not* called by jpeg_abort or jpeg_destroy; surrounding
   * application must deal with any cleanup that should happen even
   * for error exit.
   */

  METHODDEF(void)
  term_source (j_decompress_ptr /*cinfo*/)
  {
    /* no work necessary here */
  }

  /*
   * Prepare for input from a stdio stream.
   * The caller must have already opened the stream, and is responsible
   * for closing it after finishing decompression.
   */

  GLOBAL(void)
  jpeg_vl_src (j_decompress_ptr cinfo, VirtualFile* infile)
  {
    my_src_ptr src;

    /* The source object and input buffer are made permanent so that a series
     * of JPEG images can be read from the same file by calling jpeg_stdio_src
     * only before the first one.  (If we discarded the buffer at the end of
     * one image, we'd likely lose the start of the next one.)
     * This makes it unsafe to use this manager and a different source
     * manager serially with the same JPEG object.  Caveat programmer.
     */
    if (cinfo->src == NULL) {  /* first time for this JPEG object? */
      cinfo->src = (struct jpeg_source_mgr *)
        (*cinfo->mem->alloc_small) ((j_common_ptr) cinfo, JPOOL_PERMANENT, sizeof(my_source_mgr));
      src = (my_src_ptr) cinfo->src;
      src->buffer = (JOCTET *)
        (*cinfo->mem->alloc_small) ((j_common_ptr) cinfo, JPOOL_PERMANENT,
            INPUT_BUF_SIZE * sizeof(JOCTET));
    }

    src = (my_src_ptr) cinfo->src;
    src->pub.init_source = init_source;
    src->pub.fill_input_buffer = fill_input_buffer;
    src->pub.skip_input_data = skip_input_data;
    src->pub.resync_to_restart = jpeg_resync_to_restart; /* use default method */
    src->pub.term_source = term_source;
    src->infile = infile;
    src->pub.bytes_in_buffer = 0;    /* forces fill_input_buffer on first read */
    src->pub.next_input_byte = NULL; /* until buffer loaded */
  }
  //-----------------------------------------------------------------------------
  struct my_error_mgr {
    struct jpeg_error_mgr pub; /* "public" fields */
    jmp_buf setjmp_buffer;     /* for return to caller */
  };

  typedef struct my_error_mgr * my_error_ptr;

  /*
   * Here's the routine that will replace the standard error_exit method:
   */

  METHODDEF(void)
  my_error_exit (j_common_ptr cinfo)
  {
    /* cinfo->err really points to a my_error_mgr struct, so coerce pointer */
    my_error_ptr myerr = (my_error_ptr) cinfo->err;

    /* Always display the message. */
    /* We could postpone this until after returning, if we chose. */
    (*cinfo->err->output_message) (cinfo);

    /* Return control to the setjmp point */
    longjmp(myerr->setjmp_buffer, 1);
  }

//-----------------------------------------------------------------------------

  /* Expanded data destination object for stdio output */

  typedef struct {
    struct jpeg_destination_mgr pub; /* public fields */

    VirtualFile * outfile; /* target stream */
    JOCTET * buffer;       /* start of buffer */
  } my_destination_mgr;

  typedef my_destination_mgr * my_dest_ptr;

  #define OUTPUT_BUF_SIZE  4096  /* choose an efficiently fwrite'able size */

  /*
   * Initialize destination --- called by jpeg_start_compress
   * before any data is actually written.
   */

  METHODDEF(void)
  init_destination (j_compress_ptr cinfo)
  {
    my_dest_ptr dest = (my_dest_ptr) cinfo->dest;

    /* Allocate the output buffer --- it will be released when done with image */
    dest->buffer = (JOCTET *)
        (*cinfo->mem->alloc_small) ((j_common_ptr) cinfo, JPOOL_IMAGE,
            OUTPUT_BUF_SIZE * sizeof(JOCTET));

    dest->pub.next_output_byte = dest->buffer;
    dest->pub.free_in_buffer = OUTPUT_BUF_SIZE;
  }

  /*
   * Empty the output buffer --- called whenever buffer fills up.
   *
   * In typical applications, this should write the entire output buffer
   * (ignoring the current state of next_output_byte & free_in_buffer),
   * reset the pointer & count to the start of the buffer, and return TRUE
   * indicating that the buffer has been dumped.
   *
   * In applications that need to be able to suspend compression due to output
   * overrun, a FALSE return indicates that the buffer cannot be emptied now.
   * In this situation, the compressor will return to its caller (possibly with
   * an indication that it has not accepted all the supplied scanlines).  The
   * application should resume compression after it has made more room in the
   * output buffer.  Note that there are substantial restrictions on the use of
   * suspension --- see the documentation.
   *
   * When suspending, the compressor will back up to a convenient restart point
   * (typically the start of the current MCU). next_output_byte & free_in_buffer
   * indicate where the restart point will be if the current call returns FALSE.
   * Data beyond this point will be regenerated after resumption, so do not
   * write it out when emptying the buffer externally.
   */

  METHODDEF(boolean)
  empty_output_buffer (j_compress_ptr cinfo)
  {
    my_dest_ptr dest = (my_dest_ptr) cinfo->dest;

    // if (JFWRITE(dest->outfile, dest->buffer, OUTPUT_BUF_SIZE) != (size_t) OUTPUT_BUF_SIZE)
    if (dest->outfile->write(dest->buffer, OUTPUT_BUF_SIZE) != (size_t) OUTPUT_BUF_SIZE)
      ERREXIT(cinfo, JERR_FILE_WRITE);

    dest->pub.next_output_byte = dest->buffer;
    dest->pub.free_in_buffer = OUTPUT_BUF_SIZE;

    return TRUE;
  }

  /*
   * Terminate destination --- called by jpeg_finish_compress
   * after all data has been written.  Usually needs to flush buffer.
   *
   * NB: *not* called by jpeg_abort or jpeg_destroy; surrounding
   * application must deal with any cleanup that should happen even
   * for error exit.
   */

  METHODDEF(void)
  term_destination (j_compress_ptr cinfo)
  {
    my_dest_ptr dest = (my_dest_ptr)cinfo->dest;
    size_t datacount = OUTPUT_BUF_SIZE - dest->pub.free_in_buffer;

    /* Write any data remaining in the buffer */
    if (datacount > 0) {
      if (dest->outfile->write(dest->buffer, datacount) != (long long)datacount)
        ERREXIT(cinfo, JERR_FILE_WRITE);
    }
    // fixme?
    //fflush(dest->outfile);
    ///* Make sure we wrote the output file OK */
    //if (ferror(dest->outfile))
    //  ERREXIT(cinfo, JERR_FILE_WRITE);
  }

  /*
   * Prepare for output to a stdio stream.
   * The caller must have already opened the stream, and is responsible
   * for closing it after finishing compression.
   */

  GLOBAL(void)
  jpeg_vl_dest (j_compress_ptr cinfo, VirtualFile * outfile)
  {
    my_dest_ptr dest;

    /* The destination object is made permanent so that multiple JPEG images
     * can be written to the same file without re-executing jpeg_stdio_dest.
     * This makes it dangerous to use this manager and a different destination
     * manager serially with the same JPEG object, because their private object
     * sizes may be different.  Caveat programmer.
     */
    if (cinfo->dest == NULL) {  /* first time for this JPEG object? */
      cinfo->dest = (struct jpeg_destination_mgr *)
        (*cinfo->mem->alloc_small) ((j_common_ptr) cinfo, JPOOL_PERMANENT,
            sizeof(my_destination_mgr));
    }

    dest = (my_dest_ptr) cinfo->dest;
    dest->pub.init_destination = init_destination;
    dest->pub.empty_output_buffer = empty_output_buffer;
    dest->pub.term_destination = term_destination;
    dest->outfile = outfile;
  }
}
//-----------------------------------------------------------------------------
ref<Image> vl::loadJPG(VirtualFile* file)
{
  if ( !file->open(OM_ReadOnly) )
  {
    Log::error( Say("loadJPG: cannot open file '%s'\n") << file->path() );
    return NULL;
  }

  ref<Image> img = new Image;
  img->setObjectName(file->path().toStdString().c_str());

  struct jpeg_decompress_struct cinfo;

  struct my_error_mgr jerr;

  JSAMPARRAY buffer; /* Output row buffer */
  int row_stride;     /* physical row width in output buffer */

  /* Step 1: allocate and initialize JPEG decompression object */

  /* We set up the normal JPEG error routines, then override error_exit. */
  cinfo.err = jpeg_std_error(&jerr.pub);
  jerr.pub.error_exit = my_error_exit;

  // fixme? "warning C4611: interaction between '_setjmp' and C++ object destruction is non-portable"

#if 0
  /* Establish the setjmp return context for my_error_exit to use. */
  if (setjmp(jerr.setjmp_buffer)) {
    /* If we get here, the JPEG code has signaled an error.
     * We need to clean up the JPEG object, close the input file, and return. */
    jpeg_destroy_decompress(&cinfo);
    file->close();
    return 0;
  }
#endif

  /* Now we can initialize the JPEG decompression object. */
  jpeg_create_decompress(&cinfo);

  /* Step 2: specify data source (eg, a file) */

  jpeg_vl_src(&cinfo, file);

  /* Step 3: read file parameters with jpeg_read_header() */

  jpeg_read_header(&cinfo, TRUE);

  if (cinfo.jpeg_color_space == JCS_GRAYSCALE)
  {
    VL_CHECK(cinfo.out_color_space == JCS_GRAYSCALE)
    img->allocate2D(cinfo.image_width, cinfo.image_height, 1, IF_LUMINANCE, IT_UNSIGNED_BYTE);
  }
  else // JCS_RGB
  {
    VL_CHECK(cinfo.out_color_space == JCS_RGB)
    img->allocate2D(cinfo.image_width, cinfo.image_height, 1, IF_RGB, IT_UNSIGNED_BYTE);
  }

  /* Step 4: set parameters for decompression */

  // Use defaults set by jpeg_read_header()

  /* Step 5: Start decompressor */

  jpeg_start_decompress(&cinfo);

  /* JSAMPLEs per row in output buffer */
  row_stride = cinfo.output_width * cinfo.output_components;
  /* Make a one-row-high sample array that will go away when done with image */
  buffer = (*cinfo.mem->alloc_sarray) ((j_common_ptr)&cinfo, JPOOL_IMAGE, row_stride, 1);

  /* Step 6: read scanlines */

  /* Here we use the library's state variable cinfo.output_scanline as the
   * loop counter, so that we don't have to keep track ourselves.
   */
  while (cinfo.output_scanline < cinfo.output_height)
  {
    /* jpeg_read_scanlines expects an array of pointers to scanlines.
     * Here the array is only one element long, but you could ask for
     * more than one scanline at a time if that's more convenient.
     */
    jpeg_read_scanlines(&cinfo, buffer, 1);

    memcpy(img->pixels() + img->pitch()*(img->height() - cinfo.output_scanline), buffer[0], row_stride);
  }

  /* Step 7: Finish decompression */

  jpeg_finish_decompress(&cinfo);

  /* Step 8: Release JPEG decompression object */

  /* This is an important step since it will release a good deal of memory. */
  jpeg_destroy_decompress(&cinfo);

  /* After finish_decompress, we can close the input file.
   * Here we postpone it until after no more JPEG errors are possible,
   * so as to simplify the setjmp error logic above.  (Actually, I don't
   * think that jpeg_destroy can do an error exit, but why assume anything...)
   */
  file->close();

  return img;
}
//-----------------------------------------------------------------------------
ref<Image> vl::loadJPG(const String& path)
{
  ref<VirtualFile> file = defFileSystem()->locateFile(path);
  if ( !file )
  {
    Log::error( Say("File '%s' not found.\n") << path );
    return NULL;
  }
  else
    return loadJPG(file.get());
}
//-----------------------------------------------------------------------------
bool vl::isJPG(VirtualFile* file)
{
  file->open(OM_ReadOnly);
  unsigned char sig1[] = { 0xFF, 0xD8, 0xFF, 0xE0 };
  unsigned char sig2[] = { 0xFF, 0xD8, 0xFF, 0xE1 };
  unsigned char signature[4];
  file->read(signature,4);
  file->close();
  bool sig_ok = memcmp(sig1,signature,4) == 0 || memcmp(sig2,signature,4) == 0;
  return sig_ok || (file->path().toLowerCase().endsWith(".jpg") || file->path().toLowerCase().endsWith(".jpeg"));
}
//-----------------------------------------------------------------------------
bool vl::saveJPG(const Image* src, const String& path, int quality)
{
  ref<DiskFile> file = new DiskFile(path);
  return saveJPG(src, file.get(), quality);
}
//-----------------------------------------------------------------------------
bool vl::saveJPG(const Image* src, VirtualFile* fout, int quality)
{
  //if (src->dimension() != ID_2D )
  //{
  //  Log::error( Say("saveJPG('%s'): can save only 2D images.\n") << fout->path() );
  //  return false;
  //}

  int w = src->width();
  int h = src->height();
  int d = src->depth();
  if (h == 0) h=1;
  if (d == 0) d=1;
  if (src->isCubemap()) d=6;
  h = h*d;

  // convert src to IT_UNSIGNED_BYTE / IF_RGB
  ref<Image> cimg;
  if (src->type() != IT_UNSIGNED_BYTE)
  {
    cimg = src->convertType(IT_UNSIGNED_BYTE);
    src = cimg.get();
    if (!cimg)
    {
      Log::error( Say("saveJPG('%s'): could not convert image to IT_UNSIGNED_BYTE.\n") << fout->path() );
      return false;
    }
  }
  if (src->format() != IF_RGB)
  {
    cimg = src->convertFormat(IF_RGB);
    src = cimg.get();
    if (!cimg)
    {
      Log::error( Say("saveJPG('%s'): could not convert image to IF_RGB.\n") << fout->path() );
      return false;
    }
  }

  if(!fout->open(OM_WriteOnly))
  {
    Log::error( Say("JPG: could not write to '%s'.\n") << fout->path() );
    return false;
  }

  /* This struct contains the JPEG compression parameters and pointers to
   * working space (which is allocated as needed by the JPEG library).
   * It is possible to have several such structures, representing multiple
   * compression/decompression processes, in existence at once.  We refer
   * to any one struct (and its associated working data) as a "JPEG object".
   */
  struct jpeg_compress_struct cinfo;

  /* This struct represents a JPEG error handler.  It is declared separately
   * because applications often want to supply a specialized error handler
   * (see the second half of this file for an example).  But here we just
   * take the easy way out and use the standard error handler, which will
   * print a message on stderr and call exit() if compression fails.
   * Note that this struct must live as long as the main JPEG parameter
   * struct, to avoid dangling-pointer problems.
   */
  struct jpeg_error_mgr jerr;

  JSAMPROW row_pointer[1]; /* pointer to JSAMPLE row[s] */
  int row_stride;          /* physical row width in image buffer */

  /* Step 1: allocate and initialize JPEG compression object */

  /* We have to set up the error handler first, in case the initialization
   * step fails.  (Unlikely, but it could happen if you are out of memory.)
   * This routine fills in the contents of struct jerr, and returns jerr's
   * address which we place into the link field in cinfo.
   */
  cinfo.err = jpeg_std_error(&jerr);

  /* Now we can initialize the JPEG compression object. */
  jpeg_create_compress(&cinfo);

  /* Step 2: specify data destination (eg, a file) */
  /* Note: steps 2 and 3 can be done in either order. */

  ///* Here we use the library-supplied code to send compressed data to a
  // * stdio stream.  You can also write your own code to do something else.
  // * VERY IMPORTANT: use "b" option to fopen() if you are on a machine that
  // * requires it in order to write binary files.
  // */
  //if ((outfile = fopen(filename, "wb")) == NULL) {
  //  fprintf(stderr, "can't open %s\n", filename);
  //  exit(1);
  //}
  //jpeg_stdio_dest(&cinfo, outfile);
  jpeg_vl_dest(&cinfo, fout);

  /* Step 3: set parameters for compression */

  /* First we supply a description of the input image.
   * Four fields of the cinfo struct must be filled in:
   */
  cinfo.image_width      = w;   /* image width and height, in pixels */
  cinfo.image_height     = h;
  cinfo.input_components = 3;    /* # of color components per pixel */
  cinfo.in_color_space = JCS_RGB;   /* colorspace of input image */

  /* Now use the library's routine to set default compression parameters.
   * (You must set at least cinfo.in_color_space before calling this,
   * since the defaults depend on the source color space.)
   */
  jpeg_set_defaults(&cinfo);

  /* Now you can set any non-default parameters you wish to.
   * Here we just illustrate the use of quality (quantization table) scaling: */
  jpeg_set_quality(&cinfo, quality, TRUE /* limit to baseline-JPEG values */);

  /* Step 4: Start compressor */

  /* TRUE ensures that we will write a complete interchange-JPEG file.
   * Pass TRUE unless you are very sure of what you're doing.
   */
  jpeg_start_compress(&cinfo, TRUE);

  /* Step 5: while (scan lines remain to be written) */
  /*           jpeg_write_scanlines(...); */

  /* Here we use the library's state variable cinfo.next_scanline as the
   * loop counter, so that we don't have to keep track ourselves.
   * To keep things simple, we pass one scanline per call; you can pass
   * more if you wish, though.
   */
  row_stride = w * 3;  /* JSAMPLEs per row in image_buffer */

  while (cinfo.next_scanline < cinfo.image_height) 
  {
    /* jpeg_write_scanlines expects an array of pointers to scanlines.
     * Here the array is only one element long, but you could pass
     * more than one scanline at a time if that's more convenient.
     */
    row_pointer[0] = (JSAMPROW)(src->pixels() + (h - 1 - cinfo.next_scanline) * row_stride);
    jpeg_write_scanlines(&cinfo, row_pointer, 1);
  }

  /* Step 6: Finish compression */

  jpeg_finish_compress(&cinfo);

  /* Step 7: release JPEG compression object */

  /* This is an important step since it will release a good deal of memory. */
  jpeg_destroy_compress(&cinfo);

  /* And we're done! */

  fout->close();
  return true;
}
//-----------------------------------------------------------------------------
