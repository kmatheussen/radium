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

#include "ioPNG.hpp"
#include <vlCore/LoadWriterManager.hpp>
#include <vlCore/VisualizationLibrary.hpp>
#include <vlCore/FileSystem.hpp>
#include <vlCore/VirtualFile.hpp>
#include <vlCore/Image.hpp>
#include <png.h>

using namespace vl;

namespace
{
  void png_read_vfile(png_structp png_ptr, png_bytep data, png_size_t byte_count)
  {
    VirtualFile* vfile = (VirtualFile*)png_get_io_ptr(png_ptr);
    vfile->read(data, byte_count);
  }
  void png_write_vfile(png_structp png_ptr, png_bytep data, png_size_t byte_count)
  {
    VirtualFile* vfile = (VirtualFile*)png_get_io_ptr(png_ptr);
    vfile->write(data,byte_count);
  }
  void png_flush_vfile(png_structp /*png_ptr*/)
  {
    // do nothing
  }
  void vl_error_fn(png_structp /*png_ptr*/, png_const_charp error_msg)
  {
    Log::error( Say("libPNG: %s\n") << error_msg);
  }
  void vl_warning_fn(png_structp /*png_ptr*/, png_const_charp warning_msg)
  {
    Log::warning( Say("libPNG: %s\n") << warning_msg);
  }
}

//-----------------------------------------------------------------------------
ref<Image> vl::loadPNG(const String& path)
{
  ref<VirtualFile> file = defFileSystem()->locateFile(path);
  if ( !file )
  {
    Log::error( Say("File '%s' not found.\n") << path );
    return NULL;
  }
  else
    return loadPNG(file.get());
}
//-----------------------------------------------------------------------------
ref<Image> vl::loadPNG(VirtualFile* file)
{
  if ( !file->open(OM_ReadOnly) )
  {
    Log::error( Say("loadPNG: cannot load PNG file '%s'\n") << file->path() );
    return NULL;
  }

  png_structp png_ptr;
  png_infop info_ptr;
  png_infop endinfo;
  // unsigned int sig_read = 0;
  png_uint_32 width, height;
  int bit_depth, color_type;

  /* Create and initialize the png_struct with the desired error handler
  * functions.  If you want to use the default stderr and longjump method,
  * you can supply NULL for the last three parameters.  We also supply the
  * the compiler header file version, so that we know if the application
  * was compiled with a compatible version of the library.  REQUIRED
  */
  png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  png_set_error_fn(png_ptr, png_get_error_ptr(png_ptr), vl_error_fn, vl_warning_fn);

  if (png_ptr == NULL)
  {
    file->close();
    return NULL;
  }

  /* Allocate/initialize the memory for image information.  REQUIRED. */
  info_ptr = png_create_info_struct(png_ptr);
  endinfo  = png_create_info_struct(png_ptr);
  VL_CHECK(info_ptr)
  VL_CHECK(endinfo)

  // fixme? "warning C4611: interaction between '_setjmp' and C++ object destruction is non-portable"

#if 0
  if (setjmp(png_jmpbuf(png_ptr)))
  {
    /* Free all of the memory associated with the png_ptr and info_ptr */
    png_destroy_read_struct(&png_ptr, &info_ptr, &endinfo);
    file->close();
    /* If we get here, we had a problem reading the file */
    return NULL;
  }
#endif

  unsigned char header[8];
  int count = (int)file->read(header,8);
  if (count == 8 && png_check_sig(header, 8))
  {
    png_set_read_fn(png_ptr,file,png_read_vfile);
    png_set_sig_bytes(png_ptr, 8);
  }
  else
  {
    png_destroy_read_struct(&png_ptr, &info_ptr, &endinfo);
    file->close();
    return NULL;
  }

  /* The call to png_read_info() gives us all of the information from the
  * PNG file before the first IDAT (image data chunk).  REQUIRED
  */
  png_read_info(png_ptr, info_ptr);

  png_get_IHDR(png_ptr, info_ptr, &width, &height, &bit_depth, &color_type, NULL/*&interlace_type*/, NULL/*int_p_NULL*/, NULL/*int_p_NULL*/);

  ref<Image> img = new Image;
  img->setObjectName(file->path().toStdString().c_str());

  if (bit_depth == 16)
  {
    switch(color_type)
    {
      case PNG_COLOR_TYPE_GRAY:       img->allocate2D(width, height, 1, IF_LUMINANCE, IT_UNSIGNED_SHORT); break;
      case PNG_COLOR_TYPE_GRAY_ALPHA: img->allocate2D(width, height, 1, IF_LUMINANCE_ALPHA, IT_UNSIGNED_SHORT); break;
      case PNG_COLOR_TYPE_PALETTE:    img->allocate2D(width, height, 1, IF_RGB, IT_UNSIGNED_SHORT); break;
      case PNG_COLOR_TYPE_RGB:        img->allocate2D(width, height, 1, IF_RGB, IT_UNSIGNED_SHORT); break;
      case PNG_COLOR_TYPE_RGB_ALPHA:  img->allocate2D(width, height, 1, IF_RGBA, IT_UNSIGNED_SHORT); break;
      default:
        VL_TRAP()
        break;
    }
  }
  else
  {
    switch(color_type)
    {
      case PNG_COLOR_TYPE_GRAY:       img->allocate2D(width, height, 1, IF_LUMINANCE, IT_UNSIGNED_BYTE); break;
      case PNG_COLOR_TYPE_GRAY_ALPHA: img->allocate2D(width, height, 1, IF_LUMINANCE_ALPHA, IT_UNSIGNED_BYTE); break;
      case PNG_COLOR_TYPE_PALETTE:    img->allocate2D(width, height, 1, IF_RGBA, IT_UNSIGNED_BYTE); break;
      case PNG_COLOR_TYPE_RGB:        img->allocate2D(width, height, 1, IF_RGB, IT_UNSIGNED_BYTE); break;
      case PNG_COLOR_TYPE_RGB_ALPHA:  img->allocate2D(width, height, 1, IF_RGBA, IT_UNSIGNED_BYTE); break;
      default:
        VL_TRAP()
        break;
    }
  }

  /* Set up the data transformations you want.  Note that these are all
   * optional.  Only call them if you want/need them.  Many of the
   * transformations only work on specific types of images, and many
   * are mutually exclusive.
   */

   /* tell libpng to strip 16 bit/color files down to 8 bits/color */
   // png_set_strip_16(png_ptr);

   /* Strip alpha bytes from the input data without combining with the
    * background (not recommended).
    */
   /// png_set_strip_alpha(png_ptr);

   /* Extract multiple pixels with bit depths of 1, 2, and 4 from a single
    * byte into separate bytes (useful for paletted and grayscale images).
    */
   png_set_packing(png_ptr);

   /* Change the order of packed pixels to least significant bit first
    * (not useful if you are using png_set_packing). */
   // png_set_packswap(png_ptr);

   /* Expand paletted colors into true RGB triplets */
   if (color_type == PNG_COLOR_TYPE_PALETTE)
      png_set_palette_to_rgb(png_ptr);

  /* Expand grayscale images to the full 8 bits from 1, 2, or 4 bits/pixel */
  if (color_type == PNG_COLOR_TYPE_GRAY && bit_depth < 8)
    png_set_expand_gray_1_2_4_to_8(png_ptr);

  #if 1
    /* Expand paletted or RGB images with transparency to full alpha channels
    * so the data will be available as RGBA quartets.
    */
    if (png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS))
      png_set_tRNS_to_alpha(png_ptr);
  #endif

  #if 0
   /* Set the background color to draw transparent and alpha images over.
    * It is possible to set the red, green, and blue components directly
    * for paletted images instead of supplying a palette index.  Note that
    * even if the PNG file supplies a background, you are not required to
    * use it - you should use the (solid) application background if it has one.
    */
   png_color_16 my_background, *image_background;
   if (png_get_bKGD(png_ptr, info_ptr, &image_background))
      png_set_background(png_ptr, image_background, PNG_BACKGROUND_GAMMA_FILE, 1, 1.0);
   else
      png_set_background(png_ptr, &my_background, PNG_BACKGROUND_GAMMA_SCREEN, 0, 1.0);
  #endif

   // char* gamma_str = NULL;
   double screen_gamma = 2.2; /* A good guess for a PC monitors in a dimly lit room */
   // double screen_gamma = 1.7 or 1.0;  /* A good guess for Mac systems */

   /*if ((gamma_str = getenv("SCREEN_GAMMA")) != NULL)
      screen_gamma = atof(gamma_str);*/

   /* Tell libpng to handle the gamma conversion for you.  The final call
    * is a good guess for PC generated images, but it should be configurable
    * by the user at run time by the user.  It is strongly suggested that
    * your application support gamma correction.
    */

    double image_gamma;
    if (png_get_gAMA(png_ptr, info_ptr, &image_gamma))
       png_set_gamma(png_ptr, screen_gamma, image_gamma);
    else
       png_set_gamma(png_ptr, screen_gamma, 1.0/screen_gamma/*0.45455*/);

#if 0
   /* Dither RGB files down to 8 bit palette or reduce palettes
    * to the number of colors available on your screen.
    */
   if (color_type & PNG_COLOR_MASK_COLOR)
   {
      int num_palette;
      png_colorp palette;

      * This reduces the image to the application supplied palette */
      if (/* we have our own palette */)
      {
         /* An array of colors to which the image should be dithered */
         png_color std_color_cube[MAX_SCREEN_COLORS];

         png_set_dither(png_ptr, std_color_cube, MAX_SCREEN_COLORS,
            MAX_SCREEN_COLORS, png_uint_16p_NULL, 0);
      }
      * This reduces the image to the palette supplied in the file */
      else 
      if (png_get_PLTE(png_ptr, info_ptr, &palette, &num_palette))
      {
         png_uint_16p histogram = NULL;

         png_get_hIST(png_ptr, info_ptr, &histogram);

         png_set_dither(png_ptr, palette, num_palette,
                        max_screen_colors, histogram, 0);
      }
   }
#endif

#if 0
   /* invert monochrome files to have 0 as white and 1 as black */
   png_set_invert_mono(png_ptr);
#endif

#if 0
   /* If you want to shift the pixel values from the range [0,255] or
    * [0,65535] to the original [0,7] or [0,31], or whatever range the
    * colors were originally in:
    */
   if (png_get_valid(png_ptr, info_ptr, PNG_INFO_sBIT))
   {
      png_color_8p sig_bit;

      png_get_sBIT(png_ptr, info_ptr, &sig_bit);
      png_set_shift(png_ptr, sig_bit);
   }
#endif

#if 0
   /* flip the RGB pixels to BGR (or RGBA to BGRA) */
   if (color_type & PNG_COLOR_MASK_COLOR)
      png_set_bgr(png_ptr);
#endif

#if 0
   /* swap the RGBA or GA data to ARGB or AG (or BGRA to ABGR) */
   png_set_swap_alpha(png_ptr);
#endif

#if 1
   /* swap bytes of 16 bit files to least significant byte first */
    unsigned short bet = 0x00FF;
    bool little_endian_cpu = ((unsigned char*)&bet)[0] == 0xFF;
    if (little_endian_cpu && bit_depth > 8)
      png_set_swap(png_ptr);
#endif

#if 0
   /* Add filler (or alpha) byte (before/after each RGB triplet) */
   png_set_filler(png_ptr, 0xff, PNG_FILLER_AFTER);
#endif

   ///* Turn on interlace handling.  REQUIRED if you are not using
   // * png_read_image().  To see how to handle interlacing passes,
   // * see the png_read_row() method below:
   // */
   //int number_passes = png_set_interlace_handling(png_ptr);

   ///* Optional call to gamma correct and add the background to the palette
   // * and update info structure.  REQUIRED if you are expecting libpng to
   // * update the palette for you (ie you selected such a transform above).
   // */
   //png_read_update_info(png_ptr, info_ptr);

   ///* Allocate the memory to hold the image using the fields of info_ptr. */

   ///* The easiest way to read the image: */
   //// png_bytep row_pointers[height];
   //std::vector<png_bytep> row_pointers;
   //row_pointers.resize(height);

   //for (unsigned row = 0; row < height; row++)
   //{
   //   row_pointers[row] = (png_bytep)png_malloc(png_ptr, png_get_rowbytes(png_ptr, info_ptr));
   //}

   //for (int pass = 0; pass < number_passes; pass++)
   //{
   //   for (unsigned y = 0; y < height; y++)
   //   {
   //      png_read_rows(png_ptr, &row_pointers[y], png_bytepp_NULL, 1);
   //   }
   //}
   /* At this point you have read the entire image */
  
   // initialize row pointers
   std::vector<png_bytep> row_p;
   row_p.resize(height);
   for(unsigned i=0; i<height; ++i)
     row_p[height - 1 - i] = (png_bytep)img->pixels()+img->pitch()*i;

   png_read_image(png_ptr, &row_p[0]);
   png_read_end(png_ptr, endinfo);

   /* clean up after the read, and free any memory allocated - REQUIRED */
   png_destroy_read_struct(&png_ptr, &info_ptr, &endinfo);

   file->close();
   return img;
}
//-----------------------------------------------------------------------------
bool vl::isPNG(VirtualFile* file)
{
  if ( !file->open(OM_ReadOnly) )
  {
    // Log::error( Say("loadPNG: cannot load PNG file '%s'\n") << file->path() );
    return false;
  }

  unsigned char header[8];
  int count = (int)file->read(header,8);
  file->close();
  if (count == 8 && png_check_sig(header, 8))
    return true;
  else
    return false;
}
//-----------------------------------------------------------------------------
bool vl::savePNG(const Image* src, const String& path, int compression)
{
  ref<DiskFile> file = new DiskFile(path);
  return savePNG(src, file.get(), compression);
}
//-----------------------------------------------------------------------------
bool vl::savePNG(const Image* src, VirtualFile* fout, int compression)
{
  /*if ( src->dimension() != ID_2D && src->dimension() != ID_1D && src->dimension() != ID_3D )
  {
    Log::error( Say("savePNG('%s'): can save only 1D, 2D and 3D images.\n") << fout->path() );
    return false;
  }*/

  int w = src->width();
  int h = src->height();
  int d = src->depth();
  if (h == 0) h=1;
  if (d == 0) d=1;
  if (src->isCubemap()) d=6;
  h = h*d;

  ref<Image> cimg;
  // convert to IT_UNSIGNED_SHORT if necessary
  if (src->type() == IT_FLOAT || src->type() == IT_SHORT)
  {
    cimg = src->convertType(IT_UNSIGNED_SHORT);
    src = cimg.get();
    if (!cimg)
    {
      Log::error( Say("savePNG('%s'): could not convert image to IT_UNSIGNED_SHORT.\n") << fout->path() );
      return false;
    }
  }

  bool format_ok = src->format() == IF_RGB || src->format() == IF_RGBA || src->format() == IF_LUMINANCE || src->format() == IF_ALPHA || src->format() == IF_LUMINANCE_ALPHA;
  if (!format_ok)
  {
    cimg = src->convertFormat(IF_RGBA);
    src = cimg.get();
    if (!cimg)
    {
      Log::error( Say("savePNG('%s'): could not convert image to IF_RGBA.\n") << fout->path() );
      return false;
    }
  }

  VL_CHECK(src->type() == IT_UNSIGNED_BYTE || src->type() == IT_UNSIGNED_SHORT)
  VL_CHECK(src->format() == IF_RGB || src->format() == IF_RGBA || src->format() == IF_LUMINANCE || src->format() == IF_ALPHA || src->format() == IF_LUMINANCE_ALPHA)

  if(!fout->open(OM_WriteOnly))
  {
    Log::error( Say("PNG: could not write to '%s'.\n") << fout->path() );
    return false;
  }

  png_structp png  = NULL;
  png_infop   info = NULL;

  png = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  if(!png) 
    return false;

  info = png_create_info_struct(png);
  if(!info) 
    return false;

  png_set_write_fn(png,fout,png_write_vfile,png_flush_vfile);

  png_set_compression_level(png, compression);

  std::vector< png_bytep > rows;
  rows.resize(h);
  for(int i=0, y = h-1; i<h; ++i,--y)
    rows[i] = (png_bytep)(src->pixels()+src->pitch()*y);

  int color;
  switch(src->format()) 
  {
      case IF_RGB:             color = PNG_COLOR_TYPE_RGB; break;
      case IF_RGBA:            color = PNG_COLOR_TYPE_RGB_ALPHA; break;
      case IF_ALPHA:           
      case IF_LUMINANCE:       color = PNG_COLOR_TYPE_GRAY; break;
      case IF_LUMINANCE_ALPHA: color = PNG_COLOR_TYPE_GRAY_ALPHA; break;
      default: 
        return false;
  }

  int bit_depth = src->type() == IT_UNSIGNED_SHORT ? 16 : 8;

  png_set_IHDR( png, info, w, h, bit_depth, color, 
                PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);

  unsigned short bet = 0x00FF;
  bool little_endian_cpu = ((unsigned char*)&bet)[0] == 0xFF;
  if (little_endian_cpu && bit_depth == 16)
  {
    png_set_rows(png, info, &rows[0]);
    png_write_png(png, info, PNG_TRANSFORM_SWAP_ENDIAN, NULL);
  }
  else
  {
    png_write_info(png, info);
    png_write_image(png, &rows[0]);
    png_write_end(png, NULL);
  }

  png_destroy_write_struct(&png,&info);

  fout->close();
  return true;
}
//-----------------------------------------------------------------------------
