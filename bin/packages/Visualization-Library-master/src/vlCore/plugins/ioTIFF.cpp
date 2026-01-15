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

#include "ioTIFF.hpp"
#include <vlCore/LoadWriterManager.hpp>
#include <vlCore/VisualizationLibrary.hpp>
#include <vlCore/FileSystem.hpp>
#include <vlCore/VirtualFile.hpp>
#include <vlCore/Image.hpp>
#include <tiffio.h>

// mic fixme: read and write 16 bits images.

using namespace vl;

namespace
{
  void tiff_error(const char*, const char*, va_list)
  {
    vl::Log::error("ioTIFF unspecified error.\n");
  }
  void tiff_warning(const char *, const char *, va_list)
  {
  }
  tsize_t tiff_io_read_func(thandle_t fd, tdata_t buf, tsize_t size)
  {
    VirtualFile *fin = (VirtualFile*)fd;
    long long c = fin->read(buf,size);
    return (tsize_t)c;
  }
  tsize_t tiff_io_write_func(thandle_t fd, tdata_t buf, tsize_t size)
  {
    VirtualFile *fin = (VirtualFile*)fd;
    long long c = fin->write(buf,size);
    return (tsize_t)c;
  }
  toff_t tiff_io_seek_func(thandle_t fd, toff_t off, int i)
  {
    VirtualFile*fin = (VirtualFile*)fd;

    switch(i)
    {
    case SEEK_SET:
      fin->seekSet(off);
      return (tsize_t)fin->position();

    case SEEK_CUR:
      fin->seekCur(off);
      return (tsize_t)fin->position();

    case SEEK_END:
      fin->seekEnd(off);
      return (tsize_t)fin->position();

    default:
      return 0;
    }
  }
  int tiff_io_close_func(thandle_t fd) 
  {
    VirtualFile*fin = (VirtualFile*)fd;
    fin->close();
    return 0;
  }
  toff_t tiff_io_size_func(thandle_t fd)
  {
    VirtualFile *fin = (VirtualFile*)fd;
    return (tsize_t)fin->size();
  }
  int tiff_io_map_func(thandle_t, tdata_t*, toff_t*) 
  { 
    return 0; 
  }
  void tiff_io_unmap_func(thandle_t, tdata_t, toff_t) 
  {
    return;
  }
}

//-----------------------------------------------------------------------------
ref<Image> vl::loadTIFF(const String& path)
{
  ref<VirtualFile> file = defFileSystem()->locateFile(path);
  if ( !file )
  {
    Log::error( Say("File '%s' not found.\n") << path );
    return NULL;
  }
  else
    return loadTIFF(file.get());
}
//-----------------------------------------------------------------------------
ref<Image> vl::loadTIFF(VirtualFile* file)
{
  file->open(OM_ReadOnly);
  ref<Image> img = new Image;

  TIFFSetErrorHandler(tiff_error);
  TIFFSetWarningHandler(tiff_warning);

  TIFF* tif = TIFFClientOpen("tiffread", "r", reinterpret_cast<thandle_t>(file),
                tiff_io_read_func, 
                tiff_io_write_func,
                tiff_io_seek_func,
                tiff_io_close_func,
                tiff_io_size_func,
                tiff_io_map_func,
                tiff_io_unmap_func);

  if (tif) 
  {
    uint32 w, h;
    size_t npixels;
    uint32* raster;
    
    TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &w);
    TIFFGetField(tif, TIFFTAG_IMAGELENGTH, &h);
    npixels = w * h;
    raster = (uint32*) _TIFFmalloc(npixels * sizeof (uint32));
    if (raster != NULL) 
    {
      if (TIFFReadRGBAImage(tif, w, h, raster, 0)) 
      {
        img->allocate2D(w,h,1,vl::IF_RGBA,vl::IT_UNSIGNED_BYTE);
        memcpy(img->pixels(), raster, img->requiredMemory());
      }
      _TIFFfree(raster);
    }
    uint16 orientation = ORIENTATION_TOPLEFT; // default
    TIFFGetField(tif, TIFFTAG_ORIENTATION, &orientation);
    if (orientation == ORIENTATION_LEFTBOT )
      img->flipVertically();
    TIFFClose(tif);
  }
  
  file->close();
  return img;
}
//-----------------------------------------------------------------------------
bool vl::isTIFF(VirtualFile* file)
{
  if (!file->open(OM_ReadOnly))
    return false;

  unsigned char byteorder[2];
  file->read(byteorder, 2);
  bool little_endian = byteorder[0] == 'I'; // 'I' == LE, 'M' == BE
  unsigned short version = file->readUInt16(little_endian);
  file->close();

  if (byteorder[0] != byteorder[1])
    return false;

  if (byteorder[0] != 'M' && byteorder[0] != 'I')
    return false;

  if (byteorder[1] != 'M' && byteorder[1] != 'I')
    return false;

  if (version != 42)
    return false;

  return true;
}
//-----------------------------------------------------------------------------
bool vl::saveTIFF(const Image* image, const String& path)
{
  ref<DiskFile> file = new DiskFile(path);
  return saveTIFF(image, file.get());
}
//-----------------------------------------------------------------------------
bool vl::saveTIFF(const Image* src, VirtualFile* fout)
{
  // mic fixme: handle somehow not supported image type, cubemaps, 3d textures, mimaps etc.

  int w = src->width();
  int h = src->height();
  int d = src->depth();
  if (h == 0) h=1;
  if (d == 0) d=1;
  if (src->isCubemap()) d=6;
  h = h*d;

  // convert src to IT_UNSIGNED_BYTE / IF_RGBA
  ref<Image> cimg;
  if (src->type() != IT_UNSIGNED_BYTE)
  {
    cimg = src->convertType(IT_UNSIGNED_BYTE);
    src = cimg.get();
    if (!cimg)
    {
      Log::error( Say("saveTIFF('%s'): could not convert src to IT_UNSIGNED_BYTE.\n") << fout->path() );
      return false;
    }
  }
  if (src->format() != IF_RGBA)
  {
    cimg = src->convertFormat(IF_RGBA);
    src = cimg.get();
    if (!cimg)
    {
      Log::error( Say("saveTIFF('%s'): could not convert src to IF_RGBA.\n") << fout->path() );
      return false;
    }
  }

  const int SHORT     = 3;
  const int LONG      = 4;
  const int RATIONAL  = 5;

  if (!fout->open(OM_WriteOnly))
  {
    Log::error( Say("TIFF: could not open '%s' for writing.\n") << fout->path() );
    return false;
  }

  // little endian
  unsigned char little_endian[] = { 'I', 'I' };
  fout->write(little_endian, 2);

  unsigned short version = 42;
  fout->writeUInt16(version);

  unsigned long ifd_offset = 8;
  fout->writeUInt32(ifd_offset);

  unsigned short dir_count = 14;
  fout->writeUInt16(dir_count);

  unsigned short tag, type;
  unsigned long count;

  // WIDTH

  tag = 256; fout->writeUInt16(tag);
  type = SHORT; fout->writeUInt16(type);
  count = 1; fout->writeUInt32(count);
  fout->writeUInt16((unsigned short)w); fout->writeUInt16(0);

  // HEIGHT

  tag = 257; fout->writeUInt16(tag);
  type = SHORT; fout->writeUInt16(type);
  count = 1; fout->writeUInt32(count);
  fout->writeUInt16((unsigned short)h); fout->writeUInt16(0);

  // BITS PER SAMPLE

  tag = 258; fout->writeUInt16(tag);
  type = SHORT; fout->writeUInt16(type);
  count = 4; fout->writeUInt32(count);
  fout->writeUInt32(10 + dir_count*12 + 4 + 16);

  // COMPRESSION

  tag = 259; fout->writeUInt16(tag);
  type = SHORT; fout->writeUInt16(type);
  count = 1; fout->writeUInt32(count);
  fout->writeUInt16(1); fout->writeUInt16(0);

  // PHOTOMETRIC INTERPRETATION

  tag = 262; fout->writeUInt16(tag);
  type = SHORT; fout->writeUInt16(type);
  count = 1; fout->writeUInt32(count);
  fout->writeUInt16(2); fout->writeUInt16(0);

  // STRIP OFFSET

  tag = 273; fout->writeUInt16(tag);
  type = LONG; fout->writeUInt16(type);
  count = 1; fout->writeUInt32(count);
  fout->writeUInt32(10 + dir_count*12 + 4 + 16 + 8);

  // SAMPLES PER PIXEL

  tag = 277; fout->writeUInt16(tag);
  type = SHORT; fout->writeUInt16(type);
  count = 1; fout->writeUInt32(count);
  fout->writeUInt16(4); fout->writeUInt16(0);

  // ROWS PER STRIP

  tag = 278; fout->writeUInt16(tag);
  type = SHORT; fout->writeUInt16(type);
  count = 1; fout->writeUInt32(count);
  fout->writeUInt16((unsigned short)h); fout->writeUInt16(0);

  // STRIP BYTE COUNT

  tag = 279; fout->writeUInt16(tag);
  type = LONG; fout->writeUInt16(type);
  count = 1; fout->writeUInt32(count);
  fout->writeUInt32(src->requiredMemory());

  // X RESOLUTION

  tag = 282; fout->writeUInt16(tag);
  type = RATIONAL; fout->writeUInt16(type);
  count = 1; fout->writeUInt32(count);
  fout->writeUInt32(10 + dir_count*12 + 4 + 0);

  // Y RESOLUTION

  tag = 283; fout->writeUInt16(tag);
  type = RATIONAL; fout->writeUInt16(type);
  count = 1; fout->writeUInt32(count);
  fout->writeUInt32(10 + dir_count*12 + 4 + 8);

  // PLANAR CONFIGURATION

  tag = 284; fout->writeUInt16(tag);
  type = SHORT; fout->writeUInt16(type);
  count = 1; fout->writeUInt32(count);
  fout->writeUInt16(1); fout->writeUInt16(0);

  // RESOLUTION UNIT

  tag = 296; fout->writeUInt16(tag);
  type = SHORT; fout->writeUInt16(type);
  count = 1; fout->writeUInt32(count);
  fout->writeUInt16(2); fout->writeUInt16(0);

  // EXTRA SAMPLES

  tag = 338; fout->writeUInt16(tag);
  type = SHORT; fout->writeUInt16(type);
  count = 1; fout->writeUInt32(count);
  fout->writeUInt16(0); fout->writeUInt16(0);

  // next ifd offset
  fout->writeUInt32(0);

  // rational1
  fout->writeUInt32(72);
  fout->writeUInt32(1);
  // rational2
  fout->writeUInt32(72);
  fout->writeUInt32(1);

  // bits per sample
  fout->writeUInt16(8);
  fout->writeUInt16(8);
  fout->writeUInt16(8);
  fout->writeUInt16(8);

  // save the lines in reverse order
  int y = h;
  while( y-- )
    fout->write(src->pixels()+src->pitch()*y, w*src->bitsPerPixel()/8);

  fout->close();
  return true;
}
//-----------------------------------------------------------------------------
