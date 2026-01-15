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

#include "ioTGA.hpp"
#include <vlCore/LoadWriterManager.hpp>
#include <vlCore/VisualizationLibrary.hpp>
#include <vlCore/VisualizationLibrary.hpp>
#include <vlCore/FileSystem.hpp>
#include <vlCore/DiskFile.hpp>
#include <vlCore/Image.hpp>

using namespace vl;

#include <vlCore/ImageTools.hpp>

//-----------------------------------------------------------------------------
namespace
{
  const unsigned long TGA_NO_IMAGE_DATA = 0;
  const unsigned long TGA_8BIT_UNCOMPRESSED = 1;
  const unsigned long TGA_RGB_UNCOMPRESSED  = 2;
  const unsigned long TGA_GRAYSCALE_UNCOMPRESSED  = 3;
  const unsigned long TGA_8BIT_COMPRESSED   = 9;
  const unsigned long TGA_RGB_COMPRESSED    = 10;
  const unsigned long TGA_GRAYSCALE_COMPRESSED = 11;

  typedef struct
  {
    unsigned char IdFieldSize; /* at offset 18 it starts, usually 0 */
    unsigned char HasColMap;   /* 1 for indexed images, 0 otherwise */
    unsigned char ImageType;   /* see defines above */
    unsigned char ColMapOrigin[2];
    unsigned char ColMapCount_lo;
    unsigned char ColMapCount_hi;
    unsigned char ColMapEntrySize; /* 16, 24, 32 */
    unsigned char ImageOrigins[4]; /* lo/hi bytes for x/y origins */
    unsigned char Width_lo;
    unsigned char Width_hi;
    unsigned char Height_lo;
    unsigned char Height_hi;
    unsigned char BitsPerPixel;  /* 8/16(?), 16, 24, 32 */
    unsigned char ImageDescriptor; /* origin | alpha channel bits */
  } STGAHeader;
}
//-----------------------------------------------------------------------------
//! Loads a TGA file.
//! Supports the following formats:
//! - IF_RGBA 32 bit uncompressed
//! - RGB  24 bit uncompressed
//! - RGB  16 bit uncompressed
//! - IF_RGBA 32 bit RLE compressed
//! - RGB  24 bit RLE compressed
//! - RGB  16 bit RLE compressed
//! - Grayscale uncompressed
//! - Grayscale RLE compressed
//! - 8 bit palettizzed / 32 bit (IF_RGBA) per color uncompressed
//! - 8 bit palettizzed / 24 bit (rgb) per color uncompressed
//! - 8 bit palettizzed / 32 bit (IF_RGBA) per color RLE uncompressed
//! - 8 bit palettizzed / 24 bit (rgb) per color RLE uncompressed
ref<Image> vl::loadTGA( const String& path )
{
  ref<VirtualFile> file = defFileSystem()->locateFile(path);
  if ( !file )
  {
    Log::error( Say("File '%s' not found.\n") << path );
    return NULL;
  }

  return loadTGA( file.get() );
}
//-----------------------------------------------------------------------------
ref<Image> vl::loadTGA( VirtualFile* file )
{
  if ( !file->open(OM_ReadOnly) )
  {
    Log::error( Say("loadTGA: cannot load TGA file '%s'\n") << file->path() );
    return NULL;
  }

  ref<Image> img = new Image;
  img->setObjectName(file->path().toStdString().c_str());

  STGAHeader header;
  memset(&header, 0, sizeof(header));
  file->read( &header, sizeof(STGAHeader) );

  unsigned int colmap_offset = 18 + header.IdFieldSize;
  unsigned int pixels_offset = colmap_offset +
    (header.ColMapCount_lo + header.ColMapCount_hi*256)*header.ColMapEntrySize/8;
  unsigned int w = header.Width_lo  + header.Width_hi*256;
  unsigned int h = header.Height_lo + header.Height_hi*256;

  #ifndef NDEBUG
    unsigned int bpp = header.BitsPerPixel;
    const char *type = "";
    switch(header.ImageType)
    {
      case TGA_NO_IMAGE_DATA:          type = "TGA_NO_IMAGE_DATA"; break;
      case TGA_8BIT_UNCOMPRESSED:      type = "TGA_8BIT_UNCOMPRESSED"; break;
      case TGA_RGB_UNCOMPRESSED:       type = "TGA_RGB_UNCOMPRESSED"; break;
      case TGA_GRAYSCALE_UNCOMPRESSED: type = "TGA_GRAYSCALE_UNCOMPRESSED"; break;
      case TGA_8BIT_COMPRESSED:        type = "TGA_8BIT_COMPRESSED"; break;
      case TGA_RGB_COMPRESSED:         type = "TGA_RGB_COMPRESSED"; break;
      case TGA_GRAYSCALE_COMPRESSED:   type = "TGA_GRAYSCALE_COMPRESSED"; break;
    }
    Log::debug( Say("TGA %s: w=%n, h=%n, bpp=%n/%n %s\n") << file->path() << w << h << bpp << header.ColMapEntrySize << type);
  #endif

  if (header.ImageType == TGA_NO_IMAGE_DATA)
    return NULL;

  if (header.ImageType == TGA_RGB_COMPRESSED)
  {
    int pixsize = 0;
    switch(header.BitsPerPixel)
    {
      case 32: pixsize = 4; break;
      case 24: pixsize = 3; break;
      case 16: pixsize = 2; break;
    }

    if (pixsize)
    {
      img->allocate2D(w, h, 4, IF_RGBA, IT_UNSIGNED_BYTE);

      file->seekSet(pixels_offset);
      int pixcount = w*h;
      int pix = 0;
      while(pix < pixcount)
      {
        unsigned char header_ch = 0;
        file->read(&header_ch, 1);
        if (header_ch >= 128)
        {
          int count = header_ch - 128 + 1;
          unsigned char bgra[4];
          file->read(bgra, pixsize);
          while(count--)
          {
            memcpy((unsigned char*)img->pixels() + pix*pixsize, bgra, pixsize);
            pix++;
          }
        }
        else
        {
          int count = header_ch + 1;
          file->read((unsigned char*)img->pixels() + pix*pixsize, pixsize*count);
          pix += count;
        }
      }

      switch(header.BitsPerPixel)
      {
        case 24: convertRGBToRGBA(img->pixels(), img->width(), img->height(), 0xFF); // break;
        case 32: swapBytes32_BGRA_RGBA(img->pixels(), img->requiredMemory()); break;
        case 16: convertA1R5G5B5ToRGBA(img->pixels(), w*h, 0xFF); break;
      }

    }
    else
    {
      Log::error( Say("TGA ERROR: TGA_RGB_COMPRESSED %nbpp not supported.\n") << header.BitsPerPixel );
      file->close();
      return NULL;
    }

  }
  else
  if (header.ImageType == TGA_RGB_UNCOMPRESSED)
  {
    int pixsize = 0;

    switch(header.BitsPerPixel)
    {
      case 32: pixsize = 4; break;
      case 24: pixsize = 3; break;
      case 16: pixsize = 2; break;
    }

    if (pixsize)
    {
      file->seekSet(pixels_offset);
      img->allocate2D(w, h, 4, IF_RGBA, IT_UNSIGNED_BYTE);
      file->read(img->pixels(), w*h*pixsize);
      switch(header.BitsPerPixel)
      {
        case 24: convertRGBToRGBA(img->pixels(), img->width(), img->height(), 0xFF); // break;
        case 32: swapBytes32_BGRA_RGBA(img->pixels(), img->requiredMemory()); break;
        case 16: convertA1R5G5B5ToRGBA(img->pixels(), w*h, 0xFF); break;
      }
    }
    else
    {
      Log::error( Say("TGA ERROR: TGA_RGB_UNCOMPRESSED %nbpp not supported.\n") << header.BitsPerPixel );
      file->close();
      return NULL;
    }

  }
  else
  if (header.ImageType == TGA_8BIT_UNCOMPRESSED || header.ImageType == TGA_8BIT_COMPRESSED)
  {
    if (header.BitsPerPixel == 8)
    {

      unsigned int colmap_count = header.ColMapCount_lo + header.ColMapCount_hi*256;
      VL_CHECK(colmap_count<=256);
      if (header.ColMapEntrySize == 24)
      {
        TPalette3x256 palette;
        file->seekSet(colmap_offset);
        file->read(palette, colmap_count*3);

        file->seekSet(pixels_offset);

        img->allocate2D(w, h, 4, IF_RGBA, IT_UNSIGNED_BYTE);
        if (header.ImageType == TGA_8BIT_UNCOMPRESSED)
        {
          file->read(img->pixels(), w*h*1);
        }
        else // TGA_8BIT_UNCOMPRESSED
        {
          int pixsize = 1;
          int pixcount = w*h;
          int pix = 0;
          while(pix < pixcount)
          {
            unsigned char header_ch = 0;
            file->read(&header_ch, 1);
            if (header_ch >= 128)
            {
              int count = header_ch - 128 + 1;
              unsigned char bgra[4];
              file->read(bgra, pixsize);
              while(count--)
              {
                memcpy((unsigned char*)img->pixels() + pix*pixsize, bgra, pixsize);
                pix++;
              }
            }
            else
            {
              int count = header_ch + 1;
              file->read((unsigned char*)img->pixels() + pix*pixsize, pixsize*count);
              pix += count;
            }
          }
        }

        convert8ToRGBA(palette, img->pixels(), img->width(), img->height(), 0xFF);
        swapBytes32_BGRA_RGBA(img->pixels(), img->requiredMemory());
      }
      else if (header.ColMapEntrySize == 32)
      {
        TPalette4x256 palette;
        file->seekSet(colmap_offset);
        file->read(palette, colmap_count*4);

        file->seekSet( pixels_offset );
        img->allocate2D(w, h, 4, IF_RGBA, IT_UNSIGNED_BYTE);
        if (header.ImageType == TGA_8BIT_UNCOMPRESSED)
        {
          file->read(img->pixels(), w*h*1);
        }
        else // TGA_8BIT_UNCOMPRESSED
        {
          int pixsize = 1;
          int pixcount = w*h;
          int pix = 0;
          while(pix < pixcount)
          {
            unsigned char header_ch = 0;
            file->read(&header_ch, 1);
            if (header_ch >= 128)
            {
              int count = header_ch - 128 + 1;
              unsigned char bgra[4];
              file->read(bgra, pixsize);
              while(count--)
              {
                memcpy((unsigned char*)img->pixels() + pix*pixsize, bgra, pixsize);
                pix++;
              }
            }
            else
            {
              int count = header_ch + 1;
              file->read((unsigned char*)img->pixels() + pix*pixsize, pixsize*count);
              pix += count;
            }
          }
        }

        convert8ToRGBA(palette, img->pixels(), img->width(), img->height());
        swapBytes32_BGRA_RGBA(img->pixels(), img->requiredMemory());
      }
      else
      {
        Log::error( Say("TGA ERROR: TGA_8BIT_UNCOMPRESSED entry size = %n not supported.\n") << header.ColMapEntrySize );
        file->close();
        return NULL;
      }
    }
    else
    {
      Log::error( Say("TGA ERROR: TGA_8BIT_UNCOMPRESSED %nbpp bit not supported.\n") << header.BitsPerPixel );
      file->close();
      return NULL;
    }
  }
  else
  if (header.ImageType == TGA_GRAYSCALE_UNCOMPRESSED || header.ImageType == TGA_GRAYSCALE_COMPRESSED)
  {
    if (header.BitsPerPixel == 8)
    {
      file->seekSet(pixels_offset);
      img->allocate2D(w, h, 1, IF_LUMINANCE, IT_UNSIGNED_BYTE);
      if (header.ImageType == TGA_GRAYSCALE_UNCOMPRESSED)
      {
        file->read(img->pixels(), w*h);
      }
      else  // TGA_GRAYSCALE_COMPRESSED
      {
        int pixsize = 1;
        int pixcount = w*h;
        int pix = 0;
        while(pix < pixcount)
        {
          unsigned char header_ch = 0;
          file->read(&header_ch, 1);
          if (header_ch >= 128)
          {
            int count = header_ch - 128 + 1;
            unsigned char bgra[4];
            file->read(bgra, pixsize);
            while(count--)
            {
              memcpy((unsigned char*)img->pixels() + pix*pixsize, bgra, pixsize);
              pix++;
            }
          }
          else
          {
            int count = header_ch + 1;
            file->read((unsigned char*)img->pixels() + pix*pixsize, pixsize*count);
            pix += count;
          }
        }
      }
    }
    else
    {
      Log::error( Say("TGA ERROR: TGA_GRAYSCALE_UNCOMPRESSED %nbpp not supported.\n") << header.BitsPerPixel );
      file->close();
      return NULL;
    }

  }
  else
  {
    Log::error( Say("TGA ERROR: this type %n not supported.\n") << header.ImageType);
    file->close();
    return NULL;
  }

  if ((header.ImageDescriptor & (1<<5)))
    img->flipVertically();

  file->close();
  return img;
}
//-----------------------------------------------------------------------------
bool vl::saveTGA(const Image* src, const String& path)
{
  ref<DiskFile> file = new DiskFile(path);
  return saveTGA(src, file.get());
}
//-----------------------------------------------------------------------------
bool vl::saveTGA(const Image* src, VirtualFile* fout)
{
  //if (src->dimension() != ID_2D )
  //{
  //  Log::error( Say("saveTGA('%s'): can save only 2D images.\n") << fout->path() );
  //  return false;
  //}

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
      Log::error( Say("saveTGA('%s'): could not convert image to IT_UNSIGNED_BYTE.\n") << fout->path() );
      return false;
    }
  }
  if (src->format() != IF_BGRA)
  {
    cimg = src->convertFormat(IF_BGRA);
    src = cimg.get();
    if (!cimg)
    {
      Log::error( Say("saveTGA('%s'): could not convert image to IF_BGRA.\n") << fout->path() );
      return false;
    }
  }

  if(!fout->open(OM_WriteOnly))
  {
    Log::error( Say("TGA: could not write to '%s'.\n") << fout->path() );
    return false;
  }

  STGAHeader header;
  memset(&header, 0, sizeof(STGAHeader));
  // flip vertically
  // header.ImageDescriptor |= 1<<5;
  header.ImageType = TGA_RGB_UNCOMPRESSED;
  header.Width_lo  = (unsigned char)(w & 0x00FF);
  header.Width_hi  = (unsigned char)(w >> 8);
  header.Height_lo = (unsigned char)(h & 0x00FF);
  header.Height_hi = (unsigned char)(h >> 8);
  header.BitsPerPixel = 32;
  fout->write(&header, sizeof(header));

  fout->write(src->pixels(), src->requiredMemory());

  // TGA footer

  // extension area offset
  fout->writeUInt32(0);
  // developer directory offset
  fout->writeUInt32(0);
  // signature + "." + "0"
  fout->write("TRUEVISION-XFILE.", 18);

  fout->close();
  return true;
}
//-----------------------------------------------------------------------------
//! A TGA file is accepted only if it has a 'TGA' extension.
bool vl::isTGA( VirtualFile* file )
{
  if (!file->open(OM_ReadOnly))
    return false;

  STGAHeader header;
  memset(&header, 0, sizeof(header));
  file->read(&header, sizeof(STGAHeader) );

  char signature[17];
  memset(signature, 0, 17);
  file->seekEnd(-18);
  file->read(signature, 16);
  file->close();

  // unfortunately many TGA files are without this field

  if (strcmp("TRUEVISION-XFILE", signature) == 0)
    return true;

  // do some heuristic checks

  switch( header.ImageType )
  {
    case 0:
    case 1:
    case 2:
    case 3:
    case 9:
    case 10:
    case 11:
      break;
    default:
      return false;
  }

  // unsigned int colmap_offset = 18 + header.IdFieldSize;
  // unsigned int pixels_offset = colmap_offset + (header.ColMapCount_lo + header.ColMapCount_hi*256)*header.ColMapEntrySize/8;
  unsigned int width  = header.Width_lo  + header.Width_hi*256;
  unsigned int height = header.Height_lo + header.Height_hi*256;
  unsigned int bpp = header.BitsPerPixel;

  if (width * height == 0)
    return false;

  switch( bpp )
  {
    case 1:
    case 4:
    case 8:
    case 16:
    case 24:
    case 32:
      break;
    default:
      return false;
  }

  // ends with .tga
  if ( !file->path().toLowerCase().endsWith(".tga") )
    return false;

  Log::warning( Say("isTGA: the file '%s' looks like a TGA but is missing the 'TRUEVISION-XFILE' signature.\n") << file->path() );
  return true;
}
//-----------------------------------------------------------------------------
