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

#include "ioBMP.hpp"
#include <vlCore/LoadWriterManager.hpp>
#include <vlCore/VisualizationLibrary.hpp>
#include <vlCore/FileSystem.hpp>
#include <vlCore/VirtualFile.hpp>
#include <vlCore/Image.hpp>

using namespace vl;

#include <vlCore/ImageTools.hpp>

//-----------------------------------------------------------------------------
// BMP loader
//-----------------------------------------------------------------------------
namespace
{
  typedef struct
  {
    char mType[2];
    char mSize[4];
    char mReserved1[2];
    char mReserved2[2];
    char mOffBits[4];

    unsigned short Type() const { return *(const unsigned short*)mType; }
    unsigned int  Size() const { return *(const unsigned int*)mSize; }
    unsigned short Reserved1() const { return *(const unsigned short*)mReserved1; }
    unsigned short Reserved2() const { return *(const unsigned short*)mReserved2; }
    unsigned int  OffBits() const { return *(const unsigned int*)mOffBits; }
  } BitmapFileHeader;

  typedef struct
  {
    unsigned int Size() { return *(unsigned int*)mSize; }
    int Width() { return *(int*)mWidth; }
    int Height() { return *(int*)mHeight; }
    unsigned short Planes() { return *(unsigned short*)mPlanes; }
    unsigned short BitCount() { return *(unsigned short*)mBitCount; }
    unsigned int Compression() { return *(unsigned int*)mCompression; }
    unsigned int SizeImage() { return *(unsigned int*)mSizeImage; }
    int XPelsPerMeter() { return *(int*)mXPelsPerMeter; }
    int YPelsPerMeter() { return *(int*)mYPelsPerMeter; }
    unsigned int ClrUsed() { return *(unsigned int*)mClrUsed; }
    unsigned int ClrImportant() { return *(unsigned int*)mClrImportant; }

    char mSize[4];
    char mWidth[4];
    char mHeight[4];
    char mPlanes[2];
    char mBitCount[2];
    char  mCompression[4];
    char  mSizeImage[4];
    char mXPelsPerMeter[4];
    char mYPelsPerMeter[4];
    char mClrUsed[4];
    char mClrImportant[4];
  } BitmapInfoHeader;

  const unsigned int BMP_NoCompression  = 0;
  const unsigned int BMP_XRGB  = 3;
  // RLE8 = 1L;
  // RLE4 = 2L;
  // JPEG = 4L;
  // PNG  = 5L;
}
//-----------------------------------------------------------------------------
//! Loads a BMP file.
//! Supports the following formats:
//! - 8 bit palettized, uncompressed
//! - 24 bit RGB, uncompressed
//! - 32 bit RGBX, uncompressed
//! - image flipping
ref<Image> vl::loadBMP( const String& path )
{
  ref<VirtualFile> file = defFileSystem()->locateFile(path);
  if ( !file )
  {
    Log::error( Say("File '%s' not found.\n") << path );
    return NULL;
  }

  return loadBMP(file.get());
}
//-----------------------------------------------------------------------------
ref<Image> vl::loadBMP( VirtualFile* file )
{
  if ( !file->open(OM_ReadOnly) )
  {
    Log::error( Say("loadBMP: error opening file: '%s'\n") << file->path() );
    return NULL;
  }

  ref<Image> img = new Image;
  img->setObjectName(file->path().toStdString().c_str());

  BitmapFileHeader bfh;
  memset(&bfh, 0, sizeof(bfh));
  BitmapInfoHeader bih;
  memset(&bih, 0, sizeof(bih));

  file->read(&bfh, sizeof(bfh));
  if (bfh.Type() != 0x4D42)
  {
    file->close();
    Log::error( Say("The file '%s' is not a BMP file.\n") << file->path() );
    return NULL;
  }

  int head = (int)file->position();

  file->read(&bih, sizeof(bih));

  bool flip = false;
  if ( bih.Height() < 0 )
  {
    int *h = (int*)bih.mHeight;
    *h = -*h;
    flip = true;
  }
  VL_CHECK( bih.Height() * bih.Width() );

  // TODO: support bih.Compression() == BMP_Type_RLE8 e BMP_XRGB meglio
  if ( bih.Compression() != BMP_NoCompression && bih.Compression() != BMP_XRGB )
  {
    Log::error( Say("Compression %n unsupported for %s\n") << bih.Compression() << file->path() );
    file->close();
    return NULL;
  }

  switch( bih.BitCount() )
  {
    // 8 bit palettized
    case 8:
    {
      img->allocate2D(bih.Width(), bih.Height(), 4, IF_BGRA, IT_UNSIGNED_BYTE);

      TPalette4x256 palette;
      memset(palette, 0, 256*4);

      int br;
      file->seekSet( (int)head + bih.Size() );
      int palette_bytes = (bih.ClrUsed() ? bih.ClrUsed() : 256)*4;
      br = (int)file->read(palette, palette_bytes);
      /*// set alpha to 0xFF
      for(int i=0; i<palette_bytes; i+=4)
        palette[i+3] = 0xFF;*/
      if(br != palette_bytes)
      {
        file->close();
        Log::error( Say("File %s truncated: %n < %n.\n") << file->path() << br << palette_bytes );
        return NULL;
      }

      int pad_bytes = (4 - (img->width() * 1) % 4) % 4;
      int datasize = (img->width() * 1 + pad_bytes) * img->height();

      // 8bpp uncompressed

      if ( bih.Compression() == BMP_NoCompression )
      {
        br = (int)file->read(img->pixels(), datasize);
        if(br != datasize)
        {
          // fclose(fin);
          file->close();
          Log::error( Say("file %s truncated.\n") << file->path() );
          return NULL;
        }
      }
      // BMP_Type_RLE8 not supported yet
      else
      {
        // TODO
      }

      convert8ToRGBA( palette, img->pixels(), img->width(), img->height(), 4 );
    } break;

    case 32:
    case 24:
    {
      img->allocate2D(bih.Width(), bih.Height(), 4, IF_BGRA, IT_UNSIGNED_BYTE);
      file->seekSet(bfh.OffBits());

      // usato solo se 24 bits.
      int pad_bytes = (4 - (img->width() * 3) % 4) % 4;
      // fa prima se lo si legge tutto insieme.
      int datasize = bih.BitCount() == 32 ? img->width() * img->height() * 4 : (img->width() * 3 + pad_bytes) * img->height();
      int rd = (int)file->read(img->pixels(), datasize);
      if( rd < datasize )
      {
        file->close();
        Log::error( Say("File %s truncated.\n") << file->path() );
        return NULL;
      }

      // convert the buffer from 24 to 32bpp running from the end to the begining.
      if (bih.BitCount() == 24)
        convertRGBToRGBA(img->pixels(), img->width(), img->height(), 0xFF, 4);

    } break;

    default: 
    {
      file->close();
      Log::error( Say("BitCount %n unsupported for %s\n") << (int)bih.BitCount() << file->path() );
      return NULL;
    }
  }

  file->close();

  // convert BGRA to RGBA
  img = img->convertFormat(IF_RGBA);
  
  // set alpha channel to 0xFF
  for(int h=0; h<img->height(); ++h)
  {
    unsigned char* px = img->pixels() + img->pitch()*h;
    for(int x=0; x<img->width()*4; x+=4)
      px[x+3] = 0xFF;
  }

  // flip the image
  if (flip)
    img->flipVertically();

  return img;
}
//-----------------------------------------------------------------------------
bool vl::isBMP(VirtualFile* file)
{
  if (!file->open(OM_ReadOnly))
    return false;

  BitmapFileHeader bfh;
  memset(&bfh, 0, sizeof(bfh));
  BitmapInfoHeader bih;
  memset(&bih, 0, sizeof(bih));

  file->read(&bfh, sizeof(bfh));
  file->read(&bih, sizeof(bih));
  file->close();

  if (bfh.Type() != 0x4D42)
  {
    return false;
  }

  // check bit count
  switch( bih.BitCount() )
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

  // check compression
  switch( bih.Compression() )
  {
  case 0:
  case 1:
  case 2:
  case 3:
  case 4:
  case 5:
    break;
  default:
    return false;
  }

  return true;
}
//-----------------------------------------------------------------------------
