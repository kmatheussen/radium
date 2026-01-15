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

#include <vlCore/Image.hpp>
#include <vlCore/checks.hpp>
#include <vlCore/Say.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/VisualizationLibrary.hpp>
#include <vlCore/FileSystem.hpp>
#include <vlCore/VirtualFile.hpp>
#include <vlCore/glsl_math.hpp>
#include <vlCore/ResourceDatabase.hpp>
#include <vlCore/LoadWriterManager.hpp>

#include <map>
#include <cmath>

using namespace vl;

//-----------------------------------------------------------------------------
// Image
//-----------------------------------------------------------------------------
Image::~Image()
{
  reset();
}
//-----------------------------------------------------------------------------
Image::Image()
{
  VL_DEBUG_SET_OBJECT_NAME()
  mPixels = new Buffer;
  reset();
}
//-----------------------------------------------------------------------------
Image::Image(const Image& other): Object(other) 
{
  VL_DEBUG_SET_OBJECT_NAME()
  mPixels = new Buffer;
  reset();
  *this = other; 
}
//-----------------------------------------------------------------------------
Image::Image(const String& path)
{
  VL_DEBUG_SET_OBJECT_NAME()
  mPixels = new Buffer;
  reset();

  setObjectName(path.toStdString().c_str());
  ref<Image> img = loadImage(path);
  if (!img)
  {
    mFilePath = path;
    return;
  }
  // quicker than *this = *img;
  mPixels->swap(*img->mPixels);
  mMipmaps.swap(img->mMipmaps);
  mWidth  = img->mWidth;
  mHeight = img->mHeight;
  mDepth  = img->mDepth;
  mPitch  = img->mPitch;
  mByteAlign = img->mByteAlign;
  mFormat    = img->mFormat;
  mType      = img->mType;
  mIsCubemap = img->mIsCubemap;
  mIsNormalMap = img->mIsNormalMap;
  mHasAlpha    = img->mHasAlpha;
  mFilePath    = img->mFilePath;
}
//-----------------------------------------------------------------------------
//! Note that the image is not allocated
Image::Image(int x, int y, int z, int bytealign, EImageFormat format, EImageType type):
  mWidth(x), mHeight(y), mDepth(z), mPitch(0), mByteAlign(1), mFormat(format), mType(type), mIsCubemap(false), mIsNormalMap(false), mHasAlpha(false)
{
  VL_DEBUG_SET_OBJECT_NAME()
  mPixels = new Buffer;
  setByteAlignment(bytealign);

  if (x && y && z)
    allocate3D(x,y,z,bytealign,format,type);
  else
  if (x && y)
    allocate2D(x,y,bytealign,format,type);
  else
  if (x)
    allocate1D(x,format,type);
}
//-----------------------------------------------------------------------------
//! Returns true if the image has valid width/height/depth, pitch and byte alignment,
//! type/format combination.
//! \remarks
//! In order to be valid an image must be at least 1x0x0 (1D image) or 1x1x0 (2D or
//! cubemap image) or 1x1x1 (3D image). Sizes like Nx0xM, 0xNxM, 0xNx0, 0x0xN, 0x0x0
//! are not valid.\n
//! Invalid type/format combinations are for example:
//! - OGL_RGB / IT_UNSIGNED_SHORT_4_4_4_4: since here the format has 3 components while the type has 4.
//! - IF_COMPRESSED_RGB_S3TC_DXT1_EXT / IT_BYTE: since compressed formats require IT_IMPLICIT_TYPE.
bool Image::isValid() const
{
  // size check

  bool x = mWidth > 0 && mHeight == 0 && mDepth == 0;
  bool y = mWidth > 0 && mHeight > 0 && mDepth == 0;
  bool z = mWidth > 0 && mHeight > 0 && mDepth > 0;

  // format check

  bool okformat = false;

  // these go well with all but compressed formats

  switch(type())
  {
    default:
    break;

    case IT_UNSIGNED_BYTE:
    case IT_BYTE:
    case IT_UNSIGNED_SHORT:
    case IT_SHORT:
    case IT_UNSIGNED_INT:
    case IT_INT:
    case IT_FLOAT:
    {
      switch(format())
      {
        case IF_COMPRESSED_RGB_S3TC_DXT1:
        case IF_COMPRESSED_RGBA_S3TC_DXT1:
        case IF_COMPRESSED_RGBA_S3TC_DXT3:
        case IF_COMPRESSED_RGBA_S3TC_DXT5:
        {
          break;
        }

        default:
        {
          okformat = true;
        }
      }
    }
  }

  // compressed type go well only with compressed formats

  switch(type())
  {
    default:
    break;

    case IT_IMPLICIT_TYPE:
    {
      switch(format())
      {
        case IF_COMPRESSED_RGB_S3TC_DXT1:
        case IF_COMPRESSED_RGBA_S3TC_DXT1:
        case IF_COMPRESSED_RGBA_S3TC_DXT3:
        case IF_COMPRESSED_RGBA_S3TC_DXT5:
        {
          okformat = true;
          break;
        }

        default:
        {
          break;
        }
      }
    }
  }

  switch(format())
  {
    default:
    break;

    // depth stencil
    case IF_DEPTH_STENCIL:
    {
      switch(type())
      {
        case IT_FLOAT_32_UNSIGNED_INT_24_8_REV:
        case IT_UNSIGNED_INT_24_8:
          okformat = true;
        default:
        break;
      }
    }
    break;

    // three components types

    case IF_RGB:
    case IF_BGR:
    {
      switch(type())
      {
        case IT_UNSIGNED_BYTE_3_3_2:
        case IT_UNSIGNED_BYTE_2_3_3_REV:
        case IT_UNSIGNED_SHORT_5_6_5:
        case IT_UNSIGNED_SHORT_5_6_5_REV:
        case IT_UNSIGNED_INT_5_9_9_9_REV: /* EXT_texture_shared_exponent, support only GL_RGB */
        case IT_UNSIGNED_INT_10F_11F_11F_REV: /* EXT_packed_float, supports only GL_RGB */
        {
          okformat = true;
          break;
        }

        default:
        {
          break;
        }
      }
      break;
    }

    // four components types

    case IF_RGBA:
    case IF_BGRA:
    {
      switch(type())
      {
        case IT_UNSIGNED_SHORT_4_4_4_4:
        case IT_UNSIGNED_SHORT_4_4_4_4_REV:
        case IT_UNSIGNED_SHORT_5_5_5_1:
        case IT_UNSIGNED_SHORT_1_5_5_5_REV:
        case IT_UNSIGNED_INT_8_8_8_8:
        case IT_UNSIGNED_INT_8_8_8_8_REV:
        case IT_UNSIGNED_INT_10_10_10_2:
        case IT_UNSIGNED_INT_2_10_10_10_REV:
        {
          okformat = true;
          break;
        }

        default:
        {
          break;
        }
      }
    }
  }

  #ifndef NDEBUG
    bool isvalid = okformat && (x|y|z) && (pitch() % mByteAlign == 0);
    Log::debug( isvalid ? "" : ( okformat ? "Image::isValid(): invalid dimensions or pitch/bytealign combination:\n" : "Image::isValid() reported an invalid format/type combination:\n") + print() );
  #endif

  return okformat && (x|y|z) && (pitch() % mByteAlign == 0);
}
//-----------------------------------------------------------------------------
String Image::printType() const
{
  std::map<int, const char*> ty;

  ty[IT_IMPLICIT_TYPE] = "IT_IMPLICIT_TYPE";
  ty[IT_UNSIGNED_BYTE] = "IT_UNSIGNED_BYTE";
  ty[IT_BYTE] = "IT_BYTE";
  ty[IT_UNSIGNED_SHORT] = "IT_UNSIGNED_SHORT";
  ty[IT_SHORT] = "IT_SHORT";
  ty[IT_UNSIGNED_INT] = "IT_UNSIGNED_INT";
  ty[IT_INT] = "IT_INT";
  ty[IT_FLOAT] = "IT_FLOAT";
  ty[IT_UNSIGNED_BYTE_3_3_2] = "IT_UNSIGNED_BYTE_3_3_2";
  ty[IT_UNSIGNED_BYTE_2_3_3_REV] = "IT_UNSIGNED_BYTE_2_3_3_REV";
  ty[IT_UNSIGNED_SHORT_5_6_5] = "IT_UNSIGNED_SHORT_5_6_5";
  ty[IT_UNSIGNED_SHORT_5_6_5_REV] = "IT_UNSIGNED_SHORT_5_6_5_REV";
  ty[IT_UNSIGNED_SHORT_4_4_4_4] = "IT_UNSIGNED_SHORT_4_4_4_4";
  ty[IT_UNSIGNED_SHORT_4_4_4_4_REV] = "IT_UNSIGNED_SHORT_4_4_4_4_REV";
  ty[IT_UNSIGNED_SHORT_5_5_5_1] = "IT_UNSIGNED_SHORT_5_5_5_1";
  ty[IT_UNSIGNED_SHORT_1_5_5_5_REV] = "IT_UNSIGNED_SHORT_1_5_5_5_REV";
  ty[IT_UNSIGNED_INT_8_8_8_8] = "IT_UNSIGNED_INT_8_8_8_8";
  ty[IT_UNSIGNED_INT_8_8_8_8_REV] = "IT_UNSIGNED_INT_8_8_8_8_REV";
  ty[IT_UNSIGNED_INT_10_10_10_2] = "IT_UNSIGNED_INT_10_10_10_2";
  ty[IT_UNSIGNED_INT_2_10_10_10_REV] = "IT_UNSIGNED_INT_2_10_10_10_REV";
  ty[IT_UNSIGNED_INT_5_9_9_9_REV] = "IT_UNSIGNED_INT_5_9_9_9_REV";
  ty[IT_UNSIGNED_INT_10F_11F_11F_REV] = "IT_UNSIGNED_INT_10F_11F_11F_REV";
  ty[IT_UNSIGNED_INT_24_8] = "IT_UNSIGNED_INT_24_8";
  ty[IT_FLOAT_32_UNSIGNED_INT_24_8_REV] = "IT_FLOAT_32_UNSIGNED_INT_24_8_REV";

  VL_CHECK(ty[type()] != NULL)

  return ty[type()];
}
//-----------------------------------------------------------------------------
String Image::printFormat() const
{
  std::map<int, const char*> fo;

  fo[IF_RGB] = "IF_RGB";
  fo[IF_RGBA] = "IF_RGBA";
  fo[IF_BGR] = "IF_BGR";
  fo[IF_BGRA] = "IF_BGRA";
  fo[IF_RED] = "IF_RED";
  fo[IF_GREEN] = "IF_GREEN";
  fo[IF_BLUE] = "IF_BLUE";
  fo[IF_ALPHA] = "IF_ALPHA";
  fo[IF_LUMINANCE] = "IF_LUMINANCE";
  fo[IF_LUMINANCE_ALPHA] = "IF_LUMINANCE_ALPHA";
  fo[IF_DEPTH_COMPONENT] = "IF_DEPTH_COMPONENT";
  fo[IF_STENCIL_INDEX] = "IF_STENCIL_INDEX";
  fo[IF_DEPTH_STENCIL] = "IF_DEPTH_STENCIL";
  fo[IF_COMPRESSED_RGB_S3TC_DXT1] = "IF_COMPRESSED_RGB_S3TC_DXT1";
  fo[IF_COMPRESSED_RGBA_S3TC_DXT1] = "IF_COMPRESSED_RGBA_S3TC_DXT1";
  fo[IF_COMPRESSED_RGBA_S3TC_DXT3] = "IF_COMPRESSED_RGBA_S3TC_DXT3";
  fo[IF_COMPRESSED_RGBA_S3TC_DXT5] = "IF_COMPRESSED_RGBA_S3TC_DXT5";

  VL_CHECK( fo[format()] != NULL );

  return fo[format()];
}
//-----------------------------------------------------------------------------
String Image::print() const
{
  return Say(
  "name   = %s\n"
  "width  = %n\n"
  "height = %n\n"
  "depth  = %n\n"
  "format = %s\n"
  "type   = %s\n"
  "pitch  = %n\n"
  "bytealign = %n\n"
  )
  << objectName().c_str()
  << width()
  << height()
  << depth()
  << printFormat()
  << printType()
  << pitch()
  << byteAlignment();
}
//-----------------------------------------------------------------------------
EImageDimension Image::dimension() const
{
  if(mWidth > 0 && mHeight == 0 && mDepth == 0 && !mIsCubemap) return ID_1D;
  if(mWidth > 0 && mHeight > 0  && mDepth == 0 && !mIsCubemap) return ID_2D;
  if(mWidth > 0 && mHeight > 0  && mDepth >  0 && !mIsCubemap) return ID_3D;
  if(mWidth > 0 && mHeight > 0  && mDepth == 0 &&  mIsCubemap) return ID_Cubemap;
  return ID_Error;
}
//-----------------------------------------------------------------------------
//! Returns the number of bits used to represents one pixel.
int Image::bitsPerPixel(EImageType type, EImageFormat format)
{
  int comp_size = 0;

  switch(type)
  {
    default:
    break;

    case IT_UNSIGNED_BYTE:  comp_size = sizeof(unsigned char)  * 8; break;
    case IT_BYTE:           comp_size = sizeof(GLbyte)   * 8; break;
    case IT_UNSIGNED_SHORT: comp_size = sizeof(GLushort) * 8; break;
    case IT_SHORT:          comp_size = sizeof(GLshort)  * 8; break;
    case IT_UNSIGNED_INT:   comp_size = sizeof(unsigned int)   * 8; break;
    case IT_INT:            comp_size = sizeof(int)    * 8; break;
    case IT_FLOAT:          comp_size = sizeof(float)  * 8; break;

    case IT_UNSIGNED_BYTE_3_3_2:          return 8;
    case IT_UNSIGNED_BYTE_2_3_3_REV:      return 8;
    case IT_UNSIGNED_SHORT_5_6_5:         return 16;
    case IT_UNSIGNED_SHORT_5_6_5_REV:     return 16;
    case IT_UNSIGNED_SHORT_4_4_4_4:       return 16;
    case IT_UNSIGNED_SHORT_4_4_4_4_REV:   return 16;
    case IT_UNSIGNED_SHORT_5_5_5_1:       return 16;
    case IT_UNSIGNED_SHORT_1_5_5_5_REV:   return 16;
    case IT_UNSIGNED_INT_8_8_8_8:         return 32;
    case IT_UNSIGNED_INT_8_8_8_8_REV:     return 32;
    case IT_UNSIGNED_INT_10_10_10_2:      return 32;
    case IT_UNSIGNED_INT_2_10_10_10_REV:  return 32;
    case IT_UNSIGNED_INT_5_9_9_9_REV:     return 32; /* EXT_texture_shared_exponent, support only IF_RGB */
    case IT_UNSIGNED_INT_10F_11F_11F_REV: return 32; /* EXT_packed_float, supports only IF_RGB */
    case IT_UNSIGNED_INT_24_8:            return 32; /* EXT_packed_depth_stencil, supports only IF_DEPTH_STENCIL */
    case IT_FLOAT_32_UNSIGNED_INT_24_8_REV: return 64; /* EXT_depth_buffer_float, supports only IF_DEPTH_STENCIL */
  }

  switch(format)
  {
    case IF_RED:             return comp_size * 1;
    case IF_GREEN:           return comp_size * 1;
    case IF_BLUE:            return comp_size * 1;
    case IF_ALPHA:           return comp_size * 1;
    case IF_DEPTH_COMPONENT: return comp_size * 1;
    case IF_STENCIL_INDEX:   return comp_size * 1;
    case IF_LUMINANCE:       return comp_size * 1;
    case IF_LUMINANCE_ALPHA: return comp_size * 2;
    case IF_DEPTH_STENCIL:   return comp_size * 0;
    case IF_RGB:             return comp_size * 3;
    case IF_BGR:             return comp_size * 3;
    case IF_RGBA:            return comp_size * 4;
    case IF_BGRA:            return comp_size * 4;

     // compressed formats

    case IF_COMPRESSED_RGB_S3TC_DXT1:  return 4; // 8 bytes (64 bits) per block per 16 pixels
    case IF_COMPRESSED_RGBA_S3TC_DXT1: return 4; // 8 bytes (64 bits) per block per 16 pixels
    case IF_COMPRESSED_RGBA_S3TC_DXT3: return 8; // 16 bytes (128 bits) per block per 16 pixels
    case IF_COMPRESSED_RGBA_S3TC_DXT5: return 8; // 16 bytes (128 bits) per block per 16 pixels
    default:
      break;
  }

  VL_TRAP()
  return 0;
}
//-----------------------------------------------------------------------------
//! Useful to know if the image format supports alpha or not.
int Image::alphaBits() const
{
  int comp_size = 0;

  switch(format())
  {
    case IF_RED:             return comp_size * 0;
    case IF_GREEN:           return comp_size * 0;
    case IF_BLUE:            return comp_size * 0;
    case IF_ALPHA:           return comp_size * 1;
    case IF_DEPTH_COMPONENT: return comp_size * 0;
    case IF_STENCIL_INDEX:   return comp_size * 0;
    case IF_LUMINANCE:       return comp_size * 0;
    case IF_LUMINANCE_ALPHA: return comp_size * 1;
    case IF_DEPTH_STENCIL:   return comp_size * 0;
    case IF_RGB:             return comp_size * 0;
    case IF_BGR:             return comp_size * 0;
    case IF_RGBA:            return comp_size * 1;
    case IF_BGRA:            return comp_size * 1;

     // compressed formats

    case IF_COMPRESSED_RGB_S3TC_DXT1:  return 0; // 8 bytes (64 bits) per block per 16 pixels
    case IF_COMPRESSED_RGBA_S3TC_DXT1: return 1; // 8 bytes (64 bits) per block per 16 pixels
    case IF_COMPRESSED_RGBA_S3TC_DXT3: return 4; // 16 bytes (64 bits for uncompressed alpha + 64 bits for RGB) per block per 16 pixels
    case IF_COMPRESSED_RGBA_S3TC_DXT5: return 4; // 16 bytes (64 bits for   compressed alpha + 64 bits for RGB) per block per 16 pixels
    default:
      break;
  }

  switch(type())
  {
    default:
    break;

    case IT_UNSIGNED_BYTE:  comp_size = sizeof(unsigned char)  * 8; break;
    case IT_BYTE:           comp_size = sizeof(GLbyte)   * 8; break;
    case IT_UNSIGNED_SHORT: comp_size = sizeof(GLushort) * 8; break;
    case IT_SHORT:          comp_size = sizeof(GLshort)  * 8; break;
    case IT_UNSIGNED_INT:   comp_size = sizeof(unsigned int)   * 8; break;
    case IT_INT:            comp_size = sizeof(int)    * 8; break;
    case IT_FLOAT:          comp_size = sizeof(float)  * 8; break;

    case IT_UNSIGNED_BYTE_3_3_2:          return 0;
    case IT_UNSIGNED_BYTE_2_3_3_REV:      return 0;
    case IT_UNSIGNED_SHORT_5_6_5:         return 0;
    case IT_UNSIGNED_SHORT_5_6_5_REV:     return 0;
    case IT_UNSIGNED_SHORT_4_4_4_4:       return 4;
    case IT_UNSIGNED_SHORT_4_4_4_4_REV:   return 4;
    case IT_UNSIGNED_SHORT_5_5_5_1:       return 1;
    case IT_UNSIGNED_SHORT_1_5_5_5_REV:   return 1;
    case IT_UNSIGNED_INT_8_8_8_8:         return 8;
    case IT_UNSIGNED_INT_8_8_8_8_REV:     return 8;
    case IT_UNSIGNED_INT_10_10_10_2:      return 2;
    case IT_UNSIGNED_INT_2_10_10_10_REV:  return 2;
    case IT_UNSIGNED_INT_5_9_9_9_REV:     return 0; /* EXT_texture_shared_exponent, support only GL_RGB */
    case IT_UNSIGNED_INT_10F_11F_11F_REV: return 0; /* EXT_packed_float, supports only GL_RGB */
    case IT_UNSIGNED_INT_24_8:            return 0; /* EXT_packed_depth_stencil, supports only GL_DEPTH_STENCIL */
    case IT_FLOAT_32_UNSIGNED_INT_24_8_REV: return 0; /* EXT_depth_buffer_float, supports only IF_DEPTH_STENCIL */
  }

  VL_TRAP()

  return 0;
}
//-----------------------------------------------------------------------------
int Image::isCompressedFormat(EImageFormat fmt)
{
  switch(fmt)
  {
  case IF_COMPRESSED_RGB_S3TC_DXT1:
  case IF_COMPRESSED_RGBA_S3TC_DXT1:
  case IF_COMPRESSED_RGBA_S3TC_DXT3:
  case IF_COMPRESSED_RGBA_S3TC_DXT5:
    return true;

  default:
    return false;
  }
}
//-----------------------------------------------------------------------------
//! Returns the number of bytes requested to store the image.
//! Doesn't take into consideration mipmaps.
int Image::requiredMemory() const
{
  VL_CHECK( isValid() )
  return requiredMemory(width(), height(), depth(), byteAlignment(), format(), type(), isCubemap());
}
//-----------------------------------------------------------------------------
//! Returns the byte-alignment of the row of the image.
//! Possible return values are 1, 2, 4 and 8.
int Image::byteAlignment() const
{
  return mByteAlign;
}
//-----------------------------------------------------------------------------
//! Modifies the byte-alignment of the rows of the image, thus changing its pitch.
//! Only values like 0, 1, 2, 4 and 8 are allowed. If 0 is passed the byte-alignment
//! takes the value returned by "sizeof(unsigned char*)".
//! \remarks This function must be called before the allocate() function or in any
//! case on a non allocated image otherwise it won't have any effect.
void Image::setByteAlignment(int bytealign)
{
  // cannot change the alignment when an image is already allocated

  VL_CHECK(mPixels->empty());

  if (!mPixels->empty())
    return;

  switch(bytealign)
  {
  case 0:
    bytealign = sizeof(unsigned char*);
  case 1:
  case 2:
  case 4:
  case 8:
    break;
  default:
    VL_TRAP()
  }

  mByteAlign = bytealign;

  updatePitch();
}
//-----------------------------------------------------------------------------
void Image::updatePitch()
{
  int xbits  = mWidth * bitsPerPixel();
  int xbytes = xbits/8 + ( (xbits % 8) ? 1 : 0 );
  mPitch     = xbytes/mByteAlign*mByteAlign + ( (xbytes % mByteAlign) ? mByteAlign : 0 );
}
//-----------------------------------------------------------------------------
void Image::allocateCubemap(int x, int y, int bytealign, EImageFormat format, EImageType type)
{
  reset();

  setWidth(x);
  setHeight(y);
  setDepth(0);
  setFormat(format);
  setType(type);
  setByteAlignment(bytealign);
  mIsCubemap = true;

  // mPixels.clear();
  mPixels->resize(requiredMemory());
}
//-----------------------------------------------------------------------------
void Image::allocate()
{
  mMipmaps.clear();
  mPixels->resize(requiredMemory());
}
//-----------------------------------------------------------------------------
void Image::allocate1D(int x, EImageFormat format, EImageType type)
{
  reset();

  VL_CHECK(x);
  setWidth(x);
  setHeight(0);
  setDepth(0);
  setFormat(format);
  setType(type);
  setByteAlignment(1);
  mIsCubemap = false;

  mPixels->resize(requiredMemory());
}
//-----------------------------------------------------------------------------
void Image::allocate2D(int x, int y, int bytealign, EImageFormat format, EImageType type)
{
  reset();

  VL_CHECK(x);
  VL_CHECK(y);
  setWidth(x);
  setHeight(y);
  setDepth(0);
  setFormat(format);
  setType(type);
  setByteAlignment(bytealign);
  mIsCubemap = false;

  int req_mem = requiredMemory();
  if (req_mem == 0)
    Log::bug("Image::allocate2D could not allocate memory, probably your image settings are invalid.\n");
  mPixels->resize(req_mem);
}
//-----------------------------------------------------------------------------
void Image::allocate3D(int x, int y, int z, int bytealign, EImageFormat format, EImageType type)
{
  reset();

  VL_CHECK(x);
  VL_CHECK(y);
  VL_CHECK(z);
  setWidth(x);
  setHeight(y);
  setDepth(z);
  setFormat(format);
  setType(type);
  setByteAlignment(bytealign);
  mIsCubemap = false;

  mPixels->resize(requiredMemory());
}
//-----------------------------------------------------------------------------
void Image::reset(int x, int y, int z, int bytealign, EImageFormat format, EImageType type, bool is_cubemap)
{
  reset();

  setWidth(x);
  setHeight(y);
  setDepth(z);
  setFormat(format);
  setType(type);
  setByteAlignment(bytealign);
  mIsCubemap = is_cubemap;
}
//-----------------------------------------------------------------------------
void Image::reset()
{
  mPixels->clear();
  mMipmaps.clear();
  mObjectName.clear();
  mWidth = 0;
  mHeight = 0;
  mDepth = 0;
  mPitch = 0;
  mFormat = IF_RGBA;
  mType = IT_UNSIGNED_BYTE;
  mByteAlign = 1;
  mIsCubemap   = false;
  mIsNormalMap = false;
  mHasAlpha    = false;
}
//-----------------------------------------------------------------------------
Image& Image::operator=(const Image& other)
{
  super::operator=(other);

  // deep copy of the pixels
  *mPixels = *other.mPixels;

  // copy image info
  mWidth  = other.mWidth;
  mHeight  = other.mHeight;
  mDepth  = other.mDepth;
  mPitch  = other.mPitch;
  mByteAlign = other.mByteAlign;
  mFormat    = other.mFormat;
  mType      = other.mType;
  mIsCubemap = other.mIsCubemap;
  mIsNormalMap = other.mIsNormalMap;
  mHasAlpha    = other.mHasAlpha;

  // deep copy of the mipmaps
  mMipmaps.resize(other.mMipmaps.size());
  for(int i=0; i<(int)mMipmaps.size(); ++i)
  {
    mMipmaps[i] = new Image;
    *mMipmaps[i] = *other.mMipmaps[i];
  }
  return *this;
}
//-----------------------------------------------------------------------------
const unsigned char* Image::pixelsXP() const
{
  VL_CHECK( dimension() == 4 )
  if( dimension() != 4 || !pixels())
    return NULL;
  else
    return pixels();
}
//-----------------------------------------------------------------------------
unsigned char* Image::pixelsXP()
{
  VL_CHECK( dimension() == 4 )
  if( dimension() != 4 || !pixels())
    return NULL;
  else
    return pixels();
}
//-----------------------------------------------------------------------------
const unsigned char* Image::pixelsXN() const
{
  VL_CHECK( dimension() == 4 )
  if( dimension() != 4 || !pixels())
    return NULL;
  else
    return (unsigned char*)pixels() + requiredMemory2D( width(), height(), byteAlignment(), format(), type() ) * 1;
}
//-----------------------------------------------------------------------------
unsigned char* Image::pixelsXN()
{
  VL_CHECK( dimension() == 4 )
  if( dimension() != 4 || !pixels())
    return NULL;
  else
    return (unsigned char*)pixels() + requiredMemory2D( width(), height(), byteAlignment(), format(), type() ) * 1;
}
//-----------------------------------------------------------------------------
const unsigned char* Image::pixelsYP() const
{
  VL_CHECK( dimension() == 4 )
  if( dimension() != 4 || !pixels())
    return NULL;
  else
    return (unsigned char*)pixels() + requiredMemory2D( width(), height(), byteAlignment(), format(), type() ) * 2;
}
//-----------------------------------------------------------------------------
unsigned char* Image::pixelsYP()
{
  VL_CHECK( dimension() == 4 )
  if( dimension() != 4 || !pixels())
    return NULL;
  else
    return (unsigned char*)pixels() + requiredMemory2D( width(), height(), byteAlignment(), format(), type() ) * 2;
}
//-----------------------------------------------------------------------------
const unsigned char* Image::pixelsYN() const
{
  VL_CHECK( dimension() == 4 )
  if( dimension() != 4 || !pixels())
    return NULL;
  else
    return (unsigned char*)pixels() + requiredMemory2D( width(), height(), byteAlignment(), format(), type() ) * 3;
}
//-----------------------------------------------------------------------------
unsigned char* Image::pixelsYN()
{
  VL_CHECK( dimension() == 4 )
  if( dimension() != 4 || !pixels())
    return NULL;
  else
    return (unsigned char*)pixels() + requiredMemory2D( width(), height(), byteAlignment(), format(), type() ) * 3;
}
//-----------------------------------------------------------------------------
const unsigned char* Image::pixelsZP() const
{
  VL_CHECK( dimension() == 4 )
  if( dimension() != 4 || !pixels())
    return NULL;
  else
    return (unsigned char*)pixels() + requiredMemory2D( width(), height(), byteAlignment(), format(), type() ) * 4;
}
//-----------------------------------------------------------------------------
unsigned char* Image::pixelsZP()
{
  VL_CHECK( dimension() == 4 )
  if( dimension() != 4 || !pixels())
    return NULL;
  else
    return (unsigned char*)pixels() + requiredMemory2D( width(), height(), byteAlignment(), format(), type() ) * 4;
}
//-----------------------------------------------------------------------------
const unsigned char* Image::pixelsZN() const
{
  VL_CHECK( dimension() == 4 )
  if( dimension() != 4 || !pixels())
    return NULL;
  else
    return (unsigned char*)pixels() + requiredMemory2D( width(), height(), byteAlignment(), format(), type() ) * 5;
}
//-----------------------------------------------------------------------------
unsigned char* Image::pixelsZN()
{
  VL_CHECK( dimension() == 4 )
  if( dimension() != 4 || !pixels())
    return NULL;
  else
    return (unsigned char*)pixels() + requiredMemory2D( width(), height(), byteAlignment(), format(), type() ) * 5;
}
//-----------------------------------------------------------------------------
//! Returns the pixels of the specified Z slice of a 3D image
unsigned char* Image::pixelsZSlice(int slice)
{
  VL_CHECK(slice < depth());
  VL_CHECK(slice >= 0 );
  if (mIsCubemap || !pixels())
    return NULL;
  else
  {
    return (unsigned char*)pixels() + pitch()*height()*slice;
  }
}
//-----------------------------------------------------------------------------
ref<Image> vl::createCubemap(const Image* xp, const Image* xn, const Image* yp, const Image* yn, const Image* zp, const Image* zn)
{
  // check validity of all the images
  // contract:
  // - they must be all isValid() images, must have been allocated, must be 2D images, must
  // - have exactly the same dimensions, format, type, byte alignment.
  // - they must be square

  const Image* img[] = {xp, xn, yp, yn, zp, zn};

  // check they are square (later we check they have the same size)
  if (img[0]->width() != img[0]->height())
  {
    Log::error("Cubemap creation failed: all the images must be square.\n");
    return NULL;
  }

  for(int i=0; i<6; ++i)
  {
    if (img[i] == NULL || !img[i]->isValid() || img[i]->pixels() == NULL || img[i]->dimension() != 2)
    {
      Log::error("Cubemap creation failed: one or more image is invalid (could be NULL, not allocated, not 2D, wrong internal_ configuration or other).\n");
      return NULL;
    }

    // check that they have the same size
    if (img[0]->width() != img[i]->width())
    {
      Log::error("Cubemap creation failed: the faces of the cube must have the very same dimensions.\n");
      return NULL;
    }

    // check that they have the same size
    if (img[0]->height() != img[i]->height())
    {
      Log::error("Cubemap creation failed: the faces of the cube must have the very same dimensions.\n");
      return NULL;
    }

    // check that they have the same format
    if (img[0]->format() != img[i]->format())
    {
      Log::error("Cubemap creation failed: the faces of the cube must have the very same format.\n");
      return NULL;
    }

    // check that they have the same type
    if (img[0]->type() != img[i]->type())
    {
      Log::error("Cubemap creation failed: the faces of the cube must have the very same type.\n");
      return NULL;
    }

    // check that they have the same byte alignment
    if (img[0]->byteAlignment() != img[i]->byteAlignment())
    {
      Log::error("Cubemap creation failed: the faces of the cube must have the very same byte alignment.\n");
      return NULL;
    }

    // check that they have the same required memory
    if (img[0]->requiredMemory() != img[i]->requiredMemory())
    {
      Log::error("Cubemap creation failed: the faces of the cube must require the very same amount of memory.\n");
      return NULL;
    }
  }

  // create a cube map image from the given 2D images

  ref<Image> cubemap = new Image;

  cubemap->allocateCubemap( img[0]->width(), img[0]->height(), img[0]->byteAlignment(), img[0]->format(), img[0]->type() );

  memcpy( cubemap->pixelsXP(), img[0]->pixels(), img[0]->requiredMemory() );
  memcpy( cubemap->pixelsXN(), img[1]->pixels(), img[0]->requiredMemory() );
  memcpy( cubemap->pixelsYP(), img[2]->pixels(), img[0]->requiredMemory() );
  memcpy( cubemap->pixelsYN(), img[3]->pixels(), img[0]->requiredMemory() );
  memcpy( cubemap->pixelsZP(), img[4]->pixels(), img[0]->requiredMemory() );
  memcpy( cubemap->pixelsZN(), img[5]->pixels(), img[0]->requiredMemory() );

  return cubemap.get();
}
//-----------------------------------------------------------------------------
void Image::flipVertically()
{
  if (dimension() == ID_1D)
    return;

  VL_CHECK(pixels());
  int row_size = pitch();
  std::vector<unsigned char> row1;
  row1.resize(row_size);

  std::vector<unsigned char*> pxl;

  if (dimension() == ID_2D)
  {
    pxl.push_back( (unsigned char*)pixels() );
  }
  else
  if (dimension() == ID_Cubemap)
  {
    pxl.push_back( (unsigned char*)pixelsXP() );
    pxl.push_back( (unsigned char*)pixelsXN() );
    pxl.push_back( (unsigned char*)pixelsYP() );
    pxl.push_back( (unsigned char*)pixelsYN() );
    pxl.push_back( (unsigned char*)pixelsZP() );
    pxl.push_back( (unsigned char*)pixelsZN() );
  }
  else
  if (dimension() == ID_3D)
  {
    for(int zslice=0; zslice<depth(); zslice++)
      pxl.push_back( (unsigned char*)pixelsZSlice(zslice) );
  }

  for(unsigned img=0; img<pxl.size(); img++)
  {
    for(int i=0; i<height()/2; ++i)
    {
      int j = height() - 1 - i;
      memcpy(&row1[0], pxl[img]+i*row_size, row_size);
      memcpy(pxl[img]+i*row_size, pxl[img]+j*row_size, row_size);
      memcpy(pxl[img]+j*row_size,  &row1[0], row_size);
    }
  }
}
//-----------------------------------------------------------------------------
int Image::requiredMemory(int width, int height, int depth, int bytealign, EImageFormat format, EImageType type, bool is_cubemap)
{
  // byte align

  switch(bytealign)
  {
  case 1:
  case 2:
  case 4:
  case 8:
    break;
  case 0:
  default:
    bytealign = sizeof(unsigned char*);
  }

  // fix width and height to match compression scheme

  switch(format)
  {
    case IF_COMPRESSED_RGB_S3TC_DXT1:
    case IF_COMPRESSED_RGBA_S3TC_DXT1:
    case IF_COMPRESSED_RGBA_S3TC_DXT3:
    case IF_COMPRESSED_RGBA_S3TC_DXT5:
      if (width % 4)
        width = width - width % 4 + 4;
      if (height % 4)
        height = height - height % 4 + 4;
    default:
      break;
  }

  // pitch

  int xbits = width * bitsPerPixel(type, format);
  int xbytes = xbits/8 + ( (xbits % 8) ? 1 : 0 );
  int pitch  = xbytes/bytealign*bytealign + ( (xbytes % bytealign) ? bytealign : 0 );

  // computation

  height = height ? height : 1;
  depth  = depth  ? depth  : 1;
  int req_mem = pitch * height * depth;

  // minimum memory taken by a compressed block
  if (req_mem < 8 && format == IF_COMPRESSED_RGB_S3TC_DXT1)
    req_mem = 8;

  if (req_mem < 8 && format == IF_COMPRESSED_RGBA_S3TC_DXT1)
    req_mem = 8;

  if (req_mem < 16 && format == IF_COMPRESSED_RGBA_S3TC_DXT3)
    req_mem = 16;

  if (req_mem < 16 && format == IF_COMPRESSED_RGBA_S3TC_DXT5)
    req_mem = 16;

  // todo: add other compression schemes
  // ...

  // cubemap
  if (is_cubemap)
    req_mem *= 6;

  return req_mem;
}
//-----------------------------------------------------------------------------
ref<Image> vl::makeColorSpectrum(size_t width, const fvec4& c0, const fvec4& c1)
{
  std::vector<fvec4> colors;
  colors.push_back(c0); colors.push_back(c1);
  return makeColorSpectrum(width, colors);
}
//-----------------------------------------------------------------------------
ref<Image> vl::makeColorSpectrum(size_t width, const fvec4& c0, const fvec4& c1, const fvec4& c2)
{
  std::vector<fvec4> colors;
  colors.push_back(c0); colors.push_back(c1); colors.push_back(c2);
  return makeColorSpectrum(width, colors);
}
//-----------------------------------------------------------------------------
ref<Image> vl::makeColorSpectrum(size_t width, const fvec4& c0, const fvec4& c1, const fvec4& c2, const fvec4& c3)
{
  std::vector<fvec4> colors;
  colors.push_back(c0); colors.push_back(c1); colors.push_back(c2); colors.push_back(c3);
  return makeColorSpectrum(width, colors);
}
//-----------------------------------------------------------------------------
ref<Image> vl::makeColorSpectrum(size_t width, const fvec4& c0, const fvec4& c1, const fvec4& c2, const fvec4& c3, const fvec4& c4)
{
  std::vector<fvec4> colors;
  colors.push_back(c0); colors.push_back(c1); colors.push_back(c2); colors.push_back(c3); colors.push_back(c4);
  return makeColorSpectrum(width, colors);
}
//-----------------------------------------------------------------------------
ref<Image> vl::makeNonUniformColorSpectrum(size_t width, size_t col_count, const fvec4* colors, const float* col_pos)
{
  //     c0    c1    c2    c4
  // |---t0----t1----t2----t4---|
  // before t0 the color is pure c0
  // after t4 the color is pure t4

  ref<Image> img = new Image(width, 0, 0, 1, IF_RGBA, IT_UNSIGNED_BYTE);
  ubvec4* px = (ubvec4*)img->pixels();
  int last = col_count-1;
  for(int i=0; i<img->width(); ++i)
  {
    float t = (float)i/(img->width()-1);
    if (t<=col_pos[0])
      px[i] = (ubvec4)(colors[0]*255.0f);
    else
    if (t>=col_pos[last])
      px[i] = (ubvec4)(colors[last]*255.0f);
    else
    {
      for(size_t j=0; j<col_count-1; ++j)
      {
        if (t>=col_pos[j] && t<=col_pos[j+1])
        {
          float tt = 0;
          if (col_pos[j+1]-col_pos[j] != 0)
            tt = (t-col_pos[j])/(col_pos[j+1]-col_pos[j]);
          VL_CHECK(tt>=0 && tt<=1.0f)
          px[i] = (ubvec4)((colors[j] * (1.0f-tt) + colors[j+1] * tt)*255.0f);
          break;
        }
      }
    }
  }
  return img;
}
//-----------------------------------------------------------------------------
ref<Image> vl::makeNonUniformColorSpectrum(int width, const std::vector<fvec4>& colors, const std::vector<float>& col_pos)
{
  if (colors.empty() || colors.size() != col_pos.size())
    return NULL;
  else
    return makeNonUniformColorSpectrum(width, colors.size(), &colors[0], &col_pos[0]);
}
//-----------------------------------------------------------------------------
ref<Image> vl::makeColorSpectrum(size_t width, const std::vector<fvec4>& colors)
{
  ref<Image> img = new Image(width, 0, 0, 1, IF_RGBA, IT_UNSIGNED_BYTE);
  int index = colors.size() - 1;
  for(int i=0; i<img->width(); ++i)
  {
    int   coli = (int)(index * (float)i/img->width());
    float colt = (index * (float)i/img->width()) - coli;
    img->pixels()[i*4 + 0] = (unsigned char)(255.0 * (colors[coli].r()*(1.0f-colt) + colors[coli+1].r()*colt));
    img->pixels()[i*4 + 1] = (unsigned char)(255.0 * (colors[coli].g()*(1.0f-colt) + colors[coli+1].g()*colt));
    img->pixels()[i*4 + 2] = (unsigned char)(255.0 * (colors[coli].b()*(1.0f-colt) + colors[coli+1].b()*colt));
    img->pixels()[i*4 + 3] = 255;
  }
  return img;
}
//-----------------------------------------------------------------------------
//! The order of the given images is important: the first image will be used for z layer #0, the second for z layer #1 and so on.
ref<Image> vl::assemble3DImage(const std::vector< ref<Image> >& images)
{
  if (images.empty())
    return NULL;
  // sanity checks
  for(unsigned i=1; i<images.size(); ++i)
  {
    if (images[i]->width()  != images[0]->width())  return NULL;
    if (images[i]->height() != images[0]->height()) return NULL;
    if (images[i]->depth()  != images[0]->depth())  return NULL;
    if (images[i]->type()   != images[0]->type())   return NULL;
    if (images[i]->format() != images[0]->format()) return NULL;
    if (images[i]->byteAlignment()  != images[0]->byteAlignment())  return NULL;
    if (images[i]->bitsPerPixel()   != images[0]->bitsPerPixel())   return NULL;
    if (images[i]->requiredMemory() != images[0]->requiredMemory()) return NULL;
  }

  ref<Image> img = new Image;
  img->allocate3D( images[0]->width(), images[0]->height(), images.size(), 1, images[0]->format(), images[0]->type() );
  VL_CHECK(img->requiredMemory() == images[0]->requiredMemory()*(int)images.size())
  for(unsigned i=0; i<images.size(); ++i)
  {
    VL_CHECK(images[i]->pixels())
    memcpy(img->pixelsZSlice(i), images[i]->pixels(), images[i]->requiredMemory());
  }
  return img;
}
//-----------------------------------------------------------------------------
bool vl::loadImagesFromDir(const String& dir_path, const String& ext, std::vector< ref<Image> >& images)
{
  images.clear();
  if (ext.empty() || dir_path.empty())
    return false;
  ref<VirtualDirectory> dir = defFileSystem()->locateDirectory(dir_path);
  if (!dir)
    return false;
  std::vector<String> files;
  dir->listFiles(files);
  std::sort(files.begin(), files.end());
  for(unsigned i=0; i<files.size(); ++i)
  {
    if (files[i].extractFileExtension().toLowerCase() == ext.toLowerCase())
    {
      images.push_back( loadImage(files[i]) );
      if (images.back().get() == NULL)
        return false;
    }
  }
  return true;
}
//-----------------------------------------------------------------------------
ref<Image> vl::loadImage( const String& path )
{
  ref<VirtualFile> file = defFileSystem()->locateFile(path);
  if ( !file )
  {
    Log::error( Say("File '%s' not found.\n") << path );
    return NULL;
  }
  else
    return loadImage(file.get());
}
//-----------------------------------------------------------------------------
ref<Image> vl::loadImage( VirtualFile* file )
{
  ref<ResourceDatabase> res_db = defLoadWriterManager()->loadResource(file);

  if (!res_db)
  {
    Log::error( Say("vl::loadImage('%s') failed.\n") << file->path() );
    return NULL;
  }

  ref<Image> img;

  img = res_db->get<Image>(0);

  VL_CHECK( !file->isOpen() )
  file->close();

  if (img)
  {
    img->setObjectName( file->path().toStdString().c_str() );
    img->setFilePath( file->path() );
  }

  return img;
}
//-----------------------------------------------------------------------------
bool vl::saveImage( Image* img, const String& path)
{
  ref<ResourceDatabase> res_db = new ResourceDatabase;
  res_db->resources().push_back(img);
  bool ok = defLoadWriterManager()->writeResource(path, res_db.get());
  if (!ok)
    Log::error( Say("vl::saveImage('%s') failed.\n") << path );
  return ok;
}
//-----------------------------------------------------------------------------
bool vl::saveImage( Image* img, VirtualFile* file )
{
  ref<ResourceDatabase> res_db = new ResourceDatabase;
  res_db->resources().push_back(img);
  bool ok = defLoadWriterManager()->writeResource(file, res_db.get());
  if (!ok)
    Log::error( Say("vl::saveImage('%s') failed.\n") << file->path() );
  return ok;
}
//-----------------------------------------------------------------------------
ref<Image> vl::loadCubemap(const String& xp_file, const String& xn_file, const String& yp_file, const String& yn_file, const String& zp_file, const String& zn_file)
{
  ref<Image> xp = loadImage(xp_file);
  ref<Image> xn = loadImage(xn_file);
  ref<Image> yp = loadImage(yp_file);
  ref<Image> yn = loadImage(yn_file);
  ref<Image> zp = loadImage(zp_file);
  ref<Image> zn = loadImage(zn_file);

  if (!xp || !xn || !yp || !yn || !zp || !zn)
  {
    Log::error("vl::loadCubemap() failed.\n");
    return NULL;
  }

  ref<Image> img = vl::createCubemap(xp.get(), xn.get(), yp.get(), yn.get(), zp.get(), zn.get());  

  return img;
}
//-----------------------------------------------------------------------------
ref<Image> vl::loadRAW(VirtualFile* file, long long file_offset, int width, int height, int depth, int bytealign, EImageFormat format, EImageType type)
{
  ref<Image> img = new Image(width, height, depth, bytealign, format, type);
  if ( file->isOpen() || file->open(OM_ReadOnly) )
  {
    bool ok = file_offset == -1 || file->seekSet(file_offset);
    if (!ok)
    {
      Log::error( Say("loadRAW('%s'): seek set to position %n failed.\n") << file_offset );
      return NULL;
    }
    int count = (int)file->read( img->pixels(), img->requiredMemory() );
    if (count != img->requiredMemory())
      Log::error( Say("loadRAW('%s'): error reading RAW file.\n") << file->path() );
    return img;
  }
  else
  {
    Log::error( Say("loadRAW('%s'): could not open file for reading.\n") << file->path() );
    return NULL;
  }
}
//-----------------------------------------------------------------------------
ref<Image> vl::Image::convertType(EImageType new_type) const
{
  switch(type())
  {
    case IT_UNSIGNED_BYTE:
    case IT_BYTE:         
    case IT_UNSIGNED_SHORT:
    case IT_SHORT:         
    case IT_UNSIGNED_INT:  
    case IT_INT:           
    case IT_FLOAT:
      break;
    default:
      Log::error("Image::convertType(): unsupported source image type.\n");
      return NULL;
  }

  switch(new_type)
  {
    case IT_UNSIGNED_BYTE:
    case IT_BYTE:         
    case IT_UNSIGNED_SHORT:
    case IT_SHORT:         
    case IT_UNSIGNED_INT:  
    case IT_INT:           
    case IT_FLOAT:
      break;
    default:
      Log::error("Image::convertType(): unsupported destination image type.\n");
      return NULL;
  }

  switch(format())
  {
    case IF_RGB:
    case IF_RGBA:
    case IF_BGR:
    case IF_BGRA:
    case IF_RED:
    case IF_GREEN:
    case IF_BLUE:
    case IF_ALPHA:
    case IF_LUMINANCE:
    case IF_LUMINANCE_ALPHA:
    case IF_DEPTH_COMPONENT:
      break;
    default:
      Log::error("Image::convertType(): unsupported image format.\n");
      return NULL;
  }

  ref<Image> img = new Image;
  img->setObjectName( objectName().c_str() );
  img->setFormat(format());
  img->setType(new_type);
  img->setWidth(width());
  img->setHeight(height());
  img->setDepth(depth());
  img->setByteAlignment(1);
  img->mIsCubemap = isCubemap();
  img->mIsNormalMap = mIsNormalMap;
  img->mHasAlpha    = mHasAlpha;
  img->allocate();

  int components = 0;
  switch(format())
  {
    case IF_RGB:   components = 3; break;
    case IF_RGBA:  components = 4; break;
    case IF_BGR:   components = 3; break;
    case IF_BGRA:  components = 4; break;
    case IF_RED:   components = 1; break;
    case IF_GREEN: components = 1; break;
    case IF_BLUE:  components = 1; break;
    case IF_ALPHA: components = 1; break;
    case IF_LUMINANCE:       components = 1; break;
    case IF_LUMINANCE_ALPHA: components = 2; break;
    case IF_DEPTH_COMPONENT: components = 1; break;
    default:
      break;
  }

  int line_count = img->height()?img->height():1;
  if (img->depth())
    line_count *= img->depth();
  else
  if (img->isCubemap())
    line_count *= 6;

  for(int i=0; i<line_count; ++i)
  {
    const void* srcLine = pixels()      + pitch()*i;
    void* dstLine = img->pixels() + img->pitch()*i;

    const unsigned char* srcUByte  = (const unsigned char*)srcLine;
    const GLbyte*        srcSByte  = (const GLbyte*)srcLine;
    const GLushort*      srcUShort = (const GLushort*)srcLine;
    const GLshort*       srcSShort = (const GLshort*)srcLine;
    const unsigned int*  srcUInt   = (const unsigned int*)srcLine;
    const int*           srcSInt   = (const int*)srcLine;
    const float*         srcFloat  = (const float*)srcLine;

    unsigned char* dstUByte  = (unsigned char*)dstLine;
    GLbyte*        dstSByte  = (GLbyte*)dstLine;
    GLushort*      dstUShort = (GLushort*)dstLine;
    GLshort*       dstSShort = (GLshort*)dstLine;
    unsigned int*  dstUInt   = (unsigned int*)dstLine;
    int*           dstSInt   = (int*)dstLine;
    float*         dstFloat  = (float*)dstLine;

    for(int j=0; j<img->width(); ++j)
    {
      for(int c=0; c<components; ++c)
      {
        double dval = 0;
        long long qint = 0;

        // convert dst format to double

        switch(type())
        {
          case IT_UNSIGNED_BYTE:  qint = *srcUByte;  dval = qint/255.0; ++srcUByte; break;
          case IT_BYTE:           qint = *srcSByte;  dval = qint/127.0; ++srcSByte; break;
          case IT_UNSIGNED_SHORT: qint = *srcUShort; dval = qint/65535.0; ++srcUShort; break;
          case IT_SHORT:          qint = *srcSShort; dval = qint/32767.0; ++srcSShort; break;
          case IT_UNSIGNED_INT:   qint = *srcUInt;   dval = qint/4294967295.0; ++srcUInt; break;
          case IT_INT:            qint = *srcSInt;   dval = qint/2147483647.0; ++srcSInt; break;
          case IT_FLOAT:          dval = *srcFloat;  ++srcFloat; break;
          default:
            return NULL;
        }

        // clamp 0.0 >= dval >= 1.0
        dval = dval < 0.0 ? 0.0 :
               dval > 1.0 ? 1.0 :
               dval;

        // convert double to dst format

        switch(img->type())
        {
          case IT_UNSIGNED_BYTE:  *dstUByte  = (unsigned char) (dval*255.0); ++dstUByte; break;
          case IT_BYTE:           *dstSByte  = (GLbyte)  (dval*127.0); ++dstSByte; break;
          case IT_UNSIGNED_SHORT: *dstUShort = (GLushort)(dval*65535.0); ++dstUShort; break;
          case IT_SHORT:          *dstSShort = (GLshort) (dval*32767.0); ++dstSShort; break;
          case IT_UNSIGNED_INT:   *dstUInt   = (unsigned int)  (dval*4294967295.0); ++dstUInt; break;
          case IT_INT:            *dstSInt   = (int)   (dval*2147483647.0); ++dstSInt; break;
          case IT_FLOAT:          *dstFloat  = (float)dval; ++dstFloat; break;
          default:
            return NULL;
        }
      }
    }
  }

  return img;
}
//-----------------------------------------------------------------------------
namespace {
  template<typename T>
  void equalizeTemplate(void* ptr, int pitch, int comps, int w, int h, T max_val)
  {
    // find min/max
    T vmin = *((T*)ptr);
    T vmax = *((T*)ptr);
    for(int y=0; y<h; ++y)
    {
      T* px = (T*)((char*)ptr + pitch*y);
      for(int x=0; x<w; ++x)
      {
        for(int i=0; i<comps; ++i)
        {
          if (vmin > px[x*comps+i]) vmin = px[x*comps+i];
          if (vmax < px[x*comps+i]) vmax = px[x*comps+i];
        }
      }
    }
    // equalize
    T range = vmax-vmin;
    for(int y=0; y<h; ++y)
    {
      T* px = (T*)((char*)ptr + pitch*y);
      for(int x=0; x<w; ++x)
      {
        for(int i=0; i<comps; ++i)
          px[x*comps+i] = (T)(((float)px[x*comps+i]-vmin)/range*max_val);
      }
    }
  }
}
//! This function is mainly useful for images whose type() is IF_LUMINANCE, IF_RED, IF_GREEN, IF_BLUE, IF_ALPHA or IF_DEPTH_COMPONENT.
//! IF_RGB, IF_RGBA, IF_BGR, IF_BGRA and IF_LUMINANCE_ALPHA types are not optimally handled yet.
bool Image::equalize()
{
  int comps = 0;
  switch(format())
  {
    case IF_RGB:   comps = 3; break;
    case IF_RGBA:  comps = 4; break;
    case IF_BGR:   comps = 3; break;
    case IF_BGRA:  comps = 4; break;
    case IF_RED:   comps = 1; break;
    case IF_GREEN: comps = 1; break;
    case IF_BLUE:  comps = 1; break;
    case IF_ALPHA: comps = 1; break;
    case IF_LUMINANCE: comps = 1; break;
    case IF_LUMINANCE_ALPHA: comps = 2; break;
    case IF_DEPTH_COMPONENT: comps = 1; break;
    default:
      Log::error("Image::equalize(): unsupported image format().\n");
      return false;
  }
  int w = width();
  int h = height()?height():1;
  int d = depth()?depth():1;
  if (isCubemap())
    h=h*6;
  else
    h=h*d;

  switch(type())
  {
    case IT_UNSIGNED_BYTE:  equalizeTemplate<unsigned char> (pixels(), pitch(), comps, w, h, 0xFF);      break;
    case IT_UNSIGNED_SHORT: equalizeTemplate<unsigned short>(pixels(), pitch(), comps, w, h, 0xFFFF);     break;
    case IT_UNSIGNED_INT:   equalizeTemplate<unsigned int>  (pixels(), pitch(), comps, w, h, 0xFFFFFFFF); break;
    case IT_FLOAT:          equalizeTemplate<float>         (pixels(), pitch(), comps, w, h, 1.0f);       break;
      break;
    default:
      Log::error("Image::equalize(): unsupported image type(). Types supported are IT_UNSIGNED_BYTE, IT_UNSIGNED_SHORT, IT_UNSIGNED_INT, IT_FLOAT.\n");
      return false;
  }
  return true;
}
//-----------------------------------------------------------------------------
/** The \p center and \p width parameters are in Hounsfield units.
Calls contrastHounsfield(float center, float width, float intercept, float range) with \p center, \p with, \p range and 
\p intercept parameters extracted from the image tags() \p "WindowCenter", \p "WindowWidth", \p "BitsStored" and \p "RescaleIntercept".
This function is equivalent to the code below.
\code
if ( !tags()->has("WindowCenter") || !tags()->has("WindowWidth") || !tags()->has("BitsStored") || !tags()->has("RescaleIntercept"))
  return false;

float center    = tags()->value("WindowCenter").toFloat();
float width     = tags()->value("WindowWidth").toFloat();
float range     = (1<<tags()->value("BitsStored").toInt()) - 1.0f;
float intercept = tags()->value("RescaleIntercept").toFloat();

// Hounsfield units: -1000 = air, +1000 = solid bone
// Transform from Hounsfield units to normalized 0..1 units
center = (center-intercept) / range;
width  = width / range;
return contrast( center-width/2.0f, center+width/2.0f );
\endcode
*/
bool Image::contrastHounsfieldAuto()
{
  if ( !tags()->has("WindowCenter") || !tags()->has("WindowWidth") || !tags()->has("BitsStored") || !tags()->has("RescaleIntercept"))
    return false;

  float center    = tags()->value("WindowCenter").toFloat();
  float width     = tags()->value("WindowWidth").toFloat();
  float range     = (1<<tags()->value("BitsStored").toInt()) - 1.0f;
  float intercept = tags()->value("RescaleIntercept").toFloat();
  float slope     = tags()->value("RescaleSlope").toFloat();

  // Hounsfield units: -1000 = air, +1000 = solid bone
  // Transform from Hounsfield units to normalized 0..1 units
  center = (center-intercept) / range / slope;
  width  = width / range / slope;
  return contrast( center-width/2.0f, center+width/2.0f );
}
//-----------------------------------------------------------------------------
/** The \p center and \p width parameters are in Hounsfield units.
This function is equivalent to the code below:
\code
// Hounsfield units: -1000 = air, +1000 = solid bone
// Transform from Hounsfield units to normalized 0..1 units
center = (center-intercept) / range;
width  = width / range;
return contrast( center-width/2.0f, center+width/2.0f );
\endcode
*/
bool Image::contrastHounsfield(float center, float width, float intercept, float range)
{
  // Hounsfield units: -1000 = air, +1000 = solid bone
  // Transform from Hounsfield units to normalized 0..1 units
  center = (center-intercept) / range;
  width  = width / range;
  return contrast( center-width/2.0f, center+width/2.0f );
}
//-----------------------------------------------------------------------------
namespace {
  template<typename T>
  void contrastTemplate(void* ptr, int pitch, int w, int h, T max_val, float black, float white)
  {
    float range = white-black;
    for(int y=0; y<h; ++y)
    {
      T* px = (T*)((char*)ptr + pitch*y);
      for(int x=0; x<w; ++x)
      {
        float t = (float)px[x]/max_val; // 0..1
        t = (t-black)/range;
        t = vl::clamp(t, 0.0f, 1.0f);
        px[x] = (T)(t*max_val);
      }
    }
  }
}
//! This function supports only images whose type() is IF_LUMINANCE, IF_RED, IF_GREEN, IF_BLUE, IF_ALPHA or IF_DEPTH_COMPONENT.
//! The parameters \p black and \p white are in normalized units (0=black, 1=white) but are not required to be in the range between 0 and 1.
bool Image::contrast(float black, float white)
{
  switch(format())
  {
    case IF_RED:
    case IF_GREEN:
    case IF_BLUE:
    case IF_ALPHA:
    case IF_LUMINANCE:
    case IF_DEPTH_COMPONENT:
      break;
    default:
      Log::error("Image::equalize(): unsupported image format().\n");
      return false;
  }
  int w = width();
  int h = height()?height():1;
  int d = depth()?depth():1;
  if (isCubemap())
    h=h*6;
  else
    h=h*d;

  switch(type())
  {
    case IT_UNSIGNED_BYTE:  contrastTemplate<unsigned char> (pixels(), pitch(), w, h, 0xFF, black, white);       break;
    case IT_UNSIGNED_SHORT: contrastTemplate<unsigned short>(pixels(), pitch(), w, h, 0xFFFF, black, white);     break;
    case IT_UNSIGNED_INT:   contrastTemplate<unsigned int>  (pixels(), pitch(), w, h, 0xFFFFFFFF, black, white); break;
    case IT_FLOAT:          contrastTemplate<float>         (pixels(), pitch(), w, h, 1.0f, black, white);       break;
      break;
    default:
      Log::error("Image::equalize(): unsupported image type(). Types supported are IT_UNSIGNED_BYTE, IT_UNSIGNED_SHORT, IT_UNSIGNED_INT, IT_FLOAT.\n");
      return false;
  }
  return true;
}
//-----------------------------------------------------------------------------
namespace
{
  class rgbal
  {
  public:
    rgbal(): r(-1), g(-1), b(-1), a(-1), l(-1) {}
    int r,g,b,a,l;
  };

  template<typename T>
  void convert(const T*src_px, T*dst_px, T max_value, const rgbal& srco, const rgbal& dsto)
  {
    // set dst default values first
    if (dsto.r != -1)
      dst_px[dsto.r] = 0;
    if (dsto.g != -1)
      dst_px[dsto.g] = 0;
    if (dsto.b != -1)
      dst_px[dsto.b] = 0;
    if (dsto.a != -1)
      dst_px[dsto.a] = max_value;
    if (dsto.l != -1)
      dst_px[dsto.l] = 0;

    // try copy src -> dst
    if (dsto.r != -1 && srco.r != -1 )
      dst_px[dsto.r] = src_px[srco.r];
    if (dsto.g != -1 && srco.g != -1)
      dst_px[dsto.g] = src_px[srco.g];
    if (dsto.b != -1 && srco.b != -1)
      dst_px[dsto.b] = src_px[srco.b];
    if (dsto.a != -1 && srco.a != -1)
      dst_px[dsto.a] = src_px[srco.a];
    if (dsto.l != -1 && srco.l != -1)
      dst_px[dsto.l] = src_px[srco.l];

    // try rgb -> gray conversion
    if (dsto.l != -1 && srco.r != -1 && srco.g != -1 && srco.b != -1)
    {
      dvec3 col(src_px[srco.r], src_px[srco.g], src_px[srco.b]);
      double gray = dot(col / dvec3(max_value,max_value,max_value), dvec3(0.299,0.587,0.114));
      dst_px[dsto.l] = T(gray * max_value);
    }
    else
    // try r -> gray conversion
    if (dsto.l != -1 && srco.r != -1 && srco.g == -1 && srco.b == -1)
      dst_px[dsto.l] = src_px[srco.r];
    else
    // try g -> gray conversion
    if (dsto.l != -1 && srco.r == -1 && srco.g != -1 && srco.b == -1)
      dst_px[dsto.l] = src_px[srco.g];
    else
    // try b -> gray conversion
    if (dsto.l != -1 && srco.r == -1 && srco.g == -1 && srco.b != -1)
      dst_px[dsto.l] = src_px[srco.b];
    else
    // try gray -> r,g,b
    if (srco.l != -1)
    {
      if (dsto.r != -1)
        dst_px[dsto.r] = src_px[srco.l];
      if (dsto.g != -1)
        dst_px[dsto.g] = src_px[srco.l];
      if (dsto.b != -1)
        dst_px[dsto.b] = src_px[srco.l];
    }
  }
}
ref<Image> vl::Image::convertFormat(EImageFormat new_format) const
{
  switch(type())
  {
    case IT_UNSIGNED_BYTE:
    case IT_BYTE:         
    case IT_UNSIGNED_SHORT:
    case IT_SHORT:         
    case IT_UNSIGNED_INT:  
    case IT_INT:           
    case IT_FLOAT:
      break;
    default:
      Log::error("Image::convertType(): unsupported image type.\n");
      return NULL;
  }

  switch(format())
  {
    case IF_RGB:
    case IF_RGBA:
    case IF_BGR:
    case IF_BGRA:
    case IF_RED:
    case IF_GREEN:
    case IF_BLUE:
    case IF_ALPHA:
    case IF_LUMINANCE:
    case IF_LUMINANCE_ALPHA:
      break;
    default:
      Log::error("Image::convertType(): unsupported source image format.\n");
      return NULL;
  }

  switch(new_format)
  {
    case IF_RGB:
    case IF_RGBA:
    case IF_BGR:
    case IF_BGRA:
    case IF_RED:
    case IF_GREEN:
    case IF_BLUE:
    case IF_ALPHA:
    case IF_LUMINANCE:
    case IF_LUMINANCE_ALPHA:
      break;
    default:
      Log::error("Image::convertType(): unsupported destination image format.\n");
      return NULL;
  }

  ref<Image> img = new Image;
  img->setObjectName( objectName().c_str() );
  img->setFormat(new_format);
  img->setType(type());
  img->setWidth(width());
  img->setHeight(height());
  img->setDepth(depth());
  img->setByteAlignment(1);
  img->mIsCubemap = isCubemap();
  img->mIsNormalMap = mIsNormalMap;
  img->mHasAlpha    = mHasAlpha;
  img->allocate();

  rgbal srco; // = {-1,-1,-1,-1,-1}, 
  rgbal dsto; // = {-1,-1,-1,-1,-1};

  // compute src and dst color component offsets

  switch(format())
  {
    case IF_RGB:             srco.r = 0; srco.g = 1; srco.b = 2; break;
    case IF_RGBA:            srco.r = 0; srco.g = 1; srco.b = 2; srco.a = 3; break;
    case IF_BGR:             srco.r = 2; srco.g = 1; srco.b = 0; break;
    case IF_BGRA:            srco.r = 2; srco.g = 1; srco.b = 0; srco.a = 3; break;
    case IF_RED:             srco.r = 0; break;
    case IF_GREEN:           srco.g = 0; break;
    case IF_BLUE:            srco.b = 0; break;
    case IF_ALPHA:           srco.a = 0; break;
    case IF_LUMINANCE:       srco.l = 0; break;
    case IF_LUMINANCE_ALPHA: srco.l = 0; srco.a = 1; break;
    default:
      return NULL;
  }

  switch(new_format)
  {
    case IF_RGB:             dsto.r = 0; dsto.g = 1; dsto.b = 2; break;
    case IF_RGBA:            dsto.r = 0; dsto.g = 1; dsto.b = 2; dsto.a = 3; break;
    case IF_BGR:             dsto.r = 2; dsto.g = 1; dsto.b = 0; break;
    case IF_BGRA:            dsto.r = 2; dsto.g = 1; dsto.b = 0; dsto.a = 3; break;
    case IF_RED:             dsto.r = 0; break;
    case IF_GREEN:           dsto.g = 0; break;
    case IF_BLUE:            dsto.b = 0; break;
    case IF_ALPHA:           dsto.a = 0; break;
    case IF_LUMINANCE:       dsto.l = 0; break;
    case IF_LUMINANCE_ALPHA: dsto.l = 0; dsto.a = 1; break;
    default:
      return NULL;
  }

  int src_comp = 0;
  switch(format())
  {
    case IF_RGB:   src_comp = 3; break;
    case IF_RGBA:  src_comp = 4; break;
    case IF_BGR:   src_comp = 3; break;
    case IF_BGRA:  src_comp = 4; break;
    case IF_RED:   src_comp = 1; break;
    case IF_GREEN: src_comp = 1; break;
    case IF_BLUE:  src_comp = 1; break;
    case IF_ALPHA: src_comp = 1; break;
    case IF_LUMINANCE:       src_comp = 1; break;
    case IF_LUMINANCE_ALPHA: src_comp = 2; break;
    default:
      break;
  }

  int dst_comp = 0;
  switch(new_format)
  {
    case IF_RGB:   dst_comp = 3; break;
    case IF_RGBA:  dst_comp = 4; break;
    case IF_BGR:   dst_comp = 3; break;
    case IF_BGRA:  dst_comp = 4; break;
    case IF_RED:   dst_comp = 1; break;
    case IF_GREEN: dst_comp = 1; break;
    case IF_BLUE:  dst_comp = 1; break;
    case IF_ALPHA: dst_comp = 1; break;
    case IF_LUMINANCE:       dst_comp = 1; break;
    case IF_LUMINANCE_ALPHA: dst_comp = 2; break;
    case IF_DEPTH_COMPONENT: dst_comp = 1; break;
    default:
      break;
  }

  int line_count = img->height()?img->height():1;
  if (img->depth())
    line_count *= img->depth();
  else
  if (img->isCubemap())
    line_count *= 6;

  for(int i=0; i<line_count; ++i)
  {
    const void* srcLine = pixels()      + pitch()*i;
    void* dstLine = img->pixels() + img->pitch()*i;

    const unsigned char* srcUByte  = (const unsigned char*)srcLine;
    const GLbyte*        srcSByte  = (const GLbyte*)srcLine;
    const GLushort*      srcUShort = (const GLushort*)srcLine;
    const GLshort*       srcSShort = (const GLshort*)srcLine;
    const unsigned int*  srcUInt   = (const unsigned int*)srcLine;
    const int*           srcSInt   = (const int*)srcLine;
    const float*         srcFloat  = (const float*)srcLine;

    unsigned char* dstUByte  = (unsigned char*)dstLine;
    GLbyte*        dstSByte  = (GLbyte*)dstLine;
    GLushort*      dstUShort = (GLushort*)dstLine;
    GLshort*       dstSShort = (GLshort*)dstLine;
    unsigned int*  dstUInt   = (unsigned int*)dstLine;
    int*           dstSInt   = (int*)dstLine;
    float*         dstFloat  = (float*)dstLine;

    for(int j=0; j<img->width(); ++j)
    {
      switch(type())
      {
        case IT_UNSIGNED_BYTE:  convert<unsigned char> (srcUByte,  dstUByte,  255,         srco, dsto); srcUByte+=src_comp;  dstUByte+=dst_comp;  break;
        case IT_BYTE:           convert<GLbyte>        (srcSByte,  dstSByte,  127,         srco, dsto); srcSByte+=src_comp;  dstSByte+=dst_comp;  break;
        case IT_UNSIGNED_SHORT: convert<GLushort>      (srcUShort, dstUShort, 65535,       srco, dsto); srcUShort+=src_comp; dstUShort+=dst_comp; break;
        case IT_SHORT:          convert<GLshort>       (srcSShort, dstSShort, 32767,       srco, dsto); srcSShort+=src_comp; dstSShort+=dst_comp; break;
        case IT_UNSIGNED_INT:   convert<unsigned int>  (srcUInt,   dstUInt,   4294967295U, srco, dsto); srcUInt+=src_comp;   dstUInt+=dst_comp;   break;
        case IT_INT:            convert<int>           (srcSInt,   dstSInt,   2147483647,  srco, dsto); srcSInt+=src_comp;   dstSInt+=dst_comp;   break;
        case IT_FLOAT:          convert<float>         (srcFloat,  dstFloat,  1.0f,        srco, dsto); srcFloat+=src_comp;  dstFloat+=dst_comp;  break;
        default:
          return NULL;
      }
    }
  }

  return img;
}
//-----------------------------------------------------------------------------
fvec4 Image::sampleLinear(double x) const
{
  if (x < 0)
    x = 0;
  if (x >= width()-1)
    x = (width()-1) - 0.000001;

  int ix1 = (int)x;
  int ix2 = ix1+1;

  VL_CHECK(ix2<(int)width())

  float w21 = (float)vl::fract(x);
  float w11 = 1.0f - w21;

  fvec4 c11 = sample(ix1);
  fvec4 c21 = sample(ix2);

  return c11*w11 + c21*w21;
}
//-----------------------------------------------------------------------------
fvec4 Image::sampleLinear(double x, double y) const
{
  int h = height()?height():1;
  if (x < 0)
    x = 0;
  if (y < 0)
    y = 0;
  if (x >= width()-1)
    x = (width()-1)  - 0.000001;
  if (y >= h-1)
    y = (h-1) - 0.000001;

  int ix1 = (int)x;
  int iy1 = (int)y;
  int ix2 = ix1+1;
  int iy2 = iy1+1;

  VL_CHECK(ix2<(int)width())
  VL_CHECK(iy2<(int)height())

  double tx  = vl::fract(x);
  double ty  = vl::fract(y);
  double tx1 = 1.0f - vl::fract(x);
  double ty1 = 1.0f - vl::fract(y);

  float w11 = float(tx1*ty1);
  float w12 = float(tx1*ty);
  float w22 = float(tx *ty);
  float w21 = float(tx *ty1);

  fvec4 c11 = sample(ix1, iy1);
  fvec4 c12 = sample(ix1, iy2);
  fvec4 c22 = sample(ix2, iy2);
  fvec4 c21 = sample(ix2, iy1);

  return c11*w11 + c12*w12 + c22*w22 + c21*w21;
}
//-----------------------------------------------------------------------------
fvec4 Image::sampleLinear(double x, double y, double z) const
{
  if (x>width() -1.000001) x = width() -1.000001;
  if (y>height()-1.000001) y = height()-1.000001;
  if (z>depth() -1.000001) z = depth() -1.000001;
  if (x<0) x=0;
  if (y<0) y=0;
  if (z<0) z=0;
  int ix = int(x); 
  float xt = float(x - ix);
  int iy = int(y); 
  float yt = float(y - iy);
  int iz = int(z); 
  float zt = float(z - iz);
  fvec4 val0 = sample(ix  , iy,   iz);
  fvec4 val1 = sample(ix+1, iy,   iz);
  fvec4 val2 = sample(ix+1, iy+1, iz);
  fvec4 val3 = sample(ix,   iy+1, iz);
  fvec4 val4 = sample(ix  , iy,   iz+1);
  fvec4 val5 = sample(ix+1, iy,   iz+1);
  fvec4 val6 = sample(ix+1, iy+1, iz+1);
  fvec4 val7 = sample(ix,   iy+1, iz+1);
  float xt1 = 1-xt;
  float yt1 = 1-yt;
  float zt1 = 1-zt;
  fvec4 v1 = val0*(yt1) + val3*yt;
  fvec4 v2 = val1*(yt1) + val2*yt;
  fvec4 a = v1*(xt1) + v2*xt;
  v1 = val4*(yt1) + val7*yt;
  v2 = val5*(yt1) + val6*yt;
  fvec4 b = v1*(xt1) + v2*xt;
  return a*(zt1) + b*zt;
}
//-----------------------------------------------------------------------------
fvec4 Image::sample(int x, int y, int z) const
{
  VL_CHECK(x<width())
  VL_CHECK(!y || y<height())
  VL_CHECK(!z || z<depth())

  // find the start of the line
  int h = height()?height():1;
  const unsigned char* px = pixels() + y*pitch() + h*pitch()*z;

  int comp = 0;
  switch(format())
  {
    case IF_RGB:   comp = 3; break;
    case IF_RGBA:  comp = 4; break;
    case IF_BGR:   comp = 3; break;
    case IF_BGRA:  comp = 4; break;
    case IF_RED:   comp = 1; break;
    case IF_GREEN: comp = 1; break;
    case IF_BLUE:  comp = 1; break;
    case IF_ALPHA: comp = 1; break;
    case IF_LUMINANCE:       comp = 1; break;
    case IF_LUMINANCE_ALPHA: comp = 2; break;
    case IF_DEPTH_COMPONENT: comp = 1; break;
    default:
      break;
  }

  switch(type())
  {
    case IT_UNSIGNED_BYTE:  px += x*comp*1; break;
    case IT_BYTE:           px += x*comp*1; break;
    case IT_UNSIGNED_SHORT: px += x*comp*2; break;
    case IT_SHORT:          px += x*comp*2; break;
    case IT_UNSIGNED_INT:   px += x*comp*4; break;
    case IT_INT:            px += x*comp*4; break;
    case IT_FLOAT:          px += x*comp*4; break;
    default:
      break;
  }

  // convert component by component

  fvec4 pixel(0,0,0,0);

  for(int i=0; i<comp; ++i)
  {
    double value = 0;

    switch(type())
    {
      case IT_UNSIGNED_BYTE:  value = (double)((unsigned char*)px)[i]; value/=255.0; break;
      case IT_BYTE:           value = (double)((char*)px)[i]; value/=127.0; break;
      case IT_UNSIGNED_SHORT: value = (double)((unsigned short*)px)[i]; value/=65535.0; break;
      case IT_SHORT:          value = (double)((short*)px)[i]; value/=32767.0; break;
      case IT_UNSIGNED_INT:   value = (double)((unsigned int*)px)[i]; value/=4294967295.0; break;
      case IT_INT:            value = (double)((int*)px)[i]; value/=2147483647.0; break;
      case IT_FLOAT:          value = (double)((float*)px)[i]; break;
      default:
        break;
    }

    pixel[i] = (float)value;
  }

  fvec4 p(0,0,0,0);

  // arrange values based on the format
  switch(format())
  {
    case IF_RGB:   p = pixel; break;
    case IF_RGBA:  p = pixel; break;
    case IF_BGR:   p = pixel; p.r() = pixel.b(); p.b() = pixel.r(); break;
    case IF_BGRA:  p = pixel; p.r() = pixel.b(); p.b() = pixel.r(); break;
    case IF_RED:   p = fvec4(pixel[0], 0, 0, 0); break;
    case IF_GREEN: p = fvec4(0, pixel[0], 0, 0); break;
    case IF_BLUE:  p = fvec4(0, 0, pixel[0], 0); break;
    case IF_ALPHA: p = fvec4(0, 0, 0, pixel[0]); break;
    case IF_LUMINANCE:       p = fvec4(pixel[0], pixel[0], pixel[0], 0); break;
    case IF_LUMINANCE_ALPHA: p = fvec4(pixel[0], pixel[0], pixel[0], pixel[1]); break;
    case IF_DEPTH_COMPONENT: p = fvec4(pixel[0], 0, 0, 0); break;
    default:
      break;
  }

  return p;
}
//-----------------------------------------------------------------------------
ref<Image> Image::subImage(int xstart, int ystart, int width, int height)
{
  VL_CHECK(xstart+width  <= this->width())
  VL_CHECK(ystart+height <= this->height())

  ref<Image> img = new Image;
  img->allocate2D(width, height, 1, format(), type());
  // copy line by line
  for(int i=0; i<height; ++i)
  {
    unsigned char* dst = img->pixels() + img->pitch()*i;
    unsigned char* src = pixels() + pitch()*(i+ystart) + xstart*bitsPerPixel()/8;
    memcpy(dst, src, width*bitsPerPixel()/8);
  }
  return img;
}
//-----------------------------------------------------------------------------
void Image::copySubImage(Image* img_src, RectI src, ivec2 dst)
{
  ref<Image> img = img_src;
  if (img->type() != this->type())
    img = img->convertType(this->type());
  if (img->format() != this->format())
    img = img->convertFormat(this->format());
  // tests
  VL_CHECK(dst.x()>=0)
  VL_CHECK(dst.y()>=0)
  VL_CHECK(dst.x() + src.width()  <= this->width() )
  VL_CHECK(dst.y() + src.height() <= this->height())
  VL_CHECK(src.x()>=0)
  VL_CHECK(src.y()>=0)
  VL_CHECK(src.right() < img->width() )
  VL_CHECK(src.top()   < img->height())
  for(int i=0; i<src.height(); ++i)
  {
    int ysrc = i+src.y();
    int ydst = i+dst.y();
    unsigned char* psrc = img->pixels() + img->pitch()*ysrc + (src.x()*img->bitsPerPixel()/8);
    unsigned char* pdst = this->pixels() + this->pitch()*ydst + (dst.x()*this->bitsPerPixel()/8);
    memcpy(pdst, psrc, src.width()*this->bitsPerPixel()/8);
  }
}
//-----------------------------------------------------------------------------
void Image::substituteColorRGB_RGBA(unsigned int before, unsigned int after)
{
  if (type() != IT_UNSIGNED_BYTE)
  {
    Log::error("Image::substituteColorRGB_RGBA(): this function can be called only on images whose type() is IT_UNSIGNED_BYTE\n");
    return;
  }
  if (format() != IF_RGBA && format() != IF_RGB)
  {
    Log::error("Image::substituteColorRGB_RGBA(): this function can be called only on images whose format() is either IF_RGBA or IF_RGB\n");
    return;
  }
  unsigned char bef[3];
  unsigned char aft[4];
  bef[0] = (unsigned char)((before >> 16) & 0xFF);
  bef[1] = (unsigned char)((before >>  8) & 0xFF);
  bef[2] = (unsigned char)((before >>  0) & 0xFF);

  aft[0] = (unsigned char)((after  >> 24) & 0xFF);
  aft[1] = (unsigned char)((after  >> 16) & 0xFF);
  aft[2] = (unsigned char)((after  >>  8) & 0xFF);
  aft[3] = (unsigned char)((after  >>  0) & 0xFF);

  int comps = format() == IF_RGBA ? 4 : 3;
  int d = depth() ? depth() : 1;
  for(int y=0; y<height()*d; ++y)
  {
    for(int x=0; x<width(); ++x)
    {
      unsigned char* px = pixels() + pitch()*y + x*comps;
      if (px[0] == bef[0] && px[1] == bef[1] && px[2] == bef[2])
      {
        px[0] = aft[0];
        px[1] = aft[1];
        px[2] = aft[2];
        if (comps == 4)
          px[3] = aft[3];
      }
    }
  }
}
//-----------------------------------------------------------------------------
void Image::substituteColorRGB_RGB(unsigned int before, unsigned int after)
{
  if (type() != IT_UNSIGNED_BYTE)
  {
    Log::error("Image::substituteColorRGB_RGB(): this function can be called only on images whose type() is IT_UNSIGNED_BYTE\n");
    return;
  }
  if (format() != IF_RGBA && format() != IF_RGB)
  {
    Log::error("Image::substituteColorRGB_RGB(): this function can be called only on images whose format() is either IF_RGBA or IF_RGB\n");
    return;
  }
  unsigned char bef[3];
  unsigned char aft[3];
  bef[0] = (unsigned char)((before >> 16) & 0xFF);
  bef[1] = (unsigned char)((before >>  8) & 0xFF);
  bef[2] = (unsigned char)((before >>  0) & 0xFF);

  aft[0] = (unsigned char)((after  >> 24) & 0xFF);
  aft[1] = (unsigned char)((after  >> 16) & 0xFF);
  aft[2] = (unsigned char)((after  >>  8) & 0xFF);

  int comps = format() == IF_RGBA ? 4 : 3;
  int d = depth() ? depth() : 1;
  for(int y=0; y<height()*d; ++y)
  {
    for(int x=0; x<width(); ++x)
    {
      unsigned char* px = pixels() + pitch()*y + x*comps;
      if (px[0] == bef[0] && px[1] == bef[1] && px[2] == bef[2])
      {
        px[0] = aft[0];
        px[1] = aft[1];
        px[2] = aft[2];
      }
    }
  }
}
//-----------------------------------------------------------------------------
void Image::substituteColorGreenKey(unsigned int col0, unsigned int col1)
{
  if (type() != IT_UNSIGNED_BYTE)
  {
    Log::error("Image::substituteColorRGB_RGB(): this function can be called only on images whose type() is IT_UNSIGNED_BYTE\n");
    return;
  }
  if (format() != IF_RGBA && format() != IF_RGB)
  {
    Log::error("Image::substituteColorRGB_RGB(): this function can be called only on images whose format() is either IF_RGBA or IF_RGB\n");
    return;
  }
  unsigned char c0[3];
  unsigned char c1[3];
  c0[0] = (unsigned char)((col0 >> 16) & 0xFF);
  c0[1] = (unsigned char)((col0 >>  8) & 0xFF);
  c0[2] = (unsigned char)((col0 >>  0) & 0xFF);

  c1[0] = (unsigned char)((col1 >> 16) & 0xFF);
  c1[1] = (unsigned char)((col1 >>  8) & 0xFF);
  c1[2] = (unsigned char)((col1 >>  0) & 0xFF);

  int comps = format() == IF_RGBA ? 4 : 3;
  int d = depth() ? depth() : 1;
  for(int y=0; y<height()*d; ++y)
  {
    for(int x=0; x<width(); ++x)
    {
      unsigned char* px = pixels() + pitch()*y + x*comps;
      double t = (double)px[1] / 0xFF;
      px[0] = (unsigned char)(c0[0]*(1.0-t) + c1[0]*t);
      px[1] = (unsigned char)(c0[1]*(1.0-t) + c1[1]*t);
      px[2] = (unsigned char)(c0[2]*(1.0-t) + c1[2]*t);
    }
  }
}
//-----------------------------------------------------------------------------
