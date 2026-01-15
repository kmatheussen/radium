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

#include "ioDDS.hpp"
#include <vlCore/LoadWriterManager.hpp>
#include <vlCore/VisualizationLibrary.hpp>
#include <vlCore/VisualizationLibrary.hpp>
#include <vlCore/FileSystem.hpp>
#include <vlCore/VirtualFile.hpp>
#include <vlCore/Image.hpp>

// mic fixme: 
// http://msdn.microsoft.com/en-us/library/bb943991(v=vs.85).aspx#dds_variants
// - read and write float and half float images.
// - support directx 10 dds and formats
// - support A8R8G8B8, A1R5G5B5, A4R4G4B4, R8G8B8, R5G6B5

using namespace vl;

#include <vlCore/ImageTools.hpp>

//-----------------------------------------------------------------------------
// DDS loader
//-----------------------------------------------------------------------------
namespace
{
  // DDSURFACEDESC2.dwFlags
  const unsigned long DDS_CAPS            = 0x00000001;
  const unsigned long DDS_HEIGHT          = 0x00000002;
  const unsigned long DDS_WIDTH           = 0x00000004;
  const unsigned long DDS_PITCH           = 0x00000008;
  const unsigned long DDS_BACKBUFFERCOUNT = 0x00000020;
  const unsigned long DDS_ZBUFFERBITDEPTH = 0x00000040;
  const unsigned long DDS_ALPHABITDEPTH   = 0x00000080;
  const unsigned long DDS_LPSURFACE       = 0x00000800;
  const unsigned long DDS_PIXELFORMAT     = 0x00001000;
  const unsigned long DDS_CKDESTOVERLAY   = 0x00002000;
  const unsigned long DDS_CKDESTBLT       = 0x00004000;
  const unsigned long DDS_CKSRCOVERLAY    = 0x00008000;
  const unsigned long DDS_CKSRCBLT        = 0x00010000;
  const unsigned long DDS_MIPMAPCOUNT     = 0x00020000;
  const unsigned long DDS_REFRESHRATE     = 0x00040000;
  const unsigned long DDS_LINEARSIZE      = 0x00080000;
  const unsigned long DDS_TEXTURESTAGE    = 0x00100000;
  const unsigned long DDS_FVF             = 0x00200000;
  const unsigned long DDS_SRCVBHANDLE     = 0x00400000;
  const unsigned long DDS_DEPTH           = 0x00800000;
  const unsigned long DDS_ALL             = 0x007FF9EE;
  const unsigned long DDS_REQUIRED_FLAGS  = DDS_CAPS|DDS_PIXELFORMAT|DDS_WIDTH|DDS_HEIGHT;

  // DDPIXELFORMAT.dwFlags
  const unsigned long DDPF_ALPHAPIXELS     = 0x00000001; // there is an alpha channel
  const unsigned long DDPF_ALPHA           = 0x00000002; // the image is an alpha channel only image
  const unsigned long DDPF_FOURCC          = 0x00000004; // the foucc code defines the pixel format
  const unsigned long DDPF_INDEXED4        = 0x00000008;
  const unsigned long DDPF_INDEXEDTO8      = 0x00000010;
  const unsigned long DDPF_INDEXED8        = 0x00000020;
  const unsigned long DDPF_RGB             = 0x00000040; // RGB, no fourcc
  const unsigned long DDPF_RGBA            = 0x00000041; // DDPF_RGB | DDPF_ALPHAPIXELS
  const unsigned long DDPF_COMPRESSED      = 0x00000080; // It's a compressed format
  const unsigned long DDPF_RGBTOYUV        = 0x00000100;
  const unsigned long DDPF_YUV             = 0x00000200;
  const unsigned long DDPF_ZBUFFER         = 0x00000400;
  const unsigned long DDPF_PALETTEINDEXED1 = 0x00000800;
  const unsigned long DDPF_PALETTEINDEXED2 = 0x00001000;
  const unsigned long DDPF_ZPIXELS         = 0x00002000;
  const unsigned long DDPF_STENCILBUFFER   = 0x00004000;
  const unsigned long DDPF_ALPHAPREMULT    = 0x00008000;
  const unsigned long DDPF_LUMINANCE       = 0x00020000; // Is a luminance image
  const unsigned long DDPF_BUMPLUMINANCE   = 0x00040000;
  const unsigned long DDPF_BUMPDUDV        = 0x00080000;

  // DDSCAPS2.dwCaps1
  const unsigned long DDSCAPS_COMPLEX = 0x00000008; // Whenever is cubemap or volume
  const unsigned long DDSCAPS_TEXTURE = 0x00001000; // Always present
  const unsigned long DDSCAPS_MIPMAP  = 0x00400000; // It's a mipmap image

  // DDSCAPS2.dwCaps2
  const unsigned long DDSCAPS2_VOLUME            = 0x00200000; // It's a 3D texture
  const unsigned long DDSCAPS2_CUBEMAP           = 0x00000200; // It's a cubemap
  const unsigned long DDSCAPS2_CUBEMAP_POSITIVEX = 0x00000400;
  const unsigned long DDSCAPS2_CUBEMAP_NEGATIVEX = 0x00000800;
  const unsigned long DDSCAPS2_CUBEMAP_POSITIVEY = 0x00001000;
  const unsigned long DDSCAPS2_CUBEMAP_NEGATIVEY = 0x00002000;
  const unsigned long DDSCAPS2_CUBEMAP_POSITIVEZ = 0x00004000;
  const unsigned long DDSCAPS2_CUBEMAP_NEGATIVEZ = 0x00008000;
  const unsigned long DDSCAPS2_CUBEMAP_FACES = DDSCAPS2_CUBEMAP_POSITIVEX | DDSCAPS2_CUBEMAP_NEGATIVEX | 
                                               DDSCAPS2_CUBEMAP_POSITIVEY | DDSCAPS2_CUBEMAP_NEGATIVEY |
                                               DDSCAPS2_CUBEMAP_POSITIVEZ | DDSCAPS2_CUBEMAP_NEGATIVEZ ; 

  inline unsigned int makeFourCC(unsigned int ch0, unsigned int ch1, unsigned int ch2, unsigned int ch3) 
  {
      return ch0 | (ch1 << 8) | ( ch2 << 16) | ( ch3 << 24 );
  }

  inline bool isFourCC(const char* code, unsigned int fcc)
  {
    return makeFourCC(code[0], code[1], code[2], code[3]) == fcc;
  }

  // quick surface type macros

  #define IS_BGRA8(pf) \
    ((pf.dwFlags & DDPF_RGB) && \
     (pf.dwFlags & DDPF_ALPHAPIXELS) && \
     (pf.dwRGBBitCount == 32))

  #define IS_BGRX8(pf) \
    ((pf.dwFlags & DDPF_RGB) && \
     !(pf.dwFlags & DDPF_ALPHAPIXELS) && \
     (pf.dwRGBBitCount == 32))

  #define IS_BGR8(pf) \
    ((pf.dwFlags & DDPF_RGB) && \
    !(pf.dwFlags & DDPF_ALPHAPIXELS) && \
     (pf.dwRGBBitCount == 24))

  #define IS_GRAY8(pf) \
    ((((pf.dwFlags & DDPF_LUMINANCE) || (pf.dwFlags & DDPF_ALPHA) ) && \
    (pf.dwRGBBitCount == 8) && !(pf.dwFlags & DDPF_ALPHAPIXELS) ) || \
     isFourCC("G8  ", pf.dwFourCC ) )

  #define IS_GRAY8_ALPHA8(pf) \
    (((pf.dwFlags & DDPF_LUMINANCE) && \
    (pf.dwRGBBitCount == 16) && (pf.dwFlags & DDPF_ALPHAPIXELS)) || \
     isFourCC("AG8 ", pf.dwFourCC ) )

  #define IS_PALETTE8(pf) isFourCC("P8  ", pf.dwFourCC)

  #define IS_DXT1(pf) isFourCC("DXT1", pf.dwFourCC)

  #define IS_DXT3(pf) isFourCC("DXT3", pf.dwFourCC)

  #define IS_DXT5(pf) isFourCC("DXT5", pf.dwFourCC)

  typedef struct
  {
    unsigned int dwSize;
    unsigned int dwFlags;
    unsigned int dwHeight;
    unsigned int dwWidth;
    unsigned int dwPitchOrLinearSize;
    unsigned int dwDepth;
    unsigned int dwMipMapCount;
    unsigned int dwReserved1[ 11 ];

    // DDPIXELFORMAT
    struct
    {
      unsigned int dwSize;
      unsigned int dwFlags;
      unsigned int dwFourCC;
      unsigned int dwRGBBitCount;
      unsigned int dwRBitMask;
      unsigned int dwGBitMask;
      unsigned int dwBBitMask;
      unsigned int dwAlphaBitMask;
    } ddpfPixelFormat;

    // DDCAPS2
    struct
    {
      unsigned int dwCaps1;
      unsigned int dwCaps2;
      unsigned int dwReserved[2];
    } ddsCaps;

    unsigned int dwReserved2;

  } DDSURFACEDESC2;

  enum
  {
    DDS_IMAGE_NULL = 0,
    DDS_IMAGE_2D = 2,
    DDS_IMAGE_3D = 3,
    DDS_IMAGE_CUBEMAP = 4
  };
}
//-----------------------------------------------------------------------------
//! Loads a DDS file.
//! Can load 2D textures, cubemaps, mipmaps for both cubemaps and 2d textures
//! (also non square and non power of 2). \n
//!
//! Supports the following formats:
//! - RGB 24 bit, uncompressed
//! - RGBA 32 bit, uncompressed
//! - BGR 24 bit, uncompressed
//! - BGRA 32 bit, uncompressed
//! - Grayscale 8 bit
//! - Grayscale + Alpha, 8 + 8 bit
//! - 8 bit palettized (8 bit palette compression)
//! - DXT1, DXT3, DXT5
//!
//! \remarks
//! DDS images and cubemaps will look flipped if created according to the DirectX conventions. \n
//! In order to prevent this, your tools might have an "export DDS for OpenGL"-like option.
//! If not, you will either have to flip your images before exporting them to DDS or you
//! will have to flip the texture coordinates of your models.
ref<Image> vl::loadDDS(const String& path)
{
  ref<VirtualFile> file = defFileSystem()->locateFile(path);
  if ( !file )
  {
    Log::error( Say("File '%s' not found.\n") << path );
    return NULL;
  }

  return loadDDS(file.get());
}

VL_COMPILE_TIME_CHECK( sizeof(DDSURFACEDESC2) == 124 );

ref<Image> vl::loadDDS(VirtualFile* file)
{
  if ( !file->open(OM_ReadOnly) )
  {
    Log::error( Say("DDS: could not open file '%s'.\n") << file->path() );
    return NULL;
  }

  char signature[4];
  file->read(signature,4);
  if (strncmp(signature, "DDS ", 4) != 0)
  {
    file->close();
    Log::error( Say("DDS: '%s' is not a DDS file.\n") << file->path() );
    return NULL;
  }

  DDSURFACEDESC2 header;
  memset(&header, 0, sizeof(header));

  file->read(&header, sizeof(header));

  if (header.dwSize != 124 || header.ddpfPixelFormat.dwSize != 32)
  {
    Log::error( Say("DDS file '%s': corrupted header.\n") << file->path() );
    file->close();
    return NULL;
  }

  if ((header.dwFlags & DDS_REQUIRED_FLAGS) != DDS_REQUIRED_FLAGS )
    Log::warning( Say("DDS file '%s': missing DDS_REQUIRED_FLAGS flags.\n") << file->path() );

  if ((header.ddsCaps.dwCaps1 & DDSCAPS_TEXTURE) != DDSCAPS_TEXTURE)
    Log::warning( Say("DDS file '%s': missing DDSCAPS_TEXTURE flag.\n") << file->path() );

  int image_type = header.dwDepth ? DDS_IMAGE_3D : DDS_IMAGE_2D;

  if (header.ddsCaps.dwCaps2 & DDSCAPS2_CUBEMAP)
  {
    bool allfaces = (header.ddsCaps.dwCaps2 & DDSCAPS2_CUBEMAP_FACES) != DDSCAPS2_CUBEMAP_FACES;

    if (!allfaces)
    {
      Log::error( Say("DDS: '%s' contains an invalid cubemap.\n") << file->path() );
      file->close();
      return NULL;
    }
    else
      image_type = DDS_IMAGE_CUBEMAP;
  }

  bool reverse_rgba_bgra = false;
  if (header.ddpfPixelFormat.dwRBitMask != 0xFF)
    reverse_rgba_bgra = true;

  // loosen alpha mask checks

  // int width = header.dwWidth;
  // int height = header.dwHeight;
  // int depth = (header.dwFlags & DDS_DEPTH) ? header.dwDepth : 0;
  int mipmaps = (header.dwFlags & DDS_MIPMAPCOUNT) ? header.dwMipMapCount : 1; // implies DDSCAPS_MIPMAP | DDSCAPS_COMPLEX
  int hasalpha = header.ddpfPixelFormat.dwFlags & DDPF_ALPHAPIXELS;

  // int pitch = (header.dwFlags & DDS_PITCH) ? header.dwPitchOrLinearSize : 0;
  // int linearsize = (header.dwFlags & DDS_LINEARSIZE) ? header.dwPitchOrLinearSize : 0;
  // int luminance = header.ddpfPixelFormat.dwFlags & DDPF_LUMINANCE;
  // int fourcc = (header.ddpfPixelFormat.dwFlags & DDPF_FOURCC) ? header.ddpfPixelFormat.dwFourCC : 0;
  // int indexed4 = header.ddpfPixelFormat.dwFlags & DDPF_INDEXED4;
  // int indexed8 = header.ddpfPixelFormat.dwFlags & DDPF_INDEXED8;
  // int indexedto8 = header.ddpfPixelFormat.dwFlags & DDPF_INDEXEDTO8;
  // int fcc = header.ddpfPixelFormat.dwFlags & DDPF_FOURCC;
  int rgb = header.ddpfPixelFormat.dwFlags & DDPF_RGB;
  // int alpha = header.ddpfPixelFormat.dwFlags & DDPF_ALPHA;
  int bitcount = header.ddpfPixelFormat.dwRGBBitCount;

  if (rgb && bitcount == 8)
  {
    Log::warning( Say("%s: corrupted DDS format! will try to recover...\n") << file->path() );
    header.ddpfPixelFormat.dwFlags &= ~DDPF_RGB;
    header.ddpfPixelFormat.dwFlags |= DDPF_LUMINANCE;
  }

  int max_face = 1;
  if (image_type == DDS_IMAGE_CUBEMAP)
    max_face = 6;

  std::vector< ref<Image> > image;
  for(int i=0; i<mipmaps; ++i)
  {
    image.push_back( new Image );
    image.back()->setObjectName(file->path().toStdString().c_str());
  }

  if (IS_BGRA8(header.ddpfPixelFormat) || IS_BGRX8(header.ddpfPixelFormat))
  {
    for(int i=0, w = header.dwWidth, h = header.dwHeight, d = header.dwDepth; i<mipmaps; ++i, w/=2, h/=2, d/=2)
    {
      w = w == 0 ? 1 : w;
      h = h == 0 ? 1 : h;
      d = d == 0 ? 1 : d;

      if (image_type == DDS_IMAGE_2D)
        image[i]->allocate2D(w, h, 1, IF_RGBA, IT_UNSIGNED_BYTE);
      else
      if (image_type == DDS_IMAGE_CUBEMAP)
        image[i]->allocateCubemap(w, h, 1, IF_RGBA, IT_UNSIGNED_BYTE);
      else
      if (image_type == DDS_IMAGE_3D)
        image[i]->allocate3D(w, h, d, 1, IF_RGBA, IT_UNSIGNED_BYTE);
    }

    for(int face=0; face<max_face; ++face)
    {
      for(int i=0, w = header.dwWidth, h = header.dwHeight, d = header.dwDepth; i<mipmaps; ++i, w/=2, h/=2, d/=2)
      {
        w = w == 0 ? 1 : w;
        h = h == 0 ? 1 : h;
        d = d == 0 ? 1 : d;

        int req_mem = Image::requiredMemory( w, h, d, 1, IF_RGBA, IT_UNSIGNED_BYTE, false );
        int offset = req_mem * face;
        file->read(image[i]->pixels() + offset, req_mem);

        if(reverse_rgba_bgra)
          swapBytes32_BGRA_RGBA(image[i]->pixels() + offset, req_mem);

        if (IS_BGRX8(header.ddpfPixelFormat))
          fillRGBA32_Alpha(image[i]->pixels() + offset, req_mem, 0xFF);
      }
    }
  }
  else
  if (IS_BGR8(header.ddpfPixelFormat))
  {
    for(int i=0, w = header.dwWidth, h = header.dwHeight, d = header.dwDepth; i<mipmaps; ++i, w/=2, h/=2, d/=2)
    {
      w = w == 0 ? 1 : w;
      h = h == 0 ? 1 : h;
      d = d == 0 ? 1 : d;

      if (image_type == DDS_IMAGE_2D)
        image[i]->allocate2D(w, h, 1, IF_RGB, IT_UNSIGNED_BYTE);
      else
      if (image_type == DDS_IMAGE_CUBEMAP)
        image[i]->allocateCubemap(w, h, 1, IF_RGB, IT_UNSIGNED_BYTE);
      else
      if (image_type == DDS_IMAGE_3D)
        image[i]->allocate3D(w, h, d, 1, IF_RGB, IT_UNSIGNED_BYTE);
    }

    for(int face=0; face<max_face; ++face)
    {
      for(int i=0, w = header.dwWidth, h = header.dwHeight, d = header.dwDepth; i<mipmaps; ++i, w/=2, h/=2, d/=2)
      {
        w = w == 0 ? 1 : w;
        h = h == 0 ? 1 : h;
        d = d == 0 ? 1 : d;

        int req_mem = Image::requiredMemory( w, h, d, 1, IF_RGB, IT_UNSIGNED_BYTE, false );
        int offset = req_mem * face;
        file->read(image[i]->pixels() + offset, req_mem);

        if(reverse_rgba_bgra)
          swapBytes24_BGR_RGB(image[i]->pixels() + offset, req_mem);
      }
    }
  }
  else
  if (IS_GRAY8(header.ddpfPixelFormat))
  {
    for(int i=0, w = header.dwWidth, h = header.dwHeight, d = header.dwDepth; i<mipmaps; ++i, w/=2, h/=2, d/=2)
    {
      w = w == 0 ? 1 : w;
      h = h == 0 ? 1 : h;
      d = d == 0 ? 1 : d;

      if (image_type == DDS_IMAGE_2D)
        image[i]->allocate2D(w, h, 1, IF_LUMINANCE, IT_UNSIGNED_BYTE);
      else
      if (image_type == DDS_IMAGE_CUBEMAP)
        image[i]->allocateCubemap(w, h, 1, IF_LUMINANCE, IT_UNSIGNED_BYTE);
      else
      if (image_type == DDS_IMAGE_3D)
        image[i]->allocate3D(w, h, d, 1, IF_LUMINANCE, IT_UNSIGNED_BYTE);
    }

    for(int face=0; face<max_face; ++face)
    {
      for(int i=0, w = header.dwWidth, h = header.dwHeight, d = header.dwDepth; i<mipmaps; ++i, w/=2, h/=2, d/=2)
      {
        w = w == 0 ? 1 : w;
        h = h == 0 ? 1 : h;
        d = d == 0 ? 1 : d;

        int req_mem = Image::requiredMemory( w, h, d, 1, IF_LUMINANCE, IT_UNSIGNED_BYTE, false );
        int offset = req_mem*face;
        file->read(image[i]->pixels() + offset, req_mem);
      }
    }
  }
  else
  if (IS_GRAY8_ALPHA8(header.ddpfPixelFormat))
  {
    for(int i=0, w = header.dwWidth, h = header.dwHeight, d = header.dwDepth; i<mipmaps; ++i, w/=2, h/=2, d/=2)
    {
      w = w == 0 ? 1 : w;
      h = h == 0 ? 1 : h;
      d = d == 0 ? 1 : d;

      if (image_type == DDS_IMAGE_2D)
        image[i]->allocate2D(w, h, 1, IF_LUMINANCE_ALPHA, IT_UNSIGNED_BYTE);
      else
      if (image_type == DDS_IMAGE_CUBEMAP)
        image[i]->allocateCubemap(w, h, 1, IF_LUMINANCE_ALPHA, IT_UNSIGNED_BYTE);
      else
      if (image_type == DDS_IMAGE_3D)
        image[i]->allocate3D(w, h, d, 1, IF_LUMINANCE_ALPHA, IT_UNSIGNED_BYTE);
    }

    for(int face=0; face<max_face; ++face)
    {
      for(int i=0, w = header.dwWidth, h = header.dwHeight, d = header.dwDepth; i<mipmaps; ++i, w/=2, h/=2, d/=2)
      {
        w = w == 0 ? 1 : w;
        h = h == 0 ? 1 : h;
        d = d == 0 ? 1 : d;

        int req_mem = Image::requiredMemory( w, h, d, 1, IF_LUMINANCE_ALPHA, IT_UNSIGNED_BYTE, false );
        int offset = req_mem*face;
        file->read(image[i]->pixels() + offset, req_mem);

        if (!hasalpha)
          fillGray8Alpha8_Alpha(image[i]->pixels() + offset, req_mem, 0xFF);
      }
    }
  }
  else
  if (IS_PALETTE8(header.ddpfPixelFormat))
  {
    for(int i=0, w = header.dwWidth, h = header.dwHeight, d = header.dwDepth; i<mipmaps; ++i, w/=2, h/=2, d/=2)
    {
      w = w == 0 ? 1 : w;
      h = h == 0 ? 1 : h;
      d = d == 0 ? 1 : d;

      if (image_type == DDS_IMAGE_2D)
        image[i]->allocate2D(w, h, 1, IF_RGBA, IT_UNSIGNED_BYTE);
      else
      if (image_type == DDS_IMAGE_CUBEMAP)
        image[i]->allocateCubemap(w, h, 1, IF_RGBA, IT_UNSIGNED_BYTE);
      else
      if (image_type == DDS_IMAGE_3D)
        image[i]->allocate3D(w, h, d, 1, IF_RGBA, IT_UNSIGNED_BYTE);
    }

    for(int face=0; face<max_face; ++face)
    {
      TPalette4x256 palette;
      file->read( palette, sizeof(TPalette4x256) );
      for(int i=0, w = header.dwWidth, h = header.dwHeight, d = header.dwDepth; i<mipmaps; ++i, w/=2, h/=2, d/=2)
      {
        w = w == 0 ? 1 : w;
        h = h == 0 ? 1 : h;
        d = d == 0 ? 1 : d;

        int req_mem1 = w*h*d*1;
        int req_mem4 = w*h*d*4;
        int offset = req_mem4*face;
        // read the palette first
        file->read( image[i]->pixels() + offset, req_mem1 );
        convert8ToRGBA( palette, image[i]->pixels() + offset, w, h * d );
        swapBytes32_BGRA_RGBA(image[i]->pixels() + offset, req_mem4);
        if (!hasalpha)
          fillRGBA32_Alpha(image[i]->pixels() + offset, req_mem4, 0xFF);
      }
    }
  }
  else
  if ( IS_DXT1(header.ddpfPixelFormat) )
  {
    EImageFormat DXT1 = hasalpha ? IF_COMPRESSED_RGBA_S3TC_DXT1 : IF_COMPRESSED_RGB_S3TC_DXT1;

    for(int i=0, w = header.dwWidth, h = header.dwHeight, d = header.dwDepth; i<mipmaps; ++i, w/=2, h/=2, d/=2)
    {
      w = w == 0 ? 1 : w;
      h = h == 0 ? 1 : h;
      d = d == 0 ? 1 : d;

      if (image_type == DDS_IMAGE_2D)
        image[i]->allocate2D(w, h, 1, DXT1, IT_IMPLICIT_TYPE);
      else
      if (image_type == DDS_IMAGE_CUBEMAP)
        image[i]->allocateCubemap(w, h, 1, DXT1, IT_IMPLICIT_TYPE);
      else
      if (image_type == DDS_IMAGE_3D)
        image[i]->allocate3D(w, h, d, 1, DXT1, IT_IMPLICIT_TYPE);
    }

    for(int face=0; face<max_face; ++face)
    {
      for(int i=0, w = header.dwWidth, h = header.dwHeight, d = header.dwDepth; i<mipmaps; ++i, w/=2, h/=2, d/=2)
      {
        w = w == 0 ? 1 : w;
        h = h == 0 ? 1 : h;
        d = d == 0 ? 1 : d;

        int req_mem = Image::requiredMemory( w, h, d, 1, DXT1, IT_IMPLICIT_TYPE, false );
        int offset = req_mem*face;
        file->read(image[i]->pixels() + offset, req_mem);
      }
    }
  }
  else
  if ( IS_DXT3(header.ddpfPixelFormat) )
  {
    for(int i=0, w = header.dwWidth, h = header.dwHeight, d = header.dwDepth; i<mipmaps; ++i, w/=2, h/=2, d/=2)
    {
      w = w == 0 ? 1 : w;
      h = h == 0 ? 1 : h;
      d = d == 0 ? 1 : d;

      if (image_type == DDS_IMAGE_2D)
        image[i]->allocate2D(w, h, 1, IF_COMPRESSED_RGBA_S3TC_DXT3, IT_IMPLICIT_TYPE);
      else
      if (image_type == DDS_IMAGE_CUBEMAP)
        image[i]->allocateCubemap(w, h, 1, IF_COMPRESSED_RGBA_S3TC_DXT3, IT_IMPLICIT_TYPE);
      else
      if (image_type == DDS_IMAGE_3D)
        image[i]->allocate3D(w, h, d, 1, IF_COMPRESSED_RGBA_S3TC_DXT3, IT_IMPLICIT_TYPE);
    }

    for(int face=0; face<max_face; ++face)
    {
      for(int i=0, w = header.dwWidth, h = header.dwHeight, d = header.dwDepth; i<mipmaps; ++i, w/=2, h/=2, d/=2)
      {
        w = w == 0 ? 1 : w;
        h = h == 0 ? 1 : h;
        d = d == 0 ? 1 : d;

        int req_mem = Image::requiredMemory( w, h, d, 1, IF_COMPRESSED_RGBA_S3TC_DXT3, IT_IMPLICIT_TYPE, false );
        int offset = req_mem*face;
        file->read(image[i]->pixels() + offset, req_mem);
      }
    }
  }
  else
  if ( IS_DXT5(header.ddpfPixelFormat) )
  {
    for(int i=0, w = header.dwWidth, h = header.dwHeight, d = header.dwDepth; i<mipmaps; ++i, w/=2, h/=2, d/=2)
    {
      w = w == 0 ? 1 : w;
      h = h == 0 ? 1 : h;
      d = d == 0 ? 1 : d;

      if (image_type == DDS_IMAGE_2D)
        image[i]->allocate2D(w, h, 1, IF_COMPRESSED_RGBA_S3TC_DXT5, IT_IMPLICIT_TYPE);
      else
      if (image_type == DDS_IMAGE_CUBEMAP)
        image[i]->allocateCubemap(w, h, 1, IF_COMPRESSED_RGBA_S3TC_DXT5, IT_IMPLICIT_TYPE);
      else
      if (image_type == DDS_IMAGE_3D)
        image[i]->allocate3D(w, h, d, 1, IF_COMPRESSED_RGBA_S3TC_DXT5, IT_IMPLICIT_TYPE);
    }

    for(int face=0; face<max_face; ++face)
    {
      for(int i=0, w = header.dwWidth, h = header.dwHeight, d = header.dwDepth; i<mipmaps; ++i, w/=2, h/=2, d/=2)
      {
        w = w == 0 ? 1 : w;
        h = h == 0 ? 1 : h;
        d = d == 0 ? 1 : d;

        int req_mem = Image::requiredMemory( w, h, d, 1, IF_COMPRESSED_RGBA_S3TC_DXT5, IT_IMPLICIT_TYPE, false );
        int offset = req_mem*face;
        file->read(image[i]->pixels() + offset, req_mem);
      }
    }
  }
  else
  {
    Log::error( Say("DDS: not supported format for '%s'.\n") << file->path() );
    file->close();
    return NULL;
  }

  file->close();

  // We don't flip at all the DDS to be consistent since flipping
  // vertically/horizontally compressed images is not possible.
  // The textures must be already flipped using the OpenGL reference system.

  #if 0
    if (image_type == DDS_IMAGE_2D && !s3tc_compressed)
    {
      for (int i=0; i<(int)image.size(); ++i)
        internal_FlipVertically(image[i]);
    }
    else
    if (image_type == DDS_IMAGE_3D)
    {
      // should be flipped vertically and then
      // invert their order along the z-axis
      // ...
    }
    else
    if (image_type == DDS_IMAGE_CUBEMAP)
    {
      // should be flipped in a more sofisticated way, sg like:
      // X+- flip orizontally
      // Y+- flip vertically
      // Z+- exchange one with the other
      // ...
    }
  #endif

  VL_CHECK(image.size());

  ref<Image> img = new Image;
  img->setObjectName(file->path().toStdString().c_str());
  *img = *image[0];
  image.erase(image.begin());
  img->setMipmaps(image);

  return img;
}
//-----------------------------------------------------------------------------
bool vl::isDDS(VirtualFile* file)
{
  if (!file->open(OM_ReadOnly))
    return false;

  char signature[4];
  file->read(signature, 4);
  if (strncmp(signature, "DDS ", 4) != 0)
  {
    file->close();
    return false;
  }

  DDSURFACEDESC2 header;
  memset(&header, 0, sizeof(header));

  // fread(&header, 1, sizeof(header), fin);

  header.dwSize = file->readUInt32();
  header.dwFlags = file->readUInt32();
  header.dwHeight = file->readUInt32();
  header.dwWidth = file->readUInt32();
  header.dwPitchOrLinearSize = file->readUInt32();
  header.dwDepth = file->readUInt32();
  header.dwMipMapCount = file->readUInt32();
  // fread(header.dwReserved1, 1, 11*sizeof(unsigned long), fin);
  file->read(header.dwReserved1, 11*sizeof(unsigned long));
  header.ddpfPixelFormat.dwSize = file->readUInt32();
  header.ddpfPixelFormat.dwFlags = file->readUInt32();
  header.ddpfPixelFormat.dwFourCC = file->readUInt32();
  header.ddpfPixelFormat.dwRGBBitCount = file->readUInt32();
  header.ddpfPixelFormat.dwRBitMask = file->readUInt32();
  header.ddpfPixelFormat.dwGBitMask = file->readUInt32();
  header.ddpfPixelFormat.dwBBitMask = file->readUInt32();
  header.ddpfPixelFormat.dwAlphaBitMask = file->readUInt32();
  header.ddsCaps.dwCaps1 = file->readUInt32();
  header.ddsCaps.dwCaps2 = file->readUInt32();
  header.ddsCaps.dwReserved[0] = file->readUInt32();
  header.ddsCaps.dwReserved[1] = file->readUInt32();
  header.dwReserved2 = file->readUInt32();

  file->close();

  if (header.dwSize != 124 || header.ddpfPixelFormat.dwSize != 32)
    return false;

  // warn only when actually loading
#if 0
  if ((header.dwFlags & DDS_REQUIRED_FLAGS) != DDS_REQUIRED_FLAGS )
    Log::warning( Say("DDS file '%s': missing DDS_REQUIRED_FLAGS flags.\n") << file->path() );

  if ((header.ddsCaps.dwCaps1 & DDSCAPS_TEXTURE) != DDSCAPS_TEXTURE)
    Log::warning( Say("DDS file '%s': missing DDSCAPS_TEXTURE flag.\n") << file->path() );
#endif

  return true;
}
//-----------------------------------------------------------------------------
