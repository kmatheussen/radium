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

#ifndef ImageTools_INCLUDE_ONCE
#define ImageTools_INCLUDE_ONCE

#include <memory.h>

namespace vl
{
//-----------------------------------------------------------------------------
  typedef unsigned char TPalette3x256[256*3];
  typedef unsigned char TPalette4x256[256*4];
//-----------------------------------------------------------------------------
  inline void convertRGBToRGBA(void* buf, int w, int h, unsigned char alpha, int bytealign = 1)
  {
    int xbytes = w*3;
    int pitch = (xbytes / bytealign * bytealign) + ((xbytes % bytealign)? bytealign : 0);

    if(bytealign)
    {
      // compact the image

      unsigned char *pxl1 = (unsigned char *)buf;
      unsigned char *pxl2 = (unsigned char *)buf;
      int count = 0;
      for(int i=0; i<pitch*h; ++i, count++)
      {
        if (count==pitch)
          count = 0;

        *pxl1 = *pxl2;

        if (count < w*3)
          pxl1 ++;

        pxl2 ++;
      }
    }

    unsigned char * px32 = (unsigned char*)buf + w * h * 4 - 4;
    unsigned char * px24 = (unsigned char*)buf + w * h * 3 - 3;
    for(int i=0; i<w * h; ++i)
    {
      px32[0] = px24[0];
      px32[1] = px24[1];
      px32[2] = px24[2];
      px32[3] = alpha;
      px24 -= 3;
      px32 -= 4;
    }
  }
//-----------------------------------------------------------------------------
  inline void convertGrayscaleToRGBA(void* buf, int size, unsigned char alpha)
  {
    unsigned char* px32 = (unsigned char*)buf + size * 4 - 4;
    unsigned char* px8  = (unsigned char*)buf + size * 1 - 1;
    for(int i=0; i<size; ++i)
    {
      px32[0] = *px8;
      px32[1] = *px8;
      px32[2] = *px8;
      px32[3] = alpha;
      px8 -= 1;
      px32 -= 4;
    }
  }
//-----------------------------------------------------------------------------
  inline void convertA1R5G5B5ToRGBA(void* buf, int size, unsigned char alpha)
  {
    unsigned char* px32 = (unsigned char*)buf + size * 4 - 4;
    unsigned char* px8  = (unsigned char*)buf + size * 2 - 2;
    for(int i=0; i<size; ++i)
    {
      unsigned char r = (px8[1] << 1) & ~0x03;
      unsigned char g = ((px8[1] << 6) | (px8[0] >> 2)) & ~0x03;
      unsigned char b = (px8[0] << 3) & ~0x03;
      // photoshop sets it to zero
      // int a = (px8[1] & 0x80) ? 0xFF : 0;
      px32[0] = r;
      px32[1] = g;
      px32[2] = b;
      px32[3] = alpha;
      px8  -= 2;
      px32 -= 4;
    }
  }
//-----------------------------------------------------------------------------
  inline void convert8ToRGBA(const TPalette3x256 & palette, void* buf, int w, int h, unsigned char alpha, int bytealign = 1)
  {

    int xbytes = w;
    int pitch = (xbytes / bytealign * bytealign) + ((xbytes % bytealign)? bytealign : 0);

    if(bytealign)
    {
      // compact the image

      unsigned char *pxl1 = (unsigned char *)buf;
      unsigned char *pxl2 = (unsigned char *)buf;
      int count = 0;
      for(int i=0; i<pitch*h; ++i, count++)
      {
        if (count==pitch)
          count = 0;

        *pxl1 = *pxl2;

        if (count < w)
          pxl1 ++;

        pxl2 ++;
      }
    }

    unsigned char* px32 = (unsigned char*)buf + w * h * 4 - 4;
    unsigned char* px8  = (unsigned char*)buf + w * h * 1 - 1;
    for(int i=0; i<w * h; ++i)
    {
      px32[0] = palette[*px8*3+0];
      px32[1] = palette[*px8*3+1];
      px32[2] = palette[*px8*3+2];
      px32[3] = alpha;
      px8  -= 1;
      px32 -= 4;
    }
  }
//-----------------------------------------------------------------------------
  inline void convert8ToRGBA(const TPalette4x256 & palette, void* buf, int w, int h, int bytealign = 1)
  {

    int xbytes = w;
    int pitch = (xbytes / bytealign * bytealign) + ((xbytes % bytealign)? bytealign : 0);

    if(bytealign)
    {
      // compact the image

      unsigned char *pxl1 = (unsigned char *)buf;
      unsigned char *pxl2 = (unsigned char *)buf;
      int count = 0;
      for(int i=0; i<pitch*h; ++i, count++)
      {
        if (count==pitch)
          count = 0;

        *pxl1 = *pxl2;

        if (count < w)
          pxl1 ++;

        pxl2 ++;
      }
    }

    unsigned char * px32 = (unsigned char*)buf + w * h * 4 - 4;
    unsigned char * px8  = (unsigned char*)buf + w * h * 1 - 1;
    for(int i=0; i<w * h; ++i)
    {
      px32[0] = palette[*px8*4+0];
      px32[1] = palette[*px8*4+1];
      px32[2] = palette[*px8*4+2];
      px32[3] = palette[*px8*4+3];
      px8  -= 1;
      px32 -= 4;
    }
  }
//-----------------------------------------------------------------------------
  inline void swapBytes32(void* buf, int size)
  {
    unsigned char* p = (unsigned char*)buf;
    unsigned char dw[4];
    for(int i=0; i<size; i+=4, p+=4)
    {
      memcpy(dw, p, 4);
      p[0] = dw[3];
      p[3] = dw[0];
      p[1] = dw[2];
      p[2] = dw[1];
    }
  }
//-----------------------------------------------------------------------------
  inline void swapBytes32_BGRA_RGBA(void* buf, int bytecount)
  {
    unsigned char* p = (unsigned char*)buf;
    unsigned char dw[4];
    for(int i=0; i<bytecount; i+=4, p+=4)
    {
      memcpy(dw, p, 4);
      p[0] = dw[2];
      p[2] = dw[0];
    }
  }
//-----------------------------------------------------------------------------
  inline void swapBytes24_BGR_RGB(void* buf, int bytecount)
  {
    unsigned char* p = (unsigned char*)buf;
    unsigned char dw[4];
    int pxl = bytecount / 3;
    for(int i=0; i<pxl; ++i, p+=3)
    {
      memcpy(dw, p, 3);
      p[0] = dw[2];
      p[2] = dw[0];
    }
  }
//-----------------------------------------------------------------------------
  inline void fillRGBA32_Alpha(void* buf, int bytecount, unsigned char alpha)
  {
    unsigned char* pxl = (unsigned char*)buf;
    for(int i=0; i<bytecount; i+=4)
    {
      pxl[i+3] = alpha;
    }
  }
//-----------------------------------------------------------------------------
  inline void fillGray8Alpha8_Alpha(void* buf, int bytecount, unsigned char alpha)
  {
    unsigned char* pxl = (unsigned char*)buf;
    for(int i=0; i<bytecount; i+=2)
    {
      pxl[i+1] = alpha;
    }
  }
//-----------------------------------------------------------------------------
}

#endif
