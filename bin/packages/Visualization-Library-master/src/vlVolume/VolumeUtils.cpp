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

#include <vlVolume/VolumeUtils.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/glsl_math.hpp>

using namespace vl;

//-----------------------------------------------------------------------------
ref<Image> vl::genRGBAVolume(const Image* data, const Image* trfunc, const fvec3& light_dir, bool alpha_from_data)
{
  ref<Image> img;

  if(data->type() == IT_UNSIGNED_BYTE)
    img = genRGBAVolumeT<unsigned char,IT_UNSIGNED_BYTE>(data,trfunc,light_dir,alpha_from_data);
  else
  if(data->type() == IT_UNSIGNED_SHORT)
    img = genRGBAVolumeT<unsigned short,IT_UNSIGNED_SHORT>(data,trfunc,light_dir,alpha_from_data);
  else
  if(data->type() == IT_FLOAT)
    img = genRGBAVolumeT<float,IT_FLOAT>(data,trfunc,light_dir,alpha_from_data);
  else
    Log::error("genRGBAVolume() called with non supported data type().\n");

  return img;
}
//-----------------------------------------------------------------------------
ref<Image> vl::genRGBAVolume(const Image* data, const Image* trfunc, bool alpha_from_data)
{
  ref<Image> img;

  if(data->type() == IT_UNSIGNED_BYTE)
    img = genRGBAVolumeT<unsigned char,IT_UNSIGNED_BYTE>(data,trfunc,alpha_from_data);
  else
  if(data->type() == IT_UNSIGNED_SHORT)
    img = genRGBAVolumeT<unsigned short,IT_UNSIGNED_SHORT>(data,trfunc,alpha_from_data);
  else
  if(data->type() == IT_FLOAT)
    img = genRGBAVolumeT<float,IT_FLOAT>(data,trfunc,alpha_from_data);
  else
    Log::error("genRGBAVolume() called with non supported data type().\n");

  return img;
}
//-----------------------------------------------------------------------------
template<typename data_type, EImageType img_type>
ref<Image> vl::genRGBAVolumeT(const Image* data, const Image* trfunc, const fvec3& light_dir, bool alpha_from_data)
{
  if (!trfunc || !data)
    return NULL;
  if (data->format() != IF_LUMINANCE)
  {
    Log::error("genRGBAVolume() called with non IF_LUMINANCE data format().\n");
    return NULL;
  }
  if (data->type() != img_type)
  {
    Log::error("genRGBAVolume() called with invalid data type().\n");
    return NULL;
  }
  if (data->dimension() != ID_3D)
  {
    Log::error("genRGBAVolume() called with non 3D data.\n");
    return NULL;
  }
  if (trfunc->dimension() != ID_1D)
  {
    Log::error("genRGBAVolume() transfer function image must be an 1D image.\n");
    return NULL;
  }
  if (trfunc->format() != IF_RGBA)
  {
    Log::error("genRGBAVolume() transfer function format() must be IF_RGBA.\n");
    return NULL;
  }
  if (trfunc->type() != IT_UNSIGNED_BYTE)
  {
    Log::error("genRGBAVolume() transfer function format() must be IT_UNSIGNED_BYTE.\n");
    return NULL;
  }

  float normalizer_num = 0;
  switch(data->type())
  {
    case IT_UNSIGNED_BYTE:  normalizer_num = 1.0f/255.0f;   break;
    case IT_UNSIGNED_SHORT: normalizer_num = 1.0f/65535.0f; break;
    case IT_FLOAT:          normalizer_num = 1.0f;          break;
    default:
      break;
  }

  // light normalization
  fvec3 L = light_dir;
  L.normalize();
  int w = data->width();
  int h = data->height();
  int d = data->depth();
  int pitch = data->pitch();
  const unsigned char* lum_px = data->pixels();
  // generated volume
  ref<Image> volume = new Image( w, h, d, 1, IF_RGBA, IT_UNSIGNED_BYTE );
  ubvec4* rgba_px = (ubvec4*)volume->pixels();
  for(int z=0; z<d; ++z)
  {
    int z1 = z-1;
    int z2 = z+1;
    z1 = clamp(z1, 0, d-1);
    z2 = clamp(z2, 0, d-1);
    for(int y=0; y<h; ++y)
    {
      int y1 = y-1;
      int y2 = y+1;
      y1 = clamp(y1, 0, h-1);
      y2 = clamp(y2, 0, h-1);
      for(int x=0; x<w; ++x, ++rgba_px)
      {
        // value
        float lum = (*(data_type*)(lum_px + x*sizeof(data_type) + y*pitch + z*pitch*h)) * normalizer_num;
        // value -> transfer function
        float xval = lum*trfunc->width();
        VL_CHECK(xval>=0)
        if (xval > trfunc->width()-1.001f)
          xval = trfunc->width()-1.001f;
        int ix1 = (int)xval;
        int ix2 = ix1+1;
        VL_CHECK(ix2<trfunc->width())
        float w21  = (float)fract(xval);
        float w11  = 1.0f - w21;
        fvec4 c11  = (fvec4)((ubvec4*)trfunc->pixels())[ix1];
        fvec4 c21  = (fvec4)((ubvec4*)trfunc->pixels())[ix2];
        fvec4 rgba = (c11*w11 + c21*w21)*(1.0f/255.0f);

        // bake the lighting
        int x1 = x-1;
        int x2 = x+1;
        x1 = clamp(x1, 0, w-1);
        x2 = clamp(x2, 0, w-1);
        data_type vx1 = (*(data_type*)(lum_px + x1*sizeof(data_type) + y *pitch + z *pitch*h));
        data_type vx2 = (*(data_type*)(lum_px + x2*sizeof(data_type) + y *pitch + z *pitch*h));
        data_type vy1 = (*(data_type*)(lum_px + x *sizeof(data_type) + y1*pitch + z *pitch*h));
        data_type vy2 = (*(data_type*)(lum_px + x *sizeof(data_type) + y2*pitch + z *pitch*h));
        data_type vz1 = (*(data_type*)(lum_px + x *sizeof(data_type) + y *pitch + z1*pitch*h));
        data_type vz2 = (*(data_type*)(lum_px + x *sizeof(data_type) + y *pitch + z2*pitch*h));
        fvec3 N1(float(vx1-vx2), float(vy1-vy2), float(vz1-vz2));
        N1.normalize();
        fvec3 N2 = -N1 * 0.15f;
        float l1 = max(dot(N1,L),0.0f);
        float l2 = max(dot(N2,L),0.0f); // opposite dim light to enhance 3D perception
        rgba.r() = rgba.r()*l1 + rgba.r()*l2+0.2f; // +0.2f = ambient light
        rgba.g() = rgba.g()*l1 + rgba.g()*l2+0.2f;
        rgba.b() = rgba.b()*l1 + rgba.b()*l2+0.2f;
        rgba.r() = clamp(rgba.r(), 0.0f, 1.0f);
        rgba.g() = clamp(rgba.g(), 0.0f, 1.0f);
        rgba.b() = clamp(rgba.b(), 0.0f, 1.0f);

        // map pixel
        rgba_px->r() = (unsigned char)(rgba.r()*255.0f);
        rgba_px->g() = (unsigned char)(rgba.g()*255.0f);
        rgba_px->b() = (unsigned char)(rgba.b()*255.0f);
        if (alpha_from_data)
          rgba_px->a() = (unsigned char)(lum*255.0f);
        else
          rgba_px->a() = (unsigned char)(rgba.a()*255.0f);
      }
    }
  }

  return volume;
}
//-----------------------------------------------------------------------------
template<typename data_type, EImageType img_type>
ref<Image> vl::genRGBAVolumeT(const Image* data, const Image* trfunc, bool alpha_from_data)
{
  if (!trfunc || !data)
    return NULL;
  if (data->format() != IF_LUMINANCE)
  {
    Log::error("genRGBAVolume() called with non IF_LUMINANCE data format().\n");
    return NULL;
  }
  if (data->type() != img_type)
  {
    Log::error("genRGBAVolume() called with invalid data type().\n");
    return NULL;
  }  
  if (data->dimension() != ID_3D)
  {
    Log::error("genRGBAVolume() called with non 3D data.\n");
    return NULL;
  }
  if (trfunc->dimension() != ID_1D)
  {
    Log::error("genRGBAVolume() transfer function image must be an 1D image.\n");
    return NULL;
  }
  if (trfunc->format() != IF_RGBA)
  {
    Log::error("genRGBAVolume() transfer function format() must be IF_RGBA.\n");
    return NULL;
  }
  if (trfunc->type() != IT_UNSIGNED_BYTE)
  {
    Log::error("genRGBAVolume() transfer function format() must be IT_UNSIGNED_BYTE.\n");
    return NULL;
  }

  float normalizer_num = 0;
  switch(data->type())
  {
    case IT_UNSIGNED_BYTE:  normalizer_num = 1.0f/255.0f;   break;
    case IT_UNSIGNED_SHORT: normalizer_num = 1.0f/65535.0f; break;
    case IT_FLOAT:          normalizer_num = 1.0f;          break;
    default:
      break;
  }

  // light normalization
  int w = data->width();
  int h = data->height();
  int d = data->depth();
  int pitch = data->pitch();
  const unsigned char* lum_px = data->pixels();
  // generated volume
  ref<Image> volume = new Image( w, h, d, 1, IF_RGBA, IT_UNSIGNED_BYTE );
  ubvec4* rgba_px = (ubvec4*)volume->pixels();
  for(int z=0; z<d; ++z)
  {
    int z1 = z-1;
    int z2 = z+1;
    z1 = clamp(z1, 0, d-1);
    z2 = clamp(z2, 0, d-1);
    for(int y=0; y<h; ++y)
    {
      int y1 = y-1;
      int y2 = y+1;
      y1 = clamp(y1, 0, h-1);
      y2 = clamp(y2, 0, h-1);
      for(int x=0; x<w; ++x, ++rgba_px)
      {
        // value
        float lum = (*(data_type*)(lum_px + x*sizeof(data_type) + y*pitch + z*pitch*h)) * normalizer_num;
        // value -> transfer function
        float xval = lum*trfunc->width();
        VL_CHECK(xval>=0)
        if (xval > trfunc->width()-1.001f)
          xval = trfunc->width()-1.001f;
        int ix1 = (int)xval;
        int ix2 = ix1+1;
        VL_CHECK(ix2<trfunc->width())
        float w21  = (float)fract(xval);
        float w11  = 1.0f - w21;
        fvec4 c11  = (fvec4)((ubvec4*)trfunc->pixels())[ix1];
        fvec4 c21  = (fvec4)((ubvec4*)trfunc->pixels())[ix2];
        fvec4 rgba = (c11*w11 + c21*w21)*(1.0f/255.0f);

        // map pixel
        rgba_px->r() = (unsigned char)(rgba.r()*255.0f);
        rgba_px->g() = (unsigned char)(rgba.g()*255.0f);
        rgba_px->b() = (unsigned char)(rgba.b()*255.0f);
        if (alpha_from_data)
          rgba_px->a() = (unsigned char)(lum*255.0f);
        else
          rgba_px->a() = (unsigned char)(rgba.a()*255.0f);
      }
    }
  }

  return volume;
}
//-----------------------------------------------------------------------------
ref<Image> vl::genGradientNormals(const Image* img)
{
  ref<Image> gradient = new Image;
  gradient->allocate3D(img->width(), img->height(), img->depth(), 1, IF_RGB, IT_FLOAT);
  fvec3* px = (fvec3*)gradient->pixels();
  fvec3 A, B;
  for(int z=0; z<gradient->depth(); ++z)
  {
    for(int y=0; y<gradient->height(); ++y)
    {
      for(int x=0; x<gradient->width(); ++x)
      {
        // clamped coordinates
        int xp = x+1, xn = x-1;
        int yp = y+1, yn = y-1;
        int zp = z+1, zn = z-1;
        if (xn<0) xn = 0;
        if (yn<0) yn = 0;
        if (zn<0) zn = 0;
        if (xp>img->width() -1) xp = img->width() -1;
        if (yp>img->height()-1) yp = img->height()-1;
        if (zp>img->depth() -1) zp = img->depth() -1;

        A.x() = img->sample(xn,y,z).r();
        B.x() = img->sample(xp,y,z).r();
        A.y() = img->sample(x,yn,z).r();
        B.y() = img->sample(x,yp,z).r();
        A.z() = img->sample(x,y,zn).r();
        B.z() = img->sample(x,y,zp).r();

        // write normal packed into 0..1 format
        px[x + img->width()*y + img->width()*img->height()*z] = normalize(A - B) * 0.5f + 0.5f;
      }
    }
  }
  return gradient;
}
//-----------------------------------------------------------------------------
