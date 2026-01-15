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

#ifndef vlxutils_INCLUDE_ONCE
#define vlxutils_INCLUDE_ONCE

#include <vlCore/VLXValue.hpp>
#include <vlCore/Matrix4.hpp>
#include <vlCore/Vector4.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>
#include <vlCore/VLXSerializer.hpp>

namespace vl
{
  inline std::string vlx_makeTag(const Object* obj) { return std::string("<") + obj->classType().name() + ">"; }

  inline VLXValue vlx_Identifier(const std::string& str) { return VLXValue(str.c_str(), VLXValue::Identifier); }

  inline VLXValue vlx_ID(const std::string& str)        { return VLXValue(str.c_str(), VLXValue::ID); }

  inline VLXValue vlx_String(const std::string& str)     { return VLXValue(str.c_str(), VLXValue::String); }

  inline VLXValue vlx_Rawtext(const std::string& str)    { return VLXValue( new VLXRawtextBlock(NULL, str.c_str()) ); }

  inline vec2 vlx_vec2(const VLXArrayReal* arr) { VL_CHECK(arr->value().size() == 2); vec2 v; arr->copyTo(v.ptr()); return v;  }

  inline vec3 vlx_vec3(const VLXArrayReal* arr) { VL_CHECK(arr->value().size() == 3); vec3 v; arr->copyTo(v.ptr()); return v;  }

  inline vec4 vlx_vec4(const VLXArrayReal* arr) { VL_CHECK(arr->value().size() == 4); vec4 v; arr->copyTo(v.ptr()); return v;  }

  inline ivec4 vlx_ivec4(const VLXArrayInteger* arr) { VL_CHECK(arr->value().size() == 4); ivec4 v; arr->copyTo(v.ptr()); return v; }

  inline uvec4 vlx_uivec4(const VLXArrayInteger* arr) { VL_CHECK(arr->value().size() == 4); uvec4 v; arr->copyTo(v.ptr()); return v; }

  inline VLXValue vlx_toValue(const std::vector<int>& vec)
  {
    VLXValue value;
    value.setArray( new VLXArrayInteger );
    value.getArrayInteger()->value().resize( vec.size() );
    if (vec.size())
      value.getArrayInteger()->copyFrom(&vec[0]);
    return value;
  }

  inline VLXValue vlx_toValue(const vec4& vec)
  {
    VLXValue val( new VLXArrayReal );
    VLXArrayReal* arr = val.getArrayReal();
    arr->value().resize(4);
    arr->value()[0] = vec.x();
    arr->value()[1] = vec.y();
    arr->value()[2] = vec.z();
    arr->value()[3] = vec.w();
    return val;
  }

  inline VLXValue vlx_toValue(const ivec4& vec)
  {
    VLXValue val( new VLXArrayInteger );
    VLXArrayInteger* arr = val.getArrayInteger();
    arr->value().resize(4);
    arr->value()[0] = vec.x();
    arr->value()[1] = vec.y();
    arr->value()[2] = vec.z();
    arr->value()[3] = vec.w();
    return val;
  }

  inline VLXValue vlx_toValue(const uvec4& vec)
  {
    VLXValue val( new VLXArrayInteger );
    VLXArrayInteger* arr = val.getArrayInteger();
    arr->value().resize(4);
    arr->value()[0] = vec.x();
    arr->value()[1] = vec.y();
    arr->value()[2] = vec.z();
    arr->value()[3] = vec.w();
    return val;
  }

  inline VLXValue vlx_toValue(const vec3& vec)
  {
    VLXValue val( new VLXArrayReal );
    VLXArrayReal* arr = val.getArrayReal();
    arr->value().resize(3);
    arr->value()[0] = vec.x();
    arr->value()[1] = vec.y();
    arr->value()[2] = vec.z();
    return val;
  }

  inline VLXValue vlx_toValue(const vec2& vec)
  {
    VLXValue val( new VLXArrayReal );
    VLXArrayReal* arr = val.getArrayReal();
    arr->value().resize(2);
    arr->value()[0] = vec.x();
    arr->value()[1] = vec.y();
    return val;
  }

  inline bool vlx_isTranslation(const mat4& mat)
  {
    mat4 tmp = mat;
    tmp.setT( vec3(0,0,0) );
    return tmp.isIdentity();
  }

  inline bool vlx_isScaling(const mat4& mat)
  {
    mat4 tmp = mat;
    tmp.e(0,0) = 1;
    tmp.e(1,1) = 1;
    tmp.e(2,2) = 1;
    return tmp.isIdentity();
  }

  inline VLXValue vlx_toValue(const mat4& mat)
  {
    VLXValue matrix_list( new VLXList );

    if (vlx_isTranslation(mat))
    {
      VLXValue value( new VLXArrayReal("<Translate>") );
      value.getArrayReal()->value().resize(3);
      value.getArrayReal()->value()[0] = mat.getT().x();
      value.getArrayReal()->value()[1] = mat.getT().y();
      value.getArrayReal()->value()[2] = mat.getT().z();
      matrix_list.getList()->value().push_back( value );
    }
    else
    if (vlx_isScaling(mat))
    {
      VLXValue value( new VLXArrayReal("<Scale>") );
      value.getArrayReal()->value().resize(3);
      value.getArrayReal()->value()[0] = mat.e(0,0);
      value.getArrayReal()->value()[1] = mat.e(1,1);
      value.getArrayReal()->value()[2] = mat.e(2,2);
      matrix_list.getList()->value().push_back( value );
    }
    else
    {
      VLXValue value( new VLXArrayReal("<Matrix>") );
      value.getArrayReal()->value().resize(4*4);
      // if we transpose this we have to transpose also the uniform matrices
      value.getArrayReal()->copyFrom(mat.ptr());
      matrix_list.getList()->value().push_back( value );
    }

    return matrix_list;
  }

  inline mat4 vlx_mat4( const VLXArrayReal* arr )
  {
    mat4 mat;
    arr->copyTo(mat.ptr());
    return mat;
  }

  inline mat4 vlx_mat4( const VLXList* list )
  {
    mat4 mat;

    for(size_t i=0; i<list->value().size(); ++i)
    {
      const VLXValue& value = list->value()[i];
      if (value.type() != VLXValue::ArrayReal)
      {
        Log::error( Say("Line %n : parse error during matrix import.\n") << value.lineNumber() );
        return mat4::getNull();
      }
      // composition of subtransforms is made by post multiplication like for COLLADA.
      const VLXArrayReal* arr = value.getArrayReal();
      if (arr->tag() == "<Translate>")
      {
        vec3 tr = vlx_vec3( arr );
        mat = mat * mat4::getTranslation(tr);
      }
      else
      if (arr->tag() == "<Scale>")
      {
        vec3 sc = vlx_vec3( arr );
        mat = mat * mat4::getScaling(sc);
      }
      else
      if (arr->tag() == "<Matrix>")
      {
        mat4 m = vlx_mat4( arr );
        mat = mat * m;
      }
      else
      if (arr->tag() == "<LookAt>")
      {
        // implements the camera's view-matrix look-at as specified by COLLADA
        if (arr->value().size() != 9)
        {
          Log::error( Say("Line %n : <LookAt> must have 9 floats, 3 for 'eye', 3 for 'look' and 3 for 'up'.\n") << arr->lineNumber() << arr->tag() );
        }
        else
        {
          vec3 eye, look, up;
          eye.x()  = (float)arr->value()[0];
          eye.y()  = (float)arr->value()[1];
          eye.z()  = (float)arr->value()[2];
          look.x() = (float)arr->value()[3];
          look.y() = (float)arr->value()[4];
          look.z() = (float)arr->value()[5];
          up.x()   = (float)arr->value()[6];
          up.y()   = (float)arr->value()[7];
          up.z()   = (float)arr->value()[8];
          mat = mat * mat4::getLookAt(eye, look, up);
        }
      }
      else
      if (arr->tag() == "<Skew>")
      {
        Log::error("<Skew> tag not yet supported.\n");
      }
      else
      {
        Log::error( Say("Line %n : unknown tag '%s' ignored.\n") << arr->lineNumber() << arr->tag() );
      }
    }

    return mat;
  }

  inline const char* vlx_EProjectionMatrixType(EProjectionMatrixType pt)
  {
    switch(pt)
    {
    default:
    case PMT_UserProjection: return "PMT_UserProjection";
    case PMT_OrthographicProjection: return "PMT_OrthographicProjection";
    case PMT_PerspectiveProjection: return "PMT_PerspectiveProjection";
    case PMT_PerspectiveProjectionFrustum: return "PMT_PerspectiveProjectionFrustum";
    }
  }

  inline EProjectionMatrixType vlx_EProjectionMatrixType(const VLXValue& value, VLXSerializer& s)
  {
    if( value.getIdentifier() == "PMT_OrthographicProjection") return PMT_OrthographicProjection;
    if( value.getIdentifier() == "PMT_PerspectiveProjection") return PMT_PerspectiveProjection;
    if( value.getIdentifier() == "PMT_PerspectiveProjectionFrustum") return PMT_PerspectiveProjectionFrustum;
    if( value.getIdentifier() == "PMT_UserProjection") return PMT_UserProjection;

    Log::error( Say("Line %n : unknown token '%s'.\n") << value.lineNumber() << value.getIdentifier() );
    s.setError(VLXSerializer::ImportError);
    return PMT_UserProjection;
  }

  inline const char* vlx_EClearColorMode(EClearColorMode ccm)
  {
    switch(ccm)
    {
    default:
    case CCM_Float: return "CCM_Float";
    case CCM_Int: return "CCM_Int";
    case CCM_UInt: return "CCM_UInt";
    }
  }

  inline EClearColorMode vlx_EClearColorMode(const VLXValue& value, VLXSerializer& s)
  {
    if( value.getIdentifier() == "CCM_Int") return CCM_Int;
    if( value.getIdentifier() == "CCM_UInt") return CCM_UInt;
    if( value.getIdentifier() == "CCM_Float") return CCM_Float;

    Log::error( Say("Line %n : unknown token '%s'.\n") << value.lineNumber() << value.getIdentifier() );
    s.setError(VLXSerializer::ImportError);
    return CCM_Float;
  }

  inline const char* vlx_EClearFlags(EClearFlags cf)
  {
    switch(cf)
    {
    default:
    case CF_CLEAR_COLOR_DEPTH_STENCIL: return "CF_CLEAR_COLOR_DEPTH_STENCIL";
    case CF_DO_NOT_CLEAR: return "CF_DO_NOT_CLEAR";
    case CF_CLEAR_COLOR: return "CF_CLEAR_COLOR";
    case CF_CLEAR_DEPTH: return "CF_CLEAR_DEPTH";
    case CF_CLEAR_STENCIL: return "CF_CLEAR_STENCIL";
    case CF_CLEAR_COLOR_DEPTH: return "CF_CLEAR_COLOR_DEPTH";
    case CF_CLEAR_COLOR_STENCIL: return "CF_CLEAR_COLOR_STENCIL";
    case CF_CLEAR_DEPTH_STENCIL: return "CF_CLEAR_DEPTH_STENCIL";
    }
  }

  inline EClearFlags vlx_EClearFlags(const VLXValue& value, VLXSerializer& s)
  {
    if( value.getIdentifier() == "CF_DO_NOT_CLEAR") return CF_DO_NOT_CLEAR;
    if( value.getIdentifier() == "CF_CLEAR_COLOR") return CF_CLEAR_COLOR;
    if( value.getIdentifier() == "CF_CLEAR_DEPTH") return CF_CLEAR_DEPTH;
    if( value.getIdentifier() == "CF_CLEAR_STENCIL") return CF_CLEAR_STENCIL;
    if( value.getIdentifier() == "CF_CLEAR_COLOR_DEPTH") return CF_CLEAR_COLOR_DEPTH;
    if( value.getIdentifier() == "CF_CLEAR_COLOR_STENCIL") return CF_CLEAR_COLOR_STENCIL;
    if( value.getIdentifier() == "CF_CLEAR_DEPTH_STENCIL") return CF_CLEAR_DEPTH_STENCIL;
    if( value.getIdentifier() == "CF_CLEAR_COLOR_DEPTH_STENCIL") return CF_CLEAR_COLOR_DEPTH_STENCIL;

    Log::error( Say("Line %n : unknown token '%s'.\n") << value.lineNumber() << value.getIdentifier() );
    s.setError(VLXSerializer::ImportError);
    return CF_DO_NOT_CLEAR;
  }

  inline const char* vlx_EPolygonFace(EPolygonFace pf)
  {
    switch(pf)
    {
    default:
    case PF_FRONT_AND_BACK: return "PF_FRONT_AND_BACK";
    case PF_FRONT: return "PF_FRONT";
    case PF_BACK:  return "PF_BACK";
    }
  }

  inline EPolygonFace vlx_EPolygonFace(const VLXValue& value, VLXSerializer& s)
  {
    if( value.getIdentifier() == "PF_FRONT") return PF_FRONT;
    if( value.getIdentifier() == "PF_BACK") return PF_BACK;
    if( value.getIdentifier() == "PF_FRONT_AND_BACK") return PF_FRONT_AND_BACK;

    Log::error( Say("Line %n : unknown token '%s'.\n") << value.lineNumber() << value.getIdentifier() );
    s.setError(VLXSerializer::ImportError);
    return PF_FRONT_AND_BACK;
  }

  inline const char* vlx_EColorMaterial(EColorMaterial cm)
  {
    switch(cm)
    {
    default:
    case CM_AMBIENT_AND_DIFFUSE: return "CM_AMBIENT_AND_DIFFUSE";
    case CM_EMISSION: return "CM_EMISSION";
    case CM_AMBIENT: return "CM_AMBIENT";
    case CM_DIFFUSE: return "CM_DIFFUSE";
    case CM_SPECULAR: return "CM_SPECULAR";
    }
  }

  inline EColorMaterial vlx_EColorMaterial(const VLXValue& value, VLXSerializer& s)
  {
    if( value.getIdentifier() == "CM_EMISSION") return CM_EMISSION;
    if( value.getIdentifier() == "CM_AMBIENT") return CM_AMBIENT;
    if( value.getIdentifier() == "CM_DIFFUSE") return CM_DIFFUSE;
    if( value.getIdentifier() == "CM_SPECULAR") return CM_SPECULAR;
    if( value.getIdentifier() == "CM_AMBIENT_AND_DIFFUSE") return CM_AMBIENT_AND_DIFFUSE;

    Log::error( Say("Line %n : unknown token '%s'.\n") << value.lineNumber() << value.getIdentifier() );
    s.setError(VLXSerializer::ImportError);
    return CM_AMBIENT_AND_DIFFUSE;
  }

  inline const char* vlx_ETextureFormat(ETextureFormat tf)
  {
    switch(tf)
    {
    default:
    case TF_UNKNOWN: return "TF_UNKNOWN";

    case TF_ALPHA  : return "TF_ALPHA";
    case TF_ALPHA4 : return "TF_ALPHA4";
    case TF_ALPHA8 : return "TF_ALPHA8";
    case TF_ALPHA12: return "TF_ALPHA12";
    case TF_ALPHA16: return "TF_ALPHA16";

    case TF_INTENSITY  : return "TF_INTENSITY";
    case TF_INTENSITY4 : return "TF_INTENSITY4";
    case TF_INTENSITY8 : return "TF_INTENSITY8";
    case TF_INTENSITY12: return "TF_INTENSITY12";
    case TF_INTENSITY16: return "TF_INTENSITY16";
    case TF_LUMINANCE  : return "TF_LUMINANCE";
    case TF_LUMINANCE4 : return "TF_LUMINANCE4";
    case TF_LUMINANCE8 : return "TF_LUMINANCE8";
    case TF_LUMINANCE12: return "TF_LUMINANCE12";
    case TF_LUMINANCE16: return "TF_LUMINANCE16";
    case TF_LUMINANCE_ALPHA    : return "TF_LUMINANCE_ALPHA";
    case TF_LUMINANCE4_ALPHA4  : return "TF_LUMINANCE4_ALPHA4";
    case TF_LUMINANCE6_ALPHA2  : return "TF_LUMINANCE6_ALPHA2";
    case TF_LUMINANCE8_ALPHA8  : return "TF_LUMINANCE8_ALPHA8";
    case TF_LUMINANCE12_ALPHA4 : return "TF_LUMINANCE12_ALPHA4";
    case TF_LUMINANCE12_ALPHA12: return "TF_LUMINANCE12_ALPHA12";
    case TF_LUMINANCE16_ALPHA16: return "TF_LUMINANCE16_ALPHA16";
    case TF_R3_G3_B2: return "TF_R3_G3_B2";
    case TF_RGB     : return "TF_RGB";
    case TF_RGB4    : return "TF_RGB4";
    case TF_RGB5    : return "TF_RGB5";
    case TF_RGB8    : return "TF_RGB8";
    case TF_RGB10   : return "TF_RGB10";
    case TF_RGB12   : return "TF_RGB12";
    case TF_RGB16   : return "TF_RGB16";
    case TF_RGBA    : return "TF_RGBA";
    case TF_RGBA2   : return "TF_RGBA2";
    case TF_RGBA4   : return "TF_RGBA4";
    case TF_RGB5_A1 : return "TF_RGB5_A1";
    case TF_RGBA8   : return "TF_RGBA8";
    case TF_RGB10_A2: return "TF_RGB10_A2";
    case TF_RGBA12  : return "TF_RGBA12";
    case TF_RGBA16  : return "TF_RGBA16";

    // ARB_texture_float / OpenGL 3
    case TF_RGBA32F: return "TF_RGBA32F";
    case TF_RGB32F: return "TF_RGB32F";
    case TF_ALPHA32F: return "TF_ALPHA32F";
    case TF_INTENSITY32F: return "TF_INTENSITY32F";
    case TF_LUMINANCE32F: return "TF_LUMINANCE32F";
    case TF_LUMINANCE_ALPHA32F: return "TF_LUMINANCE_ALPHA32F";
    case TF_RGBA16F: return "TF_RGBA16F";
    case TF_RGB16F: return "TF_RGB16F";
    case TF_ALPHA16F: return "TF_ALPHA16F";
    case TF_INTENSITY16F: return "TF_INTENSITY16F";
    case TF_LUMINANCE16F: return "TF_LUMINANCE16F";
    case TF_LUMINANCE_ALPHA16F: return "TF_LUMINANCE_ALPHA16F";

    // from table 3.12 opengl api specs 4.1
    case TF_R8_SNORM: return "TF_R8_SNORM";
    case TF_R16_SNORM: return "TF_R16_SNORM";
    case TF_RG8_SNORM: return "TF_RG8_SNORM";
    case TF_RG16_SNORM: return "TF_RG16_SNORM";
    case TF_RGB8_SNORM: return "TF_RGB8_SNORM";
    case TF_RGBA8_SNORM: return "TF_RGBA8_SNORM";
    case TF_RGB10_A2UI: return "TF_RGB10_A2UI";
    case TF_RGBA16_SNORM: return "TF_RGBA16_SNORM";
    case TF_R11F_G11F_B10F: return "TF_R11F_G11F_B10F";
    case TF_RGB9_E5: return "TF_RGB9_E5";
    case TF_RGB8I: return "TF_RGB8I";
    case TF_RGB8UI: return "TF_RGB8UI";
    case TF_RGB16I: return "TF_RGB16I";
    case TF_RGB16UI: return "TF_RGB16UI";
    case TF_RGB32I: return "TF_RGB32I";
    case TF_RGB32UI: return "TF_RGB32UI";
    case TF_RGBA8I: return "TF_RGBA8I";
    case TF_RGBA8UI: return "TF_RGBA8UI";
    case TF_RGBA16I: return "TF_RGBA16I";
    case TF_RGBA16UI: return "TF_RGBA16UI";
    case TF_RGBA32I: return "TF_RGBA32I";
    case TF_RGBA32UI: return "TF_TF_RGBA32UI";

    // ATI_texture_float (the enums are the same as ARB_texture_float)
    //case TF_RGBA_FLOAT32_ATI: return "TF_RGBA_FLOAT32_ATI";
    //case TF_RGB_FLOAT32_ATI: return "TF_RGB_FLOAT32_ATI";
    //case TF_ALPHA_FLOAT32_ATI: return "TF_ALPHA_FLOAT32_ATI";
    //case TF_INTENSITY_FLOAT32_ATI: return "TF_INTENSITY_FLOAT32_ATI";
    //case TF_LUMINANCE_FLOAT32_ATI: return "TF_LUMINANCE_FLOAT32_ATI";
    //case TF_LUMINANCE_ALPHA_FLOAT32_ATI: return "TF_LUMINANCE_ALPHA_FLOAT32_ATI";
    //case TF_RGBA_FLOAT16_ATI: return "TF_RGBA_FLOAT16_ATI";
    //case TF_RGB_FLOAT16_ATI: return "TF_RGB_FLOAT16_ATI";
    //case TF_ALPHA_FLOAT16_ATI: return "TF_ALPHA_FLOAT16_ATI";
    //case TF_INTENSITY_FLOAT16_ATI: return "TF_INTENSITY_FLOAT16_ATI";
    //case TF_LUMINANCE_FLOAT16_ATI: return "TF_LUMINANCE_FLOAT16_ATI";
    //case TF_LUMINANCE_ALPHA_FLOAT16_ATI: return "TF_LUMINANCE_ALPHA_FLOAT16_ATI";

    // EXT_texture_shared_exponent
    // case TF_RGB9_E5_EXT: return "TF_RGB9_E5_EXT";

    // EXT_packed_float
    // case TF_11F_G11F_B10F_EXT: return "TF_11F_G11F_B10F_EXT";

    // EXT_packed_depth_stencil / GL_ARB_framebuffer_object
    case TF_DEPTH_STENCIL   : return "TF_DEPTH_STENCIL";
    case TF_DEPTH24_STENCIL8: return "TF_DEPTH24_STENCIL8";

    // ARB_depth_buffer_float
    case TF_DEPTH_COMPONENT32F: return "TF_DEPTH_COMPONENT32F";
    case TF_DEPTH32F_STENCIL8 : return "TF_DEPTH32F_STENCIL8";

    // ARB_depth_texture
    case TF_DEPTH_COMPONENT  : return "TF_DEPTH_COMPONENT";
    case TF_DEPTH_COMPONENT16: return "TF_DEPTH_COMPONENT16";
    case TF_DEPTH_COMPONENT24: return "TF_DEPTH_COMPONENT24";
    case TF_DEPTH_COMPONENT32: return "TF_DEPTH_COMPONENT32";

    // ARB_texture_compression
    case TF_COMPRESSED_ALPHA          : return "TF_COMPRESSED_ALPHA";
    case TF_COMPRESSED_INTENSITY      : return "TF_COMPRESSED_INTENSITY";
    case TF_COMPRESSED_LUMINANCE      : return "TF_COMPRESSED_LUMINANCE";
    case TF_COMPRESSED_LUMINANCE_ALPHA: return "TF_COMPRESSED_LUMINANCE_ALPHA";
    case TF_COMPRESSED_RGB            : return "TF_COMPRESSED_RGB";
    case TF_COMPRESSED_RGBA           : return "TF_COMPRESSED_RGBA";

    // 3DFX_texture_compression_FXT1
    case TF_COMPRESSED_RGB_FXT1_3DFX : return "TF_COMPRESSED_RGB_FXT1_3DFX";
    case TF_COMPRESSED_RGBA_FXT1_3DFX: return "TF_COMPRESSED_RGBA_FXT1_3DFX";

    // EXT_texture_compression_s3tc
    case TF_COMPRESSED_RGB_S3TC_DXT1_EXT : return "TF_COMPRESSED_RGB_S3TC_DXT1_EXT";
    case TF_COMPRESSED_RGBA_S3TC_DXT1_EXT: return "TF_COMPRESSED_RGBA_S3TC_DXT1_EXT";
    case TF_COMPRESSED_RGBA_S3TC_DXT3_EXT: return "TF_COMPRESSED_RGBA_S3TC_DXT3_EXT";
    case TF_COMPRESSED_RGBA_S3TC_DXT5_EXT: return "TF_COMPRESSED_RGBA_S3TC_DXT5_EXT";

    // EXT_texture_compression_latc
    case TF_COMPRESSED_LUMINANCE_LATC1_EXT             : return "TF_COMPRESSED_LUMINANCE_LATC1_EXT";
    case TF_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT      : return "TF_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT";
    case TF_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT       : return "TF_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT";
    case TF_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT: return "TF_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT";

    // EXT_texture_compression_rgtc
    case TF_COMPRESSED_RED_RGTC1_EXT             : return "TF_COMPRESSED_RED_RGTC1_EXT";
    case TF_COMPRESSED_SIGNED_RED_RGTC1_EXT      : return "TF_COMPRESSED_SIGNED_RED_RGTC1_EXT";
    case TF_COMPRESSED_RED_GREEN_RGTC2_EXT       : return "TF_COMPRESSED_RED_GREEN_RGTC2_EXT";
    case TF_COMPRESSED_SIGNED_RED_GREEN_RGTC2_EXT: return "TF_COMPRESSED_SIGNED_RED_GREEN_RGTC2_EXT";

    // EXT_texture_integer
    // case TF_RGBA32UI_EXT: return "TF_RGBA32UI_EXT";
    // case TF_RGB32UI_EXT: return "TF_RGB32UI_EXT";
    case TF_ALPHA32UI_EXT: return "TF_ALPHA32UI_EXT";
    case TF_INTENSITY32UI_EXT: return "TF_INTENSITY32UI_EXT";
    case TF_LUMINANCE32UI_EXT: return "TF_LUMINANCE32UI_EXT";
    case TF_LUMINANCE_ALPHA32UI_EXT: return "TF_LUMINANCE_ALPHA32UI_EXT";

    // case TF_RGBA16UI_EXT: return "TF_RGBA16UI_EXT";
    // case TF_RGB16UI_EXT: return "TF_RGB16UI_EXT";
    case TF_ALPHA16UI_EXT: return "TF_ALPHA16UI_EXT";
    case TF_INTENSITY16UI_EXT: return "TF_INTENSITY16UI_EXT";
    case TF_LUMINANCE16UI_EXT: return "TF_LUMINANCE16UI_EXT";
    case TF_LUMINANCE_ALPHA16UI_EXT: return "TF_LUMINANCE_ALPHA16UI_EXT";

    // case TF_RGBA8UI_EXT: return "TF_RGBA8UI_EXT";
    // case TF_RGB8UI_EXT: return "TF_RGB8UI_EXT";
    case TF_ALPHA8UI_EXT: return "TF_ALPHA8UI_EXT";
    case TF_INTENSITY8UI_EXT: return "TF_INTENSITY8UI_EXT";
    case TF_LUMINANCE8UI_EXT: return "TF_LUMINANCE8UI_EXT";
    case TF_LUMINANCE_ALPHA8UI_EXT: return "TF_LUMINANCE_ALPHA8UI_EXT";

    // case TF_RGBA32I_EXT: return "TF_RGBA32I_EXT";
    // case TF_RGB32I_EXT: return "TF_RGB32I_EXT";
    case TF_ALPHA32I_EXT: return "TF_ALPHA32I_EXT";
    case TF_INTENSITY32I_EXT: return "TF_INTENSITY32I_EXT";
    case TF_LUMINANCE32I_EXT: return "TF_LUMINANCE32I_EXT";
    case TF_LUMINANCE_ALPHA32I_EXT: return "TF_LUMINANCE_ALPHA32I_EXT";

    // case TF_RGBA16I_EXT: return "TF_RGBA16I_EXT";
    // case TF_RGB16I_EXT: return "TF_RGB16I_EXT";
    case TF_ALPHA16I_EXT: return "TF_ALPHA16I_EXT";
    case TF_INTENSITY16I_EXT: return "TF_INTENSITY16I_EXT";
    case TF_LUMINANCE16I_EXT: return "TF_LUMINANCE16I_EXT";
    case TF_LUMINANCE_ALPHA16I_EXT: return "TF_LUMINANCE_ALPHA16I_EXT";

    // case TF_RGBA8I_EXT: return "TF_RGBA8I_EXT";
    // case TF_RGB8I_EXT: return "TF_RGB8I_EXT";
    case TF_ALPHA8I_EXT: return "TF_ALPHA8I_EXT";
    case TF_INTENSITY8I_EXT: return "TF_INTENSITY8I_EXT";
    case TF_LUMINANCE8I_EXT: return "TF_LUMINANCE8I_EXT";
    case TF_LUMINANCE_ALPHA8I_EXT: return "TF_LUMINANCE_ALPHA8I_EXT";

    // GL_ARB_texture_rg
    case TF_RED: return "TF_RED";
    case TF_COMPRESSED_RED: return "TF_COMPRESSED_RED";
    case TF_COMPRESSED_RG: return "TF_COMPRESSED_RG";
    case TF_RG: return "TF_RG";
    case TF_R8: return "TF_R8";
    case TF_R16: return "TF_R16";
    case TF_RG8: return "TF_RG8";
    case TF_RG16: return "TF_RG16";
    case TF_R16F: return "TF_R16F";
    case TF_R32F: return "TF_R32F";
    case TF_RG16F: return "TF_RG16F";
    case TF_RG32F: return "TF_RG32F";
    case TF_R8I: return "TF_R8I";
    case TF_R8UI: return "TF_R8UI";
    case TF_R16I: return "TF_R16I";
    case TF_R16UI: return "TF_R16UI";
    case TF_R32I: return "TF_R32I";
    case TF_R32UI: return "TF_R32UI";
    case TF_RG8I: return "TF_RG8I";
    case TF_RG8UI: return "TF_RG8UI";
    case TF_RG16I: return "TF_RG16I";
    case TF_RG16UI: return "TF_RG16UI";
    case TF_RG32I: return "TF_RG32I";
    case TF_RG32UI: return "TF_RG32UI";

    // sRGB OpenGL 2.1
    case TF_SLUMINANCE_ALPHA: return "TF_SLUMINANCE_ALPHA";
    case TF_SLUMINANCE8_ALPHA8: return "TF_SLUMINANCE8_ALPHA8";
    case TF_SLUMINANCE: return "TF_SLUMINANCE";
    case TF_SLUMINANCE8: return "TF_SLUMINANCE8";
    case TF_COMPRESSED_SLUMINANCE: return "TF_COMPRESSED_SLUMINANCE";
    case TF_COMPRESSED_SLUMINANCE_ALPHA: return "TF_COMPRESSED_SLUMINANCE_ALPHA";

    // sRGB OpenGL 2.1 / 3.x
    case TF_SRGB: return "TF_SRGB";
    case TF_SRGB8: return "TF_SRGB8";
    case TF_SRGB_ALPHA: return "TF_SRGB_ALPHA";
    case TF_SRGB8_ALPHA8: return "TF_SRGB8_ALPHA8";
    case TF_COMPRESSED_SRGB: return "TF_COMPRESSED_SRGB";
    case TF_COMPRESSED_SRGB_ALPHA: return "TF_COMPRESSED_SRGB_ALPHA";

    // GL_EXT_texture_sRGB compressed formats
    case TF_COMPRESSED_SRGB_S3TC_DXT1_EXT: return "TF_COMPRESSED_SRGB_S3TC_DXT1_EXT";
    case TF_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT: return "TF_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT";
    case TF_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT: return "TF_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT";
    case TF_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT: return "TF_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT";
    }
  }

  inline ETextureFormat vlx_ETextureFormat(const VLXValue& value, VLXSerializer& s)
  {
    if( value.getIdentifier() == "TF_UNKNOWN") return TF_UNKNOWN;

    if( value.getIdentifier() == "TF_ALPHA") return TF_ALPHA;
    if( value.getIdentifier() == "TF_ALPHA4") return TF_ALPHA4;
    if( value.getIdentifier() == "TF_ALPHA8") return TF_ALPHA8;
    if( value.getIdentifier() == "TF_ALPHA12") return TF_ALPHA12;
    if( value.getIdentifier() == "TF_ALPHA16") return TF_ALPHA16;

    if( value.getIdentifier() == "TF_INTENSITY") return TF_INTENSITY;
    if( value.getIdentifier() == "TF_INTENSITY4") return TF_INTENSITY4;
    if( value.getIdentifier() == "TF_INTENSITY8") return TF_INTENSITY8;
    if( value.getIdentifier() == "TF_INTENSITY12") return TF_INTENSITY12;
    if( value.getIdentifier() == "TF_INTENSITY16") return TF_INTENSITY16;
    if( value.getIdentifier() == "TF_LUMINANCE") return TF_LUMINANCE;
    if( value.getIdentifier() == "TF_LUMINANCE4") return TF_LUMINANCE4;
    if( value.getIdentifier() == "TF_LUMINANCE8") return TF_LUMINANCE8;
    if( value.getIdentifier() == "TF_LUMINANCE12") return TF_LUMINANCE12;
    if( value.getIdentifier() == "TF_LUMINANCE16") return TF_LUMINANCE16;
    if( value.getIdentifier() == "TF_LUMINANCE_ALPHA") return TF_LUMINANCE_ALPHA;
    if( value.getIdentifier() == "TF_LUMINANCE4_ALPHA4") return TF_LUMINANCE4_ALPHA4;
    if( value.getIdentifier() == "TF_LUMINANCE6_ALPHA2") return TF_LUMINANCE6_ALPHA2;
    if( value.getIdentifier() == "TF_LUMINANCE8_ALPHA8") return TF_LUMINANCE8_ALPHA8;
    if( value.getIdentifier() == "TF_LUMINANCE12_ALPHA4") return TF_LUMINANCE12_ALPHA4;
    if( value.getIdentifier() == "TF_LUMINANCE12_ALPHA12") return TF_LUMINANCE12_ALPHA12;
    if( value.getIdentifier() == "TF_LUMINANCE16_ALPHA16") return TF_LUMINANCE16_ALPHA16;
    if( value.getIdentifier() == "TF_R3_G3_B2") return TF_R3_G3_B2;
    if( value.getIdentifier() == "TF_RGB") return TF_RGB;
    if( value.getIdentifier() == "TF_RGB4") return TF_RGB4;
    if( value.getIdentifier() == "TF_RGB5") return TF_RGB5;
    if( value.getIdentifier() == "TF_RGB8") return TF_RGB8;
    if( value.getIdentifier() == "TF_RGB10") return TF_RGB10;
    if( value.getIdentifier() == "TF_RGB12") return TF_RGB12;
    if( value.getIdentifier() == "TF_RGB16") return TF_RGB16;
    if( value.getIdentifier() == "TF_RGBA") return TF_RGBA;
    if( value.getIdentifier() == "TF_RGBA2") return TF_RGBA2;
    if( value.getIdentifier() == "TF_RGBA4") return TF_RGBA4;
    if( value.getIdentifier() == "TF_RGB5_A1") return TF_RGB5_A1;
    if( value.getIdentifier() == "TF_RGBA8") return TF_RGBA8;
    if( value.getIdentifier() == "TF_RGB10_A2") return TF_RGB10_A2;
    if( value.getIdentifier() == "TF_RGBA12") return TF_RGBA12;
    if( value.getIdentifier() == "TF_RGBA16") return TF_RGBA16;

    // ARB_texture_float / OpenGL 3
    if( value.getIdentifier() == "TF_RGBA32F") return TF_RGBA32F;
    if( value.getIdentifier() == "TF_RGB32F") return TF_RGB32F;
    if( value.getIdentifier() == "TF_ALPHA32F") return TF_ALPHA32F;
    if( value.getIdentifier() == "TF_INTENSITY32F") return TF_INTENSITY32F;
    if( value.getIdentifier() == "TF_LUMINANCE32F") return TF_LUMINANCE32F;
    if( value.getIdentifier() == "TF_LUMINANCE_ALPHA32F") return TF_LUMINANCE_ALPHA32F;
    if( value.getIdentifier() == "TF_RGBA16F") return TF_RGBA16F;
    if( value.getIdentifier() == "TF_RGB16F") return TF_RGB16F;
    if( value.getIdentifier() == "TF_ALPHA16F") return TF_ALPHA16F;
    if( value.getIdentifier() == "TF_INTENSITY16F") return TF_INTENSITY16F;
    if( value.getIdentifier() == "TF_LUMINANCE16F") return TF_LUMINANCE16F;
    if( value.getIdentifier() == "TF_LUMINANCE_ALPHA16F") return TF_LUMINANCE_ALPHA16F;

    // from table 3.12 opengl api specs 4.1
    if( value.getIdentifier() == "TF_R8_SNORM") return TF_R8_SNORM;
    if( value.getIdentifier() == "TF_R16_SNORM") return TF_R16_SNORM;
    if( value.getIdentifier() == "TF_RG8_SNORM") return TF_RG8_SNORM;
    if( value.getIdentifier() == "TF_RG16_SNORM") return TF_RG16_SNORM;
    if( value.getIdentifier() == "TF_RGB8_SNORM") return TF_RGB8_SNORM;
    if( value.getIdentifier() == "TF_RGBA8_SNORM") return TF_RGBA8_SNORM;
    if( value.getIdentifier() == "TF_RGB10_A2UI") return TF_RGB10_A2UI;
    if( value.getIdentifier() == "TF_RGBA16_SNORM") return TF_RGBA16_SNORM;
    if( value.getIdentifier() == "TF_R11F_G11F_B10F") return TF_R11F_G11F_B10F;
    if( value.getIdentifier() == "TF_RGB9_E5") return TF_RGB9_E5;
    if( value.getIdentifier() == "TF_RGB8I") return TF_RGB8I;
    if( value.getIdentifier() == "TF_RGB8UI") return TF_RGB8UI;
    if( value.getIdentifier() == "TF_RGB16I") return TF_RGB16I;
    if( value.getIdentifier() == "TF_RGB16UI") return TF_RGB16UI;
    if( value.getIdentifier() == "TF_RGB32I") return TF_RGB32I;
    if( value.getIdentifier() == "TF_RGB32UI") return TF_RGB32UI;
    if( value.getIdentifier() == "TF_RGBA8I") return TF_RGBA8I;
    if( value.getIdentifier() == "TF_RGBA8UI") return TF_RGBA8UI;
    if( value.getIdentifier() == "TF_RGBA16I") return TF_RGBA16I;
    if( value.getIdentifier() == "TF_RGBA16UI") return TF_RGBA16UI;
    if( value.getIdentifier() == "TF_RGBA32I") return TF_RGBA32I;
    if( value.getIdentifier() == "TF_RGBA32UI") return TF_RGBA32UI;

    // ATI_texture_float (the enums are the same as ARB_texture_float)
    if( value.getIdentifier() == "TF_RGBA_FLOAT32_ATI") return TF_RGBA_FLOAT32_ATI;
    if( value.getIdentifier() == "TF_RGB_FLOAT32_ATI") return TF_RGB_FLOAT32_ATI;
    if( value.getIdentifier() == "TF_ALPHA_FLOAT32_ATI") return TF_ALPHA_FLOAT32_ATI;
    if( value.getIdentifier() == "TF_INTENSITY_FLOAT32_ATI") return TF_INTENSITY_FLOAT32_ATI;
    if( value.getIdentifier() == "TF_LUMINANCE_FLOAT32_ATI") return TF_LUMINANCE_FLOAT32_ATI;
    if( value.getIdentifier() == "TF_LUMINANCE_ALPHA_FLOAT32_ATI") return TF_LUMINANCE_ALPHA_FLOAT32_ATI;
    if( value.getIdentifier() == "TF_RGBA_FLOAT16_ATI") return TF_RGBA_FLOAT16_ATI;
    if( value.getIdentifier() == "TF_RGB_FLOAT16_ATI") return TF_RGB_FLOAT16_ATI;
    if( value.getIdentifier() == "TF_ALPHA_FLOAT16_ATI") return TF_ALPHA_FLOAT16_ATI;
    if( value.getIdentifier() == "TF_INTENSITY_FLOAT16_ATI") return TF_INTENSITY_FLOAT16_ATI;
    if( value.getIdentifier() == "TF_LUMINANCE_FLOAT16_ATI") return TF_LUMINANCE_FLOAT16_ATI;
    if( value.getIdentifier() == "TF_LUMINANCE_ALPHA_FLOAT16_ATI") return TF_LUMINANCE_ALPHA_FLOAT16_ATI;

    // EXT_texture_shared_exponent
    if( value.getIdentifier() == "TF_RGB9_E5_EXT") return TF_RGB9_E5_EXT;

    // EXT_packed_float
    if( value.getIdentifier() == "TF_11F_G11F_B10F_EXT") return TF_11F_G11F_B10F_EXT;

    // EXT_packed_depth_stencil / GL_ARB_framebuffer_object
    if( value.getIdentifier() == "TF_DEPTH_STENCIL") return TF_DEPTH_STENCIL;
    if( value.getIdentifier() == "TF_DEPTH24_STENCIL8") return TF_DEPTH24_STENCIL8;

    // ARB_depth_buffer_float
    if( value.getIdentifier() == "TF_DEPTH_COMPONENT32F") return TF_DEPTH_COMPONENT32F;
    if( value.getIdentifier() == "TF_DEPTH32F_STENCIL8") return TF_DEPTH32F_STENCIL8;

    // ARB_depth_texture
    if( value.getIdentifier() == "TF_DEPTH_COMPONENT") return TF_DEPTH_COMPONENT;
    if( value.getIdentifier() == "TF_DEPTH_COMPONENT16") return TF_DEPTH_COMPONENT16;
    if( value.getIdentifier() == "TF_DEPTH_COMPONENT24") return TF_DEPTH_COMPONENT24;
    if( value.getIdentifier() == "TF_DEPTH_COMPONENT32") return TF_DEPTH_COMPONENT32;

    // ARB_texture_compression
    if( value.getIdentifier() == "TF_COMPRESSED_ALPHA") return TF_COMPRESSED_ALPHA;
    if( value.getIdentifier() == "TF_COMPRESSED_INTENSITY") return TF_COMPRESSED_INTENSITY;
    if( value.getIdentifier() == "TF_COMPRESSED_LUMINANCE") return TF_COMPRESSED_LUMINANCE;
    if( value.getIdentifier() == "TF_COMPRESSED_LUMINANCE_ALPHA") return TF_COMPRESSED_LUMINANCE_ALPHA;
    if( value.getIdentifier() == "TF_COMPRESSED_RGB") return TF_COMPRESSED_RGB;
    if( value.getIdentifier() == "TF_COMPRESSED_RGBA") return TF_COMPRESSED_RGBA;

    // 3DFX_texture_compression_FXT1
    if( value.getIdentifier() == "TF_COMPRESSED_RGB_FXT1_3DFX") return TF_COMPRESSED_RGB_FXT1_3DFX;
    if( value.getIdentifier() == "TF_COMPRESSED_RGBA_FXT1_3DFX") return TF_COMPRESSED_RGBA_FXT1_3DFX;

    // EXT_texture_compression_s3tc
    if( value.getIdentifier() == "TF_COMPRESSED_RGB_S3TC_DXT1_EXT") return TF_COMPRESSED_RGB_S3TC_DXT1_EXT;
    if( value.getIdentifier() == "TF_COMPRESSED_RGBA_S3TC_DXT1_EXT") return TF_COMPRESSED_RGBA_S3TC_DXT1_EXT;
    if( value.getIdentifier() == "TF_COMPRESSED_RGBA_S3TC_DXT3_EXT") return TF_COMPRESSED_RGBA_S3TC_DXT3_EXT;
    if( value.getIdentifier() == "TF_COMPRESSED_RGBA_S3TC_DXT5_EXT") return TF_COMPRESSED_RGBA_S3TC_DXT5_EXT;

    // EXT_texture_compression_latc
    if( value.getIdentifier() == "TF_COMPRESSED_LUMINANCE_LATC1_EXT") return TF_COMPRESSED_LUMINANCE_LATC1_EXT;
    if( value.getIdentifier() == "TF_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT") return TF_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT;
    if( value.getIdentifier() == "TF_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT") return TF_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT;
    if( value.getIdentifier() == "TF_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT") return TF_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT;

    // EXT_texture_compression_rgtc
    if( value.getIdentifier() == "TF_COMPRESSED_RED_RGTC1_EXT") return TF_COMPRESSED_RED_RGTC1_EXT;
    if( value.getIdentifier() == "TF_COMPRESSED_SIGNED_RED_RGTC1_EXT") return TF_COMPRESSED_SIGNED_RED_RGTC1_EXT;
    if( value.getIdentifier() == "TF_COMPRESSED_RED_GREEN_RGTC2_EXT") return TF_COMPRESSED_RED_GREEN_RGTC2_EXT;
    if( value.getIdentifier() == "TF_COMPRESSED_SIGNED_RED_GREEN_RGTC2_EXT") return TF_COMPRESSED_SIGNED_RED_GREEN_RGTC2_EXT;

    // EXT_texture_integer
    if( value.getIdentifier() == "TF_RGBA32UI_EXT") return TF_RGBA32UI_EXT;
    if( value.getIdentifier() == "TF_RGB32UI_EXT") return TF_RGB32UI_EXT;
    if( value.getIdentifier() == "TF_ALPHA32UI_EXT") return TF_ALPHA32UI_EXT;
    if( value.getIdentifier() == "TF_INTENSITY32UI_EXT") return TF_INTENSITY32UI_EXT;
    if( value.getIdentifier() == "TF_LUMINANCE32UI_EXT") return TF_LUMINANCE32UI_EXT;
    if( value.getIdentifier() == "TF_LUMINANCE_ALPHA32UI_EXT") return TF_LUMINANCE_ALPHA32UI_EXT;

    if( value.getIdentifier() == "TF_RGBA16UI_EXT") return TF_RGBA16UI_EXT;
    if( value.getIdentifier() == "TF_RGB16UI_EXT") return TF_RGB16UI_EXT;
    if( value.getIdentifier() == "TF_ALPHA16UI_EXT") return TF_ALPHA16UI_EXT;
    if( value.getIdentifier() == "TF_INTENSITY16UI_EXT") return TF_INTENSITY16UI_EXT;
    if( value.getIdentifier() == "TF_LUMINANCE16UI_EXT") return TF_LUMINANCE16UI_EXT;
    if( value.getIdentifier() == "TF_LUMINANCE_ALPHA16UI_EXT") return TF_LUMINANCE_ALPHA16UI_EXT;

    if( value.getIdentifier() == "TF_RGBA8UI_EXT") return TF_RGBA8UI_EXT;
    if( value.getIdentifier() == "TF_RGB8UI_EXT") return TF_RGB8UI_EXT;
    if( value.getIdentifier() == "TF_ALPHA8UI_EXT") return TF_ALPHA8UI_EXT;
    if( value.getIdentifier() == "TF_INTENSITY8UI_EXT") return TF_INTENSITY8UI_EXT;
    if( value.getIdentifier() == "TF_LUMINANCE8UI_EXT") return TF_LUMINANCE8UI_EXT;
    if( value.getIdentifier() == "TF_LUMINANCE_ALPHA8UI_EXT") return TF_LUMINANCE_ALPHA8UI_EXT;

    if( value.getIdentifier() == "TF_RGBA32I_EXT") return TF_RGBA32I_EXT;
    if( value.getIdentifier() == "TF_RGB32I_EXT") return TF_RGB32I_EXT;
    if( value.getIdentifier() == "TF_ALPHA32I_EXT") return TF_ALPHA32I_EXT;
    if( value.getIdentifier() == "TF_INTENSITY32I_EXT") return TF_INTENSITY32I_EXT;
    if( value.getIdentifier() == "TF_LUMINANCE32I_EXT") return TF_LUMINANCE32I_EXT;
    if( value.getIdentifier() == "TF_LUMINANCE_ALPHA32I_EXT") return TF_LUMINANCE_ALPHA32I_EXT;

    if( value.getIdentifier() == "TF_RGBA16I_EXT") return TF_RGBA16I_EXT;
    if( value.getIdentifier() == "TF_RGB16I_EXT") return TF_RGB16I_EXT;
    if( value.getIdentifier() == "TF_ALPHA16I_EXT") return TF_ALPHA16I_EXT;
    if( value.getIdentifier() == "TF_INTENSITY16I_EXT") return TF_INTENSITY16I_EXT;
    if( value.getIdentifier() == "TF_LUMINANCE16I_EXT") return TF_LUMINANCE16I_EXT;
    if( value.getIdentifier() == "TF_LUMINANCE_ALPHA16I_EXT") return TF_LUMINANCE_ALPHA16I_EXT;

    if( value.getIdentifier() == "TF_RGBA8I_EXT") return TF_RGBA8I_EXT;
    if( value.getIdentifier() == "TF_RGB8I_EXT") return TF_RGB8I_EXT;
    if( value.getIdentifier() == "TF_ALPHA8I_EXT") return TF_ALPHA8I_EXT;
    if( value.getIdentifier() == "TF_INTENSITY8I_EXT") return TF_INTENSITY8I_EXT;
    if( value.getIdentifier() == "TF_LUMINANCE8I_EXT") return TF_LUMINANCE8I_EXT;
    if( value.getIdentifier() == "TF_LUMINANCE_ALPHA8I_EXT") return TF_LUMINANCE_ALPHA8I_EXT;

    // GL_ARB_texture_rg
    if( value.getIdentifier() == "TF_RED") return TF_RED;
    if( value.getIdentifier() == "TF_COMPRESSED_RED") return TF_COMPRESSED_RED;
    if( value.getIdentifier() == "TF_COMPRESSED_RG") return TF_COMPRESSED_RG;
    if( value.getIdentifier() == "TF_RG") return TF_RG;
    if( value.getIdentifier() == "TF_R8") return TF_R8;
    if( value.getIdentifier() == "TF_R16") return TF_R16;
    if( value.getIdentifier() == "TF_RG8") return TF_RG8;
    if( value.getIdentifier() == "TF_RG16") return TF_RG16;
    if( value.getIdentifier() == "TF_R16F") return TF_R16F;
    if( value.getIdentifier() == "TF_R32F") return TF_R32F;
    if( value.getIdentifier() == "TF_RG16F") return TF_RG16F;
    if( value.getIdentifier() == "TF_RG32F") return TF_RG32F;
    if( value.getIdentifier() == "TF_R8I") return TF_R8I;
    if( value.getIdentifier() == "TF_R8UI") return TF_R8UI;
    if( value.getIdentifier() == "TF_R16I") return TF_R16I;
    if( value.getIdentifier() == "TF_R16UI") return TF_R16UI;
    if( value.getIdentifier() == "TF_R32I") return TF_R32I;
    if( value.getIdentifier() == "TF_R32UI") return TF_R32UI;
    if( value.getIdentifier() == "TF_RG8I") return TF_RG8I;
    if( value.getIdentifier() == "TF_RG8UI") return TF_RG8UI;
    if( value.getIdentifier() == "TF_RG16I") return TF_RG16I;
    if( value.getIdentifier() == "TF_RG16UI") return TF_RG16UI;
    if( value.getIdentifier() == "TF_RG32I") return TF_RG32I;
    if( value.getIdentifier() == "TF_RG32UI") return TF_RG32UI;

    // sRGB OpenGL 2.1
    if( value.getIdentifier() == "TF_SLUMINANCE_ALPHA") return TF_SLUMINANCE_ALPHA;
    if( value.getIdentifier() == "TF_SLUMINANCE8_ALPHA8") return TF_SLUMINANCE8_ALPHA8;
    if( value.getIdentifier() == "TF_SLUMINANCE") return TF_SLUMINANCE;
    if( value.getIdentifier() == "TF_SLUMINANCE8") return TF_SLUMINANCE8;
    if( value.getIdentifier() == "TF_COMPRESSED_SLUMINANCE") return TF_COMPRESSED_SLUMINANCE;
    if( value.getIdentifier() == "TF_COMPRESSED_SLUMINANCE_ALPHA") return TF_COMPRESSED_SLUMINANCE_ALPHA;

    // sRGB OpenGL 2.1 / 3.x
    if( value.getIdentifier() == "TF_SRGB") return TF_SRGB;
    if( value.getIdentifier() == "TF_SRGB8") return TF_SRGB8;
    if( value.getIdentifier() == "TF_SRGB_ALPHA") return TF_SRGB_ALPHA;
    if( value.getIdentifier() == "TF_SRGB8_ALPHA8") return TF_SRGB8_ALPHA8;
    if( value.getIdentifier() == "TF_COMPRESSED_SRGB") return TF_COMPRESSED_SRGB;
    if( value.getIdentifier() == "TF_COMPRESSED_SRGB_ALPHA") return TF_COMPRESSED_SRGB_ALPHA;

    // GL_EXT_texture_sRGB compressed formats
    if( value.getIdentifier() == "TF_COMPRESSED_SRGB_S3TC_DXT1_EXT") return TF_COMPRESSED_SRGB_S3TC_DXT1_EXT;
    if( value.getIdentifier() == "TF_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT") return TF_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT;
    if( value.getIdentifier() == "TF_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT") return TF_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT;
    if( value.getIdentifier() == "TF_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT") return TF_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT;

    Log::error( Say("Line %n : unknown token '%s'.\n") << value.lineNumber() << value.getIdentifier() );
    s.setError(VLXSerializer::ImportError);
    return TF_UNKNOWN;
  }

  inline const char* vlx_EUniformType(EUniformType type)
  {
    switch(type)
    {
      default: 
        return "UT_NONE";

      case UT_INT:      return "UT_INT";
      case UT_INT_VEC2: return "UT_INT_VEC2";
      case UT_INT_VEC3: return "UT_INT_VEC3";
      case UT_INT_VEC4: return "UT_INT_VEC4";

      case UT_UNSIGNED_INT:      return "UT_UNSIGNED_INT";
      case UT_UNSIGNED_INT_VEC2: return "UT_UNSIGNED_INT_VEC2";
      case UT_UNSIGNED_INT_VEC3: return "UT_UNSIGNED_INT_VEC3";
      case UT_UNSIGNED_INT_VEC4: return "UT_UNSIGNED_INT_VEC4";

      case UT_FLOAT:      return "UT_FLOAT";
      case UT_FLOAT_VEC2: return "UT_FLOAT_VEC2";
      case UT_FLOAT_VEC3: return "UT_FLOAT_VEC3";
      case UT_FLOAT_VEC4: return "UT_FLOAT_VEC4";

      case UT_FLOAT_MAT2: return "UT_FLOAT_MAT2";
      case UT_FLOAT_MAT3: return "UT_FLOAT_MAT3";
      case UT_FLOAT_MAT4: return "UT_FLOAT_MAT4";

      case UT_FLOAT_MAT2x3: return "UT_FLOAT_MAT2x3";
      case UT_FLOAT_MAT3x2: return "UT_FLOAT_MAT3x2";
      case UT_FLOAT_MAT2x4: return "UT_FLOAT_MAT2x4";
      case UT_FLOAT_MAT4x2: return "UT_FLOAT_MAT4x2";
      case UT_FLOAT_MAT3x4: return "UT_FLOAT_MAT3x4";
      case UT_FLOAT_MAT4x3: return "UT_FLOAT_MAT4x3";

      case UT_DOUBLE:      return "UT_DOUBLE";
      case UT_DOUBLE_VEC2: return "UT_DOUBLE_VEC2";
      case UT_DOUBLE_VEC3: return "UT_DOUBLE_VEC3";
      case UT_DOUBLE_VEC4: return "UT_DOUBLE_VEC4";

      case UT_DOUBLE_MAT2: return "UT_DOUBLE_MAT2";
      case UT_DOUBLE_MAT3: return "UT_DOUBLE_MAT3";
      case UT_DOUBLE_MAT4: return "UT_DOUBLE_MAT4";

      case UT_DOUBLE_MAT2x3: return "UT_DOUBLE_MAT2x3";
      case UT_DOUBLE_MAT3x2: return "UT_DOUBLE_MAT3x2";
      case UT_DOUBLE_MAT2x4: return "UT_DOUBLE_MAT2x4";
      case UT_DOUBLE_MAT4x2: return "UT_DOUBLE_MAT4x2";
      case UT_DOUBLE_MAT3x4: return "UT_DOUBLE_MAT3x4";
      case UT_DOUBLE_MAT4x3: return "UT_DOUBLE_MAT4x3";
    }
  }

  inline EUniformType vlx_EUniformType(const VLXValue& value, VLXSerializer& s)
  {
    if( value.getIdentifier() == "UT_INT") return UT_INT;
    if( value.getIdentifier() == "UT_INT_VEC2") return UT_INT_VEC2;
    if( value.getIdentifier() == "UT_INT_VEC3") return UT_INT_VEC3;
    if( value.getIdentifier() == "UT_INT_VEC4") return UT_INT_VEC4;

    if( value.getIdentifier() == "UT_UNSIGNED_INT") return UT_UNSIGNED_INT;
    if( value.getIdentifier() == "UT_UNSIGNED_INT_VEC2") return UT_UNSIGNED_INT_VEC2;
    if( value.getIdentifier() == "UT_UNSIGNED_INT_VEC3") return UT_UNSIGNED_INT_VEC3;
    if( value.getIdentifier() == "UT_UNSIGNED_INT_VEC4") return UT_UNSIGNED_INT_VEC4;

    if( value.getIdentifier() == "UT_FLOAT") return UT_FLOAT;
    if( value.getIdentifier() == "UT_FLOAT_VEC2") return UT_FLOAT_VEC2;
    if( value.getIdentifier() == "UT_FLOAT_VEC3") return UT_FLOAT_VEC3;
    if( value.getIdentifier() == "UT_FLOAT_VEC4") return UT_FLOAT_VEC4;

    if( value.getIdentifier() == "UT_FLOAT_MAT2") return UT_FLOAT_MAT2;
    if( value.getIdentifier() == "UT_FLOAT_MAT3") return UT_FLOAT_MAT3;
    if( value.getIdentifier() == "UT_FLOAT_MAT4") return UT_FLOAT_MAT4;

    if( value.getIdentifier() == "UT_FLOAT_MAT2x3") return UT_FLOAT_MAT2x3;
    if( value.getIdentifier() == "UT_FLOAT_MAT3x2") return UT_FLOAT_MAT3x2;
    if( value.getIdentifier() == "UT_FLOAT_MAT2x4") return UT_FLOAT_MAT2x4;
    if( value.getIdentifier() == "UT_FLOAT_MAT4x2") return UT_FLOAT_MAT4x2;
    if( value.getIdentifier() == "UT_FLOAT_MAT3x4") return UT_FLOAT_MAT3x4;
    if( value.getIdentifier() == "UT_FLOAT_MAT4x3") return UT_FLOAT_MAT4x3;

    if( value.getIdentifier() == "UT_DOUBLE") return UT_DOUBLE;
    if( value.getIdentifier() == "UT_DOUBLE_VEC2") return UT_DOUBLE_VEC2;
    if( value.getIdentifier() == "UT_DOUBLE_VEC3") return UT_DOUBLE_VEC3;
    if( value.getIdentifier() == "UT_DOUBLE_VEC4") return UT_DOUBLE_VEC4;

    if( value.getIdentifier() == "UT_DOUBLE_MAT2") return UT_DOUBLE_MAT2;
    if( value.getIdentifier() == "UT_DOUBLE_MAT3") return UT_DOUBLE_MAT3;
    if( value.getIdentifier() == "UT_DOUBLE_MAT4") return UT_DOUBLE_MAT4;

    if( value.getIdentifier() == "UT_DOUBLE_MAT2x3") return UT_DOUBLE_MAT2x3;
    if( value.getIdentifier() == "UT_DOUBLE_MAT3x2") return UT_DOUBLE_MAT3x2;
    if( value.getIdentifier() == "UT_DOUBLE_MAT2x4") return UT_DOUBLE_MAT2x4;
    if( value.getIdentifier() == "UT_DOUBLE_MAT4x2") return UT_DOUBLE_MAT4x2;
    if( value.getIdentifier() == "UT_DOUBLE_MAT3x4") return UT_DOUBLE_MAT3x4;
    if( value.getIdentifier() == "UT_DOUBLE_MAT4x3") return UT_DOUBLE_MAT4x3;

    Log::error( Say("Line %n : unknown token '%s'.\n") << value.lineNumber() << value.getIdentifier() );
    s.setError(VLXSerializer::ImportError);
    return UT_NONE;
  }

  inline const char* vlx_EDepthTextureMode(EDepthTextureMode dtm)
  {
    switch(dtm)
    {
    default:
    case DTM_RED: return "DTM_RED";
    case DTM_LUMINANCE: return "DTM_LUMINANCE";
    case DTM_INTENSITY: return "DTM_INTENSITY";
    case DTM_ALPHA: return "DTM_ALPHA";
    }
  }

  inline EDepthTextureMode vlx_EDepthTextureMode(const VLXValue& value, VLXSerializer& s)
  {
    if( value.getIdentifier() == "DTM_LUMINANCE") return DTM_LUMINANCE;
    if( value.getIdentifier() == "DTM_INTENSITY") return DTM_INTENSITY;
    if( value.getIdentifier() == "DTM_ALPHA") return DTM_ALPHA;
    if( value.getIdentifier() == "DTM_RED") return DTM_RED;

    Log::error( Say("Line %n : unknown token '%s'.\n") << value.lineNumber() << value.getIdentifier() );
    s.setError(VLXSerializer::ImportError);
    return DTM_RED;
  }

  inline const char* vlx_ETexCompareMode(ETexCompareMode tcm)
  {
    switch(tcm)
    {
    default:
    case TCM_NONE: return "TCM_NONE";
    // case TCM_COMPARE_R_TO_TEXTURE: return "TCM_COMPARE_R_TO_TEXTURE";
    case TCM_COMPARE_REF_DEPTH_TO_TEXTURE: return "TCM_COMPARE_REF_DEPTH_TO_TEXTURE";
    }
  }

  inline ETexCompareMode vlx_ETexCompareMode(const VLXValue& value, VLXSerializer& s)
  {
    if( value.getIdentifier() == "TCM_COMPARE_R_TO_TEXTURE") return TCM_COMPARE_R_TO_TEXTURE;
    if( value.getIdentifier() == "TCM_COMPARE_REF_DEPTH_TO_TEXTURE") return TCM_COMPARE_REF_DEPTH_TO_TEXTURE;
    if( value.getIdentifier() == "TCM_NONE") return TCM_NONE;

    Log::error( Say("Line %n : unknown token '%s'.\n") << value.lineNumber() << value.getIdentifier() );
    s.setError(VLXSerializer::ImportError);
    return TCM_NONE;
  }

  inline const char* vlx_ETexCompareFunc(ETexCompareFunc tcf)
  {
    switch(tcf)
    {
    default:
    case TCF_LEQUAL: return "TCF_LEQUAL";
    case TCF_GEQUAL: return "TCF_GEQUAL";
    case TCF_LESS: return "TCF_LESS";
    case TCF_GREATER: return "TCF_GREATER";
    case TCF_EQUAL: return "TCF_EQUAL";
    case TCF_NOTEQUAL: return "TCF_NOTEQUAL";
    case TCF_ALWAYS: return "TCF_ALWAYS";
    case TCF_NEVER: return "TCF_NEVER";
    }
  }

  inline ETexCompareFunc vlx_ETexCompareFunc(const VLXValue& value, VLXSerializer& s)
  {
    if( value.getIdentifier() == "TCF_GEQUAL") return TCF_GEQUAL;
    if( value.getIdentifier() == "TCF_LESS") return TCF_LESS;
    if( value.getIdentifier() == "TCF_GREATER") return TCF_GREATER;
    if( value.getIdentifier() == "TCF_EQUAL") return TCF_EQUAL;
    if( value.getIdentifier() == "TCF_NOTEQUAL") return TCF_NOTEQUAL;
    if( value.getIdentifier() == "TCF_ALWAYS") return TCF_ALWAYS;
    if( value.getIdentifier() == "TCF_NEVER") return TCF_NEVER;
    if( value.getIdentifier() == "TCF_LEQUAL") return TCF_LEQUAL;

    Log::error( Say("Line %n : unknown token '%s'.\n") << value.lineNumber() << value.getIdentifier() );
    s.setError(VLXSerializer::ImportError);
    return TCF_LEQUAL;
  }

  inline const char* vlx_ETexParamFilter(ETexParamFilter tpf)
  {
    switch(tpf)
    {
    default:
    case TPF_NEAREST: return "TPF_NEAREST";
    case TPF_LINEAR: return "TPF_LINEAR";
    case TPF_NEAREST_MIPMAP_NEAREST: return "TPF_NEAREST_MIPMAP_NEAREST";
    case TPF_LINEAR_MIPMAP_NEAREST: return "TPF_LINEAR_MIPMAP_NEAREST";
    case TPF_NEAREST_MIPMAP_LINEAR: return "TPF_NEAREST_MIPMAP_LINEAR";
    case TPF_LINEAR_MIPMAP_LINEAR: return "TPF_LINEAR_MIPMAP_LINEAR";
    }
  }

  inline ETexParamFilter vlx_ETexParamFilter(const VLXValue& value, VLXSerializer& s)
  {
    if( value.getIdentifier() == "TPF_LINEAR") return TPF_LINEAR;
    if( value.getIdentifier() == "TPF_NEAREST_MIPMAP_NEAREST") return TPF_NEAREST_MIPMAP_NEAREST;
    if( value.getIdentifier() == "TPF_LINEAR_MIPMAP_NEAREST") return TPF_LINEAR_MIPMAP_NEAREST;
    if( value.getIdentifier() == "TPF_NEAREST_MIPMAP_LINEAR") return TPF_NEAREST_MIPMAP_LINEAR;
    if( value.getIdentifier() == "TPF_LINEAR_MIPMAP_LINEAR") return TPF_LINEAR_MIPMAP_LINEAR;
    if( value.getIdentifier() == "TPF_NEAREST") return TPF_NEAREST;

    Log::error( Say("Line %n : unknown token '%s'.\n") << value.lineNumber() << value.getIdentifier() );
    s.setError(VLXSerializer::ImportError);
    return TPF_NEAREST;
  }

  inline const char* vlx_ETexParamWrap(ETexParamWrap tpw)
  {
    switch(tpw)
    {
    default:
    case TPW_REPEAT: return "TPW_REPEAT";
    case TPW_CLAMP: return "TPW_CLAMP";
    case TPW_CLAMP_TO_BORDER: return "TPW_CLAMP_TO_BORDER";
    case TPW_CLAMP_TO_EDGE: return "TPW_CLAMP_TO_EDGE";
    case TPW_MIRRORED_REPEAT: return "TPW_MIRRORED_REPEAT";
    }
  }

  inline ETexParamWrap vlx_ETexParamWrap(const VLXValue& value, VLXSerializer& s)
  {
    if( value.getIdentifier() == "TPW_CLAMP") return TPW_CLAMP;
    if( value.getIdentifier() == "TPW_CLAMP_TO_BORDER") return TPW_CLAMP_TO_BORDER;
    if( value.getIdentifier() == "TPW_CLAMP_TO_EDGE") return TPW_CLAMP_TO_EDGE;
    if( value.getIdentifier() == "TPW_MIRRORED_REPEAT") return TPW_MIRRORED_REPEAT;
    if( value.getIdentifier() == "TPW_REPEAT") return TPW_REPEAT;

    Log::error( Say("Line %n : unknown token '%s'.\n") << value.lineNumber() << value.getIdentifier() );
    s.setError(VLXSerializer::ImportError);
    return TPW_REPEAT;
  }

  inline const char* vlx_EEnable(EEnable en)
  {
    switch(en)
    {
    default:
      return "EN_UnknownEnable";
    case EN_BLEND: return "EN_BLEND";
    case EN_CULL_FACE: return "EN_CULL_FACE";
    case EN_DEPTH_TEST: return "EN_DEPTH_TEST";
    case EN_STENCIL_TEST: return "EN_STENCIL_TEST";
    case EN_DITHER: return "EN_DITHER";
    case EN_POLYGON_OFFSET_FILL: return "EN_POLYGON_OFFSET_FILL";
    case EN_POLYGON_OFFSET_LINE: return "EN_POLYGON_OFFSET_LINE";
    case EN_POLYGON_OFFSET_POINT: return "EN_POLYGON_OFFSET_POINT";
    case EN_COLOR_LOGIC_OP: return "EN_COLOR_LOGIC_OP";
    case EN_MULTISAMPLE: return "EN_MULTISAMPLE";
    case EN_POINT_SMOOTH: return "EN_POINT_SMOOTH";
    case EN_LINE_SMOOTH: return "EN_LINE_SMOOTH";
    case EN_POLYGON_SMOOTH: return "EN_POLYGON_SMOOTH";
    case EN_LINE_STIPPLE: return "EN_LINE_STIPPLE";
    case EN_POLYGON_STIPPLE: return "EN_POLYGON_STIPPLE";
    case EN_POINT_SPRITE: return "EN_POINT_SPRITE";
    case EN_PROGRAM_POINT_SIZE: return "EN_PROGRAM_POINT_SIZE";
    case EN_ALPHA_TEST: return "EN_ALPHA_TEST";
    case EN_LIGHTING: return "EN_LIGHTING";
    case EN_COLOR_SUM: return "EN_COLOR_SUM";
    case EN_FOG: return "EN_FOG";
    case EN_NORMALIZE: return "EN_NORMALIZE";
    case EN_RESCALE_NORMAL: return "EN_RESCALE_NORMAL";
    case EN_VERTEX_PROGRAM_TWO_SIDE: return "EN_VERTEX_PROGRAM_TWO_SIDE";
    case EN_TEXTURE_CUBE_MAP_SEAMLESS: return "EN_TEXTURE_CUBE_MAP_SEAMLESS";
    case EN_CLIP_DISTANCE0: return "EN_CLIP_DISTANCE0";
    case EN_CLIP_DISTANCE1: return "EN_CLIP_DISTANCE1";
    case EN_CLIP_DISTANCE2: return "EN_CLIP_DISTANCE2";
    case EN_CLIP_DISTANCE3: return "EN_CLIP_DISTANCE3";
    case EN_CLIP_DISTANCE4: return "EN_CLIP_DISTANCE4";
    case EN_CLIP_DISTANCE5: return "EN_CLIP_DISTANCE5";
    case EN_CLIP_DISTANCE6: return "EN_CLIP_DISTANCE6";
    case EN_CLIP_DISTANCE7: return "EN_CLIP_DISTANCE7";
    case EN_SAMPLE_ALPHA_TO_COVERAGE: return "EN_SAMPLE_ALPHA_TO_COVERAGE";
    case EN_SAMPLE_ALPHA_TO_ONE: return "EN_SAMPLE_ALPHA_TO_ONE";
    case EN_SAMPLE_COVERAGE: return "EN_SAMPLE_COVERAGE";
    }
  }

  inline EEnable vlx_EEnable(const VLXValue& value, VLXSerializer& s)
  {
    if( value.getIdentifier() == "EN_BLEND") return EN_BLEND;
    if( value.getIdentifier() == "EN_CULL_FACE") return EN_CULL_FACE;
    if( value.getIdentifier() == "EN_DEPTH_TEST") return EN_DEPTH_TEST;
    if( value.getIdentifier() == "EN_STENCIL_TEST") return EN_STENCIL_TEST;
    if( value.getIdentifier() == "EN_DITHER") return EN_DITHER;
    if( value.getIdentifier() == "EN_POLYGON_OFFSET_FILL") return EN_POLYGON_OFFSET_FILL;
    if( value.getIdentifier() == "EN_POLYGON_OFFSET_LINE") return EN_POLYGON_OFFSET_LINE;
    if( value.getIdentifier() == "EN_POLYGON_OFFSET_POINT") return EN_POLYGON_OFFSET_POINT;
    if( value.getIdentifier() == "EN_COLOR_LOGIC_OP") return EN_COLOR_LOGIC_OP;
    if( value.getIdentifier() == "EN_MULTISAMPLE") return EN_MULTISAMPLE;
    if( value.getIdentifier() == "EN_POINT_SMOOTH") return EN_POINT_SMOOTH;
    if( value.getIdentifier() == "EN_LINE_SMOOTH") return EN_LINE_SMOOTH;
    if( value.getIdentifier() == "EN_POLYGON_SMOOTH") return EN_POLYGON_SMOOTH;
    if( value.getIdentifier() == "EN_LINE_STIPPLE") return EN_LINE_STIPPLE;
    if( value.getIdentifier() == "EN_POLYGON_STIPPLE") return EN_POLYGON_STIPPLE;
    if( value.getIdentifier() == "EN_POINT_SPRITE") return EN_POINT_SPRITE;
    if( value.getIdentifier() == "EN_PROGRAM_POINT_SIZE") return EN_PROGRAM_POINT_SIZE;
    if( value.getIdentifier() == "EN_ALPHA_TEST") return EN_ALPHA_TEST;
    if( value.getIdentifier() == "EN_LIGHTING") return EN_LIGHTING;
    if( value.getIdentifier() == "EN_COLOR_SUM") return EN_COLOR_SUM;
    if( value.getIdentifier() == "EN_FOG") return EN_FOG;
    if( value.getIdentifier() == "EN_NORMALIZE") return EN_NORMALIZE;
    if( value.getIdentifier() == "EN_RESCALE_NORMAL") return EN_RESCALE_NORMAL;
    if( value.getIdentifier() == "EN_VERTEX_PROGRAM_TWO_SIDE") return EN_VERTEX_PROGRAM_TWO_SIDE;
    if( value.getIdentifier() == "EN_TEXTURE_CUBE_MAP_SEAMLESS") return EN_TEXTURE_CUBE_MAP_SEAMLESS;
    if( value.getIdentifier() == "EN_CLIP_DISTANCE0") return EN_CLIP_DISTANCE0;
    if( value.getIdentifier() == "EN_CLIP_DISTANCE1") return EN_CLIP_DISTANCE1;
    if( value.getIdentifier() == "EN_CLIP_DISTANCE2") return EN_CLIP_DISTANCE2;
    if( value.getIdentifier() == "EN_CLIP_DISTANCE3") return EN_CLIP_DISTANCE3;
    if( value.getIdentifier() == "EN_CLIP_DISTANCE4") return EN_CLIP_DISTANCE4;
    if( value.getIdentifier() == "EN_CLIP_DISTANCE5") return EN_CLIP_DISTANCE5;
    if( value.getIdentifier() == "EN_CLIP_DISTANCE6") return EN_CLIP_DISTANCE6;
    if( value.getIdentifier() == "EN_CLIP_DISTANCE7") return EN_CLIP_DISTANCE7;
    if( value.getIdentifier() == "EN_SAMPLE_ALPHA_TO_COVERAGE") return EN_SAMPLE_ALPHA_TO_COVERAGE;
    if( value.getIdentifier() == "EN_SAMPLE_ALPHA_TO_ONE") return EN_SAMPLE_ALPHA_TO_ONE;
    if( value.getIdentifier() == "EN_SAMPLE_COVERAGE") return EN_SAMPLE_COVERAGE;

    Log::error( Say("Line %n : unknown token '%s'.\n") << value.lineNumber() << value.getIdentifier() );
    s.setError(VLXSerializer::ImportError);
    return EN_UnknownEnable;
  }

  inline EPrimitiveType vlx_EPrimitiveType(const VLXValue& value, VLXSerializer& s)
  {
    if( value.getIdentifier() == "PT_POINTS") return PT_POINTS;
    if( value.getIdentifier() == "PT_LINES")  return PT_LINES;
    if( value.getIdentifier() == "PT_LINE_LOOP") return PT_LINE_LOOP;
    if( value.getIdentifier() == "PT_LINE_STRIP") return PT_LINE_STRIP;
    if( value.getIdentifier() == "PT_TRIANGLES") return PT_TRIANGLES;
    if( value.getIdentifier() == "PT_TRIANGLE_STRIP") return PT_TRIANGLE_STRIP;
    if( value.getIdentifier() == "PT_TRIANGLE_FAN") return PT_TRIANGLE_FAN;
    if( value.getIdentifier() == "PT_QUADS") return PT_QUADS;
    if( value.getIdentifier() == "PT_QUAD_STRIP") return PT_QUAD_STRIP;
    if( value.getIdentifier() == "PT_POLYGON") return PT_POLYGON;
    if( value.getIdentifier() == "PT_LINES_ADJACENCY") return PT_LINES_ADJACENCY;
    if( value.getIdentifier() == "PT_LINE_STRIP_ADJACENCY") return PT_LINE_STRIP_ADJACENCY;
    if( value.getIdentifier() == "PT_TRIANGLES_ADJACENCY") return PT_TRIANGLES_ADJACENCY;
    if( value.getIdentifier() == "PT_TRIANGLE_STRIP_ADJACENCY") return PT_TRIANGLES_ADJACENCY;
    if( value.getIdentifier() == "PT_PATCHES") return PT_PATCHES;
    if( value.getIdentifier() == "PT_UNKNOWN") return PT_UNKNOWN;

    Log::error( Say("Line %n : unknown token '%s'.\n") << value.lineNumber() << value.getIdentifier() );
    s.setError(VLXSerializer::ImportError);
    return PT_UNKNOWN;
  }

  inline const char* vlx_EPrimitiveType(EPrimitiveType type)
  {
    switch(type)
    {
      case PT_POINTS:                   return "PT_POINTS"; break;
      case PT_LINES:                    return "PT_LINES"; break;
      case PT_LINE_LOOP:                return "PT_LINE_LOOP"; break;
      case PT_LINE_STRIP:               return "PT_LINE_STRIP"; break;
      case PT_TRIANGLES:                return "PT_TRIANGLES"; break;
      case PT_TRIANGLE_STRIP:           return "PT_TRIANGLE_STRIP"; break;
      case PT_TRIANGLE_FAN:             return "PT_TRIANGLE_FAN"; break;
      case PT_QUADS:                    return "PT_QUADS"; break;
      case PT_QUAD_STRIP:               return "PT_QUAD_STRIP"; break;
      case PT_POLYGON:                  return "PT_POLYGON"; break;
      case PT_LINES_ADJACENCY:          return "PT_LINES_ADJACENCY"; break;
      case PT_LINE_STRIP_ADJACENCY:     return "PT_LINE_STRIP_ADJACENCY"; break;
      case PT_TRIANGLES_ADJACENCY:      return "PT_TRIANGLES_ADJACENCY"; break;
      case PT_TRIANGLE_STRIP_ADJACENCY: return "PT_TRIANGLE_STRIP_ADJACENCY"; break;
      case PT_PATCHES:                  return "PT_PATCHES"; break;
      default:
      case PT_UNKNOWN:                  return "PT_UNKNOWN"; break;
    }
  }

  inline EVertexAttribInterpretation vlx_EVertexAttribInterpretation(const VLXValue& value, VLXSerializer& s)
  {
    if( value.getIdentifier() == "VAI_NORMAL") return VAI_NORMAL;
    if( value.getIdentifier() == "VAI_INTEGER")  return VAI_INTEGER;
    if( value.getIdentifier() == "VAI_DOUBLE") return VAI_DOUBLE;

    Log::error( Say("Line %n : unknown token '%s'.\n") << value.lineNumber() << value.getIdentifier() );
    s.setError(VLXSerializer::ImportError);
    return VAI_NORMAL;
  }

  inline const char* vlx_EVertexAttribInterpretation(EVertexAttribInterpretation type)
  {
    switch(type)
    {
      default:
      case VAI_NORMAL:  return "VAI_NORMAL";  break;
      case VAI_INTEGER: return "VAI_INTEGER"; break;
      case VAI_DOUBLE:  return "VAI_DOUBLE";  break;
    }
  }

  inline ETextureDimension vlx_ETextureDimension(const VLXValue& value, VLXSerializer& s)
  {
    if( value.getIdentifier() == "TD_TEXTURE_1D") return TD_TEXTURE_1D;
    if( value.getIdentifier() == "TD_TEXTURE_2D")  return TD_TEXTURE_2D;
    if( value.getIdentifier() == "TD_TEXTURE_3D") return TD_TEXTURE_3D;
    if( value.getIdentifier() == "TD_TEXTURE_CUBE_MAP") return TD_TEXTURE_CUBE_MAP;
    if( value.getIdentifier() == "TD_TEXTURE_RECTANGLE") return TD_TEXTURE_RECTANGLE;
    if( value.getIdentifier() == "TD_TEXTURE_1D_ARRAY") return TD_TEXTURE_1D_ARRAY;
    if( value.getIdentifier() == "TD_TEXTURE_2D_ARRAY") return TD_TEXTURE_2D_ARRAY;
    if( value.getIdentifier() == "TD_TEXTURE_BUFFER") return TD_TEXTURE_BUFFER;
    if( value.getIdentifier() == "TD_TEXTURE_2D_MULTISAMPLE") return TD_TEXTURE_2D_MULTISAMPLE;
    if( value.getIdentifier() == "TD_TEXTURE_2D_MULTISAMPLE_ARRAY") return TD_TEXTURE_2D_MULTISAMPLE_ARRAY;
    if( value.getIdentifier() == "TD_TEXTURE_UNKNOWN") return TD_TEXTURE_UNKNOWN;

    Log::error( Say("Line %n : unknown token '%s'.\n") << value.lineNumber() << value.getIdentifier() );
    s.setError(VLXSerializer::ImportError);
    return TD_TEXTURE_UNKNOWN;
  }

  inline const char* vlx_ETextureDimension(ETextureDimension td)
  {
    switch(td)
    {
    case TD_TEXTURE_1D: return "TD_TEXTURE_1D";
    case TD_TEXTURE_2D:  return "TD_TEXTURE_2D";
    case TD_TEXTURE_3D: return "TD_TEXTURE_3D";
    case TD_TEXTURE_CUBE_MAP: return "TD_TEXTURE_CUBE_MAP";
    case TD_TEXTURE_RECTANGLE: return "TD_TEXTURE_RECTANGLE";
    case TD_TEXTURE_1D_ARRAY: return "TD_TEXTURE_1D_ARRAY";
    case TD_TEXTURE_2D_ARRAY: return "TD_TEXTURE_2D_ARRAY";
    case TD_TEXTURE_BUFFER: return "TD_TEXTURE_BUFFER";
    case TD_TEXTURE_2D_MULTISAMPLE: return "TD_TEXTURE_2D_MULTISAMPLE";
    case TD_TEXTURE_2D_MULTISAMPLE_ARRAY: return "TD_TEXTURE_2D_MULTISAMPLE_ARRAY";
    case TD_TEXTURE_UNKNOWN: return "TD_TEXTURE_UNKNOWN";
    default:
      Log::error( Say("Invalid texture dimension %n\n") << td );
      VL_TRAP()
      return "TD_TEXTURE_UNKNOWN";
    }
  }

}

#endif
