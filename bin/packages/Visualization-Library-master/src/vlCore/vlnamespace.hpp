/**************************************************************************************/
/*                                                                                    */
/*  Visualization Library                                                             */
/*  http://www.visualizationlibrary.org                                               */
/*                                                                                    */
/*  Copyright (c) 2005-2011, Michele Bosi                                             */
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

/**
 * \file vlnamespace.hpp
 * Visualization Library's enums in the 'vl' namespace.
*/

#ifndef vlnamespace_INCLUDE_ONCE
#define vlnamespace_INCLUDE_ONCE

#include <vlCore/OpenGLDefs.hpp>
#include <vlCore/config.hpp>

namespace vl
{
  typedef enum 
  {
    TF_UNKNOWN = 0,

    TF_ALPHA   = GL_ALPHA, 
    TF_ALPHA4  = GL_ALPHA4, 
    TF_ALPHA8  = GL_ALPHA8, 
    TF_ALPHA12 = GL_ALPHA12, 
    TF_ALPHA16 = GL_ALPHA16, 

    TF_INTENSITY   = GL_INTENSITY, 
    TF_INTENSITY4  = GL_INTENSITY4, 
    TF_INTENSITY8  = GL_INTENSITY8, 
    TF_INTENSITY12 = GL_INTENSITY12, 
    TF_INTENSITY16 = GL_INTENSITY16, 
    TF_LUMINANCE   = GL_LUMINANCE, 
    TF_LUMINANCE4  = GL_LUMINANCE4, 
    TF_LUMINANCE8  = GL_LUMINANCE8, 
    TF_LUMINANCE12 = GL_LUMINANCE12, 
    TF_LUMINANCE16 = GL_LUMINANCE16, 
    TF_LUMINANCE_ALPHA     = GL_LUMINANCE_ALPHA, 
    TF_LUMINANCE4_ALPHA4   = GL_LUMINANCE4_ALPHA4, 
    TF_LUMINANCE6_ALPHA2   = GL_LUMINANCE6_ALPHA2, 
    TF_LUMINANCE8_ALPHA8   = GL_LUMINANCE8_ALPHA8, 
    TF_LUMINANCE12_ALPHA4  = GL_LUMINANCE12_ALPHA4, 
    TF_LUMINANCE12_ALPHA12 = GL_LUMINANCE12_ALPHA12, 
    TF_LUMINANCE16_ALPHA16 = GL_LUMINANCE16_ALPHA16, 
    TF_R3_G3_B2 = GL_R3_G3_B2, 
    TF_RGB      = GL_RGB, 
    TF_RGB4     = GL_RGB4, 
    TF_RGB5     = GL_RGB5, 
    TF_RGB8     = GL_RGB8, 
    TF_RGB10    = GL_RGB10, 
    TF_RGB12    = GL_RGB12, 
    TF_RGB16    = GL_RGB16, 
    TF_RGBA     = GL_RGBA, 
    TF_RGBA2    = GL_RGBA2, 
    TF_RGBA4    = GL_RGBA4, 
    TF_RGB5_A1  = GL_RGB5_A1, 
    TF_RGBA8    = GL_RGBA8, 
    TF_RGB10_A2 = GL_RGB10_A2, 
    TF_RGBA12   = GL_RGBA12, 
    TF_RGBA16   = GL_RGBA16,

    // ARB_texture_float / OpenGL 3
    TF_RGBA32F = GL_RGBA32F,
    TF_RGB32F = GL_RGB32F,
    TF_ALPHA32F = GL_ALPHA32F_ARB,
    TF_INTENSITY32F = GL_INTENSITY32F_ARB,
    TF_LUMINANCE32F = GL_LUMINANCE32F_ARB,
    TF_LUMINANCE_ALPHA32F = GL_LUMINANCE_ALPHA32F_ARB,
    TF_RGBA16F = GL_RGBA16F,
    TF_RGB16F = GL_RGB16F,
    TF_ALPHA16F = GL_ALPHA16F_ARB,
    TF_INTENSITY16F = GL_INTENSITY16F_ARB,
    TF_LUMINANCE16F = GL_LUMINANCE16F_ARB,
    TF_LUMINANCE_ALPHA16F = GL_LUMINANCE_ALPHA16F_ARB,

    // ATI_texture_float (the enums are the same as ARB_texture_float)
    TF_RGBA_FLOAT32_ATI = GL_RGBA_FLOAT32_ATI,
    TF_RGB_FLOAT32_ATI = GL_RGB_FLOAT32_ATI,
    TF_ALPHA_FLOAT32_ATI = GL_ALPHA_FLOAT32_ATI,
    TF_INTENSITY_FLOAT32_ATI = GL_INTENSITY_FLOAT32_ATI,
    TF_LUMINANCE_FLOAT32_ATI = GL_LUMINANCE_FLOAT32_ATI,
    TF_LUMINANCE_ALPHA_FLOAT32_ATI = GL_LUMINANCE_ALPHA_FLOAT32_ATI,
    TF_RGBA_FLOAT16_ATI = GL_RGBA_FLOAT16_ATI,
    TF_RGB_FLOAT16_ATI = GL_RGB_FLOAT16_ATI,
    TF_ALPHA_FLOAT16_ATI = GL_ALPHA_FLOAT16_ATI,
    TF_INTENSITY_FLOAT16_ATI = GL_INTENSITY_FLOAT16_ATI,
    TF_LUMINANCE_FLOAT16_ATI = GL_LUMINANCE_FLOAT16_ATI,
    TF_LUMINANCE_ALPHA_FLOAT16_ATI = GL_LUMINANCE_ALPHA_FLOAT16_ATI,

    // EXT_texture_shared_exponent
    TF_RGB9_E5_EXT = GL_RGB9_E5_EXT,

    // EXT_packed_float
    TF_11F_G11F_B10F_EXT = GL_R11F_G11F_B10F_EXT,

    // EXT_packed_depth_stencil / GL_ARB_framebuffer_object
    TF_DEPTH_STENCIL    = GL_DEPTH_STENCIL,
    TF_DEPTH24_STENCIL8 = GL_DEPTH24_STENCIL8,

    // ARB_depth_buffer_float
    TF_DEPTH_COMPONENT32F = GL_DEPTH_COMPONENT32F,
    TF_DEPTH32F_STENCIL8  = GL_DEPTH32F_STENCIL8,

    // ARB_depth_texture
    TF_DEPTH_COMPONENT   = GL_DEPTH_COMPONENT,
    TF_DEPTH_COMPONENT16 = GL_DEPTH_COMPONENT16,
    TF_DEPTH_COMPONENT24 = GL_DEPTH_COMPONENT24,
    TF_DEPTH_COMPONENT32 = GL_DEPTH_COMPONENT32,

    // ARB_texture_compression
    TF_COMPRESSED_ALPHA           = GL_COMPRESSED_ALPHA_ARB,        
    TF_COMPRESSED_INTENSITY       = GL_COMPRESSED_INTENSITY_ARB,
    TF_COMPRESSED_LUMINANCE       = GL_COMPRESSED_LUMINANCE_ARB,      
    TF_COMPRESSED_LUMINANCE_ALPHA = GL_COMPRESSED_LUMINANCE_ALPHA_ARB,      
    TF_COMPRESSED_RGB             = GL_COMPRESSED_RGB_ARB,        
    TF_COMPRESSED_RGBA            = GL_COMPRESSED_RGBA_ARB,

    // 3DFX_texture_compression_FXT1
    TF_COMPRESSED_RGB_FXT1_3DFX  = GL_COMPRESSED_RGB_FXT1_3DFX,        
    TF_COMPRESSED_RGBA_FXT1_3DFX = GL_COMPRESSED_RGBA_FXT1_3DFX,        

    // EXT_texture_compression_s3tc
    TF_COMPRESSED_RGB_S3TC_DXT1_EXT  = GL_COMPRESSED_RGB_S3TC_DXT1_EXT,                   
    TF_COMPRESSED_RGBA_S3TC_DXT1_EXT = GL_COMPRESSED_RGBA_S3TC_DXT1_EXT,               
    TF_COMPRESSED_RGBA_S3TC_DXT3_EXT = GL_COMPRESSED_RGBA_S3TC_DXT3_EXT,                  
    TF_COMPRESSED_RGBA_S3TC_DXT5_EXT = GL_COMPRESSED_RGBA_S3TC_DXT5_EXT,                 

    // EXT_texture_compression_latc
    TF_COMPRESSED_LUMINANCE_LATC1_EXT              = GL_COMPRESSED_LUMINANCE_LATC1_EXT,
    TF_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT       = GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT,
    TF_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT        = GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT,
    TF_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT = GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT,

    // EXT_texture_compression_rgtc
    TF_COMPRESSED_RED_RGTC1_EXT              = GL_COMPRESSED_RED_RGTC1_EXT,                       
    TF_COMPRESSED_SIGNED_RED_RGTC1_EXT       = GL_COMPRESSED_SIGNED_RED_RGTC1_EXT,                
    TF_COMPRESSED_RED_GREEN_RGTC2_EXT        = GL_COMPRESSED_RED_GREEN_RGTC2_EXT,                 
    TF_COMPRESSED_SIGNED_RED_GREEN_RGTC2_EXT = GL_COMPRESSED_SIGNED_RED_GREEN_RGTC2_EXT,

    // EXT_texture_integer
    TF_RGBA32UI_EXT = GL_RGBA32UI_EXT,           
    TF_RGB32UI_EXT = GL_RGB32UI_EXT,            
    TF_ALPHA32UI_EXT = GL_ALPHA32UI_EXT,          
    TF_INTENSITY32UI_EXT = GL_INTENSITY32UI_EXT,      
    TF_LUMINANCE32UI_EXT = GL_LUMINANCE32UI_EXT,      
    TF_LUMINANCE_ALPHA32UI_EXT = GL_LUMINANCE_ALPHA32UI_EXT,

    TF_RGBA16UI_EXT = GL_RGBA16UI_EXT,           
    TF_RGB16UI_EXT = GL_RGB16UI_EXT,            
    TF_ALPHA16UI_EXT = GL_ALPHA16UI_EXT,          
    TF_INTENSITY16UI_EXT = GL_INTENSITY16UI_EXT,      
    TF_LUMINANCE16UI_EXT = GL_LUMINANCE16UI_EXT,      
    TF_LUMINANCE_ALPHA16UI_EXT = GL_LUMINANCE_ALPHA16UI_EXT, 

    TF_RGBA8UI_EXT = GL_RGBA8UI_EXT,           
    TF_RGB8UI_EXT = GL_RGB8UI_EXT,            
    TF_ALPHA8UI_EXT = GL_ALPHA8UI_EXT,          
    TF_INTENSITY8UI_EXT = GL_INTENSITY8UI_EXT,      
    TF_LUMINANCE8UI_EXT = GL_LUMINANCE8UI_EXT,      
    TF_LUMINANCE_ALPHA8UI_EXT = GL_LUMINANCE_ALPHA8UI_EXT,

    TF_RGBA32I_EXT = GL_RGBA32I_EXT,           
    TF_RGB32I_EXT = GL_RGB32I_EXT,            
    TF_ALPHA32I_EXT = GL_ALPHA32I_EXT,          
    TF_INTENSITY32I_EXT = GL_INTENSITY32I_EXT,      
    TF_LUMINANCE32I_EXT = GL_LUMINANCE32I_EXT,      
    TF_LUMINANCE_ALPHA32I_EXT = GL_LUMINANCE_ALPHA32I_EXT,

    TF_RGBA16I_EXT = GL_RGBA16I_EXT,           
    TF_RGB16I_EXT = GL_RGB16I_EXT,            
    TF_ALPHA16I_EXT = GL_ALPHA16I_EXT,          
    TF_INTENSITY16I_EXT = GL_INTENSITY16I_EXT,      
    TF_LUMINANCE16I_EXT = GL_LUMINANCE16I_EXT,      
    TF_LUMINANCE_ALPHA16I_EXT = GL_LUMINANCE_ALPHA16I_EXT,

    TF_RGBA8I_EXT = GL_RGBA8I_EXT,
    TF_RGB8I_EXT = GL_RGB8I_EXT,
    TF_ALPHA8I_EXT = GL_ALPHA8I_EXT,
    TF_INTENSITY8I_EXT = GL_INTENSITY8I_EXT,
    TF_LUMINANCE8I_EXT = GL_LUMINANCE8I_EXT,
    TF_LUMINANCE_ALPHA8I_EXT = GL_LUMINANCE_ALPHA8I_EXT,

    // GL_ARB_texture_rg
    TF_RED = GL_RED,
    TF_COMPRESSED_RED = GL_COMPRESSED_RED,
    TF_COMPRESSED_RG = GL_COMPRESSED_RG,
    TF_RG = GL_RG,
    TF_R8 = GL_R8,
    TF_R16 = GL_R16,
    TF_RG8 = GL_RG8,
    TF_RG16 = GL_RG16,
    TF_R16F = GL_R16F,
    TF_R32F = GL_R32F,
    TF_RG16F = GL_RG16F,
    TF_RG32F = GL_RG32F,
    TF_R8I = GL_R8I,
    TF_R8UI = GL_R8UI,
    TF_R16I = GL_R16I,
    TF_R16UI = GL_R16UI,
    TF_R32I = GL_R32I,
    TF_R32UI = GL_R32UI,
    TF_RG8I = GL_RG8I,
    TF_RG8UI = GL_RG8UI,
    TF_RG16I = GL_RG16I,
    TF_RG16UI = GL_RG16UI,
    TF_RG32I = GL_RG32I,
    TF_RG32UI = GL_RG32UI,

    // sRGB OpenGL 2.1
    TF_SLUMINANCE_ALPHA = GL_SLUMINANCE_ALPHA,
    TF_SLUMINANCE8_ALPHA8 = GL_SLUMINANCE8_ALPHA8,
    TF_SLUMINANCE = GL_SLUMINANCE,
    TF_SLUMINANCE8 = GL_SLUMINANCE8,
    TF_COMPRESSED_SLUMINANCE = GL_COMPRESSED_SLUMINANCE,
    TF_COMPRESSED_SLUMINANCE_ALPHA = GL_COMPRESSED_SLUMINANCE_ALPHA,

    // sRGB OpenGL 2.1 / 3.x
    TF_SRGB = GL_SRGB,
    TF_SRGB8 = GL_SRGB8,
    TF_SRGB_ALPHA = GL_SRGB_ALPHA,
    TF_SRGB8_ALPHA8 = GL_SRGB8_ALPHA8,
    TF_COMPRESSED_SRGB = GL_COMPRESSED_SRGB,
    TF_COMPRESSED_SRGB_ALPHA = GL_COMPRESSED_SRGB_ALPHA,

    // GL_EXT_texture_sRGB compressed formats
    TF_COMPRESSED_SRGB_S3TC_DXT1_EXT = GL_COMPRESSED_SRGB_S3TC_DXT1_EXT,
    TF_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT = GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT,
    TF_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT = GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT,
    TF_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT = GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT,

    // from table 3.12 opengl api specs 4.1
    TF_R8_SNORM = GL_R8_SNORM,
    TF_R16_SNORM = GL_R16_SNORM,
    TF_RG8_SNORM = GL_RG8_SNORM,
    TF_RG16_SNORM = GL_RG16_SNORM,
    TF_RGB8_SNORM = GL_RGB8_SNORM,
    TF_RGBA8_SNORM = GL_RGBA8_SNORM,
    TF_RGB10_A2UI = GL_RGB10_A2UI,
    TF_RGBA16_SNORM = GL_RGBA16_SNORM,
    TF_R11F_G11F_B10F = GL_R11F_G11F_B10F,
    TF_RGB9_E5 = GL_RGB9_E5,
    TF_RGB8I = GL_RGB8I,
    TF_RGB8UI = GL_RGB8UI,
    TF_RGB16I = GL_RGB16I,
    TF_RGB16UI = GL_RGB16UI,
    TF_RGB32I = GL_RGB32I,
    TF_RGB32UI = GL_RGB32UI,
    TF_RGBA8I = GL_RGBA8I,
    TF_RGBA8UI = GL_RGBA8UI,
    TF_RGBA16I = GL_RGBA16I,
    TF_RGBA16UI = GL_RGBA16UI,
    TF_RGBA32I = GL_RGBA32I,
    TF_RGBA32UI = GL_RGBA32UI

  } ETextureFormat;
    
  typedef enum
  {
    IF_RGB   = GL_RGB,
    IF_RGBA  = GL_RGBA,
    IF_BGR   = GL_BGR,
    IF_BGRA  = GL_BGRA,
    IF_RG    = GL_RG,
    IF_RG_INTEGER = GL_RG_INTEGER,
    IF_RED   = GL_RED,
    IF_GREEN = GL_GREEN,
    IF_BLUE  = GL_BLUE,
    IF_ALPHA = GL_ALPHA,
    IF_LUMINANCE       = GL_LUMINANCE,
    IF_LUMINANCE_ALPHA = GL_LUMINANCE_ALPHA,
    IF_DEPTH_COMPONENT = GL_DEPTH_COMPONENT,
    IF_STENCIL_INDEX   = GL_STENCIL_INDEX,

    // EXT_packed_depth_stencil / GL_ARB_framebuffer_object
    IF_DEPTH_STENCIL = GL_DEPTH_STENCIL,

    // compressed formats
    // note: with these format the type must be IT_IMPLICIT_TYPE

    IF_COMPRESSED_RGB_S3TC_DXT1  = GL_COMPRESSED_RGB_S3TC_DXT1_EXT,
    IF_COMPRESSED_RGBA_S3TC_DXT1 = GL_COMPRESSED_RGBA_S3TC_DXT1_EXT,
    IF_COMPRESSED_RGBA_S3TC_DXT3 = GL_COMPRESSED_RGBA_S3TC_DXT3_EXT,
    IF_COMPRESSED_RGBA_S3TC_DXT5 = GL_COMPRESSED_RGBA_S3TC_DXT5_EXT,

    // GL 3.0 (EXT_texture_integer)
    IF_RED_INTEGER   = GL_RED_INTEGER,
    IF_GREEN_INTEGER = GL_GREEN_INTEGER,
    IF_BLUE_INTEGER  = GL_BLUE_INTEGER,
    IF_ALPHA_INTEGER = GL_ALPHA_INTEGER,
    IF_RGB_INTEGER   = GL_RGB_INTEGER,
    IF_RGBA_INTEGER  = GL_RGBA_INTEGER,
    IF_BGR_INTEGER   = GL_BGR_INTEGER,
    IF_BGRA_INTEGER  = GL_BGRA_INTEGER,

    // EXT_texture_integer
    IF_LUMINANCE_INTEGER = GL_LUMINANCE_INTEGER_EXT,
    IF_LUMINANCE_ALPHA_INTEGER = GL_LUMINANCE_ALPHA_INTEGER_EXT,

  } EImageFormat;

  typedef enum
  {
    T2DT_TEXTURE_2D = GL_TEXTURE_2D,
    T2DT_TEXTURE_CUBE_MAP_POSITIVE_X = GL_TEXTURE_CUBE_MAP_POSITIVE_X,
    T2DT_TEXTURE_CUBE_MAP_NEGATIVE_X = GL_TEXTURE_CUBE_MAP_NEGATIVE_X,
    T2DT_TEXTURE_CUBE_MAP_POSITIVE_Y = GL_TEXTURE_CUBE_MAP_POSITIVE_Y,
    T2DT_TEXTURE_CUBE_MAP_NEGATIVE_Y = GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
    T2DT_TEXTURE_CUBE_MAP_POSITIVE_Z = GL_TEXTURE_CUBE_MAP_POSITIVE_Z,
    T2DT_TEXTURE_CUBE_MAP_NEGATIVE_Z = GL_TEXTURE_CUBE_MAP_NEGATIVE_Z,
    T2DT_TEXTURE_RECTANGLE           = GL_TEXTURE_RECTANGLE_ARB,
    T2DT_TEXTURE_1D_ARRAY            = GL_TEXTURE_1D_ARRAY,
    T2DT_TEXTURE_2D_MULTISAMPLE      = GL_TEXTURE_2D_MULTISAMPLE,
  } ETex2DTarget;

  typedef enum
  {
    AP_NO_ATTACHMENT = 0,

    AP_COLOR_ATTACHMENT0  = GL_COLOR_ATTACHMENT0,
    AP_COLOR_ATTACHMENT1  = GL_COLOR_ATTACHMENT1,
    AP_COLOR_ATTACHMENT2  = GL_COLOR_ATTACHMENT2,
    AP_COLOR_ATTACHMENT3  = GL_COLOR_ATTACHMENT3,
    AP_COLOR_ATTACHMENT4  = GL_COLOR_ATTACHMENT4,
    AP_COLOR_ATTACHMENT5  = GL_COLOR_ATTACHMENT5,
    AP_COLOR_ATTACHMENT6  = GL_COLOR_ATTACHMENT6,
    AP_COLOR_ATTACHMENT7  = GL_COLOR_ATTACHMENT7,
    AP_COLOR_ATTACHMENT8  = GL_COLOR_ATTACHMENT8,
    AP_COLOR_ATTACHMENT9  = GL_COLOR_ATTACHMENT9,
    AP_COLOR_ATTACHMENT10 = GL_COLOR_ATTACHMENT10,
    AP_COLOR_ATTACHMENT11 = GL_COLOR_ATTACHMENT11,
    AP_COLOR_ATTACHMENT12 = GL_COLOR_ATTACHMENT12,
    AP_COLOR_ATTACHMENT13 = GL_COLOR_ATTACHMENT13,
    AP_COLOR_ATTACHMENT14 = GL_COLOR_ATTACHMENT14,
    AP_COLOR_ATTACHMENT15 = GL_COLOR_ATTACHMENT15,

    AP_DEPTH_ATTACHMENT   = GL_DEPTH_ATTACHMENT,

    AP_STENCIL_ATTACHMENT = GL_STENCIL_ATTACHMENT,

    AP_DEPTH_STENCIL_ATTACHMENT = GL_DEPTH_STENCIL_ATTACHMENT,
  } EAttachmentPoint;

  //! Color-renderable formats as defined in section 4.4.4 of opengl api specs 4.1
  typedef enum
  {
    CBF_RED = GL_RED,
    CBF_RG = GL_RG,
    CBF_RGB = GL_RGB,
    CBF_RGBA = GL_RGBA,

    // see table 3.12 opengl api specs 4.1
    CBF_R8 = GL_R8,
    // CBF_R8_SNORM = GL_R8_SNORM,
    CBF_R16 = GL_R16,
    // CBF_R16_SNORM = GL_R16_SNORM,
    CBF_RG8 = GL_RG8,
    // CBF_RG8_SNORM = GL_RG8_SNORM,
    CBF_RG16 = GL_RG16,
    // CBF_RG16_SNORM = GL_RG16_SNORM,
    // CBF_R3_G3_B2 = GL_R3_G3_B2,
    // CBF_RGB4 = GL_RGB4,
    // CBF_RGB5 = GL_RGB5,
    // CBF_RGB8 = GL_RGB8,
    // CBF_RGB8_SNORM = GL_RGB8_SNORM,
    // CBF_RGB10 = GL_RGB10,
    // CBF_RGB12 = GL_RGB12,
    // CBF_RGB16 = GL_RGB16,
    // CBF_RGB16_SNORM = GL_RGB16_SNORM,
    // CBF_RGBA2 = GL_RGBA2,
    // CBF_RGBA4 = GL_RGBA4,
    // CBF_RGB5_A1 = GL_RGB5_A1,
    CBF_RGBA8 = GL_RGBA8,
    // CBF_RGBA8_SNORM = GL_RGBA8_SNORM,
    CBF_RGB10_A2 = GL_RGB10_A2,
    CBF_RGB10_A2UI = GL_RGB10_A2UI,
    // CBF_RGBA12 = GL_RGBA12,
    CBF_RGBA16 = GL_RGBA16,
    // CBF_RGBA16_SNORM = GL_RGBA16_SNORM,
    // CBF_SRGB8 = GL_SRGB8,
    CBF_SRGB8_ALPHA8 = GL_SRGB8_ALPHA8,
    CBF_R16F = GL_R16F,
    CBF_RG16F = GL_RG16F,
    // CBF_RGB16F = GL_RGB16F,
    CBF_RGBA16F = GL_RGBA16F,
    CBF_R32F = GL_R32F,
    CBF_RG32F = GL_RG32F,
    // CBF_RGB32F = GL_RGB32F,
    CBF_RGBA32F = GL_RGBA32F,
    CBF_R11F_G11F_B10F = GL_R11F_G11F_B10F,
    // CBF_RGB9_E5 = GL_RGB9_E5,
    CBF_R8I = GL_R8I,
    CBF_R8UI = GL_R8UI,
    CBF_R16I = GL_R16I,
    CBF_R16UI = GL_R16UI,
    CBF_R32I = GL_R32I,
    CBF_R32UI = GL_R32UI,
    CBF_RG8I = GL_RG8I,
    CBF_RG8UI = GL_RG8UI,
    CBF_RG16I = GL_RG16I,
    CBF_RG16UI = GL_RG16UI,
    CBF_RG32I = GL_RG32I,
    CBF_RG32UI = GL_RG32UI,
    // CBF_RGB8I = GL_RGB8I,
    // CBF_RGB8UI = GL_RGB8UI,
    // CBF_RGB16I = GL_RGB16I,
    // CBF_RGB16UI = GL_RGB16UI,
    // CBF_RGB32I = GL_RGB32I,
    // CBF_RGB32UI = GL_RGB32UI,
    CBF_RGBA8I = GL_RGBA8I,
    CBF_RGBA8UI = GL_RGBA8UI,
    CBF_RGBA16I = GL_RGBA16I,
    CBF_RGBA16UI = GL_RGBA16UI,
    CBF_RGBA32I = GL_RGBA32I,
    CBF_RGBA32UI = GL_RGBA32UI

  } EColorBufferFormat;

  typedef enum
  {
    DBF_DEPTH_COMPONENT    = GL_DEPTH_COMPONENT,
    DBF_DEPTH_COMPONENT16  = GL_DEPTH_COMPONENT16,
    DBF_DEPTH_COMPONENT24  = GL_DEPTH_COMPONENT24,
    DBF_DEPTH_COMPONENT32  = GL_DEPTH_COMPONENT32,
    DBF_DEPTH_COMPONENT32F = GL_DEPTH_COMPONENT32F,
  } EDepthBufferFormat;

  typedef enum
  {
    SBF_STENCIL_INDEX1  = GL_STENCIL_INDEX1_EXT,
    SBF_STENCIL_INDEX4  = GL_STENCIL_INDEX4_EXT,
    SBF_STENCIL_INDEX8  = GL_STENCIL_INDEX8_EXT,
    SBF_STENCIL_INDEX16 = GL_STENCIL_INDEX16_EXT
  } EStencilBufferFormat;

  typedef enum
  {
    DSBT_DEPTH_STENCIL     = GL_DEPTH_STENCIL,
    DSBT_DEPTH24_STENCIL8  = GL_DEPTH24_STENCIL8,
    DSBT_DEPTH32F_STENCIL8 = GL_DEPTH32F_STENCIL8,
  } EDepthStencilBufferFormat;

  typedef enum
  {
    CF_DO_NOT_CLEAR        = 0,
    CF_CLEAR_COLOR         = GL_COLOR_BUFFER_BIT,
    CF_CLEAR_DEPTH         = GL_DEPTH_BUFFER_BIT,
    CF_CLEAR_STENCIL       = GL_STENCIL_BUFFER_BIT,
    CF_CLEAR_COLOR_DEPTH   = CF_CLEAR_COLOR + CF_CLEAR_DEPTH,
    CF_CLEAR_COLOR_STENCIL = CF_CLEAR_COLOR + CF_CLEAR_STENCIL,
    CF_CLEAR_DEPTH_STENCIL = CF_CLEAR_DEPTH + CF_CLEAR_STENCIL,
    CF_CLEAR_COLOR_DEPTH_STENCIL = CF_CLEAR_COLOR + CF_CLEAR_DEPTH + CF_CLEAR_STENCIL,
  } EClearFlags;
  
  typedef enum
  {
    CCM_Float,
    CCM_Int,
    CCM_UInt
  } EClearColorMode;

  typedef enum 
  { 
    BF_ZERO=GL_ZERO, 
    BF_ONE=GL_ONE, 
    BF_SRC_COLOR=GL_SRC_COLOR, 
    BF_ONE_MINUS_SRC_COLOR=GL_ONE_MINUS_SRC_COLOR, 
    BF_DST_COLOR=GL_DST_COLOR, 
    BF_ONE_MINUS_DST_COLOR=GL_ONE_MINUS_DST_COLOR, 
    BF_SRC_ALPHA=GL_SRC_ALPHA, 
    BF_ONE_MINUS_SRC_ALPHA=GL_ONE_MINUS_SRC_ALPHA, 
    BF_DST_ALPHA=GL_DST_ALPHA, 
    BF_ONE_MINUS_DST_ALPHA=GL_ONE_MINUS_DST_ALPHA, 
    BF_CONSTANT_COLOR=GL_CONSTANT_COLOR, 
    BF_ONE_MINUS_CONSTANT_COLOR=GL_ONE_MINUS_CONSTANT_COLOR, 
    BF_CONSTANT_ALPHA=GL_CONSTANT_ALPHA, 
    BF_ONE_MINUS_CONSTANT_ALPHA=GL_ONE_MINUS_CONSTANT_ALPHA, 
    BF_SRC_ALPHA_SATURATE=GL_SRC_ALPHA_SATURATE
  } EBlendFactor;

  typedef enum 
  { 
    TD_TEXTURE_UNKNOWN = 0,
    TD_TEXTURE_1D = GL_TEXTURE_1D, 
    TD_TEXTURE_2D = GL_TEXTURE_2D, 
    TD_TEXTURE_3D = GL_TEXTURE_3D, 
    TD_TEXTURE_CUBE_MAP  = GL_TEXTURE_CUBE_MAP,
    TD_TEXTURE_RECTANGLE = GL_TEXTURE_RECTANGLE,
    TD_TEXTURE_1D_ARRAY  = GL_TEXTURE_1D_ARRAY, 
    TD_TEXTURE_2D_ARRAY  = GL_TEXTURE_2D_ARRAY, 
    TD_TEXTURE_BUFFER    = GL_TEXTURE_BUFFER,
    TD_TEXTURE_2D_MULTISAMPLE = GL_TEXTURE_2D_MULTISAMPLE,
    TD_TEXTURE_2D_MULTISAMPLE_ARRAY = GL_TEXTURE_2D_MULTISAMPLE_ARRAY
  } ETextureDimension;

  typedef enum
  {
    TCM_NONE = GL_NONE,
    TCM_COMPARE_R_TO_TEXTURE = GL_COMPARE_R_TO_TEXTURE,
    TCM_COMPARE_REF_DEPTH_TO_TEXTURE = GL_COMPARE_REF_DEPTH_TO_TEXTURE_EXT
  } ETexCompareMode;

  typedef enum
  { 
    TCF_LEQUAL   = GL_LEQUAL,
    TCF_GEQUAL   = GL_GEQUAL,
    TCF_LESS     = GL_LESS,
    TCF_GREATER  = GL_GREATER,
    TCF_EQUAL    = GL_EQUAL,
    TCF_NOTEQUAL = GL_NOTEQUAL,
    TCF_ALWAYS   = GL_ALWAYS,
    TCF_NEVER    = GL_NEVER
  } ETexCompareFunc;

  typedef enum 
  {
    DTM_LUMINANCE = GL_LUMINANCE, 
    DTM_INTENSITY = GL_INTENSITY, 
    DTM_ALPHA     = GL_ALPHA,
    DTM_RED       = GL_RED
  } EDepthTextureMode;

  typedef enum
  {
    RDB_NONE        = GL_NONE,
    RDB_FRONT_LEFT  = GL_FRONT_LEFT,
    RDB_FRONT_RIGHT = GL_FRONT_RIGHT,
    RDB_BACK_LEFT   = GL_BACK_LEFT,
    RDB_BACK_RIGHT  = GL_BACK_RIGHT,
    RDB_AUX0        = GL_AUX0,
    RDB_AUX1        = GL_AUX1,
    RDB_AUX2        = GL_AUX2,
    RDB_AUX3        = GL_AUX3,
    RDB_COLOR_ATTACHMENT0  = GL_COLOR_ATTACHMENT0_EXT,
    RDB_COLOR_ATTACHMENT1  = GL_COLOR_ATTACHMENT1_EXT,
    RDB_COLOR_ATTACHMENT2  = GL_COLOR_ATTACHMENT2_EXT,
    RDB_COLOR_ATTACHMENT3  = GL_COLOR_ATTACHMENT3_EXT,
    RDB_COLOR_ATTACHMENT4  = GL_COLOR_ATTACHMENT4_EXT,
    RDB_COLOR_ATTACHMENT5  = GL_COLOR_ATTACHMENT5_EXT,
    RDB_COLOR_ATTACHMENT6  = GL_COLOR_ATTACHMENT6_EXT,
    RDB_COLOR_ATTACHMENT7  = GL_COLOR_ATTACHMENT7_EXT,
    RDB_COLOR_ATTACHMENT8  = GL_COLOR_ATTACHMENT8_EXT,
    RDB_COLOR_ATTACHMENT9  = GL_COLOR_ATTACHMENT9_EXT,
    RDB_COLOR_ATTACHMENT10 = GL_COLOR_ATTACHMENT10_EXT,
    RDB_COLOR_ATTACHMENT11 = GL_COLOR_ATTACHMENT11_EXT,
    RDB_COLOR_ATTACHMENT12 = GL_COLOR_ATTACHMENT12_EXT,
    RDB_COLOR_ATTACHMENT13 = GL_COLOR_ATTACHMENT13_EXT,
    RDB_COLOR_ATTACHMENT14 = GL_COLOR_ATTACHMENT14_EXT,
    RDB_COLOR_ATTACHMENT15 = GL_COLOR_ATTACHMENT15_EXT
  } EReadDrawBuffer;

  typedef enum
  {
    FBB_FRAMEBUFFER      = GL_FRAMEBUFFER,
    FBB_DRAW_FRAMEBUFFER = GL_DRAW_FRAMEBUFFER,
    FBB_READ_FRAMEBUFFER = GL_READ_FRAMEBUFFER,
  } EFramebufferBind;

  typedef enum
  {
    IT_IMPLICIT_TYPE = 0, //!< The type is implicitly defined by the EImageFormat value, for ex. IF_COMPRESSED_RGB_S3TC_DXT1.

    IT_UNSIGNED_BYTE  = GL_UNSIGNED_BYTE,
    IT_BYTE           = GL_BYTE,
    IT_UNSIGNED_SHORT = GL_UNSIGNED_SHORT,
    IT_SHORT          = GL_SHORT,
    IT_UNSIGNED_INT   = GL_UNSIGNED_INT,
    IT_INT            = GL_INT,
    IT_FLOAT          = GL_FLOAT,
    IT_UNSIGNED_BYTE_3_3_2         = GL_UNSIGNED_BYTE_3_3_2,
    IT_UNSIGNED_BYTE_2_3_3_REV     = GL_UNSIGNED_BYTE_2_3_3_REV,
    IT_UNSIGNED_SHORT_5_6_5        = GL_UNSIGNED_SHORT_5_6_5,
    IT_UNSIGNED_SHORT_5_6_5_REV    = GL_UNSIGNED_SHORT_5_6_5_REV,
    IT_UNSIGNED_SHORT_4_4_4_4      = GL_UNSIGNED_SHORT_4_4_4_4,
    IT_UNSIGNED_SHORT_4_4_4_4_REV  = GL_UNSIGNED_SHORT_4_4_4_4_REV,
    IT_UNSIGNED_SHORT_5_5_5_1      = GL_UNSIGNED_SHORT_5_5_5_1,
    IT_UNSIGNED_SHORT_1_5_5_5_REV  = GL_UNSIGNED_SHORT_1_5_5_5_REV,
    IT_UNSIGNED_INT_8_8_8_8        = GL_UNSIGNED_INT_8_8_8_8,
    IT_UNSIGNED_INT_8_8_8_8_REV    = GL_UNSIGNED_INT_8_8_8_8_REV,
    IT_UNSIGNED_INT_10_10_10_2     = GL_UNSIGNED_INT_10_10_10_2,
    IT_UNSIGNED_INT_2_10_10_10_REV = GL_UNSIGNED_INT_2_10_10_10_REV,

    IT_UNSIGNED_INT_5_9_9_9_REV       = GL_UNSIGNED_INT_5_9_9_9_REV_EXT,     /* EXT_texture_shared_exponent, supports only GL_RGB */
    IT_UNSIGNED_INT_10F_11F_11F_REV   = GL_UNSIGNED_INT_10F_11F_11F_REV_EXT, /* EXT_packed_float, supports only GL_RGB */
    IT_UNSIGNED_INT_24_8              = GL_UNSIGNED_INT_24_8,                /* EXT_packed_depth_stencil/GL_ARB_framebuffer_object */
    IT_FLOAT_32_UNSIGNED_INT_24_8_REV = GL_FLOAT_32_UNSIGNED_INT_24_8_REV    /* ARB_depth_buffer_float */

  } EImageType;

  typedef enum
  {
    PT_POINTS         = GL_POINTS,
    PT_LINES          = GL_LINES,
    PT_LINE_LOOP      = GL_LINE_LOOP,

    PT_LINE_STRIP     = GL_LINE_STRIP,
    PT_TRIANGLES      = GL_TRIANGLES,
    PT_TRIANGLE_STRIP = GL_TRIANGLE_STRIP,
    PT_TRIANGLE_FAN   = GL_TRIANGLE_FAN,
    PT_QUADS          = GL_QUADS,
    PT_QUAD_STRIP     = GL_QUAD_STRIP,
    PT_POLYGON        = GL_POLYGON,

    // geometry shader
    PT_LINES_ADJACENCY          = GL_LINES_ADJACENCY_EXT,
    PT_LINE_STRIP_ADJACENCY     = GL_LINE_STRIP_ADJACENCY_EXT,
    PT_TRIANGLES_ADJACENCY      = GL_TRIANGLES_ADJACENCY_EXT,
    PT_TRIANGLE_STRIP_ADJACENCY = GL_TRIANGLE_STRIP_ADJACENCY_EXT,

    // GL_ARB_tessellation_shader
    PT_PATCHES = GL_PATCHES,

    PT_UNKNOWN,

  } EPrimitiveType;

  typedef enum 
  { 
    PF_FRONT=GL_FRONT, 
    PF_BACK=GL_BACK, 
    PF_FRONT_AND_BACK=GL_FRONT_AND_BACK 
  } EPolygonFace;

  typedef enum 
  { 
    HM_FASTEST=GL_FASTEST, 
    HM_NICEST=GL_NICEST, 
    HM_DONT_CARE=GL_DONT_CARE 
  } EHintMode;

  typedef enum 
  { 
    FF_CW=GL_CW, 
    FF_CCW=GL_CCW 
  } EFrontFace;

  typedef enum 
  { 
    FU_NEVER=GL_NEVER, 
    FU_LESS=GL_LESS, 
    FU_EQUAL=GL_EQUAL, 
    FU_LEQUAL=GL_LEQUAL, 
    FU_GREATER=GL_GREATER, 
    FU_NOTEQUAL=GL_NOTEQUAL, 
    FU_GEQUAL=GL_GEQUAL, 
    FU_ALWAYS=GL_ALWAYS 
  } EFunction;

  typedef enum 
  { 
    PM_FILL=GL_FILL, 
    PM_LINE=GL_LINE, 
    PM_POINT=GL_POINT 
  } EPolygonMode;

  typedef enum 
  { 
    SM_FLAT=GL_FLAT, 
    SM_SMOOTH=GL_SMOOTH 
  } EShadeModel;

  typedef enum 
  {
    BE_FUNC_ADD=GL_FUNC_ADD, 
    BE_FUNC_SUBTRACT=GL_FUNC_SUBTRACT, 
    BE_FUNC_REVERSE_SUBTRACT=GL_FUNC_REVERSE_SUBTRACT, 
    BE_MIN=GL_MIN, 
    BE_MAX=GL_MAX 
  } EBlendEquation;

  typedef enum 
  { 
    CM_EMISSION=GL_EMISSION, 
    CM_AMBIENT=GL_AMBIENT, 
    CM_DIFFUSE=GL_DIFFUSE, 
    CM_SPECULAR=GL_SPECULAR, 
    CM_AMBIENT_AND_DIFFUSE=GL_AMBIENT_AND_DIFFUSE 
  } EColorMaterial;

  typedef enum 
  { 
    CC_SEPARATE_SPECULAR_COLOR=GL_SEPARATE_SPECULAR_COLOR, 
    CC_SINGLE_COLOR=GL_SINGLE_COLOR 
  } EColorControl;

  typedef enum 
  {
    FM_LINEAR=GL_LINEAR, 
    FM_EXP=GL_EXP, 
    FM_EXP2=GL_EXP2 
  } EFogMode;

  typedef enum 
  { 
    LO_CLEAR=GL_CLEAR, 
    LO_SET=GL_SET, 
    LO_COPY=GL_COPY, 
    LO_COPY_INVERTED=GL_COPY_INVERTED, 
    LO_NOOP=GL_NOOP, 
    LO_INVERT=GL_INVERT, 
    LO_AND=GL_AND, 
    LO_NAND=GL_NAND, 
    LO_OR=GL_OR, 
    LO_NOR=GL_NOR, 
    LO_XOR=GL_XOR, 
    LO_EQUIV=GL_EQUIV, 
    LO_AND_REVERSE=GL_AND_REVERSE, 
    LO_AND_INVERTED=GL_AND_INVERTED, 
    LO_OR_REVERSE=GL_OR_REVERSE, 
    LO_OR_INVERTED=GL_OR_INVERTED 
  } ELogicOp;

  typedef enum 
  { 
    SO_KEEP=GL_KEEP, 
    SO_ZERO=GL_ZERO, 
    SO_REPLACE=GL_REPLACE, 
    SO_INCR=GL_INCR, 
    SO_INCR_WRAP=GL_INCR_WRAP, 
    SO_DECR=GL_DECR, 
    SO_DECR_WRAP=GL_DECR_WRAP, 
    SO_INVERT=GL_INVERT 
  } EStencilOp;

 typedef enum 
  { 
    TPF_NEAREST=GL_NEAREST, 
    TPF_LINEAR=GL_LINEAR, 
    TPF_NEAREST_MIPMAP_NEAREST=GL_NEAREST_MIPMAP_NEAREST, 
    TPF_LINEAR_MIPMAP_NEAREST=GL_LINEAR_MIPMAP_NEAREST, 
    TPF_NEAREST_MIPMAP_LINEAR=GL_NEAREST_MIPMAP_LINEAR, 
    TPF_LINEAR_MIPMAP_LINEAR=GL_LINEAR_MIPMAP_LINEAR 
  } ETexParamFilter;

  typedef enum 
  { 
    TPW_CLAMP           = GL_CLAMP, 
    TPW_CLAMP_TO_BORDER = GL_CLAMP_TO_BORDER,
    TPW_CLAMP_TO_EDGE   = GL_CLAMP_TO_EDGE,
    TPW_MIRRORED_REPEAT = GL_MIRRORED_REPEAT,
    TPW_REPEAT          = GL_REPEAT
  } ETexParamWrap;

  typedef enum 
  {
    TEM_DECAL=GL_DECAL, 
    TEM_MODULATE=GL_MODULATE, 
    TEM_ADD=GL_ADD, 
    TEM_BLEND=GL_BLEND, 
    TEM_REPLACE=GL_REPLACE, 
    TEM_COMBINE=GL_COMBINE, 
    TEM_ADD_SIGN=GL_ADD_SIGNED, 
    TEM_INTERPOLATE=GL_INTERPOLATE, 
    TEM_SUBTRACT=GL_SUBTRACT, 
    TEM_DOT3_RGB=GL_DOT3_RGB, 
    TEM_DOT3_RGBA=GL_DOT3_RGBA 
  } ETexEnvMode;

  typedef enum 
  {
    TES_TEXTURE=GL_TEXTURE, 
    TES_TEXTURE0=GL_TEXTURE0, 
    TES_TEXTURE1=GL_TEXTURE1, 
    TES_TEXTURE2=GL_TEXTURE2, 
    TES_TEXTURE3=GL_TEXTURE3, 
    TES_TEXTURE4=GL_TEXTURE4, 
    TES_TEXTURE5=GL_TEXTURE5, 
    TES_TEXTURE6=GL_TEXTURE6, 
    TES_TEXTURE7=GL_TEXTURE7, 
    TES_CONSTANT=GL_CONSTANT,
    TES_PRIMARY_COLOR=GL_PRIMARY_COLOR, 
    TES_PREVIOUS=GL_PREVIOUS
  } ETexEnvSource;

  typedef enum 
  {
    TEO_SRC_COLOR=GL_SRC_COLOR, 
    TEO_ONE_MINUS_SRC_COLOR=GL_ONE_MINUS_SRC_COLOR,
    TEO_SRC_ALPHA=GL_SRC_ALPHA, 
    TEO_ONE_MINUS_SRC_ALPHA=GL_ONE_MINUS_SRC_ALPHA
  } ETexEnvOperand;

  //! Texture generation modes, see also http://www.opengl.org/sdk/docs/man/xhtml/glTexGen.xml for more information.
  typedef enum 
  {
    TGM_DISABLED = 0, 
    TGM_EYE_LINEAR = GL_EYE_LINEAR,         //!< Not supported under OpenGL ES 1.x
    TGM_OBJECT_LINEAR = GL_OBJECT_LINEAR,   //!< Not supported under OpenGL ES 1.x
    TGM_SPHERE_MAP = GL_SPHERE_MAP,         //!< Not supported under OpenGL ES 1.x
    TGM_REFLECTION_MAP = GL_REFLECTION_MAP,
    TGM_NORMAL_MAP = GL_NORMAL_MAP           
  } ETexGenMode;

  //! Constant that enable/disable a specific OpenGL feature, see also Shader, Shader::enable(), Shader::disable(), Shader::isEnabled()
  typedef enum 
  {
    // Common ones
    EN_BLEND, //!< If enabled, blend the incoming RGBA color values with the values in the color buffers, see also BlendFunc for more information.
    EN_CULL_FACE, //!< If enabled, cull polygons based on their winding in window coordinates, see also CullFace. 
    EN_DEPTH_TEST, //!< If enabled, do depth comparisons and update the depth buffer; Note that even if the depth buffer exists and the depth mask is non-zero, the depth buffer is not updated if the depth test is disabled, see also DepthFunc and DepthRange.
    EN_STENCIL_TEST, //!< If enabled, do stencil testing and update the stencil buffer, see also StencilFunc and StencilOp.
    EN_DITHER, //!< If enabled, dither color components or indices before they are written to the color buffer.
    EN_POLYGON_OFFSET_FILL, //!< If enabled, and if the polygon is rendered in GL_FILL mode, an offset is added to depth values of a polygon's fragments before the depth comparison is performed, see also PolygonOffset.
    EN_POLYGON_OFFSET_LINE, //!< If enabled, and if the polygon is rendered in GL_LINE mode, an offset is added to depth values of a polygon's fragments before the depth comparison is performed, see also PolygonOffset.
    EN_POLYGON_OFFSET_POINT, //!< If enabled, an offset is added to depth values of a polygon's fragments before the depth comparison is performed, if the polygon is rendered in GL_POINT mode, see also PolygonOffset.
    EN_COLOR_LOGIC_OP, //!< If enabled, apply the currently selected logical operation to the incoming RGBA color and color buffer values, see also LogicOp.
    EN_MULTISAMPLE, //!< If enabled, use multiple fragment samples in computing the final color of a pixel. See glSampleCoverage.

    // Smoothing
    EN_POINT_SMOOTH, //!< If enabled, draw points with proper filtering; Otherwise, draw aliased points, see also PointSize.
    EN_LINE_SMOOTH, //!< If enabled, draw lines with correct filtering; Otherwise, draw aliased lines, see also LineWidth.
    EN_POLYGON_SMOOTH, //!< If enabled, draw polygons with proper filtering; Otherwise, draw aliased polygons; For correct antialiased polygons, an alpha buffer is needed and the polygons must be sorted front to back.

    // Stippling
    EN_LINE_STIPPLE, //!< If enabled, use the current line stipple pattern when drawing lines, see also LineStipple.
    EN_POLYGON_STIPPLE, //!< If enabled, use the current polygon stipple pattern when rendering polygons, see also PolygonStipple.

    // Point sprites
    EN_POINT_SPRITE, //!< If enabled, calculate texture coordinates for points based on texture environment and point parameter settings; Otherwise texture coordinates are constant across points.
    EN_PROGRAM_POINT_SIZE, //!< [GL_VERTEX_PROGRAM_POINT_SIZE/GL_PROGRAM_POINT_SIZE] If enabled, and a vertex shader is active, then the derived point size is taken from the (potentially clipped) shader builtin \p gl_PointSize and clamped to the implementation-dependent point size range|

    // Fixed function pipeline
    EN_ALPHA_TEST, //!< If enabled, performs alpha testing, see also AlphaFunc for more information.
    EN_LIGHTING, //!< If enabled, use the current lighting parameters to compute the vertex color; Otherwise, simply associate the current color with each vertex, see also Material, LightModel, and Light.
    EN_COLOR_SUM, //!< If enabled, add the secondary color value to the computed fragment color. 
    EN_FOG, //!< If enabled, blend a fog color into the post-texturing color, see also Fog.
    EN_NORMALIZE, //!< If enabled, normal vectors are scaled to unit length after transformation, see also vl::EN_RESCALE_NORMAL.
    EN_RESCALE_NORMAL, //!< If enabled, normals are scaled by a scaling factor derived from the modelview matrix; vl::EN_RESCALE_NORMAL requires that the originally specified normals were of unit length, and that the modelview matrix contain only uniform scales for proper results, see also vl::EN_NORMALIZE.

    // Available only under OpenGL 2.x
    EN_VERTEX_PROGRAM_TWO_SIDE, //!< If enabled, and a vertex shader is active, it specifies that the GL will choose between front and back colors based on the polygon's face direction of which the vertex being shaded is a part; It has no effect on points or lines.

    // OpenGL 3.2
    EN_TEXTURE_CUBE_MAP_SEAMLESS, //!< If enabled, cubemap textures are sampled such that when linearly sampling from the border between two adjacent faces, texels from both faces are used to generate the final sample value. When disabled, texels from only a single face are used to construct the final sample value.

    // OpenGL 3.0
    EN_CLIP_DISTANCE0, //!< If enabled, clip geometry against user-defined half space #0.
    EN_CLIP_DISTANCE1, //!< If enabled, clip geometry against user-defined half space #1.
    EN_CLIP_DISTANCE2, //!< If enabled, clip geometry against user-defined half space #2.
    EN_CLIP_DISTANCE3, //!< If enabled, clip geometry against user-defined half space #3.
    EN_CLIP_DISTANCE4, //!< If enabled, clip geometry against user-defined half space #4.
    EN_CLIP_DISTANCE5, //!< If enabled, clip geometry against user-defined half space #5.
    EN_CLIP_DISTANCE6, //!< If enabled, clip geometry against user-defined half space #6.
    EN_CLIP_DISTANCE7, //!< If enabled, clip geometry against user-defined half space #7.

    // Multisampling
    EN_SAMPLE_ALPHA_TO_COVERAGE, //!< If enabled, compute a temporary coverage value where each bit is determined by the alpha value at the corresponding sample location; The temporary coverage value is then ANDed with the fragment coverage value.
    EN_SAMPLE_ALPHA_TO_ONE, //!< If enabled, each sample alpha value is replaced by the maximum representable alpha value.
    EN_SAMPLE_COVERAGE, //!< If enabled, the fragment's coverage is ANDed with the temporary coverage value; If GL_SAMPLE_COVERAGE_INVERT is set to GL_TRUE, invert the coverage value, see also SampleCoverage.

    EN_EnableCount, //!< For internal use only.

    EN_UnknownEnable //!< For internal use only.

  } EEnable;

  typedef enum
  {
    BU_STREAM_DRAW = GL_STREAM_DRAW,   //!< Data is specified once and used at most a few times as the source of drawing and image specification commands.
    BU_STREAM_READ = GL_STREAM_READ,   //!< Data is copied once from an OpenGL buffer and is used at most a few times by the application as data values.
    BU_STREAM_COPY = GL_STREAM_COPY,   //!< Data is copied once from an OpenGL buffer and is used at most a few times as the source for drawing or image specification commands.
    BU_STATIC_DRAW = GL_STATIC_DRAW,   //!< Data is specified once and used many times as the source of drawing or image specification commands.
    BU_STATIC_READ = GL_STATIC_READ,   //!< Data is copied once from an OpenGL buffer and is used many times by the application as data values.
    BU_STATIC_COPY = GL_STATIC_COPY,   //!< Data is copied once from an OpenGL buffer and is used many times as the source for drawing or image specification commands.
    BU_DYNAMIC_DRAW = GL_DYNAMIC_DRAW, //!< Data is specified many times and used many times as the source of drawing and image specification commands.
    BU_DYNAMIC_READ = GL_DYNAMIC_READ, //!< Data is copied many times from an OpenGL buffer and is used many times by the application as data values.
    BU_DYNAMIC_COPY = GL_DYNAMIC_COPY  //!< Data is copied many times from an OpenGL buffer and is used many times as the source for drawing or image specification commands.
  } EBufferObjectUsage;

  typedef enum
  {
    BA_READ_ONLY  = GL_READ_ONLY,
    BA_WRITE_ONLY = GL_WRITE_ONLY, 
    BA_READ_WRITE = GL_READ_WRITE
  } EBufferObjectAccess;

  typedef enum
  {
    AlignLeft    = 1,
    AlignHCenter = 2,
    AlignRight   = 4,
    AlignTop     = 8,
    AlignVCenter = 16,
    AlignBottom  = 32
  } EAlign;

  typedef enum
  {
    TextAlignLeft,
    TextAlignRight,
    TextAlignCenter,
    TextAlignJustify,
  } ETextAlign;

  typedef enum
  {
    Text2D = 1,
    Text3D = 2
  } ETextMode;

  typedef enum
  {
    LeftToRightText,
    RightToLeftText,
    TopToBottomText
  } ETextLayout;

  typedef enum
  {
    NeverDepthSort,
    AlwaysDepthSort,
    AlphaDepthSort
  } EDepthSortMode;

  typedef enum
  {
    ID_None,
    ID_1D,
    ID_2D,
    ID_3D,
    ID_Cubemap,
    ID_Error
  } EImageDimension;

  typedef enum
  {
    ST_RenderStates = 1,
    ST_Enables      = 2,
    ST_TextureSamplers = 4,
    ST_Lights       = 8,
    ST_ClipPlanes   = 16
  } EStateType;

  typedef enum
  {
    RS_VertexAttrib,
    RS_VertexAttrib0 = RS_VertexAttrib,
    RS_VertexAttrib1,
    RS_VertexAttrib2,
    RS_VertexAttrib3,
    RS_VertexAttrib4,
    RS_VertexAttrib5,
    RS_VertexAttrib6,
    RS_VertexAttrib7,

    RS_AlphaFunc,
    RS_BlendColor,
    RS_BlendEquation,
    RS_BlendFunc,
    RS_Color,
    RS_ColorMask,
    RS_CullFace,
    RS_DepthFunc,
    RS_DepthMask,
    RS_DepthRange,
    RS_Fog,
    RS_FrontFace,
    RS_PolygonMode,
    RS_Hint,
    RS_LightModel,
    RS_LineStipple,
    RS_LineWidth,
    RS_LogicOp,
    RS_Material,
    RS_Normal,
    RS_PixelTransfer,
    RS_PointParameter,
    RS_PointSize,
    RS_PolygonOffset,
    RS_PolygonStipple,
    RS_SampleCoverage,
    RS_SecondaryColor,
    RS_ShadeModel,
    RS_StencilFunc,
    RS_StencilMask,
    RS_StencilOp,
    RS_GLSLProgram,

    RS_Light,
    RS_Light0 = RS_Light,
    RS_Light1,
    RS_Light2,
    RS_Light3,
    RS_Light4,
    RS_Light5,
    RS_Light6,
    RS_Light7,

    RS_ClipPlane,
    RS_ClipPlane0 = RS_ClipPlane,
    RS_ClipPlane1,
    RS_ClipPlane2,
    RS_ClipPlane3,
    RS_ClipPlane4,
    RS_ClipPlane5,

    RS_TextureSampler,
    RS_TextureSampler0  = RS_TextureSampler + 0,
    RS_TextureSampler1  = RS_TextureSampler + 1,
    RS_TextureSampler2  = RS_TextureSampler + 2,
    RS_TextureSampler3  = RS_TextureSampler + 3,
    RS_TextureSampler4  = RS_TextureSampler + 4,
    RS_TextureSampler5  = RS_TextureSampler + 5,
    RS_TextureSampler6  = RS_TextureSampler + 6,
    RS_TextureSampler7  = RS_TextureSampler + 7,
    RS_TextureSampler8  = RS_TextureSampler + 8,
    RS_TextureSampler9  = RS_TextureSampler + 9,
    RS_TextureSampler10 = RS_TextureSampler + 10,
    RS_TextureSampler11 = RS_TextureSampler + 11,
    RS_TextureSampler12 = RS_TextureSampler + 12,
    RS_TextureSampler13 = RS_TextureSampler + 13,
    RS_TextureSampler14 = RS_TextureSampler + 14,
    RS_TextureSampler15 = RS_TextureSampler + 15,
    /* ... */

    RS_TexGen   = RS_TextureSampler + VL_MAX_TEXTURE_UNITS,
    RS_TexGen0  = RS_TexGen + 0,
    RS_TexGen1  = RS_TexGen + 1,
    RS_TexGen2  = RS_TexGen + 2,
    RS_TexGen3  = RS_TexGen + 3,
    RS_TexGen4  = RS_TexGen + 4,
    RS_TexGen5  = RS_TexGen + 5,
    RS_TexGen6  = RS_TexGen + 6,
    RS_TexGen7  = RS_TexGen + 7,
    RS_TexGen8  = RS_TexGen + 8,
    RS_TexGen9  = RS_TexGen + 9,
    RS_TexGen10 = RS_TexGen + 10,
    RS_TexGen11 = RS_TexGen + 11,
    RS_TexGen12 = RS_TexGen + 12,
    RS_TexGen13 = RS_TexGen + 13,
    RS_TexGen14 = RS_TexGen + 14,
    RS_TexGen15 = RS_TexGen + 15,
    /* ... */

    RS_TexEnv   = RS_TexGen + VL_MAX_TEXTURE_UNITS,
    RS_TexEnv0  = RS_TexEnv + 0,
    RS_TexEnv1  = RS_TexEnv + 1,
    RS_TexEnv2  = RS_TexEnv + 2,
    RS_TexEnv3  = RS_TexEnv + 3,
    RS_TexEnv4  = RS_TexEnv + 4,
    RS_TexEnv5  = RS_TexEnv + 5,
    RS_TexEnv6  = RS_TexEnv + 6,
    RS_TexEnv7  = RS_TexEnv + 7,
    RS_TexEnv8  = RS_TexEnv + 8,
    RS_TexEnv9  = RS_TexEnv + 9,
    RS_TexEnv10 = RS_TexEnv + 10,
    RS_TexEnv11 = RS_TexEnv + 11,
    RS_TexEnv12 = RS_TexEnv + 12,
    RS_TexEnv13 = RS_TexEnv + 13,
    RS_TexEnv14 = RS_TexEnv + 14,
    RS_TexEnv15 = RS_TexEnv + 15,
    /* ... */

    RS_TextureMatrix   = RS_TexEnv + VL_MAX_TEXTURE_UNITS,
    RS_TextureMatrix0  = RS_TextureMatrix + 0,
    RS_TextureMatrix1  = RS_TextureMatrix + 1,
    RS_TextureMatrix2  = RS_TextureMatrix + 2,
    RS_TextureMatrix3  = RS_TextureMatrix + 3,
    RS_TextureMatrix4  = RS_TextureMatrix + 4,
    RS_TextureMatrix5  = RS_TextureMatrix + 5,
    RS_TextureMatrix6  = RS_TextureMatrix + 6,
    RS_TextureMatrix7  = RS_TextureMatrix + 7,
    RS_TextureMatrix8  = RS_TextureMatrix + 8,
    RS_TextureMatrix9  = RS_TextureMatrix + 9,
    RS_TextureMatrix10 = RS_TextureMatrix + 10,
    RS_TextureMatrix11 = RS_TextureMatrix + 11,
    RS_TextureMatrix12 = RS_TextureMatrix + 12,
    RS_TextureMatrix13 = RS_TextureMatrix + 13,
    RS_TextureMatrix14 = RS_TextureMatrix + 14,
    RS_TextureMatrix15 = RS_TextureMatrix + 15,
    /* ... */

    RS_RenderStateCount = RS_TextureMatrix15 + 1,

    RS_NONE,

  } ERenderState;

  typedef enum
  {
    GIT_POINTS              = GL_POINTS, 
    GIT_LINES               = GL_LINES,
    GIT_LINES_ADJACENCY     = GL_LINES_ADJACENCY_EXT, 
    GIT_TRIANGLES           = GL_TRIANGLES,
    GIT_TRIANGLES_ADJACENCY = GL_TRIANGLES_ADJACENCY_EXT
  } EGeometryInputType;

  typedef enum
  {
    GOT_POINTS = GL_POINTS, 
    GOT_LINE_STRIP = GL_LINE_STRIP,
    GOT_TRIANGLE_STRIP = GL_TRIANGLE_STRIP,
  } EGeometryOutputType;

  typedef enum
  {
    BB_COLOR_BUFFER_BIT   = GL_COLOR_BUFFER_BIT,
    BB_DEPTH_BUFFER_BIT   = GL_DEPTH_BUFFER_BIT, 
    BB_STENCIL_BUFFER_BIT = GL_STENCIL_BUFFER_BIT
  } EBufferBits;

  typedef enum
  {
    BT_AxisAlignedBillboard = 1,
    BT_SphericalBillboard = 2
  } EBillboardType;

  typedef enum
  {
    SM_SortBackToFront,
    SM_SortFrontToBack
  } ESortMode;

  typedef enum
  {
    GM_GetOrCreate,
    GM_DontCreate
  } EGetMode;

  typedef enum
  {
    SE_Unknown,
    SE_ASCII,
    SE_UTF8,
    SE_UTF16_BE,
    SE_UTF16_LE,
    SE_UTF32_BE,
    SE_UTF32_LE,
    SE_LATIN1,
  } EStringEncoding;

  typedef enum
  {
    OM_ReadOnly,
    OM_WriteOnly,
  } EOpenMode;

  typedef enum
  {
    Key_None = 0,

    // unicode keys

    Key_0,
    Key_1,
    Key_2,
    Key_3,
    Key_4,
    Key_5,
    Key_6,
    Key_7,
    Key_8,
    Key_9,

    Key_A,
    Key_B,
    Key_C,
    Key_D,
    Key_E,
    Key_F,
    Key_G,
    Key_H,
    Key_I,
    Key_J,
    Key_K,
    Key_L,
    Key_M,
    Key_N,
    Key_O,
    Key_P,
    Key_Q,
    Key_R,
    Key_S,
    Key_T,
    Key_U,
    Key_V,
    Key_W,
    Key_X,
    Key_Y,
    Key_Z,

    Key_Return,
    Key_BackSpace,
    Key_Tab,
    Key_Space,

    Key_Clear,
    Key_Escape,
    Key_Exclam,
    Key_QuoteDbl,
    Key_Hash,
    Key_Dollar,
    Key_Ampersand,
    Key_Quote,
    Key_LeftParen,
    Key_RightParen,
    Key_Asterisk,
    Key_Plus,
    Key_Comma,
    Key_Minus,
    Key_Period,
    Key_Slash,
    Key_Colon,
    Key_Semicolon,
    Key_Less,
    Key_Equal,
    Key_Greater,
    Key_Question,
    Key_At,
    Key_LeftBracket,
    Key_BackSlash,
    Key_RightBracket,
    Key_Caret,
    Key_Underscore,
    Key_QuoteLeft,

    // non unicode keys

    Key_Ctrl,
    Key_LeftCtrl,
    Key_RightCtrl,
    Key_Alt,
    Key_LeftAlt,
    Key_RightAlt,
    Key_Shift,
    Key_LeftShift,
    Key_RightShift,
    Key_Insert,
    Key_Delete,
    Key_Home,
    Key_End,
    Key_Print,
    Key_Pause,
    Key_PageUp,
    Key_PageDown,
    Key_Left,
    Key_Right,
    Key_Up,
    Key_Down,
    Key_F1,
    Key_F2,
    Key_F3,
    Key_F4,
    Key_F5,
    Key_F6,
    Key_F7,
    Key_F8,
    Key_F9,
    Key_F10,
    Key_F11,
    Key_F12,

    Key_Unknown,

    Key_NumberOfKeys

  } EKey;

  typedef enum
  {
    NoButton = 0,

    LeftButton   = 1,
    RightButton  = 2,
    MiddleButton = 4,

    UnknownButton,
  } EMouseButton;

  typedef enum
  {
    IN_Local                      = 0x00, //!< Does not propagates to children (thus cannot override children settings); can be overridden.
    IN_Propagate                  = 0x01, //!< Propagates to children; does not override children settings; can be overridden.
    IN_Sticky                     = 0x04, //!< Does not propagates to children (thus cannot override children settings); cannot be overridden.
    IN_Propagate_Overrides_Sticky = 0x01 | 0x02 | 0x04, //!< Propagates to children; overrides children settings; cannot be overridden.
    IN_Propagate_Overrides        = 0x01 | 0x02, //!< Propagates to children; overrides children settings; can be overridden.
    IN_Propagate_Sticky           = 0x01 | 0x04, //!< Propagates to children; does not override children settings; cannot be overridden.
  } EInheritance;

  typedef enum
  {
    ST_VERTEX_SHADER          = GL_VERTEX_SHADER, //!< A shader that is intended to run on the programmable vertex processor.
    ST_TESS_CONTROL_SHADER    = GL_TESS_CONTROL_SHADER, //!< A shader that is intended to run on the programmable tessellation processor in the control stage.
    ST_TESS_EVALUATION_SHADER = GL_TESS_EVALUATION_SHADER, //!< A shader that is intended to run on the programmable tessellation processor in the evaluation stage.
    ST_GEOMETRY_SHADER        = GL_GEOMETRY_SHADER, //!< A shader that is intended to run on the programmable geometry processor.
    ST_FRAGMENT_SHADER        = GL_FRAGMENT_SHADER //!< A shader that is intended to run on the programmable fragment processor.
  } EShaderType;

  typedef enum
  {
    TW_TESS_WINDING_ODD         = GLU_TESS_WINDING_ODD,
    TW_TESS_WINDING_NONZERO     = GLU_TESS_WINDING_NONZERO,
    TW_TESS_WINDING_POSITIVE    = GLU_TESS_WINDING_POSITIVE,
    TW_TESS_WINDING_NEGATIVE    = GLU_TESS_WINDING_NEGATIVE,
    TW_TESS_WINDING_ABS_GEQ_TWO = GLU_TESS_WINDING_ABS_GEQ_TWO
  } ETessellationWinding;

  typedef enum { 
    VEL_VERBOSITY_SILENT, //!<< No log information is generated.
    VEL_VERBOSITY_ERROR,  //!<< Outputs critical and runtime error messages.
    VEL_VERBOSITY_NORMAL, //!<< Outputs normal information messages, plus all error messages.
    VEL_VERBOSITY_DEBUG   //!<< Outputs extra information messages useful for debugging, plus all normal and error messages.
  } EVerbosityLevel;

  typedef enum 
  { 
    LL_LogNotify,
    LL_LogPrint,
    LL_LogBug,
    LL_LogError,
    LL_LogWarning,
    LL_LogDebug,
  } ELogLevel;

  typedef enum
  {
    PPCO_LOWER_LEFT = GL_LOWER_LEFT,
    PPCO_UPPER_LEFT = GL_UPPER_LEFT
  } EPointSpriteCoordOrigin;

  //! Specifies how the data of a VertexAttribInfo is sent to the OpenGL driver,
  //! see also http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttribPointer.xml
  typedef enum
  {
    VAI_NORMAL,  //!< Data will be sent using glVertexAttribPointer(), that is,
                 //!< data will be converted to floating point precision and eventually normalized.

    VAI_INTEGER, //!< Data will be sent using glVertexAttribIPointer(), that is, 
                 //!< values are always left as integer values, data format must be an \a integer type.

    VAI_DOUBLE,  //!< Data will be sent using glVertexAttribLPointer(), that is, it will be associated with a 
                 //!< shader attribute variable declared with 64-bit double precision components, data format must be \a double.
  } EVertexAttribInterpretation;

  //! Default vertex attribute bindings
  typedef enum
  {
    VA_Position  = 0,
    VA_Normal    = 1,
    VA_Color     = 2,
    VA_TexCoord0 = 3,
  } EVertexAttribBinding;

  //! Uniform types, see also vl::UniformInfo, vl::GLSLProgram, vl::Uniform, http://www.opengl.org/sdk/docs/man4/xhtml/glGetActiveUniform.xml
  typedef enum
  {
    UT_NONE = 0x0,

    UT_FLOAT = GL_FLOAT, //!< float
    UT_FLOAT_VEC2 = GL_FLOAT_VEC2, //!< vec2
    UT_FLOAT_VEC3 = GL_FLOAT_VEC3, //!< vec3
    UT_FLOAT_VEC4 = GL_FLOAT_VEC4, //!< vec4
    UT_DOUBLE = GL_DOUBLE, //!< double
    UT_DOUBLE_VEC2 = GL_DOUBLE_VEC2, //!< dvec2
    UT_DOUBLE_VEC3 = GL_DOUBLE_VEC3, //!< dvec3
    UT_DOUBLE_VEC4 = GL_DOUBLE_VEC4, //!< dvec4
    UT_INT = GL_INT, //!< int
    UT_INT_VEC2 = GL_INT_VEC2, //!< ivec2
    UT_INT_VEC3 = GL_INT_VEC3, //!< ivec3
    UT_INT_VEC4 = GL_INT_VEC4, //!< ivec4
    UT_UNSIGNED_INT = GL_UNSIGNED_INT, //!< unsigned int
    UT_UNSIGNED_INT_VEC2 = GL_UNSIGNED_INT_VEC2, //!< uvec2
    UT_UNSIGNED_INT_VEC3 = GL_UNSIGNED_INT_VEC3, //!< uvec3
    UT_UNSIGNED_INT_VEC4 = GL_UNSIGNED_INT_VEC4, //!< uvec4
    UT_BOOL = GL_BOOL, //!< bool
    UT_BOOL_VEC2 = GL_BOOL_VEC2, //!< bvec2
    UT_BOOL_VEC3 = GL_BOOL_VEC3, //!< bvec3
    UT_BOOL_VEC4 = GL_BOOL_VEC4, //!< bvec4
    UT_FLOAT_MAT2 = GL_FLOAT_MAT2, //!< mat2
    UT_FLOAT_MAT3 = GL_FLOAT_MAT3, //!< mat3
    UT_FLOAT_MAT4 = GL_FLOAT_MAT4, //!< mat4
    UT_FLOAT_MAT2x3 = GL_FLOAT_MAT2x3, //!< mat2x3
    UT_FLOAT_MAT2x4 = GL_FLOAT_MAT2x4, //!< mat2x4
    UT_FLOAT_MAT3x2 = GL_FLOAT_MAT3x2, //!< mat3x2
    UT_FLOAT_MAT3x4 = GL_FLOAT_MAT3x4 , //!< mat3x4
    UT_FLOAT_MAT4x2 = GL_FLOAT_MAT4x2 , //!< mat4x2
    UT_FLOAT_MAT4x3 = GL_FLOAT_MAT4x3 , //!< mat4x3
    UT_DOUBLE_MAT2 = GL_DOUBLE_MAT2 , //!< dmat2
    UT_DOUBLE_MAT3 = GL_DOUBLE_MAT3 , //!< dmat3
    UT_DOUBLE_MAT4 = GL_DOUBLE_MAT4 , //!< dmat4
    UT_DOUBLE_MAT2x3 = GL_DOUBLE_MAT2x3 , //!< dmat2x3
    UT_DOUBLE_MAT2x4 = GL_DOUBLE_MAT2x4 , //!< dmat2x4
    UT_DOUBLE_MAT3x2 = GL_DOUBLE_MAT3x2 , //!< dmat3x2
    UT_DOUBLE_MAT3x4 = GL_DOUBLE_MAT3x4 , //!< dmat3x4
    UT_DOUBLE_MAT4x2 = GL_DOUBLE_MAT4x2 , //!< dmat4x2
    UT_DOUBLE_MAT4x3 = GL_DOUBLE_MAT4x3 , //!< dmat4x3

    UT_SAMPLER_1D = GL_SAMPLER_1D , //!< sampler1D
    UT_SAMPLER_2D = GL_SAMPLER_2D , //!< sampler2D
    UT_SAMPLER_3D = GL_SAMPLER_3D , //!< sampler3D
    UT_SAMPLER_CUBE = GL_SAMPLER_CUBE , //!< samplerCube
    UT_SAMPLER_1D_SHADOW = GL_SAMPLER_1D_SHADOW , //!< sampler1DShadow
    UT_SAMPLER_2D_SHADOW = GL_SAMPLER_2D_SHADOW , //!< sampler2DShadow
    UT_SAMPLER_1D_ARRAY = GL_SAMPLER_1D_ARRAY , //!< sampler1DArray
    UT_SAMPLER_2D_ARRAY = GL_SAMPLER_2D_ARRAY , //!< sampler2DArray
    UT_SAMPLER_1D_ARRAY_SHADOW = GL_SAMPLER_1D_ARRAY_SHADOW , //!< sampler1DArrayShadow
    UT_SAMPLER_2D_ARRAY_SHADOW = GL_SAMPLER_2D_ARRAY_SHADOW , //!< sampler2DArrayShadow
    UT_SAMPLER_2D_MULTISAMPLE = GL_SAMPLER_2D_MULTISAMPLE , //!< sampler2DMS
    UT_SAMPLER_2D_MULTISAMPLE_ARRAY = GL_SAMPLER_2D_MULTISAMPLE_ARRAY , //!< sampler2DMSArray
    UT_SAMPLER_CUBE_SHADOW = GL_SAMPLER_CUBE_SHADOW , //!< samplerCubeShadow
    UT_SAMPLER_BUFFER = GL_SAMPLER_BUFFER , //!< samplerBuffer
    UT_SAMPLER_2D_RECT = GL_SAMPLER_2D_RECT , //!< sampler2DRect
    UT_SAMPLER_2D_RECT_SHADOW = GL_SAMPLER_2D_RECT_SHADOW , //!< sampler2DRectShadow
    UT_INT_SAMPLER_1D = GL_INT_SAMPLER_1D , //!< isampler1D
    UT_INT_SAMPLER_2D = GL_INT_SAMPLER_2D , //!< isampler2D
    UT_INT_SAMPLER_3D = GL_INT_SAMPLER_3D , //!< isampler3D
    UT_INT_SAMPLER_CUBE = GL_INT_SAMPLER_CUBE , //!< isamplerCube
    UT_INT_SAMPLER_1D_ARRAY = GL_INT_SAMPLER_1D_ARRAY , //!< isampler1DArray
    UT_INT_SAMPLER_2D_ARRAY = GL_INT_SAMPLER_2D_ARRAY , //!< isampler2DArray
    UT_INT_SAMPLER_2D_MULTISAMPLE = GL_INT_SAMPLER_2D_MULTISAMPLE , //!< isampler2DMS
    UT_INT_SAMPLER_2D_MULTISAMPLE_ARRAY = GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY , //!< isampler2DMSArray
    UT_INT_SAMPLER_BUFFER = GL_INT_SAMPLER_BUFFER , //!< isamplerBuffer
    UT_INT_SAMPLER_2D_RECT = GL_INT_SAMPLER_2D_RECT , //!< isampler2DRect
    UT_UNSIGNED_INT_SAMPLER_1D = GL_UNSIGNED_INT_SAMPLER_1D , //!< usampler1D
    UT_UNSIGNED_INT_SAMPLER_2D = GL_UNSIGNED_INT_SAMPLER_2D , //!< usampler2D
    UT_UNSIGNED_INT_SAMPLER_3D = GL_UNSIGNED_INT_SAMPLER_3D , //!< usampler3D
    UT_UNSIGNED_INT_SAMPLER_CUBE = GL_UNSIGNED_INT_SAMPLER_CUBE , //!< usamplerCube
    UT_UNSIGNED_INT_SAMPLER_1D_ARRAY = GL_UNSIGNED_INT_SAMPLER_1D_ARRAY , //!< usampler2DArray
    UT_UNSIGNED_INT_SAMPLER_2D_ARRAY = GL_UNSIGNED_INT_SAMPLER_2D_ARRAY , //!< usampler2DArray
    UT_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE = GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE , //!< usampler2DMS
    UT_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY = GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY , //!< usampler2DMSArray
    UT_UNSIGNED_INT_SAMPLER_BUFFER = GL_UNSIGNED_INT_SAMPLER_BUFFER , //!< usamplerBuffer
    UT_UNSIGNED_INT_SAMPLER_2D_RECT = GL_UNSIGNED_INT_SAMPLER_2D_RECT , //!< usampler2DRect

    UT_UniformTypeCount

  } EUniformType;

  //! GLSLProgram attribute types, see also GLSLProgram::activeAttribs() and http://www.opengl.org/sdk/docs/man4/xhtml/glGetActiveAttrib.xml
  typedef enum
  {
    AT_FLOAT = GL_FLOAT, //!< float
    AT_FLOAT_VEC2 = GL_FLOAT_VEC2, //!< vec2
    AT_FLOAT_VEC3 = GL_FLOAT_VEC3, //!< vec3
    AT_FLOAT_VEC4 = GL_FLOAT_VEC4, //!< vec4
    AT_FLOAT_MAT2 = GL_FLOAT_MAT2, //!< mat2
    AT_FLOAT_MAT3 = GL_FLOAT_MAT3, //!< mat3
    AT_FLOAT_MAT4 = GL_FLOAT_MAT4, //!< mat4
    AT_FLOAT_MAT2x3 = GL_FLOAT_MAT2x3, //!< mat2x3
    AT_FLOAT_MAT2x4 = GL_FLOAT_MAT2x4, //!< mat2x4
    AT_FLOAT_MAT3x2 = GL_FLOAT_MAT3x2, //!< mat3x2
    AT_FLOAT_MAT3x4 = GL_FLOAT_MAT3x4, //!< mat3x4
    AT_FLOAT_MAT4x2 = GL_FLOAT_MAT4x2, //!< mat4x2
    AT_FLOAT_MAT4x3 = GL_FLOAT_MAT4x3, //!< mat4x3
    AT_INT = GL_INT, //!< int
    AT_INT_VEC2 = GL_INT_VEC2, //!< ivec2
    AT_INT_VEC3 = GL_INT_VEC3, //!< ivec3
    AT_INT_VEC4 = GL_INT_VEC4, //!< ivec4
    AT_UNSIGNED_INT = GL_UNSIGNED_INT, //!< unsigned int
    AT_UNSIGNED_INT_VEC2 = GL_UNSIGNED_INT_VEC2, //!< vec2
    AT_UNSIGNED_INT_VEC3 = GL_UNSIGNED_INT_VEC3, //!< vec3
    AT_UNSIGNED_INT_VEC4 = GL_UNSIGNED_INT_VEC4, //!< vec4
    AT_DOUBLE = GL_DOUBLE, //!< double
    AT_DOUBLE_VEC2 = GL_DOUBLE_VEC2, //!< dvec2
    AT_DOUBLE_VEC3 = GL_DOUBLE_VEC3, //!< dvec3
    AT_DOUBLE_VEC4 = GL_DOUBLE_VEC4, //!< dvec4
    AT_DOUBLE_MAT2 = GL_DOUBLE_MAT2, //!< dmat2
    AT_DOUBLE_MAT3 = GL_DOUBLE_MAT3, //!< dmat3
    AT_DOUBLE_MAT4 = GL_DOUBLE_MAT4, //!< dmat4
    AT_DOUBLE_MAT2x3 = GL_DOUBLE_MAT2x3, //!< dmat2x3
    AT_DOUBLE_MAT2x4 = GL_DOUBLE_MAT2x4, //!< dmat2x4
    AT_DOUBLE_MAT3x2 = GL_DOUBLE_MAT3x2, //!< dmat3x2
    AT_DOUBLE_MAT3x4 = GL_DOUBLE_MAT3x4, //!< dmat3x4
    AT_DOUBLE_MAT4x2 = GL_DOUBLE_MAT4x2, //!< dmat4x2
    AT_DOUBLE_MAT4x3 = GL_DOUBLE_MAT4x3, //!< dmat4x3

  } EAttributeType;

  typedef enum
  {
    PMT_UserProjection,              //!< Unknown or other projection type.
    PMT_OrthographicProjection,      //!< Projection matrix generated by mat4::getOrtho() or similar. Any orthographic projection.
    PMT_PerspectiveProjection,       //!< Projection matrix generated by mat4::getPerspective() or similar. Symmetrical (on-axis) perspective projection.
    PMT_PerspectiveProjectionFrustum //!< Projection matrix generated by mat4::getFrustum() or similar. Possibly asymmetrical (off-axis) perspetive projection, like the ones used for tile-rendering.
  } EProjectionMatrixType;

  typedef enum
  {
    BUF_ForceUpdate   = 0x1,
    BUF_DiscardRamBuffer = 0x2,
  } EBufferObjectUpdateFlags;

  typedef enum
  {
    //! Keeps the local buffer on RAM and updates the BufferObject only if it is marked as dirty. The BufferObject is marked as clean after the update.
    BUM_KeepRamBuffer = 0x0,

    //! Keeps the local buffer on RAM and updates the BufferObject always, even if it is not marked as dirty. The BufferObject is marked as clean after the update.
    BUM_KeepRamBufferAndForceUpdate = BUF_ForceUpdate, 

    //! Discards the local buffer on RAM and updates the BufferObject only if it is marked as dirty. The BufferObject is marked as clean after the update.
    BUM_DiscardRamBuffer = BUF_DiscardRamBuffer,

    //! Discards the local buffer on RAM and updates the BufferObject always, even if it is not marked as dirty. The BufferObject is marked as clean after the update.
    BUM_DiscardRamBufferAndForceUpdate = BUF_DiscardRamBuffer | BUF_ForceUpdate
  } EBufferObjectUpdateMode;

  typedef enum
  {
    SCM_OwnShaders, //!< A local copy of the Shaders will be created but the contained render states will be shared.
    SCM_ShareShaders //!< The Shader pointer will be copied as is.
  } EShaderCopyMode;

  typedef enum
  {
    RCS_RenderingStarted,
    RCS_RenderingFinished
  } EResetContextStates;
}


#endif
