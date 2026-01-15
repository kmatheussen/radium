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

#include <vlGraphics/Texture.hpp>
#include <vlCore/checks.hpp>
#include <vlCore/Image.hpp>
#include <vlCore/math_utils.hpp>
#include <vlCore/Say.hpp>
#include <vlCore/Log.hpp>

using namespace vl;

namespace
{
  // if you think your application has a bug that depends on this function you are wrong
  int getDefaultFormat(ETextureFormat internal_format)
  {
    // OpenGL ES requires the internal format to be equal to the source image format when creating textures
#if defined(VL_OPENGL_ES1) || defined(VL_OPENGL_ES2)
    return internal_format;
#else
    switch(internal_format)
    {
      case TF_ALPHA:
      case TF_ALPHA4:
      case TF_ALPHA8:
      case TF_ALPHA12:
      case TF_ALPHA16:
        return GL_ALPHA;

      case TF_LUMINANCE:
      case TF_LUMINANCE4:
      case TF_LUMINANCE8:
      case TF_LUMINANCE12:
      case TF_LUMINANCE16:
      case TF_SLUMINANCE:
      case TF_SLUMINANCE8:
        return GL_LUMINANCE;

      case TF_LUMINANCE_ALPHA:
      case TF_LUMINANCE4_ALPHA4:
      case TF_LUMINANCE6_ALPHA2:
      case TF_LUMINANCE8_ALPHA8:
      case TF_LUMINANCE12_ALPHA4:
      case TF_LUMINANCE12_ALPHA12:
      case TF_LUMINANCE16_ALPHA16:
      case TF_SLUMINANCE_ALPHA:
      case TF_SLUMINANCE8_ALPHA8:
        return GL_LUMINANCE_ALPHA;

      case TF_INTENSITY:
      case TF_INTENSITY4:
      case TF_INTENSITY8:
      case TF_INTENSITY12:
      case TF_INTENSITY16:
        return GL_INTENSITY;

      case TF_RED:
      case TF_R8:
      case TF_R8_SNORM:
      case TF_R16:
      case TF_R16_SNORM:
      case TF_R16F:
      case TF_R32F:
        return GL_RED;

      case TF_RG:
      case TF_RG8:
      case TF_RG8_SNORM:
      case TF_RG16:
      case TF_RG16_SNORM:
      case TF_RG16F:
      case TF_RG32F:
        return GL_RG;

      case TF_RGB:
      case TF_RGB4:
      case TF_RGB5:
      case TF_RGB8:
      case TF_RGB8_SNORM:
      case TF_RGB10:
      case TF_RGB12:
      case TF_RGB16:
      // case TF_RGB16_SNORM:
      case TF_RGB16F:
      case TF_RGB32F:
      case TF_R3_G3_B2:
      case TF_R11F_G11F_B10F:
      // case TF_RGB9_A5:
      case TF_SRGB8:
        return GL_RGB;

      case TF_RGBA:
      case TF_RGBA2:
      case TF_RGBA4:
      case TF_RGBA8:
      case TF_RGBA8_SNORM:
      case TF_RGBA12:
      case TF_RGBA16:
      case TF_RGBA16_SNORM:
      case TF_RGBA16F:
      case TF_RGBA32F:
      case TF_RGB5_A1:
      case TF_RGB10_A2:
      case TF_SRGB8_ALPHA8:
        return GL_RGBA;

      case TF_R8I:
      case TF_R8UI:
      case TF_R16I:
      case TF_R16UI:
      case TF_R32I:
      case TF_R32UI:
        return GL_RED_INTEGER;

      case TF_RG8I:
      case TF_RG8UI:
      case TF_RG16I:
      case TF_RG16UI:
      case TF_RG32I:
      case TF_RG32UI:
        return GL_RG_INTEGER;

      case TF_RGB8I:
      // case TF_RGB8I_EXT: // Has the same value has above
      case TF_RGB8UI:
      // case TF_RGB8UI_EXT: // Has the same value has above
      case TF_RGB16I:
      // case TF_RGB16I_EXT: // Has the same value has above
      case TF_RGB16UI:
      // case TF_RGB16UI_EXT: // Has the same value has above
      case TF_RGB32I:
      // case TF_RGB32I_EXT: // Has the same value has above
      case TF_RGB32UI:
      // case TF_RGB32UI_EXT: // Has the same value has above
        return GL_RGB_INTEGER;

      case TF_RGBA8I:
      // case TF_RGBAI_EXT: // Has the same value has above
      case TF_RGBA8UI:
      // case TF_RGBA8UI_EXT: // Has the same value has above
      case TF_RGBA16I:
      // case TF_RGBA16I_EXT: // Has the same value has above
      case TF_RGBA16UI:
      // case TF_RGBA16UI_EXT: // Has the same value has above
      case TF_RGBA32I:
      // case TF_RGBA32I_EXT: // Has the same value has above
      case TF_RGBA32UI:
      // case TF_RGBA32UI_EXT: // Has the same value has above
      case TF_RGB10_A2UI:
        return GL_RGBA_INTEGER;

      case TF_DEPTH_STENCIL:
      case TF_DEPTH24_STENCIL8:
      case TF_DEPTH32F_STENCIL8:
        return GL_DEPTH_STENCIL;

      case TF_DEPTH_COMPONENT:
      case TF_DEPTH_COMPONENT16:
      case TF_DEPTH_COMPONENT24:
      case TF_DEPTH_COMPONENT32:
      case TF_DEPTH_COMPONENT32F:
        return GL_DEPTH_COMPONENT;

      // GL_EXT_texture_integer
      case TF_ALPHA8I_EXT:
      case TF_ALPHA8UI_EXT:
      case TF_ALPHA16I_EXT:
      case TF_ALPHA16UI_EXT:
      case TF_ALPHA32I_EXT:
      case TF_ALPHA32UI_EXT:
        return GL_ALPHA_INTEGER;

      case TF_INTENSITY8I_EXT:
      case TF_INTENSITY8UI_EXT:
      case TF_INTENSITY16I_EXT:
      case TF_INTENSITY16UI_EXT:
      case TF_INTENSITY32I_EXT:
      case TF_INTENSITY32UI_EXT:
        return GL_RED_INTEGER; // Nothing associated with intensity in GL_EXT_texture_integer

      case TF_LUMINANCE8I_EXT:
      case TF_LUMINANCE8UI_EXT:
      case TF_LUMINANCE16UI_EXT:
      case TF_LUMINANCE16I_EXT:
      case TF_LUMINANCE32I_EXT:
      case TF_LUMINANCE32UI_EXT:
        return GL_LUMINANCE_INTEGER_EXT;

      case TF_LUMINANCE_ALPHA8I_EXT:
      case TF_LUMINANCE_ALPHA8UI_EXT:
      case TF_LUMINANCE_ALPHA16I_EXT:
      case TF_LUMINANCE_ALPHA16UI_EXT:
      case TF_LUMINANCE_ALPHA32I_EXT:
      case TF_LUMINANCE_ALPHA32UI_EXT:
        return GL_LUMINANCE_ALPHA_INTEGER_EXT;

      default:
        return GL_RED;
    }
#endif
  }

  // if you think your application has a bug that depends on this function you are wrong
  int getDefaultType(ETextureFormat internal_format)
  {
    switch( internal_format )
    {
      case TF_ALPHA4:
      case TF_ALPHA8:
      case TF_ALPHA8UI_EXT:
      case TF_INTENSITY4:
      case TF_INTENSITY8:
      case TF_INTENSITY8UI_EXT:
      case TF_LUMINANCE4:
      case TF_LUMINANCE8:
      case TF_LUMINANCE8UI_EXT:
      case TF_LUMINANCE8_ALPHA8:
      case TF_LUMINANCE_ALPHA8UI_EXT:
      case TF_R8:
      case TF_R8UI:
      case TF_RG8:
      case TF_RG8UI:
      case TF_RGB8:
      case TF_RGB8UI:
      case TF_RGBA8:
      case TF_RGBA8UI:
        return GL_UNSIGNED_BYTE;

      case TF_ALPHA8I_EXT:
      case TF_INTENSITY8I_EXT:
      case TF_LUMINANCE8I_EXT:
      case TF_LUMINANCE_ALPHA8I_EXT:
      case TF_R8I:
      case TF_RG8I:
      case TF_RGB8I:
      case TF_RGBA8I:
        return GL_BYTE;

      case TF_ALPHA12:
      case TF_ALPHA16:
      case TF_ALPHA16UI_EXT:
      case TF_INTENSITY12:
      case TF_INTENSITY16:
      case TF_INTENSITY16UI_EXT:
      case TF_LUMINANCE12:
      case TF_LUMINANCE16:
      case TF_LUMINANCE16UI_EXT:
      case TF_LUMINANCE16_ALPHA16:
      case TF_LUMINANCE_ALPHA16UI_EXT:
      case TF_R16:
      case TF_R16UI:
      case TF_RG16:
      case TF_RG16UI:
      case TF_RGB10:
      case TF_RGB12:
      case TF_RGB16:
      case TF_RGB16UI:
      case TF_RGBA12:
      case TF_RGBA16:
      case TF_RGBA16UI:
      case TF_DEPTH_COMPONENT16:
        return GL_UNSIGNED_SHORT; 

      case TF_ALPHA16I_EXT:
      case TF_INTENSITY16I_EXT:
      case TF_LUMINANCE16I_EXT:
      case TF_LUMINANCE_ALPHA16I_EXT:
      case TF_R16I:
      case TF_RG16I:
      case TF_RGB16I:
      case TF_RGBA16I:
        return GL_SHORT;

      case TF_ALPHA32UI_EXT:
      case TF_INTENSITY32UI_EXT:
      case TF_LUMINANCE32UI_EXT:
      case TF_LUMINANCE_ALPHA32UI_EXT:
      case TF_R32UI:
      case TF_RG32UI:
      case TF_RGB32UI:
      case TF_RGBA32UI:
      case TF_DEPTH_COMPONENT24:
      case TF_DEPTH_COMPONENT32:
        return GL_UNSIGNED_INT;

      case TF_ALPHA32I_EXT:
      case TF_INTENSITY32I_EXT:
      case TF_LUMINANCE32I_EXT:
      case TF_LUMINANCE_ALPHA32I_EXT:
      case TF_R32I:
      case TF_RG32I:
      case TF_RGB32I:
      case TF_RGBA32I:
        return GL_INT;

      case TF_DEPTH24_STENCIL8:
        return GL_UNSIGNED_INT_24_8;

      case TF_R16F:
      case TF_R32F:
      case TF_RG16F:
      case TF_RG32F:
      case TF_RGB16F:
      case TF_RGB32F:
      case TF_RGBA16F:
      case TF_RGBA32F:
      case TF_R11F_G11F_B10F:
      case TF_ALPHA16F:
      case TF_ALPHA32F:
      case TF_INTENSITY16F:
      case TF_INTENSITY32F:
      case TF_LUMINANCE16F:
      case TF_LUMINANCE32F:
      case TF_LUMINANCE_ALPHA16F:
      case TF_LUMINANCE_ALPHA32F:
      case TF_DEPTH_COMPONENT32F:
        return GL_FLOAT;

      case TF_DEPTH32F_STENCIL8:
        return GL_FLOAT_32_UNSIGNED_INT_24_8_REV;

      default:
        return GL_UNSIGNED_BYTE;
    }
  }
}
//-----------------------------------------------------------------------------
// Texture
//-----------------------------------------------------------------------------
void Texture::destroyTexture()
{
  if (mHandle)
    glDeleteTextures(1, &mHandle);
  reset();
  // getTexParameter()->mDirty = true;
}
//-----------------------------------------------------------------------------
Texture::~Texture()
{
  destroyTexture();
}
//-----------------------------------------------------------------------------
void Texture::reset()
{
  setDimension(TD_TEXTURE_UNKNOWN);
  setInternalFormat(TF_UNKNOWN);
  setBorder(0);
  setWidth(0);
  setHeight(0);
  setDepth(0);
  mHandle = 0;
  mSetupParams = NULL;
  mBufferObject = NULL;
  mSamples = 0;
  mFixedSamplesLocation = true;
}
//-----------------------------------------------------------------------------
Texture::Texture(int width, ETextureFormat format, bool border)
{
  VL_DEBUG_SET_OBJECT_NAME()
  mTexParameter = new TexParameter;
  reset();
  if (!createTexture(vl::TD_TEXTURE_1D, format, width, 0, 0, border, NULL, 0, 0))
  {
    Log::error("1D texture creation failed!\n");
  }
}
//-----------------------------------------------------------------------------
Texture::Texture(int width, int height, ETextureFormat format, bool border)
{
  VL_DEBUG_SET_OBJECT_NAME()
  mTexParameter = new TexParameter;
  reset();
  if (!createTexture(vl::TD_TEXTURE_2D, format, width, height, 0, border, NULL, 0, 0))
  {
    Log::error("2D texture constructor failed!\n");
  }
}
//-----------------------------------------------------------------------------
Texture::Texture(int width, int height, int depth, ETextureFormat format, bool border)
{
  VL_DEBUG_SET_OBJECT_NAME()
  mTexParameter = new TexParameter;
  reset();
  if (!createTexture(vl::TD_TEXTURE_3D, format, width, height, depth, border, NULL, 0, 0))
  {
    Log::error("3D texture constructor failed!\n");
  }
}
//-----------------------------------------------------------------------------
Texture::Texture(const Image* image, ETextureFormat format, bool mipmaps , bool border)
{
  VL_DEBUG_SET_OBJECT_NAME()
  mTexParameter = new TexParameter;
  reset();

  if (image && image->isValid())
  {
    switch(image->dimension())
    {
#if defined(VL_OPENGL)
    case ID_1D:      prepareTexture1D(image, format, mipmaps, border); break;
#else
    case ID_1D:      prepareTexture2D(image, format, mipmaps, border); break;
#endif
    case ID_2D:      prepareTexture2D(image, format, mipmaps, border); break;
    case ID_3D:      prepareTexture3D(image, format, mipmaps, border); break;
    case ID_Cubemap: prepareTextureCubemap(image, format, mipmaps, border); break;
    default:
      break;
    }
    if( !createTexture() )
      Log::error("Texture constructor failed!\n");
  }
  else {
    Log::bug("Texture constructor called with an invalid Image!\n");
    //abort();
  }
}
//-----------------------------------------------------------------------------
Texture::Texture(const String& image_path, ETextureFormat format, bool mipmaps , bool border)
{
  VL_DEBUG_SET_OBJECT_NAME()
  mTexParameter = new TexParameter;
  reset();

  ref<Image> image = vl::loadImage(image_path);

  if (image && image->isValid())
  {
    switch(image->dimension())
    {
#if defined(VL_OPENGL)
    case ID_1D:      prepareTexture1D(image.get(), format, mipmaps, border); break;
#else
    case ID_1D:      prepareTexture2D(image.get(), format, mipmaps, border); break;
#endif
    case ID_2D:      prepareTexture2D(image.get(), format, mipmaps, border); break;
    case ID_3D:      prepareTexture3D(image.get(), format, mipmaps, border); break;
    case ID_Cubemap: prepareTextureCubemap(image.get(), format, mipmaps, border); break;
    default:
      break;
    }
    if( !createTexture() )
      Log::error("Texture constructor failed!\n");
  }
  else
    Log::bug("Texture constructor called with an invalid Image!\n");
}
//-----------------------------------------------------------------------------
Texture::Texture()
{
  VL_DEBUG_SET_OBJECT_NAME()
  mTexParameter = new TexParameter;
  reset();
}
//-----------------------------------------------------------------------------
bool Texture::isValid() const
{
  bool a = mWidth != 0 && mHeight == 0 && mDepth == 0;
  bool b = mWidth != 0 && mHeight != 0 && mDepth == 0;
  bool c = mWidth != 0 && mHeight != 0 && mDepth != 0;
  return handle() != 0 && (a|b|c);
}
//-----------------------------------------------------------------------------
bool Texture::supports(ETextureDimension tex_dimension, ETextureFormat tex_format, int mip_level, EImageDimension img_dimension, int w, int h, int d, bool border, int samples, bool fixedsamplelocations, bool verbose)
{
  VL_CHECK_OGL();

  // clear errors

  glGetError();

  // texture buffer

  if ( tex_dimension == TD_TEXTURE_2D_MULTISAMPLE || tex_dimension == TD_TEXTURE_2D_MULTISAMPLE_ARRAY )
  {
    if (!Has_Texture_Multisample)
    {
      if (verbose) Log::error("Texture::supports(): multisample textures not supported by the current hardware.\n");
      return false;
    }

    if (border)
    {
      if (verbose) Log::error("Texture::supports(): multisample textures cannot have borders.\n");
      return false;
    }

    if (mip_level)
    {
      if (verbose) Log::error("Texture::supports(): multisample textures cannot have mip levels other than 0.\n");
      return false;
    }

    // these should be non zero
    VL_CHECK( w && h );
  }

  if ( tex_dimension == TD_TEXTURE_BUFFER )
  {
    if (!Has_Texture_Buffer)
    {
      if (verbose) Log::error("Texture::supports(): texture buffer not supported by the current hardware.\n");
      return false;
    }

    if (border)
    {
      if (verbose) Log::error("Texture::supports(): a texture buffer cannot have borders.\n");
      return false;
    }

    if (mip_level)
    {
      if (verbose) Log::error("Texture::supports(): a texture buffer cannot have mip levels other than 0.\n");
      return false;
    }

    // these should be zero
    VL_CHECK( !(w||h||d) );
  }

  // cubemaps

  if ( tex_dimension == TD_TEXTURE_CUBE_MAP )
  {
    if (!Has_Cubemap_Textures)
    {
      if (verbose) Log::error("Texture::supports(): texture cubemap not supported by the current hardware.\n");
      return false;
    }

    if ( w != h )
    {
      if (verbose) Log::error("Texture::supports(): cubemaps must have square faces.\n");
      return false;
    }
  }

  // texture arrays

  if ( tex_dimension == TD_TEXTURE_1D_ARRAY || tex_dimension == TD_TEXTURE_2D_ARRAY )
  {
    if (border)
    {
      if (verbose) Log::error("Texture::supports(): you cannot create a texture array with borders.\n");
      return false;
    }

    if(!Has_Texture_Array)
    {
      if (verbose) Log::error("Texture::supports(): texture array not supported by the current hardware.\n");
      return false;
    }

    if ( img_dimension )
    {
      if ( (img_dimension != ID_2D && tex_dimension == TD_TEXTURE_1D_ARRAY) || ( img_dimension != ID_3D && tex_dimension == TD_TEXTURE_2D_ARRAY ) )
      {
        if (verbose) Log::error("Texture::supports(): the image dimensions are not suitable to create a texture array."
          "To create a 1D texture array you need a 2D image and to create a 2D texture array you need a 3D image.\n");
        return false;
      }
    }
  }

  // texture rectangle

  if (tex_dimension == TD_TEXTURE_RECTANGLE)
  {
    if (!Has_Texture_Rectangle)
    {
      if (verbose) Log::error("Texture::supports(): texture rectangle not supported by the current hardware.\n");
      return false;
    }

    if ( mip_level != 0 )
    {
      if (verbose) Log::error("Texture::supports(): TD_TEXTURE_RECTANGLE textures do not support mipmapping level other than zero.\n");
      return false;
    }

    if (border)
    {
      if (verbose) Log::error("Texture::supports(): TD_TEXTURE_RECTANGLE textures do not allow textures borders\n");
      return false;
    }
  }

  // OpenGL ES does not support proxy textures and glGetTexLevelParameter*
#if defined(VL_OPENGL)
  int width = 0;

  int default_format = getDefaultFormat(tex_format);
  int default_type   = getDefaultType(tex_format);

  if (tex_dimension == TD_TEXTURE_BUFFER)
  {
    width = 1; // pass the test
  }
  else
  if (tex_dimension == TD_TEXTURE_2D_MULTISAMPLE)
  {
    glTexImage2DMultisample(GL_PROXY_TEXTURE_2D_MULTISAMPLE, samples, tex_format, w, h, fixedsamplelocations );
    if ( glGetError() )
    {
      if (verbose) Log::error( Say("Texture::supports(): 2d multisample texture requested with too many samples for the current hardware! (%n)\n") << samples );
      return false;
    }
    glGetTexLevelParameteriv(GL_PROXY_TEXTURE_2D_MULTISAMPLE, 0, GL_TEXTURE_WIDTH, &width); VL_CHECK_OGL();
  }
  else
  if (tex_dimension == TD_TEXTURE_2D_MULTISAMPLE_ARRAY)
  {
    glTexImage3DMultisample(GL_PROXY_TEXTURE_2D_MULTISAMPLE_ARRAY, samples, tex_format, w, h, d, fixedsamplelocations ); VL_CHECK_OGL();
    if ( glGetError() )
    {
      if (verbose) Log::error( Say("Texture::supports(): multisample texture array requested with too many samples for the current hardware! (%n)\n") << samples );
      return false;
    }
    glGetTexLevelParameteriv(GL_PROXY_TEXTURE_2D_MULTISAMPLE_ARRAY, 0, GL_TEXTURE_WIDTH, &width); VL_CHECK_OGL();
  }
  else
  if (tex_dimension == TD_TEXTURE_CUBE_MAP)
  {
    glTexImage2D(GL_PROXY_TEXTURE_CUBE_MAP, mip_level, tex_format, w + (border?2:0), h + (border?2:0), border?1:0, default_format, default_type, NULL);
    glGetTexLevelParameteriv(GL_PROXY_TEXTURE_CUBE_MAP, mip_level, GL_TEXTURE_WIDTH, &width);
  }
  else
  if (tex_dimension == TD_TEXTURE_2D_ARRAY)
  {
    glTexImage3D(GL_PROXY_TEXTURE_2D_ARRAY, mip_level, tex_format, w + (border?2:0), h + (border?2:0), d + (border?2:0), border?1:0, default_format, default_type, NULL);
    glGetTexLevelParameteriv(GL_PROXY_TEXTURE_2D_ARRAY, mip_level, GL_TEXTURE_WIDTH, &width);
  }
  else
  if (tex_dimension == TD_TEXTURE_3D)
  {
    glTexImage3D(GL_PROXY_TEXTURE_3D, mip_level, tex_format, w + (border?2:0), h + (border?2:0), d + (border?2:0), border?1:0, default_format, default_type, NULL);
    glGetTexLevelParameteriv(GL_PROXY_TEXTURE_3D, mip_level, GL_TEXTURE_WIDTH, &width);
  }
  else
  if (tex_dimension == TD_TEXTURE_RECTANGLE)
  {
    glTexImage2D(GL_PROXY_TEXTURE_RECTANGLE, mip_level, tex_format, w, h, 0, default_format, default_type, NULL);
    glGetTexLevelParameteriv(GL_PROXY_TEXTURE_RECTANGLE, mip_level, GL_TEXTURE_WIDTH, &width);
  }
  else
  if (tex_dimension == TD_TEXTURE_1D_ARRAY)
  {
    glTexImage2D(GL_PROXY_TEXTURE_1D_ARRAY, mip_level, tex_format, w, h, 0, default_format, default_type, NULL);
    glGetTexLevelParameteriv(GL_PROXY_TEXTURE_1D_ARRAY, mip_level, GL_TEXTURE_WIDTH, &width);
  }
  else
  if (tex_dimension == TD_TEXTURE_2D)
  {
    glTexImage2D(GL_PROXY_TEXTURE_2D, mip_level, tex_format, w + (border?2:0), h + (border?2:0), border?1:0, default_format, default_type, NULL);
    glGetTexLevelParameteriv(GL_PROXY_TEXTURE_2D, mip_level, GL_TEXTURE_WIDTH, &width);
  }
  else
  if (tex_dimension == TD_TEXTURE_1D)
  {
    glTexImage1D(GL_PROXY_TEXTURE_1D, mip_level, tex_format, w + (border?2:0), border?1:0, default_format, default_type, NULL);
    glGetTexLevelParameteriv(GL_PROXY_TEXTURE_1D, mip_level, GL_TEXTURE_WIDTH, &width);
  }

  GLenum err = glGetError();
  return err == 0 && width != 0;
#else
  return true;
#endif
}
//-----------------------------------------------------------------------------
bool Texture::createTexture(ETextureDimension tex_dimension, ETextureFormat tex_format, int w, int h, int d, bool border, BufferObject* buffer_object, int samples, bool fixedsamplelocations)
{
  VL_CHECK_OGL()

  if ( tex_dimension == TD_TEXTURE_BUFFER )
  {
    if( !buffer_object || !buffer_object->handle() || !glIsBuffer(buffer_object->handle()) )
    {
      Log::bug( "Texture::createTexture() requires a non NULL valid buffer object in order to create a texture buffer!\n" );
      VL_CHECK(buffer_object);
      VL_CHECK(buffer_object->handle());
      VL_CHECK(glIsBuffer(buffer_object->handle()));
      return false;
    }
    else
    {
      // the buffer object must not be empty!
      GLint buffer_size = 0;
      glBindBuffer(GL_TEXTURE_BUFFER, buffer_object->handle());
      glGetBufferParameteriv(GL_TEXTURE_BUFFER, GL_BUFFER_SIZE, &buffer_size);
      glBindBuffer(GL_TEXTURE_BUFFER, 0);
      if ( buffer_size == 0 )
      {
        Log::bug("Texture::createTexture(): cannot create a texture buffer with an empty buffer object!\n"); VL_TRAP();
        return false;
      }
    }
  }

  if (mHandle)
  {
    Log::bug("Texture::createTexture(): a texture can be created only once!\n");
    return false;
  }
  else
  {
    if ( !supports(tex_dimension , tex_format, 0, ID_None, w, h, d, border, samples, fixedsamplelocations, true) )
    {
      VL_CHECK_OGL()
      Log::bug("Texture::createTexture(): the format/size combination requested is not supported!\n"); VL_TRAP();
      return false;
    }

    reset();

    glGenTextures( 1, &mHandle ); VL_CHECK_OGL();

    if (!mHandle)
    {
      Log::bug("Texture::createTexture(): texture creation failed!\n");
      VL_TRAP();
      return false;
    }

    setDimension(tex_dimension);
    setInternalFormat(tex_format);
    setWidth(w);
    setHeight(h);
    setDepth(d);
    setBorder(border);
    mBufferObject = buffer_object; // the user cannot change this
    mSamples = samples;
    mFixedSamplesLocation = fixedsamplelocations;
    glBindTexture(tex_dimension, mHandle); VL_CHECK_OGL();

    int default_format = getDefaultFormat(tex_format);
    int default_type   = getDefaultType(tex_format);

    if (tex_dimension == TD_TEXTURE_2D_MULTISAMPLE)
    {
      glTexImage2DMultisample(GL_TEXTURE_2D_MULTISAMPLE, samples, tex_format, w, h, fixedsamplelocations ); VL_CHECK_OGL();
    }
    else
    if (tex_dimension == TD_TEXTURE_2D_MULTISAMPLE_ARRAY)
    {
      glTexImage3DMultisample(GL_TEXTURE_2D_MULTISAMPLE_ARRAY, samples, tex_format, w, h, d, fixedsamplelocations ); VL_CHECK_OGL();
    }
    else
    if (tex_dimension == TD_TEXTURE_BUFFER)
    {
      VL_CHECK(buffer_object)
      VL_CHECK(buffer_object->handle())
      glTexBuffer(GL_TEXTURE_BUFFER, tex_format, buffer_object->handle());
      unsigned int glerr = glGetError();
      if (glerr != GL_NO_ERROR)
      {
        String msg( (const char*)getGLErrorString(glerr) );
        Log::bug( "Texture::createTexture(): glTexBuffer() failed with error: '" + msg + "'.\n" );
        Log::error("Probably you supplied a non supported texture format! Review the glTexBuffer() man page for a complete list of supported texture formats.\n");
        VL_TRAP();
      }
    }
    else
    if (tex_dimension == TD_TEXTURE_CUBE_MAP)
    {
      glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X, 0, tex_format, w + (border?2:0), h + (border?2:0), border?1:0, default_format, default_type, NULL);
      glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_X, 0, tex_format, w + (border?2:0), h + (border?2:0), border?1:0, default_format, default_type, NULL);
      glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_Y, 0, tex_format, w + (border?2:0), h + (border?2:0), border?1:0, default_format, default_type, NULL);
      glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_Y, 0, tex_format, w + (border?2:0), h + (border?2:0), border?1:0, default_format, default_type, NULL);
      glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_Z, 0, tex_format, w + (border?2:0), h + (border?2:0), border?1:0, default_format, default_type, NULL);
      glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, 0, tex_format, w + (border?2:0), h + (border?2:0), border?1:0, default_format, default_type, NULL);
      VL_CHECK_OGL();
    }
    else
    if (tex_dimension == TD_TEXTURE_2D_ARRAY)
    {
      VL_glTexImage3D(GL_TEXTURE_2D_ARRAY, 0, tex_format, w + (border?2:0), h + (border?2:0), d + (border?2:0), border?1:0, default_format, default_type, NULL);
      VL_CHECK_OGL();
    }
    else
    if (tex_dimension == TD_TEXTURE_3D)
    {
      VL_glTexImage3D(GL_TEXTURE_3D, 0, tex_format, w + (border?2:0), h + (border?2:0), d + (border?2:0), border?1:0, default_format, default_type, NULL);
      VL_CHECK_OGL();
    }
    else
    if (tex_dimension == TD_TEXTURE_RECTANGLE)
    {
      glTexImage2D(GL_TEXTURE_RECTANGLE, 0, tex_format, w, h, 0, default_format, default_type, NULL);
      VL_CHECK_OGL();
    }
    else
    if (tex_dimension == TD_TEXTURE_1D_ARRAY)
    {
      glTexImage2D(GL_TEXTURE_1D_ARRAY, 0, tex_format, w, h, 0, default_format, default_type, NULL);
      VL_CHECK_OGL();
    }
    else
    if (tex_dimension == TD_TEXTURE_2D)
    {
      glTexImage2D(GL_TEXTURE_2D, 0, tex_format, w + (border?2:0), h + (border?2:0), border?1:0, default_format, default_type, NULL);
      VL_CHECK_OGL();
    }
    else
    if (tex_dimension == TD_TEXTURE_1D)
    {
      glTexImage1D(GL_TEXTURE_1D, 0, tex_format, w + (border?2:0), border?1:0, default_format, default_type, NULL);
      VL_CHECK_OGL();
    }

    glBindTexture(tex_dimension, 0); VL_CHECK_OGL();
    return true;
  }
}
//-----------------------------------------------------------------------------
bool Texture::setMipLevel(int mip_level, const Image* img, bool gen_mipmaps)
{
  VL_CHECK_OGL()

#if defined(VL_OPENGL_ES1) || defined(VL_OPENGL_ES2)
    if (internalFormat() != img->format())
    {
      Log::bug("Texture::setMipLevel(): under OpenGL ES the texture internal format must match the source image format!\n");
      return false;
    }
#endif

  if ( dimension() == TD_TEXTURE_BUFFER || dimension() == TD_TEXTURE_2D_MULTISAMPLE || dimension() == TD_TEXTURE_2D_MULTISAMPLE_ARRAY )
  {
    Log::bug("You cannot call Texture::setMipLevel() on a texture buffer or on a multisample texture!\n");
    return false;
  }

  if (!mHandle)
  {
    Log::error("Texture::setMipLevel(): texture hasn't been created yet, please call createTexture() first!\n");
    VL_TRAP();
    return false;
  }

  if ( !supports(dimension(), internalFormat(), mip_level, img->dimension(), img->width(), img->height(), img->depth(), border(), 0, 0, true) )
  {
    VL_CHECK_OGL()
    Log::error("Texture::setMipLevel(): the format/size combination requested is not supported.\n");
    return false;
  }

  glPixelStorei( GL_UNPACK_ALIGNMENT, img->byteAlignment() ); VL_CHECK_OGL()

  glBindTexture( dimension(), mHandle ); VL_CHECK_OGL()

  int w = width()  + (border()?2:0);
  int h = height() + (border()?2:0);
  int d = depth()  + (border()?2:0);
  int is_compressed = (int)img->format() == (int)internalFormat() && isCompressedFormat( internalFormat() );

  bool use_glu = false;
  GLint generate_mipmap_orig = GL_FALSE;
  if ( gen_mipmaps )
  {
    if ( Has_glGenerateMipmaps )
    {
      // do nothing, we will use glGenerateMipmaps later
    }
    else
    if( Has_GL_GENERATE_MIPMAP )
    {
      glGetTexParameteriv( dimension(), GL_GENERATE_MIPMAP, &generate_mipmap_orig ); VL_CHECK_OGL()
      glTexParameteri(dimension(), GL_GENERATE_MIPMAP, GL_TRUE); VL_CHECK_OGL()
    }
    else
    {
      if (mip_level > 0) // because GLU regenerates the whole mip-mapping chain
        Log::error("Texture::setMipLevel(): automatic mipmaps generation for levels below 0 requires OpenGL 1.4 minimum.\n");
      else
        use_glu = true;

      #define VL_IS_POW_2(x) ((x != 0) && ((x & (x - 1)) == 0))
      if ( !VL_IS_POW_2(w) || !VL_IS_POW_2(h) )
        Log::warning("Texture::setMipLevel(): the image will be rescaled to the nearest upper power of 2.\n");
    }
  }

  if ( use_glu && is_compressed )
  {
    Log::error("Texture::setMipLevel(): could not generate compressed mipmaps, OpenGL 1.4 required.\n");
    use_glu = false;
  }

  if ( use_glu && dimension() == TD_TEXTURE_3D )
  {
    Log::error("Texture::setMipLevel(): could not generate 3D mipmaps, OpenGL 1.4 required.\n");
    use_glu = false;
  }

  if (dimension() == TD_TEXTURE_CUBE_MAP)
  {
    if (is_compressed)
    {
      VL_CHECK( img->requiredMemory() % 6 == 0 );
      int bytes = img->requiredMemory() / 6;
      glCompressedTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X, mip_level, internalFormat(), w, h, border()?1:0, bytes, img->pixelsXP());
      glCompressedTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_X, mip_level, internalFormat(), w, h, border()?1:0, bytes, img->pixelsXN());
      glCompressedTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_Y, mip_level, internalFormat(), w, h, border()?1:0, bytes, img->pixelsYP());
      glCompressedTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_Y, mip_level, internalFormat(), w, h, border()?1:0, bytes, img->pixelsYN());
      glCompressedTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_Z, mip_level, internalFormat(), w, h, border()?1:0, bytes, img->pixelsZP());
      glCompressedTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, mip_level, internalFormat(), w, h, border()?1:0, bytes, img->pixelsZN());
      VL_CHECK_OGL()
    }
    else
    {
      if (use_glu)
      {
        gluBuild2DMipmaps(GL_TEXTURE_CUBE_MAP_POSITIVE_X, internalFormat(), w, h, img->format(), img->type(), img->pixelsXP());
        gluBuild2DMipmaps(GL_TEXTURE_CUBE_MAP_NEGATIVE_X, internalFormat(), w, h, img->format(), img->type(), img->pixelsXN());
        gluBuild2DMipmaps(GL_TEXTURE_CUBE_MAP_POSITIVE_Y, internalFormat(), w, h, img->format(), img->type(), img->pixelsYP());
        gluBuild2DMipmaps(GL_TEXTURE_CUBE_MAP_NEGATIVE_Y, internalFormat(), w, h, img->format(), img->type(), img->pixelsYN());
        gluBuild2DMipmaps(GL_TEXTURE_CUBE_MAP_POSITIVE_Z, internalFormat(), w, h, img->format(), img->type(), img->pixelsZP());
        gluBuild2DMipmaps(GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, internalFormat(), w, h, img->format(), img->type(), img->pixelsZN());
        VL_CHECK_OGL()
      }
      else
      {
        glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X, mip_level, internalFormat(), w, h, border()?1:0, img->format(), img->type(), img->pixelsXP());
        glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_X, mip_level, internalFormat(), w, h, border()?1:0, img->format(), img->type(), img->pixelsXN());
        glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_Y, mip_level, internalFormat(), w, h, border()?1:0, img->format(), img->type(), img->pixelsYP());
        glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_Y, mip_level, internalFormat(), w, h, border()?1:0, img->format(), img->type(), img->pixelsYN());
        glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_Z, mip_level, internalFormat(), w, h, border()?1:0, img->format(), img->type(), img->pixelsZP());
        glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, mip_level, internalFormat(), w, h, border()?1:0, img->format(), img->type(), img->pixelsZN());
        VL_CHECK_OGL()
      }
    }
  }
  else
  if (dimension() == TD_TEXTURE_2D_ARRAY)
  {
    if (is_compressed)
    {
      VL_glCompressedTexImage3D(GL_TEXTURE_2D_ARRAY, mip_level, internalFormat(), w, h, d, border()?1:0, img->requiredMemory(), img->pixels());
      VL_CHECK_OGL()
    }
    else
    {
      VL_glTexImage3D(GL_TEXTURE_2D_ARRAY, mip_level, internalFormat(), w, h, d, border()?1:0, img->format(), img->type(), img->pixels());
      VL_CHECK_OGL()
    }
  }
  else
  if (dimension() == TD_TEXTURE_3D)
  {
    if (is_compressed)
    {
      VL_glCompressedTexImage3D(GL_TEXTURE_3D, mip_level, internalFormat(), w, h, d, border()?1:0, img->requiredMemory(), img->pixels());
      VL_CHECK_OGL()
    }
    else
    {
      VL_glTexImage3D(GL_TEXTURE_3D, mip_level, internalFormat(), w, h, d, border()?1:0, img->format(), img->type(), img->pixels());
      VL_CHECK_OGL()
    }
  }
  else
  if (dimension() == TD_TEXTURE_RECTANGLE)
  {
    if (is_compressed)
    {
      glCompressedTexImage2D(GL_TEXTURE_RECTANGLE, mip_level, internalFormat(), width(), height(), 0, img->requiredMemory(), img->pixels());
      VL_CHECK_OGL()
    }
    else
    {
      glTexImage2D(GL_TEXTURE_RECTANGLE, mip_level, internalFormat(), width(), height(), 0, img->format(), img->type(), img->pixels());
      VL_CHECK_OGL()
    }
  }
  else
  if (dimension() == TD_TEXTURE_1D_ARRAY)
  {
    if (is_compressed)
    {
      glCompressedTexImage2D(GL_TEXTURE_1D_ARRAY, mip_level, internalFormat(), width(), height(), 0, img->requiredMemory(), img->pixels());
      VL_CHECK_OGL()
    }
    else
    {
      glTexImage2D(GL_TEXTURE_1D_ARRAY, mip_level, internalFormat(), width(), height(), 0, img->format(), img->type(), img->pixels());
      VL_CHECK_OGL()
    }
  }
  else
  if (dimension() == TD_TEXTURE_2D)
  {
    if (is_compressed)
    {
      glCompressedTexImage2D(GL_TEXTURE_2D, mip_level, internalFormat(), w, h, border()?1:0, img->requiredMemory(), img->pixels());
      VL_CHECK_OGL()
    }
    else
    {
      if (use_glu)
      {
        gluBuild2DMipmaps(GL_TEXTURE_2D, internalFormat(), w, h, img->format(), img->type(), img->pixels());
        VL_CHECK_OGL()
      }
      else
      {
        glTexImage2D(GL_TEXTURE_2D, mip_level, internalFormat(), w, h, border()?1:0, img->format(), img->type(), img->pixels());
        VL_CHECK_OGL()
      }
    }
  }
  else
  if (dimension() == TD_TEXTURE_1D)
  {
    if (is_compressed)
    {
      glCompressedTexImage1D(GL_TEXTURE_1D, mip_level, internalFormat(), w, border()?1:0, img->requiredMemory(), img->pixels());
      VL_CHECK_OGL()
    }
    else
    {
      if (use_glu)
      {
        gluBuild1DMipmaps(GL_TEXTURE_1D, internalFormat(), w, img->format(), img->type(), img->pixels());
        VL_CHECK_OGL()
      }
      else
      {
        glTexImage1D(GL_TEXTURE_1D, mip_level, internalFormat(), w, border()?1:0, img->format(), img->type(), img->pixels());
        VL_CHECK_OGL()
      }
    }
  }

  if ( gen_mipmaps )
  {
    if ( Has_glGenerateMipmaps )
    {
      glGenerateMipmap( dimension() );
    }
    else
    if ( Has_GL_GENERATE_MIPMAP )
    {
      glTexParameteri(dimension(), GL_GENERATE_MIPMAP, generate_mipmap_orig); VL_CHECK_OGL()
    }
  }

  glBindTexture( dimension(), 0 ); VL_CHECK_OGL()

  glPixelStorei( GL_UNPACK_ALIGNMENT, 4 );

  return true;
}
//-----------------------------------------------------------------------------
bool Texture::createTexture()
{
  VL_CHECK_OGL()

  if (!setupParams())
    return false;

  class InOutCondition
  {
    Texture* mTex;
  public:
    InOutCondition(Texture* tex): mTex(tex) {}
    ~InOutCondition() 
    {
      // make sure no errors were generated
      VL_CHECK_OGL()

      // release Image and BufferObject from SetupParams
      if (mTex->mSetupParams)
      {
        if (mTex->mSetupParams->imagePath().empty() && mTex->mSetupParams->image())
          mTex->mSetupParams->setImagePath( mTex->mSetupParams->image()->filePath() );
        mTex->mSetupParams->setImage(NULL);
        mTex->mSetupParams->setBufferObject(NULL);
      }
    }
  };

  InOutCondition in_out_condition(this);

  ETextureFormat tex_format = setupParams()->format();
  ETextureDimension tex_dimension = setupParams()->dimension();
  bool gen_mipmaps = setupParams()->genMipmaps();
  bool border = setupParams()->border();
  if ( !setupParams()->image() && !setupParams()->imagePath().empty() ) 
  {
    setupParams()->setImage( loadImage( setupParams()->imagePath() ).get() );
    if (!setupParams()->image())
    {
      vl::Log::error( Say("Texture::createTexture(): could not load image file '%s'\n") << setupParams()->imagePath() );
      return false;
    }
  }

  ref<Image> img = const_cast<Image*>(setupParams()->image());

  int w = setupParams()->width();
  int h = setupParams()->height();
  int d = setupParams()->depth();
  if (img)
  {
    setObjectName( img->objectName().c_str() );
    w = img->width();
    h = img->height();
    d = img->depth();
    // guess from image format
    if (tex_format == TF_UNKNOWN)
      tex_format = (ETextureFormat)img->format();
  }
  //w = w > 0 ? w : 1;
  //h = h > 0 ? h : 1;
  //d = d > 0 ? d : 1;

  if ( !createTexture( tex_dimension, tex_format, w, h, d, border, setupParams()->bufferObject(), setupParams()->samples(), setupParams()->fixedSamplesLocations() ) )
    return false;

  VL_CHECK_OGL()

  if (img)
  {
    // compile mipmapping levels
    std::vector<const vl::Image*> mipmaps;
    mipmaps.push_back(img.get());
    for(int i=0; i<(int)img->mipmaps().size(); ++i)
      mipmaps.push_back( img->mipmaps()[i].get() );

    bool ok = false;

    if (!gen_mipmaps) // no mipmaps
    {
      ok = setMipLevel(0, img.get(), false);
    }
    else 
    {
      if (mipmaps.size() > 1) // explicit mipmaps
      {
        for(int i=0; i<(int)mipmaps.size(); ++i)
          ok = setMipLevel(i, mipmaps[i], false);
      }
      else // automatic mipmaps generation
      if (mipmaps.size() == 1)
      {
        ok = setMipLevel(0, img.get(), true);
      }
    }
    return ok;
  }
  else
    return true;
}
//-----------------------------------------------------------------------------
void Texture::clone(const Texture& other)
{
  // keep its own copyof SetupParams
  if (other.mSetupParams)
    mSetupParams = new SetupParams(*other.mSetupParams);
  else
    mSetupParams = NULL;
  mHandle        = other.mHandle;
  mTexParameter  = other.mTexParameter;
  mBufferObject  = other.mBufferObject;
  mFormat        = other.mFormat;
  mDimension     = other.mDimension;
  mWidth         = other.mWidth;
  mHeight        = other.mHeight;
  mDepth         = other.mDepth;
  mSamples       = other.mSamples;
  mBorder        = other.mBorder;
  mFixedSamplesLocation = other.mFixedSamplesLocation;
}
//-----------------------------------------------------------------------------
bool Texture::isDepthTexture() const
{
  switch(internalFormat())
  {
    case TF_DEPTH_STENCIL:
    case TF_DEPTH24_STENCIL8:

    case TF_DEPTH_COMPONENT32F:
    case TF_DEPTH32F_STENCIL8:

    case TF_DEPTH_COMPONENT:
    case TF_DEPTH_COMPONENT16:
    case TF_DEPTH_COMPONENT24:
    case TF_DEPTH_COMPONENT32:
      return true;

    default:
      return false;
  }
}
//-----------------------------------------------------------------------------
bool Texture::isCompressedFormat(int format)
{
  int comp[] =
  {
    TF_COMPRESSED_ALPHA,
    TF_COMPRESSED_INTENSITY,
    TF_COMPRESSED_LUMINANCE,
    TF_COMPRESSED_LUMINANCE_ALPHA,
    TF_COMPRESSED_RGB,
    TF_COMPRESSED_RGBA,

    // 3DFX_texture_compression_FXT1
    TF_COMPRESSED_RGB_FXT1_3DFX,
    TF_COMPRESSED_RGBA_FXT1_3DFX,

    // EXT_texture_compression_s3tc
    TF_COMPRESSED_RGB_S3TC_DXT1_EXT,
    TF_COMPRESSED_RGBA_S3TC_DXT1_EXT,
    TF_COMPRESSED_RGBA_S3TC_DXT3_EXT,
    TF_COMPRESSED_RGBA_S3TC_DXT5_EXT,

    // GL_EXT_texture_compression_latc
    TF_COMPRESSED_LUMINANCE_LATC1_EXT,
    TF_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT,
    TF_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT,
    TF_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT,

    // EXT_texture_compression_rgtc
    TF_COMPRESSED_RED_RGTC1_EXT,
    TF_COMPRESSED_SIGNED_RED_RGTC1_EXT,
    TF_COMPRESSED_RED_GREEN_RGTC2_EXT,
    TF_COMPRESSED_SIGNED_RED_GREEN_RGTC2_EXT,

    0
  };

  for(int i=0; comp[i]; ++i)
  {
    if(comp[i] == format)
      return true;
  }

  return false;
}
//-----------------------------------------------------------------------------
