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

#ifndef CopyTexSubImage_INCLUDE_ONCE
#define CopyTexSubImage_INCLUDE_ONCE

#include <vlGraphics/Camera.hpp>
#include <vlGraphics/Texture.hpp>
#include <vlGraphics/RenderEventCallback.hpp>

namespace vl
{
  //-----------------------------------------------------------------------------
  // CopyTexSubImage
  //-----------------------------------------------------------------------------
  /** Wrapper class of the OpenGL function glCopyTexSubImage.
    *
    * Is the base class of CopyTexSubImage1D, CopyTexSubImage2D, CopyTexSubImage3D.
    * Copies a rectangular pixels area from the specified read buffer to the specified portion of the texture.
    * Can be bound to a Rendering as a RenderEventCallback in order to automatically copy 
    * the result of a rendering into a texture, see Rendering.
    *
    * \note
    * This class supports Frame-Buffer-Objects, 3D textures, cubemaps.
    *
    * \sa 
    * FramebufferObject, FBOAbstractAttachment, Rendering, RenderEventCallback, CopyTexSubImage1D, CopyTexSubImage2D, CopyTexSubImage3D */
  class CopyTexSubImage: public RenderEventCallback
  {
    VL_INSTRUMENT_ABSTRACT_CLASS(vl::CopyTexSubImage, RenderEventCallback)

  public:
    CopyTexSubImage(): mReadBuffer(RDB_BACK_LEFT) 
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }

    /** The source buffer used when copying color buffers, ignored when using depth textures. */
    EReadDrawBuffer readBuffer() const { return mReadBuffer; }

    /** The source buffer used when copying color buffers, ignored when using depth textures. */
    void setReadBuffer(EReadDrawBuffer render_buffer) { mReadBuffer = render_buffer; }

    virtual bool onRenderingStarted(const RenderingAbstract*)
    {
      copyPixels();
      return true;
    }

    virtual bool onRenderingFinished(const RenderingAbstract*)
    {
      copyPixels();
      return true;
    }

    virtual bool onRendererStarted(const RendererAbstract*)
    {
      copyPixels();
      return true;
    }

    virtual bool onRendererFinished(const RendererAbstract*)
    {
      copyPixels();
      return true;
    }

    /**
     * Copies the pixels from the specified read buffer to the specified texture.
    */
    virtual void copyPixels() = 0;

  protected:
    EReadDrawBuffer mReadBuffer;
  };
  //-----------------------------------------------------------------------------
  //! Wraps glCopyTexSubImage1D, see also CopyTexSubImage.
  class CopyTexSubImage1D: public CopyTexSubImage
  {
    VL_INSTRUMENT_CLASS(vl::CopyTexSubImage1D, CopyTexSubImage)

  public:
    CopyTexSubImage1D(int level, int xoffset, int x, int y, int width, Texture* texture=NULL, EReadDrawBuffer read_buffer=RDB_BACK_LEFT)
    {
      VL_DEBUG_SET_OBJECT_NAME()
      mLevel = level;
      mXOffset = xoffset;
      mX = x;
      mY = y;
      mWidth  = width;
      mTexture = texture;
      setReadBuffer(read_buffer);
    }

    void setTexture(Texture* tex) { mTexture = tex; }
    void setLevel(int level) { mLevel = level; }
    void setXOffset(int xoffset) { mXOffset = xoffset; }
    void setX(int x) { mX = x; }
    void setY(int y) { mY = y; }
    void setWidth(int width) { mWidth = width; }

    Texture* texture() { return mTexture.get(); }
    const Texture* texture() const { return mTexture.get(); }
    int level() const { return mLevel; }
    int xoffset() const { return mXOffset; }
    int x() const { return mX; }
    int y() const { return mY; }
    int width() const { return mWidth; }

    virtual void copyPixels()
    {
      VL_CHECK_OGL()
      VL_CHECK(texture()->dimension() == TD_TEXTURE_1D)

      VL_CHECK(texture()->handle())
      VL_CHECK(xoffset() >= 0)
      VL_CHECK(x() >= 0)
      VL_CHECK(y() >= 0)
      VL_CHECK(xoffset()+width() <= texture()->width())

#if defined(VL_OPENGL)
      int read_buffer = 0;
      if (!texture()->isDepthTexture())
      {
        glGetIntegerv(GL_READ_BUFFER, &read_buffer); VL_CHECK_OGL()
        glReadBuffer(readBuffer()); VL_CHECK_OGL()
      }
#endif

      glBindTexture(TD_TEXTURE_1D, texture()->handle() ); VL_CHECK_OGL()
      glCopyTexSubImage1D(TD_TEXTURE_1D, level(), xoffset(), x(), y(), width()); VL_CHECK_OGL()
      glBindTexture(TD_TEXTURE_1D, 0 ); VL_CHECK_OGL()

#if defined(VL_OPENGL)
      if (read_buffer)
      {
        glReadBuffer( read_buffer ); VL_CHECK_OGL()
      }
#endif
    }

  protected:
    ref<Texture> mTexture;
    int mLevel;
    int mXOffset;
    int mX;
    int mY;
    int mWidth;
  };
  //-----------------------------------------------------------------------------
  //! Wraps glCopyTexSubImage2D, see also CopyTexSubImage. To be used also for 1D array textures.
  class CopyTexSubImage2D: public CopyTexSubImage
  {
    VL_INSTRUMENT_CLASS(vl::CopyTexSubImage2D, CopyTexSubImage)

  public:
    CopyTexSubImage2D(int level, int xoffset, int yoffset, int x, int y, int width, int height, Texture* texture=NULL, ETex2DTarget target=T2DT_TEXTURE_2D, EReadDrawBuffer read_buffer=RDB_BACK_LEFT)
    {
      VL_DEBUG_SET_OBJECT_NAME()

      mLevel = level;
      mXOffset = xoffset;
      mYOffset = yoffset;
      mX = x;
      mY = y;
      mWidth  = width;
      mHeight = height;
      mTexture = texture;
      mTarget = target;
      setReadBuffer(read_buffer);
    }

    void setTexture(Texture* tex) { mTexture = tex; }
    void setLevel(int level) { mLevel = level; }
    void setXOffset(int xoffset) { mXOffset = xoffset; }
    void setYOffset(int yoffset) { mYOffset = yoffset; }
    void setX(int x) { mX = x; }
    void setY(int y) { mY = y; }
    void setWidth(int width) { mWidth = width; }
    void setHeight(int height) { mHeight = height; }
    void setTarget(ETex2DTarget target) { mTarget = target; }

    Texture* texture() { return mTexture.get(); }
    const Texture* texture() const { return mTexture.get(); }
    int level() const { return mLevel; }
    int xoffset() const { return mXOffset; }
    int yoffset() const { return mYOffset; }
    int x() const { return mX; }
    int y() const { return mY; }
    int width() const { return mWidth; }
    int height() const { return mHeight; }
    ETex2DTarget target() const { return mTarget; }

    virtual void copyPixels()
    {
      VL_CHECK_OGL()

      VL_CHECK(texture()->handle())
      VL_CHECK(xoffset() >= 0)
      VL_CHECK(yoffset() >= 0)
      VL_CHECK(x() >= 0)
      VL_CHECK(y() >= 0)
      VL_CHECK(xoffset()+width() <= texture()->width())
      VL_CHECK(yoffset()+height() <= texture()->height())

#if defined(VL_OPENGL)
      int read_buffer = 0;
      if (!texture()->isDepthTexture())
      {
        glGetIntegerv(GL_READ_BUFFER, &read_buffer); VL_CHECK_OGL()
        glReadBuffer(readBuffer()); VL_CHECK_OGL()
      }
#endif

      int bind_target = 0;
      switch( target() )
      {
        case T2DT_TEXTURE_CUBE_MAP_POSITIVE_X:
        case T2DT_TEXTURE_CUBE_MAP_NEGATIVE_X:
        case T2DT_TEXTURE_CUBE_MAP_POSITIVE_Y:
        case T2DT_TEXTURE_CUBE_MAP_NEGATIVE_Y:
        case T2DT_TEXTURE_CUBE_MAP_POSITIVE_Z:
        case T2DT_TEXTURE_CUBE_MAP_NEGATIVE_Z:
          bind_target = GL_TEXTURE_CUBE_MAP;
          break;
        default:
          bind_target = target();
      };

      glBindTexture( bind_target, texture()->handle() ); VL_CHECK_OGL()
      glCopyTexSubImage2D( target(), level(), xoffset(), yoffset(), x(), y(), width(), height() ); VL_CHECK_OGL()
      glBindTexture( bind_target, 0 ); VL_CHECK_OGL()

#if defined(VL_OPENGL)
      if (read_buffer)
      {
        glReadBuffer( read_buffer ); VL_CHECK_OGL()
      }
#endif
    }

  protected:
    ref<Texture> mTexture;
    int mLevel;
    int mXOffset;
    int mYOffset;
    int mX;
    int mY;
    int mWidth;
    int mHeight;
    ETex2DTarget mTarget;
  };
  //-----------------------------------------------------------------------------
  //! Wraps glCopyTexSubImage3D, see also CopyTexSubImage. To be used also for 2D array textures.
  class CopyTexSubImage3D: public CopyTexSubImage
  {
    VL_INSTRUMENT_CLASS(vl::CopyTexSubImage3D, CopyTexSubImage)

  public:
    CopyTexSubImage3D(int level, int xoffset, int yoffset, int zoffset, int x, int y, int width, int height, Texture* texture, EReadDrawBuffer read_buffer=RDB_BACK_LEFT)
    {
      VL_DEBUG_SET_OBJECT_NAME()

      mLevel = level;
      mXOffset = xoffset;
      mYOffset = yoffset;
      mZOffset = zoffset;
      mX = x;
      mY = y;
      mWidth  = width;
      mHeight = height;
      mTexture = texture;
      setReadBuffer(read_buffer);
    }

    void setTexture(Texture* tex) { mTexture = tex; }
    void setLevel(int level) { mLevel = level; }
    void setXOffset(int xoffset) { mXOffset = xoffset; }
    void setYOffset(int yoffset) { mYOffset = yoffset; }
    void setZOffset(int zoffset) { mZOffset = zoffset; }
    void setX(int x) { mX = x; }
    void setY(int y) { mY = y; }
    void setWidth(int width) { mWidth = width; }
    void setHeight(int height) { mHeight = height; }

    Texture* texture() { return mTexture.get(); }
    const Texture* texture() const { return mTexture.get(); }
    int level() const { return mLevel; }
    int xoffset() const { return mXOffset; }
    int yoffset() const { return mYOffset; }
    int zoffset() const { return mZOffset; }
    int x() const { return mX; }
    int y() const { return mY; }
    int width() const { return mWidth; }
    int height() const { return mHeight; }

    virtual void copyPixels()
    {
      if (Has_Texture_3D)
      {
        VL_CHECK_OGL()
        VL_CHECK( texture()->dimension() == TD_TEXTURE_3D )

        VL_CHECK(texture()->handle())
        VL_CHECK(xoffset() >= 0)
        VL_CHECK(yoffset() >= 0)
        VL_CHECK(zoffset() >= 0)
        VL_CHECK(x() >= 0)
        VL_CHECK(y() >= 0)
        VL_CHECK(xoffset()+width()  <= texture()->width())
        VL_CHECK(yoffset()+height() <= texture()->height())
        VL_CHECK(zoffset() < texture()->depth())

#if defined(VL_OPENGL)
        int read_buffer = 0;
        if (!texture()->isDepthTexture())
        {
          glGetIntegerv(GL_READ_BUFFER, &read_buffer); VL_CHECK_OGL()
          glReadBuffer(readBuffer()); VL_CHECK_OGL()
        }
#endif

        glBindTexture(texture()->dimension(), texture()->handle() ); VL_CHECK_OGL()
        VL_glCopyTexSubImage3D(texture()->dimension(), level(), xoffset(), yoffset(), zoffset(), x(), y(), width(), height()); VL_CHECK_OGL()
        glBindTexture(texture()->dimension(), 0 );

#if defined(VL_OPENGL)
        if (read_buffer)
        {
          glReadBuffer( read_buffer ); VL_CHECK_OGL()
        }
#endif
      }
      else
        Log::error("CopyTexSubImage3D requires OpenGL 1.2!\n");
    }

  protected:
    ref<Texture> mTexture;
    int mLevel;
    int mXOffset;
    int mYOffset;
    int mZOffset;
    int mX;
    int mY;
    int mWidth;
    int mHeight;
  };
  //-----------------------------------------------------------------------------
}

#endif
