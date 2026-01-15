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

#ifndef Framebuffer_INCLUDE_ONCE
#define Framebuffer_INCLUDE_ONCE

#include <vlCore/vlnamespace.hpp>
#include <vlCore/Object.hpp>
#include <vlGraphics/OpenGL.hpp>
#include <vector>

namespace vl
{
  class OpenGLContext;
  //-----------------------------------------------------------------------------
  // Framebuffer
  //-----------------------------------------------------------------------------
  /** The Framebuffer class defines an abstract 'surface' where OpenGL can render into.
   * \sa OpenGLContext::framebuffer() and FramebufferObject
   */
  class VLGRAPHICS_EXPORT Framebuffer: public Object
  {
    VL_INSTRUMENT_CLASS(vl::Framebuffer, Object)

    friend class OpenGLContext;

  public:
    /** Constructor, see also OpenGLContext::framebuffer(). */
    Framebuffer(OpenGLContext* ctx, int w, int h, EReadDrawBuffer draw_buffer, EReadDrawBuffer read_buffer):
    mOpenGLContext(ctx), mWidth(w), mHeight(h)
    {
      VL_DEBUG_SET_OBJECT_NAME()
      setDrawBuffer(draw_buffer);
      setReadBuffer(read_buffer);
    }

    /** The OpenGLContext bound to a render target. */
    OpenGLContext* openglContext() { return mOpenGLContext; }
    
    /** The OpenGLContext bound to a render target. */
    const OpenGLContext* openglContext() const { return mOpenGLContext; }

    /** The width of a render target. */
    int width() const { return mWidth; }
    
    /** The height of a render target. */
    int height() const { return mHeight; }
    
    /** The width of a render target. */
    void setWidth(int width) { mWidth = width; }
    
    /** The height of a render target. */
    void setHeight(int height) { mHeight = height; }

    /** The framebuffer object id as used by glBindFramebuffer, Framebuffer::handle() always returns 0, i.e., the standard OpenGL framebuffer. */
    virtual GLuint handle() const { return 0; }

    /** Activates the Framebuffer by calling bindFramebuffer() and bindDrawBuffers() */
    void activate(EFramebufferBind target = FBB_FRAMEBUFFER)
    {
      bindFramebuffer(target);
    }

    /** 
      * Calls glBindFramebuffer(target, 0) thus activating the the framebuffer 0, that is, the normal OpenGL buffers.
      * \note This method is overridden in FramebufferObject in order to activate the appropriate framebuffer object.
      */
    virtual void bindFramebuffer(EFramebufferBind target = FBB_FRAMEBUFFER)
    {
      VL_CHECK_OGL()

      // the base render target is the framebuffer 0, that is, the normal OpenGL buffers
      VL_glBindFramebuffer(target, 0); VL_CHECK_OGL()

#if defined(VL_OPENGL)
      // bind draw buffers
      bindDrawBuffers();

      // bind read buffer
      bindReadBuffer();
#endif
      
      VL_CHECK_OGL()
    }

    /** Binds to the currently active framebuffer object (including the 0 one) the read buffer specified by setReadBuffer(). */
    void bindReadBuffer();

    /** Binds to the currently active framebuffer object (including the 0 one) the draw buffers specified by setDrawBuffers(). */
    void bindDrawBuffers() const;

    /** Returns \p true if the draw buffers bound to this render target are legal for this render target type. */
    bool checkDrawBuffers() const;

    /** Specifies the color buffer to be drawn into. */
    void setDrawBuffer(EReadDrawBuffer draw_buffer)
    {
      mDrawBuffers.clear();
      mDrawBuffers.push_back(draw_buffer);
    }

    /** Specifies a list of color buffers to be drawn into. */
    void setDrawBuffers(EReadDrawBuffer draw_buffer1, EReadDrawBuffer draw_buffer2)
    {
      mDrawBuffers.clear();
      mDrawBuffers.push_back(draw_buffer1);
      mDrawBuffers.push_back(draw_buffer2);
    }

    /** Specifies a list of color buffers to be drawn into. */
    void setDrawBuffers(EReadDrawBuffer draw_buffer1, EReadDrawBuffer draw_buffer2, EReadDrawBuffer draw_buffer3)
    {
      mDrawBuffers.clear();
      mDrawBuffers.push_back(draw_buffer1);
      mDrawBuffers.push_back(draw_buffer2);
      mDrawBuffers.push_back(draw_buffer3);
    }

    /** Specifies a list of color buffers to be drawn into. */
    void setDrawBuffers(EReadDrawBuffer draw_buffer1, EReadDrawBuffer draw_buffer2, EReadDrawBuffer draw_buffer3, EReadDrawBuffer draw_buffer4)
    {
      mDrawBuffers.clear();
      mDrawBuffers.push_back(draw_buffer1);
      mDrawBuffers.push_back(draw_buffer2);
      mDrawBuffers.push_back(draw_buffer3);
      mDrawBuffers.push_back(draw_buffer4);
    }

    /** Specifies a list of color buffers to be drawn into. */
    void setDrawBuffers(const std::vector< EReadDrawBuffer >& draw_buffers) { mDrawBuffers = draw_buffers; }

    /** The color buffers to be drawn into. */
    const std::vector< EReadDrawBuffer >& drawBuffers() { return mDrawBuffers; }

    /** The read-buffer bound when the render target is activated. */
    EReadDrawBuffer readBuffer() const { return mReadBuffer; }
    
    /** The read-buffer bound when the render target is activated. */
    void setReadBuffer(EReadDrawBuffer read_buffer) { mReadBuffer = read_buffer; }

  private:
    std::vector< EReadDrawBuffer > mDrawBuffers;
    EReadDrawBuffer mReadBuffer;
    OpenGLContext* mOpenGLContext;
    int mWidth;
    int mHeight;
  };
  //------------------------------------------------------------------------------
}

#endif
