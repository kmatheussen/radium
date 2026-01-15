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

#ifndef BlitFramebuffer_INCLUDE_ONCE
#define BlitFramebuffer_INCLUDE_ONCE

#include <vlGraphics/RenderEventCallback.hpp>
#include <vlGraphics/FramebufferObject.hpp>

namespace vl
{
  /**
   * A RenderEventCallback that can be used to copy pixels from a framebuffer to another as
   * described in GL_EXT_framebuffer_blit.
  */
  class BlitFramebuffer: public RenderEventCallback
  {
    VL_INSTRUMENT_CLASS(vl::BlitFramebuffer, RenderEventCallback)

  public:
    BlitFramebuffer()
    {
      VL_DEBUG_SET_OBJECT_NAME()
      setSrcRect(0,0,640,480);
      setDstRect(0,0,640,480);
      mBufferMask = 0;
      mLinearFilteringEnabled = false;
      mReadBuffer = RDB_COLOR_ATTACHMENT0;
    }

    //! Performs the actual pixel copy from the read framebuffer to the draw framebuffer
    void copyPixels()
    {
      if (Has_GL_EXT_framebuffer_blit||Has_GL_ARB_framebuffer_object)
      {
        VL_CHECK_OGL()

        // save FBOs
        GLint read_fbo = 0;
        GLint draw_fbo = 0;
        glGetIntegerv(GL_READ_FRAMEBUFFER_BINDING, &read_fbo); VL_CHECK_OGL()
        glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, &draw_fbo); VL_CHECK_OGL()

        // initializes the source render target
        readFramebuffer()->activate(FBB_READ_FRAMEBUFFER); VL_CHECK_OGL()
        
        // override read buffer specified by readFramebuffer()->activate() with the one specified by the user here
        glReadBuffer(mReadBuffer); VL_CHECK_OGL()
        
        // activate destination render target
        drawFramebuffer()->activate(FBB_DRAW_FRAMEBUFFER); VL_CHECK_OGL()

        // note: keep the draw-buffers as specified by drawFramebuffer()->activate()
        // ...

        // performs the blit
        VL_glBlitFramebuffer( mSrcRect[0], mSrcRect[1], mSrcRect[2], mSrcRect[3], 
                              mDstRect[0], mDstRect[1], mDstRect[2], mDstRect[3], 
                              mBufferMask,
                              mLinearFilteringEnabled ? GL_LINEAR : GL_NEAREST);
        VL_CHECK_OGL()

        // restore FBOs
        VL_glBindFramebuffer( GL_READ_FRAMEBUFFER_EXT, read_fbo );
        VL_CHECK_OGL()
        VL_glBindFramebuffer( GL_DRAW_FRAMEBUFFER_EXT, draw_fbo );
        VL_CHECK_OGL()
      }
    }

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

    /** The render-target used as source during blitting. */
    void setReadFramebuffer(Framebuffer* fbo) { mReadFramebuffer = fbo; }

    /** The render-target used as source during blitting. */
    const Framebuffer* readFramebuffer() const { return mReadFramebuffer.get(); }

    /** The render-target used as source during blitting. */
    Framebuffer* readFramebuffer() { return mReadFramebuffer.get(); }

    /** The read-buffer of the read-render-target used as pixel source during blitting. */
    void setReadBuffer(EReadDrawBuffer read_buffer) { mReadBuffer = read_buffer; }

    /** The read-buffer of the read-render-target used as pixel source during blitting. */
    EReadDrawBuffer readBuffer() const { return mReadBuffer; }

    /** The render-target used as destination during blitting. */
    void setDrawFramebuffer(Framebuffer* fbo) { mDrawFramebuffer = fbo; }

    /** The render-target used as destination during blitting. */
    const Framebuffer* drawFramebuffer() const { return mDrawFramebuffer.get(); }

    /** The render-target used as destination during blitting. */
    Framebuffer* drawFramebuffer() { return mDrawFramebuffer.get(); }

    void setSrcRect(int x0, int y0, int x1, int y1)
    {
      mSrcRect[0] = x0;
      mSrcRect[1] = y0;
      mSrcRect[2] = x1;
      mSrcRect[3] = y1;
    }

    void setDstRect(int x0, int y0, int x1, int y1)
    {
      mDstRect[0] = x0;
      mDstRect[1] = y0;
      mDstRect[2] = x1;
      mDstRect[3] = y1;
    }

    const int* srcRect() const { return mSrcRect; }
    const int* dstRect() const { return mDstRect; }

    //! takes a bitmask combination of EBufferBits
    void setBufferMask(int buffer_mask) { mBufferMask = buffer_mask; }
    int bufferMask() const { return mBufferMask; }

    void setLinearFilteringEnabled( bool enable_linear_filtering ) { mLinearFilteringEnabled = enable_linear_filtering; }
    bool linearFilteringEnabled() const { return mLinearFilteringEnabled; }

  protected:
    ref<Framebuffer> mReadFramebuffer;
    ref<Framebuffer> mDrawFramebuffer;
    int mSrcRect[4];
    int mDstRect[4];
    int mBufferMask;
    EReadDrawBuffer mReadBuffer;
    bool mLinearFilteringEnabled;
  };
};

#endif
