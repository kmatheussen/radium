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

#ifndef RendererAbstract_INCLUDE_ONCE
#define RendererAbstract_INCLUDE_ONCE

#include <vlCore/Collection.hpp>
#include <vlGraphics/RenderEventCallback.hpp>
#include <vlGraphics/link_config.hpp>

namespace vl
{
  class Camera;
  class RenderQueue;
  class Framebuffer;

  //-----------------------------------------------------------------------------
  // RendererAbstract
  //-----------------------------------------------------------------------------
  /** Base class providing all the basic funtionalities of a Renderer. 

  */
  class VLGRAPHICS_EXPORT RendererAbstract: public Object
  {
    VL_INSTRUMENT_ABSTRACT_CLASS(vl::RendererAbstract, Object)

  public:
    RendererAbstract()
    {
      VL_DEBUG_SET_OBJECT_NAME()
      mOnStartedCallbacks  = new Collection<RenderEventCallback>;
      mOnFinishedCallbacks = new Collection<RenderEventCallback>;
      mClearFlags = CF_CLEAR_COLOR_DEPTH;
      mEnableMask = 0xFFFFFFFF;
      mRenderTick = 0;
      mFrameClock = 0;
    }

    RendererAbstract& operator=(const RendererAbstract& other)
    {
      *mOnStartedCallbacks  = *other.mOnStartedCallbacks;
      *mOnFinishedCallbacks = *other.mOnFinishedCallbacks;
      mClearFlags = other.mClearFlags;
      mEnableMask = other.mEnableMask;
      /* mRenderTick = other.mRenderTick; */ // render-tick remains local
      /* mFrameClock = other.mFrameClock; */ // update time remains local
      return *this;
    }

    /** Takes as input the render queue to render and returns a possibly filtered render queue for further processing. 
      * Renderer's implementation of this function always returns \p in_render_queue. */
    virtual const RenderQueue* render(const RenderQueue* in_render_queue, Camera* camera, real frame_clock) = 0;

    /** The Framebuffer on which the rendering is performed. */
    virtual const Framebuffer* framebuffer() const = 0;

    /** The Framebuffer on which the rendering is performed. */
    virtual Framebuffer* framebuffer() = 0;

    /** Dispatches the onRendererStarted() event to the registered RenderEventCallback objects. */
    void dispatchOnRendererStarted()
    {
      Collection<RenderEventCallback>& cb = *mOnStartedCallbacks;
      for(int i=0; i<cb.size(); ++i)
      {
        if ( cb[i]->isEnabled() && cb[i]->onRendererStarted(this) && cb[i]->removeAfterCall() )
        {
          onStartedCallbacks()->eraseAt( i );
          --i;
        }
      }
    }

    /** Dispatches the onRendererFinished() event to the registered RenderEventCallback objects. */
    void dispatchOnRendererFinished()
    {
      Collection<RenderEventCallback>& cb = *mOnFinishedCallbacks;
      for(int i=0; i<cb.size(); ++i)
      {
        if ( cb[i]->isEnabled() && cb[i]->onRendererFinished(this) && cb[i]->removeAfterCall() )
        {
          onFinishedCallbacks()->eraseAt( i );
          --i;
        }
      }
    }

    //! Returns the list of RenderEventCallback objects registered to onRendererFinished() event notification.
    Collection<RenderEventCallback>* onFinishedCallbacks() { return mOnFinishedCallbacks.get(); }

    //! Returns the list of RenderEventCallback objects registered to onRendererFinished() event notification.
    const Collection<RenderEventCallback>* onFinishedCallbacks() const { return mOnFinishedCallbacks.get(); }

    //! Returns the list of RenderEventCallback objects registered to onRendererStarted() event notification.
    Collection<RenderEventCallback>* onStartedCallbacks() { return mOnStartedCallbacks.get(); }

    //! Returns the list of RenderEventCallback objects registered to onRendererStarted() event notification.
    const Collection<RenderEventCallback>* onStartedCallbacks() const { return mOnStartedCallbacks.get(); }

    /** The current render tick number, equivalent to the number or calls made to the render() method. */
    unsigned long renderTick() const { return mRenderTick; }

    /** Increments the rendering tick count. */
    void incrementRenderTick() { ++mRenderTick; }

    /** The clear flags used to clear the viewport. */
    void setClearFlags(EClearFlags clear_flags) { mClearFlags = clear_flags; }

    /** The clear flags used to clear the viewport. */
    EClearFlags clearFlags() const { return mClearFlags; }

    /** Enable mask used to enable/disable the rendering of matching Actors. */
    void setEnableMask(unsigned int mask) { mEnableMask = mask; }

    /** Enable mask used to enable/disable the rendering of matching Actors. */
    unsigned int enableMask() const { return mEnableMask; }

    /** The current rendring frame time. vl::Renderer passes this value to ActorEventCallback::onActorRenderStarted() */
    void setFrameClock(real t) { mFrameClock = t; }

    /** The current rendring frame time. vl::Renderer passes this value to ActorEventCallback::onActorRenderStarted() */
    real frameClock() const { return mFrameClock; }

  protected:
    ref< Collection<RenderEventCallback> > mOnFinishedCallbacks;
    ref< Collection<RenderEventCallback> > mOnStartedCallbacks;
    unsigned long mRenderTick;
    unsigned int mEnableMask;
    EClearFlags mClearFlags;
    real mFrameClock;
  };
  //------------------------------------------------------------------------------
}

#endif
