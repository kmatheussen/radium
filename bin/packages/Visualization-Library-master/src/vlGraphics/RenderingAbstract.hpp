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

#ifndef RenderingAbstract_INCLUDE_ONCE
#define RenderingAbstract_INCLUDE_ONCE

#include <vlCore/Collection.hpp>
#include <vlCore/vlnamespace.hpp>
#include <vlGraphics/link_config.hpp>

namespace vl
{
  class RenderEventCallback;

  //! The RenderingAbstract class is the base of all the rendering related sub-classes.
  class VLGRAPHICS_EXPORT RenderingAbstract: public Object
  {
    VL_INSTRUMENT_ABSTRACT_CLASS(vl::RenderingAbstract, Object)

  public:
    //! Constructor.
    RenderingAbstract();

    /** Executes the rendering. */
    virtual void render() = 0;

    RenderingAbstract& operator=(const RenderingAbstract& other);

    /** Calls the RenderEventCallback::onRenderingStarted() method of all the active callback objects.*/
    void dispatchOnRenderingStarted();

    /** Calls the RenderEventCallback::onRenderingFinished() method of all the active callback objects.*/
    void dispatchOnRenderingFinished();

    //! Returns the list of RenderEventCallback objects registered to the onRenderingStarted() event notification.
    Collection<RenderEventCallback>* onStartedCallbacks() { return mOnStartedCallbacks.get(); }

    //! Returns the list of RenderEventCallback objects registered to the onRenderingStarted() event notification.
    const Collection<RenderEventCallback>* onStartedCallbacks() const { return mOnStartedCallbacks.get(); }

    //! Returns the list of RenderEventCallback objects registered to the onRenderingFinished() event notification.
    Collection<RenderEventCallback>* onFinishedCallbacks() { return mOnFinishedCallbacks.get(); }

    //! Returns the list of RenderEventCallback objects registered to the onRenderingFinished() event notification.
    const Collection<RenderEventCallback>* onFinishedCallbacks() const { return mOnFinishedCallbacks.get(); }

    //! The enable mask of the Rendering, used to define wheter the rendering is enabled or not, and which objects should be rendered.
    //! @sa
    //! vl::Actor::setEnableMask()
    void setEnableMask(unsigned int mask) { mEnableMask = mask; }
    
    //! The enable mask of the Rendering, used to define wheter the rendering is enabled or not, and which objects should be rendered.
    unsigned int enableMask() const { return mEnableMask; }
    
    //! Utility function equivalent to \p "(mask & mEnableMask) != 0".
    bool isEnabled(unsigned int mask) { return (mask & mEnableMask) != 0; }

    //! The update time of the current rendering frame.
    void setFrameClock(real cur_time) { mFrameClock = cur_time; }

    //! The update time of the current rendering frame.
    real frameClock() const { return mFrameClock; }

  protected:
    ref< Collection<RenderEventCallback> > mOnStartedCallbacks;
    ref< Collection<RenderEventCallback> > mOnFinishedCallbacks;
    real mFrameClock;
    unsigned int mEnableMask;
  };
}

#endif
