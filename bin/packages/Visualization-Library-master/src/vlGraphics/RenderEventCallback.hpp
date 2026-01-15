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

#ifndef RenderEventCallback_INCLUDE_ONCE
#define RenderEventCallback_INCLUDE_ONCE

#include <vlCore/Object.hpp>
#include <vlCore/vlnamespace.hpp>

namespace vl
{
  class RenderingAbstract;
  class RendererAbstract;
  //-----------------------------------------------------------------------------
  // RenderEventCallback
  //-----------------------------------------------------------------------------
  /**
   * An abstract class used to react to rendering events. You can bind a callback to both Rendering and Renderer.
   * The callbacks are executed in the same order in which they appear in the containing collection.
   * \sa 
   * - Rendering::renderEventCallbacks() 
   * - Renderer::renderEventCallbacks().
   * - vl::RenderingAbstract, vl::Rendering
   * - vl::RendererAbstract, vl::Renderer */
  class RenderEventCallback: public Object
  {
    VL_INSTRUMENT_ABSTRACT_CLASS(vl::RenderEventCallback, Object)

  public:
    RenderEventCallback(): mRemoveAfterCall(false), mEnabled(true)
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }

    /** Reimplement to react to this event.
      * \return \p true if the callback reacted to the given event. */
    virtual bool onRenderingStarted(const RenderingAbstract*) = 0;

    /** Reimplement to react to this event.
      * \return \p true if the callback reacted to the given event. */
    virtual bool onRenderingFinished(const RenderingAbstract*) = 0;

    /** Reimplement to react to this event.
      * \return \p true if the callback reacted to the given event. */
    virtual bool onRendererStarted(const RendererAbstract*) = 0;

    /** Reimplement to react to this event.
      * \return \p true if the callback reacted to the given event. */
    virtual bool onRendererFinished(const RendererAbstract*) = 0;

    //! Defines if the callback shall be removed after being executed.
    //! Note that the callback is removed only if the renderingCallback() method returns \p true.
    void setRemoveAfterCall(bool remove) { mRemoveAfterCall = remove; }

    //! Defines if the callback shall be removed after being executed.
    //! Note that the callback is removed only if the renderingCallback() method returns \p true.
    bool removeAfterCall() const { return mRemoveAfterCall; }

    /** Enabled/disabled callback. Disabled callbacks won't be notified of any event. */
    void setEnabled(bool enabled) { mEnabled = enabled; }

    /** Whether the callback is enabled or not. Disabled callbacks won't be notified of any event. */
    bool isEnabled() const { return mEnabled; }

  protected:
    bool mRemoveAfterCall;
    bool mEnabled;
  };
}

#endif
