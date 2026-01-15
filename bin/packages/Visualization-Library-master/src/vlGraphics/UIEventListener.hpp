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

#ifndef EventListener_INCLUDE_ONCE
#define EventListener_INCLUDE_ONCE

#include <vlGraphics/link_config.hpp>
#include <vlCore/String.hpp>

namespace vl
{
  class OpenGLContext;
  //-----------------------------------------------------------------------------
  // UIEventListener
  //-----------------------------------------------------------------------------
  /** The UIEventListener class listens to the events emitted by an OpenGLContext.
  \remarks
  - Qt amongst others does not distinguish between left and right \a alt, \a shift and \a control keys, which means that you
	  won't receive messages like Key_LeftCtrl, Key_RightCtrl, Key_LeftAlt, Key_RightAlt, Key_LeftShift and Key_RightShift
	  but only the more general Key_Ctrl, Key_Alt and Key_Shift messages.
  - SDL supports Unicode only partially at the moment, which means that you will receive Unicode codes only for key press events and not
	  for release events.
  - keyPressed() returns the correct state only for the keys whose messages have been received by the OpenGLContext. This means that for
	  example if a key was pressed when the OpenGLContext did not have the keyboard focus, the function keyPressed() will wrongly
	  report a \a released state for that key. If a key release event is not sent to the OpenGLContext because generated when the
	  OpenGLContext did not have the keyboard focus, the function keyPressed() will wrongly report a \a pressed state for that key. */
  class VLGRAPHICS_EXPORT UIEventListener: public Object
  {
    VL_INSTRUMENT_ABSTRACT_CLASS(vl::UIEventListener, Object)

    friend class OpenGLContext;

  public:
    /** Constructor. */
    UIEventListener(): mOpenGLContext(NULL), mEnabled(true) 
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }

    /** Event generated when the bound OpenGLContext bocomes initialized or when the event listener is bound to an initialized OpenGLContext. */
    virtual void initEvent() = 0;

    /** Event generated right before the bound OpenGLContext is destroyed. */
    virtual void destroyEvent() = 0;

    /** Event generated when the bound OpenGLContext does not have any other message to process 
        and OpenGLContext::continuousUpdate() is set to \p true or somebody calls OpenGLContext::update(). */
    virtual void updateEvent() = 0;
    
    /** Event generated whenever setEnabled() is called. */
    virtual void enableEvent(bool enabled) = 0;

    /** Event generated whenever a listener is bound to an OpenGLContext context. */
    virtual void addedListenerEvent(OpenGLContext*) = 0;

    /** Event generated whenever a listener is unbound from an OpenGLContext context. */
    virtual void removedListenerEvent(OpenGLContext*) = 0;

    /** Event generated when the mouse moves. */
    virtual void mouseMoveEvent(int x, int y) = 0;

    /** Event generated when one of the mouse buttons is released. */
    virtual void mouseUpEvent(EMouseButton button, int x, int y) = 0;

    /** Event generated when one of the mouse buttons is pressed. */
    virtual void mouseDownEvent(EMouseButton button, int x, int y) = 0;

    /** Event generated when the mouse wheel rotated. */
    virtual void mouseWheelEvent(int n) = 0;

    /** Event generated when a key is pressed. */
    virtual void keyPressEvent(unsigned short unicode_ch, EKey key) = 0;

    /** Event generated when a key is released. */
    virtual void keyReleaseEvent(unsigned short unicode_ch, EKey key) = 0;

    /** Event generated when the bound OpenGLContext is resized. */
    virtual void resizeEvent(int x, int y) = 0;

    /** Event generated when one or more files are dropped on the bound OpenGLContext's area. */
    virtual void fileDroppedEvent(const std::vector<String>& files) = 0;

    /** Event generated when the bound OpenGLContext is shown or hidden. */
    virtual void visibilityEvent(bool visible) = 0;

    /** Enables or disables a UIEventListener.
        \note When an UIEventListener is enabled or disabled its enableEvent(bool enabled) method is called. */
    virtual void setEnabled(bool enabled) { if (mEnabled != enabled) { mEnabled = enabled; enableEvent(enabled); } }

    /** Returns whether the UIEventListener is currently enabled or not. */
    bool isEnabled() const { return mEnabled; }

    /** Returns the OpenGLContext to which this UIEventListener is bound or NULL if no context is bound. */
    OpenGLContext* openglContext();

    /** Returns the OpenGLContext to which this UIEventListener is bound or NULL if no context is bound. */
    const OpenGLContext* openglContext() const;

  private:
    OpenGLContext* mOpenGLContext;
    bool mEnabled;
  };
}

#endif
