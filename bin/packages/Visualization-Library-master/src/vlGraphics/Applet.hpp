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

#ifndef Applet_INCLUDE_ONCE
#define Applet_INCLUDE_ONCE

#include <vlGraphics/UIEventListener.hpp>
#include <vlGraphics/TrackballManipulator.hpp>
#include <vlGraphics/GhostCameraManipulator.hpp>
#include <vlGraphics/SceneManagerActorTree.hpp>
#include <vlGraphics/Rendering.hpp>
#include <vlGraphics/ReadPixels.hpp>

namespace vl
{
//-----------------------------------------------------------------------------
// Applet
//-----------------------------------------------------------------------------
  /** 
   * The Applet class is an utilitly UIEventListener that features a ghost manipulator, 
   * trackball manipulator, an FPS counter and a simple rendering pipeline. 
   * 
   * Default key bindings:
   * - Key_Escape: calls openglContext()->quitApplication()
   * - Key_T: enables the TrackballManipulator.
   * - Key_F: enables the GhostCameraManipulator (fly mode).
   * - Key_F1: toggles fullscreen mode if supported.
   * - Key_F5: saves a screenshot of the current OpenGL window. 
   * - Key_C: toggles the continuous update of the OpenGL window (see also OpenGLContext::setContinuousUpdate()).
   * - Key_U: updates the OpenGL window content by calling openglContext()->update(). 
   */
  class VLGRAPHICS_EXPORT Applet: public UIEventListener
  {
    VL_INSTRUMENT_CLASS(vl::Applet, UIEventListener)

  public:
    /** Constructor */
    Applet();

    /** 
     * Initializes the default rendering (with Rendering), the default scene manager (with SceneManagerActorTree) 
     * and camera manipulators (GhostCameraManipulator and TrackballManipulator). 
     */
    void initialize();

    // --- UIEventListener ---

    virtual void addedListenerEvent(OpenGLContext* openglContext);

    virtual void removedListenerEvent(OpenGLContext*);

    virtual void keyReleaseEvent(unsigned short, EKey key);

    virtual void destroyEvent();

    virtual void updateEvent();

    virtual void resizeEvent(int, int);

    virtual void initEvent() {}
    
    virtual void enableEvent(bool) {}

    virtual void mouseMoveEvent(int, int) {}

    virtual void mouseUpEvent(EMouseButton, int, int) {}

    virtual void mouseDownEvent(EMouseButton, int, int) {}

    virtual void mouseWheelEvent(int) {}

    virtual void keyPressEvent(unsigned short, EKey) {}

    virtual void fileDroppedEvent(const std::vector<String>&) {}

    virtual void visibilityEvent(bool) {}

    // --- --- ---

    /** The rendering used by the Applet, by default a Rendering. */
    RenderingAbstract* rendering() { return mRendering.get(); }
    
    /** The rendering used by the Applet, by default a Rendering. */
    const RenderingAbstract* rendering() const { return mRendering.get(); }
    
    /** Sets the rendering used by the Applet, by default a Rendering. */
    void setRendering(RenderingAbstract* rendering) { mRendering = rendering; }

    /** The scene manager used by the default rendering. */
    SceneManagerActorTree* sceneManager() { return mSceneManagerActorTree.get(); }

    /** The scene manager used by the default rendering. */
    const SceneManagerActorTree* sceneManager() const { return mSceneManagerActorTree.get(); }

    /** GhostCameraManipulator used by the applet, activated by the "F" key. */
    void setGhostCameraManipulator(GhostCameraManipulator* gcm) { mFly = gcm; }

    /** GhostCameraManipulator used by the applet, activated by the "F" key. */
    GhostCameraManipulator* ghostCameraManipulator() { return mFly.get(); }
    const GhostCameraManipulator* ghostCameraManipulator() const { return mFly.get(); }

    /** TrackballManipulator used by the applet, activated by the "T" key. */
    void setTrackball(TrackballManipulator* trackball) { mTrackball = trackball; }

    /** TrackballManipulator used by the applet, activated by the "T" key. */
    TrackballManipulator* trackball() { return mTrackball.get(); }
    const TrackballManipulator* trackball() const { return mTrackball.get(); }

    /** Current average frames per second (updated every 500ms). */
    double fps() const { return mFPS; }

    /** 
     * Override this to update the content of your scene. 
     * Called by updateEvent() right before rendering()->render() and swapping opengl front/back buffers. 
     * \note Since updateScene() is called by updateEvent() this function is called only if somebody
     * requests a OpenGLContext::update() or if OpenGLContext::continuousUpdate() is set to \p true. 
     */
	  virtual void updateScene() {}

    /** Sets the applet name, used for the window title and for naming screenshots. */
    void setAppletName(const String& app_name) { mAppletName = app_name; } 

    /** The applet name, used for the window title and for naming screenshots. */
    const String& appletName() const { return mAppletName; }

  protected:
    void bindManipulators(Camera* camera);

  private:
    ref<RenderingAbstract> mRendering;
    ref<GhostCameraManipulator> mFly;
    ref<TrackballManipulator> mTrackball;
    ref<SceneManagerActorTree> mSceneManagerActorTree;
    ref<ReadPixels> mReadPixels;
    String mAppletName;
    double mStartTime;
    double mFPS;
    int mFrameCount;
  };
}

#endif
