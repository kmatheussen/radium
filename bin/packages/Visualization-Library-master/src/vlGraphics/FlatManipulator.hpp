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

#ifndef FlatManipulator_INCLUDE_ONCE
#define FlatManipulator_INCLUDE_ONCE

#include <vlCore/Vector3.hpp>
#include <vlCore/Vector4.hpp>
#include <vlGraphics/UIEventListener.hpp>
#include <vlGraphics/Camera.hpp>
#include <vlGraphics/Geometry.hpp>

// mic fixme: clean up this mess

namespace vl
{

  //------------------------------------------------------------------------------
  // FlatManipulator
  //------------------------------------------------------------------------------
  /**
   * This class lets you pan (translate) and zoom a 2D scene using the mouse.
   * The manipulator will perform the actions using only the camera's view matrix,
   * so no other transform is required to be bound.
   * \note Before using a FlatManipulator you have to specify a Camera object
   * using the function setCamera()
   */

class FlatManipulator: public UIEventListener
{
  VL_INSTRUMENT_CLASS(vl::FlatManipulator, UIEventListener)

  public:
    typedef enum { NoMode, TranslationMode, ZoomMode } EManipMode;

    //! Constructor.
    FlatManipulator();

    // --- UIEventListener ---

    virtual void mouseDownEvent(EMouseButton, int x, int y);

    virtual void mouseUpEvent(EMouseButton, int x, int y);

    virtual void mouseMoveEvent(int x, int y);

    virtual void enableEvent(bool enabled);

    virtual void initEvent() {}

    virtual void destroyEvent() {}

    virtual void updateEvent() {}

    virtual void addedListenerEvent(OpenGLContext*) {}

    virtual void removedListenerEvent(OpenGLContext*) {}

    virtual void mouseWheelEvent(int) {}

    virtual void keyPressEvent(unsigned short, EKey) {}

    virtual void keyReleaseEvent(unsigned short, EKey) {}

    virtual void resizeEvent(int, int) {}

    virtual void fileDroppedEvent(const std::vector<String>&) {}

    virtual void visibilityEvent(bool) {}

    // --- user methods ---

    //! The camera through which the manipulator is used.
    void setCamera(Camera* camera);

    //! The camera through which the manipulator is used.
    Camera* camera() { return mCamera.get(); }

    //! Mouse button used to zoom.
    int zoomButton() const { return mZoomButton; }

    //! Mouse button used to zoom.
    void setZoomButton(int mouse_button) { mZoomButton = mouse_button; }

    //! Mouse button used to translate the view.
    int translationButton() const { return mTranslationButton; }

    //! Mouse button used to translate the view.
    void setTranslationButton(int mouse_button) { mTranslationButton = mouse_button; }

    //! Zoom speed multiplicative factor (default = 1).
    float zoomSpeed() const { return mZoomSpeed; }

    //! Zoom speed multiplicative factor (default = 1).
    void setZoomSpeed(float speed) { mZoomSpeed = speed; }

    //! Returns the current manipulator state.
    EManipMode mode() const { return mMode; }

  protected:

    bool mouseInViewport(int mx, int my, int& vx, int& vy);

  protected:
    ref<Camera> mCamera;
    vec2        mMouseStart;
    vec2        mPixelSize;
    EManipMode  mMode;
    int         mTranslationButton;
    int         mZoomButton;
    float       mZoomSpeed;
  };

/////////////////////////////////////////////////////////////////////////////////

/** Makes an orthogonal cross-hair scales (reticle).
    It creates a geometry for a 2D or 3D orthogonal scales,
    with evenly-spaced big ticks at every 10 steps, mid-ticks at
    every 5 steps and ticks at every step.
    By passing true to any X, Y or Z params, one can configure
    which axes will be generated.

    The number of ticks per arm (half-axis), the step between ticks
    and the tick size are customizable.
*/

vl::ref<vl::Geometry> makeScales(bool X = true,
                                 bool Y = true,
                                 bool Z = true,
                                 int numArmTicks = 50,
                                 float mmStep = 10,
                                 float mmTickSize = 4,
                                 vl::fvec4 color = vl::fvec4(1,1,1,1));
}

#endif
