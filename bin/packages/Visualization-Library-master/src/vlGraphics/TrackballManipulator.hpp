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

#ifndef TrackballManipulator_INCLUDE_ONCE
#define TrackballManipulator_INCLUDE_ONCE

#include <vlGraphics/UIEventListener.hpp>
#include <vlGraphics/Camera.hpp>
#include <vlCore/Vector3.hpp>

namespace vl
{
  class Transform;
  class Rendering;
  class SceneManager;
  class ActorCollection;

  //------------------------------------------------------------------------------
  // TrackballManipulator
  //------------------------------------------------------------------------------
  /**
   * This class lets you rotate a Camera or a Transform node using a virtual trackball.
   * If you set a Transform node to manipulate, using the function setTransform(), the trackball
   * will manipulate the given Transform (rotation only, panning and zooming will always affect 
   * the bound Camera). If no Transform is specified or a NULL one is passed to the function 
   * setTransform() then the trackball will manipulate the current camera.
   * \note In any case, before using a TrackballManipulator you have to specify a Camera object 
   * using the function setCamera().
   * \note The Transform is expected to contain only rotation and translation information. 
   * Other transformations like shearing, scaling, projection, and so on can produce unspecified results.
   * \sa GhostCameraManipulator
   */
  class VLGRAPHICS_EXPORT TrackballManipulator: public UIEventListener
  {
    VL_INSTRUMENT_CLASS(vl::TrackballManipulator, UIEventListener)

  public:
    typedef enum { NoMode, RotationMode, TranslationMode, ZoomMode } ETrackballMode;
  
  public:
    //! Constructor.
    TrackballManipulator(): mMode(NoMode),
      mRotationButton(LeftButton), mTranslationButton(MiddleButton), mZoomButton(RightButton), 
      mRotationSpeed(1.0f),        mTranslationSpeed(1.0f),          mZoomSpeed(1.0f)
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }

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

    //! The camera through which the trackball manipulator is used.
    void setCamera(Camera* camera) { mCamera = camera; }

    //! The camera through which the trackball manipulator is used.
    Camera* camera() { return mCamera.get(); }

    //! The center point around which the camera will rotate
    void setPivot(vec3 pivot) { mPivot = pivot; }

    //! The center point around which the camera will rotate
    vec3 pivot() const { return mPivot; }

    //! If NULL the trackball will manipulate the camera transform, if non NULL the trackball will manipulate the specified transform.
    void setTransform(Transform* tr) { mTransform = tr; }

    //! If NULL the trackball will manipulate the camera transform, if non NULL the trackball will manipulate the specified transform.
    Transform* transform() { return mTransform.get(); }

    //! Mouse button used to rotate.
    int rotationButton() const { return mRotationButton; }

    //! Mouse button used to rotate.
    void setRotationButton(int mouse_button) { mRotationButton = mouse_button; }

    //! Mouse button used to zoom.
    int zoomButton() const { return mZoomButton; }

    //! Mouse button used to zoom.
    void setZoomButton(int mouse_button) { mZoomButton = mouse_button; }

    //! Mouse button used to translate the view.
    int translationButton() const { return mTranslationButton; }

    //! Mouse button used to translate the view.
    void setTranslationButton(int mouse_button) { mTranslationButton = mouse_button; }

    //! Rotation speed multiplicative factor (default = 1).
    float rotationSpeed() const { return mRotationSpeed; }
    
    //! Rotation speed multiplicative factor (default = 1).
    void setRotationSpeed(float speed) { mRotationSpeed = speed; }

    //! Zoom speed multiplicative factor (default = 1).
    float zoomSpeed() const { return mZoomSpeed; }

    //! Zoom speed multiplicative factor (default = 1).
    void setZoomSpeed(float speed) { mZoomSpeed = speed; }

    //! Translation speed multiplicative factor (default = 1).
    float translationSpeed() const { return mTranslationSpeed; }

    //! Translation speed multiplicative factor (default = 1).
    void setTranslationSpeed(float speed) { mTranslationSpeed = speed; }

    //! Adjusts the camera position in order to nicely see the scene. It also position the rotation pivot to the center of the AABB. See also Camera::adjustView().
    void adjustView(const AABB& aabb, const vec3& dir, const vec3& up, real bias=1.0f);

    //! Adjusts the camera position in order to nicely see the scene. It also position the rotation pivot to the center of the AABB containing the Actor[s]. See also Camera::adjustView().
    void adjustView(ActorCollection& actors, const vec3& dir, const vec3& up, real bias=1.0f);

    //! Adjusts the camera position in order to nicely see the scene. It also position the rotation pivot to the center of the AABB containing the given scene manager. See also Camera::adjustView().
    void adjustView(SceneManager* scene, const vec3& dir, const vec3& up, real bias=1.0f);

    //! Adjusts the camera position in order to nicely see the scene. It also position the rotation pivot to the center of the AABB containing all the scene managers part of the given rendering. See also Camera::adjustView().
    void adjustView(Rendering* rendering, const vec3& dir, const vec3& up, real bias=1.0f);

    // --- Advanced methods ---

    mat4 trackballRotation(int x, int y);

    vec3 computeVector(int x, int y);

    //! Returns the current trackball manipulator state.
    ETrackballMode mode() const { return mMode; }

  protected:
    ref<Camera> mCamera;
    ivec2 mMouseStart;
    mat4 mStartMatrix;
    vec3 mPivot;
    vec3 mStartCameraPos;
    vec3 mStartPivot;
    ref<Transform> mTransform;
    ETrackballMode mMode;
    int mRotationButton;
    int mTranslationButton;
    int mZoomButton;
    float mRotationSpeed;
    float mTranslationSpeed;
    float mZoomSpeed;
  };
}

#endif
