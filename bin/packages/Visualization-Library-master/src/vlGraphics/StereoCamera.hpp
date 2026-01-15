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

#ifndef StereoCamera_INCLUDE_ONCE
#define StereoCamera_INCLUDE_ONCE

#include <vlGraphics/Camera.hpp>

namespace vl
{
  /** Utility class to setup a pair of left/right cameras for stereo rendering. 
      \par Usage
      - Assign the mono camera representing the point of view of your observer using the setMonoCamera() method.
      - Assign the left and right cameras representing the left and right eyes with the setLeftCamera()/setRightCamera() methods.
      - Set the desired convergence and eye separation using setConvergence() and setEyeSeparation().
      - Call updateLeftRightCameras() whenever the mono camera or viewport changes.

      \sa App_Stereo.cpp for a basic example of how to setup stereo rendering using anaglyphs. */
  class StereoCamera: public Object
  {
    VL_INSTRUMENT_CLASS(vl::StereoCamera, Object)

  public:
    StereoCamera()
    {
      mConvergence = 20;
      mEyeSeparation = 1;
    }

    /** Distance of the convergence plane from the camera. 
        The points laying on the convergence plane look the same from both the left and right camera. */
    void setConvergence( float convergence ) { mConvergence = convergence; }
    /** Distance of the convergence plane from the camera. 
        The points laying on the convergence plane look the same from both the left and right camera. */
    float convergence() const { return mConvergence; }

    /** The distance between the center of the two eyes. */
    void setEyeSeparation( float eye_separation ) { mEyeSeparation = eye_separation; }
    /** The distance between the center of the two eyes. */
    float eyeSeparation() const { return mEyeSeparation; }

    /** The Camera used to drive the left and right cameras. 
        The mono camera viewport will be automatically used by the left and right cameras as well. */
    void setMonoCamera(Camera* camera) { mMonoCamera = camera; }
    /** The Camera used to drive the left and right cameras. 
        The mono camera viewport will be automatically used by the left and right cameras as well. */
    Camera* monoCamera() { return mMonoCamera.get(); }
    /** The Camera used to drive the left and right cameras. 
        The mono camera viewport will be automatically used by the left and right cameras as well. */
    const Camera* monoCamera() const { return mMonoCamera.get(); }

    /** The Camera representing the left eye. */
    void setLeftCamera(Camera* camera) { mLeftCamera = camera; }
    /** The Camera representing the left eye. */
    Camera* leftCamera() { return mLeftCamera.get(); }
    /** The Camera representing the left eye. */
    const Camera* leftCamera() const { return mLeftCamera.get(); }

    /** The Camera representing the right eye. */
    void setRightCamera(Camera* camera) { mRightCamera = camera; }
    /** The Camera representing the right eye. */
    Camera* rigthCamera() { return mRightCamera.get(); }
    /** The Camera representing the right eye. */
    const Camera* rightCamera() const { return mRightCamera.get(); }

    /** Updates the left and right cameras based on the mono camera view matrix and viewport. */
    void updateLeftRightCameras()
    {
      mLeftCamera->setViewport( mMonoCamera->viewport() );
      mRightCamera->setViewport( mMonoCamera->viewport() );

      float aspect_ratio = (float)mMonoCamera->viewport()->width()/mMonoCamera->viewport()->height();
      float near_clip = mMonoCamera->nearPlane();
      float far_clip  = mMonoCamera->farPlane();
      float radians = mMonoCamera->fov()/2*fDEG_TO_RAD;
      float wd2 = near_clip * tan(radians);
      float ndfl = near_clip / mConvergence;
      float top, bottom, left, right;
      top    =   wd2;
      bottom = - wd2;

      left   = - aspect_ratio * wd2 - mEyeSeparation/2 * ndfl;
      right  =   aspect_ratio * wd2 - mEyeSeparation/2 * ndfl;
      mLeftCamera->setProjectionFrustum(left, right, bottom, top, near_clip, far_clip);
      mLeftCamera->setViewMatrix( mat4::getTranslation(-mEyeSeparation/2, 0, 0)*mMonoCamera->viewMatrix() );

      left  = - aspect_ratio * wd2 + mEyeSeparation/2 * ndfl;
      right =   aspect_ratio * wd2 + mEyeSeparation/2 * ndfl;
      mRightCamera->setProjectionFrustum(left, right, bottom, top, near_clip, far_clip);
      mRightCamera->setViewMatrix( mat4::getTranslation(+mEyeSeparation/2, 0, 0)*mMonoCamera->viewMatrix() );
    }

  private:
    ref<Camera> mMonoCamera;
    ref<Camera> mLeftCamera;
    ref<Camera> mRightCamera;
    float mConvergence;
    float mEyeSeparation;
  };
}

#endif
