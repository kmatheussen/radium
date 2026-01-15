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

#ifndef GhostCameraManipulator_INCLUDE_ONCE
#define GhostCameraManipulator_INCLUDE_ONCE

#include <vlGraphics/OpenGLContext.hpp>

namespace vl
{
  class Camera;
  //------------------------------------------------------------------------------
  // GhostCameraManipulator
  //------------------------------------------------------------------------------
  /** 
   * The GhostCameraManipulator class is an UIEventListener that controls the position and orientation of a Camera.
   * Using the GhostCameraManipulator class the user can freely fly around in the scene as if it was a ghost.
   * Default key bindings:
   * - Forward = \p Key_W
   * - Backward = \p Key_S
   * - Left = \p Key_A
   * - Right = \p Key_D
   * - Up = \p Key_W + \p Key_Shift
   * - Down = \p Key_S + \p Key_Shift
   * \sa TrackballManipulator
   */
  class VLGRAPHICS_EXPORT GhostCameraManipulator: public UIEventListener
  {
    VL_INSTRUMENT_CLASS(vl::GhostCameraManipulator, UIEventListener)

  public:
    /** Constructor. */
    GhostCameraManipulator();

    // ---  UIEventListener ---

    virtual void mouseMoveEvent(int x, int y);

    void enableEvent(bool enabled);

    virtual void updateEvent();

    virtual void initEvent() {}

    virtual void destroyEvent() {}

    virtual void addedListenerEvent(OpenGLContext*) {}

    virtual void removedListenerEvent(OpenGLContext*) {}

    virtual void mouseUpEvent(EMouseButton, int, int) {}

    virtual void mouseDownEvent(EMouseButton, int, int) {}

    virtual void mouseWheelEvent(int) {}

    virtual void keyPressEvent(unsigned short, EKey) {}

    virtual void keyReleaseEvent(unsigned short, EKey) {}

    virtual void resizeEvent(int, int) {}

    virtual void fileDroppedEvent(const std::vector<String>&) {}

    virtual void visibilityEvent(bool) {}

    // --- --- ---

    /** The camera to be manipulated. */
    void setCamera(Camera* camera);

    /** The camera to be manipulated. */
    Camera* camera();
    
    /** The camera to be manipulated. */
    const Camera* camera() const;

    /** Key bindings to move forward (default = Key_W). */
    void setKeysForward(EKey key, EKey modifier = Key_None)  { mKeysForward[0] = key; mKeysForward[1] = modifier; };
    
    /** Key bindings to move backward (default = Key_S). */
    void setKeysBackward(EKey key, EKey modifier = Key_None) { mKeysBackward[0] = key; mKeysBackward[1] = modifier; };
    
    /** Key bindings to move left (default = Key_A). */
    void setKeysLeft(EKey key, EKey modifier = Key_None)     { mKeysLeft[0] = key; mKeysLeft[1] = modifier; };
    
    /** Key bindings to move right (default = Key_D). */
    void setKeysRight(EKey key, EKey modifier = Key_None)    { mKeysRight[0] = key; mKeysRight[1] = modifier; };
    
    /** Key bindings to move up (default = Key_W + Key_Shift). */
    void setKeysUp(EKey key, EKey modifier = Key_None)       { mKeysUp[0] = key; mKeysUp[1] = modifier; };
    
    /** Key bindings to move down (default = Key_S + Key_Shift). */
    void setKeysDown(EKey key, EKey modifier = Key_None)     { mKeysDown[0] = key; mKeysDown[1] = modifier; };

    /** The camera rotation speed (default = 0.5). */
    void setRotationSpeed(real speed) { mRotationSpeed = speed; }
    
    /** The camera rotation speed (default = 0.5). */
    real rotationSpeed() const { return mRotationSpeed; }
    
    /** The camera translation speed (default = 50). */
    void setMovementSpeed(real speed) { mMovementSpeed = speed; }

    /** The camera translation speed (default = 50). */
    real movementSpeed() const { return mMovementSpeed; }

  protected:
    void setPosition(vec3 position) { mPosition = position; }

    const vec3& position() const { return mPosition; }

    void setXDegrees(real degree) { mXDegrees = degree; }

    real xDegrees() { return mXDegrees; }

    void setYDegrees(real degree) { mYDegrees = degree; }

    real yDegrees() { return mYDegrees; }

  protected:
    ref<Camera> mCamera;
    vec3 mPosition;
    real mLastTime;
    real mRotationSpeed;
    real mMovementSpeed;
    real mXDegrees;
    real mYDegrees;
    EKey mKeysForward[2];
    EKey mKeysBackward[2];
    EKey mKeysUp[2];
    EKey mKeysDown[2];
    EKey mKeysLeft[2];
    EKey mKeysRight[2];
  };
}

#endif
