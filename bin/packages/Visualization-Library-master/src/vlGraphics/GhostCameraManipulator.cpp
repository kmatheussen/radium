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

#include <vlGraphics/GhostCameraManipulator.hpp>
#include <vlCore/Time.hpp>
#include <vlGraphics/Camera.hpp>

using namespace vl;

//-----------------------------------------------------------------------------
// GhostCameraManipulator
//-----------------------------------------------------------------------------
GhostCameraManipulator::GhostCameraManipulator()
{
  VL_DEBUG_SET_OBJECT_NAME()
  mRotationSpeed = 0.5;
  mMovementSpeed = 50.0f;
  mXDegrees = 0;
  mYDegrees = 0;
  mLastTime = 0;
  mPosition = vec3(0,0,0);

  setKeysForward(Key_W);
  setKeysBackward(Key_S);
  setKeysLeft(Key_A);
  setKeysRight(Key_D);
  setKeysUp(Key_W, Key_Shift);
  setKeysDown(Key_S, Key_Shift);
}
//-----------------------------------------------------------------------------
void GhostCameraManipulator::mouseMoveEvent(int x, int y)
{
  if ( camera() == NULL )
    return;

  VL_CHECK(openglContext());

  int cx = (int)camera()->viewport()->center().x();
  int cy = openglContext()->framebuffer()->height() - camera()->viewport()->height()/2 - camera()->viewport()->y();
  mXDegrees -= (y - cy) * mRotationSpeed;
  mYDegrees -= (x - cx) * mRotationSpeed;
  openglContext()->ignoreNextMouseMoveEvent();
  openglContext()->setMousePosition(cx, cy);
}
//-----------------------------------------------------------------------------
void GhostCameraManipulator::updateEvent()
{
  if (camera() == NULL)
    return;

  if (mLastTime == 0)
  {
    mLastTime = Time::currentTime();
    return;
  }
  real dt = Time::currentTime() - mLastTime;
  mLastTime = Time::currentTime();

  mat4 m = mat4::getTranslation(mPosition);
  m *= mat4::getRotation( mYDegrees, vec3(0,1,0), mXDegrees, vec3(1,0,0) );
  camera()->setModelingMatrix(m);

  vec3 direction;
  bool okmodifier;
  bool modifier = openglContext()->isKeyPressed( Key_Alt ) || openglContext()->isKeyPressed( Key_Ctrl ) || openglContext()->isKeyPressed( Key_Shift );

  okmodifier = (mKeysLeft[1] == Key_None) ? !modifier : openglContext()->isKeyPressed( mKeysLeft[1] );
  if ( openglContext()->isKeyPressed(mKeysLeft[0]) && okmodifier )
    direction.x() = -1;

  okmodifier = (mKeysRight[1] == Key_None) ? !modifier : openglContext()->isKeyPressed(mKeysRight[1]);
  if ( openglContext()->isKeyPressed(mKeysRight[0]) && okmodifier )
    direction.x() = +1;

  okmodifier = (mKeysBackward[1] == Key_None) ? !modifier : openglContext()->isKeyPressed(mKeysBackward[1]);
  if ( openglContext()->isKeyPressed(mKeysBackward[0]) && okmodifier )
    direction.z() = -1;

  okmodifier = (mKeysForward[1] == Key_None) ? !modifier : openglContext()->isKeyPressed(mKeysForward[1]);
  if ( openglContext()->isKeyPressed(mKeysForward[0]) && okmodifier )
    direction.z() = +1;

  okmodifier = (mKeysUp[1] == Key_None) ? !modifier : openglContext()->isKeyPressed(mKeysUp[1]);
  if ( openglContext()->isKeyPressed(mKeysUp[0]) && okmodifier )
    direction.y() = +1;

  okmodifier = (mKeysDown[1] == Key_None) ? !modifier : openglContext()->isKeyPressed(mKeysDown[1]);
  if ( openglContext()->isKeyPressed(mKeysDown[0]) && okmodifier )
    direction.y() = -1;

  vec3 dir;
  dir += camera()->modelingMatrix().getX() * direction.x();
  dir += camera()->modelingMatrix().getY() * direction.y();
  dir -= camera()->modelingMatrix().getZ() * direction.z();
  dir.normalize();
  mPosition += dir * (real)(dt * mMovementSpeed);
}
//-----------------------------------------------------------------------------
void GhostCameraManipulator::setCamera(Camera* camera) { mCamera = camera; }
//-----------------------------------------------------------------------------
Camera* GhostCameraManipulator::camera() { return mCamera.get(); }
const Camera* GhostCameraManipulator::camera() const { return mCamera.get(); }
//-----------------------------------------------------------------------------
void GhostCameraManipulator::enableEvent(bool enabled)
{
  if (enabled)
  {
    if ( camera() == NULL )
      return;

    setPosition( camera()->modelingMatrix().getT() );
    real x, y;
    camera()->modelingMatrix().getYXRotationAngles( y, x );
    setXDegrees(x);
    setYDegrees(y);

    if (openglContext())
      openglContext()->setMouseVisible(false);

    if ( openglContext() && openglContext()->framebuffer() )
    {
      int cx = (int)camera()->viewport()->center().x();
      int cy = openglContext()->framebuffer()->height() - camera()->viewport()->height() / 2 - camera()->viewport()->y();
      openglContext()->ignoreNextMouseMoveEvent();
      openglContext()->setMousePosition(cx, cy);
    }

    // requires continuous update
    openglContext()->setContinuousUpdate(true);
  }
}
//-----------------------------------------------------------------------------
