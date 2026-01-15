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

#include <vlGraphics/TrackballManipulator.hpp>
#include <vlGraphics/OpenGLContext.hpp>
#include <vlGraphics/Actor.hpp>
#include <vlGraphics/SceneManager.hpp>
#include <vlGraphics/Rendering.hpp>

using namespace vl;

//-----------------------------------------------------------------------------
void TrackballManipulator::mouseDownEvent(EMouseButton btn, int x, int y)
{
  if ( camera() == NULL )
    return;

  // if already busy ignore the event
  if (mode() != NoMode)
    return;

  // enter new mode
  if (btn == rotationButton())
    mMode = RotationMode;
  else
  if (btn == translationButton())
    mMode = TranslationMode;
  else
  if (btn == zoomButton())
    mMode = ZoomMode;

  VL_CHECK(openglContext()->framebuffer())

  // set x/y relative to the viewport
  x -= camera()->viewport()->x();
  y -= openglContext()->framebuffer()->height() - 1 - (camera()->viewport()->y() + camera()->viewport()->height() -1);

  // check that the click is in the viewport
  int w = camera()->viewport()->width();
  int h = camera()->viewport()->height();

  if (x<0 || y<0 || x>=w || y>=h)
    return;

  mMouseStart.x() = x;
  mMouseStart.y() = y;

  if (mTransform)
  {
    mStartMatrix = mTransform->localMatrix();
    mPivot = mStartMatrix.getT();
  }
  else
    mStartMatrix = camera()->modelingMatrix();

  mStartCameraPos = camera()->modelingMatrix().getT();
  mStartPivot = mPivot;
}
//-----------------------------------------------------------------------------
void TrackballManipulator::mouseUpEvent(EMouseButton btn, int /*x*/, int /*y*/)
{
  if ( camera() == NULL )
    return;

  // if the trackball is not doing anything ignore the event
  if (mode() == NoMode)
    return;

  // leave the mode
  if (btn == rotationButton() && mMode == RotationMode)
    mMode = NoMode;
  else
  if (btn == translationButton() && mMode == TranslationMode)
    mMode = NoMode;
  else
  if (btn == zoomButton() && mMode == ZoomMode)
    mMode = NoMode;
}
//-----------------------------------------------------------------------------
void TrackballManipulator::mouseMoveEvent(int x, int y)
{
  if ( camera() == NULL )
    return;

  // ignore the event if the trackball is not in any mode
  if (mode() == NoMode)
    return;

  VL_CHECK(openglContext()->framebuffer())
  // set x/y relative to the top/left cornder of the viewport
  x -= camera()->viewport()->x();
  y -= openglContext()->framebuffer()->height() - 1 - (camera()->viewport()->y() + camera()->viewport()->height() -1);

  if (mode() == RotationMode)
  {
    if (mTransform)
    {
      mTransform->setLocalMatrix( mat4::getTranslation(mPivot) * trackballRotation(x,y) * mat4::getTranslation(-mPivot) * mStartMatrix );
      mTransform->computeWorldMatrix();
      mStartMatrix = mTransform->localMatrix();
    }
    else
    {
      camera()->setModelingMatrix( mat4::getTranslation(mPivot) * trackballRotation(x,y) * mat4::getTranslation(-mPivot) * mStartMatrix );
      mStartMatrix = camera()->modelingMatrix();
    }

    mMouseStart.x() = x;
    mMouseStart.y() = y;
  }
  else
  if (mode() == ZoomMode)
  {
    float t = (y-mMouseStart.y()) / 200.0f;
    t *= zoomSpeed();
    real distance = (mStartCameraPos - mPivot).length();
    vec3 camera_pos = mStartCameraPos - camera()->modelingMatrix().getZ()*t*distance;
    mat4 m = camera()->modelingMatrix();
    m.setT(camera_pos);
    camera()->setModelingMatrix(m);
  }
  else
  if (mode() == TranslationMode)
  {
    float tx = (mMouseStart.x() - x)  / 400.0f;
    float ty = -(mMouseStart.y() - y) / 400.0f;
    tx *= translationSpeed();
    ty *= translationSpeed();
    real distance = (mStartCameraPos - mPivot).length();
    vec3 up    = camera()->modelingMatrix().getY();
    vec3 right = camera()->modelingMatrix().getX();
    mat4 m = camera()->modelingMatrix();
    m.setT(mStartCameraPos + up*distance*ty + right*distance*tx);
    camera()->setModelingMatrix(m);
    mPivot = mStartPivot + up*distance*ty + right*distance*tx;
  }

  // update the view
  openglContext()->update();
}
//-----------------------------------------------------------------------------
mat4 TrackballManipulator::trackballRotation(int x, int y)
{
  if( x==mMouseStart.x()  && y==mMouseStart.y() )
    return mat4();

  VL_CHECK(camera())
  vec3 a = computeVector(mMouseStart.x(), mMouseStart.y());
  vec3 b = computeVector(x, y);
  vec3 n = cross(a, b);
  n.normalize();
  a.normalize();
  b.normalize();
  real dot_a_b = dot(a,b);
  dot_a_b = clamp(dot_a_b,(real)-1.0,(real)+1.0);
  real alpha = acos(dot_a_b) * (mTransform ? 1 : -1);
  alpha = alpha * rotationSpeed();
  vec3 nc =  camera()->modelingMatrix().get3x3() * n;
  if (mTransform && mTransform->parent())
    nc = mTransform->parent()->getComputedWorldMatrix().getInverse() * nc;
  nc.normalize();
  return mat4::getRotation(alpha*(real)dRAD_TO_DEG, nc);
}
//-----------------------------------------------------------------------------
vec3 TrackballManipulator::computeVector(int x, int y)
{
  vec3 c(camera()->viewport()->width() / 2.0f, camera()->viewport()->height() / 2.0f, 0);

  float sphere_x = camera()->viewport()->width()  * 0.5f;
  float sphere_y = camera()->viewport()->height() * 0.5f;

  VL_CHECK(camera())
  vec3 v((real)x,(real)y,0);
  v -= c;
  v.x() /= sphere_x;
  v.y() /= sphere_y;
  v.y() = -v.y();
  //if (v.length() > 1.0f)
  //  v.normalize();

  real z2 = 1.0f - v.x()*v.x() - v.y()*v.y();
  if (z2 < 0) 
    z2 = 0;
  v.z() = sqrt( z2 );
  v.normalize();
  return v;
}
//-----------------------------------------------------------------------------
void TrackballManipulator::adjustView(const AABB& aabb, const vec3& dir, const vec3& up, real bias)
{
  VL_CHECK(camera())
  VL_CHECK(!aabb.isNull())
  /*setTransform(NULL);*/
  setPivot( aabb.center() );
  camera()->adjustView(aabb, dir, up, bias);
}
//-----------------------------------------------------------------------------
void TrackballManipulator::adjustView(ActorCollection& actors, const vec3& dir, const vec3& up, real bias)
{
  AABB aabb;
  for(int i=0; i<actors.size(); ++i)
  {
    if (actors.at(i)->transform())
    {
      actors.at(i)->transform()->computeWorldMatrix();
    }
    actors.at(i)->computeBounds();
    aabb += actors.at(i)->boundingBox();
  }
  adjustView(aabb, dir, up, bias);
}
//-----------------------------------------------------------------------------
void TrackballManipulator::adjustView(SceneManager* scene, const vec3& dir, const vec3& up, real bias)
{
  ActorCollection actors;
  scene->extractActors(actors);
  adjustView(actors, dir, up, bias);
}
//-----------------------------------------------------------------------------
void TrackballManipulator::adjustView(Rendering* rendering, const vec3& dir, const vec3& up, real bias)
{
  ActorCollection actors;
  for(int i=0; i<rendering->sceneManagers()->size(); ++i)
    rendering->sceneManagers()->at(i)->extractActors(actors);
  adjustView(actors, dir, up, bias);
}
//-----------------------------------------------------------------------------
void TrackballManipulator::enableEvent(bool enabled)
{ 
  if (enabled)
  {
    mMode = NoMode;
    if ( openglContext() )
    {
      openglContext()->setMouseVisible(true);
      openglContext()->setContinuousUpdate(false);
    }
  }
}
//-----------------------------------------------------------------------------
