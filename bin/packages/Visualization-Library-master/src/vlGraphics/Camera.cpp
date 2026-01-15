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

#include <vlGraphics/Camera.hpp>
#include <vlGraphics/OpenGL.hpp>
#include <vlCore/AABB.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>

#undef near
#undef far

using namespace vl;

//-----------------------------------------------------------------------------
// Camera
//-----------------------------------------------------------------------------
Camera::Camera()
{
  VL_DEBUG_SET_OBJECT_NAME()
  mFrustum.planes().resize(6);
  mFOV = 60.0;
  mNearPlane = (real)0.05;
  mFarPlane  = (real)10000.0;
  mLeft = mRight = mTop = mBottom = -1;
  mViewport = new Viewport;

  mProjectionMatrix  = mat4::getPerspective(fov(), 640.0f/480.0f, nearPlane(), farPlane());
  mProjectionType = PMT_PerspectiveProjection;
}
//-----------------------------------------------------------------------------
void Camera::applyModelViewMatrix(const mat4& model_matrix) const
{
  /* some OpenGL drivers (ATI) require this instead of the more general (and mathematically correct) viewMatrix() */
  mat4 viewm = viewMatrix();
  viewm.e(3,0) = 0.0;
  viewm.e(3,1) = 0.0;
  viewm.e(3,2) = 0.0;
  viewm.e(3,3) = 1.0;

  glMatrixMode(GL_MODELVIEW);
#if 0
  VL_glLoadMatrix( viewm.ptr() );
  VL_glMultMatrix( matrix.ptr() );
#elif 0
  viewm = viewm * matrix;
  VL_glLoadMatrix( viewm.ptr() );
#else
  VL_glLoadMatrix( (viewm * model_matrix).ptr() );
#endif
}
//-----------------------------------------------------------------------------
void Camera::applyProjMatrix() const
{
  // projection matrix
  glMatrixMode( GL_PROJECTION );
  VL_glLoadMatrix( projectionMatrix().ptr() );
}
//-----------------------------------------------------------------------------
void Camera::applyViewMatrix() const
{
  /* some OpenGL drivers (ATI) require this instead of the more general (and mathematically correct) viewMatrix() */
  mat4 viewm = viewMatrix();
  viewm.e(3,0) = 0.0;
  viewm.e(3,1) = 0.0;
  viewm.e(3,2) = 0.0;
  viewm.e(3,3) = 1.0;
  glMatrixMode(GL_MODELVIEW);
  VL_glLoadMatrix( viewm.ptr() );
}
//-----------------------------------------------------------------------------
void Camera::computeNearFarOptimizedProjMatrix(const Sphere& scene_bounding_sphere)
{
  // near/far clipping planes optimization
  if (!scene_bounding_sphere.isNull())
  {
    // transform the sphere in camera coordinates
    Sphere camera_sphere;
    scene_bounding_sphere.transformed(camera_sphere, viewMatrix());

    // visible objects are in the negative z, but we need a positive distance for the near and far clipping planes
    mNearPlane = -(camera_sphere.center().z() + camera_sphere.radius());
    mFarPlane  = -(camera_sphere.center().z() - camera_sphere.radius());

    // clamp to positive epsilon: can't let near and far clipping planes go behind the camera!
    real epsilon = camera_sphere.radius() / 1000.0f;
    mFarPlane  = max(mFarPlane,  epsilon * 2); // alway more than the near
    mNearPlane = max(mNearPlane, epsilon * 1);

    switch(projectionMatrixType())
    {
    case PMT_OrthographicProjection: setProjectionOrtho(mLeft, mRight, mBottom, mTop, mNearPlane, mFarPlane);
      break;
    case PMT_PerspectiveProjection:  setProjectionPerspective(); 
      break;

    // we cannot do this: if we change the near plane we have to recompute also left, right, bottom and top!
    // case PMT_PerspectiveProjectionFrustum: setProjectionFrustum(mLeft, mRight, mBottom, mTop, mNearPlane, mFarPlane); 
    //   break;

    default:
      Log::bug("Camera::computeNearFarOptimizedProjMatrix() called on unsupported projection type.\n");
    }
  }
}
//-----------------------------------------------------------------------------
void Camera::adjustView(const AABB& aabb, const vec3& dir, const vec3& up, real bias)
{
  VL_CHECK(bias >= 0)
  VL_CHECK(!aabb.isNull())
  if (bias < 0)
    vl::Log::bug("Camera::adjustView(): 'bias' must be >= 0.\n");

  vec3 center = aabb.center();

  Sphere sphere(aabb);
  const vec3& C = modelingMatrix().getT();
  const vec3& V = -modelingMatrix().getZ();
  const real  R = sphere.radius();

  // extract the frustum planes based on the current view and projection matrices
  mat4 viewproj = projectionMatrix() * viewMatrix();
  Frustum frustum; frustum.planes().resize(6);
  extractPlanes( &frustum.planes()[0], viewproj );
  // iterate over left/right/top/bottom clipping planes. the planes are in world coords.
  real max_t = 0;
  for(int i=0; i<4; ++i)
  {
    const vec3& O = frustum.plane(i).origin() * frustum.plane(i).normal();
    const vec3& N = frustum.plane(i).normal();
    real t = - (R + dot(O,N) - dot(C,N)) / dot(N,V);
    VL_CHECK(t>=0)
    if (t > max_t)
      max_t = t;
  }
  real dist = max_t;
  mat4 m = mat4::getLookAt(center+dir*dist*bias,center,up);
  setViewMatrix(m);
}
//-----------------------------------------------------------------------------
void Camera::computeFrustumPlanes()
{
  // build modelview matrix
  mat4 viewproj = projectionMatrix() * viewMatrix();
  // frustum plane extraction
  mFrustum.planes().resize(6);
  extractPlanes( &mFrustum.planes()[0], viewproj );
}
//-----------------------------------------------------------------------------
void Camera::setProjectionFrustum(real left, real right, real bottom, real top, real near, real far)
{
  // see http://www.opengl.org/resources/faq/technical/transformations.htm
  setFOV( 2.0f*atan((top-bottom)*0.5f/near) );
  setNearPlane(near);
  setFarPlane(far);
  setProjectionMatrix(mat4::getFrustum(left, right, bottom, top, near, far), PMT_PerspectiveProjectionFrustum);
}
//-----------------------------------------------------------------------------
void Camera::setProjectionPerspective(real fov, real near, real far)
{
  setFOV(fov);
  setNearPlane(near);
  setFarPlane(far);
  setProjectionMatrix(mat4::getPerspective(fov, aspectRatio(), near, far), PMT_PerspectiveProjection);
}
//-----------------------------------------------------------------------------
void Camera::setProjectionPerspective()
{
  setProjectionMatrix(mat4::getPerspective(fov(), aspectRatio(), nearPlane(), farPlane()), PMT_PerspectiveProjection);
}
//-----------------------------------------------------------------------------
void Camera::setProjectionOrtho()
{
  mLeft   = 0;
  mRight  = (real)mViewport->width();
  mBottom = 0;
  mTop    = (real)mViewport->height();
  mFOV = -1;
  setProjectionMatrix( mat4::getOrtho( mLeft, mRight, mBottom, mTop, mNearPlane, mFarPlane), PMT_OrthographicProjection );
}
//-----------------------------------------------------------------------------
void Camera::setProjectionOrtho(real left, real right, real bottom, real top, real znear, real zfar)
{
  mLeft   = left;
  mRight  = right;
  mBottom = bottom;
  mTop    = top;
  mFOV = -1;
  mNearPlane = znear;
  mFarPlane  = zfar;
  setProjectionMatrix( mat4::getOrtho( mLeft, mRight, mBottom, mTop, mNearPlane, mFarPlane), PMT_OrthographicProjection );
}
//-----------------------------------------------------------------------------
void Camera::setProjectionOrtho(real offset)
{
  mLeft   = offset;
  mRight  = viewport()->width() + offset;
  mBottom = offset;
  mTop    = viewport()->height() + offset;
  mFOV = -1;
  mNearPlane = -1;
  mFarPlane  = +1;
  setProjectionMatrix( mat4::getOrtho( mLeft, mRight, mBottom, mTop, mNearPlane, mFarPlane), PMT_OrthographicProjection );
}
//-----------------------------------------------------------------------------
void Camera::setViewMatrixLookAt( const vec3& eye, const vec3& at, const vec3& up)
{
  // note: this sets both the local matrix and the view matrix
  setViewMatrix( mat4::getLookAt(eye, at, up) );
}
//-----------------------------------------------------------------------------
void Camera::getViewMatrixAsLookAt( vec3& eye, vec3& at, vec3& up, vec3& right) const
{
  mModelingMatrix.getAsLookAtModeling(eye, at, up, right);
}
//-----------------------------------------------------------------------------
bool Camera::project(const vec4& in, vec4& out) const
{
  out = mProjectionMatrix * mViewMatrix * in;

  if (out.w() == 0.0f)
    return false;

  out.x() /= out.w();
  out.y() /= out.w();
  out.z() /= out.w();

  // map to range 0-1
  out.x() = out.x() * 0.5f + 0.5f;
  out.y() = out.y() * 0.5f + 0.5f;
  out.z() = out.z() * 0.5f + 0.5f;

  // map to viewport
  out.x() = out.x() * mViewport->width()  + mViewport->x();
  out.y() = out.y() * mViewport->height() + mViewport->y();
  return true;
}
//-----------------------------------------------------------------------------
bool Camera::unproject(const vec3& win, vec4& out) const
{
    vec4 v;
    v.x() = win.x();
    v.y() = win.y();
    v.z() = win.z();
    v.w() = 1.0;

    // map from viewport to 0-1
    v.x() = (v.x() - mViewport->x()) / mViewport->width();
    v.y() = (v.y() - mViewport->y()) / mViewport->height();

    // map to range -1 to 1
    v.x() = v.x() * 2.0f - 1.0f;
    v.y() = v.y() * 2.0f - 1.0f;
    v.z() = v.z() * 2.0f - 1.0f;

    real det=0;
    mat4 inverse = (mProjectionMatrix * mViewMatrix).getInverse(&det);
    if (!det)
      return false;

    v = inverse * v;
    if (v.w() == 0.0)
      return false;

    out = v / v.w();
    return true;
}
//-----------------------------------------------------------------------------
bool Camera::unproject(std::vector<vec3>& win) const
{
  real det=0;
  mat4 inverse = (mProjectionMatrix * mViewMatrix).getInverse(&det);
  if (!det)
    return false;

  bool ok = true;
  for(unsigned i=0; i<win.size(); ++i)
  {
    vec4 v;
    v = vec4( win[i], 1.0 );

    // map from viewport to 0-1
    v.x() = (v.x() - mViewport->x()) / mViewport->width();
    v.y() = (v.y() - mViewport->y()) / mViewport->height();

    // map to range -1 to 1
    v.x() = v.x() * 2.0f - 1.0f;
    v.y() = v.y() * 2.0f - 1.0f;
    v.z() = v.z() * 2.0f - 1.0f;

    v = inverse * v;
    if (v.w() == 0.0)
    {
      ok = false;
      continue;
    }

    v = v / v.w();
    win[i] = v.xyz();
  }
  return ok;
}
//-----------------------------------------------------------------------------
Ray Camera::computeRay(int winx, int winy)
{
  vl::vec4 out;
  if (!unproject( vl::vec3((real)winx,(real)winy,0), out ))
    return Ray();
  else
  {
    vl::Ray ray;
    ray.setOrigin(out.xyz());
    ray.setDirection( (out.xyz() - modelingMatrix().getT()).normalize() );
    return ray;
  }
}
//-----------------------------------------------------------------------------
Frustum Camera::computeRayFrustum(int winx, int winy)
{
  /*
      n3
    D-----C
    |     |
  n4|  O  |n2
    |     |
    A-----B
      n1
  */
  // compute the frustum passing through the adjacent pixels
  vl::vec4 A1,B1,C1,D1;
  vl::vec4 A2,B2,C2,D2;
  unproject( vl::vec3((real)winx-1,(real)winy-1,0),    A1 );
  unproject( vl::vec3((real)winx+1,(real)winy-1,0),    B1 );
  unproject( vl::vec3((real)winx+1,(real)winy+1,0),    C1 );
  unproject( vl::vec3((real)winx-1,(real)winy+1,0),    D1 );
  unproject( vl::vec3((real)winx-1,(real)winy-1,0.1f), A2 );
  unproject( vl::vec3((real)winx+1,(real)winy-1,0.1f), B2 );
  unproject( vl::vec3((real)winx+1,(real)winy+1,0.1f), C2 );
  unproject( vl::vec3((real)winx-1,(real)winy+1,0.1f), D2 );

  vec3 n1 = -cross(A2.xyz()-A1.xyz(),B1.xyz()-A1.xyz());
  vec3 n2 = -cross(B2.xyz()-B1.xyz(),C1.xyz()-B1.xyz());
  vec3 n3 = -cross(C2.xyz()-C1.xyz(),D1.xyz()-C1.xyz());
  vec3 n4 = -cross(D2.xyz()-D1.xyz(),A1.xyz()-D1.xyz());
  Frustum frustum;
  frustum.planes().push_back( Plane( A1.xyz(), n1 ) );
  frustum.planes().push_back( Plane( B1.xyz(), n2 ) );
  frustum.planes().push_back( Plane( C1.xyz(), n3 ) );
  frustum.planes().push_back( Plane( D1.xyz(), n4 ) );
  return frustum;
}
//-----------------------------------------------------------------------------
