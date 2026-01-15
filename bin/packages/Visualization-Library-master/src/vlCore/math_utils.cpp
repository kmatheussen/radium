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

#include <vlCore/math_utils.hpp>
#include <vlCore/AABB.hpp>
#include <vlCore/Vector2.hpp>
#include <vlCore/Vector4.hpp>
#include <vlCore/Plane.hpp>
#include <vlCore/checks.hpp>
#include <vlCore/MersenneTwister.hpp>
#include <cstdlib>

using namespace vl;

//-----------------------------------------------------------------------------
real vl::random(real min, real max)
{
  real t = (real)defMersenneTwister()->rand53();
  return min + (max-min)*t;
}
//-----------------------------------------------------------------------------
u32 vl::randomU32(u32 min, u32 max)
{
  return defMersenneTwister()->randInt(max-min) + min;
}
//-----------------------------------------------------------------------------
i32 vl::randomI32(i32 min, i32 max)
{
  return defMersenneTwister()->randInt(max-min) + min;
}
//-----------------------------------------------------------------------------
int vl::greaterEqualPow2(int n)
{
  int pow2=2;
  for(int i=0; i<20; ++i) {
    if (pow2 >= n)
      return pow2;
    pow2 = pow2 * 2;
  }
  return pow2;
}
//-----------------------------------------------------------------------------
int vl::smallerEqualPow2(int n)
{
  int pow2=2;
  for(int i=0; i<20; ++i) {
    if (pow2 > n)
      return pow2/2;
    pow2 = pow2 * 2;
  }
  return pow2;
}
//-----------------------------------------------------------------------------
void vl::extractPlanes( Plane* planes, const mat4& modelviewproj )
{
  // see also http://www2.ravensoft.com/users/ggribb/plane%20extraction.pdf
  // see also http://zach.in.tu-clausthal.de/teaching/cg_literatur/lighthouse3d_view_frustum_culling/index.html
  // the equation is a*x+b*y+x*z+d = 0
  // where <a b c> is the normal of the plane
  // the normals are pointing inside the viewfrustum

  // 1) we want the planes to point outward so we reverse them
  // 2) because of 1) "d" becomes the distance of the plane from the origin

  vec3 n;
  real d;

  // left clipping plane
  n.x() = modelviewproj.e(3,0) + modelviewproj.e(0,0);
  n.y() = modelviewproj.e(3,1) + modelviewproj.e(0,1);
  n.z() = modelviewproj.e(3,2) + modelviewproj.e(0,2);
  d = modelviewproj.e(3,3) + modelviewproj.e(0,3);
  d /= n.length();
  n.normalize();
  planes[0] = Plane(d,-n);

  // right clipping plane
  n.x() = modelviewproj.e(3,0) - modelviewproj.e(0,0);
  n.y() = modelviewproj.e(3,1) - modelviewproj.e(0,1);
  n.z() = modelviewproj.e(3,2) - modelviewproj.e(0,2);
  d = modelviewproj.e(3,3) - modelviewproj.e(0,3);
  d /= n.length();
  n.normalize();
  planes[1] = Plane(d,-n);

  // top clipping plane
  n.x() = modelviewproj.e(3,0) - modelviewproj.e(1,0);
  n.y() = modelviewproj.e(3,1) - modelviewproj.e(1,1);
  n.z() = modelviewproj.e(3,2) - modelviewproj.e(1,2);
  d = modelviewproj.e(3,3) - modelviewproj.e(1,3);
  d /= n.length();
  n.normalize();
  planes[2] = Plane(d,-n);

  // bottom clipping plane
  n.x() = modelviewproj.e(3,0) + modelviewproj.e(1,0);
  n.y() = modelviewproj.e(3,1) + modelviewproj.e(1,1);
  n.z() = modelviewproj.e(3,2) + modelviewproj.e(1,2);
  d = modelviewproj.e(3,3) + modelviewproj.e(1,3);
  d /= n.length();
  n.normalize();
  planes[3] = Plane(d,-n);

  // near clipping plane
  n.x() = modelviewproj.e(3,0) + modelviewproj.e(2,0);
  n.y() = modelviewproj.e(3,1) + modelviewproj.e(2,1);
  n.z() = modelviewproj.e(3,2) + modelviewproj.e(2,2);
  d = modelviewproj.e(3,3) + modelviewproj.e(2,3);
  d /= n.length();
  n.normalize();
  planes[4] = Plane(d,-n);

  // far clipping plane
  n.x() = modelviewproj.e(3,0) - modelviewproj.e(2,0);
  n.y() = modelviewproj.e(3,1) - modelviewproj.e(2,1);
  n.z() = modelviewproj.e(3,2) - modelviewproj.e(2,2);
  d = modelviewproj.e(3,3) - modelviewproj.e(2,3);
  d /= n.length();
  n.normalize();
  planes[5] = Plane(d,-n);
}
