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

#include <vlCore/Plane.hpp>
#include <vlCore/AABB.hpp>

using namespace vl;

//-----------------------------------------------------------------------------
// Plane
//-----------------------------------------------------------------------------
real Plane::distance(const vec3 &v) const 
{
  return dot(v, mNormal) - mOrigin;
}
//-----------------------------------------------------------------------------
bool Plane::isOutside(const AABB& aabb) const
{
  vec3 pt;
  pt.x() = mNormal.x() >= 0 ? aabb.minCorner().x() : aabb.maxCorner().x();
  pt.y() = mNormal.y() >= 0 ? aabb.minCorner().y() : aabb.maxCorner().y();
  pt.z() = mNormal.z() >= 0 ? aabb.minCorner().z() : aabb.maxCorner().z();
  return distance(pt) >= 0;
}
//-----------------------------------------------------------------------------
int Plane::classify(const AABB& aabb) const
{
  vec3 corner[] = 
  {
    vec3(aabb.minCorner().x(), aabb.minCorner().y(), aabb.minCorner().z()),
    vec3(aabb.minCorner().x(), aabb.minCorner().y(), aabb.maxCorner().z()),
    vec3(aabb.minCorner().x(), aabb.maxCorner().y(), aabb.minCorner().z()),
    vec3(aabb.minCorner().x(), aabb.maxCorner().y(), aabb.maxCorner().z()),
    vec3(aabb.maxCorner().x(), aabb.minCorner().y(), aabb.minCorner().z()),
    vec3(aabb.maxCorner().x(), aabb.minCorner().y(), aabb.maxCorner().z()),
    vec3(aabb.maxCorner().x(), aabb.maxCorner().y(), aabb.minCorner().z()),
    vec3(aabb.maxCorner().x(), aabb.maxCorner().y(), aabb.maxCorner().z())
  };

  int left  = 0;
  int right = 0;
  real const NEPS = -0.0001f;
  real const PEPS = +0.0001f;

  for(int i=0;i<8; ++i)
  {
    if ( distance(corner[i]) < NEPS ) 
      left++;
    else 
    if ( distance(corner[i]) > PEPS ) 
      right++;
    // else -> we don't count the points on the plane

    if(left && right) // its clearly intersecting the plane
      return 0;
  }

  if (left)
    return -1;
  else 
  if (right)
    return +1;
  else // all the points were on the plane
    return 0;
}
//-----------------------------------------------------------------------------
