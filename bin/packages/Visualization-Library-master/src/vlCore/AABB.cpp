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

#include <vlCore/AABB.hpp>

using namespace vl;

//-----------------------------------------------------------------------------
// AABB
//-----------------------------------------------------------------------------
AABB::AABB() 
{
  setNull();
}
//-----------------------------------------------------------------------------
AABB::AABB( const vec3& center, real radius ) 
{
  mMax = center + radius;
  mMin = center - radius;
}
//-----------------------------------------------------------------------------
AABB::AABB( const vec3& pt1, const vec3& pt2, real displace) 
{
  mMax = mMin = pt1;
  if ( mMax.x() < pt2.x() ) mMax.x() = pt2.x();
  if ( mMax.y() < pt2.y() ) mMax.y() = pt2.y();
  if ( mMax.z() < pt2.z() ) mMax.z() = pt2.z();
  if ( mMin.x() > pt2.x() ) mMin.x() = pt2.x();
  if ( mMin.y() > pt2.y() ) mMin.y() = pt2.y();
  if ( mMin.z() > pt2.z() ) mMin.z() = pt2.z();

  mMax = mMax + displace;
  mMin = mMin - displace;
}
//-----------------------------------------------------------------------------
void AABB::enlarge(real displace) {
  if ( isNull() )
    return;

  mMax = mMax + displace;
  mMin = mMin - displace;
}
//-----------------------------------------------------------------------------
bool AABB::intersects(const AABB& bb) const
{
  if (isNull() || bb.isNull())
    return false;

  if (mMax.x() < bb.mMin.x())
    return false;

  if (mMax.z() < bb.mMin.z())
    return false;

  if (mMin.x() > bb.mMax.x())
    return false;

  if (mMin.z() > bb.mMax.z())
    return false;

  return true;
}
//-----------------------------------------------------------------------------
vec3 AABB::clip(const vec3& v, bool clipx, bool clipy, bool clipz) const 
{
  if (isNull())
    return v;

  vec3 tmp = v;

  if (clipx) {
      if (v.x() > mMax.x())
        tmp.x() = mMax.x();
      if (v.x() < mMin.x())
        tmp.x() = mMin.x();
  }

  if (clipy) {
      if (v.y() > mMax.y())
        tmp.y() = mMax.y();
      if (v.y() < mMin.y())
        tmp.y() = mMin.y();
    }

  if (clipz) {
      if (v.z() > mMax.z())
        tmp.z() = mMax.z();
      if (v.z() < mMin.z())
        tmp.z() = mMin.z();
    }
    return tmp;
}
//-----------------------------------------------------------------------------
bool AABB::isInside(const vec3& v, bool clipx, bool clipy, bool clipz) const 
{
  vec3 t = v;
  return v == clip(t, clipx, clipy, clipz);
}
//-----------------------------------------------------------------------------
bool AABB::isInside(const vec3& v) const 
{
  return v.x() >= minCorner().x() && v.x() <= maxCorner().x() &&
         v.y() >= minCorner().y() && v.y() <= maxCorner().y() &&
         v.z() >= minCorner().z() && v.z() <= maxCorner().z();
}
//-----------------------------------------------------------------------------
void AABB::addPoint(const vec3& v, real radius) 
{
  if (isNull())
  {
    mMax = v;
    mMin = v;
  }

  if ( mMax.x() < v.x() + radius) mMax.x() = v.x() + radius;
  if ( mMax.y() < v.y() + radius) mMax.y() = v.y() + radius;
  if ( mMax.z() < v.z() + radius) mMax.z() = v.z() + radius;
  if ( mMin.x() > v.x() - radius) mMin.x() = v.x() - radius;
  if ( mMin.y() > v.y() - radius) mMin.y() = v.y() - radius;
  if ( mMin.z() > v.z() - radius) mMin.z() = v.z() - radius;
}
//-----------------------------------------------------------------------------
real AABB::width() const {
  if (isNull())
    return 0;
  else
    return mMax.x() - mMin.x();
}
//-----------------------------------------------------------------------------
real AABB::height() const {
  if (isNull())
    return 0;
  else
    return mMax.y() - mMin.y();
}
//-----------------------------------------------------------------------------
real AABB::depth() const {
  if (isNull())
    return 0;
  else
    return mMax.z() - mMin.z();
}
//-----------------------------------------------------------------------------
AABB AABB::operator+(const AABB& aabb) const 
{
  if(isNull())
    return aabb;
  if (aabb.isNull())
    return *this;
  AABB tmp = aabb;
  tmp.addPoint( mMin );
  tmp.addPoint( mMax );
  return tmp;
}
//-----------------------------------------------------------------------------
vec3 AABB::center() const
{
  vec3 c = (minCorner() + maxCorner()) / 2.0f;
  return c;
}
//-----------------------------------------------------------------------------
