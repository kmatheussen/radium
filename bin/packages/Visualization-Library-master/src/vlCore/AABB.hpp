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

#ifndef AABB_INCLUDE_ONCE
#define AABB_INCLUDE_ONCE

#include <vlCore/Vector3.hpp>
#include <vlCore/Matrix4.hpp>

namespace vl
{
  //-----------------------------------------------------------------------------
  // AABB
  //-----------------------------------------------------------------------------
  /** The AABB class implements an axis-aligned bounding box using vl::real precision. */
  class VLCORE_EXPORT AABB 
  {
  public:
    /** Constructs a null AABB. */
    AABB();

    /** Constructs an AABB large enough to contain a sphere with the specified \p radius and \p center. */
    AABB( const vec3& center, real radius );

    /** Constructs an AABB large enough to contain the two specified points and enlarged by the amount specified by \p displace. */
    AABB( const vec3& pt1, const vec3& pt2, real displace=0);

    /** Sets ths AABB as null, that is, empty. */
    void setNull() { mMin = 1; mMax = -1; }

    /** Returns true if the AABB is null. */
    bool isNull()  const { return mMin.x() > mMax.x() || mMin.y() > mMax.y() || mMin.z() > mMax.z(); }

    /** Returns true if the AABB contains a single point, that is, if the min and max corners of the AABB are equal. */
    bool isPoint() const { return mMin == mMax; }

    /** Enlarges the AABB in all directions by \p displace amount. 
        As a result every edge of the AABB will be \p displace*2 longer. */
    void enlarge(real displace);

    /** Returns true if an AABB intersects with the given AABB. */
    bool intersects(const AABB & bb) const;

    /** Clips the position of the given \p p point to be inside an AABB. */
    vec3 clip(const vec3& p, bool clipx=true, bool clipy=true, bool clipz=true) const;

    /** Returns true if the given point is inside the AABB. 
        This method allows you to restrict the test to any of the x, y, z axes. */
    bool isInside(const vec3& p, bool clipx, bool clipy, bool clipz) const;

    /** Returns true if the given point is inside the AABB. */
    bool isInside(const vec3& p) const;

    /** Returns the width of the AABB computed as max.x - min.x */
    real width() const;

    /** Returns the height of the AABB computed as max.y - min.y */
    real height() const;

    /** Returns the depth of the AABB computed as max.z - min.z */
    real depth() const;

    /** Returns true if two AABB are identical. */
    bool operator==(const AABB& aabb) const
    {
      return mMin == aabb.mMin && mMax == aabb.mMax;
    }

    /** Returns true if two AABB are not identical. */
    bool operator!=(const AABB& aabb) const
    {
      return !operator==(aabb);
    }

    /** Returns an AABB which contains the two source AABB. */
    AABB operator+(const AABB& aabb) const;

    /** Enlarges (if necessary) an AABB so that it contains the given AABB. */
    AABB& operator+=(const AABB& other)
    {
      *this = *this + other;
      return *this;
    }

    /** Returns an AABB which contains the source AABB and the given point. */
    AABB operator+(const vec3& p)
    {
      AABB aabb = *this;
      aabb += p;
      return aabb;
    }

    /** Enlarges (if necessary) an AABB to contain the given point. */
    const AABB& operator+=(const vec3& p)
    {
      addPoint(p);
      return *this;
    }

    /** Returns the center of the AABB. */
    vec3 center() const;

    /** Returns the longest dimension of the AABB. */
    real longestSideLength() const
    {
      real side = width();
      if (height() > side)
        side = height();
      if (depth() > side)
        side = depth();
      return side;
    }

    /** Updates the AABB to contain the given point. 
        The point can represent a sphere if \p radius > 0. */
    void addPoint(const vec3& p, real radius);

    /** Updates the AABB to contain the given point. */
    void addPoint(const vec3& p) 
    {
      if (isNull())
      {
        mMax = p;
        mMin = p;
        return;
      }

      if ( mMax.x() < p.x() ) mMax.x() = p.x();
      if ( mMax.y() < p.y() ) mMax.y() = p.y();
      if ( mMax.z() < p.z() ) mMax.z() = p.z();
      if ( mMin.x() > p.x() ) mMin.x() = p.x();
      if ( mMin.y() > p.y() ) mMin.y() = p.y();
      if ( mMin.z() > p.z() ) mMin.z() = p.z();
    }

    /** Transforms an AABB by the given matrix and returns it into the \p out parameter. */
    void transformed(AABB& out, const mat4& mat) const 
    {
      out.setNull();
      if ( !isNull() )
      {
        out.addPoint( mat * vec3(minCorner().x(), minCorner().y(), minCorner().z()) );
        out.addPoint( mat * vec3(minCorner().x(), maxCorner().y(), minCorner().z()) );
        out.addPoint( mat * vec3(maxCorner().x(), maxCorner().y(), minCorner().z()) );
        out.addPoint( mat * vec3(maxCorner().x(), minCorner().y(), minCorner().z()) );
        out.addPoint( mat * vec3(minCorner().x(), minCorner().y(), maxCorner().z()) );
        out.addPoint( mat * vec3(minCorner().x(), maxCorner().y(), maxCorner().z()) );
        out.addPoint( mat * vec3(maxCorner().x(), maxCorner().y(), maxCorner().z()) );
        out.addPoint( mat * vec3(maxCorner().x(), minCorner().y(), maxCorner().z()) );
      }
    }

    /** Returns the AABB transformed by the given matrix. */
    AABB transformed(const mat4& mat) const 
    {
      AABB aabb;
      transformed(aabb, mat);
      return aabb;
    }
    
    /** Returns the corner of the AABB with the minimum x y z coordinates. */
    const vec3& minCorner() const { return mMin; }

    /** Returns the corner of the AABB with the maximum x y z coordinates. */
    const vec3& maxCorner() const { return mMax; }

    /** Sets the corner of the AABB with the minimum x y z coordinates. */
    void setMinCorner(real x, real y, real z) { mMin.x() = x; mMin.y() = y; mMin.z() = z; }

    /** Sets the corner of the AABB with the minimum x y z coordinates. */
    void setMinCorner(const vec3& v) { mMin = v; }

    /** Sets the corner of the AABB with the maximum x y z coordinates. */
    void setMaxCorner(real x, real y, real z) { mMax.x() = x; mMax.y() = y; mMax.z() = z; }

    /** Sets the corner of the AABB with the maximum x y z coordinates. */
    void setMaxCorner(const vec3& v) { mMax = v; }

    /** Returns the volume of the AABB. */
    real volume() const { return width() * height() * depth(); }

  protected:
    vec3 mMin;
    vec3 mMax;
  };
}

#endif
