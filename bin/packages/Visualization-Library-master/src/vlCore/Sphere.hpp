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

#ifndef Sphere_INCLUDE_ONCE
#define Sphere_INCLUDE_ONCE

#include <vlCore/AABB.hpp>

namespace vl
{
//-----------------------------------------------------------------------------
// Sphere
//-----------------------------------------------------------------------------
  /** The Sphere class defines a sphere using a center and a radius using vl::real precision. */
  class VLCORE_EXPORT Sphere
  {
  public:
    /** Constructor: creates a null sphere. */
    Sphere(): mRadius(-1) { }

    /** Constructor: creates a sphere with the given center and radius. */
    Sphere(const vec3& center, real radius): mCenter(center), mRadius(radius) {}

    /** Copy-constructor. */
    Sphere(const AABB& aabb) { *this = aabb; }

    /** Sets the sphere as null.*/
    void setNull()  { mRadius =-1.0f; mCenter = vec3(0,0,0); }

    /** Returns true if the sphere is null, ie, if radius is < 0. */    
    bool isNull()  const { return mRadius <  0.0f; }
    
    /** Returns true if the sphere as radius == 0 */
    bool isPoint() const { return mRadius == 0.0f; }
    
    /** Sets the center of the sphere. */
    void setCenter(const vec3& center) { mCenter = center; }
    
    /** Returns the center of the sphere. */
    const vec3& center() const { return mCenter; }
    
    /** Sets the radius of the sphere. */
    void setRadius( real radius ) { mRadius = radius; }
    
    /** Returns the radius of the sphere. */
    real radius() const { return mRadius; }

    /** Returns true if a sphere contains the specified sphere. */
    bool includes(const Sphere& other) const
    {
      if (isNull())
        return false;
      else
      if (other.isNull())
        return true;
      else
      {
        real distance = (center() - other.center()).length();
        return radius() >= distance + other.radius();
      }
    }

    /** Returns true if two spheres are identical. */
    bool operator==(const Sphere& other) const 
    {
      return mCenter == other.mCenter && mRadius == other.mRadius;
    }

    /** Returns true if two spheres are not identical. */
    bool operator!=(const Sphere& other) const
    {
      return !operator==(other);
    }
    
    /** Constructs a sphere that contains the specified AABB. */
    Sphere& operator=(const AABB& aabb)
    {
      if (aabb.isNull())
        setNull();
      else
      {
        mCenter = aabb.center();
        mRadius = (aabb.minCorner() - aabb.maxCorner()).length() / (real)2.0;
      }
      return *this;
    }

    /** Returns a sphere that contains the two specified spheres. */
    Sphere operator+(const Sphere& other)
    {
      Sphere t = *this;
      return t += other;
    }

    /** Enlarges the sphere to contain the specified sphere. */
    const Sphere& operator+=(const Sphere& other)
    {
      if (this->isNull())
        *this = other;
      else
      if (other.includes(*this))
      {
        *this = other;
      }
      else
      if (!other.isNull() && !this->includes(other))
      {
        vec3 v = other.center() - this->center();
        if (v.isNull())
        {
          // the center remains the same
          // sets the maximum radius
          setRadius( radius() > other.radius() ? radius() : other.radius() );
        }
        else
        {
          v.normalize();
          vec3 p0 = this->center() - v * this->radius();
          vec3 p1 = other.center() + v * other.radius();
          setCenter( (p0 + p1)*(real)0.5 );
          setRadius( (p0 - p1).length()*(real)0.5 );
        }
      }

      return *this;
    }

    /** Returns a sphere that contains the original sphere transformed by the given matrix. */
    void transformed(Sphere& out, const mat4& mat) const 
    {
      out.setNull();
      if ( !isNull() )
      {
        out.mCenter = mat * center();
        // vec3 p = center() + vec3( (real)0.577350269189625840, (real)0.577350269189625840, (real)0.577350269189625840 ) * radius();
        // p = mat * p;
        // p = p - out.center();
        // out.setRadius(p.length());
        vec3 p0 = center() + vec3(radius(),0,0);
        vec3 p1 = center() + vec3(0,radius(),0);
        vec3 p2 = center() + vec3(0,0,radius());
        p0 = mat * p0;
        p1 = mat * p1;
        p2 = mat * p2;
        real d0 = (p0 - out.mCenter).lengthSquared();
        real d1 = (p1 - out.mCenter).lengthSquared();
        real d2 = (p2 - out.mCenter).lengthSquared();
        out.mRadius = ::sqrt( d0>d1 ? (d0>d2?d0:d2) : (d1>d2?d1:d2) );
      }
    }

    /** Returns a sphere that contains the original sphere transformed by the given matrix. */
    Sphere transformed(const mat4& mat) const 
    {
      Sphere sphere;
      transformed(sphere, mat);
      return sphere;
    }

  protected:
    vec3 mCenter;
    real mRadius;
  };
}

#endif
