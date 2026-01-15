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

#ifndef Frustum_INCLUDE_ONCE
#define Frustum_INCLUDE_ONCE

#include <vlCore/Plane.hpp>
#include <vlCore/AABB.hpp>
#include <vlCore/Sphere.hpp>

namespace vl
{
  //-----------------------------------------------------------------------------
  // Frustum
  //-----------------------------------------------------------------------------
  /**
   * A set of planes defining a frustum used for culling purposes (frustum culling).
   *
   * \sa Camera, Viewport
  */
  class Frustum: public Object
  {
    VL_INSTRUMENT_CLASS(vl::Frustum, Object)

  public:
    std::vector<Plane>& planes() { return mPlanes; }
    const std::vector<Plane>& planes() const { return mPlanes; }

    void setPlane(unsigned i, const Plane& plane) { VL_CHECK(i<mPlanes.size()); mPlanes[i] = plane; }
    const Plane& plane(unsigned i) const { VL_CHECK(i<mPlanes.size()); return mPlanes[i]; }

    bool cull(const Sphere& sphere) const
    {
      // null spheres are always visible
      if (sphere.isNull())
        return false;
      for(unsigned i=0; i<planes().size(); ++i)
      {
        if ( plane(i).distance(sphere.center()) > sphere.radius() )
          return true;
      }
      return false;
    }

    bool cull(const AABB& aabb) const
    {
      if (aabb.isNull())
        return false;
      for(unsigned i=0; i<planes().size(); ++i)
      {
        if ( plane(i).isOutside(aabb) )
          return true;
      }
      return false;
    }

    bool cull(const std::vector<fvec3>& points) const
    {
      for(unsigned i=0; i<planes().size(); ++i)
      {
        unsigned j=0;
        for(; j<points.size(); ++j)
          if ( plane(i).distance((vec3)points[j]) <= 0 )
            break;
        if(j == points.size())
          return true;
      }
      return false;
    }

  protected:
    std::vector<Plane> mPlanes;
  };
}

#endif
