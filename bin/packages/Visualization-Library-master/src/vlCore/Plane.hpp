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

#ifndef Plane_INCLUDE_ONCE
#define Plane_INCLUDE_ONCE

#include <vlCore/Object.hpp>
#include <vlCore/glsl_math.hpp>
#include <vlCore/Transform.hpp>

namespace vl
{
  class AABB;

  //-----------------------------------------------------------------------------
  // Plane
  //-----------------------------------------------------------------------------
  /**
   * The Plane class defines a plane using a normal and an origin.
  */
  class VLCORE_EXPORT Plane: public Object
  {
    VL_INSTRUMENT_CLASS(vl::Plane, Object)

  public:
    Plane( real o=0.0f, vec3 n=vec3(0,0,0) ): mNormal(n), mOrigin(o) 
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }

    Plane( const vec3& o, const vec3& n ) 
    {
      VL_DEBUG_SET_OBJECT_NAME()
      mNormal = n;
      mOrigin = dot(o, n);
    }

    real distance(const vec3 &v) const;

    //! returns 0 if the AABB intersects the plane, 1 if it's in the positive side, 
    //! -1 if it's in the negative side.
    int classify(const AABB&) const;

    bool isOutside(const AABB&) const;

    const vec3& normal() const { return mNormal; }

    real origin() const { return mOrigin; }

    void setNormal(const vec3& normal) { mNormal = normal; }

    void setOrigin(real origin) { mOrigin = origin; }

  protected:
    vec3 mNormal;
    real mOrigin;
  };
}

#endif
