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

#ifndef Billboard_INCLUDE_ONCE
#define Billboard_INCLUDE_ONCE

#include <vlCore/Transform.hpp>
#include <vlGraphics/link_config.hpp>

namespace vl
{
  //-----------------------------------------------------------------------------
  // Billboard
  //-----------------------------------------------------------------------------
  /**
   * A Transform that aligns an Actor towards the camera.
  */
  class VLGRAPHICS_EXPORT Billboard: public Transform
  {
    VL_INSTRUMENT_CLASS(vl::Billboard, Transform)

  public:
    Billboard();
    //! The billboard position and rotation center
    void setPosition(const vec3& pos);
    //! The billboard position and rotation center
    void setPosition(real x, real y, real z) { setPosition( vec3(x,y,z) ); }
    //! The billboard position and rotation center
    vec3 position();
    //! The rotation axis in world coordinates. Used only for axis aligned billboards.
    void setAxis(const vec3& axis) { mAxis = axis; mAxis.normalize(); }
    //! The rotation axis in world coordinates. Used only for axis aligned billboards.
    const vec3& axis() const { return mAxis; }
    //! Used only for axis aligned billboards.
    void setNormal(const vec3& normal) { mNormal = normal; mNormal.normalize(); }
    //! Used only for axis aligned billboards.
    const vec3& normal() const { return mNormal; }
    virtual void computeWorldMatrix(Camera* camera=NULL);
    //! The type of the billboard.
    EBillboardType type() const { return mType; }
    //! The type of the billboard.
    void setType(EBillboardType type) { mType = type; }

  protected:
    vec3 mAxis;
    vec3 mNormal;
    EBillboardType mType;
  };
  //-----------------------------------------------------------------------------
}

#endif
