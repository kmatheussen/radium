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

#ifndef Interpolator_INCLUDE_ONCE
#define Interpolator_INCLUDE_ONCE

#include <vlCore/Object.hpp>
#include <vlCore/glsl_math.hpp>
#include <vlCore/Vector4.hpp>
#include <vector>

namespace vl
{
  /** Abstract class for all the interpolators.
   *
   * An Interpolator is an object that implements a specific interpolation scheme (linear, Catmull-Rom, Bezier etc.) to interpolate data such as vectors, colors, rotations an so on.
   * Interpolators are very useful for tasks such as animating an object's position and/or rotation, creating a smooth curved line from a set of control points, creating extrusion paths and silhouettes etc.
   *
   * \sa 
   *
   * LinearInterpolator, CatmullRomInterpolator, the \ref pagGuideInterpolators "Interpolators Tutorial" page and the Extrusion class.
   *
   * <img src="pics/pagGuideInterpolators1.png"> */
  class Interpolator: public Object
  {
    VL_INSTRUMENT_CLASS(vl::Interpolator, Object)
  public:
    Interpolator() 
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }
  };
  //! Abstract class that interpolates vl::fvec4 values
  class InterpolatorFVec4: public Interpolator
  {
    VL_INSTRUMENT_ABSTRACT_CLASS(vl::InterpolatorFVec4, Interpolator)
  public:
    //! Samples the interpolator at the given point.
    virtual vl::fvec4 computePoint(float t) const = 0;
  };
  //! Abstract class that interpolates vl::fvec3 values
  class InterpolatorFVec3: public Interpolator
  {
    VL_INSTRUMENT_ABSTRACT_CLASS(vl::InterpolatorFVec3, Interpolator)
  public:
    //! Samples the interpolator at the given point.
    virtual vl::fvec3 computePoint(float t) const = 0;
  };
  //! Abstract class that interpolates vl::fvec2 values
  class InterpolatorFVec2: public Interpolator
  {
    VL_INSTRUMENT_ABSTRACT_CLASS(vl::InterpolatorFVec2, Interpolator)
  public:
    //! Samples the interpolator at the given point.
    virtual vl::fvec2 computePoint(float t) const = 0;
  };
  //! Abstract class that interpolates \p float values
  class InterpolatorFloat: public Interpolator
  {
    VL_INSTRUMENT_ABSTRACT_CLASS(vl::InterpolatorFloat, Interpolator)
  public:
    //! Samples the interpolator at the given point.
    virtual float computePoint(float t) const = 0;
  };
  //! Abstract class that interpolates vl::dvec4 values
  class InterpolatorDVec4: public Interpolator
  {
    VL_INSTRUMENT_ABSTRACT_CLASS(vl::InterpolatorDVec4, Interpolator)
  public:
    //! Samples the interpolator at the given point.
    virtual vl::dvec4 computePoint(float t) const = 0;
  };
  //! Abstract class that interpolates vl::dvec3 values
  class InterpolatorDVec3: public Interpolator
  {
    VL_INSTRUMENT_ABSTRACT_CLASS(vl::InterpolatorDVec3, Interpolator)
  public:
    //! Samples the interpolator at the given point.
    virtual vl::dvec3 computePoint(float t) const = 0;
  };
  //! Abstract class that interpolates vl::dvec2 values
  class InterpolatorDVec2: public Interpolator
  {
    VL_INSTRUMENT_ABSTRACT_CLASS(vl::InterpolatorDVec2, Interpolator)
  public:
    //! Samples the interpolator at the given point.
    virtual vl::dvec2 computePoint(float t) const = 0;
  };
  //! Abstract class that interpolates \p double values.
  class InterpolatorDouble: public Interpolator
  {
    VL_INSTRUMENT_ABSTRACT_CLASS(vl::InterpolatorDouble, Interpolator)
  public:
    //! Samples the interpolator at the given point.
    virtual double computePoint(float t) const = 0;
  };
}

#endif
