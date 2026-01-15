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

#ifndef CatmullRomInterpolator_INCLUDE_ONCE
#define CatmullRomInterpolator_INCLUDE_ONCE

#include <vlCore/Interpolator.hpp>

namespace vl
{
  /**
   * The LinearInterpolator class is a template class that implements Catmull-Rom spline interpolation.
   * Catmull-Rom spline interpolation allows smoother interpolations than simple linear interpolation, is thus indicated 
   * for example when you want to smoothly interpolate from one position to another or from one color to another.
   * \sa LinearInterpolator, Interpolator and the \ref pagGuideInterpolators "Interpolators Tutorial" page.
   */
  template<typename T>
  class CatmullRomInterpolator: public Object
  {
    VL_INSTRUMENT_CLASS(vl::CatmullRomInterpolator<typename T>, Object)

  public:
    CatmullRomInterpolator() 
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }

    CatmullRomInterpolator(const std::vector<T>& path): mPath(path) {}

    //! Call this function after having specified the control points if you want to automatically generate the start/end control points.
    void setupEndPoints(bool is_loop)
    {
      VL_CHECK(mPath.size()>=2)
      if (mPath.size()<2)
        return;

      mCatmullRomSpline = mPath;

      /*
       D-------C
       .       |
       .       |
       .       |
       A-------B
      */

      if (is_loop)
      {
        T a = mCatmullRomSpline[0];
        T b = mCatmullRomSpline[1];
        T d = mCatmullRomSpline[mCatmullRomSpline.size()-1];

        mCatmullRomSpline.insert(mCatmullRomSpline.begin(),d);
        mCatmullRomSpline.push_back(a);
        mCatmullRomSpline.push_back(b);
      }
      else
      {
        T a = mCatmullRomSpline[0] + (mCatmullRomSpline[0] - mCatmullRomSpline[1]);
        T b = mCatmullRomSpline[mCatmullRomSpline.size()-1] + (mCatmullRomSpline[mCatmullRomSpline.size()-1] - mCatmullRomSpline[mCatmullRomSpline.size()-2]);
        mCatmullRomSpline.insert(mCatmullRomSpline.begin(),a);
        mCatmullRomSpline.push_back(b);
      }

    }

    //! Samples the Catmull-Rom spline at the given point. The \p t parameter must be in the range 0.0 ... 1.0 included.
    T computePoint(float t) const
    {
      VL_CHECK(mCatmullRomSpline.size() >= 4)
      size_t size = mCatmullRomSpline.size()-2;
      t = clamp(t, 0.0f, 1.0f);
      if (t == 0.0f)
        return mCatmullRomSpline[1];
      else
      if (t == 1.0f)
        return mCatmullRomSpline[ mCatmullRomSpline.size()-1-1 ];
      else
      {
        int i    = 1 + (int)((size-1)*t);
        int i0 = i-1;
        int i1 = i;
        int i2 = i+1;
        int i3 = i+2;
        VL_CHECK(i3<(int)mCatmullRomSpline.size())
        float tt = (size-1)*t - int((size-1)*t); // frac generates rounding errors
        T p0 = mCatmullRomSpline[i0];
        T p1 = mCatmullRomSpline[i1];
        T p2 = mCatmullRomSpline[i2];
        T p3 = mCatmullRomSpline[i3];
        T p  = ( (p1 * 2.0f) + (-p0 + p2)          * tt +
               ( p0*2.0f - p1*5.0f + p2*4.0f - p3) * tt*tt +
               ( p0*-1 + p1*3.0f - p2*3.0f + p3)   * tt*tt*tt ) * 0.5f;
        return p;
      }
    }

    //! The control points defining the Catmull-Rom spline.
    //! Because of the Catmull-Rom formula the interpolated path must start and end with an extra control point 
    //! (one on each side) and cannot have less than 4 control points. You can also automatically generate such 
    //! extra control points by calling the setupEndPoints() method.
    void setPath(const std::vector<T>& path) { mPath = path; }
    
    //! The control points defining the Catmull-Rom spline.
    const std::vector<T>& path() const { return mPath; }
    
    //! The control points defining the Catmull-Rom spline.
    std::vector<T>& path() { return mPath; }

  protected:
    std::vector<T> mPath;
    std::vector<T> mCatmullRomSpline;
  };

  typedef CatmullRomInterpolator<float>     CatmullRomInterpolatorFloat_T;
  typedef CatmullRomInterpolator<fvec2> CatmullRomInterpolatorFVec2_T;
  typedef CatmullRomInterpolator<fvec3> CatmullRomInterpolatorFVec3_T;
  typedef CatmullRomInterpolator<fvec4> CatmullRomInterpolatorFVec4_T;
  typedef CatmullRomInterpolator<double>    CatmullRomInterpolatorDouble_T;
  typedef CatmullRomInterpolator<dvec2> CatmullRomInterpolatorDVec2_T;
  typedef CatmullRomInterpolator<dvec3> CatmullRomInterpolatorDVec3_T;
  typedef CatmullRomInterpolator<dvec4> CatmullRomInterpolatorDVec4_T;

  //! Interpolates fvec4 values using a CatmullRomInterpolator.
  class CatmullRomInterpolatorFVec4: public InterpolatorFVec4
  {
    VL_INSTRUMENT_CLASS(vl::CatmullRomInterpolatorFVec4, InterpolatorFVec4)
  public:
    CatmullRomInterpolatorFVec4(): mInterpolator( new CatmullRomInterpolatorFVec4_T ) {}
    CatmullRomInterpolatorFVec4(const std::vector<fvec4>& path): mInterpolator( new CatmullRomInterpolatorFVec4_T(path) ) {}
    fvec4 computePoint(float t) const { return interpolator()->computePoint(t); }
    CatmullRomInterpolatorFVec4_T* interpolator() { return mInterpolator.get(); }
    const CatmullRomInterpolatorFVec4_T* interpolator() const { return mInterpolator.get(); }
    void setInterpolator(CatmullRomInterpolatorFVec4_T* interpolator) { mInterpolator = interpolator; }
  protected:
    ref<CatmullRomInterpolatorFVec4_T> mInterpolator;
  };
  //! Interpolates fvec3 values using a CatmullRomInterpolator.
  class CatmullRomInterpolatorFVec3: public InterpolatorFVec3
  {
    VL_INSTRUMENT_CLASS(vl::CatmullRomInterpolatorFVec3, InterpolatorFVec3)
  public:
    CatmullRomInterpolatorFVec3(): mInterpolator( new CatmullRomInterpolatorFVec3_T ) {}
    CatmullRomInterpolatorFVec3(const std::vector<fvec3>& path): mInterpolator( new CatmullRomInterpolatorFVec3_T(path) ) {}
    fvec3 computePoint(float t) const { return interpolator()->computePoint(t); }
    CatmullRomInterpolatorFVec3_T* interpolator() { return mInterpolator.get(); }
    const CatmullRomInterpolatorFVec3_T* interpolator() const { return mInterpolator.get(); }
    void setInterpolator(CatmullRomInterpolatorFVec3_T* interpolator) { mInterpolator = interpolator; }
  protected:
    ref<CatmullRomInterpolatorFVec3_T> mInterpolator;
  };
  //! Interpolates fvec2 values using a CatmullRomInterpolator.
  class CatmullRomInterpolatorFVec2: public InterpolatorFVec2
  {
    VL_INSTRUMENT_CLASS(vl::CatmullRomInterpolatorFVec2, InterpolatorFVec2)
  public:
    CatmullRomInterpolatorFVec2(): mInterpolator( new CatmullRomInterpolatorFVec2_T ) {}
    CatmullRomInterpolatorFVec2(const std::vector<fvec2>& path): mInterpolator( new CatmullRomInterpolatorFVec2_T(path) ) {}
    fvec2 computePoint(float t) const { return interpolator()->computePoint(t); }
    CatmullRomInterpolatorFVec2_T* interpolator() { return mInterpolator.get(); }
    const CatmullRomInterpolatorFVec2_T* interpolator() const { return mInterpolator.get(); }
    void setInterpolator(CatmullRomInterpolatorFVec2_T* interpolator) { mInterpolator = interpolator; }
  protected:
    ref<CatmullRomInterpolatorFVec2_T> mInterpolator;
  };
  //! Interpolates \p float values using a CatmullRomInterpolator.
  class CatmullRomInterpolatorFloat: public InterpolatorFloat
  {
    VL_INSTRUMENT_CLASS(vl::CatmullRomInterpolatorFloat, InterpolatorFloat)
  public:
    CatmullRomInterpolatorFloat(): mInterpolator( new CatmullRomInterpolatorFloat_T ) {}
    CatmullRomInterpolatorFloat(const std::vector<float>& path): mInterpolator( new CatmullRomInterpolatorFloat_T(path) ) {}
    float computePoint(float t) const { return interpolator()->computePoint(t); }
    CatmullRomInterpolatorFloat_T* interpolator() { return mInterpolator.get(); }
    const CatmullRomInterpolatorFloat_T* interpolator() const { return mInterpolator.get(); }
    void setInterpolator(CatmullRomInterpolatorFloat_T* interpolator) { mInterpolator = interpolator; }
  protected:
    ref<CatmullRomInterpolatorFloat_T> mInterpolator;
  };
  //! Interpolates dvec4 values using a CatmullRomInterpolator.
  class CatmullRomInterpolatorDVec4: public InterpolatorDVec4
  {
    VL_INSTRUMENT_CLASS(vl::CatmullRomInterpolatorDVec4, InterpolatorDVec4)
  public:
    CatmullRomInterpolatorDVec4(): mInterpolator( new CatmullRomInterpolatorDVec4_T ) {}
    CatmullRomInterpolatorDVec4(const std::vector<dvec4>& path): mInterpolator( new CatmullRomInterpolatorDVec4_T(path) ) {}
    dvec4 computePoint(float t) const { return interpolator()->computePoint(t); }
    CatmullRomInterpolatorDVec4_T* interpolator() { return mInterpolator.get(); }
    const CatmullRomInterpolatorDVec4_T* interpolator() const { return mInterpolator.get(); }
    void setInterpolator(CatmullRomInterpolatorDVec4_T* interpolator) { mInterpolator = interpolator; }
  protected:
    ref<CatmullRomInterpolatorDVec4_T> mInterpolator;
  };
  //! Interpolates dvec3 values using a CatmullRomInterpolator.
  class CatmullRomInterpolatorDVec3: public InterpolatorDVec3
  {
    VL_INSTRUMENT_CLASS(vl::CatmullRomInterpolatorDVec3, InterpolatorDVec3)
  public:
    CatmullRomInterpolatorDVec3(): mInterpolator( new CatmullRomInterpolatorDVec3_T ) {}
    CatmullRomInterpolatorDVec3(const std::vector<dvec3>& path): mInterpolator( new CatmullRomInterpolatorDVec3_T(path) ) {}
    dvec3 computePoint(float t) const { return interpolator()->computePoint(t); }
    CatmullRomInterpolatorDVec3_T* interpolator() { return mInterpolator.get(); }
    const CatmullRomInterpolatorDVec3_T* interpolator() const { return mInterpolator.get(); }
    void setInterpolator(CatmullRomInterpolatorDVec3_T* interpolator) { mInterpolator = interpolator; }
  protected:
    ref<CatmullRomInterpolatorDVec3_T> mInterpolator;
  };
  //! Interpolates dvec2 values using a CatmullRomInterpolator.
  class CatmullRomInterpolatorDVec2: public InterpolatorDVec2
  {
    VL_INSTRUMENT_CLASS(vl::CatmullRomInterpolatorDVec2, InterpolatorDVec2)
  public:
    CatmullRomInterpolatorDVec2(): mInterpolator( new CatmullRomInterpolatorDVec2_T ) {}
    CatmullRomInterpolatorDVec2(const std::vector<dvec2>& path): mInterpolator( new CatmullRomInterpolatorDVec2_T(path) ) {}
    dvec2 computePoint(float t) const { return interpolator()->computePoint(t); }
    CatmullRomInterpolatorDVec2_T* interpolator() { return mInterpolator.get(); }
    const CatmullRomInterpolatorDVec2_T* interpolator() const { return mInterpolator.get(); }
    void setInterpolator(CatmullRomInterpolatorDVec2_T* interpolator) { mInterpolator = interpolator; }
  protected:
    ref<CatmullRomInterpolatorDVec2_T> mInterpolator;
  };
  //! Interpolates \p double values using a CatmullRomInterpolator.
  class CatmullRomInterpolatorDouble: public InterpolatorDouble
  {
    VL_INSTRUMENT_CLASS(vl::CatmullRomInterpolatorDouble, InterpolatorDouble)
  public:
    CatmullRomInterpolatorDouble(): mInterpolator( new CatmullRomInterpolatorDouble_T ) {}
    CatmullRomInterpolatorDouble(const std::vector<double>& path): mInterpolator( new CatmullRomInterpolatorDouble_T(path) ) {}
    double computePoint(float t) const { return interpolator()->computePoint(t); }
    CatmullRomInterpolatorDouble_T* interpolator() { return mInterpolator.get(); }
    const CatmullRomInterpolatorDouble_T* interpolator() const { return mInterpolator.get(); }
    void setInterpolator(CatmullRomInterpolatorDouble_T* interpolator) { mInterpolator = interpolator; }
  protected:
    ref<CatmullRomInterpolatorDouble_T> mInterpolator;
  };
}

#endif
