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

#ifndef Vector4_INCLUDE_ONCE
#define Vector4_INCLUDE_ONCE

#include <vlCore/Vector3.hpp>

namespace vl
{
  /**
   * The Vector4 class is a template class that implements a generic 4 components vector, see also vl::fvec4, vl::dvec4, vl::uvec4, vl::ivec4, vl::svec4, vl::usvec4, vl::bvec4, vl::ubvec4.
   * \sa Vector3, Vector2, Matrix4, Matrix3, Matrix2
   */
  template<typename T_Scalar>
  class Vector4
  {
  public:
    typedef T_Scalar scalar_type;
    static const int scalar_count = 4;
    Vector4(const Vector4& other) { *this = other; }
    Vector4() { x() = y() = z() = w() = 0; }

    template<class T>
    explicit Vector4(const T& other)
    {
      x() = (T_Scalar)other.x();
      y() = (T_Scalar)other.y();
      z() = (T_Scalar)other.z();
      w() = (T_Scalar)other.w();
    }

    explicit Vector4(T_Scalar x, T_Scalar y, T_Scalar z, T_Scalar w)
    {
      mScalar[0] = x;
      mScalar[1] = y;
      mScalar[2] = z;
      mScalar[3] = w;
    }

    explicit Vector4(const Vector3<T_Scalar>& v, T_Scalar w)
    {
      mScalar[0] = v.x();
      mScalar[1] = v.y();
      mScalar[2] = v.z();
      mScalar[3] = w;
    }

    explicit Vector4(const Vector2<T_Scalar>& u, const Vector2<T_Scalar>& v)
    {
      mScalar[0] = u.x();
      mScalar[1] = u.y();
      mScalar[2] = v.x();
      mScalar[3] = v.y();
    }

    T_Scalar* ptr() { return mScalar; }
    const T_Scalar* ptr() const { return mScalar; }

    const T_Scalar& x() const { return mScalar[0]; }
    const T_Scalar& y() const { return mScalar[1]; }
    const T_Scalar& z() const { return mScalar[2]; }
    const T_Scalar& w() const { return mScalar[3]; }

    T_Scalar& x() { return mScalar[0]; }
    T_Scalar& y() { return mScalar[1]; }
    T_Scalar& z() { return mScalar[2]; }
    T_Scalar& w() { return mScalar[3]; }

    const T_Scalar& r() const { return mScalar[0]; }
    const T_Scalar& g() const { return mScalar[1]; }
    const T_Scalar& b() const { return mScalar[2]; }
    const T_Scalar& a() const { return mScalar[3]; }

    T_Scalar& r() { return mScalar[0]; }
    T_Scalar& g() { return mScalar[1]; }
    T_Scalar& b() { return mScalar[2]; }
    T_Scalar& a() { return mScalar[3]; }

    const T_Scalar& s() const { return mScalar[0]; }
    const T_Scalar& t() const { return mScalar[1]; }
    const T_Scalar& p() const { return mScalar[2]; }
    const T_Scalar& q() const { return mScalar[3]; }

    T_Scalar& s() { return mScalar[0]; }
    T_Scalar& t() { return mScalar[1]; }
    T_Scalar& p() { return mScalar[2]; }
    T_Scalar& q() { return mScalar[3]; }

    Vector3<T_Scalar> xyz() const { return Vector3<T_Scalar>(x(),y(),z()); }
    Vector3<T_Scalar> rgb() const { return Vector3<T_Scalar>(x(),y(),z()); }
    Vector3<T_Scalar> stp() const { return Vector3<T_Scalar>(x(),y(),z()); }

    Vector2<T_Scalar> xy() const { return Vector2<T_Scalar>(x(),y()); }
    Vector2<T_Scalar> rg() const { return Vector2<T_Scalar>(x(),y()); }
    Vector2<T_Scalar> st() const { return Vector2<T_Scalar>(x(),y()); }

    Vector4 operator+(const Vector4& other) const
    {
      return Vector4(x()+other.x(), y()+other.y(), z()+other.z(), w()+other.w());
    }
    Vector4 operator-(const Vector4& other) const
    {
      return Vector4(x()-other.x(), y()-other.y(), z()-other.z(), w()-other.w());
    }
    Vector4 operator*(const Vector4& other) const
    {
      return Vector4(x()*other.x(), y()*other.y(), z()*other.z(), w()*other.w());
    }
    Vector4 operator/(const Vector4& other) const
    {
      return Vector4(x()/other.x(), y()/other.y(), z()/other.z(), w()/other.w());
    }
    Vector4 operator+(T_Scalar val) const
    {
      return Vector4(x()+val, y()+val, z()+val, w()+val);
    }
    Vector4 operator-(T_Scalar val) const
    {
      return Vector4(x()-val, y()-val, z()-val, w()-val);
    }
    Vector4 operator*(T_Scalar val) const
    {
      return Vector4(x()*val, y()*val, z()*val, w()*val);
    }
    Vector4 operator/(T_Scalar val) const
    {
      return Vector4(x()/val, y()/val, z()/val, w()/val);
    }
    Vector4 operator-() const
    {
      return Vector4(-x(), -y(), -z(), -w());
    }
    Vector4& operator+=(const Vector4& other)
    {
      *this = *this + other;
      return *this;
    }
    Vector4& operator-=(const Vector4& other)
    {
      *this = *this - other;
      return *this;
    }
    Vector4& operator*=(const Vector4& other)
    {
      *this = *this * other;
      return *this;
    }
    Vector4& operator/=(const Vector4& other)
    {
      *this = *this / other;
      return *this;
    }
    Vector4& operator+=(T_Scalar val)
    {
      *this = *this + val;
      return *this;
    }
    Vector4& operator-=(T_Scalar val)
    {
      *this = *this - val;
      return *this;
    }
    Vector4& operator*=(T_Scalar val)
    {
      *this = *this * val;
      return *this;
    }
    Vector4& operator/=(T_Scalar val)
    {
      *this = *this / val;
      return *this;
    }
    Vector4& operator=(const Vector4& other)
    {
      x() = other.x();
      y() = other.y();
      z() = other.z();
      w() = other.w();
      return *this;
    }
    Vector4& operator=(T_Scalar val)
    {
      x() = y() = z() = w() = val;
      return *this;
    }
    bool operator==(const Vector4& other) const
    {
      return x() == other.x() && y() == other.y() && z() == other.z() && w() == other.w();
    }
    bool operator!=(const Vector4& other) const
    {
      return !operator==(other);
    }
    bool operator<(const Vector4& other) const
    {
      if (x() != other.x())
        return x() < other.x();
      else
      if (y() != other.y())
        return y() < other.y();
      else
      if (z() != other.z())
        return z() < other.z();
      else
        return w() < other.w();
    }
    T_Scalar& operator[](unsigned i) { return mScalar[i]; }
    const T_Scalar& operator[](unsigned i) const { return mScalar[i]; }
    T_Scalar length() const { return (T_Scalar)::sqrt(x()*x()+y()*y()+z()*z()+w()*w()); }
    T_Scalar lengthSquared() const { return x()*x()+y()*y()+z()*z()+w()*w(); }
    bool isNull() const { return !x() && !y() && !z() && !w(); }
    const Vector4& normalize(T_Scalar *len=NULL) 
    {
      T_Scalar l = length();
      if (len)
        *len = l;
      if (l)
        *this *= (T_Scalar)(1.0/l); 
      return *this; 
    }

  protected:
    T_Scalar mScalar[scalar_count];
  };

  template<typename T>
  inline const Vector4<T> operator*(T val, const Vector4<T>& v)
  {
    return v * val;
  }

  //! A 4 components vector with \p int precision.
  typedef Vector4<int> ivec4;
  //! A 4 components vector with \p unsigned int precision.
  typedef Vector4<unsigned int> uvec4;
  //! A 4 components vector with \p float precision.
  typedef Vector4<float> fvec4;
  //! A 4 components vector with \p double precision.
  typedef Vector4<double> dvec4;
  //! A 4 components vector with \p char precision.
  typedef Vector4<char> bvec4;
  //! A 4 components vector with \p unsigned char precision.
  typedef Vector4<unsigned char> ubvec4;
  //! A 4 components vector with \p short precision.
  typedef Vector4<short> svec4;
  //! A 4 components vector with \p unsigned short precision.
  typedef Vector4<unsigned short> usvec4;

  #if VL_PIPELINE_PRECISION == 2
    //! Defined as: \p 'typedef \p dvec4 \p vec4'. See also \ref VL_PIPELINE_PRECISION.
    typedef dvec4 vec4;
  #else
    //! Defined as: \p 'typedef \p fvec4 \p vec4'. See also \ref VL_PIPELINE_PRECISION.
    typedef fvec4 vec4;
  #endif

  inline float dot(const fvec4& v1, const fvec4& v2) { return v1.x()*v2.x() + v1.y()*v2.y() + v1.z()*v2.z() + v1.w()*v2.w(); }
  inline double dot(const dvec4& v1, const dvec4& v2) { return v1.x()*v2.x() + v1.y()*v2.y() + v1.z()*v2.z() + v1.w()*v2.w(); }
  inline float dot(const ivec4& v1, const ivec4& v2) { return (float)(v1.x()*v2.x() + v1.y()*v2.y() + v1.z()*v2.z() + v1.w()*v2.w()); }
  inline float dot(const uvec4& v1, const uvec4& v2) { return (float)(v1.x()*v2.x() + v1.y()*v2.y() + v1.z()*v2.z() + v1.w()*v2.w()); }

  inline fvec4 min(const fvec4& a, const fvec4& b)
  {
    return fvec4( a.x() < b.x() ? a.x() : b.x(),
      a.y() < b.y() ? a.y() : b.y(),
      a.z() < b.z() ? a.z() : b.z(),
      a.w() < b.w() ? a.w() : b.w() );
  }
  inline fvec4 min(const fvec4& a, float b)
  {
    return fvec4( a.x() < b ? a.x() : b,
      a.y() < b ? a.y() : b,
      a.z() < b ? a.z() : b,
      a.w() < b ? a.w() : b );
  }
  inline dvec4 min(const dvec4& a, const dvec4& b)
  {
    return dvec4( a.x() < b.x() ? a.x() : b.x(),
      a.y() < b.y() ? a.y() : b.y(),
      a.z() < b.z() ? a.z() : b.z(),
      a.w() < b.w() ? a.w() : b.w() );
  }
  inline dvec4 min(const dvec4& a, double b)
  {
    return dvec4( a.x() < b ? a.x() : b,
      a.y() < b ? a.y() : b,
      a.z() < b ? a.z() : b,
      a.w() < b ? a.w() : b );
  }
  inline ivec4 min(const ivec4& a, const ivec4& b)
  {
    return ivec4( a.x() < b.x() ? a.x() : b.x(),
      a.y() < b.y() ? a.y() : b.y(),
      a.z() < b.z() ? a.z() : b.z(),
      a.w() < b.w() ? a.w() : b.w() );
  }
  inline ivec4 min(const ivec4& a, int b)
  {
    return ivec4( a.x() < b ? a.x() : b,
      a.y() < b ? a.y() : b,
      a.z() < b ? a.z() : b,
      a.w() < b ? a.w() : b );
  }
  inline uvec4 min(const uvec4& a, const uvec4& b)
  {
    return uvec4( a.x() < b.x() ? a.x() : b.x(),
      a.y() < b.y() ? a.y() : b.y(),
      a.z() < b.z() ? a.z() : b.z(),
      a.w() < b.w() ? a.w() : b.w() );
  }
  inline uvec4 min(const uvec4& a, unsigned int b)
  {
    return uvec4( a.x() < b ? a.x() : b,
      a.y() < b ? a.y() : b,
      a.z() < b ? a.z() : b,
      a.w() < b ? a.w() : b );
  }
  inline fvec4 max(const fvec4& a, const fvec4& b)
  {
    return fvec4( a.x() > b.x() ? a.x() : b.x(),
      a.y() > b.y() ? a.y() : b.y(),
      a.z() > b.z() ? a.z() : b.z(),
      a.w() > b.w() ? a.w() : b.w() );
  }
  inline fvec4 max(const fvec4& a, float b)
  {
    return fvec4( a.x() > b ? a.x() : b,
      a.y() > b ? a.y() : b,
      a.z() > b ? a.z() : b,
      a.w() > b ? a.w() : b );
  }
  inline dvec4 max(const dvec4& a, const dvec4& b)
  {
    return dvec4( a.x() > b.x() ? a.x() : b.x(),
      a.y() > b.y() ? a.y() : b.y(),
      a.z() > b.z() ? a.z() : b.z(),
      a.w() > b.w() ? a.w() : b.w() );
  }
  inline dvec4 max(const dvec4& a, double b)
  {
    return dvec4( a.x() > b ? a.x() : b,
      a.y() > b ? a.y() : b,
      a.z() > b ? a.z() : b,
      a.w() > b ? a.w() : b );
  }
  inline ivec4 max(const ivec4& a, const ivec4& b)
  {
    return ivec4( a.x() > b.x() ? a.x() : b.x(),
      a.y() > b.y() ? a.y() : b.y(),
      a.z() > b.z() ? a.z() : b.z(),
      a.w() > b.w() ? a.w() : b.w() );
  }
  inline ivec4 max(const ivec4& a, int b)
  {
    return ivec4( a.x() > b ? a.x() : b,
      a.y() > b ? a.y() : b,
      a.z() > b ? a.z() : b,
      a.w() > b ? a.w() : b );
  }
  inline uvec4 max(const uvec4& a, const uvec4& b)
  {
    return uvec4( a.x() > b.x() ? a.x() : b.x(),
      a.y() > b.y() ? a.y() : b.y(),
      a.z() > b.z() ? a.z() : b.z(),
      a.w() > b.w() ? a.w() : b.w() );
  }
  inline uvec4 max(const uvec4& a, unsigned int b)
  {
    return uvec4( a.x() > b ? a.x() : b,
      a.y() > b ? a.y() : b,
      a.z() > b ? a.z() : b,
      a.w() > b ? a.w() : b );
  }
  inline fvec4 clamp(const fvec4& x, float minval, float maxval) { return min(max(x,minval),maxval); }
  inline fvec4 clamp(const fvec4& x, const fvec4& minval, const fvec4& maxval) { return min(max(x,minval),maxval); }
  inline dvec4 clamp(const dvec4& x, double minval, double maxval) { return min(max(x,minval),maxval); }
  inline dvec4 clamp(const dvec4& x, const dvec4& minval, const dvec4& maxval) { return min(max(x,minval),maxval); }
  inline ivec4 clamp(const ivec4& x, int minval, int maxval) { return min(max(x,minval),maxval); }
  inline ivec4 clamp(const ivec4& x, const ivec4& minval, const ivec4& maxval) { return min(max(x,minval),maxval); }
  inline uvec4 clamp(const uvec4& x, unsigned int minval, unsigned int maxval) { return min(max(x,minval),maxval); }
  inline uvec4 clamp(const uvec4& x, const uvec4& minval, const uvec4& maxval) { return min(max(x,minval),maxval); }
}

#endif
