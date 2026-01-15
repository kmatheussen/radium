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

#ifndef Vector3_INCLUDE_ONCE
#define Vector3_INCLUDE_ONCE

#include <vlCore/Vector2.hpp>

namespace vl
{
  /**
   * The Vector3 class is a template class that implements a generic 3 components vector, see also vl::fvec3, vl::dvec3, vl::uvec3, vl::ivec3, vl::svec3, vl::usvec3, vl::bvec3, vl::ubvec3.
   * \sa Vector4, Vector2, Matrix4, Matrix3, Matrix2
   */
  template<typename T_Scalar>
  class Vector3
  {
  public:
    typedef T_Scalar scalar_type;
    static const int scalar_count = 3;
    Vector3(const Vector3& other) { *this = other; }
    Vector3() { x() = y() = z() = 0; }

    template<class T>
    explicit Vector3(const T& other)
    {
      x() = (T_Scalar)other.x();
      y() = (T_Scalar)other.y();
      z() = (T_Scalar)other.z();
    }

    explicit Vector3(T_Scalar x, T_Scalar y, T_Scalar z)
    {
      mScalar[0] = x;
      mScalar[1] = y;
      mScalar[2] = z;
    }

    explicit Vector3(const Vector2<T_Scalar>& v, T_Scalar z)
    {
      mScalar[0] = v.x();
      mScalar[1] = v.y();
      mScalar[2] = z;
    }

    T_Scalar* ptr() { return mScalar; }
    const T_Scalar* ptr() const { return mScalar; }

    const T_Scalar& x() const { return mScalar[0]; }
    const T_Scalar& y() const { return mScalar[1]; }
    const T_Scalar& z() const { return mScalar[2]; }

    T_Scalar& x() { return mScalar[0]; }
    T_Scalar& y() { return mScalar[1]; }
    T_Scalar& z() { return mScalar[2]; }

    const T_Scalar& r() const { return mScalar[0]; }
    const T_Scalar& g() const { return mScalar[1]; }
    const T_Scalar& b() const { return mScalar[2]; }

    T_Scalar& r() { return mScalar[0]; }
    T_Scalar& g() { return mScalar[1]; }
    T_Scalar& b() { return mScalar[2]; }

    const T_Scalar& s() const { return mScalar[0]; }
    const T_Scalar& t() const { return mScalar[1]; }
    const T_Scalar& p() const { return mScalar[2]; }

    T_Scalar& s() { return mScalar[0]; }
    T_Scalar& t() { return mScalar[1]; }
    T_Scalar& p() { return mScalar[2]; }

    Vector2<T_Scalar> xy() const { return Vector2<T_Scalar>(x(),y()); }
    Vector2<T_Scalar> st() const { return Vector2<T_Scalar>(x(),y()); }

    Vector3 operator+(const Vector3& other) const
    {
      return Vector3(x()+other.x(), y()+other.y(), z()+other.z());
    }
    Vector3 operator-(const Vector3& other) const
    {
      return Vector3(x()-other.x(), y()-other.y(), z()-other.z());
    }
    Vector3 operator*(const Vector3& other) const
    {
      return Vector3(x()*other.x(), y()*other.y(), z()*other.z());
    }
    Vector3 operator/(const Vector3& other) const
    {
      return Vector3(x()/other.x(), y()/other.y(), z()/other.z());
    }
    Vector3 operator+(T_Scalar val) const
    {
      return Vector3(x()+val, y()+val, z()+val);
    }
    Vector3 operator-(T_Scalar val) const
    {
      return Vector3(x()-val, y()-val, z()-val);
    }
    Vector3 operator*(T_Scalar val) const
    {
      return Vector3(x()*val, y()*val, z()*val);
    }
    Vector3 operator/(T_Scalar val) const
    {
      return Vector3(x()/val, y()/val, z()/val);
    }
    Vector3 operator-() const
    {
      return Vector3(-x(), -y(), -z());
    }
    Vector3& operator+=(const Vector3& other)
    {
      *this = *this + other;
      return *this;
    }
    Vector3& operator-=(const Vector3& other)
    {
      *this = *this - other;
      return *this;
    }
    Vector3& operator*=(const Vector3& other)
    {
      *this = *this * other;
      return *this;
    }
    Vector3& operator/=(const Vector3& other)
    {
      *this = *this / other;
      return *this;
    }
    Vector3& operator+=(T_Scalar val)
    {
      *this = *this + val;
      return *this;
    }
    Vector3& operator-=(T_Scalar val)
    {
      *this = *this - val;
      return *this;
    }
    Vector3& operator*=(T_Scalar val)
    {
      *this = *this * val;
      return *this;
    }
    Vector3& operator/=(T_Scalar val)
    {
      *this = *this / val;
      return *this;
    }
    Vector3& operator=(const Vector3& other)
    {
      x() = other.x();
      y() = other.y();
      z() = other.z();
      return *this;
    }
    Vector3& operator=(T_Scalar val)
    {
      x() = y() = z() = val;
      return *this;
    }
    bool operator==(const Vector3& other) const
    {
      return x() == other.x() && y() == other.y() && z() == other.z();
    }
    bool operator!=(const Vector3& other) const
    {
      return !operator==(other);
    }
    bool operator<(const Vector3& other) const
    {
      if (x() != other.x())
        return x() < other.x();
      else
      if (y() != other.y())
        return y() < other.y();
      else
        return z() < other.z();
    }
    T_Scalar& operator[](unsigned i) { return mScalar[i]; }
    const T_Scalar& operator[](unsigned i) const { return mScalar[i]; }
    T_Scalar length() const { return ::sqrt(x()*x()+y()*y()+z()*z()); }
    T_Scalar lengthSquared() const { return x()*x()+y()*y()+z()*z(); }
    bool isNull() const { return !x() && !y() && !z(); }
    const Vector3& normalize(T_Scalar *len=NULL)
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
  inline const Vector3<T> operator*(T val, const Vector3<T>& v)
  {
    return v * val;
  }

  //! A 3 components vector with \p int precision.
  typedef Vector3<int> ivec3;
  //! A 3 components vector with \p unsigned int precision.
  typedef Vector3<unsigned int> uvec3;
  //! A 3 components vector with \p float precision.
  typedef Vector3<float> fvec3;
  //! A 3 components vector with \p double precision.
  typedef Vector3<double> dvec3;
  //! A 3 components vector with \p char precision.
  typedef Vector3<char> bvec3;
  //! A 3 components vector with \p unsigned char precision.
  typedef Vector3<unsigned char> ubvec3;
  //! A 3 components vector with \p short precision.
  typedef Vector3<short> svec3;
  //! A 3 components vector with \p unsigned short precision.
  typedef Vector3<unsigned short> usvec3;

  #if VL_PIPELINE_PRECISION == 2
    //! Defined as: \p 'typedef \p dvec3 \p vec3'. See also \ref VL_PIPELINE_PRECISION.
    typedef dvec3 vec3;
  #else
    //! Defined as: \p 'typedef \p fvec3 \p vec3'. See also \ref VL_PIPELINE_PRECISION.
    typedef fvec3 vec3;
  #endif

  inline float dot(const fvec3& v1, const fvec3& v2) { return v1.x()*v2.x() + v1.y()*v2.y() + v1.z()*v2.z(); }
  inline double dot(const dvec3& v1, const dvec3& v2) { return v1.x()*v2.x() + v1.y()*v2.y() + v1.z()*v2.z(); }
  inline float dot(const ivec3& v1, const ivec3& v2) { return (float)(v1.x()*v2.x() + v1.y()*v2.y() + v1.z()*v2.z()); }
  inline float dot(const uvec3& v1, const uvec3& v2) { return (float)(v1.x()*v2.x() + v1.y()*v2.y() + v1.z()*v2.z()); }

  inline fvec3 cross(const fvec3& v1, const fvec3& v2)
  {
    fvec3 t;
    t.x() = v1.y()*v2.z() - v1.z()*v2.y();
    t.y() = v1.z()*v2.x() - v1.x()*v2.z();
    t.z() = v1.x()*v2.y() - v1.y()*v2.x();
    return t;
  }

  inline dvec3 cross(const dvec3& v1, const dvec3& v2)
  {
    dvec3 t;
    t.x() = v1.y()*v2.z() - v1.z()*v2.y();
    t.y() = v1.z()*v2.x() - v1.x()*v2.z();
    t.z() = v1.x()*v2.y() - v1.y()*v2.x();
    return t;
  }

  inline fvec3 min(const fvec3& a, const fvec3& b)
  {
    return fvec3( a.x() < b.x() ? a.x() : b.x(),
      a.y() < b.y() ? a.y() : b.y(),
      a.z() < b.z() ? a.z() : b.z() );
  }
  inline fvec3 min(const fvec3& a, float b)
  {
    return fvec3( a.x() < b ? a.x() : b,
      a.y() < b ? a.y() : b,
      a.z() < b ? a.z() : b );
  }
  inline dvec3 min(const dvec3& a, const dvec3& b)
  {
    return dvec3( a.x() < b.x() ? a.x() : b.x(),
      a.y() < b.y() ? a.y() : b.y(),
      a.z() < b.z() ? a.z() : b.z() );
  }
  inline dvec3 min(const dvec3& a, double b)
  {
    return dvec3( a.x() < b ? a.x() : b,
      a.y() < b ? a.y() : b,
      a.z() < b ? a.z() : b );
  }
  inline ivec3 min(const ivec3& a, const ivec3& b)
  {
    return ivec3( a.x() < b.x() ? a.x() : b.x(),
      a.y() < b.y() ? a.y() : b.y(),
      a.z() < b.z() ? a.z() : b.z() );
  }
  inline ivec3 min(const ivec3& a, int b)
  {
    return ivec3( a.x() < b ? a.x() : b,
      a.y() < b ? a.y() : b,
      a.z() < b ? a.z() : b );
  }
  inline uvec3 min(const uvec3& a, const uvec3& b)
  {
    return uvec3( a.x() < b.x() ? a.x() : b.x(),
      a.y() < b.y() ? a.y() : b.y(),
      a.z() < b.z() ? a.z() : b.z() );
  }
  inline uvec3 min(const uvec3& a, unsigned int b)
  {
    return uvec3( a.x() < b ? a.x() : b,
      a.y() < b ? a.y() : b,
      a.z() < b ? a.z() : b );
  }
  inline fvec3 max(const fvec3& a, const fvec3& b)
  {
    return fvec3( a.x() > b.x() ? a.x() : b.x(),
      a.y() > b.y() ? a.y() : b.y(),
      a.z() > b.z() ? a.z() : b.z() );
  }
  inline fvec3 max(const fvec3& a, float b)
  {
    return fvec3( a.x() > b ? a.x() : b,
      a.y() > b ? a.y() : b,
      a.z() > b ? a.z() : b );
  }
  inline dvec3 max(const dvec3& a, const dvec3& b)
  {
    return dvec3( a.x() > b.x() ? a.x() : b.x(),
      a.y() > b.y() ? a.y() : b.y(),
      a.z() > b.z() ? a.z() : b.z() );
  }
  inline dvec3 max(const dvec3& a, double b)
  {
    return dvec3( a.x() > b ? a.x() : b,
      a.y() > b ? a.y() : b,
      a.z() > b ? a.z() : b );
  }
  inline ivec3 max(const ivec3& a, const ivec3& b)
  {
    return ivec3( a.x() > b.x() ? a.x() : b.x(),
      a.y() > b.y() ? a.y() : b.y(),
      a.z() > b.z() ? a.z() : b.z() );
  }
  inline ivec3 max(const ivec3& a, int b)
  {
    return ivec3( a.x() > b ? a.x() : b,
      a.y() > b ? a.y() : b,
      a.z() > b ? a.z() : b );
  }
  inline uvec3 max(const uvec3& a, const uvec3& b)
  {
    return uvec3( a.x() > b.x() ? a.x() : b.x(),
      a.y() > b.y() ? a.y() : b.y(),
      a.z() > b.z() ? a.z() : b.z() );
  }
  inline uvec3 max(const uvec3& a, unsigned int b)
  {
    return uvec3( a.x() > b ? a.x() : b,
      a.y() > b ? a.y() : b,
      a.z() > b ? a.z() : b );
  }
  inline fvec3 clamp(const fvec3& x, float minval, float maxval) { return min(max(x,minval),maxval); }
  inline fvec3 clamp(const fvec3& x, const fvec3& minval, const fvec3& maxval) { return min(max(x,minval),maxval); }
  inline ivec3 clamp(const ivec3& x, const ivec3& minval, const ivec3& maxval) { return min(max(x,minval),maxval); }
  inline dvec3 clamp(const dvec3& x, double minval, double maxval) { return min(max(x,minval),maxval); }
  inline dvec3 clamp(const dvec3& x, const dvec3& minval, const dvec3& maxval) { return min(max(x,minval),maxval); }
  inline ivec3 clamp(const ivec3& x, int minval, int maxval) { return min(max(x,minval),maxval); }
  inline uvec3 clamp(const uvec3& x, unsigned int minval, unsigned int maxval) { return min(max(x,minval),maxval); }
  inline uvec3 clamp(const uvec3& x, const uvec3& minval, const uvec3& maxval) { return min(max(x,minval),maxval); }
}

#endif
