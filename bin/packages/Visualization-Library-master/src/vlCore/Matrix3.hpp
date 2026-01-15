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

#ifndef Matrix3_INCLUDE_ONCE
#define Matrix3_INCLUDE_ONCE

#include <vlCore/Vector3.hpp>
#include <vlCore/Matrix2.hpp>

namespace vl
{
  //-----------------------------------------------------------------------------
  // Matrix3
  //-----------------------------------------------------------------------------
  /**
   * The Matrix3 class is a template class that implements a generic 3x3 matrix, see also vl::dmat3, vl::fmat3, vl::umat3, vl::imat3.
   * \sa Vector4, Vector3, Vector2, Matrix4, Matrix2
   */
  template<typename T_Scalar>
  class Matrix3
  {
  public:
    typedef T_Scalar scalar_type;
    //-----------------------------------------------------------------------------
    template<typename T>
    explicit Matrix3(const Matrix3<T>& m)
    {
      e(0,0) = (T_Scalar)m.e(0,0); e(1,0) = (T_Scalar)m.e(1,0); e(2,0) = (T_Scalar)m.e(2,0);
      e(0,1) = (T_Scalar)m.e(0,1); e(1,1) = (T_Scalar)m.e(1,1); e(2,1) = (T_Scalar)m.e(2,1);
      e(0,2) = (T_Scalar)m.e(0,2); e(1,2) = (T_Scalar)m.e(1,2); e(2,2) = (T_Scalar)m.e(2,2);
    }
    //-----------------------------------------------------------------------------
    Matrix3()
    {
      setIdentity();
    }
    //-----------------------------------------------------------------------------
    explicit Matrix3(T_Scalar n)
    {
      setIdentity();
      e(0,0) = e(1,1) = e(2,2) = n;
    }
    //-----------------------------------------------------------------------------
    explicit Matrix3(T_Scalar e00, T_Scalar e01, T_Scalar e02,
                      T_Scalar e10, T_Scalar e11, T_Scalar e12,
                      T_Scalar e20, T_Scalar e21, T_Scalar e22)
    {
      e(0,0) = e00; e(0,1) = e01; e(0,2) = e02; 
      e(1,0) = e10; e(1,1) = e11; e(1,2) = e12; 
      e(2,0) = e20; e(2,1) = e21; e(2,2) = e22;
    }
    //-----------------------------------------------------------------------------
    Matrix3& fill(T_Scalar val)
    {
      e(0,0) = e(1,0) = e(2,0) = 
      e(0,1) = e(1,1) = e(2,1) = 
      e(0,2) = e(1,2) = e(2,2) = val;
      return *this;
    }
    //-----------------------------------------------------------------------------
    T_Scalar diff(const Matrix3& other) const
    {
      T_Scalar err = 0;
      for(int i=0; i<3; ++i)
        for(int j=0; j<3; ++j)
          if (e(j,i) > other.e(j,i)) // avoid fabs/abs
            err += e(j,i) - other.e(j,i);
          else
            err += other.e(j,i) - e(j,i);
      return err;
    }
    //-----------------------------------------------------------------------------
    Vector2<T_Scalar> getX() const
    {
      Vector2<T_Scalar> v;
      v.x() = e(0,0);
      v.y() = e(1,0);
      return v;
    }
    //-----------------------------------------------------------------------------
    Vector2<T_Scalar> getY() const
    {
      Vector2<T_Scalar> v;
      v.x() = e(0,1);
      v.y() = e(1,1);
      return v;
    }
    //-----------------------------------------------------------------------------
    Vector2<T_Scalar> getT() const
    {
      Vector2<T_Scalar> v;
      v.x() = e(0,2);
      v.y() = e(1,2);
      return v;
    }
    //-----------------------------------------------------------------------------
    Matrix3& setX(const Vector2<T_Scalar>& v) 
    {
      e(0,0) = v.x();
      e(1,0) = v.y();
      return *this;
    }
    //-----------------------------------------------------------------------------
    Matrix3& setY(const Vector2<T_Scalar>& v) 
    {
      e(0,1) = v.x();
      e(1,1) = v.y();
      return *this;
    }
    //-----------------------------------------------------------------------------
    Matrix3& setT(const Vector2<T_Scalar>& v) 
    {
      e(0,2) = v.x();
      e(1,2) = v.y();
      return *this;
    }
    //-----------------------------------------------------------------------------
    bool operator==(const Matrix3& m) const 
    {
      return memcmp(m.mVec, mVec, sizeof(T_Scalar)*9) == 0;
    }
    //-----------------------------------------------------------------------------
    bool operator!=(const Matrix3& m) const 
    {
      return !operator==(m);
    }
    //-----------------------------------------------------------------------------
    Matrix3& operator=(const Matrix3& m) 
    {
      memcpy(mVec, m.mVec, sizeof(T_Scalar)*9);
      return *this;
    }
    //-----------------------------------------------------------------------------
    Matrix3 operator+(const Matrix3& m) const
    {
      Matrix3 t;
      for(int i=0; i<3; ++i)
        for(int j=0; j<3; ++j)
          t.e(j,i) = e(j,i) + m.e(j,i);
      return t;
    }
    //-----------------------------------------------------------------------------
    Matrix3& operator+=(const Matrix3& m)
    {
      for(int i=0; i<3; ++i)
        for(int j=0; j<3; ++j)
          e(j,i) += m.e(j,i);
      return *this;
    }
    //-----------------------------------------------------------------------------
    Matrix3 operator-(const Matrix3& m) const
    {
      Matrix3 t;
      for(int i=0; i<3; ++i)
        for(int j=0; j<3; ++j)
          t.e(j,i) = e(j,i) - m.e(j,i);
      return t;
    }
    //-----------------------------------------------------------------------------
    Matrix3& operator-=(const Matrix3& m)
    {
      for(int i=0; i<3; ++i)
        for(int j=0; j<3; ++j)
          e(j,i) -= m.e(j,i);
      return *this;
    }
    //-----------------------------------------------------------------------------
    Matrix3& operator*=(const Matrix3& m)
    {
      return postMultiply(m);
    }
    //-----------------------------------------------------------------------------
    Matrix3 operator-() const
    {
      Matrix3 t;
      for(int i=0; i<3; ++i)
        for(int j=0; j<3; ++j)
          t.e(j,i) = -e(j,i);
      return t;
    }
    //-----------------------------------------------------------------------------
    Matrix3 operator+(T_Scalar d) const
    {
      Matrix3 t;
      for(int i=0; i<3; ++i)
        for(int j=0; j<3; ++j)
          t.e(j,i) = e(j,i) + d;
      return t;
    }
    //-----------------------------------------------------------------------------
    Matrix3& operator+=(T_Scalar d)
    {
      for(int i=0; i<3; ++i)
        for(int j=0; j<3; ++j)
          e(j,i) += d;
      return *this;
    }
    //-----------------------------------------------------------------------------
    Matrix3 operator-(T_Scalar d) const
    {
      Matrix3 t;
      for(int i=0; i<3; ++i)
        for(int j=0; j<3; ++j)
          t.e(j,i) = e(j,i) - d;
      return t;
    }
    //-----------------------------------------------------------------------------
    Matrix3& operator-=(T_Scalar d)
    {
      for(int i=0; i<3; ++i)
        for(int j=0; j<3; ++j)
          e(j,i) -= d;
      return *this;
    }
    //-----------------------------------------------------------------------------
    Matrix3 operator*(T_Scalar d) const
    {
      Matrix3 t;
      for(int i=0; i<3; ++i)
        for(int j=0; j<3; ++j)
          t.e(j,i) = e(j,i) * d;
      return t;
    }
    //-----------------------------------------------------------------------------
    Matrix3& operator*=(T_Scalar d)
    {
      for(int i=0; i<3; ++i)
        for(int j=0; j<3; ++j)
          e(j,i) *= d;
      return *this;
    }
    //-----------------------------------------------------------------------------
    Matrix3 operator/(T_Scalar d) const
    {
      d = (T_Scalar)1 / d;
      Matrix3 t;
      for(int i=0; i<3; ++i)
        for(int j=0; j<3; ++j)
          t.e(j,i) = e(j,i) * d;
      return t;
    }
    //-----------------------------------------------------------------------------
    Matrix3& operator/=(T_Scalar d)
    {
      d = (T_Scalar)1 / d;
      for(int i=0; i<3; ++i)
        for(int j=0; j<3; ++j)
          e(j,i) *= d;
      return *this;
    }
    //-----------------------------------------------------------------------------
    bool isIdentity() const
    {
      Matrix3 i;
      return memcmp(ptr(), i.ptr(), sizeof(T_Scalar)*9) == 0;
    }
    //-----------------------------------------------------------------------------
    Matrix2<T_Scalar> get2x2() const
    {
      Matrix2<T_Scalar> t;
      t.e(0,0) = e(0,0); t.e(1,0) = e(1,0);
      t.e(0,1) = e(0,1); t.e(1,1) = e(1,1);
      return t;
    }
    //-----------------------------------------------------------------------------
    //! This writes only on the upper 2x2 part of the matrix without touching the last row and column. 
    void set2x2(const Matrix2<T_Scalar>& m)
    {
      e(0,0) = m.e(0,0); e(1,0) = m.e(1,0);
      e(0,1) = m.e(0,1); e(1,1) = m.e(1,1);
    }
    //-----------------------------------------------------------------------------
    T_Scalar* ptr()
    {
      return &e(0,0);
    }
    //-----------------------------------------------------------------------------
    const T_Scalar* ptr() const
    {
      return &e(0,0);
    }
    //-----------------------------------------------------------------------------
    Matrix3& transpose()
    {
      T_Scalar tmp;
      for(int i=0; i<3; ++i)
        for(int j=i; j<3; ++j)
        {
          tmp = e(j,i);
          e(j,i) = e(i,j);
          e(i,j) = tmp;
        }
      return *this;
    }
    //-----------------------------------------------------------------------------
    Matrix3 getTransposed() const
    {
      Matrix3 m;
      for(int i=0; i<3; ++i)
        for(int j=0; j<3; ++j)
          m.e(j,i) = e(i,j);
      return m;
    }
    //-----------------------------------------------------------------------------
    Matrix3& getTransposed(Matrix3& dest) const
    {
      for(int i=0; i<3; ++i)
        for(int j=0; j<3; ++j)
          dest.e(j,i) = e(i,j);
      return dest;
    }
    //-----------------------------------------------------------------------------
    bool isNull() const
    {
      for(int i=0; i<3; ++i)
        for(int j=0; j<3; ++j)
          if(mVec[j][i] != 0)
            return false;
      return true;
    }
    //-----------------------------------------------------------------------------
    Matrix3& setNull() 
    {
      fill(0);
      return *this;
    }
    //-----------------------------------------------------------------------------
    static Matrix3& getNull(Matrix3& out)
    {
      out.fill(0);
      return out;
    }
    //-----------------------------------------------------------------------------
    static Matrix3 getNull()
    {
      return Matrix3().fill(0);
    }
    //-----------------------------------------------------------------------------
    Matrix3& setIdentity()
    {
      static const T_Scalar I3d[] = 
      { 
        (T_Scalar)1, (T_Scalar)0, (T_Scalar)0,  
        (T_Scalar)0, (T_Scalar)1, (T_Scalar)0, 
        (T_Scalar)0, (T_Scalar)0, (T_Scalar)1, 
      };
      memcpy(mVec, I3d, sizeof(T_Scalar)*9);
      return *this;
    }
    //-----------------------------------------------------------------------------
    static Matrix3 getIdentity()
    {
      return Matrix3();
    }
    //-----------------------------------------------------------------------------
    static Matrix3& getIdentity(Matrix3& out)
    {
      out.setIdentity();
      return out;
    }
    //-----------------------------------------------------------------------------
    T_Scalar getInverse(Matrix3& dest) const;
    //-----------------------------------------------------------------------------
    Matrix3 getInverse(T_Scalar *determinant=NULL) const
    {
      Matrix3 tmp;
      T_Scalar det = getInverse(tmp);
      if (determinant)
        *determinant = det;
      return tmp;
    }
    //-----------------------------------------------------------------------------
    Matrix3& invert(T_Scalar *determinant=NULL)
    {
      T_Scalar det = getInverse(*this);
      if (determinant)
        *determinant = det;
      return *this;
    }
    //-----------------------------------------------------------------------------
    static Matrix3 getRotation(T_Scalar degrees);
    //-----------------------------------------------------------------------------
    Matrix3& rotate(T_Scalar degrees)
    {
      return preMultiply(getRotation(degrees));
    }
    //-----------------------------------------------------------------------------
    static Matrix3& getTranslation(Matrix3& out, const Vector2<T_Scalar>& v)
    {
      return getTranslation(out, v.x(), v.y());
    }
    //-----------------------------------------------------------------------------
    static Matrix3 getTranslation(const Vector2<T_Scalar>& v)
    {
      return getTranslation(v.x(), v.y());
    }
    //-----------------------------------------------------------------------------
    static Matrix3 getTranslation(T_Scalar x, T_Scalar y)
    {
      Matrix3 m;
      return getTranslation(m, x, y);
    }
    //-----------------------------------------------------------------------------
    static Matrix3& getTranslation(Matrix3& out, T_Scalar x, T_Scalar y)
    {
      out.setIdentity();
      out.e(0,2) = x;
      out.e(1,2) = y;
      return out;
    }
    //-----------------------------------------------------------------------------
    Matrix3& translate(T_Scalar x, T_Scalar y)
    {
      return preMultiply(getTranslation(x,y));
    }
    //-----------------------------------------------------------------------------
    Matrix3& translate(const Vector2<T_Scalar>& v)
    {
      return preMultiply(getTranslation(v));
    }
    //-----------------------------------------------------------------------------
    static Matrix3& getScaling(Matrix3& out, const Vector2<T_Scalar>& v)
    {
      return getScaling(out, v.x(), v.y());
    }
    //-----------------------------------------------------------------------------
    static Matrix3 getScaling(const Vector2<T_Scalar>& v)
    {
      Matrix3 m;
      return getScaling(m, v.x(), v.y());
    }
    //-----------------------------------------------------------------------------
    static Matrix3 getScaling(T_Scalar x, T_Scalar y)
    {
      Matrix3 m;
      return getScaling(m, x, y);
    }
    //-----------------------------------------------------------------------------
    static Matrix3& getScaling(Matrix3& out, T_Scalar x, T_Scalar y)
    {
      out.setIdentity();
      out.e(0,0) = x;
      out.e(1,1) = y;
      return out;
    }
    //-----------------------------------------------------------------------------
    Matrix3& scale(T_Scalar x, T_Scalar y)
    {
      return preMultiply(getScaling(x,y));
    }
    //-----------------------------------------------------------------------------
    Matrix3& scale(const Vector2<T_Scalar>& v)
    {
      return preMultiply(getScaling(v.x(),v.y()));
    }
    //-----------------------------------------------------------------------------
    static Matrix3& multiply(Matrix3& out, const Matrix3& p, const Matrix3& q)
    {
      VL_CHECK(out.ptr() != p.ptr() && out.ptr() != q.ptr());

      out.e(0,0) = q.e(0,0)*p.e(0,0) + q.e(1,0)*p.e(0,1) + q.e(2,0)*p.e(0,2);
      out.e(0,1) = q.e(0,1)*p.e(0,0) + q.e(1,1)*p.e(0,1) + q.e(2,1)*p.e(0,2);
      out.e(0,2) = q.e(0,2)*p.e(0,0) + q.e(1,2)*p.e(0,1) + q.e(2,2)*p.e(0,2);

      out.e(1,0) = q.e(0,0)*p.e(1,0) + q.e(1,0)*p.e(1,1) + q.e(2,0)*p.e(1,2);
      out.e(1,1) = q.e(0,1)*p.e(1,0) + q.e(1,1)*p.e(1,1) + q.e(2,1)*p.e(1,2);
      out.e(1,2) = q.e(0,2)*p.e(1,0) + q.e(1,2)*p.e(1,1) + q.e(2,2)*p.e(1,2);

      out.e(2,0) = q.e(0,0)*p.e(2,0) + q.e(1,0)*p.e(2,1) + q.e(2,0)*p.e(2,2);
      out.e(2,1) = q.e(0,1)*p.e(2,0) + q.e(1,1)*p.e(2,1) + q.e(2,1)*p.e(2,2);
      out.e(2,2) = q.e(0,2)*p.e(2,0) + q.e(1,2)*p.e(2,1) + q.e(2,2)*p.e(2,2);

      return out;
    }
    //-----------------------------------------------------------------------------
    Matrix3& postMultiply(const Matrix3& m)
    {
      Matrix3<T_Scalar> t;
      return *this = multiply(t, *this, m);
    }
    //-----------------------------------------------------------------------------
    Matrix3& preMultiply(const Matrix3& m)
    {
      Matrix3<T_Scalar> t;
      return *this = multiply(t, m, *this);
    }
    //-----------------------------------------------------------------------------

    const T_Scalar& e(int i, int j) const { return mVec[j][i]; }
    T_Scalar& e(int i, int j) { return mVec[j][i]; }

  private:
    const Vector3<T_Scalar>& operator[](unsigned int i) const { VL_CHECK(i<3); return mVec[i]; }
    Vector3<T_Scalar>& operator[](unsigned int i) { VL_CHECK(i<3); return mVec[i]; }

  protected:
    Vector3<T_Scalar> mVec[3];
  };

  //-----------------------------------------------------------------------------
  // OPERATORS
  //-----------------------------------------------------------------------------
  template<typename T_Scalar>
  inline Matrix3<T_Scalar> operator*(const Matrix3<T_Scalar>& p, const Matrix3<T_Scalar>& q)
  {
    Matrix3<T_Scalar> t;
    Matrix3<T_Scalar>::multiply(t, p, q);
    return t;
  }
  //-----------------------------------------------------------------------------
  template<typename T_Scalar>
  inline Matrix3<T_Scalar> operator+(T_Scalar d, const Matrix3<T_Scalar>& m)
  {
    return m + d;
  }
  //-----------------------------------------------------------------------------
  template<typename T_Scalar>
  inline Matrix3<T_Scalar> operator*(T_Scalar d, const Matrix3<T_Scalar>& m)
  {
    return m * d;
  }
  //-----------------------------------------------------------------------------
  //! Post multiplication: matrix * column vector
  template<typename T_Scalar>
  inline Vector3<T_Scalar> operator*(const Matrix3<T_Scalar>& m, const Vector3<T_Scalar>& v)
  {
    Vector3<T_Scalar> t;
    t.x() = v.x()*m.e(0,0) + v.y()*m.e(0,1) + v.z()*m.e(0,2);
    t.y() = v.x()*m.e(1,0) + v.y()*m.e(1,1) + v.z()*m.e(1,2);
    t.z() = v.x()*m.e(2,0) + v.y()*m.e(2,1) + v.z()*m.e(2,2);
    return t;
  }
  //-----------------------------------------------------------------------------
  //! Post multiplication: matrix * column vector
  //! The incoming vector is considered a Vector3<T_Scalar> with the component z = 0
  template<typename T_Scalar>
  inline Vector2<T_Scalar> operator*(const Matrix3<T_Scalar>& m, const Vector2<T_Scalar>& v)
  {
    Vector2<T_Scalar> t;
    t.x() = v.x()*m.e(0,0) + v.y()*m.e(0,1) /*+ 0*m.e(0,2)*/;
    t.y() = v.x()*m.e(1,0) + v.y()*m.e(1,1) /*+ 0*m.e(1,2)*/;
    return t;
  }
  //-----------------------------------------------------------------------------
  //! pre-multiplication: row vector * matrix
  template<typename T_Scalar>
  inline Vector3<T_Scalar> operator*(const Vector3<T_Scalar>& v, const Matrix3<T_Scalar>& m)
  {
    Vector3<T_Scalar> t;
    t.x() = v.x()*m.e(0,0) + v.y()*m.e(1,0) + v.z()*m.e(2,0);
    t.y() = v.x()*m.e(0,1) + v.y()*m.e(1,1) + v.z()*m.e(2,1);
    t.z() = v.x()*m.e(0,2) + v.y()*m.e(1,2) + v.z()*m.e(2,2);
    return t;
  }
  //-----------------------------------------------------------------------------
  //! pre-multiplication: row vector * matrix
  //! The incoming vector is considered a Vector3<T_Scalar> with the component z = 0
  template<typename T_Scalar>
  inline Vector2<T_Scalar> operator*(const Vector2<T_Scalar>& v, const Matrix3<T_Scalar>& m)
  {
    Vector2<T_Scalar> t;
    t.x() = v.x()*m.e(0,0) + v.y()*m.e(1,0) /*+ 0*m.e(2,0)*/;
    t.y() = v.x()*m.e(0,1) + v.y()*m.e(1,1) /*+ 0*m.e(2,1)*/;
    return t;
  }
  //-----------------------------------------------------------------------------
  template<typename T_Scalar>
  Matrix3<T_Scalar> Matrix3<T_Scalar>::getRotation(T_Scalar degrees)
  {
    Matrix3<T_Scalar> rot;
    degrees = degrees * (T_Scalar)dDEG_TO_RAD;
    T_Scalar s = (T_Scalar) sin(degrees);
    T_Scalar c = (T_Scalar) cos(degrees);
    rot.e(0,0) = (T_Scalar)c;
    rot.e(1,1) = (T_Scalar)c;
    rot.e(1,0) = (T_Scalar)s;
    rot.e(0,1) = -(T_Scalar)s;
    return rot;
  }
  //-----------------------------------------------------------------------------
  template<typename T_Scalar>
  T_Scalar Matrix3<T_Scalar>::getInverse(Matrix3<T_Scalar>& dest) const
  {
    if (&dest == this)
    {
      Matrix3<T_Scalar> tmp;
      T_Scalar det = getInverse(tmp);
      dest = tmp;
      return det;
    }
    else
    {
      const T_Scalar& a11 = e(0,0);
      const T_Scalar& a21 = e(1,0);
      const T_Scalar& a31 = e(2,0);
      const T_Scalar& a12 = e(0,1);
      const T_Scalar& a22 = e(1,1);
      const T_Scalar& a32 = e(2,1);
      const T_Scalar& a13 = e(0,2);
      const T_Scalar& a23 = e(1,2);
      const T_Scalar& a33 = e(2,2);

      T_Scalar A = a22*a33 - a32*a23;
      T_Scalar B = a23*a31 - a33*a21;
      T_Scalar C = a21*a32 - a31*a22;

      T_Scalar det = a11*A + a12*B + a13*C;

      if (det == 0)
        dest.fill(0);
      else
        dest = Matrix3<T_Scalar>(A, a13*a32 - a33*a12, a12*a23 - a22*a13, 
                                 B, a11*a33 - a31*a13, a13*a21 - a23*a11,
                                 C, a12*a31 - a32*a11, a11*a22 - a21*a12) / det;
      return det;
    }
  }
  //-----------------------------------------------------------------------------

  //! A 3x3 matrix using \p double precision.
  typedef Matrix3<double> dmat3;
  //! A 3x3 matrix using \p float precision.
  typedef Matrix3<float>  fmat3;
  //! A 3x3 matrix using \p int precision.
  typedef Matrix3<int>    imat3;
  //! A 3x3 matrix using \p unsigned int precision.
  typedef Matrix3<unsigned int>  umat3;

  #if VL_PIPELINE_PRECISION == 2
    //! Defined as: \p 'typedef \p dmat3 \p mat3'. See also \ref VL_PIPELINE_PRECISION.
    typedef dmat3 mat3;
  #else
    //! Defined as: \p 'typedef \p fmat3 \p mat3'. See also \ref VL_PIPELINE_PRECISION.
    typedef fmat3 mat3;
  #endif
}

#endif
