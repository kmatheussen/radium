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

#ifndef Matrix4_INCLUDE_ONCE
#define Matrix4_INCLUDE_ONCE

#include <vlCore/Vector4.hpp>
#include <vlCore/Matrix3.hpp>

namespace vl
{
  //-----------------------------------------------------------------------------
  // Matrix4
  //-----------------------------------------------------------------------------
  /**
   * The Matrix4 class is a template class that implements a generic 4x4 matrix, see also vl::dmat4, vl::fmat4, vl::umat4, vl::imat4
   * \sa Vector4, Vector3, Vector2, Matrix3, Matrix2
   */
  template<typename T_Scalar>
  class Matrix4
  {
  public:
    typedef T_Scalar scalar_type;
    //-----------------------------------------------------------------------------
    template<typename T>
    explicit Matrix4(const Matrix4<T>& m)
    {
      e(0,0) = (T_Scalar)m.e(0,0); e(1,0) = (T_Scalar)m.e(1,0); e(2,0) = (T_Scalar)m.e(2,0); e(3,0) = (T_Scalar)m.e(3,0);
      e(0,1) = (T_Scalar)m.e(0,1); e(1,1) = (T_Scalar)m.e(1,1); e(2,1) = (T_Scalar)m.e(2,1); e(3,1) = (T_Scalar)m.e(3,1);
      e(0,2) = (T_Scalar)m.e(0,2); e(1,2) = (T_Scalar)m.e(1,2); e(2,2) = (T_Scalar)m.e(2,2); e(3,2) = (T_Scalar)m.e(3,2);
      e(0,3) = (T_Scalar)m.e(0,3); e(1,3) = (T_Scalar)m.e(1,3); e(2,3) = (T_Scalar)m.e(2,3); e(3,3) = (T_Scalar)m.e(3,3);
    }
    //-----------------------------------------------------------------------------
    Matrix4()
    {
      setIdentity();
    }
    //-----------------------------------------------------------------------------
    explicit Matrix4(T_Scalar n)
    {
      setIdentity();
      e(0,0) = e(1,1) = e(2,2) = e(3,3) = n; 
    }
    //-----------------------------------------------------------------------------
    explicit Matrix4(T_Scalar* val)
    {
      fillPtr(val);
    }
    //-----------------------------------------------------------------------------
    explicit Matrix4( T_Scalar e00, T_Scalar e01, T_Scalar e02, T_Scalar e03,
                      T_Scalar e10, T_Scalar e11, T_Scalar e12, T_Scalar e13,
                      T_Scalar e20, T_Scalar e21, T_Scalar e22, T_Scalar e23,
                      T_Scalar e30, T_Scalar e31, T_Scalar e32, T_Scalar e33)
    {
      e(0,0) = e00; e(0,1) = e01; e(0,2) = e02; e(0,3) = e03;
      e(1,0) = e10; e(1,1) = e11; e(1,2) = e12; e(1,3) = e13;
      e(2,0) = e20; e(2,1) = e21; e(2,2) = e22; e(2,3) = e23;
      e(3,0) = e30; e(3,1) = e31; e(3,2) = e32; e(3,3) = e33;
    }
    //-----------------------------------------------------------------------------
    Matrix4& fill(T_Scalar val)
    {
      e(0,0) = e(1,0) = e(2,0) = e(3,0) = 
      e(0,1) = e(1,1) = e(2,1) = e(3,1) = 
      e(0,2) = e(1,2) = e(2,2) = e(3,2) = 
      e(0,3) = e(1,3) = e(2,3) = e(3,3) = val;
      return *this;
    }
    //-----------------------------------------------------------------------------
    Matrix4& fillPtr(T_Scalar* val)
    {
      memcpy(ptr(), val, sizeof(T_Scalar)*16);
      return *this;
    }
    //-----------------------------------------------------------------------------
    T_Scalar diff(const Matrix4& other) const
    {
      T_Scalar err = 0;
      for(int i=0; i<4; ++i)
        for(int j=0; j<4; ++j)
          if (e(j,i) > other.e(j,i)) // avoid fabs/abs
            err += e(j,i) - other.e(j,i);
          else
            err += other.e(j,i) - e(j,i);
      return err;
    }
    //-----------------------------------------------------------------------------
    Vector3<T_Scalar> getX() const
    {
      return Vector3<T_Scalar>(mVec[0].x(), mVec[0].y(), mVec[0].z());
    }
    //-----------------------------------------------------------------------------
    Vector3<T_Scalar> getY() const
    {
      return Vector3<T_Scalar>(mVec[1].x(), mVec[1].y(), mVec[1].z());
    }
    //-----------------------------------------------------------------------------
    Vector3<T_Scalar> getZ() const
    {
      return Vector3<T_Scalar>(mVec[2].x(), mVec[2].y(), mVec[2].z());
    }
    //-----------------------------------------------------------------------------
    Vector3<T_Scalar> getT() const
    {
      return Vector3<T_Scalar>(mVec[3].x(), mVec[3].y(), mVec[3].z());
    }
    //-----------------------------------------------------------------------------
    Matrix4& setX(const Vector3<T_Scalar>& v) 
    {
      mVec[0].x() = v.x();
      mVec[0].y() = v.y();
      mVec[0].z() = v.z();
      return *this;
    }
    //-----------------------------------------------------------------------------
    Matrix4& setY(const Vector3<T_Scalar>& v) 
    {
      mVec[1].x() = v.x();
      mVec[1].y() = v.y();
      mVec[1].z() = v.z();
      return *this;
    }
    //-----------------------------------------------------------------------------
    Matrix4& setZ(const Vector3<T_Scalar>& v) 
    {
      mVec[2].x() = v.x();
      mVec[2].y() = v.y();
      mVec[2].z() = v.z();
      return *this;
    }
    //-----------------------------------------------------------------------------
    Matrix4& setT(const Vector3<T_Scalar>& v) 
    {
      mVec[3].x() = v.x();
      mVec[3].y() = v.y();
      mVec[3].z() = v.z();
      return *this;
    }
    //-----------------------------------------------------------------------------
    bool operator==(const Matrix4& m) const
    {
      return memcmp(m.mVec, mVec, sizeof(T_Scalar)*4*4) == 0;
    }
    //-----------------------------------------------------------------------------
    bool operator!=(const Matrix4& m) const
    {
      return !operator==(m);
    }
    //-----------------------------------------------------------------------------
    Matrix4& operator=(const Matrix4& m)
    {
      memcpy(mVec, m.mVec, sizeof(T_Scalar)*16);
      return *this;
    }
    //-----------------------------------------------------------------------------
    Matrix4 operator+(const Matrix4& m) const
    {
      Matrix4 t;
      for(int i=0; i<4; ++i)
        for(int j=0; j<4; ++j)
          t.e(j,i) = e(j,i) + m.e(j,i);

      return t;
    }
    //-----------------------------------------------------------------------------
    Matrix4& operator+=(const Matrix4& m)
    {
      for(int i=0; i<4; ++i)
        for(int j=0; j<4; ++j)
          e(j,i) += m.e(i,j);
      return *this;
    }
    //-----------------------------------------------------------------------------
    Matrix4 operator-(const Matrix4& m) const
    {
      Matrix4 t;
      for(int i=0; i<4; ++i)
        for(int j=0; j<4; ++j)
          t.e(j,i) = e(j,i) - m.e(j,i);

      return t;
    }
    //-----------------------------------------------------------------------------
    Matrix4& operator-=(const Matrix4& m)
    {
      for(int i=0; i<4; ++i)
        for(int j=0; j<4; ++j)
          e(j,i) -= m.e(i,j);
      return *this;
    }
    //-----------------------------------------------------------------------------
    Matrix4& operator*=(const Matrix4& m)
    {
      return postMultiply(m);
    }
    //-----------------------------------------------------------------------------
    Matrix4 operator-() const
    {
      Matrix4 t;
      for(int i=0; i<4; ++i)
        for(int j=0; j<4; ++j)
          t.e(j,i) = -e(j,i);
      return t;
    }
    //-----------------------------------------------------------------------------
    Matrix4 operator+(T_Scalar d) const
    {
      Matrix4 t;
      for(int i=0; i<4; ++i)
        for(int j=0; j<4; ++j)
          t.e(j,i) = e(j,i) + d;
      return t;
    }
    //-----------------------------------------------------------------------------
    Matrix4& operator+=(T_Scalar d)
    {
      for(int i=0; i<4; ++i)
        for(int j=0; j<4; ++j)
          e(j,i) += d;
      return *this;
    }
    //-----------------------------------------------------------------------------
    Matrix4 operator-(T_Scalar d) const
    {
      Matrix4 t;
      for(int i=0; i<4; ++i)
        for(int j=0; j<4; ++j)
          t.e(j,i) = e(j,i) - d;
      return t;
    }
    //-----------------------------------------------------------------------------
    Matrix4& operator-=(T_Scalar d)
    {
      for(int i=0; i<4; ++i)
        for(int j=0; j<4; ++j)
          e(j,i) -= d;
      return *this;
    }
    //-----------------------------------------------------------------------------
    Matrix4 operator*(T_Scalar d) const
    {
      Matrix4 t;
      for(int i=0; i<4; ++i)
        for(int j=0; j<4; ++j)
          t.e(j,i) = e(j,i) * d;
      return t;
    }
    //-----------------------------------------------------------------------------
    Matrix4& operator*=(T_Scalar d)
    {
      for(int i=0; i<4; ++i)
        for(int j=0; j<4; ++j)
          e(j,i) *= d;
      return *this;
    }
    //-----------------------------------------------------------------------------
    Matrix4 operator/(T_Scalar d) const
    {
      d = (T_Scalar)1 / d;
      Matrix4 t;
      for(int i=0; i<4; ++i)
        for(int j=0; j<4; ++j)
          t.e(j,i) = e(j,i) * d;
      return t;
    }
    //-----------------------------------------------------------------------------
    Matrix4& operator/=(T_Scalar d)
    {
      d = (T_Scalar)1 / d;
      for(int i=0; i<4; ++i)
        for(int j=0; j<4; ++j)
          e(j,i) *= d;
      return *this;
    }
    //-----------------------------------------------------------------------------
    bool isIdentity() const
    {
      Matrix4 i;
      return memcmp(ptr(), i.ptr(), sizeof(T_Scalar)*16) == 0;
    }
    //-----------------------------------------------------------------------------
    Matrix4 as3x3() const
    {
      Matrix4 t = *this;
      t[0][3] = 0;
      t[1][3] = 0;
      t[2][3] = 0;
      t[3][3] = 1;
      t[3][0] = 0;
      t[3][1] = 0;
      t[3][2] = 0;
      return t;
    }
    //-----------------------------------------------------------------------------
    Matrix3<T_Scalar> get3x3() const
    {
      Matrix3<T_Scalar> t;
      t.e(0,0) = e(0,0); t.e(1,0) = e(1,0); t.e(2,0) = e(2,0);
      t.e(0,1) = e(0,1); t.e(1,1) = e(1,1); t.e(2,1) = e(2,1);
      t.e(0,2) = e(0,2); t.e(1,2) = e(1,2); t.e(2,2) = e(2,2);
      return t;
    }
    //-----------------------------------------------------------------------------
    //! This writes only on the upper 3x3 part of the matrix without touching the last row and column. 
    void set3x3(const Matrix3<T_Scalar>& m)
    {
      e(0,0) = m.e(0,0); e(1,0) = m.e(1,0); e(2,0) = m.e(2,0);
      e(0,1) = m.e(0,1); e(1,1) = m.e(1,1); e(2,1) = m.e(2,1);
      e(0,2) = m.e(0,2); e(1,2) = m.e(1,2); e(2,2) = m.e(2,2);
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
    Matrix4& transpose()
    {
      T_Scalar tmp;
      for(int i=0; i<4; ++i)
        for(int j=i; j<4; ++j)
        {
          tmp = e(j,i);
          e(j,i) = e(i,j);
          e(i,j) = tmp;
        }
      return *this;
    }
    //-----------------------------------------------------------------------------
    Matrix4 getTransposed() const
    {
      Matrix4 m;
      for(int i=0; i<4; ++i)
        for(int j=0; j<4; ++j)
          m.e(j,i) = e(i,j);
      return m;
    }
    //-----------------------------------------------------------------------------
    Matrix4& getTransposed(Matrix4& dest) const
    {
      for(int i=0; i<4; ++i)
        for(int j=0; j<4; ++j)
          dest.e(j,i) = e(i,j);
      return dest;
    }
    //-----------------------------------------------------------------------------
    bool isNull() const
    {
      for(int i=0; i<4; ++i)
        for(int j=0; j<4; ++j)
          if(e(i,j) != 0)
            return false;
      return true;
    }
    //-----------------------------------------------------------------------------
    Matrix4& setNull() 
    {
      fill(0);
      return *this;
    }
    //-----------------------------------------------------------------------------
    static Matrix4& getNull(Matrix4& out)
    {
      out.fill(0);
      return out;
    }
    //-----------------------------------------------------------------------------
    static Matrix4 getNull()
    {
      return Matrix4().fill(0);
    }
    //-----------------------------------------------------------------------------
    Matrix4& setIdentity()
    {
      static const T_Scalar I4d[] = 
      {
        (T_Scalar)1, (T_Scalar)0, (T_Scalar)0, (T_Scalar)0, 
        (T_Scalar)0, (T_Scalar)1, (T_Scalar)0, (T_Scalar)0, 
        (T_Scalar)0, (T_Scalar)0, (T_Scalar)1, (T_Scalar)0, 
        (T_Scalar)0, (T_Scalar)0, (T_Scalar)0, (T_Scalar)1 
      };
      memcpy(mVec, I4d, sizeof(T_Scalar)*16);
      return *this;
    }
    //-----------------------------------------------------------------------------
    static Matrix4 getIdentity()
    {
      return Matrix4();
    }
    //-----------------------------------------------------------------------------
    static Matrix4& getIdentity(Matrix4& out)
    {
      out.setIdentity();
      return out;
    }
    //-----------------------------------------------------------------------------
    T_Scalar getInverse(Matrix4& dest) const;
    //-----------------------------------------------------------------------------
    Matrix4 getInverse(T_Scalar *determinant=NULL) const
    {
      Matrix4 tmp;
      T_Scalar det = getInverse(tmp);
      if (determinant)
        *determinant = det;
      return tmp;
    }
    //-----------------------------------------------------------------------------
    Matrix4& invert(T_Scalar *determinant=NULL)
    {
      T_Scalar det = getInverse(*this);
      if (determinant)
        *determinant = det;
      return *this;
    }
    //-----------------------------------------------------------------------------
    static Matrix4 getPerspective(T_Scalar fovy, T_Scalar aspect_ratio, T_Scalar znear, T_Scalar zfar);
    //-----------------------------------------------------------------------------
    static Matrix4 getFrustum(T_Scalar pleft, T_Scalar pright, T_Scalar pbottom, T_Scalar ptop, T_Scalar pnear, T_Scalar pfar);
    //-----------------------------------------------------------------------------
    static Matrix4 getOrtho(T_Scalar pleft, T_Scalar pright, T_Scalar pbottom, T_Scalar ptop, T_Scalar pnear, T_Scalar pfar);
    //-----------------------------------------------------------------------------
    static Matrix4 getOrtho2D(T_Scalar pleft, T_Scalar pright, T_Scalar pbottom, T_Scalar ptop);
    //-----------------------------------------------------------------------------
    static Matrix4 getLookAtModeling(const Vector3<T_Scalar>& eye, const Vector3<T_Scalar>& at, const Vector3<T_Scalar>& up);
    //-----------------------------------------------------------------------------
    static Matrix4 getLookAt(const Vector3<T_Scalar>& eye, const Vector3<T_Scalar>& at, const Vector3<T_Scalar>& up);
    //-----------------------------------------------------------------------------
    void getAsLookAtModeling(Vector3<T_Scalar>& eye, Vector3<T_Scalar>& at, Vector3<T_Scalar>& up, Vector3<T_Scalar>& right) const;
    //-----------------------------------------------------------------------------
    void getAsLookAt(Vector3<T_Scalar>& eye, Vector3<T_Scalar>& at, Vector3<T_Scalar>& up, Vector3<T_Scalar>& right) const;
    //-----------------------------------------------------------------------------
    void getYXRotationAngles(T_Scalar& degrees_y, T_Scalar& degrees_x) const;
    //-----------------------------------------------------------------------------
    static Matrix4& getRotation(Matrix4& out, T_Scalar degrees, T_Scalar x, T_Scalar y, T_Scalar z);
    //-----------------------------------------------------------------------------
    static Matrix4 getRotation(T_Scalar degrees, T_Scalar x, T_Scalar y, T_Scalar z)
    {
      Matrix4 m;
      return getRotation(m, degrees, x, y, z);
    }
    //-----------------------------------------------------------------------------
    static Matrix4 getRotation(T_Scalar degrees, const Vector3<T_Scalar>& v)
    {
      return getRotation(degrees, v.x(), v.y(), v.z());
    }
    //-----------------------------------------------------------------------------
    static Matrix4 getRotation(T_Scalar degrees1, const Vector3<T_Scalar>& v1, T_Scalar degrees2, const Vector3<T_Scalar>& v2)
    {
      return getRotation(degrees1, v1.x(), v1.y(), v1.z()) * getRotation(degrees2, v2.x(), v2.y(), v2.z());
    }
    //-----------------------------------------------------------------------------
    static Matrix4 getRotation(T_Scalar degrees1, const Vector3<T_Scalar>& v1, T_Scalar degrees2, const Vector3<T_Scalar>& v2, T_Scalar degrees3, const Vector3<T_Scalar>& v3)
    {
      return getRotation(degrees1, v1.x(), v1.y(), v1.z()) * getRotation(degrees2, v2.x(), v2.y(), v2.z()) * getRotation(degrees3, v3.x(), v3.y(), v3.z());
    }
    //-----------------------------------------------------------------------------
    Matrix4& rotate(T_Scalar degrees, const Vector3<T_Scalar>& v)
    {
      return rotate(degrees, v.x(), v.y(), v.z());
    }
    //-----------------------------------------------------------------------------
    Matrix4& rotate(T_Scalar degrees, T_Scalar x, T_Scalar y, T_Scalar z)
    {
      return preMultiply(getRotation(degrees, x, y, z));
    }
    //-----------------------------------------------------------------------------
    Matrix4& rotate(T_Scalar degrees1, const Vector3<T_Scalar>& v1, T_Scalar degrees2, const Vector3<T_Scalar>& v2)
    {
      return preMultiply(getRotation(degrees1, v1, degrees2, v2));
    }
    //-----------------------------------------------------------------------------
    Matrix4& rotate(T_Scalar degrees1, const Vector3<T_Scalar>& v1, T_Scalar degrees2, const Vector3<T_Scalar>& v2, T_Scalar degrees3, const Vector3<T_Scalar>& v3)
    {
      return preMultiply(getRotation(degrees1, v1, degrees2, v2, degrees3, v3));
    }
    //-----------------------------------------------------------------------------
    static Matrix4 getRotationXYZ(T_Scalar degX, T_Scalar degY, T_Scalar degZ)
    {
      return getRotation(degX, 1,0,0) * getRotation(degY, 0,1,0) * getRotation(degZ, 0,0,1);
    }
    //-----------------------------------------------------------------------------
    Matrix4& rotateXYZ(T_Scalar degX, T_Scalar degY, T_Scalar degZ)
    {
      return preMultiply(getRotationXYZ(degX, degY, degZ));
    }
    //-----------------------------------------------------------------------------
    static Matrix4 getRotationZYX(T_Scalar degZ, T_Scalar degY, T_Scalar degX)
    {
      return getRotation(degZ, 0,0,1) * getRotation(degY, 0,1,0) * getRotation(degX, 1,0,0);
    }
    //-----------------------------------------------------------------------------
    Matrix4& rotateZYX(T_Scalar degZ, T_Scalar degY, T_Scalar degX)
    {
      return preMultiply(getRotationZYX(degZ, degY, degX));
    }
    //-----------------------------------------------------------------------------
    static Matrix4& getRotation(Matrix4& out, const Vector3<T_Scalar>& from, const Vector3<T_Scalar>& to);
    //-----------------------------------------------------------------------------
    static Matrix4 getRotation(const Vector3<T_Scalar>& from, const Vector3<T_Scalar>& to)
    {
      Matrix4 m;
      return getRotation(m, from, to);
    }
    //-----------------------------------------------------------------------------
    Matrix4& rotate(const Vector4<T_Scalar>& from, const Vector4<T_Scalar>& to)
    {
      return preMultiply(getRotation(from, to));
    }
    //-----------------------------------------------------------------------------
    Matrix4& rotate(const Vector3<T_Scalar>& from, const Vector3<T_Scalar>& to)
    {
      return preMultiply(getRotation(from, to));
    }
    //-----------------------------------------------------------------------------
    static Matrix4& getTranslation(Matrix4&out, const Vector3<T_Scalar>& v)
    {
      return getTranslation(out, v.x(), v.y(), v.z());
    }
    //-----------------------------------------------------------------------------
    static Matrix4 getTranslation(const Vector3<T_Scalar>& v)
    {
      Matrix4 m;
      return getTranslation(m, v.x(), v.y(), v.z());
    }
    //-----------------------------------------------------------------------------
    static Matrix4 getTranslation(T_Scalar x, T_Scalar y, T_Scalar z)
    {
      Matrix4 m;
      return getTranslation(m, x, y, z);
    }
    //-----------------------------------------------------------------------------
    static Matrix4& getTranslation(Matrix4& out, T_Scalar x, T_Scalar y, T_Scalar z)
    {
      out.setIdentity();
      out.e(0,3) = x;
      out.e(1,3) = y;
      out.e(2,3) = z;
      return out;
    }
    //-----------------------------------------------------------------------------
    Matrix4& translate(T_Scalar x, T_Scalar y, T_Scalar z)
    {
      return preMultiply(getTranslation(x,y,z));
    }
    //-----------------------------------------------------------------------------
    Matrix4& translate(const Vector3<T_Scalar>& v)
    {
      return preMultiply(getTranslation(v));
    }
    //-----------------------------------------------------------------------------
    static Matrix4& getScaling(Matrix4& out, const Vector3<T_Scalar>& v)
    {
      return getScaling(out, v.x(), v.y(), v.z());
    }
    //-----------------------------------------------------------------------------
    static Matrix4 getScaling(const Vector3<T_Scalar>& v)
    {
      Matrix4 m;
      return getScaling(m, v.x(), v.y(), v.z());
    }
    //-----------------------------------------------------------------------------
    static Matrix4 getScaling(T_Scalar x, T_Scalar y, T_Scalar z)
    {
      Matrix4 m;
      return getScaling(m, x, y, z);
    }
    //-----------------------------------------------------------------------------
    static Matrix4& getScaling(Matrix4& out, T_Scalar x, T_Scalar y, T_Scalar z)
    {
      out.setIdentity();
      out.e(0,0) = x;
      out.e(1,1) = y;
      out.e(2,2) = z;
      return out;
    }
    //-----------------------------------------------------------------------------
    Matrix4& scale(T_Scalar x, T_Scalar y, T_Scalar z)
    {
      return preMultiply(getScaling(x,y,z));
    }
    //-----------------------------------------------------------------------------
    Matrix4& scale(const Vector3<T_Scalar> v)
    {
      return preMultiply(getScaling(v.x(), v.y(), v.z()));
    }
    //-----------------------------------------------------------------------------
    static Matrix4& multiply(Matrix4& out, const Matrix4& p, const Matrix4& q)
    {
      VL_CHECK(out.ptr() != p.ptr() && out.ptr() != q.ptr());

      out.e(0,0) = q.e(0,0)*p.e(0,0) + q.e(1,0)*p.e(0,1) + q.e(2,0)*p.e(0,2) + q.e(3,0)*p.e(0,3);
      out.e(0,1) = q.e(0,1)*p.e(0,0) + q.e(1,1)*p.e(0,1) + q.e(2,1)*p.e(0,2) + q.e(3,1)*p.e(0,3);
      out.e(0,2) = q.e(0,2)*p.e(0,0) + q.e(1,2)*p.e(0,1) + q.e(2,2)*p.e(0,2) + q.e(3,2)*p.e(0,3);
      out.e(0,3) = q.e(0,3)*p.e(0,0) + q.e(1,3)*p.e(0,1) + q.e(2,3)*p.e(0,2) + q.e(3,3)*p.e(0,3);

      out.e(1,0) = q.e(0,0)*p.e(1,0) + q.e(1,0)*p.e(1,1) + q.e(2,0)*p.e(1,2) + q.e(3,0)*p.e(1,3);
      out.e(1,1) = q.e(0,1)*p.e(1,0) + q.e(1,1)*p.e(1,1) + q.e(2,1)*p.e(1,2) + q.e(3,1)*p.e(1,3);
      out.e(1,2) = q.e(0,2)*p.e(1,0) + q.e(1,2)*p.e(1,1) + q.e(2,2)*p.e(1,2) + q.e(3,2)*p.e(1,3);
      out.e(1,3) = q.e(0,3)*p.e(1,0) + q.e(1,3)*p.e(1,1) + q.e(2,3)*p.e(1,2) + q.e(3,3)*p.e(1,3);

      out.e(2,0) = q.e(0,0)*p.e(2,0) + q.e(1,0)*p.e(2,1) + q.e(2,0)*p.e(2,2) + q.e(3,0)*p.e(2,3);
      out.e(2,1) = q.e(0,1)*p.e(2,0) + q.e(1,1)*p.e(2,1) + q.e(2,1)*p.e(2,2) + q.e(3,1)*p.e(2,3);
      out.e(2,2) = q.e(0,2)*p.e(2,0) + q.e(1,2)*p.e(2,1) + q.e(2,2)*p.e(2,2) + q.e(3,2)*p.e(2,3);
      out.e(2,3) = q.e(0,3)*p.e(2,0) + q.e(1,3)*p.e(2,1) + q.e(2,3)*p.e(2,2) + q.e(3,3)*p.e(2,3);

      out.e(3,0) = q.e(0,0)*p.e(3,0) + q.e(1,0)*p.e(3,1) + q.e(2,0)*p.e(3,2) + q.e(3,0)*p.e(3,3);
      out.e(3,1) = q.e(0,1)*p.e(3,0) + q.e(1,1)*p.e(3,1) + q.e(2,1)*p.e(3,2) + q.e(3,1)*p.e(3,3);
      out.e(3,2) = q.e(0,2)*p.e(3,0) + q.e(1,2)*p.e(3,1) + q.e(2,2)*p.e(3,2) + q.e(3,2)*p.e(3,3);
      out.e(3,3) = q.e(0,3)*p.e(3,0) + q.e(1,3)*p.e(3,1) + q.e(2,3)*p.e(3,2) + q.e(3,3)*p.e(3,3);

      return out;
    }
    //-----------------------------------------------------------------------------
    Matrix4& postMultiply(const Matrix4& m)
    {
      Matrix4<T_Scalar> t;
      return *this = multiply(t, *this, m);
    }
    //-----------------------------------------------------------------------------
    Matrix4& preMultiply(const Matrix4& m)
    {
      Matrix4<T_Scalar> t;
      return *this = multiply(t, m, *this);
    }
    //-----------------------------------------------------------------------------

    const T_Scalar& e(int i, int j) const { return mVec[j][i]; }
    T_Scalar& e(int i, int j) { return mVec[j][i]; }

  private:
    const Vector4<T_Scalar>& operator[](unsigned int i) const { VL_CHECK(i<4); return mVec[i]; }
    Vector4<T_Scalar>& operator[](unsigned int i) { VL_CHECK(i<4); return mVec[i]; }

  protected:
    Vector4<T_Scalar> mVec[4];
  };
  //-----------------------------------------------------------------------------
  // OPERATORS
  //-----------------------------------------------------------------------------
  template<typename T_Scalar>
  inline Matrix4<T_Scalar> operator*(const Matrix4<T_Scalar>& p, const Matrix4<T_Scalar>& q)
  {
    Matrix4<T_Scalar> t;
    Matrix4<T_Scalar>::multiply(t, p, q);
    return t;
  }
  //-----------------------------------------------------------------------------
  template<typename T_Scalar>
  inline Matrix4<T_Scalar> operator+(T_Scalar d, const Matrix4<T_Scalar>& m)
  {
    return m + d;
  }
  //-----------------------------------------------------------------------------
  template<typename T_Scalar>
  inline Matrix4<T_Scalar> operator*(T_Scalar d, const Matrix4<T_Scalar>& m)
  {
    return m * d;
  }
  //-----------------------------------------------------------------------------
  //! Post multiplication: matrix * column vector
  template<typename T_Scalar>
  inline Vector4<T_Scalar> operator*(const Matrix4<T_Scalar>& m, const Vector4<T_Scalar>& v)
  {
    return Vector4<T_Scalar>(
      v.x()*m.e(0,0) + v.y()*m.e(0,1) + v.z()*m.e(0,2) + v.w()*m.e(0,3),
      v.x()*m.e(1,0) + v.y()*m.e(1,1) + v.z()*m.e(1,2) + v.w()*m.e(1,3),
      v.x()*m.e(2,0) + v.y()*m.e(2,1) + v.z()*m.e(2,2) + v.w()*m.e(2,3),
      v.x()*m.e(3,0) + v.y()*m.e(3,1) + v.z()*m.e(3,2) + v.w()*m.e(3,3)
   );
  }
  //-----------------------------------------------------------------------------
  //! Post multiplication: matrix * column vector
  //! The incoming vector is considered a Vector4<T_Scalar> with the component w = 1
  template<typename T_Scalar>
  inline Vector3<T_Scalar> operator*(const Matrix4<T_Scalar>& m, const Vector3<T_Scalar>& v)
  {
    return Vector3<T_Scalar>(
      v.x()*m.e(0,0) + v.y()*m.e(0,1) + v.z()*m.e(0,2) + /*1**/m.e(0,3),
      v.x()*m.e(1,0) + v.y()*m.e(1,1) + v.z()*m.e(1,2) + /*1**/m.e(1,3),
      v.x()*m.e(2,0) + v.y()*m.e(2,1) + v.z()*m.e(2,2) + /*1**/m.e(2,3)
   );
  }
  //-----------------------------------------------------------------------------
  //! Post multiplication: matrix * column vector
  //! The incoming vector is considered a Vector4<T_Scalar> with components: z = 0 and w = 1
  template<typename T_Scalar>
  inline Vector2<T_Scalar> operator*(const Matrix4<T_Scalar>& m, const Vector2<T_Scalar>& v)
  {
    return Vector2<T_Scalar>(
      v.x()*m.e(0,0) + v.y()*m.e(0,1) + /*0*m.e(0,2) +*/ /*1**/m.e(0,3),
      v.x()*m.e(1,0) + v.y()*m.e(1,1) + /*0*m.e(1,2) +*/ /*1**/m.e(1,3)
   );
  }
  //-----------------------------------------------------------------------------
  //! pre-multiplication: row vector * matrix
  template<typename T_Scalar>
  inline Vector4<T_Scalar> operator*(const Vector4<T_Scalar>& v, const Matrix4<T_Scalar>& m)
  {
    return Vector4<T_Scalar>(
      v.x()*m.e(0,0) + v.y()*m.e(1,0) + v.z()*m.e(2,0) + v.w()*m.e(3,0),
      v.x()*m.e(0,1) + v.y()*m.e(1,1) + v.z()*m.e(2,1) + v.w()*m.e(3,1),
      v.x()*m.e(0,2) + v.y()*m.e(1,2) + v.z()*m.e(2,2) + v.w()*m.e(3,2),
      v.x()*m.e(0,3) + v.y()*m.e(1,3) + v.z()*m.e(2,3) + v.w()*m.e(3,3)
   );
  }
  //-----------------------------------------------------------------------------
  //! pre-multiplication: row vector * matrix
  //! The incoming vector is considered a Vector4<T_Scalar> with the component w = 1
  template<typename T_Scalar>
  inline Vector3<T_Scalar> operator*(const Vector3<T_Scalar>& v, const Matrix4<T_Scalar>& m)
  {
    return Vector3<T_Scalar>(
      v.x()*m.e(0,0) + v.y()*m.e(1,0) + v.z()*m.e(2,0) + /*1**/m.e(3,0),
      v.x()*m.e(0,1) + v.y()*m.e(1,1) + v.z()*m.e(2,1) + /*1**/m.e(3,1),
      v.x()*m.e(0,2) + v.y()*m.e(1,2) + v.z()*m.e(2,2) + /*1**/m.e(3,2)
   );
  }
  //-----------------------------------------------------------------------------
  //! pre-multiplication: row vector * matrix
  //! The incoming vector is considered a Vector4<T_Scalar> with components: z = 0 and w = 1
  template<typename T_Scalar>
  inline Vector2<T_Scalar> operator*(const Vector2<T_Scalar>& v, const Matrix4<T_Scalar>& m)
  {
    return Vector2<T_Scalar>(
      v.x()*m.e(0,0) + v.y()*m.e(1,0) + /*0*m.e(2,0) +*/ /*1**/m.e(3,0),
      v.x()*m.e(0,1) + v.y()*m.e(1,1) + /*0*m.e(2,1) +*/ /*1**/m.e(3,1)
   );
  }
  //-----------------------------------------------------------------------------
  template<typename T_Scalar>
  Matrix4<T_Scalar> Matrix4<T_Scalar>::getLookAtModeling(const Vector3<T_Scalar>& eye, const Vector3<T_Scalar>& at, const Vector3<T_Scalar>& up)
  {
    Vector3<T_Scalar> zaxis = (eye-at).normalize();
    Vector3<T_Scalar> xaxis = cross(up, zaxis).normalize();
    Vector3<T_Scalar> yaxis = cross(zaxis, xaxis);

    // look at modeling
    T_Scalar la_modeling[] =
    {
       xaxis.x()          , xaxis.y()          , xaxis.z()          , 0,
       yaxis.x()          , yaxis.y()          , yaxis.z()          , 0,
       zaxis.x()          , zaxis.y()          , zaxis.z()          , 0,
       eye.x()            , eye.y()            , eye.z()            , 1
    };

    return Matrix4<T_Scalar>(la_modeling);
  }
  //-----------------------------------------------------------------------------
  template<typename T_Scalar>
  Matrix4<T_Scalar> Matrix4<T_Scalar>::getLookAt(const Vector3<T_Scalar>& eye, const Vector3<T_Scalar>& at, const Vector3<T_Scalar>& up)
  {
    Vector3<T_Scalar> zaxis = (eye-at).normalize();
    Vector3<T_Scalar> xaxis = cross(up, zaxis).normalize();
    Vector3<T_Scalar> yaxis = cross(zaxis, xaxis);

    // look at view
    T_Scalar la_view[] =
    {
       xaxis.x()          , yaxis.x()          , zaxis.x()          , 0,
       xaxis.y()          , yaxis.y()          , zaxis.y()          , 0,
       xaxis.z()          , yaxis.z()          , zaxis.z()          , 0,
       -dot(xaxis,eye), -dot(yaxis,eye), -dot(zaxis,eye), 1
    };

    return Matrix4<T_Scalar>(la_view);
  }
  //-----------------------------------------------------------------------------
  template<typename T_Scalar>
  void Matrix4<T_Scalar>::getAsLookAtModeling(Vector3<T_Scalar>& eye, Vector3<T_Scalar>& at, Vector3<T_Scalar>& up, Vector3<T_Scalar>& right) const
  {
    eye = getT();

    at = -getZ();
    // look.normalize();

    up = getY();
    // up.normalize();

    right = getX();
    // right.normalize();
  }
  //-----------------------------------------------------------------------------
  template<typename T_Scalar>
  void Matrix4<T_Scalar>::getAsLookAt(Vector3<T_Scalar>& eye, Vector3<T_Scalar>& at, Vector3<T_Scalar>& up, Vector3<T_Scalar>& right) const
  {
    Matrix4<T_Scalar> m = *this;
    m.invert();
    m.getAsLookAtModeling(eye, at, up, right);
  }
  //-----------------------------------------------------------------------------
  template<typename T_Scalar>
  Matrix4<T_Scalar> Matrix4<T_Scalar>::getPerspective(T_Scalar fovy, T_Scalar aspect_ratio, T_Scalar znear, T_Scalar zfar)
  {
    Matrix4<T_Scalar> m;

    T_Scalar rads = (fovy / ((T_Scalar)2)) * (T_Scalar)dDEG_TO_RAD;
    T_Scalar dz = zfar - znear;
    T_Scalar sa = sin(rads);
    if ((dz == 0) || (sa == 0) || (aspect_ratio == 0)) 
      return m * 0;
    T_Scalar ctan = cos(rads) / sa;

    m.e(0,0) = (T_Scalar)(ctan / aspect_ratio);
    m.e(1,1) = (T_Scalar)(ctan);
    m.e(2,2) = (T_Scalar)(-(zfar + znear) / dz);
    m.e(3,2) = -((T_Scalar)1);
    m.e(2,3) = (T_Scalar)(-((T_Scalar)2) * znear * zfar / dz);
    m.e(3,3) = 0;

    return m;
  }
  //-----------------------------------------------------------------------------
  template<typename T_Scalar>
  Matrix4<T_Scalar> Matrix4<T_Scalar>::getFrustum(T_Scalar left, T_Scalar right, T_Scalar bottom, T_Scalar top, T_Scalar pnear, T_Scalar pfar)
  {
    Matrix4<T_Scalar> m;

    if (pnear <= 0 || pfar <= 0 || pnear == pfar || left == right || top == bottom)
      return m * 0;

    T_Scalar x =  (((T_Scalar)2)*pnear)  / (right-left);
    T_Scalar y =  (((T_Scalar)2)*pnear)  / (top-bottom);
    T_Scalar a =  (right+left) / (right-left);
    T_Scalar b =  (top+bottom) / (top-bottom);
    T_Scalar c = -(pfar+pnear)   / (pfar-pnear);
    T_Scalar d = -(((T_Scalar)2)*pfar*pnear) / (pfar-pnear);

    m.e(0,0) = x;  m.e(0,1) = 0;  m.e(0,2) = a;    m.e(0,3) = 0;
    m.e(1,0) = 0;  m.e(1,1) = y;  m.e(1,2) = b;    m.e(1,3) = 0;
    m.e(2,0) = 0;  m.e(2,1) = 0;  m.e(2,2) = c;    m.e(2,3) = d;
    m.e(3,0) = 0;  m.e(3,1) = 0;  m.e(3,2) = -((T_Scalar)1); m.e(3,3) = 0;

    return m;
  }
  //-----------------------------------------------------------------------------
  template<typename T_Scalar>
  Matrix4<T_Scalar> Matrix4<T_Scalar>::getOrtho(T_Scalar left, T_Scalar right, T_Scalar bottom, T_Scalar top, T_Scalar pnear, T_Scalar pfar)
  {
    Matrix4<T_Scalar> m;

    m.e(0,0) = ((T_Scalar)2) / (right-left);
    m.e(0,1) = 0;
    m.e(0,2) = 0;
    m.e(0,3) = -(right+left) / (right-left);

    m.e(1,0) = 0;
    m.e(1,1) = ((T_Scalar)2) / (top-bottom);
    m.e(1,2) = 0;
    m.e(1,3) = -(top+bottom) / (top-bottom);

    m.e(2,0) = 0;
    m.e(2,1) = 0;
    m.e(2,2) = -((T_Scalar)2) / (pfar-pnear);
    m.e(2,3) = -(pfar+pnear) / (pfar-pnear);

    m.e(3,0) = 0;
    m.e(3,1) = 0;
    m.e(3,2) = 0;
    m.e(3,3) = ((T_Scalar)1);

    return m;
  }
  //-----------------------------------------------------------------------------
  template<typename T_Scalar>
  Matrix4<T_Scalar> Matrix4<T_Scalar>::getOrtho2D(T_Scalar left, T_Scalar right, T_Scalar bottom, T_Scalar top)
  {
    return getOrtho(left, right, bottom, top, -1, +1);
  }
  //-----------------------------------------------------------------------------
  template<typename T_Scalar>
  Matrix4<T_Scalar>& Matrix4<T_Scalar>::getRotation(Matrix4<T_Scalar>& out, T_Scalar degrees, T_Scalar x, T_Scalar y, T_Scalar z)
  {
    out.setIdentity();

    if (degrees == 0 || (x == 0 && y ==0 && z == 0))
      return out;
      
    degrees = T_Scalar(degrees * dDEG_TO_RAD);

    T_Scalar xx, yy, zz, xy, yz, zx, xs, ys, zs, one_c, s, c;

    s = (T_Scalar) sin(degrees);
    c = (T_Scalar) cos(degrees);

    // simple cases
    if (x == 0) 
    {
      if (y == 0) 
      {
        if (z != 0) 
        {
          // rotate only around z-axis
          out.e(0,0) = (T_Scalar)c;
          out.e(1,1) = (T_Scalar)c;
          if (z < 0) 
          {
            out.e(1,0) = -(T_Scalar)s;
            out.e(0,1) = (T_Scalar)s;
          }
          else 
          {
            out.e(1,0) = (T_Scalar)s;
            out.e(0,1) = -(T_Scalar)s;
          }
          return out;
        }
      }
      else if (z == 0) 
      {
        // rotate only around y-axis
        out.e(0,0) = (T_Scalar)c;
        out.e(2,2) = (T_Scalar)c;
        if (y < 0) 
        {
          out.e(2,0) = (T_Scalar)s;
          out.e(0,2) = -(T_Scalar)s;
        }
        else 
        {
          out.e(2,0) = -(T_Scalar)s;
          out.e(0,2) = (T_Scalar)s;
        }
        return out;
      }
    }
    else if (y == 0) 
    {
      if (z == 0) 
      {
        // rotate only around x-axis
        out.e(1,1) = (T_Scalar)c;
        out.e(2,2) = (T_Scalar)c;
        if (x < 0) 
        {
          out.e(2,1) = -(T_Scalar)s;
          out.e(1,2) = (T_Scalar)s;
        }
        else 
        {
          out.e(2,1) = (T_Scalar)s;
          out.e(1,2) = -(T_Scalar)s;
        }
        return out;
      }
    }

    // Beginning of general axisa to matrix conversion
    T_Scalar dot = x*x + y*y + z*z;

    if (dot > (T_Scalar)((T_Scalar)1.0001) || dot < (T_Scalar)0.99999) 
    {
      T_Scalar mag = (T_Scalar) sqrt(dot);
      x /= mag;
      y /= mag;
      z /= mag;
    }

    xx = x *x;
    yy = y * y;
    zz = z * z;
    xy = x * y;
    yz = y * z;
    zx = z * x;
    xs = x * s;
    ys = y * s;
    zs = z * s;
    one_c = ((T_Scalar)1) - c;

    out.e(0,0) = (T_Scalar)((one_c * xx) + c); out.e(1,0) = (T_Scalar)((one_c * xy) + zs); out.e(2,0) = (T_Scalar)((one_c * zx) - ys);
    out.e(0,1) = (T_Scalar)((one_c * xy) - zs); out.e(1,1) = (T_Scalar)((one_c * yy) + c); out.e(2,1) = (T_Scalar)((one_c * yz) + xs);
    out.e(0,2) = (T_Scalar)((one_c * zx) + ys); out.e(1,2) = (T_Scalar)((one_c * yz) - xs); out.e(2,2) = (T_Scalar)((one_c * zz) + c);
    return out;
  }
  //-----------------------------------------------------------------------------
  template<typename T_Scalar>
  T_Scalar Matrix4<T_Scalar>::getInverse(Matrix4<T_Scalar>& dest) const
  {
    const T_Scalar* in = ptr();
    T_Scalar* out      = dest.ptr();

    // | 0 | 4 | 8  | 12 |
    // | 1 | 5 | 9  | 13 |
    // | 2 | 6 | 10 | 14 |
    // | 3 | 7 | 11 | 15 |

    // | a | b | c | d |
    // | e | f | g | h |
    // | i | l | m | n |
    // | o | p | q | r |

    const T_Scalar a = in[0]; const T_Scalar b = in[4]; const T_Scalar c = in[ 8]; const T_Scalar d = in[12];
    const T_Scalar e = in[1]; const T_Scalar f = in[5]; const T_Scalar g = in[ 9]; const T_Scalar h = in[13];
    const T_Scalar i = in[2]; const T_Scalar l = in[6]; const T_Scalar m = in[10]; const T_Scalar n = in[14];
    const T_Scalar o = in[3]; const T_Scalar p = in[7]; const T_Scalar q = in[11]; const T_Scalar r = in[15];

    // 3x3 determinant:
    //
    //   [ a b c ]
    //   [ d e f ] = aei - ahf + dhc - dbi + gbf - gec = (aei + dhc + gbf) - (ahf + dbi + gec)
    //   [ g h i ]

    const T_Scalar mr = m*r;
    const T_Scalar gn = g*n;
    const T_Scalar el = e*l;
    const T_Scalar ip = i*p;
    const T_Scalar mo = m*o;
    const T_Scalar hl = h*l;
    const T_Scalar mp = m*p;
    const T_Scalar nq = n*q;
    const T_Scalar gl = g*l;
    const T_Scalar no = n*o;
    const T_Scalar gi = g*i;
    const T_Scalar np = n*p;
    const T_Scalar fi = f*i;
    const T_Scalar rc = r*c;
    const T_Scalar be = b*e;
    const T_Scalar af = a*f;
    const T_Scalar de = d*e;
    const T_Scalar df = d*f;
    const T_Scalar ch = c*h;
    const T_Scalar qh = q*h;

    // | f | g | h |
    // | l | m | n |
    // | p | q | r |
    T_Scalar Ca = +(( f*mr + gn*p + hl*q ) - ( h*mp + nq*f + r*gl ));

    // | e | g | h |
    // | i | m | n |
    // | o | q | r |
    T_Scalar Cb = -(( e*mr + gn*o + i*qh ) - ( h*mo + gi*r + nq*e ));

    // | e | f | h |
    // | i | l | n |
    // | o | p | r |
    T_Scalar Cc = +(( el*r + ip*h + f*no ) - ( hl*o + np*e + fi*r ));

    // | e | f | g |
    // | i | l | m |
    // | o | p | q |
    T_Scalar Cd = -(( el*q + f*mo + g*ip ) - ( gl*o + mp*e + q*fi ));

    T_Scalar det = a*Ca + b*Cb + c*Cc + d*Cd;

    // singular matrix
    if (det == 0)
      return det;

    // | b | c | d |
    // | l | m | n |
    // | p | q | r |
    T_Scalar Ce = -(( b*mr + c*np + d*l*q ) - ( d*mp + nq*b + rc*l ));

    // | a | c | d |
    // | i | m | n |
    // | o | q | r |
    T_Scalar Cf = +(( a*mr + c*no + d*i*q ) - ( d*mo + nq*a + rc*i ));

    // | a | b | d |
    // | i | l | n |
    // | o | p | r |
    T_Scalar Cg = -(( a*l*r + b*no + d*ip ) - ( d*l*o + np*a + r*b*i ));

    // | a | b | c |
    // | i | l | m |
    // | o | p | q |
    T_Scalar Ch = +(( a*l*q + b*mo + c*ip ) - ( c*l*o + mp*a + q*b*i ));


    // | b | c | d |
    // | f | g | h |
    // | p | q | r |
    T_Scalar Ci = +(( b*g*r + ch*p + df*q ) - ( d*g*p + q*h*b + rc*f ));

    // | a | c | d |
    // | e | g | h |
    // | o | q | r |
    T_Scalar Cl = -(( a*g*r + ch*o + de*q ) - ( d*g*o  + qh*a + rc*e ));

    // | a | b | d |
    // | e | f | h |
    // | o | p | r |
    T_Scalar Cm = +(( af*r + b*h*o + de*p ) - ( df*o + h*p*a + r*be ));

    // | a | b | c |
    // | e | f | g |
    // | o | p | q |
    T_Scalar Cn = -(( af*q + b*g*o + c*e*p ) - ( c*f*o + g*p*a + q*be ));


    // | b | c | d |
    // | f | g | h |
    // | l | m | n |
    T_Scalar Co = -(( b*gn + c*hl + df*m ) - ( d*gl + h*m*b + n*c*f ));

    // | a | c | d |
    // | e | g | h |
    // | i | m | n |
    T_Scalar Cp = +(( a*gn + ch*i + de*m ) - ( d*gi + h*m*a + n*c*e ));

    // | a | b | d |
    // | e | f | h |
    // | i | l | n |
    T_Scalar Cq = -(( af*n + b*h*i + d*el ) - ( d*fi + hl*a + n*be ));

    // | a | b | c |
    // | e | f | g |
    // | i | l | m |
    T_Scalar Cr = +(( af*m + b*gi + c*el ) - ( c*fi + gl*a + m*be ));

#if 0
    T_Scalar det2 = e*Ce + f*Cf + g*Cg + h*Ch;
    T_Scalar det3 = i*Ci + l*Cl + m*Cm + n*Cn;
    T_Scalar det4 = o*Co + p*Cp + q*Cq + r*Cr;
    VL_CHECK( fabs(det - det1) < 0.0001 );
    VL_CHECK( fabs(det - det3) < 0.0001 );
    VL_CHECK( fabs(det - det4) < 0.0001 );
#endif

    T_Scalar inv_det = 1 / det;

    out[0]  = inv_det * Ca;
    out[1]  = inv_det * Cb;
    out[2]  = inv_det * Cc;
    out[3]  = inv_det * Cd;
    out[4]  = inv_det * Ce;
    out[5]  = inv_det * Cf;
    out[6]  = inv_det * Cg;
    out[7]  = inv_det * Ch;
    out[8]  = inv_det * Ci;
    out[9]  = inv_det * Cl;
    out[10] = inv_det * Cm;
    out[11] = inv_det * Cn;
    out[12] = inv_det * Co;
    out[13] = inv_det * Cp;
    out[14] = inv_det * Cq;
    out[15] = inv_det * Cr;

    return det;
  }
  //-----------------------------------------------------------------------------
  template<typename T_Scalar>
  Matrix4<T_Scalar>& Matrix4<T_Scalar>::getRotation(Matrix4<T_Scalar>& out, const Vector3<T_Scalar>& from, const Vector3<T_Scalar>& to)
  {
    Vector3<T_Scalar> a,b;
    a = from;
    b = to;
    a.normalize();
    b.normalize();
    T_Scalar cosa = dot(a,b);
    cosa = clamp(cosa,-((T_Scalar)1),+((T_Scalar)1));
    Vector3<T_Scalar> axis,n2;
    axis = cross(a,b);
    axis.normalize();
    T_Scalar alpha = acos(cosa);
    return getRotation(out, alpha*(T_Scalar)dRAD_TO_DEG, axis.x(), axis.y(), axis.z());
  }
  //-----------------------------------------------------------------------------
  //! If this matrix can be represented as \p RY(degrees_y) * \p RX(degrees_x), where 
  //! RX and RY are getRotation matrices around the X and Y axis respectively, this 
  //! function returns the getRotation angles \p degrees_y and \p degrees_x.
  //! \note This function can only retrieve angles that satisfy the following conditions:
  //! - -180 <= degrees_y <= 180
  //! - -180 <= degrees_x <= 180 and degrees_x != 90
  template<typename T_Scalar>
  void Matrix4<T_Scalar>::getYXRotationAngles(T_Scalar& degrees_y, T_Scalar& degrees_x) const
  {
    Vector3<T_Scalar> vx = getX();
    Vector3<T_Scalar> vy = getY();
    Vector3<T_Scalar> vz = getZ();

    vx.normalize();
    vy.normalize();
    vz.normalize();

    T_Scalar kx = dot(vy,Vector3<T_Scalar>(0,1,0));
    kx = clamp(kx,-((T_Scalar)1),+((T_Scalar)1));
    degrees_x = acos(kx) * (T_Scalar)dRAD_TO_DEG;
    if(dot(vz, Vector3<T_Scalar>(0,1,0)) > 0)
      degrees_x = -degrees_x;

    T_Scalar ky = dot(vx, Vector3<T_Scalar>(1,0,0));
    ky = clamp(ky,-((T_Scalar)1),+((T_Scalar)1));
    degrees_y = acos(ky) * (T_Scalar)dRAD_TO_DEG;
    if(dot(vz, Vector3<T_Scalar>(1,0,0)) < 0)
      degrees_y = -degrees_y;
    if (fabs(degrees_x) > (T_Scalar)90)
      degrees_y = -degrees_y;
  }

  //-----------------------------------------------------------------------------

  //! A 4x4 matrix using \p double precision.
  typedef Matrix4<double> dmat4;
  //! A 4x4 matrix using \p float precision.
  typedef Matrix4<float>  fmat4;
  //! A 4x4 matrix using \p int precision.
  typedef Matrix4<int>    imat4;
  //! A 4x4 matrix using \p unsigned int precision.
  typedef Matrix4<unsigned int>  umat4;

  #if VL_PIPELINE_PRECISION == 2
    //! Defined as: \p 'typedef \p dmat4 \p mat4'. See also \ref VL_PIPELINE_PRECISION.
    typedef dmat4 mat4;
  #else
    //! Defined as: \p 'typedef \p fmat4 \p mat4'. See also \ref VL_PIPELINE_PRECISION.
    typedef fmat4 mat4;
  #endif
}

#endif
