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

#ifndef Quaternion_INCLUDE_ONCE
#define Quaternion_INCLUDE_ONCE

#include <vlCore/glsl_math.hpp>

namespace vl
{
  //-----------------------------------------------------------------------------
  // Quaternion
  //-----------------------------------------------------------------------------
  /** Implements a Quaternion usually used to represent rotations and orientations. */
  template<typename T_Scalar>
  class Quaternion
  {
  public:
    typedef T_Scalar scalar_type;
    //-----------------------------------------------------------------------------
    //! Constructor.
    Quaternion()
    {
      setNoRotation();
    }
    //-----------------------------------------------------------------------------
    //! Copy-constructor.
    template<typename T>
    explicit Quaternion(const Quaternion<T>& quat)
    {
      mXYZW.x() = (T_Scalar)quat.xyzw().x();
      mXYZW.y() = (T_Scalar)quat.xyzw().y();
      mXYZW.z() = (T_Scalar)quat.xyzw().z();
      mXYZW.w() = (T_Scalar)quat.xyzw().w();
    }
    //-----------------------------------------------------------------------------
    //! Constructor.
    explicit Quaternion(T_Scalar x, T_Scalar y, T_Scalar z, T_Scalar w)
    {
      mXYZW.x() = x;
      mXYZW.y() = y;
      mXYZW.z() = z;
      mXYZW.w() = w;
    }
    //-----------------------------------------------------------------------------
    //! Axis-angle constructor.
    explicit Quaternion(T_Scalar degrees, const Vector3<T_Scalar>& axis)
    {
      setFromAxisAngle(axis, degrees);
    }
    //-----------------------------------------------------------------------------
    //! Constructor from vec4.
    explicit Quaternion(const Vector4<T_Scalar>& v)
    {
      mXYZW.x() = (T_Scalar)v.x();
      mXYZW.y() = (T_Scalar)v.y();
      mXYZW.z() = (T_Scalar)v.z();
      mXYZW.w() = (T_Scalar)v.w();
    }
    //-----------------------------------------------------------------------------
    //! Assignment operator.
    Quaternion& operator=(const Quaternion& q)
    {
      mXYZW.x() = q.x();
      mXYZW.y() = q.y();
      mXYZW.z() = q.z();
      mXYZW.w() = q.w();
      return *this;
    }
    //-----------------------------------------------------------------------------
    //! Assignment operator for vec4
    Quaternion& operator=(const Vector4<T_Scalar>& v)
    {
      mXYZW.x() = (T_Scalar)v.x();
      mXYZW.y() = (T_Scalar)v.y();
      mXYZW.z() = (T_Scalar)v.z();
      mXYZW.w() = (T_Scalar)v.w();
      return *this;
    }
    //-----------------------------------------------------------------------------
    bool operator==(const Quaternion& q) const
    {
      return x() == q.x() && y() == q.y() && z() == q.z() && w() == q.w();
    }
    //-----------------------------------------------------------------------------
    bool operator!=(const Quaternion& q) const
    {
      return !operator==(q);
    }
    //-----------------------------------------------------------------------------
    //! Lexicographic ordering
    bool operator<(const Quaternion& other) const
    {
      if (x() != other.x())
        return x() < other.x();
      if (y() != other.y())
        return y() < other.y();
      if (z() != other.z())
        return z() < other.z();
      else
        return w() < other.w();
    }
    //-----------------------------------------------------------------------------
    //! Returns the internal vec4 used to contain the xyzw the quaternion components.
    const Vector4<T_Scalar>& xyzw() const { return mXYZW; }
    //-----------------------------------------------------------------------------
    //! Returns the internal vec4 used to contain the xyzw the quaternion components.
    Vector4<T_Scalar>& xyzw() { return mXYZW; }
    //-----------------------------------------------------------------------------
    T_Scalar& x() { return mXYZW.x(); }
    //-----------------------------------------------------------------------------
    T_Scalar& y() { return mXYZW.y(); }
    //-----------------------------------------------------------------------------
    T_Scalar& z() { return mXYZW.z(); }
    //-----------------------------------------------------------------------------
    T_Scalar& w() { return mXYZW.w(); }
    //-----------------------------------------------------------------------------
    const T_Scalar& x() const { return mXYZW.x(); }
    //-----------------------------------------------------------------------------
    const T_Scalar& y() const { return mXYZW.y(); }
    //-----------------------------------------------------------------------------
    const T_Scalar& z() const { return mXYZW.z(); }
    //-----------------------------------------------------------------------------
    const T_Scalar& w() const { return mXYZW.w(); }
    //-----------------------------------------------------------------------------
    Quaternion operator*(T_Scalar val) const
    {
      Quaternion t = *this;
      t.x() *= val;
      t.y() *= val;
      t.z() *= val;
      t.w() *= val;
      return t;
    }
    //-----------------------------------------------------------------------------
    Quaternion& operator*=(T_Scalar val)
    {
      x() *= val;
      y() *= val;
      z() *= val;
      w() *= val;
      return *this;
    }
    //-----------------------------------------------------------------------------
    Quaternion operator/(T_Scalar val) const
    {
      Quaternion t = *this;
      val = (T_Scalar)1.0 / val;
      t.x() *= val;
      t.y() *= val;
      t.z() *= val;
      t.w() *= val;
      return t;
    }
    //-----------------------------------------------------------------------------
    Quaternion& operator/=(T_Scalar val)
    {
      val = (T_Scalar)1.0 / val;
      x() *= val;
      y() *= val;
      z() *= val;
      w() *= val;
      return *this;
    }
    //-----------------------------------------------------------------------------
    Quaternion operator+(const Quaternion& q) const
    {
      Quaternion t = *this;
      t.x() += q.x();
      t.y() += q.y();
      t.z() += q.z();
      t.w() += q.w();
      return t; 
    }
    //-----------------------------------------------------------------------------
    Quaternion& operator+=(const Quaternion& q)
    {
      x() += q.x();
      y() += q.y();
      z() += q.z();
      w() += q.w();
      return *this;
    }
    //-----------------------------------------------------------------------------
    Quaternion operator-(const Quaternion& q) const
    {
      Quaternion t = *this;
      t.x() -= q.x();
      t.y() -= q.y();
      t.z() -= q.z();
      t.w() -= q.w();
      return t; 
    }
    //-----------------------------------------------------------------------------
    Quaternion& operator-=(const Quaternion& q)
    {
      x() -= q.x();
      y() -= q.y();
      z() -= q.z();
      w() -= q.w();
      return *this;
    }
    //-----------------------------------------------------------------------------
    //! Returns the negated quaternion.
    Quaternion operator-() const
    {
      return Quaternion(-x(), -y(), -z(), -w());
    }
    //-----------------------------------------------------------------------------
    //! Sets all the components of the quaternion to zero.
    Quaternion& setZero()
    {
      mXYZW.x() = 0;
      mXYZW.y() = 0;
      mXYZW.z() = 0;
      mXYZW.w() = 0;
      return *this;
    }
    //-----------------------------------------------------------------------------
    //! Returns the zero quaternion.
    static Quaternion getZero()
    {
      return Quaternion().setZero();
    }
    //-----------------------------------------------------------------------------
    //! Returns the zero quaternion.
    static Quaternion& getZero(Quaternion& q)
    {
      return q.setZero();
    }
    //-----------------------------------------------------------------------------
    //! Set the quaternion to no-rotation, i.e. Quaternion(0,0,0,1).
    Quaternion& setNoRotation()
    {
      mXYZW.x() = 0;
      mXYZW.y() = 0;
      mXYZW.z() = 0;
      mXYZW.w() = 1;
      return *this;
    }
    //-----------------------------------------------------------------------------
    //! Returns the no-rotation quaternion, i.e. Quaternion(0,0,0,1).
    static Quaternion getNoRotation()
    {
      return Quaternion();
    }
    //-----------------------------------------------------------------------------
    //! Returns the no-rotation quaternion, i.e. Quaternion(0,0,0,1).
    static Quaternion& getNoRotation(Quaternion& q)
    {
      return q.setNoRotation();
    }
    //-----------------------------------------------------------------------------
    //! Sets the quaternion to represent the rotation transforming \p from into \p to.
    Quaternion& setFromVectors(const Vector3<T_Scalar>& from, const Vector3<T_Scalar>& to);
    //-----------------------------------------------------------------------------
    //! Sets the quaternion to represent the rotation transforming \p from into \p to.
    static Quaternion getFromVectors(const Vector3<T_Scalar>& from, const Vector3<T_Scalar>& to)
    {
      return Quaternion().setFromVectors(from, to);
    }
    //-----------------------------------------------------------------------------
    //! Sets the quaternion to represent the rotation transforming \p from into \p to.
    static Quaternion& getFromVectors(Quaternion& q, const Vector3<T_Scalar>& from, const Vector3<T_Scalar>& to)
    {
      return q.setFromVectors(from, to);
    }
    //-----------------------------------------------------------------------------
    //! Creates a quaternion representing the given rotation matrix.
    //! see also http://www.gamasutra.com/features/19980703/quaternions_01.htm \n
    Quaternion& setFromMatrix(const Matrix4<T_Scalar>& m);
    //-----------------------------------------------------------------------------
    //! Converts the given rotation matrix into a quaternion.
    static Quaternion getFromMatrix(const Matrix4<T_Scalar>& m)
    {
      return Quaternion().setFromMatrix(m);
    }
    //-----------------------------------------------------------------------------
    //! Converts the given rotation matrix into a quaternion.
    static Quaternion& getFromMatrix(Quaternion& q, const Matrix4<T_Scalar>& m)
    {
      return q.setFromMatrix(m);
    }
    //-----------------------------------------------------------------------------
    //! Creates a quaternion representing the given rotation matrix.
    //! see also http://www.gamasutra.com/features/19980703/quaternions_01.htm \n
    Quaternion& setFromMatrix(const Matrix3<T_Scalar>& m);
    //-----------------------------------------------------------------------------
    //! Converts the given rotation matrix into a quaternion.
    static Quaternion getFromMatrix(const Matrix3<T_Scalar>& m)
    {
      return Quaternion().setFromMatrix(m);
    }
    //-----------------------------------------------------------------------------
    //! Converts the given rotation matrix into a quaternion.
    static Quaternion& getFromMatrix(Quaternion& q, const Matrix3<T_Scalar>& m)
    {
      return q.setFromMatrix(m);
    }
    //-----------------------------------------------------------------------------
    Quaternion& setFromEulerXYZ(T_Scalar degX, T_Scalar degY, T_Scalar degZ);
    //-----------------------------------------------------------------------------
    static Quaternion getFromEulerXYZ(T_Scalar degX, T_Scalar degY, T_Scalar degZ)
    {
      return Quaternion().setFromEulerXYZ(degX, degY, degZ);
    }
    //-----------------------------------------------------------------------------
    static Quaternion& getFromEulerXYZ(Quaternion& q, T_Scalar degX, T_Scalar degY, T_Scalar degZ)
    {
      return q.setFromEulerXYZ(degX, degY, degZ);
    }
    //-----------------------------------------------------------------------------
    Quaternion& setFromEulerZYX(T_Scalar degZ, T_Scalar degY, T_Scalar degX);
    //-----------------------------------------------------------------------------
    static Quaternion getFromEulerZYX(T_Scalar degZ, T_Scalar degY, T_Scalar degX)
    {
      return Quaternion().setFromEulerZYX(degZ, degY, degX);
    }
    //-----------------------------------------------------------------------------
    static Quaternion& getFromEulerZYX(Quaternion& q, T_Scalar degZ, T_Scalar degY, T_Scalar degX)
    {
      return q.setFromEulerZYX(degZ, degY, degX);
    }
    //-----------------------------------------------------------------------------
    Quaternion& setFromAxisAngle(const Vector3<T_Scalar>& axis, T_Scalar degrees);
    //-----------------------------------------------------------------------------
    static Quaternion getFromAxisAngle(const Vector3<T_Scalar>& axis, T_Scalar degrees)
    {
      return Quaternion().setFromAxisAngle(axis, degrees);
    }
    //-----------------------------------------------------------------------------
    static Quaternion& getFromAxisAngle(Quaternion& q, const Vector3<T_Scalar>& axis, T_Scalar degrees)
    {
      return q.setFromAxisAngle(axis, degrees);
    }
    //-----------------------------------------------------------------------------
    //! Converts a quaternion to an axis-angle representation.
    void toAxisAngle( Vector3<T_Scalar>& axis, T_Scalar& degrees ) const;
    //-----------------------------------------------------------------------------
    //! Converts a quaternion to a 4x4 rotation matrix.
    Matrix4<T_Scalar> toMatrix4() const;
    //-----------------------------------------------------------------------------
    //! Converts a quaternion to a 4x4 rotation matrix.
    Matrix4<T_Scalar>& toMatrix4(Matrix4<T_Scalar>&) const;
    //-----------------------------------------------------------------------------
    //! Converts a quaternion to a 3x3 rotation matrix.
    Matrix3<T_Scalar> toMatrix3() const;
    //-----------------------------------------------------------------------------
    //! Converts a quaternion to a 3x3 rotation matrix.
    Matrix3<T_Scalar>& toMatrix3(Matrix3<T_Scalar>&) const;
    //-----------------------------------------------------------------------------
    //! Returns the dot product between a quaternion and the given quaternion.
    T_Scalar dot(const Quaternion& q) const
    {
      return x()*q.x() + y()*q.y() + z()*q.z() + w()*q.w();
    }
    //-----------------------------------------------------------------------------
    //! Returns the length of a quaternion.
    T_Scalar length() const  { return mXYZW.length(); }
    //-----------------------------------------------------------------------------
    //! Normalizes a quaternion.
    //! \p len returns the original length of the quaternion.
    Quaternion& normalize(T_Scalar* len=NULL) { mXYZW.normalize(len); return *this; }
    //-----------------------------------------------------------------------------
    //! Returns the normalized version of a quaternion.
    //! \p len returns the original length of the quaternion.
    Quaternion getNormalized(T_Scalar* len=NULL) const { Quaternion t = *this; t.normalize(len); return t; }
    //-----------------------------------------------------------------------------
    //! Returns the normalized version of a quaternion.
    //! \p len returns the original length of the quaternion.
    Quaternion& getNormalized(Quaternion& q, T_Scalar* len=NULL) const { q = *this; q.normalize(len); return q; }
    //-----------------------------------------------------------------------------
    //! Returns the squared length of a quaternion.
    T_Scalar lengthSquared() const
    {
      return x()*x() + y()*y() + z()*z() + w()*w();
    }
    //-----------------------------------------------------------------------------
    //! Returns the conjugate of a quaternion.
    Quaternion getConjugate() const
    {
      return Quaternion(-x(), -y(), -z(), w());
    }
    //-----------------------------------------------------------------------------
    //! Returns the conjugate of a quaternion.
    Quaternion& getConjugate(Quaternion& q) const
    {
      q = Quaternion(-x(), -y(), -z(), w());
      return q;
    }
    //-----------------------------------------------------------------------------
    //! Returns the inverse of a quaternion.
    Quaternion getInverse() const
    {
      return getConjugate() / lengthSquared();
    }
    //-----------------------------------------------------------------------------
    //! Returns the inverse of a quaternion.
    Quaternion& getInverse(Quaternion& q) const
    {
      q = getConjugate() / lengthSquared();
      return q;
    }
    //-----------------------------------------------------------------------------
    //! Spherical linear interpolation of two quaternions.
    //! See also http://www.gamasutra.com/features/19980703/quaternions_01.htm \n
    //! Properties: NO commutative, YES torque-minimal, YES constant velocity.
    static Quaternion getSlerp(T_Scalar t, const Quaternion& a, const Quaternion& b);
    //-----------------------------------------------------------------------------
    //! Spherical linear interpolation of two quaternions.
    //! See also http://www.gamasutra.com/features/19980703/quaternions_01.htm \n
    //! Properties: NO commutative, YES torque-minimal, YES constant velocity.
    static Quaternion& getSlerp(Quaternion& out, T_Scalar t, const Quaternion& a, const Quaternion& b);
    //-----------------------------------------------------------------------------
    //! Spherical cubic interpolation of two quaternions
    static Quaternion getSquad(T_Scalar t, const Quaternion& a, const Quaternion& p, const Quaternion& q, const Quaternion& b)
    {
      return getSlerp((T_Scalar)2.0*t*((T_Scalar)1.0-t), getSlerp(t,a,b), getSlerp(t,p,q));
    }
    //-----------------------------------------------------------------------------
    //! Spherical cubic interpolation of two quaternions
    static Quaternion& getSquad(Quaternion& out, T_Scalar t, const Quaternion& a, const Quaternion& p, const Quaternion& q, const Quaternion& b)
    {
      return getSlerp(out, (T_Scalar)2.0*t*((T_Scalar)1.0-t), getSlerp(t,a,b), getSlerp(t,p,q));
    }
    //-----------------------------------------------------------------------------
    //! Normalized spherical interpolation of two quaternions.
    //! See also http://number-none.com/product/Understanding%20Slerp,%20Then%20Not%20Using%20It/ \n
    //! Properties: YES commutative, YES torque-minimal, NO constant velocity.
    static Quaternion getNlerp( T_Scalar t, const Quaternion& a, const Quaternion& b )
    {
      Quaternion q = a + (b - a) * t;
      q.normalize();
      return q;
    }
    //-----------------------------------------------------------------------------
    //! Normalized spherical interpolation of two quaternions.
    //! See also http://number-none.com/product/Understanding%20Slerp,%20Then%20Not%20Using%20It/ \n
    //! Properties: YES commutative, YES torque-minimal, NO constant velocity.
    static Quaternion& getNlerp( Quaternion& out, T_Scalar t, const Quaternion& a, const Quaternion& b )
    {
      out = a + (b - a) * t;
      out.normalize();
      return out;
    }
    //-----------------------------------------------------------------------------
    
  protected:
    Vector4<T_Scalar> mXYZW;
  };
  //-----------------------------------------------------------------------------
  template<typename T_Scalar>
  inline Quaternion<T_Scalar> operator*(T_Scalar r, const Quaternion<T_Scalar>& q)
  {
    return q * r;
  }
  //-----------------------------------------------------------------------------
  template<typename T_Scalar>
  inline Quaternion<T_Scalar> operator*(const Quaternion<T_Scalar>& q1, const Quaternion<T_Scalar>& q2)
  {
    Quaternion<T_Scalar> q;
    q.x() = q1.w() * q2.x() + q1.x() * q2.w() + q1.y() * q2.z() - q1.z() * q2.y();
    q.y() = q1.w() * q2.y() + q1.y() * q2.w() + q1.z() * q2.x() - q1.x() * q2.z();
    q.z() = q1.w() * q2.z() + q1.z() * q2.w() + q1.x() * q2.y() - q1.y() * q2.x();
    q.w() = q1.w() * q2.w() - q1.x() * q2.x() - q1.y() * q2.y() - q1.z() * q2.z();
    return q;
  }
  //-----------------------------------------------------------------------------
  // post multiplication
  template<typename T_Scalar>
  inline Vector3<T_Scalar> operator*(const Quaternion<T_Scalar>&q, const Vector3<T_Scalar>& v)
  {
    // Matrix conversion formula based implementation
    T_Scalar x2 = q.x() * q.x();
    T_Scalar y2 = q.y() * q.y();
    T_Scalar z2 = q.z() * q.z();
    T_Scalar xy = q.x() * q.y();
    T_Scalar xz = q.x() * q.z();
    T_Scalar yz = q.y() * q.z();
    T_Scalar wx = q.w() * q.x();
    T_Scalar wy = q.w() * q.y();
    T_Scalar wz = q.w() * q.z();

    Vector3<T_Scalar> r;
    r.x() = ( v.x()*(1.0f - 2.0f * (y2 + z2)) + v.y()*(2.0f * (xy - wz)) + v.z()*(2.0f * (xz + wy)) );
    r.y() = ( v.x()*(2.0f * (xy + wz)) + v.y()*(1.0f - 2.0f * (x2 + z2)) + v.z()*(2.0f * (yz - wx)) );
    r.z() = ( v.x()*(2.0f * (xz - wy)) + v.y()*(2.0f * (yz + wx)) + v.z()*(1.0f - 2.0f * (x2 + y2)) );
    return r;
  }
  //-----------------------------------------------------------------------------
  // post multiplication
  template<typename T_Scalar>
  inline Vector4<T_Scalar> operator*(const Quaternion<T_Scalar>&q, const Vector4<T_Scalar>& v)
  {
    return Vector4<T_Scalar>( q * v.xyz(), v.w() );
  }
  //-----------------------------------------------------------------------------
  template<typename T_Scalar>
  Quaternion<T_Scalar>& Quaternion<T_Scalar>::setFromEulerXYZ(T_Scalar degX, T_Scalar degY, T_Scalar degZ )
  {
    *this = Quaternion<T_Scalar>(degX, Vector3<T_Scalar>(1,0,0)) * Quaternion<T_Scalar>(degY, Vector3<T_Scalar>(0,1,0)) * Quaternion<T_Scalar>(degZ, Vector3<T_Scalar>(0,0,1));
    return *this;
  }
  //-----------------------------------------------------------------------------
  template<typename T_Scalar>
  Quaternion<T_Scalar>& Quaternion<T_Scalar>::setFromEulerZYX(T_Scalar degZ, T_Scalar degY, T_Scalar degX )
  {
    *this = Quaternion<T_Scalar>(degZ, Vector3<T_Scalar>(0,0,1)) * Quaternion<T_Scalar>(degY, Vector3<T_Scalar>(0,1,0)) * Quaternion<T_Scalar>(degX, Vector3<T_Scalar>(1,0,0));
    return *this;
  }
  //-----------------------------------------------------------------------------
  template<typename T_Scalar>
  Quaternion<T_Scalar>& Quaternion<T_Scalar>::setFromMatrix(const Matrix4<T_Scalar>& m)
  {
    return setFromMatrix( m.get3x3() );
  }
  //-----------------------------------------------------------------------------
  template<typename T_Scalar>
  Quaternion<T_Scalar>& Quaternion<T_Scalar>::setFromMatrix(const Matrix3<T_Scalar>& m)
  {
    T_Scalar tr, s, q[4];

    int next[3] = {1, 2, 0};

    tr = m.e(0,0) + m.e(1,1) + m.e(2,2);

    // check the diagonal
    if (tr + (T_Scalar)1.0 > 0.0) 
    {
      s = vl::sqrt(tr + (T_Scalar)1.0);
      w() = s / (T_Scalar)2.0;
      s = (T_Scalar)0.5 / s;
      x() = (m.e(2,1) - m.e(1,2)) * s;
      y() = (m.e(0,2) - m.e(2,0)) * s;
      z() = (m.e(1,0) - m.e(0,1)) * s;
    } 
    else 
    {    
      // diagonal is negative
      int i, j, k;
      i = 0;
      if (m.e(1,1) > m.e(0,0)) i = 1;
      if (m.e(2,2) > m.e(i,i)) i = 2;
      j = next[i];
      k = next[j];

      s = vl::sqrt((m.e(i,i) - (m.e(j,j) + m.e(k,k))) + (T_Scalar)1.0);

      q[i] = s * (T_Scalar)0.5;

      if (s != 0.0) 
        s = (T_Scalar)0.5 / s;

      q[3] = (m.e(k,j) - m.e(j,k)) * s;
      q[j] = (m.e(j,i) + m.e(i,j)) * s;
      q[k] = (m.e(k,i) + m.e(i,k)) * s;

      x() = q[0];
      y() = q[1];
      z() = q[2];
      w() = q[3];
    }

    return *this;
  }
  //-----------------------------------------------------------------------------
  template<typename T_Scalar>
  Quaternion<T_Scalar>& Quaternion<T_Scalar>::setFromAxisAngle( const Vector3<T_Scalar>& axis, T_Scalar degrees )
  {
    degrees *= (T_Scalar)dDEG_TO_RAD;
    T_Scalar sa2 = sin(degrees * (T_Scalar)0.5);
    Vector3<T_Scalar> na = axis;
    na.normalize();
    mXYZW.x() = na.x() * sa2;
    mXYZW.y() = na.y() * sa2;
    mXYZW.z() = na.z() * sa2;
    mXYZW.w() = cos(degrees * (T_Scalar)0.5);
    return *this;
  }
  //-----------------------------------------------------------------------------
  template<typename T_Scalar>
  void Quaternion<T_Scalar>::toAxisAngle( Vector3<T_Scalar>& axis, T_Scalar& degrees ) const
  {
    T_Scalar iscale = sqrt( x()*x() + y()*y() + z()*z() );
    if (iscale == 0)
    {
      axis.x() = 0;
      axis.y() = 0;
      axis.z() = 0;
      degrees  = 0;
    }
    else
    {
      iscale = T_Scalar(1.0) / iscale;
      axis.x() = x() * iscale;
      axis.y() = y() * iscale;
      axis.z() = z() * iscale;
      VL_CHECK(w()>=-1.0 && w()<=+1.0)
      T_Scalar tw = clamp(w(),(T_Scalar)-1.0,(T_Scalar)+1.0);
      degrees = acos( tw ) * (T_Scalar)2.0 * (T_Scalar)dRAD_TO_DEG;
    }
  }
  //-----------------------------------------------------------------------------
  template<typename T_Scalar>
  Matrix4<T_Scalar>& Quaternion<T_Scalar>::toMatrix4(Matrix4<T_Scalar>& out) const
  {
    T_Scalar x2 = x() * x();
    T_Scalar y2 = y() * y();
    T_Scalar z2 = z() * z();
    T_Scalar xy = x() * y();
    T_Scalar xz = x() * z();
    T_Scalar yz = y() * z();
    T_Scalar wx = w() * x();
    T_Scalar wy = w() * y();
    T_Scalar wz = w() * z();

    return out = Matrix4<T_Scalar>( 
      (1.0f - 2.0f * (y2 + z2)), (2.0f * (xy - wz)),        (2.0f * (xz + wy)),        0.0f,
      (2.0f * (xy + wz)),        (1.0f - 2.0f * (x2 + z2)), (2.0f * (yz - wx)),        0.0f,
      (2.0f * (xz - wy)),        (2.0f * (yz + wx)),        (1.0f - 2.0f * (x2 + y2)), 0.0f,
      0.0f,                      0.0f,                      0.0f,                      1.0f );
  }
  //-----------------------------------------------------------------------------
  template<typename T_Scalar>
  Matrix4<T_Scalar> Quaternion<T_Scalar>::toMatrix4() const
  {
    Matrix4<T_Scalar> out;
    return toMatrix4(out);
  }
  //-----------------------------------------------------------------------------
  template<typename T_Scalar>
  Matrix3<T_Scalar>& Quaternion<T_Scalar>::toMatrix3(Matrix3<T_Scalar>& out) const
  {
    T_Scalar x2 = x() * x();
    T_Scalar y2 = y() * y();
    T_Scalar z2 = z() * z();
    T_Scalar xy = x() * y();
    T_Scalar xz = x() * z();
    T_Scalar yz = y() * z();
    T_Scalar wx = w() * x();
    T_Scalar wy = w() * y();
    T_Scalar wz = w() * z();

    return out = Matrix3<T_Scalar>( 
      (1.0f - 2.0f * (y2 + z2)), (2.0f * (xy + wz)),        (2.0f * (xz - wy)),
      (2.0f * (xy - wz)),        (1.0f - 2.0f * (x2 + z2)), (2.0f * (yz + wx)),
      (2.0f * (xz + wy)),        (2.0f * (yz - wx)),        (1.0f - 2.0f * (x2 + y2)) );
  }
  //-----------------------------------------------------------------------------
  template<typename T_Scalar>
  Matrix3<T_Scalar> Quaternion<T_Scalar>::toMatrix3() const
  {
    Matrix3<T_Scalar> out;
    return toMatrix3(out);
  }
  //-----------------------------------------------------------------------------
  template<typename T_Scalar>
  Quaternion<T_Scalar> Quaternion<T_Scalar>::getSlerp( T_Scalar t, const Quaternion<T_Scalar>& a, const Quaternion<T_Scalar>& b )
  {
    Quaternion<T_Scalar> q;
    getSlerp(q, t, a, b);
    return q;
  }
  //-----------------------------------------------------------------------------
  template<typename T_Scalar>
  Quaternion<T_Scalar>& Quaternion<T_Scalar>::getSlerp( Quaternion<T_Scalar>& out, T_Scalar t, const Quaternion<T_Scalar>& a, const Quaternion<T_Scalar>& b )
  {
    T_Scalar scale_a, scale_b, omega, sinom;
    T_Scalar cosom = a.dot(b);

    Quaternion<T_Scalar> b2(b);

    if ( cosom < 0 )
    { 
      cosom = -cosom; 
      b2 = -b;
    }

    // clamp rounding errors
    cosom = cosom > (T_Scalar)1 ? (T_Scalar)1 : cosom;

    if( cosom < (T_Scalar)1.0 )
    {
      omega = acos(cosom);
      sinom = sin(omega);
      VL_CHECK(sinom != 0)
        scale_a = sin(((T_Scalar)1.0-t) * omega) / sinom;
      scale_b = sin(t * omega) / sinom;
    }
    else
    {
      // linear interpolation for degenerate cases
      scale_a = (T_Scalar)1.0 - t;
      scale_b = t;
    }

    return out = (a*scale_a) + (b2*scale_b);
  }
  //-----------------------------------------------------------------------------
  template<typename T_Scalar>
  Quaternion<T_Scalar>& Quaternion<T_Scalar>::setFromVectors(const Vector3<T_Scalar>& from, const Vector3<T_Scalar>& to)
  {
    Vector3<T_Scalar> a,b;
    a = from;
    b = to;
    a.normalize();
    b.normalize();
    Vector3<T_Scalar> axis = cross(a,b);
    T_Scalar len = 0;
    axis.normalize(&len);
    if(len)
    {
      T_Scalar cosa = vl::dot(a,b);
      cosa = clamp(cosa, (T_Scalar)-1.0, (T_Scalar)+1.0);
      T_Scalar alpha = acos( cosa );
      setFromAxisAngle(axis, alpha*(T_Scalar)dRAD_TO_DEG);
    }
    else
      setNoRotation();
    return *this;
  }
  //-----------------------------------------------------------------------------
  typedef Quaternion<float>  fquat;
  typedef Quaternion<double> dquat;
  typedef Quaternion<real>   quat;
  //-----------------------------------------------------------------------------
}

#endif
