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

#ifndef glslmath_INCLUDE_ONCE
#define glslmath_INCLUDE_ONCE

/**
  \file glsl_math.hpp Implements the OpenGL Shading Language convenience functions for scalar and vector operations.

  This functions are particularly useful when you want to port C++ code to GLSL and vice versa, or when you want to quickly 
  prototype in C++ an algorithm that will be ported later to GLSL. 
  
  Note that most of this functions take as arguments not only \p int, \p unsigned \p int, \p float and \p double variables
  but also their vector counterparts like \p fvec4, \p Vector4<T>, \p ivec4, \p uvec4, \p fvec3, \p fvec2 etc.
  For example you can do the following:
\code
// clamp a float value
float f = someValueFloat()
f = vl::clamp(f, 0.0f, 1.0f);

// clamp a vector value
vl::fvec4 v = someValueVec4();
v = vl::clamp(v, vl::fvec4(1,2,3,4), vl::fvec4(5,6,7,8));

// the same goes for functions like mix(), min(), max(), sin(), cos(), floor() etc.
\endcode

  This module also implements other convenience functions like isnan(), isinf(), isinf_pos(), isinf_neg().

  The functions are divided in the following categories:

  \par Angle and trigonometry functions
  radians(), degrees(), sin(), cos(), tan(), asin(), acos(), atan()

  \par Hyperbolic functions
  sinh(), cosh(), tanh(), asinh(), acosh(), atanh()

  \par Exponential functions
  pow(), exp(), log(), exp2(), log2(), log10(), sqrt(), inversesqrt(), 

  \par Common functions
  abs(), sign(), floor(), trunc(), round(), roundEven(), ceil(), fract(), mod(), modf(), min(), max(), clamp(), mix(), step(), smoothstep(), isnan(), isinf()

  \par Geometric functions
  length(), distance(), dot(), cross(), normalize(), faceforward(), reflect(), refract()

  \par Matrix functions
  matrixCompMult(), outerProduct(), transpose(), 

  \par Vector relational functions
  lessThan(), lessThanEqual(), greaterThan(), greaterThanEqual(), equal(), notEqual(), any(), all(), not()

  \sa
  For more information please refer to the official OpenGL Shading Language manual and specifications.
  See also the official <a target=_blank href=http://www.opengl.org/documentation/specs>OpenGL & OpenGL Utility Specifications</a> page.

 */

#include <cmath>
#include <limits>
#include <vlCore/Vector4.hpp>
#include <vlCore/Matrix4.hpp>

#undef min
#undef max

#if defined(__CUDACC__)
#undef isnan
#undef isinf
#endif

namespace vl
{
  // hyperbolic functions not implemented in Visual C++

  // taken from http://support.microsoft.com/kb/625449/it

  template<typename T>
  T asinh(T x) { return log(x+::sqrt(x*x+1)); }

  template<typename T>
  T acosh(T x)
  {
    // must be x>=1, if not return Nan (Not a Number)
    if(!(x>=1)) return ::sqrt((T)-1);

    // return only the positive result (as sqrt does).
    return log(x+::sqrt(x*x-1));
  }

  template<typename T>
  T atanh(T x)
  {
    // must be x>-1, x<1, if not return Nan (Not a Number)
    if(!(x>-1 && x<1)) return ::sqrt((T)-1);

    return log((1+x)/(1-x))/2;
  }

  // isinf, isnan functions not implemented in Visual C++

  template<typename T> bool isnan(T value) { return !(value == value); }
  template<typename T> bool isinf(T value) { return value < std::numeric_limits<T>::min() || value > std::numeric_limits<T>::max(); }
  template<typename T> bool isinf_pos(T value) { return value > std::numeric_limits<T>::max(); }
  template<typename T> bool isinf_neg(T value) { return value < std::numeric_limits<T>::min(); }

  //-----------------------------------------------------------------------------
  // GLSL functions
  //-----------------------------------------------------------------------------

  template<typename T>
  T modf(T a, T& intpart);

  // --------------- angle and trigonometric functions ---------------

  // --------------- radians ---------------

  template<typename T>
  T radians(T degrees) { return degrees * (T)dDEG_TO_RAD; }

  template<typename T>
  Vector2<T> radians(const Vector2<T>& degrees) {
    return Vector2<T>( degrees.x() * (T)dDEG_TO_RAD,
                       degrees.y() * (T)dDEG_TO_RAD );
  }

  template<typename T>
  Vector3<T> radians(const Vector3<T>& degrees) {
    return Vector3<T>( degrees.x() * (T)dDEG_TO_RAD,
                       degrees.y() * (T)dDEG_TO_RAD,
                       degrees.z() * (T)dDEG_TO_RAD );
  }

  template<typename T>
  Vector4<T> radians(const Vector4<T>& degrees) {
    return Vector4<T>( degrees.x() * (T)dDEG_TO_RAD,
                       degrees.y() * (T)dDEG_TO_RAD,
                       degrees.z() * (T)dDEG_TO_RAD,
                       degrees.w() * (T)dDEG_TO_RAD );
  }

  // --------------- degrees ---------------

  template<typename T>
  T degrees(T radians) { return radians * (T)dRAD_TO_DEG; }

  template<typename T>
  Vector2<T> degrees(const Vector2<T>& radians) {
    return Vector2<T>( radians.x() * (T)dRAD_TO_DEG,
                       radians.y() * (T)dRAD_TO_DEG );
  }

  template<typename T>
  Vector3<T> degrees(const Vector3<T>& radians) {
    return Vector3<T>( radians.x() * (T)dRAD_TO_DEG,
                       radians.y() * (T)dRAD_TO_DEG,
                       radians.z() * (T)dRAD_TO_DEG );
  }

  template<typename T>
  Vector4<T> degrees(const Vector4<T>& radians) {
    return Vector4<T>( radians.x() * (T)dRAD_TO_DEG,
                       radians.y() * (T)dRAD_TO_DEG,
                       radians.z() * (T)dRAD_TO_DEG,
                       radians.w() * (T)dRAD_TO_DEG );
  }

  // --------------- sin ---------------

  template<typename T>
  T sin(T a) { return ::sin(a); }

  template<typename T>
  Vector2<T> sin(const Vector2<T>& angle) {
    return Vector2<T>( ::sin(angle.x()),
                       ::sin(angle.y()) );
  }

  template<typename T>
  Vector3<T> sin(const Vector3<T>& angle) {
    return Vector3<T>( ::sin(angle.x()),
                       ::sin(angle.y()),
                       ::sin(angle.z()) );
  }

  template<typename T>
  Vector4<T> sin(const Vector4<T>& angle) {
    return Vector4<T>( ::sin(angle.x()),
                       ::sin(angle.y()),
                       ::sin(angle.z()),
                       ::sin(angle.w()) );
  }

  // --------------- cos ---------------

  template<typename T>
  T cos(T a) { return ::cos(a); }
  
  template<typename T>
  Vector2<T> cos(const Vector2<T>& angle) {
    return Vector2<T>( ::cos(angle.x()),
                           ::cos(angle.y()) );
  }
  
  template<typename T>
  Vector3<T> cos(const Vector3<T>& angle) {
    return Vector3<T>( ::cos(angle.x()),
                       ::cos(angle.y()),
                       ::cos(angle.z()) );
  }
  
  template<typename T>
  Vector4<T> cos(const Vector4<T>& angle) {
    return Vector4<T>( ::cos(angle.x()),
                       ::cos(angle.y()),
                       ::cos(angle.z()),
                       ::cos(angle.w()) );
  }

  // --------------- tan ---------------

  template<typename T>
  T tan(T a) { return ::tan(a); }

  template<typename T>
  Vector2<T> tan(const Vector2<T>& angle) {
    return Vector2<T>( ::tan(angle.x()),
                       ::tan(angle.y()) );
  }

  template<typename T>
  Vector3<T> tan(const Vector3<T>& angle) {
    return Vector3<T>( ::tan(angle.x()),
                       ::tan(angle.y()),
                       ::tan(angle.z()) );
  }

  template<typename T>
  Vector4<T> tan(const Vector4<T>& angle) {
    return Vector4<T>( ::tan(angle.x()),
                       ::tan(angle.y()),
                       ::tan(angle.z()),
                       ::tan(angle.w()) );
  }

  // --------------- atan ---------------

  template<typename T>
  T atan(T a) { return ::atan(a); }

  template<typename T>
  Vector2<T> atan(const Vector2<T>& a, const Vector2<T>& b) {
    return Vector2<T>( ::atan2(a.x(), b.x()),
                       ::atan2(a.y(), b.y()) );
  }

  template<typename T>
  Vector3<T> atan(const Vector3<T>& a, const Vector3<T>& b) {
    return Vector3<T>( ::atan2(a.x(), b.x()),
                       ::atan2(a.y(), b.y()),
                       ::atan2(a.z(), b.z()) );
  }

  template<typename T>
  Vector4<T> atan(const Vector4<T>& a, const Vector4<T>& b) {
    return Vector4<T>( ::atan2(a.x(), b.x()),
                       ::atan2(a.y(), b.y()),
                       ::atan2(a.z(), b.z()),
                       ::atan2(a.w(), b.w()) );
  }

  // --------------- asin ---------------

  template<typename T>
  T asin(T a) { return ::asin(a); }

  template<typename T>
  Vector2<T> asin(const Vector2<T>& angle) {
    return Vector2<T>( ::asin(angle.x()),
                       ::asin(angle.y()) );
  }

  template<typename T>
  Vector3<T> asin(const Vector3<T>& angle) {
    return Vector3<T>( ::asin(angle.x()),
                       ::asin(angle.y()),
                       ::asin(angle.z()) );
  }

  template<typename T>
  Vector4<T> asin(const Vector4<T>& angle) {
    return Vector4<T>( ::asin(angle.x()),
                       ::asin(angle.y()),
                       ::asin(angle.z()),
                       ::asin(angle.w()) );
  }

  // --------------- acos ---------------

  template<typename T>
  T acos(T a) { return ::acos(a); }

  template<typename T>
  Vector2<T> acos(const Vector2<T>& angle) {
    return Vector2<T>( ::acos(angle.x()),
                       ::acos(angle.y()) );
  }

  template<typename T>
  Vector3<T> acos(const Vector3<T>& angle) {
    return Vector3<T>( ::acos(angle.x()),
                       ::acos(angle.y()),
                       ::acos(angle.z()) );
  }

  template<typename T>
  Vector4<T> acos(const Vector4<T>& angle) {
    return Vector4<T>( ::acos(angle.x()),
                       ::acos(angle.y()),
                       ::acos(angle.z()),
                       ::acos(angle.w()) );
  }

  // --------------- hyperbolic functions ---------------

  // --------------- sinh ---------------

  template<typename T>
  T sinh(T a) { return (exp(a) - exp(-a)) / 2; }

  template<typename T>
  Vector2<T> sinh(const Vector2<T>& a) { return Vector2<T>( sinh(a.x()), sinh(a.y()) ); }

  template<typename T>
  Vector3<T> sinh(const Vector3<T>& a) { return Vector3<T>( sinh(a.x()), sinh(a.y()), sinh(a.z()) ); }

  template<typename T>
  Vector4<T> sinh(const Vector4<T>& a) { return Vector4<T>( sinh(a.x()), sinh(a.y()), sinh(a.z()), sinh(a.w()) ); }

  // --------------- cosh ---------------

  template<typename T>
  T cosh(T a) { return (exp(a) + exp(-a)) / 2; }

  template<typename T>
  Vector2<T> cosh(const Vector2<T>& a) { return Vector2<T>( cosh(a.x()), cosh(a.y()) ); }

  template<typename T>
  Vector3<T> cosh(const Vector3<T>& a) { return Vector3<T>( cosh(a.x()), cosh(a.y()), cosh(a.z()) ); }

  template<typename T>
  Vector4<T> cosh(const Vector4<T>& a) { return Vector4<T>( cosh(a.x()), cosh(a.y()), cosh(a.z()), cosh(a.w()) ); }

  // --------------- tanh ---------------

  template<typename T>
  T tanh(T a) { return sinh(a) / cosh(a); }

  template<typename T>
  Vector2<T> tanh(const Vector2<T>& a) { return Vector2<T>( tanh(a.x()), tanh(a.y()) ); }

  template<typename T>
  Vector3<T> tanh(const Vector3<T>& a) { return Vector3<T>( tanh(a.x()), tanh(a.y()), tanh(a.z()) ); }

  template<typename T>
  Vector4<T> tanh(const Vector4<T>& a) { return Vector4<T>( tanh(a.x()), tanh(a.y()), tanh(a.z()), tanh(a.w()) ); }

  // --------------- asinh ---------------

  template<typename T>
  Vector2<T> asinh(const Vector2<T>& a) { return Vector2<T>( asinh(a.x()), asinh(a.y()) ); }

  template<typename T>
  Vector3<T> asinh(const Vector3<T>& a) { return Vector3<T>( asinh(a.x()), asinh(a.y()), asinh(a.z()) ); }

  template<typename T>
  Vector4<T> asinh(const Vector4<T>& a) { return Vector4<T>( asinh(a.x()), asinh(a.y()), asinh(a.z()), asinh(a.w()) ); }

  // --------------- acosh ---------------

  template<typename T>
  Vector2<T> acosh(const Vector2<T>& a) { return Vector2<T>( acosh(a.x()), acosh(a.y()) ); }

  template<typename T>
  Vector3<T> acosh(const Vector3<T>& a) { return Vector3<T>( acosh(a.x()), acosh(a.y()), acosh(a.z()) ); }

  template<typename T>
  Vector4<T> acosh(const Vector4<T>& a) { return Vector4<T>( acosh(a.x()), acosh(a.y()), acosh(a.z()), acosh(a.w()) ); }

  // --------------- atanh ---------------

  template<typename T>
  Vector2<T> atanh(const Vector2<T>& a) { return Vector2<T>( atanh(a.x()), atanh(a.y()) ); }

  template<typename T>
  Vector3<T> atanh(const Vector3<T>& a) { return Vector3<T>( atanh(a.x()), atanh(a.y()), atanh(a.z()) ); }

  template<typename T>
  Vector4<T> atanh(const Vector4<T>& a) { return Vector4<T>( atanh(a.x()), atanh(a.y()), atanh(a.z()), atanh(a.w()) ); }

  // --------------- exponential functions ---------------

  // --------------- pow ---------------

  template<typename T>
  T pow(T a, T b) { return ::pow(a, b); }

  template<typename T>
  Vector2<T> pow(const Vector2<T>& a, const Vector2<T>& b) {
    return Vector2<T>( ::pow(a.x(), b.x()),
                       ::pow(a.y(), b.y()) );
  }

  template<typename T>
  Vector3<T> pow(const Vector3<T>& a, const Vector3<T>& b) {
    return Vector3<T>( ::pow(a.x(), b.x()),
                       ::pow(a.y(), b.y()),
                       ::pow(a.z(), b.z()) );
  }

  template<typename T>
  Vector4<T> pow(const Vector4<T>& a, const Vector4<T>& b) {
    return Vector4<T>( ::pow(a.x(), b.x()),
                       ::pow(a.y(), b.y()),
                       ::pow(a.z(), b.z()),
                       ::pow(a.w(), b.w()) );
  }

  // --------------- exp ---------------

  template<typename T>
  T exp(T a) { return ::exp(a); }

  template<typename T>
  Vector2<T> exp(const Vector2<T>& a) {
    return Vector2<T>( ::exp(a.x()),
                       ::exp(a.y()) );
  }

  template<typename T>
  Vector3<T> exp(const Vector3<T>& a) {
    return Vector3<T>( ::exp(a.x()),
                       ::exp(a.y()),
                       ::exp(a.z()) );
  }

  template<typename T>
  Vector4<T> exp(const Vector4<T>& a) {
    return Vector4<T>( ::exp(a.x()),
                       ::exp(a.y()),
                       ::exp(a.z()),
                       ::exp(a.w()) );
  }

  // --------------- log ---------------

  template<typename T>
  T log(T a) { return ::log(a); }

  template<typename T>
  Vector2<T> log(const Vector2<T>& a) {
    return Vector2<T>( ::log(a.x()),
                       ::log(a.y()) );
  }

  template<typename T>
  Vector3<T> log(const Vector3<T>& a) {
    return Vector3<T>( ::log(a.x()),
                       ::log(a.y()),
                       ::log(a.z()) );
  }

  template<typename T>
  Vector4<T> log(const Vector4<T>& a) {
    return Vector4<T>( ::log(a.x()),
                       ::log(a.y()),
                       ::log(a.z()),
                       ::log(a.w()) );
  }

  // --------------- exp2 ---------------

  template<typename T>
  T exp2(T a) { return ::pow(2, a); }

  template<typename T>
  Vector2<T> exp2(const Vector2<T>& a) {
    return Vector2<T>( ::pow(2, a.x()),
                       ::pow(2, a.y()) );
  }

  template<typename T>
  Vector3<T> exp2(const Vector3<T>& a) {
    return Vector3<T>( ::pow(2, a.x()),
                       ::pow(2, a.y()),
                       ::pow(2, a.z()) );
  }

  template<typename T>
  Vector4<T> exp2(const Vector4<T>& a) {
    return Vector4<T>( ::pow(2, a.x()),
                       ::pow(2, a.y()),
                       ::pow(2, a.z()),
                       ::pow(2, a.w()) );
  }

  // --------------- log2 ---------------

  template<typename T>
  T log2(T a) { return log10(a) / log10(2); }

  template<typename T>
  Vector2<T> log2(const Vector2<T>& a) {
    return Vector2<T>( log2(a.x()),
                  log2(a.y()) );
  }

  template<typename T>
  Vector3<T> log2(const Vector3<T>& a) {
    return Vector3<T>( log2(a.x()),
                  log2(a.y()),
                  log2(a.z()) );
  }

  template<typename T>
  Vector4<T> log2(const Vector4<T>& a) {
    return Vector4<T>( log2(a.x()),
                  log2(a.y()),
                  log2(a.z()),
                  log2(a.w()) );
  }

  // --------------- log10 ---------------

  // this is not present in the GLSL standard

  template<typename T>
  T log10(T a) { return ::log10(a); }

  template<typename T>
  Vector2<T> log10(const Vector2<T>& a) {
    return Vector2<T>( ::log10(a.x()),
                       ::log10(a.y()) );
  }

  template<typename T>
  Vector3<T> log10(const Vector3<T>& a) {
    return Vector3<T>( ::log10(a.x()),
                       ::log10(a.y()),
                       ::log10(a.z()) );
  }

  template<typename T>
  Vector4<T> log10(const Vector4<T>& a) {
    return Vector4<T>( ::log10(a.x()),
                       ::log10(a.y()),
                       ::log10(a.z()),
                       ::log10(a.w()) );
  }

  // --------------- sqrt ---------------

  template<typename T>
  T sqrt(T a) { return ::sqrt(a); }

  template<typename T>
  Vector2<T> sqrt(const Vector2<T>& a) {
    return Vector2<T>( ::sqrt(a.x()),
                       ::sqrt(a.y()) );
  }

  template<typename T>
  Vector3<T> sqrt(const Vector3<T>& a) {
    return Vector3<T>( ::sqrt(a.x()),
                       ::sqrt(a.y()),
                       ::sqrt(a.z()) );
  }

  template<typename T>
  Vector4<T> sqrt(const Vector4<T>& a) {
    return Vector4<T>( ::sqrt(a.x()),
                       ::sqrt(a.y()),
                       ::sqrt(a.z()),
                       ::sqrt(a.w()) );
  }

  // --------------- inversesqrt ---------------

  template<typename T>
  T inversesqrt(T a) { return ::sqrt(a); }

  template<typename T>
  Vector2<T> inversesqrt(const Vector2<T>& a) {
    return Vector2<T>( T(1) / ::sqrt(a.x()),
                       T(1) / ::sqrt(a.y()) );
  }

  template<typename T>
  Vector3<T> inversesqrt(const Vector3<T>& a) {
    return Vector3<T>( T(1) / ::sqrt(a.x()),
                       T(1) / ::sqrt(a.y()),
                       T(1) / ::sqrt(a.z()) );
  }

  template<typename T>
  Vector4<T> inversesqrt(const Vector4<T>& a) {
    return Vector4<T>( T(1) / ::sqrt(a.x()),
                       T(1) / ::sqrt(a.y()),
                       T(1) / ::sqrt(a.z()),
                       T(1) / ::sqrt(a.w()) );
  }

  // --------------- common functions ---------------

  // --------------- abs ---------------

  template<typename T>
  T abs(T a) { return a >= 0 ? a : -a; }

  template<typename T>
  Vector2<T> abs(const Vector2<T>& a)
  {
    return Vector2<T>( a.x() >= 0 ? a.x() : -a.x(), a.y() >= 0 ? a.y() : -a.y() );
  }

  template<typename T>
  Vector3<T> abs(const Vector3<T>& a)
  {
    return Vector3<T>( a.x() >= 0 ? a.x() : -a.x(), a.y() >= 0 ? a.y() : -a.y(),  a.z() >= 0 ? a.z() : -a.z() );
  }

  template<typename T>
  Vector4<T> abs(const Vector4<T>& a)
  {
    return Vector4<T>( a.x() >= 0 ? a.x() : -a.x(), a.y() >= 0 ? a.y() : -a.y(), a.z() >= 0 ? a.z() : -a.z(), a.w() >= 0 ? a.w() : -a.w() );
  }

  // --------------- sign ---------------

  template<typename T>
  T sign(T a) { return a > 0 ? 1 : a == 0 ? 0 : (T)-1; }

  template<typename T>
  Vector2<T> sign(const Vector2<T> & a)
  {
    return Vector2<T>( a.x() > 0 ? 1 : a.x() == 0 ? 0 : (T)-1,
                       a.y() > 0 ? 1 : a.y() == 0 ? 0 : (T)-1 );
  }

  template<typename T>
  Vector3<T> sign(const Vector3<T> & a)
  {
    return Vector3<T>( a.x() > 0 ? 1 : a.x() == 0 ? 0 : (T)-1,
                       a.y() > 0 ? 1 : a.y() == 0 ? 0 : (T)-1,
                       a.z() > 0 ? 1 : a.z() == 0 ? 0 : (T)-1 );
  }

  template<typename T>
  Vector4<T> sign(const Vector4<T> & a)
  {
    return Vector4<T>( a.x() > 0 ? 1 : a.x() == 0 ? 0 : (T)-1,
                       a.y() > 0 ? 1 : a.y() == 0 ? 0 : (T)-1,
                       a.z() > 0 ? 1 : a.z() == 0 ? 0 : (T)-1,
                       a.w() > 0 ? 1 : a.w() == 0 ? 0 : (T)-1 );
  }

  // --------------- floor ---------------

  template<typename T>
  T floor(T a) { return ::floor(a); }

  template<typename T>
  Vector2<T> floor(const Vector2<T>& a) {
    return Vector2<T>( ::floor(a.x()),
                       ::floor(a.y()) );
  }

  template<typename T>
  Vector3<T> floor(const Vector3<T>& a) {
    return Vector3<T>( ::floor(a.x()),
                       ::floor(a.y()),
                       ::floor(a.z()) );
  }

  template<typename T>
  Vector4<T> floor(const Vector4<T>& a) {
    return Vector4<T>( ::floor(a.x()),
                       ::floor(a.y()),
                       ::floor(a.z()),
                       ::floor(a.w()) );
  }

  // --------------- fract ---------------

  template<typename T>
  T fract(T a) { return a - floor(a); }

  template<typename T>
  Vector2<T> fract(const Vector2<T>& a) { return a - floor(a); }

  template<typename T>
  Vector3<T> fract(const Vector3<T>& a) { return a - floor(a); }

  template<typename T>
  Vector4<T> fract(const Vector4<T>& a) { return a - floor(a); }

  // --------------- trunc ---------------

  template<typename T>
  T trunc(T a) { return a - fract(a); }

  template<typename T>
  Vector2<T> trunc(const Vector2<T>& a) {
    return Vector2<T>( a.x() - fract(a.x()),
                  a.y() - fract(a.y()) );
  }

  template<typename T>
  Vector3<T> trunc(const Vector3<T>& a) {
    return Vector3<T>( a.x() - fract(a.x()),
                  a.y() - fract(a.y()),
                  a.z() - fract(a.z()) );
  }

  template<typename T>
  Vector4<T> trunc(const Vector4<T>& a) {
    return Vector4<T>( a.x() - fract(a.x()),
                  a.y() - fract(a.y()),
                  a.z() - fract(a.z()),
                  a.w() - fract(a.w()) );
  }

  // --------------- round ---------------

  template<typename T>
  T round(T x) { return ((x - floor(x)) >= 0.5) ? ceil(x) : floor(x); }

  template<typename T>
  Vector2<T> round(const Vector2<T>& a) {
    return Vector2<T>( round(a.x()),
                  round(a.y()) );
  }

  template<typename T>
  Vector3<T> round(const Vector3<T>& a) {
    return Vector3<T>( round(a.x()),
                  round(a.y()),
                  round(a.z()) );
  }

  template<typename T>
  Vector4<T> round(const Vector4<T>& a) {
    return Vector4<T>( round(a.x()),
                  round(a.y()),
                  round(a.z()),
                  round(a.w()) );
  }

  // --------------- modf ---------------

  inline
  float modf(float a, float& intpart) {
    #if defined(_MSC_VER)
      return ::modf(a,&intpart);
    #else
      double dintpart = intpart;
      float r = (float)::modf((double)a,&dintpart);
      intpart = (float)dintpart;
      return r;
    #endif
  }

  inline
  double modf(double a, double& intpart) { return ::modf(a,&intpart); }

  template<typename T>
  Vector2<T> modf(const Vector2<T>& a, Vector2<T>& intpart) {
    return Vector2<T>( modf(a.x(), intpart.x()),
                  modf(a.y(), intpart.y()) );
  }

  template<typename T>
  Vector3<T> modf(const Vector3<T>& a, Vector3<T>& intpart) {
    return Vector3<T>( modf(a.x(), intpart.x()),
                  modf(a.y(), intpart.y()),
                  modf(a.z(), intpart.z()) );
  }

  template<typename T>
  Vector4<T> modf(const Vector4<T>& a, Vector4<T>& intpart) {
    return Vector4<T>( modf(a.x(), intpart.x()),
                  modf(a.y(), intpart.y()),
                  modf(a.z(), intpart.z()),
                  modf(a.w(), intpart.w()) );
  }

  // --------------- roundEven ---------------

  inline 
  float roundEven(float a, float epsilon)
  {
    if( a < 0 )
      return -roundEven(-a, epsilon);
    else
    {
      float intpart;
      vl::modf( a, intpart );

      // 0.5 case
      if ((a -(intpart + 0.5f)) < epsilon)
      {
        // is even
        if (::fmod(intpart, 2) < epsilon)
          return intpart;
        else
        // is odd
          return ceil(intpart + 0.5f);
      }
      else
      // all the other cases
        return round(a);
    }
  }

  inline 
  double roundEven(double a, double epsilon)
  {
    if( a < 0 )
      return -roundEven(-a, epsilon);
    else
    {
      double intpart;
      vl::modf( a, intpart );

      // 0.5 case
      if ((a -(intpart + 0.5)) < epsilon)
      {
        // is even
        if (::fmod(intpart, 2) < epsilon)
          return intpart;
        else
        // is odd
          return ceil(intpart + 0.5);
      }
      else
      // all the other cases
        return round(a);
    }
  }

  template<typename T>
  Vector2<T> roundEven(const Vector2<T>& a, T epsilon = 0.00001) {
    return Vector2<T>( roundEven(a.x(), epsilon),
                       roundEven(a.y(), epsilon) );
  }

  template<typename T>
  Vector3<T> roundEven(const Vector3<T>& a, T epsilon = 0.00001) {
    return Vector3<T>( roundEven(a.x(), epsilon),
                       roundEven(a.y(), epsilon),
                       roundEven(a.z(), epsilon) );
  }

  template<typename T>
  Vector4<T> roundEven(const Vector4<T>& a, T epsilon = 0.00001) {
    return Vector4<T>( roundEven(a.x(), epsilon),
                       roundEven(a.y(), epsilon),
                       roundEven(a.z(), epsilon),
                       roundEven(a.w(), epsilon) );
  }

  // --------------- ceil ---------------

  template<typename T>
  T ceil(T a) { return ::ceil(a); }

  template<typename T>
  Vector2<T> ceil(const Vector2<T>& a) {
    return Vector2<T>( ::ceil(a.x()),
                       ::ceil(a.y()) );
  }

  template<typename T>
  Vector3<T> ceil(const Vector3<T>& a) {
    return Vector3<T>( ::ceil(a.x()),
                       ::ceil(a.y()),
                       ::ceil(a.z()) );
  }

  template<typename T>
  Vector4<T> ceil(const Vector4<T>& a) {
    return Vector4<T>( ::ceil(a.x()),
                       ::ceil(a.y()),
                       ::ceil(a.z()),
                       ::ceil(a.w()) );
  }

  // --------------- mod ---------------

  template<typename T>
  T mod(T a, T b) { return a - b * floor(a/b); }

  template<typename T>
  Vector2<T> mod(const Vector2<T>& a, T b) { return a - b * floor(a/b); }

  template<typename T>
  Vector3<T> mod(const Vector3<T>& a, T b) { return a - b * floor(a/b); }

  template<typename T>
  Vector4<T> mod(const Vector4<T>& a, T b) { return a - b * floor(a/b); }

  template<typename T>
  Vector2<T> mod(const Vector2<T>& a, const Vector2<T>& b) { return a - b * floor(a/b); }

  template<typename T>
  Vector3<T> mod(const Vector3<T>& a, const Vector3<T>& b) { return a - b * floor(a/b); }

  template<typename T>
  Vector4<T> mod(const Vector4<T>& a, const Vector4<T>& b) { return a - b * floor(a/b); }

  // --------------- mix ---------------

  template<typename T>
  T mix(T a, T b, T t) { return a*(1-t) + b*t; }

  template<typename T>
  Vector2<T> mix(const Vector2<T>& a, const Vector2<T>& b, T t) { return a*(1-t) + b*t; }

  template<typename T>
  Vector3<T> mix(const Vector3<T>& a, const Vector3<T>& b, T t) { return a*(1-t) + b*t; }

  template<typename T>
  Vector4<T> mix(const Vector4<T>& a, const Vector4<T>& b, T t) { return a*(1-t) + b*t; }

  template<typename T>
  Vector2<T> mix(const Vector2<T>& a, const Vector2<T>& b, const Vector2<T>& t)
  {
    return Vector2<T>( a.x()*(1-t.x()) + b.x()*t.x(),
                  a.y()*(1-t.y()) + b.y()*t.y() );
  }

  template<typename T>
  Vector3<T> mix(const Vector3<T>& a, const Vector3<T>& b, const Vector3<T>& t)
  {
    return Vector3<T>( a.x()*(1-t.x()) + b.x()*t.x(),
                  a.y()*(1-t.y()) + b.y()*t.y(),
                  a.z()*(1-t.z()) + b.z()*t.z() );
  }

  template<typename T>
  Vector4<T> mix(const Vector4<T>& a, const Vector4<T>& b, const Vector4<T>& t)
  {
    return Vector4<T>( a.x()*(1-t.x()) + b.x()*t.x(),
                  a.y()*(1-t.y()) + b.y()*t.y(),
                  a.z()*(1-t.z()) + b.z()*t.z(),
                  a.w()*(1-t.w()) + b.w()*t.w() );
  }

  // --------------- step ---------------

  template<typename T>
  T step( T edge, T a ) { if (a<edge) return 0; else return 1; }

  template<typename T>
  Vector2<T> step( const Vector2<T>& edge, const Vector2<T>& a )
  {
    return Vector2<T>( a.x()<edge.x() ? 0 : (T)1,
                       a.y()<edge.y() ? 0 : (T)1 );
  }

  template<typename T>
  Vector3<T> step( const Vector3<T>& edge, const Vector3<T>& a )
  {
    return Vector3<T>( a.x()<edge.x() ? 0 : (T)1,
                       a.y()<edge.y() ? 0 : (T)1,
                       a.z()<edge.z() ? 0 : (T)1 );
  }

  template<typename T>
  Vector4<T> step( const Vector4<T>& edge, const Vector4<T>& a )
  {
    return Vector4<T>( a.x()<edge.x() ? 0 : (T)1,
                       a.y()<edge.y() ? 0 : (T)1,
                       a.z()<edge.z() ? 0 : (T)1,
                       a.w()<edge.w() ? 0 : (T)1 );
  }
  // --------------- smoothstep ---------------

  template<typename T>
  T smoothstep(T edge0, T edge1, T a)
  {
    T t = clamp( (a - edge0) / (edge1 - edge0), (T)0, (T)1);
    return t * t * (3 - 2 * t);
  }

  template<typename T>
  Vector2<T> smoothstep(const Vector2<T>& edge0, const Vector2<T>& edge1, const Vector2<T>& a)
  {
    Vector2<T> v;
    T t;
    t = clamp( (a.x() - edge0.x()) / (edge1.x() - edge0.x()), (T)0, (T)1); v.x() = t * t * (3 - 2 * t);
    t = clamp( (a.y() - edge0.y()) / (edge1.y() - edge0.y()), (T)0, (T)1); v.y() = t * t * (3 - 2 * t);
    return v;
  }

  template<typename T>
  Vector3<T> smoothstep(const Vector3<T>& edge0, const Vector3<T>& edge1, const Vector3<T>& a)
  {
    Vector3<T> v;
    T t;
    t = clamp( (a.x() - edge0.x()) / (edge1.x() - edge0.x()), (T)0, (T)1); v.x() = t * t * (3 - 2 * t);
    t = clamp( (a.y() - edge0.y()) / (edge1.y() - edge0.y()), (T)0, (T)1); v.y() = t * t * (3 - 2 * t);
    t = clamp( (a.z() - edge0.z()) / (edge1.z() - edge0.z()), (T)0, (T)1); v.z() = t * t * (3 - 2 * t);
    return v;
  }

  template<typename T>
  Vector4<T> smoothstep(const Vector4<T>& edge0, const Vector4<T>& edge1, const Vector4<T>& a)
  {
    Vector4<T> v;
    T t;
    t = clamp( (a.x() - edge0.x()) / (edge1.x() - edge0.x()), (T)0, (T)1); v.x() = t * t * (3 - 2 * t);
    t = clamp( (a.y() - edge0.y()) / (edge1.y() - edge0.y()), (T)0, (T)1); v.y() = t * t * (3 - 2 * t);
    t = clamp( (a.z() - edge0.z()) / (edge1.z() - edge0.z()), (T)0, (T)1); v.z() = t * t * (3 - 2 * t);
    t = clamp( (a.w() - edge0.w()) / (edge1.w() - edge0.w()), (T)0, (T)1); v.w() = t * t * (3 - 2 * t);
    return v;
  }

  // --------------- isnan ---------------

  template<typename T>
  ivec2 isnan(const Vector2<T>& a) { return ivec2( isnan(a.x()), isnan(a.y()) ); }

  template<typename T>
  ivec3 isnan(const Vector3<T>& a) { return ivec3( isnan(a.x()), isnan(a.y()), isnan(a.z()) ); }

  template<typename T>
  ivec4 isnan(const Vector4<T>& a) { return ivec4( isnan(a.x()), isnan(a.y()), isnan(a.z()), isnan(a.w()) ); }

  // --------------- isinf ---------------

  template<typename T>
  ivec2 isinf(const Vector2<T>& a) { return ivec2( isinf(a.x()), isinf(a.y()) ); }

  template<typename T>
  ivec3 isinf(const Vector3<T>& a) { return ivec3( isinf(a.x()), isinf(a.y()), isinf(a.z()) ); }

  template<typename T>
  ivec4 isinf(const Vector4<T>& a) { return ivec4( isinf(a.x()), isinf(a.y()), isinf(a.z()), isinf(a.w()) ); }

  // --------------- geometric functions ---------------

  // --------------- length ---------------

  template<typename T>
  T length(T v) { return v; }

  template<typename T>
  T length(const Vector2<T>& v) { return v.length(); }

  template<typename T>
  T length(const Vector3<T>& v) { return v.length(); }

  template<typename T>
  T length(const Vector4<T>& v) { return v.length(); }

  // --------------- distance ---------------

  template<typename T>
  T distance(T p0, T p1) { return length(p0-p1); }

  template<typename T>
  T distance(const Vector2<T>& p0, const Vector2<T>& p1) { return length(p0-p1); }

  template<typename T>
  T distance(const Vector3<T>& p0, const Vector3<T>& p1) { return length(p0-p1); }

  template<typename T>
  T distance(const Vector4<T>& p0, const Vector4<T>& p1) { return length(p0-p1); }

  // --------------- dot ---------------

  inline float dot(float a, float b) { return a*b; }

  // ....................................

  inline double dot(double a, double b) { return a*b; }

  // ....................................

  inline real dot(int a, int b) { return (real)a*b; }

  // ....................................

  inline real dot(unsigned int a, unsigned int b) { return (real)a*b; }

  // --------------- normalize ---------------

  template<typename T>
  T normalize(T) { return (T)1; }

  template<typename T>
  Vector2<T> normalize(const Vector2<T>& v) { Vector2<T> t = v; t.normalize(); return t; }

  template<typename T>
  Vector3<T> normalize(const Vector3<T>& v) { Vector3<T> t = v; t.normalize(); return t; }

  template<typename T>
  Vector4<T> normalize(const Vector4<T>& v) { Vector4<T> t = v; t.normalize(); return t; }

  // --------------- faceforward ---------------

  template<typename T>
  T faceforward(T N, T I, T Nref) { if ( dot(Nref,I) < 0 ) return N; else return -N; }

  template<typename T>
  Vector2<T> faceforward(const Vector2<T>& N, const Vector2<T>& I, const Vector2<T>& Nref) { if ( dot(Nref,I) < 0 ) return N; else return -N; }

  template<typename T>
  Vector3<T> faceforward(const Vector3<T>& N, const Vector3<T>& I, const Vector3<T>& Nref) { if ( dot(Nref,I) < 0 ) return N; else return -N; }

  template<typename T>
  Vector4<T> faceforward(const Vector4<T>& N, const Vector4<T>& I, const Vector4<T>& Nref) { if ( dot(Nref,I) < 0 ) return N; else return -N; }

  // --------------- reflect ----------------

  template<typename T>
  T reflect(T I, T N) { return I-2*dot(N,I)*N; }

  template<typename T>
  Vector2<T> reflect(const Vector2<T>& I, const Vector2<T>& N) { return I-2*dot(N,I)*N; }

  template<typename T>
  Vector3<T> reflect(const Vector3<T>& I, const Vector3<T>& N) { return I-2*dot(N,I)*N; }

  template<typename T>
  Vector4<T> reflect(const Vector4<T>& I, const Vector4<T>& N) { return I-2*dot(N,I)*N; }

  // --------------- refract ---------------

  template<typename T>
  T refract(T I, T N, T eta)
  {
    T k = 1 - eta * eta * (1 - dot(N, I) * dot(N, I));
    if (k < 0)
      return 0;
    else
      return eta * I - (eta * dot(N, I) + ::sqrt(k)) * N;
  }

  template<typename T>
  Vector2<T> refract(const Vector2<T>& I, const Vector2<T>& N, T eta)
  {
    T k = 1 - eta * eta * (1 - dot(N, I) * dot(N, I));
    if (k < 0)
      return Vector2<T>(0,0);
    else
      return eta * I - N * (eta * dot(N, I) + ::sqrt(k));
  }

  template<typename T>
  Vector3<T> refract(const Vector3<T>& I, const Vector3<T>& N, T eta)
  {
    T k = 1 - eta * eta * (1 - dot(N, I) * dot(N, I));
    if (k < 0)
      return Vector3<T>(0,0,0);
    else
      return eta * I - N * (eta * dot(N, I) + ::sqrt(k));
  }

  template<typename T>
  Vector4<T> refract(const Vector4<T>& I, const Vector4<T>& N, T eta)
  {
    T k = 1 - eta * eta * (1 - dot(N, I) * dot(N, I));
    if (k < 0)
      return Vector4<T>(0,0,0,0);
    else
      return eta * I - N * (eta * dot(N, I) + ::sqrt(k));
  }

  // --------------- matrix functions ---------------

  // --------------- matrixCompMult ---------------

  template<typename T>
  Matrix2<T> matrixCompMult(const Matrix2<T>& a, const Matrix2<T>& b)
  {
    Matrix2<T> t;
    for(int i=0; i<2; ++i)
      for(int j=0; j<2; ++j)
        t.e(j,i) = a.e(j,i) * b.e(j,i);
    return t;
  }

  template<typename T>
  Matrix3<T> matrixCompMult(const Matrix3<T>& a, const Matrix3<T>& b)
  {
    Matrix3<T> t;
    for(int i=0; i<3; ++i)
      for(int j=0; j<3; ++j)
        t.e(j,i) = a.e(j,i) * b.e(j,i);
    return t;
  }

  template<typename T>
  Matrix4<T> matrixCompMult(const Matrix4<T>& a, const Matrix4<T>& b)
  {
    Matrix4<T> t;
    for(int i=0; i<4; ++i)
      for(int j=0; j<4; ++j)
        t.e(j,i) = a.e(j,i) * b.e(j,i);
    return t;
  }

  // --------------- outerProduct ---------------

  template<typename T>
  Matrix2<T> outerProduct(const Vector2<T>& a, const Vector2<T>& b)
  {
    Matrix2<T> m;
    for(int i=0; i<2; ++i)
      for(int j=0; j<2; ++j)
        m.e(i,j) = a[i] * b[j];
    return m;
  }

  template<typename T>
  Matrix3<T> outerProduct(const Vector3<T>& a, const Vector3<T>& b)
  {
    Matrix3<T> m;
    for(int i=0; i<3; ++i)
      for(int j=0; j<3; ++j)
        m.e(i,j) = a[i] * b[j];
    return m;
  }

  template<typename T>
  Matrix4<T> outerProduct(const Vector4<T>& a, const Vector4<T>& b)
  {
    Matrix4<T> m;
    for(int i=0; i<4; ++i)
      for(int j=0; j<4; ++j)
        m.e(i,j) = a[i] * b[j];
    return m;
  }

  // --------------- transpose ---------------

  template<typename T>
  Matrix2<T> transpose(const Matrix2<T>& a)
  {
    Matrix2<T> t;
    for(int i=0; i<2; ++i)
      for(int j=0; j<2; ++j)
        t.e(j,i) = a.e(i,j);
    return t;
  }

  template<typename T>
  Matrix3<T> transpose(const Matrix3<T>& a)
  {
    Matrix3<T> t;
    for(int i=0; i<3; ++i)
      for(int j=0; j<3; ++j)
        t.e(j,i) = a.e(i,j);
    return t;
  }

  template<typename T>
  Matrix4<T> transpose(const Matrix4<T>& a)
  {
    Matrix4<T> t;
    for(int i=0; i<4; ++i)
      for(int j=0; j<4; ++j)
        t.e(j,i) = a.e(i,j);
    return t;
  }

  // --------------- vector relational functions ---------------

  // --------------- lessThan ---------------

  template<typename T>
  ivec4 lessThan(const Vector4<T>& a, const Vector4<T>& b) {
    return ivec4( a.x() < b.x() ? 1 : 0,
                  a.y() < b.y() ? 1 : 0,
                  a.z() < b.z() ? 1 : 0,
                  a.w() < b.w() ? 1 : 0 );
  }

  template<typename T>
  ivec3 lessThan(const Vector3<T>& a, const Vector3<T>& b) {
    return ivec3( a.x() < b.x() ? 1 : 0,
                  a.y() < b.y() ? 1 : 0,
                  a.z() < b.z() ? 1 : 0 );
  }

  template<typename T>
  ivec2 lessThan(const Vector2<T>& a, const Vector2<T>& b) {
    return ivec2( a.x() < b.x() ? 1 : 0,
                  a.y() < b.y() ? 1 : 0 );
  }

  // --------------- lessThanEqual ---------------

  template<typename T>
  ivec4 lessThanEqual(const Vector4<T>& a, const Vector4<T>& b) {
    return ivec4( a.x() <= b.x() ? 1 : 0,
                  a.y() <= b.y() ? 1 : 0,
                  a.z() <= b.z() ? 1 : 0,
                  a.w() <= b.w() ? 1 : 0 );
  }

  template<typename T>
  ivec3 lessThanEqual(const Vector3<T>& a, const Vector3<T>& b) {
    return ivec3( a.x() <= b.x() ? 1 : 0,
                  a.y() <= b.y() ? 1 : 0,
                  a.z() <= b.z() ? 1 : 0 );
  }

  template<typename T>
  ivec2 lessThanEqual(const Vector2<T>& a, const Vector2<T>& b) {
    return ivec2( a.x() <= b.x() ? 1 : 0,
                  a.y() <= b.y() ? 1 : 0 );
  }

  // --------------- greaterThan ---------------

  template<typename T>
  ivec4 greaterThan(const Vector4<T>& a, const Vector4<T>& b) {
    return ivec4( a.x() > b.x() ? 1 : 0,
                  a.y() > b.y() ? 1 : 0,
                  a.z() > b.z() ? 1 : 0,
                  a.w() > b.w() ? 1 : 0 );
  }

  template<typename T>
  ivec3 greaterThan(const Vector3<T>& a, const Vector3<T>& b) {
    return ivec3( a.x() > b.x() ? 1 : 0,
                  a.y() > b.y() ? 1 : 0,
                  a.z() > b.z() ? 1 : 0 );
  }

  template<typename T>
  ivec2 greaterThan(const Vector2<T>& a, const Vector2<T>& b) {
    return ivec2( a.x() > b.x() ? 1 : 0,
                  a.y() > b.y() ? 1 : 0 );
  }

  // --------------- greaterThanEqual ---------------

  template<typename T>
  ivec4 greaterThanEqual(const Vector4<T>& a, const Vector4<T>& b) {
    return ivec4( a.x() >= b.x() ? 1 : 0,
                  a.y() >= b.y() ? 1 : 0,
                  a.z() >= b.z() ? 1 : 0,
                  a.w() >= b.w() ? 1 : 0 );
  }

  template<typename T>
  ivec3 greaterThanEqual(const Vector3<T>& a, const Vector3<T>& b) {
    return ivec3( a.x() >= b.x() ? 1 : 0,
                  a.y() >= b.y() ? 1 : 0,
                  a.z() >= b.z() ? 1 : 0 );
  }

  template<typename T>
  ivec2 greaterThanEqual(const Vector2<T>& a, const Vector2<T>& b) {
    return ivec2( a.x() >= b.x() ? 1 : 0,
                  a.y() >= b.y() ? 1 : 0 );
  }

  // --------------- equal ---------------

  template<typename T>
  ivec4 equal(const Vector4<T>& a, const Vector4<T>& b) {
    return ivec4( a.x() == b.x() ? 1 : 0,
                  a.y() == b.y() ? 1 : 0,
                  a.z() == b.z() ? 1 : 0,
                  a.w() == b.w() ? 1 : 0 );
  }

  template<typename T>
  ivec3 equal(const Vector3<T>& a, const Vector3<T>& b) {
    return ivec3( a.x() == b.x() ? 1 : 0,
                  a.y() == b.y() ? 1 : 0,
                  a.z() == b.z() ? 1 : 0 );
  }

  template<typename T>
  ivec2 equal(const Vector2<T>& a, const Vector2<T>& b) {
    return ivec2( a.x() == b.x() ? 1 : 0,
                  a.y() == b.y() ? 1 : 0 );
  }

  // --------------- notEqual ---------------

  template<typename T>
  ivec4 notEqual(const Vector4<T>& a, const Vector4<T>& b) {
    return ivec4( a.x() != b.x() ? 1 : 0,
                  a.y() != b.y() ? 1 : 0,
                  a.z() != b.z() ? 1 : 0,
                  a.w() != b.w() ? 1 : 0 );
  }

  template<typename T>
  ivec3 notEqual(const Vector3<T>& a, const Vector3<T>& b) {
    return ivec3( a.x() != b.x() ? 1 : 0,
                  a.y() != b.y() ? 1 : 0,
                  a.z() != b.z() ? 1 : 0 );
  }

  template<typename T>
  ivec2 notEqual(const Vector2<T>& a, const Vector2<T>& b) {
    return ivec2( a.x() != b.x() ? 1 : 0,
                  a.y() != b.y() ? 1 : 0 );
  }

  // --------------- any ---------------

  inline bool any(const ivec2& a) { return a.x() != 0 || a.y() != 0; }
  inline bool any(const ivec3& a) { return a.x() != 0 || a.y() != 0 || a.z() != 0; }
  inline bool any(const ivec4& a) { return a.x() != 0 || a.y() != 0 || a.z() != 0 || a.w() != 0; }

  // --------------- all ---------------

  inline bool all(const ivec2& a) { return a.x() != 0 && a.y() != 0; }
  inline bool all(const ivec3& a) { return a.x() != 0 && a.y() != 0 && a.z() != 0; }
  inline bool all(const ivec4& a) { return a.x() != 0 && a.y() != 0 && a.z() != 0 && a.w() != 0; }

  // --------------- not ---------------

#if defined(_MSC_VER)
  inline ivec2 not(const ivec2& a) { return ivec2( a.x() != 0 ? 0 : 1, a.y() != 0 ? 0 : 1); }
  inline ivec3 not(const ivec3& a) { return ivec3( a.x() != 0 ? 0 : 1, a.y() != 0 ? 0 : 1, a.z() != 0 ? 0 : 1); }
  inline ivec4 not(const ivec4& a) { return ivec4( a.x() != 0 ? 0 : 1, a.y() != 0 ? 0 : 1, a.z() != 0 ? 0 : 1, a.w() != 0 ? 0 : 1 ); }
#endif
}

#endif
