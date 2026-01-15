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

#ifndef stdtypes_INCLUDE_ONCE
#define stdtypes_INCLUDE_ONCE

#include <vlCore/config.hpp>
#include <vlCore/checks.hpp>

namespace vl
{
  /** 8 bits signed integer */
  typedef char i8;
  /** 8 bits unsigned integer */
  typedef unsigned char u8;
  /** 16 bits signed integer */
  typedef short i16;
  /** 16 bits unsigned integer */
  typedef unsigned short u16;
  /** 32 bits signed integer */
  typedef int i32;
  /** 32 bits unsigned integer */
  typedef unsigned int u32;
  /** 64 bits signed integer */
  typedef long long i64;
  /** 64 bits unsigned integer */
  typedef unsigned long long u64;
  /** 32 bits floating point value */
  typedef float f32;
  /** 64 bits floating point value */
  typedef double f64;

  // trigonometric constants

  //! Greek Pi constant using \p double precision.
  const double dPi = 3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117067982148086513282306647093845;
  //! Constant to convert degree into radian using \p double precision.
  const double dDEG_TO_RAD = dPi / 180.0;
  //! Constant to convert radian into degree using \p double precision.
  const double dRAD_TO_DEG = 180.0 / dPi;

  //! Greek Pi constant using \p float precision.
  const float fPi = (float)3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117067982148086513282306647093845;
  //! Constant to convert degree into radian using \p float precision.
  const float fDEG_TO_RAD = float(dPi / 180.0);
  //! Constant to convert radian into degree using \p float precision.
  const float fRAD_TO_DEG = float(180.0 / dPi);

  class degree;

  /** Simple class representing quantities in radians, converts automatically to vl::degree and real. */
  class radian
  {
  public:
    radian(real val): mValue(val) {}
    operator real() { return mValue; }
    operator degree();

  private:
    real mValue;
  };

  /** Simple class representing quantities in degrees, converts automatically to vl::radian and real. */
  class degree
  {
  public:
    degree(real val): mValue(val) {}
    operator real() { return mValue; }
    operator radian();

  private:
    real mValue;
  };

  inline radian::operator degree() { return mValue*(real)dRAD_TO_DEG; }
  inline degree::operator radian() { return mValue*(real)dDEG_TO_RAD; }

  //! Swaps the byte order of the given object.
  template<typename T>
  void swapBytes(T& value)
  {
    union 
    {
      T* value;
      char* ptr;
    } u;
    u.value = &value;
    char* a = u.ptr;
    char* b = u.ptr + sizeof(T) - 1;
    while(a<b)
    {
      char tmp = *a;
      *a = *b;
      *b = tmp;
      ++a;
      --b;
    }
  }

  VL_COMPILE_TIME_CHECK( sizeof(i8)*8  == 8 );
  VL_COMPILE_TIME_CHECK( sizeof(u8)*8  == 8 );
  VL_COMPILE_TIME_CHECK( sizeof(i16)*8 == 16 );
  VL_COMPILE_TIME_CHECK( sizeof(u16)*8 == 16 );
  VL_COMPILE_TIME_CHECK( sizeof(i32)*8 == 32 );
  VL_COMPILE_TIME_CHECK( sizeof(u32)*8 == 32 );
  VL_COMPILE_TIME_CHECK( sizeof(i64)*8 == 64 );
  VL_COMPILE_TIME_CHECK( sizeof(u64)*8 == 64 );
  VL_COMPILE_TIME_CHECK( sizeof(f32)*8 == 32 );
  VL_COMPILE_TIME_CHECK( sizeof(f64)*8 == 64 );
}

#endif
