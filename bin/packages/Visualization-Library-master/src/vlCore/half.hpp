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

#ifndef HalfFloat_INCLUDE_ONCE
#define HalfFloat_INCLUDE_ONCE

#include <vlCore/Matrix4.hpp>

namespace vl
{
  //! Represents an half-precision floating point value.
  class half
  {
  public:
    half(): bits(0) {}
    
    half(const half& hf): bits(hf.bits) {}
    
    half(int i): bits(convertFloatToHalf((float)i).bits) {}
    
    half(long long i): bits(convertFloatToHalf((float)i).bits) {}
    
    half(float f): bits(convertFloatToHalf(f).bits) {}
    
    half(double d): bits(convertFloatToHalf((float)d).bits) {}

    operator float() const
    {
      return convertHalfToFloat(*this);
    }

    operator double() const
    {
      return (double)convertHalfToFloat(*this);
    }

    operator int() const
    {
      return (int)convertHalfToFloat(*this);
    }

    operator long long() const
    {
      return (long long)convertHalfToFloat(*this);
    }

    half& operator=(const half& other)
    {
      bits = other.bits;
      return *this;
    }

    half operator+(const half& other) const
    {
      return convertFloatToHalf( convertHalfToFloat(*this) + convertHalfToFloat(other) );
    }

    half& operator+=(const half& other)
    {
      return *this = convertFloatToHalf( convertHalfToFloat(*this) + convertHalfToFloat(other) );
    }

    half operator-(const half& other) const
    {
      return convertFloatToHalf( convertHalfToFloat(*this) - convertHalfToFloat(other) );
    }

    half& operator-=(const half& other)
    {
      return *this = convertFloatToHalf( convertHalfToFloat(*this) - convertHalfToFloat(other) );
    }

    half operator*(const half& other) const
    {
      return convertFloatToHalf( convertHalfToFloat(*this) * convertHalfToFloat(other) );
    }

    half& operator*=(const half& other)
    {
      return *this = convertFloatToHalf( convertHalfToFloat(*this) * convertHalfToFloat(other) );
    }

    half operator/(const half& other) const
    {
      return convertFloatToHalf( convertHalfToFloat(*this) / convertHalfToFloat(other) );
    }

    half& operator/=(const half& other)
    {
      return *this = convertFloatToHalf( convertHalfToFloat(*this) / convertHalfToFloat(other) );
    }

    bool isZero() const
    {
      return (bits & ((1 << 15)-1)) == 0;
    }

    operator bool() const
    {
      return !isZero();
    }

    bool operator==(const half& other) const
    {
      if (isNaN() && other.isNaN())
        return false;
      else
      if (isZero() && other.isZero())
        return true;
      else
        return bits == other.bits;
    }

    bool operator==(const float& other) const
    {
      return operator==( convertFloatToHalf(other) );
    }

    bool operator==(const double& other) const
    {
      return operator==( convertFloatToHalf((float)other) );
    }

    bool operator==(const int& other) const
    {
      return operator==( convertFloatToHalf((float)other) );
    }

    bool operator==(const long long& other) const
    {
      return operator==( convertFloatToHalf((float)other) );
    }

    bool operator!=(const half& other) const
    {
      if (isNaN() && other.isNaN())
        return false;
      else
      if (isZero() && other.isZero())
        return false;
      else
        return bits != other.bits;
    }

    bool operator!=(const float& other) const
    {
      return operator!=( convertFloatToHalf(other) );
    }

    bool operator!=(const double& other) const
    {
      return operator!=( convertFloatToHalf((float)other) );
    }

    bool operator!=(const int& other) const
    {
      return operator!=( convertFloatToHalf((float)other) );
    }

    bool operator!=(const long long& other) const
    {
      return operator!=( convertFloatToHalf((float)other) );
    }

    bool operator<(const half& other) const
    {
      return convertHalfToFloat(*this) < convertHalfToFloat(other);
    }

    bool operator>(const half& other) const
    {
      return convertHalfToFloat(*this) < convertHalfToFloat(other);
    }

    bool isNaN() const
    {
      unsigned int mantissa = (unsigned int) (bits & (( 1 << 10) - 1) );
      unsigned int exp = (unsigned int) (bits & HALF_FLOAT_MAX_BIASED_EXP);
      return exp == HALF_FLOAT_MAX_BIASED_EXP && mantissa != 0;
    }

    bool isinf() const
    {
      unsigned int mantissa = (unsigned int) (bits & (( 1 << 10) - 1) );
      unsigned int exp = (unsigned int) (bits & HALF_FLOAT_MAX_BIASED_EXP);
      return exp == HALF_FLOAT_MAX_BIASED_EXP && mantissa == 0;
    }

    bool isinf_pos() const
    {
      unsigned int sign = (unsigned int) ( bits >> 15);
      unsigned int mantissa = (unsigned int) (bits & (( 1 << 10) - 1) );
      unsigned int exp = (unsigned int) (bits & HALF_FLOAT_MAX_BIASED_EXP);
      return exp == HALF_FLOAT_MAX_BIASED_EXP && mantissa == 0 && sign == 0;
    }

    bool isinf_neg() const
    {
      unsigned int sign = (unsigned int) ( bits >> 15);
      unsigned int mantissa = (unsigned int) (bits & (( 1 << 10) - 1) );
      unsigned int exp = (unsigned int) (bits & HALF_FLOAT_MAX_BIASED_EXP);
      return exp == HALF_FLOAT_MAX_BIASED_EXP && mantissa == 0 && sign == 1;
    }

    bool isdenorm() const
    {
      unsigned int mantissa = (unsigned int) (bits & (( 1 << 10) - 1) );
      unsigned int exp = (unsigned int) (bits & HALF_FLOAT_MAX_BIASED_EXP);
      return exp == 0 && mantissa != 0;
    }

    half operator-() const
    {
      half h = *this;
      h.bits ^= 1 << 15;
      return h;
    }

    //---------------------------------------------------------------------------
    static half infinity()
    {
      half h;
      h.bits = HALF_FLOAT_MAX_BIASED_EXP;
      return h;
    }
    //---------------------------------------------------------------------------
    static half NaN()
    {
      half h;
      h.bits = HALF_FLOAT_MAX_BIASED_EXP | (( 1 << 10) - 1);
      return h;
    }
    //---------------------------------------------------------------------------
    static void convertDoubleToHalf(const double* d, half* h, int count)
    {
      for(int i=0; i<count; ++i)
        h[i] = convertFloatToHalf((float)d[i]);
    }
    //---------------------------------------------------------------------------
    static void convertHalfToDouble(const half* h, double* d, int count)
    {
      for(int i=0; i<count; ++i)
        d[i] = (double)convertHalfToFloat(h[i]);
    }
    //---------------------------------------------------------------------------
    static half convertFloatToHalf(float f)
    {
      union { float f; unsigned int x; } val;
      val.f = f;
      unsigned int sign = (unsigned short)(val.x>>31);
      unsigned int mantissa = val.x & ((1 << 23)-1);
      unsigned int exp = val.x & FLOAT_MAX_BIASED_EXP;
      typedef unsigned short hfloat;
      half hf;

      if (exp >= HALF_FLOAT_MAX_BIASED_EXP_AS_SINGLE_FP_EXP)
      {
        // check if the original single precision float number is a NaN
        if (mantissa && (exp == FLOAT_MAX_BIASED_EXP))
        {
          // we have a single precision NaN
          mantissa = (1<<23) - 1;
        }
        else
        {
          // 16-bit half-float representation stores number as Inf
          mantissa = 0;
        }
        hf.bits = (((hfloat)sign) << 15) | (hfloat)(HALF_FLOAT_MAX_BIASED_EXP) | (hfloat)(mantissa >> 13);
      }
      // check if exponent is <= -15
      else
      if (exp <= HALF_FLOAT_MIN_BIASED_EXP_AS_SINGLE_FP_EXP)
      {
        // store a denorm half-float value or zero
        exp = (HALF_FLOAT_MIN_BIASED_EXP_AS_SINGLE_FP_EXP - exp) >> 23;
        mantissa >>= (14 + exp);
        hf.bits = (((hfloat)sign) << 15) | (hfloat)(mantissa);
      }
      else
      {
        hf.bits = (((hfloat)sign)<<15) | 
                  (hfloat)((exp - HALF_FLOAT_MIN_BIASED_EXP_AS_SINGLE_FP_EXP) >> 13) | 
                  (hfloat)(mantissa >> 13);
      }
      return hf;
    }
    //---------------------------------------------------------------------------
    static float convertHalfToFloat(const half& h)
    {
      unsigned short hf = h.bits;
      unsigned int sign = (unsigned int) ( hf >> 15);
      unsigned int mantissa = (unsigned int) (hf & (( 1 << 10) - 1) );
      unsigned int exp = (unsigned int) (hf & HALF_FLOAT_MAX_BIASED_EXP);

      if (exp == HALF_FLOAT_MAX_BIASED_EXP)
      {
        // we have a half-float NaN or Inf
        // half-float NaNs will be converted to a single precision NaN
        // half-float Infs will be converted to a single precision Inf
        exp = FLOAT_MAX_BIASED_EXP;
        if ( mantissa)
          mantissa = (1 << 23 ) - 1;   // set all bits to indicate a NaN
      }
      else if (exp == 0x0)
      {
        // convert half-float zero/denorm to single precision value
        if ( mantissa)
        {
          mantissa <<= 1;
          exp = HALF_FLOAT_MIN_BIASED_EXP_AS_SINGLE_FP_EXP;
          // check for leading 1 in denorm mantissa
          while ((mantissa & (1 << 10) ) == 0)
          {
            // for every leading 0, decrement single precision exponent by 1
            // and shift half-float mantissa value to the left
            mantissa <<= 1;
            exp -= (1 << 23 );
          }
          // clamp the mantissa to 10-bits
          mantissa &= (( 1 << 10) - 1);
          // shift left to generate single-precision mantissa of 23-bits
          mantissa <<= 13;
        }
      }
      else
      {
        // shift left to generate single-precision mantissa of 23-bits
        mantissa <<= 13;
        // generate single precision biased exponent value
        exp = ( exp << 13) + HALF_FLOAT_MIN_BIASED_EXP_AS_SINGLE_FP_EXP;
      }
      union { float f; unsigned int x; } val;
      val.x = ( sign << 31) | exp | mantissa;
      return val.f;
    }
    //---------------------------------------------------------------------------
    static void convertFloatToHalf(const float* f, half* h, int count)
    {
      for(int i=0; i<count; ++i)
      {
        union { float f; unsigned int x; } val;
        val.f = f[i];
        unsigned int sign = (unsigned short)(val.x>>31);
        unsigned int mantissa = val.x & ((1 << 23)-1);
        unsigned int exp = val.x & FLOAT_MAX_BIASED_EXP;
        typedef unsigned short hfloat;
        hfloat& hf = h[i].bits;

        if (exp >= HALF_FLOAT_MAX_BIASED_EXP_AS_SINGLE_FP_EXP)
        {
          // check if the original single precision float number is a NaN
          if (mantissa && (exp == FLOAT_MAX_BIASED_EXP))
          {
            // we have a single precision NaN
            mantissa = (1<<23) - 1;
          }
          else
          {
            // 16-bit half-float representation stores number as Inf
            mantissa = 0;
          }
          hf = (((hfloat)sign) << 15) | (hfloat)(HALF_FLOAT_MAX_BIASED_EXP) | (hfloat)(mantissa >> 13);
        }
        // check if exponent is <= -15
        else
        if (exp <= HALF_FLOAT_MIN_BIASED_EXP_AS_SINGLE_FP_EXP)
        {
          // store a denorm half-float value or zero
          exp = (HALF_FLOAT_MIN_BIASED_EXP_AS_SINGLE_FP_EXP - exp) >> 23;
          mantissa >>= (14 + exp);
          hf = (((hfloat)sign) << 15) | (hfloat)(mantissa);
        }
        else
        {
          hf = (((hfloat)sign)<<15) | 
               (hfloat)((exp - HALF_FLOAT_MIN_BIASED_EXP_AS_SINGLE_FP_EXP) >> 13) | 
               (hfloat)(mantissa >> 13);
        }
      }
    }
    //---------------------------------------------------------------------------
    static void convertHalfToFloat(const half* h, float *f, int count)
    {
      for(int i=0; i<count; ++i)
      {
        const unsigned short& hf = h[i].bits;
        unsigned int sign = (unsigned int) ( hf >> 15);
        unsigned int mantissa = (unsigned int) (hf & (( 1 << 10) - 1) );
        unsigned int exp = (unsigned int) (hf & HALF_FLOAT_MAX_BIASED_EXP);

        if (exp == HALF_FLOAT_MAX_BIASED_EXP)
        {
          // we have a half-float NaN or Inf
          // half-float NaNs will be converted to a single precision NaN
          // half-float Infs will be converted to a single precision Inf
          exp = FLOAT_MAX_BIASED_EXP;
          if ( mantissa)
            mantissa = (1 << 23 ) - 1;   // set all bits to indicate a NaN
        }
        else if (exp == 0x0)
        {
          // convert half-float zero/denorm to single precision value
          if ( mantissa)
          {
            mantissa <<= 1;
            exp = HALF_FLOAT_MIN_BIASED_EXP_AS_SINGLE_FP_EXP;
            // check for leading 1 in denorm mantissa
            while ((mantissa & (1 << 10) ) == 0)
            {
              // for every leading 0, decrement single precision exponent by 1
              // and shift half-float mantissa value to the left
              mantissa <<= 1;
              exp -= (1 << 23 );
            }
            // clamp the mantissa to 10-bits
            mantissa &= (( 1 << 10) - 1);
            // shift left to generate single-precision mantissa of 23-bits
            mantissa <<= 13;
          }
        }
        else
        {
          // shift left to generate single-precision mantissa of 23-bits
          mantissa <<= 13;
          // generate single precision biased exponent value
          exp = ( exp << 13) + HALF_FLOAT_MIN_BIASED_EXP_AS_SINGLE_FP_EXP;
        }
        union { float f; unsigned int x; } val;
        val.x = ( sign << 31) | exp | mantissa;
        f[i] = val.f;
      }
    }
    //---------------------------------------------------------------------------
  public:
    unsigned short bits;

  private:
    // -15 stored using a single precision bias of 127
    static const unsigned int  HALF_FLOAT_MIN_BIASED_EXP_AS_SINGLE_FP_EXP = 0x38000000;
    
    // max exponent value in single precision that will be converted 
    // to Inf or Nan when stored as a half-float
    static const unsigned int  HALF_FLOAT_MAX_BIASED_EXP_AS_SINGLE_FP_EXP = 0x47800000;
    
    // 255 is the max exponent biased value
    static const unsigned int  FLOAT_MAX_BIASED_EXP = (0xFF << 23);
    static const unsigned int  HALF_FLOAT_MAX_BIASED_EXP = (0x1F << 10);

  };
  //-----------------------------------------------------------------------------
  inline float operator/(float a, const half& b)
  {
    return (float)a / half::convertHalfToFloat(b);
  }
  //-----------------------------------------------------------------------------
  inline float operator/(double a, const half& b)
  {
    return (float)a / half::convertHalfToFloat(b);
  }
  //-----------------------------------------------------------------------------
  inline float operator/(int a, const half& b)
  {
    return (float)a / half::convertHalfToFloat(b);
  }
  //-----------------------------------------------------------------------------
  inline float operator*(float a, const half& b)
  {
    return (float)a * half::convertHalfToFloat(b);
  }
  //-----------------------------------------------------------------------------
  inline float operator*(double a, const half& b)
  {
    return (float)a * half::convertHalfToFloat(b);
  }
  //-----------------------------------------------------------------------------
  inline float operator*(int a, const half& b)
  {
    return (float)a * half::convertHalfToFloat(b);
  }
  //-----------------------------------------------------------------------------
  inline float operator+(float a, const half& b)
  {
    return (float)a + half::convertHalfToFloat(b);
  }
  //-----------------------------------------------------------------------------
  inline float operator+(double a, const half& b)
  {
    return (float)a + half::convertHalfToFloat(b);
  }
  //-----------------------------------------------------------------------------
  inline float operator+(int a, const half& b)
  {
    return (float)a + half::convertHalfToFloat(b);
  }
  //-----------------------------------------------------------------------------
  inline float operator-(float a, const half& b)
  {
    return (float)a - half::convertHalfToFloat(b);
  }
  //-----------------------------------------------------------------------------
  inline float operator-(double a, const half& b)
  {
    return (float)a - half::convertHalfToFloat(b);
  }
  //-----------------------------------------------------------------------------
  inline float operator-(int a, const half& b)
  {
    return (float)a - half::convertHalfToFloat(b);
  }
  //-----------------------------------------------------------------------------
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //-----------------------------------------------------------------------------
  inline float operator/(const half& a, float b)
  {
    return half::convertHalfToFloat(a) / (float)b ;
  }
  //-----------------------------------------------------------------------------
  inline float operator/(const half& a, double b)
  {
    return half::convertHalfToFloat(a) / (float)b ;
  }
  //-----------------------------------------------------------------------------
  inline float operator/(const half& a, int b)
  {
    return half::convertHalfToFloat(a) / (float)b ;
  }
  //-----------------------------------------------------------------------------
  inline float operator*(const half& a, float b)
  {
    return half::convertHalfToFloat(a) * (float)b ;
  }
  //-----------------------------------------------------------------------------
  inline float operator*(const half& a, double b)
  {
    return half::convertHalfToFloat(a) * (float)b ;
  }
  //-----------------------------------------------------------------------------
  inline float operator*(const half& a, int b)
  {
    return half::convertHalfToFloat(a) * (float)b ;
  }
  //-----------------------------------------------------------------------------
  inline float operator+(const half& a, float b)
  {
    return half::convertHalfToFloat(a) + (float)b ;
  }
  //-----------------------------------------------------------------------------
  inline float operator+(const half& a, double b)
  {
    return half::convertHalfToFloat(a) + (float)b ;
  }
  //-----------------------------------------------------------------------------
  inline float operator+(const half& a, int b)
  {
    return half::convertHalfToFloat(a) + (float)b ;
  }
  //-----------------------------------------------------------------------------
  inline float operator-(const half& a, float b)
  {
    return half::convertHalfToFloat(a) - (float)b ;
  }
  //-----------------------------------------------------------------------------
  inline float operator-(const half& a, double b)
  {
    return half::convertHalfToFloat(a) - (float)b ;
  }
  //-----------------------------------------------------------------------------
  inline float operator-(const half& a, int b)
  {
    return half::convertHalfToFloat(a) - (float)b ;
  }
  //-----------------------------------------------------------------------------
  template<> inline half Vector4<half>::length() const { return (half)::sqrt( (float)x()*(float)x()+(float)y()*(float)y()+(float)z()*(float)z()+(float)w()*(float)w()); }
  template<> inline half Vector4<half>::lengthSquared() const { return (half)((float)x()*(float)x()+(float)y()*(float)y()+(float)z()*(float)z()+(float)w()*(float)w()); }

  template<> inline half Vector3<half>::length() const { return (half)::sqrt( (float)x()*(float)x()+(float)y()*(float)y()+(float)z()*(float)z()); }
  template<> inline half Vector3<half>::lengthSquared() const { return (half)((float)x()*(float)x()+(float)y()*(float)y()+(float)z()*(float)z()); }

  template<> inline half Vector2<half>::length() const { return (half)::sqrt( (float)x()*(float)x()+(float)y()*(float)y()); }
  template<> inline half Vector2<half>::lengthSquared() const { return (half)((float)x()*(float)x()+(float)y()*(float)y()); }
  //-----------------------------------------------------------------------------
  typedef Vector4<half> hvec4;
  typedef Vector3<half> hvec3;
  typedef Vector2<half> hvec2;
  //-----------------------------------------------------------------------------
  typedef Matrix4<half> hmat4;
  typedef Matrix3<half> hmat3;
  typedef Matrix2<half> hmat2;
  //-----------------------------------------------------------------------------
}

#endif
