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

#ifndef Say_INCLUDE_ONCE
#define Say_INCLUDE_ONCE

#include <vlCore/String.hpp>
#include <string>

namespace vl
{
  //-----------------------------------------------------------------------------
  // SayArg
  //-----------------------------------------------------------------------------
  //! Used internally by the Say class
  class VLCORE_EXPORT SayArg
  {
  friend class Say;
  public:
    SayArg();

    explicit SayArg(const unsigned char* d);

    explicit SayArg(const std::string& d);

    explicit SayArg(const char* d);

    explicit SayArg(void* d);

    explicit SayArg(const String& d);

    explicit SayArg(double d);

    explicit SayArg(float d);

    explicit SayArg(unsigned char d);

    explicit SayArg(signed char d);

    explicit SayArg(unsigned short d);

    explicit SayArg(signed short d);

    explicit SayArg(unsigned int d);

    explicit SayArg(signed int d);

    explicit SayArg(unsigned long d);

    explicit SayArg(signed long d);

    explicit SayArg(unsigned long long d);

    explicit SayArg(signed long long d);

  protected:
    void init();

    String str;
    double float64;
    unsigned long long ulonglong;
    signed   long long slonglong;

    enum
    {
      NO_TYPE,
      STRING,
      FLOAT64,
      ULONGLONG,
      SLONGLONG
    } type;

  };
  //-----------------------------------------------------------------------------
  // Say
  //-----------------------------------------------------------------------------
  //! A simple String formatting class
  //! \remarks
  //! Things that we cannot do:
  //! - Exponential rounding
  //! - Explicit parameter reference
  //! - Dynamic field and decimal length
  //!
  //! Things we can do:
  //! - Handles: String, char*, signed/unsigned char/short/int/long long, float, double
  //! - Shows both integer and fractional numbers in the following bases 2, 8, 10, 16 (actually any base between 2 and 16 but the parser knows only these 4)
  //! - Field size
  //! - Field alignment
  //! - "+" Sign
  //! - Decimal field size with rounding (only for normal notation for now)
  //! - Uppercase and lowercase output
  //! - "Euro" notation ouput for numbers
  //! - Normal and exponential notation
  //! - Clear and helpful debugging messages
  //! - Typesafe, portable, easy to use, clean
  class VLCORE_EXPORT Say: public std::vector<SayArg>
  {
  public:
    String format_string;

    Say(const String& fstr)
    {
      format_string = fstr;
    }

    Say& operator<<(const SayArg& p)
    {
      push_back(p);
      return *this;
    }

    template <typename T>
    Say& operator<<(T p)
    {
      push_back(SayArg(p));
      return *this;
    }

    operator String()
    {
      return parse(*this);
    }

    String str() const
    {
      return parse(*this);
    }

  protected:
    String parse( const Say& pset ) const;

    String euronotation(const String& str, int base) const;
      
    String format(unsigned long long n, int base, int field, int decimals, int align, int fill, int plus, int finalizer, int eur) const;

    String format(signed long long nn, int base, int field, int decimals, int align, int fill, int plus, int finalizer, int eur) const;
      
    String format(double num, int base, int field, int decimals, int align, int fill, int plus, int finalizer, int eur) const;

    String pipeline(const String& str, int base, int field, int decimals, int finalizer, int align, int eur, int fill, int negative, int plus) const;
  };
}

#endif
