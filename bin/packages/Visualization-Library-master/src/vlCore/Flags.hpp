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

#ifndef Flags_INCLUDE_ONCE
#define Flags_INCLUDE_ONCE

namespace vl
{
  //! Simple class to manage flags in a type safe manner.
  template <typename T_FlagEnum>
  class Flags
  {
    // friends:
    template <typename T> friend Flags<T> operator|(T flag1, T flag2);
    template <typename T> friend Flags<T> operator&(T flag1, T flag2);

  public:
    typedef T_FlagEnum flag_type;
    typedef unsigned int flag_storage;

  public:
    Flags(): mFlags(0)  { }

    flag_storage flags() const { return mFlags; }

    void reset() { mFlags = 0; }

    // --- using Flags ---

    Flags operator|(const Flags& flag) const
    {
      Flags other = *this;
      other.mFlags |= flag.mFlags;
      return other;
    }

    Flags operator&(const Flags& flag) const
    {
      Flags other = *this;
      other.mFlags &= flag.mFlags;
      return other;
    }

    Flags operator^(const Flags& flag) const
    {
      Flags other = *this;
      other.mFlags ^= flag.mFlags;
      return other;
    }

    Flags operator-(const Flags& flag) const
    {
      Flags other = *this;
      other.mFlags &= ~flag.mFlags;
      return other;
    }

    // --- using T_FlagEnum ---

    Flags& set(T_FlagEnum flag)
    {
      mFlags |= (flag_storage)flag;
      return *this;
    }

    Flags& unset(T_FlagEnum flag)
    {
      mFlags &= ~(flag_storage)flag;
      return *this;
    }

    Flags& operator=(T_FlagEnum flag)
    {
      mFlags = flag;
      return this;
    }

    Flags operator|(T_FlagEnum flag) const
    {
      Flags other = *this;
      other.mFlags |= (flag_storage)flag;
      return other;
    }

    Flags operator&(T_FlagEnum flag) const
    {
      Flags other = *this;
      other.mFlags &= (flag_storage)flag;
      return other;
    }

    Flags operator^(T_FlagEnum flag) const
    {
      Flags other = *this;
      other.mFlags ^= (flag_storage)flag;
      return other;
    }

    Flags operator-(T_FlagEnum flag) const
    {
      Flags other = *this;
      other.unset(flag);
      return other;
    }

    operator bool() const 
    {
      return mFlags != 0;
    }

  private:
    flag_storage mFlags;
  };

  template <typename T>
  Flags<T> operator|(T flag1, T flag2)
  {
    Flags<T> flags;
    flags.mFlags = (typename Flags<T>::flag_storage)flag1 | (typename Flags<T>::flag_storage)flag2;
    return flags;
  }

  template <typename T>
  Flags<T> operator&(T flag1, T flag2)
  {
    Flags<T> flags;
    flags.mFlags = (typename Flags<T>::flag_storage)flag1 & (typename Flags<T>::flag_storage)flag2;
    return flags;
  }
}

#define VL_DECLARE_FLAGS(EnumType, FlagTypeName)                                \
  template<EnumType> Flags<EnumType> operator|(EnumType flag1, EnumType flag2); \
  template<EnumType> Flags<EnumType> operator&(EnumType flag1, EnumType flag2); \
  typedef Flags<EnumType> FlagTypeName;

/*** 

usage:
 
enum MyFlagEnum
{
  Flag1 = 0x1,
  Flag2 = 0x2,
  Flag3 = 0x4,
};
 
VL_DECLARE_FLAGS(MyFlagEnum, MyFlags)

...

MyFlags f = Flag3 | Flag1;

***/

#endif
