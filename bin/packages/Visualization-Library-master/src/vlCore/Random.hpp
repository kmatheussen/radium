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

#ifndef Random_INCLUDE_ONCE
#define Random_INCLUDE_ONCE

#include <vlCore/Object.hpp>

namespace vl
{
  //! Cryptographic random number generator.
  //! For non-ryptographic fast and high quality random number generation use vl::MersenneTwister.
  class VLCORE_EXPORT Random: public Object
  {
    VL_INSTRUMENT_CLASS(vl::Random, Object)

  public:
    //! Constructor.
    Random();

    //! Destructor.
    virtual ~Random();

    //! Fills the specified buffer with random data generated using the best quality random number generation facilities available.
    //! Under Windows (including MinGW) \p CryptGenRandom is used, while under Unix-like operating systems \p /dev/urandom is used.
    //! If no special random number generation facility is detected the function falls back to use a MersenneTwister.
    //! \return This method returns \p false if the function had to fallback to MersenneTwister otherwise returns \p true.
    virtual bool fillRandom(void* ptr, size_t bytes) const;

    //! Fills the specified buffer with random data generated using a defMersienneTwister(). 
    //! MersienneTwister produces high quality random number and can be much faster than fillRandom().
    void fillRandomMersenneTwister(void* ptr, size_t bytes) const;

  private:
#if defined(_MSC_VER) || defined(__MINGW32__)
    void* hCryptProv;
#elif defined(__GNUG__) && !defined(__MINGW32__)
  FILE* mDefURandom;
#endif
  };
}

#endif
