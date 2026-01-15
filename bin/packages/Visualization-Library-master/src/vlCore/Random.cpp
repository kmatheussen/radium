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

#include <vlCore/Random.hpp>
#include <vlCore/Time.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/MersenneTwister.hpp>
#include <cstdlib>

#if defined(VL_PLATFORM_WINDOWS)
  #include <wincrypt.h>
#endif

using namespace vl;

//-----------------------------------------------------------------------------
Random::Random()
{
  VL_DEBUG_SET_OBJECT_NAME()
#if defined(_MSC_VER) || defined(__MINGW32__)
  hCryptProv = NULL;
  if( !CryptAcquireContext( (HCRYPTPROV*)&hCryptProv, NULL, NULL, PROV_RSA_FULL, 0) )
    hCryptProv = NULL;
#elif defined(__GNUG__) && !defined(__MINGW32__)
  mDefURandom = fopen("/dev/urandom", "rb");
#endif
}
//-----------------------------------------------------------------------------
Random::~Random()
{
#if defined(_MSC_VER) || defined(__MINGW32__)
  if( hCryptProv  )
  {
    CryptReleaseContext( (HCRYPTPROV)hCryptProv, 0 );
    hCryptProv = NULL;
  }
#elif defined(__GNUG__) && !defined(__MINGW32__)
  if (mDefURandom)
  {
    fclose(mDefURandom);
    mDefURandom = NULL;
  }
#endif
}
//-----------------------------------------------------------------------------
bool Random::fillRandom(void* ptr, size_t bytes) const
{
#if defined(_MSC_VER) || defined(__MINGW32__)
  if( !(hCryptProv && CryptGenRandom( (HCRYPTPROV)hCryptProv, bytes, (BYTE*)ptr)) )
  {
    fillRandomMersenneTwister(ptr, bytes);
    return false;
  }
  else
    return true;
#elif defined(__GNUG__) && !defined(__MINGW32__)
  if ( mDefURandom && fread(ptr, 1, bytes, mDefURandom) == bytes )
    return true;
  else
  {
    fillRandomMersenneTwister(ptr, bytes);
    return false;
  }
#else
  fillRandomMersenneTwister(ptr, bytes);
  return false;
#endif
}
//-----------------------------------------------------------------------------
void Random::fillRandomMersenneTwister(void* ptr, size_t bytes) const
{
  unsigned int rnd = 0;

  unsigned char* cptr = (unsigned char*)ptr;
  memset(cptr, 0, bytes);
  for (size_t i=0; i<bytes; ++i)
  {
    defMersenneTwister()->randInt( rnd );
    cptr[i] ^= (rnd>>0)  & 0xFF;
    cptr[i] ^= (rnd>>8)  & 0xFF;
    cptr[i] ^= (rnd>>16) & 0xFF;
    cptr[i] ^= (rnd>>12) & 0xFF;
  }
}
//-----------------------------------------------------------------------------
