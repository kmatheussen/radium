//-----------------------------------------------------------------------------
// MurmurHash3 was written by Austin Appleby, and is placed in the public
// domain. The author hereby disclaims copyright to this source code.

// Adaptations for Visualization Library by Michele Bosi

// Homepage: http://code.google.com/p/smhasher/w/list

#ifndef _MURMURHASH3_H_
#define _MURMURHASH3_H_

#include <vlCore/std_types.hpp>

namespace vl
{
  VLCORE_EXPORT void MurmurHash3_x86_32  ( const void * key, int len, u32 seed, void * out );

  VLCORE_EXPORT void MurmurHash3_x86_128 ( const void * key, int len, u32 seed, void * out );

  VLCORE_EXPORT void MurmurHash3_x64_128 ( const void * key, int len, u32 seed, void * out );
}

#endif // _MURMURHASH3_H_
