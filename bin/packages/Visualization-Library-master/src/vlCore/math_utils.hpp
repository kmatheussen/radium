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

#ifndef mathutils_INCLUDE_ONCE
#define mathutils_INCLUDE_ONCE

#include <vlCore/glsl_math.hpp>
#include <vlCore/Plane.hpp>
#include <cstdio>
#include <memory.h>
#include <vector>

namespace vl
{
  /** 
   * Returns a random number N between 'min' and 'max' (included) with 53 bits of randomness generated using MersenneTwister->rand53().
   */
  VLCORE_EXPORT real random(real min, real max);

  /** 
   * Returns a random number N between 'min' and 'max' (included) generated using MersenneTwister->randInt().
   */
  VLCORE_EXPORT u32 randomU32(u32 min, u32 max);

  /** 
   * Returns a random number N between 'min' and 'max' (included) generated using MersenneTwister->randInt().
   */
  VLCORE_EXPORT i32 randomI32(i32 min, i32 max);

  /** 
   * Returns a number N that is a power of 2 and that is equal to or greater than 'n'.
   */
  VLCORE_EXPORT int greaterEqualPow2(int n);

  /** 
   * Returns a number N that is a power of 2 and that is equal to or smaller than 'n'.
   */
  VLCORE_EXPORT int smallerEqualPow2(int n);

  /**
   * Extracts the 6 frustum planes for the given model-view-projection matrix.
   */
  VLCORE_EXPORT void extractPlanes( Plane* planes, const mat4& modelviewproj );
}

#endif
