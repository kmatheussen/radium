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

#ifndef CHECK_INCLUDED
#define CHECK_INCLUDED

#include <vlCore/config.hpp>

// Required for IsDebuggerPresent()
#if defined(VL_PLATFORM_WINDOWS)
  #ifndef WIN32_LEAN_AND_MEAN
    #define WIN32_LEAN_AND_MEAN 1
  #endif
  #ifndef NOMINMAX
    #define NOMINMAX
  #endif
  #include <windows.h>
#endif

#if defined(__GNUG__) || defined(__MINGW32__) 
  #include <cstdio>
#endif

namespace vl
{
  VLCORE_EXPORT void log_failed_check(const char*, const char*, int);

  VLCORE_EXPORT void abort_vl();

  // Compile-time check
  #define VL_COMPILE_TIME_CHECK( expr ) typedef char compile_time_assert[ (expr) ? 1 : -1 ];

  #if defined(_DEBUG) || !defined(NDEBUG) || VL_FORCE_CHECKS == 1

    // Visual Studio
    #if defined(_MSC_VER) 
      #define VL_TRAP() { if (IsDebuggerPresent()) { __debugbreak(); } else ::vl::abort_vl(); }
    // GNU GCC
    #elif defined(__GNUG__) || defined(__MINGW32__) 
      #define VL_TRAP() { fflush(stdout); fflush(stderr); asm("int $0x3"); }
    #else 
      #define VL_TRAP() { ::vl::abort_vl(); }
    #endif

    #define VL_CHECK(expr) { if(!(expr)) { ::vl::log_failed_check(#expr,__FILE__,__LINE__); VL_TRAP() } }
    #define VL_WARN(expr)  { if(!(expr)) { ::vl::log_failed_check(#expr,__FILE__,__LINE__); } }

    // MSDN checked iterators
    // #define _SECURE_SCL 1
  #else
    #define VL_WARN(expr) {}
    #define VL_CHECK(expr) {}
    #define VL_TRAP() {}

    // MSDN checked iterators
    // #define _SECURE_SCL 0
  #endif
}

#endif
