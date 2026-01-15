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

#include <vlCore/Time.hpp>
#include <ctime>
#include <cmath>

#if defined(VL_PLATFORM_LINUX) || defined(VL_PLATFORM_MACOSX)
  #include <sys/time.h> // gettimeofday()
  #include <unistd.h>   // usleep()
#endif

using namespace vl;

//-----------------------------------------------------------------------------
// Time
//-----------------------------------------------------------------------------
Time::Time()
{
  VL_DEBUG_SET_OBJECT_NAME()

  for(int i=0; i<VL_MAX_TIMERS; ++i)
    mStart[i] = -1;

  #if defined(VL_PLATFORM_WINDOWS)
    SYSTEMTIME local_time;
    GetLocalTime(&local_time);
    mYear = local_time.wYear;
    mMonth = local_time.wMonth;
    mDayOfWeek = local_time.wDayOfWeek;
    mDayOfMonth = local_time.wDay;
    mHour = local_time.wHour;
    mMinute = local_time.wMinute;
    mSecond = local_time.wSecond;
    mMicrosecond = local_time.wMilliseconds * 1000;
  #elif defined(__GNUG__)
    time_t secs;
    ::time(&secs);
    tm* date = localtime( &secs );

    mYear = date->tm_year + 1900;
    mMonth = date->tm_mon;
    mDayOfWeek = date->tm_wday;
    mDayOfMonth = date->tm_mday;
    mHour = date->tm_hour;
    mMinute = date->tm_min;
    mSecond = date->tm_sec;

    struct timeval tv;
    gettimeofday( &tv, NULL );
    mMicrosecond = tv.tv_usec;
  #endif
}
//-----------------------------------------------------------------------------
namespace vl
{
  unsigned long long gStartTime = 0;

  void initStartTime()
  {
    #if defined(VL_PLATFORM_WINDOWS)
      LARGE_INTEGER Frequency;
      LARGE_INTEGER PerformanceCount;
      BOOL has_timer = QueryPerformanceFrequency( &Frequency );
      if (has_timer)
      {
        // DWORD_PTR oldmask = ::SetThreadAffinityMask(::GetCurrentThread(), 1);
        QueryPerformanceCounter( &PerformanceCount );
        // ::SetThreadAffinityMask(::GetCurrentThread(), oldmask);
        gStartTime = PerformanceCount.QuadPart;
      }
      else
      {
        gStartTime = GetTickCount();
      }
    #elif defined(__GNUG__)
      struct timeval tv;
      gettimeofday( &tv, NULL );
      gStartTime = (unsigned long long)tv.tv_sec * 1000000 + (unsigned long long)tv.tv_usec;
    #endif
  }
}
//-----------------------------------------------------------------------------
//! Seconds passed from an arbitrary origin
//! QueryPerformanceFrequency should be called only once in the application lifetime
real Time::currentTime()
{
  if (gStartTime == 0)
    initStartTime();

  VL_CHECK(gStartTime);

  #if defined(VL_PLATFORM_WINDOWS)
    // Win32
    LARGE_INTEGER Frequency;
    LARGE_INTEGER PerformanceCount;
    BOOL has_timer = QueryPerformanceFrequency( &Frequency );
    if (has_timer)
    {
      // DWORD_PTR oldmask = ::SetThreadAffinityMask(::GetCurrentThread(), 1);
      QueryPerformanceCounter( &PerformanceCount );
      // ::SetThreadAffinityMask(::GetCurrentThread(), oldmask);
      return (real)(PerformanceCount.QuadPart-gStartTime)/Frequency.QuadPart;
    }
    else
    {
      return (GetTickCount()-gStartTime) / 1000.0f;
    }
  #elif defined(__GNUG__)
    struct timeval tv;
    gettimeofday( &tv, NULL );
    return ((unsigned long long)tv.tv_sec * 1000000 + (unsigned long long)tv.tv_usec - gStartTime) * 0.000001f;
  #endif
}
//-----------------------------------------------------------------------------
void Time::sleep(unsigned int milliseconds)
{
  #if defined(VL_PLATFORM_WINDOWS)
    Sleep(milliseconds);
  #elif defined(__GNUG__)
    usleep(milliseconds*1000);
  #endif
}
//-----------------------------------------------------------------------------
