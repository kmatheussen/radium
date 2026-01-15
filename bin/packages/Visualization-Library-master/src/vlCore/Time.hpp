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

#ifndef Time_INCLUDE_ONCE
#define Time_INCLUDE_ONCE

#include <vlCore/Object.hpp>

namespace vl
{
  //-----------------------------------------------------------------------------
  // Time
  // Under Windows, due to BIOS or HAL bugs this function could return different
  // results on different threads, which means that if more than one thread needs
  // to sinchronize with the others using this function they should use the time
  // returned in one of those threads as reference.
  //-----------------------------------------------------------------------------
  /**
   * Simple class to be used as a timer and to retrieve the current time and date.
  */
  class VLCORE_EXPORT Time: public Object
  {
    VL_INSTRUMENT_CLASS(vl::Time, Object)

  public:
    Time();

    int year() const { return mYear; }
    
    int month() const { return mMonth; }
    
    int dayOfWeek() const { return mDayOfWeek; }
    
    int dayOfMonth() const { return mDayOfMonth; }
    
    int hour() const { return mHour; }
    
    int minute() const { return mMinute; }
    
    int second() const { return mSecond; }
    
    int microsecond() const { return mMicrosecond; }

    static real currentTime();
    
    static void sleep(unsigned int milliseconds);

    void start(int index=0) { mStart[index] = currentTime(); }
    
    void stop(int index=0)  { mStart[index] = -1.0; }
    
    bool isStarted(int index=0) const { return mStart[index] != -1; }
    
    real elapsed(int index=0) const { return mStart[index] >= 0 ? currentTime() - mStart[index] : -1; }

  protected:
    int mYear; // 1601 through 30827.
    int mMonth; // 1..12
    int mDayOfWeek; // 0 = Sunday, 6 = Saturday
    int mDayOfMonth; // 1..31
    int mHour; // 0..23
    int mMinute; // 0..59
    int mSecond; // 0..59
    int mMicrosecond; // 0 ... 999999

    real mStart[VL_MAX_TIMERS];
  };
}

#endif
