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

#ifndef UID_INCLUDE_ONCE
#define UID_INCLUDE_ONCE

#include <vlCore/checks.hpp>
#include <string>
#include <string.h>

namespace vl
{
  class Random;

  //! Simple implementation of a 16 bytes Universally Unique ID based on http://www.ietf.org/rfc/rfc4122.txt
  class VLCORE_EXPORT UUID
  {
  public:
    //! Constructor, by default it is set to all zeros.
    UUID();

    //! Generates a Version 4 UUID as defined by RFC4122 using the specified random number generator.
    void generateVersion4(const Random& random);

    //! Init the UUID from the specified string which must be at least 38 characters long and must be of the form {xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx}.
    bool fromString(const char* guid_str);

    //! Fills a buffer with an ascii representation of the UUID of type {xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx}.
    //! \param guid_str must point to a buffer of at least 38 byes; if \p zero_terminate is \p true (default) then it must be at least 39 bytes large.
    //! \param zero_terminate if \p true the function inserts a trailing 0 at the end of the string, in this case \p guid_str must point to a buffer 39 bytes large at least.
    void fillString(char* guid_str, bool zero_terminate = true) const;

    //! Fills the given std::string with an ASCII representation of the UUID of type {xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx}.
    void toStdString(std::string& guid_str) const;

    //! Returns an std::string jwith an ASCII representation of the UUID of type {xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx}.
    std::string toStdString() const;

    bool operator==(const UUID& other) const
    {
      return memcmp(this, &other, sizeof(*this)) == 0;
    }

    bool operator!=(const UUID& other) const
    {
      return memcmp(this, &other, sizeof(*this)) != 0;
    }

    bool operator<(const UUID& other) const
    {
      return memcmp(this, &other, sizeof(*this)) < 0;
    }

  public:
    unsigned int   mTimeLow;
    unsigned short mTimeMid;
    unsigned short mTimeHiAndVersion;
    unsigned char  mClockSeqHiAndReserved;
    unsigned char  mClockSeqLow;
    unsigned char  mNode[6];
  };
// ----------------------------------------------------------------------------
  VL_COMPILE_TIME_CHECK(sizeof(UUID) == 16)
// ----------------------------------------------------------------------------
}

// ----------------------------------------------------------------------------

#endif
