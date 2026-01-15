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

#include <vlCore/UUID.hpp>
#include <vlCore/Time.hpp>
#include <vlCore/Random.hpp>
#include <vector>

using namespace vl;

//-----------------------------------------------------------------------------
UUID::UUID()
{
  memset(this, 0, sizeof(*this));
}
//-----------------------------------------------------------------------------
void UUID::generateVersion4(const Random& random)
{
  random.fillRandom(this,sizeof(*this));

  // Set the two most significant bits (bits 6 and 7) of the ClockSeqHiAndReserved to zero and one, respectively.
  mClockSeqHiAndReserved &= 0x3F;
  mClockSeqHiAndReserved |= 0x80;

  // Set the four most significant bits (bits 12 through 15) of the TimeHiAndVersion field to the 4-bit version number from Section 4.1.3.
  mTimeHiAndVersion &= 0x0FFF;
  mTimeHiAndVersion |= 0x4000;
}
//-----------------------------------------------------------------------------
void UUID::toStdString(std::string& guid_str) const
{
  char str[38];
  fillString(str, false);
  // avoid reallocation
  guid_str.resize(38);
  for(int i=0; i<38; ++i)
    guid_str[i] = str[i];
}
//-----------------------------------------------------------------------------
void UUID::fillString(char* guid_str, bool zero_terminate) const
{
  const char* hex= "0123456789abcdef";

  // 01234567890123456789012345678901234567
  // {xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx}

  if (zero_terminate)
    guid_str[38] = 0;

  guid_str[0]  = '{';
  guid_str[9]  = '-';
  guid_str[14] = '-';
  guid_str[19] = '-';
  guid_str[24] = '-';
  guid_str[37] = '}';

  // TimeLow:
  for(int i=0; i<8; ++i)
    guid_str[i+1] = hex[ (mTimeLow >> (28-4*i)) & 0xF];

  // TimeMid:
  for(int i=0; i<4; ++i)
    guid_str[i+10] = hex[ (mTimeMid >> (12-4*i)) & 0xF];

  // TimeHiAndVersion:
  for(int i=0; i<4; ++i)
    guid_str[i+15] = hex[ (mTimeHiAndVersion >> (12-4*i)) & 0xF];

  // ClockSeqHiAndReserved:
  guid_str[20] = hex[ (mClockSeqHiAndReserved >> (4-4*0)) & 0xF];
  guid_str[21] = hex[ (mClockSeqHiAndReserved >> (4-4*1)) & 0xF];

  // ClockSeqLow:
  guid_str[22] = hex[ (mClockSeqLow >> (4-4*0)) & 0xF];
  guid_str[23] = hex[ (mClockSeqLow >> (4-4*1)) & 0xF];  

  // Node:
  guid_str[25] = hex[ (mNode[0] >> (4-4*0)) & 0xF];
  guid_str[26] = hex[ (mNode[0] >> (4-4*1)) & 0xF];
  guid_str[27] = hex[ (mNode[1] >> (4-4*0)) & 0xF];
  guid_str[28] = hex[ (mNode[1] >> (4-4*1)) & 0xF];
  guid_str[29] = hex[ (mNode[2] >> (4-4*0)) & 0xF];
  guid_str[30] = hex[ (mNode[2] >> (4-4*1)) & 0xF];
  guid_str[31] = hex[ (mNode[3] >> (4-4*0)) & 0xF];
  guid_str[32] = hex[ (mNode[3] >> (4-4*1)) & 0xF];
  guid_str[33] = hex[ (mNode[4] >> (4-4*0)) & 0xF];
  guid_str[34] = hex[ (mNode[4] >> (4-4*1)) & 0xF];
  guid_str[35] = hex[ (mNode[5] >> (4-4*0)) & 0xF];
  guid_str[36] = hex[ (mNode[5] >> (4-4*1)) & 0xF];

  // equivalent to the following formatting:
#if 0
  char guid_cstr[100];
  sprintf(guid_cstr, "{%8.8x-%4.4x-%4.4x-%2.2x%2.2x-%2.2x%2.2x%2.2x%2.2x%2.2x%2.2x}",
    mTimeLow, mTimeMid, mTimeHiAndVersion, 
    mClockSeqHiAndReserved, mClockSeqLow,
    mNode[0], mNode[1], mNode[2], mNode[3], mNode[4], mNode[5]);
  VL_CHECK( memcmp(guid_str, guid_cstr, 38) == 0 )
#endif
}
//-----------------------------------------------------------------------------
std::string UUID::toStdString() const
{
  std::string guid_str;
  toStdString(guid_str);
  return guid_str;
}
//-----------------------------------------------------------------------------
bool UUID::fromString(const char* guid_str)
{
  char xxx[38];
  memcpy(xxx, guid_str, 38);

  if (guid_str[0] != '{')
    return false;
  
  if (guid_str[37] != '}')
    return false;
  
  if (guid_str[9] != '-')
    return false;
  
  if (guid_str[14] != '-')
    return false;
  
  if (guid_str[19] != '-')
    return false;
  
  if (guid_str[24] != '-')
    return false;

  // to lower case, check hex numbers, convert to nybbles
  unsigned int nibble[38];
  for(int i=0; i<38; ++i)
  {
    if (i==0 || i==37 || i==9 || i==14 || i==19 || i==24 )
      continue;

    if ( guid_str[i]>='0' && guid_str[i]<='9' )
      nibble[i] = guid_str[i] - '0';
    else
    if ( guid_str[i]>='a' && guid_str[i]<='f' )
      nibble[i] = guid_str[i] - 'a' + 10;
    else
    if ( guid_str[i]>='A' && guid_str[i]<='F' )
      nibble[i] = guid_str[i] - 'A' + 10;
    else
      return false;
  }

  // 01234567890123456789012345678901234567
  // {xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx}

  memset(this, 0, sizeof(*this));

  mTimeLow = (nibble[1] << 28) | (nibble[2] << 24) | (nibble[3] << 20) | (nibble[4] << 16) | 
             (nibble[5] << 12) | (nibble[6] <<  8) | (nibble[7] <<  4) | nibble[8];

  mTimeMid = (unsigned short)((nibble[10] << 12) | (nibble[11] <<  8) | (nibble[12] <<  4) | nibble[13]);

  mTimeHiAndVersion = (unsigned short)((nibble[15] << 12) | (nibble[16] <<  8) | (nibble[17] <<  4) | nibble[18]);

  mClockSeqHiAndReserved  = (unsigned char)((nibble[20] << 4) | nibble[21]);

  mClockSeqLow = (unsigned char)((nibble[22] << 4) | (nibble[23] << 0));

  mNode[0] = (unsigned char)((nibble[25] << 4) | nibble[26]);
  mNode[1] = (unsigned char)((nibble[27] << 4) | nibble[28]);
  mNode[2] = (unsigned char)((nibble[29] << 4) | nibble[30]);
  mNode[3] = (unsigned char)((nibble[31] << 4) | nibble[32]);
  mNode[4] = (unsigned char)((nibble[33] << 4) | nibble[34]);
  mNode[5] = (unsigned char)((nibble[35] << 4) | nibble[36]);

  return true;
}
//-----------------------------------------------------------------------------

