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

#ifndef CRC32CheckSum_INCLUDE_ONCE
#define CRC32CheckSum_INCLUDE_ONCE

#include <vlCore/VirtualFile.hpp>

namespace vl
{
  /**
   * Computes the a CRC32 checksum of a given buffer or VirtualFile
  */
  class CRC32CheckSum
  {
    // Lower this if you need to limit the amount of data allocated to the stack, for example to 16K.
    static const int CHUNK_SIZE = 128*1024;

  public:
    //! Constructor.
    CRC32CheckSum() { crc32_init(); }

    unsigned int compute(const void* buf, int length) 
    {
      const unsigned char* buffer = (const unsigned char*)buf;
      unsigned int mCRC32 = 0xffffffff; 
      while(length--) 
        mCRC32 = (mCRC32 >> 8) ^ crc32_table[(mCRC32 & 0xFF) ^ *buffer++];
      return mCRC32 ^ 0xffffffff; 
    }

    unsigned int compute(VirtualFile* stream) 
    {
      std::vector<unsigned char> buffer;
      buffer.resize(CHUNK_SIZE);
      unsigned char *ptr = &buffer.front(); 
      startCRC32();

      for( int length = (int)stream->read(ptr, CHUNK_SIZE); length; length = (int)stream->read(ptr, CHUNK_SIZE) )
        continueCRC32(ptr,length);

      return finalizeCRC32();
    }

    void startCRC32() { mCRC32 = 0xffffffff; }
    unsigned int finalizeCRC32() { return mCRC32 ^ 0xffffffff; }
    void continueCRC32(unsigned char* ptr, int length)
    {
      while(length--) 
        mCRC32 = (mCRC32 >> 8) ^ crc32_table[(mCRC32 & 0xFF) ^ *ptr++];
    }

  protected:
    void crc32_init() 
    {
      mCRC32 = 0;
      unsigned int ulPolynomial = 0x04c11db7;
      for(int i = 0; i <= 0xFF; i++) 
      { 
        crc32_table[i]=reflect(i, 8) << 24; 
        for (int j = 0; j < 8; j++) 
          crc32_table[i] = (crc32_table[i] << 1) ^ (crc32_table[i] & (1 << 31) ? ulPolynomial : 0); 
        crc32_table[i] = reflect(crc32_table[i], 32); 
      } 
    }

    unsigned int reflect(unsigned int val, char ch) 
    {
      unsigned int refl = 0;
      for(int i = 1; i < (ch + 1); i++) 
      { 
        if(val & 1) 
          refl |= 1 << (ch - i); 
        val >>= 1; 
      } 
      return refl; 
    }

  protected:
    unsigned int mCRC32;
    unsigned int crc32_table[256];
  };
}

#endif
