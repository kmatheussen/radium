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

#include <vlCore/MD5CheckSum.hpp>
#include <vlCore/VirtualFile.hpp>
#include <cstdio>
#include <cstring>

#include "3rdparty/md5/md5.c"

using namespace vl;

std::string MD5CheckSum::toStdString() const
{
  char sum[33];
  sprintf ( sum, "%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x", 
    mMD5[0], mMD5[1], mMD5[2],  mMD5[3],  mMD5[4],  mMD5[5],  mMD5[6],  mMD5[7],
    mMD5[8], mMD5[9], mMD5[10], mMD5[11], mMD5[12], mMD5[13], mMD5[14], mMD5[15] );
  return sum;
}
//-----------------------------------------------------------------------------
void MD5CheckSum::compute(void* buffer, int len)
{
  MD5Context md5context;
  MD5Init(&md5context);
  MD5Update(&md5context, (unsigned char*)buffer, len);
  MD5Final(mMD5,&md5context);
}
//-----------------------------------------------------------------------------
void MD5CheckSum::compute(VirtualFile* file)
{
  const int CHUNK_SIZE = 128*1024;
  std::vector<unsigned char> buffer;
  buffer.resize(CHUNK_SIZE);
  MD5Context md5context;
  MD5Init(&md5context);

  for( unsigned bytes_read = (unsigned)file->read(&buffer.front(), CHUNK_SIZE); 
       bytes_read; 
       bytes_read = (unsigned)file->read(&buffer.front(), CHUNK_SIZE) )
  {
    MD5Update(&md5context, &buffer.front(), bytes_read);
  }

  MD5Final(mMD5,&md5context);
}
//-----------------------------------------------------------------------------
