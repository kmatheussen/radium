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

#ifndef MD5CheckSum_INCLUDE_ONCE
#define MD5CheckSum_INCLUDE_ONCE

#include <vlCore/config.hpp>
#include <string>
#include <string.h>

namespace vl
{
  class VirtualFile;

  /**
   * Computes the MD5 of a given buffer or VirtualFile.
  */
  class VLCORE_EXPORT MD5CheckSum
  {
  public:
    MD5CheckSum() { memset(mMD5, 0, 16); }
    
    const unsigned char* md5() const { return mMD5; }
    
    std::string toStdString() const;
    
    void compute(void* buffer, int len);
    
    void compute(VirtualFile* file);

    bool operator==(const MD5CheckSum& other) const { return memcmp(mMD5, other.mMD5, 16) == 0; }
    
    bool operator!=(const MD5CheckSum& other) const { return memcmp(mMD5, other.mMD5, 16) != 0; }
    
    bool operator< (const MD5CheckSum& other) const { return memcmp(mMD5, other.mMD5, 16) <  0; }

  protected:
    unsigned char mMD5[16];
  };
}

#endif
