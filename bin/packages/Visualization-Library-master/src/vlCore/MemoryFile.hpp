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

#ifndef MemoryFile_INCLUDE_ONCE
#define MemoryFile_INCLUDE_ONCE

#include <vlCore/VirtualFile.hpp>
#include <vlCore/Buffer.hpp>

namespace vl
{
//---------------------------------------------------------------------------
// MemoryFile
//---------------------------------------------------------------------------
  /**
   * A VirtualFile to manipulate files stored in memory.
   *
   * \sa
   * - VirtualDirectory
   * - DiskDirectory
   * - MemoryDirectory
   * - ZippedDirectory
   * - FileSystem
   * - VirtualFile
   * - DiskFile
   * - ZippedFile
  */
  class VLCORE_EXPORT MemoryFile: public VirtualFile
  {
    VL_INSTRUMENT_CLASS(vl::MemoryFile, VirtualFile)

  public:
    MemoryFile();

    const Buffer* buffer() const { return mBuffer.get(); }

    Buffer* buffer() { return mBuffer.get(); }

    //! This is useful when you want to point more MemoryFiles to the same Buffer object.
    void setBuffer(Buffer* buffer) { mBuffer = buffer; }

    unsigned char* ptr() { return mBuffer->ptr(); }

    //! A MemoryFile always exists.
    virtual bool exists() const { return true; }

    virtual bool open(EOpenMode mode);

    virtual bool isOpen() const { return mIsOpen; }

    virtual void close() { mPtr = 0; mIsOpen = false; }

    void allocateBuffer(long long byte_count) { mBuffer->resize((int)byte_count); }

    virtual long long size() const { return mBuffer->bytesUsed(); }

    //! Copies the data of any kind of VirtualFile
    void copy(VirtualFile* file);

    MemoryFile& operator=(const MemoryFile& other) { close(); super::operator=(other); mBuffer = other.mBuffer; return *this; }

    ref<VirtualFile> clone() const;

  protected:
    virtual long long position_Implementation() const;

    virtual long long read_Implementation(void* buffer, long long byte_count);

    virtual long long write_Implementation(const void* /*buffer*/, long long /*byte_count*/) { return 0; } // not supported yet

    virtual bool seekSet_Implementation(long long offset);

  protected:
    ref<Buffer> mBuffer;
    long long mPtr;
    bool mIsOpen;
  };
}

#endif
