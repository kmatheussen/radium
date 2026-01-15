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

#include <vlCore/MemoryFile.hpp>
#include <vlCore/Log.hpp>

using namespace vl;

//---------------------------------------------------------------------------
// MemoryFile
//---------------------------------------------------------------------------
MemoryFile::MemoryFile()
{
  mBuffer = new Buffer;
  mPtr = 0;
  mIsOpen = false;
}
//---------------------------------------------------------------------------
bool MemoryFile::open(EOpenMode mode)
{
  if ( isOpen() )
  {
    Log::error("MemoryFile::open(): file already open.\n");
    return false;
  }

  // for now support only OM_ReadOnly mode
  mIsOpen = (mode == OM_ReadOnly);
  return mIsOpen;
}
//---------------------------------------------------------------------------
void MemoryFile::copy(VirtualFile* file)
{
  if (file->isOpen())
    file->close();

  allocateBuffer(file->size());

  file->open(OM_ReadOnly);

  file->read( ptr(), size() );

  file->close();
}
//---------------------------------------------------------------------------
long long MemoryFile::position_Implementation() const
{
  if (!isOpen())
    return -1;
  return mPtr;
}
//---------------------------------------------------------------------------
long long MemoryFile::read_Implementation(void* buffer, long long byte_count)
{
  if (!isOpen())
    return 0;
  if ( mBuffer->bytesUsed() )
  {
    long long bytes_left = mBuffer->bytesUsed() - mPtr;
    byte_count = byte_count < bytes_left ? byte_count : bytes_left;
    memcpy(buffer, ptr() + mPtr, (size_t)byte_count);
    mPtr += byte_count;
    return byte_count;
  }
  else
  {
    // no error is generated for an empty memory stream
    return 0;
  }
}
//---------------------------------------------------------------------------
bool MemoryFile::seekSet_Implementation(long long offset)
{
  if (!isOpen())
    return false;
  mPtr = offset;
  if (mPtr < 0) mPtr = 0;
  if (mPtr > static_cast<long long>(mBuffer->bytesUsed()))
	  mPtr = mBuffer->bytesUsed();
  return true;
}
//-----------------------------------------------------------------------------
ref<VirtualFile> MemoryFile::clone() const
{
  ref<MemoryFile> file = new MemoryFile;
  file->operator=(*this);
  return file;
}
//---------------------------------------------------------------------------
