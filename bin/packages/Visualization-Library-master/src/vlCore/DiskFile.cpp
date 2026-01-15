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

#include <vlCore/DiskFile.hpp>
#include <stdio.h>

#if defined(__APPLE__) || (__FreeBSD__)
  #define fseeko64 fseeko
#endif

using namespace vl;

//-----------------------------------------------------------------------------
// DiskFile
//-----------------------------------------------------------------------------
DiskFile::DiskFile(const String& path)
{
  mHandle = NULL;
  setPath(path);
}
//-----------------------------------------------------------------------------
DiskFile::~DiskFile()
{
  close();
}
//-----------------------------------------------------------------------------
bool DiskFile::open(const String& path, EOpenMode mode)
{
  setPath(path);
  return open(mode);
}
//-----------------------------------------------------------------------------
bool DiskFile::open(EOpenMode mode)
{
  if ( isOpen() )
  {
    Log::error("DiskFile::open(): file already open.\n");
    return false;
  }

#if defined(VL_PLATFORM_WINDOWS)
  mHandle = INVALID_HANDLE_VALUE;
  switch(mode)
  {
  case OM_ReadOnly:
      mHandle = CreateFile( (const wchar_t*)path().ptr(),
                            GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL ); break;
  case OM_WriteOnly:
      mHandle = CreateFile( (const wchar_t*)path().ptr(),
                            GENERIC_WRITE, FILE_SHARE_READ, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL ); break;
  default:
    break;
  }
  if (mHandle == INVALID_HANDLE_VALUE)
#elif defined(__GNUG__)
  // encode to utf8 for linux
  std::vector<unsigned char> utf8;
  path().toUTF8( utf8, false );
  switch(mode)
  {
  case OM_ReadOnly:  mHandle = fopen( (char*)&utf8[0], "rb"); break;
  case OM_WriteOnly: mHandle = fopen( (char*)&utf8[0], "wb"); break;
  default:
    break;
  }
  if (mHandle == NULL)
#endif
  // see the ifs above
  {
    Log::error( Say("DiskFile::open(): error opening input file '%s'\n") << path() );
    return false;
  }

  return true;
}
//-----------------------------------------------------------------------------
bool DiskFile::isOpen() const
{
  return mHandle != NULL;
}
//-----------------------------------------------------------------------------
void DiskFile::close()
{
  if (mHandle)
  {
    #if defined(VL_PLATFORM_WINDOWS)
      CloseHandle(mHandle);
    #elif defined(__GNUG__)
      fclose(mHandle);
    #endif
  }
  mHandle = NULL;
}
//-----------------------------------------------------------------------------
long long DiskFile::size() const
{
  #if defined(VL_PLATFORM_WINDOWS)
    // opens the file
    HANDLE hdl = CreateFile(
      (const wchar_t*)path().ptr(),
      FILE_READ_ATTRIBUTES,
      FILE_SHARE_READ,
      NULL,
      OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL,
      NULL
    );

    if (mHandle == INVALID_HANDLE_VALUE)
    {
      Log::error( Say("DiskFile::size(): file '%s' does not seem to exist.\n") << path() );
      return 0;
    }

    #if defined(__MINGW32_VERSION)
      DWORD size = GetFileSize(hdl, NULL);
      CloseHandle(hdl);
      if (size != INVALID_FILE_SIZE )
        return size;
      else
        return -1;
    #else
      LARGE_INTEGER file_size;
      BOOL ok = GetFileSizeEx( hdl, &file_size );
      CloseHandle(hdl);
      return ok ? file_size.QuadPart : -1;
    #endif
  #elif defined(__GNUG__)
    // http://linux.die.net/man/2/stat
    struct stat mybuf;
    memset(&mybuf, 0, sizeof(struct stat));
    // encode to utf8 for linux
    std::vector<unsigned char> utf8;
    path().toUTF8( utf8, false );
    if (utf8.empty())
      return 0;
    if (stat((char*)&utf8[0], &mybuf) == -1)
      return 0;
    else
      return (long long)mybuf.st_size;
  #endif
}
//-----------------------------------------------------------------------------
bool DiskFile::exists() const
{
  if (path().empty())
    return false;
  #if defined(VL_PLATFORM_WINDOWS)
    /*VL_CHECK( sizeof(wchar_t) == sizeof(unsigned short) )*/
    // opens the file
    HANDLE hdl = CreateFile(
      (const wchar_t*)path().ptr(),
      FILE_READ_ATTRIBUTES,
      FILE_SHARE_READ,
      NULL,
      OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL,
      NULL
    );

    if (hdl != INVALID_HANDLE_VALUE)
    {
      CloseHandle(hdl);
      return true;
    }
    else
      return false;
  #elif defined(__GNUG__)
    std::vector<unsigned char> utf8;
    path().toUTF8( utf8, false );
    if (utf8.empty())
      return false;
    FILE* fin = fopen( (char*)&utf8[0], "rb");
    if (fin != NULL)
    {
      fclose(fin);
      return true;
    }
    else
      return false;
  #endif
}
//-----------------------------------------------------------------------------
long long DiskFile::read_Implementation(void* buffer, long long byte_count)
{
  if (!mHandle)
  {
    Log::error("DiskFile::read_Implementation() called on closed file!\n");
    return 0;
  }

  long long count = 0;
  #if defined(VL_PLATFORM_WINDOWS)

    #if 0
      // avoid win xp problem: read fails if request to read much more bytes than the file size
      // but makes the reading way too slow.
      long long file_size = size();
      byte_count = byte_count < file_size ? byte_count : (long long)file_size;
    #endif

    char* ptr = (char*)buffer;
    DWORD NumberOfBytesRead = 0;
    // reads blocks of 20 megs due to possible problems under win xp
    long long bytes = byte_count < 20*1024*1024 ? byte_count : 20*1024*1024;
    while( ReadFile( mHandle, ptr, (DWORD)bytes, &NumberOfBytesRead, NULL ) && NumberOfBytesRead )
    {
      byte_count -= NumberOfBytesRead;
      bytes = byte_count < 20*1024*1024 ? byte_count : 20*1024*1024;
      ptr   += NumberOfBytesRead;
      count += NumberOfBytesRead;
    }
  #elif defined(__GNUG__)
    char* ptr = (char*)buffer;
    long long bytes_read = 0;
    while( (bytes_read = fread(ptr, 1, byte_count, mHandle)) )
    {
      byte_count -= bytes_read;
      ptr   += bytes_read;
      count += bytes_read;
    }
    if (ferror(mHandle))
      perror("Error reading file: ");
  #endif
  return count;
}
//-----------------------------------------------------------------------------
long long DiskFile::write_Implementation(const void* buffer, long long byte_count)
{
  if (!mHandle)
  {
    Log::error("DiskFile::write_Implementation() called on closed file!\n");
    return 0;
  }

  long long count = 0;
  #if defined(VL_PLATFORM_WINDOWS)
    DWORD NumberOfBytesWritten = 0;
    WriteFile( mHandle, buffer, (DWORD)byte_count, &NumberOfBytesWritten, NULL );
    count = NumberOfBytesWritten;
  #elif defined(__GNUG__)
    count = fwrite( buffer, 1, byte_count, mHandle );
    if (ferror(mHandle))
      perror("Error reading file: ");
  #endif
  return count;
}
//-----------------------------------------------------------------------------
long long DiskFile::position_Implementation() const
{
  if (!mHandle)
  {
    Log::error("DiskFile::position_Implementation() called on closed file!\n");
    return -1;
  }

  #if defined(VL_PLATFORM_WINDOWS)
    LARGE_INTEGER position;
    position.QuadPart = 0;
    SetFilePointerEx(
      mHandle,
      position,
      &position,
      FILE_CURRENT );
    return position.QuadPart;
  #elif defined(__GNUG__)
    return ftell(mHandle);
  #endif
}
//-----------------------------------------------------------------------------
bool DiskFile::seekSet_Implementation(long long offset)
{
  if (!mHandle)
  {
    Log::error("DiskFile::seekSet_Implementation() called on closed file!\n");
    return false;
  }

  #if defined(VL_PLATFORM_WINDOWS)
    LARGE_INTEGER position;
    position.QuadPart = offset;
    if (mHandle)
    {
      SetFilePointerEx(
        mHandle,
        position,
        &position,
        FILE_BEGIN );
    }
  #elif defined(__GNUG__)
    fseeko64(mHandle, offset, SEEK_SET);
  #endif
  return true;
}
//-----------------------------------------------------------------------------
ref<VirtualFile> DiskFile::clone() const
{
  ref<DiskFile> file = new DiskFile;
  file->operator=(*this);
  return file;
}
//-----------------------------------------------------------------------------
