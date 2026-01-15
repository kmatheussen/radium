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

#ifndef File_INCLUDE_ONCE
#define File_INCLUDE_ONCE

#include <vlCore/Object.hpp>
#include <vlCore/vlnamespace.hpp>
#include <vlCore/String.hpp>
#include <vlCore/MD5CheckSum.hpp>
#include <map>

namespace vl
{

//---------------------------------------------------------------------------
// VirtualFile
//---------------------------------------------------------------------------
  /**
   * An abstract class representing a file.
   *
   * \sa
   * - VirtualDirectory
   * - DiskDirectory
   * - MemoryDirectory
   * - ZippedDirectory
   * - FileSystem
   * - DiskFile
   * - MemoryFile
   * - ZippedFile
  */
  class VLCORE_EXPORT VirtualFile: public Object
  {
    VL_INSTRUMENT_ABSTRACT_CLASS(vl::VirtualFile, Object)

  protected:
    VirtualFile(const VirtualFile& other): Object(other) {}

  public:
    //! Constructor.
    VirtualFile() {}

    //! Computes the crc32 of the entire file.
    unsigned int crc32();

    //! Computes the md5 of the entire file.
    MD5CheckSum md5();

    //! Opens the file in the specified mode.
    virtual bool open(EOpenMode mode) = 0;

    //! Returns \p true if the file has been opened.
    virtual bool isOpen() const = 0;

    //! Returns \p true if the file exists.
    virtual bool exists() const = 0;

    //! Closes the file.
    virtual void close() = 0;

    //! Returns the size of the file in bytes.
    virtual long long size() const = 0;

    //! Creates a clone of this class instance.
    virtual ref<VirtualFile> clone() const = 0;

    VirtualFile& operator=(const VirtualFile& other) { super::operator=(other); mPath = other.mPath; return *this; }

    //! Returns the \p path of the file.
    const String& path() const { return mPath; }

    //! Changes the path bound to a VirtualFile. Use carefully this function, you shouldn't rename a VirtualFile managed by a VirtualDirectory.
    void setPath(const String& name) { mPath = name; mPath.normalizeSlashes(); }

    //! Reads byte_count bytes and returns to the original position.
    //! Returns the number of bytes read.
    long long peek(void* buffer, long long byte_count);

    //! Reads \p byte_count bytes from a file. Returns the number of bytes actually read.
    long long read(void* buffer, long long byte_count);

    //! Writes \p byte_count bytes to a file. Returns the number of bytes actually written.
    long long write(const void* buffer, long long byte_count);

    //! Returns the current position in the file.
    long long position() const;

    //! Changes the current read/write position of a file.
    bool seekSet(long long offset);

    //! Changes the current read/write position of a file.
    bool seekCur(long long offset);

    //! Changes the current read/write position of a file.
    bool seekEnd(long long offset);

    //! Returns true if \p position() >= \p size()
    bool endOfFile() const { return position() >= size(); }

    //! Loads the entire file in the specified vector.
    //! Returns the number of bytes read.
    //! The file must be closed before calling this function.
    long long load(std::vector<char>& data);

    //! Loads the entire file in the specified buffer.
    //! Returns the number of bytes read.
    //! The file must be closed before calling this function.
    long long load(void* buffer, long long max=-1);

    //! Reads single entry.
    double             readDouble(bool little_endian_data=true);
    //! Reads single entry.
    float              readFloat (bool little_endian_data=true);
    //! Reads single entry.
    unsigned long long readUInt64(bool little_endian_data=true);
    //! Reads single entry.
    long long          readSInt64(bool little_endian_data=true);
    //! Reads single entry.
    unsigned int       readUInt32(bool little_endian_data=true);
    //! Reads single entry.
    int                readSInt32(bool little_endian_data=true);
    //! Reads single entry.
    unsigned short     readUInt16(bool little_endian_data=true);
    //! Reads single entry.
    short              readSInt16(bool little_endian_data=true);
    //! Reads single entry.
    unsigned char      readUInt8();
    //! Reads single entry.
    char               readSInt8();

    //! Reads multiple entries. Returns the number of bytes read.
    long long readDouble(double*             buffer, long long count, bool little_endian_data=true);
    //! Reads multiple entries. Returns the number of bytes read.
    long long readFloat (float*              buffer, long long count, bool little_endian_data=true);
    //! Reads multiple entries. Returns the number of bytes read.
    long long readUInt64(unsigned long long* buffer, long long count, bool little_endian_data=true);
    //! Reads multiple entries. Returns the number of bytes read.
    long long readSInt64(long long*          buffer, long long count, bool little_endian_data=true);
    //! Reads multiple entries. Returns the number of bytes read.
    long long readUInt32(unsigned int*       buffer, long long count, bool little_endian_data=true);
    //! Reads multiple entries. Returns the number of bytes read.
    long long readSInt32(int*                buffer, long long count, bool little_endian_data=true);
    //! Reads multiple entries. Returns the number of bytes read.
    long long readUInt16(unsigned short*     buffer, long long count, bool little_endian_data=true);
    //! Reads multiple entries. Returns the number of bytes read.
    long long readSInt16(short*              buffer, long long count, bool little_endian_data=true);
    //! Reads multiple entries. Returns the number of bytes read.
    long long readUInt8 (unsigned char*      buffer, long long count);
    //! Reads multiple entries. Returns the number of bytes read.
    long long readSInt8 (char*               buffer, long long count);

    //! Writes a single entry. Returns the number of bytes written.
    long long writeDouble(double             data, bool little_endian_data=true);
    //! Writes a single entry. Returns the number of bytes written.
    long long writeFloat (float              data, bool little_endian_data=true);
    //! Writes a single entry. Returns the number of bytes written.
    long long writeUInt64(unsigned long long data, bool little_endian_data=true);
    //! Writes a single entry. Returns the number of bytes written.
    long long writeSInt64(long long          data, bool little_endian_data=true);
    //! Writes a single entry. Returns the number of bytes written.
    long long writeUInt32(unsigned int       data, bool little_endian_data=true);
    //! Writes a single entry. Returns the number of bytes written.
    long long writeSInt32(int                data, bool little_endian_data=true);
    //! Writes a single entry. Returns the number of bytes written.
    long long writeUInt16(unsigned short     data, bool little_endian_data=true);
    //! Writes a single entry. Returns the number of bytes written.
    long long writeSInt16(short              data, bool little_endian_data=true);
    //! Writes a single entry. Returns the number of bytes written.
    long long writeUInt8 (unsigned char      data);
    //! Writes a single entry. Returns the number of bytes written.
    long long writeSInt8 (char               data);

    //! Writes multiple entries. Returns the number of bytes written.
    long long writeDouble(const double*             buffer, long long count, bool little_endian_data=true);
    //! Writes multiple entries. Returns the number of bytes written.
    long long writeFloat (const float*              buffer, long long count, bool little_endian_data=true);
    //! Writes multiple entries. Returns the number of bytes written.
    long long writeUInt64(const unsigned long long* buffer, long long count, bool little_endian_data=true);
    //! Writes multiple entries. Returns the number of bytes written.
    long long writeSInt64(const long long*          buffer, long long count, bool little_endian_data=true);
    //! Writes multiple entries. Returns the number of bytes written.
    long long writeUInt32(const unsigned int*       buffer, long long count, bool little_endian_data=true);
    //! Writes multiple entries. Returns the number of bytes written.
    long long writeSInt32(const int*                buffer, long long count, bool little_endian_data=true);
    //! Writes multiple entries. Returns the number of bytes written.
    long long writeUInt16(const unsigned short*     buffer, long long count, bool little_endian_data=true);
    //! Writes multiple entries. Returns the number of bytes written.
    long long writeSInt16(const short*              buffer, long long count, bool little_endian_data=true);
    //! Writes multiple entries. Returns the number of bytes written.
    long long writeUInt8 (const unsigned char*      buffer, long long count);
    //! Writes multiple entries. Returns the number of bytes written.
    long long writeSInt8 (const char*               buffer, long long count);

  protected:
    virtual long long read_Implementation(void* buffer, long long byte_count) = 0;
    virtual long long write_Implementation(const void* buffer, long long byte_count) = 0;
    virtual long long position_Implementation() const = 0;
    virtual bool seekSet_Implementation(long long offset) = 0;

  protected:
    long long write64(const void* buffer, long long count, bool little_endian_data=true);
    long long write32(const void* buffer, long long count, bool little_endian_data=true);
    long long write16(const void* buffer, long long count, bool little_endian_data=true);

    long long read64(void* buffer, long long count, bool little_endian_data=true);
    long long read32(void* buffer, long long count, bool little_endian_data=true);
    long long read16(void* buffer, long long count, bool little_endian_data=true);

    long long write64(const void* buffer, bool little_endian_data=true);
    long long write32(const void* buffer, bool little_endian_data=true);
    long long write16(const void* buffer, bool little_endian_data=true);

    long long read64(void* buffer, bool little_endian_data=true);
    long long read32(void* buffer, bool little_endian_data=true);
    long long read16(void* buffer, bool little_endian_data=true);

  protected:
    String mPath;
  };
//-----------------------------------------------------------------------------
  //! Utility function, equivalent to \p vl::defFileSystem()->locateFile(path)
  VLCORE_EXPORT ref<VirtualFile> locateFile(const String& path);
//-----------------------------------------------------------------------------
}

#endif
