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

#include <vlCore/VirtualFile.hpp>
#include <vlCore/FileSystem.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>
#include <vlCore/CRC32CheckSum.hpp>

using namespace vl;

//-----------------------------------------------------------------------------
ref<VirtualFile> vl::locateFile(const String& path)  { return defFileSystem()->locateFile(path); } 
//-----------------------------------------------------------------------------
unsigned int VirtualFile::crc32()
{
  unsigned int sum = 0;

  if ( open(OM_ReadOnly) )
  {
    CRC32CheckSum check_sum;
    sum = check_sum.compute(this);
    close();
  }

  return sum;
}
//-----------------------------------------------------------------------------
MD5CheckSum VirtualFile::md5()
{
  MD5CheckSum check_sum;
  if ( open(OM_ReadOnly) )
  {
    check_sum.compute(this);
    close();
  }
  return check_sum;
}
//-----------------------------------------------------------------------------
long long VirtualFile::peek(void* buffer, long long byte_count)
{
  if ( !isOpen() )
  {
    Log::error("VirtualFile::peek(): the file is closed.\n");
    return 0;
  }
  long long pos = position();
  long long read_bytes = read(buffer, byte_count);
  if ( !seekSet( pos ) )
    Log::error("VirtualFile::peek() called on a non seek-able VirtualFile.\n");
  return read_bytes;
}
//-----------------------------------------------------------------------------
long long VirtualFile::read(void* buffer, long long byte_count)
{
  if (byte_count > 0)
    return read_Implementation(buffer, byte_count);
  else
    return 0;
}
//-----------------------------------------------------------------------------
long long VirtualFile::write(const void* buffer, long long byte_count)
{
  if (byte_count > 0)
    return write_Implementation(buffer, byte_count);
  else
    return 0;
}
//-----------------------------------------------------------------------------
long long VirtualFile::position() const
{
  return position_Implementation();
}
//-----------------------------------------------------------------------------
bool VirtualFile::seekSet(long long offset)
{
  if (offset < 0)
  {
    Log::error( Say("VirtualFile::seekSet(%n): invalid offset.\n") << offset);
    seekSet_Implementation(0);
    return false;
  }
  if (offset > size() )
  {
    Log::error( Say("VirtualFile::seekSet(%n): invalid offset past end of stream.\n") << offset);
    seekSet_Implementation(size());
    return false;
  }

  return seekSet_Implementation(offset);
}
//-----------------------------------------------------------------------------
bool VirtualFile::seekCur(long long offset)
{
  return seekSet( position() + offset );
}
//-----------------------------------------------------------------------------
bool VirtualFile::seekEnd(long long offset)
{
  return seekSet( size() + offset );
}
//-----------------------------------------------------------------------------
long long VirtualFile::load(std::vector<char>& data)
{
  data.resize( (size_t)size() );
  if (data.size())
    return load(&data[0], data.size());
  else
    return 0;
}
//-----------------------------------------------------------------------------
long long VirtualFile::load(void* buffer, long long max)
{
  if (max<0)
    max = size();
  if ( open(OM_ReadOnly) )
  {
    long long bytes = read(buffer,max);
    close();
    return bytes;
  }
  else
  {
    Log::error( Say("Cannot load file '%s'.\n") << path() );
    return 0;
  }
}
//-----------------------------------------------------------------------------
// UTIITY FUNCTIONS - READ SINGLE VALUE
//-----------------------------------------------------------------------------
double VirtualFile::readDouble(bool little_endian_data)
{
  double data = 0;
  read64(&data, little_endian_data);
  return data;
}
//-----------------------------------------------------------------------------
float VirtualFile::readFloat(bool little_endian_data)
{
  float data = 0;
  read32(&data, little_endian_data);
  return data;
}
//-----------------------------------------------------------------------------
unsigned long long VirtualFile::readUInt64(bool little_endian_data)
{
  unsigned long long data = 0;
  read64(&data, little_endian_data);
  return data;
}
//-----------------------------------------------------------------------------
long long VirtualFile::readSInt64(bool little_endian_data)
{
  long long data = 0;
  read64(&data, little_endian_data);
  return data;
}
//-----------------------------------------------------------------------------
unsigned int VirtualFile::readUInt32(bool little_endian_data)
{
  unsigned long data = 0;
  read32(&data, little_endian_data);
  return data;
}
//-----------------------------------------------------------------------------
int VirtualFile::readSInt32(bool little_endian_data)
{
  int data = 0;
  read32(&data, little_endian_data);
  return data;
}
//-----------------------------------------------------------------------------
unsigned short VirtualFile::readUInt16(bool little_endian_data)
{
  unsigned short data = 0;
  read16(&data, little_endian_data);
  return data;
}
//-----------------------------------------------------------------------------
short VirtualFile::readSInt16(bool little_endian_data)
{
  short data = 0;
  read16(&data, little_endian_data);
  return data;
}
//-----------------------------------------------------------------------------
unsigned char VirtualFile::readUInt8()
{
  unsigned char data = 0;
  read(&data, 1);
  return data;
}
//-----------------------------------------------------------------------------
char VirtualFile::readSInt8()
{
  char data = 0;
  read(&data, 1);
  return data;
}
//-----------------------------------------------------------------------------
// UTIITY FUNCTIONS - READ MULTIPLE VALUES
//-----------------------------------------------------------------------------
long long VirtualFile::readDouble(double* buffer, long long count, bool little_endian_data)
{
  return read64(buffer, count, little_endian_data);
}
//-----------------------------------------------------------------------------
long long VirtualFile::readFloat(float* buffer, long long count, bool little_endian_data)
{
  return read32(buffer, count, little_endian_data);
}
//-----------------------------------------------------------------------------
long long VirtualFile::readUInt64(unsigned long long* buffer, long long count, bool little_endian_data)
{
  return read64(buffer, count, little_endian_data);
}
//-----------------------------------------------------------------------------
long long VirtualFile::readSInt64(long long* buffer, long long count, bool little_endian_data)
{
  return read64(buffer, count, little_endian_data);
}
//-----------------------------------------------------------------------------
long long VirtualFile::readUInt32(unsigned int* buffer, long long count, bool little_endian_data)
{
  return read32(buffer, count, little_endian_data);
}
//-----------------------------------------------------------------------------
long long VirtualFile::readSInt32(int* buffer, long long count, bool little_endian_data)
{
  return read32(buffer, count, little_endian_data);
}
//-----------------------------------------------------------------------------
long long VirtualFile::readUInt16(unsigned short* buffer, long long count, bool little_endian_data)
{
  return read16(buffer, count, little_endian_data);
}
//-----------------------------------------------------------------------------
long long VirtualFile::readSInt16(short* buffer, long long count, bool little_endian_data)
{
  return read16(buffer, count, little_endian_data);
}
//-----------------------------------------------------------------------------
long long VirtualFile::readUInt8(unsigned char* buffer, long long count)
{
  return read(buffer, count);
}
//-----------------------------------------------------------------------------
long long VirtualFile::readSInt8(char* buffer, long long count)
{
  return read(buffer, count);
}
//-----------------------------------------------------------------------------
// UTILITY FUNCTIONS - WRITE SINGLE VALUES
//-----------------------------------------------------------------------------
long long VirtualFile::writeDouble(double data, bool little_endian_data)
{
  return write64(&data, little_endian_data);
}
//-----------------------------------------------------------------------------
long long VirtualFile::writeFloat(float data, bool little_endian_data)
{
  return write32(&data, little_endian_data);
}
//-----------------------------------------------------------------------------
long long VirtualFile::writeUInt64(unsigned long long data, bool little_endian_data)
{
  return write64(&data, little_endian_data);
}
//-----------------------------------------------------------------------------
long long VirtualFile::writeSInt64(long long data, bool little_endian_data)
{
  return write64(&data, little_endian_data);
}
//-----------------------------------------------------------------------------
long long VirtualFile::writeUInt32(unsigned int data, bool little_endian_data)
{
  return write32(&data, little_endian_data);
}
//-----------------------------------------------------------------------------
long long VirtualFile::writeSInt32(int data, bool little_endian_data)
{
  return write32(&data, little_endian_data);
}
//-----------------------------------------------------------------------------
long long VirtualFile::writeUInt16(unsigned short data, bool little_endian_data)
{
  return write16(&data, little_endian_data);
}
//-----------------------------------------------------------------------------
long long VirtualFile::writeSInt16(short data, bool little_endian_data)
{
  return write16(&data, little_endian_data);
}
//-----------------------------------------------------------------------------
long long VirtualFile::writeUInt8(unsigned char data)
{
  return write(&data, 1);
}
//-----------------------------------------------------------------------------
long long VirtualFile::writeSInt8(char data)
{
  return write(&data, 1);
}
//-----------------------------------------------------------------------------
// UTILITY FUNCTIONS - WRITE MULTIPLE VALUES
//-----------------------------------------------------------------------------
long long VirtualFile::writeDouble(const double* buffer, long long count, bool little_endian_data)
{
  return write64(buffer, count, little_endian_data);
}
//-----------------------------------------------------------------------------
long long VirtualFile::writeFloat(const float* buffer, long long count, bool little_endian_data)
{
  return write32(buffer, count, little_endian_data);
}
//-----------------------------------------------------------------------------
long long VirtualFile::writeUInt64(const unsigned long long* buffer, long long count, bool little_endian_data)
{
  return write64(buffer, count, little_endian_data);
}
//-----------------------------------------------------------------------------
long long VirtualFile::writeSInt64(const long long* buffer, long long count, bool little_endian_data)
{
  return write64(buffer, count, little_endian_data);
}
//-----------------------------------------------------------------------------
long long VirtualFile::writeUInt32(const unsigned int* buffer, long long count, bool little_endian_data)
{
  return write32(buffer, count, little_endian_data);
}
//-----------------------------------------------------------------------------
long long VirtualFile::writeSInt32(const int* buffer, long long count, bool little_endian_data)
{
  return write32(buffer, count, little_endian_data);
}
//-----------------------------------------------------------------------------
long long VirtualFile::writeUInt16(const unsigned short* buffer, long long count, bool little_endian_data)
{
  return write16(buffer, count, little_endian_data);
}
//-----------------------------------------------------------------------------
long long VirtualFile::writeSInt16(const short* buffer, long long count, bool little_endian_data)
{
  return write16(buffer, count, little_endian_data);
}
//-----------------------------------------------------------------------------
long long VirtualFile::writeUInt8(const unsigned char* buffer, long long count)
{
  return write(buffer, count);
}
//-----------------------------------------------------------------------------
long long VirtualFile::writeSInt8(const char* buffer, long long count)
{
  return write(buffer, count);
}
//-----------------------------------------------------------------------------
// GENERIC IO FUNCTIONS
//-----------------------------------------------------------------------------
long long VirtualFile::write64(const void* buffer, long long count, bool little_endian_data)
{
  unsigned short bet = 0x00FF;
  bool little_endian_cpu = ((unsigned char*)&bet)[0] == 0xFF;
  if (little_endian_cpu != little_endian_data)
  {
    long long ret = 0;
    for(long long i=0; i<count; ++i)
      ret += write64( (char*)buffer+i*8, little_endian_data );
    return ret;
  }
  else
    return write(buffer, 8*count);
}
//-----------------------------------------------------------------------------
long long VirtualFile::write32(const void* buffer, long long count, bool little_endian_data)
{
  unsigned short bet = 0x00FF;
  bool little_endian_cpu = ((unsigned char*)&bet)[0] == 0xFF;
  if (little_endian_cpu != little_endian_data)
  {
    long long ret = 0;
    for(long long i=0; i<count; ++i)
      ret += write32( (char*)buffer+i*4, little_endian_data );
    return ret;
  }
  else
    return write(buffer, 4*count);
}
//-----------------------------------------------------------------------------
long long VirtualFile::write16(const void* buffer, long long count, bool little_endian_data)
{
  unsigned short bet = 0x00FF;
  bool little_endian_cpu = ((unsigned char*)&bet)[0] == 0xFF;
  if (little_endian_cpu != little_endian_data)
  {
    long long ret = 0;
    for(long long i=0; i<count; ++i)
      ret += write16( (char*)buffer+i*2, little_endian_data );
    return ret;
  }
  else
    return write(buffer, 2*count);
}
//-----------------------------------------------------------------------------
long long VirtualFile::read64(void* buffer, long long count, bool little_endian_data)
{
  long long ret = read(buffer, 8*count);
  unsigned short bet = 0x00FF;
  bool little_endian_cpu = ((unsigned char*)&bet)[0] == 0xFF;
  if ( little_endian_cpu != little_endian_data )
  {
    char* bytes = (char*)buffer;
    for(int i=0; i<count; ++i, bytes+=8)
    {
      char tmp;
      tmp = bytes[0]; bytes[0] = bytes[7]; bytes[7] = tmp;
      tmp = bytes[1]; bytes[1] = bytes[6]; bytes[6] = tmp;
      tmp = bytes[2]; bytes[2] = bytes[5]; bytes[5] = tmp;
      tmp = bytes[3]; bytes[3] = bytes[4]; bytes[4] = tmp;
    }
  }
  return ret;
}
//-----------------------------------------------------------------------------
long long VirtualFile::read32(void* buffer, long long count, bool little_endian_data)
{
  long long ret = read(buffer, 4*count);
  unsigned short bet = 0x00FF;
  bool little_endian_cpu = ((unsigned char*)&bet)[0] == 0xFF;
  if ( little_endian_cpu != little_endian_data )
  {
    char* bytes = (char*)buffer;
    for(int i=0; i<count; ++i, bytes+=4)
    {
      char tmp;
      tmp = bytes[0]; bytes[0] = bytes[3]; bytes[3] = tmp;
      tmp = bytes[1]; bytes[1] = bytes[2]; bytes[2] = tmp;
    }
  }
  return ret;
}
//-----------------------------------------------------------------------------
long long VirtualFile::read16(void* buffer, long long count, bool little_endian_data)
{
  long long ret = read(buffer, 2*count);
  unsigned short bet = 0x00FF;
  bool little_endian_cpu = ((unsigned char*)&bet)[0] == 0xFF;
  if ( little_endian_cpu != little_endian_data )
  {
    char* bytes = (char*)buffer;
    for(int i=0; i<count; ++i, bytes+=2)
    {
      char tmp = bytes[0]; bytes[0] = bytes[1]; bytes[1] = tmp;
    }
  }
  return ret;
}
//-----------------------------------------------------------------------------
long long VirtualFile::write64(const void* buffer, bool little_endian_data)
{
  unsigned short bet = 0x00FF;
  bool little_endian_cpu = ((unsigned char*)&bet)[0] == 0xFF;
  long long dummy = *(long long*)buffer;
  char* byte = (char*)&dummy;
  if (little_endian_cpu != little_endian_data)
  {
    char tmp;
    tmp = byte[0]; byte[0] = byte[7]; byte[7] = tmp;
    tmp = byte[1]; byte[1] = byte[6]; byte[6] = tmp;
    tmp = byte[2]; byte[2] = byte[5]; byte[5] = tmp;
    tmp = byte[3]; byte[3] = byte[4]; byte[4] = tmp;
  }
  return write(byte, 8);
}
//-----------------------------------------------------------------------------
long long VirtualFile::write32(const void* buffer, bool little_endian_data)
{
  unsigned short bet = 0x00FF;
  bool little_endian_cpu = ((unsigned char*)&bet)[0] == 0xFF;
  int dummy = *(int*)buffer;
  char* byte = (char*)&dummy;
  if (little_endian_cpu != little_endian_data)
  {
    char tmp;
    tmp = byte[0]; byte[0] = byte[3]; byte[3] = tmp;
    tmp = byte[1]; byte[1] = byte[2]; byte[2] = tmp;
  }
  return write(byte, 4);
}
//-----------------------------------------------------------------------------
long long VirtualFile::write16(const void* buffer, bool little_endian_data)
{
  unsigned short bet = 0x00FF;
  bool little_endian_cpu = ((unsigned char*)&bet)[0] == 0xFF;
  short dummy = *(short*)buffer;
  char* byte = (char*)&dummy;
  if (little_endian_cpu != little_endian_data)
  {
    char tmp = byte[0]; byte[0] = byte[1]; byte[1] = tmp;
  }
  return write(byte, 2);
}
//-----------------------------------------------------------------------------
long long VirtualFile::read64(void* buffer, bool little_endian_data)
{
  char* bytes = (char*)buffer;
  long long ret = read(bytes, 8);
  unsigned short bet = 0x00FF;
  bool little_endian_cpu = ((unsigned char*)&bet)[0] == 0xFF;
  if ( little_endian_cpu != little_endian_data )
  {
    char tmp;
    tmp = bytes[0]; bytes[0] = bytes[7]; bytes[7] = tmp;
    tmp = bytes[1]; bytes[1] = bytes[6]; bytes[6] = tmp;
    tmp = bytes[2]; bytes[2] = bytes[5]; bytes[5] = tmp;
    tmp = bytes[3]; bytes[3] = bytes[4]; bytes[4] = tmp;
  }
  return ret;
}
//-----------------------------------------------------------------------------
long long VirtualFile::read32(void* buffer, bool little_endian_data)
{
  char* bytes = (char*)buffer;
  long long ret = read(bytes, 4);
  unsigned short bet = 0x00FF;
  bool little_endian_cpu = ((unsigned char*)&bet)[0] == 0xFF;
  if ( little_endian_cpu != little_endian_data )
  {
    char tmp;
    tmp = bytes[0]; bytes[0] = bytes[3]; bytes[3] = tmp;
    tmp = bytes[1]; bytes[1] = bytes[2]; bytes[2] = tmp;
  }
  return ret;
}
//-----------------------------------------------------------------------------
long long VirtualFile::read16(void* buffer, bool little_endian_data)
{
  char* bytes = (char*)buffer;
  long long ret = read(bytes, 2);
  unsigned short bet = 0x00FF;
  bool little_endian_cpu = ((unsigned char*)&bet)[0] == 0xFF;
  if ( little_endian_cpu != little_endian_data )
  {
    char tmp = bytes[0]; bytes[0] = bytes[1]; bytes[1] = tmp;
  }
  return ret;
}
//-----------------------------------------------------------------------------
