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

#include <vlCore/ZippedFile.hpp>
#include <vlCore/CRC32CheckSum.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>
#include <zlib.h>
#include <stdio.h>

using namespace vl;

bool vl::compress(const void* data, size_t size, std::vector<unsigned char>& out_data, int level)
{
  const size_t CHUNK_SIZE = 128*1024;
  int ret, flush;
  unsigned have;
  z_stream strm;
  const unsigned char* in = (const unsigned char*)data;
  unsigned char out[CHUNK_SIZE];

  strm.zalloc = Z_NULL;
  strm.zfree = Z_NULL;
  strm.opaque = Z_NULL;
  ret = deflateInit(&strm, level);
  if (ret != Z_OK)
    return false;

  size_t avail = size;

  do
  {
    strm.avail_in = std::min(avail, CHUNK_SIZE);
    avail -= strm.avail_in;
    strm.next_in = (unsigned char*)in;
    in += strm.avail_in;
    flush = avail == 0 ? Z_FINISH : Z_NO_FLUSH;

    do
    {
      strm.avail_out = CHUNK_SIZE;
      strm.next_out = out;

      ret = deflate(&strm, flush);
      if(ret == Z_STREAM_ERROR)
      {
        deflateEnd(&strm);
        return false;
      }

      have = CHUNK_SIZE - strm.avail_out;
      out_data.insert( out_data.end(), out, out+have );
    } while (strm.avail_out == 0);

    VL_CHECK(strm.avail_in == 0);

  } while (flush != Z_FINISH);

  VL_CHECK(ret == Z_STREAM_END);

  deflateEnd(&strm);
  return true;
}

bool vl::decompress(const void* cdata, size_t csize, void* data_out)
{
  const size_t CHUNK_SIZE = 128*1024;
  int ret;
  unsigned have;
  z_stream strm;
  unsigned char* in = (unsigned char*)cdata;
  unsigned char out[CHUNK_SIZE];
  char* out_ptr = (char*)data_out;

  strm.zalloc = Z_NULL;
  strm.zfree = Z_NULL;
  strm.opaque = Z_NULL;
  strm.avail_in = 0;
  strm.next_in = Z_NULL;
  ret = inflateInit(&strm);
  if (ret != Z_OK)
      return false;

  size_t avail = csize;

  do
  {
    strm.avail_in = std::min(avail, CHUNK_SIZE);
    if (strm.avail_in == 0)
      break;
    avail -= strm.avail_in;
    strm.next_in = in;
    in += strm.avail_in;

    do
    {
      strm.avail_out = CHUNK_SIZE;
      strm.next_out = out;

      ret = inflate(&strm, Z_NO_FLUSH);
      VL_CHECK(ret != Z_STREAM_ERROR);
      switch (ret)
      {
      case Z_NEED_DICT:
      case Z_DATA_ERROR:
      case Z_MEM_ERROR:
        inflateEnd(&strm);
        return false;
      }

      have = CHUNK_SIZE - strm.avail_out;
      // data_out.insert( data_out.end(), out, out + have );
      memcpy(out_ptr, out, have);
      out_ptr += have;
    }
    while (strm.avail_out == 0);

    VL_CHECK(strm.avail_in == 0);

  } while (ret != Z_STREAM_END);

  inflateEnd(&strm);

  /*VL_CHECK(ret == Z_STREAM_END)*/
  return ret == Z_STREAM_END;
}

namespace
{
//-----------------------------------------------------------------------------
  inline int zcompress(FILE *source, FILE *dest, int level)
  {
    const int CHUNK_SIZE = 128*1024;
    int ret, flush;
    unsigned have;
    z_stream strm;
    unsigned char in[CHUNK_SIZE];
    unsigned char out[CHUNK_SIZE];

    strm.zalloc = Z_NULL;
    strm.zfree = Z_NULL;
    strm.opaque = Z_NULL;
    ret = deflateInit(&strm, level);
    if (ret != Z_OK)
      return ret;

    do
    {
      strm.avail_in = (uInt)fread(in, 1, CHUNK_SIZE, source);
      if (ferror(source))
      {
        deflateEnd(&strm);
        return Z_ERRNO;
      }
      flush = feof(source) ? Z_FINISH : Z_NO_FLUSH;
      strm.next_in = in;

      do
      {
        strm.avail_out = CHUNK_SIZE;
        strm.next_out = out;

        ret = deflate(&strm, flush);
        VL_CHECK(ret != Z_STREAM_ERROR);

        have = CHUNK_SIZE - strm.avail_out;
        if (fwrite(out, 1, have, dest) != have || ferror(dest))
        {
          deflateEnd(&strm);
          return Z_ERRNO;
        }

      } while (strm.avail_out == 0);

      VL_CHECK(strm.avail_in == 0);

    } while (flush != Z_FINISH);

    VL_CHECK(ret == Z_STREAM_END);

    (void)deflateEnd(&strm);
    return Z_OK;
  }
//-----------------------------------------------------------------------------
  inline int zdecompress(FILE *source, FILE *dest)
  {
    const int CHUNK_SIZE = 128*1024;
    int ret;
    unsigned have;
    z_stream strm;
    unsigned char in[CHUNK_SIZE];
    unsigned char out[CHUNK_SIZE];

    strm.zalloc = Z_NULL;
    strm.zfree = Z_NULL;
    strm.opaque = Z_NULL;
    strm.avail_in = 0;
    strm.next_in = Z_NULL;
    ret = inflateInit(&strm);
    if (ret != Z_OK)
        return ret;

    do
    {
      strm.avail_in = (uInt)fread(in, 1, CHUNK_SIZE, source);
      if (ferror(source))
      {
        inflateEnd(&strm);
        return Z_ERRNO;
      }
      if (strm.avail_in == 0)
        break;
      strm.next_in = in;

      do
      {
        strm.avail_out = CHUNK_SIZE;
        strm.next_out = out;

        ret = inflate(&strm, Z_NO_FLUSH);
        VL_CHECK(ret != Z_STREAM_ERROR);
        switch (ret)
        {
        case Z_NEED_DICT:
          ret = Z_DATA_ERROR;
        case Z_DATA_ERROR:
        case Z_MEM_ERROR:
          inflateEnd(&strm);
          return ret;
        }

        have = CHUNK_SIZE - strm.avail_out;
        if (fwrite(out, 1, have, dest) != have || ferror(dest))
        {
          inflateEnd(&strm);
          return Z_ERRNO;
        }
      }
      while (strm.avail_out == 0);

      VL_CHECK(strm.avail_in == 0);

    } while (ret != Z_STREAM_END);

    inflateEnd(&strm);
    return ret == Z_STREAM_END ? Z_OK : Z_DATA_ERROR;
  }
//-----------------------------------------------------------------------------
  inline int zdecompress(VirtualFile *source, char *dest, unsigned int bytes_to_read)
  {
    const unsigned int CHUNK_SIZE = 128*1024;
    int ret;
    unsigned have;
    z_stream strm;
    unsigned char in[CHUNK_SIZE];
    unsigned char out[CHUNK_SIZE];

    strm.zalloc = Z_NULL;
    strm.zfree = Z_NULL;
    strm.opaque = Z_NULL;
    strm.avail_in = 0;
    strm.next_in = Z_NULL;

    ret = inflateInit2(&strm, -15);
    if (ret != Z_OK)
      return ret;

    do
    {
      unsigned int byte_count = CHUNK_SIZE < bytes_to_read  ? CHUNK_SIZE : bytes_to_read;
      strm.avail_in = (uInt)source->read(in, byte_count);
      bytes_to_read -= strm.avail_in;

      if (strm.avail_in == 0)
        break;
      strm.next_in = in;

      do
      {
        strm.avail_out = CHUNK_SIZE;
        strm.next_out = out;

        ret = inflate(&strm, Z_NO_FLUSH);
        VL_CHECK(ret != Z_STREAM_ERROR);
        switch (ret)
        {
        case Z_NEED_DICT:
          ret = Z_DATA_ERROR;
        case Z_DATA_ERROR:
        case Z_MEM_ERROR:
          inflateEnd(&strm);
          return ret;
        }

        have = CHUNK_SIZE - strm.avail_out;
        memcpy(dest, out, have);
        dest += have;
      }
      while (strm.avail_out == 0);

      VL_CHECK(strm.avail_in == 0);

    } while (ret != Z_STREAM_END);

    inflateEnd(&strm);
    return ret == Z_STREAM_END ? Z_OK : Z_DATA_ERROR;
  }
}
//-----------------------------------------------------------------------------
// ZippedFile
//-----------------------------------------------------------------------------
ZippedFile::ZippedFile() 
{ 
  mReadBytes = -1; 
  mZStream = new z_stream_s;
  memset(mZStream, 0, sizeof(z_stream_s));
}
//-----------------------------------------------------------------------------
ZippedFile::~ZippedFile() 
{ 
  close();
  mReadBytes = -1; 
  delete mZStream; mZStream = NULL;
}
//-----------------------------------------------------------------------------
const ZippedFileInfo* ZippedFile::zippedFileInfo() const { return mZippedFileInfo.get(); }
//-----------------------------------------------------------------------------
ZippedFileInfo* ZippedFile::zippedFileInfo() { return mZippedFileInfo.get(); }
//-----------------------------------------------------------------------------
void ZippedFile::setZippedFileInfo(ZippedFileInfo* info) { mZippedFileInfo = info; }
//-----------------------------------------------------------------------------
bool ZippedFile::exists() const
{
  return zippedFileInfo() && zippedFileInfo()->sourceZipFile() && zippedFileInfo()->zippedFileOffset();
}
//-----------------------------------------------------------------------------
bool ZippedFile::open(EOpenMode mode)
{
  if ( zippedFileInfo()->versionNeeded() > 20 )
  {
    Log::error("ZippedFile::open(): unsupported archive version.\n");
    return false;
  }

  /*if ( zippedFileInfo()->generalPurposeFlag() & 1 )
  {
    Log::error("ZippedFile::extract(): encription not supported.\n");
    return false;
  }*/

  if ( zippedFileInfo()->compressionMethod() != 8 && zippedFileInfo()->compressionMethod() != 0 )
  {
    Log::error("ZippedFile::open(): unsupported compression method.\n");
    return false;
  }

  if ( isOpen() )
  {
    Log::error("ZippedFile::open(): file already open.\n");
    return false;
  }

  if ( mode != OM_ReadOnly )
  {
    Log::error("ZippedFile::open(): only OM_ReadOnly mode is supported.\n");
    return false;
  }

  if ( !zippedFileInfo()->sourceZipFile() )
  {
    Log::error("ZippedFile::open(): no source zip stream defined.\n");
    return false;
  }

  if ( zippedFileInfo()->sourceZipFile()->isOpen() )
  {
    Log::error("ZippedFile::open(): source zip stream is already open. Only one ZippedFile at a time can read from the same source.\n");
    return false;
  }

  if ( !zippedFileInfo()->sourceZipFile()->open(mode) )
  {
    Log::error("ZippedFile::open(): could not open source zip stream.\n");
    return false;
  }

  if ( !zippedFileInfo()->sourceZipFile()->seekSet( zippedFileInfo()->zippedFileOffset() ) )
  {
    Log::error("ZippedFile::open(): error seeking beginning of compressed file.\n");
    zippedFileInfo()->sourceZipFile()->close();
    return false;
  }

  /* inflate state */
  mZStream->zalloc   = Z_NULL;
  mZStream->zfree    = Z_NULL;
  mZStream->opaque   = Z_NULL;
  mZStream->avail_in = 0;
  mZStream->next_in  = Z_NULL;
  if ( zippedFileInfo()->compressionMethod() == 8 && inflateInit2(mZStream, -15) != Z_OK )
  {
    zippedFileInfo()->sourceZipFile()->close();
    return false;
  }

  mReadBytes = 0;
  mUncompressedBufferPtr = 0;
  mUncompressedBuffer.clear();

  return true;
}
//-----------------------------------------------------------------------------
bool ZippedFile::isOpen() const { return mReadBytes != -1; }
//-----------------------------------------------------------------------------
void ZippedFile::close()
{
  if ( mZStream->next_in != Z_NULL )
    inflateEnd(mZStream);
  memset(mZStream, 0, sizeof(z_stream_s));
  mReadBytes = -1;
  if (zippedFileInfo() && zippedFileInfo()->sourceZipFile())
    zippedFileInfo()->sourceZipFile()->close();
  mUncompressedBufferPtr = 0;
  mUncompressedBuffer.clear();
}
//-----------------------------------------------------------------------------
long long ZippedFile::size() const
{
  if ( mZippedFileInfo )
    return mZippedFileInfo->uncompressedSize();
  else
    return 0;
}
//-----------------------------------------------------------------------------
void ZippedFile::resetStream()
{
  close();
  open(OM_ReadOnly);
}
//-----------------------------------------------------------------------------
bool ZippedFile::seekSet_Implementation(long long pos)
{
  if (pos<position())
    resetStream();

  unsigned char buffer[CHUNK_SIZE];
  long long remained = pos - position();
  long long eat_bytes = remained < CHUNK_SIZE ? remained : CHUNK_SIZE;
  while ( (remained -= read(buffer, eat_bytes)) )
    eat_bytes = remained < CHUNK_SIZE ? remained : CHUNK_SIZE;

  return position() == pos;
}
//-----------------------------------------------------------------------------
long long ZippedFile::read_Implementation(void* buffer, long long bytes_to_read)
{
  if ( bytes_to_read < 1 )
    return 0;

  if (!isOpen())
    return 0;

  if ( mReadBytes >= zippedFileInfo()->uncompressedSize() )
    return 0;

  if ( zippedFileInfo()->compressionMethod() == 0 )
  {
    long long bytes = zippedFileInfo()->uncompressedSize() - mReadBytes;
    bytes = bytes < bytes_to_read ? bytes : bytes_to_read;
    long long read_bytes = zippedFileInfo()->sourceZipFile()->read(buffer, bytes);
    mReadBytes += read_bytes;
    return read_bytes;
  }
  else
  if ( zippedFileInfo()->compressionMethod() == 8 )
  {

    long long read_bytes = 0;

    if ( mUncompressedBuffer.empty() )
      fillUncompressedBuffer();

    do
    {
      long long bytes = bytes_to_read < (int)mUncompressedBuffer.size()-mUncompressedBufferPtr ? bytes_to_read : (int)mUncompressedBuffer.size()-mUncompressedBufferPtr;
      // copy uncompressed data
      VL_CHECK( mUncompressedBufferPtr < (int)mUncompressedBuffer.size() )
      if (bytes)
        memcpy((char*)buffer+read_bytes, &mUncompressedBuffer[0]+mUncompressedBufferPtr, (size_t)bytes);

      mReadBytes += bytes;
      read_bytes += bytes;
      mUncompressedBufferPtr += (int)bytes;
      bytes_to_read -= bytes;
      // remove read data from the buffer
      // mUncompressedBuffer.erase( mUncompressedBuffer.begin(), mUncompressedBuffer.begin() + (size_t)bytes );
      if(mUncompressedBufferPtr == (int)mUncompressedBuffer.size())
      {
        mUncompressedBuffer.clear();
        mUncompressedBufferPtr = 0;
      }

    } while( bytes_to_read && fillUncompressedBuffer() );

    return read_bytes;
  }
  else
    return 0;
}
//-----------------------------------------------------------------------------
bool ZippedFile::extract(char* destination, bool check_sum)
{
  ZippedFileInfo* zfile_info = zippedFileInfo();

  if ( zfile_info->mUncompressedSize == 0 )
    return true;

  if (isOpen())
  {
    Log::error("ZippedFile::extract(): the file is already open.\n");
    return false;
  }

  if ( zfile_info->versionNeeded() > 20 )
  {
    Log::error("ZippedFile::extract(): unsupported archive version.\n");
    return false;
  }

  if ( zfile_info->generalPurposeFlag() & 1 )
  {
    Log::error("ZippedFile::extract(): encription not supported.\n");
    return false;
  }

  ref<VirtualFile> zip = zfile_info->sourceZipFile();

  if ( !zip )
  {
    Log::error("ZippedFile::extract(): no source zip stream defined.\n");
    return false;
  }

  if ( zip->isOpen() )
  {
    Log::error("ZippedFile::extract(): source zip stream is already open. Only one ZippedFile at a time can read from the same source.\n");
    return false;
  }

  if ( !zip->open(OM_ReadOnly) )
  {
    Log::error("ZippedFile::extract(): could not open source zip stream.\n");
    return false;
  }

  if ( !zip->seekSet( zfile_info->zippedFileOffset() ) )
  {
    Log::error("ZippedFile::extract(): not a seek-able zip stream.\n");
    zip->close();
    return false;
  }

  switch(zfile_info->mCompressionMethod)
  {
    default:
      Log::error("ZippedFile::extract(): unsupported compression method.\n");
      break;
    case 0: // store
    case 8: // deflate 32K
    {
      if (zfile_info->mCompressionMethod == 8)
        zdecompress( zip.get(), destination, zfile_info->mCompressedSize );
      else
      if (zfile_info->mCompressionMethod == 0)
        zip->read( destination, zfile_info->mUncompressedSize );

      if (check_sum)
      {
        CRC32CheckSum checksum;
        unsigned int crc = checksum.compute( destination, (int)zfile_info->mUncompressedSize );
        // printf("crc = 0x%08x | 0x%08x -> %s\n", crc, zfile_info->mCRC32, zfile_info->mCRC32 == crc ? "MATCH" : "ERROR!");
        VL_CHECK( zfile_info->mCRC32 == crc );
        if ( zfile_info->mCRC32 != crc )
        {
          zip->close();
          return false;
        }
      }
    }
  }

  zip->close();
  return true;
}
//-----------------------------------------------------------------------------
bool ZippedFile::fillUncompressedBuffer()
{
  VL_CHECK(mUncompressedBufferPtr == (int)mUncompressedBuffer.size())

  VL_CHECK( mUncompressedBuffer.empty() )

  mUncompressedBufferPtr = 0;

  if (!isOpen())
    return false;

  if ( mReadBytes >= zippedFileInfo()->uncompressedSize() )
    return false;

  int have = 0;
  int ret  = 0;

  unsigned int compressed_read_bytes = (int)(zippedFileInfo()->sourceZipFile()->position() - zippedFileInfo()->zippedFileOffset());

  int bytes_to_read = (unsigned)CHUNK_SIZE < (zippedFileInfo()->compressedSize() - compressed_read_bytes)?
                      (unsigned)CHUNK_SIZE : (zippedFileInfo()->compressedSize() - compressed_read_bytes);
  mZStream->avail_in = (uInt)zippedFileInfo()->sourceZipFile()->read(mZipBufferIn, bytes_to_read);

  if (mZStream->avail_in == 0)
    return false;
  mZStream->next_in = mZipBufferIn;

  do
  {
    mZStream->avail_out = CHUNK_SIZE;
    mZStream->next_out  = mZipBufferOut;

    ret = inflate(mZStream, Z_NO_FLUSH);
    VL_CHECK(ret != Z_STREAM_ERROR);
    switch (ret)
    {
    case Z_NEED_DICT:
    case Z_DATA_ERROR:
    case Z_MEM_ERROR:
        inflateEnd(mZStream);
        memset(mZStream, 0, sizeof(z_stream_s));
        Log::error("ZippedFile::read(): error reading zip stream.\n");
        return false;
    }

    have = CHUNK_SIZE - mZStream->avail_out;
    if (!have)
      break;
    int start = (int)mUncompressedBuffer.size();
    mUncompressedBuffer.resize(start + have);
    memcpy(&mUncompressedBuffer[0] + start, mZipBufferOut, have);
  }
  while ( mZStream->avail_out == 0 );

  return true;
}
//-----------------------------------------------------------------------------
ref<VirtualFile> ZippedFile::clone() const
{
  ref<ZippedFile> file = new ZippedFile;
  file->operator=(*this);
  return file;
}
//-----------------------------------------------------------------------------
