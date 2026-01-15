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

#include <vlCore/VisualizationLibrary.hpp>
#include <vlCore/GZipCodec.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>
#include <zlib.h>

using namespace vl;

//-----------------------------------------------------------------------------
// GZipCodec
//-----------------------------------------------------------------------------
GZipCodec::GZipCodec(VirtualFile* stream): mStream(stream) 
{
  mCompressionLevel = 6;
  mReadBytes = -1; 
  mWrittenBytes = -1;
  mZStream = new z_stream_s;
  memset(mZStream, 0, sizeof(z_stream_s));
  mUncompressedSize = -1;
  mWarnOnSeek = true;
}
//-----------------------------------------------------------------------------
GZipCodec::GZipCodec(const String& gz_path): mStream(NULL) 
{
  mCompressionLevel = 6;
  mReadBytes = -1; 
  mWrittenBytes = -1;
  mZStream = new z_stream_s;
  memset(mZStream, 0, sizeof(z_stream_s));
  mUncompressedSize = -1;
  setPath(gz_path);
  mWarnOnSeek = true;
}
//-----------------------------------------------------------------------------
GZipCodec::~GZipCodec() 
{ 
  close(); 
  delete mZStream; mZStream = NULL; 
  mUncompressedSize = -1;
  mWrittenBytes = -1;
}
//-----------------------------------------------------------------------------
bool GZipCodec::open(EOpenMode mode)
{
  if (isOpen())
  {
    Log::error("GZipCodec::open(): file already open.\n");
    return false;
  }
  switch(mode)
  {
    case OM_ReadOnly:  mMode = ZDecompress; break;
    case OM_WriteOnly: mMode = ZCompress;   break;
    default: mMode = ZNone;
  }
  mReadBytes = 0;
  mWrittenBytes = 0;
  mUncompressedBufferPtr = 0;
  mUncompressedBuffer.clear();
  /* z_stream_s */
  memset(mZStream, 0, sizeof(z_stream_s));
  mZStream->zalloc   = Z_NULL;
  mZStream->zfree    = Z_NULL;
  mZStream->opaque   = Z_NULL;
  mZStream->avail_in = 0;
  mZStream->next_in  = Z_NULL;
  if (mMode == ZDecompress)
  {
    if ( inflateInit2(mZStream, 15+32/*autodected gzip header*/) != Z_OK )
    {
      Log::error("GZipCodec::open(): inflateInit2 failed.\n");
      return false;
    }
  }
  else
  if (mMode == ZCompress)
  {
    if (deflateInit2(mZStream, compressionLevel(), Z_DEFLATED, 15+16/*gz compression*/, 8/*mem level*/, Z_DEFAULT_STRATEGY) != Z_OK)
    {
      Log::error("GZipCodec::open(): deflateInit2 failed.\n");
      return false;
    }
  }
  if (!stream() && !path().empty())
  {
    ref<VirtualFile> file = vl::locateFile(path());
    setStream(file.get());
  }
  if (!stream())
  {
    Log::error("GZipCodec::open(): no input or output stream defined. See the setStream() method documentation.\n");
    return false;
  }
  if (!stream()->open(mode))
  {
    Log::error("GZipCodec::open(): input or output stream open failed.\n");
    return false;
  }
  mStreamSize = stream()->size();
  return true;
}
//-----------------------------------------------------------------------------
void GZipCodec::close()
{
  if ( mMode == ZDecompress )
    inflateEnd(mZStream);
  else
  if ( mMode == ZCompress )
  {
    // flush data
    unsigned char next_out[CHUNK_SIZE];
    unsigned char dummy_buffer=0;
    mZStream->avail_in = 0;
    mZStream->next_in  = (Bytef*)&dummy_buffer;
    do
    {
      mZStream->avail_out = CHUNK_SIZE;
      mZStream->next_out  = next_out;
      int ret = deflate(mZStream, Z_FINISH);
      if (ret == Z_STREAM_ERROR)
      {
        Log::error("GZStream::write(): Z_STREAM_ERROR.\n");
        deflateEnd(mZStream);
        memset(mZStream, 0, sizeof(z_stream_s));
        break;
      }
      unsigned have = CHUNK_SIZE - mZStream->avail_out;
      if (have>0) 
      {
        long long written = stream()->write(next_out, have);
        if (written < have)
          Log::error("GZStream: write failed.\n");
      }
    } while (mZStream->avail_out == 0);
    deflateEnd(mZStream);
  }
  if (stream())
    stream()->close();
  memset(mZStream, 0, sizeof(z_stream_s));
  mMode = ZNone;
  mReadBytes = -1;
  mWrittenBytes = -1;
  mUncompressedBufferPtr = 0;
  mUncompressedBuffer.clear();
}
//-----------------------------------------------------------------------------
ref<VirtualFile> GZipCodec::clone() const
{
  ref<GZipCodec> file = new GZipCodec;
  file->operator=(*this);
  return file;
}
//-----------------------------------------------------------------------------
GZipCodec& GZipCodec::operator=(const GZipCodec& other) 
{ 
  close(); 
  super::operator=(other); 
  mCompressionLevel = other.mCompressionLevel;
  if (other.mStream)
    mStream = other.mStream->clone();
  return *this; 
}
//-----------------------------------------------------------------------------
long long GZipCodec::read_Implementation(void* buffer, long long bytes_to_read)
{
  if ( bytes_to_read < 1 )
    return 0;
  if (!isOpen())
  {
    Log::error("GZStream::read(): read requested on closed stream.\n");
    return 0;
  }
  /*if ( mReadBytes >= ??? )
    return 0;*/
  long long read_bytes = 0;
  if ( mUncompressedBuffer.empty() )
    fillUncompressedBuffer();
  if ( mUncompressedBuffer.empty() )
    return 0;
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
  } while( bytes_to_read && fillUncompressedBuffer() && !mUncompressedBuffer.empty() );
  return read_bytes;
}
//-----------------------------------------------------------------------------
long long GZipCodec::write_Implementation(const void* buffer, long long byte_count)
{
  unsigned char next_out[CHUNK_SIZE];
  mZStream->avail_in = (uInt)byte_count;
  mZStream->next_in  = (Bytef*)buffer;
  do
  {
    mZStream->avail_out = CHUNK_SIZE;
    mZStream->next_out  = next_out;
    int ret = deflate(mZStream, Z_NO_FLUSH);
    if (ret == Z_STREAM_ERROR)
    {
      Log::error("GZStream::write(): Z_STREAM_ERROR.\n");
      return 0;
    }
    unsigned have = CHUNK_SIZE - mZStream->avail_out;
    if (have>0) 
    {
      long long written = stream()->write(next_out, have);
      if (written < have)
        Log::error("GZStream: write failed.\n");
    }
  } while (mZStream->avail_out == 0);
  mWrittenBytes += byte_count;
  return byte_count;
}
//-----------------------------------------------------------------------------
long long GZipCodec::position_Implementation() const
{
  return mReadBytes;
}
//-----------------------------------------------------------------------------
void GZipCodec::resetStream()
{
  close();
  open(OM_ReadOnly);
}
//-----------------------------------------------------------------------------
bool GZipCodec::seekSet_Implementation(long long pos)
{
  if (mMode == ZDecompress)
  {
    if (warnOnSeek())
      Log::print( Say("Performance warning: GZipCodec::seek() requested for file %s. For maximum performances avoid seeking a GZipCodec, especially avoid seeking backwards.\n") << path() );

    if (pos<position())
      resetStream();

    unsigned char buffer[CHUNK_SIZE];
    long long remained = pos - position();
    long long eat_bytes = remained < CHUNK_SIZE ? remained : CHUNK_SIZE;
    while ( (remained -= read(buffer, eat_bytes)) )
      eat_bytes = remained < CHUNK_SIZE ? remained : CHUNK_SIZE;
    return position() == pos;
  }
  else
  {
    Log::error("GZStream: seek supported only by OM_ReadOnly open mode.\n");
    return false;
  }
}
//-----------------------------------------------------------------------------
bool GZipCodec::fillUncompressedBuffer()
{
  VL_CHECK(mUncompressedBufferPtr == (int)mUncompressedBuffer.size())
  VL_CHECK( mUncompressedBuffer.empty() )
  mUncompressedBufferPtr = 0;
  VL_CHECK(isOpen())
  /*if (!isOpen())
    return false;*/
  /*if ( mReadBytes >= ??? )
    return false;*/
  int have = 0;
  int ret  = 0;
  /*long long compressed_read_bytes = stream()->position();*/
  long long bytes_to_read = CHUNK_SIZE < (mStreamSize - stream()->position())?
                            CHUNK_SIZE : (mStreamSize - stream()->position());
  mZStream->avail_in = (uInt)stream()->read(mZipBufferIn, bytes_to_read);
  if (mZStream->avail_in == 0)
    return true;
  mZStream->next_in = mZipBufferIn;
  do
  {
    mZStream->avail_out = CHUNK_SIZE;
    mZStream->next_out  = mZipBufferOut;
    ret = inflate(mZStream, Z_NO_FLUSH);
    switch (ret)
    {
    case Z_STREAM_ERROR:
    case Z_NEED_DICT:
    case Z_DATA_ERROR:
    case Z_MEM_ERROR:
        close();
        Log::error("GZStream: error reading gzip stream.\n");
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
long long GZipCodec::uncompressedSize()
{
  if (mMode == ZDecompress || mMode == ZNone)
  {
    if (stream() && mUncompressedSize == -1)
    {
      if (stream()->isOpen())
      {
        long long pos = stream()->position();
        stream()->seekEnd(-4);
        mUncompressedSize = stream()->readUInt32();
        stream()->seekSet(pos);
      }
      else
      {
        stream()->open(OM_ReadOnly);
        stream()->seekEnd(-4);
        mUncompressedSize = stream()->readUInt32();
        stream()->close();
      }
    }
    return mUncompressedSize;
  }
  else
    return -1;
}
//-----------------------------------------------------------------------------
long long GZipCodec::size() const 
{ 
  if (mMode == ZCompress)
    return mWrittenBytes;
  else
    return const_cast<GZipCodec*>(this)->uncompressedSize();
}
//-----------------------------------------------------------------------------
void GZipCodec::setStream(VirtualFile* str) 
{ 
  if (stream() && stream()->isOpen()) 
    stream()->close(); 
  mStream = str; 
  mUncompressedSize = -1; 
  mWrittenBytes = -1; 
  setPath( str ? str->path() : String() );
}
//-----------------------------------------------------------------------------
float GZipCodec::compressionRatio() const
{
  double comp = (double)compressedSize();
  double unco = (double)size();
  return float(comp/unco);
}
//-----------------------------------------------------------------------------
