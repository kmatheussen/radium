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

#ifndef GZipCodec_INCLUDE_ONCE
#define GZipCodec_INCLUDE_ONCE

#include <vlCore/VirtualFile.hpp>
struct z_stream_s;

namespace vl
{
  /**
   * The GZipCodec class is a VirtualFile that transparently encodes and decodes a stream of data using the GZip compression algorithm.
   */
  class VLCORE_EXPORT GZipCodec: public VirtualFile
  {
    VL_INSTRUMENT_CLASS(vl::GZipCodec, VirtualFile)

    // Lower this if you need to limit the amount of data allocated to the stack, for example to 16K.
    static const int CHUNK_SIZE = 128*1024;

  public:
    //! Constructor
    GZipCodec(VirtualFile* stream=NULL);

    //! Constructor
    GZipCodec(const String& gz_path);

    //! Destructor
    ~GZipCodec();

    //! Opens a compressed stream. 
    //! - If \p mode == \p OM_ReadOnly the stream will be decompressed during read operations. 
    //! - If \p mode == \p OM_WriteOnly the stream will be compressed during write operations.
    virtual bool open(EOpenMode mode);

    //! Returns true if the file is open for read or writing.
    virtual bool isOpen() const { return mReadBytes != -1; }

    //! Returns true if the input or output stream file exists.
    virtual bool exists() const { return mStream ? mStream->exists() : false; }

    //! Closes the GZipStream.
    virtual void close();

    //! Returns the uncompressed size of the stream. This is not a thread safe function.
    virtual long long size() const;

    //! Returns an equivalent GZipCodec.
    virtual ref<VirtualFile> clone() const;

    GZipCodec& operator=(const GZipCodec& other);

    //! Sets the compression level used during write operations. 
    //! \param level Values can be between 0 (faster compression) to 9 (slower but better compression).
    void setCompressionLevel(int level) { mCompressionLevel = level; }
    
    int compressionLevel() const { return mCompressionLevel; }

    //! Installs the VirtualFile representing the GZip file to be read or to be written.
    void setStream(VirtualFile* stream);
    
    //! Returns the VirtualFile representing the GZip file to be read or to be written.
    const VirtualFile* stream() const { return mStream.get(); }
    
    //! Returns the VirtualFile representing the GZip file to be read or to be written.
    VirtualFile* stream() { return mStream.get(); }

    /** 
     * Returns the uncompressed size of the stream.
     * \note This function needs to seek to the end of the GZip stream in order to read the uncompressed file size.
     */
    long long uncompressedSize();
    
    //! Returns the size of the compressed stream as returned by \p stream()->size().
    long long compressedSize() const { return stream() ? stream()->size() : -1; }
    
    //! Returns the compression ratio computed as \p compressedsize/uncompressedsize.
    float compressionRatio() const;

    bool warnOnSeek() const { return mWarnOnSeek; }
    
    void setWarnOnSeek(bool warn_on) { mWarnOnSeek = warn_on; }

  protected:
    virtual long long read_Implementation(void* buffer, long long bytes_to_read);
    virtual long long write_Implementation(const void* buffer, long long byte_count);
    virtual long long position_Implementation() const;
    void resetStream();
    bool seekSet_Implementation(long long pos);
    bool fillUncompressedBuffer();

  protected:
    int mCompressionLevel;
    ref<VirtualFile> mStream;
    long long mReadBytes;
    long long mWrittenBytes;
    bool mWarnOnSeek;

    z_stream_s* mZStream;
    unsigned char mZipBufferIn[CHUNK_SIZE];
    unsigned char mZipBufferOut[CHUNK_SIZE];
    std::vector<char> mUncompressedBuffer;
    int mUncompressedBufferPtr;
    long long mStreamSize;
    long long mUncompressedSize;
    enum { ZNone, ZCompress, ZDecompress } mMode;
  };
}

#endif
