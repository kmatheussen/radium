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

#ifndef ZippedFile_INCLUDE_ONCE
#define ZippedFile_INCLUDE_ONCE

#include <vlCore/VirtualFile.hpp>
struct z_stream_s;

namespace vl
{
//---------------------------------------------------------------------------
// ZippedFileInfo
//---------------------------------------------------------------------------
  /**
   * Collects the information about a ZippedFile.
  */
  class ZippedFileInfo: public Object
  {
    friend class ZippedFile;
    friend class ZippedDirectory;

    VL_INSTRUMENT_CLASS(vl::ZippedFileInfo, Object)

  public:
    ZippedFileInfo()
    {
      mVersionNeeded = 0;
      mGeneralPurposeFlag = 0;
      mCompressionMethod = 0;
      mCRC32 = 0;
      mCompressedSize = 0;
      mUncompressedSize = 0;
      mFileNameLength = 0;
      mExtraFieldLength = 0;
      mSecond = 0;
      mMinute = 0;
      mHour = 0;
      mDay = 0;
      mMonth = 0;
      mYear = 0;
      mZippedFileOffset = 0;
    }

  public:
    unsigned short versionNeeded() const { return mVersionNeeded; }
    unsigned short generalPurposeFlag() const { return mGeneralPurposeFlag; }
    unsigned short compressionMethod() const { return mCompressionMethod; }
    unsigned int crc32() const { return mCRC32; }
    unsigned int compressedSize() const { return mCompressedSize; }
    unsigned int uncompressedSize() const { return mUncompressedSize; }
    unsigned short fileNameLength() const { return mFileNameLength; }
    unsigned short extraFieldLength() const { return mExtraFieldLength; }
    int second() const { return mSecond; }
    int minute() const { return mMinute; }
    int hour() const { return mHour; }
    int day() const { return mDay; }
    int month() const { return mMonth; }
    int year() const { return mYear; }
    const String& path() const { return mFileName; }
    // offset of the compressed data in the source zip file
    unsigned int zippedFileOffset() const { return mZippedFileOffset; }
    // source stream used to seek and read the compressed zip data
    const VirtualFile* sourceZipFile() const { return mSourceZipFile.get(); }
    VirtualFile* sourceZipFile() { return mSourceZipFile.get(); }
    void setSourceZipFile(VirtualFile* file) { mSourceZipFile = file; }

  public:
    unsigned short mVersionNeeded;
    unsigned short mGeneralPurposeFlag;
    unsigned short mCompressionMethod;
    unsigned int mCRC32;
    unsigned int mCompressedSize;
    unsigned int mUncompressedSize;
    unsigned short mFileNameLength;
    unsigned short mExtraFieldLength;
    int mSecond;
    int mMinute;
    int mHour;
    int mDay;
    int mMonth;
    int mYear;
    String mFileName;
    // offset of the compressed data in the zip file
    unsigned int mZippedFileOffset;
    // source stream used to seek and read the compressed zip data
    ref<VirtualFile> mSourceZipFile;
  };
//---------------------------------------------------------------------------
// ZippedFile
//---------------------------------------------------------------------------
  /**
   * A VirtualFile used to read a file contained in a .zip archive.
   *
   * \sa
   * - VirtualDirectory
   * - DiskDirectory
   * - MemoryDirectory
   * - ZippedDirectory
   * - FileSystem
   * - VirtualFile
   * - DiskFile
   * - MemoryFile
  */
  class VLCORE_EXPORT ZippedFile: public VirtualFile
  {
    VL_INSTRUMENT_CLASS(vl::ZippedFile, VirtualFile)

    // Lower this if you need to limit the amount of data allocated to the stack, for example to 16K.
    static const int CHUNK_SIZE = 128*1024;

  public:

    ZippedFile();
    ~ZippedFile();

    const ZippedFileInfo* zippedFileInfo() const;

    ZippedFileInfo* zippedFileInfo();

    void setZippedFileInfo(ZippedFileInfo* info);

    //! This returns true if zippedFileInfo() has been properly set up but does not check
    //! the existence of this file in the source zip file. To do so call ZippedDirectory::exists().
    virtual bool exists() const;

    virtual bool open(EOpenMode mode);

    virtual bool isOpen() const;

    virtual void close();

    virtual long long size() const;

    bool extract(char* destination, bool check_sum = true);

    ZippedFile& operator=(const ZippedFile& other) 
    { 
      close(); 
      super::operator=(other); 
      mZippedFileInfo = new ZippedFileInfo(*other.mZippedFileInfo); 
      if (mZippedFileInfo->sourceZipFile())
      {
        ref<VirtualFile> src_zip_copy = mZippedFileInfo->sourceZipFile()->clone();
        mZippedFileInfo->setSourceZipFile(src_zip_copy.get());
      }
      return *this; 
    }

    virtual ref<VirtualFile> clone() const;

    void resetStream();

  protected:
    virtual long long read_Implementation(void* buffer, long long bytes_to_read);

    virtual long long write_Implementation(const void* /*buffer*/, long long /*byte_count*/) { return 0; } // not supported yet

    virtual bool fillUncompressedBuffer();

    virtual long long position_Implementation() const { return mReadBytes; }

    virtual bool seekSet_Implementation(long long);

  protected:
    ref<ZippedFileInfo> mZippedFileInfo;
    long long mReadBytes;

    z_stream_s* mZStream;
    unsigned char mZipBufferIn[CHUNK_SIZE];
    unsigned char mZipBufferOut[CHUNK_SIZE];
    std::vector<char> mUncompressedBuffer;
    int mUncompressedBufferPtr;
  };
  //---------------------------------------------------------------------------
  // utilty functions
  bool compress(const void* data, size_t size, std::vector<unsigned char>& out, int level);
  bool decompress(const void* cdata, size_t csize, void* data_out);
  //---------------------------------------------------------------------------
}

#endif
