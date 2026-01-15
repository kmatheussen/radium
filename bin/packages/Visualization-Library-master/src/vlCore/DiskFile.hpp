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

#ifndef DiskFile_INCLUDE_ONCE
#define DiskFile_INCLUDE_ONCE

#include <vlCore/VirtualFile.hpp>
#include <vlCore/DiskDirectory.hpp>

#if defined(VL_PLATFORM_LINUX) || defined(VL_PLATFORM_MACOSX)
  #include <sys/types.h>
  #include <sys/stat.h>
  #include <unistd.h>
#endif

namespace vl
{
  class DiskDirectory;
//---------------------------------------------------------------------------
// DiskFile
//---------------------------------------------------------------------------
  /**
   * A VirtualFile that operates on regular disk files.
   *
   * \sa
   * - VirtualDirectory
   * - DiskDirectory
   * - MemoryDirectory
   * - ZippedDirectory
   * - FileSystem
   * - VirtualFile
   * - MemoryFile
   * - ZippedFile
  */
  class VLCORE_EXPORT DiskFile: public VirtualFile
  {
    VL_INSTRUMENT_CLASS(vl::DiskFile, VirtualFile)

    friend class DiskDirectory;
  protected:
    DiskFile(const DiskFile& other): VirtualFile(other) {}

  public:
    DiskFile(const String& path = String());

    ~DiskFile();

    //! The specified path is relative to the parent directory. See setPhysicalPath().
    bool open(const String& path, EOpenMode mode);

    virtual bool open(EOpenMode mode);

    virtual bool isOpen() const;

    virtual void close();

    //! Returns the file size in bytes or -1 on error.
    virtual long long size() const;

    virtual bool exists() const;

    DiskFile& operator=(const DiskFile& other) { close(); super::operator=(other); return *this; }

    virtual ref<VirtualFile> clone() const;

  protected:
    virtual long long read_Implementation(void* buffer, long long byte_count);

    virtual long long write_Implementation(const void* buffer, long long byte_count);

    virtual long long position_Implementation() const;

    virtual bool seekSet_Implementation(long long offset);

    ref<DiskDirectory> parentDir() const;

  protected:
    #if defined(VL_PLATFORM_WINDOWS)
      HANDLE mHandle;
    #else
      FILE*  mHandle;
    #endif

  protected:
  };

}

#endif
