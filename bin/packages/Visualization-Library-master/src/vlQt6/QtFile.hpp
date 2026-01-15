/**************************************************************************************/
/*                                                                                    */
/*  Visualization Library                                                             */
/*  http://visualizationlibrary.org                                                   */
/*                                                                                    */
/*  Copyright (c) 2005-2020, Michele Bosi                                             */
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

#ifndef QtFile_INCLUDE_ONCE
#define QtFile_INCLUDE_ONCE

#include <vlQt6/link_config.hpp>
#include <vlCore/VirtualFile.hpp>
#include <vlQt6/QtDirectory.hpp>
#include <QFile>

namespace vl
{
  class QtDirectory;
  //---------------------------------------------------------------------------
  // QtFile
  //---------------------------------------------------------------------------
  /**
   * A VirtualFile that uses Qt's QFile.
   *
   * \sa
   * - VirtualDirectory
   * - DiskDirectory
   * - QtDirectory
   * - MemoryDirectory
   * - ZippedDirectory
   * - FileSystem
   * - VirtualFile
   * - MemoryFile
   * - DiskFile
   * - ZippedFile
  */
  class VLQT6_EXPORT QtFile : public VirtualFile
  {
    VL_INSTRUMENT_CLASS(vl::QtFile, VirtualFile)

    friend class QtDirectory;

  protected:
    QtFile(const QtFile &other) : VirtualFile(other) {}

  public:
    QtFile(const String &path = String());

    ~QtFile();

    //! The specified path is relative to the parent directory. See setPhysicalPath().
    bool open(const String &path, EOpenMode mode);

    virtual bool open(EOpenMode mode);

    virtual bool isOpen() const;

    virtual void close();

    //! Returns the file size in bytes or -1 on error.
    virtual long long size() const;

    virtual bool exists() const;

    QtFile &operator=(const QtFile &other)
    {
      close();
      super::operator=(other);
      return *this;
    }

    virtual ref<VirtualFile> clone() const;

  protected:
    virtual long long read_Implementation(void *buffer, long long byte_count);

    virtual long long write_Implementation(const void *buffer, long long byte_count);

    virtual long long position_Implementation() const;

    virtual bool seekSet_Implementation(long long offset);

  protected:
    QFile mQFile;

  protected:
  };

} // namespace vl

#endif
