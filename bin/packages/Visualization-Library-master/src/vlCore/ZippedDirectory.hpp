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

#ifndef ZippedDirectory_INCLUDE_ONCE
#define ZippedDirectory_INCLUDE_ONCE

#include <vlCore/VirtualDirectory.hpp>
#include <vlCore/DiskFile.hpp>
#include <vlCore/ZippedFile.hpp>
#include <algorithm>

namespace vl
{
//---------------------------------------------------------------------------
// ZippedDirectory
//---------------------------------------------------------------------------
  /**
   * A VirtualDirectory capable of reading files from a .zip file.
   *
   * \sa
   * - VirtualDirectory
   * - DiskDirectory
   * - MemoryDirectory
   * - FileSystem
   * - VirtualFile
   * - DiskFile
   * - MemoryFile
   * - ZippedFile
  */
  class VLCORE_EXPORT ZippedDirectory: public VirtualDirectory
  {
    VL_INSTRUMENT_CLASS(vl::ZippedDirectory, VirtualDirectory)

  public:
    ZippedDirectory();

    ZippedDirectory(const String& zip_file);

    ZippedDirectory(VirtualFile* zip_file);

    bool setPath(const String& name);

    const VirtualFile* sourceZipFile() const;

    VirtualFile* sourceZipFile();

    void setSourceZipFile(VirtualFile* file);

    //! Sets the source zip file to NULL and disposes all the files contained in this directory.
    void reset();

    ref<VirtualFile> file(const String& name) const;

    //! Accepts absolute and relative paths
    ref<ZippedFile> zippedFile(const String& name) const;

    void listFilesRecursive(std::vector<String>& file_list ) const;

    int zippedFileCount() const;

    const ZippedFile* zippedFile(int index) const;

    ZippedFile* zippedFile(int index);

    void listSubDirs(std::vector<String>& dirs, bool append=false) const;
    ref<ZippedDirectory> zippedSubDir(const String& subdir_name) const;
    ref<VirtualDirectory> subDir(const String& subdir_name) const { return zippedSubDir(subdir_name); }

    void listFiles(std::vector<String>& file_list, bool append=false) const;

  bool isCorrupted();

  protected:
    bool init();

  protected:
    std::map< String, ref<ZippedFile> > mFiles;
    ref<VirtualFile> mSourceZipFile;
  };

}

#endif

