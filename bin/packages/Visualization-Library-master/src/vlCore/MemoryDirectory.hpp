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

#ifndef MemoryDirectory_INCLUDE_ONCE
#define MemoryDirectory_INCLUDE_ONCE

#include <vlCore/VirtualDirectory.hpp>
#include <vlCore/MemoryFile.hpp>
#include <vlCore/String.hpp>
#include <map>
#include <set>

namespace vl
{
  /**
   * A VirtualDirectory to manipulate directories stored in memory.
   *
   * \sa
   * - VirtualDirectory
   * - DiskDirectory
   * - MemoryDirectory
   * - ZippedDirectory
   * - FileSystem
   * - VirtualFile
   * - DiskFile
   * - ZippedFile
  */
  class VLCORE_EXPORT MemoryDirectory: public VirtualDirectory
  {
    VL_INSTRUMENT_CLASS(vl::MemoryDirectory, VirtualDirectory)

  public:
    MemoryDirectory(const String& path="."): VirtualDirectory(path) {}

    virtual bool setPath(const String& name);

    //! The string file->path() must contain the full path including the MemoryDirectory's path()
    bool addFile(MemoryFile* file);

    //! The string file->path() must contain the full path including the MemoryDirectory's path()
    bool removeFile(MemoryFile* file);

    bool removeFile(const String& name);

    void eraseAllFiles() { mFiles.clear(); }

    virtual ref<VirtualFile> file(const String& name) const { return memoryFile(name); }

    ref<MemoryFile> memoryFile(const String& name) const;

    void listFilesRecursive( std::vector<String>& file_list ) const;

    void listSubDirs(std::vector<String>& dirs, bool append=false) const;

    ref<MemoryDirectory> memorySubDir(const String& subdir_name) const;

    ref<VirtualDirectory> subDir(const String& subdir_name) const { return memorySubDir(subdir_name); }

    void listFiles(std::vector<String>& file_list, bool append=false) const;

    //! Clones the content of another directory  (empty directories are never cloned).
    void clone(VirtualDirectory* directory, const String& match = "*");

  protected:
    std::map< String, ref<MemoryFile> > mFiles;
  };
};

#endif
