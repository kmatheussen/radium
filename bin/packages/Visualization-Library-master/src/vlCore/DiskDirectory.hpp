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

#ifndef DiskDirectory_INCLUDE_ONCE
#define DiskDirectory_INCLUDE_ONCE

#include <vlCore/VirtualDirectory.hpp>

namespace vl
{
  class DiskFile;
//---------------------------------------------------------------------------
// DiskDirectory
//---------------------------------------------------------------------------
  /**
   * A VirtualDirectory that operates on reguar disk directories.
   *
   * \sa
   * - MemoryDirectory
   * - ZippedDirectory
   * - FileSystem
   * - VirtualFile
   * - DiskFile
   * - MemoryFile
   * - ZippedFile
  */
  class VLCORE_EXPORT DiskDirectory: public VirtualDirectory
  {
    VL_INSTRUMENT_CLASS(vl::DiskDirectory, VirtualDirectory)

  public:
    DiskDirectory();

    DiskDirectory( const String& path );

    //! Use carefully this function, since this search the whole given file system tree.
    void listFilesRecursive(std::vector<String>& file_list) const;

    void listFiles(std::vector<String>& file_list, bool append=false) const;
    
    void listFiles(std::vector< ref<DiskFile> >& file_list, bool append=false) const;
    
    void listSubDirs(std::vector<String>& dirs, bool append=false) const;
    
    ref<DiskDirectory> diskSubDir(const String& subdir_name) const;
    
    ref<VirtualDirectory> subDir(const String& subdir_name) const { return diskSubDir(subdir_name); }

    virtual ref<VirtualFile> file(const String& name) const;

    virtual ref<DiskFile> diskFile(const String& name) const;

    bool exists() const;

  protected:
    void listFilesRecursive_internal(std::vector<String>& file_list) const;
  };
}

#endif
