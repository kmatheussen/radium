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

#ifndef FileSystem_INCLUDE_ONCE
#define FileSystem_INCLUDE_ONCE

#include <vlCore/VirtualDirectory.hpp>
#include <vlCore/DiskFile.hpp>
#include <vector>

namespace vl
{
//-----------------------------------------------------------------------------
// FileSystem
//-----------------------------------------------------------------------------
  /**
   * Manages multiple VirtualDirectory objects.
   * Useful when you want to query more than one VirtualDirectory from a single point.
   *
   * \sa
   * - VirtualDirectory
   * - DiskDirectory
   * - MemoryDirectory
   * - ZippedDirectory
   * - VirtualFile
   * - DiskFile
   * - MemoryFile
   * - ZippedFile
  */
  class VLCORE_EXPORT FileSystem: public Object
  {
    VL_INSTRUMENT_CLASS(vl::FileSystem, Object)

  public:
    FileSystem() 
    { 
      VL_DEBUG_SET_OBJECT_NAME()
    }

    /** Looks for a VirtualFile on the disk and in the currently active FileSystem. */
    virtual ref<VirtualFile> locateFile(const String& full_path, const String& alternate_path=String()) const;

    virtual ref<VirtualDirectory> locateDirectory(const String& name) const;

    //! Returns the names of all the files contained in the previously added Directories.
    //! Files from different Directories but with the same name appear more than once in 'file_list'.
    //! Use carefully this function as it enumerates recursively all the files in the directory trees.
    virtual void listFilesRecursive(std::vector<String>& file_list ) const;

    //! Returns the names of all the files matching the given filter.
    //! Use carefully this function since it enumerates recursively all the files in the directory trees.
    //! The \p match parameter must be of the type \p "*abc" or \p "*abc*" or \p "abc*".
    virtual void listFilesRecursive(std::vector<String>& file_list, const String& match) const;

    //! Returns the list of VirtualDirectory objects added to a FileSystem
    std::vector< ref<VirtualDirectory> >& directories() { return mDirectories; }
    
    //! Returns the list of VirtualDirectory objects added to a FileSystem
    const std::vector< ref<VirtualDirectory> >& directories() const { return mDirectories; }

  protected:
    std::vector< ref<VirtualDirectory> > mDirectories;
  };

  //! Returns the default FileSystem used by VisualizationLibrary
  VLCORE_EXPORT FileSystem* defFileSystem();

  //! Sets the default FileSystem used by VisualizationLibrary
  VLCORE_EXPORT void setDefFileSystem(FileSystem* fs);
}

#endif
