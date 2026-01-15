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

#ifndef VirtualDirectory_INCUDE_ONCE
#define VirtualDirectory_INCUDE_ONCE

#include <vlCore/Object.hpp>
#include <vlCore/String.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>
#include <vlCore/VirtualFile.hpp>

namespace vl
{
//---------------------------------------------------------------------------
// VirtualDirectory
//---------------------------------------------------------------------------
  /** Abstact class representing a directory of files.  
  \sa
  - DiskDirectory
  - MemoryDirectory
  - ZippedDirectory
  - FileSystem
  - VirtualFile
  - DiskFile
  - MemoryFile
  - ZippedFile
  */
  class VLCORE_EXPORT VirtualDirectory: public Object
  {
    VL_INSTRUMENT_ABSTRACT_CLASS(vl::VirtualDirectory, Object)

  public:
    //! Constructor
    VirtualDirectory(): mPath("/") {}

    //! Constructor
    //! \param path Defines the path name of the virtual dirctory, must be a non empty string.
    VirtualDirectory( const String& path ): mPath(path) {}

    //! Changes the \p path \p name of a VirtualDirectory. Must not be an empty string.
    virtual bool setPath(const String& path);

    virtual const String& path() const { return mPath; }

    //! Checks the existence of a file in the directory
    bool fileExists(const String& name) const { return file(name).get() != NULL; }

    //! Returns the VirtualFile with the given name if any, NULL otherwise.
    virtual ref<VirtualFile> file(const String& name) const = 0;

    //! Returns the list of files contained in the VirtualDirectory.
    //! If there are subdirectories the files will be searched recursively.
    virtual void listFilesRecursive(std::vector<String>& file_list) const = 0;

    //! Returns the list of files contained in the VirtualDirectory that match the expression 'match'.
    //! The \p match parameter must be of the type \p "*abc" or \p "*abc*" or \p "abc*" or \p "*".
    virtual void listFilesRecursive(std::vector<String>& file_list, const String& match) const
    {
      listFilesRecursive(file_list);
      if (match != "*")
        String::filterStrings(file_list, match);
    }

    virtual void listFiles(std::vector<String>& file_list, bool append=false) const = 0;

    virtual void listSubDirs(std::vector<String>& dirs, bool append=false) const = 0;
    virtual ref<VirtualDirectory> subDir(const String& subdir_name) const  = 0;

  protected:
    String translatePath(const String& p) const;

  protected:
    String mPath;
  };
}

#endif
