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

#include <vlCore/FileSystem.hpp>
#include <vlCore/DiskDirectory.hpp>
#include <vlCore/GlobalSettings.hpp>

using namespace vl;

//-----------------------------------------------------------------------------
/** 
If more than a file exists with the same name the VirtualFile returned is the one found
in the VirtualDirectory added the latest. This means that a VirtualDirectory overrides
the ones added before if duplicate file name exists.
If a file with the given name exists in the "." directory it will be located and returned immediately.

Visualization Library locates a file scanning the following paths (in the specified order):

- first split \p full_path into \p file_path and \p file_name

- look for the following file paths:
  -# ./file_path/file_name
  -# /fs/file_path/file_name
  -# ./alternate_path/file_path/file_name
  -# /fs/alternate_path/file_path/file_name
  -# ./file_name
  -# /fs/file_name
  -# ./alternate_path/file_name
  -# /fs/alternate_path/file_name

\p "/fs" stays for Visualization Library currently mounted FileSystems.
*/
ref<VirtualFile> FileSystem::locateFile(const String& full_path, const String& alternate_path) const
{
  std::vector<String> paths;
  paths.push_back(full_path);
  if (!alternate_path.empty()) 
    paths.push_back(alternate_path + '/' + full_path);
  if( full_path.extractFileName() != full_path )
  {
    paths.push_back(full_path.extractFileName());
    if (!alternate_path.empty()) 
      paths.push_back(alternate_path + '/' + full_path.extractFileName());
  }

  for(unsigned ipath=0; ipath<paths.size(); ++ipath)
  {
    paths[ipath].normalizeSlashes();

    // first look in the "." directory
    ref<DiskFile> disk_file = new DiskFile( paths[ipath] );
    if ( disk_file->exists() )
      return disk_file;

    // iterate backwards
    for( int idir=directories().size(); idir--; )
    {
      // returns the first one found
      ref<VirtualFile> file  = directories()[idir]->file( paths[ipath] );
      if (file)
        return file;
    }
  }

  return NULL;
}
//-----------------------------------------------------------------------------
ref<VirtualDirectory> FileSystem::locateDirectory(const String& name) const
{
  // first look in the "." directory
  ref<DiskDirectory> disk_directory = new DiskDirectory( name );
  if ( disk_directory->exists() )
    return disk_directory;

  // iterate backwards
  for( int idir=directories().size(); idir--; )
  {
    // returns the first one found
    ref<VirtualDirectory> dir = directories()[idir]->subDir(name);
    if (dir)
      return dir;
  }

  return NULL;
}
//-----------------------------------------------------------------------------
void FileSystem::listFilesRecursive(std::vector<String>& file_list ) const
{
  file_list.clear();
  std::vector<String> file_list_part;
  // iterate backwards
  for( int idir=directories().size(); idir--; )
  {
    directories()[idir]->listFilesRecursive(file_list_part);
    file_list.reserve( file_list.size() + file_list_part.size() );
    for(unsigned j=0; j<file_list_part.size(); ++j)
      file_list.push_back( file_list_part[j] );
  }
}
//-----------------------------------------------------------------------------
void FileSystem::listFilesRecursive(std::vector<String>& file_list, const String& match) const
{
  file_list.clear();
  std::vector<String> file_list_part;
  // iterate backwards
  for( int idir=directories().size(); idir--; )
  {
    directories()[idir]->listFilesRecursive(file_list_part, match);
    file_list.reserve( file_list.size() + file_list_part.size() );
    for(unsigned j=0; j<file_list_part.size(); ++j)
      file_list.push_back( file_list_part[j] );
  }
}
//-----------------------------------------------------------------------------
