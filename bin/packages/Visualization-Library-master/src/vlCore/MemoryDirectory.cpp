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

#include <vlCore/MemoryDirectory.hpp>

using namespace vl;

//---------------------------------------------------------------------------
bool MemoryDirectory::setPath(const String& name)
{
  String root = name;
  root.trim();
  root.normalizeSlashes();
  if (root.empty())
  {
    Log::error("MemoryDirectory::setPath() given an empty path.\n");
    return false;
  }
  if (!root.endsWith('/'))
  {
    // Log::warning( Say("MemoryDirectory::setPath() : path (%s) must end with a '/'.\n") << root );
    root += '/';
  }

  std::map< String, ref<MemoryFile> > file_map;
  for( std::map< String, ref<MemoryFile> >::iterator it = mFiles.begin(); it != mFiles.end(); ++it )
  {
    String p = it->first;
    if ( !p.startsWith(path()) )
    {
      Log::warning( Say("MemoryDirectory::setPath() : invalid path file '%s'.\n") << p );
      continue;
    }
    p = p.right(-path().length());
    it->second->setPath(root + p);
    file_map[it->second->path()] = it->second;
  }
  mFiles = file_map;
  mPath = root;
  return true;
}
//---------------------------------------------------------------------------
bool MemoryDirectory::addFile(MemoryFile* file) 
{ 
  if (path().empty())
  {
    Log::error( "VirtualDirectory::path() must not be empty!\n" );
    return false;
  }

  if ( !file->path().startsWith(path()) )
  {
    Log::error( Say("File '%s' does not belong to MemoryDirectory '%s'\n") << file->path() << path() );
    return false;
  }

  String p = file->path();
  p.normalizeSlashes();
  file->setPath( p );

  mFiles[file->path()] = file;
  return true;
}
//---------------------------------------------------------------------------
bool MemoryDirectory::removeFile(MemoryFile* file) 
{ 
  return removeFile( file->path() );
}
//---------------------------------------------------------------------------
bool MemoryDirectory::removeFile(const String& name)
{ 
  bool ok = mFiles.find( name ) != mFiles.end();
  mFiles.erase( name ); 
  return ok;
}
//---------------------------------------------------------------------------
ref<MemoryFile> MemoryDirectory::memoryFile(const String& name) const
{
  String p = translatePath(name);
  std::map< String, ref<MemoryFile> >::const_iterator it = mFiles.find(p);
  if (it == mFiles.end())
    return NULL;
  else
  {
    ref<MemoryFile> mem_file = new MemoryFile;
    mem_file->operator=(*it->second);
    return mem_file.get();
  }
}
//---------------------------------------------------------------------------
void MemoryDirectory::listFilesRecursive( std::vector<String>& file_list ) const
{
  file_list.clear();
  if (path().empty())
  {
    Log::error( "VirtualDirectory::path() must not be empty!\n" );
    return;
  }
  for( std::map< String, ref<MemoryFile> >::const_iterator it = mFiles.begin(); it != mFiles.end();  ++it)
  {
    if (!it->first.startsWith(path()))
      vl::Log::warning( Say("MemoryFile '%s' does not belong to MemoryDirectory '%s'.\n") << it->first << path() );
    file_list.push_back( it->first );
  }
}
//---------------------------------------------------------------------------
void MemoryDirectory::listSubDirs(std::vector<String>& dirs, bool append) const
{
  if (!append)
    dirs.clear();
  if (path().empty())
  {
    Log::error( "VirtualDirectory::path() must not be empty!\n" );
    return;
  }
  std::set<String> sub_dirs;
  for( std::map< String, ref<MemoryFile> >::const_iterator it = mFiles.begin(); it != mFiles.end(); ++it )
  {
    VL_CHECK(it->first.startsWith(path()))
    String p = it->first.extractPath();
    if (path().length())
      p = p.right(-path().length());
    while(p.startsWith('/'))
      p = p.right(-1);
    String drive_letter;
    if (p.length()>3 && p[1] == ':' && p[2] == '/')
    {
      drive_letter = p.left(3);
      p = p.right(-3);
    }
    if (p.empty()) // is a file
      continue;
    std::vector<String> tokens;
    p.split('/',tokens,true);
    if (tokens.size())
      sub_dirs.insert(path() + tokens[0]);
  }
  for(std::set<String>::const_iterator it = sub_dirs.begin(); it != sub_dirs.end(); ++it)
    dirs.push_back(*it);
}
//---------------------------------------------------------------------------
ref<MemoryDirectory> MemoryDirectory::memorySubDir(const String& subdir_name) const
{
  String p = translatePath(subdir_name);
  if (path().empty())
  {
    Log::error( "VirtualDirectory::path() must not be empty!\n" );
    return NULL;
  }
  ref<MemoryDirectory> dir = new MemoryDirectory(p);
  for( std::map< String, ref<MemoryFile> >::const_iterator it = mFiles.begin(); it != mFiles.end(); ++it )
  {
    if (it->first.startsWith(p+'/'))
    {
      ref<MemoryFile> mfile = static_cast<MemoryFile*>(it->second->clone().get());
      VL_CHECK(mfile)
      dir->mFiles[mfile->path()] = mfile;
    }
  }

  if (dir->mFiles.empty())
    return NULL;
  else
    return dir;
}
//---------------------------------------------------------------------------
void MemoryDirectory::listFiles(std::vector<String>& file_list, bool append) const
{
  if (!append)
    file_list.clear();
  if (path().empty())
  {
    Log::error( "VirtualDirectory::path() must not be empty!\n" );
    return;
  }
  for( std::map< String, ref<MemoryFile> >::const_iterator it = mFiles.begin(); it != mFiles.end(); ++it )
  {
    if (it->first.extractPath().left(-1) == path())
      file_list.push_back( it->first );
  }
}
//---------------------------------------------------------------------------
void MemoryDirectory::clone(VirtualDirectory* directory, const String& match)
{
  setPath(directory->path());
  eraseAllFiles();
  std::vector<String> file_list;
  directory->listFilesRecursive(file_list, match);
  for(unsigned i=0; i<file_list.size(); ++i)
  {
    ref<VirtualFile> file = directory->file( file_list[i] );
    if (file)
    {
      ref<MemoryFile> mem_file = new MemoryFile;
      mem_file->copy(file.get());
      mem_file->setPath( file->path() );
      addFile(mem_file.get());
    }
  }
}
//---------------------------------------------------------------------------
