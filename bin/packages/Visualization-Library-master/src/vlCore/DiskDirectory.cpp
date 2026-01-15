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

#include <vlCore/DiskDirectory.hpp>
#include <vlCore/DiskFile.hpp>
#include <algorithm>

#if defined(__GNUG__)
  #include <sys/types.h>
  #include <dirent.h>
  #if defined(__APPLE__) || (__FreeBSD__)
    #define dirent64 dirent
    #define readdir64_r readdir_r
  #endif
#endif

using namespace vl;

//-----------------------------------------------------------------------------
// DiskDirectory
//-----------------------------------------------------------------------------
DiskDirectory::DiskDirectory( const String& name )
{
  setPath(name);
}
//-----------------------------------------------------------------------------
DiskDirectory::DiskDirectory()
{
}
//-----------------------------------------------------------------------------
void DiskDirectory::listFilesRecursive(std::vector<String>& file_list) const
{
  file_list.clear();
  listFilesRecursive_internal(file_list);
}
//-----------------------------------------------------------------------------
ref<DiskDirectory> DiskDirectory::diskSubDir(const String& subdir_name) const
{
  if (path().empty())
  {
    Log::error( "VirtualDirectory::path() must not be empty!\n" );
    return NULL;
  }

  std::vector<String> dir_list;
  String p = translatePath(subdir_name).right(-path().length()-1);
  this->listSubDirs(dir_list);
  String cur_p = path();
  for(int i=0; i<(int)dir_list.size(); ++i)
  {
    dir_list[i] = dir_list[i].right(-cur_p.length()-1);
    if (p.startsWith(dir_list[i]+'/') || p == dir_list[i])
    {
      ref<DiskDirectory> dir = new DiskDirectory(cur_p + '/' + dir_list[i]);
      if (!dir)
        return NULL;
      cur_p = cur_p + '/' + dir_list[i];
      p = p.right(-dir_list[i].length()-1);
      dir->listSubDirs(dir_list);
      i=-1;
      if (p.empty())
        return dir;
    }
  }
  return NULL;
}
//-----------------------------------------------------------------------------
bool DiskDirectory::exists() const
{
  if (path().empty())
  {
    Log::error( "VirtualDirectory::path() must not be empty!\n" );
    return false;
  }
  #if defined(VL_PLATFORM_WINDOWS)
    WIN32_FIND_DATA FindFileData;
    memset( &FindFileData, 0, sizeof(FindFileData) );
    String wild = path() + "*";
    HANDLE hFind = FindFirstFile( (wchar_t*)wild.ptr(), &FindFileData );
    if( hFind == INVALID_HANDLE_VALUE )
      return false;
    else
    {
      FindClose(hFind);
      return true;
    }
  #elif defined(__GNUG__)
    std::vector<unsigned char> utf8;
    path().toUTF8( utf8, false );
    DIR* dp = opendir((char*)&utf8[0]);
    if(dp == NULL)
      return false;
    else
    {
      closedir(dp);
      return true;
    }
  #endif
}
//-----------------------------------------------------------------------------
void DiskDirectory::listSubDirs(std::vector<String>& dirs, bool append) const
{
  if (!append)
    dirs.clear();
  if (path().empty())
  {
    Log::error( "VirtualDirectory::path() must not be empty!\n" );
    return;
  }
  #if defined(VL_PLATFORM_WINDOWS)
    WIN32_FIND_DATA FindFileData;
    memset( &FindFileData, 0, sizeof(FindFileData) );
    String wild = path() + "*";
    HANDLE hFind = FindFirstFile( (wchar_t*)wild.ptr(), &FindFileData );
    if( hFind == INVALID_HANDLE_VALUE )
      Log::error( Say("Cannot open directory '%s' for directory listing.\n") << path() );
    else
    {
      do
      {
        if ( FindFileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY )
        {
          String name;
          name = FindFileData.cFileName;
          if (name != L"." && name != L"..")
            dirs.push_back( path() + name + '/' );
        }
      } while( FindNextFile(hFind, &FindFileData) != 0 );

      DWORD dwError = GetLastError();
      FindClose(hFind);
      if (dwError != ERROR_NO_MORE_FILES)
      {
        // do nothing
        // printf ("FindNextFile error. Error code is %u\n", dwError);
      }
    }
  #elif defined(__GNUG__)
    std::vector<unsigned char> utf8;
    path().toUTF8( utf8, false );
    struct dirent64 dirp;
    struct dirent64* dirp2;
    DIR* dp = opendir((char*)&utf8[0]);
    if(dp != NULL)
    {
      while(readdir64_r(dp, &dirp, &dirp2) == 0 && dirp2 != NULL)
      {
        String name = dirp.d_name;
        if (name == ".." || name == ".")
          continue;

        name = path() + name + '/';
        // To discover the linked directories we try to open them instead of reading 'dirp.d_type'
        name.toUTF8( utf8, false );
        DIR* is_dir = opendir((char*)&utf8[0]);
        if (is_dir)
          closedir(is_dir);
        // 4  = directory
        // 8  = file
        // 10 = file or directory link
        // to discover directory links you can try to open them with opendir()
        // if (dirp.d_type == 4 )
        if (is_dir)
          dirs.push_back( name );
      }
      closedir(dp);
    }
    else
      Log::error( Say("Cannot open directory '%s' for directory listing.\n") << path() );
  #endif
}
//-----------------------------------------------------------------------------
void DiskDirectory::listFiles(std::vector< ref<DiskFile> >& file_list, bool append) const
{
  // fixme: test
  if (!append)
    file_list.clear();
  std::vector<String> file_names;
  listFiles(file_names,false);
  for(unsigned i=0; i<file_names.size(); ++i)
  {
    ref<DiskFile> file = new DiskFile;
    // file->setPath( path() + file_names[i] );
    file->setPath( file_names[i] );
    file_list.push_back(file);
  }
}
//-----------------------------------------------------------------------------
void DiskDirectory::listFiles(std::vector<String>& files, bool append) const
{
  if (!append)
    files.clear();
  if (path().empty())
  {
    Log::error( "VirtualDirectory::path() must not be empty!\n" );
    return;
  }
  #if defined(VL_PLATFORM_WINDOWS)
    WIN32_FIND_DATA FindFileData;
    memset( &FindFileData, 0, sizeof(FindFileData) );
    String wild = path() + "/*";
    HANDLE hFind = FindFirstFile( (wchar_t*)wild.ptr(), &FindFileData );
    if( hFind == INVALID_HANDLE_VALUE )
    {
      Log::error( Say("Cannot open directory '%s' for file listing.\n") << path() );
    }
    else
    {
      do
      {
        if ( (FindFileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) == 0 )
        {
          String name;
          name = FindFileData.cFileName;
          // the paths are relative
          files.push_back( path() + name );
        }
      } while( FindNextFile(hFind, &FindFileData) != 0 );

      DWORD dwError = GetLastError();
      FindClose(hFind);
      if (dwError != ERROR_NO_MORE_FILES)
      {
        // do nothing
        // printf ("FindNextFile error. Error is %u\n", dwError);
      }
    }
  #elif defined(__GNUG__)
    std::vector<unsigned char> utf8;
    path().toUTF8( utf8, false );
    struct dirent64 dirp;
    struct dirent64* dirp2;
    DIR* dp = opendir((char*)&utf8[0]);
    if(dp != NULL)
    {
      while(readdir64_r(dp, &dirp, &dirp2) == 0 && dirp2 != NULL)
      {
        String name = dirp.d_name;
        if (name == ".." || name == ".")
          continue;

        name = path() + name;
        // whatever is not a directory is a file for us
        name.toUTF8( utf8, false );
        DIR* is_dir = opendir((char*)&utf8[0]);
        if (is_dir)
          closedir(is_dir);
        // 4  = directory
        // 8  = file
        // 10 = file or directory link
        // to discover directory links you can try to open them with opendir()
        // if (dirp.d_type == 4 )
        if (!is_dir)
          files.push_back( name );
      }
      closedir(dp);
    }
    else
      Log::error( Say("Cannot open directory '%s' for file listing.\n") << path() );
  #endif
}
//-----------------------------------------------------------------------------
ref<VirtualFile> DiskDirectory::file(const String& name) const
{
  return diskFile(name);
}
//-----------------------------------------------------------------------------
ref<DiskFile> DiskDirectory::diskFile(const String& name) const
{
  String p = translatePath(name);
  ref<DiskFile> file = new DiskFile(p);
  if (file->exists())
    return file;
  else
    return NULL;
}
//-----------------------------------------------------------------------------
void DiskDirectory::listFilesRecursive_internal(std::vector<String>& file_list) const
{
  // add local child
  listFiles(file_list, true);
  // descend recursively
  std::vector<String> dir_list;
  listSubDirs(dir_list);
  for(unsigned i=0; i<dir_list.size(); ++i)
  {
    VL_CHECK(dir_list[i] != ".")
    VL_CHECK(dir_list[i] != "..")

    DiskDirectory sub_dir( dir_list[i] );
    sub_dir.listFilesRecursive_internal(file_list);
  }
}
//-----------------------------------------------------------------------------
