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

#include <vlCore/ZippedDirectory.hpp>
#include <vlCore/VisualizationLibrary.hpp>
#include <vlCore/FileSystem.hpp>
#include <set>

using namespace vl;
//-----------------------------------------------------------------------------
ZippedDirectory::ZippedDirectory() {}
//-----------------------------------------------------------------------------
ZippedDirectory::ZippedDirectory(const String& zip_file)
{
  ref<VirtualFile> v_file = defFileSystem()->locateFile(zip_file);
  if (v_file)
    setSourceZipFile(v_file.get());
  else
    Log::error( Say("ZippedDirectory() could not locate zip file '%s'.\n") << zip_file );
}
//-----------------------------------------------------------------------------
ZippedDirectory::ZippedDirectory(VirtualFile* zip_file)
{
  if (zip_file)
    setSourceZipFile(zip_file);
}
//-----------------------------------------------------------------------------
bool ZippedDirectory::setPath(const String& name)
{
  String root = name;
  root.trim();
  root.normalizeSlashes();
  if (root.empty())
  {
    Log::error("ZippedDirectory::setPath() given an empty path.\n");
    return false;
  }
  if (!root.endsWith('/'))
  {
    // Log::warning( Say("ZippedDirectory::setPath() : path (%s) must end with a '/'.\n") << root );
    root += '/';
  }

  std::map< String, ref<ZippedFile> > file_map;
  for( std::map< String, ref<ZippedFile> >::iterator it = mFiles.begin(); it != mFiles.end(); ++it )
  {
    String p = it->first;
    if ( !p.startsWith(path()) )
    {
      Log::warning( Say("ZippedDirectory::setPath() : invalid path file '%s'.\n") << p );
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
//-----------------------------------------------------------------------------
const VirtualFile* ZippedDirectory::sourceZipFile() const
{
  return mSourceZipFile.get();
}
//-----------------------------------------------------------------------------
VirtualFile* ZippedDirectory::sourceZipFile()
{
  return mSourceZipFile.get();
}
//-----------------------------------------------------------------------------
void ZippedDirectory::setSourceZipFile(VirtualFile* file)
{
  reset();
  mSourceZipFile = file;
  if(file)
    init();
}
//-----------------------------------------------------------------------------
void ZippedDirectory::reset()
{
  mSourceZipFile = NULL;
  mFiles.clear();
}
//-----------------------------------------------------------------------------
bool ZippedDirectory::init()
{
  mFiles.clear();
  if (path().empty())
  {
    Log::error( "VirtualDirectory::path() must not be empty!\n" );
    return false;
  }
  ref<VirtualFile> zip = sourceZipFile();
  if (!zip)
  {
    Log::error("ZippedFile::init() called but not zip file mounted.\n");
    return false;
  }
  zip->close();
  if ( !zip->open(OM_ReadOnly) )
  {
    Log::error("ZippedDirectory::init(): cannot open source zip file.\n");
    return false;
  }

  for( unsigned int local_file_header_signature = 0;
       zip->read(&local_file_header_signature, 4) && local_file_header_signature == 0x04034b50;
       local_file_header_signature = 0 )
  {
    // creates and fills a new ZippedFileInfo and a new ZippedFile
    ref<ZippedFileInfo> zfile_info = new ZippedFileInfo;
    zfile_info->setSourceZipFile( sourceZipFile()->clone().get() );

    unsigned short last_mod_file_time;
    unsigned short last_mod_file_date;

    zfile_info->mVersionNeeded = zip->readUInt16();
    zfile_info->mGeneralPurposeFlag = zip->readUInt16();
    zfile_info->mCompressionMethod = zip->readUInt16();
    last_mod_file_time = zip->readUInt16();
    last_mod_file_date = zip->readUInt16();
    zfile_info->mCRC32 = zip->readUInt32();
    zfile_info->mCompressedSize = zip->readUInt32();
    zfile_info->mUncompressedSize = zip->readUInt32();
    zfile_info->mFileNameLength = zip->readUInt16();
    zfile_info->mExtraFieldLength = zip->readUInt16();

    String name;

    // file name
    std::string file_name;
    file_name.resize(zfile_info->mFileNameLength);
    zip->read(&file_name[0], file_name.size());
    file_name.push_back(0);
    name = String::fromUTF8( file_name.c_str() );
    name.normalizeSlashes();

    // don't add directories
    if (name.endsWith('/'))
      continue;

    ref<ZippedFile> zipped_file = new ZippedFile;
    zipped_file->setZippedFileInfo( zfile_info.get() );

    zfile_info->mFileName = name;
    zipped_file->setPath( path() + name );
    mFiles[zipped_file->path()] = zipped_file;

    // extra field
    if ( zfile_info->mExtraFieldLength )
    {
      std::vector<char> extra_field;
      extra_field.resize(zfile_info->mExtraFieldLength);
      zip->read(&extra_field[0], extra_field.size());
    }

    // MS DOS Time              MS DOS Date
    // 0 - 4   5 - 10  11 - 15  16 - 20       21 - 24         25 - 31
    // second  minute  hour     day (1 - 31)  month (1 - 12)  years from 1980

    zfile_info->mSecond = int(( last_mod_file_time & 31 ) / 31.0f * 59.5f);
    zfile_info->mMinute = (last_mod_file_time>>5)  & 63;
    zfile_info->mHour   = (last_mod_file_time>>11) & 31;
    zfile_info->mDay    = last_mod_file_date       & 31;
    zfile_info->mMonth  = (last_mod_file_date>>5)  & 15;
    zfile_info->mYear   = ((last_mod_file_date>>9) & 127 ) + 1980;

    #if 0
      #if !defined(NDEBUG)
        printf("-------------------------\n");
        printf("%s\n", name.toStdString().c_str());
        printf("mVersionNeeded       = %d\n", zfile_info->mVersionNeeded);
        printf("mGeneralPurposeFlag  = %d\n", zfile_info->mGeneralPurposeFlag);
        printf("mCompressionMethod   = %d\n", zfile_info->mCompressionMethod);
        printf("Time and date        = %d/%d/%d %d:%d:%d\n", zfile_info->mYear, zfile_info->mMonth, zfile_info->mDay, zfile_info->mHour, zfile_info->mMinute, zfile_info->mSecond);
        printf("mUncompressedSize    = %d\n", zfile_info->mUncompressedSize);
        printf("mCompressedSize      = %d -> %.1f\n", zfile_info->mCompressedSize, 100.0f * zfile_info->mCompressedSize / zfile_info->mUncompressedSize);
        printf("mCRC32               = %08X\n", zfile_info->mCRC32);
      #endif
    #endif

    long long cur_pos = zip->position();

    // 2*4 + 4*4 is the length of a v2.0 zip header
    if (cur_pos < 2*4 + 4*4)
    {
      Log::error("ZippedDirectory::init(): mounted a non seek-able zip file.\n");
      zip->close();
      mFiles.clear();
      return false;
    }

    zfile_info->mZippedFileOffset = (unsigned int)cur_pos;

    // skip compressed data
    zip->seekCur( zfile_info->mCompressedSize );

    if ( zfile_info->mGeneralPurposeFlag & (1<<3) )
    {
      zfile_info->mCRC32 = zip->readUInt32();
      // sometimes the first 4 bytes are the header according to the specs... !!!
      if (zfile_info->mCRC32 == 0x08074b50)
        zfile_info->mCRC32 = zip->readUInt32();
      zfile_info->mCompressedSize = zip->readUInt32();
      zfile_info->mUncompressedSize = zip->readUInt32();
    }
  }

  if (zip->position() == 4)
  {
    Log::error( Say("ZippedDirectory::init(): '%s' does not look like a zip file.\n") << sourceZipFile()->path() );
    return false;
  }

  zip->close();
  return true;
}
//-----------------------------------------------------------------------------
ref<VirtualFile> ZippedDirectory::file(const String& name) const
{
  return zippedFile(name);
}
//-----------------------------------------------------------------------------
int ZippedDirectory::zippedFileCount() const
{
  return (int)mFiles.size();
}
//-----------------------------------------------------------------------------
const ZippedFile* ZippedDirectory::zippedFile(int index) const
{
  std::map< String, ref<ZippedFile> >::const_iterator it = mFiles.begin();
  for(int i=0; i<index && it != mFiles.end(); ++i)
    ++it;
  if ( it == mFiles.end() )
    return NULL;
  else
  	return it->second.get();
}
//-----------------------------------------------------------------------------
ZippedFile* ZippedDirectory::zippedFile(int index)
{
  std::map< String, ref<ZippedFile> >::iterator it = mFiles.begin();
  for(int i=0; i<index && it != mFiles.end(); ++i)
    ++it;
  if ( it == mFiles.end() )
    return NULL;
  else
  	return it->second.get();
}
//-----------------------------------------------------------------------------
ref<ZippedFile> ZippedDirectory::zippedFile(const String& name) const
{
  String p = translatePath(name);
  std::map< String, ref<ZippedFile> >::const_iterator it = mFiles.find(p);
  if (it == mFiles.end())
    return NULL;
  else
  {
    ref<ZippedFile> zip_file = new ZippedFile;
    zip_file->operator=(*it->second);
    return zip_file.get();
  }
}
//-----------------------------------------------------------------------------
void ZippedDirectory::listFilesRecursive( std::vector<String>& file_list ) const
{
  file_list.clear();
  if (path().empty())
  {
    Log::error( "VirtualDirectory::path() must not be empty!\n" );
    return;
  }
  for( std::map< String, ref<ZippedFile> >::const_iterator it = mFiles.begin(); it != mFiles.end();  ++it)
  {
    if (!it->first.startsWith(path()))
      vl::Log::warning( Say("ZippedFile '%s' does not belong to ZippedDirectory '%s'.\n") << it->first << path() );
    file_list.push_back( it->first );
  }
}
//-----------------------------------------------------------------------------
void ZippedDirectory::listSubDirs(std::vector<String>& dirs, bool append) const
{
  if (!append)
    dirs.clear();
  if (path().empty())
  {
    Log::error( "VirtualDirectory::path() must not be empty!\n" );
    return;
  }
  std::set<String> sub_dirs;
  for( std::map< String, ref<ZippedFile> >::const_iterator it = mFiles.begin(); it != mFiles.end(); ++it )
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
//-----------------------------------------------------------------------------
ref<ZippedDirectory> ZippedDirectory::zippedSubDir(const String& subdir_name) const
{
  if (path().empty())
  {
    Log::error( "VirtualDirectory::path() must not be empty!\n" );
    return NULL;
  }
  String p = translatePath(subdir_name);
  ref<ZippedDirectory> dir = new ZippedDirectory(p);
  for( std::map< String, ref<ZippedFile> >::const_iterator it = mFiles.begin(); it != mFiles.end(); ++it )
  {
    if (it->first.startsWith(p+'/'))
    {
      ref<ZippedFile> mfile = static_cast<ZippedFile*>(it->second->clone().get());
      VL_CHECK(mfile)
      dir->mFiles[mfile->path()] = mfile;
    }
  }

  if (dir->mFiles.empty())
    return NULL;
  else
    return dir;
}
//-----------------------------------------------------------------------------
void ZippedDirectory::listFiles(std::vector<String>& file_list, bool append) const
{
  if (!append)
    file_list.clear();
  if (path().empty())
  {
    Log::error( "VirtualDirectory::path() must not be empty!\n" );
    return;
  }
  for( std::map< String, ref<ZippedFile> >::const_iterator it = mFiles.begin(); it != mFiles.end(); ++it )
  {
    if (it->first.extractPath().left(-1) == path())
      file_list.push_back( it->first );
  }
}
//-----------------------------------------------------------------------------
bool ZippedDirectory::isCorrupted()
{
  if ( !init() )
    return true;
  for (int i=0; i<zippedFileCount(); ++i)
    if ( zippedFile(i)->crc32() != zippedFile(i)->zippedFileInfo()->crc32() )
      return true;
  return false;
}
//-----------------------------------------------------------------------------
