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

#include <vlQt6/QtDirectory.hpp>
#include <vlQt6/QtFile.hpp>
#include <algorithm>
#include <QDir>

using namespace vl;

//-----------------------------------------------------------------------------
// QtDirectory
//-----------------------------------------------------------------------------
QtDirectory::QtDirectory(const String &name)
{
  setPath(name);
}
//-----------------------------------------------------------------------------
QtDirectory::QtDirectory()
{
}
//-----------------------------------------------------------------------------
void QtDirectory::listFilesRecursive(std::vector<String> &file_list) const
{
  file_list.clear();
  listFilesRecursive_internal(file_list);
}
//-----------------------------------------------------------------------------
ref<QtDirectory> QtDirectory::qtSubDir(const String &subdir_name) const
{
  if (path().empty())
  {
    Log::error("QtDirectory::path() must not be empty!\n");
    return NULL;
  }

  std::vector<String> dir_list;
  String p = translatePath(subdir_name).right(-path().length() - 1);
  this->listSubDirs(dir_list);
  String cur_p = path();
  for (int i = 0; i < (int)dir_list.size(); ++i)
  {
    dir_list[i] = dir_list[i].right(-cur_p.length() - 1);
    if (p.startsWith(dir_list[i] + '/') || p == dir_list[i])
    {
      ref<QtDirectory> dir = new QtDirectory(cur_p + '/' + dir_list[i]);
      if (!dir)
        return NULL;
      cur_p = cur_p + '/' + dir_list[i];
      p = p.right(-dir_list[i].length() - 1);
      dir->listSubDirs(dir_list);
      i = -1;
      if (p.empty())
        return dir;
    }
  }
  return NULL;
}
//-----------------------------------------------------------------------------
bool QtDirectory::exists() const
{
  if (path().empty())
  {
    Log::error("QtDirectory::path() must not be empty!\n");
    return false;
  }
  return QDir(QString(path().toStdString().c_str())).exists();
}
//-----------------------------------------------------------------------------
void QtDirectory::listSubDirs(std::vector<String> &dirs_out, bool append) const
{
  if (!append)
  {
    dirs_out.clear();
  }

  if (path().empty())
  {
    Log::error("QtDirectory::path() must not be empty!\n");
    return;
  }

  const char *p = path().toStdString().c_str();
  QStringList subdirs = QDir(QString(p)).entryList(QDir::Dirs | QDir::NoDotAndDotDot);

  for (int i = 0; i < subdirs.size(); ++i)
  {
    vl::String name = vl::String::fromStdString(subdirs.at(i).toStdString());
    dirs_out.push_back(path() + name + '/');
  }
}
//-----------------------------------------------------------------------------
void QtDirectory::listFiles(std::vector<ref<QtFile>> &file_list, bool append) const
{
  if (!append)
    file_list.clear();
  std::vector<String> file_names;
  listFiles(file_names, false);
  for (unsigned i = 0; i < file_names.size(); ++i)
  {
    ref<QtFile> file = new QtFile;
    // file->setPath( path() + file_names[i] );
    file->setPath(file_names[i]);
    file_list.push_back(file);
  }
}
//-----------------------------------------------------------------------------
void QtDirectory::listFiles(std::vector<String> &files_out, bool append) const
{
  if (!append)
  {
    files_out.clear();
  }

  if (path().empty())
  {
    Log::error("QtDirectory::path() must not be empty!\n");
    return;
  }

  const char *p = path().toStdString().c_str();
  QStringList files = QDir(QString(p)).entryList(QDir::Files);

  for (int i = 0; i < files.size(); ++i)
  {
    vl::String name = vl::String::fromStdString(files.at(i).toStdString());
    files_out.push_back(path() + name + '/');
  }
}
//-----------------------------------------------------------------------------
ref<VirtualFile> QtDirectory::file(const String &name) const
{
  return qtFile(name);
}
//-----------------------------------------------------------------------------
ref<QtFile> QtDirectory::qtFile(const String &name) const
{
  String p = translatePath(name);
  ref<QtFile> file = new QtFile(p);
  if (file->exists())
    return file;
  else
    return NULL;
}
//-----------------------------------------------------------------------------
void QtDirectory::listFilesRecursive_internal(std::vector<String> &file_list) const
{
  // add local child
  listFiles(file_list, true);
  // descend recursively
  std::vector<String> dir_list;
  listSubDirs(dir_list);
  for (unsigned i = 0; i < dir_list.size(); ++i)
  {
    VL_CHECK(dir_list[i] != ".")
    VL_CHECK(dir_list[i] != "..")

    QtDirectory sub_dir(dir_list[i]);
    sub_dir.listFilesRecursive_internal(file_list);
  }
}
//-----------------------------------------------------------------------------
