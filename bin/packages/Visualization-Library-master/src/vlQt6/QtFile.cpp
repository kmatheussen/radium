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

#include <vlQt6/QtFile.hpp>

using namespace vl;

//-----------------------------------------------------------------------------
// QtFile
//-----------------------------------------------------------------------------
QtFile::QtFile(const String &path)
{
  setPath(path);
}
//-----------------------------------------------------------------------------
QtFile::~QtFile()
{
  close();
}
//-----------------------------------------------------------------------------
bool QtFile::open(const String &path, EOpenMode mode)
{
  setPath(path);
  return open(mode);
}
//-----------------------------------------------------------------------------
bool QtFile::open(EOpenMode mode)
{
  if (isOpen())
  {
    Log::error("QtFile::open(): file already open.\n");
    return false;
  }

  QIODevice::OpenMode qmode;

  if (mode == OM_ReadOnly)
  {
    qmode = QIODevice::ReadOnly;
  }
  else
  {
    qmode = QIODevice::WriteOnly;
  }

  mQFile.setFileName(path().toStdString().c_str());
  if (!mQFile.open(qmode))
  {
    Log::error(Say("QtFile::open(): error opening file '%s'\n") << path());
    return false;
  }

  return true;
}
//-----------------------------------------------------------------------------
bool QtFile::isOpen() const
{
  return mQFile.isOpen();
}
//-----------------------------------------------------------------------------
void QtFile::close()
{
  mQFile.close();
}
//-----------------------------------------------------------------------------
long long QtFile::size() const
{
  return QFile(path().toStdString().c_str()).size();
}
//-----------------------------------------------------------------------------
bool QtFile::exists() const
{
  return !path().empty() && QFile::exists(path().toStdString().c_str());
}
//-----------------------------------------------------------------------------
long long QtFile::read_Implementation(void *buffer, long long byte_count)
{
  if (!mQFile.isOpen())
  {
    Log::error("QtFile::read_Implementation() called on closed file!\n");
    return 0;
  }
  return mQFile.read((char *)buffer, byte_count);
}
//-----------------------------------------------------------------------------
long long QtFile::write_Implementation(const void *buffer, long long byte_count)
{
  if (!mQFile.isOpen())
  {
    Log::error("QtFile::write_Implementation() called on closed file!\n");
    return 0;
  }
  return mQFile.write((char *)buffer, byte_count);
}
//-----------------------------------------------------------------------------
long long QtFile::position_Implementation() const
{
  if (!mQFile.isOpen())
  {
    Log::error("QtFile::position_Implementation() called on closed file!\n");
    return -1;
  }
  return mQFile.pos();
}
//-----------------------------------------------------------------------------
bool QtFile::seekSet_Implementation(long long offset)
{
  if (!mQFile.isOpen())
  {
    Log::error("QtFile::seekSet_Implementation() called on closed file!\n");
    return false;
  }
  mQFile.seek(offset);
  return true;
}
//-----------------------------------------------------------------------------
ref<VirtualFile> QtFile::clone() const
{
  ref<QtFile> file = new QtFile;
  file->operator=(*this);
  return file;
}
//-----------------------------------------------------------------------------
