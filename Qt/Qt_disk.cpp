/* Copyright 2012 Kjetil S. Matheussen

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */

#include <unistd.h>

#include <QString>
#include <QFile>
#include <QTextStream>
#include <QTemporaryFile>

#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"

#include "../common/OS_disk_proc.h"


bool DISK_file_exists(const wchar_t *wfilename){
  QString filename = STRING_get_qstring(wfilename);
  return QFile::exists(filename);
}


struct _radium_os_disk {
  enum Type{
    READ,
    WRITE
  };

private:
  
  QString filename;
  QFile *read_file;
  QTemporaryFile *temporary_write_file;

public:

  enum Type type;
  bool is_binary;
  QTextStream *stream;  

  _radium_os_disk(QString filename, enum Type type, bool is_binary=false)
    : filename(filename)
    , read_file(NULL)
    , temporary_write_file(NULL)
    , type(type)
    , is_binary(is_binary)
    , stream(NULL)
  {
  }

  QFile *file(void){
    if (type==WRITE){
      R_ASSERT(temporary_write_file!=NULL);
      return temporary_write_file;
    } else {
      R_ASSERT(read_file!=NULL);
      return read_file;
    }
  }
  
  bool open(void){
    R_ASSERT(temporary_write_file==NULL);
    R_ASSERT(read_file==NULL);
    
    if (type==WRITE) {
      
      R_ASSERT(is_binary==false); // not supported yet
      
      temporary_write_file = new QTemporaryFile();
      if (temporary_write_file->open()==false)
        goto failed;
      
    } else {
      
      read_file = new QFile(filename);

      if (is_binary) {
        if (read_file->open(QIODevice::ReadOnly)==false)
          goto failed;
      } else {
        if (read_file->open(QIODevice::ReadOnly | QIODevice::Text)==false)
          goto failed;
      }      
    }

    stream = new QTextStream(file());
    stream->setCodec("UTF-8");

    return true;

    
  failed:
    if (temporary_write_file!=NULL)
      delete temporary_write_file;
    if (read_file!=NULL)
      delete read_file;
    return false;    
  }

  bool transfer_temporary_file_to_file(void){
    R_ASSERT(type==WRITE);
    R_ASSERT(temporary_write_file != NULL);
    
    if (QFile::exists(filename))
      QFile::remove(filename);
      
    return QFile::copy(temporary_write_file->fileName(), filename);
  }

  QString error_to_string(QFile::FileError error){
    switch(error){
      case QFile::NoError: {
        R_ASSERT(false);
        return "";
      }
      case QFile::ConnectError: return "connect error";
      case QFile::ReadError: return "read error";
      case QFile::WriteError: return "write error";
      case QFile::FatalError: return "fatal error";
      case QFile::ResourceError: return "resource error";
      case QFile::OpenError: return "open error";
      case QFile::AbortError: return "abort error";
      case QFile::TimeOutError: return "timeout error";
      case QFile::UnspecifiedError: return "unspecified error";
      case QFile::RemoveError: return "remove error";
      case QFile::RenameError: return "rename error";
      case QFile::PositionError: return "position error";
      case QFile::ResizeError: return "resize error";
      case QFile::PermissionsError: return "permission error";
      case QFile::CopyError: return "copy error";
    }

    R_ASSERT(false);
    return "Unknown error";
  }

  bool close(void){
    bool ret = true;

    file()->close();
    
    QFile::FileError error = file()->error();
    if (error != 0) {
      GFX_Message(NULL, "Error %s file: %s",type==WRITE ? "writing to" : "reading from", error_to_string(error).toUtf8().constData());
      ret = false;
    }

    if (type==WRITE) {
      bool copyret = transfer_temporary_file_to_file();
      if (copyret==false)
        ret = false;
    }

    return ret;
  }
  
  bool set_pos(int64_t pos){
    R_ASSERT(is_binary==true);
    return file()->seek(pos);
  }
  
  bool spool(int64_t how_much){
    R_ASSERT(is_binary==true);
    return file()->seek(read_file->pos() + how_much);
  }

  int64_t pos(void){
    R_ASSERT(is_binary==true);
    return file()->pos();
  }

  ~_radium_os_disk(){
    if (read_file!=NULL)
      delete read_file;
    if (temporary_write_file!=NULL)
      delete temporary_write_file;
    if (stream!=NULL)
      delete stream;
  }
};


disk_t *DISK_open_for_writing(QString filename){
  disk_t *disk = new disk_t(filename, disk_t::WRITE);
  
  if (disk->open()==false){
    delete disk;
    return NULL;
  }
  
  return disk;
}

disk_t *DISK_open_for_writing(const wchar_t *wfilename){
  QString filename = STRING_get_qstring(wfilename);
  return DISK_open_for_writing(filename);
}


disk_t *DISK_open_for_reading(QString filename){
  disk_t *disk = new disk_t(filename, disk_t::READ);

  if (disk->open()==false){
    delete disk;
    return NULL;
  }
  
  return disk;
}

disk_t *DISK_open_for_reading(const wchar_t *wfilename){
  QString filename = STRING_get_qstring(wfilename);
  return DISK_open_for_reading(filename);
}

disk_t *DISK_open_binary_for_reading(const wchar_t *wfilename){
  QString filename = STRING_get_qstring(wfilename);
  
  disk_t *disk = new disk_t(filename, disk_t::READ, true);

  if (disk->open()==false){
    delete disk;
    return NULL;
  }
  
  return disk;
}

static int write_qstring(disk_t *disk, QString s){
  R_ASSERT(disk->is_binary==false);
  R_ASSERT(disk->type==disk_t::WRITE);

  int pos = disk->stream->pos();
  *disk->stream << s;
  disk->stream->flush();
  return disk->stream->pos() - pos;
}

int DISK_write_wchar(disk_t *disk, const wchar_t *wdata){
  QString data = STRING_get_qstring(wdata);
  return write_qstring(disk, data);
}

int DISK_write(disk_t *disk, const char *cdata){
  QString data = QString::fromUtf8(cdata);
  return write_qstring(disk, data);
}


QString g_file_at_end("_________FILE_AT_END");

QString DISK_read_qstring_line(disk_t *disk){
  R_ASSERT(disk->is_binary==false);
  R_ASSERT(disk->type==disk_t::READ);
  
  if (disk->stream->atEnd())
    return g_file_at_end;

  return disk->stream->readLine();
}

wchar_t *DISK_read_wchar_line(disk_t *disk){
  QString line = DISK_read_qstring_line(disk);
  
  if (line==g_file_at_end)
    return NULL;

  return STRING_create(line);
}

char *DISK_readline(disk_t *disk){
  QString line = DISK_read_qstring_line(disk);
  
  if (line==g_file_at_end)
    return NULL;

  return talloc_strdup(line.toUtf8().constData());
}

char *DISK_read_trimmed_line(disk_t *disk){
  QString line = DISK_read_qstring_line(disk);
  
  if (line==g_file_at_end)
    return NULL;

  return talloc_strdup(line.trimmed().toUtf8().constData());
}

bool DISK_set_pos(disk_t *disk, int64_t pos){
  return disk->set_pos(pos);
}

bool DISK_spool(disk_t *disk, int64_t how_much){
  return disk->spool(how_much);
}

int64_t DISK_pos(disk_t *disk){
  return disk->pos();
}

int DISK_read_binary(disk_t *disk, void *destination, int num_bytes){
  R_ASSERT(disk->is_binary==true);
  R_ASSERT(disk->type==disk_t::READ);
  return disk->file()->read((char*)destination, num_bytes);
}

bool DISK_close_and_delete(disk_t *disk){
  bool ret = disk->close();

  delete disk;

  return ret;
}

