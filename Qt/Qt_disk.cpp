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
#include <QSaveFile>
#include <QDir>
#include <QDateTime>

#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"
#include "../common/Mutex.hpp"

#include "../api/api_proc.h"

#include "../common/OS_disk_proc.h"



#define SUPPORT_TEMP_WRITING_FUNCTIONS 0

const wchar_t *DISK_get_absolute_file_path(const wchar_t *wfilename){
  QFileInfo info(STRING_get_qstring(wfilename));
  return STRING_create(QDir::toNativeSeparators(info.absoluteFilePath()));
}

int64_t DISK_get_creation_time(const wchar_t *wfilename){
  QFileInfo info(STRING_get_qstring(wfilename));
  info.setCaching(false);
  return info.lastModified().toMSecsSinceEpoch();
}

bool DISK_file_exists(const wchar_t *wfilename){
  QString filename = STRING_get_qstring(wfilename);
  return QFile::exists(filename);
}

bool DISK_dir_exists(const wchar_t *wdirname){
  QString filename = STRING_get_qstring(wdirname);
  return QDir(filename).exists();
}

bool DISK_create_dir(const wchar_t *wdirname){
  if(DISK_dir_exists(wdirname))
    return true;
  QDir::root().mkpath(STRING_get_qstring(wdirname));
  return DISK_dir_exists(wdirname);
}

bool DISK_delete_file(const wchar_t *wfilename){
  if(!DISK_file_exists(wfilename))
    return false;
  
  QFile::remove(STRING_get_qstring(wfilename));

  return !DISK_file_exists(wfilename);
}

void DISK_delete_all_files_in_dir(const wchar_t *wdirname){
  QDir dir(STRING_get_qstring(wdirname));
  R_ASSERT_RETURN_IF_FALSE(dir.absolutePath() != QDir::root().absolutePath());
  
  QFileInfoList files = dir.entryInfoList(QDir::Files|QDir::NoDotAndDotDot);
  
  for(const auto &file : files)
    QFile::remove(file.absoluteFilePath());
}

const wchar_t *DISK_get_dir_separator(void){
  return STRING_create(QDir::separator());
}

const wchar_t *DISK_create_non_existant_filename(const wchar_t *filename){
  if (DISK_file_exists(filename)==false)
    return filename;

  QFileInfo info(STRING_get_qstring(filename));
  
  QString dirname = info.absoluteDir().absolutePath();
  QString basename = info.baseName();
  QString suffix = info.completeSuffix();
  if (suffix != "")
    suffix = "." + suffix;

  for(int i=2;i<10000;i++){
    QString maybe = dirname + QDir::separator() + basename + "_" + QString::number(i) + suffix;
    if (DISK_file_exists(STRING_create(maybe))==false)
      return STRING_create(maybe);
  }

  return filename;
}

bool DISK_copy(const wchar_t *old_file, const wchar_t *new_file){
  return QFile::copy(STRING_get_qstring(old_file), STRING_get_qstring(new_file));
}

const wchar_t *DISK_get_temp_dir(void){
  return STRING_create(QDir::tempPath());
}

const wchar_t *DISK_copy_to_temp_file(const wchar_t *old_file){
  QFileInfo info(STRING_get_qstring(old_file));

  QString dirname = info.absoluteDir().absolutePath();
  QString basename = info.baseName();
  QString suffix = info.completeSuffix();
  if (suffix != "")
    suffix = "." + suffix;

  QString template_ = STRING_get_qstring(DISK_get_temp_dir()) + QDir::separator() + "radium_copied" + suffix;
  const wchar_t *temp_file = DISK_create_non_existant_filename(STRING_create(template_));

  if (temp_file==NULL)
    return NULL;

  printf("  TEMP_FILE: -%S-\n", temp_file);

  if (DISK_copy(old_file, temp_file)==false){
    DISK_delete_file(temp_file);
    return NULL;
  }

  return temp_file;
}

static const char *g_last_error = "";


struct _radium_os_disk {
  enum Type{
    READ,
    WRITE
  };

  QString filename;

private:
  
  QFile *read_file;
  QSaveFile *save_file; //temporary_write_file;

public:
  
  int curr_read_line = 0;
  enum Type type;
  bool is_binary;
  QTextStream *stream;  

  bool has_set_pos_without_reading = false;
  
  _radium_os_disk(QString filename, enum Type type, bool is_binary=false)
    : filename(filename)
    , read_file(NULL)
    , save_file(NULL)
    , type(type)
    , is_binary(is_binary)
    , stream(NULL)
  {
  }

  ~_radium_os_disk(){
    if (stream!=NULL)
      delete stream;
    if (read_file!=NULL)
      delete read_file;
    if (save_file!=NULL)
      delete save_file;
  }

  QFileDevice *file(void){
    if (type==WRITE){
      R_ASSERT(save_file!=NULL);
      return save_file;
    } else {
      R_ASSERT(read_file!=NULL);
      return read_file;
    }
  }
  
  bool open(void){
    R_ASSERT(save_file==NULL);
    R_ASSERT(read_file==NULL);
    
    if (type==WRITE) {
      
      save_file = new QSaveFile(filename);

      if (is_binary) {
        if (save_file->open(QIODevice::WriteOnly)==false)
          goto failed;
      } else {
        if (save_file->open(QIODevice::WriteOnly | QIODevice::Text)==false)
          goto failed;
      }

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

    if (is_binary==false){
      stream = new QTextStream(file());
      stream->setCodec("UTF-8");
    }

    return true;

    
  failed:
    
    if (save_file!=NULL) {
      
      g_last_error = error_to_string(save_file->error());
      
      delete save_file;
      save_file = NULL;
    }
    
    if (read_file!=NULL) {

      g_last_error = error_to_string(read_file->error());
      
      delete read_file;
      read_file = NULL;
    }
    
    return false;    
  }

  bool transfer_temporary_file_to_file(void){
    R_ASSERT(type==WRITE);
    R_ASSERT(save_file != NULL);

#if SUPPORT_TEMP_WRITING_FUNCTIONS
    if (filename=="")
      return true;
#endif

    QString backup_filename = filename + ".bak";
    bool is_renamed = false;

    if (QFile::exists(filename)) {

      if (QFile::exists(backup_filename))
        QFile::remove(backup_filename);

      is_renamed = QFile::copy(filename, backup_filename);
    }

    bool ret = save_file->commit(); //QFile::copy(temporary_write_file->fileName(), filename);
    if (ret==false){
      QString message("Error. Unable to save file \"" + filename + "\"" + (is_renamed==false?"":(QString(" (The old file was renamed to \"")+backup_filename+"\").")));
      if (THREADING_is_main_thread())
        addMessage(message.toUtf8().constData());
      else
        RT_message("%s", message.toUtf8().constData());
    }
    
    return ret;
  }

  const char *error_to_string(QFile::FileError error){
    switch(error){
      case QFile::NoError: {
        R_ASSERT(false);
        return "";
      }
        //case QFile::ConnectError: return "connect error";
      case QFile::ReadError: return "Read error";
      case QFile::WriteError: return "Write error";
      case QFile::FatalError: return "Fatal error";
      case QFile::ResourceError: return "No more disk space, too many files open, or out of memory";
      case QFile::OpenError: return "Could not open file";
      case QFile::AbortError: return "Operation was aborted";
      case QFile::TimeOutError: return "Timeout";
      case QFile::UnspecifiedError: return "Unspecified error";
      case QFile::RemoveError: return "File could not be removed";
      case QFile::RenameError: return "File could not be renamed";
      case QFile::PositionError: return "Could not search to position";
      case QFile::ResizeError: return "Could not resize file";
      case QFile::PermissionsError: return "No permission to open file";
      case QFile::CopyError: return "Could not copy file";
    }

    R_ASSERT_NON_RELEASE(false);
    return "Unknown error";
  }

  const char* get_error(void){
    QFile::FileError error = file()->error();
    if (error == QFileDevice::NoError)
      return NULL;

    return error_to_string(error);
  }
  
  bool close(void){
    bool ret = true;

    if (type==READ) {

      file()->close();

      const char *error = get_error();
      if (error != NULL){
        GFX_Message(NULL, "Error %s file %S: %s",type==WRITE ? "writing to" : "reading from", STRING_create(filename), error);
        ret = false;
      }

    } else {

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
};


static disk_t *open_for_writing(QString filename, bool is_binary){
  disk_t *disk = new disk_t(filename, disk_t::WRITE, is_binary);
  
  if (disk->open()==false){
    delete disk;
    return NULL;
  }
  
  return disk;
}

disk_t *DISK_open_for_writing(QString filename){
  return open_for_writing(filename, false);
}

disk_t *DISK_open_binary_for_writing(QString filename){
  return open_for_writing(filename, true);
}

disk_t *DISK_open_for_writing(const wchar_t *wfilename){
  QString filename = STRING_get_qstring(wfilename);
  return DISK_open_for_writing(filename);
}

disk_t *DISK_open_binary_for_writing(const wchar_t *wfilename){
  QString filename = STRING_get_qstring(wfilename);
  return DISK_open_binary_for_writing(filename);
}

#if SUPPORT_TEMP_WRITING_FUNCTIONS
disk_t *DISK_open_temp_for_writing(void){
  GFX_Message(NULL, "Warning, never tested");
    
  disk_t *disk = new disk_t("", disk_t::WRITE);
  
  if (disk->open()==false){
    delete disk;
    return NULL;
  }
  
  return disk;
}

wchar_t *DISK_close_temp_for_writing(disk_t *disk){
  GFX_Message(NULL, "Warning, never tested");
  
  disk->close();

  QByteArray data = disk->temporary_write_file->readAll();

  wchar_t *ret = STRING_create(data.toUtf8().constData());

  delete disk;

  return ret;
}
#endif

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

const char* DISK_get_error(disk_t *disk){
  if(disk==NULL)
    return g_last_error;
  else
    return disk->get_error();
}

wchar_t *DISK_get_filename(disk_t *disk){
  return STRING_create(disk->filename);
}

int DISK_write_qstring(disk_t *disk, QString s){
  R_ASSERT(disk->is_binary==false);
  R_ASSERT(disk->type==disk_t::WRITE);

  int64_t pos = disk->stream->pos();
  *disk->stream << s;
  disk->stream->flush();
  return int(disk->stream->pos() - pos);
}

int DISK_write_wchar(disk_t *disk, const wchar_t *wdata){
  QString data = STRING_get_qstring(wdata);
  return DISK_write_qstring(disk, data);
}

int DISK_write(disk_t *disk, const char *cdata){
  QString data = QString::fromUtf8(cdata);
  return DISK_write_qstring(disk, data);
}

QString g_file_at_end("_________FILE_AT_END");

int DISK_get_curr_read_line(disk_t *disk){
  R_ASSERT(disk->is_binary==false);
  R_ASSERT(disk->type==disk_t::READ);

  return disk->curr_read_line;
}

bool DISK_at_end(disk_t *disk){
  return disk->stream->atEnd();
}

QString DISK_read_qstring_line(disk_t *disk){
  R_ASSERT(disk->is_binary==false);
  R_ASSERT(disk->type==disk_t::READ);

  disk->curr_read_line++;
  
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
  R_ASSERT(disk->has_set_pos_without_reading==false);
  disk->has_set_pos_without_reading = true;
  return disk->set_pos(pos);
}

bool DISK_spool(disk_t *disk, int64_t how_much){
  return disk->spool(how_much);
}

int64_t DISK_pos(disk_t *disk){
  return disk->pos();
}

bool DISK_is_binary(disk_t *disk){
  return disk->is_binary;
}

int64_t DISK_read_binary(disk_t *disk, void *destination, int64_t num_bytes){
  R_ASSERT_RETURN_IF_FALSE2(disk->is_binary==true, -1);
  R_ASSERT_RETURN_IF_FALSE2(disk->type==disk_t::READ, -1);

  disk->has_set_pos_without_reading = false;
  
  int64_t ret = disk->file()->read((char*)destination, num_bytes);
  
  if (ret==-1)
    GFX_addMessage("Failed reading from %s. Error code: %d.\n(The error codes are listed at http://doc.qt.io/qt-5/qfiledevice.html#error)",
                   disk->filename.toUtf8().constData(),
                   (int)disk->file()->error()
                   );
  
  return ret;
}

int64_t DISK_write_binary(disk_t *disk, const void *source, int64_t num_bytes){
  R_ASSERT_RETURN_IF_FALSE2(disk->is_binary==true, -1);
  R_ASSERT_RETURN_IF_FALSE2(disk->type==disk_t::WRITE, -1);

  int64_t ret = disk->file()->write((const char*)source, num_bytes);
  
  if (ret!=num_bytes)
    GFX_addMessage("Failed writing to %s. Number of bytes written: %d, excpected %d. Error code: %d.\n(The error codes are listed at http://doc.qt.io/qt-5/qfiledevice.html#error)",
                   disk->filename.toUtf8().constData(),
                   (int)ret,
                   (int)num_bytes,
                   (int)disk->file()->error()
                   );
  
  return ret;
}

bool DISK_close_and_delete(disk_t *disk){
  bool ret = disk->close();

  delete disk;

  return ret;
}

// Only used for audio files, so we don't bother with compression.
const char *DISK_file_to_base64(const wchar_t *wfilename){
  disk_t *disk = DISK_open_binary_for_reading(wfilename);

  if (disk==NULL)
    return NULL;

  QByteArray data = disk->file()->readAll();

  DISK_close_and_delete(disk);

  return talloc_strdup(data.toBase64().constData());
}

static QMap<QString, QTemporaryFile*> g_temporary_files;
static radium::Mutex g_mutex;

// Only used for audio files, so we don't bother with decompression.
const wchar_t *DISK_base64_to_file(const wchar_t *wfilename, const char *chars){
  QFile *file;

  QTemporaryFile *temporary_write_file = NULL;
    
  QFile outfile;

  QByteArray data = QByteArray::fromBase64(chars);
  
  if (wfilename==NULL) {

    temporary_write_file = new QTemporaryFile;
    
    file = temporary_write_file;
    
  } else {
    
    outfile.setFileName(STRING_get_qstring(wfilename));
  
    file = &outfile;
  }

  if (file->open(QIODevice::WriteOnly)==false){
    GFX_Message(NULL, "Unable to open file \"%s\" (%s)", file->fileName().toUtf8().constData(), file->errorString().toUtf8().constData());
    return NULL;
  }

  if (file->write(data) != data.size()){
    GFX_Message(NULL, "Unable to write to file \"%s\" (%s)", file->fileName().toUtf8().constData(), file->errorString().toUtf8().constData());
    file->close();
    return NULL;
  }

  file->close();

  if (wfilename==NULL){
    radium::ScopedMutex lock(g_mutex);
    g_temporary_files[temporary_write_file->fileName()] = temporary_write_file;
  }else
    R_ASSERT(temporary_write_file==NULL);
  
  return STRING_create(file->fileName());
}

void DISK_delete_base64_file(const wchar_t *wfilename){
  radium::ScopedMutex lock(g_mutex);
  
  QString key = STRING_get_qstring(wfilename);
  QTemporaryFile *file = g_temporary_files[key];

  R_ASSERT_RETURN_IF_FALSE(file!=NULL);

  g_temporary_files.remove(key);
  
  delete file;
}

void DISK_cleanup(void){
  radium::ScopedMutex lock(g_mutex);
  
  for(auto *file : g_temporary_files.values())
    delete file;

  g_temporary_files.clear();
}

#if defined(FOR_WINDOWS)
static QString file_to_string(QString filename){
  QFile file(filename);
  bool ret = file.open(QIODevice::ReadOnly | QIODevice::Text);
  if( ret )
    {
      QTextStream stream(&file);
      QString content = stream.readAll();
      return content;
    }
  return "(unable to open file -"+filename+"-)";
}
#endif

// 'program' must be placed in the program bin path.
// The returned value must be manually freed.
// Can be called from any thread.
// Leaks memory.
char *DISK_run_program_that_writes_to_temp_file(const char *program, const char *arg1, const char *arg2, const char *arg3){
  QString filename;

  {
    QTemporaryFile file(QDir::tempPath() + QDir::separator() + "radium_addr2line");
    bool succ = file.open();
    if (succ==false)
      return strdup("(Unable to open temporary file)");
    
    filename = file.fileName();
  }
  
#if defined(FOR_WINDOWS)

  QString full_path_program = OS_get_full_program_file_path(program);
  
  wchar_t *p = STRING_create(full_path_program, false);
  wchar_t *p1 = STRING_create(QString("\"") + full_path_program + "\"", false);
  wchar_t *a1 = STRING_create(QString("\"") + arg1 + "\"", false); // _wspawnl is really stupid. (https://blogs.msdn.microsoft.com/twistylittlepassagesallalike/2011/04/23/everyone-quotes-command-line-arguments-the-wrong-way/)
  wchar_t *a2 = STRING_create(QString("\"") + arg2 + "\"", false);
  wchar_t *a3 = STRING_create(QString("\"") + arg3 + "\"", false);
  wchar_t *a4 = STRING_create("\""+filename+"\"", false);
  printf("   file.fileName(): -%s-\n",filename.toUtf8().constData());


  if(_wspawnl(_P_WAIT, p, p1, a1, a2, a3, a4, NULL)==-1){
    char *temp = (char*)malloc(strlen(program)+strlen(arg1)+1024);    
    sprintf(temp, "Couldn't launch %s: \"%s\"\n",program,arg1);
    fprintf(stderr,temp);
    //SYSTEM_show_message(strdup(temp));
    //Sleep(3000);
    return temp;
  }
  
  QString ret = file_to_string(filename).trimmed();

  QFile file(filename);
  file.remove();
  
  return strdup(ret.toUtf8().constData());
  
#else
  
  RError("Not implemented\n");
  return strdup("not implemented");
  
#endif
}

