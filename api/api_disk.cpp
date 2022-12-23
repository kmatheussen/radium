/* Copyright 2017 Kjetil S. Matheussen

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








#include "../common/includepython.h"

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wshorten-64-to-32"
#include <QVector> // Shortening warning in the QVector header. Temporarily turned off by the surrounding pragmas.
#pragma clang diagnostic pop



#include <QFileInfo>
#include <QFile>
#include <QDateTime>
#include <QDir>
#include <QStack>
#include <QSet>
#include <QCoreApplication>
#include <QDirIterator>
#include <QFileInfo>
#include <QThread>

#define INCLUDE_SNDFILE_OPEN_FUNCTIONS 1
#include "../common/nsmtracker.h"

#include "../common/OS_disk_proc.h"
#include "../common/OS_string_proc.h"
#include "../common/visual_proc.h"
#include "../common/settings_proc.h"

#include "../embedded_scheme/s7extra_proc.h"

#include "api_common_proc.h"

#include "radium_proc.h"


static int64_t g_curr_disknum = 0;

static QHash<file_t,disk_t*> g_disks;


filepath_t getPath(const_char *path_string){
  return make_filepath(STRING_create(path_string));
}

filepath_t getHomePath(void){
  return make_filepath(OS_get_home_path());
}

filepath_t getProgramPath(void){
  return make_filepath(QCoreApplication::applicationDirPath());
}

const_char* getPathString(filepath_t filepath){
  if (isIllegalFilepath(filepath)){
    handleError("Illegal filepath argument 1");
    return "";
  }
  
  return STRING_get_chars(filepath.id);
}

filepath_t appendFilePaths(filepath_t path1, filepath_t path2){  
  if (isIllegalFilepath(path1)){
    handleError("Illegal filepath argument 1");
    return path1;
  }
  
  if (isIllegalFilepath(path2)){
    handleError("Illegal filepath argument 2");
    return path2;
  }
  
  return make_filepath(STRING_get_qstring(path1.id) + QDir::separator() + STRING_get_qstring(path2.id));
}

filepath_t getParentPath(filepath_t path){
  if (isIllegalFilepath(path)){
    handleError("Illegal filepath argument 1");
    return path;
  }
  
  QDir dir(STRING_get_qstring(path.id));
  dir.cdUp();  
  return make_filepath(dir.absolutePath());
}

filepath_t getDirPath(filepath_t path){
  if (isIllegalFilepath(path)){
    handleError("Illegal filepath argument 1");
    return path;
  }

  return DISK_get_absolute_dir_path(path);
}

filepath_t getPathWithoutDir(filepath_t path){
  if (isIllegalFilepath(path)){
    handleError("Illegal filepath argument 1");
    return path;
  }

  return DISK_get_pathless_file_path(path);
}

bool fileExists(filepath_t path){
  if (isIllegalFilepath(path)){
    handleError("Illegal filepath argument 1");
    return false;
  }
  
  QString filename = STRING_get_qstring(path.id);
  return QFileInfo::exists(filename);
}

extern QStringList get_sample_name_filters(void);

#ifdef FOR_WINDOWS
extern Q_CORE_EXPORT int qt_ntfs_permission_lookup;
#endif


dyn_t getFileInfo(filepath_t w_path){
  if (isIllegalFilepath(w_path)){
    handleError("getFileInfo: illegal path for argument 1");
    return g_uninitialized_dyn;
  }
  
  QString path = STRING_get_qstring(w_path.id);

  QFileInfo info(path);

  /*
    Happens when a symbolic link points to a non-existing file or directory.
  if (!info.exists()){
    showAsyncMessage(talloc_format("Directory \"%S\" does not exist.", STRING_create(path)));
    return g_uninitialized_dyn;
  }
  */
  
  /*
    Commented out. We never want to return g_uninitialized or any other non-hashtable value.
#ifdef FOR_WINDOWS
  qt_ntfs_permission_lookup++;
#endif

  bool is_readable =  info.isReadable();

#ifdef FOR_WINDOWS
  qt_ntfs_permission_lookup--;
#endif

  if (!is_readable){
    showAsyncMessage(talloc_format("Directory \"%S\" is not readable", STRING_create(path)));
    return g_uninitialized_dyn;
  }
  */
  
  hash_t *ret = HASH_create(10);

  if(info.isDir()){
    if(QDir(QDir::cleanPath(path)).isRoot()){ // check for isRoot in case the path is "/../"
      printf("ends 2\n");
      path = QDir::root().absolutePath();
    } else if(path.endsWith(".")){
      printf("ends 1\n");
      //QDir dir = info.dir();//absoluteDir();
      //dir.cdUp();
      //path = dir.cleanPath(); // "/tmp/tmp/.." -> "/tmp/tmp"
      path = QDir::cleanPath(path);
    }
  }

  HASH_put_filepath(ret, ":path", make_filepath(path));
  HASH_put_filepath(ret, ":filename", make_filepath(info.fileName())); //STRING_create(info.fileName()));
  
  HASH_put_bool(ret, ":is-dir", info.isDir());
  HASH_put_bool(ret, ":is-sym-link", info.isSymLink());
  HASH_put_bool(ret, ":exists", info.exists());
  HASH_put_int(ret, ":last-modified", info.lastModified().toSecsSinceEpoch());
  HASH_put_bool(ret, ":is-hidden", info.isHidden());
  HASH_put_int(ret, ":size", info.size());
  HASH_put_string(ret, ":suffix", STRING_create(info.suffix()));

  bool could_be_audiofile = false;
  for(auto filter : get_sample_name_filters())
    if (info.fileName().endsWith(filter.mid(1))){
      could_be_audiofile = true;
      break;
    }

  SF_INFO sf_info = {};
  SNDFILE *sndfile = !could_be_audiofile ? NULL : radium_sf_open(path, SFM_READ, &sf_info);

  HASH_put_bool(ret, ":is-audiofile", sndfile != NULL);
  if (sndfile != NULL){
    HASH_put_int(ret, ":num-ch", sf_info.channels);
    HASH_put_int(ret, ":num-frames", sf_info.frames);
    HASH_put_float(ret, ":samplerate", sf_info.samplerate);
    const char *format;
    switch(sf_info.format & 0xffff){
      case SF_FORMAT_PCM_S8       : format = "8bit" ; break;
      case SF_FORMAT_PCM_16       : format = "16bit" ; break;
      case SF_FORMAT_PCM_24       : format = "24bit" ; break;
      case SF_FORMAT_PCM_32       : format = "32bit" ; break;
        
      case SF_FORMAT_PCM_U8       : format = "8bit" ; break;
        
      case SF_FORMAT_FLOAT        : format = "Float" ; break;
      case SF_FORMAT_DOUBLE       : format = "Double" ; break;
        
      case SF_FORMAT_ULAW         : format = "U-Law" ; break;
      case SF_FORMAT_ALAW         : format = "A-Law" ; break;
      case SF_FORMAT_IMA_ADPCM    : format = "ADPCM" ; break;
      case SF_FORMAT_MS_ADPCM     : format = "ADPCM" ; break;
        
      case SF_FORMAT_GSM610       : format = "GSM" ; break;
      case SF_FORMAT_VOX_ADPCM    : format = "ADPCM" ; break;
        
      case SF_FORMAT_G721_32      : format = "ADPCM" ; break;
      case SF_FORMAT_G723_24      : format = "ADPCM" ; break;
      case SF_FORMAT_G723_40      : format = "ADPCM" ; break;
        
      case SF_FORMAT_DWVW_12      : format = "12bit" ; break;
      case SF_FORMAT_DWVW_16      : format = "16bit" ; break;
      case SF_FORMAT_DWVW_24      : format = "24bit" ; break;
      case SF_FORMAT_DWVW_N       : format = "Nbit" ; break;
        
      case SF_FORMAT_DPCM_8       : format = "8bit" ; break;
      case SF_FORMAT_DPCM_16      : format = "16bit" ; break;
        
      case SF_FORMAT_VORBIS       : format = "Vorbis" ; break;
      default                     : format = "Unknown" ; break;
    }
    HASH_put_chars(ret, ":format", format);
    sf_close(sndfile);
  }
  
  return DYN_create_hash(ret);
}

bool dirExists(filepath_t path){
  return DISK_dir_exists(path);
}
  
bool createDir(filepath_t path){
  return DISK_create_dir(path);
}


static bool call_callback(func_t* callback, bool in_main_thread, bool is_finished, const wchar_t *path){
  int64_t num_calls = g_num_calls_to_handleError;

  //printf("  Call callback -%S-\n", STRING_create(w_to_qstring(path)));

  if(path==NULL || !wcscmp(path,L""))
    R_ASSERT(is_finished);
  
  bool ret = S7CALL(bool_bool_dyn,
                    callback,
                    is_finished,                
                    (path==NULL || !wcscmp(path,L"")) ? g_uninitialized_dyn : getFileInfo(make_filepath(path))
                    );

  if (num_calls != g_num_calls_to_handleError) // Return false if the callback generated any errors.
    return false;

  return ret;
}

static void traverse(QString path, func_t* callback, bool in_main_thread){

  DEFINE_ATOMIC(bool, callback_has_returned_false) = false;
  int64_t last_id = -1;

  /*
  QStringList all;
  all << "*";
  all << "*.*";
  QDirIterator it(path, all, QDir(path).isRoot() ? QDir::NoDotDot : QDir::NoFilter);
  */
  bool is_root = QDir(path).isRoot();
  QDirIterator it(path);

  while (it.hasNext() && ATOMIC_GET(callback_has_returned_false)==false) {
    
    QString path = it.next();
    if(is_root && path=="/..")
      continue;

    QFileInfo info(path);
    if (info.fileName()=="." || info.fileName()=="..")
      continue;

    if (in_main_thread){

      if (!call_callback(callback,
                         true, 
                         false,
                         STRING_create(path)
                         ))
        ATOMIC_SET(callback_has_returned_false, true);

    } else {

      // Note: There's no hard limit on the number of simultaneous callbacks in THREADING_run_on_main_thread_async.
      // It just allocate new memory when needed.

      last_id = THREADING_run_on_main_thread_async([callback, path, &ATOMIC_NAME(callback_has_returned_false)](){
          if(!call_callback(callback,
                            true, 
                            false,
                            STRING_create(path)
                            ))
            ATOMIC_SET(callback_has_returned_false, true);
        });
    }

  }

  if (in_main_thread){

    if (ATOMIC_GET(callback_has_returned_false)==false)
      call_callback(callback, true, true, L"");

  } else {

    if (last_id >= 0)
      THREADING_wait_for_async_function(last_id);

    if (ATOMIC_GET(callback_has_returned_false)==false) {

      THREADING_run_on_main_thread_and_wait([callback](){

          call_callback(callback, false, true, L"");

      });

    }

  }
}

bool iterateDirectory(filepath_t daspath, bool async, func_t* callback){  
  if (isIllegalFilepath(daspath)){
    handleError("Illegal filepath argument 1");
    return false;
  }
  
  QString path = STRING_get_qstring(daspath.id);

  QFileInfo info(path);

  if (!info.exists()){
    showAsyncMessage(talloc_format("Directory \"%S\" does not exist.", STRING_create(path)));
    call_callback(callback, true, true, L"");
    return false;
  }

#ifdef FOR_WINDOWS
  qt_ntfs_permission_lookup++;
#endif

#ifdef FOR_WINDOWS
  bool is_readable = true; // isReadable() returns false for network disks on windows, even when the network disk is actually readable.
#else
  bool is_readable = info.isReadable();
#endif
  
#ifdef FOR_WINDOWS
  qt_ntfs_permission_lookup--;
#endif

  if (!is_readable){
    showAsyncMessage(talloc_format("Directory \"%S\" is not readable", STRING_create(path)));
    call_callback(callback, true, true, L"");
    return false;
  }
 
  if(async){

    class MyThread : QThread{
      QString _path;
      func_t *_callback;
      int64_t _gc_protect_pos;

    public:
      MyThread(QString path, func_t *callback)
        : _path(path)
        , _callback(callback)
        , _gc_protect_pos(s7extra_protect(callback))
      {
        setObjectName("iterate_directory_thread");
        start();
        connect(this, SIGNAL(finished()), this, SLOT(deleteLater()));
      }

      ~MyThread(){
        s7extra_unprotect(_callback, _gc_protect_pos);
      }

      void run() override {
        traverse(_path, _callback, false);
      }
    };

    new MyThread(path, callback);

  } else {

    traverse(path, callback, true);

  }

  return true;
}

const wchar_t *g_illegal_filepath_string = L"_______________RADIUM. illegal file path________________"; // Don't change. May be saved to disk.

filepath_t g_illegal_filepath = {.id = g_illegal_filepath_string};

filepath_t createIllegalFilepath(void){
  return g_illegal_filepath;
}

bool isLegalFilepath(filepath_t file){
  if (file.id==NULL){
    R_ASSERT_NON_RELEASE(false);
    return false;
  }
  
  if (file.id==g_illegal_filepath_string)
    return false;
  
  return wcscmp(file.id, g_illegal_filepath_string);
}

bool isIllegalFilepath(filepath_t file){
  if (file.id==NULL){
    R_ASSERT_NON_RELEASE(false);
    return true;
  }

  if (file.id==g_illegal_filepath_string)
    return true;
  
  return !wcscmp(file.id, g_illegal_filepath_string);
}

const_char* getBase64FromFilepath(filepath_t filepath){
  /*
  if (isIllegalFilepath(filepath)){
    handleError("getBase64FromFilepath: illegal filepath argument");
    return "";
  }
  */
  return STRING_get_chars(STRING_toBase64(filepath.id));
}

filepath_t getFilepathFromBase64(const_char* base64text){
  return make_filepath(w_path_to_path(base64text));
}

file_t createIllegalFile(void){
  return make_file(-1);
}

bool isIllegalFile(file_t file){
  return file.id==-1;
}


file_t openFileForReading(filepath_t w_path){
  if (isIllegalFilepath(w_path)){
    handleError("openFileForReading: illegal filepath argument 1");
    return createIllegalFile();
  }

  disk_t *disk = DISK_open_for_reading(w_path);
  if (disk==NULL){
    handleError("Unable to open file %S for reading\n", w_path.id);
    return createIllegalFile();
  }

  file_t disknum = make_file(++g_curr_disknum);
  g_disks[disknum] = disk;
  return disknum;
}

file_t openFileForWriting(filepath_t w_path){
  if (isIllegalFilepath(w_path)){
    handleError("Illegal filepath argument 1");
    return createIllegalFile();
  }

  disk_t *disk = DISK_open_for_writing(w_path);
  if (disk==NULL){
    handleError("Unable to open file for writing (couldn't create temporary file, check your file system)");
    return createIllegalFile();
  }

  file_t disknum = make_file(++g_curr_disknum);
  g_disks[disknum] = disk;
  return disknum;
}

bool closeFile(file_t disknum){
  disk_t *disk = g_disks.value(disknum);
  if (disk==NULL){
    handleError("closeFile: No file #%d", (int)disknum.id);
    return false;
  }

  g_disks.remove(disknum);

  return DISK_close_and_delete(disk);
}

bool writeToFile(file_t disknum, const_char* string){
  disk_t *disk = g_disks.value(disknum);
  if (disk==NULL){
    handleError("writeToFile: No file #%d", (int)disknum.id);
    return -1;
  }

  bool success = DISK_write(disk, string);

  if (!success){
    handleError("writeToFile failed: \"%s\"", DISK_get_error(disk));
  }

  return success;
}

bool fileAtEnd(file_t disknum){
  disk_t *disk = g_disks.value(disknum);
  if (disk==NULL){
    handleError("writeToFile: No file #%d", (int)disknum.id);
    return false;
  }

  return DISK_at_end(disk);
}

const_char* readLineFromFile(file_t disknum){
  disk_t *disk = g_disks.value(disknum);
  if (disk==NULL){
    handleError("writeToFile: No file #%d", (int)disknum.id);
    return "";
  }

  const_char* ret = DISK_readline(disk);
  if (ret==NULL){
    handleError("readLineFromFile: Attempting to read past end of file");
    return "";
  }

  return ret;
}



// read binary
#include "../common/read_binary.h"


file_t openFileForBinaryReading(filepath_t w_path){
  if (isIllegalFilepath(w_path)){
    handleError("Illegal filepath argument 1");
    return createIllegalFile();
  }
  
  disk_t *disk = DISK_open_binary_for_reading(w_path);
  if (disk==NULL){
    handleError("Unable to open file %S for reading\n", w_path.id);
    return createIllegalFile();
  }

  R_ASSERT(DISK_is_binary(disk));

  file_t disknum = make_file(++g_curr_disknum);
  g_disks[disknum] = disk;
  return disknum;
}


static bool read_binary(const_char* funcname, file_t disknum, unsigned char dest[], int64_t num_bytes){
  disk_t *disk = g_disks.value(disknum);
  if (disk==NULL){
    handleError("%s: No file #%d", funcname, (int)disknum.id);
    return false;
  }

  if (DISK_is_binary(disk)==false){
    handleError("%s: File #%d is not opened in binary mode", funcname, (int)disknum.id);
    return false;
  }

  int64_t num_bytes_read = DISK_read_binary(disk, dest, num_bytes);
  if(num_bytes_read != num_bytes){
    handleError("%s: Reading file failed", funcname);
    return false;
  }

  return true;
}


// 32 bit

int readLe32FromFile(file_t disknum){
  unsigned char chars[4];
  if(read_binary("readLe32FromFile", disknum, chars, 4)==false)
    return 0;

  return get_le_32(chars);
}

int64_t readLeU32FromFile(file_t disknum){
  unsigned char chars[4];
  if(read_binary("readLeU32FromFile", disknum, chars, 4)==false)
    return 0;

  return get_le_u32(chars);
}

/*
// there is no get_be_32 function
int readBe32FromFile(file_t disknum){
  unsigned char chars[4];
  if(read_binary("readLe32FromFile", disknum, chars, 4)==false)
    return 0;

  return get_be_32(chars);
}
*/

int64_t readBeU32FromFile(file_t disknum){
  unsigned char chars[4];
  if(read_binary("readLeU32FromFile", disknum, chars, 4)==false)
    return 0;

  return get_be_u32(chars);
}

// 16 bit

int readLe16FromFile(file_t disknum){
  unsigned char chars[2];
  if(read_binary("readLe16FromFile", disknum, chars, 2)==false)
    return 0;

  return get_le_16(chars);
}

/*
// there is no get_le_u16 function
int readLeU16FromFile(file_t disknum){
  unsigned char chars[2];
  if(read_binary("readLeU16FromFile", disknum, chars, 2)==false)
    return 0;

  return get_le_u16(chars);
}
*/

/*
// there is no get_be_16 function
int readBe16FromFile(file_t disknum){
  unsigned char chars[2];
  if(read_binary("readBe16FromFile", disknum, chars, 2)==false)
    return 0;

  return get_be_16(chars);
}
*/

int readBeU16FromFile(file_t disknum){
  unsigned char chars[2];
  if(read_binary("readBeU16FromFile", disknum, chars, 2)==false)
    return 0;

  return get_be_u16(chars);
}

// 8 bit

int read8FromFile(file_t disknum){
  disk_t *disk = g_disks.value(disknum);
  if (disk==NULL){
    handleError("readU8FromFile: No file #%d", (int)disknum.id);
    return 0;
  }
  
  if (DISK_is_binary(disk)==false){
    handleError("read8FromFile: File #%d is not opened in binary mode", (int)disknum.id);
    return 0;
  }

  int8_t size_chars[1] = {};
  if(DISK_read_binary(disk, size_chars, 1) !=1 ){
    handleError("read8FromFile: unable to read from file #%d",(int)disknum.id);
    return 0;
  }

  return size_chars[0];
}

int readU8FromFile(file_t disknum){
  unsigned char chars[1];
  if(read_binary("read8FromFile", disknum, chars, 1)==false)
    return 0;

  return chars[0];
}

void putSettings(const_char* key, const_char* value){
  SETTINGS_write_string(key, value);
}
void putSettingsW(const_char* key, const_char* w_value){
  SETTINGS_write_string(key, w_to_qstring(w_value));
}

void putSettingsF(const_char* key, filepath_t w_value){
  if (isIllegalFilepath(w_value)){
    handleError("Illegal filepath argument 2");
    return;
  }

  SETTINGS_write_string(key, STRING_get_qstring(w_value.id));
}

const_char* getSettings(const_char* key, const_char* default_value){
  return SETTINGS_read_string(key, default_value);
}

const_char* getSettingsW(const_char* key, const_char* default_w_value){
  return qstring_to_w(SETTINGS_read_qstring(key, w_to_qstring(default_w_value)));
}

filepath_t getSettingsF(const_char* key, filepath_t default_w_value){
  if (isIllegalFilepath(default_w_value)){
    handleError("Illegal filepath argument 2");
    return default_w_value;
  }
  
  return make_filepath(SETTINGS_read_qstring(key, STRING_get_qstring(default_w_value.id)));
}

dyn_t getAllSettings(const_char* starting_with) {
  const vector_t *lines = SETTINGS_get_all_lines_starting_with(starting_with);

  hash_t *ret = HASH_create(lines->num_elements);
  
  VECTOR_FOR_EACH(const char *, line_c, lines){

    QString line(line_c);

    {
      int pos = line.indexOf(QString("#"));
      if (pos!=-1)
        line.truncate(pos);
    }

    int pos = line.indexOf("=");
    if (pos==-1)
      continue;

    QString key = line.left(pos).trimmed();
    QString value = line.remove(0,pos+1).trimmed();

    HASH_put_qstring(ret, key.toUtf8().constData(), value);
    
  }END_VECTOR_FOR_EACH;

  return DYN_create_hash(ret);
}
