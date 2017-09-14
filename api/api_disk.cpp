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

#include "../common/nsmtracker.h"

#include "../common/OS_disk_proc.h"
#include "../common/OS_string_proc.h"
#include "../common/visual_proc.h"

#include "api_common_proc.h"

#include "radium_proc.h"


static int64_t g_curr_disknum = 0;

static QHash<int64_t,disk_t*> g_disks;


static QString w_to_qstring(const_char* w_path){
  return STRING_get_qstring(STRING_fromBase64(STRING_create(w_path)));
}

static const_char* qstring_to_w(const QString path){
  return talloc_strdup(path.toUtf8().toBase64().constData());
}


const_char* getPath(const_char *path_string){
  return toBase64(path_string);
}

const_char* getHomePath(void){
  return qstring_to_w(OS_get_home_path());
}

const_char* getProgramPath(void){
  return qstring_to_w(QCoreApplication::applicationDirPath());
}

const_char* getPathString(const_char* w_path){
  return fromBase64(w_path);
}

const_char* appendFilePaths(const_char* w_path1, const_char* w_path2){
  return qstring_to_w(w_to_qstring(w_path1) + QDir::separator() + w_to_qstring(w_path2));
}

bool fileExists(const_char* w_path){
  QString filename = w_to_qstring(w_path);
  return QFileInfo::exists(filename);
}

int64_t openFileForReading(const_char* w_path){
  disk_t *disk = DISK_open_for_reading(w_to_qstring(w_path));
  if (disk==NULL){
    handleError("Unable to open file %s for reading\n", w_to_qstring(w_path).toUtf8().constData());
    return -1;
  }

  int64_t disknum = ++g_curr_disknum;
  g_disks[disknum] = disk;
  return disknum;
}

int64_t openFileForWriting(const_char* w_path){
  disk_t *disk = DISK_open_for_writing(w_to_qstring(w_path));
  if (disk==NULL){
    handleError("Unable to open file for writing (couldn't create temporary file, check your file system)");
    return -1;
  }

  int64_t disknum = ++g_curr_disknum;
  g_disks[disknum] = disk;
  return disknum;
}

bool closeFile(int64_t disknum){
  disk_t *disk = g_disks[disknum];
  if (disk==NULL){
    handleError("closeFile: No file #%d", (int)disknum);
    return false;
  }

  g_disks.remove(disknum);

  return DISK_close_and_delete(disk);
}

int writeToFile(int64_t disknum, const_char* string){
  disk_t *disk = g_disks[disknum];
  if (disk==NULL){
    handleError("writeToFile: No file #%d", (int)disknum);
    return -1;
  }

  int bytes_written = DISK_write(disk, string);
  int string_len = (int)strlen(string);
  if (bytes_written != string_len){
    handleError("writeToFile: Unable to write all bytes to file. (%d / %d)", bytes_written, string_len);
  }

  return bytes_written;
}

bool fileAtEnd(int64_t disknum){
  disk_t *disk = g_disks[disknum];
  if (disk==NULL){
    handleError("writeToFile: No file #%d", (int)disknum);
    return false;
  }

  return DISK_at_end(disk);
}

const_char* readLineFromFile(int64_t disknum){
  disk_t *disk = g_disks[disknum];
  if (disk==NULL){
    handleError("writeToFile: No file #%d", (int)disknum);
    return "";
  }

  const_char* ret = DISK_read_trimmed_line(disk);
  if (ret==NULL){
    handleError("readLineFromFile: Attempting to read past end of file");
    return "";
  }

  return ret;
}



// read binary
#include "../common/read_binary.h"


int64_t openFileForBinaryReading(const_char* w_path){
  disk_t *disk = DISK_open_binary_for_reading(STRING_create(w_to_qstring(w_path)));
  if (disk==NULL){
    handleError("Unable to open file %s for reading\n", w_to_qstring(w_path).toUtf8().constData());
    return -1;
  }

  R_ASSERT(DISK_is_binary(disk));

  int64_t disknum = ++g_curr_disknum;
  g_disks[disknum] = disk;
  return disknum;
}


static bool read_binary(const_char* funcname, int64_t disknum, unsigned char dest[], int64_t num_bytes){
  disk_t *disk = g_disks[disknum];
  if (disk==NULL){
    handleError("%s: No file #%d", funcname, (int)disknum);
    return false;
  }

  if (DISK_is_binary(disk)==false){
    handleError("%s: File #%d is not opened in binary mode", funcname, (int)disknum);
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

int readLe32FromFile(int64_t disknum){
  unsigned char chars[4];
  if(read_binary("readLe32FromFile", disknum, chars, 4)==false)
    return 0;

  return get_le_32(chars);
}

int64_t readLeU32FromFile(int64_t disknum){
  unsigned char chars[4];
  if(read_binary("readLeU32FromFile", disknum, chars, 4)==false)
    return 0;

  return get_le_u32(chars);
}

/*
// there is no get_be_32 function
int readBe32FromFile(int64_t disknum){
  unsigned char chars[4];
  if(read_binary("readLe32FromFile", disknum, chars, 4)==false)
    return 0;

  return get_be_32(chars);
}
*/

int64_t readBeU32FromFile(int64_t disknum){
  unsigned char chars[4];
  if(read_binary("readLeU32FromFile", disknum, chars, 4)==false)
    return 0;

  return get_be_u32(chars);
}

// 16 bit

int readLe16FromFile(int64_t disknum){
  unsigned char chars[2];
  if(read_binary("readLe16FromFile", disknum, chars, 2)==false)
    return 0;

  return get_le_16(chars);
}

/*
// there is no get_le_u16 function
int readLeU16FromFile(int64_t disknum){
  unsigned char chars[2];
  if(read_binary("readLeU16FromFile", disknum, chars, 2)==false)
    return 0;

  return get_le_u16(chars);
}
*/

/*
// there is no get_be_16 function
int readBe16FromFile(int64_t disknum){
  unsigned char chars[2];
  if(read_binary("readBe16FromFile", disknum, chars, 2)==false)
    return 0;

  return get_be_16(chars);
}
*/

int readBeU16FromFile(int64_t disknum){
  unsigned char chars[2];
  if(read_binary("readBeU16FromFile", disknum, chars, 2)==false)
    return 0;

  return get_be_u16(chars);
}

// 8 bit

int read8FromFile(int64_t disknum){
  disk_t *disk = g_disks[disknum];
  if (disk==NULL){
    handleError("readU8FromFile: No file #%d", (int)disknum);
    return 0;
  }
  
  if (DISK_is_binary(disk)==false){
    handleError("read8FromFile: File #%d is not opened in binary mode", (int)disknum);
    return 0;
  }

  int8_t size_chars[1] = {};
  if(DISK_read_binary(disk, size_chars, 1) !=1 ){
    handleError("read8FromFile: unable to read from file #%d",(int)disknum);
    return 0;
  }

  return size_chars[0];
}

int readU8FromFile(int64_t disknum){
  unsigned char chars[1];
  if(read_binary("read8FromFile", disknum, chars, 1)==false)
    return 0;

  return chars[0];
}

