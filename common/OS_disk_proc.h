
#ifndef COMMON_OS_DISK_PROC_H
#define COMMON_OS_DISK_PROC_H

#include "OS_string_proc.h"

// Some helper functions. The file API in Qt is quite often ridiculous.
extern LANGSPEC filepath_t DISK_get_absolute_dir_path(filepath_t wfilename);
extern LANGSPEC filepath_t DISK_get_absolute_file_path(filepath_t wfilename);
extern LANGSPEC filepath_t DISK_get_pathless_file_path(filepath_t wfilename);
extern LANGSPEC filepath_t DISK_get_filename_without_suffix(filepath_t wfilename);
extern LANGSPEC filepath_t DISK_get_filename_without_path_and_suffix(filepath_t wfilename);
extern LANGSPEC filepath_t DISK_create_legal_filename(filepath_t filename);
extern LANGSPEC int64_t DISK_get_creation_time(filepath_t wfilename);
extern LANGSPEC bool DISK_file_exists(filepath_t filename);
extern LANGSPEC bool DISK_dir_exists(filepath_t dirname);
extern LANGSPEC bool DISK_create_dir(filepath_t wdirname);
extern LANGSPEC filepath_t DISK_create_unique_filename(filepath_t template_);
extern LANGSPEC filepath_t DISK_get_final_symlink_target(filepath_t filepath, bool show_error); // traverses all the way down to the actual file.
extern LANGSPEC filepath_t DISK_link_copy_file(filepath_t dirname, filepath_t filename, bool show_error); // creates a unique symlink to filename in dirname if filename isn't placed in dirname.
extern LANGSPEC bool DISK_delete_file(filepath_t filename);
extern LANGSPEC void DISK_delete_all_files_in_dir(filepath_t wdirname);
extern LANGSPEC const wchar_t *DISK_get_dir_separator(void);
extern LANGSPEC filepath_t DISK_create_non_existant_filename(filepath_t filename);
extern LANGSPEC filepath_t DISK_get_temp_dir(void);
extern LANGSPEC bool DISK_copy(filepath_t old_file, filepath_t new_file);
extern LANGSPEC filepath_t DISK_copy_to_temp_file(filepath_t old_file);

// OPEN
extern LANGSPEC disk_t *DISK_open_for_writing(filepath_t filename);
extern LANGSPEC disk_t *DISK_open_for_reading(filepath_t filename);
extern LANGSPEC disk_t *DISK_open_binary_for_reading(filepath_t filename);
extern LANGSPEC disk_t *DISK_open_binary_for_writing(filepath_t wfilename);

extern LANGSPEC disk_t *DISK_open_temp_for_writing(void);
extern LANGSPEC wchar_t *DISK_close_temp_for_writing(disk_t *disk);

// If disk==NULL, then return reason file couldn't be opened.
// else, Returns NULL if there is no error.
extern LANGSPEC const char* DISK_get_error(disk_t *disk);

extern LANGSPEC filepath_t DISK_get_filename(disk_t *disk);
  

extern LANGSPEC bool DISK_write_wchar(disk_t *disk, const wchar_t *data); // returns false if writing failed. Call DISK_error() to get error message.
extern LANGSPEC bool DISK_write(disk_t *disk, const char *data); // returns false if writing failed. Call DISK_error() to get error message.

#define DISK_printf(disk, ...) DISK_write(disk, talloc_format(__VA_ARGS__))


// READ TEXT
extern LANGSPEC bool DISK_at_end(disk_t *disk);
extern LANGSPEC int DISK_get_curr_read_line(disk_t *disk);
extern LANGSPEC wchar_t *DISK_read_wchar_line(disk_t *disk);
extern LANGSPEC char *DISK_readline(disk_t *disk);
extern LANGSPEC char *DISK_read_trimmed_line(disk_t *disk);


// READ BINARY
extern LANGSPEC bool DISK_set_pos(disk_t *disk, int64_t pos); // File must have been opened with 'DISK_open_binary_for_reading'.
extern LANGSPEC bool DISK_spool(disk_t *disk, int64_t how_much); // File must have been opened with 'DISK_open_binary_for_reading'.
extern LANGSPEC int64_t DISK_pos(disk_t *disk); // File must have been opened with 'DISK_open_binary_for_reading'.
extern LANGSPEC bool DISK_is_binary(disk_t *disk);
extern LANGSPEC int64_t DISK_read_binary(disk_t *disk, void *destination, int64_t num_bytes); // return actual number of bytes read. File must have been opened with 'DISK_open_binary_for_reading'.

// WRITE BINARY
extern LANGSPEC bool DISK_write_binary(disk_t *disk, const void *source, int64_t num_bytes); // returns false if writing failed. Call DISK_error() to get error message.

// CLOSE
extern LANGSPEC bool DISK_close_and_delete(disk_t *disk);

// READ AND WRITE FILES TO/FROM BASE64 STRINGS
extern LANGSPEC const char *DISK_file_to_base64(filepath_t wfilename); // Returns NULL if error.
extern LANGSPEC filepath_t DISK_base64_to_file(filepath_t wfilename, const char *chars); // If wfilename==NULL, the name of a temporary file is returned. Returns NULL if error.
extern LANGSPEC void DISK_delete_base64_file(filepath_t wfilename); // Deletes temporary file created by a successfull call to DISK_base64_to_file(NULL,...).
extern LANGSPEC void DISK_cleanup(void);

#include "OS_disk2_proc.h"

#ifdef __cplusplus
namespace radium{
  
  struct ScopedReadFile{
    disk_t *_file;
    ScopedReadFile(filepath_t filename){
      _file = DISK_open_for_reading(filename);
    }

    ~ScopedReadFile(){
      if (_file != NULL)
        DISK_close_and_delete(_file);
    }
  };
  
  struct ScopedWriteFile{
    disk_t *_file;
    ScopedWriteFile(filepath_t filename){
      _file = DISK_open_for_writing(filename);
    }

    ~ScopedWriteFile(){
      if (_file != NULL)
        DISK_close_and_delete(_file);
    }
  };
}
#endif


#ifdef USE_QT4
#include <QString>

extern disk_t *DISK_open_for_writing(QString filename);
extern disk_t *DISK_open_for_reading(QString filename);

extern disk_t *DISK_open_binary_for_writing(QString filename);

extern QString g_file_at_end;
QString DISK_read_qstring_line(disk_t *disk); // returns g_file_at_end if end of file
bool DISK_write_qstring(disk_t *disk, QString s); // returns false if writing failed. Call DISK_error() to get error message.

static inline QString DISK_read_qstring_file(disk_t *disk){
  QString res;
    
  for(;;){
    QString line = DISK_read_qstring_line(disk);
    if (line == g_file_at_end)
      break;
    else
      res += line + "\n";      
  }

  return res;
}

static inline QString DISK_file_to_qstring(filepath_t filename){
  disk_t *disk = DISK_open_for_reading(filename);
  R_ASSERT_RETURN_IF_FALSE2(disk!=NULL, "");
  QString ret = DISK_read_qstring_file(disk);
  DISK_close_and_delete(disk);
  return ret;
}
#endif




extern void SaveOsStuff(void);
extern void LoadOsStuff(void);

#endif // COMMON_OS_DISK_PROC_H
