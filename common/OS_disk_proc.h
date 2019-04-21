
#ifndef COMMON_OS_DISK_PROC_H
#define COMMON_OS_DISK_PROC_H

#include "OS_string_proc.h"

// Some helper functions. The file API in Qt is quite often ridiculous.
extern LANGSPEC const wchar_t *DISK_get_absolute_dir_path(const wchar_t *wfilename);
extern LANGSPEC const wchar_t *DISK_get_absolute_file_path(const wchar_t *wfilename);
extern LANGSPEC const wchar_t *DISK_get_pathless_file_path(const wchar_t *wfilename);
extern LANGSPEC int64_t DISK_get_creation_time(const wchar_t *wfilename);
extern LANGSPEC bool DISK_file_exists(const wchar_t *filename);
extern LANGSPEC bool DISK_dir_exists(const wchar_t *dirname);
extern LANGSPEC bool DISK_create_dir(const wchar_t *wdirname);
extern LANGSPEC bool DISK_delete_file(const wchar_t *filename);
extern LANGSPEC void DISK_delete_all_files_in_dir(const wchar_t *wdirname);
extern LANGSPEC const wchar_t *DISK_get_dir_separator(void);
extern LANGSPEC const wchar_t *DISK_create_non_existant_filename(const wchar_t *filename);
extern LANGSPEC const wchar_t *DISK_get_temp_dir(void);
extern LANGSPEC bool DISK_copy(const wchar_t *old_file, const wchar_t *new_file);
extern LANGSPEC const wchar_t *DISK_copy_to_temp_file(const wchar_t *old_file);

// OPEN
extern LANGSPEC disk_t *DISK_open_for_writing(const wchar_t *filename);
extern LANGSPEC disk_t *DISK_open_for_reading(const wchar_t *filename);
extern LANGSPEC disk_t *DISK_open_binary_for_reading(const wchar_t *filename);
extern LANGSPEC disk_t *DISK_open_binary_for_writing(const wchar_t *wfilename);

extern LANGSPEC disk_t *DISK_open_temp_for_writing(void);
extern LANGSPEC wchar_t *DISK_close_temp_for_writing(disk_t *disk);

// If disk==NULL, then return reason file couldn't be opened.
// else, Returns NULL if there is no error.
extern LANGSPEC const char* DISK_get_error(disk_t *disk);

extern LANGSPEC wchar_t *DISK_get_filename(disk_t *disk);
  

// WRITE TEXT
extern LANGSPEC int DISK_write_wchar(disk_t *disk, const wchar_t *data);
extern LANGSPEC int DISK_write(disk_t *disk, const char *data);

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
extern LANGSPEC int64_t DISK_write_binary(disk_t *disk, const void *source, int64_t num_bytes);

// CLOSE
extern LANGSPEC bool DISK_close_and_delete(disk_t *disk);

// READ AND WRITE FILES TO/FROM BASE64 STRINGS
extern LANGSPEC const char *DISK_file_to_base64(const wchar_t *wfilename); // Returns NULL if error.
extern LANGSPEC const wchar_t *DISK_base64_to_file(const wchar_t *wfilename, const char *chars); // If wfilename==NULL, the name of a temporary file is returned. Returns NULL if error.
extern LANGSPEC void DISK_delete_base64_file(const wchar_t *wfilename); // Deletes temporary file created by a successfull call to DISK_base64_to_file(NULL,...).
extern LANGSPEC void DISK_cleanup(void);

#include "OS_disk2_proc.h"



#ifdef USE_QT4
#include <QString>

extern disk_t *DISK_open_for_writing(QString filename);
extern disk_t *DISK_open_for_reading(QString filename);

extern disk_t *DISK_open_binary_for_writing(QString filename);

extern QString g_file_at_end;
QString DISK_read_qstring_line(disk_t *disk); // returns g_file_at_end if end of file
int DISK_write_qstring(disk_t *disk, QString s);

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

static inline QString DISK_file_to_qstring(QString filename){
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
