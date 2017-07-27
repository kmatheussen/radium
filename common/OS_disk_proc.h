
#ifndef COMMON_OS_DISK_PROC_H
#define COMMON_OS_DISK_PROC_H

#include "OS_string_proc.h"


extern LANGSPEC bool DISK_file_exists(const wchar_t *filename);


// OPEN
extern LANGSPEC disk_t *DISK_open_for_writing(const wchar_t *filename);
extern LANGSPEC disk_t *DISK_open_for_reading(const wchar_t *filename);
extern LANGSPEC disk_t *DISK_open_binary_for_reading(const wchar_t *filename);

extern LANGSPEC disk_t *DISK_open_temp_for_writing(void);
extern LANGSPEC wchar_t *DISK_close_temp_for_writing(disk_t *disk);

extern LANGSPEC wchar_t *DISK_get_filename(disk_t *disk);
  

// WRITE TEXT
extern LANGSPEC int DISK_write_wchar(disk_t *disk, const wchar_t *data);
extern LANGSPEC int DISK_write(disk_t *disk, const char *data);

#define DISK_printf(disk, ...) DISK_write(disk, talloc_format(__VA_ARGS__))


// READ TEXT
extern LANGSPEC int DISK_get_curr_read_line(disk_t *disk);
extern LANGSPEC wchar_t *DISK_read_wchar_line(disk_t *disk);
extern LANGSPEC char *DISK_readline(disk_t *disk);
extern LANGSPEC char *DISK_read_trimmed_line(disk_t *disk);


// READ BINARY
extern LANGSPEC bool DISK_set_pos(disk_t *disk, int64_t pos); // File must have been opened with 'DISK_open_binary_for_reading'.
extern LANGSPEC bool DISK_spool(disk_t *disk, int64_t how_much); // File must have been opened with 'DISK_open_binary_for_reading'.
extern LANGSPEC int64_t DISK_pos(disk_t *disk); // File must have been opened with 'DISK_open_binary_for_reading'.
extern LANGSPEC int64_t DISK_read_binary(disk_t *disk, void *destination, int64_t num_bytes); // return actual number of bytes read. File must have been opened with 'DISK_open_binary_for_reading'.


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
