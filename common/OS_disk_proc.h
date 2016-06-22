
#ifndef COMMON_OS_DISK_PROC_H
#define COMMON_OS_DISK_PROC_H

#include "OS_string_proc.h"

typedef struct _radium_os_disk disk_t;

extern LANGSPEC bool DISK_file_exists(const wchar_t *filename);


// OPEN
extern LANGSPEC disk_t *DISK_open_for_writing(const wchar_t *filename);
extern LANGSPEC disk_t *DISK_open_for_reading(const wchar_t *filename);
extern LANGSPEC disk_t *DISK_open_binary_for_reading(const wchar_t *filename);


// WRITE TEXT
extern LANGSPEC int DISK_write_wchar(disk_t *disk, const wchar_t *data);
extern LANGSPEC int DISK_write(disk_t *disk, const char *data);

#define DISK_printf(disk, fmt, ...) DISK_write(disk, talloc_format(fmt, __VA_ARGS__))


// READ TEXT
extern LANGSPEC wchar_t *DISK_read_wchar_line(disk_t *disk);
extern LANGSPEC char *DISK_readline(disk_t *disk);
extern LANGSPEC char *DISK_read_trimmed_line(disk_t *disk);


// READ BINARY
extern LANGSPEC bool DISK_set_pos(disk_t *disk, int64_t pos); // File must have been opened with 'DISK_open_binary_for_reading'.
extern LANGSPEC bool DISK_spool(disk_t *disk, int64_t how_much); // File must have been opened with 'DISK_open_binary_for_reading'.
extern LANGSPEC int64_t DISK_pos(disk_t *disk); // File must have been opened with 'DISK_open_binary_for_reading'.
extern LANGSPEC int DISK_read_binary(disk_t *disk, void *destination, int num_bytes); // return actual number of bytes read. File must have been opened with 'DISK_open_binary_for_reading'.


// CLOSE
extern LANGSPEC bool DISK_close_and_delete(disk_t *disk);


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
#endif



extern void SaveOsStuff(void);
extern void LoadOsStuff(void);

#endif // COMMON_OS_DISK_PROC_H
