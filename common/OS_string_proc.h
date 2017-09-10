
#ifndef COMMON_OS_STRING_PROC_H
#define COMMON_OS_STRING_PROC_H

#include "OS_settings_proc.h"


//typedef struct _string string_t;

// implemented in Qt/Qt_Settings.cpp
extern LANGSPEC wchar_t *STRING_create(const char *s);
extern LANGSPEC wchar_t *STRING_copy(const wchar_t *s);

extern LANGSPEC char* STRING_get_chars(const wchar_t *string);
extern LANGSPEC char* STRING_get_utf8_chars(const char* s);
  
extern LANGSPEC bool STRING_starts_with(const wchar_t *string, const char *endswith);
extern LANGSPEC bool STRING_ends_with(const wchar_t *string, const char *s);
extern LANGSPEC bool STRING_equals(const wchar_t *string, const char *s2);
extern LANGSPEC bool STRING_equals2(const wchar_t *string, const wchar_t *s2);
extern LANGSPEC wchar_t *STRING_replace(const wchar_t *string, const char *a, const char *b);

static inline int STRING_get_int(const wchar_t *string){
  return atoi(STRING_get_chars(string));
}
static inline int64_t STRING_get_int64(const wchar_t *string){
  return atoll(STRING_get_chars(string));
}
static inline double STRING_get_double(const wchar_t *string){
  return OS_get_double_from_string(STRING_get_chars(string));
}
extern LANGSPEC wchar_t *STRING_append(const wchar_t *s1, const wchar_t *w2);

extern LANGSPEC wchar_t *STRING_toBase64(const wchar_t *s);
extern LANGSPEC wchar_t *STRING_fromBase64(const wchar_t *encoded);
extern LANGSPEC const wchar_t *STRING_get_sha1(const wchar_t *string);
  

#ifdef USE_QT4

#include <QString>

wchar_t *STRING_append(const wchar_t *s1, const char *w2);

wchar_t *STRING_create(const QString s, bool use_gc_alloc = true);

static inline QString STRING_get_qstring(const wchar_t *string){ // TODO: Rename to STRING_create_qstring.
  return QString::fromWCharArray(string);
}

bool STRING_is_local8Bit_compatible(QString s);

#endif



#endif // COMMON_OS_STRING_PROC_H
