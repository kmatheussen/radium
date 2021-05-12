
#ifndef COMMON_OS_STRING_PROC_H
#define COMMON_OS_STRING_PROC_H

//typedef struct _string string_t;

// implemented in Qt/Qt_Settings.cpp
extern LANGSPEC const wchar_t *STRING_create(const char *s);

//extern LANGSPEC wchar_t *STRING_copy(const wchar_t *s);
static inline const wchar_t *STRING_copy(const wchar_t *s){
  return talloc_wcsdup(s);
}

static inline int STRING_length(const wchar_t *s){
  return wcslen(s);
}

extern LANGSPEC const char* STRING_get_chars(const wchar_t *string);
extern LANGSPEC const char* STRING_get_utf8_chars(const char* s);

extern LANGSPEC bool STRING_starts_with(const wchar_t *string, const char *startswith);
extern LANGSPEC bool STRING_starts_with2(const wchar_t *string, const wchar_t *startswith);
extern LANGSPEC const wchar_t *STRING_remove_starts_with(const wchar_t *string, const char *startswith);
extern LANGSPEC bool STRING_ends_with(const wchar_t *string, const char *endswiths);
extern LANGSPEC bool STRING_equals(const wchar_t *string, const char *s2);
extern LANGSPEC bool STRING_equals2(const wchar_t *string, const wchar_t *s2);
extern LANGSPEC const wchar_t *STRING_replace(const wchar_t *string, const char *a, const char *b);

extern LANGSPEC const wchar_t *STRING_append(const wchar_t *s1, const wchar_t *w2);

extern LANGSPEC const wchar_t *STRING_to_upper(const wchar_t *string);
extern LANGSPEC const wchar_t *STRING_to_lower(const wchar_t *string);

extern LANGSPEC int STRING_find_pos(const wchar_t *string, int start_pos, const char *what_to_find); // returns -1 if not found.
extern LANGSPEC const wchar_t *STRING_remove_start(const wchar_t *string, int new_start_pos);
extern LANGSPEC const wchar_t *STRING_remove_end(const wchar_t *string, int new_end_pos);


extern LANGSPEC const wchar_t *STRING_toBase64(const wchar_t *s);
extern LANGSPEC const wchar_t *STRING_fromBase64(const wchar_t *encoded);
extern LANGSPEC const wchar_t *STRING_get_sha1(const wchar_t *string);

extern LANGSPEC const wchar_t *STRING_trim(const wchar_t *string);

#ifndef TEST_PLACEMENT

#include "OS_settings_proc.h"

static inline int STRING_get_int(const wchar_t *string){
  return atoi(STRING_get_chars(string));
}
static inline int64_t STRING_get_int64(const wchar_t *string){
  return atoll(STRING_get_chars(string));
}
static inline double STRING_get_double(const wchar_t *string){
  return OS_get_double_from_string(STRING_get_chars(string));
}

#endif


#ifdef USE_QT4

#include <QString>

const wchar_t *STRING_append(const wchar_t *s1, const char *w2);

const wchar_t *STRING_create(const QString s, bool use_gc_alloc = true);

static inline QString STRING_get_qstring(const wchar_t *string){ // TODO: Rename to STRING_create_qstring.
  if (string==NULL)
    return QString("");
  else
    return QString::fromWCharArray(string);
}

bool STRING_is_local8Bit_compatible(QString s);

#endif

#ifdef __cplusplus

namespace radium{

#define DEBUG_RADIUM_STRING 0

#if DEBUG_RADIUM_STRING
#define D(A) A
#else
#define D(A)
#endif

// Horrible. Will replace with a new one using radium::GcHolder instead.
// Copy constructors, manual reference counters and so forth are funny concepts, but not something there's any sober reason to have to deal with with when making non-programming programs.
// Proper behind-the-scene garbage collectors is what you want in real life programming.
struct String{

private:
  struct InternalString{
    
    const wchar_t *string;
    int num_users;

    InternalString(const wchar_t *string)
      : string(V_wcsdup(string))
      , num_users(1)
    {
      D(printf("... Created \"%S\"\n", this->string));
    }
    
    ~InternalString(){    
      D(printf("... Destroyed \"%S\"\n", string));
      V_free(const_cast<wchar_t*>(string));
    }

    void inc(void){
      num_users++;
    }
    
    void dec(void){
      num_users--;
      if (num_users==0)
        delete this;
    }
    
    mutable int _length = -1;
    
    int length(void) const {
      if (_length==-1)
        _length = wcslen(string);

      return _length;
    }
  };

  InternalString *string;

public:

  String(const wchar_t *string)
    : string(new InternalString(string))
  {
    R_ASSERT(THREADING_is_main_thread());
    D(printf("  String(%S): %d\n", get(), (int)this->string->num_users));
  }

  String()
    : String(L"")
  {}
  
  String(const char *string)
    : String(STRING_create(string))
  {
  }

  String(const String& old_string)
    : string(old_string.string)
  {
    R_ASSERT(THREADING_is_main_thread());
    
    string->inc();
    
    D(printf("  String(%S): %d\n", get(), string->num_users));
  }

  String& operator=(const String &copy){
    R_ASSERT(THREADING_is_main_thread());

    D(printf("   String::operator=. Old: -%S-. New: -%S-\n", get(), copy.get()));
      
    copy.string->inc();
    string->dec();
    string = copy.string;
    
    return *this;
  }

  String& operator=(const wchar_t *string){
    return operator=(String(string));
  }
    
  ~String(){
    R_ASSERT(THREADING_is_main_thread());

    D(printf("     ~String(%S): %d\n", get(), string->num_users-1));
    
    string->dec();
  }

  const wchar_t* get(void) const {
    R_ASSERT_NON_RELEASE(THREADING_is_main_thread());
    return string->string;
  }

  const wchar_t* get_from_another_thread(void) const {
    R_ASSERT_NON_RELEASE(!THREADING_is_main_thread() || PLAYER_current_thread_has_lock());
    return string->string;
  }

  int length(void) const {
    return string->length();
  }

  bool is_empty(void) const {
    return length()==0;
  }
  
  /*
  operator const wchar_t*() const {
    R_ASSERT(THREADING_is_main_thread());
    return string->string;
  }
  */
};
}

#undef D

#endif


#endif // COMMON_OS_STRING_PROC_H
