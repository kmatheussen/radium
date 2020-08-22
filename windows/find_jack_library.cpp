#ifdef _WIN32

#include <windows.h>

#include <QCoreApplication>
#include <QString>
#include <QDir>
#include <QFileInfo>

#include "find_jack_library_proc.h"


# ifdef __x86_64__
   #define LIBJACKNAME "libjack64.dll"
# else
   #define LIBJACKNAME "libjack.dll"
# endif


static wchar_t *get_wchar_t(const QString s){
  int size = (int)sizeof(wchar_t)*(s.length()+1);
  wchar_t *array = (wchar_t*)malloc(size);
  memset(array, 0, size);
  s.toWCharArray(array);
  return array;
}

static QString find_libjack_dir2(void){
  return QCoreApplication::applicationDirPath() + QDir::separator() + "jack_local";
}

wchar_t *find_libjack_dir(void){
  return get_wchar_t(find_libjack_dir2());
}

bool jack_is_installed_globally(void){
  static bool has_found_value = false;
  static bool is_installed_globally = false;

  if (has_found_value == false) {
    HMODULE lib = LoadLibraryA(LIBJACKNAME);
    
    if (lib != NULL){
      FreeLibrary(lib);
      is_installed_globally = true;
    } else {
      is_installed_globally = false;
    }

    has_found_value = true;
  }

  return is_installed_globally;
}

wchar_t *find_libjack_library(bool jack_is_installed_globally){
  if (jack_is_installed_globally) {
    
    return get_wchar_t(LIBJACKNAME);
    
  } else {
    
#ifdef _WIN64
    return get_wchar_t(find_libjack_dir2() + QDir::separator() + "libjack64.dll");
#else
    return get_wchar_t(find_libjack_dir2() + QDir::separator() + "win32libs" + QDir::separator() + "libjack.dll");
#endif
    
  }
  
}

#endif // _WIN32

