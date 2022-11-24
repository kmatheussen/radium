#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>
  
bool jack_is_installed_globally(void);
wchar_t *find_libjack_dir(void);
wchar_t *find_libjack_library(bool jack_is_installed_globally);

#ifdef __cplusplus
}
#endif
