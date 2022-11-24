/* Copyright 2012 Kjetil S. Matheussen

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


#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "../common/nsmtracker.h"
#include "../common/OS_settings_proc.h"

#if 0
// This function is moved to Qt/Qt_settings.c
char *OS_get_config_filename(void){
#if __linux__ || defined(FOR_MACOSX)
  char temp[500];
  sprintf(temp,"mkdir %s/.radium 2>/dev/null",getenv("HOME"));
  if(system(temp)==-1)
    GFX_Message(NULL, "Unable to create \"%s/.radium\" directory",getenv("HOME"));

  return talloc_format("%s/.radium/config",getenv("HOME"));
#endif // __linux__

#ifdef FOR_WINDOWS
  return talloc_strdup("config");
#endif
}
#endif

#if 0
void OS_make_config_file_expired(const char *key){
  char *config_file = OS_get_config_filename(key);
  char temp[500];
  sprintf(temp,"%s_bu",config_file);
  rename(config_file,temp);
}
#endif
