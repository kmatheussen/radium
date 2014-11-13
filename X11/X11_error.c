/* Copyright 2003 Kjetil S. Matheussen

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

#ifdef __linux__

#include <stdio.h>
#include <stdarg.h>


#include "../common/nsmtracker.h"
#include "../common/OS_settings_proc.h"

#include "../common/OS_error_proc.h"

int SYSTEM_show_message(char *message){

  char command[1000];
  sprintf(command,
          "%s/packages/"
          "xmessage-1.0.3/xmessage -buttons continue:0,quit:1,\"ignore warnings and errors for two seconds\":2,\"ignore warnings and errors for the rest of the program\":3"
          "-nearmouse -print -default yes \"%s.\"",
          OS_get_program_path(),
          message);
  
  fprintf(stderr,"command: %s\n",command);
  //abort();
  
  return system(command)>>8;  
}

#endif // __linux__
