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

#include "../common/nsmtracker.h"
#include "../common/OS_settings_proc.h"

#include "../common/OS_error_proc.h"

#include <stdio.h>
#include <stdarg.h>
#include <time.h>


static double get_ms(void){
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return ts.tv_sec * 1000.0 + ((double)ts.tv_nsec) / 1000000.0;
}


bool Error_init(void){return true;}

enum{
  IS_ERROR,
  IS_WARNING
};

static void show_message(int type, char *message){
  static double last_ignore = 0.0;
  static bool ignore_rest_of_the_program = false;

  char *typestring = type==IS_ERROR?"Error":"Warning";

  fprintf(stderr,"%s: %s\n",typestring, message);

  if(ignore_rest_of_the_program==true)
    return;

  if(get_ms()-last_ignore < 2000)
    return;

  {
    char command[1000];
    sprintf(command,
            "%s/packages/"
            "xmessage-1.0.3/xmessage -buttons continue:0,quit:1,\"ignore warnings and errors for two seconds\":2,\"ignore warnings and errors for the rest of the program\":3"
            "-nearmouse -print -default yes \"%s: %s.\"",
            OS_get_program_path(),
            typestring,
            message);

    int ret=system(command)>>8;

    switch(ret){
    case 0: break;
    case 1: abort();
    case 2: last_ignore=get_ms(); break;
    case 3: ignore_rest_of_the_program=true; break;
    }
  }
}

void RError(const char *fmt,...){
  char message[1000];
  va_list argp;
  
  va_start(argp,fmt);
  /*	vfprintf(stderr,fmt,argp); */
  vsprintf(message,fmt,argp);
  va_end(argp);

  show_message(IS_ERROR,message);
}

void RWarning(const char *fmt,...){
  char message[1000];
  va_list argp;
  
  va_start(argp,fmt);
  /*	vfprintf(stderr,fmt,argp); */
  vsprintf(message,fmt,argp);
  va_end(argp);

  show_message(IS_WARNING,message);
}

void RWarning_not_prod(const char *fmt,...){
#ifndef RELEASE
  char message[1000];
  va_list argp;
  
  va_start(argp,fmt);
  /*	vfprintf(stderr,fmt,argp); */
  vsprintf(message,fmt,argp);
  va_end(argp);

  show_message(IS_WARNING,message);
#endif
}

void Error_uninit(void){}

#endif // __linux__
