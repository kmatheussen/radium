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

#include <stdio.h>
#include <stdarg.h>

#include "nsmtracker.h"
//#include "OS_settings_proc.h"
#include "visual_proc.h"
#include "OS_Player_proc.h"
#include "vector_proc.h"

#include "OS_error_proc.h"



bool Error_init(void){return true;}

enum{
  IS_ERROR,
  IS_WARNING
};

#ifndef RELEASE
static void show_message(int type, char *message){
  static double last_ignore = -3000.0; // don't want to ignore errors the first two seconds.
  static bool ignore_rest_of_the_program = false;

  char *typestring = type==IS_ERROR?"Error":"Warning";

  if(ignore_rest_of_the_program==true)
    return;

  if(TIME_get_ms()-last_ignore < 2000)
    return;

  char full_message[1000];
  sprintf(full_message,"%s: %s", typestring, message);

#if 0 // Always use SYSTEM_show_message.
  vector_t v = {0};
  VECTOR_push_back(&v, "continue");
  VECTOR_push_back(&v, "quit");
  VECTOR_push_back(&v, "ignore warnings and errors for two seconds");
  VECTOR_push_back(&v, "ignore warnings and errors for the rest of the program");
  
  int ret = GFX_Message(&v, full_message);
  if (ret==-1)
    ret = SYSTEM_show_message(full_message);
#else
  int ret = SYSTEM_show_message(full_message);
#endif
      
  switch(ret){
  case 0: break;
  case 1: {
    char *hello = NULL;
    hello[0] = 50;
    abort();
  }
  case 2: last_ignore=TIME_get_ms(); break;
  case 3: ignore_rest_of_the_program=true; break;
  }
}
#endif

void RError_internal(const char *fmt,...){
  char message[1000];
  va_list argp;
  
  va_start(argp,fmt);
  /*	vfprintf(stderr,fmt,argp); */
  vsnprintf(message,998,fmt,argp);
  va_end(argp);

  CRASHREPORTER_send_assert_message(CT_ERROR, "RError: %s",message);
  //show_message(IS_ERROR,message);
}

void RWarning_internal(const char *fmt,...){
  char message[1000];
  va_list argp;
  
  va_start(argp,fmt);
  /*	vfprintf(stderr,fmt,argp); */
  vsnprintf(message,998,fmt,argp);
  va_end(argp);

  CRASHREPORTER_send_assert_message(CT_WARNING, "RWarning: %s",message);
  //show_message(IS_WARNING,message);
}

void RWarning_not_prod_internal(const char *fmt,...){
#ifndef RELEASE
  char message[1000];
  va_list argp;
  
  va_start(argp,fmt);
  /*	vfprintf(stderr,fmt,argp); */
  vsnprintf(message,998,fmt,argp);
  va_end(argp);

  show_message(IS_WARNING,message);
#endif
}

void Error_uninit(void){}
