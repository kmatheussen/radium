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

#include <Python.h>

#include "../common/nsmtracker.h"

#include <string.h>
#include "../common/visual_proc.h"

#include "X11_ReqType_proc.h"
#include "X11_keyboard_proc.h"


void GFX_WriteString_Do(ReqType reqtype,char *text){
  char temp[500];
  sprintf(temp,"X11_ReqType.GFX_WriteString(0,\"%s\")",text);
  PyRun_SimpleString(temp);
}

static void GFX_WriteMultiLineString(ReqType reqtype,char *text){
  char temp[500];
  char temp2[500];
  int tempstart=0;
  int lokke=0;
  for(;;){
    if(text[lokke]=='\0' || text[lokke]=='\n'){
      temp[tempstart]='\0';
      if(text[lokke]=='\0'){
	sprintf(temp2,"X11_ReqType.GFX_WriteString(0,\"%s\")",temp);
	PyRun_SimpleString(temp2);
	return;
      }else if(strlen(temp)>0) {
	sprintf(temp2,"X11_ReqType.GFX_WriteString(0,\"%s\"" "\"\\n\""  ")",temp);
	PyRun_SimpleString(temp2);
      }
      tempstart=0;
    }else{
      temp[tempstart]=text[lokke];
      tempstart++;
    }
    lokke++;
  }
}


ReqType GFX_OpenReq(struct Tracker_Windows *tvisual,int width,int height,char *title){
  char temp[500];
  static bool pythonsourceimported=false;

  if(pythonsourceimported==false){
    PyRun_SimpleString("import X11_ReqType");
    pythonsourceimported=true;
  }

  sprintf(temp,"X11_ReqType.GFX_OpenReqType(%d,%d,\"%s\")",width,height,title);
  PyRun_SimpleString(temp);
  return (ReqType)1;
}

void GFX_CloseReq(struct Tracker_Windows *tvisual,ReqType reqtype){
  PyRun_SimpleString("X11_ReqType.GFX_CloseReqType(0)");
  if(tvisual!=NULL)
    GFX_EditorWindowToFront(tvisual);
}


void GFX_WriteString(ReqType reqtype,char *text){
#if 1
  GFX_WriteMultiLineString(reqtype,text);
#else
  char temp[500];
  sprintf(temp,"X11_ReqType.GFX_WriteString(0,\"%s\")",text);
  PyRun_SimpleString(temp);
#endif
}


char *GFX_ReadStringFromPythonCommand(char *pythoncommand){
  FILE *file;
  char filename[50];
  char temp[strlen(pythoncommand)+500];
  char *ret=talloc_atomic(strlen(pythoncommand)+500);
  ret[0] = '\0';

  sprintf(filename,"/tmp/radiumipctempfileXXXXXX");
  mkstemp(filename);

  sprintf(temp,pythoncommand,filename);


  PyRun_SimpleString(temp);


  file=fopen(filename,"r");
  if(file==NULL) {
    RError("GFX_ReadStringFromPythonCommand: File is null -%s-.\n",filename);
    return ret;
  }

  fgets(ret,strlen(pythoncommand)+499,file);
  fclose(file);

  printf("Tried to read -%s-\n",ret);
  sprintf(temp,"rm %s",filename);
  if(system(temp)==-1)
    RWarning("Unable to delete \"%s\"",filename);
  
  return ret;
}


void GFX_ReadString(ReqType reqtype,char *buffer,int bufferlength){

  sprintf(buffer,GFX_ReadStringFromPythonCommand("X11_ReqType.GFX_ReadString(\"%s\")"));

  //printf("resetting keys\n");
  //X11_ResetKeysUpDowns();

#if 0

  FILE *file;
  char filename[]="/tmp/radiumreqtempfileXXXXXX";
  char temp[500];


  mkstemp(filename);

  sprintf(temp,"X11_ReqType.GFX_ReadString(\"%s\")",filename);

  PyRun_SimpleString(temp);

  file=fopen(filename,"r");
  fgets(buffer,bufferlength,file);
  fclose(file);

  sprintf(temp,"rm %s",filename);
  if(system(temp)==-1)
    RWarning("Unable to delete \"%s\"",filename);

#endif
}

