/* Copyright 2003-2012 Kjetil S. Matheussen

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

// Hmm, dont think this file is used.

#ifndef GUIISQT

void X11_StartQtStuff(void){
  PyRun_SimpleString("import X11_Qtstuff");
  PyRun_SimpleString("X11_Qtstuff.GFX_StartQtstuff()");
}


char *GFX_GetLoadFileName(
	struct Tracker_Windows *tvisual,
	ReqType reqtype,
	char *seltext,
	char *dir
){
  char *ret=GFX_ReadStringFromPythonCommand("X11_Qtstuff.GFX_OpenFileRequester(\"%s\")");
  if(ret==NULL || strlen(ret)==0)
    return NULL;
  else
    return ret;
}


char *GFX_GetSaveFileName(
	struct Tracker_Windows *tvisual,
	ReqType reqtype,
	char *seltext,
	char *dir
){
  char *ret = GFX_ReadStringFromPythonCommand("X11_Qtstuff.GFX_SaveFileRequester(\"%s\")");
  if(ret==NULL || strlen(ret)==0)
    return NULL;
  else
    return ret;
}

#endif
