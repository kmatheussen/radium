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


#include "Python.h"

#include "../common/nsmtracker.h"
#include "../common/blocklist_proc.h"
#include "../common/OS_Bs_edit_proc.h"

#include "X11.h"
#include "X11_ReqType_proc.h"

#include "X11_Bs_edit_proc.h"


extern struct Root *root;

void BS_resizewindow(void){}


void X11_StartBlockSelector(void){
  char temp[500];
  PyRun_SimpleString("import X11_BlockSelector");

  sprintf(temp,"X11_BlockSelector.BS_StartBlockSelector()");
  PyRun_SimpleString(temp);
}

/*
void X11_EndBlockSelector(void){
  PyRun_SimpleString("X11_BlockSelector.BS_EndBlockSelector()");
}
*/

void BS_UpdateBlockList(void){
  int num_blocks=root->song->num_blocks;
  char temp[num_blocks*200];
  struct Blocks *block=root->song->blocks;
  int lokke;

  sprintf(temp,"X11_BlockSelector.BS_UpdateBlockList(");
  for(lokke=0;lokke<num_blocks;lokke++){
    if(lokke==0){
      sprintf(temp,"%s\"%s\"",temp,block->name);
    }else{
      sprintf(temp,"%s,\"%s\"",temp,block->name);
    }
    block=NextBlock(block);
  }
  sprintf(temp,"%s)",temp);

  PyRun_SimpleString(temp);

  BS_SelectBlock(root->song->tracker_windows->wblock->block);
}

void BS_UpdatePlayList(void){
  char temp[5000];
  int lokke;
  struct Blocks *block=BL_GetBlockFromPos(0);

  sprintf(temp,"X11_BlockSelector.BS_UpdatePlayList(");

  lokke=0;
  block=BL_GetBlockFromPos(lokke);

  while(block!=NULL){
    sprintf(temp,"%s%s%d ",lokke==0?"":",",temp,block->l.num);
    lokke++;
    block=BL_GetBlockFromPos(lokke);
  }
  sprintf(temp,"%s)",temp);
  PyRun_SimpleString(temp);
}

void BS_SelectBlock(struct Blocks *block){
  char temp[500];
  sprintf(temp,"X11_BlockSelector.BS_SelectBlock(%d)",block->l.num);
  PyRun_SimpleString(temp);
}

void BS_SelectPlaylistPos(int pos){
  char temp[500];
  sprintf(temp,"X11_BlockSelector.BS_SelectPlaylistPos(%d)",pos);
  PyRun_SimpleString(temp);
}

void BS_SetX11Window(int x11window){
  char temp[500];
  sprintf(temp,"X11_BlockSelector.BS_SetX11Window(%d)",x11window);
  PyRun_SimpleString(temp);
}

void BS_StopXSend(void){
  PyRun_SimpleString("X11_BlockSelector.BS_StopXSend()");
}




