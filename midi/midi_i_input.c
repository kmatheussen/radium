/* Copyright 2000 Kjetil S. Matheussen

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






#include "nsmtracker.h"

#include "midi_i_plugin.h"
#include "midi_i_plugin_proc.h"
#include "../common/notes_proc.h"
#include "../common/blts_proc.h"
#include "../common/OS_Ptask2Mtask_proc.h"

#include "midi_i_input_proc.h"

extern struct Root *root;

// Need memory barriers, or queue here. Generally it's not a very flexible API, but it's just used for editing, not recording.

static volatile int g_cc=0,g_data1,g_data2;

static volatile struct PatchData *g_through_patchdata = NULL;

void MIDI_InputMessageHasBeenReceived(int cc,int data1,int data2){
  if(cc>=0xf0) // Too much drama
    return;

  if(g_cc!=0) // Too quick.
    return;

  //printf("got new message. on/off:%d. Message: %x,%x,%x\n",(int)root->editonoff,cc,data1,data2);

  // should be a memory barrier here somewhere.

  if(g_through_patchdata!=NULL)
    MyMyPutMidi(g_through_patchdata->midi_port,(g_cc&0xf0)|g_through_patchdata->channel,data1,data2); // send out the message again to the patch and channel specified at the current track

  if((cc&0xf0)==0x90 && data2!=0) {
    g_cc = cc;
    g_data1 = data1;
    g_data2 = data2;

    Ptask2Mtask();
  }
}

void MIDI_SetThroughPatch(struct Patch *patch){
  //printf("Sat new patch %p\n",patch);
  if(patch!=NULL)
    g_through_patchdata=(struct PatchData *)patch->patchdata;
}

void MIDI_HandleInputMessage(void){
  //int cc = g_cc;
  int data1 = g_data1;
  //int data2 = g_data2;

  // should be a memory barrier here somewhere.

  if(g_cc==0)
    return;

  g_cc = 0;

  //printf("got here %d\n",(int)root->editonoff);

  if( ! root->editonoff)
    return;

  InsertNoteCurrPos(root->song->tracker_windows,data1,0);
}


