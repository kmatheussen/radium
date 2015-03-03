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
#include "../common/player_proc.h"
#include "../common/patch_proc.h"

#include "midi_i_input_proc.h"

extern struct Root *root;

// Need memory barriers, or queue here. Generally it's not a very flexible API, but it's just used for editing, not recording.

static volatile int g_cc=0,g_data1,g_data2;

static struct Patch *g_through_patch = NULL;

// TODO: This isn't always working properly. Going to change rtmidi API.

extern const char *NotesTexts3[131];

void MIDI_InputMessageHasBeenReceived(int cc,int data1,int data2){
  //printf("got new message. on/off:%d. Message: %x,%x,%x\n",(int)root->editonoff,cc,data1,data2);
  //static int num=0;
  //num++;

  if(cc>=0xf0) // Too much drama
    return;


  if(cc>=0x80 && cc<0xa0){
    struct Patch *patch = g_through_patch;
    if(patch!=NULL){
      //printf("%d: got note %s (0x%x 0x%x)\n",num-1,NotesTexts3[data1],cc,data2);
      if(data2>0 && cc>=0x90)
        PATCH_play_note(patch,data1,-1,scale(data2,0,127,0,1),0.5);
      else
        PATCH_stop_note(patch,data1,-1);
    //MyMyPutMidi(patchdata->midi_port,(cc&0xf0)|patchdata->channel,data1,data2); // send out the message again to the patch and channel specified at the current track
    }
  }

  if(g_cc!=0) // Too quick.
    return;

  // should be a memory barrier here somewhere.

  if((cc&0xf0)==0x90 && data2!=0) {
    g_cc = cc;
    g_data1 = data1;
    g_data2 = data2;

    Ptask2Mtask();
  }
}

// This is safe. A patch is never deleted.
void MIDI_SetThroughPatch(struct Patch *patch){
  //printf("Sat new patch %p\n",patch);
  if(patch!=NULL)
    g_through_patch=patch;
}


void MIDI_HandleInputMessage(void){
  //int cc = g_cc;
  int data1 = g_data1;
  //int data2 = g_data2;

  // should be a memory barrier here somewhere.

  if(g_cc==0)
    return;

  g_cc = 0;

  if( ! root->editonoff)
    return;

  InsertNoteCurrPos(root->song->tracker_windows,data1,false);
  root->song->tracker_windows->must_redraw = true;
}

