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




#include "nsmtracker.h"
#include "undo.h"
#include "OS_Player_proc.h"

#include "../audio/SoundPlugin.h"
#include "../Qt/Qt_instruments_proc.h"

#include "undo_patchvoice_proc.h"


extern struct Root *root;

struct Undo_PatchVoice{
  struct Patch *patch;
  int voicenum;
  struct PatchVoice voice;
};

static void *Undo_Do_PatchVoice(
                                 struct Tracker_Windows *window,
                                 struct WBlocks *wblock,
                                 struct WTracks *wtrack,
                                 int realline,
                                 void *pointer
                                 );

static void ADD_UNDO_FUNC(PatchVoice(
                                     struct Tracker_Windows *window,
                                     struct WBlocks *wblock,
                                     struct Patch *patch,
                                     int voicenum
                                     )
                          )
{
  struct Undo_PatchVoice *undo_ae=talloc(sizeof(struct Undo_PatchVoice));
  
  undo_ae->patch = patch;
  undo_ae->voicenum = voicenum;
  undo_ae->voice = patch->voices[voicenum];


  printf("********* Storing patchvoice undo. Value: %d\n",undo_ae->voice.is_on);

  Undo_Add_dont_stop_playing(
           window->l.num,
           wblock->l.num,
           wblock->wtrack->l.num,
           wblock->curr_realline,
           undo_ae,
           Undo_Do_PatchVoice,
           "Patch voice"
           );

}

void ADD_UNDO_FUNC(PatchVoice_CurrPos(struct Patch *patch, int voicenum)){
  struct Tracker_Windows *window = root->song->tracker_windows;

  CALL_ADD_UNDO_FUNC(PatchVoice(window,window->wblock, patch, voicenum));
}

static void *Undo_Do_PatchVoice(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){

  struct Undo_PatchVoice *undo_ae=pointer;
  struct Patch *patch = undo_ae->patch;
  int voicenum = undo_ae->voicenum;

  struct PatchVoice new_patch_voice = patch->voices[voicenum];

  printf("Calling Undo_do for %d. Old value: %d. Setting it to %d\n", voicenum,new_patch_voice.is_on,undo_ae->voice.is_on);
  //if(new_patch_voice.is_on==undo_ae->voice.is_on)
  //  abort();
  
  PLAYER_lock();
  {
    patch->voices[voicenum] = undo_ae->voice;
  }
  PLAYER_unlock();

  GFX_update_instrument_widget(undo_ae->patch);

  undo_ae->voice = new_patch_voice;

  return undo_ae;
}

