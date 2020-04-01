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
#include "instruments_proc.h"
#include "patch_proc.h"
#include "undo.h"
#include "OS_Player_proc.h"

#include "../audio/SoundPlugin.h"

#include "../Qt/Qt_instruments_proc.h"
#include "../mixergui/QM_chip.h"

#include "undo_patchname_proc.h"

// Also undo/redoes the forward_event boolean variable. Event destinations are stored in the mixer gui undo/redo system.

extern struct Root *root;

struct Undo_PatchName{
  struct Patch *patch;
  const char *name;
  bool name_is_edited;
  bool forward_events;
};

static void *Undo_Do_PatchName(
                                 struct Tracker_Windows *window,
                                 struct WBlocks *wblock,
                                 struct WTracks *wtrack,
                                 int realline,
                                 void *pointer
                                 );

static void Undo_PatchName(
                             struct Tracker_Windows *window,
                             struct WBlocks *wblock,
                             struct Patch *patch
                             )
{
  struct Undo_PatchName *undo_ae=talloc(sizeof(struct Undo_PatchName));
  
  undo_ae->patch = patch;
  undo_ae->name = talloc_strdup(patch->name);
  undo_ae->name_is_edited = patch->name_is_edited;
  undo_ae->forward_events = patch->forward_events;
  
  
  //printf("********* Storing patchvoice undo. Value: %d\n",undo_ae->voice.is_on);

  Undo_Add_dont_stop_playing(
                             window->l.num,
                             wblock->l.num,
                             wblock->wtrack->l.num,
                             wblock->curr_realline,
                             undo_ae,
                             Undo_Do_PatchName,
                             "Patch name"
                             );
}

void ADD_UNDO_FUNC(PatchName_CurrPos(struct Patch *patch)){
  struct Tracker_Windows *window = root->song->tracker_windows;

  Undo_PatchName(window,window->wblock, patch);
}

static void *Undo_Do_PatchName(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){

  struct Undo_PatchName *undo_ae=pointer;
  struct Patch *patch = undo_ae->patch;

  const char *new_name = talloc_strdup(patch->name);
  bool new_name_is_edited = patch->name_is_edited;
  bool new_forward_events = patch->forward_events;

  //printf("Calling Undo_do for %d. Old value: %d. Setting it to %d\n", voicenum,new_patch_voice.is_on,undo_ae->voice.is_on);
  //if(new_patch_voice.is_on==undo_ae->voice.is_on)
  //  abort();

  PATCH_set_name(patch, undo_ae->name);
  patch->name_is_edited = undo_ae->name_is_edited;
  patch->forward_events = undo_ae->forward_events;

  undo_ae->name = new_name;
  undo_ae->name_is_edited = new_name_is_edited;
  undo_ae->forward_events = new_forward_events;

  return undo_ae;
}

