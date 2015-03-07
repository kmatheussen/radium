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


#include <stdio.h>
#include <stdlib.h>

#include "nsmtracker.h"
#include "list_proc.h"
#include "placement_proc.h"
#include "notes_proc.h"
#include "undo_notes_proc.h"
#include "instruments_proc.h"
#include "../midi/midi_i_plugin_proc.h"
#include "../audio/SoundPlugin.h"

#include "tracks_proc.h"




extern struct Root *root;


bool TRACK_get_min_and_max_pitches(struct Tracks *track, float *ret_min_pitch, float *ret_max_pitch){
  float min_pitch = 10000.0f;
  float max_pitch = -10000.0f;

  int num_pitches = 0;
  
  // find min_pitch and max_pitch
  {
    struct Notes *note = track->notes;
    while(note!=NULL){
      min_pitch = R_MIN(note->note, min_pitch);
      max_pitch = R_MAX(note->note, max_pitch);
      num_pitches ++;
      struct Pitches *pitch = note->pitches;
      while(pitch != NULL){
        min_pitch = R_MIN(pitch->note, min_pitch);
        max_pitch = R_MAX(pitch->note, max_pitch);
        num_pitches ++;
        pitch = NextPitch(pitch);
      }
      note = NextNote(note);
    }

    float pitch_range = max_pitch - min_pitch;

    min_pitch = min_pitch - pitch_range/8.0f;
    if(min_pitch < 0)
      min_pitch = 0;

    max_pitch = max_pitch + pitch_range/8.0f;
    if(max_pitch >127)
      max_pitch = 127;
  }

  if(min_pitch == 10000.0f)
    return false;
  else {
    if (num_pitches>3) {
      *ret_min_pitch = min_pitch;
      *ret_max_pitch = max_pitch;
    } else {
      *ret_min_pitch = 0;
      *ret_max_pitch = 128;
    }
    return true;
  }
}


void CloseTrack(struct Blocks *block, NInt tracknum){
	struct Tracks *temp=(struct Tracks *)ListFindElement1(&block->tracks->l,tracknum);

	ListRemoveElement1(&block->tracks,&temp->l);
}

bool TRACK_has_peaks(struct Tracks *track){
  struct Patch *patch=track->patch;

  if(patch==NULL || patch->patchdata==NULL)
    return false;

  if(patch->instrument==get_audio_instrument() && ((SoundPlugin*)patch->patchdata)->type->get_peaks!=NULL){
    return true;
  }else{
    return false;
  }
}

// l.num must not be set here!
void InitTrack(struct Tracks *track){
	track->onoff=1;
	track->trackname="(click me)";
	track->volume=800;
	track->panonoff=false;
	track->volumeonoff=true;
        MIDI_init_track(track);
}

static void NewTrack(struct Blocks *block,struct Tracks *track){
  InitTrack(track);
  ListAddElement1(&block->tracks,&track->l);
}

void AppendTrack(struct Blocks *block){
	int tracknum=ListFindFirstFreePlace1(&block->tracks->l);

	struct Tracks *temp=talloc(sizeof(struct Tracks));

        //printf("AppendTrack. num: %d\n",tracknum);

	temp->l.num=tracknum;
	NewTrack(block,temp);
}


void TRACK_make_monophonic_destructively(struct Tracks *track){
  struct Notes *note = track->notes;

  bool have_made_undo = false;

  while(note!=NULL){
    struct Notes *next = NextNote(note);
    if (next==NULL)
      break;

    if (PlaceGreaterThan(&note->end, &next->l.p)){
      PlaceCopy(&note->end, &next->l.p);
      if (have_made_undo==false){
        struct Tracker_Windows *window = root->song->tracker_windows;
        struct WBlocks *wblock = window->wblock;
        
        Undo_Notes(window,
                   wblock->block,
                   track,
                   wblock->curr_realline
                   );

        have_made_undo = true;
      }
    }

    note = next;
  }
}

void TRACK_make_monophonic_destructively_CurrPos(struct Tracker_Windows *window){
  TRACK_make_monophonic_destructively(window->wblock->wtrack->track);
}
