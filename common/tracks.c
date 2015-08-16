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
#include "vector_proc.h"
#include "placement_proc.h"
#include "notes_proc.h"
#include "undo_notes_proc.h"
#include "undo_blocks_proc.h"
#include "undo.h"
#include "instruments_proc.h"
#include "../midi/midi_i_plugin_proc.h"
#include "../audio/SoundPlugin.h"
#include "visual_proc.h"
#include "OS_Player_proc.h"
#include "player_proc.h"
#include "track_insert_proc.h"
#include "clipboard_track_copy_proc.h"
#include "clipboard_track_paste_proc.h"

#include "tracks_proc.h"



extern char *NotesTexts3[];

extern struct Root *root;

float TRACK_get_min_pitch(const struct Tracks *track){
  float min_pitch = 10000.0f;

  int num_pitches = 0;
  
  {
    struct Notes *note = track->notes;
    while(note!=NULL){
      min_pitch = R_MIN(note->note, min_pitch);
      num_pitches ++;
      struct Pitches *pitch = note->pitches;
      while(pitch != NULL){
        min_pitch = R_MIN(pitch->note, min_pitch);
        num_pitches ++;
        pitch = NextPitch(pitch);
      }
      note = NextNote(note);
    }
  }

  if (num_pitches==0)
    return -1;
  else
    return min_pitch;
}

float TRACK_get_max_pitch(const struct Tracks *track){
  float max_pitch = -1;

  int num_pitches = 0;
  
  {
    struct Notes *note = track->notes;
    while(note!=NULL){
      max_pitch = R_MAX(note->note, max_pitch);
      num_pitches ++;
      struct Pitches *pitch = note->pitches;
      while(pitch != NULL){
        max_pitch = R_MAX(pitch->note, max_pitch);
        num_pitches ++;
        pitch = NextPitch(pitch);
      }
      note = NextNote(note);
    }
  }

  if (num_pitches==0)
    return -1;
  else
    return max_pitch;
}


bool TRACK_get_min_and_max_pitches(const struct Tracks *track, float *ret_min_pitch, float *ret_max_pitch){
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
	track->volume=1000;
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
  struct Tracker_Windows *window = root->song->tracker_windows;
  struct WBlocks *wblock = window->wblock;

  struct Notes *note = track->notes;

  bool have_made_undo = false;
  
  while(note!=NULL){
    struct Notes *next = NextNote(note);
    if (next==NULL)
      break;

    if (PlaceGreaterThan(&note->end, &next->l.p)){

      PLAYER_lock();{

        if (PlaceEqual(&note->l.p, &next->l.p)) {

          ListRemoveElement3(&track->notes, &next->l);                           

        } else {

          PlaceCopy(&note->end, &next->l.p);
          note = next;

        }

      }PLAYER_unlock();

      if (have_made_undo==false){      
        Undo_Notes(window,
                   wblock->block,
                   track,
                   wblock->curr_realline
                   );

        have_made_undo = true;
      }

    } else {

      note = next;

    }

  }

  if (have_made_undo==false)
    GFX_Message(NULL, "Track is already monophonic");
  else
    window->must_redraw = true;
}

void TRACK_make_monophonic_destructively_CurrPos(struct Tracker_Windows *window){
  TRACK_make_monophonic_destructively(window->wblock->wtrack->track);
}




void TRACK_split_into_monophonic_tracks(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack){

  PlayStop();

  vector_t notesvector = {0};
  
  struct Tracks *track = wtrack->track;

  struct Notes *notes = track->notes;
  struct Notes *notes_nexttrack = NULL;

  bool have_made_undo = false;

  if (NOTES_sorted_by_pitch_questionmark(track->notes)==false) {
    Undo_Block_CurrPos(window);    
    have_made_undo = true;
    notes = NOTES_sort_by_pitch(notes);
  }
  
  while(notes != NULL){

    struct Notes *notes_root = notes;
    
    while(notes != NULL) {

      struct Notes *next = NextNote(notes);
      if (next==NULL)
        break;

      if (PlaceGreaterThan(&notes->end, &next->l.p)){

        if (have_made_undo==false) {
            have_made_undo=true;
        }
        
        ListRemoveElement3(&notes, &next->l);                           
        ListAddElement3_a(&notes_nexttrack, &next->l);

      } else
        notes = next;
    }

    VECTOR_push_back(&notesvector, notes_root);

    notes = notes_nexttrack;
    notes_nexttrack = NULL;
  }

  if (have_made_undo==false){
    GFX_Message(NULL, "Track is already monophonic");
    return;
  }

  int num_tracks = notesvector.num_elements;

  track->notes = NULL;

  struct WTracks *wtrack_copy = CB_CopyTrack(wblock,wtrack);
  wtrack_copy->track->fxs = NULL;

  InsertTracks(window, wblock, wtrack->l.num+1, num_tracks-1);

  printf("Vector length: %d\n",num_tracks);
  int i;
  for(i=0;i<num_tracks;i++){
    struct Notes *notes = notesvector.elements[i];
    printf("  %d: %d\n", i, ListFindNumElements3(&notes->l));
    while(notes != NULL){
      printf("    %s\n",NotesTexts3[(int)notes->note]);
      notes = NextNote(notes);
    }
    
    struct WTracks *towtrack = ListFindElement1(&wblock->wtracks->l, wtrack->l.num+i);
    
    if (i>0)
      CB_PasteTrack(wblock, wtrack_copy, towtrack);

    towtrack->track->notes = notesvector.elements[i];
  }

  window->must_redraw = true;
}

/*
hash_t *TRACKS_get_patch_state(bool create_undo){
  hash_t *state=HASH_create(16);
  
  struct Blocks *block = root->song->blocks;
  while (block != NULL){

    hash_t *block_state=HASH_create(16);
    HASH_put_hash_at(state, "blocks", block->l.num, block_state);
                     
    struct Tracks *track = block->tracks;
    while (track != NULL) {
      
      int patch_id = track->patch==NULL ? -1 : track->patch->id;
      HASH_put_int_at(block_state, "track_patch_id", track->l.num, patch_id);
      
      track = NextTrack(track);
    }
    block = NextBlock(block);
  }

  return state;
}

void TRACKS_set_patch_state(hash_t *state){

  struct Blocks *block = root->song->blocks;
  while (block != NULL){

    hash_t *block_state=HASH_get_hash_at(state, "blocks", block->l.num);
    
    struct Tracks *track = block->tracks;
    while (track != NULL) {
      
      int patch_id = HASH_get_int_at(block_state, "track_patch_id", track->l.num);

      struct Patches *patch = PATCH_get_from_id(patch_id);
      if (patch != track->patch) {
        PATCH_handle_fx_when_theres_a_new_patch_for_track(track, track->patch, patch);      
        track->patch = patch;
      }
      
      track = NextTrack(track);
    }
    block = NextBlock(block);
  }
}
*/
