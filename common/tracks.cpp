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

#include <gc.h>

#include "nsmtracker.h"
#include "TimeData.hpp"
#include "list_proc.h"
#include "vector_proc.h"
#include "placement_proc.h"
#include "notes_proc.h"
#include "undo_notes_proc.h"
#include "undo_blocks_proc.h"
#include "undo.h"
#include "instruments_proc.h"
#include "../midi/midi_instrument_proc.h"
#include "../audio/SoundPlugin.h"
#include "visual_proc.h"
#include "OS_Player_proc.h"
#include "player_proc.h"
#include "track_insert_proc.h"
#include "clipboard_track_copy_proc.h"
#include "clipboard_track_paste_proc.h"
#include "time_proc.h"
#include "TallocWithDestructor.hpp"

#include "tracks_proc.h"



extern char *NotesTexts3[];

extern struct Root *root;

void CloseTrack(struct Blocks *block, NInt tracknum){
	struct Tracks *temp=(struct Tracks *)ListFindElement1(&block->tracks->l,tracknum);

	ListRemoveElement1(&block->tracks,&temp->l);
        g_editor_blocks_generation++;
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


static int g_num_live_tracks = 0;

static void trackgcfinalizer(void *actual_mem_start, void *user_data){
  struct Tracks *track = (struct Tracks*)actual_mem_start;
  printf("    GC-ed TRACK %d. Num: %d\n", track->l.num, g_num_live_tracks);

  // Will probably convert the whole program to C++, and then we don't need this stuff.
  {
    delete track->_notes2;
    delete track->_gfx_notes2;

    delete track->stops2;
  }
  
  g_num_live_tracks--;
}

void talloc_with_destructor_gc_finalizer(void *actual_mem_start, void *user_data){
  radium::SuperTallocWithDestructor *gakkgakk = static_cast<radium::SuperTallocWithDestructor*>(user_data);

  R_ASSERT_RETURN_IF_FALSE(gakkgakk==dynamic_cast<radium::SuperTallocWithDestructor*>(gakkgakk));
  
  gakkgakk->_super_finalizer();

  delete gakkgakk;
}

//int g_num_gcable = 0;

void gc_able_gc_finalizer(void *actual_mem_start, void *user_data){
  R_ASSERT(user_data==NULL);
  
  radium::GC_able *gakkgakk = static_cast<radium::GC_able*>(actual_mem_start);

  R_ASSERT_RETURN_IF_FALSE(gakkgakk==dynamic_cast<radium::GC_able*>(gakkgakk));

  //printf("    GC_ABLE: %d\n", g_num_gcable);
  //abort();

  if (gakkgakk->_destructor_has_run == true){
    R_ASSERT_NON_RELEASE(false);
    return;
  }

#if 0  // Commented out since vptr is deleted by the destructor.
#if !defined(RELEASE)
  gakkgakk->_allowed_to_call_destructor = true;
#endif
#endif
  
  delete gakkgakk;
}

// l.num must not be set here!
static void InitTrack(struct Tracks *track){
	track->onoff=1;
	track->trackname="(click me)";
	track->volume=1000;
	track->panonoff=false;
	track->volumeonoff=true;
        MIDI_init_track(track);

        track->_notes2 = new r::NoteTimeData;
        //track->_gfx_notes2 = new r::NoteTimeData;
        track->_gfx_notes2 = NULL;
        
        track->stops2 = new r::StopTimeData;

        g_num_live_tracks++;
        GC_register_finalizer(track, trackgcfinalizer, NULL, NULL, NULL);

        printf("--------------------------------NUM live tracks: %d-------------------------\n", g_num_live_tracks);
}

struct Tracks *TRACK_create(int tracknum){
  struct Tracks *track=(struct Tracks*)talloc(sizeof(struct Tracks));
  track->l.num = tracknum;
  InitTrack(track);
  return track;
}

static void NewTrack(struct Blocks *block,struct Tracks *track){
  ListAddElement1(&block->tracks,&track->l);
  g_editor_blocks_generation++;
}

void AppendTrack(struct Blocks *block){
        int tracknum = block->tracks==NULL ? 0 : ListFindFirstFreePlace1(&block->tracks->l);

	struct Tracks *temp=TRACK_create(tracknum);

        //printf("AppendTrack. num: %d\n",tracknum);

	temp->l.num=tracknum;
	NewTrack(block,temp);

        TIME_block_num_tracks_have_changed(block); // Update track->timing fields.
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

    if (note->end > place2ratio(next->l.p)) {

      if (have_made_undo==false){      
        ADD_UNDO(Notes(window,
                       wblock->block,
                       track,
                       wblock->curr_realline
                       )
                 );

        have_made_undo = true;
      }

      PLAYER_lock();{

        if (PlaceEqual(&note->l.p, &next->l.p)) {

          ListRemoveElement3(&track->notes, &next->l);                           

        } else {

          CutNoteAt(wblock->block, track, note, &next->l.p);
          note = next;

        }

      }PLAYER_unlock();

    } else {

      note = next;

    }

  }

  if (have_made_undo==false)
    GFX_Message2(NULL, true, "Track is already monophonic");
  else
    window->must_redraw = true;
}

/*
void TRACK_make_monophonic_destructively_CurrPos(struct Tracker_Windows *window){
  TRACK_make_monophonic_destructively(window->wblock->wtrack->track);
}
*/



bool TRACK_split_into_monophonic_tracks(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack){
  
  PlayStop(); // This function is too chaotic. Don't bother pausing player.

  vector_t notesvector = {};
  
  struct Tracks *track = wtrack->track;

  struct Notes *notes = track->notes;
  struct Notes *notes_nexttrack = NULL;

  bool have_made_undo = false;

  if (NOTES_sorted_by_pitch_questionmark(track->notes)==false) {
    ADD_UNDO(Block_CurrPos(window));    
    have_made_undo = true;
    notes = NOTES_sort_by_pitch(notes);
  }
  
  while(notes != NULL){

    struct Notes *notes_root = notes;
    
    while(notes != NULL) {

      struct Notes *next = NextNote(notes);
      if (next==NULL)
        break;

      if (notes->end > place2ratio(next->l.p)){

        if (have_made_undo==false) {
          ADD_UNDO(Block_CurrPos(window));    
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

  if (have_made_undo==false)
    return false;

  int num_tracks = notesvector.num_elements;

  track->notes = NULL;

  struct WTracks *wtrack_copy = CB_CopyTrack(wblock,wtrack);
  VECTOR_clean(&wtrack_copy->track->fxs);

  InsertTracks(window, wblock, wtrack->l.num+1, num_tracks-1);

  printf("Vector length: %d\n",num_tracks);
  int i;
  for(i=0;i<num_tracks;i++){
    struct Notes *notes = (struct Notes*)notesvector.elements[i];
    printf("  %d: %d\n", i, ListFindNumElements3((struct ListHeader3*)notes));
    while(notes != NULL){
      printf("    %s\n",NotesTexts3[(int)notes->note]);
      notes = NextNote(notes);
    }
    
    struct WTracks *towtrack = (struct WTracks*)ListFindElement1(&wblock->wtracks->l, wtrack->l.num+i);
    
    if (i>0)
      co_CB_PasteTrack(wblock, wtrack_copy, towtrack);

    towtrack->track->notes = (struct Notes*)notesvector.elements[i];
  }

  window->must_redraw = true;

  return true;
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
      HASH_put_instrument_at(block_state, "track_patch_id", track->l.num, patch_id);
      
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
      
      int patch_id = HASH_get_instrument_at(block_state, "track_patch_id", track->l.num);

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
