/* Copyright 2001 Kjetil S. Matheussen

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



/*
   Handles playing of notes while scrolling (ie. up down arrow, etc. )
   while not playing. (playing of fxes could be annoying; may be
   implemented later).
*/

#include "includepython.h"

#include "nsmtracker.h"
#include "playerclass.h"
#include "wblocks_proc.h"
#include "patch_proc.h"
#include "placement_proc.h"
#include "vector_proc.h"
#include "realline_calc_proc.h"

#include "../api/api_proc.h"

#include "scroll_play_proc.h"


extern PlayerClass *pc;
extern struct Root *root;



#define MAX_SCROLLPLAYTRACKS 128

static vector_t scrollplaying_notes[MAX_SCROLLPLAYTRACKS] = {{0}}; // [NO_STATIC_ARRAY_WARNING]



static void Scroll_play_down3(
                      struct WBlocks *wblock,
                      const Place *p1,
                      const Place *p2,
                      const struct Notes *dont_play_this_note
){
  struct Tracks *track = wblock->block->tracks;

  while(track != NULL){
    
    if (track->onoff==1){
      
      struct Patch *patch=track->patch;
      
      if (patch != NULL){
        
        struct Notes *note = track->notes;
        
        while(note != NULL){
          if (note != dont_play_this_note){
            if (PlaceIsBetween2(&note->l.p, p1, p2))
              PATCH_play_note(patch, 
                              create_note_t(NULL,
                                            note->id,
                                            note->note,
                                            VELOCITY_get(note->velocity),
                                            TRACK_get_pan(track),
                                            ATOMIC_GET(track->midi_channel),
                                            0,
                                            0)
                              );

            Place endplace = ratio2place(note->end);
            if (PlaceIsBetween2(&endplace, p1, p2))
              PATCH_stop_note(patch,
                              create_note_t(NULL,
                                            note->id,
                                            note->note,
                                            0,
                                            TRACK_get_pan(track),
                                            ATOMIC_GET(track->midi_channel),
                                            0,
                                            0
                                            )
                              );
          }
          
          note = NextNote(note);
        }
      }
    }
    
    track = NextTrack(track);
  }
}

static void stop_all_notes_in_track(struct Tracks *track){
  int tracknum = track->l.num;

  if (tracknum < MAX_SCROLLPLAYTRACKS) {

    struct Patch *patch=track->patch;

    if (patch!=NULL) {
            
      VECTOR_FOR_EACH(struct Notes *note, &scrollplaying_notes[tracknum]){
        PATCH_stop_note(patch,create_note_t(NULL,
                                            note->id,
                                            note->note,
                                            0,
                                            0,
                                            ATOMIC_GET(track->midi_channel),
                                            0,
                                            0
                                            ));
      }END_VECTOR_FOR_EACH;
      
    }

    VECTOR_clean(&scrollplaying_notes[tracknum]);
  }
}

static void Scroll_play_up3(
                     struct WBlocks *wblock,
                     const Place *p1,
                     const Place *p2
){
  struct Tracks *track = wblock->block->tracks;

  while(track != NULL && track->l.num < MAX_SCROLLPLAYTRACKS) {

    if (track->onoff==1){
      
      struct Patch *patch = track->patch;
      
      if (patch != NULL){
        
        struct Notes *note = track->notes;
        
        // First stop previously playing notes on this track, if any.
        while(note != NULL){
          if (PlaceIsBetween2(&note->l.p, p1, p2)){
            stop_all_notes_in_track(track);
            break;
          }
          note = NextNote(note);
        }
        
        // Then play new notes.
        while(note != NULL){
          if (PlaceIsBetween2(&note->l.p, p1, p2)) {
            PATCH_play_note(patch,
                            create_note_t(NULL,
                                          note->id,
                                          note->note, 
                                          VELOCITY_get(note->velocity),
                                          TRACK_get_pan(track),
                                          ATOMIC_GET(track->midi_channel),
                                          0,
                                          0
                                          )
                            );
            VECTOR_push_back(&scrollplaying_notes[track->l.num], note);
          }
          
          note = NextNote(note);
        }      
      }

    }
    
    track = NextTrack(track);
  }
}

static void Scroll_play_down2(
                             struct WBlocks *wblock,
                             int start_realline,
                             int end_realline,
                             const struct Notes *dont_play_this_note
){
  Place p1 = {0};
  Place p2 = {0};

  set_p1_and_p2(wblock, start_realline, end_realline, &p1, &p2);
  
  Scroll_play_down3(wblock, &p1, &p2, dont_play_this_note);
}

static void Scroll_play_up2(
                           struct WBlocks *wblock,
                           int start_realline,
                           int end_realline
){
  Place p1 = {0};
  Place p2 = {0};

  set_p1_and_p2(wblock, start_realline, end_realline, &p1, &p2);
  
  Scroll_play_up3(wblock, &p1, &p2);
}


void Scroll_play_down(
                      struct WBlocks *wblock,
                      int start_realline,
                      int end_realline,
                      const struct Notes *dont_play_this_note
){
  if(is_playing() || doScrollPlay()==false) return;

  start_realline=R_BOUNDARIES(0,start_realline,wblock->num_reallines-1);
  end_realline=R_BOUNDARIES(0,end_realline,wblock->num_reallines-1);
  
  Scroll_play_down2(wblock,start_realline,end_realline, dont_play_this_note);
}


void Scroll_play_up(
                    struct WBlocks *wblock,
                    int start_realline,
                    int end_realline
){
  if(is_playing() || doScrollPlay()==false) return;

  start_realline=R_BOUNDARIES(0,start_realline,wblock->num_reallines-1);
  end_realline=R_BOUNDARIES(0,end_realline,wblock->num_reallines-1);

  Scroll_play_up2(wblock,start_realline,end_realline);
}




