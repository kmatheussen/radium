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


//#define MAX_SCROLLPLAYTRACKS 128

namespace{
struct IdAndNoteVal{
  int64_t _id;
  float _val;
};
}

static std::vector<std::vector<IdAndNoteVal>*> g_scrollplaying_notes;

/*
__attribute__((constructor)) static void initialize_g_scrollplaing_notes(void) {
  g_scrollplaying_notes = new std::vector<IdAndNoteVal>[MAX_SCROLLPLAYTRACKS]; //(vector_t *)calloc(sizeof(vector_t), MAX_SCROLLPLAYTRACKS);
}
*/

static void Scroll_play_down3(
                              struct WBlocks *wblock,
                              const Ratio r1, //Place *p1,
                              const Ratio r2, //Place *p2,
                              const int64_t dont_play_this_note
){
  struct Tracks *track = wblock->block->tracks;

  while(track != NULL){
    
    if (track->onoff==1){
      
      struct Patch *patch=track->patch;
      
      if (patch != NULL){

#if 0
        struct Notes *note = track->notes;
        
        while(note != NULL){
          if (note->id != dont_play_this_note){
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
#else
        const r::NoteTimeData::Reader reader(track->_notes2);

        for(const r::NotePtr &note : reader) { 
          if (note->_id != dont_play_this_note){

            const Ratio time = note->get_time();
            
            if (time > r2)
              break;

            if (time >= r1 && time < r2)
              PATCH_play_note(patch, 
                              create_note_t(NULL,
                                            note->_id,
                                            note->_val,
                                            VELOCITY_get(note->d._velocity),
                                            TRACK_get_pan(track),
                                            ATOMIC_GET(track->midi_channel),
                                            0,
                                            0)
                              );

            //Place endplace = ratio2place(note->d._end);
            if (note->d._end >= r1 &&  note->d._end < r2) //PlaceIsBetween2(&endplace, p1, p2))
              PATCH_stop_note(patch,
                              create_note_t(NULL,
                                            note->_id,
                                            note->_val,
                                            0,
                                            TRACK_get_pan(track),
                                            ATOMIC_GET(track->midi_channel),
                                            0,
                                            0
                                            )
                              );
            }
        }
#endif
      }
    }
    
    track = NextTrack(track);
  }
}

static void stop_all_notes_in_track(struct Tracks *track){
  const int tracknum = track->l.num;

  if (tracknum < (int)g_scrollplaying_notes.size()) {
    
    struct Patch *patch=track->patch;

    if (patch!=NULL)      
      for(const auto &note : *g_scrollplaying_notes[tracknum]){
        PATCH_stop_note(patch,create_note_t(NULL,
                                            note._id,
                                            note._val,
                                            0,
                                            0,
                                            ATOMIC_GET(track->midi_channel),
                                            0,
                                            0
                                            ));
      }
    
    g_scrollplaying_notes[tracknum]->clear();
  }
}

static void Scroll_play_up3(
                     struct WBlocks *wblock,
                     const Ratio &r1,
                     const Ratio &r2
){
  struct Tracks *track = wblock->block->tracks;

  while(track != NULL) {

    if (track->onoff==1){
      
      struct Patch *patch = track->patch;
      
      if (patch != NULL){
        
#if 0        
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
            VECTOR_push_back(&g_scrollplaying_notes[track->l.num], note);
          }
          
          note = NextNote(note);
        }
#else
        const r::NoteTimeData::Reader reader(track->_notes2);

        bool is_first = true;
        
        for(const r::NotePtr &note : reader.get_iterator_left(r1)) {
          const Ratio time = note->get_time();

          if (time >= r1 && time < r2) {
            
            if (is_first) {
              stop_all_notes_in_track(track);
              is_first = false;
            }

            PATCH_play_note(patch,
                            create_note_t(NULL,
                                          note->_id,
                                          note->_val,
                                          VELOCITY_get(note->d._velocity),
                                          TRACK_get_pan(track),
                                          ATOMIC_GET(track->midi_channel),
                                          0,
                                          0
                                          )
                            );

            int i = 0;
            while(track->l.num >= (int)g_scrollplaying_notes.size() && i++ < 10000)
              g_scrollplaying_notes.push_back(new std::vector<IdAndNoteVal>);

            if (i < 9999)
              g_scrollplaying_notes[track->l.num]->push_back({note->_id, note->_val});
           }
        }

#endif
      }

    }
    
    track = NextTrack(track);
  }
}

static void Scroll_play_down2(
                             struct WBlocks *wblock,
                             int start_realline,
                             int end_realline,
                             const int64_t dont_play_this_note
){
  Ratio r1,r2;

  if (set_r1_and_r2(wblock, start_realline, end_realline, r1, r2))
    Scroll_play_down3(wblock, r1, r2, dont_play_this_note);
}

static void Scroll_play_up2(
                           struct WBlocks *wblock,
                           int start_realline,
                           int end_realline
){
#if 0
  Place p1 = {};
  Place p2 = {};
  
  set_p1_and_p2(wblock, start_realline, end_realline, &p1, &p2);
  
  Scroll_play_up3(wblock, &p1, &p2);
#else
  Ratio r1,r2;
  
  if (set_r1_and_r2(wblock, start_realline, end_realline, r1, r2))
    Scroll_play_up3(wblock, r1, r2);
#endif
}


void Scroll_play_down(
                      struct WBlocks *wblock,
                      int start_realline,
                      int end_realline,
                      const int64_t dont_play_this_note
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




