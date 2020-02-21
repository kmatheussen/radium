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


#ifndef RADIUM_COMMON_TIME_PROC_H
#define RADIUM_COMMON_TIME_PROC_H

#include <math.h>

#include "placement_proc.h"
#include "list_proc.h"


static inline bool has_same_tempo(double tempo1, double tempo2){
  return fabs(tempo1 - tempo2) < 0.003; // approx.
}


enum SwingingMode{
  NON_SWINGING_MODE,               // Only makes sense calling from the main thread.
  EDITOR_BLOCK_SWINGING_MODE,      // Only makes sense calling from the main thread. Returns non-swinging values if editor is not set to swing along.
  EDITOR_CURR_TRACK_SWINGING_MODE, // Uses current_track->times if current_track is not a timing track. If not, use block->times_with_global_swing instead. Returns non-swinging values if editor is not set to swing along. Can only be used by the main thread.
  PLUGINS_AND_JACK_TRANSPORT_SWINGING_MODE,
  SEQUENCER_TIMELINE_SWINGING_MODE,        // Only makes sense calling from the main thread.
  SEQUENCER_EDITOR_SEQBLOCK_SWINGING_MODE, // Only makes sense calling from the main thread.
};

static inline const struct STimes *get_stimes_from_swinging_mode(const struct Blocks *block,
                                                                 enum SwingingMode swinging_mode)
{
  
  switch(swinging_mode){

    
    case NON_SWINGING_MODE:
      
      R_ASSERT_NON_RELEASE(THREADING_is_main_thread());
      
      return block->times_without_global_swings;
      

    case EDITOR_BLOCK_SWINGING_MODE:
      
      R_ASSERT_NON_RELEASE(THREADING_is_main_thread());
            
      if (root->song->editor_should_swing_along==false)
        return block->times_without_global_swings;
      else
        return block->times_with_global_swings;
      

    case EDITOR_CURR_TRACK_SWINGING_MODE:
      
      R_ASSERT_NON_RELEASE(THREADING_is_main_thread());

      if (root->song->editor_should_swing_along==false)
        
        return block->times_without_global_swings;
      
      else if (root->song->tracker_windows->curr_track < 0)
        
          return block->times_with_global_swings;
      
      else {

        const struct Tracker_Windows *window = root->song->tracker_windows;

        struct WBlocks *wblock = window->wblock;
        
        if (wblock->block != block) {
          
          int blocknum = block->l.num;
          
          wblock = (struct WBlocks*)ListFindElement1_r0(&window->wblocks->l, blocknum);
          
          if (wblock==NULL || wblock->block != block){
            R_ASSERT_NON_RELEASE(false);
            return block->times_with_global_swings;
          }
          
        } else {

#if !defined(RELEASE)
          int tracknum = window->curr_track;

          R_ASSERT(tracknum==wblock->wtrack->l.num);
          
          R_ASSERT(window->curr_track==wblock->wtrack->l.num);
        
          R_ASSERT(window->curr_track==wblock->wtrack->track->l.num);
        
          R_ASSERT(ListFindElement1(&block->tracks->l, window->curr_track) == &wblock->wtrack->track->l);
#endif
          
        }

        const struct WTracks *wtrack = wblock->wtrack;
              
#if !defined(RELEASE)
        {
          struct Tracks *track = wtrack->track;
          while(track!=NULL){
            
            if (track->times==NULL || track->times->tchanges==NULL)
              abort();

            if (track!=wblock->wtrack->track && track->times==wblock->block->times_with_global_swings)
              continue;
            
            for(int i = 0;;i++){
              if (equal_doubles(track->times->tchanges[i].y2, 0.0)){
                if(!equal_doubles(track->times->tchanges[i].y1, 0.0))
                  abort();
                if(!equal_doubles(track->times->tchanges[i].x1, 0.0))
                  abort();
                if(!equal_doubles(track->times->tchanges[i].x2, 0.0))
                  abort();
                if(!equal_doubles(track->times->tchanges[i].t1, 0.0))
                  abort();
                if(!equal_doubles(track->times->tchanges[i].t2, 0.0))
                  abort();
                if(!equal_doubles(track->times->tchanges[i].logt1, 0.0))
                  abort();
                if(!equal_doubles(track->times->tchanges[i].logt2t1, 0.0))
                  abort();
                if(!equal_doubles(track->times->tchanges[i].glidingswing_scale_value, 0.0))
                  abort();
                break;
              }
              if(track->times->tchanges[i].y1 < 0 || track->times->tchanges[i].y1 > 99999999)
                abort();
            }

            STime last_time=-1;
            for(int i=0;i<=wblock->block->num_lines;i++){
              STime time = track->times[i].time;
              
              if (time <= last_time)
                abort();
              
              if(track->times[i].tchanges->y1 < 0 || track->times[i].tchanges->y1 > 99999999)
                abort();
              
              last_time = time;
            }
            
            track = NextTrack(track);
          }
        }
#endif

        return wtrack->track->times;
      }
      

    case PLUGINS_AND_JACK_TRANSPORT_SWINGING_MODE:
      if (root->song->plugins_should_receive_swing_tempo)
        return block->times_with_global_swings;
      else
        return block->times_without_global_swings;
      

    case SEQUENCER_TIMELINE_SWINGING_MODE:
      
      R_ASSERT_NON_RELEASE(THREADING_is_main_thread());
      
      if (root->song->use_swinging_beats_in_sequencer)
        return block->times_with_global_swings;
      else
        return block->times_without_global_swings;
      

    case SEQUENCER_EDITOR_SEQBLOCK_SWINGING_MODE:
      
      R_ASSERT_NON_RELEASE(THREADING_is_main_thread());
      
      if (root->song->display_swinging_beats_in_seqblocks_in_sequencer)
        return block->times_with_global_swings;
      else
        return block->times_without_global_swings;
      

    default:
      R_ASSERT(false);
      return block->times_with_global_swings;
  }
}


// Can be called from any thread.
extern LANGSPEC double Place2STime_from_times2(
                                               const struct STimes *stimes,
                                               double place_as_float
                                               );

// Can be called from any thread.
extern LANGSPEC STime Place2STime_from_times(int num_lines, const struct STimes *times, const Place *p);

// Can be called from any thread.
extern LANGSPEC STime Place2STime(
                                  const struct Blocks *block,
                                  const Place *p,
                                  enum SwingingMode swinging_mode
                                  );

// Can be called from any thread.
extern LANGSPEC STime Place2STime2(
                                   const struct Blocks *block,
                                   const Place *p,
                                   const struct Tracks *track
                                   );

/*
static inline STime Place2SwingingSTime(
	const struct Blocks *block,
	const Place *p
){
  return Place2STime_from_times(block->num_time_lines, block->times_with_global_swings, p);
}

static inline STime Place2TrackSwingingSTime(
                                             const struct Blocks *block,
                                             const struct Tracks *track,
                                             const Place *p
){
  return Place2STime_from_times(block->num_time_lines, track->times, p);
}

static inline STime Place2NonSwingingSTime(
	const struct Blocks *block,
	const Place *p
){
  return Place2STime_from_times(block->num_time_lines, block->times_without_global_swings, p);
}
*/

extern LANGSPEC double STime2Place_f(
                                    const struct Blocks *block,
                                    double time,
                                    enum SwingingMode swinging_mode
                                    );

extern LANGSPEC Place STime2Place(
                                  const struct Blocks *block,
                                  STime time,
                                  enum SwingingMode swinging_mode
                                  );

extern LANGSPEC double STime2Place2_f(
                                    const struct Blocks *block,
                                    double time,
                                    const struct Tracks *track
                                    );

extern LANGSPEC Place STime2Place2(
                                  const struct Blocks *block,
                                  STime time,
                                  const struct Tracks *track
                                  );

// Can be called from any thread.
static inline STime getBlockSTimeLength(const struct Blocks *block){                       
  if (block==NULL){
#if !defined(RELEASE)
    abort();
#else
    return 48000; // This should never happen, and it has never happened.
#endif
  }

#if !defined(RELEASE)
  if (THREADING_is_main_thread()){
    if (block->num_lines != block->num_time_lines) // 'num_time_lines' is the actual allocated number of lines.
      RWarning("block->num_lines != block->num_time_lines: %d != %d",block->num_lines, block->num_time_lines);

    R_ASSERT(block->times_with_global_swings[block->num_time_lines].time == block->length);
    R_ASSERT(block->times_without_global_swings[block->num_time_lines].time == block->length);
  }
#endif
  
  return block->length;
}

static inline bool isSTimeInBlock(const struct Blocks *block,STime time){
  STime block_length = getBlockSTimeLength(block);
  if(time > block_length)
    return false;
  else
    return true;
}



extern LANGSPEC void TIME_block_tempos_have_changed(struct Blocks *block);
extern LANGSPEC void TIME_block_LPBs_have_changed(struct Blocks *block);
extern LANGSPEC void TIME_block_signatures_have_changed(struct Blocks *block);
extern LANGSPEC void TIME_block_num_lines_have_changed(struct Blocks *block);
extern LANGSPEC void TIME_block_num_tracks_have_changed(struct Blocks *block);
extern LANGSPEC void TIME_block_swings_have_changed(struct Blocks *block);
extern LANGSPEC void TIME_global_tempos_have_changed(void);
extern LANGSPEC void TIME_global_LPB_has_changed(void);
extern LANGSPEC void TIME_global_signature_has_changed(void);
extern LANGSPEC void TIME_everything_has_changed2(const struct Root *root, struct Song *song);
extern LANGSPEC void TIME_everything_has_changed(void);
extern LANGSPEC void TIME_everything_in_block_has_changed2(struct Blocks *block, const struct Root *root, const struct Song *song);
extern LANGSPEC void TIME_everything_in_block_has_changed(struct Blocks *block);
  
#endif

