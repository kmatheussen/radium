/* Copyright 2016 Kjetil S. Matheussen

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



#include <math.h>
#include <gc.h>

#include "nsmtracker.h"
#include "player_proc.h"
#include "vector_proc.h"
#include "player_pause_proc.h"
#include "time_proc.h"
#include "hashmap_proc.h"
#include "list_proc.h"
#include "OS_Player_proc.h"
#include "scheduler_proc.h"
#include "../audio/Mixer_proc.h"
#include "OS_Bs_edit_proc.h"
#include "song_tempo_automation_proc.h"
#include "seqtrack_automation_proc.h"
#include "patch_proc.h"


#include "seqtrack_proc.h"


// 'seqblock_where_time_is' can be NULL, but it works faster if it is not null. (Not quite sure if that is the whole difference)
int64_t get_abstime_from_seqtime(const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock_where_time_is, int64_t seqtime){
  int64_t last_seq_end_time = 0;
  double last_abs_end_time = 0; // Is double because of reltempo multiplication.

  int64_t block_stime = seqblock_where_time_is == NULL ? -1 : seqtime-seqblock_where_time_is->time;
    
  VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
    
    struct Blocks *block            = seqblock->block;
    double         tempo_multiplier = ATOMIC_DOUBLE_GET(block->reltempo);
    
    int64_t pause_duration  = seqblock->time - last_seq_end_time; // (reltempo is not applied to pauses)
    
    int64_t seq_start_time  = seqblock->time;
    double  abs_start_time  = last_abs_end_time + pause_duration;

    if (seqblock_where_time_is == NULL) {
      if (seqtime < seq_start_time)
        return last_abs_end_time + (seqtime - last_seq_end_time);
    }
    
    if (seqblock == seqblock_where_time_is)
      return abs_start_time + ((double)block_stime / ATOMIC_DOUBLE_GET(seqblock->block->reltempo)); // Important that we round down.
      
    int64_t seq_block_duration = getBlockSTimeLength(seqblock->block);
    int64_t abs_block_duration = ((double)seq_block_duration / tempo_multiplier);
    
    int64_t seq_end_time = seq_start_time + seq_block_duration;
    double  abs_end_time = abs_start_time + abs_block_duration;

    if (seqblock_where_time_is == NULL){
      if (seqtime >= seq_start_time && seqtime < seq_end_time)
        return abs_start_time + ((double) (seqtime-seq_start_time) / ATOMIC_DOUBLE_GET(seqblock->block->reltempo)); // Important that we round down.
    }
    
    //last_abs_start_time = abs_start_time;
    last_seq_end_time   = seq_end_time;
    last_abs_end_time   = abs_end_time;
    
    //printf("  start/end: %f  ->   %f\n",seqblock->start_time,seqblock->end_time);
  }END_VECTOR_FOR_EACH;

  if (seqblock_where_time_is == NULL)
    return last_abs_end_time + (seqtime - last_seq_end_time);

  // !!!
  R_ASSERT(false);
  // !!!
  
  return seqblock_where_time_is->time + ((double)block_stime / ATOMIC_DOUBLE_GET(seqblock_where_time_is->block->reltempo)); // fallback.
}

// Returns in frame format, not in seconds. (int64_t is always in frames, double is always in seconds)
int64_t get_seqtime_from_abstime(const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock_to_ignore, int64_t abstime){
  //int64_t last_seq_start_time = 0;
  int64_t last_seq_end_time = 0;
  //double last_abs_start_time = 0;
  double last_abs_end_time = 0; // Is double because of reltempo multiplication.
    
  VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){

    if (seqblock != seqblock_to_ignore) { // seqblock_to_ignore is used when moving a seqblock.
      
      struct Blocks *block            = seqblock->block;
      double         tempo_multiplier = ATOMIC_DOUBLE_GET(block->reltempo);
      
      int64_t pause_duration  = seqblock->time - last_seq_end_time; // (reltempo is not applied to pauses)
      
      int64_t seq_start_time  = seqblock->time;
      double  abs_start_time  = last_abs_end_time + pause_duration;
      
      if (abstime <= abs_start_time)
        break;
      
      int64_t seq_block_duration = getBlockSTimeLength(seqblock->block);
      int64_t abs_block_duration = ((double)seq_block_duration / tempo_multiplier);
      
      int64_t seq_end_time = seq_start_time + seq_block_duration;
      double  abs_end_time = abs_start_time + abs_block_duration;

      if (abstime <= abs_end_time)
        return scale(abstime, abs_start_time, abs_end_time, seq_start_time, seq_end_time);
      
      //last_abs_start_time = abs_start_time;
      last_seq_end_time   = seq_end_time;
      last_abs_end_time   = abs_end_time;

    }
    
    //printf("  start/end: %f  ->   %f\n",seqblock->start_time,seqblock->end_time);
  }END_VECTOR_FOR_EACH;

  return last_seq_end_time + (abstime - last_abs_end_time); // We lose the decimals here. Wonder if this inaccuracy could build up. Maybe using double/seconds everywhere instead would be better...
}


static bool plays_same_seqblock_completely_later_in_seqtrack(struct SeqTrack *seqtrack, int pos, int64_t before_seqtime){
  const struct SeqBlock *seqblock = (struct SeqBlock *)seqtrack->seqblocks.elements[pos];
  const struct Blocks *block = seqblock->block;
  
  for(int i=pos+1 ; i < seqtrack->seqblocks.num_elements ; i++){

    const struct SeqBlock *seqblock2 = (struct SeqBlock *)seqtrack->seqblocks.elements[pos];
    const struct Blocks *block2 = seqblock2->block;
    int64_t start_seqblock2_seqtime = seqblock2->time;
    int64_t end_seqblock2_seqtime = start_seqblock2_seqtime + getBlockSTimeLength(block2);
    
    if (end_seqblock2_seqtime > before_seqtime)
      return false;
    
    if (block==block2)
      return true;
  }

  return false;
}

void SONG_call_me_before_starting_to_play_song_MIDDLE(int64_t abstime){

  if (abstime==0)
    return;
  
  // Sequencer automation
  //
  // We init sequencer automation before editor automation since sequencer automation is called before editor automation in the player.
  //
  ALL_SEQTRACKS_FOR_EACH(){
    
    SEQTRACK_AUTOMATION_call_me_before_starting_to_play_song_MIDDLE(seqtrack, abstime);
    
  }END_ALL_SEQTRACKS_FOR_EACH;
    

  
  // Editor automation
  //    
  ALL_SEQTRACKS_FOR_EACH(){

    int64_t seqtime = get_seqtime_from_abstime(seqtrack, NULL, abstime);

    for(int i=0 ; i < seqtrack->seqblocks.num_elements ; i++){

      const struct SeqBlock *seqblock = (struct SeqBlock *)seqtrack->seqblocks.elements[i];
      const struct Blocks *block = seqblock->block;

      if (seqblock->time >= seqtime)
        break;
                  
      if (!plays_same_seqblock_completely_later_in_seqtrack(seqtrack, i, seqtime)){

        STime block_stime = R_MIN(getBlockSTimeLength(block), seqtime - seqblock->time);

        FX_call_me_before_starting_to_play_song(seqtrack, seqblock, block_stime);
        
      }
                                           
    }
    
  }END_ALL_SEQTRACKS_FOR_EACH;

}


static int64_t convert_seqtime(struct SeqTrack *from_seqtrack, struct SeqTrack *to_seqtrack, int64_t from_seqtime){
  int64_t abstime = get_abstime_from_seqtime(from_seqtrack, NULL, from_seqtime);
  //printf("in: %f, abstime: %f. out: %f\n",(double)from_seqtime/44100.0, (double)abstime/44100.0, (double)get_seqtime_from_abstime(to_seqtrack, NULL, abstime)/44100.0);
  return get_seqtime_from_abstime(to_seqtrack, NULL, abstime);
}

static struct SeqBlock *SEQBLOCK_create(struct Blocks *block){
  struct SeqBlock *seqblock = (struct SeqBlock*)talloc(sizeof(struct SeqBlock));
  seqblock->block = block;
  seqblock->time = -1;
  seqblock->gfx_time = seqblock->time;
  return seqblock;
}


// Ensures that two seqblocks doesn't overlap, and that a seqblock doesn't start before 0.
// Preserves original pause times.
static void legalize_seqtrack_timing(struct SeqTrack *seqtrack, bool is_gfx){

#if !defined(RELEASE)
  if (!is_gfx)
    R_ASSERT(PLAYER_current_thread_has_lock());
#endif
  
  int64_t last_end_time = 0;
  int64_t time_to_add = 0;
  
  VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
    int64_t seq_block_start = seqblock->time + time_to_add;

    if (seq_block_start < last_end_time) {
      time_to_add += (last_end_time - seq_block_start);
      seq_block_start = last_end_time;
    }

    if (seq_block_start != seqblock->time) {
      if (!is_gfx)
        seqblock->time = seq_block_start;
      seqblock->gfx_time = seq_block_start;
    }

    last_end_time = seq_block_start + getBlockSTimeLength(seqblock->block);
      
  }END_VECTOR_FOR_EACH;

}

void RT_legalize_seqtrack_timing(struct SeqTrack *seqtrack){
  radium::PlayerRecursiveLock lock;
  legalize_seqtrack_timing(seqtrack, false);
}

static void update_all_seqblock_start_and_end_times(struct SeqTrack *seqtrack, bool is_gfx){

  double last_abs_end_time = 0;
  int64_t last_end_time = 0;

  double sample_rate = MIXER_get_sample_rate();

  // seqtrack->seqblocks
  //
  vector_t *seqblocks = &seqtrack->seqblocks;
  VECTOR_FOR_EACH(struct SeqBlock *, seqblock, seqblocks){
    struct Blocks *block            = seqblock->block;
    double         tempo_multiplier = ATOMIC_DOUBLE_GET(block->reltempo);
    
    int64_t seq_block_start = is_gfx ? seqblock->gfx_time : seqblock->time;
    
    int64_t seq_block_pause = seq_block_start - last_end_time; // (reltempo is not applied to pauses)
    double  abs_block_pause = seq_block_pause / sample_rate;
    
    double abs_block_start = last_abs_end_time + abs_block_pause;
    
    int64_t seq_block_duration = getBlockSTimeLength(seqblock->block);
    double  abs_block_duration = ((double)seq_block_duration / tempo_multiplier) / sample_rate;
    
    int64_t seq_end_time = seq_block_start + seq_block_duration;
    double  abs_end_time = abs_block_start + abs_block_duration;
    
    seqblock->start_time = abs_block_start;
    seqblock->end_time = abs_end_time;
    
    last_abs_end_time = abs_end_time;
    last_end_time = seq_end_time;
    
    //printf("  start/end: %f  ->   %f\n",seqblock->start_time,seqblock->end_time);
  }END_VECTOR_FOR_EACH;
  

  // seqtrack->gfx_gfx_seqblocks
  //
  seqblocks = &seqtrack->gfx_gfx_seqblocks;
  VECTOR_FOR_EACH(struct SeqBlock *, seqblock, seqblocks){
    seqblock->start_time = get_abstime_from_seqtime(seqtrack, NULL, seqblock->time) / sample_rate;
    seqblock->end_time = get_abstime_from_seqtime(seqtrack, NULL, seqblock->time + getBlockSTimeLength(seqblock->block)) / sample_rate;
    //printf("   %d: %f %f\n", iterator666, seqblock->start_time / 44100.0, seqblock->end_time / 44100.0);
  }END_VECTOR_FOR_EACH;
}

void SEQTRACK_update_all_seqblock_start_and_end_times(struct SeqTrack *seqtrack){
  update_all_seqblock_start_and_end_times(seqtrack, false);
}

void SEQTRACK_update_all_seqblock_gfx_start_and_end_times(struct SeqTrack *seqtrack){
  update_all_seqblock_start_and_end_times(seqtrack, true);
}

// Don't need player lock here. 'start_time' and 'end_time' is only used in the main thread.
void SEQUENCER_update_all_seqblock_start_and_end_times(void){
  R_ASSERT(THREADING_is_main_thread());
  
  VECTOR_FOR_EACH(struct SeqTrack *, seqtrack, &root->song->seqtracks){
    SEQTRACK_update_all_seqblock_start_and_end_times(seqtrack);
  }END_VECTOR_FOR_EACH;
}


/**
 * Find closest bar start, start
 */

static struct SeqTrack *find_closest_seqtrack_with_barorbeat_start(int seqtracknum){
#if 1
  return (struct SeqTrack*)root->song->seqtracks.elements[0];
#else
  if (seqtracknum==0)
    return (struct SeqTrack*)root->song->seqtracks.elements[0];

  seqtracknum--;
  
  while(seqtracknum > 0){
    const struct SeqTrack *seqtrack = (struct SeqTrack*)root->song->seqtracks.elements[seqtracknum];
    if (seqtrack->seqblocks.num_elements > 0)
      return seqtrack;

    seqtracknum--;
  }

  return (struct SeqTrack*)root->song->seqtracks.elements[0];
#endif
}

/*
static int64_t find_bar_start_before(struct SeqBlock *seqblock, int64_t seqtime){
  struct Blocks *block = seqblock->block;

  struct Beats *beat = NextBeat(block->beats);
  while (beat != NULL){
    if (beat->beat_num==1)
      break;
    beat = NextBeat(beat);
  }

  int64_t bar_length;
  
  if (beat==NULL)
    bar_length = getBlockSTimeLength(block);
  else
    bar_length = Place2STime(block, &beat->l.p);
  
  return seqblock->time - bar_length;
}
*/

namespace{
  enum class WhatToFind{
    NOTHING,
    LINE,
    BEAT,
    BAR
  };
}

static int64_t find_barorbeat_start_inside(const struct SeqBlock *seqblock, int64_t seqtime, WhatToFind what){
  const struct Blocks *block = seqblock->block;

  int64_t ret = seqblock->time;
  int64_t mindist = INT64_MAX;
  
  if (what==WhatToFind::NOTHING)
    return seqtime;
  
  if (what==WhatToFind::LINE){
    for(int line=0;line<block->num_lines;line++){
      int64_t stime = seqblock->time + block->times[line].time;
      int64_t dist = R_ABS(stime-seqtime);
      //printf("bar/beat: %d/%d. seqtime: %f. bartime: %f. dist: %f\n",beat->bar_num,beat->beat_num,(double)seqtime/44100.0, (double)bartime/44100.0,(double)dist/44100.0);
      if (dist < mindist){
        mindist = dist;
        ret = stime;
      }
      if (stime >= seqtime)
        break;
    }
    return ret;
  }

  const struct Beats *beat = block->beats;

  while (beat != NULL){
    if (beat->beat_num==1 || what==WhatToFind::BEAT){
      int64_t beattime = seqblock->time + Place2STime(block, &beat->l.p);
      int64_t dist = R_ABS(beattime-seqtime);
      //printf("bar/beat: %d/%d. seqtime: %f. bartime: %f. dist: %f\n",beat->bar_num,beat->beat_num,(double)seqtime/44100.0, (double)bartime/44100.0,(double)dist/44100.0);
      if (dist < mindist){
        mindist = dist;
        ret = beattime;
      }
      if (beattime >= seqtime)
        break;
    }
    beat = NextBeat(beat);
  }

  //printf("  GOT: %f.\n\n", (double)ret/44100.0);
  return ret;
}

static int64_t find_barorbeat_start_after(const struct SeqBlock *seqblock, int64_t seqtime, int64_t maxtime, WhatToFind what){
  const struct Blocks *block = seqblock->block;
  int64_t blocklen = getBlockSTimeLength(block);
  int64_t interval_length;

  if (what==WhatToFind::NOTHING) {

    return seqtime;
  
  } else if (what==WhatToFind::LINE){
    
    interval_length = blocklen - block->times[block->num_lines-1].time;

  } else {

    const struct Beats *last_barorbeat = NULL;
    
    const struct Beats *beat = NextBeat(block->beats);
    while (beat != NULL){
      if (beat->beat_num==1 || what==WhatToFind::BEAT)
        last_barorbeat = beat;
      beat = NextBeat(beat);
    }
    
    if (last_barorbeat==NULL)
      interval_length = blocklen;
    else
      interval_length = blocklen - Place2STime(block, &last_barorbeat->l.p);
  }

  interval_length /= ATOMIC_DOUBLE_GET(seqblock->block->reltempo);

  int64_t ret = seqblock->time + blocklen;
  int64_t mindiff = R_ABS(ret-seqtime);
  int64_t lastdiff = mindiff;
  
  int64_t maybe = ret + interval_length;
  while(maybe < maxtime){
    int64_t diff = R_ABS(maybe-seqtime);
    if (diff > lastdiff)
      break;
    
    if(diff < mindiff){
      mindiff = diff;
      ret = maybe;
    }

    lastdiff = diff;
    maybe += interval_length;
  }
  
  return ret;
}

int64_t find_closest_barorbeat_start(int seqtracknum, int64_t pos_abstime, WhatToFind what){

  //struct SeqTrack *pos_seqtrack = (struct SeqTrack*)root->song->seqtracks.elements[seqtracknum];
  struct SeqTrack *seqtrack = find_closest_seqtrack_with_barorbeat_start(seqtracknum);

  //printf("pos_seqtime: %f\n",(double)pos_seqtime/44100.0);
  //int64_t seqtime = convert_seqtime(pos_seqtrack, seqtrack, pos_seqtime);
  int64_t seqtime = get_seqtime_from_abstime(seqtrack, NULL, pos_abstime);
                         
  int64_t barorbeat_start_time = 0;

  struct SeqBlock *last_seqblock = NULL;
  
  VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
    
    int64_t starttime = seqblock->time;
    int64_t endtime = seqblock->time + getBlockSTimeLength(seqblock->block);
    
    if (seqtime >= starttime && seqtime < endtime) {
      barorbeat_start_time = find_barorbeat_start_inside(seqblock, seqtime, what);
      goto gotit;
    }
    
    if (seqtime < starttime && last_seqblock==NULL) {
      return pos_abstime;
      //bar_start_time = pos_seqtime; //find_bar_start_before(seqblock, seqtime);
      //goto gotit;
    }
    
    if (seqtime < starttime) {
      barorbeat_start_time = find_barorbeat_start_after(last_seqblock, seqtime, starttime, what);
      goto gotit;
    }
    
    last_seqblock = seqblock;
  }END_VECTOR_FOR_EACH;

  if (last_seqblock==NULL)
    return pos_abstime;
  else
    barorbeat_start_time = find_barorbeat_start_after(last_seqblock, seqtime, INT64_MAX, what);
  
 gotit:

  //printf("Converting %f to %f\n",(double)bar_start_time/44100.0, (double)convert_seqtime(seqtrack, pos_seqtrack, bar_start_time)/44100.0);
  //return convert_seqtime(seqtrack, pos_seqtrack, bar_start_time);
  return get_abstime_from_seqtime(seqtrack, NULL, barorbeat_start_time);
}

int64_t SEQUENCER_find_closest_bar_start(int seqtracknum, int64_t pos_abstime){
  return find_closest_barorbeat_start(seqtracknum, pos_abstime, WhatToFind::BAR);
}

int64_t SEQUENCER_find_closest_beat_start(int seqtracknum, int64_t pos_abstime){
  return find_closest_barorbeat_start(seqtracknum, pos_abstime, WhatToFind::BEAT);
}


int64_t SEQUENCER_find_closest_line_start(int seqtracknum, int64_t pos_abstime){
  return find_closest_barorbeat_start(seqtracknum, pos_abstime, WhatToFind::LINE);
}

/**
 * Find closest bar start, end
 */


static void set_plain_seqtrack_timing_no_pauses(struct SeqTrack *seqtrack){
  R_ASSERT(PLAYER_current_thread_has_lock());
    
  int64_t time = 0;
  
  VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
    seqblock->time = time;
    seqblock->gfx_time = time;
    time += getBlockSTimeLength(seqblock->block);
  }END_VECTOR_FOR_EACH;
}


hash_t *SEQBLOCK_get_state(const struct SeqBlock *seqblock){
  hash_t *state = HASH_create(2);
  
  HASH_put_int(state, "blocknum", seqblock->block->l.num);
  HASH_put_int(state, "time", seqblock->time);
  HASH_put_float(state, "samplerate", MIXER_get_sample_rate());
  
  return state;
}

struct SeqBlock *SEQBLOCK_create_from_state(const hash_t *state){
  
  int blocknum = HASH_get_int32(state, "blocknum");

  double samplerate = HASH_get_float(state, "samplerate");

  struct SeqBlock *seqblock = SEQBLOCK_create((struct Blocks*)ListFindElement1(&root->song->blocks->l, blocknum));
  if (seqblock==NULL){
    // not supposed to happen, but it did happen once when undoing/redoing a lot after running the api autotester.
    // (assertion reporter popped up in the call to ListFindElement1 above)
    seqblock = SEQBLOCK_create((struct Blocks*)ListFindElement1(&root->song->blocks->l, 0));
  }
  
  seqblock->time = round(double(HASH_get_int(state, "time")) * MIXER_get_sample_rate() / samplerate);

  seqblock->gfx_time = seqblock->time;
  
  return seqblock;
}

static void seqtrackgcfinalizer(void *actual_mem_start, void *user_data){
  struct SeqTrack *seqtrack = (struct SeqTrack*)user_data;
  //printf("FINALIZING seqtrack\n");
  //getchar();
  SEQTRACK_AUTOMATION_free(seqtrack->seqtrackautomation);
}

struct SeqTrack *SEQTRACK_create(const hash_t *automation_state){
  struct SeqTrack *seqtrack = (struct SeqTrack*)talloc(sizeof(struct SeqTrack));

  memset(seqtrack, 0, sizeof(struct SeqTrack));
  seqtrack->scheduler = SCHEDULER_create();

  auto *seqtrackautomation = SEQTRACK_AUTOMATION_create(seqtrack, automation_state);
  seqtrack->seqtrackautomation = seqtrackautomation;
  
  GC_register_finalizer(seqtrack, seqtrackgcfinalizer, seqtrack, NULL, NULL);

  return seqtrack;
}
                   
hash_t *SEQTRACK_get_state(const struct SeqTrack *seqtrack){
  hash_t *state = HASH_create(seqtrack->seqblocks.num_elements);

  for(int i=0;i<seqtrack->seqblocks.num_elements;i++)
    HASH_put_hash_at(state, "seqblock", i, SEQBLOCK_get_state((struct SeqBlock*)seqtrack->seqblocks.elements[i]));

  HASH_put_hash(state, "automation", SEQTRACK_AUTOMATION_get_state(seqtrack->seqtrackautomation));
                
  return state;
}

struct SeqTrack *SEQTRACK_create_from_state(const hash_t *state){
  const hash_t *automation_state = NULL;
  if (HASH_has_key(state, "automation"))
    automation_state = HASH_get_hash(state, "automation");

  struct SeqTrack *seqtrack = SEQTRACK_create(automation_state);

  int num_seqblocks = HASH_get_array_size(state, "seqblock");

  for(int i=0;i<num_seqblocks;i++)
    VECTOR_push_back(&seqtrack->seqblocks, SEQBLOCK_create_from_state(HASH_get_hash_at(state, "seqblock", i)));

  RT_legalize_seqtrack_timing(seqtrack); // Block length may change slightly between versions due to different ways to calculate timings.

  return seqtrack;
}

// Compatibility with old songs
struct SeqTrack *SEQTRACK_create_from_playlist(const int *playlist, int len){
  vector_t seqblocks = {0};
  
  for(int pos=0;pos<len;pos++)
    VECTOR_push_back(&seqblocks, SEQBLOCK_create((struct Blocks *)ListFindElement1(&root->song->blocks->l,playlist[pos])));
  
  struct SeqTrack *seqtrack = SEQTRACK_create(NULL);
    
  seqtrack->seqblocks = seqblocks;

  {
    radium::PlayerLock lock;
    set_plain_seqtrack_timing_no_pauses(seqtrack);
  }
  
  return seqtrack;
}

void SEQTRACK_delete_seqblock(struct SeqTrack *seqtrack, const struct SeqBlock *seqblock){
  int pos = VECTOR_find_pos(&seqtrack->seqblocks, seqblock);
  R_ASSERT_RETURN_IF_FALSE(pos>=0);

  int64_t abstimes[seqtrack->seqblocks.num_elements];

  VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
    abstimes[iterator666] = get_abstime_from_seqtime(seqtrack, NULL, seqblock->time);
    //printf("bef %d: %f\n", iterator666, abstimes[iterator666] / 44100.0);
  }END_VECTOR_FOR_EACH;

  //printf("    SEQTRACK_delete_seqblock\n");
  
  {
    radium::PlayerPause pause(is_playing_song());
    radium::PlayerRecursiveLock lock;
      
    VECTOR_delete(&seqtrack->seqblocks, pos);

    VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
      if (iterator666 >= pos){
        seqblock->time = get_seqtime_from_abstime(seqtrack, seqblock, abstimes[iterator666+1]);
        seqblock->gfx_time = seqblock->time;
        //printf("Skewing %f -> %f\n", (seqblock->time-skew) / 44100.0, seqblock->time / 44100.0);
      }
    }END_VECTOR_FOR_EACH;

    RT_legalize_seqtrack_timing(seqtrack);  // Shouldn't be necessary, but we call it just in case.
  }

  RT_SEQUENCER_update_sequencer_and_playlist();
}

void SEQTRACK_delete_gfx_gfx_seqblock(struct SeqTrack *seqtrack, const struct SeqBlock *seqblock){
  VECTOR_remove(&seqtrack->gfx_gfx_seqblocks, seqblock);
}

static const struct SeqBlock *get_prev_seqblock(const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock){
  int pos = VECTOR_find_pos(&seqtrack->seqblocks, seqblock);
  R_ASSERT_RETURN_IF_FALSE2(pos!=-1, NULL);
  
  if (pos==0)
    return NULL;
  else
    return (struct SeqBlock*)seqtrack->seqblocks.elements[pos-1];
}

static const struct SeqBlock *get_next_seqblock(const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock){
  int pos = VECTOR_find_pos(&seqtrack->seqblocks, seqblock);
  R_ASSERT_RETURN_IF_FALSE2(pos!=-1, NULL);
  
  if (pos==seqtrack->seqblocks.num_elements-1)
    return NULL;
  else
    return (struct SeqBlock*)seqtrack->seqblocks.elements[pos+1];
}

// 'how_much' can be negative.
void SEQTRACK_move_all_seqblocks_to_the_right_of(struct SeqTrack *seqtrack, int seqblocknum, int64_t how_much){
  printf("move_all_to_left %d %d\n",seqblocknum, (int)how_much);

  {
    radium::PlayerPause pause(is_playing_song());
    radium::PlayerLock lock;
    
    VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
      if (iterator666 >= seqblocknum) {
        seqblock->time += how_much;
        seqblock->gfx_time = seqblock->time;

        printf("  Dec %d in %d by %d\n", (int)seqblock->time, iterator666, (int)how_much);
      }
    }END_VECTOR_FOR_EACH;
    
    RT_legalize_seqtrack_timing(seqtrack);
  }

  RT_SEQUENCER_update_sequencer_and_playlist();
}

static void move_seqblock(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, int64_t new_abs_time, bool is_gfx){
  R_ASSERT_RETURN_IF_FALSE(seqblock!=NULL);

  int64_t new_seqblock_time = get_seqtime_from_abstime(seqtrack, seqblock, new_abs_time);
  
  if (new_seqblock_time < 0)
    new_seqblock_time = 0;

  //printf("abs_time: %f.   seqblock_time: %f\n",(double)new_abs_time/48000.0, (double)new_seqblock_time/48000.0);
  
  const struct SeqBlock *prev_seqblock = get_prev_seqblock(seqtrack, seqblock);
  const struct SeqBlock *next_seqblock = get_next_seqblock(seqtrack, seqblock);

  int64_t time1 = is_gfx ? seqblock->gfx_time : seqblock->time;
  int64_t time2 = time1 + getBlockSTimeLength(seqblock->block);
  int64_t duration = time2-time1;
  
  int64_t mintime;
  int64_t maxtime;

  if (false && is_gfx) {
    mintime = 0;
    maxtime = SONG_get_gfx_length() + 10000000;
  } else {
    mintime = prev_seqblock==NULL ?  0 : prev_seqblock->time + getBlockSTimeLength(prev_seqblock->block);
    maxtime = next_seqblock==NULL ? -1 : next_seqblock->time - duration;

    if (maxtime==-1)
      new_seqblock_time = R_MAX(mintime, new_seqblock_time);
    else
      new_seqblock_time = R_BOUNDARIES(mintime, new_seqblock_time, maxtime);
  }
  
  if (new_seqblock_time==time1)
    return;

  double old_song_visible_length = SONG_get_gfx_length();

  if (is_gfx) {
    
    seqblock->gfx_time = new_seqblock_time;
    
    legalize_seqtrack_timing(seqtrack, true);
    
  } else {
    radium::PlayerPause pause(is_playing_song());
    radium::PlayerLock lock;

    seqblock->time = new_seqblock_time;
    seqblock->gfx_time = seqblock->time;

    legalize_seqtrack_timing(seqtrack, false);
  }

  double new_song_visible_length = SONG_get_gfx_length();

  if (!is_gfx || new_song_visible_length != old_song_visible_length || seqtrack==root->song->seqtracks.elements[0])
    SEQUENCER_update();
  else{
    SEQTRACK_update(seqtrack);
    SEQNAV_update();
  }

  BS_UpdatePlayList();
}

void SEQTRACK_move_seqblock(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, int64_t new_abs_time){
  move_seqblock(seqtrack, seqblock, new_abs_time, false);
}

void SEQTRACK_move_gfx_seqblock(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, int64_t new_abs_time){
  move_seqblock(seqtrack, seqblock, new_abs_time, true);
}

void SEQTRACK_move_gfx_gfx_seqblock(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, int64_t new_abs_time){
  int64_t new_seqblock_time = get_seqtime_from_abstime(seqtrack, seqblock, new_abs_time);
  
  if (new_seqblock_time < 0)
    new_seqblock_time = 0;

  seqblock->time = new_seqblock_time;
  seqblock->gfx_time = seqblock->time;

  SEQUENCER_update();
}

void SEQTRACK_insert_silence(struct SeqTrack *seqtrack, int64_t seqtime, int64_t length){

  {
    radium::PlayerPause pause(is_playing_song());
    radium::PlayerLock lock;
    
    VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){

      if (seqblock->time >= seqtime) {
        seqblock->time += length;
        seqblock->gfx_time = seqblock->time;
      }
      
    }END_VECTOR_FOR_EACH;

    RT_legalize_seqtrack_timing(seqtrack);
  }
      
  SEQUENCER_update();
  BS_UpdatePlayList();
}

static int get_seqblock_pos(vector_t *seqblocks, int64_t seqtime){  

  VECTOR_FOR_EACH(struct SeqBlock *, seqblock, seqblocks){

    if (seqblock->time >= seqtime)
      return iterator666;
    
  }END_VECTOR_FOR_EACH;

  return seqblocks->num_elements;
}

int SEQTRACK_insert_seqblock(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, int64_t seqtime){
  // Assert that the seqblock is not in a seqtrack already.
  VECTOR_FOR_EACH(struct SeqTrack *, seqtrack_here, &root->song->seqtracks){
    R_ASSERT_RETURN_IF_FALSE2(!VECTOR_is_in_vector(&seqtrack_here->seqblocks, seqblock), 0);
  }END_VECTOR_FOR_EACH;

  int64_t abstimes[R_MAX(1, seqtrack->seqblocks.num_elements)]; // arrays of size 0 causes ubsan hit

  VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
    abstimes[iterator666] = get_abstime_from_seqtime(seqtrack, NULL, seqblock->time);
    //printf("bef %d: %f\n", iterator666, abstimes[iterator666] / 44100.0);
  }END_VECTOR_FOR_EACH;

  int pos = get_seqblock_pos(&seqtrack->seqblocks, seqtime);
    
  VECTOR_ensure_space_for_one_more_element(&seqtrack->seqblocks);

  {
    radium::PlayerPause pause(is_playing_song());
    radium::PlayerLock lock;

    seqblock->time = seqtime;
    seqblock->gfx_time = seqblock->time;
    
    VECTOR_insert(&seqtrack->seqblocks, seqblock, pos);

    VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
      if (iterator666 > pos){
        seqblock->time = get_seqtime_from_abstime(seqtrack, seqblock, abstimes[iterator666-1]);
        seqblock->gfx_time = seqblock->time;
      }
    }END_VECTOR_FOR_EACH;

    RT_legalize_seqtrack_timing(seqtrack);
  }

  RT_SEQUENCER_update_sequencer_and_playlist();

  return pos;
}

int SEQTRACK_insert_block(struct SeqTrack *seqtrack, struct Blocks *block, int64_t seqtime){
  struct SeqBlock *seqblock = SEQBLOCK_create(block);
  return SEQTRACK_insert_seqblock(seqtrack, seqblock, seqtime);
}

int SEQTRACK_insert_gfx_gfx_block(struct SeqTrack *seqtrack, struct Blocks *block, int64_t seqtime){
  struct SeqBlock *seqblock = SEQBLOCK_create(block);
  seqblock->time = seqtime;
  seqblock->gfx_time = seqblock->time;
  seqblock->is_selected = true;

  vector_t *seqblocks = &seqtrack->gfx_gfx_seqblocks;

  int pos = get_seqblock_pos(seqblocks, seqtime);
  VECTOR_insert(seqblocks, seqblock, pos);

  SEQUENCER_update();

  return pos;
}

double SEQTRACK_get_length(struct SeqTrack *seqtrack){
  int num_seqblocks = seqtrack->seqblocks.num_elements;
    
  if (num_seqblocks==0)
    return 0.0;
  
  SEQTRACK_update_all_seqblock_start_and_end_times(seqtrack);
  
  struct SeqBlock *last_seqblock = (struct SeqBlock*)seqtrack->seqblocks.elements[num_seqblocks-1];

  return last_seqblock->end_time;
}

double SEQTRACK_get_gfx_length(struct SeqTrack *seqtrack){
  int num_seqblocks = seqtrack->seqblocks.num_elements;
    
  if (num_seqblocks==0)
    return 0.0;
  
  SEQTRACK_update_all_seqblock_gfx_start_and_end_times(seqtrack);
  
  struct SeqBlock *last_seqblock = (struct SeqBlock*)seqtrack->seqblocks.elements[num_seqblocks-1];

  return last_seqblock->end_time;
}

void SEQUENCER_remove_block_from_seqtracks(struct Blocks *block){
  QVector<QPair<struct SeqTrack*, struct SeqBlock* > > to_remove;
  
  VECTOR_FOR_EACH(struct SeqTrack *, seqtrack, &root->song->seqtracks){
    VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
      if (seqblock->block==block)
        to_remove.push_back(QPair<struct SeqTrack*, struct SeqBlock* >(seqtrack, seqblock));
    }END_VECTOR_FOR_EACH;
  }END_VECTOR_FOR_EACH;

  if(to_remove.size() > 0){
    radium::PlayerPause pause(is_playing_song());
    radium::PlayerLock lock;

    for(auto pair : to_remove)
      SEQTRACK_delete_seqblock(pair.first, pair.second);
  }

  RT_SEQUENCER_update_sequencer_and_playlist();
}

void SEQUENCER_insert_seqtrack(struct SeqTrack *new_seqtrack, int pos){

  if (new_seqtrack==NULL)
    new_seqtrack = SEQTRACK_create(NULL);
  
  VECTOR_ensure_space_for_one_more_element(&root->song->seqtracks);

  {
    radium::PlayerPause pause(is_playing_song());
    radium::PlayerLock lock;
            
    VECTOR_insert(&root->song->seqtracks, new_seqtrack, pos);
  }

  RT_SEQUENCER_update_sequencer_and_playlist();
}

void SEQUENCER_append_seqtrack(struct SeqTrack *new_seqtrack){
  SEQUENCER_insert_seqtrack(new_seqtrack, root->song->seqtracks.num_elements);
}
  
void SEQUENCER_replace_seqtrack(struct SeqTrack *new_seqtrack, int pos){
  {
    radium::PlayerPause pause(is_playing_song());
    radium::PlayerLock lock;
    
    VECTOR_set(&root->song->seqtracks, pos, new_seqtrack);

    RT_SEQUENCER_update_sequencer_and_playlist();
  }
  
}

void SEQUENCER_delete_seqtrack(int pos){
  R_ASSERT_RETURN_IF_FALSE(pos >= 0);
  R_ASSERT_RETURN_IF_FALSE(root->song->seqtracks.num_elements > 1); // There must always be a seqtrack
  R_ASSERT_RETURN_IF_FALSE(pos < root->song->seqtracks.num_elements);
  
  {
    radium::PlayerPause pause(is_playing_song());
    radium::PlayerLock lock;
    
    VECTOR_delete(&root->song->seqtracks, pos);

    int curr_seqtracknum = ATOMIC_GET(root->song->curr_seqtracknum);
    if (curr_seqtracknum >= root->song->seqtracks.num_elements)
      ATOMIC_SET(root->song->curr_seqtracknum, root->song->seqtracks.num_elements -1);
    
    RT_SEQUENCER_update_sequencer_and_playlist();      
  }
  
}

void SEQUENCER_set_looping(bool do_loop){
  ATOMIC_SET(root->song->looping.enabled, do_loop);
  SEQUENCER_update();
}

bool SEQUENCER_is_looping(void){
  return ATOMIC_GET(root->song->looping.enabled);
}

void SEQUENCER_set_loop_start(int64_t start){  
  root->song->looping.start = R_BOUNDARIES(0, start, ATOMIC_GET(root->song->looping.end)-1);
  SEQUENCER_update();
}

int64_t SEQUENCER_get_loop_start(void){
  return root->song->looping.start;
}

void SEQUENCER_set_loop_end(int64_t end){
  ATOMIC_SET(root->song->looping.end, R_BOUNDARIES(root->song->looping.start+1, end, SONG_get_gfx_length()*MIXER_get_sample_rate()));
  //printf("   Set end. %d %d %d\n",(int)(root->song->looping.start+1), (int)end, (int)(SONG_get_length()*MIXER_get_sample_rate()));
  SEQUENCER_update();
}

int64_t SEQUENCER_get_loop_end(void){
  return ATOMIC_GET(root->song->looping.end);
}

double SONG_get_length(void){
  double len = 0;
  
  VECTOR_FOR_EACH(struct SeqTrack *, seqtrack, &root->song->seqtracks){
    double seqtrack_len = SEQTRACK_get_length(seqtrack);
    if (seqtrack_len > len)
      len = seqtrack_len;
  }END_VECTOR_FOR_EACH;

  return len;
}

double SONG_get_gfx_length(void){
  double len = 0;
  
  VECTOR_FOR_EACH(struct SeqTrack *, seqtrack, &root->song->seqtracks){
    double seqtrack_len = SEQTRACK_get_gfx_length(seqtrack);
    if (seqtrack_len > len)
      len = seqtrack_len;
  }END_VECTOR_FOR_EACH;

  return len + SEQUENCER_EXTRA_SONG_LENGTH;
}

// Called from SONG_create()
void SEQUENCER_init(struct Song *song){
  TEMPOAUTOMATION_reset();
  song->block_seqtrack = SEQTRACK_create(NULL);
  song->looping.start = 0;

  if (g_is_starting_up)
    ATOMIC_SET(song->looping.end, 30 * 48000.0);
  else
    ATOMIC_SET(song->looping.end, 30 * MIXER_get_sample_rate());
}

// Only called during program startup
void SONG_init(void){
  struct SeqTrack *seqtrack = SEQTRACK_create(NULL);

  VECTOR_ensure_space_for_one_more_element(&seqtrack->seqblocks);
  
  VECTOR_ensure_space_for_one_more_element(&root->song->seqtracks);
  
  struct SeqBlock *seqblock = SEQBLOCK_create(root->song->blocks);

  SEQUENCER_init(root->song);
  
  PLAYER_lock();{
    
    VECTOR_clean(&root->song->seqtracks);
    
    VECTOR_push_back(&root->song->seqtracks, seqtrack);
    
    VECTOR_push_back(&seqtrack->seqblocks, seqblock);

    set_plain_seqtrack_timing_no_pauses(seqtrack);

  }PLAYER_unlock();
}


hash_t *SEQUENCER_get_state(void){
  hash_t *state = HASH_create(root->song->seqtracks.num_elements);

  VECTOR_FOR_EACH(const struct SeqTrack *, seqtrack, &root->song->seqtracks){
    hash_t *seqtrack_state = SEQTRACK_get_state(seqtrack);
    HASH_put_hash_at(state, "seqtracks", iterator666, seqtrack_state);
  }END_VECTOR_FOR_EACH;

  HASH_put_int(state, "curr_seqtracknum", ATOMIC_GET(root->song->curr_seqtracknum));

  // I'm not 100% sure, but I think we need this one since song tempo automation automatically changes length when the song changes length.
  // (modifying song tempo automation is a light operation + that it's atomically real time safe, so it doesn't matter much if we do this)
  HASH_put_hash(state, "song_tempo_automation", TEMPOAUTOMATION_get_state());

  HASH_put_bool(state, "looping_enabled", SEQUENCER_is_looping());
  HASH_put_int(state, "loop_start", SEQUENCER_get_loop_start());
  HASH_put_int(state, "loop_end", SEQUENCER_get_loop_end());
  
  return state;
}

void SEQUENCER_create_from_state(hash_t *state){

  {
    SEQUENCER_ScopedGfxDisable gfx_disable;
    
    //printf("        CREATING FROM STATE\n");

    // Need to do this first since widgets are not positioned correctly if it's done last. Not quite sure why.
    if(HASH_has_key(state, "song_tempo_automation"))
      TEMPOAUTOMATION_create_from_state(HASH_get_hash(state, "song_tempo_automation"));

    
    vector_t seqtracks = {0};
  
    int num_seqtracks = HASH_get_array_size(state, "seqtracks");
    R_ASSERT_RETURN_IF_FALSE(num_seqtracks > 0);
    
    for(int i = 0 ; i < num_seqtracks ; i++){
      struct SeqTrack *seqtrack = SEQTRACK_create_from_state(HASH_get_hash_at(state, "seqtracks", i));
      VECTOR_push_back(&seqtracks, seqtrack);
    }
    
    int new_curr_seqtracknum = HASH_has_key(state, "curr_seqtracknum") ? HASH_get_int32(state, "curr_seqtracknum") : 0;
    
    {
      radium::PlayerPause pause(is_playing_song());
      radium::PlayerLock lock;
      
      root->song->seqtracks = seqtracks;

      ATOMIC_SET(root->song->curr_seqtracknum, new_curr_seqtracknum);
    }

    if(HASH_has_key(state, "loop_start")) {
      root->song->looping.start = HASH_get_int(state, "loop_start");
      ATOMIC_SET(root->song->looping.end, HASH_get_int(state, "loop_end"));
      SEQUENCER_set_looping(HASH_get_bool(state, "looping_enabled"));
    } else {
      root->song->looping.start = 0;
      ATOMIC_SET(root->song->looping.end, 30 * MIXER_get_sample_rate());
      SEQUENCER_set_looping(false);
    }
  }

  BS_UpdatePlayList();
  SEQUENCER_update();
}



hash_t *SEQUENCER_get_automations_state(void){
  hash_t *state = HASH_create(root->song->seqtracks.num_elements);

  VECTOR_FOR_EACH(const struct SeqTrack *, seqtrack, &root->song->seqtracks){
    hash_t *seqtrack_state = SEQTRACK_AUTOMATION_get_state(seqtrack->seqtrackautomation);
    HASH_put_hash_at(state, "seqtrackautomations", iterator666, seqtrack_state);
  }END_VECTOR_FOR_EACH;

  return state;
}

void SEQUENCER_create_automations_from_state(hash_t *state){
  int num_seqtracks = HASH_get_array_size(state, "seqtrackautomations");
  R_ASSERT_RETURN_IF_FALSE(num_seqtracks > 0);
  R_ASSERT_RETURN_IF_FALSE(num_seqtracks == root->song->seqtracks.num_elements);
  
  for(int i = 0 ; i < num_seqtracks ; i++){
    struct SeqTrack *seqtrack = (struct SeqTrack *)root->song->seqtracks.elements[i];

    auto *old_seqtrackautomation = seqtrack->seqtrackautomation;
    auto *new_seqtrackautomation = SEQTRACK_AUTOMATION_create(seqtrack, HASH_get_hash_at(state, "seqtrackautomations", i));

    {
      radium::PlayerLock lock;
      seqtrack->seqtrackautomation = new_seqtrackautomation;
    }

    SEQTRACK_AUTOMATION_free(old_seqtrackautomation);
  }

  SEQUENCER_update();
}

