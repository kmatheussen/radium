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

#include <QFileInfo>
#include <QSet>

#define RADIUM_ACCESS_SEQBLOCK_ENVELOPE 1

#include "nsmtracker.h"
#include "player_proc.h"
#include "vector_proc.h"
#include "placement_proc.h"
#include "player_pause_proc.h"
#include "time_proc.h"
#include "hashmap_proc.h"
#include "list_proc.h"
#include "disk.h"
#include "OS_Player_proc.h"
#include "scheduler_proc.h"
#include "../audio/Mixer_proc.h"
#include "OS_Bs_edit_proc.h"
#include "song_tempo_automation_proc.h"
#include "seqtrack_automation_proc.h"
#include "seqblock_envelope_proc.h"
#include "patch_proc.h"
#include "Semaphores.hpp"
#include "Dynvec_proc.h"
#include "instruments_proc.h"
#include "visual_proc.h"

#include "../api/api_proc.h"

#include "../audio/SoundPlugin.h"
#include "../audio/Seqtrack_plugin_proc.h"
#include "../audio/Juce_plugins_proc.h"

#include "seqtrack_proc.h"

static int seqblocks_comp(const void *a, const void *b){
  const struct SeqBlock *s1 = (const struct SeqBlock *)a;
  const struct SeqBlock *s2 = (const struct SeqBlock *)b;
  
  if (s1->t.time < s2->t.time)
    return -1;
  else if (s1->t.time > s2->t.time)
    return 1;
  else
    return 0;
}


static int64_t get_seqblock_stime_default_duration(const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock){
  if (seqblock->block==NULL) {
    
    R_ASSERT_RETURN_IF_FALSE2(seqblock->sample_id>=0, pc->pfreq);
    R_ASSERT_RETURN_IF_FALSE2(seqtrack->patch!=NULL, pc->pfreq);
    
    struct SoundPlugin *plugin = (struct SoundPlugin*) seqtrack->patch->patchdata;
    R_ASSERT_RETURN_IF_FALSE2(plugin!=NULL, pc->pfreq);
    
    int64_t num_frames = SEQTRACKPLUGIN_get_total_num_frames_for_sample(plugin, seqblock->sample_id);
    R_ASSERT_RETURN_IF_FALSE2(num_frames>0, pc->pfreq);
    
    return num_frames;
    
  } else {
    
    return getBlockSTimeLength(seqblock->block);
    
  }
}


static inline int64_t get_seqblock_endtime(const struct SeqBlock *seqblock){
  return seqblock->t.time2;
}

static inline int64_t get_seqblock_duration(const struct SeqBlock *seqblock){
  return seqblock->t.time2 - seqblock->t.time;
}

static int64_t SEQBLOCK_get_seq_time(const struct SeqBlock *seqblock, int64_t blocktime){
  return seqblock->t.time + blocktime_to_seqtime(seqblock, blocktime);
}

static void move_seqblock(struct SeqBlock *seqblock, int64_t new_start){
  int64_t duration = get_seqblock_duration(seqblock);

  seqblock->t.time = new_start;
  seqblock->t.time2 = new_start + duration;
}

void SEQTRACK_move_seqblock(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, int64_t new_start){
  R_ASSERT_RETURN_IF_FALSE(new_start >= 0);
  
  radium::PlayerPause pause;
  radium::PlayerRecursiveLock lock;
  move_seqblock(seqblock, new_start);
  RT_legalize_seqtrack_timing(seqtrack, NULL);
}
  
static bool seqblock_has_stretch(const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock){
  const SeqBlockTimings &timing = seqblock->t;

  return (timing.interior_end - timing.interior_start) != get_seqblock_duration(seqblock);
}


static void set_seqblock_stretch(const struct SeqTrack *seqtrack, struct SeqBlock *seqblock){
  double reltempo = seqblock->block==NULL ? 1.0 : ATOMIC_DOUBLE_GET(seqblock->block->reltempo);

  seqblock->t.stretch = (double)get_seqblock_duration(seqblock) / (double)(seqblock->t.interior_end - seqblock->t.interior_start);
  if (reltempo != 1.0)
    seqblock->t.stretch_without_tempo_multiplier = seqblock->t.stretch * reltempo;
}


static bool plays_same_seqblock_completely_later_in_seqtrack(struct SeqTrack *seqtrack, int pos, int64_t before_seqtime){
  const struct SeqBlock *seqblock = (struct SeqBlock *)seqtrack->seqblocks.elements[pos];
  const struct Blocks *block = seqblock->block;
  R_ASSERT_RETURN_IF_FALSE2(block!=NULL, false);
  
  for(int i=pos+1 ; i < seqtrack->seqblocks.num_elements ; i++){
    const struct SeqBlock *seqblock2 = (struct SeqBlock *)seqtrack->seqblocks.elements[pos];
    const struct Blocks *block2 = seqblock2->block;

    if(block2==NULL)
      continue;
    
    int64_t start_seqblock2_seqtime = seqblock2->t.time;
    int64_t end_seqblock2_seqtime = start_seqblock2_seqtime + get_seqblock_duration(seqblock);
    
    if (end_seqblock2_seqtime > before_seqtime)
      return false;
    
    if (block==block2)
      return true;
  }

  return false;
}


static bool ensure_seqtrack_has_instrument(struct SeqTrack *seqtrack){
  if (seqtrack->for_audiofiles==false)
    return false;
  
  if (seqtrack->patch == NULL || seqtrack->patch->patchdata==NULL) { // seqtrack->patch == NULL when seqtrack never has played an audio file, and seqtrack->patch->patchdata is null if the seqtrack plugin was deleted manually.
    
    radium::ScopedIgnoreUndo ignore_undo; // Because we can't delete seqtrack plugin when it has samples.

    int seqtracknum = get_seqtracknum(seqtrack);
    
    int64_t patch_id = createAudioInstrument(SEQTRACKPLUGIN_NAME, SEQTRACKPLUGIN_NAME, talloc_format("Seqtrack %d", seqtracknum), 0, 0);
    R_ASSERT_RETURN_IF_FALSE2(patch_id >= 0, false);
    
    struct Patch *patch = PATCH_get_from_id(patch_id);
    R_ASSERT_RETURN_IF_FALSE2(patch!=NULL, false);
    
    connectAudioInstrumentToMainPipe(patch_id);
    autopositionInstrument(patch_id);

    {
      radium::PlayerLock lock;
      seqtrack->patch = patch;
    }
  }

  return true;
}


void SEQTRACK_call_me_very_often(void){
  if (is_called_every_ms(500))
    VECTOR_FOR_EACH(struct SeqTrack *, seqtrack, &root->song->seqtracks){

      if (ensure_seqtrack_has_instrument(seqtrack)==true){

        if (seqtrack->for_audiofiles)
          R_ASSERT(seqtrack->patch!=NULL);
        else
          R_ASSERT(seqtrack->patch==NULL);
        
        if (seqtrack->patch!=NULL){
          
          SoundPlugin *plugin = (SoundPlugin*)seqtrack->patch->patchdata;
          R_ASSERT_NON_RELEASE(plugin!=NULL);
          
          if (plugin !=NULL)
            SEQTRACKPLUGIN_called_very_often(plugin);
          
        }
      }

    }END_VECTOR_FOR_EACH;
}


void SONG_call_me_before_starting_to_play_song(int64_t seqtime){

  // Sequencer automation
  //
  // We init sequencer automation before editor automation since sequencer automation is called before editor automation in the player.
  //
  if (seqtime > 0) {
    VECTOR_FOR_EACH(struct SeqTrack *, seqtrack, &root->song->seqtracks){
      SEQTRACK_AUTOMATION_call_me_before_starting_to_play_song_MIDDLE(seqtrack, seqtime);      
    }END_VECTOR_FOR_EACH;
  }

  radium::FutureSignalTrackingSemaphore gotit;

  VECTOR_FOR_EACH(struct SeqTrack *, seqtrack, &root->song->seqtracks){

    // Seqtrackplugin (Read some samples from audio files into memory)
    //
    if (seqtrack->patch!=NULL){
      
      SoundPlugin *plugin = (SoundPlugin*)seqtrack->patch->patchdata;
      if (plugin !=NULL)
        SEQTRACKPLUGIN_prepare_to_play(plugin, seqtrack, seqtime, &gotit);
      
    }

    // Editor automation
    //
    if (seqtime > 0) {
      for(int i=0 ; i < seqtrack->seqblocks.num_elements ; i++){
        
        const struct SeqBlock *seqblock = (struct SeqBlock *)seqtrack->seqblocks.elements[i];
        
        if (seqblock->t.time >= seqtime)
          break;
        
        const struct Blocks *block = seqblock->block;
        
        if (block != NULL) {
          if (!plays_same_seqblock_completely_later_in_seqtrack(seqtrack, i, seqtime)){
            
            STime blocktime = R_MIN(getBlockSTimeLength(block), seqtime_to_blocktime(seqblock, seqtime - seqblock->t.time));
            
            FX_call_me_before_starting_to_play_song(seqtrack, seqblock, blocktime);
          }
        }
      }
    }

  }END_VECTOR_FOR_EACH;

  gotit.wait_for_all_future_signals();
}

  
static void seqblockgcfinalizer(void *actual_mem_start, void *user_data){
  struct SeqBlock *seqblock = (struct SeqBlock*)user_data;
  //printf("FINALIZING seqtrack\n");
  //getchar();
  SEQBLOCK_ENVELOPE_free(seqblock->envelope);

  delete seqblock->fade_in_envelope;
  delete seqblock->fade_out_envelope;
}

static void default_duration_changed(struct SeqBlock *seqblock, int64_t default_duration){
  R_ASSERT(default_duration > 0);
  
  seqblock->t.time2 = seqblock->t.time + default_duration;
  
  seqblock->t.default_duration = default_duration;

  seqblock->t.interior_end = default_duration;
}

static int64_t g_seqblock_id = 0;
static int64_t new_seqblock_id(void){
  return g_seqblock_id++;
}

void SEQBLOCK_init(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, struct Blocks *block, const dyn_t envelope_state, double state_samplerate, bool *track_is_disabled, int64_t time){
  
  seqblock->id = new_seqblock_id();
  
  seqblock->block = block;
  seqblock->sample_id = -1;
  seqblock->track_is_disabled = track_is_disabled;

  seqblock->t.time = time;

  if (block != NULL) {
    default_duration_changed(seqblock, getBlockSTimeLength(block));
    
    seqblock->t.start_place = p_Create(0,0,1);
    
    seqblock->t.end_place = p_Create(block->num_lines,0,1);
  }else{
    default_duration_changed(seqblock, 48000);
  }
  
  seqblock->t.stretch = 1.0;

  seqblock->t.stretch_without_tempo_multiplier = 1.0;

  seqblock->fadein = 0;
  seqblock->fadeout = 0;

  seqblock->fade_in_envelope = new radium::Envelope(FADE_LINEAR, 1.0, true);
  seqblock->fade_out_envelope = new radium::Envelope(FADE_LINEAR, 1.0, false);

  if(seqtrack != NULL){
    R_ASSERT(false==PLAYER_current_thread_has_lock());
    seqblock->envelope = SEQBLOCK_ENVELOPE_create(seqtrack, seqblock, envelope_state, state_samplerate);
    GC_register_finalizer(seqblock, seqblockgcfinalizer, seqblock, NULL, NULL);
  }else{
    R_ASSERT(true==PLAYER_current_thread_has_lock());
  }

  seqblock->envelope_volume = 1.0;
  seqblock->envelope_db = 0.0;
}

static struct SeqBlock *SEQBLOCK_create_block(struct SeqTrack *seqtrack, struct Blocks *block, const dyn_t envelope_state, double state_samplerate, int64_t time){
  if (seqtrack->for_audiofiles==true)
    return NULL;
  
  struct SeqBlock *seqblock = (struct SeqBlock*)talloc(sizeof(struct SeqBlock));
  SEQBLOCK_init(seqtrack,
                seqblock,
                block,
                envelope_state,
                state_samplerate,
                (bool*)talloc_atomic_clean(sizeof(bool)*MAX_DISABLED_SEQBLOCK_TRACKS),
                time
                );
  return seqblock;
}

static struct SeqBlock *SEQBLOCK_create_sample(struct SeqTrack *seqtrack, int seqtracknum, const wchar_t *filename, const dyn_t envelope_state, double state_samplerate, int64_t seqtime, Seqblock_Type type){
 
  if (seqtrack->for_audiofiles==false)
    return NULL;
 
  if (ensure_seqtrack_has_instrument(seqtrack)==false)
    return NULL;

  struct SeqBlock *seqblock = (struct SeqBlock*)talloc(sizeof(struct SeqBlock));
  SEQBLOCK_init(seqtrack,
                seqblock,
                NULL,
                envelope_state,
                state_samplerate,
                NULL,
                seqtime 
                );

  seqblock->sample_filename = STRING_copy(filename);
  
  SoundPlugin *plugin = (SoundPlugin*) seqtrack->patch->patchdata;
  
  R_ASSERT_RETURN_IF_FALSE2(!strcmp(SEQTRACKPLUGIN_NAME, plugin->type->type_name), NULL);

  seqblock->sample_id = SEQTRACKPLUGIN_add_sample(seqtrack, plugin, filename, seqblock, type);
  if (seqblock->sample_id==-1)
    return NULL;

  if (type==Seqblock_Type::RECORDING) {
    
    seqblock->sample_filename_without_path = L"";
    
  } else {
    seqblock->sample_filename_without_path = STRING_copy(SEQTRACKPLUGIN_get_sample_name(plugin, seqblock->sample_id, false));

    int64_t duration = SEQTRACKPLUGIN_get_total_num_frames_for_sample(plugin, seqblock->sample_id);
  
    default_duration_changed(seqblock, duration);  
    SEQBLOCK_ENVELOPE_duration_changed(seqblock, duration, NULL);
  }
  
  return seqblock;
}


// Ensures that two seqblocks doesn't overlap, and that a seqblock doesn't start before 0.
// Preserves original pause times.
static void legalize_seqtrack_timing(struct SeqTrack *seqtrack, radium::PlayerLockOnlyIfNeeded *lock){
  //printf("Legalizing timing\n");  
  
#if !defined(RELEASE)
  //if (lock==NULL)
  //  R_ASSERT(PLAYER_current_thread_has_lock()); // hits when initializing a seqtrack.
  
  //if (!is_gfx)
  //  R_ASSERT(PLAYER_current_thread_has_lock()); (not true when creating from state. I.e. creating new seqtrack that isn't live yet)
  R_ASSERT(is_playing_song()==false);
#endif

  int64_t last_end_time = 0;
  int64_t time_to_add = 0;
  
  VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
    if (seqblock->block != NULL){
      int64_t seq_block_start = seqblock->t.time + time_to_add;
      
      if (seq_block_start < last_end_time) {
        time_to_add += (last_end_time - seq_block_start);
        seq_block_start = last_end_time;
      }
      
      if (seq_block_start != seqblock->t.time){
        if (lock)
          lock->lock();
        move_seqblock(seqblock, seq_block_start);
      }
      
      last_end_time = seqblock->t.time2;
    }
  }END_VECTOR_FOR_EACH;

  //VECTOR_sort(&seqtrack->seqblocks, seqblocks_comp);
}

void RT_legalize_seqtrack_timing(struct SeqTrack *seqtrack, radium::PlayerLockOnlyIfNeeded *lock){
  legalize_seqtrack_timing(seqtrack, lock);
}

/*
void SEQTRACK_update_all_seqblock_start_and_end_times(struct SeqTrack *seqtrack){
  update_all_seqblock_start_and_end_times(seqtrack, &seqtrack->seqblocks);
}

// Don't need player lock here. 'start_time' and 'end_time' is only used in the main thread.
void SEQUENCER_update_all_seqblock_start_and_end_times(void){
  R_ASSERT(THREADING_is_main_thread());
  
  VECTOR_FOR_EACH(struct SeqTrack *, seqtrack, &root->song->seqtracks){
    SEQTRACK_update_all_seqblock_start_and_end_times(seqtrack);
  }END_VECTOR_FOR_EACH;
}
*/


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
  
  return seqblock->t.time - bar_length;
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
  R_ASSERT_RETURN_IF_FALSE2(block != NULL, seqtime);
  
  int64_t ret = seqblock->t.time;
  int64_t mindist = INT64_MAX;
  
  if (what==WhatToFind::NOTHING)
    return seqtime;
  
  if (what==WhatToFind::LINE){
    for(int line=0;line<block->num_lines;line++){
      int64_t stime = seqblock->t.time + blocktime_to_seqtime(seqblock, block->times[line].time);
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
      int64_t beattime = seqblock->t.time + blocktime_to_seqtime(seqblock, Place2STime(block, &beat->l.p));
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
  R_ASSERT_RETURN_IF_FALSE2(block != NULL, seqtime);

  int64_t blocklen = getBlockSTimeLength(block);
  int64_t block_interval_length;

  if (what==WhatToFind::NOTHING) {

    return seqtime;
  
  } else if (what==WhatToFind::LINE){
    
    block_interval_length = blocklen - block->times[block->num_lines-1].time;

  } else {

    const struct Beats *last_barorbeat = NULL;
    
    const struct Beats *beat = NextBeat(block->beats);
    while (beat != NULL){
      if (beat->beat_num==1 || what==WhatToFind::BEAT)
        last_barorbeat = beat;
      beat = NextBeat(beat);
    }
    
    if (last_barorbeat==NULL)
      block_interval_length = blocklen; // no bars in the seqblock
    else
      block_interval_length = blocklen - Place2STime(block, &last_barorbeat->l.p); // this is arguable not correct if the block stops before the beat should have ended...
  }

  int64_t interval_length = blocktime_to_seqtime(seqblock, block_interval_length); // / ATOMIC_DOUBLE_GET(block->reltempo));
  //printf("  ... interval_length: %f\n", (double)interval_length / 44100.0);
  
  int64_t ret = SEQBLOCK_get_seq_endtime(seqblock);
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

int64_t find_closest_barorbeat_start(int seqtracknum, int64_t seqtime, WhatToFind what){

  //struct SeqTrack *pos_seqtrack = (struct SeqTrack*)root->song->seqtracks.elements[seqtracknum];
  struct SeqTrack *seqtrack = find_closest_seqtrack_with_barorbeat_start(seqtracknum);
                         
  int64_t barorbeat_start_time = 0;

  struct SeqBlock *last_seqblock = NULL;
  
  VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){

    if (seqblock->block==NULL)
      continue;
      
    int64_t starttime = seqblock->t.time;
    int64_t endtime = SEQBLOCK_get_seq_endtime(seqblock);
    
    if (seqtime >= starttime && seqtime < endtime) {
      //printf("inside:  ");
      barorbeat_start_time = find_barorbeat_start_inside(seqblock, seqtime, what);
      goto gotit;
    }
    
    if (seqtime < starttime && last_seqblock==NULL) {
      //printf("pos_abstime:  ");
      return seqtime;
      //bar_start_time = pos_seqtime; //find_bar_start_before(seqblock, seqtime);
      //goto gotit;
    }
    
    if (seqtime < starttime) {
      //printf("after:  ");
      barorbeat_start_time = find_barorbeat_start_after(last_seqblock, seqtime, starttime, what);
      goto gotit;
    }
    
    last_seqblock = seqblock;
  }END_VECTOR_FOR_EACH;

  if (last_seqblock==NULL){
    //printf("pos_abstime2:  ");
    return seqtime;
  } else {
    //printf("after2:  ");
    barorbeat_start_time = find_barorbeat_start_after(last_seqblock, seqtime, INT64_MAX, what);
  }
  
 gotit:

  //printf("Converting %f to %f\n",(double)bar_start_time/44100.0, (double)convert_seqtime(seqtrack, pos_seqtrack, bar_start_time)/44100.0);
  //return convert_seqtime(seqtrack, pos_seqtrack, bar_start_time);
  return barorbeat_start_time;
}

int64_t SEQUENCER_find_closest_bar_start(int seqtracknum, int64_t seqtime){
  return find_closest_barorbeat_start(seqtracknum, seqtime, WhatToFind::BAR);
}

int64_t SEQUENCER_find_closest_beat_start(int seqtracknum, int64_t seqtime){
  return find_closest_barorbeat_start(seqtracknum, seqtime, WhatToFind::BEAT);
}


int64_t SEQUENCER_find_closest_line_start(int seqtracknum, int64_t seqstime){
  return find_closest_barorbeat_start(seqtracknum, seqstime, WhatToFind::LINE);
}

/**
 * Find closest bar start, end
 */


static void set_plain_seqtrack_timing_no_pauses(struct SeqTrack *seqtrack){
  R_ASSERT(PLAYER_current_thread_has_lock());
    
  int64_t time = 0;
  
  VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
    move_seqblock(seqblock, time);
    time += get_seqblock_duration(seqblock);
  }END_VECTOR_FOR_EACH;
}


hash_t *SEQBLOCK_get_state(const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock, bool always_get_real_end_time){
  hash_t *state = HASH_create(2);

  HASH_put_int(state, ":id", seqblock->id);
  
  if(seqblock->block != NULL) {
    HASH_put_int(state, ":blocknum", seqblock->block->l.num);
  } else {
    SoundPlugin *plugin = (SoundPlugin*) seqtrack->patch->patchdata;
    const wchar_t *filename = L"";
    if (plugin != NULL)
      filename = SEQTRACKPLUGIN_get_sample_name(plugin, seqblock->sample_id, true);
    else
      R_ASSERT(false);
    HASH_put_string(state, ":sample", filename);

#if !defined(RELEASE)
    QFileInfo info(STRING_get_qstring(filename));
    if (!info.isAbsolute())
      abort();
#endif

  }
  
  HASH_put_int(state, ":start-time", seqblock->t.time);
  HASH_put_int(state, ":end-time", (seqblock_has_stretch(seqtrack, seqblock) || always_get_real_end_time) ? seqblock->t.time2 : -1); // time2 = -1 if there is no stretch. If not, we may set stretch by mistake if loading the song with a different sample rate.

  HASH_put_int(state, ":interior-start", seqblock->t.interior_start);
  HASH_put_int(state, ":interior-end", seqblock->t.interior_end);

  HASH_put_float(state, ":samplerate", MIXER_get_sample_rate());

  if (seqblock->track_is_disabled != NULL) {
    for(int i=0;i<MAX_DISABLED_SEQBLOCK_TRACKS;i++){
      
      if (seqblock->track_is_disabled[i]==true){
        dynvec_t track_disabled = {};
        
        for(int i=0;i<MAX_DISABLED_SEQBLOCK_TRACKS;i++)
          DYNVEC_push_back(&track_disabled, DYN_create_bool(seqblock->track_is_disabled[i]));
        
        HASH_put_dyn(state, ":tracks-disabled", DYN_create_array(track_disabled));
        
        break;
      }
      
    }
  }

  HASH_put_float(state, ":fade-in", seqblock->fadein);
  HASH_put_float(state, ":fade-out", seqblock->fadeout);

  HASH_put_chars(state, ":fade-in-shape", fade_shape_to_string(seqblock->fade_in_envelope->_shape));
  HASH_put_chars(state, ":fade-out-shape", fade_shape_to_string(seqblock->fade_out_envelope->_shape));

  HASH_put_bool(state, ":envelope-enabled", seqblock->envelope_enabled);
  HASH_put_dyn(state, ":envelope", SEQBLOCK_ENVELOPE_get_state(seqblock->envelope));

  return state;
}


static hash_t *get_new_seqblock_state_from_old(const hash_t *old, const struct Song *song){
  hash_t *new_state = HASH_create(10);

  if (HASH_has_key(old, "samplerate"))
    HASH_put_float(new_state, ":samplerate", HASH_get_float(old, "samplerate"));

  HASH_put_int(new_state, ":start-time", HASH_get_int(old, "time"));

  if (HASH_has_key(old, "time2"))
    HASH_put_int(new_state, ":end-time", HASH_get_int(old, "time2"));
  else
    HASH_put_int(new_state, ":end-time", -1);

  int blocknum=-1;
  
  if (HASH_has_key(old, "blocknum")){
    blocknum = HASH_get_int32(old, "blocknum");
    HASH_put_int(new_state, ":blocknum", blocknum);
  }

  const wchar_t *sample = NULL;
  
  if (HASH_has_key(old, ":sample")){
    sample = HASH_get_string(old, ":sample");
    HASH_put_string(new_state, ":sample", sample);
  }

  HASH_put_int(new_state, ":interior-start", 0);
  
  if (blocknum==-1){
    R_ASSERT(false);
    HASH_put_int(new_state, ":interior-end", 192345);
  }else{
    struct Blocks *block = (struct Blocks*)ListFindElement1(&song->blocks->l, blocknum);
    HASH_put_int(new_state, ":interior-end", getBlockSTimeLength(block));
  }
  
  dynvec_t track_disabled = {};

  for(int i=0;i<MAX_DISABLED_SEQBLOCK_TRACKS;i++){
    bool is_disabled = false;

    if (HASH_has_key_at(old, "track_disabled", i))
      is_disabled = HASH_get_bool_at(old, "track_disabled", i);

    DYNVEC_push_back(&track_disabled, DYN_create_bool(is_disabled));
  }

  HASH_put_dyn(new_state, ":tracks-disabled", DYN_create_array(track_disabled));

  if (HASH_has_key(old, ":envelope")){
    HASH_put_bool(new_state, ":envelope-enabled", HASH_get_bool(old, ":envelope-enabled"));
    HASH_put_dyn(new_state, ":envelope", HASH_get_dyn(old, ":envelope"));
  }

  if (HASH_has_key(old, ":fade-in")){
    HASH_put_float(new_state, ":fade-in", HASH_get_float(old, ":fade-in"));
    HASH_put_float(new_state, ":fade-out", HASH_get_float(old, ":fade-out"));
  }
  
  return new_state;
}

#if 0
static hash_t *get_old_seqblock_state_from_new(const hash_t *new_state){
  hash_t *old = HASH_create(10);

  if (HASH_has_key(new_state, ":samplerate"))
    HASH_put_float(old, "samplerate", HASH_get_float(new_state, ":samplerate"));

  HASH_put_int(old, "time", HASH_get_int(new_state, ":start-time"));

  if (HASH_has_key(new_state, ":end-time"))
    HASH_put_int(old, "time2", HASH_get_int(new_state, ":end-time"));
  else
    HASH_put_int(old, "time2", -1);

  if (HASH_has_key(new_state, ":blocknum")){
    int blocknum = HASH_get_int32(new_state, ":blocknum");
    HASH_put_int(old, "blocknum", blocknum);
  }

  if (HASH_has_key(new_state, ":sample")){
    HASH_put_string(old, ":sample", HASH_get_string(new_state, ":sample"));
  }
  
  if (HASH_has_key(new_state, ":tracks-disabled")){

    dyn_t dyn = HASH_get_dyn(new_state, ":tracks-disabled");

    R_ASSERT_RETURN_IF_FALSE2(dyn.type==ARRAY_TYPE, old);

    const dynvec_t *track_disabled = dyn.array;

    R_ASSERT_RETURN_IF_FALSE2(track_disabled->num_elements==MAX_DISABLED_SEQBLOCK_TRACKS, old);

    for(int i=0;i<MAX_DISABLED_SEQBLOCK_TRACKS;i++)
      HASH_put_bool_at(old, "track_disabled", i, track_disabled->elements[i].bool_number);
  }

  HASH_put_bool(old, ":envelope-enabled", HASH_get_bool(new_state, ":envelope-enabled"));
  HASH_put_hash(old, ":envelope", HASH_get_hash(new_state, ":envelope"));

  HASH_put_float(old, ":fade-in", HASH_get_float(new_state, ":fade-in"));
  HASH_put_float(old, ":fade-out", HASH_get_float(new_state, ":fade-out"));

  return old;
}
#endif


#define SHOW_ERROR(ReturnValue, Fmt, ...)      \
  (                                                                     \
   ({                                                                   \
     if (error_type==SHOW_ASSERTION)                                    \
       RError(Fmt, __VA_ARGS__);                                        \
     else                                                               \
       handleError(Fmt, __VA_ARGS__);                                   \
   }),                                                                  \
   ReturnValue)




// TODO: Move this function to hashmap_proc
// TODO: Move this function to hasnmap.c so that it can be performed without looking up key twice.
template <typename T>
static bool get_value(const hash_t *state,
                      const char *key,
                      enum DynType expected_type,
                      T (*get_value_func)(const hash_t *state, const char *key),
                      enum ShowAssertionOrThrowAPIException error_type,
                      T &ret
){
  if (HASH_has_key(state, key)==false)
    return SHOW_ERROR(false, "Sequencer block state does not contain the key %s", key);

  if (HASH_get_type(state, key) != expected_type)
    return SHOW_ERROR(false, "Wrong type in hash table for key \"%s\" when loading sequencer block. Expected %s, found %s",
                      key,
                      DYN_type_name(expected_type),
                      DYN_type_name(HASH_get_type(state, key)));

  ret = get_value_func(state, key);
  return true;
}

// Is static since seqblocks should only be created in this file.
static struct SeqBlock *SEQBLOCK_create_from_state(struct SeqTrack *seqtrack, int seqtracknum, const hash_t *state, enum ShowAssertionOrThrowAPIException error_type, Seqblock_Type type){
  //R_ASSERT(is_gfx==true);
  
  double adjust_for_samplerate = 1.0;
  double state_samplerate = -1.0;
  
  if (HASH_has_key(state, ":samplerate")){
    double maybe_state_samplerate;
    if (get_value(state, ":samplerate", FLOAT_TYPE, HASH_get_float, error_type, maybe_state_samplerate)==false)
      return NULL;

    double samplerate = pc->pfreq;
    
    if (fabs(maybe_state_samplerate-samplerate)>1){
      state_samplerate = maybe_state_samplerate;
      adjust_for_samplerate = samplerate/state_samplerate;
    }
  }

  int64_t time,time2;

  if (get_value(state, ":start-time", INT_TYPE, HASH_get_int, error_type, time)==false)
    return NULL;

  if (get_value(state, ":end-time", INT_TYPE, HASH_get_int, error_type, time2)==false)
    return NULL;

  R_ASSERT_RETURN_IF_FALSE3(time >= 0 && (time2==-1 || time < time2),
                            error_type,
                            NULL,
                            "Illegal sequencer block start-time and end-time values. Start: %d. End: %d", (int)time, (int)time2
                            );

  if (adjust_for_samplerate != 1.0){
    time = round(double(time) * adjust_for_samplerate);
    if (time2 != -1)
      time2 = round(double(time2) * adjust_for_samplerate);
  }

  int64_t interior_start, interior_end;
  
  if (get_value(state, ":interior-start", INT_TYPE, HASH_get_int, error_type, interior_start)==false)
    return NULL;

  if (get_value(state, ":interior-end", INT_TYPE, HASH_get_int, error_type, interior_end)==false)
    return NULL;

  R_ASSERT_RETURN_IF_FALSE3(interior_start >= 0 && interior_start < interior_end,
                            error_type, NULL,
                            "Illegal sequencer block interior-start and interior-end values. Start: %d. End: %d", (int)interior_start, (int)interior_end
                            );


  if (adjust_for_samplerate != 1.0){
    interior_start = round(double(interior_start) * adjust_for_samplerate);
    interior_end   = round(double(interior_end) * adjust_for_samplerate);
  }

  dyn_t envelope = g_uninitialized_dyn;
  if (HASH_has_key(state, ":envelope"))
    envelope = HASH_get_dyn(state, ":envelope");

  struct SeqBlock *seqblock;

  if (HASH_has_key(state, ":blocknum")){
    int  blocknum;
    if (get_value(state, ":blocknum", INT_TYPE, HASH_get_int32, error_type, blocknum)==false)
      return NULL;

    struct Blocks *block = (struct Blocks*)ListFindElement1_r0(&root->song->blocks->l, blocknum);
    R_ASSERT_RETURN_IF_FALSE3(block!=NULL, error_type, NULL,
                              "Block %d not found", blocknum);

    seqblock = SEQBLOCK_create_block(seqtrack, block, envelope, state_samplerate, time);
    if (seqblock==NULL)
      return NULL;
    
  } else {
    
    const wchar_t *filename = L"";
    if (get_value(state, ":sample", STRING_TYPE, HASH_get_string, error_type, filename)==false)
      return NULL;

    seqblock = SEQBLOCK_create_sample(seqtrack, seqtracknum, filename, envelope, state_samplerate, time, type);
    if (seqblock==NULL)
      return NULL;

  }

  int64_t default_duration = get_seqblock_stime_default_duration(seqtrack, seqblock);

  if (interior_end > default_duration){

    if (llabs(interior_end-default_duration) < 16) { // Avoid minor rounding errors

      int64_t diff = interior_end-interior_start;
      
      interior_end = default_duration;

      // In case interior_start and interior_end are very crammed together.
      if (interior_start >= interior_end){
        interior_start -= diff;
        if (interior_start < 0)
          interior_start = 0;
      }
  

    } else {
      
      if (error_type==THROW_API_EXCEPTION){

        if (seqblock->block==NULL){
          R_ASSERT_RETURN_IF_FALSE2(seqtrack->patch!=NULL && seqtrack->patch->patchdata!=NULL, NULL);
          SoundPlugin *plugin = (SoundPlugin*) seqtrack->patch->patchdata;
          SEQTRACKPLUGIN_request_remove_sample(plugin, seqblock->sample_id, type);
        }
        
        handleError("interior-end value is larger than the default block duration: %d > %d", (int)interior_end, (int)default_duration);
#if 0 //!defined(RELEASE)
        printf("Backtrace:\n%s\n", JUCE_get_backtrace());
#endif
        return NULL;
        
      } else {
        
        bool show_rerror = seqblock->block==NULL;
#if defined(RELEASE)
        if (show_rerror==true && fabs(interior_end-default_duration) < 16)
          show_rerror = false;  // Don't show error if it's caused by a rounding error.
#endif
        
        if (show_rerror)
          RError("interior-end value is larger than the default block duration: %d > %d", (int)interior_end, (int)default_duration);
        
        interior_end = default_duration;
        
      }
      
    }
  }

  R_ASSERT(seqblock->t.time==time);

  if (time2==-1)
    time2 = time + (interior_end-interior_start);

  seqblock->t.time2 = time2;
  seqblock->t.interior_start = interior_start;
  seqblock->t.interior_end = interior_end;
 
  set_seqblock_stretch(seqtrack, seqblock);

  R_ASSERT(seqblock->t.time2 > seqblock->t.time);

  R_ASSERT(seqblock->t.stretch > 0);

  if (HASH_has_key(state, ":id") && false==HASH_has_key(state, ":new-block")){
    int64_t id = HASH_get_int(state, ":id");
    if(id >= 0){
      seqblock->id = id;
      g_seqblock_id = R_MAX(g_seqblock_id, seqblock->id);
    }else{
      R_ASSERT(id==-1);
    }
  }

  if (HASH_has_key(state, ":tracks-disabled")){
    dyn_t dyn;
    if (get_value(state, ":tracks-disabled", ARRAY_TYPE, HASH_get_dyn, error_type, dyn)==false)
      return seqblock;
    const dynvec_t *vec = dyn.array;
    R_ASSERT_RETURN_IF_FALSE2(vec->num_elements==MAX_DISABLED_SEQBLOCK_TRACKS, seqblock);
    for(int i=0;i<MAX_DISABLED_SEQBLOCK_TRACKS;i++){
      seqblock->track_is_disabled[i] = vec->elements[i].bool_number;
    }
  }

  if (HASH_has_key(state, ":envelope-enabled"))
    seqblock->envelope_enabled = HASH_get_bool(state, ":envelope-enabled");

  if (HASH_has_key(state, ":fade-in")){
    seqblock->fadein = HASH_get_float(state, ":fade-in");
    seqblock->fadeout = HASH_get_float(state, ":fade-out");
  }

  {
    delete seqblock->fade_in_envelope;
    delete seqblock->fade_out_envelope;

    FadeShape inshape = FADE_FAST; // Older songs only had fade fast.
    FadeShape outshape = FADE_FAST; // Older songs only had fade fast.

    if (HASH_has_key(state, ":fade-in-shape")){
      inshape = string_to_fade_shape(HASH_get_chars(state, ":fade-in-shape"));
      outshape = string_to_fade_shape(HASH_get_chars(state, ":fade-out-shape"));
    }

    seqblock->fade_in_envelope = new radium::Envelope(inshape, 1.0, true);
    seqblock->fade_out_envelope = new radium::Envelope(outshape, 1.0, false);
  }

  return seqblock;
}

static void seqtrackgcfinalizer(void *actual_mem_start, void *user_data){
  struct SeqTrack *seqtrack = (struct SeqTrack*)user_data;
  //printf("FINALIZING seqtrack\n");
  //getchar();
  SEQTRACK_AUTOMATION_free(seqtrack->seqtrackautomation);
}

static int SEQTRACK_insert_seqblock(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, int64_t seqtime, int64_t end_seqtime);
  
int SEQBLOCK_insert_seqblock_from_state(hash_t *hash, enum ShowAssertionOrThrowAPIException error_type){
  int seqtracknum = HASH_get_int32(hash, ":seqtracknum");
  if (seqtracknum < 0 || seqtracknum >= root->song->seqtracks.num_elements)
    return SHOW_ERROR(-1, "No seqtrack #%d", seqtracknum);

  struct SeqTrack *seqtrack = (struct SeqTrack*)root->song->seqtracks.elements[seqtracknum];

  HASH_put_bool(hash, ":new-block", true); // To avoid two seqblocks with the same id.
  
  struct SeqBlock *seqblock = SEQBLOCK_create_from_state(seqtrack, seqtracknum, hash, error_type, Seqblock_Type::REGULAR);
  if (seqblock==NULL)
    return -1;

  return SEQTRACK_insert_seqblock(seqtrack, seqblock, seqblock->t.time, seqblock->t.time2);
}

struct SeqTrack *SEQTRACK_create(const hash_t *automation_state, double state_samplerate, bool for_audiofiles){
  struct SeqTrack *seqtrack = (struct SeqTrack*)talloc(sizeof(struct SeqTrack));
  //memset(seqtrack, 0, sizeof(struct SeqTrack));

  seqtrack->for_audiofiles = for_audiofiles;

  seqtrack->scheduler = SCHEDULER_create();

  auto *seqtrackautomation = SEQTRACK_AUTOMATION_create(seqtrack, automation_state, state_samplerate);
  seqtrack->seqtrackautomation = seqtrackautomation;

  GC_register_finalizer(seqtrack, seqtrackgcfinalizer, seqtrack, NULL, NULL);

  return seqtrack;
}

hash_t *SEQTRACK_get_state(const struct SeqTrack *seqtrack /* , bool get_old_format */){
  hash_t *state = HASH_create(seqtrack->seqblocks.num_elements);

  HASH_put_bool(state, "for_audiofiles", seqtrack->for_audiofiles);

  for(int i=0;i<seqtrack->seqblocks.num_elements;i++){
    hash_t *seqblock_state = SEQBLOCK_get_state(seqtrack, (struct SeqBlock*)seqtrack->seqblocks.elements[i], false);
    /*
    if (get_old_format)
      seqblock_state = get_old_seqblock_state_from_new(seqblock_state);
    */
    HASH_put_hash_at(state, "seqblock", i, seqblock_state);
  }

  HASH_put_hash(state, "automation", SEQTRACK_AUTOMATION_get_state(seqtrack->seqtrackautomation));

  if (seqtrack->patch != NULL)
    HASH_put_int(state, "patch_id", seqtrack->patch->id);

  if (seqtrack->name != NULL)
    HASH_put_chars(state, "name", seqtrack->name);
  
  return state;
}

static void remove_all_gfx_samples(struct SeqTrack *seqtrack){
  SoundPlugin *plugin = NULL;
  
  R_ASSERT_RETURN_IF_FALSE(seqtrack->gfx_seqblocks != NULL);
  
  VECTOR_FOR_EACH(struct SeqBlock *, seqblock, seqtrack->gfx_seqblocks){
    if (seqblock->block == NULL){ 
      if (plugin==NULL){
        R_ASSERT_RETURN_IF_FALSE(seqtrack->patch!=NULL);
        R_ASSERT_RETURN_IF_FALSE(seqtrack->patch->patchdata!=NULL);
        plugin = (SoundPlugin*) seqtrack->patch->patchdata;
        R_ASSERT_RETURN_IF_FALSE(!strcmp(SEQTRACKPLUGIN_NAME, plugin->type->type_name));
      }        
      SEQTRACKPLUGIN_request_remove_sample(plugin, seqblock->sample_id, Seqblock_Type::GFX);
      SEQTRACKPLUGIN_assert_samples(plugin);
    }
  }END_VECTOR_FOR_EACH;
}


void SEQTRACK_create_gfx_seqblocks_from_state(const dyn_t seqblocks_state, struct SeqTrack *seqtrack, const int seqtracknum, enum ShowAssertionOrThrowAPIException error_type){
  R_ASSERT_RETURN_IF_FALSE(seqblocks_state.type==ARRAY_TYPE);

  if (seqtrack->gfx_seqblocks != NULL){

    remove_all_gfx_samples(seqtrack);
    
    VECTOR_clean(seqtrack->gfx_seqblocks);
    
  } else {
    
    seqtrack->gfx_seqblocks = (vector_t*)talloc(sizeof(vector_t));
    
  }

  QSet<int64_t> used;

  for(const dyn_t dyn : seqblocks_state.array){    
    R_ASSERT_RETURN_IF_FALSE(dyn.type==HASH_TYPE);
    struct SeqBlock *seqblock = SEQBLOCK_create_from_state(seqtrack, seqtracknum, dyn.hash, error_type, Seqblock_Type::GFX);
    if (seqblock != NULL){

      if(used.contains(seqblock->id)){        
        SHOW_ERROR(false , "List of new Gfx seqblocks contains two seqblocks with the same id: %d", (int)seqblock->id);
        seqblock->id = new_seqblock_id();
      }
      
      used << seqblock->id;
        
      VECTOR_push_back(seqtrack->gfx_seqblocks, seqblock);
    }
  }

  VECTOR_sort2(seqtrack->gfx_seqblocks, seqblocks_comp);

#if 0
  // Assert that there are no pauses, and all blocks are in order.
  printf("\n\n");
  int64_t prev_end = 0;
  VECTOR_FOR_EACH(struct SeqBlock *, seqblock, seqtrack->gfx_seqblocks){
    printf("%d: %d -> %d.\n", iterator666, (int)seqblock->t.time, (int)seqblock->t.time2);
    if (llabs(prev_end-seqblock->t.time) > 500)
      abort();
    prev_end = seqblock->t.time2;
  }END_VECTOR_FOR_EACH;
  printf("\n\n");
#endif

  //SEQUENCER_update(SEQUPDATE_TIME | SEQUPDATE_PLAYLIST);
  SEQTRACK_update(seqtrack);
  SEQUENCER_update(SEQUPDATE_NAVIGATOR | SEQUPDATE_PLAYLIST);
}

dyn_t SEQTRACK_get_seqblocks_state(const struct SeqTrack *seqtrack){
  dynvec_t vec = {};

  VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
    DYNVEC_push_back(&vec, DYN_create_hash(SEQBLOCK_get_state(seqtrack, seqblock, true)));
  }END_VECTOR_FOR_EACH;

  return DYN_create_array(vec);
}

void SEQTRACK_cancel_gfx_seqblocks(struct SeqTrack *seqtrack){
  remove_all_gfx_samples(seqtrack);
  SEQTRACKPLUGIN_assert_samples2(seqtrack);
  
  seqtrack->gfx_seqblocks = NULL;
  SEQTRACKPLUGIN_assert_samples2(seqtrack);

  SEQUENCER_update(SEQUPDATE_TIME | SEQUPDATE_PLAYLIST);
}

void SEQTRACK_apply_gfx_seqblocks(struct SeqTrack *seqtrack, const int seqtracknum, bool seqtrack_is_live){

  R_ASSERT_RETURN_IF_FALSE(seqtrack->gfx_seqblocks != NULL);
  
  int len1 = seqtrack->gfx_seqblocks->num_elements;

  //printf("Apply len gfx: %d. Len: %d\n", len1, seqtrack->seqblocks.num_elements);

  SoundPlugin *plugin = NULL;

  if (seqtrack->patch != NULL && seqtrack->patch->patchdata!=NULL)
    plugin = (SoundPlugin*) seqtrack->patch->patchdata;
  
  {
    radium::PlayerPause pause(seqtrack_is_live && is_playing_song());

    if (plugin != NULL)
      SEQTRACKPLUGIN_apply_gfx_samples(plugin);

    {
      radium::PlayerLock lock(seqtrack_is_live);

      //int bef=seqtrack->seqblocks.num_elements;
      int bef_gfx = seqtrack->gfx_seqblocks->num_elements;
      
      seqtrack->seqblocks = *seqtrack->gfx_seqblocks;
      seqtrack->gfx_seqblocks = NULL;

      int aft=seqtrack->seqblocks.num_elements;
      R_ASSERT(bef_gfx==aft);

      //printf("bef: %d. bef_gfx: %d. Aft: %d\n",bef,bef_gfx,aft);
      
      SEQTRACKPLUGIN_assert_samples2(seqtrack);
      
      legalize_seqtrack_timing(seqtrack, NULL);
    }

    SEQTRACKPLUGIN_assert_samples2(seqtrack);
  }

  SEQTRACKPLUGIN_assert_samples2(seqtrack);

  R_ASSERT(len1==seqtrack->seqblocks.num_elements);
  SEQUENCER_update(SEQUPDATE_TIME | SEQUPDATE_PLAYLIST);
  R_ASSERT(len1==seqtrack->seqblocks.num_elements);
}

  
static QVector<SeqTrack*> SEQTRACK_create_from_state(const hash_t *state, double state_samplerate, int seqtracknum, enum ShowAssertionOrThrowAPIException error_type, struct Song *song){
  const hash_t *automation_state = NULL;
  if (HASH_has_key(state, "automation"))
    automation_state = HASH_get_hash(state, "automation");

  bool for_audiofiles = false; // fix, maybe
  if (HASH_has_key(state, "for_audiofiles"))
    for_audiofiles = HASH_get_bool(state, "for_audiofiles");

  struct SeqTrack *seqtrack = SEQTRACK_create(automation_state, state_samplerate, for_audiofiles);
  struct SeqTrack *seqtrack_extra = NULL; // For loading older songs. In older songs, both audiofiles and editor blocks could be placed in all seqtracks. (and they still can, probably, but it makes very little sense to allow it)

  if (HASH_has_key(state, "patch_id")){
    int64_t patch_id = HASH_get_int(state, "patch_id");
    if (patch_id >= 0){
      struct Patch *patch = PATCH_get_from_id(patch_id);
      if (patch != NULL) {
        seqtrack->patch = patch;
      } else {
        R_ASSERT(false);
      }
    } else {
      R_ASSERT(false);
    }
  }

  if (HASH_has_key(state, "name"))
    seqtrack->name = HASH_get_chars(state, "name");

  R_ASSERT(seqtrack->gfx_seqblocks==NULL);  
  vector_t gfx_seqblocks = {};
  seqtrack->gfx_seqblocks = &gfx_seqblocks;

  vector_t gfx_seqblocks_extra = {};

  int num_seqblocks = HASH_get_array_size(state, "seqblock");

  for(int i=0;i<num_seqblocks;i++){
    hash_t *seqblock_state = HASH_get_hash_at(state, "seqblock", i);
    if(HASH_has_key(seqblock_state, "time"))
      seqblock_state = get_new_seqblock_state_from_old(HASH_get_hash_at(state, "seqblock", i), song);

    HASH_put_bool(seqblock_state, ":new-block", true);  // To avoid two seqblocks with the same id.

    bool seqblock_for_audiofiles = HASH_has_key(seqblock_state, ":blocknum")==false;

    if (seqblock_for_audiofiles == for_audiofiles){
      
      struct SeqBlock *seqblock = SEQBLOCK_create_from_state(seqtrack, seqtracknum, seqblock_state, error_type, Seqblock_Type::GFX);
      VECTOR_push_back(seqtrack->gfx_seqblocks, seqblock);
      
    } else {
    
      if (seqtrack_extra==NULL){
        seqtrack_extra = SEQTRACK_create(NULL, state_samplerate, !for_audiofiles);
        seqtrack_extra->gfx_seqblocks = &gfx_seqblocks_extra;
      }
      
      struct SeqBlock *seqblock = SEQBLOCK_create_from_state(seqtrack_extra, seqtracknum+1, seqblock_state, error_type, Seqblock_Type::GFX);
      VECTOR_push_back(seqtrack_extra->gfx_seqblocks, seqblock);

    }
  }

  //VECTOR_sort(seqtrack->gfx_seqblocks, seqblocks_comp);
  
  SEQTRACK_apply_gfx_seqblocks(seqtrack, seqtracknum, false);
  if (seqtrack_extra != NULL)
    SEQTRACK_apply_gfx_seqblocks(seqtrack_extra, seqtracknum, false);


  {
    QVector<SeqTrack*> ret;
    
    ret.push_back(seqtrack);
    if (seqtrack_extra != NULL)
      ret.push_back(seqtrack_extra);
    
    return ret;
  }
}

// Compatibility with old songs
struct SeqTrack *SEQTRACK_create_from_playlist(const int *playlist, int len){
  vector_t seqblocks = {0};
  
  struct SeqTrack *seqtrack = SEQTRACK_create(NULL, -1, false);
    
  for(int pos=0;pos<len;pos++)
    VECTOR_push_back(&seqblocks,
                     SEQBLOCK_create_block(seqtrack,
                                           (struct Blocks *)ListFindElement1(&root->song->blocks->l,playlist[pos]),
                                           g_uninitialized_dyn,
                                           -1,
                                           -1));
  
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

  int64_t seqtimes[seqtrack->seqblocks.num_elements];

  VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
    seqtimes[iterator666] = seqblock->t.time;
  }END_VECTOR_FOR_EACH;

  //printf("    SEQTRACK_delete_seqblock\n");

  if (seqblock->block==NULL){
    SoundPlugin *plugin = (SoundPlugin*) seqtrack->patch->patchdata;
    SEQTRACKPLUGIN_request_remove_sample(plugin, seqblock->sample_id, Seqblock_Type::REGULAR);
    if (atomic_pointer_read_relaxed((void**)&seqtrack->curr_sample_seqblock)==seqblock)
      atomic_pointer_write_relaxed((void**)&seqtrack->curr_sample_seqblock, NULL);
  }

  {
    radium::PlayerPause pause(is_playing_song());
    radium::PlayerRecursiveLock lock;

    VECTOR_delete(&seqtrack->seqblocks, pos);

    VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
      if (iterator666 >= pos){
        move_seqblock(seqblock, seqtimes[iterator666+1]);
        //printf("Skewing %f -> %f\n", (seqblock->t.time-skew) / 44100.0, seqblock->t.time / 44100.0);
      }
    }END_VECTOR_FOR_EACH;
    
    SEQTRACKPLUGIN_assert_samples2(seqtrack);
    
    RT_legalize_seqtrack_timing(seqtrack, NULL);  // Shouldn't be necessary, but we call it just in case.
  }

  SEQUENCER_update(SEQUPDATE_TIME | SEQUPDATE_PLAYLIST);

#if !defined(RELEASE)
  //memset((void*)seqblock, 0, sizeof(struct SeqBlock));
  if(seqblock->block != NULL) // Can't release sample seqblock memory here since it may still be used in Seqtrack_plugin.cpp.
    tfree((void*)seqblock); // Only for debugging.
#endif

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
  
  const SeqBlockTimings &timing = seqblock->t;
  
  for(int i=pos+1;i<seqtrack->seqblocks.num_elements;i++){
    struct SeqBlock *seqblock2 = (struct SeqBlock*)seqtrack->seqblocks.elements[i];
    const SeqBlockTimings &timing2 = seqblock2->t;
    if (timing2.time >= timing.time2)
      return seqblock2;
  }

  return NULL;
}

// 'how_much' can be negative.
void SEQTRACK_move_all_seqblocks_to_the_right_of(struct SeqTrack *seqtrack, int seqblocknum, int64_t how_much){
  printf("move_all_to_left %d %d\n",seqblocknum, (int)how_much);

  {
    radium::PlayerPause pause(is_playing_song());
    radium::PlayerLock lock;
    
    VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
      if (iterator666 >= seqblocknum)
        move_seqblock(seqblock, seqblock->t.time+how_much);
    }END_VECTOR_FOR_EACH;
    
    RT_legalize_seqtrack_timing(seqtrack, NULL);
  }

  SEQUENCER_update(SEQUPDATE_TIME | SEQUPDATE_PLAYLIST);
}


// Recording

static wchar_t *get_recording_path(const struct SoundPlugin *plugin){
  QString filename = STRING_get_qstring(dc.filename).replace(QRegExp(".rad$"), "_rad");      

  const wchar_t *pathdir = STRING_create(QFileInfo(filename).absoluteFilePath() + "_audio");

  if (DISK_create_dir(pathdir)==false){
    GFX_addMessage("Unable to create directory %S.", pathdir);
    return NULL;
  }

  wchar_t *last_dir = STRING_replace(STRING_replace(STRING_create(plugin->patch->name),
                                                    "/",
                                                    "_slash_"),
                                     "\\",
                                     "_backslash_");

  QString s = STRING_get_qstring(last_dir);

  s.remove(QRegExp("[^a-zA-Z\\d\\s]"));

  if (s=="")
    s = "noname";

  wchar_t *recording_path = STRING_append(pathdir,
                                          STRING_append(STRING_create(OS_get_directory_separator()),
                                                        STRING_create(s)));

  if (DISK_create_dir(recording_path)==false){
    GFX_addMessage("Unable to create directory \"%S\".\n", recording_path);
    return NULL;
  }

  return recording_path;
}

void SEQTRACK_set_recording(struct SeqTrack *seqtrack, bool is_recording){
  R_ASSERT_RETURN_IF_FALSE(seqtrack->for_audiofiles);
  
  if (is_recording==seqtrack->is_recording)
    return;

  R_ASSERT_RETURN_IF_FALSE(seqtrack->patch!=NULL);
  
  struct SoundPlugin *plugin = (struct SoundPlugin*) seqtrack->patch->patchdata;

  R_ASSERT_RETURN_IF_FALSE(plugin!=NULL);
  
  if (is_recording==false)
    SEQTRACKPLUGIN_disable_recording(seqtrack, plugin);
  else {

    wchar_t *recording_path = get_recording_path(plugin);
    if (recording_path==NULL)
      return;
    
    SEQTRACKPLUGIN_enable_recording(seqtrack, plugin, STRING_append(recording_path, STRING_create(OS_get_directory_separator())), 2);
  }
  
  seqtrack->is_recording = is_recording;

  SEQUENCER_update(SEQUPDATE_HEADERS);
}


bool SEQBLOCK_set_fade_in_shape(struct SeqBlock *seqblock, enum FadeShape shape){
  auto *old_fade = seqblock->fade_in_envelope;
  if (old_fade->_shape==shape)
    return false;

  auto *new_fade = new radium::Envelope(shape, 1.0, true);

  {
    radium::PlayerLock lock;
    seqblock->fade_in_envelope = new_fade;
  }

  delete old_fade;

  SEQUENCER_update(SEQUPDATE_TIME);

  return true;
}

bool SEQBLOCK_set_fade_out_shape(struct SeqBlock *seqblock, enum FadeShape shape){
  auto *old_fade = seqblock->fade_out_envelope;
  if (old_fade->_shape==shape)
    return false;

  auto *new_fade = new radium::Envelope(shape, 1.0, false);

  {
    radium::PlayerLock lock;
    seqblock->fade_out_envelope = new_fade;
  }

  delete old_fade;

  SEQUENCER_update(SEQUPDATE_TIME);

  return true;
}

    
// Called from scheduler.c, before scheduling editor things.
// Returns true if there is more to play.
bool RT_SEQTRACK_called_before_editor(struct SeqTrack *seqtrack){

  bool more_things_to_do = false;

  if (is_really_playing_song())
    more_things_to_do = RT_SEQTRACK_AUTOMATION_called_per_block(seqtrack);

  RT_SEQBLOCK_ENVELOPE_called_before_editor(seqtrack);
  
  if (seqtrack->patch==NULL)
    return more_things_to_do;

  SoundPlugin *plugin = (SoundPlugin*)seqtrack->patch->patchdata;
  if (plugin==NULL)
    return more_things_to_do;
  
  if (RT_SEQTRACKPLUGIN_called_per_block(plugin, seqtrack))
    more_things_to_do = true;

  return more_things_to_do;
}


void SEQUENCER_timing_has_changed(radium::PlayerLockOnlyIfNeeded &lock){
  R_ASSERT_NON_RELEASE(false==PLAYER_current_thread_has_lock());

  VECTOR_FOR_EACH(struct SeqTrack *, seqtrack, &root->song->seqtracks){

    VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
      if (seqblock->block != NULL) {
        const int64_t default_duration = get_seqblock_stime_default_duration(seqtrack, seqblock);
        
        if (seqblock->t.default_duration != default_duration){

          lock.lock();
            
          double stretch = seqblock->t.stretch;
          if (stretch==1.0)
            seqblock->t.time2 = seqblock->t.time + default_duration;
          else
            seqblock->t.time2 = seqblock->t.time + round(stretch*(double)default_duration);
          
          seqblock->t.default_duration = default_duration;
          
          seqblock->t.interior_end = default_duration;

          SEQBLOCK_ENVELOPE_duration_changed(seqblock, default_duration, &lock);

          lock.maybe_pause(iterator666);
        }
      }
    }END_VECTOR_FOR_EACH;

  }END_VECTOR_FOR_EACH;
}

/*
void SEQTRACK_move_gfx_gfx_seqblock(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, int64_t new_abs_time){
  int64_t new_seqblock_time = get_seqtime_from_abstime(seqtrack, seqblock, new_abs_time);
  
  if (new_seqblock_time < 0)
    new_seqblock_time = 0;

  seqblock->t.time = new_seqblock_time;

  SEQUENCER_update();
}
*/

void SEQTRACK_insert_silence(struct SeqTrack *seqtrack, int64_t seqtime, int64_t length){

  {
    radium::PlayerPause pause(is_playing_song());
    radium::PlayerLock lock;
    
    VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){

      if (seqblock->t.time >= seqtime)
        move_seqblock(seqblock, seqblock->t.time+length);
      
    }END_VECTOR_FOR_EACH;

    RT_legalize_seqtrack_timing(seqtrack, NULL);
  }

  SEQUENCER_update(SEQUPDATE_TIME | SEQUPDATE_PLAYLIST);
}

static int get_seqblock_pos(vector_t *seqblocks, int64_t seqtime){  

  VECTOR_FOR_EACH(struct SeqBlock *, seqblock, seqblocks){

    if (seqblock->t.time >= seqtime)
      return iterator666;
    
  }END_VECTOR_FOR_EACH;

  return seqblocks->num_elements;
}

// Is static since there is no reason to call this from the outside since seqblocks should only be created in this file.
static int SEQTRACK_insert_seqblock(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, int64_t seqtime, int64_t end_seqtime){
  if (end_seqtime != -1)
    R_ASSERT_RETURN_IF_FALSE2(end_seqtime >= seqtime, -1);

  if (end_seqtime != -1){
    seqblock->t.time2 = end_seqtime;
    set_seqblock_stretch(seqtrack, seqblock);
  }

  // Assert that the seqblock is not in a seqtrack already.
  VECTOR_FOR_EACH(struct SeqTrack *, seqtrack_here, &root->song->seqtracks){
    R_ASSERT_RETURN_IF_FALSE2(!VECTOR_is_in_vector(&seqtrack_here->seqblocks, seqblock), 0);
  }END_VECTOR_FOR_EACH;

  /*
  int64_t seqtimes[R_MAX(1, seqtrack->seqblocks.num_elements)]; // Using R_MAX since arrays of size 0 causes ubsan hit

  VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
    seqtimes[iterator666] = seqblock->t.time;
    //printf("bef %d: %f\n", iterator666, abstimes[iterator666] / 44100.0);
  }END_VECTOR_FOR_EACH;
  */
  
  int pos = get_seqblock_pos(&seqtrack->seqblocks, seqtime);
    
  VECTOR_ensure_space_for_one_more_element(&seqtrack->seqblocks);

  {
    radium::PlayerPause pause(is_playing_song());
    radium::PlayerLock lock;

    move_seqblock(seqblock, seqtime);
    if (end_seqtime != -1){
      seqblock->t.time2 = end_seqtime;
      set_seqblock_stretch(seqtrack, seqblock);
    }
    
    VECTOR_insert(&seqtrack->seqblocks, seqblock, pos);

    /*
    VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
      if (iterator666 > pos)
        move_seqblock(seqblock, seqtimes[iterator666-1]);
    }END_VECTOR_FOR_EACH;
    */
    
    SEQTRACKPLUGIN_assert_samples2(seqtrack);

    RT_legalize_seqtrack_timing(seqtrack, NULL);
  }

  int seqtracknum = get_seqtracknum(seqtrack);
  if (seqtracknum >= 0)
    ATOMIC_SET(root->song->curr_seqtracknum, seqtracknum);
  
  SEQUENCER_update(SEQUPDATE_TIME | SEQUPDATE_PLAYLIST | SEQUPDATE_PLAYLIST);

  return pos;
}

int SEQTRACK_insert_block(struct SeqTrack *seqtrack, struct Blocks *block, int64_t seqtime, int64_t end_seqtime){
  struct SeqBlock *seqblock = SEQBLOCK_create_block(seqtrack, block, g_uninitialized_dyn, -1, -1);
  if (seqblock==NULL)
    return -1;
  return SEQTRACK_insert_seqblock(seqtrack, seqblock, seqtime, end_seqtime);
}

int insert_gfx_gfx_block(struct SeqTrack *seqtrack, struct SeqBlock *seqblock){
  seqblock->is_selected = true;

  vector_t *seqblocks = &seqtrack->gfx_gfx_seqblocks;

  int pos = get_seqblock_pos(seqblocks, seqblock->t.time);
  VECTOR_insert(seqblocks, seqblock, pos);

  SEQUENCER_update(SEQUPDATE_TIME);

  return pos;
}

int SEQTRACK_insert_gfx_gfx_block(struct SeqTrack *seqtrack, int seqtracknum, const hash_t *state, enum ShowAssertionOrThrowAPIException error_type){
  struct SeqBlock *seqblock = SEQBLOCK_create_from_state(seqtrack, seqtracknum, state, error_type, Seqblock_Type::GFX_GFX);
  if (seqblock==NULL)
    return -1;

  return insert_gfx_gfx_block(seqtrack, seqblock);
}


static struct SeqBlock *create_sample_seqblock(struct SeqTrack *seqtrack, int seqtracknum, const wchar_t *filename, int64_t seqtime, int64_t end_seqtime, Seqblock_Type type){
  if (end_seqtime != -1)
    R_ASSERT_RETURN_IF_FALSE2(end_seqtime > seqtime, NULL);

  struct SeqBlock *seqblock = SEQBLOCK_create_sample(seqtrack, seqtracknum, filename, g_uninitialized_dyn, -1, -1, type);
  if (seqblock==NULL)
    return NULL;

  if (type==Seqblock_Type::RECORDING) {
    
    seqblock->t.default_duration = end_seqtime - seqtime;
    default_duration_changed(seqblock, seqblock->t.default_duration);
    
  } else {
    
    seqblock->t.default_duration = get_seqblock_stime_default_duration(seqtrack, seqblock);
    
  }
  
  R_ASSERT_RETURN_IF_FALSE2(seqblock->t.default_duration > 0, NULL);
  
  if (end_seqtime==-1){
    R_ASSERT(type==Seqblock_Type::REGULAR);
    end_seqtime = seqtime + seqblock->t.default_duration;
  }

  return seqblock;
}

int SEQTRACK_insert_sample(struct SeqTrack *seqtrack, int seqtracknum, const wchar_t *filename, int64_t seqtime, int64_t end_seqtime){
  struct SeqBlock *seqblock = create_sample_seqblock(seqtrack, seqtracknum, filename, seqtime, end_seqtime, Seqblock_Type::REGULAR);
  if (seqblock==NULL)
    return -1;
  
  return SEQTRACK_insert_seqblock(seqtrack, seqblock, seqtime, end_seqtime);    
}

struct SeqBlock *SEQTRACK_add_recording_seqblock(struct SeqTrack *seqtrack, int64_t seqtime, int64_t end_seqtime){
  int seqtracknum = get_seqtracknum(seqtrack);
  R_ASSERT_RETURN_IF_FALSE2(seqtracknum >= 0, NULL);
  
  struct SeqBlock *seqblock = create_sample_seqblock(seqtrack, seqtracknum, L"", seqtime, end_seqtime, Seqblock_Type::RECORDING);
  if (seqblock==NULL)
    return NULL;

  seqblock->t.time = seqtime;
  seqblock->t.time2 = end_seqtime;
  
  VECTOR_push_back(&seqtrack->recording_seqblocks, seqblock);
  return seqblock;
}

void SEQTRACK_remove_recording_seqblock(struct SeqTrack *seqtrack, struct SeqBlock *seqblock){
  R_ASSERT_RETURN_IF_FALSE(VECTOR_contains(&seqtrack->recording_seqblocks, seqblock));
                           
  VECTOR_remove(&seqtrack->recording_seqblocks, seqblock);
}
  
double SEQTRACK_get_length(struct SeqTrack *seqtrack){
  int64_t ret = 0;

  struct SeqBlock *last_seqblock = (struct SeqBlock*)VECTOR_last(&seqtrack->seqblocks);
  if (last_seqblock != NULL)
    ret = last_seqblock->t.time2;

  int num_automations = SEQTRACK_AUTOMATION_get_num_automations(seqtrack->seqtrackautomation);

  for(int automationnum = 0 ; automationnum < num_automations ; automationnum++){
    int num_nodes = SEQTRACK_AUTOMATION_get_num_nodes(seqtrack->seqtrackautomation, automationnum);
    int64_t len = R_MAX(ret, SEQTRACK_AUTOMATION_get_seqtime(seqtrack->seqtrackautomation, automationnum, num_nodes-1));
    if (len > ret)
      ret = len;
  }

  return (double)ret / (double)pc->pfreq;
}

// The returned vector contains the vector to paint last first, and vice versa.
QVector<struct SeqBlock*> SEQTRACK_get_seqblocks_in_z_order(const struct SeqTrack *seqtrack, bool is_gfx_gfx){

  QVector<struct SeqBlock*> ret;
  
  const vector_t *seqblocks = is_gfx_gfx ? &seqtrack->gfx_gfx_seqblocks : gfx_seqblocks(seqtrack);

  if(seqblocks->num_elements==0)
    return ret;

  // Create a hash table to avoid O(n^2) when adding the ordered seqblocks.
  QHash<int64_t, struct SeqBlock*> seqblocks_hash;
  VECTOR_FOR_EACH(struct SeqBlock *, seqblock, seqblocks){
    if(seqblocks_hash.contains(seqblock->id)){
      R_ASSERT(false);
      seqblock->id = new_seqblock_id();
    }
    seqblocks_hash[seqblock->id] = seqblock;
  }END_VECTOR_FOR_EACH;
  
      
  QSet<int64_t> already_added;
  
  // Add ordered seqblocks.
  for(const dyn_t &dyn : seqtrack->seqblocks_z_order){
    struct SeqBlock *seqblock = seqblocks_hash[dyn.int_number];
    if (seqblock!=NULL){ // seqblocks_z_order is not required to be up to date.
      ret.push_back(seqblock);
      already_added << seqblock->id;
    }
  }
  
  // Add unordered seqblocks.
  VECTOR_FOR_EACH(struct SeqBlock *, seqblock, seqblocks){
    if (false==already_added.contains(seqblock->id))
      ret.push_back(seqblock);
  }END_VECTOR_FOR_EACH;
  
  return ret;
}

void SEQUENCER_remove_block_from_seqtracks(struct Blocks *block){
  R_ASSERT_RETURN_IF_FALSE(block!=NULL);
  
  QVector<QPair<struct SeqTrack*, struct SeqBlock* > > to_remove;
  
  VECTOR_FOR_EACH(struct SeqTrack *, seqtrack, &root->song->seqtracks){
    VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
      if (seqblock->block==block)
        to_remove.push_back(QPair<struct SeqTrack*, struct SeqBlock* >(seqtrack, seqblock));
    }END_VECTOR_FOR_EACH;
  }END_VECTOR_FOR_EACH;

  if(to_remove.size() > 0){
    radium::PlayerPause pause(is_playing_song());
    //radium::PlayerLock lock; Commented out since SEQTRACK_delete_seqblock does lots of things.

    for(auto pair : to_remove)
      SEQTRACK_delete_seqblock(pair.first, pair.second);
  }

  SEQUENCER_update(SEQUPDATE_TIME | SEQUPDATE_PLAYLIST);
}

void SEQUENCER_insert_seqtrack(struct SeqTrack *new_seqtrack, int pos, bool for_audiofiles){

  if(new_seqtrack!=NULL)
    R_ASSERT(new_seqtrack->for_audiofiles==for_audiofiles);

  if (new_seqtrack==NULL)
    new_seqtrack = SEQTRACK_create(NULL, -1, for_audiofiles);
  
  VECTOR_ensure_space_for_one_more_element(&root->song->seqtracks);

  {
    radium::PlayerPause pause(is_playing_song());
    radium::PlayerLock lock;
            
    VECTOR_insert(&root->song->seqtracks, new_seqtrack, pos);
  }

  evalScheme(talloc_format("(FROM_C-call-me-when-num-seqtracks-might-have-changed %d)", root->song->seqtracks.num_elements+1));

  SEQUENCER_update(SEQUPDATE_EVERYTHING);
}

void SEQUENCER_append_seqtrack(struct SeqTrack *new_seqtrack, bool for_audiofiles){
  SEQUENCER_insert_seqtrack(new_seqtrack, root->song->seqtracks.num_elements, for_audiofiles);
}

static void call_me_after_seqtrack_has_been_removed(struct SeqTrack *seqtrack){
  struct Patch *patch = seqtrack->patch;
  if (patch != NULL){
    SoundPlugin *plugin = (SoundPlugin*) seqtrack->patch->patchdata;
    R_ASSERT_RETURN_IF_FALSE(plugin!=NULL);
    
    VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
      
      if (seqblock->block==NULL)
        SEQTRACKPLUGIN_request_remove_sample(plugin, seqblock->sample_id, Seqblock_Type::REGULAR);
      
    }END_VECTOR_FOR_EACH;

    deleteInstrument(patch->id);
  }
}

void SEQUENCER_replace_seqtrack(struct SeqTrack *new_seqtrack, int pos){
  struct SeqTrack *old_seqtrack = (struct SeqTrack *)root->song->seqtracks.elements[pos];
  
  {
    radium::PlayerPause pause(is_playing_song());
    radium::PlayerLock lock;

    VECTOR_set(&root->song->seqtracks, pos, new_seqtrack);
  }

  SEQUENCER_update(SEQUPDATE_EVERYTHING);

  call_me_after_seqtrack_has_been_removed(old_seqtrack);
}

void SEQUENCER_delete_seqtrack(int pos){
  R_ASSERT_RETURN_IF_FALSE(pos >= 0);
  R_ASSERT_RETURN_IF_FALSE(root->song->seqtracks.num_elements > 1); // There must always be a seqtrack
  R_ASSERT_RETURN_IF_FALSE(pos < root->song->seqtracks.num_elements);

  struct SeqTrack *old_seqtrack = (struct SeqTrack *)root->song->seqtracks.elements[pos];
  
  {
    radium::PlayerPause pause(is_playing_song());
    radium::PlayerLock lock;
    
    VECTOR_delete(&root->song->seqtracks, pos);

    int curr_seqtracknum = ATOMIC_GET(root->song->curr_seqtracknum);
    if (curr_seqtracknum >= root->song->seqtracks.num_elements)
      ATOMIC_SET(root->song->curr_seqtracknum, root->song->seqtracks.num_elements -1);
  }

  SEQUENCER_update(SEQUPDATE_EVERYTHING);
  
  call_me_after_seqtrack_has_been_removed(old_seqtrack);
  evalScheme(talloc_format("(FROM_C-call-me-when-num-seqtracks-might-have-changed %d)", root->song->seqtracks.num_elements-1));

#if !defined(RELEASE)
  //memset(old_seqtrack, 0, sizeof(struct SeqTrack));
  tfree((void*)old_seqtrack);
#endif
}

void SEQUENCER_set_looping(bool do_loop){
  ATOMIC_SET(root->song->looping.enabled, do_loop);
  SEQUENCER_update(SEQUPDATE_TIME|SEQUPDATE_TIMELINE);
}

bool SEQUENCER_is_looping(void){
  return ATOMIC_GET(root->song->looping.enabled);
}

void SEQUENCER_set_loop_start(int64_t start){  
  root->song->looping.start = R_BOUNDARIES(0, start, ATOMIC_GET(root->song->looping.end)-1);
  SEQUENCER_update(SEQUPDATE_TIME|SEQUPDATE_TIMELINE);
}

int64_t SEQUENCER_get_loop_start(void){
  return root->song->looping.start;
}

void SEQUENCER_set_loop_end(int64_t end){
  ATOMIC_SET(root->song->looping.end, R_BOUNDARIES(root->song->looping.start+1, end, SONG_get_length()*MIXER_get_sample_rate()));
  //printf("   Set end. %d %d %d\n",(int)(root->song->looping.start+1), (int)end, (int)(SONG_get_length()*MIXER_get_sample_rate()));
  SEQUENCER_update(SEQUPDATE_TIME|SEQUPDATE_TIMELINE);
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

// Called from SONG_create()
void SEQUENCER_init(struct Song *song){
  TEMPOAUTOMATION_reset();
  song->block_seqtrack = SEQTRACK_create(NULL, -1, false);
  song->looping.start = 0;

  if (g_is_starting_up)
    ATOMIC_SET(song->looping.end, 30 * 48000.0);
  else
    ATOMIC_SET(song->looping.end, 30 * MIXER_get_sample_rate());
}

// Only called during program startup
void SONG_init(void){
  struct SeqTrack *seqtrack = SEQTRACK_create(NULL, -1, false); // for editor blocks

  VECTOR_ensure_space_for_one_more_element(&seqtrack->seqblocks);
  
  VECTOR_ensure_space_for_one_more_element(&root->song->seqtracks);
  
  struct SeqBlock *seqblock = SEQBLOCK_create_block(seqtrack, root->song->blocks, g_uninitialized_dyn, -1, -1);

  SEQUENCER_init(root->song);
  
  PLAYER_lock();{
    
    VECTOR_clean(&root->song->seqtracks);
    
    VECTOR_push_back(&root->song->seqtracks, seqtrack);

    VECTOR_push_back(&seqtrack->seqblocks, seqblock);

    set_plain_seqtrack_timing_no_pauses(seqtrack);

  }PLAYER_unlock();

  evalScheme(talloc_format("(FROM_C-call-me-when-num-seqtracks-might-have-changed %d)", root->song->seqtracks.num_elements));
}


hash_t *SEQUENCER_get_state(void /*bool get_old_format*/){
  hash_t *state = HASH_create(root->song->seqtracks.num_elements);

  HASH_put_float(state, "samplerate", pc->pfreq);
  
  VECTOR_FOR_EACH(const struct SeqTrack *, seqtrack, &root->song->seqtracks){
    hash_t *seqtrack_state = SEQTRACK_get_state(seqtrack /*, get_old_format */);
    HASH_put_hash_at(state, "seqtracks", iterator666, seqtrack_state);
  }END_VECTOR_FOR_EACH;

  HASH_put_bool(state, "contains_seqtime", false); // Earlier, the sequencer had two types if time formats, seqtime and abstime, which complicated things extremely.

  HASH_put_int(state, "curr_seqtracknum", ATOMIC_GET(root->song->curr_seqtracknum));

  // I'm not 100% sure, but I think we need this one since song tempo automation automatically changes length when the song changes length.
  // (modifying song tempo automation is a light operation + that it's atomically real time safe, so it doesn't matter much if we do this)
  HASH_put_hash(state, "song_tempo_automation", TEMPOAUTOMATION_get_state());

  HASH_put_bool(state, "looping_enabled", SEQUENCER_is_looping());
  HASH_put_int(state, "loop_start", SEQUENCER_get_loop_start());
  HASH_put_int(state, "loop_end", SEQUENCER_get_loop_end());
  
  return state;
}

static double find_samplerate_in_old_song_state(hash_t *song_state){
  int num_seqtracks = HASH_get_array_size(song_state, "seqtracks");
  R_ASSERT_RETURN_IF_FALSE2(num_seqtracks > 0, -1);
  
  for(int i = 0 ; i < num_seqtracks ; i++){
    hash_t *seqtrack_state = HASH_get_hash_at(song_state, "seqtracks", i);

    int num_seqblocks = HASH_get_array_size(seqtrack_state, "seqblock");

    for(int i=0;i<num_seqblocks;i++){
      hash_t *seqblock_state = HASH_get_hash_at(seqtrack_state, "seqblock", i);
      
      if (HASH_has_key(seqblock_state, ":samplerate"))
        return HASH_get_float(seqblock_state, ":samplerate");
        
      if (HASH_has_key(seqblock_state, "samplerate"))
        return HASH_get_float(seqblock_state, "samplerate");
    }

  }

  return -1.0;
}

void SEQUENCER_create_from_state(hash_t *state, struct Song *song){

  {
    SEQUENCER_ScopedGfxDisable gfx_disable;

    // Remove all sequencer samples from the seqtrack plugin.
    {
      VECTOR_FOR_EACH(struct SeqTrack *, seqtrack, &root->song->seqtracks){
        VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
          if (seqblock->block==NULL){
            if (seqtrack->patch == NULL)
              R_ASSERT(false);
            else {
              SoundPlugin *plugin = (SoundPlugin*) seqtrack->patch->patchdata;
              if (plugin==NULL){
                R_ASSERT_NON_RELEASE(false); // this might happen legally. Not sure.
              }else
                SEQTRACKPLUGIN_request_remove_sample(plugin, seqblock->sample_id, Seqblock_Type::REGULAR);
            }
          }
        }END_VECTOR_FOR_EACH;
      }END_VECTOR_FOR_EACH;
    }
    
    //printf("        CREATING FROM STATE\n");

    
    double state_samplerate;
    
    if (HASH_has_key(state, "samplerate"))
      state_samplerate = HASH_get_float(state, "samplerate");
    else
      state_samplerate = find_samplerate_in_old_song_state(state);
    
    if (state_samplerate > 0){
      
      double samplerate = pc->pfreq;
      
      if (fabs(state_samplerate-samplerate)>1){
        
        R_ASSERT_NON_RELEASE(g_is_loading);
        
        if(g_is_loading && false==STRING_starts_with2(STRING_to_upper(dc.filename_with_full_path), STRING_to_upper(OS_get_program_path2())))
          GFX_addMessage("Warning, the song was saved with a samplerate of %dHz, while we are using a samplerate of %dHz. The song might not sound the same, and some instruments and effects may behave differently.\n",
                         (int)state_samplerate,
                         pc->pfreq
                         );
      } else {
        
        state_samplerate = -1.0;
        
      }      
    }


  
    // Need to do this first since widgets are not positioned correctly if it's done last. Not quite sure why.
    if(HASH_has_key(state, "song_tempo_automation"))
      TEMPOAUTOMATION_create_from_state(HASH_get_hash(state, "song_tempo_automation"), state_samplerate);
    
    vector_t seqtracks = {0};

    {
      int num_seqtracks = HASH_get_array_size(state, "seqtracks");
      R_ASSERT_RETURN_IF_FALSE(num_seqtracks > 0);
      
      int seqtracknum = 0;
      
      for(int i = 0 ; i < num_seqtracks ; i++){
        auto seqtracks_for_seqtrack = SEQTRACK_create_from_state(HASH_get_hash_at(state, "seqtracks", i), state_samplerate, seqtracknum, SHOW_ASSERTION, song);
        
        for(auto *seqtrack : seqtracks_for_seqtrack)
          VECTOR_push_back(&seqtracks, seqtrack);
        
        seqtracknum += seqtracks_for_seqtrack.size();
      }
    }
    
    int new_curr_seqtracknum = HASH_has_key(state, "curr_seqtracknum") ? HASH_get_int32(state, "curr_seqtracknum") : 0;
    
    {
      radium::PlayerPause pause(is_playing_song());
      radium::PlayerLock lock;
      
      root->song->seqtracks = seqtracks;

      ATOMIC_SET(root->song->curr_seqtracknum, new_curr_seqtracknum);

      VECTOR_FOR_EACH(struct SeqTrack *, seqtrack, &root->song->seqtracks){
        SEQTRACKPLUGIN_assert_samples2(seqtrack);
      }END_VECTOR_FOR_EACH;
    }

    evalScheme(talloc_format("(FROM_C-call-me-when-num-seqtracks-might-have-changed %d)", root->song->seqtracks.num_elements));

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
  
  if (false==HASH_has_key(state, "contains_seqtime")){
    R_ASSERT_NON_RELEASE(g_is_loading);
    SEQUENCER_block_changes_tempo_multiplier(NULL, -1); // Remove seqtime.
  }

  SEQUENCER_update(SEQUPDATE_EVERYTHING);
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
  R_ASSERT_NON_RELEASE(g_is_loading==false);
  
  int num_seqtracks = HASH_get_array_size(state, "seqtrackautomations");
  R_ASSERT_RETURN_IF_FALSE(num_seqtracks > 0);
  R_ASSERT_RETURN_IF_FALSE(num_seqtracks == root->song->seqtracks.num_elements);
  
  for(int i = 0 ; i < num_seqtracks ; i++){
    struct SeqTrack *seqtrack = (struct SeqTrack *)root->song->seqtracks.elements[i];

    auto *old_seqtrackautomation = seqtrack->seqtrackautomation;
    auto *new_seqtrackautomation = SEQTRACK_AUTOMATION_create(seqtrack, HASH_get_hash_at(state, "seqtrackautomations", i), -1);

    {
      radium::PlayerLock lock;
      seqtrack->seqtrackautomation = new_seqtrackautomation;
    }

    SEQTRACK_AUTOMATION_free(old_seqtrackautomation);
  }

  SEQUENCER_update(SEQUPDATE_TIME);
}


static hash_t *get_envelopes_state(const struct SeqTrack *seqtrack){
  hash_t *state = HASH_create(seqtrack->seqblocks.num_elements);

  VECTOR_FOR_EACH(const struct SeqBlock *, seqblock, &seqtrack->seqblocks){
    dyn_t envelope = SEQBLOCK_ENVELOPE_get_state(seqblock->envelope);
    HASH_put_dyn_at(state, "seqblockenvelope", iterator666, envelope);
  }END_VECTOR_FOR_EACH;  

  return state;
}

hash_t *SEQUENCER_get_envelopes_state(void){
  hash_t *state = HASH_create(root->song->seqtracks.num_elements);

  VECTOR_FOR_EACH(const struct SeqTrack *, seqtrack, &root->song->seqtracks){
    
    hash_t *seqtrack_state = get_envelopes_state(seqtrack);
    HASH_put_hash_at(state, "seqtrackenvelopes", iterator666, seqtrack_state);
    
  }END_VECTOR_FOR_EACH;

  return state;
}

static void apply_envelopes_state(const struct SeqTrack *seqtrack, const hash_t *envelopes){
  R_ASSERT_NON_RELEASE(g_is_loading==false);
  
  int num_seqblocks = HASH_get_array_size(envelopes, "seqblockenvelope");
  R_ASSERT_RETURN_IF_FALSE(num_seqblocks == seqtrack->seqblocks.num_elements);

  VECTOR_FOR_EACH(const struct SeqBlock *, seqblock, &seqtrack->seqblocks){
    
    const dyn_t envelope = HASH_get_dyn_at(envelopes, "seqblockenvelope", iterator666);
    SEQBLOCK_ENVELOPE_apply_state(seqblock->envelope, envelope, -1);
    
  }END_VECTOR_FOR_EACH;  
}

void SEQUENCER_create_envelopes_from_state(hash_t *state){
  int num_seqtracks = HASH_get_array_size(state, "seqtrackenvelopes");
  R_ASSERT_RETURN_IF_FALSE(num_seqtracks > 0);
  R_ASSERT_RETURN_IF_FALSE(num_seqtracks == root->song->seqtracks.num_elements);
  
  for(int i = 0 ; i < num_seqtracks ; i++){
    struct SeqTrack *seqtrack = (struct SeqTrack *)root->song->seqtracks.elements[i];

    apply_envelopes_state(seqtrack, HASH_get_hash_at(state, "seqtrackenvelopes", i));
  }

  SEQUENCER_update(SEQUPDATE_TIME);
}

// When playing song, block->tempo_multiplier is ignored. Instead seqblocks are stretched.
void SEQUENCER_block_changes_tempo_multiplier(const struct Blocks *block, double new_tempo_multiplier){
  radium::PlayerPauseOnlyIfNeeded pause(is_playing_song());
  radium::PlayerLockOnlyIfNeeded lock;

  if (block==NULL)
    R_ASSERT_RETURN_IF_FALSE(g_is_loading==true);

  bool is_loading_old_song = block==NULL;

  VECTOR_FOR_EACH(struct SeqTrack *, seqtrack, &root->song->seqtracks){
    int64_t skew = 0;

    VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){

      int64_t add_skew = 0;

      if (seqblock->block == block || is_loading_old_song){

        if (is_loading_old_song)
          new_tempo_multiplier = ATOMIC_DOUBLE_GET(seqblock->block->reltempo);

        //printf("new_tempo_multiplier: %f\n", new_tempo_multiplier);
        pause.need_it();
        lock.lock();

        double nonstretched_seqblock_duration = seqblock->t.interior_end - seqblock->t.interior_start;

        double stretch_without_tempo_multiplier = is_loading_old_song ? 1.0 : seqblock->t.stretch_without_tempo_multiplier;

        seqblock->t.stretch = stretch_without_tempo_multiplier / new_tempo_multiplier;

        int64_t new_time2 = seqblock->t.time + (nonstretched_seqblock_duration * seqblock->t.stretch);
        if (new_time2 < seqblock->t.time + 64){
          //R_ASSERT_NON_RELEASE(new_time2 > seqblock->t.time);
          R_ASSERT_NON_RELEASE(false);
          new_time2 = seqblock->t.time + 64;
        }

        add_skew = new_time2-seqblock->t.time2;

        seqblock->t.time2 = new_time2;

      }
      
      if (skew != 0){
        seqblock->t.time += skew;
        seqblock->t.time2 += skew;
      }

      skew += add_skew;
      
      lock.maybe_pause(iterator666);
    
    }END_VECTOR_FOR_EACH;

    if (skew > 0)
      legalize_seqtrack_timing(seqtrack, &lock);

  }END_VECTOR_FOR_EACH;  
}

