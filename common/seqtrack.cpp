
#include <math.h>

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

#include "seqtrack_proc.h"

int64_t get_abstime_from_seqtime(const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock_where_time_is, int64_t block_seqtime){
  int64_t last_seq_end_time = 0;
  double last_abs_end_time = 0; // Is double because of reltempo multiplication.
    
  VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
    
    struct Blocks *block            = seqblock->block;
    double         tempo_multiplier = block->reltempo;
    
    int64_t pause_duration  = seqblock->time - last_seq_end_time; // (reltempo is not applied to pauses)
    
    int64_t seq_start_time  = seqblock->time;
    double  abs_start_time  = last_abs_end_time + pause_duration;
      
    if (seqblock == seqblock_where_time_is)
      return abs_start_time + ((double)block_seqtime / seqblock->block->reltempo); // Important that we round down.
      
    int64_t seq_block_duration = getBlockSTimeLength(seqblock->block);
    int64_t abs_block_duration = ((double)seq_block_duration / tempo_multiplier);
    
    int64_t seq_end_time = seq_start_time + seq_block_duration;
    double  abs_end_time = abs_start_time + abs_block_duration;
    
    //last_abs_start_time = abs_start_time;
    last_seq_end_time   = seq_end_time;
    last_abs_end_time   = abs_end_time;
    
    //printf("  start/end: %f  ->   %f\n",seqblock->start_time,seqblock->end_time);
  }END_VECTOR_FOR_EACH;

  // !!!
  R_ASSERT(false);
  // !!!
  
  return seqblock_where_time_is->time + ((double)block_seqtime / seqblock_where_time_is->block->reltempo); // fallback.
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
      double         tempo_multiplier = block->reltempo;
      
      int64_t pause_duration  = seqblock->time - last_seq_end_time; // (reltempo is not applied to pauses)
      
      int64_t seq_start_time  = seqblock->time;
      double  abs_start_time  = last_abs_end_time + pause_duration;
      
      if (abstime <= abs_start_time)
        break;
      
      int64_t seq_block_duration = getBlockSTimeLength(seqblock->block);
      int64_t abs_block_duration = ((double)seq_block_duration / tempo_multiplier);
      
      int64_t seq_end_time = seq_start_time + seq_block_duration;
      double  abs_end_time = abs_start_time + abs_block_duration;
      
      //last_abs_start_time = abs_start_time;
      last_seq_end_time   = seq_end_time;
      last_abs_end_time   = abs_end_time;

    }
    
    //printf("  start/end: %f  ->   %f\n",seqblock->start_time,seqblock->end_time);
  }END_VECTOR_FOR_EACH;

  return last_seq_end_time + (abstime - last_abs_end_time); // We lose the decimals here. Wonder if this inaccuracy could build up. Maybe using double/seconds everywhere instead would be better...
}

static struct SeqBlock *get_seqblock(struct Blocks *block){
  struct SeqBlock *seqblock = (struct SeqBlock*)talloc(sizeof(struct SeqBlock));
  seqblock->block = block;
  seqblock->time = -1;
  seqblock->gfx_time = seqblock->time;
  return seqblock;
}

// Ensures that two seqblocks doesn't overlap, and that a seqblock doesn't start before 0.
// Preserves original pause times.
static void legalize_seqtrack_timing(struct SeqTrack *seqtrack, bool is_gfx){

  if (!is_gfx)
    R_ASSERT(PLAYER_current_thread_has_lock());
  
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
  legalize_seqtrack_timing(seqtrack, false);
}

static void update_all_seqblock_start_and_end_times(struct SeqTrack *seqtrack, bool is_gfx){

  double last_abs_end_time = 0;
  int64_t last_end_time = 0;

  double sample_rate = MIXER_get_sample_rate();
  
  VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
    struct Blocks *block            = seqblock->block;
    double         tempo_multiplier = block->reltempo;

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
  struct SeqBlock *seqblock = (struct SeqBlock*)talloc(sizeof(struct SeqBlock));
  int blocknum = HASH_get_int32(state, "blocknum");

  double samplerate = HASH_get_float(state, "samplerate");
      
  seqblock->block = (struct Blocks*)ListFindElement1(&root->song->blocks->l, blocknum);
  seqblock->time = HASH_get_int(state, "time");

  if (fabs(samplerate - MIXER_get_sample_rate()) < 1)
    seqblock->time *= samplerate / MIXER_get_sample_rate();
    
  seqblock->gfx_time = seqblock->time;
  
  return seqblock;
}

void SEQTRACK_init(struct SeqTrack *seqtrack){
  memset(seqtrack, 0, sizeof(struct SeqTrack));
  seqtrack->scheduler = SCHEDULER_create();
}

struct SeqTrack *SEQTRACK_create(void){
  struct SeqTrack *seqtrack = (struct SeqTrack*)talloc(sizeof(struct SeqTrack));
  SEQTRACK_init(seqtrack);

  return seqtrack;
}
                   
hash_t *SEQTRACK_get_state(const struct SeqTrack *seqtrack){
  hash_t *state = HASH_create(seqtrack->seqblocks.num_elements);

  for(int i=0;i<seqtrack->seqblocks.num_elements;i++)
    HASH_put_hash_at(state, "seqblock", i, SEQBLOCK_get_state((struct SeqBlock*)seqtrack->seqblocks.elements[i]));

  return state;
}

struct SeqTrack *SEQTRACK_create_from_state(const hash_t *state){
  struct SeqTrack *seqtrack = SEQTRACK_create();

  int num_seqblocks = HASH_get_num_elements(state);

  for(int i=0;i<num_seqblocks;i++)
    VECTOR_push_back(&seqtrack->seqblocks, SEQBLOCK_create_from_state(HASH_get_hash_at(state, "seqblock", i)));

  return seqtrack;
}

// Compatibility with old songs
struct SeqTrack *SEQTRACK_create_from_playlist(const int *playlist, int len){
  vector_t seqblocks = {0};
  
  for(int pos=0;pos<len;pos++)
    VECTOR_push_back(&seqblocks, get_seqblock((struct Blocks *)ListFindElement1(&root->song->blocks->l,playlist[pos])));
  
  struct SeqTrack *seqtrack = SEQTRACK_create();
    
  seqtrack->seqblocks = seqblocks;

  {
    radium::PlayerLock lock;
    set_plain_seqtrack_timing_no_pauses(seqtrack);
  }
  
  return seqtrack;
}

void SEQTRACK_delete_seqblock(struct SeqTrack *seqtrack, const struct SeqBlock *seqblock){
  {
    radium::PlayerPause pause;
    radium::PlayerRecursiveLock lock;

    VECTOR_remove(&seqtrack->seqblocks, seqblock);
  }

  RT_SEQUENCER_update_sequencer_and_playlist();
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
    radium::PlayerPause pause;
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
    radium::PlayerPause pause;
    radium::PlayerLock lock;

    seqblock->time = new_seqblock_time;
    seqblock->gfx_time = seqblock->time;

    RT_legalize_seqtrack_timing(seqtrack);
  }

  double new_song_visible_length = SONG_get_gfx_length();

  if (new_song_visible_length != old_song_visible_length)
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

void SEQTRACK_insert_silence(struct SeqTrack *seqtrack, int64_t seqtime, int64_t length){

  {
    radium::PlayerPause pause;
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

void SEQTRACK_insert_seqblock(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, int64_t seqtime){

  // Assert that the seqblock is not in a seqtrack already.
  VECTOR_FOR_EACH(struct SeqTrack *, seqtrack_here, &root->song->seqtracks){
    R_ASSERT_RETURN_IF_FALSE(!VECTOR_is_in_vector(&seqtrack_here->seqblocks, seqblock));
  }END_VECTOR_FOR_EACH;

  
  int pos = 0;
  
  VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){

    pos = iterator666;

    if (seqblock->time >= seqtime)
      goto gotit;
    
  }END_VECTOR_FOR_EACH;

  pos = seqtrack->seqblocks.num_elements;
  
 gotit:
    
  VECTOR_ensure_space_for_one_more_element(&seqtrack->seqblocks);

  {
    radium::PlayerPause pause;
    radium::PlayerLock lock;
    
    seqblock->time = seqtime;
    seqblock->gfx_time = seqblock->time;

    VECTOR_insert(&seqtrack->seqblocks, seqblock, pos);
    
    RT_legalize_seqtrack_timing(seqtrack);
  }

  RT_SEQUENCER_update_sequencer_and_playlist();
}

void SEQTRACK_insert_block(struct SeqTrack *seqtrack, struct Blocks *block, int64_t seqtime){
  struct SeqBlock *seqblock = get_seqblock(block);
  SEQTRACK_insert_seqblock(seqtrack, seqblock, seqtime);
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
    radium::PlayerPause pause;
    radium::PlayerLock lock;

    for(auto pair : to_remove)
      SEQTRACK_delete_seqblock(pair.first, pair.second);
  }

  RT_SEQUENCER_update_sequencer_and_playlist();
}

void SONG_insert_seqtrack(struct SeqTrack *new_seqtrack, int pos){
  
  VECTOR_ensure_space_for_one_more_element(&root->song->seqtracks);

  {
    radium::PlayerPause pause;
    radium::PlayerLock lock;
            
    VECTOR_insert(&root->song->seqtracks, new_seqtrack, pos);
  }

  RT_SEQUENCER_update_sequencer_and_playlist();
}

void SONG_replace_seqtrack(struct SeqTrack *new_seqtrack, int pos){
  {
    radium::PlayerPause pause;
    radium::PlayerLock lock;
    
    VECTOR_set(&root->song->seqtracks, pos, new_seqtrack);

    RT_SEQUENCER_update_sequencer_and_playlist();
  }
  
}

void SONG_delete_seqtrack(int pos){
  R_ASSERT_RETURN_IF_FALSE(pos >= 0);
  R_ASSERT_RETURN_IF_FALSE(root->song->seqtracks.num_elements > 1); // There must always be a seqtrack
  R_ASSERT_RETURN_IF_FALSE(pos < root->song->seqtracks.num_elements);
  
  {
    radium::PlayerPause pause;
    radium::PlayerLock lock;
    
    VECTOR_delete(&root->song->seqtracks, pos);

    RT_SEQUENCER_update_sequencer_and_playlist();      
  }

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
  

void SONG_init(void){
  struct SeqTrack *seqtrack = (struct SeqTrack*)talloc(sizeof(struct SeqTrack));
  VECTOR_ensure_space_for_one_more_element(&seqtrack->seqblocks);
  
  VECTOR_ensure_space_for_one_more_element(&root->song->seqtracks);
  
  struct SeqBlock *seqblock = get_seqblock(root->song->blocks);
  
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

  return state;
}

void SEQUENCER_create_from_state(hash_t *state){

  //printf("        CREATING FROM STATE\n");
  
  vector_t seqtracks = {0};
  
  int num_seqtracks = HASH_get_num_elements(state);
  for(int i = 0 ; i < num_seqtracks ; i++){
    struct SeqTrack *seqtrack = SEQTRACK_create_from_state(HASH_get_hash_at(state, "seqtracks", i));
    VECTOR_push_back(&seqtracks, seqtrack);
  }

  {
    radium::PlayerPause pause;
    radium::PlayerLock lock;

    root->song->seqtracks = seqtracks;
  }

  BS_UpdatePlayList();
}
