

#ifndef _RADIUM_COMMON_SEQTRACK_PROC_H
#define _RADIUM_COMMON_SEQTRACK_PROC_H


#include <math.h>

#include "../common/time_proc.h"

#include "../audio/Seqtrack_plugin_proc.h"



#define SEQNAV_SIZE_HANDLE_WIDTH 50
#define SEQUENCER_EXTRA_SONG_LENGTH 30.0 // sequencer gui always shows 30 seconds more than the song length



extern LANGSPEC void SEQBLOCK_set_gain(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, float gain);
extern LANGSPEC float SEQBLOCK_get_gain(const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock);
extern LANGSPEC float SEQBLOCK_get_max_sample_gain(const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock);

static inline int64_t SEQBLOCK_get_seq_duration(const struct SeqBlock *seqblock){
  return seqblock->t.time2 - seqblock->t.time;
}

static inline int64_t SEQBLOCK_get_seq_endtime(const struct SeqBlock *seqblock){
  return seqblock->t.time2;
}


static inline double seqtime_to_blocktime_double(const struct SeqBlock *seqblock, double seqtime){
  if(seqblock->t.stretch==1.0)
    return seqtime;
  else
    return seqtime / seqblock->t.stretch;
}


static inline int64_t seqtime_to_blocktime(const struct SeqBlock *seqblock, int64_t seqtime){
  if(seqblock->t.stretch==1.0)
    return seqtime;
  else
    return round((double)seqtime / seqblock->t.stretch);
}

static inline double blocktime_to_seqtime_double(const double stretch, const double blocktime){
  if(stretch==1.0)
    return blocktime;
  else
    return blocktime * stretch;
}

static inline int64_t blocktime_to_seqtime2(const double stretch, const int64_t blocktime){
  if(stretch==1.0)
    return blocktime;
  else
    return round((double)blocktime * stretch);
}

static inline int64_t blocktime_to_seqtime(const struct SeqBlock *seqblock, const int64_t blocktime){
  return blocktime_to_seqtime2(seqblock->t.stretch, blocktime);
}

#ifdef __cplusplus
static inline int64_t blocktime_to_seqtime2(const double stretch, const double blocktime){
  if(stretch==1.0)
    return blocktime;
  else
    return round(blocktime * stretch);
}

static inline int64_t blocktime_to_seqtime(const struct SeqBlock *seqblock, const double blocktime){
  return blocktime_to_seqtime2(seqblock->t.stretch, blocktime);
}
#endif

static inline int64_t get_seqblock_place_time(const struct SeqBlock *seqblock, const Place p){
  return seqblock->t.time + blocktime_to_seqtime(seqblock, Place2STime(seqblock->block, &p));
}
                                 
static inline int64_t get_seqblock_place_time2(const struct SeqBlock *seqblock, const struct Tracks *track, const Place p){
  R_ASSERT_NON_RELEASE(track->times!=NULL);
  return seqblock->t.time + blocktime_to_seqtime(seqblock, Place2STime_from_times(seqblock->block->num_lines, track->times, &p));
}
                                 

static inline bool seqblock_is_stretched(const struct SeqBlock *seqblock){
  return fabs(seqblock->t.stretch - 1.0) > 0.00001;
}

static inline bool seqblock_is_speeded(const struct SeqBlock *seqblock){
  return fabs(seqblock->t.speed - 1.0) > 0.00001;
}

struct SoundPlugin;

static inline double get_seqblock_noninterior_start(const struct SeqBlock *seqblock){
  const struct SeqBlockTimings *timing = &seqblock->t;
    
  double t1 = timing->time;
  double i1 = timing->interior_start;
  
  if (i1==0)
    return t1;

  double stretch = timing->stretch * timing->speed;

  if (stretch==1.0)
    return t1 - i1;

  double s1 = t1 - (i1*stretch);
  return s1;
}

static inline double get_seqblock_noninterior_end(const struct SeqBlock *seqblock){
  const struct SeqBlockTimings *timing = &seqblock->t;
  
  double t2 = timing->time2;
  double i2 = timing->default_duration - timing->interior_end;

  if(i2==0)
    return t2;

  double stretch = timing->stretch * timing->speed;

  if (stretch==1.0)
    return t2 + i2;

  double s2 = t2 + (i2*stretch);
  return s2;
}

extern LANGSPEC void SEQTRACK_move_seqblock(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, int64_t new_start); // Careful, doesn't keep order and doesn't assert that much.

extern LANGSPEC void SEQTRACK_call_me_very_often(void);

extern LANGSPEC void SONG_call_me_before_starting_to_play_song(int64_t abstime);

// 'seqblock' must be nulled out before calling.
extern LANGSPEC void SEQBLOCK_init(const struct SeqTrack *from_seqtrack, struct SeqBlock *seqblock, struct Blocks *block, const hash_t *seqblock_state, double state_samplerate, bool *track_is_disabled, int64_t time);
  
// sequencer gfx
#ifdef USE_QT4
#include <QWidget>
extern QWidget *SEQUENCER_getWidget_r0(void); // does not throw assertion if sequencer widget hasn't been created yet.
extern QWidget *SEQUENCER_getWidget(void);
extern QWidget *SEQUENCER_getFrameWidget(void);
extern QWidget *SEQUENCER_create_navigator_widget(void);
#endif

extern LANGSPEC float SEQUENCER_get_x1(void);
extern LANGSPEC float SEQUENCER_get_x2(void);
extern LANGSPEC float SEQUENCER_get_y1(void);
extern LANGSPEC float SEQUENCER_get_y2(void);

extern LANGSPEC float SEQTRACKS_get_x1(void);
extern LANGSPEC float SEQTRACKS_get_x2(void);
extern LANGSPEC float SEQTRACKS_get_y1(void);
extern LANGSPEC float SEQTRACKS_get_y2(void);

extern LANGSPEC float SEQUENCER_get_left_part_x1(void);
extern LANGSPEC float SEQUENCER_get_left_part_x2(void);
extern LANGSPEC float SEQUENCER_get_left_part_y1(void);
extern LANGSPEC float SEQUENCER_get_left_part_y2(void);

extern LANGSPEC bool SEQUENCER_right_part_is_empty(void);
extern LANGSPEC void SEQUENCER_set_right_part_is_empty(bool is_empty);

extern LANGSPEC float SEQUENCER_get_right_part_x1(void);
extern LANGSPEC float SEQUENCER_get_right_part_x2(void);
extern LANGSPEC float SEQUENCER_get_right_part_y1(void);
extern LANGSPEC float SEQUENCER_get_right_part_y2(void);

extern LANGSPEC void SEQUENCER_disable_gfx_updates(void);
extern LANGSPEC void SEQUENCER_enable_gfx_updates(void);

#ifdef __cplusplus
struct SEQUENCER_ScopedGfxDisable{
  SEQUENCER_ScopedGfxDisable(){
    SEQUENCER_disable_gfx_updates();
  }
  ~SEQUENCER_ScopedGfxDisable(){
    SEQUENCER_enable_gfx_updates();
  }
};
#endif

extern LANGSPEC double SEQUENCER_get_visible_start_time(void);
extern LANGSPEC double SEQUENCER_get_visible_end_time(void);
extern LANGSPEC void SEQUENCER_set_visible_start_and_end_time(int64_t start_time, int64_t end_time);
extern LANGSPEC void SEQUENCER_set_visible_start_time(int64_t val);
extern LANGSPEC void SEQUENCER_set_visible_end_time(int64_t val);

extern LANGSPEC void SEQUENCER_hide_because_instrument_widget_is_large(void);
extern LANGSPEC void SEQUENCER_show_because_instrument_widget_is_large(void);
extern LANGSPEC bool SEQUENCER_has_mouse_pointer(void);

extern LANGSPEC void SEQUENCER_set_selection_rectangle(float x1, float y1, float x2, float y2);
extern LANGSPEC void SEQUENCER_unset_selection_rectangle(void);

// sequencer navigator gfx
extern LANGSPEC float SEQNAV_get_x1(void);
extern LANGSPEC float SEQNAV_get_x2(void);
extern LANGSPEC float SEQNAV_get_y1(void);
extern LANGSPEC float SEQNAV_get_y2(void);
extern LANGSPEC float SEQNAV_get_left_handle_x(void);
extern LANGSPEC float SEQNAV_get_right_handle_x(void);
//extern LANGSPEC void SEQNAV_update(void);
//extern LANGSPEC void RT_SEQUENCER_update_sequencer_and_playlist(void);

// seqtrack gfx
extern LANGSPEC float SEQTRACK_get_x1(int seqtracknum);
extern LANGSPEC float SEQTRACK_get_x2(int seqtracknum);
extern LANGSPEC float SEQTRACK_get_y1(int seqtracknum);
extern LANGSPEC float SEQTRACK_get_y2(int seqtracknum);

extern LANGSPEC int SEQUENCER_get_lowest_reasonable_topmost_seqtracknum(void);
extern LANGSPEC bool SEQUENCER_last_seqtrack_is_visible(void);

#ifdef __cplusplus
extern void SEQTRACK_update_with_borders(const struct SeqTrack *seqtrack, int64_t start_time, int64_t end_time);
extern void SEQTRACK_update(const struct SeqTrack *seqtrack, int64_t start_time, int64_t end_time);
#endif

extern LANGSPEC void SEQTRACK_update_with_nodes(const struct SeqTrack *seqtrack, int64_t start_time, int64_t end_time);
extern LANGSPEC void SEQTRACK_update_with_borders(const struct SeqTrack *seqtrack);
extern LANGSPEC void SEQTRACK_update(const struct SeqTrack *seqtrack);
extern LANGSPEC void SEQBLOCK_update(const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock);
extern LANGSPEC void SEQBLOCK_update2(const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock, int64_t start_time, int64_t end_time);
extern LANGSPEC void SEQBLOCK_update_with_borders(const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock);

extern LANGSPEC void SEQUENCER_update_seqblocks_holding_editor_block(struct Blocks *block);


//extern LANGSPEC void SEQTRACK_recreate(int seqtracknum);


extern LANGSPEC float SEQTEMPO_get_x1(void);
extern LANGSPEC float SEQTEMPO_get_x2(void);
extern LANGSPEC float SEQTEMPO_get_y1(void);
extern LANGSPEC float SEQTEMPO_get_y2(void);
extern LANGSPEC void SEQTEMPO_set_visible(bool visible);
extern LANGSPEC bool SEQTEMPO_is_visible(void);

// sequencer timeline and looping
extern LANGSPEC float SEQTIMELINE_get_x1(void);
extern LANGSPEC float SEQTIMELINE_get_x2(void);
extern LANGSPEC float SEQTIMELINE_get_y1(void);
extern LANGSPEC float SEQTIMELINE_get_y2(void);

extern LANGSPEC void SEQUENCER_set_looping(bool do_loop, int64_t start, int64_t end);
extern LANGSPEC bool SEQUENCER_is_looping(void);
extern LANGSPEC void SEQUENCER_set_loop_start(int64_t start);
extern LANGSPEC int64_t SEQUENCER_get_loop_start(void);
extern LANGSPEC void SEQUENCER_set_loop_end(int64_t end);
extern LANGSPEC int64_t SEQUENCER_get_loop_end(void);

extern LANGSPEC void SEQUENCER_set_punching(bool do_punch);
extern LANGSPEC bool SEQUENCER_is_punching(void);
extern LANGSPEC void SEQUENCER_set_punch_start(int64_t start);
extern LANGSPEC int64_t SEQUENCER_get_punch_start(void);
extern LANGSPEC void SEQUENCER_set_punch_end(int64_t end);
extern LANGSPEC int64_t SEQUENCER_get_punch_end(void);

// seqblocks gfx
extern LANGSPEC float SEQBLOCK_get_x1(int seqblocknum, int seqtracknum);
extern LANGSPEC float SEQBLOCK_get_x2(int seqblocknum, int seqtracknum);
extern LANGSPEC float SEQBLOCK_get_y1(int seqblocknum, int seqtracknum);
extern LANGSPEC float SEQBLOCK_get_y2(int seqblocknum, int seqtracknum);
extern LANGSPEC float SEQBLOCK_get_header_height(void); // name of block, etc.

#if __cplusplus
float SEQBLOCK_get_x1(const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock);
float SEQBLOCK_get_x2(const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock);
#endif

// seqblock left fade
extern LANGSPEC float SEQBLOCK_get_left_fade_x1(int seqblocknum, int seqtracknum);
extern LANGSPEC float SEQBLOCK_get_left_fade_y1(int seqblocknum, int seqtracknum);
extern LANGSPEC float SEQBLOCK_get_left_fade_x2(int seqblocknum, int seqtracknum);
extern LANGSPEC float SEQBLOCK_get_left_fade_y2(int seqblocknum, int seqtracknum);

// seqblock right fade
extern LANGSPEC float SEQBLOCK_get_right_fade_x1(int seqblocknum, int seqtracknum);
extern LANGSPEC float SEQBLOCK_get_right_fade_y1(int seqblocknum, int seqtracknum);
extern LANGSPEC float SEQBLOCK_get_right_fade_x2(int seqblocknum, int seqtracknum);
extern LANGSPEC float SEQBLOCK_get_right_fade_y2(int seqblocknum, int seqtracknum);

// seqblock left interior
extern LANGSPEC float SEQBLOCK_get_left_interior_x1(int seqblocknum, int seqtracknum);
extern LANGSPEC float SEQBLOCK_get_left_interior_y1(int seqblocknum, int seqtracknum);
extern LANGSPEC float SEQBLOCK_get_left_interior_x2(int seqblocknum, int seqtracknum);
extern LANGSPEC float SEQBLOCK_get_left_interior_y2(int seqblocknum, int seqtracknum);

// seqblock right interior
extern LANGSPEC float SEQBLOCK_get_right_interior_x1(int seqblocknum, int seqtracknum);
extern LANGSPEC float SEQBLOCK_get_right_interior_y1(int seqblocknum, int seqtracknum);
extern LANGSPEC float SEQBLOCK_get_right_interior_x2(int seqblocknum, int seqtracknum);
extern LANGSPEC float SEQBLOCK_get_right_interior_y2(int seqblocknum, int seqtracknum);

// seqblock left speed
extern LANGSPEC float SEQBLOCK_get_left_speed_x1(int seqblocknum, int seqtracknum);
extern LANGSPEC float SEQBLOCK_get_left_speed_y1(int seqblocknum, int seqtracknum);
extern LANGSPEC float SEQBLOCK_get_left_speed_x2(int seqblocknum, int seqtracknum);
extern LANGSPEC float SEQBLOCK_get_left_speed_y2(int seqblocknum, int seqtracknum);

// seqblock right speed
extern LANGSPEC float SEQBLOCK_get_right_speed_x1(int seqblocknum, int seqtracknum);
extern LANGSPEC float SEQBLOCK_get_right_speed_y1(int seqblocknum, int seqtracknum);
extern LANGSPEC float SEQBLOCK_get_right_speed_x2(int seqblocknum, int seqtracknum);
extern LANGSPEC float SEQBLOCK_get_right_speed_y2(int seqblocknum, int seqtracknum);

// seqblock left stretch
extern LANGSPEC float SEQBLOCK_get_left_stretch_x1(int seqblocknum, int seqtracknum);
extern LANGSPEC float SEQBLOCK_get_left_stretch_y1(int seqblocknum, int seqtracknum);
extern LANGSPEC float SEQBLOCK_get_left_stretch_x2(int seqblocknum, int seqtracknum);
extern LANGSPEC float SEQBLOCK_get_left_stretch_y2(int seqblocknum, int seqtracknum);

// seqblock right stretch
extern LANGSPEC float SEQBLOCK_get_right_stretch_x1(int seqblocknum, int seqtracknum);
extern LANGSPEC float SEQBLOCK_get_right_stretch_y1(int seqblocknum, int seqtracknum);
extern LANGSPEC float SEQBLOCK_get_right_stretch_x2(int seqblocknum, int seqtracknum);
extern LANGSPEC float SEQBLOCK_get_right_stretch_y2(int seqblocknum, int seqtracknum);

// sequencer_gfx
//extern LANGSPEC void SEQTRACK_update_all_seqblock_start_and_end_times(struct SeqTrack *seqtrack);
//extern LANGSPEC void SEQUENCER_update_all_seqblock_positions(void);

#define SEQUPDATE_TIME 1
#define SEQUPDATE_TIMELINE 2
#define SEQUPDATE_SONGTEMPO 4
#define SEQUPDATE_HEADERS 8
#define SEQUPDATE_TRACKORDER 16
#define SEQUPDATE_PLAYLIST 32
#define SEQUPDATE_BLOCKLIST 64
#define SEQUPDATE_NAVIGATOR 128
#define SEQUPDATE_TRACKCOORDINATES 256
#define SEQUPDATE_RIGHT_PART 512
#define SEQUPDATE_TIMING 1024 // tempos and signatures
#define SEQUPDATE_EVERYTHING (1|2|4|8|16|32|64|128|256|512|1024)

extern LANGSPEC void SEQUENCER_update(uint32_t what); // Can be called from any thread, and also while holding the player lock.

  
// seqtrack
#ifdef __cplusplus
extern LANGSPEC void RT_legalize_seqtrack_timing(struct SeqTrack *seqtrack, radium::PlayerLockOnlyIfNeeded *lock);
#endif

extern LANGSPEC void SEQTRACK_move_all_seqblocks_to_the_right_of(struct SeqTrack *seqtrack, int seqblocknum, int64_t how_much);
extern LANGSPEC void SEQTRACK_delete_seqblock(struct SeqTrack *seqtrack, const struct SeqBlock *seqblock, bool notify_listeners);
extern LANGSPEC void SEQTRACK_delete_gfx_gfx_seqblock(struct SeqTrack *seqtrack, const struct SeqBlock *seqblock);
extern LANGSPEC void SEQTRACK_set_recording(struct SeqTrack *seqtrack, bool is_recording);

/*
Too inconvenient. Use apply_gfx_seqblocks instead of these two functions.
extern LANGSPEC bool SEQBLOCK_set_interior_start(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, int64_t new_interior_start, bool is_gfx); // returns true if something was changed
extern LANGSPEC bool SEQBLOCK_set_interior_end(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, int64_t new_interior_end, bool is_gfx); // returns true if something was changed
*/

// return true if shape was changed. (I.e. not equal to old shape)
bool SEQBLOCK_set_fade_in_shape(struct SeqBlock *seqblock, enum FadeShape shape);
bool SEQBLOCK_set_fade_out_shape(struct SeqBlock *seqblock, enum FadeShape shape);


extern LANGSPEC bool RT_SEQTRACK_called_before_editor(struct SeqTrack *seqtrack); // Sets seqtrack->curr_sample_seqblock when starting/stopping playing audio file.

#ifdef __cplusplus
extern LANGSPEC void SEQUENCER_timing_has_changed(radium::PlayerLockOnlyIfNeeded &lock);
#endif

//extern LANGSPEC void SEQTRACK_move_gfx_gfx_seqblock(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, int64_t new_abs_time);

extern LANGSPEC void SEQTRACK_insert_silence(struct SeqTrack *seqtrack, int64_t seqtime, int64_t length);
extern LANGSPEC int SEQTRACK_insert_block(struct SeqTrack *seqtrack, struct Blocks *block, int64_t seqtime, int64_t end_seqtime);
extern LANGSPEC int SEQTRACK_insert_gfx_gfx_block(struct SeqTrack *seqtrack, int seqtracknum, const hash_t *state, enum ShowAssertionOrThrowAPIException error_type);
extern LANGSPEC int SEQTRACK_insert_sample(struct SeqTrack *seqtrack, int seqtracknum, const wchar_t *filename, int64_t seqtime, int64_t end_seqtime);
extern LANGSPEC struct SeqBlock *SEQTRACK_add_recording_seqblock(struct SeqTrack *seqtrack, int64_t seqtime, int64_t end_seqtime);
extern LANGSPEC void SEQTRACK_remove_recording_seqblock(struct SeqTrack *seqtrack, struct SeqBlock *seqblock);
extern LANGSPEC void SEQUENCER_remove_sample_from_song(const wchar_t *filename);
extern LANGSPEC double SEQTRACK_get_length(struct SeqTrack *seqtrack);
//extern LANGSPEC void SEQTRACK_init(struct SeqTrack *seqtrack, const hash_t *automation_state);

#if defined(__cplusplus) && defined(QVECTOR_H)
// The returned vector contains the vector to paint last first, and vice versa.
QVector<struct SeqBlock*> SEQTRACK_get_seqblocks_in_z_order(const struct SeqTrack *seqtrack, bool is_gfx_gfx);
#endif


enum GridType{  
  NO_GRID = 0,
  BAR_GRID = 1,
  BEAT_GRID = 2,
  LINE_GRID = 3
};
#define FIRST_LEGAL_GRID NO_GRID
#define LAST_LEGAL_GRID LINE_GRID

static inline const char *grid_type_to_string(enum GridType what){
  switch(what){
  case NO_GRID: return "no";
  case BAR_GRID: return "bar";
  case BEAT_GRID: return "beat";
  case LINE_GRID: return "line";
  }
  R_ASSERT(false);
  return "";
}

static inline enum GridType string_to_grid_type(const char *what, bool *is_error){
  if (!strcmp(what, "no")) return NO_GRID;
  if (!strcmp(what, "bar")) return BAR_GRID;
  if (!strcmp(what, "beat")) return BEAT_GRID;
  if (!strcmp(what, "line")) return LINE_GRID;

  if(is_error!=NULL)
    *is_error = true;
  else
    R_ASSERT(false);
  
  return NO_GRID;
}

#if defined(__cplusplus)

namespace radium{
enum class IterateSeqblocksCallbackReturn{
  ISCR_BREAK,
  ISCR_CONTINUE
};
}

void SEQUENCER_iterate_time_seqblocks(int64_t start_seqtime, int64_t end_seqtime, bool include_previous_and_next_seqblock,
                                      std::function<radium::IterateSeqblocksCallbackReturn(const struct SeqTrack*, const struct SeqBlock *,const struct Blocks*,const struct SeqBlock *)> callback
                                      );
void SEQUENCER_iterate_time(int64_t start_seqtime, int64_t end_seqtime, GridType what_to_find,
                            std::function<bool(int64_t,int,int,int)> callback);
#endif

extern LANGSPEC void SEQUENCER_set_grid_type(enum GridType grid_type);
extern LANGSPEC enum GridType SEQUENCER_get_grid_type(void);

extern LANGSPEC int64_t SEQUENCER_find_closest_grid_start(int64_t seqtime, enum GridType what);
extern LANGSPEC int64_t SEQUENCER_find_closest_bar_start(int64_t pos_seqtime);
extern LANGSPEC int64_t SEQUENCER_find_closest_beat_start(int64_t pos_seqtime);
extern LANGSPEC int64_t SEQUENCER_find_closest_line_start(int64_t pos_seqtime);


extern LANGSPEC hash_t *SEQBLOCK_get_state(const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock, bool always_get_real_end_time);
extern LANGSPEC int SEQBLOCK_insert_seqblock_from_state(hash_t *hash, enum ShowAssertionOrThrowAPIException error_type);
extern LANGSPEC void SEQBLOCK_replace_seqblock(hash_t *hash, bool must_replace_same_id, enum ShowAssertionOrThrowAPIException error_type);

extern LANGSPEC struct SeqTrack *SEQTRACK_create(const hash_t *automation_state, int seqtracknum, double state_samplerate, bool for_audiofiles, bool is_bus);
extern LANGSPEC struct SeqTrack *SEQTRACK_create_from_playlist(const int *playlist, int len);


extern LANGSPEC void SEQTRACK_create_gfx_seqblocks_from_state(const dyn_t seqblocks_state, struct SeqTrack *seqtrack, const int seqtracknum, enum ShowAssertionOrThrowAPIException error_type);
extern LANGSPEC dyn_t SEQTRACK_get_seqblocks_state(const struct SeqTrack *seqtrack);
extern LANGSPEC dyn_t SEQTRACK_get_gfx_seqblocks_state(const struct SeqTrack *seqtrack);
extern LANGSPEC void SEQTRACK_cancel_gfx_seqblocks(struct SeqTrack *seqtrack);
extern LANGSPEC void SEQTRACK_apply_gfx_seqblocks(struct SeqTrack *seqtrack, const int seqtracknum, bool seqtrack_is_live);


// sequencer
extern LANGSPEC void SEQUENCER_remove_block_from_seqtracks(struct Blocks *block);
extern LANGSPEC hash_t *SEQUENCER_get_state(void /*bool get_old_format*/);
extern LANGSPEC void SEQUENCER_create_from_state(hash_t *state, struct Song *song);
//extern LANGSPEC void SEQUENCER_update_all_seqblock_start_and_end_times(void);
extern LANGSPEC void SEQUENCER_insert_seqtrack(struct SeqTrack *new_seqtrack, int pos, bool for_audiofiles, bool is_bus);
extern LANGSPEC void SEQUENCER_append_seqtrack(struct SeqTrack *new_seqtrack, bool for_audiofiles, bool is_bus);
extern LANGSPEC void SEQUENCER_replace_seqtrack(struct SeqTrack *new_seqtrack, int pos);
extern LANGSPEC void SEQUENCER_delete_seqtrack(int pos);

extern LANGSPEC vector_t SEQUENCER_get_all_used_audiofile_names_note_USED(void);
extern LANGSPEC vector_t SEQUENCER_get_all_unused_audiofile_names_note_UNUSED(void);
extern LANGSPEC void SEQUENCER_make_all_used_audio_files_undeletable(void);
  
// song
extern LANGSPEC double SONG_get_length(void);
extern LANGSPEC void SEQUENCER_init(struct Song *song);
extern LANGSPEC void SONG_init(void);

// automation state
extern LANGSPEC hash_t *SEQUENCER_get_automations_state(void);
extern LANGSPEC void SEQUENCER_create_automations_from_state(hash_t *state);

extern LANGSPEC hash_t *SEQUENCER_get_seqblock_automation_state(int automationnum, int seqblocknum, int seqtracknum);
extern LANGSPEC hash_t *SEQUENCER_create_seqblock_automation_from_state(hash_t *state, bool return_old);

extern LANGSPEC hash_t *SEQUENCER_get_seqblock_automations_state(void);
extern LANGSPEC void SEQUENCER_create_seqblock_automations_from_state(hash_t *state);

extern LANGSPEC void SEQUENCER_block_changes_tempo_multiplier(const struct Blocks *block, double new_tempo_multiplier);

static inline double get_seqblock_time1_s(const struct SeqBlock *seqblock){
  return (double)seqblock->t.time / (double)pc->pfreq;
}

static inline double get_seqblock_time2_s(const struct SeqBlock *seqblock){
  return (double)seqblock->t.time2 / (double)pc->pfreq;
}

static inline const vector_t *gfx_seqblocks(const struct SeqTrack *seqtrack){
  if(seqtrack->gfx_seqblocks != NULL)
    return seqtrack->gfx_seqblocks;
  else
    return &seqtrack->seqblocks;
}
  
static inline const vector_t *gfx_seqblocks2(const struct SeqTrack *seqtrack, bool use_gfx_if_possible){
  if(use_gfx_if_possible && seqtrack->gfx_seqblocks != NULL)
    return seqtrack->gfx_seqblocks;
  else
    return &seqtrack->seqblocks;
}
  
static inline const wchar_t *get_seqblock_sample_name(const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock, bool full_path){
  R_ASSERT_RETURN_IF_FALSE2(seqblock->block==NULL, L"");
  R_ASSERT_RETURN_IF_FALSE2(seqtrack->patch!=NULL, L"");

  /*
  const struct SoundPlugin *plugin = (struct SoundPlugin*)seqtrack->patch->patchdata;

  if (plugin==NULL)
    return L"";
  */
  
  //return SEQTRACKPLUGIN_get_sample_name(plugin, seqblock->sample_id, full_path);
  if (full_path)
    return seqblock->sample_filename;
  else
    return seqblock->sample_filename_without_path;
}

static inline int get_seqtracknum(const struct SeqTrack *seqtrack){
  return VECTOR_find_pos(&root->song->seqtracks, seqtrack);
}

static inline int get_seqtracknum_from_patch(Patch *patch){
  VECTOR_FOR_EACH(struct SeqTrack *, seqtrack, &root->song->seqtracks){
    if (seqtrack->patch==patch)
      return iterator666;
  }END_VECTOR_FOR_EACH;

  return -1;
}



static inline int get_seqblocknum(const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock){
  return VECTOR_find_pos(&seqtrack->seqblocks, seqblock);
}

static inline int get_gfxseqblocknum(const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock){
  return VECTOR_find_pos(gfx_seqblocks(seqtrack), seqblock);
}

static inline int get_gfxgfxseqblocknum(const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock){
  return VECTOR_find_pos(&seqtrack->gfx_gfx_seqblocks, seqblock);
}

#if defined(USE_QT4) && defined(QSTRING_H)

struct SoundPlugin;

static inline QString get_seqblock_name(const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock, QString separator = ": ", bool right_justify = false){

  if (seqblock->name != NULL)
    return STRING_get_qstring(seqblock->name);
  
  if (seqblock->block==NULL) {

    return STRING_get_qstring(get_seqblock_sample_name(seqtrack, seqblock, false));
    
  } else {

    const struct Blocks *block = seqblock->block;
    
    if (right_justify){
#ifdef __cplusplus
      int justify_blocklist = ::log10(root->song->num_blocks) + 1;
#else
      int justify_blocklist = log10(root->song->num_blocks) + 1;
#endif
      return QString::number(block->l.num).rightJustified(justify_blocklist, ' ') + separator + QString(block->name);
    } else {
      return QString::number(block->l.num) + separator + QString(block->name);
    }
  }

  return "";
}

#endif


// recording config

static inline int get_num_recording_soundfile_channels(const struct SeqtrackRecordingConfig *config){
  int ret = 0;
  
  for (int soundfile_channel = 0 ; soundfile_channel < NUM_CHANNELS_RECORDING_MATRIX ; soundfile_channel++)
    for(int ch=0;ch<NUM_CHANNELS_RECORDING_MATRIX;ch++)
      if (config->matrix[ch][soundfile_channel]==true)
        ret = R_MAX(ret, soundfile_channel+1);

  return ret;
}

static inline void reset_recording_config(struct SeqtrackRecordingConfig *config){
  memset(config, 0, sizeof(struct SeqtrackRecordingConfig));
  
  config->record_from_system_input = true;
  config->matrix[0][0] = true;
  config->matrix[1][1] = true;
}

static inline struct SeqtrackRecordingConfig *get_seqtrack_recording_config(struct SeqTrack *seqtrack){
  if (seqtrack->use_custom_recording_config)
    return &seqtrack->custom_recording_config;
  else
    return &root->song->default_recording_config;
}

#endif

