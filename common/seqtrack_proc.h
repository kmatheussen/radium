

#ifndef _RADIUM_COMMON_SEQTRACK_PROC_H
#define _RADIUM_COMMON_SEQTRACK_PROC_H


#include <math.h>


#include "../audio/Seqtrack_plugin_proc.h"



#define SEQNAV_SIZE_HANDLE_WIDTH 50
#define SEQUENCER_EXTRA_SONG_LENGTH 30.0 // sequencer gui always shows 30 seconds more than the song length


enum GridType{  
  NO_GRID = 0,
  BAR_GRID = 1,
  BEAT_GRID = 2
};
#define FIRST_LEGAL_GRID NO_GRID
#define LAST_LEGAL_GRID BEAT_GRID



static inline int64_t SEQBLOCK_get_seq_duration(const struct SeqBlock *seqblock){
  return seqblock->time2 - seqblock->time;
}

static inline int64_t SEQBLOCK_get_seq_endtime(const struct SeqBlock *seqblock){
  return seqblock->time2;
}


static inline double seqtime_to_blocktime_double(const struct SeqBlock *seqblock, double seqtime){
  if(seqblock->stretch==1.0)
    return seqtime;
  else
    return seqtime / seqblock->stretch;
}


static inline int64_t seqtime_to_blocktime(const struct SeqBlock *seqblock, int64_t seqtime){
  if(seqblock->stretch==1.0)
    return seqtime;
  else
    return round((double)seqtime / seqblock->stretch);
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
  return blocktime_to_seqtime2(seqblock->stretch, blocktime);
}


extern LANGSPEC int64_t get_abstime_from_seqtime(const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock, int64_t seqtime); // Result is rounded down to nearest integer. 'seqblock' may be NULL.
extern LANGSPEC int64_t get_seqtime_from_abstime(const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock_to_ignore, int64_t abstime); // Result is rounded down to nearest integer.

extern LANGSPEC void SEQTRACK_call_me_very_often(void);

extern LANGSPEC void SONG_call_me_before_starting_to_play_song(int64_t abstime);

// 'seqblock' must be nulled out before calling.
extern LANGSPEC void SEQBLOCK_init(struct SeqBlock *seqblock, struct Blocks *block, bool *track_is_disabled, int64_t time);
  
// sequencer gfx
#ifdef USE_QT4
#include <QWidget>
extern QWidget *SEQUENCER_getWidget(void);
#endif

extern struct SeqBlock *g_curr_seqblock;

extern LANGSPEC float SEQUENCER_get_x1(void);
extern LANGSPEC float SEQUENCER_get_x2(void);
extern LANGSPEC float SEQUENCER_get_y1(void);
extern LANGSPEC float SEQUENCER_get_y2(void);

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

extern LANGSPEC void SEQUENCER_set_grid_type(enum GridType grid_type);

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
extern LANGSPEC void SEQNAV_update(void);
extern LANGSPEC void RT_SEQUENCER_update_sequencer_and_playlist(void);

// seqtrack gfx
extern LANGSPEC float SEQTRACK_get_x1(int seqtracknum);
extern LANGSPEC float SEQTRACK_get_x2(int seqtracknum);
extern LANGSPEC float SEQTRACK_get_y1(int seqtracknum);
extern LANGSPEC float SEQTRACK_get_y2(int seqtracknum);

extern LANGSPEC void SEQTRACK_update(struct SeqTrack *seqtrack);
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

extern LANGSPEC void SEQUENCER_set_looping(bool do_loop);
extern LANGSPEC bool SEQUENCER_is_looping(void);
extern LANGSPEC void SEQUENCER_set_loop_start(int64_t start);
extern LANGSPEC int64_t SEQUENCER_get_loop_start(void);
extern LANGSPEC void SEQUENCER_set_loop_end(int64_t end);
extern LANGSPEC int64_t SEQUENCER_get_loop_end(void);

// seqblocks gfx
extern LANGSPEC float SEQBLOCK_get_x1(int seqblocknum, int seqtracknum);
extern LANGSPEC float SEQBLOCK_get_x2(int seqblocknum, int seqtracknum);
extern LANGSPEC float SEQBLOCK_get_y1(int seqblocknum, int seqtracknum);
extern LANGSPEC float SEQBLOCK_get_y2(int seqblocknum, int seqtracknum);

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
extern LANGSPEC void SEQTRACK_update_all_seqblock_start_and_end_times(struct SeqTrack *seqtrack);
extern LANGSPEC void SEQTRACK_update_all_seqblock_gfx_start_and_end_times(struct SeqTrack *seqtrack);
//extern LANGSPEC void SEQUENCER_update_all_seqblock_positions(void);
extern LANGSPEC void SEQUENCER_update(void); // Can be called from a different thread than the main thread. That thread might have to be QThread though.

  
// seqtrack
extern LANGSPEC void RT_legalize_seqtrack_timing(struct SeqTrack *seqtrack);

extern LANGSPEC void SEQTRACK_move_all_seqblocks_to_the_right_of(struct SeqTrack *seqtrack, int seqblocknum, int64_t how_much);
extern LANGSPEC void SEQTRACK_delete_seqblock(struct SeqTrack *seqtrack, const struct SeqBlock *seqblock);
extern LANGSPEC void SEQTRACK_delete_gfx_gfx_seqblock(struct SeqTrack *seqtrack, const struct SeqBlock *seqblock);

extern LANGSPEC bool SEQTRACK_set_seqblock_start_and_stop(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, int64_t new_start_time, int64_t new_end_time, const bool is_gfx);
    
extern LANGSPEC void SEQTRACK_move_seqblock(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, int64_t new_time);
extern LANGSPEC void SEQTRACK_move_gfx_seqblock(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, int64_t new_time);

extern LANGSPEC void RT_SEQTRACK_called_per_block(const struct SeqTrack *seqtrack);

extern LANGSPEC void SEQUENCER_timing_has_changed(void);

//extern LANGSPEC void SEQTRACK_move_gfx_gfx_seqblock(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, int64_t new_abs_time);

extern LANGSPEC void SEQTRACK_insert_silence(struct SeqTrack *seqtrack, int64_t seqtime, int64_t length);
extern LANGSPEC int SEQTRACK_insert_seqblock(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, int64_t seqtime, int64_t end_seqtime);
extern LANGSPEC int SEQTRACK_insert_block(struct SeqTrack *seqtrack, struct Blocks *block, int64_t seqtime, int64_t end_seqtime);
extern LANGSPEC int SEQTRACK_insert_gfx_gfx_block(struct SeqTrack *seqtrack, int seqtracknum, struct Blocks *block, const wchar_t *filename, int64_t seqtime, int64_t end_seqtime);
extern LANGSPEC int SEQTRACK_insert_sample(struct SeqTrack *seqtrack, int seqtracknum, const wchar_t *filename, int64_t seqtime, int64_t end_seqtime);
extern LANGSPEC double SEQTRACK_get_length(struct SeqTrack *seqtrack);
extern LANGSPEC double SEQTRACK_get_gfx_length(struct SeqTrack *seqtrack);
//extern LANGSPEC void SEQTRACK_init(struct SeqTrack *seqtrack, const hash_t *automation_state);
extern LANGSPEC struct SeqTrack *SEQTRACK_create(const hash_t *automation_state);
extern LANGSPEC struct SeqTrack *SEQTRACK_create_from_playlist(const int *playlist, int len);


// sequencer
extern LANGSPEC void SEQUENCER_remove_block_from_seqtracks(struct Blocks *block);
extern LANGSPEC int64_t SEQUENCER_find_closest_bar_start(int seqtracknum, int64_t pos_seqtime);
extern LANGSPEC int64_t SEQUENCER_find_closest_beat_start(int seqtracknum, int64_t pos_abstime);
extern LANGSPEC int64_t SEQUENCER_find_closest_line_start(int seqtracknum, int64_t pos_abstime);
extern LANGSPEC hash_t *SEQUENCER_get_state(void);
extern LANGSPEC void SEQUENCER_create_from_state(hash_t *state);
extern LANGSPEC void SEQUENCER_update_all_seqblock_start_and_end_times(void);
extern LANGSPEC void SEQUENCER_insert_seqtrack(struct SeqTrack *new_seqtrack, int pos);
extern LANGSPEC void SEQUENCER_append_seqtrack(struct SeqTrack *new_seqtrack);
extern LANGSPEC void SEQUENCER_replace_seqtrack(struct SeqTrack *new_seqtrack, int pos);
extern LANGSPEC void SEQUENCER_delete_seqtrack(int pos);

// song
extern LANGSPEC double SONG_get_length(void);
extern LANGSPEC double SONG_get_gfx_length(void);
extern LANGSPEC void SEQUENCER_init(struct Song *song);
extern LANGSPEC void SONG_init(void);

// automation state
extern LANGSPEC hash_t *SEQUENCER_get_automations_state(void);
extern LANGSPEC void SEQUENCER_create_automations_from_state(hash_t *state);

static inline const wchar_t *get_seqblock_sample_name(const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock, bool full_path){
  R_ASSERT_RETURN_IF_FALSE2(seqblock->block==NULL, L"");
  R_ASSERT_RETURN_IF_FALSE2(seqtrack->patch!=NULL, L"");

  const struct SoundPlugin *plugin = (struct SoundPlugin*)seqtrack->patch->patchdata;

  if (plugin==NULL)
    return L"";
  //R_ASSERT_RETURN_IF_FALSE2(plugin!=NULL, L"");
    
  return SEQTRACKPLUGIN_get_sample_name(plugin, seqblock->sample_id, full_path);
}


#if defined(USE_QT4) && defined(QSTRING_H)

struct SoundPlugin;

static inline QString get_seqblock_name(const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock, QString separator = ": ", bool right_justify = false){

  if (seqblock->block==NULL) {

    return STRING_get_qstring(get_seqblock_sample_name(seqtrack, seqblock, false));
    
  } else {

    const struct Blocks *block = seqblock->block;
    
    if (right_justify){
      int justify_blocklist = log10(root->song->num_blocks) + 1;
      return QString::number(block->l.num).rightJustified(justify_blocklist, ' ') + separator + QString(block->name);
    } else {
      return QString::number(block->l.num) + separator + QString(block->name);
    }
  }

  return "";
}

#endif

#endif

