/* Copyright 2012 Kjetil S. Matheussen

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



#include "../common/nsmtracker.h"
#include "../common/TimeData.hpp"
#include "../common/FX.hpp"
#include "../common/disk.h"
#include "../common/vector_proc.h"
#include "../common/visual_proc.h"
#include "../common/instruments_proc.h"
#include "../common/patch_proc.h"
#include "../common/player_proc.h"
#include "../common/OS_visual_input.h"
#include "../common/OS_Player_proc.h"
#include "../common/fxlines_proc.h"
#include "../common/windows_proc.h"
#include "../common/settings_proc.h"
#include "../common/scheduler_proc.h"
#include "../common/player_pause_proc.h"
#include "../common/undo_tracks_proc.h"
#include "../common/sequencer_proc.h"

#include "SoundPlugin.h"
#include "Mixer_proc.h"

#include "SoundPlugin_proc.h"
#include "SoundPluginRegistry_proc.h"
#include "SoundProducer_proc.h"
#include "Seqtrack_plugin_proc.h"

#include "../mixergui/QM_MixerWidget.h"
#include "../mixergui/QM_chip.h"
//#include "../mixergui/undo_chip_addremove_proc.h"
#include "../mixergui/undo_mixer_connections_proc.h"

#include "../Qt/Qt_instruments_proc.h"

#include "audio_instrument_proc.h"

#define MAX_FX_VAL (1<<16)
//(1<<30)

enum{
  PERMANENT_PATCH_ID_BUS1 = 1,
  PERMANENT_PATCH_ID_BUS2 = 2,
  PERMANENT_PATCH_ID_MAIN_PIPE = 3,
  
  PERMANENT_PATCH_ID_BUS3 = 4,
  PERMANENT_PATCH_ID_BUS4 = 5,
  PERMANENT_PATCH_ID_BUS5 = 6,
};

    
/* Audio Patch */

static int64_t RT_scheduled_send_play_note_to_plugin(struct SeqTrack *seqtrack, int64_t time, union SuperType *args){
  struct Patch *patch = (struct Patch*)args[0].pointer;

  SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;

  if(plugin==NULL || plugin->type->stop_note == NULL)
    return DONT_RESCHEDULE;

  const note_t note = create_note_from_args(&args[1]);

  //printf("     Time: %d, Delta time: %d\n", (int)time, (int)PLAYER_get_block_delta_time(time));

  if (!Patch_addPlayingVoice(&plugin->playing_voices, note, seqtrack))
    return DONT_RESCHEDULE;

  if (note.sample_pos==0 || ATOMIC_GET(plugin->enable_sample_seek))
    plugin->type->play_note(plugin, PLAYER_get_block_delta_time(seqtrack, time), note);

  return DONT_RESCHEDULE;
}

static void AUDIO_playnote(struct SeqTrack *seqtrack, struct Patch *patch, note_t note, STime time){
  SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;

  if(plugin==NULL || plugin->type->play_note == NULL)
    return;

  RT_PLUGIN_touch(plugin);
    
  const int latency = RT_SP_get_input_latency(plugin->sp);

  if (latency == 0) {
    if (!Patch_addPlayingVoice(&plugin->playing_voices, note, seqtrack))
      return;

    if (note.sample_pos==0 || ATOMIC_GET(plugin->enable_sample_seek))
      plugin->type->play_note(plugin, PLAYER_get_block_delta_time(seqtrack, time), note);
    
    return;
  }

  time += (double)latency * get_note_reltempo(note);

  union SuperType args[8];
  args[0].pointer = patch;
  put_note_into_args(&args[1], note);

  //printf("   Scheduling %d (latency: %d). block_reltempo: %f\n", (int)time, latency, note.block_reltempo);
  SCHEDULER_add_event(seqtrack, time, RT_scheduled_send_play_note_to_plugin, &args[0], 8, SCHEDULER_NOTE_ON_PRIORITY);
}


// velocity

static void set_note_volume_now(struct SeqTrack *seqtrack, struct Patch *patch, const note_t note, int64_t time){
  
  /*
  // Modulators are not latency-compensated, so this would be incorrect if volume is set by a modulator. Instead, voice velocity is applied in RT_PATCH_play_note and RT_PATCH_change_velocity.
  //

  const struct PatchVoice &voice = patch->voices[note.voicenum];
  note.velocity *= get_voice_velocity(voice);
  */
  
  SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;

  if(plugin==NULL || plugin->type->set_note_volume == NULL)
    return;

  plugin->type->set_note_volume(plugin, PLAYER_get_block_delta_time(seqtrack, time), note);
}

static int64_t RT_scheduled_send_note_volume_to_plugin(struct SeqTrack *seqtrack, int64_t time, union SuperType *args){
  struct Patch *patch = (struct Patch*)args[0].pointer;

  const note_t note = create_note_from_args(&args[1]);

  set_note_volume_now(seqtrack, patch, note, time);

  return DONT_RESCHEDULE;
}

static void AUDIO_changevelocity(struct SeqTrack *seqtrack, struct Patch *patch,note_t note,STime time){
  SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;

  if(plugin==NULL || plugin->type->set_note_volume == NULL)
    return;
  
  const int latency = RT_SP_get_input_latency(plugin->sp);

  if (latency == 0) {
    set_note_volume_now(seqtrack, patch, note, time);
    return;
  }

  time += ((double)latency * get_note_reltempo(note));

  union SuperType args[8];
  args[0].pointer = patch;
  put_note_into_args(&args[1], note);
  
  SCHEDULER_add_event(seqtrack, time, RT_scheduled_send_note_volume_to_plugin, &args[0], 8, SCHEDULER_VELOCITY_PRIORITY);
}

// pitch

static int64_t RT_scheduled_send_note_pitch_to_plugin(struct SeqTrack *seqtrack, int64_t time, union SuperType *args){
  struct Patch *patch = (struct Patch*)args[0].pointer;

  SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;

  if(plugin==NULL || plugin->type->set_note_pitch == NULL){
    R_ASSERT_NON_RELEASE(plugin==NULL); // we should have already checked plugin->type->set_note_pitch == NULL before calling this function.
    return DONT_RESCHEDULE;
  }

  const note_t note = create_note_from_args(&args[1]);

  plugin->type->set_note_pitch(plugin, PLAYER_get_block_delta_time(seqtrack, time), note);

  return DONT_RESCHEDULE;
}

static void AUDIO_changepitch(struct SeqTrack *seqtrack, struct Patch *patch,note_t note,STime time){
  SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;

  if(plugin==NULL || plugin->type->set_note_pitch == NULL)
    return;
      
  RT_PLUGIN_touch(plugin);
    
  const int latency = RT_SP_get_input_latency(plugin->sp);

  if (latency == 0) {
    plugin->type->set_note_pitch(plugin, PLAYER_get_block_delta_time(seqtrack, time), note); 
    return;
  }

  time += ((double)latency * get_note_reltempo(note));

  union SuperType args[8];
  args[0].pointer = patch;
  put_note_into_args(&args[1], note);
  
  SCHEDULER_add_event(seqtrack, time, RT_scheduled_send_note_pitch_to_plugin, &args[0], 8, SCHEDULER_PITCH_PRIORITY);
}


// pan

static int64_t RT_scheduled_send_note_pan_to_plugin(struct SeqTrack *seqtrack, int64_t time, union SuperType *args){
  struct Patch *patch = (struct Patch*)args[0].pointer;

  SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;

  if(plugin==NULL || plugin->type->set_note_volume == NULL)
    return DONT_RESCHEDULE;

  const note_t note = create_note_from_args(&args[1]);

  plugin->type->set_note_pan(plugin, PLAYER_get_block_delta_time(seqtrack, time), note);

  return DONT_RESCHEDULE;
}

static void AUDIO_changepan(struct SeqTrack *seqtrack, struct Patch *patch,note_t note,STime time){
  SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;

  if(plugin==NULL || plugin->type->set_note_pan == NULL)
    return;

  RT_PLUGIN_touch(plugin);
    
  const int latency = RT_SP_get_input_latency(plugin->sp);

  if (latency == 0) {
    plugin->type->set_note_pan(plugin, PLAYER_get_block_delta_time(seqtrack, time), note); 
    return;
  }

  time += ((double)latency * get_note_reltempo(note));

  union SuperType args[8];
  args[0].pointer = patch;
  put_note_into_args(&args[1], note);
  
  SCHEDULER_add_event(seqtrack, time, RT_scheduled_send_note_pan_to_plugin, &args[0], 8, SCHEDULER_PAN_PRIORITY);
}


// raw midi

static int64_t RT_scheduled_send_raw_midi_to_plugin(struct SeqTrack *seqtrack, int64_t time, union SuperType *args){
  struct Patch *patch = (struct Patch*)args[0].pointer;
  
  SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;

  if(plugin==NULL || plugin->type->send_raw_midi_message == NULL)
    return DONT_RESCHEDULE;

  uint32_t msg = args[1].uint32_num;
  
  plugin->type->send_raw_midi_message(plugin, PLAYER_get_block_delta_time(seqtrack, time), msg);

  return DONT_RESCHEDULE;
}

static void AUDIO_sendrawmidimessage(struct SeqTrack *seqtrack, struct Patch *patch, uint32_t msg, STime time, double block_reltempo){
  SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;

  if(plugin==NULL || plugin->type->send_raw_midi_message == NULL)
    return;
      
  RT_PLUGIN_touch(plugin);
    
  const int latency = RT_SP_get_input_latency(plugin->sp);

  if (latency == 0) {
    plugin->type->send_raw_midi_message(plugin, PLAYER_get_block_delta_time(seqtrack, time), msg); 
    return;
  }

  time += ((double)latency * block_reltempo);

  union SuperType args[2];
  args[0].pointer = patch;
  args[1].uint32_num = msg;
  
  SCHEDULER_add_event(seqtrack, time, RT_scheduled_send_raw_midi_to_plugin, &args[0], 2, SCHEDULER_RAWMIDIMESSAGE_PRIORITY);
}

static int64_t RT_scheduled_send_stop_note_to_plugin(struct SeqTrack *seqtrack, int64_t time, union SuperType *args){
  struct Patch *patch = (struct Patch*)args[0].pointer;

  SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;

  if(plugin==NULL || plugin->type->stop_note == NULL)
    return DONT_RESCHEDULE;

  const note_t note = create_note_from_args(&args[1]);

  Patch_removePlayingVoice(&plugin->playing_voices, note.id, seqtrack, note.seqblock);
  plugin->type->stop_note(plugin, PLAYER_get_block_delta_time(seqtrack, time), note);

  return DONT_RESCHEDULE;
}

static void AUDIO_stopnote(struct SeqTrack *seqtrack, struct Patch *patch,note_t note,STime time){
  SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;

  if(plugin==NULL || plugin->type->stop_note == NULL)
    return;

  RT_PLUGIN_touch(plugin);
    
  const int latency = RT_SP_get_input_latency(plugin->sp);

  //printf("  stopnote called. %d, time: %d\n",(int)note.id, (int)time);
  if (latency == 0 || time==-1) {
    Patch_removePlayingVoice(&plugin->playing_voices, note.id, seqtrack, note.seqblock);
    plugin->type->stop_note(plugin, PLAYER_get_block_delta_time(seqtrack, time), note);
    return;
  }

  time += ((double)latency * get_note_reltempo(note));

  union SuperType args[8];
  args[0].pointer = patch;
  put_note_into_args(&args[1], note);
  
  SCHEDULER_add_event(seqtrack, time, RT_scheduled_send_stop_note_to_plugin, &args[0], 8, SCHEDULER_NOTE_OFF_PRIORITY);
}

void AUDIO_stop_all_notes(struct Patch *patch){
  R_ASSERT_RETURN_IF_FALSE(patch->instrument==get_audio_instrument());
  
  SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;

  if(plugin==NULL || plugin->type->stop_note == NULL)
    return;

  if (plugin->playing_voices != NULL) {
    
    PLUGIN_touch(plugin);
    
    while(plugin->playing_voices != NULL) {

      note_t note = plugin->playing_voices->note;
      struct SeqTrack *seqtrack = plugin->playing_voices->seqtrack;
      
      Patch_removePlayingVoice(&plugin->playing_voices, note.id, seqtrack, note.seqblock);
      plugin->type->stop_note(plugin, 0, note);
    }

  }

}


static void AUDIO_closePatch(struct Patch *patch){
}

#if 0
static float scale(float x, float x1, float x2, float y1, float y2){
  return y1 + ( ((x-x1)*(y2-y1))
                /
                (x2-x1)
                );
}
#endif

static void AUDIO_changeTrackPan(int newpan,const struct Tracks *track){
  struct Patch *patch = track->patch;

  //printf("Changing track pan. Val: %d. patc: %p, plugin: %p\n",newpan,patch,patch->patchdata);

  if(patch==NULL){ // This really cant happen.
    RError("What?");
    return;
  }

  if(patch->instrument!=get_audio_instrument())
    return;
  
#if 0 // It's up to the plugin whether it wants to use the track pan. Seems like only the sample player uses it... It's difficult to use since many tracks can share the same instrument.
  SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;

  if(plugin==NULL)
    return;

  PLUGIN_set_effect_value(plugin, 
                          -1, 
                          plugin->type->num_effects + EFFNUM_PAN,
                          scale(newpan, -MAXTRACKPAN,MAXTRACKPAN, 0.0,1.0),
                          PLUGIN_NONSTORED_TYPE,
                          PLUGIN_DONT_STORE_VALUE);
  //GFX_update_instrument_widget(patch);
#endif
}

// Must only be called from AUDIO_InitPatch and disk_patches.c
void AUDIO_set_patch_attributes(struct Patch *patch, void *patchdata) {
  patch->playnote       = AUDIO_playnote;
  patch->stopnote       = AUDIO_stopnote;
  patch->changevelocity = AUDIO_changevelocity;
  patch->changepitch    = AUDIO_changepitch;
  patch->changepan    = AUDIO_changepan;
  patch->sendrawmidimessage = AUDIO_sendrawmidimessage;
  patch->closePatch     = AUDIO_closePatch;
  patch->changeTrackPan = AUDIO_changeTrackPan;

  //R_ASSERT(patchdata!=NULL);
  patch->patchdata = patchdata;

  patch->instrument=get_audio_instrument();
}

static void AUDIO_set_permanent_id(struct Patch *patch, struct SoundPlugin *plugin){
  R_ASSERT_RETURN_IF_FALSE(plugin!=NULL);
  
  if (plugin == get_main_pipe())
    patch->permanent_id = PERMANENT_PATCH_ID_MAIN_PIPE;
  else if (MIXER_get_buses().bus1 != NULL && plugin == SP_get_plugin(MIXER_get_buses().bus1))
    patch->permanent_id = PERMANENT_PATCH_ID_BUS1;
  else if (MIXER_get_buses().bus2 != NULL && plugin == SP_get_plugin(MIXER_get_buses().bus2))
    patch->permanent_id = PERMANENT_PATCH_ID_BUS2;
  else if (MIXER_get_buses().bus3 != NULL && plugin == SP_get_plugin(MIXER_get_buses().bus3))
    patch->permanent_id = PERMANENT_PATCH_ID_BUS2;
  else if (MIXER_get_buses().bus4 != NULL && plugin == SP_get_plugin(MIXER_get_buses().bus4))
    patch->permanent_id = PERMANENT_PATCH_ID_BUS2;
  else if (MIXER_get_buses().bus5 != NULL && plugin == SP_get_plugin(MIXER_get_buses().bus5))
    patch->permanent_id = PERMANENT_PATCH_ID_BUS2;
  else
    patch->permanent_id = 0;
}

void AUDIO_update_all_permanent_ids(void){
  VECTOR_FOR_EACH(struct Patch *, patch, &get_audio_instrument()->patches){
    AUDIO_set_permanent_id(patch, (struct SoundPlugin*)patch->patchdata);
  }END_VECTOR_FOR_EACH;
}

// this is quite flaky.
static bool state_only_contains_plugin(hash_t *state){
  if (HASH_get_num_elements(state) != 4)
    return true;

  if (!HASH_has_key(state, "patch"))
    return true;

  if (!HASH_has_key(state, "plugin"))
    return true;

  if (!HASH_has_key(state, "x"))
    return true;

  if (!HASH_has_key(state, "y"))
    return true;

  return false;
}

// x and y are ignored if audio_state!=NULL (since audio state has its own "x" and "y")
// The function can not return false if is_loading_song==true.
bool AUDIO_InitPatch2(struct Patch *patch, const char *type_name, const char *plugin_name, hash_t *audio_state, bool is_loading_song, bool set_as_current, float x, float y) {
  printf("AUDIO_InitPatch2 called\n");

  R_ASSERT_RETURN_IF_FALSE2(patch->instrument==get_audio_instrument(), false);
      
  SoundPluginType *type = NULL;
  struct SoundPlugin *plugin = NULL;

  bool state_only_has_plugin = audio_state!=NULL && state_only_contains_plugin(audio_state);

  if (audio_state!=NULL){
    
    R_ASSERT(type_name==NULL);
    R_ASSERT(plugin_name==NULL);

    hash_t *plugin_state;

    if (state_only_has_plugin)
      plugin_state = audio_state;
    else
      plugin_state = HASH_get_hash(audio_state, "plugin");
  
    plugin = PLUGIN_create_from_state(patch, plugin_state, is_loading_song);

    if (plugin!=NULL)
      type = plugin->type;
    
  } else {

    R_ASSERT_RETURN_IF_FALSE2(type_name!=NULL, false);
    R_ASSERT_RETURN_IF_FALSE2(plugin_name!=NULL, false);
    
    type = PR_get_plugin_type_by_name(NULL, type_name, plugin_name);
    if (type==NULL){
      if (is_loading_song==false){
        GFX_Message(NULL, "Audio plugin %s / %s not found", type_name, plugin_name);
        return false;
      } else {
        GFX_Message(NULL, "Audio plugin %s / %s not found. Replacing \"%s\" with a pipe.", type_name, plugin_name,  patch->name);
        return AUDIO_InitPatch2(patch, "Pipe", "Pipe", NULL, true, set_as_current, x, y);
      }
    }

    plugin = PLUGIN_create(patch, type, NULL, is_loading_song);

  }

  if (plugin==NULL){
    if (is_loading_song==false)
      return false;
    else{
      
      if (type_name==NULL || plugin_name==NULL)
        GFX_Message(NULL, "Unable to load \"%s\". Replacing with a pipe.", patch->name);
      else
        GFX_Message(NULL, "Unable to load Audio plugin %s / %s. Replacing \"%s\" with a pipe.", type_name, plugin_name,  patch->name);

      return AUDIO_InitPatch2(patch, "Pipe", "Pipe", NULL, true, set_as_current, x, y);
    }
  }

#if 0
  // Add this check here (and not at the beginning of the function) since type_name is NULL when creating from state.
  if (false==is_loading_song && !g_is_loading){
    if (!strcmp(plugin->type->type_name,"Bus")){
      GFX_Message(NULL, "Only 5 buses are supported. If you need more than 5 buses, please make a feature request.");
      PLUGIN_delete(plugin);
      return false;
    }
  }
#endif
  
  bool needs_name = patch->name==NULL || (strlen(patch->name)==0);

  if (needs_name){
    const char *name = PLUGIN_generate_new_patchname(plugin);
    PATCH_set_name(patch, name);
  }

  AUDIO_set_patch_attributes(patch, plugin);
  AUDIO_set_permanent_id(patch, plugin);

  struct SoundProducer *sound_producer = SP_create(plugin, MIXER_get_buses());
  R_ASSERT_RETURN_IF_FALSE2(sound_producer!=NULL, false);

  if (audio_state != NULL && !state_only_has_plugin) {
    x = HASH_get_float(audio_state, "x");
    y = HASH_get_float(audio_state, "y");
  }

  //printf("x: %f, y: %f\n",x,y);
  //getchar();

  // Create mixer object
  CHIP_create(sound_producer, x, y);
  //MW_move_chip_to_slot(patch, x, y);
  MW_cleanup_chip_positions();

  // Create instrument widget
  InstrumentWidget_create_audio_instrument_widget(patch, is_loading_song ? false : set_as_current);
  
  patch->is_usable = true;

  if (!is_loading_song)
    PLUGIN_DLoad(plugin);
  
  return true;
}



/* Audio Instrument */
#if 0
typedef struct{
  int effect_num;
} AUDIO_FX_data_t;
#endif

#if 0
static int AUDIO_getMaxVelocity(const struct Patch *patch){
  return MAX_FX_VAL;
}
#endif

static void AUDIO_close_FX(struct FX *fx,const struct Tracks *track){
  //struct Patch *patch = track->patch;

  // Commented out. May hold player lock.
  // printf("AUDIO_close_FX called for track %d\n",track->l.num);
  
  //SoundPlugin *plugin = (SoundPlugin*) track->patch->patchdata;
  //AUDIO_FX_data_t *fxdata = (AUDIO_FX_data_t*)fx->fxdata;

  //OS_SLIDER_release_automation_pointers(patch,fx->effect_num);
}

static float get_effect_val_from_fx_val(int fxval){
  return (double)fxval / (double)MAX_FX_VAL;
}
                                    
static void send_fx_to_plugin(struct SeqTrack *seqtrack, SoundPlugin *plugin, STime time, FX_when when, int val, int effect_num){
  R_ASSERT_NON_RELEASE(FX_when_is_automation(when));
  
  //printf("send_fx_to_plugin %s. effect_num: %d, effect_value: %f. Time: %d. When: %d\n",plugin->patch->name, effect_num, get_effect_val_from_fx_val(val), (int)time, (int)when);
  
  PLUGIN_set_effect_value(plugin,
                          PLAYER_get_block_delta_time(seqtrack, time),
                          effect_num,
                          get_effect_val_from_fx_val(val),
                          DONT_STORE_VALUE,
                          when,
                          EFFECT_FORMAT_SCALED
                          );
}

static int64_t RT_scheduled_send_fx_to_plugin(struct SeqTrack *seqtrack, int64_t time, union SuperType *args){
  struct Patch *patch = (struct Patch*)args[0].pointer;

  SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;

  if(plugin==NULL)
    return DONT_RESCHEDULE;

  FX_when when = (FX_when)args[1].int_num;
  int val = (int)args[2].int_num;
  int effect_num = (int)args[3].int_num;

  send_fx_to_plugin(seqtrack, plugin, time, when, val, effect_num);

  return DONT_RESCHEDULE;
}


static void AUDIO_treat_FX(struct SeqTrack *seqtrack, struct FX *fx,int val,STime time,int skip, FX_when when, double block_reltempo){
  struct Patch *patch = fx->patch;
  
  R_ASSERT_RETURN_IF_FALSE(patch->instrument==get_audio_instrument());
          
  SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;
  //AUDIO_FX_data_t *fxdata = (AUDIO_FX_data_t*)fx->fxdata;
  if (plugin==NULL) // i.e. plugin has been deleted and removed from the patch.
    return;

  int effect_num = fx->effect_num;

  if (effect_num >= plugin->type->num_effects + NUM_SYSTEM_EFFECTS){
#if !defined(RELEASE)
    RWarning("DEBUG MODE: effect_num >= plugin->type->num_effects: %d >= %d", effect_num, plugin->type->num_effects);
#endif
    return;
  }

  RT_PLUGIN_touch(plugin);

  const int latency = RT_SP_get_input_latency(plugin->sp);

  if (latency<2 && fabs(seqtrack->start_time-time)<2 && when != FX_start) { // Note: FX_start is always scheduled to ensure that FX_end for the same fx is always sent out before FX_start.
    
    send_fx_to_plugin(seqtrack, plugin, time, when, val, effect_num);
    
  } else {

    time += ((double)latency * block_reltempo);

    union SuperType args[4];
    args[0].pointer = patch;
    args[1].int_num = when;
    args[2].int_num = val;
    args[3].int_num = effect_num;
    
    //printf("   Scheduling FX. Time: %d (latency: %d). block_reltempo: %f. Time: %d. When: %d\n", (int)time, latency, block_reltempo, (int)time, (int)when);
    SCHEDULER_add_event(seqtrack, time, RT_scheduled_send_fx_to_plugin, &args[0], 4, when==FX_end ? SCHEDULER_FX_END_PRIORITY : SCHEDULER_FX_PRIORITY);
  }
}

static void AUDIO_FX_call_me_before_starting_to_play_song_MIDDLE(struct FX *fx, int val, int64_t abstime, FX_when when){
  struct Patch *patch = fx->patch;
  
  R_ASSERT_RETURN_IF_FALSE(patch->instrument==get_audio_instrument());
          
  SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;
  //AUDIO_FX_data_t *fxdata = (AUDIO_FX_data_t*)fx->fxdata;
  if (plugin==NULL){ // i.e. plugin has been deleted and removed from the patch.
    R_ASSERT_NON_RELEASE(false); // probably not supposed to happen here though.
    return;
  }

  int effect_num = fx->effect_num;

  if (effect_num >= plugin->type->num_effects + NUM_SYSTEM_EFFECTS){
#if !defined(RELEASE)
    RWarning("DEBUG MODE: effect_num >= plugin->type->num_effects: %d >= %d", effect_num, plugin->type->num_effects + NUM_SYSTEM_EFFECTS);
#endif
    return;
  }

  PLUGIN_call_me_before_starting_to_play_song_MIDDLE(plugin, abstime, effect_num, get_effect_val_from_fx_val(val), when, EFFECT_FORMAT_SCALED);
}

// NOT called from RT thread
static int AUDIO_default_FX_value(const struct FX *fx){
  struct Patch *patch = fx->patch;
  
  R_ASSERT_RETURN_IF_FALSE2(patch->instrument==get_audio_instrument(), fx->min);
          
  SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;
  //AUDIO_FX_data_t *fxdata = (AUDIO_FX_data_t*)fx->fxdata;
  if (plugin==NULL) // i.e. plugin has been deleted and removed from the patch.
    return (fx->min+fx->max)/2;

  PLUGIN_touch(plugin);
      
  return PLUGIN_get_effect_value(plugin, fx->effect_num, VALUE_FROM_PLUGIN) * MAX_FX_VAL;
}

#if 0
static void AUDIO_set_FX_string(struct FX *fx,int val,const struct Tracks *track,char *string){
  SoundPlugin *plugin = (SoundPlugin*) track->patch->patchdata;
  //AUDIO_FX_data_t *fxdata = (AUDIO_FX_data_t*)fx->fxdata;

  float effect_val = val / (float)MAX_FX_VAL;

  PLUGIN_get_display_value_string(plugin, fx->effect_num, effect_val, string, 512);
}
#endif

static void AUDIO_save_FX(struct FX *fx,const struct Tracks *track);
static void *AUDIO_LoadFX(struct FX *fx,const struct Tracks *track);

static void init_fx(struct FX *fx, int effect_num, const char *name, struct SoundPlugin *plugin){

  //AUDIO_FX_data_t *fxdata = talloc_atomic(sizeof(AUDIO_FX_data_t));
  fx->effect_num      = effect_num;
  //fx->fxdata          = fxdata;

  fx->name    = (const char*)talloc_strdup(name);
  fx->min     = 0;
  fx->max     = MAX_FX_VAL;
  fx->is_enabled = true;
  fx->closeFX = AUDIO_close_FX;
  fx->SaveFX  = AUDIO_save_FX;
  fx->treatFX = AUDIO_treat_FX;
  fx->call_me_before_starting_to_play_song_MIDDLE = AUDIO_FX_call_me_before_starting_to_play_song_MIDDLE;
  fx->defaultFXValue = AUDIO_default_FX_value;
  //fx->setFXstring = AUDIO_set_FX_string;

  if (plugin==NULL)
    fx->color = AUTOMATION1_COLOR_NUM; // Hapens when loading song. plugin is not available yet, so the color will be set later.
  else
    fx->color = get_effect_color(plugin, effect_num);
}

static int AUDIO_getNumFxs(const struct Patch *patch){
  SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;
  if (plugin==NULL){
    R_ASSERT_NON_RELEASE(false);
    return 0;
  }
  
  const SoundPluginType *plugin_type = plugin->type;
  return plugin_type->num_effects+NUM_SYSTEM_EFFECTS;
}

static const char *AUDIO_getFxName(const struct Patch *patch, int fxnum){
  SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;
  if (plugin==NULL){
    R_ASSERT_NON_RELEASE(false);
    return "";
  }
  
  const SoundPluginType *plugin_type = plugin->type;

  //PLUGIN_touch(plugin);
  
  int num_effects = plugin_type->num_effects+NUM_SYSTEM_EFFECTS;

  R_ASSERT_RETURN_IF_FALSE2(fxnum>=0 && fxnum < num_effects, "");

  return PLUGIN_get_effect_name(plugin, fxnum);
}

static struct FX *AUDIO_createFX(const struct Tracks *track, struct Patch *patch, int effect_num){
#ifndef RELEASE
  R_ASSERT(track->patch != NULL);
#endif
  
  struct FX *fx=(struct FX *)talloc(sizeof(struct FX));
  fx->patch = patch;

  SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;
  if (plugin==NULL){
    R_ASSERT_NON_RELEASE(false);
    return fx;
  }

  PLUGIN_touch(plugin);
  
  const char *name = PLUGIN_get_effect_name(plugin, effect_num);

  init_fx(fx,effect_num,name,plugin);

  return fx;
}

struct PatchEffect{
  struct Patch *patch;
  int effect_num;
  QString name;
  
  PatchEffect(struct Patch *patch, int effect_num, const char *name)
    : patch(patch)
    , effect_num(effect_num)
    , name(name)
  {}

  PatchEffect()
    : PatchEffect(NULL, 0, NULL)
  {}
};

static void add_patch_effects_to_menu2(vector_t *menu, QVector<PatchEffect> &patch_effects, const char *name, struct Patch *patch = NULL, int effect_num = 0){
  VECTOR_push_back(menu,name);
  patch_effects.push_back(PatchEffect(patch, effect_num, name));
}

static void add_patch_effects_to_menu(vector_t *menu, QVector<PatchEffect> &patch_effects, struct Patch *patch){
  SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;
  if (plugin==NULL) return;
  
  const SoundPluginType *plugin_type = plugin->type;
  //int num_effects = plugin_type->num_effects+NUM_SYSTEM_EFFECTS;
    
  for(int i=0;i<EFFNUM_VOICE1_ONOFF;i++) {
    if (i>0 && i==plugin_type->num_effects)      
      add_patch_effects_to_menu2(menu, patch_effects, "----------System effects");
    
    const char *name = PLUGIN_get_effect_name(plugin, i);
    if (strncmp(name, NOTUSED_EFFECT_NAME, strlen(NOTUSED_EFFECT_NAME)))
      add_patch_effects_to_menu2(menu, patch_effects, name, patch, i);
  }

  add_patch_effects_to_menu2(menu, patch_effects, "----------Note duplicator effects");
  
  const int num_voice_effects = (NUM_SYSTEM_EFFECTS-EFFNUM_VOICE1_ONOFF) / NUM_PATCH_VOICES; // = 2
      
  for(int voicenum=0;voicenum<NUM_PATCH_VOICES;voicenum++){

    add_patch_effects_to_menu2(menu, patch_effects, talloc_format("[submenu start]Voice %d",voicenum+1));
    
    for(int i=0;i<num_voice_effects;i++){

        /*
          voicenum: 0,1,2,3,4,5,6,7
          i: 0,1
          effect_num: 0, 7, 1, 8, 2, 9, 3, 10
          effect_num = i*7 + voicenum
          effect_num(0) = 0 + 0 = 0
          effect_num(1) = 7 + 0 = 7
          effect_num(2) = 0 + 1 = 1
          effect_num(3) = 7 + 1 = 8
          effect_num(4) = 0 + 2 = 2
          effect_num(5) = 7 + 2 = 9
        */
      
      int effect_num = plugin_type->num_effects + EFFNUM_VOICE1_ONOFF + i*NUM_PATCH_VOICES + voicenum;
    
      const char *name = PLUGIN_get_effect_name(plugin, effect_num);
      if (strncmp(name, NOTUSED_EFFECT_NAME, strlen(NOTUSED_EFFECT_NAME)))
        add_patch_effects_to_menu2(menu, patch_effects, name, patch, effect_num);
    }

    add_patch_effects_to_menu2(menu, patch_effects, "[submenu end]");
  }
}

static void AUDIO_getFX(struct Tracker_Windows *window,const struct Tracks *track, std::function<void(struct FX*)> callback){
  //const char *menutitle="Select FX";

  vector_t v = {};
  QVector<PatchEffect> patch_effects;
  

#if 1 // Enable selecting fx from other instruments

  vector_t all_patches = get_audio_instrument()->patches;

  if (all_patches.num_elements > 1){

    add_patch_effects_to_menu2(&v, patch_effects, "[submenu start]Other instruments");
    
    VECTOR_FOR_EACH(struct Patch *, patch,&all_patches){

      if (patch != track->patch){
        
        add_patch_effects_to_menu2(&v, patch_effects, talloc_format("[submenu start]%s",patch->name));
                                   
        add_patch_effects_to_menu(&v, patch_effects, patch);

        add_patch_effects_to_menu2(&v, patch_effects, "[submenu end]");
      }

    }END_VECTOR_FOR_EACH;

    add_patch_effects_to_menu2(&v, patch_effects, "[submenu end]");
  }

  add_patch_effects_to_menu2(&v, patch_effects, "---------Instrument effects");

#else

  {
    SoundPlugin *plugin = (SoundPlugin*) track->patch->patchdata;
    const SoundPluginType *plugin_type = plugin->type;
    int num_effects = plugin_type->num_effects+NUM_SYSTEM_EFFECTS;
    
    PLUGIN_touch(plugin);
      
    if(num_effects==0){
      VECTOR_push_back(&v,"No effects available");
      GFX_Menu(window,NULL,"No FX available",v,true);
      return FX_FAILED;
    }
  }

#endif
  
  add_patch_effects_to_menu(&v, patch_effects, track->patch);

  GFX_Menu3(v,[patch_effects, callback](int selection, bool onoff){
      
      if(-1==selection)
        return;
      
      if(selection<0 || selection>=patch_effects.size()){
        R_ASSERT(false);
        return;
      }
      
      const PatchEffect &pe = patch_effects.at(selection);

      R_ASSERT(pe.patch != NULL);

      if (pe.patch != NULL && pe.patch->patchdata != NULL){
        struct FX *fx = (struct FX*)talloc(sizeof(struct FX));
        
        fx->patch = pe.patch;
        
        init_fx(fx,pe.effect_num, pe.name.toUtf8().constData(), (struct SoundPlugin*)(pe.patch->patchdata));
        
        return callback(fx);
      }

    });
}

static void AUDIO_save_FX(struct FX *fx,const struct Tracks *track){
  //printf("AUDIO_save_FX called for track %d\n",track->l.num);

  DC_start("FXDATA");{

    DC_SSI("num",fx->effect_num); // Why save the same number a FOURTH time?
    DC_SSS("name",fx->name);

  }DC_end();
}

static void *AUDIO_LoadFX(struct FX *fx,const struct Tracks *track){
  const char **objs=NULL;
  const char *vars[2]={"num","name"};

  init_fx(fx,0,"Effect name was not set in file (\?\?\?)",NULL);

  GENERAL_LOAD(0,2)

var0:
	fx->effect_num = DC_LoadI(); // the effect num may change later, if the plugin has implemented get_effect_num, and it returns a different value.
	goto start;

var1:
        fx->name = DC_LoadSNoMatterWhat();
	goto start;

var2:
var3:
var4:
var5:
var6:
var7:
var8:
var9:
var10:
var11:
var12:
var13:
var14:
var15:
var16:
var17:
var18:
var19:
 var20:
 var21:
        
obj0:
obj1:
obj2:
obj3:
obj4:
obj5:
obj6:

error:
end:
        return NULL;
}

bool AUDIO_maybe_warn_about_automating_bus_onoff(const struct Patch *patch, const char *effect_name, const char *what, const char *info){
  R_ASSERT_RETURN_IF_FALSE2(patch!=NULL, false);
  R_ASSERT_RETURN_IF_FALSE2(patch->instrument==get_audio_instrument(), false);
  R_ASSERT_RETURN_IF_FALSE2(patch->patchdata!=NULL, false);

  bool gotit
    =  !strcmp(effect_name, "System Reverb On/Off")
    || !strcmp(effect_name, "System Chorus On/Off")
    || !strcmp(effect_name, "System Aux 1 On/Off")
    || !strcmp(effect_name, "System Aux 2 On/Off")
    || !strcmp(effect_name, "System Aux 3 On/Off");

  if (gotit) {
    GFX_addMessage("Note: The effect \"%s\" has been removed from Radium.<br>"
                   "The %s of this effect for the instrument named \"%s\"%s could not be loaded.",
                   effect_name,
                   what,
                   patch->name,
                   info);
    /*
    GFX_addMessage("Note: This song is %s \"%s\" for the instrument \"%s\"%s. "
                   "You might have to manually enable the send connection for the bus, for the automation to have effect. (This is only an issue when playing older songs.)",
                   what,
                   PLUGIN_get_effect_name(plugin, effect_num),
                   patch->name,
                   info);                   
    */
    return true;
  }

  return false;
}

void DLoadAudioInstrument(struct Root *newroot, struct Song *song){
  struct Blocks *block = song->blocks;
  while(block!=NULL){
    struct Tracks *track = block->tracks;
    while(track!=NULL){
      struct Patch *patch = track->patch;

      if (patch!=NULL && patch->instrument == get_audio_instrument()) {
        
        QVector<struct FXs*> to_remove;
        
        VECTOR_FOR_EACH(struct FXs *, fxs, &track->fxs){
          struct FX *fx = fxs->fx;
          struct Patch *fx_patch = fx->patch;
          
          SoundPlugin *plugin = (SoundPlugin *)fx_patch->patchdata;
          
          if(plugin!=NULL){

            int load_version = g_disk_load_radium_version_major*10000 + g_disk_load_radium_version_minor*100 + g_disk_load_radium_version_revision;
            
            if (!strcmp(fx->name, "Crossfade") && !strcmp("Sample Player", plugin->type->type_name) && load_version < 60974) {
              GFX_addMessage("Warning: This song automaties the Crossfade effect in a song saved with an earlier version of Radium that had a linear mapping between the slider and the crossfade value. You might need to adjust the automation:<br>"
                             "<UL>"
                             "<LI>Name of Sampler instrument: \"%s\""
                             "<LI>Block: %d: %s."
                             "<LI>Track: %d"
                             "</UL>",
                             patch->name,
                             block->l.num, block->name,
                             track->l.num);
            }
            
            if (AUDIO_maybe_warn_about_automating_bus_onoff(fx->patch, fx->name, "automation", talloc_format(" in block %d, track %d", block->l.num, track->l.num))){
              
              to_remove.push_back(fxs);
              
            } else {

              fx->name = PLUGIN_get_new_name_if_name_has_changed(plugin, fx->name);
              
              int effect_num = PLUGIN_get_effect_num(plugin, fx->name, NULL);
              if (effect_num >= 0){
                
                fx->effect_num = effect_num;
                
              }
            
              fx->color = get_effect_color(plugin, fx->effect_num);

            }
                          
          }
          
        }END_VECTOR_FOR_EACH;

        for(struct FXs *fxs : to_remove){
          VECTOR_remove(&track->fxs, fxs);
        }
      }
      
      track = NextTrack(track);
    }
    block = NextBlock(block);
  }

  vector_t patches = get_audio_instrument()->patches;
  VECTOR_FOR_EACH(struct Patch *, patch, &patches){
    struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
    R_ASSERT(plugin!=NULL);
    PLUGIN_DLoad(plugin);
  }END_VECTOR_FOR_EACH;
}

/*
static int AUDIO_getPatch(struct Tracker_Windows *window,ReqType reqtype,const struct Tracks *track,struct Patch *patch){
  return PATCH_SUCCESS;
}
*/

static void AUDIO_CloseInstrument(struct Instruments *instrument){}
//static void AUDIO_InitTrack(struct Instruments *instrument,const struct Tracks *track){}
static void AUDIO_StopPlaying(struct Instruments *instrument){
  R_ASSERT(PLAYER_current_thread_has_lock());
  
  VECTOR_FOR_EACH(struct Patch *, patch, &instrument->patches){
    SoundPlugin *plugin = (SoundPlugin *)patch->patchdata;
    if (plugin!=NULL && plugin->type->player_is_stopped != NULL){
      PLAYER_maybe_pause_lock_a_little_bit(iterator666);
      plugin->type->player_is_stopped(plugin);
    }
  }END_VECTOR_FOR_EACH;
}

static void AUDIO_RT_StopPlaying(struct Instruments *instrument){
  R_ASSERT(PLAYER_current_thread_has_lock());
  
  VECTOR_FOR_EACH(struct Patch *, patch, &instrument->patches){
    
    SoundPlugin *plugin = (SoundPlugin *)patch->patchdata;
    if (plugin!=NULL && plugin->type->RT_player_is_stopped != NULL)
      plugin->type->RT_player_is_stopped(plugin);
    
  }END_VECTOR_FOR_EACH;
}

static void AUDIO_PP_Update(struct Instruments *instrument,struct Patch *patch, bool is_loading){
  if(patch->patchdata==NULL){
    RError("plugin==NULL for %s\n",patch->name);
    return;
  }

  GFX_PP_Update(patch, is_loading);
}

static void *AUDIO_CopyInstrumentData(const struct Tracks *track){
  return NULL;
}

static void AUDIO_PlaySongHook(struct Instruments *instrument, int64_t abstime){
  VECTOR_FOR_EACH(struct Patch *, patch,&instrument->patches){

    struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
    
    if (plugin==NULL) {
      
      R_ASSERT_NON_RELEASE(false);
      
    } else {

      PLUGIN_call_me_before_starting_to_play_song_START(plugin);
      
    }
    
  }END_VECTOR_FOR_EACH;

  
  SONG_call_me_before_starting_to_play_song(abstime); // calls PLUGIN_call_me_before_starting_to_play_song_MIDDLE (and other things).

  
  VECTOR_FOR_EACH(struct Patch *, patch,&instrument->patches){

    struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
    if (plugin!=NULL)
      PLUGIN_call_me_before_starting_to_play_song_END(plugin);
    
  }END_VECTOR_FOR_EACH;

}

static void handle_fx_when_patch_is_replaced(struct Blocks *block,
                                             struct Tracks *track,
                                             const struct Patch *old_patch,
                                             struct Patch *new_patch,
                                             bool same_instrument_type,
                                             int num_old_effects,
                                             int num_new_effects,
                                             const struct SoundPlugin *new_plugin,
                                             bool *has_paused,
                                             bool add_undo)
{
  bool has_made_undo = false;
  
 again:
  {
    int i;
    for(i=0 ; i < track->fxs.num_elements ; i++){
      struct FXs *fxs = (struct FXs *)track->fxs.elements[i];
      struct FX *fx = fxs->fx;
      
      if (fx->patch == old_patch) {
        
        if ((*has_paused) == false){
          PC_Pause();
          *has_paused = true;
        }

        if (add_undo && has_made_undo==false){
          ADD_UNDO(Track_CurrPos(block->l.num, track->l.num));
          has_made_undo = true;
        }
        
        if (new_patch == NULL) {

          VECTOR_remove(&track->fxs, fxs);
          goto again;
          
        } else if (same_instrument_type) {
          
          fx->patch = new_patch;
          
        } else {
          
          if(fx->effect_num >= num_old_effects){
            fx->effect_num = num_new_effects + (fx->effect_num - num_old_effects);
            fx->color = get_effect_color(new_plugin, fx->effect_num);
            fx->patch = new_patch;
          }else{
            VECTOR_remove(&track->fxs, fxs);
            goto again;
          }
        }
      }
    }
  }
}
  
static void AUDIO_handle_fx_when_a_patch_has_been_replaced(const struct Patch *old_patch, struct Patch *new_patch, struct Blocks *only_check_this_block, struct Tracks *only_check_this_track, bool *has_paused){

  R_ASSERT_RETURN_IF_FALSE(old_patch != NULL);

  const SoundPlugin *old_plugin = (const SoundPlugin*) old_patch->patchdata;
  R_ASSERT_RETURN_IF_FALSE(old_plugin!=NULL);

  const SoundPluginType *old_type = old_plugin->type;
  int num_old_effects = old_type->num_effects;

  const SoundPlugin *new_plugin = new_patch==NULL ? NULL : (const SoundPlugin*) new_patch->patchdata;
  if (new_patch != NULL && new_plugin==NULL){
    R_ASSERT_NON_RELEASE(false);
    return;
  }
  
  const SoundPluginType *new_type = new_patch==NULL ? NULL : new_plugin->type;
  int num_new_effects = new_patch==NULL ? -1 : new_type->num_effects;

  bool same_instrument_type = false;
  
  if(true
     && new_patch != NULL 
     && !strcmp(old_type->type_name, new_type->type_name)
     && !strcmp(old_type->name,      new_type->name)
     )
    same_instrument_type = true;
  

  if (only_check_this_track != NULL) {
    
    handle_fx_when_patch_is_replaced(only_check_this_block, only_check_this_track, old_patch, new_patch, same_instrument_type, num_old_effects, num_new_effects, new_plugin, has_paused, false);
    
  } else {

    FOR_EACH_TRACK(){
      handle_fx_when_patch_is_replaced(block, track, old_patch, new_patch, same_instrument_type, num_old_effects, num_new_effects, new_plugin, has_paused, true);
    }END_FOR_EACH_TRACK;
    
  }

  ValidateCursorPos(root->song->tracker_windows);
}

hash_t *AUDIO_get_audio_patch_state(const struct Patch *patch){
  hash_t *state=HASH_create(4);

  R_ASSERT_RETURN_IF_FALSE2(patch->is_usable, NULL);
  R_ASSERT_RETURN_IF_FALSE2(patch->patchdata != NULL, NULL);
  
  const SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
  
  /**
   *
   * READ THIS!
   *
   * If changing the state format, the 'state_only_contains_plugin' function above must be updated. (test by adding instrument and undo + redo)
   *
  */
#if !defined(RELEASE)
  if (CHIP_get_pos_x(patch) < -80000 || CHIP_get_pos_y(patch) < -80000)
    abort();
#endif
  
  HASH_put_instrument(state, "patch", patch->id);
  HASH_put_float(state, "x", CHIP_get_pos_x(patch));
  HASH_put_float(state, "y", CHIP_get_pos_y(patch));

  R_ASSERT_RETURN_IF_FALSE2(plugin!=NULL, state);

  HASH_put_hash(state, "plugin", PLUGIN_get_state(plugin));

  return state;
}

static void AUDIO_remove_patchdata(struct Patch *patch){
  ADD_UNDO(MixerConnections_CurrPos());
  
  SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;
  struct SoundProducer *sound_producer = plugin->sp;

  ATOMIC_SET(plugin->is_shutting_down, true);
  
  InstrumentWidget_delete(patch);
    
  hash_t *state = PLUGIN_get_state(plugin);
          
  CHIP_delete(patch);

  PLAYER_lock();{
    patch->patchdata = NULL;
    patch->state = state;
    patch->is_usable = false;
  }PLAYER_unlock();

  SP_delete(sound_producer);
  PLUGIN_delete(plugin);

  MW_update_all_chips();
}

static void AUDIO_setPatchData(struct Patch *patch, const char *key, const char *value, bool program_state_is_valid){}
static const char *AUDIO_getPatchData(struct Patch *patch, const char *key){
  return NULL;
}


extern SoundPlugin *g_system_bus;

bool AUDIO_is_permanent_patch(const struct Patch *patch){
  R_ASSERT_RETURN_IF_FALSE2(patch->patchdata!=NULL, true);
  
  const SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;
  if (plugin==NULL)
    return false;

  if(plugin==get_main_pipe())
    return true;

  const SoundProducer *sp = SP_get_sound_producer(plugin);
                        
  Buses buses = MIXER_get_buses();
  if (sp==buses.bus1 || sp==buses.bus2 || sp==buses.bus3 || sp==buses.bus4 || sp==buses.bus5) {
    
    return true;

  /*
  else if (!strcmp(plugin->type->type_name,"Bus")) {
    return true;
  */
  
  /*
  } else if (!strcmp(plugin->type->type_name,SEQTRACKPLUGIN_NAME)){
    printf("NUM samples: %d\n", SEQTRACKPLUGIN_get_num_samples(plugin));
    return false==SEQTRACKPLUGIN_can_be_deleted(plugin);
  */
  
  }else
    return false;
}

static struct Patch *get_patch_for_plugin(SoundPlugin *plugin){
  VECTOR_FOR_EACH(struct Patch *, patch,&get_audio_instrument()->patches){
    if(patch->patchdata==plugin)
      return patch;
  }END_VECTOR_FOR_EACH;
  return NULL;
}

struct Patch *AUDIO_get_the_replacement_for_old_permanent_patch(struct Patch *old_patch){
  R_ASSERT(old_patch->permanent_id != 0);

  if (old_patch->permanent_id==PERMANENT_PATCH_ID_BUS1)
    return get_patch_for_plugin(SP_get_plugin(MIXER_get_buses().bus1));
    
  else if (old_patch->permanent_id==PERMANENT_PATCH_ID_BUS2)
    return get_patch_for_plugin(SP_get_plugin(MIXER_get_buses().bus2));

  else if (old_patch->permanent_id==PERMANENT_PATCH_ID_BUS3)
    return get_patch_for_plugin(SP_get_plugin(MIXER_get_buses().bus3));

  else if (old_patch->permanent_id==PERMANENT_PATCH_ID_BUS4)
    return get_patch_for_plugin(SP_get_plugin(MIXER_get_buses().bus4));

  else if (old_patch->permanent_id==PERMANENT_PATCH_ID_BUS5)
    return get_patch_for_plugin(SP_get_plugin(MIXER_get_buses().bus5));

  else if (old_patch->permanent_id==PERMANENT_PATCH_ID_MAIN_PIPE)
    return get_patch_for_plugin(get_main_pipe());

  else
    RError("Unknown permanent_id: %d", old_patch->permanent_id);
  
  return NULL;
}


int AUDIO_initInstrumentPlugIn(struct Instruments *instrument){
  instrument->instrumentname = "Audio instrument";

  //instrument->getMaxVelocity      = AUDIO_getMaxVelocity;
  instrument->getNumFxs           = AUDIO_getNumFxs;
  instrument->getFxName           = AUDIO_getFxName;
  instrument->createFX            = AUDIO_createFX;
  instrument->getFX               = AUDIO_getFX;
  //instrument->getPatch            = AUDIO_getPatch;
  instrument->CloseInstrument     = AUDIO_CloseInstrument;
  //instrument->InitTrack           = AUDIO_InitTrack;
  instrument->StopPlaying         = AUDIO_StopPlaying;
  instrument->RT_StopPlaying      = AUDIO_RT_StopPlaying;

  instrument->CopyInstrumentData = AUDIO_CopyInstrumentData;
  instrument->PlaySongHook       = AUDIO_PlaySongHook;
  instrument->LoadFX             = AUDIO_LoadFX;

  instrument->PP_Update = AUDIO_PP_Update;

  instrument->handle_fx_when_a_patch_has_been_replaced = AUDIO_handle_fx_when_a_patch_has_been_replaced;
  instrument->remove_patchdata = AUDIO_remove_patchdata;

  instrument->setPatchData = AUDIO_setPatchData;
  instrument->getPatchData = AUDIO_getPatchData;

  return INSTRUMENT_SUCCESS;
}
