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


#ifndef AUDIO_SOUNDPLUGIN_PROC_H
#define AUDIO_SOUNDPLUGIN_PROC_H

#include "Mixer_proc.h"

/*
enum ValueType{
  PLUGIN_STORED_TYPE,
  PLUGIN_NONSTORED_TYPE
};
*/

enum StoreitType{
  STORE_VALUE,
  DONT_STORE_VALUE
};

enum WhereToGetValue{
  VALUE_FROM_PLUGIN,
  VALUE_FROM_STORAGE
};

extern LANGSPEC SoundPlugin *PLUGIN_create(struct Patch *patch, SoundPluginType *plugin_type, hash_t *plugin_state, bool is_loading);
extern LANGSPEC void PLUGIN_delete(SoundPlugin *plugin);
extern LANGSPEC void PLUGIN_update_smooth_values(SoundPlugin *plugin);

extern LANGSPEC int PLUGIN_get_num_visible_effects(SoundPlugin *plugin);

extern LANGSPEC int PLUGIN_get_effect_format(struct SoundPlugin *plugin, int effect_num);
extern LANGSPEC const char *PLUGIN_get_new_name_if_name_has_changed(struct SoundPlugin *plugin, const char *effect_name);
extern LANGSPEC int PLUGIN_get_effect_num(struct SoundPlugin *plugin, const char *effect_name, char **error_message); // if 'error_message' is NULL, the function will display an error message if there is an error.
extern LANGSPEC const char *PLUGIN_get_effect_name(SoundPlugin *plugin, int effect_num);
extern LANGSPEC const char *PLUGIN_get_effect_description(const struct SoundPluginType *plugin_type, int effect_num);
extern LANGSPEC void PLUGIN_get_display_value_string(struct SoundPlugin *plugin, int effect_num, char *buffer, int buffersize);

extern LANGSPEC void PLUGIN_call_me_when_gui_closes(SoundPlugin *plugin); // Only necessary to call if closed on its own, but its safe to call from plugin::type::hide_gui() (although that call won't do anything). Can be called right before, or right after, the GUI actually closes.
extern LANGSPEC bool PLUGIN_gui_is_visible(SoundPlugin *plugin, int64_t parentgui);
extern LANGSPEC void PLUGIN_close_gui(SoundPlugin *plugin);
extern LANGSPEC bool PLUGIN_open_gui(SoundPlugin *plugin, int64_t parentgui);

extern LANGSPEC void PLUGIN_call_me_before_starting_to_play_song_END(SoundPlugin *plugin);
extern LANGSPEC void PLUGIN_call_me_before_starting_to_play_song_MIDDLE(SoundPlugin *plugin, int64_t abstime, int effect_num, float value, FX_when when, enum ValueFormat value_format);
extern LANGSPEC void PLUGIN_call_me_before_starting_to_play_song_START(SoundPlugin *plugin);

extern LANGSPEC void PLUGIN_call_me_very_often_from_main_thread(void);

#ifdef __cplusplus
// Must/should be called from the plugin if it changes value by itself. After initialization that is.
//
// The function must be called even if storeit_type==DONT_STORE_VALUE
//
// Both 'native_value' and 'scaled_value' must be valid.
//
// The function should not be called if it was triggered by a call to plugin->set_effect_value(). (it's not the end of the world, but at least automation recording can be screwed up a little bit)
// (It should be simple to remove this limiations though, in a way that should provide a much better solution than hacking around with timers, comparing values, and so forth.)
//
// Thread-safe.
//
extern void PLUGIN_call_me_when_an_effect_value_has_changed(struct SoundPlugin *plugin,
                                                            int effect_num,
                                                            float native_value,
                                                            float scaled_value,
                                                            bool make_undo,
                                                            enum StoreitType storeit_type = STORE_VALUE,
                                                            FX_when when = FX_single,
                                                            bool update_instrument_widget = true,
                                                            bool is_sent_from_midi_learn = false
                                                            );
#endif

extern LANGSPEC void PLUGIN_set_effect_value(struct SoundPlugin *plugin,
                                             int time,
                                             int effect_num,
                                             float value,
                                             enum StoreitType storeit_type,
                                             FX_when when,
                                             enum ValueFormat value_format);

extern LANGSPEC float PLUGIN_get_effect_value2(struct SoundPlugin *plugin,
                                               int effect_num,
                                               enum WhereToGetValue where,
                                               enum ValueFormat value_format);

// Returns scaled value.
extern LANGSPEC float PLUGIN_get_effect_value(struct SoundPlugin *plugin,
                                              int effect_num,
                                              enum WhereToGetValue where);

extern LANGSPEC void PLUGIN_recreate_from_state(SoundPlugin *plugin, hash_t *state, bool is_loading);

extern LANGSPEC void PLUGIN_apply_ab_state(SoundPlugin *plugin, hash_t *state);
extern LANGSPEC hash_t *PLUGIN_get_ab_state(SoundPlugin *plugin);

extern LANGSPEC hash_t *PLUGIN_get_effects_state(SoundPlugin *plugin);
extern LANGSPEC hash_t *PLUGIN_get_state(SoundPlugin *plugin);

extern LANGSPEC void PLUGIN_set_effects_from_state(SoundPlugin *plugin, hash_t *effects);
extern LANGSPEC float PLUGIN_get_last_written_effect_from_name(SoundPlugin *plugin, const char *effect_name);
extern LANGSPEC float PLUGIN_get_effect_from_name(SoundPlugin *plugin, const char *effect_name, enum WhereToGetValue where, enum ValueFormat value_format);
extern LANGSPEC void PLUGIN_set_effect_from_name(SoundPlugin *plugin, const char *effect_name, float value); // scaled format
extern LANGSPEC void PLUGIN_DLoad(SoundPlugin *plugin);
extern LANGSPEC SoundPlugin *PLUGIN_create_from_state(struct Patch *patch, hash_t *state, bool is_loading);
extern LANGSPEC void PLUGIN_change_ab(SoundPlugin *plugin, int ab); // Only called from AUDIOWIDGET_set_ab. Call AUDIOWIDGET_set_ab instead.
extern LANGSPEC void PLUGIN_reset_ab(SoundPlugin *plugin, int ab);
extern LANGSPEC char *PLUGIN_generate_new_patchname(SoundPluginType *plugin_type);

extern LANGSPEC void PLUGIN_add_midi_learn(SoundPlugin *plugin, int effect_num);
extern LANGSPEC bool PLUGIN_remove_midi_learn(SoundPlugin *plugin, int effect_num, bool show_error_if_not_here);
extern LANGSPEC bool PLUGIN_has_midi_learn(SoundPlugin *plugin, int _effect_num);

extern LANGSPEC bool PLUGIN_is_recording_automation(const SoundPlugin *plugin, const int effect_num);
extern LANGSPEC void PLUGIN_set_recording_automation(SoundPlugin *plugin, int effect_num, bool is_recording);
extern LANGSPEC void PLUGIN_set_all_effects_to_not_recording(SoundPlugin *plugin);

extern LANGSPEC void PLUGIN_set_autosuspend_behavior(SoundPlugin *plugin, enum AutoSuspendBehavior new_behavior);
extern LANGSPEC enum AutoSuspendBehavior PLUGIN_get_autosuspend_behavior(const SoundPlugin *plugin);
extern LANGSPEC void PLUGIN_set_random_behavior(SoundPlugin *plugin, const int effect_num, bool do_random);
extern LANGSPEC bool PLUGIN_get_random_behavior(SoundPlugin *plugin, const int effect_num);

static inline void RT_PLUGIN_touch(SoundPlugin *plugin){
  //  if (plugin->patch!=NULL && !strcmp(plugin->patch->name,"Test"))
  //    printf("Touching %s\n",plugin->patch==NULL ? "(null)" : plugin->patch->name);

  R_ASSERT_NON_RELEASE(THREADING_is_runner_thread() || PLAYER_current_thread_has_lock());

  if (plugin != NULL) {

    // We can use RELAXED on these two variables since they would be set to the same values if accessed simultaneously. I'm pretty sure that's a valid reason to use RELAXED.
    // And furthermore, reading these two values are protected by other mechanisms, so they are never read at the same time as they are written.
    ATOMIC_SET_RELAXED(plugin->_RT_is_autosuspending, false);
    ATOMIC_SET_RELAXED(plugin->_RT_time_of_last_activity, RT_MIXER_get_last_used_time());
    
  } else {
    
    R_ASSERT_NON_RELEASE(false);
    
  }
}

#ifdef __cplusplus
static inline void PLUGIN_touch(SoundPlugin *plugin){
  radium::PlayerRecursiveLock lock;
  RT_PLUGIN_touch(plugin);
}
#endif

extern LANGSPEC bool RT_PLUGIN_can_autosuspend(const SoundPlugin *plugin, int64_t time);
//extern LANGSPEC bool PLUGIN_can_autosuspend(SoundPlugin *plugin);
  

extern LANGSPEC void PLUGIN_reset(SoundPlugin *plugin);
extern LANGSPEC void PLUGIN_reset_one_effect(SoundPlugin *plugin, int effect_num);

extern LANGSPEC void PLUGIN_random(SoundPlugin *plugin);

extern LANGSPEC void PLUGIN_show_info_window(const SoundPluginType *type, SoundPlugin *plugin, int64_t parentgui);

extern LANGSPEC const wchar_t *PLUGIN_DISK_get_audio_filename(hash_t *state);

// Defined in Qt/Qt_Main.cpp
extern LANGSPEC void RT_schedule_mixer_strips_remake(int64_t id); // id==-1: remake all, id==-2: remake none (only strip order may have changed).
extern LANGSPEC void RT_schedule_mixer_strips_redraw(void);


#if defined(USE_QT4) && defined(QSTRING_H)

#include "Modulator_plugin_proc.h"
#include "../common/instruments_proc.h"

static inline QString get_parameter_prepend_text(const struct Patch *patch, int effect_num){
  QString ret;
 
  if (MODULATOR_get_id(patch, effect_num) >= 0)
    ret = "m";
  
  if(patch->instrument==get_audio_instrument()){
    SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
    R_ASSERT_RETURN_IF_FALSE2(plugin!=NULL, ret);
    
    if (PLUGIN_has_midi_learn(plugin, effect_num))
      ret += "M";

    if(effect_num<plugin->type->num_effects && !PLUGIN_get_random_behavior(plugin, effect_num))
      ret += "r";
  }

  if (ret=="")
    return "";
  else
    return "[" + ret + "] ";
}
                                         
#endif


#endif // AUDIO_SOUNDPLUGIN_PROC_H
