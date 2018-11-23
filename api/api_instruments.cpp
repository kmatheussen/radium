/* Copyright 2001-2012 Kjetil S. Matheussen

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

#include "Python.h"
#include "radium_proc.h"

#include <string.h>
#include "../common/nsmtracker.h"
#include "../common/list_proc.h"
#include "../common/vector_proc.h"
#include "../common/placement_proc.h"
#include "../common/OS_visual_input.h"
#include "../common/undo.h"
#include "../common/undo_tracks_proc.h"
#include "../common/gfx_wtrackheaders_proc.h"
#include "../common/player_pause_proc.h"
#include "../common/seqtrack_proc.h"
#include "../common/seqtrack_automation_proc.h"
#include "../audio/undo_plugin_state_proc.h"

#include "../embedded_scheme/s7extra_proc.h"

#include "../midi/midi_instrument.h"
#include "../midi/midi_instrument_proc.h"
#include "../midi/midi_i_input_proc.h"
#include "../midi/midi_menues_proc.h"

#include "../audio/SoundPlugin.h"
#include "../audio/SoundPlugin_proc.h"
#include "../audio/AudioMeterPeaks_proc.h"
#include "../audio/SoundProducer_proc.h"
#include "../audio/SoundPluginRegistry_proc.h"
#include "../audio/Mixer_proc.h"
#include "../audio/Sampler_plugin_proc.h"
#include "../audio/audio_instrument_proc.h"
#include "../audio/Presets_proc.h"
#include "../audio/undo_audio_effect_proc.h"
#include "../audio/undo_connection_enabled_proc.h"
#include "../audio/undo_audio_connection_gain_proc.h"
#include "../audio/Modulator_plugin_proc.h"

#include "../mixergui/QM_MixerWidget.h"
#include "../mixergui/QM_chip.h"
#include "../mixergui/undo_chip_position_proc.h"
//#include "../mixergui/undo_chip_addremove_proc.h"
#include "../mixergui/undo_mixer_connections_proc.h"
#include "../mixergui/undo_mixer_proc.h"

#include "../OpenGL/Render_proc.h"

#include "../Qt/Qt_instruments_proc.h"

#include "../common/patch_proc.h"
#include "../common/undo_patchname_proc.h"
#include "../common/instruments_proc.h"
#include "../common/settings_proc.h"

#include "api_common_proc.h"


#include "api_instruments_proc.h"



extern struct Root *root;



static bool g_audio_buzy_loop = false;

bool doAudioBuzyLoop(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_audio_buzy_loop = SETTINGS_read_bool("audio_buzy_loop", false);
    has_inited = true;
  }

  return g_audio_buzy_loop;
}

void setAudioBuzyLoop(bool doit){
  g_audio_buzy_loop = doit;
  SETTINGS_write_bool("audio_buzy_loop", doit);
}

//

DEFINE_ATOMIC(bool, g_enable_autobypass) = false;
DEFINE_ATOMIC(int, g_autobypass_delay) = 500;


bool autobypassEnabled(void){
  static bool has_inited = false;

  if (has_inited==false){
    ATOMIC_SET(g_enable_autobypass, SETTINGS_read_bool("enable_autobypass", false));
    has_inited = true;
  }

  return ATOMIC_GET(g_enable_autobypass);
}

void setAutobypassEnabled(bool doit){
  if (doit != ATOMIC_GET(g_enable_autobypass)) {
    ATOMIC_SET(g_enable_autobypass, doit);
    SETTINGS_write_bool("enable_autobypass", doit);
    PREFERENCES_update();
  }
}

int getAutoBypassDelay(void){
  static bool has_inited = false;

  if (has_inited==false){
    ATOMIC_SET(g_autobypass_delay, SETTINGS_read_int32("autobypass_delay", 500));
    has_inited = true;
  }

  return ATOMIC_GET(g_autobypass_delay);
}

void setAutobypassDelay(int val){
  if (val != ATOMIC_GET(g_autobypass_delay)) {
    ATOMIC_SET(g_autobypass_delay, val);
    SETTINGS_write_int("autobypass_delay", val);
    PREFERENCES_update();
  }
}


static bool g_undo_solo = false;

bool doUndoSolo(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_undo_solo = SETTINGS_read_bool("undo_solo", g_undo_solo);
    has_inited = true;
  }

  return g_undo_solo;
}

void setUndoSolo(bool doit){
  g_undo_solo = doit;
  SETTINGS_write_bool("undo_solo", doit);
}


// Warning, All these functions (except selectPatchForTrack) must be called via python (does not update graphics, or handle undo/redo))
// (TODO: detect this automatically.)


void selectInstrumentForTrack(int tracknum){
  S7CALL2(void_int,"select-track-instrument", tracknum);
}

void requestReplaceInstrument(int64_t instrument_id, const_char* instrument_description, int64_t parentgui){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  dyn_t instrconf = createNewInstrumentConf(0,0,false,false,
                                            true,
                                            CHIP_get_num_in_connections(patch)>0,
                                            CHIP_get_num_out_connections(patch)>0,
                                            parentgui
                                            );

  S7CALL2(void_int_charpointer_dyn,"async-replace-instrument", instrument_id, instrument_description, instrconf);
}

void requestLoadInstrumentPreset(int64_t instrument_id, const_char* instrument_description, int64_t parentgui){
  S7CALL2(void_int_charpointer_int,"async-load-instrument-preset", instrument_id, instrument_description, parentgui);
}

void saveInstrumentPreset(dyn_t instrument_ids, int64_t parentgui){
  if (instrument_ids.type != ARRAY_TYPE){
    handleError("saveInstrumentPreset: Excpected array as first argument, found %s", DYN_type_name(instrument_ids.type));
    return;
  }
  dynvec_t *dynvec = instrument_ids.array;

  if (dynvec->num_elements<1){
    handleError("saveInstrumentPreset: \"instrument_ids\" is an empty array");
    return;
  }

  vector_t patches = {};

  for(int i=0;i<dynvec->num_elements;i++){
    
    if (dynvec->elements[i].type != INT_TYPE){
      handleError("saveInstrumentPreset: Element #%d is not an instrument id. Found: %s", i, DYN_type_name(dynvec->elements[i].type));
      return;
    }
    
    struct Patch *patch = getPatchFromNum(dynvec->elements[i].int_number);
    if(patch==NULL)
      return;
    
    VECTOR_push_back(&patches, patch);
  }

  PRESET_save(&patches, false, parentgui);
}

int64_t getInstrumentForTrack(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window=NULL;
  struct WTracks *wtrack;
  struct WBlocks *wblock;

  wtrack=getWTrackFromNumA(
	windownum,
	&window,
	blocknum,
	&wblock,
	tracknum
	);

  if(wtrack==NULL) return -1;

  struct Patch *patch = wtrack->track->patch;

  if (patch==NULL)
    return -2;

  return patch->id;
}

void setInstrumentForTrack(int64_t instrument_id, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window=NULL;
  struct WTracks *wtrack;
  struct WBlocks *wblock;

  wtrack=getWTrackFromNumA(
	windownum,
	&window,
	blocknum,
	&wblock,
	tracknum
	);

  if(wtrack==NULL) return;

  struct Patch *new_patch = getPatchFromNum(instrument_id);
  if(new_patch==NULL)
    return;

  struct Patch *old_patch = wtrack->track->patch;

  if (new_patch==old_patch)
    return;

  ADD_UNDO(Track_CurrPos(wblock->l.num, wtrack->l.num));

  {

    bool has_paused = false;
    
    if (old_patch != NULL)
      PATCH_handle_fx_when_theres_a_new_patch_for_track(wblock->block, wtrack->track, old_patch, new_patch, &has_paused);

    new_patch->has_been_assigned_to_editor_track = true;
    
    {
      SCOPED_PLAYER_LOCK_IF_PLAYING();
      wtrack->track->patch = new_patch;
    }

    if (has_paused)
      PC_StopPause(NULL);
  }
  
  wblock->block->is_dirty = true;

  (*new_patch->instrument->PP_Update)(new_patch->instrument,new_patch,false);
}

void replaceUseOfInstrument(int64_t old_instrument_id, int64_t new_instrument_id){
  struct Patch *old_patch = getPatchFromNum(old_instrument_id);
  if (old_patch==NULL)
    return;
  
  struct Patch *new_patch = new_instrument_id==-1 ? NULL : getPatchFromNum(new_instrument_id);
  if (new_patch==old_patch)
    return;

  UNDO_OPEN_REC();{
    PATCH_handle_editor_and_automation_when_replacing_patch(old_patch, new_patch);
  }UNDO_CLOSE();
}
  
// 

static bool g_split_into_monophonic_tracks_after_recording = false;

bool doSplitIntoMonophonicTracksAfterRecordingFromMidi(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_split_into_monophonic_tracks_after_recording = SETTINGS_read_bool("split_into_monophonic_tracks_after_recording", false);
    has_inited = true;
  }

  return g_split_into_monophonic_tracks_after_recording;
}

void setSplitIntoMonophonicTracksAfterRecordingFromMidi(bool doit){
  g_split_into_monophonic_tracks_after_recording = doit;
  SETTINGS_write_bool("split_into_monophonic_tracks_after_recording", doit);
}


//

DEFINE_ATOMIC(bool, g_use_track_channel_for_midi_input) = true;

bool doUseTrackChannelForMidiInput(void){
  static DEFINE_ATOMIC(bool, has_inited) = false;

  if (ATOMIC_GET(has_inited)==false){
    ATOMIC_SET(g_use_track_channel_for_midi_input, SETTINGS_read_bool("use_track_channel_for_midi_input", true));
    ATOMIC_SET(has_inited, true);
  }

  return ATOMIC_GET(g_use_track_channel_for_midi_input);
}

void setUseTrackChannelForMidiInput(bool doit){
  ATOMIC_SET(g_use_track_channel_for_midi_input, doit);
  SETTINGS_write_bool("use_track_channel_for_midi_input", doit);
}


int64_t createMIDIInstrument(const_char *name) {
  struct Patch *patch = PATCH_create_midi(name);
  GFX_PP_Update(patch,false);
  return patch->id;
}

// There was a good reason for the 'name' parameter. Think it had something to do with replace instrument, and whether to use old name or autogenerate new one.
int64_t createAudioInstrument(const_char *type_name, const_char *plugin_name, const_char *name, float x, float y) {
  printf("createAudioInstrument called\n");
  
  if (name!=NULL && strlen(name)==0)
    name = NULL;

  struct Patch *patch = PATCH_create_audio(type_name, plugin_name, name, NULL, x, y);
  if (patch==NULL)
    return -1;

  //MW_move_chip_to_slot(patch, x, y); // Ensure it is placed in a slot. (x and y comes from mouse positions, which are not necessarily slotted). <--- Changed. x and y must be slotted before calling this function.
  
  {
    struct SoundPlugin *plugin = static_cast<struct SoundPlugin*>(patch->patchdata);
    PR_inc_plugin_usage_number(plugin->type);
  }

  return patch->id;
}

int64_t createAudioInstrumentFromPreset(const char *filename, const_char *name, float x, float y) {
  return PRESET_load(STRING_create(filename), name, false, x, y);
}

const char *getAudioInstrumentDescription(const_char* container_name, const_char* type_name, const_char* plugin_name){
  return talloc_format("1%s:%s:%s", container_name, type_name, plugin_name);
}

static bool get_type_name_from_description(const char *instrument_description, const char **container_name, const char **type_name, const char **plugin_name){
  if (instrument_description[0]=='1'){

    char *descr = talloc_strdup(instrument_description);
    int sep_pos = 1;
    int sep_poss[2] = {};
    int sep_n = 0;
    while(sep_n < 2){
      char c = descr[sep_pos];
      if(c==0){
        handleError("Illegal instrument_description: %s (missing colon separator)",instrument_description);
        return -1;
      }
      if (c==':'){
        sep_poss[sep_n] = sep_pos;
        sep_n++;
      }
      sep_pos++;
    }
    descr[sep_poss[0]] = 0;
    descr[sep_poss[1]] = 0;

    *container_name = STRING_get_chars(STRING_fromBase64(STRING_create(&descr[1])));
    *type_name = STRING_get_chars(STRING_fromBase64(STRING_create(&descr[sep_poss[0]+1])));
    *plugin_name = STRING_get_chars(STRING_fromBase64(STRING_create(&descr[sep_poss[1]+1])));

    return true;

  }

  return false;
}
  
int64_t createAudioInstrumentFromDescription(const char *instrument_description, const_char *name, float x, float y){
  if (strlen(instrument_description)==0)
    return -1;

  if (name!=NULL && strlen(name)==0)
    name = NULL;

  const char *container_name;
  const char *type_name;
  const char *plugin_name;

  if (get_type_name_from_description(instrument_description, &container_name, &type_name, &plugin_name)){

    printf("  ---------- Container: -%s-, type: -%s-, plugin: -%s-\n", container_name, type_name, plugin_name);
    
    if (strlen(container_name) > 0)
      PR_ensure_container_is_populated(container_name, type_name); // Might fail, but we let createAudioInstrument print error message.

    return createAudioInstrument(type_name, plugin_name, name, x, y);
    
  } else if (instrument_description[0]=='2'){
    
    wchar_t *filename = STRING_fromBase64(STRING_create(&instrument_description[1]));
    //printf("filename: %s\n",filename);

    return PRESET_load(filename, name, true, x, y);
    
  } else if (instrument_description[0]=='3'){

    return MW_paste(x, y);
        
  } else {

    handleError("Illegal instrument_description: %s (string doesn't start with '1', '2' or '3')",instrument_description);
    return -1;

  }
}

int64_t cloneAudioInstrument(int64_t instrument_id, float x, float y){
  struct Patch *old_patch = getAudioPatchFromNum(instrument_id);
  if(old_patch==NULL)
    return -1;
  
  hash_t *state = PATCH_get_state(old_patch);

  struct Patch *new_patch = PATCH_create_audio(NULL, NULL, talloc_format("Clone of %s",old_patch->name), state, x, y);
  if (new_patch==NULL)
    return -1;

  return new_patch->id;
}



dyn_t createNewInstrumentConf(float x, float y,
                              bool connect_to_main_pipe,
                              bool do_autoconnect, bool include_load_preset,
                              bool must_have_inputs, bool must_have_outputs,
                              int64_t parentgui)
{
  hash_t *conf = HASH_create(7);
  HASH_put_float(conf, ":x", x);
  HASH_put_float(conf, ":y", y);
  HASH_put_bool(conf, ":connect-to-main-pipe", connect_to_main_pipe);
  HASH_put_bool(conf, ":do-autoconnect", do_autoconnect);
  HASH_put_bool(conf, ":include-load-preset", include_load_preset);
  HASH_put_bool(conf, ":must-have-inputs", must_have_inputs);
  HASH_put_bool(conf, ":must-have-outputs", must_have_outputs);
  HASH_put_int(conf, ":parentgui", parentgui);
  return DYN_create_hash(conf);
}

void createInstrumentDescriptionPopupMenu(dyn_t instrconf){
  S7CALL2(void_dyn,"create-instrument-popup-menu", instrconf);
}

dyn_t getAllSinglePresetsInPath(const_char* path){
  wchar_t *wpath = (path==NULL || strlen(path)==0) ? NULL : STRING_fromBase64(STRING_create(path));
    
  vector_t rec_presets = PRESET_get_all_rec_files_in_path(wpath);
  
  dynvec_t ret = {};

  VECTOR_FOR_EACH(const wchar_t *, path, &rec_presets){
    DYNVEC_push_back(&ret, DYN_create_string(STRING_toBase64(path)));
  }END_VECTOR_FOR_EACH;
  
  return DYN_create_array(ret);
}

dyn_t getAllMultiPresetsInPath(const_char* path){
  wchar_t *wpath = (path==NULL || strlen(path)==0) ? NULL : STRING_fromBase64(STRING_create(path));
    
  vector_t rec_presets = PRESET_get_all_mrec_files_in_path(wpath);

  dynvec_t ret = {};

  VECTOR_FOR_EACH(const wchar_t *, path, &rec_presets){
    DYNVEC_push_back(&ret, DYN_create_string(STRING_toBase64(path)));
  }END_VECTOR_FOR_EACH;
  
  return DYN_create_array(ret);
}

void requestLoadPresetInstrumentDescription(int64_t parentgui, func_t* callback){
  PRESET_request_load_instrument_description(parentgui, callback);
}

bool instrumentPresetInClipboard(void){
  return PRESET_has_copy();
}

int getNumInstrumentEffects(int64_t instrument_id){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  return patch->instrument->getFxNames(patch)->num_elements;
}

const_char* getInstrumentEffectName(int effect_num, int64_t instrument_id){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return "";

  vector_t *elements = patch->instrument->getFxNames(patch);
  
  if (effect_num >= elements->num_elements){
    handleError("effect_num >= num_effects: %d >= %d",effect_num, elements->num_elements);
    return "";
  }
    
  return talloc_strdup((const char*)elements->elements[effect_num]);
}

bool instrumentHasBeenUsed(int64_t instrument_id){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;

  return patch->has_been_assigned_to_editor_track;
}

bool hasPureData(void){
#if WITH_PD
  return true;
#else
  return false;
#endif
}

void setInstrumentSample(int64_t instrument_id, const_char *filename){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("Instrument #%d has been closed", (int)instrument_id);
    return;
  }

  if (strcmp(plugin->type->type_name, "Sample Player")) {
    handleError("instrument %d is not a Sample Player", (int)instrument_id);
    return;
  }

  ADD_UNDO(PluginState(patch, NULL));
  
  SAMPLER_set_new_sample(plugin, STRING_create(filename), -1);
}

void setRandomInstrumentSample(int64_t instrument_id, const_char *path){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("Instrument #%d has been closed", (int)instrument_id);
    return;
  }

  if (strcmp(plugin->type->type_name, "Sample Player")) {
    handleError("instrument %d is not a Sample Player", (int)instrument_id);
    return;
  }

  if (path==NULL || path[0]==0)
    path = NULL;

  ADD_UNDO(PluginState(patch, NULL));
  
  SAMPLER_set_random_sample(plugin, path==NULL ? NULL : STRING_create(path));
}

void setRandomSampleForAllSelectedInstruments(void){
  evalScheme("(set-random-sample-for-all-selected-sampler-instruments)");
}
  
void setInstrumentLoopData(int64_t instrument_id, int start, int length){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("Instrument #%d has been closed", (int)instrument_id);
    return;
  }

  if (strcmp(plugin->type->type_name, "Sample Player")) {
    handleError("instrument %d is not a Sample Player", (int)instrument_id);
    return;
  }


  SAMPLER_set_loop_data(plugin, start, length);

  GFX_update_instrument_widget(patch);

}

const_char *getInstrumentName(int64_t instrument_id) {
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return "";

  return patch->name;
}

void setInstrumentName(const_char *name, int64_t instrument_id) {
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  if (strcmp(name, patch->name)){

    ADD_UNDO(PatchName_CurrPos(patch));
    
    PATCH_set_name(patch, name);
    patch->name_is_edited = true;
    
    (*patch->instrument->PP_Update)(patch->instrument,patch,false);
  }
}

const_char* getInstrumentComment(int64_t instrument_id) {
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return "";

  if (patch->comment==NULL)
    return "";

  return patch->comment;
}

void setInstrumentComment(const_char* comment, int64_t instrument_id){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  patch->comment = talloc_strdup(comment);
}

bool instrumentNameWasAutogenerated(int64_t instrument_id){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;

  return !patch->name_is_edited;
}

void setInstrumentColor(const_char *colorname, int64_t instrument_id){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  unsigned int color = GFX_get_color_from_colorname(colorname);
  patch->color = color;

  if (patch->instrument==get_audio_instrument()){
    struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
    if (plugin==NULL){
      handleError("Instrument #%d has been closed", (int)instrument_id);
      return;
    }
    CHIP_update(plugin);
  }

  int num_connections = getNumInAudioConnections(patch->id);
  for(int i = 0 ; i < num_connections ; i++)
    remakeMixerStrips(getAudioConnectionSourceInstrument(i, patch->id));
  
  remakeMixerStrips(patch->id);

  root->song->tracker_windows->must_redraw=true;
}

const char *getInstrumentColor(int64_t instrument_id){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return "";

  return GFX_get_colorname_from_color(patch->color);
}

bool instrumentIsImplicitlyMuted(int64_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;
  
  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("Instrument #%d has been closed", (int)instrument_id);
    return false;
  }

  struct SoundProducer *sp = SP_get_sound_producer(plugin);
  R_ASSERT_RETURN_IF_FALSE2(sp!=NULL, false);
  
  return SP_mute_because_someone_else_has_solo_left_parenthesis_and_we_dont_right_parenthesis(sp);
}


float getInstrumentEffect(int64_t instrument_id, const_char* effect_name){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("Instrument #%d has been closed", (int)instrument_id);
    return 0.0;
  }

  return PLUGIN_get_effect_from_name(plugin, effect_name, VALUE_FROM_PLUGIN);
}

float getStoredInstrumentEffect(int64_t instrument_id, const_char* effect_name){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("Instrument #%d has been closed", (int)instrument_id);
    return 0.0;
  }

  return PLUGIN_get_effect_from_name(plugin, effect_name, VALUE_FROM_STORAGE);
}

static int get_effect_num(const struct Patch *patch, const_char* effect_name){
  char *error_message = NULL;
  int effect_num = PATCH_get_effect_num(patch, effect_name, &error_message);
  if (effect_num==-1)
    handleError("%s", error_message);
  return effect_num;
}

float getDefaultInstrumentEffect(int64_t instrument_id, const_char* effect_name){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("Instrument #%d has been closed", (int)instrument_id);
    return 0.0;
  }

  int effect_num = get_effect_num(patch, effect_name);
  if (effect_num < 0)
    return 0.0f;
  
  return plugin->initial_effect_values_scaled[effect_num];
}

static void post_set_effect(struct Patch *patch, struct SoundPlugin *plugin, const char *effect_name){
  if (!strcmp(effect_name, "System Volume") ||
      !strcmp(effect_name, "System In") ||
      !strcmp(effect_name, "System Solo On/Off") ||
      !strcmp(effect_name, "System Volume On/Off") ||
      !strcmp(effect_name, "System In On/Off") ||
      !strcmp(effect_name, "System Effects On/Off"))
    CHIP_update(plugin);
  
  GFX_update_instrument_widget(patch);
}
                       
void setInstrumentEffect(int64_t instrument_id, const char *effect_name, float value){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("Instrument #%d has been closed", (int)instrument_id);
    return;
  }

  /*
  if (strcmp(plugin->type->type_name, "Sample Player")) {
    handleError("instrument %d is not a Sample Player plugin", (int)instrument_id);
    return;
  }
  */

  if (value < -0.01f || value > 1.01f){ // Allow a little bit below 0 and a little bit above 1. Probably No need to report floating point rounding errors.
    handleError("setInstrumentEffect: effect value must be between 0 and 1. Value: %f", value);
    return;
  }
  
  PLUGIN_set_effect_from_name(plugin, effect_name, value);

  post_set_effect(patch, plugin, effect_name);
}

void resetInstrumentEffect(int64_t instrument_id, const char *effect_name){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("Instrument #%d has been closed", (int)instrument_id);
    return;
  }

  int effect_num = get_effect_num(patch, effect_name);
  if (effect_num < 0)
    return;
  
  PLUGIN_reset_one_effect(plugin, effect_num);

  post_set_effect(patch, plugin, effect_name);
}



const_char* getInstrumentEffectColor(int64_t instrument_id, const_char* effect_name){
  const struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return "";

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("Instrument #%d has been closed", (int)instrument_id);
    return "";
  }

  int effect_num = get_effect_num(patch, effect_name);
  if(effect_num==-1)
    return "";

  return GFX_get_colorname_from_color(GFX_get_color(get_effect_color(plugin, effect_num)));
}

bool getNoteDuplicatorSetNewValueImmediately(int64_t instrument_id, const_char* effect_name){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;

#define C(Name, Bool)                                                   \
  else if (!strcmp(Name, effect_name))                                  \
    return !Bool
  
#define CT(Num)                                                         \
  C("System Transpose Voice " # Num, patch->voices[Num-1].only_set_new_transpose_when_note_on)
#define CV(Num)                                                         \
  C("System Volume Voice " # Num, patch->voices[Num-1].only_set_new_volume_when_note_on)
#define CP(Num)                                                         \
  C("System Pan Voice " # Num, patch->voices[Num-1].only_set_new_pan_when_note_on)
    
    
  if (false)
    return false;
  CT(1);CT(2);CT(3);CT(4);CT(5);CT(6);CT(7);
  CV(1);CV(2);CV(3);CV(4);CV(5);CV(6);CV(7);
  CP(1);CP(2);CP(3);CP(4);CP(5);CP(6);CP(7);

  
  handleError("getNoteDuplicatorSetNewValueImmediately: Unsupported effect \"%s\"", effect_name);
  return false;

#undef CP
#undef CV
#undef CT
#undef C
}

void setNoteDuplicatorSetNewValueImmediately(int64_t instrument_id, const_char* effect_name, bool set_immediately){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

#define C(Name, Bool)                                                   \
  else if (!strcmp(Name, effect_name)) do{                              \
      radium::PlayerLock lock;                                          \
      Bool = !set_immediately;                                          \
    }while(0)                                                           \
      
#define CT(Num)                                                         \
  C("System Transpose Voice " # Num, patch->voices[Num-1].only_set_new_transpose_when_note_on)
#define CV(Num)                                                         \
  C("System Volume Voice " # Num, patch->voices[Num-1].only_set_new_volume_when_note_on)
#define CP(Num)                                                         \
  C("System Pan Voice " # Num, patch->voices[Num-1].only_set_new_pan_when_note_on)
  
  if (false)
    return;
  CT(1);CT(2);CT(3);CT(4);CT(5);CT(6);CT(7);
  CV(1);CV(2);CV(3);CV(4);CV(5);CV(6);CV(7);
  CP(1);CP(2);CP(3);CP(4);CP(5);CP(6);CP(7);
  else {
    handleError("setNoteDuplicatorSetNewValueImmediately: Unsupported effect \"%s\"", effect_name);
  }
  
  

#undef CP
#undef CV
#undef CT
#undef C
}


// midi learn
///////////////

bool instrumentEffectHasMidiLearn(int64_t instrument_id, const_char* effect_name){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;
  
  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("Instrument #%d has been closed", (int)instrument_id);
    return false;
  }

  int effect_num = get_effect_num(patch, effect_name);
  if(effect_num==-1)
    return false;
  
  return PLUGIN_has_midi_learn(plugin, effect_num);
}

void addInstrumentEffectMidiLearn(int64_t instrument_id, const_char* effect_name){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;
  
  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("Instrument #%d has been closed", (int)instrument_id);
    return;
  }

  int effect_num = get_effect_num(patch, effect_name);
  if(effect_num==-1)
    return;

  if (PLUGIN_has_midi_learn(plugin, effect_num)){
    handleError("addInstrumentEffectMidiLearn: %s / %s already has MIDI learn", plugin->patch->name, effect_name);
    return;
  }
  
  PLUGIN_add_midi_learn(plugin, effect_num);  
}

void removeInstrumentEffectMidiLearn(int64_t instrument_id, const_char* effect_name){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;
  
  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("Instrument #%d has been closed", (int)instrument_id);
    return;
  }

  int effect_num = get_effect_num(patch, effect_name);
  if(effect_num==-1)
    return;

  if (false == PLUGIN_has_midi_learn(plugin, effect_num)){
    handleError("removeInstrumentEffectMidiLearn: %s / %s doesn't have MIDI learn", plugin->patch->name, effect_name);
    return;
  }
  
  PLUGIN_remove_midi_learn(plugin, effect_num, true);
}



bool addAutomationToCurrentEditorTrack(int64_t instrument_id, const_char* effect_name){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("Instrument #%d has been closed", (int)instrument_id);
    return false;
  }

  int effect_num = get_effect_num(patch, effect_name);
  if (effect_num==-1)
    return false;

  int blocknum = currentBlock(-1);
  int tracknum = R_MAX(0, currentTrack(blocknum, -1));
  int64_t track_instrument_id = getInstrumentForTrack(tracknum, blocknum, -1);
  
  if (track_instrument_id < 0) {
    track_instrument_id = instrument_id;
    setInstrumentForTrack(instrument_id, tracknum, blocknum, -1);
  }
        
  float value_ = PLUGIN_get_effect_value(plugin, effect_num, VALUE_FROM_STORAGE);
  
  int fxnum = getFx(effect_name, tracknum, patch->id, blocknum, -1);
  
  if (fxnum >= 0){
    
    addFxnode(value_,
              p_Create(currentLine(blocknum, -1), 0, 1),
              fxnum,
              tracknum,
              blocknum,
              -1);
    
    
  } else {
    
    addFx(value_,
          p_Create(currentLine(blocknum, -1), 0, 1),
          effect_name,
          tracknum,
          patch->id,
          blocknum,
          -1);
  }

  return true;
}

bool addAutomationToCurrentSequencerTrack(int64_t instrument_id, const_char* effect_name){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("Instrument #%d has been closed", (int)instrument_id);
    return false;
  }

  int effect_num = get_effect_num(patch, effect_name);
  if (effect_num==-1)
    return false;
  
  struct SeqTrack *seqtrack = SEQUENCER_get_curr_seqtrack();
  if (seqtrack == NULL)
    return false;
  
  undoSeqtrackAutomations();
  
  float value_ = PLUGIN_get_effect_value(plugin, effect_num, VALUE_FROM_STORAGE);
  
  int64_t pos1 = ATOMIC_DOUBLE_GET(pc->song_abstime); //is_playing() && pc->playtype==PLAYSONG ? ATOMIC_DOUBLE_GET(pc->song_abstime) : 0;
  
  int64_t visible_duration = R_MAX(100, SEQUENCER_get_visible_end_time() - SEQUENCER_get_visible_start_time());
  
  int64_t pos2 = pos1 + visible_duration / 10;
  
  SEQTRACK_AUTOMATION_add_automation(seqtrack->seqtrackautomation, patch, effect_num, pos1, value_, LOGTYPE_LINEAR, pos2, value_, NULL, NULL);

  return true;
}
  
              
void setInstrumentSolo(int64_t instrument_id, bool do_solo){
  S7CALL2(void_int_bool,"FROM-C-set-solo!", instrument_id, do_solo);
}

void setInstrumentMute(int64_t instrument_id, bool do_mute){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
  int effect_num = get_mute_effectnum(plugin->type);

  float new_val = do_mute ? 0.0 : 1.0;
  
  ADD_UNDO(AudioEffect_CurrPos(patch, effect_num));

  PLUGIN_set_effect_value(plugin, -1, effect_num, new_val, STORE_VALUE, FX_single, EFFECT_FORMAT_SCALED);
}

void setInstrumentBypass(int64_t instrument_id, bool do_bypass){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
  int num_effects = plugin->type->num_effects;
  ADD_UNDO(AudioEffect_CurrPos((struct Patch*)patch, num_effects+EFFNUM_EFFECTS_ONOFF));
  float new_val = do_bypass ? 0.0 : 1.0;
  PLUGIN_set_effect_value(plugin, -1, num_effects+EFFNUM_EFFECTS_ONOFF, new_val, STORE_VALUE, FX_single, EFFECT_FORMAT_SCALED);
}

void undoInstrumentEffect(int64_t instrument_id, const char *effect_name){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("Instrument #%d (\"%s\") has been closed", (int)instrument_id, patch->name);
    return;
  }

  int effect_num = get_effect_num(patch, effect_name);
  if (effect_num==-1)
    return;
  
  ADD_UNDO(AudioEffect_CurrPos(patch, effect_num));
}

#if 0
void setInstrumentVolume(int64_t instrument_id, float volume) {
  struct Instruments *instrument = getInstrumentFromNum(instrument_id);
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL) return NULL;

  (*patch->instrument->PP_Update)(instrument,patch,false);
}

float getInstrumentVolume(int64_t instrument_id) {
  return 0.0f;
}
#endif

float getMinDb(void){
  return MIN_DB;
}

float getMaxDb(void){
  return MAX_DB;
}

float dbToGain(float db){
  return db2gain(db);
}

float gainToDb(float gain){
  return gain2db(gain);
}

void setInstrumentData(int64_t instrument_id, const_char *key, const_char *value) {
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  patch->instrument->setPatchData(patch, key, value, true);

  (*patch->instrument->PP_Update)(patch->instrument,patch,false);
}

const_char *getInstrumentData(int64_t instrument_id, const_char *key) {
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return "";

  return patch->instrument->getPatchData(patch, key);
}


int getNumMIDIInstruments(void){
  return get_MIDI_instrument()->patches.num_elements;
}

int64_t getMIDIInstrumentId(int instrument_num){
  if (instrument_num>=getNumMIDIInstruments()){
    handleError("No instrument #%d", (int)instrument_num);
    return -1;
  }
  struct Patch *patch = (struct Patch*)get_MIDI_instrument()->patches.elements[instrument_num];
  return patch->id;
}

int getNumAudioInstruments(void){
  return get_audio_instrument()->patches.num_elements;
}

int64_t getAudioInstrumentId(int instrument_num){
  if (instrument_num>=getNumAudioInstruments()){
    handleError("No instrument #%d", (int)instrument_num);
    return -1;
  }
  struct Patch *patch = (struct Patch*)get_audio_instrument()->patches.elements[instrument_num];
  return patch->id;
}

int64_t getAudioBusId(int bus_num){
  if (bus_num < 0 || bus_num>=5){
    handleError("There is no bus %d", bus_num);
    return -1;
  }

  struct Patch *patch = MIXER_get_bus(bus_num);
  if (patch==NULL)
    return -1; // never happens
  
  return patch->id;
}

bool instrumentIsBusDescendant(int64_t instrument_id){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;

  if (patch->instrument == get_audio_instrument()){
    struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
    if (plugin==NULL){
      handleError("Instrument #%d has been closed", (int)instrument_id);
      return true;
    }

    struct SoundProducer *sp = SP_get_sound_producer(plugin);
    R_ASSERT_RETURN_IF_FALSE2(sp!=NULL, false);
    return SP_get_bus_descendant_type(sp)==IS_BUS_DESCENDANT;
  }else
    return false;
}

bool instrumentIsPermanent(int64_t instrument_id){  
  if (g_is_replacing_main_pipe==true) // hack
    return false;

  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;

  if (patch->instrument == get_audio_instrument())
    return AUDIO_is_permanent_patch(patch);
  else
    return true; // Can not delete midi instruments.
}

bool instrumentIsAudio(int64_t instrument_id){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;

  return patch->instrument == get_audio_instrument();
}

// Mixer GUI
float getInstrumentX(int64_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0.0;

  return CHIP_get_pos_x(patch);
}

float getInstrumentY(int64_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0.0;

  return CHIP_get_pos_y(patch);
}

// workaround
/*
float getMixerSlottedX(float from_x){
  float from_y=0,x,y;
  MW_get_slotted_x_y(from_x, from_y, &x, &y);
  return x;
}

// workaround
float getMixerSlottedY(float from_y){
  float from_x=0,x,y;
  MW_get_slotted_x_y(from_x, from_y, &x, &y);
  return y;
}
*/

void setInstrumentPosition(float x, float y, int64_t instrument_id, bool auto_slot){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  ADD_UNDO(ChipPos_CurrPos(patch));

#if 0
  CHIP_set_pos(patch,x,y);
#else
  float to_x,to_y;
  MW_get_slotted_x_y(x, y, &to_x, &to_y);
  CHIP_set_pos(patch,to_x,to_y);
#endif
}

void autopositionInstrument(int64_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  ADD_UNDO(ChipPos_CurrPos(patch));
  CHIP_autopos(patch);
}

const_char* getInstrumentTypeName(int64_t instrument_id){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return "";

  if (patch->instrument==get_MIDI_instrument())
    return "MIDI";

  struct SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
  return plugin->type->type_name;
}

const_char* getInstrumentPluginName(int64_t instrument_id){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return "";

  if (patch->instrument==get_MIDI_instrument())
    return "MIDI";

  struct SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
  return plugin->type->name;
}

const_char* getInstrumentInfo(int64_t instrument_id){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return "";

  if (patch->instrument==get_MIDI_instrument())
    return "A MIDI instrument";

  struct SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
  if (plugin->type->info)
    return plugin->type->info;

  return "";
}

dyn_t getSelectedInstruments(void){
  dynvec_t ret = {};
  vector_t patches = MW_get_selected_patches();

  VECTOR_FOR_EACH(struct Patch *, patch,&patches){
    DYNVEC_push_back(&ret, DYN_create_int(patch->id));
  }END_VECTOR_FOR_EACH;

  return DYN_create_array(ret);
}

int numSelectedInstruments(void){
  int ret = 0;
  
  VECTOR_FOR_EACH(struct Patch *, patch, &get_audio_instrument()->patches){
    struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
    if (plugin!=NULL && ATOMIC_GET(plugin->is_selected))
      ret++;
  }END_VECTOR_FOR_EACH;

  return ret;
}

bool instrumentIsSelected(int64_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("Instrument #%d has been closed", (int)instrument_id);
    return false;
  }  

  return ATOMIC_GET(plugin->is_selected);
}



// connections

void connectAudioInstrumentToMainPipe(int64_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("Instrument #%d has been closed", (int)instrument_id);
    return;
  }

  ADD_UNDO(MixerConnections_CurrPos());
  MW_connect_plugin_to_main_pipe(plugin);
}

bool autoconnectInstrument(int64_t instrument_id, float x, float y){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("Instrument #%d has been closed", (int)instrument_id);
    return false;
  }

  ADD_UNDO(MixerConnections_CurrPos());
  return MW_autoconnect(patch, x, y);
}

int getNumInAudioConnections(int64_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  return CHIP_get_num_in_connections(patch);
}

int getNumInEventConnections(int64_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  return CHIP_get_num_in_econnections(patch);
}

int getNumOutAudioConnections(int64_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  return CHIP_get_num_out_connections(patch);
}

int getNumOutEventConnections(int64_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  return CHIP_get_num_out_econnections(patch);
}

int64_t getAudioConnectionSourceInstrument(int connectionnum, int64_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  struct Patch *source = CHIP_get_source(patch, connectionnum);
  if (source == NULL)
    return 0;
    
  return source->id;
}

int64_t getEventConnectionSourceInstrument(int connectionnum, int64_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  struct Patch *source = CHIP_get_esource(patch, connectionnum);
  if (source == NULL)
    return 0;
    
  return source->id;
}

int64_t getAudioConnectionDestInstrument(int connectionnum, int64_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  struct Patch *dest = CHIP_get_dest(patch, connectionnum);
  if (dest == NULL)
    return 0;
  
  return dest->id;
}

int64_t getEventConnectionDestInstrument(int connectionnum, int64_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  struct Patch *dest = CHIP_get_edest(patch, connectionnum);
  if (dest == NULL)
    return 0;
  
  return dest->id;
}

void undoMixerConnections(void){
  ADD_UNDO(MixerConnections_CurrPos());  
}

void createAudioConnection(int64_t source_id, int64_t dest_id, float gain){
  struct Patch *source = getAudioPatchFromNum(source_id);
  if(source==NULL)
    return;

  struct Patch *dest = getAudioPatchFromNum(dest_id);
  if(dest==NULL)
    return;
  
  MW_connect(source, dest); 

  if (gain != 1.0)
    setAudioConnectionGain(source_id, dest_id, gain, true);
}

void deleteAudioConnection(int64_t source_id, int64_t dest_id){
  struct Patch *source = getAudioPatchFromNum(source_id);
  if(source==NULL)
    return;

  struct Patch *dest = getAudioPatchFromNum(dest_id);
  if(dest==NULL)
    return;

  if (MW_disconnect(source, dest)==false)
    handleError("Could not find audio connection between \"%s\" and \"%s\"", source->name, dest->name);
}

bool changeAudioConnections(dyn_t changes){
  return CONNECTIONS_apply_changes(changes);
}

bool hasAudioConnection(int64_t source_id, int64_t dest_id){
  struct Patch *source = getAudioPatchFromNum(source_id);
  if(source==NULL)
    return false;

  struct Patch *dest = getAudioPatchFromNum(dest_id);
  if(dest==NULL)
    return false;

  return MW_are_connected(source, dest);
}

void createEventConnection(int64_t source_id, int64_t dest_id){
  struct Patch *source = getAudioPatchFromNum(source_id);
  if(source==NULL)
    return;

  struct Patch *dest = getAudioPatchFromNum(dest_id);
  if(dest==NULL)
    return;

  MW_econnect(source, dest); 
}
                           
void deleteEventConnection(int64_t source_id, int64_t dest_id){
  struct Patch *source = getAudioPatchFromNum(source_id);
  if(source==NULL)
    return;

  struct Patch *dest = getAudioPatchFromNum(dest_id);
  if(dest==NULL)
    return;

  if (MW_edisconnect(source, dest)==false)
    handleError("Could not find audio connection between \"%s\" and \"%s\"", source->name, dest->name);
}

bool hasEventConnection(int64_t source_id, int64_t dest_id){
  struct Patch *source = getAudioPatchFromNum(source_id);
  if(source==NULL)
    return false;

  struct Patch *dest = getAudioPatchFromNum(dest_id);
  if(dest==NULL)
    return false;

  return MW_are_econnected(source, dest);
}

static bool get_connection_gain_enabled(int64_t source_id, int64_t dest_id, float *gain, bool *is_enabled, bool show_error_if_not_connected){
  struct Patch *source = getAudioPatchFromNum(source_id);
  if(source==NULL)
    return false;

  struct Patch *dest = getAudioPatchFromNum(dest_id);
  if(dest==NULL)
    return false;

  struct SoundPlugin *source_plugin = (struct SoundPlugin*)source->patchdata;
  if (source_plugin==NULL){
    handleError("Instrument #%d has been closed", (int)source_id);
    return false;
  }

  struct SoundPlugin *dest_plugin = (struct SoundPlugin*)dest->patchdata;
  if (dest_plugin==NULL){
    handleError("Instrument #%d has been closed", (int)dest_id);
    return false;
  }

  struct SoundProducer *source_sp = SP_get_sound_producer(source_plugin);
  struct SoundProducer *dest_sp = SP_get_sound_producer(dest_plugin);

  const char *error = NULL;

  if (gain != NULL)
    *gain = SP_get_link_gain(dest_sp, source_sp, &error);

  if (error==NULL && is_enabled!=NULL)
    *is_enabled = SP_get_link_enabled(dest_sp, source_sp, &error);

  if (error!=NULL){
    if (show_error_if_not_connected)
      handleError("Could not find audio connection between instrument %d (%s) and instrument %d (%s): %s", (int)source_id, source->name, (int)dest_id, dest->name, error);
    return false;
  }

  return true;
}

float getAudioConnectionGain(int64_t source_id, int64_t dest_id, bool show_error_if_not_connected){
  float ret;
  if (get_connection_gain_enabled(source_id, dest_id, &ret, NULL, show_error_if_not_connected))
    return ret;
  else
    return 0.0;
}

bool getConnectionEnabled(int64_t source_id, int64_t dest_id, bool show_error_if_not_connected){
  bool ret;
  if (get_connection_gain_enabled(source_id, dest_id, NULL, &ret, show_error_if_not_connected))
    return ret;
  else
    return false;
}

static void set_connection_gain_enabled(int64_t source_id, int64_t dest_id, const float *gain, const bool *is_enabled, bool redraw_mixer_strips){
  struct Patch *source = getAudioPatchFromNum(source_id);
  if(source==NULL)
    return;

  struct Patch *dest = getAudioPatchFromNum(dest_id);
  if(dest==NULL)
    return;

  struct SoundPlugin *source_plugin = (struct SoundPlugin*)source->patchdata;
  if (source_plugin==NULL){
    handleError("Instrument #%d has been closed", (int)source_id);
    return;
  }

  struct SoundPlugin *dest_plugin = (struct SoundPlugin*)dest->patchdata;
  if (dest_plugin==NULL){
    handleError("Instrument #%d has been closed", (int)dest_id);
    return;
  }

  struct SoundProducer *source_sp = SP_get_sound_producer(source_plugin);
  struct SoundProducer *dest_sp = SP_get_sound_producer(dest_plugin);

  const char *error = NULL;
  bool changed1 = false;
  bool changed2 = false;

  if (gain != NULL)
    changed1 = SP_set_link_gain(dest_sp, source_sp, *gain, &error);

  if (is_enabled != NULL && error==NULL)
    changed2 = SP_set_link_enabled(dest_sp, source_sp, *is_enabled, &error);

  if (error!=NULL)
    handleError("Could not find audio connection between instrument %d and instrument %d", (int)source_id, (int)dest_id);
  else
    if ((changed1||changed2) && redraw_mixer_strips){
      //printf("       Remake: setAudioConnectionGain\n");
      redrawMixerStrips(false);
    }
}

void setAudioConnectionGain(int64_t source_id, int64_t dest_id, float gain, bool redraw_mixer_strips){
  set_connection_gain_enabled(source_id, dest_id, &gain, NULL, redraw_mixer_strips);
}

void setConnectionEnabled(int64_t source_id, int64_t dest_id, bool is_enabled, bool redraw_mixer_strips){
  set_connection_gain_enabled(source_id, dest_id, NULL, &is_enabled, redraw_mixer_strips);
}

void undoConnectionEnabled(int64_t source_id, int64_t dest_id){
  struct Patch *source = getAudioPatchFromNum(source_id);
  if(source==NULL)
    return;

  struct Patch *dest = getAudioPatchFromNum(dest_id);
  if(dest==NULL)
    return;

  ADD_UNDO(ConnectionEnabled_CurrPos(source, dest));
}


void undoAudioConnectionGain(int64_t source_id, int64_t dest_id){
  struct Patch *source = getAudioPatchFromNum(source_id);
  if(source==NULL)
    return;

  struct Patch *dest = getAudioPatchFromNum(dest_id);
  if(dest==NULL)
    return;

  ADD_UNDO(AudioConnectionGain_CurrPos(source, dest));
}


// modulators

int64_t createModulator(void){
  struct Patch *curr_patch = g_currpatch;
  
  int64_t instrument_id = createAudioInstrument(MODULATOR_NAME, MODULATOR_NAME, "", 0, 0);
  if (instrument_id==-1)
    return -1;
  
  if (curr_patch != NULL)
    GFX_PP_Update(curr_patch, false); // Set back current instrument.
  
  const struct Patch *modulator_patch = PATCH_get_from_id(instrument_id);
  
  ADD_UNDO(ChipPos_CurrPos(modulator_patch));
  autopositionInstrument(instrument_id);

  return instrument_id;
}


bool hasModulator(int64_t instrument_id, const char *effect_name){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;

  int effect_num = get_effect_num(patch, effect_name);
  if (effect_num==-1)
    return false;

  return MODULATOR_has_modulator(patch, effect_num);
}

static void addModulator2(int64_t instrument_id, const char *effect_name, int64_t modulator_instrument_id, bool supposed_to_already_have_modulator){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  int effect_num = get_effect_num(patch, effect_name);
  if (effect_num==-1)
    return;

  if (modulator_instrument_id != -1){
    
    struct Patch *modulator_patch = getPatchFromNum(modulator_instrument_id);
    if (modulator_patch==NULL)
      return;
    
    int modulator_id = MODULATOR_get_id_from_modulator_patch(modulator_patch);
    if (modulator_id==-1) {
      handleError("addModulator/replaceModulator: \"%s\" is not a modulator instrument", modulator_patch->name);
      return;
    }

    MODULATOR_add_target(modulator_id, patch, effect_num, supposed_to_already_have_modulator);
    
  } else {

    int64_t modulator_id = MODULATOR_get_id(patch, effect_num);
    
    if (supposed_to_already_have_modulator) {
      if (modulator_id == -1){
        handleError("replaceModulator: Effect %s in instrument \"%s\" does not have a modulator", effect_name, getInstrumentName(instrument_id));
        return;
      }
    } else {
      if (modulator_id >= 0){
        handleError("addModulator: Effect %s in instrument \"%s\" already has a modulator: %s", effect_name, getInstrumentName(instrument_id), MODULATOR_get_description(modulator_id));
        return;
      }
    }
    
    MODULATOR_maybe_create_and_add_target(patch, effect_num, supposed_to_already_have_modulator);

  }
}

void addModulator(int64_t instrument_id, const char *effect_name, int64_t modulator_instrument_id){
  addModulator2(instrument_id, effect_name, modulator_instrument_id, false);
}

void replaceModulator(int64_t instrument_id, const char *effect_name, int64_t modulator_instrument_id){
  addModulator2(instrument_id, effect_name, modulator_instrument_id, true);
}

void removeModulator(int64_t instrument_id, const char *effect_name){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  int effect_num = get_effect_num(patch, effect_name);
  if (effect_num==-1)
    return;

  int64_t modulator_id = MODULATOR_get_id(patch, effect_num);

  if (modulator_id == -1){
    handleError("removeModulator: Effect %s in instrument \"%s\" does not have a modulator", effect_name, getInstrumentName(instrument_id));
    return;
  }

  MODULATOR_remove_target(modulator_id, patch, effect_num);
}

static const char *get_modulator_patch_description(const struct Patch *modulator_patch){
  int64_t modulator_id = MODULATOR_get_id_from_modulator_patch(modulator_patch);

  if (modulator_id < 0){
    handleError("Patch \"%s\" is not a modulator", modulator_patch->name);
    return "";
  }

  return talloc_format("%s: %s", modulator_patch->name, MODULATOR_get_description(modulator_id));
}

const char *getModulatorDescription3(int64_t modulator_instrument_id){
  const struct Patch *patch = getPatchFromNum(modulator_instrument_id);
  if(patch==NULL)
    return "";

  return get_modulator_patch_description(patch);
}

const char *getModulatorDescription2(int64_t instrument_id, int effect_num){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return "";
  
  int64_t modulator_id = MODULATOR_get_id(patch, effect_num);

  if (modulator_id == -1){
    handleError("getModulatorDescription: Effect %d in instrument \"%s\" does not have a modulator", effect_num, getInstrumentName(instrument_id));
    return "";
  }

  const struct Patch *modulation_patch = MODULATOR_get_modulator_patch(patch, effect_num);

  return get_modulator_patch_description(modulation_patch);
}

const char *getModulatorDescription(int64_t instrument_id, const char *effect_name){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return "";
  
  int effect_num = get_effect_num(patch, effect_name);
  if (effect_num==-1)
    return "";

  return getModulatorDescription2(instrument_id, effect_num);
}

dyn_t getModulatorInstruments(void){
  dynvec_t dynvec = {};

  const dyn_t dynstate = MODULATORS_get_connections_state(); // Using this function as a way to get a list of all modulator instruments. (easier than filtering modulators from the list of all patches)
  const dynvec_t *vec = dynstate.array;
  
  for(int i = 0 ; i < vec->num_elements ; i++){
    dyn_t dynstate = vec->elements[i];
    
    hash_t *modulator_state = dynstate.hash;
    int64_t patch_id = HASH_get_int(modulator_state, "modulator_patch_id");

    DYNVEC_push_back(&dynvec, DYN_create_int(patch_id));
  }

  return DYN_create_array(dynvec);
}

// Note, called quite often.
dyn_t getModulatorTargets(int64_t modulator_instrument_id){

  struct Patch *patch = getAudioPatchFromNum(modulator_instrument_id);
  if(patch==NULL)
    return g_empty_dynvec;

  if (!MODULATOR_is_modulator(modulator_instrument_id)){
    handleError("getModulatorTargets: Instrument #%d is not a modulator", (int)modulator_instrument_id);
    return g_empty_dynvec;
  }
    
  return DYN_create_array(MODULATOR_get_modulator_targets(modulator_instrument_id));
}

void setModulatorEnabled(int64_t instrument_id, const char *effect_name, bool enabled){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  int effect_num = get_effect_num(patch, effect_name);
  if (effect_num==-1)
    return;

  int64_t modulator_id = MODULATOR_get_id(patch, effect_num);

  if (modulator_id == -1){
    handleError("removeModulator: Effect %s in instrument \"%s\" does not have a modulator", effect_name, getInstrumentName(instrument_id));
    return;
  }

  MODULATOR_set_target_enabled(modulator_id, patch, effect_num, enabled);
}

bool getModulatorEnabled(int64_t instrument_id, const char *effect_name){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;

  int effect_num = get_effect_num(patch, effect_name);
  if (effect_num==-1)
    return false;

  int64_t modulator_id = MODULATOR_get_id(patch, effect_num);

  if (modulator_id == -1){
    handleError("removeModulator: Effect %s in instrument \"%s\" does not have a modulator", effect_name, getInstrumentName(instrument_id));
    return false;
  }

  return MODULATOR_get_target_enabled(modulator_id, patch, effect_num);
}

                               
/*
void addModulator(int64_t instrument_id, const char *effect_name, int64_t modulator_instrument_id){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  if (get_modulator(instrument_id, effect_name) >= 0){
    handleError("addModulator: Effect %s in instrument \"%s\" already has a modulator: %s", effect_name, getInstrumentName(instrument_id), getModulatorDescription(modulator_instrument_id));
    return;
  }
}

void removeModulator(int64_t instrument_id, const char *effect_name, int64_t modulator_instrument_id){
}

void replaceModulator(int64_t instrument_id, const char *effect_name, int64_t modulator_instrument_id){
}

int64_t getModulator(int64_t instrument_id, const char *effect_name){
}

dyn_t get_modulators_instruments(void){
}

const_char *getModulatorDescription(int64_t modulator_instrument_id){
}
*/

int getNumInputChannels(int64_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("Instrument #%d has been closed", (int)instrument_id);
    return 0;
  }

  return plugin->type->num_inputs;
}

int getNumOutputChannels(int64_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("Instrument #%d has been closed", (int)instrument_id);
    return 0;
  }

  return plugin->type->num_outputs;
}

void deleteInstrument(int64_t instrument_id){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  UNDO_OPEN_REC();{

    PATCH_make_inactive(patch);

  }UNDO_CLOSE();

  root->song->tracker_windows->must_redraw=true;

}

void internalReplaceMainPipe(int64_t new_main_pipe_id){
  if (g_is_replacing_main_pipe==false){
    handleError("Can not call this function like this");
    return;
  }
  
  struct Patch *patch = PATCH_get_from_id(new_main_pipe_id);
  if (patch != NULL)
    PATCH_replace_main_pipe(patch);
  else
    R_ASSERT(false);

  g_is_replacing_main_pipe = false;
}

bool instrumentIsOpenAndAudio(int64_t instrument_id){
  const struct Patch *patch = PATCH_get_from_id(instrument_id);
  if (patch==NULL)
    return false;

  return patch->instrument == get_audio_instrument();
}

bool instrumentIsOpen(int64_t instrument_id){
  return PATCH_get_from_id(instrument_id) != NULL;
}

const_char* getSampleBookmarks(int num){
  return SETTINGS_read_string(talloc_format("sample_bookmarks%d",num), "/");
}

void setSampleBookmarks(int num, const_char* path){
  SETTINGS_write_string(talloc_format("sample_bookmarks%d",num), path);
}

void midi_resetAllControllers(void){
  printf("midi_resetAllControllers called\n");
  MIDIResetAllControllers();
}

void midi_localKeyboardOn(void){
  MIDILocalKeyboardOn();
}

void midi_localKeyboardOff(void){
  MIDILocalKeyboardOff();
}

void midi_allNotesOff(void){
  MIDIAllNotesOff();
}

void midi_allSoundsOff(void){
  MIDIAllSoundsOff();
}

void midi_recordAccurately(bool accurately){
  MIDI_set_record_accurately(accurately);
}

void midi_alwaysRecordVelocity(bool doit){
  MIDI_set_record_velocity(doit);
}

void midi_setInputPort(void){
  MIDISetInputPort(true);
}

#define NUM_IDS 2048
static int playnote_ids_pos = 0;
static int64_t playnote_ids[NUM_IDS] = {};
static float initial_pitches[NUM_IDS] = {};

int playNote(float pitch, float velocity, float pan, int midi_channel, int64_t instrument_id){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return -1;

  if (midi_channel < 0 || midi_channel > 15){
    handleError("midi_channel must be between 0 and 15. Found %d", midi_channel);
    return -1;
  }
  
  if (pitch <= 0){
    handleError("pitch must be higher than 0. Found %f", pitch);
    return -1;
  }
  
  if (velocity < 0){
    handleError("velocity must be 0 or higher. Found %f", velocity);
    return -1;
  }
  
  int ret = playnote_ids_pos;

  playnote_ids[ret] = PATCH_play_note(patch, create_note_t(NULL, -1, pitch, velocity, pan, midi_channel, 0, 0));
  initial_pitches[ret] = pitch;
    
  playnote_ids_pos++;
  if (playnote_ids_pos==NUM_IDS)
    playnote_ids_pos = 0;
  
  return ret;
}

void changeNotePitch(float pitch, int playnote_id, int midi_channel, int64_t instrument_id){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  if (playnote_id < 0 || playnote_id >= NUM_IDS) {
    handleError("playnote_id %d not found", playnote_id);
    return;
  }

  if (midi_channel < 0 || midi_channel > 15){
    handleError("midi_channel must be between 0 and 15. Found %d", midi_channel);
    return;
  }

  if (pitch <= 0){
    handleError("pitch must be higher than 0. Found %f", pitch);
    return;
  }
  
  //printf("change pitch %f %d %d\n",pitch,playnote_id,instrument_id);
  PATCH_change_pitch(patch, create_note_t(NULL,
                                          playnote_ids[playnote_id],
                                          pitch,
                                          0,
                                          pitch,
                                          midi_channel,
                                          0,
                                          0
                                          ));
}

void stopNote(int playnote_id, int midi_channel, int64_t instrument_id){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  if (playnote_id < 0 || playnote_id >= NUM_IDS) {
    handleError("playnote_id %d not found", playnote_id);
    return;
  }

  if (midi_channel < 0 || midi_channel > 15){
    handleError("midi_channel must be between 0 and 15. Found %d", midi_channel);
    return;
  }
  
  //printf("stop note %d %d\n",playnote_id,instrument_id);
  PATCH_stop_note(patch, create_note_t(NULL,
                                       playnote_ids[playnote_id],
                                       initial_pitches[playnote_id],
                                       0,
                                       0,
                                       midi_channel,
                                       0,
                                       0
                                       ));
}

bool hasNativeInstrumentGui(int64_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;

  struct SoundPlugin *plugin = (struct SoundPlugin *)patch->patchdata;

  if (plugin != NULL){
    
    if(plugin->type->show_gui != NULL)
      return true;
  }

  return false;
}

bool showInstrumentGui(int64_t instrument_id, int64_t parentgui, bool show_instrument_window_if_not_visible){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;

  //struct Patch *currpatch_before = g_currpatch;
  
  bool instrument_window_was_visible = GFX_InstrumentWindowIsVisible();
    
  //if(showInstrumentWidgetWhenDoubleClickingSoundObject())
  if(show_instrument_window_if_not_visible)
    GFX_InstrumentWindowToFront();

  struct Instruments *instrument = get_audio_instrument();
  instrument->PP_Update(instrument,patch,false);
  
  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin != NULL){
    
    if(plugin->type->show_gui != NULL) {
    
      return PLUGIN_open_gui(plugin, parentgui);
      
    } else {

      bool instrument_window_is_visible = GFX_InstrumentWindowIsVisible();

      bool show_message = true;
      if (!instrument_window_was_visible && instrument_window_is_visible)
        show_message = false;
      
      //else if (instrument_window_is_visible && currpatch_before != g_currpatch)
      //  show_message = true;

      //printf("was: %d, is: %d. show: %d\n", instrument_window_was_visible, instrument_window_is_visible, show_message);

      if (show_message)
        GFX_Message2(NULL, true, "Instrument %s of type %s / %s does not have a native GUI", patch->name, plugin->type->type_name, plugin->type->name);
      
    }
    
  }

  return false;
}

bool hideInstrumentGui(int64_t instrument_id){

  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;

  //if(show_instrument_window_if_not_visible)
  //  GFX_InstrumentWindowToFront();

  //struct Instruments *instrument = get_audio_instrument();
  //instrument->PP_Update(instrument,patch,false);
  
  struct SoundPlugin *plugin = (struct SoundPlugin *)patch->patchdata;
  if (plugin != NULL){

    PLUGIN_close_gui(plugin);

    return true;
  }

  return false;
}

bool instrumentGuiIsVisible(int64_t instrument_id, int64_t parentgui){

  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;

  //if(show_instrument_window_if_not_visible)
  //  GFX_InstrumentWindowToFront();

  //struct Instruments *instrument = get_audio_instrument();
  //instrument->PP_Update(instrument,patch,false);
  
  struct SoundPlugin *plugin = (struct SoundPlugin *)patch->patchdata;
  if (plugin != NULL)
    return PLUGIN_gui_is_visible(plugin, parentgui);
  else
    return false;
}

void internal_instrumentGuiHasBeenHidden(int64_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  struct SoundPlugin *plugin = (struct SoundPlugin *)patch->patchdata;
  if (plugin != NULL)
    PLUGIN_call_me_when_gui_closes(plugin);
  else{
    R_ASSERT_NON_RELEASE(false);
  }
}

int64_t getCurrentInstrument(void){
  if (g_currpatch==NULL)
    return -1;
  return g_currpatch->id;
}

void setCurrentInstrument(int64_t instrument_id, bool show_instrument_window_if_not_visible){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  if (patch==g_currpatch)
    return;

  //if(showInstrumentWidgetWhenDoubleClickingSoundObject())
  if(show_instrument_window_if_not_visible)
    GFX_InstrumentWindowToFront();

  patch->instrument->PP_Update(patch->instrument, patch, false);
}

void showInstrumentInfo(dyn_t instrument_id_or_description, int64_t parentgui){
  if (instrument_id_or_description.type==INT_TYPE){
    
    int64_t instrument_id = instrument_id_or_description.int_number;
    struct Patch *patch = getAudioPatchFromNum(instrument_id);
    if(patch==NULL)
      return;
  
    struct SoundPlugin *plugin = (struct SoundPlugin *)patch->patchdata;
    if (plugin != NULL)
      PLUGIN_show_info_window(plugin->type, plugin, parentgui);
    
  } else if (instrument_id_or_description.type==STRING_TYPE){
    
    const char *instrument_description = STRING_get_chars(instrument_id_or_description.string);

    const char *container_name;
    const char *type_name;
    const char *plugin_name;
    
    if (get_type_name_from_description(instrument_description, &container_name, &type_name, &plugin_name)){
      
      printf("  ---------- Container: -%s-, type: -%s-, plugin: -%s-\n", container_name, type_name, plugin_name);

      SoundPluginType *type = PR_get_plugin_type_by_name(container_name, type_name, plugin_name);

      if (type != NULL){
        PLUGIN_show_info_window(type, NULL, parentgui);
      }
      
    } else
      handleError("Unable to determine instrument type from description");
    

  } else {
    
    handleError("Illegal first argument for showInstrumentInfo. Expected INT_TYPE or STRING_TYPE, found %s", DYN_type_name(instrument_id_or_description.type));
      
  }
}



/********** Instrument deletion generation ***************/

static int64_t g_deletion_generation = 0;
static int64_t g_audio_deletion_generation = 0;

void API_instrument_call_me_when_instrument_is_deleted(struct Patch *patch){
  g_deletion_generation++;
  if (patch->instrument==get_audio_instrument())
    g_audio_deletion_generation++;
}

int64_t getInstrumentDeletionGeneration(void){
  return g_deletion_generation;
}

int64_t getAudioInstrumentDeletionGeneration(void){
  return g_audio_deletion_generation;
}



/******** Effect monitors ************/

struct EffectMonitor{
  int64_t id;

  struct Patch *patch;
  
  int64_t instrument_id;
  bool monitor_stored;
  bool monitor_automation;
  int effect_num;
  func_t *func;

  float last_stored_value;
  float last_automation_value;
};

static int64_t g_effect_monitor_id = 0;
static vector_t g_effect_monitors = {};

static struct EffectMonitor *find_effect_monitor_from_id(int64_t id){
  VECTOR_FOR_EACH(struct EffectMonitor *, effect_monitor, &g_effect_monitors){
    if (effect_monitor->id==id)
      return effect_monitor;
  }END_VECTOR_FOR_EACH;

  return NULL;
}

/*
static struct EffectMonitor *find_effect_monitor(int effect_num, int64_t instrument_id){
  VECTOR_FOR_EACH(struct EffectMonitor *effect_monitor, &g_effect_monitors){
    if (effect_monitor->effect_num==effect_num && effect_monitor->instrument_id==instrument_id)
      return effect_monitor;
  }END_VECTOR_FOR_EACH;

  return NULL;
}
*/

int64_t addEffectMonitor(const char *effect_name, int64_t instrument_id, bool monitor_stored, bool monitor_automation, func_t *func){
  if (!monitor_automation && !monitor_stored){
    handleError("addEfectMonitor: at least one of monitor_stored and monitor_automation must be true");
    return -1;
  }
  
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return -1;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("addEffectMonitor: Instrument #%d has been closed", (int)instrument_id);
    return -1;
  }

  int effect_num = get_effect_num(patch, effect_name);
  if (effect_num==-1)
    return -1;
  
    /*
  if (find_effect_monitor(effect_num, instrument_id) != NULL){
    handleError("There is already an effect monitor for %s / %d", getInstrumentName(instrument_id), effect_num);
    return -1;
  }
    */
    
  struct EffectMonitor *effect_monitor = (struct EffectMonitor *)talloc(sizeof(struct EffectMonitor));

  effect_monitor->id = g_effect_monitor_id++;
  effect_monitor->patch = patch;
  
  effect_monitor->effect_num = effect_num;
  effect_monitor->instrument_id = instrument_id;
  effect_monitor->monitor_stored = monitor_stored;
  effect_monitor->monitor_automation = monitor_automation;
  effect_monitor->func = func;

  effect_monitor->last_stored_value = 0;
  effect_monitor->last_automation_value = -10;

  VECTOR_push_back(&g_effect_monitors, effect_monitor);

  s7extra_protect(effect_monitor->func);  

  return effect_monitor->id;
}

void removeEffectMonitor(int64_t effect_monitor_id){
  struct EffectMonitor *effect_monitor = find_effect_monitor_from_id(effect_monitor_id);
  if (effect_monitor==NULL){
    handleError("No effect monitor #%d", (int)effect_monitor_id);
    return;
  }

  s7extra_unprotect(effect_monitor->func);
    
  VECTOR_remove(&g_effect_monitors, effect_monitor);
}


void API_instruments_call_regularly(void){
  VECTOR_FOR_EACH(struct EffectMonitor *, effect_monitor, &g_effect_monitors){
    struct Patch *patch = effect_monitor->patch;
    struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
    if(plugin!=NULL){

      bool send_stored=false, send_automation = false;
      
      float stored_now = -1;
      float automation_now = -10;

      if (effect_monitor->monitor_stored){
        stored_now = plugin->stored_effect_values_scaled[effect_monitor->effect_num];
        if (stored_now != effect_monitor->last_stored_value){
          effect_monitor->last_stored_value = stored_now;
          send_stored = true;
        }
      }

      if (effect_monitor->monitor_automation){
        automation_now = safe_float_read(&plugin->slider_automation_values[effect_monitor->effect_num]);
        if (automation_now != effect_monitor->last_automation_value){
          effect_monitor->last_automation_value = automation_now;
          send_automation = true;
        }
      }
      
      if (send_stored || send_automation){
        S7CALL(void_dyn_dyn,
               effect_monitor->func,
               send_stored ? DYN_create_float(stored_now) : g_dyn_false,
               send_automation ? DYN_create_float(automation_now) : g_dyn_false
               );
      }
    }
  }END_VECTOR_FOR_EACH;
}



// Mixer strips
////////////////////////////////////////////////

void redrawMixerStrips(bool immediately){
  if (immediately)
    evalScheme("(redraw-mixer-strips)");
  else
    RT_schedule_mixer_strips_redraw(); // We don't want to redraw immediately in case we remake when a connection is being deleted or created, and we don't want to remake several times in a row either, or too often.
}

void remakeMixerStrips(int64_t id){
  RT_schedule_mixer_strips_remake(id); // We don't want to redraw immediately in case we remake when a connection is being deleted or created, and we don't want to remake several times in a row either, or too often.
}

bool hasWideInstrumentStrip(int64_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id); // calling getAudioPatchFromNum instead of getPatchFromNum works as an assertion
  if(patch==NULL)
    return true;

  return patch->wide_mixer_strip;
}

void setWideInstrumentStrip(int64_t instrument_id, bool is_wide){
  struct Patch *patch = getAudioPatchFromNum(instrument_id); // calling getAudioPatchFromNum instead of getPatchFromNum works as an assertion
  if(patch==NULL)
    return;

  patch->wide_mixer_strip=is_wide;
}

void setMixerStripCommentsVisible(bool val){
  if(root->song->mixer_comments_visible != val){
    root->song->mixer_comments_visible = val;
    remakeMixerStrips(-1);
  }
}
  
bool mixerStripCommentsVisible(void){
  return root->song->mixer_comments_visible;
}
