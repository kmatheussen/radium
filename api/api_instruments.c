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
#include "../common/OS_visual_input.h"
#include "../common/undo.h"
#include "../common/undo_tracks_proc.h"
#include "../common/gfx_wtrackheaders_proc.h"
#include "../common/player_pause_proc.h"
#include "../audio/undo_plugin_state_proc.h"

#include "../embedded_scheme/s7extra_proc.h"

#include "../midi/midi_i_plugin.h"
#include "../midi/midi_i_plugin_proc.h"
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

  PATCH_handle_editor_and_automation_when_replacing_patch(old_patch, new_patch);
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
    struct SoundPlugin *plugin = patch->patchdata;
    PR_inc_plugin_usage_number(plugin->type);
  }

  return patch->id;
}

int64_t createAudioInstrumentFromPreset(const char *filename, const_char *name, float x, float y) {
  return PRESET_load(STRING_create(filename), name, false, x, y);
}

bool get_type_name_from_description(const char *instrument_description, const char **container_name, const char **type_name, const char **plugin_name){
  if (instrument_description[0]=='1'){

    char *descr = talloc_strdup(instrument_description);
    int sep_pos = 1;
    int sep_poss[2] = {0};
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
  
  dynvec_t ret = {0};

  VECTOR_FOR_EACH(wchar_t *path, &rec_presets){
    DYNVEC_push_back(&ret, DYN_create_string(STRING_toBase64(path)));
  }END_VECTOR_FOR_EACH;
  
  return DYN_create_array(ret);
}

dyn_t getAllMultiPresetsInPath(const_char* path){
  wchar_t *wpath = (path==NULL || strlen(path)==0) ? NULL : STRING_fromBase64(STRING_create(path));
    
  vector_t rec_presets = PRESET_get_all_mrec_files_in_path(wpath);

  dynvec_t ret = {0};

  VECTOR_FOR_EACH(wchar_t *path, &rec_presets){
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
    
  return talloc_strdup(elements->elements[effect_num]);
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

  ADD_UNDO(PluginState_CurrPos(patch));
  
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

  ADD_UNDO(PluginState_CurrPos(patch));
  
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

  if (!strcmp(effect_name, "System Volume") ||
      !strcmp(effect_name, "System In") ||
      !strcmp(effect_name, "System Solo On/Off") ||
      !strcmp(effect_name, "System Volume On/Off") ||
      !strcmp(effect_name, "System In On/Off") ||
      !strcmp(effect_name, "System Effects On/Off"))
    CHIP_update(plugin);
  
  GFX_update_instrument_widget(patch);
}


const_char* getInstrumentEffectColor(int64_t instrument_id, const_char* effect_name){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return "";

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("Instrument #%d has been closed", (int)instrument_id);
    return "";
  }

  int effect_num = PLUGIN_get_effect_num(plugin, effect_name, false);
  if (effect_num==-1){
    handleError("Unknown effect \"%s\" in instrument #%d (\"%s\")", effect_name, (int)instrument_id, patch->name);
    return "";
  }

  return GFX_get_colorname_from_color(GFX_get_color(get_effect_color(plugin, effect_num)));
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

  int effect_num = PLUGIN_get_effect_num(plugin, effect_name, false);

  if (effect_num==-1){
    handleError("Unknown effect \"%s\" in instrument #%d (\"%s\")", effect_name, (int)instrument_id, patch->name);
    return;
  }
  
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
  struct Patch *patch = get_MIDI_instrument()->patches.elements[instrument_num];
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
  struct Patch *patch = get_audio_instrument()->patches.elements[instrument_num];
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
  dynvec_t ret = {0};
  vector_t patches = MW_get_selected_patches();

  VECTOR_FOR_EACH(struct Patch *patch,&patches){
    DYNVEC_push_back(&ret, DYN_create_int(patch->id));
  }END_VECTOR_FOR_EACH;

  return DYN_create_array(ret);
}

int numSelectedInstruments(void){
  int ret = 0;
  
  VECTOR_FOR_EACH(struct Patch *patch, &get_audio_instrument()->patches){
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

bool get_connection_gain_enabled(int64_t source_id, int64_t dest_id, float *gain, bool *is_enabled){
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
    handleError("Could not find audio connection between instrument %d and instrument %d", (int)source_id, (int)dest_id);
    return false;
  }

  return true;
}

float getAudioConnectionGain(int64_t source_id, int64_t dest_id){
  float ret;
  if (get_connection_gain_enabled(source_id, dest_id, &ret, NULL))
    return ret;
  else
    return 0.0;
}

bool getConnectionEnabled(int64_t source_id, int64_t dest_id){
  bool ret;
  if (get_connection_gain_enabled(source_id, dest_id, NULL, &ret))
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
static int note_ids_pos = 0;
static int64_t note_ids[NUM_IDS] = {0}; // TODO: Change int to int64_t everywhere in the api.
static float initial_pitches[NUM_IDS] = {0}; // TODO: Change int to int64_t everywhere in the api.

int playNote(float pitch, float velocity, float pan, int midi_channel, int64_t instrument_id){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return -1;

  int ret = note_ids_pos;

  note_ids[ret] = PATCH_play_note(patch, create_note_t(NULL, -1, pitch, velocity, pan, midi_channel, 0, 0));
  initial_pitches[ret] = pitch;
    
  note_ids_pos++;
  if (note_ids_pos==NUM_IDS)
    note_ids_pos = 0;
  
  return ret;
}

void changeNotePitch(float pitch, int note_id, int midi_channel, int64_t instrument_id){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  if (note_id < 0 || note_id >= NUM_IDS) {
    handleError("note_id %d not found", note_id);
    return;
  }

  //printf("change pitch %f %d %d\n",pitch,note_id,instrument_id);
  PATCH_change_pitch(patch, create_note_t(NULL,
                                          note_ids[note_id],
                                          pitch,
                                          0,
                                          pitch,
                                          midi_channel,
                                          0,
                                          0
                                          ));
}

void stopNote(int note_id, int midi_channel, int64_t instrument_id){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  if (note_id < 0 || note_id >= NUM_IDS) {
    handleError("note_id %d not found", note_id);
    return;
  }

  //printf("stop note %d %d\n",note_id,instrument_id);
  PATCH_stop_note(patch, create_note_t(NULL,
                                       note_ids[note_id],
                                       initial_pitches[note_id],
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

  struct SoundPlugin *plugin = patch->patchdata;

  if (plugin != NULL){
    
    if(plugin->type->show_gui != NULL)
      return true;
  }

  return false;
}

bool showInstrumentGui(int64_t instrument_id, bool show_instrument_window_if_not_visible){
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
  
  struct SoundPlugin *plugin = patch->patchdata;
  if (plugin != NULL){
    
    if(plugin->type->show_gui != NULL) {
    
      plugin->type->show_gui(plugin);
      return true;
      
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
  
  struct SoundPlugin *plugin = patch->patchdata;
  if (plugin != NULL){
    
    if(plugin->type->hide_gui != NULL) {
    
      plugin->type->hide_gui(plugin);
      return true;
      
    }
  }

  return false;
}

bool instrumentGuiIsVisible(int64_t instrument_id){

  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;

  //if(show_instrument_window_if_not_visible)
  //  GFX_InstrumentWindowToFront();

  //struct Instruments *instrument = get_audio_instrument();
  //instrument->PP_Update(instrument,patch,false);
  
  struct SoundPlugin *plugin = patch->patchdata;
  if (plugin != NULL){
    
    if(plugin->type->gui_is_visible != NULL) {
    
      return plugin->type->gui_is_visible(plugin);
      
    }
  }

  return false;
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
  
    struct SoundPlugin *plugin = patch->patchdata;
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
static vector_t g_effect_monitors = {0};

static struct EffectMonitor *find_effect_monitor_from_id(int64_t id){
  VECTOR_FOR_EACH(struct EffectMonitor *effect_monitor, &g_effect_monitors){
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
  R_ASSERT_NON_RELEASE(monitor_automation || monitor_stored);

  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return -1;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("addEffectMonitor: Instrument #%d has been closed", (int)instrument_id);
    return -1;
  }

  int effect_num = PLUGIN_get_effect_num(plugin, effect_name, false);

  if (effect_num==-1){
    handleError("Unknown effect \"%s\" in instrument #%d (\"%s\")", effect_name, (int)instrument_id, patch->name);
    return -1;
  }
  
    /*
  if (find_effect_monitor(effect_num, instrument_id) != NULL){
    handleError("There is already an effect monitor for %s / %d", getInstrumentName(instrument_id), effect_num);
    return -1;
  }
    */
    
  struct EffectMonitor *effect_monitor = talloc(sizeof(struct EffectMonitor));

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
  VECTOR_FOR_EACH(struct EffectMonitor *effect_monitor, &g_effect_monitors){
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
