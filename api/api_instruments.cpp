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
#include "../common/undo_instrument_color_proc.h"
#include "../common/player_pause_proc.h"
#include "../common/sequencer_proc.h"
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
#include "../audio/Pd_plugin_proc.h"
#include "../audio/SampleReader_proc.h"

#include "../mixergui/QM_MixerWidget.h"
#include "../mixergui/QM_chip.h"
#include "../mixergui/undo_chip_position_proc.h"
//#include "../mixergui/undo_chip_addremove_proc.h"
#include "../mixergui/undo_mixer_connections_proc.h"
#include "../mixergui/undo_mixer_proc.h"

#include "../OpenGL/Render_proc.h"

#include "../Qt/Qt_instruments_proc.h"
#include "../Qt/Qt_colors_proc.h"

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



static bool g_undo_bypass = true;

bool doUndoBypass(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_undo_bypass = SETTINGS_read_bool("undo_bypass", g_undo_bypass);
    has_inited = true;
  }

  return g_undo_bypass;
}

void setUndoBypass(bool doit){
  g_undo_bypass = doit;
  SETTINGS_write_bool("undo_bypass", doit);
}



void selectInstrumentForTrack(int tracknum){
  S7CALL2(void_int,"select-track-instrument", tracknum);
}

void requestReplaceInstrument(instrument_t instrument_id, const_char* instrument_description, int64_t parentgui){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  dyn_t instrconf = createNewInstrumentConf(0,0,false,false,
                                            true,
                                            CHIP_get_num_in_connections(patch)>0,
                                            CHIP_get_num_out_connections(patch)>0,
                                            parentgui
                                            );

  S7CALL2(void_instrument_charpointer_dyn,"async-replace-instrument", instrument_id, instrument_description, instrconf);
}

void requestLoadInstrumentPreset(instrument_t instrument_id, const_char* instrument_description, int64_t parentgui){
  S7CALL2(void_instrument_charpointer_int,"async-load-instrument-preset", instrument_id, instrument_description, parentgui);
}

void saveInstrumentPreset(dynvec_t instrument_ids, int64_t parentgui){

  if (instrument_ids.num_elements<1){
    handleError("saveInstrumentPreset: \"instrument_ids\" is an empty array");
    return;
  }

  vector_t patches = {};

  for(int i=0;i<instrument_ids.num_elements;i++){
    
    if (instrument_ids.elements[i].type != INSTRUMENT_TYPE){
      handleError("saveInstrumentPreset: Element #%d is not an instrument id. Found: %s", i, DYN_type_name(instrument_ids.elements[i].type));
      return;
    }
    
    struct Patch *patch = getPatchFromNum(instrument_ids.elements[i].instrument);
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

bool hasInstrumentForTrack(int tracknum, int blocknum, int windownum){
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

  if(wtrack==NULL) return false;

  struct Patch *patch = wtrack->track->patch;

  if (patch==NULL)
    return false;

  return true;
}

bool isLegalInstrument(instrument_t instrument_id){
  return instrument_id >= 0;
}
  
instrument_t createIllegalInstrument(void){
  return -1;
}
  
void setInstrumentForTrack(instrument_t instrument_id, int tracknum, int blocknum, int windownum){
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
  static bool s_has_inited = false;

  if (s_has_inited==false){
    ATOMIC_SET(g_use_track_channel_for_midi_input, SETTINGS_read_bool("use_track_channel_for_midi_input", true));
    s_has_inited = true;
  }

  return ATOMIC_GET(g_use_track_channel_for_midi_input);
}

void setUseTrackChannelForMidiInput(bool doit){
  ATOMIC_SET(g_use_track_channel_for_midi_input, doit);
  SETTINGS_write_bool("use_track_channel_for_midi_input", doit);
}


DEFINE_ATOMIC(bool, g_send_midi_input_to_current_instrument) = true;

bool isSendingMidiInputToCurrentInstrument(void){
  static bool s_has_inited = false;

  if (s_has_inited==false){
    ATOMIC_SET(g_send_midi_input_to_current_instrument, SETTINGS_read_bool("send_midi_input_to_current_instrument", true));
    s_has_inited = true;
  }

  return ATOMIC_GET(g_send_midi_input_to_current_instrument);
}

void setSendMidiInputToCurrentInstrument(bool doit){
  ATOMIC_SET(g_send_midi_input_to_current_instrument, doit);
  SETTINGS_write_bool("send_midi_input_to_current_instrument", doit);
}


int64_t createMIDIInstrument(const_char *name) {
  struct Patch *patch = PATCH_create_midi(name);
  GFX_PP_Update(patch,false);
  return patch->id;
}

// There was a good reason for the 'name' parameter. Think it had something to do with replace instrument, and whether to use old name or autogenerate new one.
instrument_t createAudioInstrument(const_char *type_name, const_char *plugin_name, const_char *name, float x, float y, bool set_as_current) {
  printf("createAudioInstrument called\n");
  
  if (name!=NULL && strlen(name)==0)
    name = NULL;

  struct Patch *patch = PATCH_create_audio(type_name, plugin_name, name, NULL, set_as_current, x, y);
  if (patch==NULL)
    return -1;

  //MW_move_chip_to_slot(patch, x, y); // Ensure it is placed in a slot. (x and y comes from mouse positions, which are not necessarily slotted). <--- Changed. x and y must be slotted before calling this function.
  
  {
    struct SoundPlugin *plugin = static_cast<struct SoundPlugin*>(patch->patchdata);
    PR_inc_plugin_usage_number(plugin->type);
  }

  return patch->id;
}

int64_t createAudioInstrumentFromPreset(const char *filename, const_char *name, float x, float y, bool set_as_current) {
  return PRESET_load(STRING_create(filename), name, false, set_as_current, x, y);
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
  
int64_t createAudioInstrumentFromDescription(const char *instrument_description, const_char *name, float x, float y, bool set_as_current){
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

    return createAudioInstrument(type_name, plugin_name, name, x, y, set_as_current);
    
  } else if (instrument_description[0]=='2'){
    
    wchar_t *filename = STRING_fromBase64(STRING_create(&instrument_description[1]));
    //printf("filename: %s\n",filename);

    return PRESET_load(filename, name, true, set_as_current, x, y);
    
  } else if (instrument_description[0]=='3'){

    return MW_paste(x, y);
        
  } else {

    handleError("Illegal instrument_description: %s (string doesn't start with '1', '2' or '3')",instrument_description);
    return -1;

  }
}

int64_t cloneAudioInstrument(instrument_t instrument_id, float x, float y, bool set_as_current){
  struct Patch *old_patch = getAudioPatchFromNum(instrument_id);
  if(old_patch==NULL)
    return -1;
  
  hash_t *state = PATCH_get_state(old_patch);

  struct Patch *new_patch = PATCH_create_audio(NULL, NULL, talloc_format("Clone of %s",old_patch->name), state, set_as_current, x, y);
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

dynvec_t getAllSinglePresetsInPath(const_char* path){
  wchar_t *wpath = (path==NULL || strlen(path)==0) ? NULL : STRING_fromBase64(STRING_create(path));
    
  vector_t rec_presets = PRESET_get_all_rec_files_in_path(wpath);
  
  dynvec_t ret = {};

  VECTOR_FOR_EACH(const wchar_t *, path, &rec_presets){
    DYNVEC_push_back(&ret, DYN_create_string(STRING_toBase64(path)));
  }END_VECTOR_FOR_EACH;
  
  return ret;
}

dynvec_t getAllMultiPresetsInPath(const_char* path){
  wchar_t *wpath = (path==NULL || strlen(path)==0) ? NULL : STRING_fromBase64(STRING_create(path));
    
  vector_t rec_presets = PRESET_get_all_mrec_files_in_path(wpath);

  dynvec_t ret = {};

  VECTOR_FOR_EACH(const wchar_t *, path, &rec_presets){
    DYNVEC_push_back(&ret, DYN_create_string(STRING_toBase64(path)));
  }END_VECTOR_FOR_EACH;
  
  return ret;
}

void requestLoadPresetInstrumentDescription(int64_t parentgui, func_t* callback){
  PRESET_request_load_instrument_description(parentgui, callback);
}

bool instrumentPresetInClipboard(void){
  return PRESET_has_copy();
}

int getNumInstrumentEffects(instrument_t instrument_id){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  return patch->instrument->getNumFxs(patch);
}

const_char* getInstrumentEffectName(int effect_num, instrument_t instrument_id){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return "";

  int num_fxs = patch->instrument->getNumFxs(patch);
  if (effect_num < 0 || effect_num >= num_fxs){
    handleError("getInstrumentEffectName: effect_num >= num_effects: %d >= %d", effect_num, num_fxs);
    return "";
  }
  
  return patch->instrument->getFxName(patch, effect_num);
}

bool instrumentHasBeenUsed(instrument_t instrument_id){
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

void setInstrumentSample(instrument_t instrument_id, const_char *filename){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("setInstrumentSample: Instrument #%d has been closed", (int)instrument_id);
    return;
  }

  if (strcmp(plugin->type->type_name, "Sample Player")) {
    handleError("setInstrumentSample: instrument %d is not a Sample Player", (int)instrument_id);
    return;
  }

  ADD_UNDO(PluginState(patch, NULL));
  
  SAMPLER_set_new_sample(plugin, STRING_create(filename), -1);
}

void setRandomInstrumentSample(instrument_t instrument_id, const_char *path){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("setRandomInstrumentSample: Instrument #%d has been closed", (int)instrument_id);
    return;
  }

  if (strcmp(plugin->type->type_name, "Sample Player")) {
    handleError("setRandomInstrumentSample: instrument %d is not a Sample Player", (int)instrument_id);
    return;
  }

  if (path==NULL || path[0]==0)
    path = NULL;

  ADD_UNDO(PluginState(patch, NULL));
  
  SAMPLER_set_random_sample(plugin, path==NULL ? NULL : STRING_create(path));
}

void setRandomSampleForAllSelectedInstruments(void){
  S7CALL2(void_void, "set-random-sample-for-all-selected-sampler-instruments");
}
  
void setInstrumentLoopData(instrument_t instrument_id, int64_t start, int64_t length){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("setInstrumentLoopData: Instrument #%d has been closed", (int)instrument_id);
    return;
  }

  if (strcmp(plugin->type->type_name, "Sample Player")) {
    handleError("setInstrumentLoopData: instrument %d is not a Sample Player", (int)instrument_id);
    return;
  }


  SAMPLER_set_loop_data(plugin, start, length);
}

const_char *getInstrumentName(instrument_t instrument_id) {
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return "";

  return patch->name;
}

void setInstrumentName(const_char *name, instrument_t instrument_id) {
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

const_char* getInstrumentComment(instrument_t instrument_id) {
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return "";

  if (patch->comment==NULL)
    return "";

  return patch->comment;
}

void setInstrumentComment(const_char* comment, instrument_t instrument_id){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  patch->comment = talloc_strdup(comment);
}

bool instrumentNameWasAutogenerated(instrument_t instrument_id){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;

  return !patch->name_is_edited;
}

void API_setInstrumentColor(const_char *colorname, instrument_t instrument_id, bool create_undo){

  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  if (create_undo)
    ADD_UNDO(InstrumentColor(instrument_id));
  
  unsigned int color = GFX_get_color_from_colorname(colorname);
  patch->color = color;

  if (patch->instrument==get_audio_instrument()){
    struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
    if (plugin==NULL){
      handleError("setInstrumentColor: Instrument #%d has been closed", (int)instrument_id);
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

void setInstrumentColor(const_char *colorname, instrument_t instrument_id){
  API_setInstrumentColor(colorname, instrument_id, true);
}

void generateNewInstrumentColor(instrument_t instrument_id, float mix_background){
  const char *new_color = generateNewColor(mix_background);
  setInstrumentColor(new_color, instrument_id);
}
  
void generateNewColorForAllSelectedInstruments(float mix_background){
  S7CALL2(void_float,"FROM-C-generate-new-color-for-all-selected-instruments", mix_background);
}
  
const char *getInstrumentColor(instrument_t instrument_id, bool get_displayed_color){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return "";

  QColor color = get_displayed_instrument_color(patch);
 
  return talloc_strdup(color.name(QColor::HexArgb).toUtf8());
}

bool instrumentIsImplicitlyMuted(instrument_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("instrumentIsImplicitlyMuted: Instrument #%d has been closed", (int)instrument_id);
    return false;
  }

  return plugin->is_implicitly_muted;
}

void setInstrumentIsImplicitlyMuted(bool doit, instrument_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("setInstrumentIsImplicitlyMuted: Instrument #%d has been closed", (int)instrument_id);
    return;
  }

  if (plugin->is_implicitly_muted == doit)
    return;
  
  plugin->is_implicitly_muted = doit;
  CHIP_update(plugin);
  SEQUENCER_update(SEQUPDATE_HEADERS|SEQUPDATE_RIGHT_PART);
}

bool instrumentIsImplicitlySoloed(instrument_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("instrumentIsImplicitlySoloed: Instrument #%d has been closed", (int)instrument_id);
    return false;
  }

  return plugin->is_implicitly_soloed;
}

void setInstrumentIsImplicitlySoloed(bool doit, instrument_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("setInstrumentIsImplicitlySoloed: Instrument #%d has been closed", (int)instrument_id);
    return;
  }


  if (plugin->is_implicitly_soloed == doit)
    return;
  
  plugin->is_implicitly_soloed = doit;
  CHIP_update(plugin);
  SEQUENCER_update(SEQUPDATE_HEADERS|SEQUPDATE_RIGHT_PART);
}


float getInstrumentEffect(instrument_t instrument_id, const_char* effect_name){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("getInstrumentEffect: Instrument #%d has been closed", (int)instrument_id);
    return 0.0;
  }

  return PLUGIN_get_effect_from_name(plugin, effect_name, VALUE_FROM_PLUGIN, EFFECT_FORMAT_SCALED);
}

float getNativeInstrumentEffect(instrument_t instrument_id, const_char* effect_name){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("getNativeInstrumentEffect: Instrument #%d has been closed", (int)instrument_id);
    return 0.0;
  }

  return PLUGIN_get_effect_from_name(plugin, effect_name, VALUE_FROM_PLUGIN, EFFECT_FORMAT_NATIVE);
}

float getStoredInstrumentEffect(instrument_t instrument_id, const_char* effect_name){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("getStoredInstrumentEffect: Instrument #%d has been closed", (int)instrument_id);
    return 0.0;
  }

  return PLUGIN_get_effect_from_name(plugin, effect_name, VALUE_FROM_STORAGE, EFFECT_FORMAT_SCALED);
}

static int get_effect_num(const struct Patch *patch, const_char* effect_name){
  char *error_message = NULL;
  int effect_num = PATCH_get_effect_num(patch, effect_name, &error_message);
  if (effect_num==-1)
    handleError("%s", error_message);
  return effect_num;
}

float getDefaultInstrumentEffect(instrument_t instrument_id, const_char* effect_name){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("getDefaultInstrumentEffect: Instrument #%d has been closed", (int)instrument_id);
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
                       
void setInstrumentEffect(instrument_t instrument_id, const char *effect_name, float value){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("setInstrumentEffect: Instrument #%d has been closed", (int)instrument_id);
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

void resetInstrumentEffect(instrument_t instrument_id, const char *effect_name){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("resetInstrumentEffect: Instrument #%d has been closed", (int)instrument_id);
    return;
  }

  int effect_num = get_effect_num(patch, effect_name);
  if (effect_num < 0)
    return;

  PLUGIN_reset_one_effect(plugin, effect_num);

  post_set_effect(patch, plugin, effect_name);
}

void deletePdController(instrument_t instrument_id, const char *effect_name){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("deletePdController: Instrument #%d has been closed", (int)instrument_id);
    return;
  }

  if (strcmp(plugin->type->type_name, "Pd")){
    handleError("deletePdController: Instrument #%d is not a Pd instrument: \"%s\"", (int)instrument_id, plugin->type->type_name);
    return;
  }
  
  int effect_num = get_effect_num(patch, effect_name);
  if (effect_num < 0)
    return;

  PD_delete_controller(plugin, effect_num);
}

const_char* getInstrumentEffectColor(instrument_t instrument_id, const_char* effect_name){
  const struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return "";

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("getInstrumentEffectColor: Instrument #%d has been closed", (int)instrument_id);
    return "";
  }

  int effect_num = get_effect_num(patch, effect_name);
  if(effect_num==-1)
    return "";

  return GFX_get_colorname_from_color(GFX_get_color(get_effect_color(plugin, effect_num)));
}

bool instrumentAlwaysReceiveMidiInput(instrument_t instrument_id){
  const struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;
  
  return ATOMIC_GET(patch->always_receive_midi_input);
}

void setInstrumentAlwaysReceiveMidiInput(instrument_t instrument_id, bool always_receive_midi_input){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;
  
  ATOMIC_SET(patch->always_receive_midi_input, always_receive_midi_input);
}

bool getNoteDuplicatorSetNewValueImmediately(instrument_t instrument_id, const_char* effect_name){
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

void setNoteDuplicatorSetNewValueImmediately(instrument_t instrument_id, const_char* effect_name, bool set_immediately){
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

bool instrumentEffectHasMidiLearn(instrument_t instrument_id, const_char* effect_name){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;
  
  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("instrumentEffectHasMidiLearn: Instrument #%d has been closed", (int)instrument_id);
    return false;
  }

  int effect_num = get_effect_num(patch, effect_name);
  if(effect_num==-1)
    return false;
  
  return PLUGIN_has_midi_learn(plugin, effect_num);
}

void addInstrumentEffectMidiLearn(instrument_t instrument_id, const_char* effect_name){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;
  
  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("addInstrumentEffectMidiLearn: Instrument #%d has been closed", (int)instrument_id);
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

void removeInstrumentEffectMidiLearn(instrument_t instrument_id, const_char* effect_name){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;
  
  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("removeInstrumentEffectMidiLearn: Instrument #%d has been closed", (int)instrument_id);
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



bool addAutomationToCurrentEditorTrack(instrument_t instrument_id, const_char* effect_name){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("addAutomationToCurrentEditorTrack: Instrument #%d has been closed", (int)instrument_id);
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

  undoFxs(tracknum, blocknum, -1);

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

bool addAutomationToCurrentSequencerTrack(instrument_t instrument_id, const_char* effect_name){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("addAutomationToCurrentSequencerTrack: Instrument #%d has been closed", (int)instrument_id);
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
  
void setInstrumentEffectChangesValueWhenPressingRandom(instrument_t instrument_id, const_char* effect_name, bool doit){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("setInstrumentEffectChangesValueWhenPressingRandom: Instrument #%d has been closed", (int)instrument_id);
    return;
  }

  int effect_num = get_effect_num(patch, effect_name);
  if (effect_num < 0)
    return;

  PLUGIN_set_random_behavior(plugin, effect_num, doit);
  
  (*patch->instrument->PP_Update)(patch->instrument,patch,false);
}

bool getInstrumentEffectChangesValueWhenPressingRandom(instrument_t instrument_id, const_char* effect_name){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("getInstrumentEffectChangesValueWhenPressingRandom: Instrument #%d has been closed", (int)instrument_id);
    return false;
  }

  int effect_num = get_effect_num(patch, effect_name);
  if (effect_num < 0)
    return false;

  return PLUGIN_get_random_behavior(plugin, effect_num);
}

void startRecordingInstrumentAutomationInEditor(instrument_t instrument_id, const_char* effect_name, bool do_start_recording_not_stop_recording){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("startRecordingInstrumentAutomationInEditor: Instrument #%d has been closed", (int)instrument_id);
    return;
  }

  int effect_num = get_effect_num(patch, effect_name);
  if (effect_num < 0)
    return;

  PLUGIN_set_recording_automation(plugin, effect_num, do_start_recording_not_stop_recording);
}

bool getInstrumentSolo(instrument_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("getInstrumentSolo: Instrument #%d has been closed", (int)instrument_id);
    return false;
  }
  
  return PLUGIN_get_soloed(plugin);
}

void setInstrumentSolo(bool do_solo, instrument_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("setInstrumentSolo: Instrument #%d has been closed", (int)instrument_id);
    return;
  }

  if (do_solo==PLUGIN_get_soloed(plugin))
    return;

  int effect_num = plugin->type->num_effects + EFFNUM_SOLO_ONOFF;

  ADD_UNDO(AudioEffect_CurrPos(patch, effect_num, AE_NO_FLAGS));

  PLUGIN_set_soloed(plugin, do_solo);
}

bool switchInstrumentSolo(instrument_t instrument_id){
  bool set_to = !getInstrumentSolo(instrument_id);
  setInstrumentSolo(set_to, instrument_id);
  return set_to;
}
  
bool getInstrumentMute(instrument_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("getInstrumentMute: Instrument #%d has been closed", (int)instrument_id);
    return false;
  }
  
  return PLUGIN_get_muted(plugin);
}

void setInstrumentMute(bool do_mute, instrument_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
  int effect_num = get_mute_effectnum(plugin->type);

  ADD_UNDO(AudioEffect_CurrPos(patch, effect_num, AE_NO_FLAGS));

  PLUGIN_set_muted(plugin, do_mute);
}

bool switchInstrumentMute(instrument_t instrument_id){
  bool set_to = !getInstrumentMute(instrument_id);
  setInstrumentMute(set_to, instrument_id);
  return set_to;
}
  
bool getInstrumentBypass(instrument_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("getInstrumentBypass: Instrument #%d has been closed", (int)instrument_id);
    return false;
  }

  return PLUGIN_get_effect_value(plugin, plugin->type->num_effects + EFFNUM_EFFECTS_ONOFF, VALUE_FROM_PLUGIN) < 0.5;
}

void setInstrumentBypass(bool do_bypass, instrument_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
  int num_effects = plugin->type->num_effects;
  int effect_num = num_effects+EFFNUM_EFFECTS_ONOFF;

  bool is_bypassed = PLUGIN_get_effect_value(plugin, effect_num, VALUE_FROM_PLUGIN) < 0.5;

  //printf("  old val: %f. is_bypassed: %d. do_bypass: %d\n", PLUGIN_get_effect_value(plugin, effect_num, VALUE_FROM_PLUGIN), is_bypassed, do_bypass);

  if(is_bypassed==do_bypass)
    return;
     
  ADD_UNDO(AudioEffect_CurrPos((struct Patch*)patch, effect_num, AE_NO_FLAGS));
  
  float new_val = do_bypass ? 0.0 : 1.0;
  PLUGIN_set_effect_value(plugin, -1, num_effects+EFFNUM_EFFECTS_ONOFF, new_val, STORE_VALUE, FX_single, EFFECT_FORMAT_SCALED);
}

bool switchInstrumentBypass(instrument_t instrument_id){
  bool set_to = !getInstrumentBypass(instrument_id);
  setInstrumentBypass(set_to, instrument_id);
  return set_to;
}
  

void setSoloForInstruments(dynvec_t instruments, bool doit){
  S7CALL2(void_dynvec_bool, "FROM_C-set-solo-for-instruments", instruments, doit);
}
void setMuteForInstruments(dynvec_t instruments, bool doit){
  S7CALL2(void_dynvec_bool, "FROM_C-set-mute-for-instruments", instruments, doit);
}
void setBypassForInstruments(dynvec_t instruments, bool doit){
  S7CALL2(void_dynvec_bool, "FROM_C-set-bypass-for-instruments", instruments, doit);
}


void switchSoloForSelectedInstruments(void){
  S7CALL2(void_void,"FROM_C-switch-solo-for-selected-instruments");
}

void switchMuteForSelectedInstruments(void){
  S7CALL2(void_void,"FROM_C-switch-mute-for-selected-instruments");
}

void switchBypassForSelectedInstruments(void){
  S7CALL2(void_void,"FROM_C-switch-bypass-for-selected-instruments");
}

bool atLeastOneInstrumentHasSolo(void){
  VECTOR_FOR_EACH(struct Patch *, patch, &get_audio_instrument()->patches){
    SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
    if (plugin!=NULL && ATOMIC_GET(plugin->solo_is_on))
      return true;
  }END_VECTOR_FOR_EACH;

  return false;
}

void undoInstrumentEffect(instrument_t instrument_id, const char *effect_name){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("undoInstrumentEffect: Instrument #%d (\"%s\") has been closed", (int)instrument_id, patch->name);
    return;
  }

  int effect_num = get_effect_num(patch, effect_name);
  if (effect_num==-1)
    return;
  
  ADD_UNDO(AudioEffect_CurrPos(patch, effect_num, AE_NO_FLAGS));
}

#if 0
void setInstrumentVolume(instrument_t instrument_id, float volume) {
  struct Instruments *instrument = getInstrumentFromNum(instrument_id);
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL) return NULL;

  (*patch->instrument->PP_Update)(instrument,patch,false);
}

float getInstrumentVolume(instrument_t instrument_id) {
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

void setInstrumentData(instrument_t instrument_id, const_char *key, const_char *value) {
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  patch->instrument->setPatchData(patch, key, value, true);

  (*patch->instrument->PP_Update)(patch->instrument,patch,false);
}

const_char *getInstrumentData(instrument_t instrument_id, const_char *key) {
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

int getAudioInstrumentNum(instrument_t instrument_id){
  VECTOR_FOR_EACH(struct Patch *, patch, &get_audio_instrument()->patches){
    if (patch->id==instrument_id)
      return iterator666;
  }END_VECTOR_FOR_EACH;

  handleError("No instrument %d", (int)instrument_id);
  return -1;
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

bool instrumentIsSeqtrackBus(instrument_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("instrumentIsSeqtrackBus: Instrument #%d has been closed", (int)instrument_id);
    return true;
  }

  if (strcmp(plugin->type->type_name, SEQTRACKPLUGIN_NAME))
    return false;
  
  VECTOR_FOR_EACH(struct SeqTrack *, seqtrack, &root->song->seqtracks){
    if (seqtrack->patch==patch)
      return seqtrack->is_bus;
  }END_VECTOR_FOR_EACH;

  return false;
}

bool instrumentIsPermanent(instrument_t instrument_id){  
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

bool instrumentIsAudio(instrument_t instrument_id){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;

  return patch->instrument == get_audio_instrument();
}

// Mixer GUI
float getInstrumentX(instrument_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0.0;

  return CHIP_get_pos_x(patch);
}

float getInstrumentY(instrument_t instrument_id){
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

void setInstrumentPosition(float x, float y, instrument_t instrument_id, bool auto_slot){
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

void autopositionInstrument(instrument_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  ADD_UNDO(ChipPos_CurrPos(patch));
  CHIP_autopos(patch);
}

const_char* getInstrumentTypeName(instrument_t instrument_id){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return "";

  if (patch->instrument==get_MIDI_instrument())
    return "MIDI";

  struct SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
  return plugin->type->type_name;
}

const_char* getInstrumentPluginName(instrument_t instrument_id){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return "";

  if (patch->instrument==get_MIDI_instrument())
    return "MIDI";

  struct SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
  return plugin->type->name;
}

const_char* getInstrumentInfo(instrument_t instrument_id){
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

dynvec_t getSelectedInstruments(void){
  dynvec_t ret = {};
  vector_t patches = MW_get_selected_patches();

  VECTOR_FOR_EACH(struct Patch *, patch,&patches){
    DYNVEC_push_back(&ret, DYN_create_instrument(patch->id));
  }END_VECTOR_FOR_EACH;

  return ret;
}

dynvec_t getExtendedSelectedInstruments(void){
  dynvec_t ret = getSelectedInstruments();

  if (ret.num_elements==0){

    int64_t curr = getCurrentInstrument();
    if (curr >= 0)
      DYNVEC_push_back(&ret, DYN_create_instrument(curr));

  }
  
  return ret;
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

bool instrumentIsSelected(instrument_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("instrumentIsSelected: Instrument #%d has been closed", (int)instrument_id);
    return false;
  }  

  return ATOMIC_GET(plugin->is_selected);
}



// connections

void connectAudioInstrumentToMainPipe(instrument_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("connectAudioInstrumentToMainPipe: Instrument #%d has been closed", (int)instrument_id);
    return;
  }

  ADD_UNDO(MixerConnections_CurrPos());
  MW_connect_plugin_to_main_pipe(plugin);
}

bool autoconnectInstrument(instrument_t instrument_id, float x, float y){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("autoconnectInstrument: Instrument #%d has been closed", (int)instrument_id);
    return false;
  }

  ADD_UNDO(MixerConnections_CurrPos());
  return MW_autoconnect(patch, x, y);
}

int getNumInAudioConnections(instrument_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  return CHIP_get_num_in_connections(patch);
}

int getNumInEventConnections(instrument_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  return CHIP_get_num_in_econnections(patch);
}

int getNumOutAudioConnections(instrument_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  return CHIP_get_num_out_connections(patch);
}

int getNumOutEventConnections(instrument_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  return CHIP_get_num_out_econnections(patch);
}

int64_t getAudioConnectionSourceInstrument(int connectionnum, instrument_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  struct Patch *source = CHIP_get_source(patch, connectionnum);
  if (source == NULL)
    return 0;
    
  return source->id;
}

instrument_t getEventConnectionSourceInstrument(int connectionnum, instrument_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  struct Patch *source = CHIP_get_esource(patch, connectionnum);
  if (source == NULL)
    return 0;
    
  return source->id;
}

instrument_t getAudioConnectionDestInstrument(int connectionnum, instrument_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  struct Patch *dest = CHIP_get_dest(patch, connectionnum);
  if (dest == NULL)
    return 0;
  
  return dest->id;
}

int64_t getEventConnectionDestInstrument(int connectionnum, instrument_t instrument_id){
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

// TODO: Note: Currently never used. changeAudioConnections is used instead. TODO: If used, MW_connect should have a gain argument so that the volume is set immediately. May be used by 3rd parties though, so should probably fix this as soon as possible.
void createAudioConnection(int64_t source_id, int64_t dest_id, float gain, int connection_type){
  struct Patch *source = getAudioPatchFromNum(source_id);
  if(source==NULL)
    return;

  struct Patch *dest = getAudioPatchFromNum(dest_id);
  if(dest==NULL)
    return;
  
  MW_connect(source, dest, get_connection_type_from_int(connection_type)); 

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
    handleError("deleteAudioConnection: Could not find audio connection between \"%s\" and \"%s\"", source->name, dest->name);
}

bool changeAudioConnections(dynvec_t changes){
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
    handleError("deleteEventConnection: Could not find event connection between \"%s\" and \"%s\"", source->name, dest->name);
}

bool canAudioConnect(int64_t source_id, int64_t dest_id){
  struct Patch *source = getAudioPatchFromNum(source_id);
  if(source==NULL)
    return false;

  struct Patch *dest = getAudioPatchFromNum(dest_id);
  if(dest==NULL)
    return false;

  return CONNECTION_can_connect(source, dest);
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

static bool get_connection_gain_enabled(const char *funcname, int64_t source_id, int64_t dest_id, float *gain, bool *is_enabled, bool show_error_if_not_connected){
  struct Patch *source = getAudioPatchFromNum(source_id);
  if(source==NULL)
    return false;

  struct Patch *dest = getAudioPatchFromNum(dest_id);
  if(dest==NULL)
    return false;

  const char *error = NULL;

  AudioConnection *connection = CONNECTION_find_audio_connection(source, dest);
  if (connection==NULL)
    error = "Not found";

  if (error==NULL && gain != NULL) {
    
    struct SoundProducer *source_sp = connection->from->_sound_producer;
    struct SoundProducer *dest_sp = connection->to->_sound_producer;

    *gain = SP_get_link_gain(dest_sp, source_sp, &error);
  }
  
  if (error==NULL && is_enabled!=NULL) {

      *is_enabled = connection->get_enabled();
      
  }
  
  if (error!=NULL){
    if (show_error_if_not_connected)
      handleError("%s: Could not find audio connection between instrument %d (%s) and instrument %d (%s): %s", funcname, (int)source_id, source->name, (int)dest_id, dest->name, error);
    return false;
  }

  return true;
}

float getAudioConnectionGain(int64_t source_id, int64_t dest_id, bool show_error_if_not_connected){
  float ret;
  if (get_connection_gain_enabled("getAudioConnectionGain", source_id, dest_id, &ret, NULL, show_error_if_not_connected))
    return ret;
  else
    return 0.0;
}

bool getConnectionEnabled(int64_t source_id, int64_t dest_id, bool show_error_if_not_connected){
  bool ret;
  if (get_connection_gain_enabled("getConnectioEnabled", source_id, dest_id, NULL, &ret, show_error_if_not_connected))
    return ret;
  else
    return false;
}

static void set_connection_gain_enabled(const char *funcname, int64_t source_id, int64_t dest_id, const float *gain, const bool *is_enabled, bool redraw_mixer_strips){
  struct Patch *source = getAudioPatchFromNum(source_id);
  if(source==NULL)
    return;

  struct Patch *dest = getAudioPatchFromNum(dest_id);
  if(dest==NULL)
    return;

  bool changed1 = false;
  bool changed2 = false;
  
  const char *error = NULL;

  AudioConnection *connection = CONNECTION_find_audio_connection(source, dest);
  if (connection==NULL)
    error = "Not found";

  if (error==NULL && gain != NULL) {
    struct SoundProducer *source_sp = connection->from->_sound_producer;
    struct SoundProducer *dest_sp = connection->to->_sound_producer;
    changed1 = SP_set_link_gain(dest_sp, source_sp, *gain, &error);
  }
  
  if (error==NULL && is_enabled)
    changed2 = connection->set_enabled(*is_enabled, &error);

  
  if ((changed1||changed2) && redraw_mixer_strips){
    //printf("       Remake: setAudioConnectionGain\n");
    redrawMixerStrips(false);
  }
    
  if (error!=NULL)
    handleError("%s: Could not find audio connection between instrument %d (%s) and instrument %d (%s): %s", funcname, (int)source_id, source->name, (int)dest_id, dest->name, error);
}

void setAudioConnectionGain(int64_t source_id, int64_t dest_id, float gain, bool redraw_mixer_strips){
  set_connection_gain_enabled("setAudioConnectionGain", source_id, dest_id, &gain, NULL, redraw_mixer_strips);
}

void setConnectionEnabled(int64_t source_id, int64_t dest_id, bool is_enabled, bool redraw_mixer_strips){
  set_connection_gain_enabled("setConnectionEnabled", source_id, dest_id, NULL, &is_enabled, redraw_mixer_strips);
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

int getAudioConnectionType(int64_t source_id, int64_t dest_id){

  const struct Patch *source = getAudioPatchFromNum(source_id);
  if(source==NULL)
    return get_int_from_connection_type(ConnectionType::NOT_SET);

  const struct Patch *dest = getAudioPatchFromNum(dest_id);
  if(dest==NULL)
    return get_int_from_connection_type(ConnectionType::NOT_SET);

  AudioConnection *connection = CONNECTION_find_audio_connection(source, dest);
  if (connection==NULL)
    return get_int_from_connection_type(ConnectionType::NOT_SET);

  return get_int_from_connection_type(connection->get_connection_type());
}

  
void setAudioConnectionType(int64_t source_id, int64_t dest_id, int audio_connection_type){
  const struct Patch *source = getAudioPatchFromNum(source_id);
  if(source==NULL)
    return;

  const struct Patch *dest = getAudioPatchFromNum(dest_id);
  if(dest==NULL)
    return;

  AudioConnection *connection = CONNECTION_find_audio_connection(source, dest);
  if (connection==NULL)
    return;

  connection->set_connection_type(audio_connection_type);
  remakeMixerStrips(-1);
}

void setCurrMixerConfigNum(int num){
  if (num < 0 || num >= MW_NUM_AB) {
    handleError("setCurrMixerConfigNum: Wrong number: %d", num);
    return;
  }

  MW_change_ab(num, true);
}

void resetMixerConfigNum(int num){
  if (num < -1 || num >= MW_NUM_AB) {
    handleError("setCurrMixerConfigNum: Wrong number: %d", num);
    return;
  }
  MW_reset_ab(num);
}

bool mixerConfigNumIsUsed(int num){
  if (num < 0 || num >= MW_NUM_AB) {
    handleError("mixerConfigNumIsUsed. Wrong number: %d", num);
    return false;
  }
  return MW_ab_is_used(num);
}

// modulators

int64_t createModulator(void){
  //struct Patch *curr_patch = PATCH_get_current();
  
  instrument_t instrument_id = createAudioInstrument(MODULATOR_NAME, MODULATOR_NAME, "", 0, 0, false);
  if (instrument_id==-1){
    printf("\n\n NOT FOUND\n\n");
    getchar();
    return -1;
  }
  
  //if (curr_patch != NULL)
  //  GFX_PP_Update(curr_patch, false); // Set back current instrument.
  
  const struct Patch *modulator_patch = PATCH_get_from_id(instrument_id);
  
  ADD_UNDO(ChipPos_CurrPos(modulator_patch));
  autopositionInstrument(instrument_id);

  return instrument_id;
}


bool hasModulator(instrument_t instrument_id, const char *effect_name){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;

  int effect_num = get_effect_num(patch, effect_name);
  if (effect_num==-1)
    return false;

  return MODULATOR_has_modulator(patch, effect_num);
}

static void addModulator2(instrument_t instrument_id, const char *effect_name, instrument_t modulator_instrument_id, bool supposed_to_already_have_modulator){
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

void addModulator(instrument_t instrument_id, const char *effect_name, instrument_t modulator_instrument_id){
  addModulator2(instrument_id, effect_name, modulator_instrument_id, false);
}

void replaceModulator(instrument_t instrument_id, const char *effect_name, instrument_t modulator_instrument_id){
  addModulator2(instrument_id, effect_name, modulator_instrument_id, true);
}

void removeModulator(instrument_t instrument_id, const char *effect_name){
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

  if(patch==PATCH_get_current())
    patch->instrument->PP_Update(patch->instrument, patch, false);
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

const char *getModulatorDescription2(instrument_t instrument_id, int effect_num){
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

const char *getModulatorDescription(instrument_t instrument_id, const char *effect_name){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return "";
  
  int effect_num = get_effect_num(patch, effect_name);
  if (effect_num==-1)
    return "";

  return getModulatorDescription2(instrument_id, effect_num);
}

dynvec_t getModulatorInstruments(void){
  dynvec_t dynvec = {};

  const dyn_t dynstate = MODULATORS_get_connections_state(); // Using this function as a way to get a list of all modulator instruments. (easier than filtering modulators from the list of all patches)
  const dynvec_t *vec = dynstate.array;
  
  for(int i = 0 ; i < vec->num_elements ; i++){
    dyn_t dynstate = vec->elements[i];
    
    hash_t *modulator_state = dynstate.hash;
    int64_t patch_id = HASH_get_instrument(modulator_state, "modulator_patch_id");

    DYNVEC_push_back(&dynvec, DYN_create_instrument(patch_id));
  }

  return dynvec;
}

// Note, called quite often.
dynvec_t getModulatorTargets(int64_t modulator_instrument_id){

  dynvec_t empty_ret = {};
  
  struct Patch *patch = getAudioPatchFromNum(modulator_instrument_id);
  if(patch==NULL)
    return empty_ret;

  if (!MODULATOR_is_modulator(modulator_instrument_id)){
    handleError("getModulatorTargets: Instrument #%d is not a modulator", (int)modulator_instrument_id);
    return empty_ret;
  }

  return MODULATOR_get_modulator_targets(modulator_instrument_id);
}

void setModulatorEnabled(instrument_t instrument_id, const char *effect_name, bool enabled){
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

bool getModulatorEnabled(instrument_t instrument_id, const char *effect_name){
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
void addModulator(instrument_t instrument_id, const char *effect_name, int64_t modulator_instrument_id){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  if (get_modulator(instrument_id, effect_name) >= 0){
    handleError("addModulator: Effect %s in instrument \"%s\" already has a modulator: %s", effect_name, getInstrumentName(instrument_id), getModulatorDescription(modulator_instrument_id));
    return;
  }
}

void removeModulator(instrument_t instrument_id, const char *effect_name, int64_t modulator_instrument_id){
}

void replaceModulator(instrument_t instrument_id, const char *effect_name, int64_t modulator_instrument_id){
}

int64_t getModulator(instrument_t instrument_id, const char *effect_name){
}

dyn_t get_modulators_instruments(void){
}

const_char *getModulatorDescription(int64_t modulator_instrument_id){
}
*/

int getNumInputChannels(instrument_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("getNumInputChannels: Instrument #%d has been closed", (int)instrument_id);
    return 0;
  }

  return plugin->type->num_inputs;
}

int getNumOutputChannels(instrument_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  if (plugin==NULL){
    handleError("getNumOutputChannels: Instrument #%d has been closed", (int)instrument_id);
    return 0;
  }

  return plugin->type->num_outputs;
}

void deleteInstrument(instrument_t instrument_id){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  if (patch->instrument==get_audio_instrument()){

    /*
      No point. This check only prevents seqtrack with seqblocks from being deleted, which there is no reason to prevent here.

    if (AUDIO_is_permanent_patch(patch)==true){
      handleError("Instrument \"%s\" can not be deleted", patch->name);;
      return;
    }
    */
    
    SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;
    if (plugin==NULL){
      R_ASSERT_NON_RELEASE(false);
      return;
    }

    // Check if instrument is a seqtrack instrument. If so, we delete the seqtrack instead of deleting the instrument directly.
    if (!strcmp(plugin->type->type_name, SEQTRACKPLUGIN_NAME)){
      
      int seqtracknum = get_seqtracknum_from_patch(patch);
      R_ASSERT_RETURN_IF_FALSE(seqtracknum>=0);
      
      deleteSeqtrack(seqtracknum, true);
      return;
      
    }
  }
  
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

instrument_t getMainPipeInstrument(void){
  return 0;
}

bool instrumentIsOpenAndAudio(instrument_t instrument_id){
  const struct Patch *patch = instrument_id==-1 ? PATCH_get_current() : PATCH_get_from_id(instrument_id);
  if (patch==NULL)
    return false;

  return patch->instrument == get_audio_instrument();
}

bool instrumentIsOpen(instrument_t instrument_id){
  return (instrument_id==-1 ? PATCH_get_current() : PATCH_get_from_id(instrument_id)) != NULL;
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
static int64_t *playnote_ids;
static float *initial_pitches;

__attribute__((constructor)) static void initialize_playnote_ids_and_initial_pitches() {
  playnote_ids = (int64_t*)calloc(sizeof(int64_t), NUM_IDS);
  initial_pitches = (float*)calloc(sizeof(float), NUM_IDS);
};


int playNote(float pitch, float velocity, float pan, int midi_channel, instrument_t instrument_id){
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

void changeNotePitch(float pitch, int playnote_id, int midi_channel, instrument_t instrument_id){
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

void stopNote(int playnote_id, int midi_channel, instrument_t instrument_id){
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

bool hasNativeInstrumentGui(instrument_t instrument_id){
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

bool showInstrumentGui(instrument_t instrument_id, int64_t parentgui, bool show_instrument_window_if_not_visible){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;

  //struct Patch *currpatch_before = PATCH_get_current();
  
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
      
      //else if (instrument_window_is_visible && currpatch_before != PATCH_get_current())
      //  show_message = true;

      //printf("was: %d, is: %d. show: %d\n", instrument_window_was_visible, instrument_window_is_visible, show_message);

      if (show_message)
        GFX_Message2(NULL, true, "Instrument %s of type %s / %s does not have a native GUI", patch->name, plugin->type->type_name, plugin->type->name);
      
    }
    
  }

  return false;
}

bool hideInstrumentGui(instrument_t instrument_id){

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

bool instrumentGuiIsVisible(instrument_t instrument_id, int64_t parentgui){

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

void internal_instrumentGuiHasBeenHidden(instrument_t instrument_id){
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
  return PATCH_get_current()->id;
}

void setCurrentInstrument(instrument_t instrument_id, bool show_instrument_window_if_not_visible){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  struct Patch *old_currpatch = PATCH_get_current();

  if (patch==old_currpatch)
    return;

  //if(showInstrumentWidgetWhenDoubleClickingSoundObject())
  if(show_instrument_window_if_not_visible)
    GFX_InstrumentWindowToFront();
  
  patch->instrument->PP_Update(patch->instrument, patch, false);

  if (instrumentIsOpenAndAudio(instrument_id)){
    Chip *chip = CHIP_get(NULL, patch);
    R_ASSERT(chip!=NULL);
    if (chip!=NULL)
      MW_set_selected_chip(chip); // To unselect previously selected chip. (if not the program looks buggy)
  }
                       
  /*
  if(old_currpatch!=NULL && instrumentIsOpenAndAudio(old_currpatch->id)){
    struct SoundPlugin *plugin = (struct SoundPlugin*)old_currpatch->patchdata;
    R_ASSERT(plugin!=NULL);
    if (plugin!=NULL)
      CHIP_update(plugin);
  }
  */
}

void setCurrentInstrumentLeft(void){
  S7CALL2(void_void,"FROM_C-move-current-instrument-left");
}

void setCurrentInstrumentRight(void){
  S7CALL2(void_void,"FROM_C-move-current-instrument-right");
}

void setCurrentInstrumentUp(void){
  S7CALL2(void_void,"FROM_C-move-current-instrument-up");
}

void setCurrentInstrumentDown(void){
  S7CALL2(void_void,"FROM_C-move-current-instrument-down");
}

void showInstrumentInfo(dyn_t instrument_id_or_description, int64_t parentgui){
  if (instrument_id_or_description.type==INSTRUMENT_TYPE){
    
    instrument_t instrument_id = instrument_id_or_description.instrument;
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
    
    handleError("Illegal first argument for showInstrumentInfo. Expected INSTRUMENT_TYPE or STRING_TYPE, found %s", DYN_type_name(instrument_id_or_description.type));
      
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

namespace{
  
  struct EffectMonitor{
    int64_t id = 0;
    
    radium::GcHolder<struct Patch> patch;
    
    instrument_t instrument_id = 0;
    bool monitor_stored = 0;
    bool monitor_automation = 0;
    int effect_num = 0;
    radium::ProtectedS7Extra<func_t*> func = radium::ProtectedS7Extra<func_t*>("effectmonitor");
    
    float last_stored_value = 0;
    float last_automation_value = 0;
  };

 static int64_t g_effect_monitor_id = 0;
 static QVector<EffectMonitor*> g_effect_monitors;
 
}

static struct EffectMonitor *find_effect_monitor_from_id(int64_t id){
  for(auto *monitor : g_effect_monitors)
    if (monitor->id==id)
      return monitor;

  return NULL;
}

/*
static struct EffectMonitor *find_effect_monitor(int effect_num, instrument_t instrument_id){
  VECTOR_FOR_EACH(struct EffectMonitor *effect_monitor, &g_effect_monitors){
    if (effect_monitor->effect_num==effect_num && effect_monitor->instrument_id==instrument_id)
      return effect_monitor;
  }END_VECTOR_FOR_EACH;

  return NULL;
}
*/

int64_t addEffectMonitor(const char *effect_name, instrument_t instrument_id, bool monitor_stored, bool monitor_automation, func_t *func){
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
    
  struct EffectMonitor *effect_monitor = new EffectMonitor;

  effect_monitor->id = g_effect_monitor_id++;
  effect_monitor->patch.set(patch);
  
  effect_monitor->effect_num = effect_num;
  effect_monitor->instrument_id = instrument_id;
  effect_monitor->monitor_stored = monitor_stored;
  effect_monitor->monitor_automation = monitor_automation;
  effect_monitor->func.set(func);

  effect_monitor->last_stored_value = 0;
  effect_monitor->last_automation_value = -10;

  g_effect_monitors.push_back(effect_monitor);

#if !defined(RELEASE)
  printf("   addEffectMonitor: Size: %d\n", g_effect_monitors.size());
#endif
  
  return effect_monitor->id;
}

void removeEffectMonitor(int64_t effect_monitor_id, bool throw_exception_if_not_found){
  struct EffectMonitor *effect_monitor = find_effect_monitor_from_id(effect_monitor_id);
  
  if (effect_monitor==NULL){
    
    if (throw_exception_if_not_found)
      handleError("No effect monitor #%d", (int)effect_monitor_id);
    
    return;
  }

  int num = g_effect_monitors.removeAll(effect_monitor);
  R_ASSERT(num==1);

  delete effect_monitor;
}

void API_remove_effect_monitors_for_instrument(struct Patch *patch){
 again:
  
  for(auto *monitor : g_effect_monitors){
    if (monitor->patch.data()==patch){
      
      int num = g_effect_monitors.removeAll(monitor);
      R_ASSERT(num==1);
      
      delete monitor;
      
      goto again;
    }
  }  
}

void API_instruments_call_regularly(void){
  int i = 0;
  
  while(i < g_effect_monitors.size()){
    
    EffectMonitor *effect_monitor = g_effect_monitors.at(i);
    
    struct Patch *patch = effect_monitor->patch.data();

    {
      struct Patch *patch2 = PATCH_get_from_id(patch->id);
      if (patch2 != patch) {      
        R_ASSERT_NON_RELEASE(patch2==NULL);
        R_ASSERT_NON_RELEASE(false);
        g_effect_monitors.removeAt(i);
        printf("    WARNING: removeEffectMonitor not called for deleted instrument \"%s\". Removing automatically.\n", patch->name);
        continue;
      }
    }
    
    struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
    
    if(plugin==NULL){

#if !defined(RELEASE)
      printf("   API_instruments_call_regularly: SoundPlugin for instrument \"%s\" is NULL\n", patch->name);
#endif
      
    } else {

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
               effect_monitor->func.v,
               send_stored ? DYN_create_float(stored_now) : g_dyn_false,
               send_automation ? DYN_create_float(automation_now) : g_dyn_false
               );
      }
    }

    i++;
  }
}



// Mixer strips
////////////////////////////////////////////////

void redrawMixerStrips(bool immediately){
  if (immediately)
    S7CALL2(void_void, "FROM_C-redraw-mixer-strips");
  else
    RT_schedule_mixer_strips_redraw(); // We don't want to redraw immediately in case we remake when a connection is being deleted or created, and we don't want to remake several times in a row either, or too often.
}

void remakeMixerStrips(int64_t id){
  RT_schedule_mixer_strips_remake(id); // We don't want to redraw immediately in case we remake when a connection is being deleted or created, and we don't want to remake several times in a row either, or too often.
}

bool hasWideInstrumentStrip(instrument_t instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id); // calling getAudioPatchFromNum instead of getPatchFromNum works as an assertion
  if(patch==NULL)
    return true;

  return patch->wide_mixer_strip;
}

void setWideInstrumentStrip(instrument_t instrument_id, bool is_wide){
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

void setIncludePanAndDryInWetSignal(bool val){
  if(root->song->include_pan_and_dry_in_wet_signal != val){
    radium::PlayerLock lock;
    root->song->include_pan_and_dry_in_wet_signal = val;
  }
}
  
bool includePanAndDryInWetSignal(void){
  return root->song->include_pan_and_dry_in_wet_signal;
}
