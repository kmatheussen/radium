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

#include "../embedded_scheme/s7extra_proc.h"

#include "../midi/midi_i_plugin.h"
#include "../midi/midi_i_plugin_proc.h"
#include "../midi/midi_i_input_proc.h"
#include "../midi/midi_menues_proc.h"

#include "../audio/SoundPlugin.h"
#include "../audio/SoundPlugin_proc.h"
#include "../audio/SoundProducer_proc.h"
#include "../audio/SoundPluginRegistry_proc.h"
#include "../audio/Mixer_proc.h"
#include "../audio/Sampler_plugin_proc.h"
#include "../audio/audio_instrument_proc.h"

#include "../mixergui/QM_MixerWidget.h"
#include "../mixergui/QM_chip.h"
#include "../mixergui/undo_chip_position_proc.h"
//#include "../mixergui/undo_chip_addremove_proc.h"
#include "../mixergui/undo_mixer_connections_proc.h"
#include "../mixergui/undo_mixer_proc.h"

#include "../Qt/Qt_instruments_proc.h"

#include "../common/patch_proc.h"
#include "../common/instruments_proc.h"

#include "api_common_proc.h"


extern struct Root *root;

// Warning, All these functions (except selectPatchForTrack) must be called via python (does not update graphics, or handle undo/redo))
// (TODO: detect this automatically.)


void selectInstrumentForTrack(int tracknum){
  s7extra_callFunc2_void_int("select-track-instrument", tracknum);
}

void replaceInstrument(int instrument_id, const_char* instrument_description){
  s7extra_callFunc2_void_int_charpointer("replace-instrument", instrument_id, instrument_description);
}

void loadInstrumentPreset(int instrument_id, const_char* instrument_description){
  s7extra_callFunc2_void_int_charpointer("load-instrument-preset", instrument_id, instrument_description);
}

int getInstrumentForTrack(int tracknum, int blocknum, int windownum){
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

void setInstrumentForTrack(int instrument_id, int tracknum, int blocknum, int windownum){
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

  ADD_UNDO(Track(window,wblock,wtrack,wblock->curr_realline));

  PLAYER_lock();{
    
    if (old_patch != NULL)
      handle_fx_when_theres_a_new_patch_for_track(wtrack->track, old_patch, new_patch);
    
    wtrack->track->patch = new_patch;
    
  }PLAYER_unlock();
  
  wblock->block->is_dirty = true;

  (*new_patch->instrument->PP_Update)(new_patch->instrument,new_patch);
}

int createMIDIInstrument(char *name) {
  struct Patch *patch = PATCH_create_midi(name);
  GFX_PP_Update(patch);
  return patch->id;
}

// There was a good reason for the 'name' parameter. Think it had something to do with replace instrument, and whether to use old name or autogenerate new one.
int createAudioInstrument(char *type_name, char *plugin_name, char *name) {
  if (name!=NULL && strlen(name)==0)
    name = NULL;

  struct Patch *patch = PATCH_create_audio(type_name, plugin_name, name, NULL);
  if (patch==NULL)
    return -1;

  return patch->id;
}

static hash_t *get_preset_state_from_filename(const wchar_t *filename){
  //last_preset_path = QFileInfo(filename).absoluteDir().path();
  
  disk_t *file = DISK_open_for_reading(filename);
  if(file==NULL){
    GFX_Message(NULL, "Could not open file.");
    return NULL;
  }

  hash_t *state = HASH_load(file);
  DISK_close_and_delete(file);

  if(state==NULL){
    GFX_Message(NULL, "File does not appear to be a valid effects settings file");
    return NULL;
  }

  //last_filename = QFileInfo(filename).baseName();

  return state;
}

static int createAudioInstrumentFromPreset2(const wchar_t *filename, char *name) {
  if (name!=NULL && strlen(name)==0)
    name = NULL;

  hash_t *state = get_preset_state_from_filename(filename);

  InstrumentWidget_set_last_used_preset_filename(filename);
  
  struct Patch *patch = PATCH_create_audio(NULL, NULL, name, state);
  if (patch==NULL)
    return -1;

  return patch->id;
}

int createAudioInstrumentFromPreset(const char *filename, char *name) {
  return createAudioInstrumentFromPreset2(STRING_create(filename), name);
}

int createAudioInstrumentFromDescription(const char *instrument_description, char *name){
  if (strlen(instrument_description)==0)
    return -1;

  if (name!=NULL && strlen(name)==0)
    name = NULL;

  if (instrument_description[0]=='1'){
    char *descr = talloc_strdup(instrument_description);
    int sep_pos = 1;
    while(descr[sep_pos]!=':'){
      if(descr[sep_pos]==0){
        GFX_Message(NULL, "Illegal instrument_description: %s (missing colon separator)",instrument_description);
        return -1;
      }
      sep_pos++;
    }
    descr[sep_pos] = 0;
    char *type_name = STRING_get_chars(STRING_fromBase64(STRING_create(&descr[1])));
    char *plugin_name = STRING_get_chars(STRING_fromBase64(STRING_create(&descr[sep_pos+1])));
    return createAudioInstrument(type_name, plugin_name, name);
  }

  if (instrument_description[0]=='2'){
    wchar_t *filename = STRING_fromBase64(STRING_create(&instrument_description[1]));
    //printf("filename: %s\n",filename);

    return createAudioInstrumentFromPreset2(filename, name);
  }

  GFX_Message(NULL, "Illegal instrument_description: %s (string doesn't start with '1' or '2')",instrument_description);
  return -1;  
}

int cloneAudioInstrument(int instrument_id){
  struct Patch *old_patch = getAudioPatchFromNum(instrument_id);
  if(old_patch==NULL)
    return -1;
  
  SoundPlugin *old_plugin = (struct SoundPlugin*)old_patch->patchdata;
  hash_t *state = PLUGIN_get_state(old_plugin);

  struct Patch *new_patch = PATCH_create_audio(NULL, NULL, talloc_format("Clone of %s",old_patch->name), state);
  if (new_patch==NULL)
    return -1;

  return new_patch->id;
}

void connectAudioInstrumentToMainPipe(int instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  ADD_UNDO(MixerConnections_CurrPos());
  MW_autoconnect_plugin((SoundPlugin *)patch->patchdata);
}

const_char* instrumentDescriptionPopupMenu(void){
  return MW_popup_plugin_selector2();
}

const_char* requestLoadPresetInstrumentDescription(void){
  return MW_request_load_preset_instrument_description();
}

int getNumInstrumentEffects(int instrument_id){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  return patch->instrument->getFxNames(patch)->num_elements;
}

const_char* getInstrumentEffectName(int effect_num, int instrument_id){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return "";

  vector_t *elements = patch->instrument->getFxNames(patch);
  
  if (effect_num >= elements->num_elements){
    RError("effect_num >= num_effects: %d >= %d",effect_num, elements->num_elements);
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

void setInstrumentSample(int instrument_id, char *filename){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;

  if (strcmp(plugin->type->name, "Sample Player")) {
    GFX_Message(NULL, "instrument %d is not a Sample Player", instrument_id);
    return;
  }


  SAMPLER_set_new_sample(plugin, STRING_create(filename), -1);
}

void setInstrumentLoopData(int instrument_id, int start, int length){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;

  if (strcmp(plugin->type->name, "Sample Player")) {
    GFX_Message(NULL, "instrument %d is not a Sample Player", instrument_id);
    return;
  }


  SAMPLER_set_loop_data(plugin, start, length);

  GFX_update_instrument_widget(patch);

}

char *getInstrumentName(int instrument_id) {
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return "";

  return (char*)patch->name;
}

void setInstrumentName(char *name, int instrument_id) {
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  patch->name = talloc_strdup(name);
  patch->name_is_edited = true;
  
  (*patch->instrument->PP_Update)(patch->instrument,patch);
}

bool instrumentNameWasAutogenerated(int instrument_id){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;

  return !patch->name_is_edited;
}

void setInstrumentEffect(int instrument_id, char *effect_name, float value){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;

  if (strcmp(plugin->type->name, "Sample Player")) {
    GFX_Message(NULL, "instrument %d is not a Sample Player plugin", instrument_id);
    return;
  }

  PLUGIN_set_effect_from_name(plugin, effect_name, value);

  GFX_update_instrument_widget(patch);
}


#if 0
void setInstrumentVolume(int instrument_id, float volume) {
  struct Instruments *instrument = getInstrumentFromNum(instrument_id);
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL) return NULL;

  (*patch->instrument->PP_Update)(instrument,patch);
}

float getInstrumentVolume(int instrument_id) {
  return 0.0f;
}
#endif

void setInstrumentData(int instrument_id, char *key, char *value) {
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  patch->instrument->setPatchData(patch, key, value);

  (*patch->instrument->PP_Update)(patch->instrument,patch);
}

char *getInstrumentData(int instrument_id, char *key) {
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return "";

  return patch->instrument->getPatchData(patch, key);
}


int getNumAudioInstruments(void){
  return get_audio_instrument()->patches.num_elements;
}

int getAudioInstrumentId(int instrument_num){
  if (instrument_num>=getNumAudioInstruments()){
    RError("No instrument #%d",instrument_num);
    return -1;
  }
  struct Patch *patch = get_audio_instrument()->patches.elements[instrument_num];
  return patch->id;
}

bool instrumentIsPermanent(int instrument_id){  
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return false;

  if (patch->instrument == get_audio_instrument())
    return AUDIO_is_permanent_patch(patch);
  else
    return true; // Can not delete midi instruments.
}


// Mixer GUI
float getInstrumentX(int instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0.0;

  return CHIP_get_pos_x(patch);
}

float getInstrumentY(int instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0.0;

  return CHIP_get_pos_y(patch);
}

void setInstrumentPosition(float x, float y, int instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  ADD_UNDO(ChipPos_CurrPos(patch));
  
  CHIP_set_pos(patch,x,y);
}

int getNumInAudioConnections(int instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  return CHIP_get_num_in_connections(patch);
}

int getNumInEventConnections(int instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  return CHIP_get_num_in_econnections(patch);
}

int getNumOutAudioConnections(int instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  return CHIP_get_num_out_connections(patch);
}

int getNumOutEventConnections(int instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  return CHIP_get_num_out_econnections(patch);
}

int getAudioConnectionSourceInstrument(int connectionnum, int instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  struct Patch *source = CHIP_get_source(patch, connectionnum);
  if (source == NULL)
    return 0;
    
  return source->id;
}

int getEventConnectionSourceInstrument(int connectionnum, int instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  struct Patch *source = CHIP_get_esource(patch, connectionnum);
  if (source == NULL)
    return 0;
    
  return source->id;
}

int getAudioConnectionDestInstrument(int connectionnum, int instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  struct Patch *dest = CHIP_get_dest(patch, connectionnum);
  if (dest == NULL)
    return 0;
  
  return dest->id;
}

int getEventConnectionDestInstrument(int connectionnum, int instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return 0;

  struct Patch *dest = CHIP_get_edest(patch, connectionnum);
  if (dest == NULL)
    return 0;
  
  return dest->id;
}

void createAudioConnection(int source_id, int dest_id){
  struct Patch *source = getAudioPatchFromNum(source_id);
  if(source==NULL)
    return;

  struct Patch *dest = getAudioPatchFromNum(dest_id);
  if(dest==NULL)
    return;

  MW_connect(source, dest); 
}
                           
void createEventConnection(int source_id, int dest_id){
  struct Patch *source = getAudioPatchFromNum(source_id);
  if(source==NULL)
    return;

  struct Patch *dest = getAudioPatchFromNum(dest_id);
  if(dest==NULL)
    return;

  MW_econnect(source, dest); 
}
                           
void deleteInstrument(int instrument_id){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return;

  Undo_Open_rec();{

    PATCH_make_inactive(patch);

  }Undo_Close();

  root->song->tracker_windows->must_redraw=true;
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
  MIDISetInputPort();
}
