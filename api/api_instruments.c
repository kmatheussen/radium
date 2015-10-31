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

#include "../midi/midi_i_plugin.h"
#include "../midi/midi_i_plugin_proc.h"
#include "../midi/midi_i_input_proc.h"
#include "../midi/midi_menues_proc.h"

#include "../audio/SoundPlugin.h"
#include "../audio/SoundPlugin_proc.h"
#include "../audio/SoundPluginRegistry_proc.h"
#include "../audio/Mixer_proc.h"
#include "../audio/Sampler_plugin_proc.h"

#include "../Qt/Qt_instruments_proc.h"

#include "../common/patch_proc.h"
#include "../common/instruments_proc.h"

#include "api_common_proc.h"


extern struct Root *root;

// Warning, All these functions (except selectPatchForTrack) must be called via python (does not update graphics, or handle undo/redo))
// (TODO: detect this automatically.)


void selectPatchForTrack(int tracknum,int blocknum,int windownum){
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

  PATCH_select_patch_for_track(window,wtrack,false);
}

void setInstrumentForTrack(int instrument_num, int tracknum, int blocknum, int windownum){
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

  struct Patch *patch = PATCH_get_from_id(instrument_num);
  if(patch==NULL){
    RError("instrument %d not found", instrument_num);
    return;
  }

  wtrack->track->patch = patch;

  wblock->block->is_dirty = true;

  (*patch->instrument->PP_Update)(patch->instrument,patch);
}

int createMIDIInstrument(char *name) {
  struct Patch *patch = NewPatchCurrPos(MIDI_INSTRUMENT_TYPE, NULL, name);
  GFX_PP_Update(patch);
  return patch->id;
}

int createAudioInstrument(char *type_name, char *plugin_name, char *name) {
  SoundPluginType *type = PR_get_plugin_type_by_name(NULL, type_name, plugin_name);
  if (type==NULL){
    GFX_Message(NULL, "Audio plugin %s / %s not found", type_name, plugin_name);
    return -1;
  }

  SoundPlugin *plugin = add_new_audio_instrument_widget(type,-100000,-100000,true,talloc_strdup(name),MIXER_get_buses());
  if (plugin==NULL)
    return -1;

  struct Patch *patch = (struct Patch*)plugin->patch;
  if (patch==NULL){
    RError(NULL, "Plugin contains no patch");
    return -1;
  }

  GFX_PP_Update(patch);
  return patch->id;
}

void setInstrumentSample(int instrument_num, char *filename){
  struct Patch *patch = PATCH_get_from_id(instrument_num);
  if(patch==NULL){
    GFX_Message(NULL, "instrument %d not found", instrument_num);
    return;
  }

  if (patch->instrument != get_audio_instrument()) {
    GFX_Message(NULL, "instrument %d is not an audio instrument", instrument_num);
    return;
  }

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;

  if (strcmp(plugin->type->name, "Sample Player")) {
    GFX_Message(NULL, "instrument %d is not a Sample Player plugin", instrument_num);
    return;
  }


  SAMPLER_set_new_sample(plugin, STRING_create(filename), -1);
}

void setInstrumentLoopData(int instrument_num, int start, int length){
  struct Patch *patch = PATCH_get_from_id(instrument_num);
  if(patch==NULL){
    GFX_Message(NULL, "instrument %d not found", instrument_num);
    return;
  }

  if (patch->instrument != get_audio_instrument()) {
    GFX_Message(NULL, "instrument %d is not an audio instrument", instrument_num);
    return;
  }

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;

  if (strcmp(plugin->type->name, "Sample Player")) {
    GFX_Message(NULL, "instrument %d is not a Sample Player plugin", instrument_num);
    return;
  }


  SAMPLER_set_loop_data(plugin, start, length);

  GFX_update_instrument_widget(patch);

}


void setInstrumentName(int instrument_num, char *name) {
  struct Patch *patch = PATCH_get_from_id(instrument_num);
  if(patch==NULL){
    GFX_Message(NULL, "instrument %d not found", instrument_num);
    return;
  }

  patch->name = talloc_strdup(name);

  (*patch->instrument->PP_Update)(patch->instrument,patch);
}

void setInstrumentEffect(int instrument_num, char *effect_name, float value){
  struct Patch *patch = PATCH_get_from_id(instrument_num);
  if(patch==NULL){
    GFX_Message(NULL, "instrument %d not found", instrument_num);
    return;
  }

  if (patch->instrument != get_audio_instrument()) {
    GFX_Message(NULL, "instrument %d is not an audio instrument", instrument_num);
    return;
  }

  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;

  if (strcmp(plugin->type->name, "Sample Player")) {
    GFX_Message(NULL, "instrument %d is not a Sample Player plugin", instrument_num);
    return;
  }

  PLUGIN_set_effect_from_name(plugin, effect_name, value);

  GFX_update_instrument_widget(patch);
}

char *getInstrumentName(int instrument_num) {
  struct Patch *patch = PATCH_get_from_id(instrument_num);
  if(patch==NULL){
    GFX_Message(NULL, "instrument %d not found", instrument_num);
    return NULL;
  }

  return (char*)patch->name;
}

#if 0
void setInstrumentVolume(int instrument_num, float volume) {
  struct Instruments *instrument = getInstrumentFromNum(instrument_num);
  struct Patch *patch = getPatchFromNum(instrument_num);
  if(patch==NULL) return NULL;

  (*patch->instrument->PP_Update)(instrument,patch);
}

float getInstrumentVolume(int instrument_num) {
  return 0.0f;
}
#endif

void setInstrumentData(int instrument_num, char *key, char *value) {
  struct Patch *patch = PATCH_get_from_id(instrument_num);
  if(patch==NULL){
    GFX_Message(NULL, "instrument %d not found", instrument_num);
    return;
  }

  patch->instrument->setPatchData(patch, key, value);

  (*patch->instrument->PP_Update)(patch->instrument,patch);
}

char *getInstrumentData(int instrument_num, char *key) {
  struct Patch *patch = PATCH_get_from_id(instrument_num);
  if(patch==NULL){
    GFX_Message(NULL, "instrument %d not found", instrument_num);
    return NULL;
  }

  return patch->instrument->getPatchData(patch, key);
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
