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

  struct Instruments *instrument = getInstrumentFromNum(instrument_num);
  struct Patch *patch = getPatchFromNum(instrument_num);
  if(patch==NULL) return;

  wtrack->track->patch = patch;

  wblock->block->is_dirty = true;

  (*instrument->PP_Update)(instrument,patch);
}

int createNewInstrument(char *type, char *name) {
  int instrument_num;

  /*
  struct Patch *patch=talloc(sizeof(struct Patch)); // ??? What is the point of this one? (This function is not finished, it's only used by the midi importer.
  patch->id = PATCH_get_new_id();
  patch->forward_events = true;
  */

  struct Instruments *instrument = NULL;

  if(!strcmp("midi", type)) {
    struct Patch *patch = NewPatchCurrPos(MIDI_INSTRUMENT_TYPE, NULL, name);
    GFX_PP_Update(patch);
    instrument = get_MIDI_instrument();
    instrument_num = 0;
  } else if(!strcmp("audio", type)) {
    abort();
//AUDIO_InitPatch(patch);
    instrument = get_audio_instrument();
    instrument_num = 1;
  }else{
    RError("Unknown instrument type '%s'.", type);
    return 0;
  }

  return getInstrumentPatchNum(instrument_num, instrument->patches.num_elements-1);
}

void setInstrumentName(int instrument_num, char *name) {
  struct Instruments *instrument = getInstrumentFromNum(instrument_num);
  struct Patch *patch = getPatchFromNum(instrument_num);
  if(patch==NULL) return;

  patch->name = talloc_strdup(name);

  (*instrument->PP_Update)(instrument,patch);
}

char *getInstrumentName(int instrument_num) {
  struct Patch *patch = getPatchFromNum(instrument_num);
  if(patch==NULL) return NULL;

  return (char*)patch->name;
}

#if 0
void setInstrumentVolume(int instrument_num, float volume) {
  struct Instruments *instrument = getInstrumentFromNum(instrument_num);
  struct Patch *patch = getPatchFromNum(instrument_num);
  if(patch==NULL) return NULL;

  (*instrument->PP_Update)(instrument,patch);
}

float getInstrumentVolume(int instrument_num) {
  return 0.0f;
}
#endif

void setInstrumentData(int instrument_num, char *key, char *value) {
  struct Instruments *instrument = getInstrumentFromNum(instrument_num);
  struct Patch *patch = getPatchFromNum(instrument_num);
  if(patch==NULL) return;

  instrument->setPatchData(patch, key, value);

  (*instrument->PP_Update)(instrument,patch);
}

char *getInstrumentData(int instrument_num, char *key) {
  struct Instruments *instrument = getInstrumentFromNum(instrument_num);
  struct Patch *patch = getPatchFromNum(instrument_num);
  if(patch==NULL) return "";

  return instrument->getPatchData(patch, key);
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
