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
#include "../midi/midi_i_plugin.h"
#include "../midi/midi_i_plugin_proc.h"
#include "../common/patch_proc.h"

#include "api_common_proc.h"


extern struct Root *root;



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

  SelectPatch(window,wtrack->track);
}


int createNewInstrument(char *type, char *name) {
  int instrument_num;
  struct Patch *patch=talloc(sizeof(struct Patch));
  struct Instruments *instrument;

  if(!strcmp("midi", type)) {
    MIDI_InitPatch(patch);
    instrument_num = 0;
#if 0
  } else if(!strcmp("audio", type)) {
    AUDIO_InitPatch(patch);
    instrument_num = 1;
#endif
  }else{
    RError("Unknown instrument type '%s'.", type);
    return 0;
  }

  patch->name = talloc_strdup(name);

  instrument = getInstrumentFromNum(instrument_num);

  ListAddElement1_ff(&instrument->patches,&patch->l);
  (*instrument->PP_Update)(instrument,patch);

  return getInstrumentPatchNum(instrument_num, patch->l.num);
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

  return patch->name;
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
  if(patch==NULL) return NULL;

  return instrument->getPatchData(patch, key);
}

