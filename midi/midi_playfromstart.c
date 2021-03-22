/* Copyright 2000 Kjetil S. Matheussen

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




#include "nsmtracker.h"
#include "../common/vector_proc.h"
#include "midi_instrument.h"
#include "midi_instrument_proc.h"
#include "OS_midi_proc.h"

#include "midi_playfromstart_proc.h"

void MIDIPlaySongHook(struct Instruments *instrument, int64_t abstime){
  VECTOR_FOR_EACH(struct Patch *patch,&instrument->patches){

    struct PatchData *patchdata=(struct PatchData *)patch->patchdata;
    int channel=patchdata->channel;
    
    if(patchdata->volumeonoff){
      MIDI_send3(patchdata, 0xb0|channel,0x7,patchdata->volume);
    }
    if(patchdata->panonoff){
      MIDI_send3(patchdata, 0xb0|channel,0xa,patchdata->pan);
    }
    
    {
      int lokke;
      for(lokke=0;lokke<8;lokke++){
        if(patchdata->cc[lokke]>=0 && patchdata->ccsonoff[lokke]){
          MIDI_send3(patchdata, 0xb0|channel,patchdata->cc[lokke],patchdata->ccvalues[lokke]);
        }
      }
    }

  }END_VECTOR_FOR_EACH;
  
}



