/* Copyright 2003 Kjetil S. Matheussen

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

#include "../common/gfx_wtrackheaders_proc.h"
#include "../common/list_proc.h"
#include "midi_instrument.h"
#include "../common/patch_proc.h"
#include "midi_instrument_proc.h"
#include "../common/playerclass.h"

#include "../Qt/Qt_instruments_proc.h"


#include "midi_getEvents_proc.h"
#include "OS_midigfx_proc.h"


extern bool useOx90ForNoteOff;

// Only used by the gtk1 midi instruments window

void MIDIGetEvents(
		   struct Instruments *instrument,
		   int arg0,
		   int arg1,
		   int arg2,
		   int arg3
		   )
{
  struct Tracker_Windows *window=root->song->tracker_windows;
  struct Patch *patch=PATCH_get_current();
  struct PatchData *patchdata;
  int channel;

  switch(arg0){
  case MIDIEVENT_USE0x90FORNOTEOFF:
    useOx90ForNoteOff=arg1;
    break;
  case MIDIEVENT_NEWPATCH:
    SelectPatch(window,window->wblock->wtrack->track);
    return;
    break;
  case MIDIEVENT_SETSTANDARDVEL:
    PATCH_get_current()->standardvel=arg1;
    break;
  case MIDIEVENT_CHANGECURRENTPATCH:
    {
      struct Patch *patch=NULL;
      
      if(arg1!=0){
	patch=ListFindElement1(&instrument->patches->l,arg1);
      }
      
      if( ! is_playing()){
	window->wblock->wtrack->track->patch=patch;
        if (patch != NULL)
          patch->has_been_assigned_to_editor_track = true;
	DrawWTrackHeader(window,window->wblock,window->wblock->wtrack);
      }
      instrument->PP_Update(instrument,patch,false);
    }
    break;
  case MIDIEVENT_CHANGECURRENTPORT:
    printf("MIDIEVENT_CHANGE.. %d\n",arg1);
    break;
  default:
    break;
  }

  patchdata=(struct PatchData *)patch->patchdata;
  channel=patchdata->channel;
  struct MidiPort *midi_port = patchdata->midi_port;

  printf("Got MidiEvent %d, %d,%d,%d\n",arg0,arg1,arg2,arg3);


  if(arg0>0x7f){
    R_PutMidi3(midi_port,arg0,arg1,arg2);
  }else{
    switch(arg0){
    case MIDIEVENT_SETMIDIINPUT:
      break;
    case MIDIEVENT_CHANGEPATCHNAME:
      break;    
    case MIDIEVENT_SETPORT:
      break;
    case MIDIEVENT_CHANGEPORT:
      break;
    case MIDIEVENT_SETCHANNEL:
      if(arg1<1 || arg1>16){
	MIDIGFX_SetChannel(patchdata->channel);
      }else{
	patchdata->channel=arg1-1;
	MIDIGFX_UpdateAll();
      }
      break;
    case MIDIEVENT_SETMSB:
      if(arg1>=-1 && arg1<128){
	patchdata->MSB=arg1;
      }else{
	MIDIGFX_SetMSB(patchdata->MSB);
      }
      break;
    case MIDIEVENT_SETLSB:
      if(arg1>=-1 && arg1<128){
	patchdata->LSB=arg1;
      }else{
	MIDIGFX_SetLSB(patchdata->LSB);
      }
      break;
    case MIDIEVENT_SETPRESET:
      patchdata->preset=arg1-1;
      break;
    case MIDIEVENT_PANNINGONOFF:
      patchdata->panonoff=patchdata->panonoff?false:true;
      MIDIGFX_SetPanSlider(patchdata->panonoff,patchdata->pan);
      break;
    case MIDIEVENT_SETPANNING:
      patchdata->pan=arg1;
      D_PutMidi3(midi_port,0xb0|channel,0xa,patchdata->pan);
      break;
    case MIDIEVENT_SETVOLONOFF:
      patchdata->volumeonoff=patchdata->volumeonoff?false:true;
      MIDIGFX_SetVolumeSlider(patchdata->volumeonoff,patchdata->volume);
      break;
    case MIDIEVENT_SETVOL:
      patchdata->volume=arg1;
      D_PutMidi3(midi_port,0xb0|channel,0x7,patchdata->volume);
      break;
    case MIDIEVENT_CC_ONOFF:
      patchdata->ccsonoff[arg1]=arg2==1?true:false;
      MIDIGFX_SetCCSlider(arg1,patchdata->ccsonoff[arg1],patchdata->ccvalues[arg1]);
      break;
    case MIDIEVENT_CC_VAL:
      patchdata->ccvalues[arg1]=arg2;
      break;
    default:
      printf("Unknown MIDIEVENT message: %d\n",arg0);
      break;
    }
  }
}


