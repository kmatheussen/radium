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
#include "../common/list_proc.h"
#include "../midi/midi_i_plugin.h"

#include "Python.h"

#include "X11_MidiProperties_proc.h"

#include "../midi/OS_midi_proc.h"
#include "../midi/OS_midigfx_proc.h"


struct Patch *currpatch=NULL;

extern struct Root *root;


bool X11_StartMidiProperties(){
  PyRun_SimpleString("import X11_MidiProperties");
  PyRun_SimpleString("X11_MidiProperties.MIDI_StartMidiProperties()");

  return true;
}


void X11_MIDI_PP_Update_doit(struct Instruments *instrument,struct Patch *patch);

void X11_MIDI_PP_Update(struct Instruments *instrument,struct Patch *patch){
  if(currpatch==patch) return;
  X11_MIDI_PP_Update_doit(instrument,patch);
}


void X11_MIDI_PP_Update_doit(struct Instruments *instrument,struct Patch *patch){
  NInt patchnum;

  if(patch==NULL){
    patchnum=-1;
  }else{
    patchnum=patch->l.num;
  }
  
  currpatch=patch;
  
  if(currpatch==NULL){
    PyRun_SimpleString("X11_MidiProperties.UpdateEmptyPatch()");
  }else{
    NInt num_patches;
    struct PatchData *patchdata;
    struct MyMidiLinks *mymidilink;
    struct ChannelSpesific *cs;
    int lokke;
    int num_ports;
    char **ports;
    char temp[5000];

    num_patches=ListFindNumElements1(&instrument->patches->l);
    patchdata=(struct PatchData *)patch->patchdata;
    mymidilink=patchdata->mymidilink;
    cs=&mymidilink->channelspesific[patchdata->channel];

    sprintf(temp,"X11_MidiProperties.UpdatePatch([\"%s\"",instrument->patches->name);

    patch=NextPatch(instrument->patches);
    while(patch!=NULL){
      sprintf(temp,"%s,\"%s\"",temp,patch->name);
      patch=NextPatch(patch);
    }

    sprintf(temp,"%s],%d,[",temp,patchnum);
    printf("patchnum: %d\n",patchnum);

    ports=MIDI_getPortNames(&num_ports);
    for(lokke=0;lokke<num_ports;lokke++){
      sprintf(temp,"%s\"%s\"%s",temp,ports[lokke],lokke==num_ports-1?"":",");
    }


    sprintf(temp,"%s],\"%s\",%d,%d,%d,%d,%d,%d,%d,%d,%d,",
	    temp,
	    mymidilink->name,
	    patchdata->channel+1,
	    patchdata->MSB,
	    patchdata->LSB,
	    patchdata->preset+1,
	    cs->panonoff==true?1:0,
	    cs->pan,
	    cs->volumeonoff==true?1:0,
	    cs->volume,
	    currpatch->standardvel
	    );

    sprintf(temp,"%s[",temp);
    for(lokke=0;lokke<8;lokke++){
      sprintf(temp,"%s%d%s",temp,cs->ccsonoff[lokke]==true?1:0,lokke<7?",":"");
    }

    sprintf(temp,"%s],[",temp);

    for(lokke=0;lokke<8;lokke++){
      sprintf(temp,"%s\"%s\"%s",temp,mymidilink->ccnames[lokke],lokke<7?",":"");
    }
    
    sprintf(temp,"%s],[",temp);

    for(lokke=0;lokke<8;lokke++){
      sprintf(temp,"%s%d%s",temp,mymidilink->standardccs[lokke],lokke<7?",":"");
    }

    sprintf(temp,"%s],[",temp);

    for(lokke=0;lokke<8;lokke++){
      sprintf(temp,"%s%d%s",temp,cs->ccvalues[lokke],lokke<7?",":"");
    }

    sprintf(temp,"%s])",temp);

    PyRun_SimpleString(temp);
  }
}

void X11_MidiProperties_SetX11Window(int x11window){
  char temp[600];
  sprintf(temp,"X11_MidiProperties.MIDI_SetX11Window(%d)",x11window);
  PyRun_SimpleString(temp);
}

void X11_MidiProperties_StopXSend(void){
  PyRun_SimpleString("X11_MidiProperties.MIDI_StopXSend()");
}

void MIDIGFX_UpdateAll(void){
     X11_MIDI_PP_Update_doit(root->def_instrument,currpatch);
}

void MIDIGFX_SetPanSlider(bool on,int value){
  X11_MIDI_PP_Update_doit(root->def_instrument,currpatch);
}

void MIDIGFX_SetVolumeSlider(bool on,int value){
  X11_MIDI_PP_Update_doit(root->def_instrument,currpatch);
}

void MIDIGFX_SetLSB(int lsb){
  X11_MIDI_PP_Update_doit(root->def_instrument,currpatch);
}

void MIDIGFX_SetMSB(int msb){
  X11_MIDI_PP_Update_doit(root->def_instrument,currpatch);
}

void MIDIGFX_SetChannel(int ch){
  X11_MIDI_PP_Update_doit(root->def_instrument,currpatch);
}

void MIDIGFX_SetCCSlider(int slidernum,bool on,int value){
  X11_MIDI_PP_Update_doit(root->def_instrument,currpatch);
}

