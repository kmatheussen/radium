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


#include "nsmtracker.h"
#include "midi_i_plugin.h"


#include "midi_MyMidiLink_proc.h"


extern struct MyMidiLinks *usedmidilinks;


void MIDI_initMyMidiLink(struct MyMidiLinks *mymidilink){
  int lokke,lokke2;

  mymidilink->next=usedmidilinks;  
  
  mymidilink->standardccs[0]=0x5d;
  mymidilink->standardccs[1]=0x5b;
  mymidilink->standardccs[2]=0x49;
  mymidilink->standardccs[3]=0x48;
  mymidilink->standardccs[4]=0x4a;
  mymidilink->standardccs[5]=0x47;
  mymidilink->standardccs[6]=0x5e;
  mymidilink->standardccs[7]=0x1;
  
  
  mymidilink->ccnames[0]="Chorus";
  mymidilink->ccnames[1]="Reverb";
  mymidilink->ccnames[2]="Attack";
  mymidilink->ccnames[3]="Release";
  mymidilink->ccnames[4]="CutOff";
  mymidilink->ccnames[5]="Resonance";
  mymidilink->ccnames[6]="Variation Depth";
  mymidilink->ccnames[7]="Modulation";
  
  
  for(lokke=0;lokke<16;lokke++){
    mymidilink->channelspecific[lokke].MSB=-1;
    mymidilink->channelspecific[lokke].LSB=-1;
    mymidilink->channelspecific[lokke].preset=-1;
    
    mymidilink->channelspecific[lokke].volumeonoff=false;
    mymidilink->channelspecific[lokke].panonoff=false;
    
    mymidilink->channelspecific[lokke].volume=100;
    mymidilink->channelspecific[lokke].pan=0x40;
    
    for(lokke2=0;lokke2<8;lokke2++){
      mymidilink->channelspecific[lokke].ccvalues[lokke2]=0;
      mymidilink->channelspecific[lokke].ccsonoff[lokke2]=false;
    }
  }
  
  usedmidilinks=mymidilink;

}

