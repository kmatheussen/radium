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



#include "../../common/nsmtracker.h"
#include "../midi_instrument.h"

#include "../midi_MyMidiLink_proc.h"


#include "../OS_midi_proc.h"


static void MIDI_print(uint32_t msg,int maxbuff){
  unsigned int d3=(msg>>8)&0xff;
  unsigned int d2=(msg>>16)&0xff;
  unsigned int d1=(msg>>24)&0xff;
  int message=d1&0xf0;
  int channel=d1&0xf;

  printf("Dummy midi driver got ");
  switch(message){
  case 0x80:
    printf("NOTE OFF at channel %d, note %d, velocity %d\n",channel,d2,d3);
    break;
  case 0x90:
    printf("NOTE ON at channel %d, note %d, velocity %d\n",channel,d2,d3);
  default:
    printf("0x%x at channel %d, with data %d/%d\n",(unsigned int)message,channel,d2,d3);
  }
}

extern void MIDI_Delete(void){
  
}



extern struct MyMidiLinks *MIDI_getMyMidiLink(void){
  struct MyMidiLinks *mymidilink;
  //  printf("MIDI_getMyMidiLink\n");

  mymidilink=talloc(sizeof(struct MyMidiLinks));
  mymidilink->name="Dummy midi patch";

  MIDI_initMyMidiLink(mymidilink);

  return mymidilink;
}


// NOTE! When name==null, name is really "out.0" for camd.

extern struct MyMidiLinks *MIDI_GetMyMidiLink(char *name){
  //printf("MIDI_GetMyMidiLink\n");
  return MIDI_getMyMidiLink();
}


#if 0
// This is a GUI function.
void MIDI_PP_Update(struct Instruments *instrument,struct Patch *patch){
  printf("MIDI_PP_Update");    
}
#endif

//GoodPutMidi(mymidilink->midilink,(ULONG)((cc<<24)|(data1<<16)|(data2<<8)),(ULONG)maxbuff);

extern void GoodPutMidi(struct MidiLink *midilink,
		   uint32_t msg,
		   uint32_t maxbuff
		   )
{
  MIDI_print(msg,maxbuff);
  ////////printf("GoodPutMidi %x - %d\n",msg,maxbuff);
}

extern void PutMidi(struct MidiLink *midilink,
	       uint32_t msg
	       )
{
  MIDI_print(msg,0);
  //printf("PutMidi %x\n",msg);
}


//#include "../X11/X11_MidiProperties_proc.h"

extern bool MIDI_New(struct Instruments *instrument){
  instrument->PP_Update=MIDIGFX_PP_Update;
  //instrument->PP_Update=X11_MIDI_PP_Update;
  return true;
}


