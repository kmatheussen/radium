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



/* Some lines of code copy-and-pasted from the seq24 source code. -Kjetil. */



#include "../../common/nsmtracker.h"
#include "../../common/visual_proc.h"

#include "../midi_i_plugin.h"

#include "../midi_MyMidiLink_proc.h"


#include "../OS_midi_proc.h"



static snd_seq_t *radium_seq;
static int radium_queue;

static bool alsaseq_opened=false;



/*
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
*/


char **MIDI_getPortNames(int *retsize){
  snd_seq_client_info_t *cinfo;
  snd_seq_port_info_t *pinfo;
  int num=0;
  int dasretsize=5000;
  char **ret=talloc(sizeof(char *)*dasretsize);
  
  snd_seq_client_info_alloca(&cinfo);
  snd_seq_client_info_set_client(cinfo, -1);

  while (snd_seq_query_next_client(radium_seq, cinfo) >= 0){
    int client = snd_seq_client_info_get_client(cinfo);
    snd_seq_port_info_alloca(&pinfo);
    snd_seq_port_info_set_client(pinfo, client);
    snd_seq_port_info_set_port(pinfo, -1);
    if(client==0) continue;
    //if (client == m_client || client == 0) continue;
    printf("Name: %s\n",snd_seq_client_info_get_name(cinfo));
    //    if(!strcmp("Hydrogen",snd_seq_client_info_get_name(cinfo))){
    while (snd_seq_query_next_port(radium_seq, pinfo) >= 0 ){
      int cap =  snd_seq_port_info_get_capability(pinfo);

      if ( snd_seq_port_info_get_client(pinfo) != 0 ){
	if ( (cap & SND_SEQ_PORT_CAP_SUBS_WRITE) != 0){
	  ret[num]=talloc(strlen(snd_seq_port_info_get_name(pinfo))+1);
	  sprintf(ret[num],      snd_seq_port_info_get_name(pinfo));
	  num++;
	  printf("gakk %s\n", snd_seq_port_info_get_name(pinfo));
	  if(num==dasretsize){
	    fprintf(stderr,"To many midi ports. If this is a problem, contact the author to increase buffer. (What the @e¶qer5@e¶!!!) \n");
	    goto endwhile;
	  }
	}
      }
      //}
      //break;
    }
  }
 endwhile:

  *retsize=num;

  return ret;
}


struct MyMidiLinks *MIDI_getMyMidiLink(struct Tracker_Windows *window,ReqType reqtype,char *name){
  struct MyMidiLinks *mymidilink;
  //  printf("MIDI_getMyMidiLink\n");
  snd_seq_client_info_t *cinfo;
  snd_seq_port_info_t *pinfo = NULL;
  int client;
  int ret;

  //name="FLUID Synth (3405)";
  //name="FLUID Synth (4081)";
  //name="amSynth";
  //name="Client-128";
  //name="Hydrogen Midi-In";
  //name="Synth input port (29830)";

  mymidilink=talloc(sizeof(struct MyMidiLinks));
  mymidilink->midilink=talloc(sizeof(struct MidiLink));
  mymidilink->name="Dummy midi patch";


  if(name==NULL){
    int num_ports;
    char **portnames=MIDI_getPortNames(&num_ports);
    int sel=GFX_Menu(window,reqtype,"Select port",num_ports,portnames);
    name=portnames[sel];
  }

  snd_seq_client_info_alloca(&cinfo);
  snd_seq_client_info_set_client(cinfo, -1);

  while (snd_seq_query_next_client(radium_seq, cinfo) >= 0){
    client = snd_seq_client_info_get_client(cinfo);
    snd_seq_port_info_alloca(&pinfo);
    snd_seq_port_info_set_client(pinfo, client);
    snd_seq_port_info_set_port(pinfo, -1);
    if(client==0) continue;
    //if (client == m_client || client == 0) continue;
    printf("Name: %s\n",snd_seq_client_info_get_name(cinfo));
    //if(!strcmp("Hydrogen",snd_seq_client_info_get_name(cinfo))){
    while (snd_seq_query_next_port(radium_seq, pinfo) >= 0 ){
      int cap =  snd_seq_port_info_get_capability(pinfo);
      
      if ( snd_seq_port_info_get_client(pinfo) != 0 ){
	if ( (cap & SND_SEQ_PORT_CAP_SUBS_WRITE) != 0){
	  if(!strcmp(name,snd_seq_port_info_get_name(pinfo))){
	    printf("gakk %s\n", snd_seq_port_info_get_name(pinfo));
	    goto gotit;
	  }
	}
      }
    }
  }

 gotit:

  mymidilink->midilink->port = snd_seq_create_simple_port(radium_seq, 
							  "radium out",
							  SND_SEQ_PORT_CAP_NO_EXPORT |
							  SND_SEQ_PORT_CAP_READ,
							  SND_SEQ_PORT_TYPE_MIDI_GENERIC |
							  SND_SEQ_PORT_TYPE_APPLICATION );

  ret = snd_seq_connect_to(
			   radium_seq, 
			   mymidilink->midilink->port,
			   snd_seq_port_info_get_client(pinfo),
			   snd_seq_port_info_get_port(pinfo)
			   );


  printf("myport: %d, connectret: %d\n",mymidilink->midilink->port,ret);



  MIDI_initMyMidiLink(mymidilink);

  return mymidilink;
}


#if 0
// NOTE! When name==null, name is really "out.0" for camd.

struct MyMidiLinks *MIDI_GetMyMidiLink(char *name){
  //printf("MIDI_GetMyMidiLink\n");
  return MIDI_getMyMidiLink();
}
#endif


void MIDI_PP_Update(struct Instruments *instrument,struct Patch *patch){
  printf("MIDI_PP_Update");    
}

//GoodPutMidi(mymidilink->midilink,(ULONG)((cc<<24)|(data1<<16)|(data2<<8)),(ULONG)maxbuff);


static void alsaseq_PutMidi(
			    struct MidiLink *midilink,
			    uint32_t msg
			    )
{
  unsigned int d3=(msg>>8)&0xff;
  unsigned int d2=(msg>>16)&0xff;
  unsigned int d1=(msg>>24)&0xff;

  unsigned char buffer[3]={d1,d2,d3};
  snd_seq_event_t ev;
  snd_midi_event_t *midi_ev;
  snd_midi_event_new( 10, &midi_ev );
  
  snd_seq_ev_clear( &ev );
  snd_midi_event_encode(midi_ev,
			buffer,
			3,
			&ev); 
  snd_midi_event_free( midi_ev );
  
  snd_seq_ev_set_source(&ev,midilink->port);
  snd_seq_ev_set_subs(&ev);
  
  snd_seq_ev_schedule_tick( &ev, radium_queue, 1, 1);
  
  snd_seq_event_output(radium_seq, &ev);
  
  snd_seq_drain_output(radium_seq);

}

void GoodPutMidi(struct MidiLink *midilink,
		   uint32_t msg,
		   uint32_t maxbuff
		   )
{
  alsaseq_PutMidi(midilink,msg);
  //MIDI_print(msg,maxbuff);
  ////////printf("GoodPutMidi %x - %d\n",msg,maxbuff);
}


void PutMidi(struct MidiLink *midilink,
	       uint32_t msg
	       )
{
  alsaseq_PutMidi(midilink,msg);
  //MIDI_print(msg,0);
  //printf("PutMidi %x\n",msg);
}



#include "../X11/X11_MidiProperties_proc.h"


void MIDI_Delete(void){
  snd_seq_event_t ev;

  if(alsaseq_opened==true){
    /* kill timer */
    snd_seq_ev_clear(&ev);
    
    snd_seq_stop_queue( radium_seq, radium_queue, &ev );
    snd_seq_free_queue( radium_seq, radium_queue );
    
    snd_seq_close(radium_seq);  
  }
}


bool MIDI_New(struct Instruments *instrument){

  printf("ALSASEQ_MIDI_New\n");

#if 1
  if(snd_seq_open(&radium_seq,"default",SND_SEQ_OPEN_DUPLEX,0)!=0){
    fprintf(stderr,"Could not open ALSA sequencer.\n");
    return true;
  }

  alsaseq_opened=true;


  radium_queue = snd_seq_alloc_queue(radium_seq); 


  snd_seq_start_queue(radium_seq, radium_queue, NULL );

#endif

  //  instrument->PP_Update=MIDI_PP_Update;
  instrument->PP_Update=X11_MIDI_PP_Update;
  return true;
}




