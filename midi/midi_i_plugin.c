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


#include <string.h>


#include "nsmtracker.h"
#include "../common/vector_proc.h"
#include "../common/visual_proc.h"
#include "../common/playerclass.h"
#include "../common/settings_proc.h"
#include "../common/instruments_proc.h"
#include "../common/fxlines_proc.h"
#include "../common/player_pause_proc.h"
#include "../common/undo_tracks_proc.h"

#include "midi_i_plugin.h"

#include "midi_playfromstart_proc.h"
#include "midi_fx_proc.h"
#include "disk_midi_fx_proc.h"

//#include "../instrprop/Amiga_instrprop_edit_proc.h"
#include "OS_midi_proc.h"
#include "OS_midigfx_proc.h"



#include "midi_i_plugin_proc.h"




/* Wrappers, all midi goes thru here. */

static void OnOffNotesTrack(
                            struct MidiPort *midi_port,
                            int cc,
                            int data1,
                            int data2
){
  int channel = cc & 0xf;
  int command = cc & 0xf0;

  if(command==0x80 || ((command==0x90 && data2==0))){
    if(midi_port->num_ons[channel][data1]>0)
      midi_port->num_ons[channel][data1]--;
  }else{
    if(command==0x90){
      midi_port->num_ons[channel][data1]++;
    }
  }
}

void MyGoodPutMidi(
                   struct MidiPort *midi_port,
                   int cc,
                   int data1,
                   int data2,
                   int maxbuff
){
  OS_GoodPutMidi(midi_port->port,cc,data1,data2,-1.0,(uint32_t)maxbuff);
  OnOffNotesTrack(midi_port,cc,data1,data2);
}

void MyMyPutMidi(
                 struct MidiPort *midi_port,
                 int cc,
                 int data1,
                 int data2
){
  OS_PutMidi(midi_port->port,cc,data1,data2,-1.0);
  OnOffNotesTrack(midi_port,cc,data1,data2);
}


/****************** My put-midi function.  **************************/
/****************** Do only call from the player-task (nah).  *************/
extern PlayerClass *pc;

void MyPutMidi(
               struct MidiPort *midi_port,
               //	uint32_t msg,
               int cc,
               int data1,
               int data2,
               STime time,
               int maxbuff,
               int skip
){
	if(cc<0x80 || cc>0xef || (data1&0xff)>0x7f || (data2&0xff)>0x7f){
          GFX_Message(NULL, "Error. Faulty midi-message. status: %x, data1: %x, data2: %x\n",cc,data1,data2);
          return;
	}

	OS_GoodPutMidi(midi_port->port,cc,data1,data2,time,(uint32_t)maxbuff);

	OnOffNotesTrack(midi_port,cc,data1,data2);
}



/******************** notes **************************/

static void MIDIplaynote(struct SeqTrack *seqtrack,
                         struct Patch *patch,
                         note_t note,
                         STime time
){
	const struct PatchData *patchdata=(struct PatchData *)patch->patchdata;

        if(patchdata==NULL)
          return;

        struct MidiPort *midi_port = patchdata->midi_port;
	const int channel=patchdata->channel; // We don't use note.midi_channel here.

	int maxbuf=70;

        int notenum = note.pitch;

        if(notenum>127)
          notenum=127;
        if(notenum<0)
          notenum=0;

        int velocity = scale(note.velocity,0,1,0,127);

        if(velocity>127)
          velocity=127;

        if(velocity<0)
          velocity=0;

	/* Check if the patch has other LSB/MSB/Preset than is currently
	   set for the channel.
	*/
	if(
		(
			midi_port->LSB[channel] != patchdata->LSB ||
			midi_port->MSB[channel] != patchdata->MSB
		) &&
			patchdata->LSB!=-1
	){
		if(patchdata->MSB!=-1){
			PutMidi3(
				midi_port,
				0xb0|channel,
				32,
				patchdata->MSB,
                                time,
				10
			);
			midi_port->MSB[channel] = patchdata->MSB;
		}

		PutMidi3(
			midi_port,
			0xb0|channel,
			32,
			patchdata->LSB,
                        time,
			100000
		);
		midi_port->LSB[channel] = patchdata->LSB;

		PutMidi2(
			midi_port,
			0xc0|channel,
			patchdata->preset,
                        time,
			100000
		);
		midi_port->preset[channel] = patchdata->preset;
		maxbuf=100000;

	}else{
		if(
			patchdata->preset!=-1 &&
			midi_port->preset[channel] != patchdata->preset
		){
			PutMidi2(
				midi_port,
				0xc0|channel,
				patchdata->preset,
                                time,
				10
			);
			midi_port->preset[channel] = patchdata->preset;
			maxbuf=100000;
		}
	}

	PutMidi3(
		midi_port,
		0x90|channel,
		notenum,
		velocity,
                time,
		maxbuf
	);
}


static bool useOx90ForNoteOff=false;

bool MIDI_get_use_0x90_for_note_off(void){
  return useOx90ForNoteOff;
}

void MIDI_set_use_0x90_for_note_off(bool doit){
  SETTINGS_write_int("use_0x90_for_note_off",doit?1:0);
  useOx90ForNoteOff = doit;
}


static void MIDIstopnote(struct SeqTrack *seqtrack,
                         struct Patch *patch,
                         note_t note,
                         STime time
){
  struct PatchData *patchdata=(struct PatchData *)patch->patchdata;

  if(patchdata==NULL)
    return;

  int notenum = note.pitch;

  if(notenum>127)
    notenum=127;
  if(notenum<0)
    notenum=0;

#if 0
  uint32_t tem=(uint32_t)(((0x80)<<24)|((notenum)<<16)|((velocity)<<8));
  if(tem>>24!=0x80){
    Pdebug("NoteStopAI! %x, val: %x\n",tem,notenum);
    return;
  }
#endif

  PutMidi3(
           patchdata->midi_port,
           (useOx90ForNoteOff==true?0x90:0x80)|patchdata->channel,
           notenum,
           0, //useOx90ForNoteOff==true?0:velocity,
           time,
           10
           );

}

/******************* Velocity *************************/

static void MIDIchangevelocity(struct SeqTrack *seqtrack,struct Patch *patch,note_t note,STime time){
	struct PatchData *patchdata=(struct PatchData *)patch->patchdata;

        if(patchdata==NULL)
          return;

        int notenum = note.pitch;

        if(notenum>127)
          notenum=127;
        if(notenum<0)
          notenum=0;

        int velocity = scale(note.velocity,0,1,0,127);

        if(velocity>127)
          velocity=127;

        if(velocity<0)
          velocity=0;

        //printf("Sending aftertouch. channel: %d, note: %d, val: %d\n",patchdata->channel,note->note,velocity);
	PutMidi3(
		patchdata->midi_port,
		0xa0|patchdata->channel,
		notenum,
		velocity,
                time,
		10
	);
}


static void MIDIchangepitch(struct SeqTrack *seqtrack,struct Patch *patch,note_t note,STime time){ // never called
}

static void MIDIsendrawmidimessage(struct SeqTrack *seqtrack,struct Patch *patch,uint32_t msg,STime time, double block_reltempo){ // never called
}

/******************** patch **************************/

static void MIDIchangeTrackPan(int newpan,const struct Tracks *track){
	struct PatchData *patchdata=(struct PatchData *)track->patch->patchdata;

        if(patchdata==NULL)
          return;

	D_PutMidi3(
		patchdata->midi_port,
		0xb0|patchdata->channel,
		10,
		R_BOUNDARIES(
                             0,
                             (127*(newpan+MAXTRACKPAN)/(MAXTRACKPAN*2)),
                             127
		)
	);
/*
	RError("newpan: %d, pan: %d\n",
		newpan,
		boundaries(
			0,
			(127*(newpan+MAXTRACKPAN)/(MAXTRACKPAN*2)),
			127
		)
	);
*/
}
/*
-1000 - 1000
0 - 127
*/

static struct PatchData *getPatchData(struct Patch *patch){
  return patch->patchdata;
}

void MIDISetPatchData(struct Patch *patch, const char *key, const char *value, bool program_state_is_valid){
  if(false){

  }else if(!strcasecmp(key,"port")){
    getPatchData(patch)->midi_port = MIDIgetPort(NULL, NULL, value==NULL ? NULL : !strcmp("",value) ? NULL : value, program_state_is_valid);
    printf("Sat patchdata(%s)->midi_port to %s\n",patch->name,value);

  }else if(!strcasecmp(key,"channel")){
    getPatchData(patch)->channel = atoi(value);

  }else if(!strcasecmp(key,"LSB")){
    getPatchData(patch)->LSB = atoi(value);

  }else if(!strcasecmp(key,"MSB")){
    getPatchData(patch)->MSB = atoi(value);

  }else if(!strcasecmp(key,"preset")){
    getPatchData(patch)->preset = atoi(value);

  } else
    GFX_Message2(NULL, program_state_is_valid, "MIDISetPatchData: Unknown key \"%s\" for midi instrument", key);
}

static char *MIDIGetPatchData(struct Patch *patch, const char *key){
  if(false){

  }else if(!strcasecmp(key,"port")){
    return getPatchData(patch)->midi_port->name;

  }else if(!strcasecmp(key,"channel")){
    return talloc_numberstring(getPatchData(patch)->channel);

  }else if(!strcasecmp(key,"LSB")){
    return talloc_numberstring(getPatchData(patch)->LSB);

  }else if(!strcasecmp(key,"MSB")){
    return talloc_numberstring(getPatchData(patch)->MSB);

  }else if(!strcasecmp(key,"preset")){
    return talloc_numberstring(getPatchData(patch)->preset);

  } else
    RWarning("MIDIGetPatchData: Unknown key \"%s\" for midi instrument", key);

  return "";
}

void MIDIclosePatch(struct Patch *patch){
	return;
}

static struct PatchData *createPatchData(void) {
  struct PatchData *patchdata=talloc(sizeof(struct PatchData));
  patchdata->preset=-1;
  patchdata->MSB=-1;
  patchdata->LSB=-1;

  patchdata->cc[0]=0x5d;
  patchdata->cc[1]=0x5b;
  patchdata->cc[2]=0x49;
  patchdata->cc[3]=0x48;
  patchdata->cc[4]=0x4a;
  patchdata->cc[5]=0x47;
  patchdata->cc[6]=0x5e;
  patchdata->cc[7]=0x1;
  
  
  patchdata->ccnames[0]="Chorus";
  patchdata->ccnames[1]="Reverb";
  patchdata->ccnames[2]="Attack";
  patchdata->ccnames[3]="Release";
  patchdata->ccnames[4]="CutOff";
  patchdata->ccnames[5]="Resonance";
  patchdata->ccnames[6]="Variation Depth";
  patchdata->ccnames[7]="Modulation";
  
  
  patchdata->volumeonoff=false;
  patchdata->panonoff=false;
    
  patchdata->volume=100;
  patchdata->pan=0x40;
    
  int lokke2;
  for(lokke2=0;lokke2<8;lokke2++){
    patchdata->ccvalues[lokke2]=0;
    patchdata->ccsonoff[lokke2]=false;
  }

  {
    char *portname = MIDI_getDefaultOutputPort();
    if (portname==NULL)
      portname = "default";
    patchdata->midi_port = MIDIgetPort(NULL,NULL,portname,false);
  }

  return patchdata;
}

static struct MidiPort *g_midi_ports = NULL;

static bool is_member(char *name, char**names){
  int i=0;
  while(names[i]!=NULL){
    if(!strcmp(name,names[i]))
      return true;
    i++;
  }
  return false;
}

char **MIDI_getPortNames(int *retsize, bool is_input){
  int num_os_names;
  char **os_names = is_input ? MIDI_getInputPortOsNames(&num_os_names) : MIDI_getOutputPortOsNames(&num_os_names);

  int num_midi_ports = 0;
  struct MidiPort *midi_port = g_midi_ports;

  while(midi_port != NULL) {
    midi_port = midi_port->next;
    num_midi_ports++;
  }

  *retsize = num_os_names;
  char **ret = talloc((num_os_names+num_midi_ports+1)*sizeof(char*));
  memcpy(ret,os_names,sizeof(char*)*num_os_names);

  // Add existing ports;
  if(is_input==false){
    midi_port = g_midi_ports;
    while(midi_port != NULL){
      if(is_member(midi_port->name, ret)==false){
        ret[*retsize] = talloc_strdup(midi_port->name);
        *retsize = *retsize + 1;
      }
      midi_port=midi_port->next;
    }
  }

  return ret;
}

extern struct Root *root;

char *MIDIrequestPortName(struct Tracker_Windows *window, ReqType reqtype, bool is_input, bool program_state_is_valid){
  int num_ports;

  char **portnames=MIDI_getPortNames(&num_ports, is_input);
  vector_t v={0};
  int i;

#if defined(FOR_WINDOWS)
  if(num_ports==0)
    return NULL;
#endif

  for(i=0;i<num_ports;i++)
    VECTOR_push_back(&v,portnames[i]);

#if !defined(FOR_WINDOWS)
  VECTOR_push_back(&v,"Create new port");
#endif

  int sel=GFX_Menu(window,reqtype,"Select port",v,program_state_is_valid);
  if(sel==-1)
    return NULL;

  if(sel==num_ports)
    return GFX_GetString(NULL,reqtype,"Name: ",program_state_is_valid);

  return portnames[sel];
}

// This function must never return NULL.
struct MidiPort *MIDIgetPort(struct Tracker_Windows *window,ReqType reqtype,const char *name,bool program_state_is_valid){
  //printf("    Calling MIDIgetPort -%s-\n", name);
  bool created_new_port = false;

  while(name==NULL){
    name = MIDIrequestPortName(window,reqtype,false,program_state_is_valid);
  }

  struct MidiPort *midi_port = g_midi_ports;

  while (midi_port != NULL) {
    if(!strcmp(midi_port->name,name)) {
      printf("Found existing midi_port for %s\n",name);
      break;
    }
    midi_port = midi_port->next;
  }

  if (midi_port == NULL){
    midi_port = talloc(sizeof(struct MidiPort));
    midi_port->name = talloc_strdup(name);
    midi_port->port = MIDI_getMidiPortOs(window,reqtype,name);
    created_new_port = true;
  }

  int num_input_ports;
  char **input_port_names = MIDI_OS_get_connected_input_ports(&num_input_ports);
  for(int i=0;i<num_input_ports;i++){
    printf("   Testing -%s- vs. -%s-\n", input_port_names[i], midi_port->name);
    char *portname = midi_port->name;
    if (!strncmp(portname,"Radium: ",8))
      portname += 8;

    if (!strcmp(input_port_names[i], portname)){
      vector_t v = {0};

      int yes = VECTOR_push_back(&v, "Yes");
      int no = VECTOR_push_back(&v, "No");

      int result = GFX_Message2(&v, program_state_is_valid, "Are you sure? Port \"%s\" is an input port. If you send MIDI data to this port, you risk starting a recursive connection that's impossible to stop.", midi_port->name);

      if (g_user_interaction_enabled==false || result!=yes)
        return MIDIgetPort(window, reqtype, NULL, program_state_is_valid);

      (void)no;
    }
  }

  if (created_new_port==true){
    midi_port->next = g_midi_ports;
    g_midi_ports = midi_port;
  }

  return midi_port;
}

void MIDI_InitPatch(struct Patch *patch) {
  patch->playnote=MIDIplaynote;
  patch->stopnote=MIDIstopnote;
  patch->changevelocity=MIDIchangevelocity;
  patch->changepitch=MIDIchangepitch;
  patch->sendrawmidimessage=MIDIsendrawmidimessage;
  patch->closePatch=MIDIclosePatch;
  patch->changeTrackPan=MIDIchangeTrackPan;

  patch->patchdata = createPatchData();

  patch->instrument = get_MIDI_instrument();
  
  patch->is_usable = true;
}

/*
static int MIDIgetPatch(
	struct Tracker_Windows *window,
	ReqType reqtype,
	const struct Tracks *track,
	struct Patch *patch,
        bool program_state_is_valid
){
        MIDI_InitPatch(patch);

	struct MidiPort *midi_port = MIDIgetPort(window,reqtype,NULL,program_state_is_valid);

	struct PatchData *patchdata=getPatchData(patch);
        patchdata->midi_port = midi_port;

        patchdata->channel=GFX_GetInteger(window,reqtype,"Channel: (1-16) ",1,16,program_state_is_valid);
        if(patchdata->channel>0)
          patchdata->channel--;

	patchdata->preset=GFX_GetInteger(window,reqtype,"Preset: (1-128) ",1,128,program_state_is_valid);
        patchdata->preset--;

	return PATCH_SUCCESS;
}
*/


/******************* instrument ***********************/

#if 0
static int MIDIgetMaxVelocity(const struct Patch *patch){
	return 127;
}
#endif

  
void MIDICloseInstrument(struct Instruments *instrument){
  struct MidiPort *midi_port = g_midi_ports;
  while(midi_port != NULL){
    MIDI_closeMidiPortOs(midi_port->port);
    midi_port=midi_port->next;
  }

  MIDI_Delete();
}


void MIDI_init_track(struct Tracks *track){
	struct TrackInstrumentData *tid;
	tid=talloc(sizeof(struct TrackInstrumentData));
	track->midi_instrumentdata=tid;
}

void MIDIStopPlaying(struct Instruments *instrument){
  VECTOR_FOR_EACH(struct Patch *patch, &instrument->patches){

    struct PatchData *patchdata=(struct PatchData *)patch->patchdata;
    struct MidiPort *midi_port = patchdata->midi_port;
    int ch;

    for(ch=0;ch<16;ch++){
      int notenum;
      for(notenum=0;notenum<128;notenum++)
        while(midi_port->num_ons[ch][notenum] > 0)
          R_PutMidi3(midi_port,0x80|ch,notenum,0x00);
    }

  }END_VECTOR_FOR_EACH;
}

static void handle_fx_when_patch_is_replaced(struct Blocks *block,
                                             struct Tracks *track,
                                             const struct Patch *old_patch,
                                             struct Patch *new_patch,
                                             bool *has_paused,
                                             bool add_undo)
{
  bool has_made_undo = false;

  VECTOR_FOR_EACH(struct FXs *fxs, &track->fxs){
    struct FX *fx = fxs->fx;
    if (fx->patch == old_patch) {
      
      if ( (*has_paused)==false ){
        PC_Pause();
        *has_paused = true;
      }
      
      if (add_undo && has_made_undo==false){
        ADD_UNDO(Track_CurrPos(block->l.num, track->l.num));
        has_made_undo = true;
      }
      
      fx->patch = new_patch; // Only need to change patch. All patches use the same fx system.
    }
    
  }END_VECTOR_FOR_EACH;
}


static void MIDI_handle_fx_when_a_patch_has_been_replaced(const struct Patch *old_patch, struct Patch *new_patch, struct Blocks *only_check_this_block, struct Tracks *only_check_this_track, bool *has_paused){

  if (only_check_this_track != NULL) {

    handle_fx_when_patch_is_replaced(only_check_this_block, only_check_this_track, old_patch, new_patch, has_paused, false);

  } else {

    FOR_EACH_TRACK(){
      handle_fx_when_patch_is_replaced(NULL, track, old_patch, new_patch, has_paused, true);
    }END_FOR_EACH_TRACK;

  }
}

static void MIDI_remove_patchdata(struct Patch *patch){
  PLAYER_lock();{
    patch->patchdata = NULL;
  }PLAYER_unlock();
}

static void MIDI_PP_Update(struct Instruments *instrument,struct Patch *patch, bool is_loading){
  GFX_PP_Update(patch,is_loading);
}

int MIDI_initInstrumentPlugIn(struct Instruments *instrument){

  MIDI_set_use_0x90_for_note_off(SETTINGS_read_int("use_0x90_for_note_off",0)==0?false:true);

  if(MIDI_New(instrument)==false){
    GFX_Message(NULL, "Unable to open MIDI");
    return INSTRUMENT_FAILED;
  }

  instrument->instrumentname   = "MIDI instrument";
  //instrument->getMaxVelocity = &MIDIgetMaxVelocity;
  instrument->getFxNames       = MIDI_getFxNames;
  instrument->createFX         = MIDI_createFX;
  instrument->getFX            = &MIDIgetFX;
  //instrument->getPatch         = &MIDIgetPatch;
  instrument->CloseInstrument  = MIDICloseInstrument;
  instrument->StopPlaying      = MIDIStopPlaying;

  instrument->CopyInstrumentData = MIDI_CopyInstrumentData;
  instrument->PlaySongHook       = MIDIPlaySongHook;
  instrument->LoadFX             = MIDILoadFX;

  instrument->PP_Update = MIDI_PP_Update;

  instrument->handle_fx_when_a_patch_has_been_replaced = MIDI_handle_fx_when_a_patch_has_been_replaced;
  instrument->remove_patchdata                         = MIDI_remove_patchdata;

  instrument->setPatchData = MIDISetPatchData;
  instrument->getPatchData = MIDIGetPatchData;

  return INSTRUMENT_SUCCESS;
  
}






