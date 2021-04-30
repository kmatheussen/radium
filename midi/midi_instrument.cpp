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
#include "../common/TimeData.hpp"
#include "../common/FX.hpp"
#include "../common/vector_proc.h"
#include "../common/visual_proc.h"
#include "../common/playerclass.h"
#include "../common/settings_proc.h"
#include "../common/instruments_proc.h"
#include "../common/fxlines_proc.h"
#include "../common/player_pause_proc.h"
#include "../common/undo_tracks_proc.h"

#include "midi_instrument.h"

#include "midi_ports_proc.h"
#include "midi_playfromstart_proc.h"
#include "midi_fx_proc.h"
#include "disk_midi_fx_proc.h"

//#include "../instrprop/Amiga_instrprop_edit_proc.h"
#include "OS_midi_proc.h"
#include "OS_midigfx_proc.h"



#include "midi_instrument_proc.h"


struct MidiPort{
  struct MidiPort *next;
  
  const char *name;

  radium::MidiOutputPort *port;
};



extern LANGSPEC void MIDI_send3(struct PatchData *patchdata, const int byte1, const int byte2, const int byte3){
  if (patchdata==NULL)
    return;
  
  struct MidiPort *midi_port = patchdata->midi_port;
  if (midi_port==NULL){
    R_ASSERT_NON_RELEASE(false);
    return;
  }

  MIDIPORT_send_message(midi_port->port,
                        byte1,
                        byte2,
                        byte3,
                        patchdata->LSB,
                        patchdata->MSB,
                        patchdata->preset
                        );
}

extern LANGSPEC void MIDI_send2(struct PatchData *patchdata, const int byte1, const int byte2){
  MIDI_send3(patchdata, byte1, byte2, 0);
}


/******************** notes **************************/

static void MIDIplaynote(struct SeqTrack *seqtrack,
                         struct Patch *patch,
                         note_t note,
                         STime time
){
	struct PatchData *patchdata=(struct PatchData *)patch->patchdata;

        if(patchdata==NULL)
          return;

	const int channel=patchdata->channel; // We don't use note.midi_channel here.

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

        MIDI_send3(patchdata, 0x90|channel, notenum, velocity);
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

  MIDI_send3(patchdata,
             (useOx90ForNoteOff==true?0x90:0x80)|patchdata->channel,
             notenum,
             0 //useOx90ForNoteOff==true?0:velocity,
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
        MIDI_send3(
                   patchdata,
                   0xa0|patchdata->channel,
                   notenum,
                   velocity);
}


static void MIDIchangepitch(struct SeqTrack *seqtrack,struct Patch *patch,note_t note,STime time){
}

static void MIDIchangepan(struct SeqTrack *seqtrack,struct Patch *patch,note_t note,STime time){
}

static void MIDIsendrawmidimessage(struct SeqTrack *seqtrack,struct Patch *patch,uint32_t msg,STime time, double block_reltempo){
}

/******************** patch **************************/

static void MIDIchangeTrackPan(int newpan,const struct Tracks *track){
	struct PatchData *patchdata=(struct PatchData *)track->patch->patchdata;

        if(patchdata==NULL)
          return;

        MIDI_send3(patchdata,
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

static struct PatchData *createPatchData(void);
  
static struct PatchData *getPatchData(struct Patch *patch){
  struct PatchData *ret = NULL;
  
  if (patch==NULL){
    R_ASSERT(false);
  } else if (patch->instrument != get_MIDI_instrument()) {
    R_ASSERT(false);
  } else {
    ret = (struct PatchData *)patch->patchdata;
    R_ASSERT(ret!=NULL);
  }

  if (ret==NULL)
    ret = createPatchData();
  
  return ret;
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

static const char *MIDIGetPatchData(struct Patch *patch, const char *key){
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

static void MIDIclosePatch(struct Patch *patch){
	return;
}

static struct PatchData *createPatchData(void) {
  struct PatchData *patchdata=(struct PatchData *)talloc(sizeof(struct PatchData));
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
    const char *portname = MIDI_OS_getDefaultOutputPort();
    if (portname==NULL)
      portname = "default";
    patchdata->midi_port = MIDIgetPort(NULL,NULL,portname,false);
  }

  return patchdata;
}


//static hash_t g_midi_ports = {};
struct MidiPort *g_midi_ports = NULL;

static bool is_member(const char *name, const char**names){
  int i=0;
  while(names[i]!=NULL){
    if(!strcmp(name,names[i]))
      return true;
    i++;
  }
  return false;
}

const char **MIDI_getPortNames(int *retsize, bool is_input){
  int num_os_names;
  const char **os_names = is_input ? MIDI_OS_getInputPortNames(&num_os_names) : MIDI_OS_getOutputPortNames(&num_os_names);

  int num_midi_ports = 0;
  struct MidiPort *midi_port = g_midi_ports;

  while(midi_port != NULL) {
    midi_port = midi_port->next;
    num_midi_ports++;
  }

  *retsize = num_os_names;
  const char **ret = (const char**)talloc((num_os_names+num_midi_ports+1)*sizeof(char*));
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

const char *MIDIrequestPortName(struct Tracker_Windows *window, ReqType reqtype, bool is_input, bool program_state_is_valid){
  int num_ports;

  const char **portnames=MIDI_getPortNames(&num_ports, is_input);
  vector_t v={};
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
    midi_port = (struct MidiPort*)talloc(sizeof(struct MidiPort));
    midi_port->name = talloc_strdup(name);
    midi_port->port = MIDIPORT_open_output(name, true, true);
    created_new_port = true;
  }

  int num_input_ports;
  const char **input_port_names = MIDIPORT_get_editor_input_ports(&num_input_ports);
  for(int i=0;i<num_input_ports;i++){
    printf("   Testing -%s- vs. -%s-\n", input_port_names[i], midi_port->name);
    const char *portname = midi_port->name;
    if (!strncmp(portname,"Radium: ",8))
      portname += 8;

    if (!strcmp(input_port_names[i], portname)){

      static hash_t *dont_ask_again = NULL;
      if (dont_ask_again==NULL)
        dont_ask_again = HASH_create(10);

      if (HASH_has_key(dont_ask_again, portname)==false){
        vector_t v = {};
        
        int no = VECTOR_push_back(&v, "No");(void)no;
        int yes = VECTOR_push_back(&v, "Yes");
        int yes_dont_ask_again = VECTOR_push_back(&v, "Yes (don't ask again)");
        
        int result = GFX_Message2(&v, program_state_is_valid, "Are you sure you want to connect to \"%s\"? We are also connected to an input port with the same name. If this device sends out its input, you risk starting a recursive connection that will be impossible to stop.", midi_port->name);

        if (result==yes_dont_ask_again)
          HASH_put_bool(dont_ask_again, portname, true);
        
        if (g_user_interaction_enabled==false || (result!=yes && result!=yes_dont_ask_again))
          return MIDIgetPort(window, reqtype, NULL, program_state_is_valid);
      }
      
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
  patch->changepan=MIDIchangepan;
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
    MIDIPORT_close_output(midi_port->port);
    midi_port=midi_port->next;
  }

  MIDIPORT_shut_down();
}


void MIDI_init_track(struct Tracks *track){
	struct TrackInstrumentData *tid;
	tid=(struct TrackInstrumentData *)talloc(sizeof(struct TrackInstrumentData));
	track->midi_instrumentdata=tid;
}

static void MIDIStopPlaying(struct Instruments *instrument){
  /*
  double times[R_MAX(1, instrument->patches.num_elements)];
  int i=0;
  double time = TIME_get_ms();
  */
  
  VECTOR_FOR_EACH(struct Patch *, patch, &instrument->patches){
    struct PatchData *patchdata=(struct PatchData *)patch->patchdata;
    struct MidiPort *midi_port = patchdata->midi_port;

    if (midi_port!=NULL && midi_port->port!=NULL)      
      MIDIPORT_stop_all_notes(midi_port->port);
    
  }END_VECTOR_FOR_EACH;

  /*
  for(int i=0;i<instrument->patches.num_elements;i++){
    printf("%d: %f\n", i, times[i]-time);
  }
  */
}

static void handle_fx_when_patch_is_replaced(struct Blocks *block,
                                             struct Tracks *track,
                                             const struct Patch *old_patch,
                                             struct Patch *new_patch,
                                             bool *has_paused,
                                             bool add_undo)
{
  bool has_made_undo = false;

  VECTOR_FOR_EACH(struct FXs *, fxs, &track->fxs){
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

const char* MIDI_get_port_name(const struct PatchData *patchdata){
  return patchdata->midi_port->name;
}

static void MIDI_remove_patchdata(struct Patch *patch){
  PLAYER_lock();{
    patch->patchdata = NULL;
  }PLAYER_unlock();
}

static void MIDI_PP_Update(struct Instruments *instrument,struct Patch *patch, bool is_loading){
  GFX_PP_Update(patch,is_loading);
}

#define APP_GetVars()                                            \
  struct Patch *patch=PATCH_get_current();                       \
  do{                                                            \
    if(patch->instrument!=get_MIDI_instrument())                 \
      return 0;                                                  \
  }while(0);




int MIDIResetAllControllers( void )
{
  printf("midiresetallcontrollers called\n");
	/* routine when (sub)item "Reset All Controllers" is selected. */

	APP_GetVars()
	struct PatchData *patchdata;
	int channel;

	patchdata=(struct PatchData *)patch->patchdata;

	for(channel=0;channel<16;channel++)
          MIDI_send3(patchdata, 0xb0|channel, 121, 0);

	return 0;
}

int MIDILocalKeyboardOn( void )
{
	/* routine when (sub)item "Local Keyboard On" is selected. */

	APP_GetVars()
	struct PatchData *patchdata;
	int channel;

	patchdata=(struct PatchData *)patch->patchdata;

	for(channel=0;channel<16;channel++){
          MIDI_send3(patchdata, 0xb0|channel,122,127);
	}

	return 0;
}

int MIDILocalKeyboardOff( void )
{
	/* routine when (sub)item "Local Keyboard Off" is selected. */

	APP_GetVars()
	struct PatchData *patchdata;
	int channel;

	patchdata=(struct PatchData *)patch->patchdata;

	for(channel=0;channel<16;channel++){
          MIDI_send3(patchdata, 0xb0|channel,122,0);
	}

	return 0;
}

int MIDIAllNotesOff( void )
{
	/* routine when (sub)item "All Notes Off" is selected. */

	APP_GetVars()
	struct PatchData *patchdata;
	int channel;

	patchdata=(struct PatchData *)patch->patchdata;

	for(channel=0;channel<16;channel++){
          MIDI_send3(patchdata, 0xb0|channel,123,0);
	}

	return 0;
}


int MIDIAllSoundsOff( void )
{
	/* routine when (sub)item "All Sounds Off" is selected. */

	APP_GetVars()
	struct PatchData *patchdata;
	int channel;

	patchdata=(struct PatchData *)patch->patchdata;

	for(channel=0;channel<16;channel++){
          MIDI_send3(patchdata, 0xb0|channel,120,0);
	}

	return 0;
}

int MIDISetInputPort(bool program_state_is_valid){
  const char *portname = MIDIrequestPortName(root->song->tracker_windows,NULL,true,program_state_is_valid);
  if(portname!=NULL){
    if (!MIDI_has_editor_input_port(portname))
      MIDI_add_editor_input_port(portname);
  }
  return 0;
}

int MIDI_initInstrumentPlugIn(struct Instruments *instrument){

  MIDI_set_use_0x90_for_note_off(SETTINGS_read_int("use_0x90_for_note_off",0)==0?false:true);

  MIDIPORT_init();

  instrument->instrumentname   = "MIDI instrument";
  //instrument->getMaxVelocity = &MIDIgetMaxVelocity;
  instrument->getNumFxs        = MIDI_getNumFxs;
  instrument->getFxName        = MIDI_getFxName;
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






