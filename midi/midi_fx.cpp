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
#include "../common/visual_proc.h"
#include "../common/instruments_proc.h"
#include <string.h>
#include "disk_midi_fx_proc.h"

#include "midi_fx_proc.h"


static struct MIDI_FX MIDI_fxs[MIDI_NUM_FX]={ // [NO_STATIC_ARRAY_WARNING]
        {"Program Change",0,127,PROGRAMCHANGE_CC},

	{"-----------------------",0,0,-1},

	{"Channel Preassure",0,127,CHANNELPREASSURE_CC},

	{"-----------------------",0,0,-1},

	{"Pitch7",-64,63,PITCH7_CC},
	{"Pitch14",-0x2000,0x1fff,PITCH14_CC},

	{"-----------------------",0,0,-1},

	{"Modulation7.",0,127,1},
	{"Modulation14.",0,0x3fff,129},

	{"-----------------------",0,0,-1},

	{"Portamento7.",0,127,5},
	{"Portamento14.",0,0x3fff,5},

	{"-----------------------",0,0,-1},

	{"Volume7.",0,127,7},
	{"Volume14.",0,0x3fff,135},

	{"-----------------------",0,0,-1},

	{"Pan7.",-64,63,10},
	{"Pan14.",-0x2000,0x1fff,138},

	{"-----------------------",0,0,-1},

	{"Chorus.",0,127,93},
	{"Reverb.",0,127,91},
	{"Attack.",0,127,73},
	{"Release.",0,127,72},
	{"CutOff",0,127,74},
	{"Resonance",0,127,71},
	{"Tremolo",0,127,92},
	{"Phaser",0,127,95},

	{"-----------------------",0,0,-1},
	{"Other CC",0,0,OTHER_CC}
};

static const char *midi_fxs_fullnames[MIDI_NUM_FX]={ // [NO_STATIC_ARRAY_WARNING]
	"Program Change",

	"-----------------------",

	"Channel Preassure",

	"-----------------------",

	"Pitch7",
	"Pitch14",

	"-----------------------",

	"Modulation7.  CC> 1",
	"Modulation14. CC> 1-34",

	"-----------------------",

	"Portamento7.  CC> 5",
	"Portamento14. CC> 5-37",

	"-----------------------",

	"Volume7.      CC> 7",
	"Volume14.     CC> 7-39",

	"-----------------------",

	"Pan7.         CC> 10",
	"Pan14.        CC> 10-42",

	"-----------------------",

	"Chorus.       CC> 93",
	"Reverb.       CC> 91",
	"Attack.       CC> 73",
	"Release.      CC> 72",
	"CutOff        CC> 74",
	"Resonance     CC> 71",
	"Tremolo       CC> 92",
	"Phaser        CC> 95",

	"-----------------------",
	"Other CC",
};



/***************** FX **********************************/

static void treat_cc7(const struct Patch *patch, int effect_num, int val, STime time){
  //struct MIDI_FX *midi_fx=(struct MIDI_FX *)fx->fxdata;
	struct PatchData *patchdata;
	struct MidiPort *midi_port;
	int channel;

        R_ASSERT_RETURN_IF_FALSE(patch->instrument==get_MIDI_instrument());
                                 
	patchdata=(struct PatchData *)patch->patchdata;
        if (patchdata==NULL)
          return;
        
	midi_port=patchdata->midi_port;
	channel=patchdata->channel;

        const int skip = 0;
	PutMidi3_FX(midi_port,0xb0|channel,effect_num,val,time,10,skip);
}

static void MIDI_treatFX_CC7(struct SeqTrack *seqtrack,struct FX *fx,int val,STime time,int skip, FX_when when, double block_reltempo){
    const struct Patch *patch = fx->patch;
    R_ASSERT_RETURN_IF_FALSE(patch!=NULL);

    treat_cc7(patch,fx->effect_num, val,time);
}

static void treat_cc14(const struct Patch *patch, int effect_num, int val, STime time){
    //struct MIDI_FX *midi_fx=(struct MIDI_FX *)fx->fxdata;
	struct PatchData *patchdata;
	struct MidiPort *midi_port;
	int channel;

        R_ASSERT_RETURN_IF_FALSE(patch->instrument==get_MIDI_instrument());
        
	patchdata=(struct PatchData *)patch->patchdata;
        if (patchdata==NULL) // There is actually no way to delete a midi instrument, but it feels right to have this test anyway.
          return;
        
	midi_port=patchdata->midi_port;
	channel=patchdata->channel;

        const int skip = 0;
	PutMidi3_FX(midi_port,0xb0|channel,effect_num-128,val>>7,time,10,skip);
	PutMidi3_FX(midi_port,0xb0|channel,effect_num-128+32,val&127,time,10,skip);
}

static void MIDI_treatFX_CC14(struct SeqTrack *seqtrack,struct FX *fx,int val,STime time,int skip, FX_when when, double block_reltempo){
    const struct Patch *patch = fx->patch;
    R_ASSERT_RETURN_IF_FALSE(patch!=NULL);

    treat_cc14(patch,fx->effect_num,val,time);
}

static void treat_pan7(const struct Patch *patch, int val, STime time){
	struct PatchData *patchdata;
	struct MidiPort *midi_port;
	int channel;

        R_ASSERT_RETURN_IF_FALSE(patch->instrument==get_MIDI_instrument());
                
	patchdata=(struct PatchData *)patch->patchdata;
        if (patchdata==NULL)
          return;
        
	midi_port=patchdata->midi_port;
	channel=patchdata->channel;

        const int skip = 0;
	PutMidi3_FX(midi_port,0xb0|channel,10,val+0x40,time,10,skip);
}

static void MIDI_treatFX_Pan7(struct SeqTrack *seqtrack,struct FX *fx,int val,STime time,int skip, FX_when when, double block_reltempo){
  const struct Patch *patch = fx->patch;
  R_ASSERT_RETURN_IF_FALSE(patch!=NULL);

  treat_pan7(patch, val, time);
}


static void treat_pan14(const struct Patch *patch, int val, STime time){
	struct PatchData *patchdata;
	struct MidiPort *midi_port;
	int channel;

        R_ASSERT_RETURN_IF_FALSE(patch->instrument==get_MIDI_instrument());
                
	patchdata=(struct PatchData *)patch->patchdata;
        if (patchdata==NULL)
          return;
        
	midi_port=patchdata->midi_port;
	channel=patchdata->channel;

	val+=0x2000;

        const int skip = 0;
	PutMidi3_FX(midi_port,0xb0|channel,10,val>>7,time,10,skip);
	PutMidi3_FX(midi_port,0xb0|channel,42,val&127,time,10,skip);
}

static void MIDI_treatFX_Pan14(struct SeqTrack *seqtrack,struct FX *fx,int val,STime time,int skip, FX_when when, double block_reltempo){
  const struct Patch *patch = fx->patch;
  R_ASSERT_RETURN_IF_FALSE(patch!=NULL);

  treat_pan14(patch,val,time);
}

static void treat_program_change(const struct Patch *patch, int val, STime time){
	struct PatchData *patchdata;
	struct MidiPort *midi_port;
	int channel;

        R_ASSERT_RETURN_IF_FALSE(patch->instrument==get_MIDI_instrument());
        
	patchdata=(struct PatchData *)patch->patchdata;
        if (patchdata==NULL)
          return;
        
	midi_port=patchdata->midi_port;
	channel=patchdata->channel;

	PutMidi2(midi_port,0xc0|channel,val,time,50);
}

static void MIDI_treatFX_ProgramChange(struct SeqTrack *seqtrack,struct FX *fx,int val,STime time,int skip, FX_when when, double block_reltempo){
  const struct Patch *patch = fx->patch;
  R_ASSERT_RETURN_IF_FALSE(patch!=NULL);
  treat_program_change(patch, val, time);
}

static void treat_channel_preassure(const struct Patch *patch, int val, STime time){
	struct PatchData *patchdata;
	struct MidiPort *midi_port;
	int channel;

        R_ASSERT_RETURN_IF_FALSE(patch->instrument==get_MIDI_instrument());
        
	patchdata=(struct PatchData *)patch->patchdata;
        if (patchdata==NULL)
          return;
        
	midi_port=patchdata->midi_port;
	channel=patchdata->channel;

        const int skip = 0;
	PutMidi2_FX(midi_port,0xd0|channel,val,time,10,skip);
}

static void MIDI_treatFX_ChannelPreassure(struct SeqTrack *seqtrack,struct FX *fx,int val,STime time,int skip, FX_when when, double block_reltempo){
  const struct Patch *patch = fx->patch;
  R_ASSERT_RETURN_IF_FALSE(patch!=NULL);

  treat_channel_preassure(patch, val, time);
}

static void treat_pitch7(const struct Patch *patch, int val, STime time){
	struct PatchData *patchdata;
	struct MidiPort *midi_port;
	int channel;

        R_ASSERT_RETURN_IF_FALSE(patch->instrument==get_MIDI_instrument());

	patchdata=(struct PatchData *)patch->patchdata;
        if (patchdata==NULL)
          return;
        
	midi_port=patchdata->midi_port;
	channel=patchdata->channel;

        const int skip = 0;
	PutMidi3_FX(midi_port,0xe0|channel,0,val+0x40,time,10,skip);
}

static void MIDI_treatFX_Pitch7(struct SeqTrack *seqtrack,struct FX *fx,int val,STime time,int skip, FX_when when, double block_reltempo){
  const struct Patch *patch = fx->patch;
  R_ASSERT_RETURN_IF_FALSE(patch!=NULL);

  treat_pitch7(patch, val, time);
}

static void treat_pitch14(const struct Patch *patch, int val, STime time){
	struct PatchData *patchdata;
	struct MidiPort *midi_port;
	int channel;

        R_ASSERT_RETURN_IF_FALSE(patch->instrument==get_MIDI_instrument());

	patchdata=(struct PatchData *)patch->patchdata;
        if (patchdata==NULL)
          return;
        
	midi_port=patchdata->midi_port;
	channel=patchdata->channel;

	val+=0x2000;

        const int skip = 0;
	PutMidi3_FX(midi_port,0xe0|channel,val&127,val>>7,time,10,skip);
  
}
  
static void MIDI_treatFX_Pitch14(struct SeqTrack *seqtrack,struct FX *fx,int val,STime time,int skip, FX_when when, double block_reltempo){
  const struct Patch *patch = fx->patch;
  R_ASSERT_RETURN_IF_FALSE(patch!=NULL);

  treat_pitch14(patch, val, time);
}

static void MIDI_FX_call_me_before_starting_to_play_song_MIDDLE(struct FX *fx, int val, int64_t abstime, FX_when when){
  // The MIDI instrument will eventually be removed, and replaced by an audio instrument sending out midi. So we don't spend time implementing this.
}
  
static bool isFXUsed(struct TrackInstrumentData *tid,struct MIDI_FX *midi_fx){
	struct UsedTrackMidiCCs *usmf=tid->usmf;

	while(usmf!=NULL){
		if(midi_fx->effect_num==usmf->midi_fx->effect_num) return true;
		usmf=usmf->next;
	}

	return false;
}

void MIDI_closeFX(struct FX *fx,const struct Tracks *track){
	struct TrackInstrumentData *tid=(struct TrackInstrumentData *)track->midi_instrumentdata;
	struct UsedTrackMidiCCs *usmf=tid->usmf;
	struct UsedTrackMidiCCs *prev=NULL;
	struct MIDI_FX *midi_fx=(struct MIDI_FX *)fx->fxdata;

/*
	if(tid==NULL) printf("tid==NULL\n");
	if(usmf==NULL) printf("usmf==NULL\n");
*/

	while(usmf!=NULL){
		if(usmf->midi_fx==midi_fx){
			if(prev==NULL){
				tid->usmf=usmf->next;
			}else{
				prev->next=usmf->next;
			}
			return;
		}
		prev=usmf;
		usmf=usmf->next;
	}

	RError("Error in function 'MIDI_CloseFX' in file 'plug-ins/midi_fx.c'\n");
	return;
}

bool MIDISetTreatFX(struct FX *fx,struct MIDI_FX *midi_fx){
	switch(fx->effect_num){
		case PROGRAMCHANGE_CC:
			fx->treatFX=MIDI_treatFX_ProgramChange;
			break;
		case CHANNELPREASSURE_CC:
			fx->treatFX=MIDI_treatFX_ChannelPreassure;
			break;
		case PITCH7_CC:
			fx->treatFX=MIDI_treatFX_Pitch7;
			break;
		case PITCH14_CC:
			fx->treatFX=MIDI_treatFX_Pitch14;
			break;
		default:
			switch(midi_fx->max){
				case 127:
					fx->treatFX=MIDI_treatFX_CC7;
					break;
				case 0x3fff:
					fx->treatFX=MIDI_treatFX_CC14;
					break;
				case 0x3f:
					fx->treatFX=MIDI_treatFX_Pan7;
					break;
				case 0x1fff:
					fx->treatFX=MIDI_treatFX_Pan14;
					break;
				default:
					RError(
						"Error in function 'MIDIsetTreatFX' in file 'plug-ins/midi_fx.c'\n"
						"fx->effect_num: %d, midi_fx->max: %d\n",
						fx->effect_num,midi_fx->max
					);
					return false;
			}
			break;
	}
	return true;
}

static int scale_127(float scaled_value){
  return R_BOUNDARIES(0, scale(scaled_value, 0, 1, 0, 127), 127);
}

static int scale_0x3fff(float scaled_value){
  return R_BOUNDARIES(0, scale(scaled_value, 0, 1, 0, 0x3fff), 0x3fff);
}

static int scale_pitchpan7(float scaled_value){
  return R_BOUNDARIES(-64, scale(scaled_value, 0, 1, -64, 63), 63);
}

static int scale_pitchpan14(float scaled_value){
  return R_BOUNDARIES(-0x2000, scale(scaled_value, 0, 1, -0x2000, 0x1fff), 0x1fff);
}

// Called by the envelope controller
void MIDI_set_effect_value(const struct Patch *patch, STime time, int effect_num, float scaled_value){
  R_ASSERT_RETURN_IF_FALSE(patch!=NULL);
  
	switch(effect_num){
		case PROGRAMCHANGE_CC:
                  treat_program_change(patch, scale_127(scaled_value), time);
                  break;
		case CHANNELPREASSURE_CC:
                  treat_channel_preassure(patch, scale_127(scaled_value), time);
                  break;
		case PITCH7_CC:
                  treat_pitch7(patch, scale_pitchpan7(scaled_value), time);
                  break;
		case PITCH14_CC:                  
                  treat_pitch14(patch, scale_pitchpan14(scaled_value), time);
                  break;

                case 10: // pan7
                  //printf("PAN7. In: %f. Out: %d\n", scaled_value, scale_pitchpan7(scaled_value)); 
                  treat_pan7(patch, scale_pitchpan7(scaled_value), time);
                  break;
                  
                case 128+10: // pan14
                  treat_pan14(patch, scale_pitchpan7(scaled_value), time);
                  break;

		default:
                  if(effect_num < 128)
                    treat_cc7(patch, effect_num, scale_127(scaled_value), time);
                  else if (effect_num < (256-32))
                    treat_cc14(patch, effect_num, scale_0x3fff(scaled_value), time);
                  else
                    RError(
                           "Error in function 'MIDI_set_effect_value' in file 'plug-ins/midi_fx.c'\n"
                           "effect_num: %d. patch name: %s. Value: %f", effect_num, patch->name, scaled_value
                           );
                  break;
	}  
}


int MIDI_get_effect_num(const struct Patch *patch, const char *effect_name, char **error_message){
  for(int i = 0 ; i< MIDI_NUM_FX ; i++){
    struct MIDI_FX fx = MIDI_fxs[i];
    if (!strcmp(effect_name, fx.name))
      return fx.effect_num;
  }

  int ret = atoi(effect_name);
  if (ret < 0){
    return -1;
  }

  return ret;
}


// NOT called from RT thread
static int MIDI_default_FX_value(const struct FX *fx){
  return (fx->min + fx->max) / 2;
}

int MIDI_init_fx(const struct Tracks *track,struct FX *fx, struct MIDI_FX *midi_fx){

	struct TrackInstrumentData *tid=(struct TrackInstrumentData *)track->midi_instrumentdata;
	struct UsedTrackMidiCCs *usmf;

        fx->patch = track->patch;
        
        fx->effect_num = midi_fx->effect_num;

        int num_fx_colors = AUTOMATION8_COLOR_NUM - AUTOMATION1_COLOR_NUM;
        fx->color = (enum ColorNums)(AUTOMATION1_COLOR_NUM + (fx->effect_num%num_fx_colors));

	fx->fxdata=midi_fx;

	fx->name    = midi_fx->name;
	fx->min     = midi_fx->min;
	fx->max     = midi_fx->max;
        fx->is_enabled = true;
	fx->closeFX = &MIDI_closeFX;
	fx->SaveFX  = MIDISaveFX;

	if( ! MIDISetTreatFX(fx,midi_fx)){
          return FX_FAILED;
	}

        fx->call_me_before_starting_to_play_song_MIDDLE = MIDI_FX_call_me_before_starting_to_play_song_MIDDLE;
        
        fx->defaultFXValue = MIDI_default_FX_value;
  
	usmf          = (struct UsedTrackMidiCCs*)talloc(sizeof(struct UsedTrackMidiCCs));
	usmf->next    = tid->usmf;
	tid->usmf     = usmf;
	usmf->midi_fx = midi_fx;

	return FX_SUCCESS;
}

struct FX *MIDI_createFX(const struct Tracks *track, struct Patch *patch, int effect_num){
  //RError("MIDI_getFxNames is not implemented. Expect the unexpected.");

  R_ASSERT(track->patch==patch);

  if (track->patch != patch)
    patch = track->patch;
  
  //struct Patch *patch = track->patch; // patch can not be NULL (we got instrument through track-patch)

  struct MIDI_FX *midi_fx = &MIDI_fxs[effect_num];
        
  struct FX *fx=(struct FX *)talloc(sizeof(struct FX));

  int num_fx_colors = AUTOMATION8_COLOR_NUM - AUTOMATION1_COLOR_NUM;
  fx->color = (enum ColorNums)(AUTOMATION1_COLOR_NUM + (effect_num%num_fx_colors));

  if (MIDI_init_fx(track, fx, midi_fx)==FX_FAILED)
    return NULL;
  
  return fx;
}


int MIDI_getNumFxs(const struct Patch *patch){
  return MIDI_NUM_FX;
}

const char *MIDI_getFxName(const struct Patch *patch, int fxnum){
  if(fxnum<0 || fxnum >= MIDI_NUM_FX){
    RError("MIDI_getFxName %s - %d\n", patch->name, fxnum);
    return "";
  }
    
  return midi_fxs_fullnames[fxnum];
}

void MIDIgetFX(struct Tracker_Windows *window,const struct Tracks *track, std::function<void(struct FX*)> callback){

	//const char *menutitle="Select FX";

        vector_t v={};

	for(int lokke=0;lokke<MIDI_NUM_FX;lokke++)
          VECTOR_push_back(&v,midi_fxs_fullnames[lokke]);

        GFX_Menu3(v,[window, track, callback](int selection, bool onoff){

            if(-1==selection)
              return;
            
            struct MIDI_FX *midi_fx = &MIDI_fxs[selection];
            
            struct TrackInstrumentData *tid=(struct TrackInstrumentData *)track->midi_instrumentdata;
            
            //menutitle="FX already used";
            
            if(midi_fx->effect_num==-1){
              MIDIgetFX(window, track, callback);
              return;
            }
            
            if(midi_fx->effect_num==OTHER_CC){
              midi_fx=(struct MIDI_FX*)talloc(sizeof(struct MIDI_FX));
              
              ReqType reqtype = GFX_OpenReq(window,30,10,"");
              
              midi_fx->effect_num=GFX_GetInteger(window,reqtype,"CC >",0,127,true);
              if(midi_fx->effect_num==-1){
                GFX_CloseReq(window,reqtype);
                return;
              }
              
              midi_fx->max=127;
              
              if(midi_fx->effect_num<16){
                int onlymsb=-1;
                while(onlymsb==-1){
                  vector_t v={};
                  VECTOR_push_back(&v,"7");
                  VECTOR_push_back(&v,"14");
                  onlymsb=GFX_Menu(window,reqtype,"Resolution?",v,true);
                }
                if(onlymsb==1){
                  midi_fx->effect_num+=128;
                  midi_fx->max=0x3fff;
                }
              }
              
              if(isFXUsed(tid,midi_fx)){
                GFX_addMessage("CC %d is already used", midi_fx->effect_num);
                GFX_CloseReq(window,reqtype);
                return;
              }
              
            again:
              while(midi_fx->name==NULL)
                midi_fx->name=GFX_GetString(window,reqtype,"Name >",true);
              
              for(int lokke=0;lokke<(int)strlen(midi_fx->name);lokke++){
                if(midi_fx->name[lokke]==':' || midi_fx->name[lokke]=='/'){
                  midi_fx->name=GFX_GetString(window,reqtype,"(Name can not contain ':' or '/') >",true);
                  goto again;
                }
              }
              
              GFX_CloseReq(window,reqtype);
            }
            
            if( ! isFXUsed(tid,midi_fx)){
              struct FX *fx = (struct FX*)talloc(sizeof(struct FX));        
              MIDI_init_fx(track, fx, midi_fx);
              
              callback(fx);
            }
          });
}



/* Make a copy, necesarry for undo/redo and track-copy. */

void *MIDI_CopyInstrumentData(const struct Tracks *track){
	struct TrackInstrumentData *tid=(struct TrackInstrumentData *)track->midi_instrumentdata;
	struct UsedTrackMidiCCs *usmf=tid->usmf;
	struct UsedTrackMidiCCs *to_usmf;
	struct TrackInstrumentData *to=(struct TrackInstrumentData *)talloc(sizeof(struct TrackInstrumentData));
	struct UsedTrackMidiCCs **to_to_usmf=&to->usmf;

	while(usmf!=NULL){
                to_usmf=(struct UsedTrackMidiCCs *)talloc(sizeof(struct UsedTrackMidiCCs));
		*to_to_usmf=to_usmf;
		to_usmf->midi_fx=usmf->midi_fx;
		to_to_usmf=&to_usmf->next;
		usmf=usmf->next;
	}

	return to;
}





