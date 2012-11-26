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


#include <unistd.h>


#include "nsmtracker.h"
#include "visual_proc.h"
#include "list_proc.h"
#include "vector_proc.h"
#include "instruments_proc.h"
#include "gfx_wtrackheaders_proc.h"
#include "fxlines_proc.h"
#include "player_proc.h"
#include "OS_Player_proc.h"
#include "scheduler_proc.h"
#include "trackreallines_proc.h"

#include "undo.h"
#include "undo_tracks_proc.h"
#include "undo_patch_proc.h"
#include "windows_proc.h"
#include "../api/api_common_proc.h"
#include "../midi/midi_i_plugin_proc.h"
#include "../audio/audio_instrument_proc.h"
#include "../mixergui/undo_chip_addremove_proc.h"
#include "../mixergui/undo_mixer_connections_proc.h"

#include "../audio/SoundPlugin.h"
#include "../audio/SoundPluginRegistry_proc.h"
#include "../audio/Mixer_proc.h"
#include "../Qt/Qt_instruments_proc.h"

#include "patch_proc.h"


// A Major cleanup is needed for the patch/instrument system.
// (introduction of id numbers helped though)

extern struct Root *root;

struct Patch *g_currpatch=NULL;

static vector_t *get_all_patches(void){
  vector_t *v=talloc(sizeof(vector_t));
  struct Instruments *instrument = get_all_instruments();
  while(instrument!=NULL){
    VECTOR_append(v,&instrument->patches);
    instrument = NextInstrument(instrument);
  }
  return v;
}

int PATCH_get_new_id(void){
  vector_t *v=get_all_patches();
  int id=0;
  int i;
  for(i=0;i<v->num_elements;i++){
    struct Patch *patch=v->elements[i];
    if(patch->id>=id)
      id=patch->id+1;
  }

  return id;
}

struct Patch *PATCH_get_from_id(int id){
  vector_t *v=get_all_patches();
  int i;
  for(i=0;i<v->num_elements;i++){
    struct Patch *patch=v->elements[i];
    if(patch->id==id)
      return patch;
  }
  return NULL;
}

static void handle_fx_when_theres_a_new_patch_for_track(struct Tracks *track, struct Patch *old_patch, struct Patch *new_patch){
  if(old_patch==NULL || new_patch==NULL)
    track->fxs = NULL;
  else if(old_patch->instrument == new_patch->instrument)
    old_patch->instrument->handle_fx_when_theres_a_new_patch_for_track(track,old_patch,new_patch);
  else
    track->fxs = NULL;
}

void PATCH_init_voices(struct Patch *patch){
  patch->voices[0].is_on=true;

  patch->voices[1].transpose=12;
  patch->voices[2].transpose=19;
  patch->voices[3].transpose=24;
  patch->voices[4].transpose=-12;
  patch->voices[5].transpose=-24;

  patch->voices[0].time_format = TIME_IN_MS;
  patch->voices[1].time_format = TIME_IN_MS;
  patch->voices[2].time_format = TIME_IN_MS;
  patch->voices[3].time_format = TIME_IN_MS;
  patch->voices[4].time_format = TIME_IN_MS;
  patch->voices[5].time_format = TIME_IN_MS;
}

struct Patch *NewPatchCurrPos(int instrumenttype, void *patchdata, const char *name){
  struct Patch *patch=(struct Patch*)talloc(sizeof(struct Patch));
  patch->id = PATCH_get_new_id();
  
  patch->name = talloc_strdup(name);
  
  PATCH_init_voices(patch);
  
  if(instrumenttype==MIDI_INSTRUMENT_TYPE){
    MIDI_InitPatch(patch, patchdata);
  }else if(instrumenttype==AUDIO_INSTRUMENT_TYPE){
    AUDIO_InitPatch(patch, patchdata);
  }else{
    fprintf(stderr,"Unkown instrumenttype %d\n",instrumenttype);
    abort();
  }
  
  {
    VECTOR_push_back(&patch->instrument->patches,patch);
  }
  
  return patch;
}

struct Patch *NewPatchCurrPos_set_track(int instrumenttype, void *patchdata, const char *name){
  struct Tracker_Windows *window=NULL;
  struct WTracks *wtrack;
  struct WBlocks *wblock;

  wtrack=getWTrackFromNumA(
                           -1,
                           &window,
                           -1,
                           &wblock,
                           -1
                           );

  if(wtrack==NULL) return NULL;

  {
    struct Patch *patch=(struct Patch*)talloc(sizeof(struct Patch));
    patch->id = PATCH_get_new_id();

    patch->name = talloc_strdup(name);

    PATCH_init_voices(patch);

    if(instrumenttype==MIDI_INSTRUMENT_TYPE){
      MIDI_InitPatch(patch, patchdata);
    }else if(instrumenttype==AUDIO_INSTRUMENT_TYPE){
      AUDIO_InitPatch(patch, patchdata);
    }else{
      fprintf(stderr,"Unkown instrumenttype %d\n",instrumenttype);
      abort();
    }

    {
      Undo_Track_CurrPos(window);
      handle_fx_when_theres_a_new_patch_for_track(wtrack->track,wtrack->track->patch,patch);
      wtrack->track->patch = patch;
    }

    {
      VECTOR_push_back(&patch->instrument->patches,patch);
    }

    UpdateTrackReallines(window,wblock,wtrack);
    UpdateFXNodeLines(window,wblock,wtrack);
    DrawUpTrackerWindow(window);

    return patch;
  }

}

static void remove_patch_from_song(struct Patch *patch){
  struct Tracker_Windows *window = root->song->tracker_windows;
  struct WBlocks *wblock = window->wblocks;
  while(wblock!=NULL){
    struct WTracks *wtrack = wblock->wtracks;
    while(wtrack!=NULL){
      struct Tracks *track = wtrack->track;
      if(track->patch==patch){

        PlayHardStop();

        Undo_Track(window,wblock,wtrack,wblock->curr_realline);
        handle_fx_when_theres_a_new_patch_for_track(track,track->patch,NULL);
        track->patch = NULL;
        UpdateTrackReallines(window,wblock,wtrack);
        UpdateFXNodeLines(window,wblock,wtrack);
      }
      wtrack = NextWTrack(wtrack);
    }
    wblock = NextWBlock(wblock);
  }  
}

void PATCH_delete(struct Patch *patch){
  if(patch->instrument==get_audio_instrument())
    if(AUDIO_is_permanent_patch(patch)==true){
      GFX_Message("Can not be deleted");
      return;
    }

  Undo_Open();{

    remove_patch_from_song(patch);

    if(patch->instrument==get_audio_instrument()){
      Undo_MixerConnections_CurrPos();
      Undo_Chip_Remove_CurrPos(patch);
    }

    GFX_remove_patch_gui(patch);

    patch->instrument->remove_patch(patch);

    Undo_Patch_CurrPos();
    VECTOR_remove(&patch->instrument->patches,patch);

  }Undo_Close();

  DrawUpTrackerWindow(root->song->tracker_windows);
}

void PATCH_select_patch_for_track(struct Tracker_Windows *window,struct WTracks *wtrack, bool use_popup){
	ReqType reqtype;

	PlayStop();

        vector_t *patches=get_all_patches();

	NInt num_patches=patches->num_elements;

        if(num_patches==0)
          return;

        if(use_popup==true)
          reqtype=NULL;
        else
          reqtype=GFX_OpenReq(window,70,(int)(num_patches+50),"Select Patch");

        vector_t v={0};
        VECTOR_FOR_EACH(struct Patch *patch,patches){
          VECTOR_push_back(&v,patch->name);
        }END_VECTOR_FOR_EACH;

        VECTOR_push_back(&v,"<New MIDI Instrument>");
        VECTOR_push_back(&v,"<New Sample Player>");
        VECTOR_push_back(&v,"<New FluidSynth>");
        VECTOR_push_back(&v,"<New Audio Instrument>");

        {
          struct Patch *patch = NULL;

          int selection=GFX_Menu(window,reqtype,"Select Patch",&v);

          if(selection>=0){

            Undo_Open();{

              Undo_Track(window,window->wblock,wtrack,window->wblock->curr_realline);
              
              if(selection<num_patches){
                patch=patches->elements[selection];

              }else if(selection==num_patches){
                patch = NewPatchCurrPos(MIDI_INSTRUMENT_TYPE, NULL, "Unnamed");
                GFX_PP_Update(patch);

              }else if(selection==num_patches+1){
                SoundPlugin *plugin = add_new_audio_instrument_widget(PR_get_plugin_type_by_name("Sample Player","Sample Player"),-100000,-100000,true,NULL);
                if(plugin!=NULL)
                  patch = plugin->patch;
            
              }else if(selection==num_patches+2){
                SoundPlugin *plugin = add_new_audio_instrument_widget(PR_get_plugin_type_by_name("FluidSynth","FluidSynth"),-100000,-100000,true,NULL);
                if(plugin!=NULL)
                  patch = plugin->patch;
            
              }else if(selection==num_patches+3){
                SoundPlugin *plugin = add_new_audio_instrument_widget(NULL,-100000,-100000,true,NULL);
                if(plugin!=NULL)
                  patch = plugin->patch;

              }else
                printf("Unknown option\n");

              if(patch!=NULL){
                struct Tracks *track=wtrack->track;

                handle_fx_when_theres_a_new_patch_for_track(track,track->patch,patch);

                track->patch=patch;
              
                UpdateTrackReallines(window,window->wblock,wtrack);
                UpdateFXNodeLines(window,window->wblock,wtrack);
                DrawUpTrackerWindow(window);
              
                (*patch->instrument->PP_Update)(patch->instrument,patch);
              }

            }Undo_Close();
          }

        }


        if(reqtype!=NULL)
          GFX_CloseReq(window,reqtype);
}


void PATCH_init(void){
  //MUTEX_INITIALIZE();
}



int PATCH_radiumvelocity_to_patchvelocity(struct Patch *patch,int velocity){
  if(patch->instrument==NULL)
    return velocity;
  else
    return scale(velocity,
                 0,MAX_VELOCITY,
                 0,patch->instrument->getMaxVelocity(patch));
}

int PATCH_patchvelocity_to_radiumvelocity(struct Patch *patch,int velocity){
  if(patch->instrument==NULL)
    return velocity;
  else
    return scale(velocity,
                 0,patch->instrument->getMaxVelocity(patch),
                 0,MAX_VELOCITY);
}


////////////////////////////////////
// Play note

static float get_voice_velocity(struct PatchVoice *voice){
  if(voice->volume<=35)
    return scale(voice->volume,-35,35,0,2);
  else
    return scale(voice->volume,35,70,2,7);
}

static void RT_play_voice(struct Patch *patch, int notenum,int velocity,struct Tracks *track,STime time){
  //printf("\n\n___RT_play_voice. note %d, time: %d, velocity: %d\n\n",notenum,(int)time,velocity);

  if(notenum < 1 || notenum>127)
    return;

  if(track!=NULL && track->volumeonoff)
    velocity = PATCH_radiumvelocity_to_patchvelocity(patch,track->volume*velocity)/MAXTRACKVOL;
  else
    velocity = PATCH_radiumvelocity_to_patchvelocity(patch,velocity);

  float pan = 0.0f;

  if(track!=NULL && track->panonoff)
    pan = scale(track->pan,-MAXTRACKPAN,MAXTRACKPAN,-1.0f,1.0f);

  if(time < patch->last_time)
    time = patch->last_time;
  else 
    patch->last_time = time;

  patch->num_ons[notenum]++;
  patch->playnote(patch,notenum,velocity,time,pan);
}

static void RT_scheduled_play_voice(int64_t time, union SuperType *args){
  struct Tracks *track = args[0].pointer;
  struct Patch *patch = args[1].pointer;

  int notenum = args[2].int_num;
  int velocity = args[3].int_num;

  if(track!=NULL && track->patch != patch)
    return;

  //printf("playing scheduled play note: %d. time: %d, velocity: %d\n",notenum,(int)time,velocity);
  //return;

  RT_play_voice(patch,notenum,velocity,track,time);
}


static void RT_scheduled_stop_voice(int64_t time_into_the_future, union SuperType *args);

void RT_PATCH_play_note(struct Patch *patch,int notenum,int velocity,struct Tracks *track,STime time){
  //printf("\n\n___Starting note %d, time: %d\n\n",notenum,(int)time);

  if(time==-1)
    time = patch->last_time;

  patch->notes_num_ons[notenum]++;

  float sample_rate = MIXER_get_sample_rate();

  int i;
  for(i=0;i<MAX_PATCH_VOICES;i++){
    struct PatchVoice *voice = &patch->voices[i];

    if(voice->is_on==true){

      int voice_notenum = notenum + voice->transpose;
      int voice_velocity = velocity * get_voice_velocity(voice);

      union SuperType args[4];

      args[0].pointer = track;
      args[1].pointer = patch;
      args[2].int_num = voice_notenum;
      args[3].int_num = voice_velocity;

      // voice ON
      if(voice->start<0.001f){
        RT_play_voice(patch,voice_notenum,voice_velocity,track,time);
      }else{
        SCHEDULER_add_event(time + voice->start*sample_rate/1000, RT_scheduled_play_voice, &args[0], 4, SCHEDULER_ADD_AFTER_SAME_TIME);
      }
       
      // voice OFF
      if(voice->length>0.001) // The voice decides when to stop by itself.
        SCHEDULER_add_event(time + (voice->start+voice->length)*sample_rate/1000, RT_scheduled_stop_voice, &args[0], 4, SCHEDULER_ADD_BEFORE_SAME_TIME);
    }
  }
}


void PATCH_play_note(struct Patch *patch,int notenum,int velocity,struct Tracks *track){
  PLAYER_lock();{
    RT_PATCH_play_note(patch,notenum,velocity,track,-1);
  }PLAYER_unlock();
}


////////////////////////////////////
// Stop note

static void RT_stop_voice(struct Patch *patch, int notenum,int velocity,struct Tracks *track,STime time){
  if(notenum < 1 || notenum>127)
    return;

  if(track!=NULL && track->volumeonoff)
    velocity = PATCH_radiumvelocity_to_patchvelocity(patch,track->volume*velocity)/MAXTRACKVOL;
  else
    velocity = PATCH_radiumvelocity_to_patchvelocity(patch,velocity);

  patch->last_time = time;

  if(patch->num_ons[notenum]>0)
    patch->num_ons[notenum]--;

  //printf("__stopping note: %d. time: %d\n",notenum,(int)time);
  patch->stopnote(patch,notenum,velocity,time);
}

static void RT_scheduled_stop_voice(int64_t time, union SuperType *args){
  struct Tracks *track = args[0].pointer;
  struct Patch *patch = args[1].pointer;

  int notenum = args[2].int_num;
  int velocity = args[3].int_num;

  if(track!=NULL && track->patch != patch)
    return;

  //printf("stopping scheduled play note: %d. time: %d, velocity: %d\n",notenum,(int)time,velocity);
  //return;

  RT_stop_voice(patch,notenum,velocity,track,time);
}

void RT_PATCH_stop_note(struct Patch *patch,int notenum,int velocity,struct Tracks *track,STime time){
  //printf("\n\nRT_PATCH_STOP_NOTE. ___Stopping note %d, time: %d\n\n",notenum,(int)time);

  if(time==-1)
    time = patch->last_time;

  if(patch->notes_num_ons[notenum]>0)
    patch->notes_num_ons[notenum]--;

  float sample_rate = MIXER_get_sample_rate();

  int i;
  for(i=0;i<MAX_PATCH_VOICES;i++){
    struct PatchVoice *voice = &patch->voices[i];

    if(voice->is_on==true){

      if(voice->length<=0.001) { // i.e. this voice uses the stopping time which is defined in the editor.

        int voice_notenum = notenum + voice->transpose;
        int voice_velocity = velocity * get_voice_velocity(voice);

        if(voice->start<0.001f){

          RT_stop_voice(patch,voice_notenum,voice_velocity,track,time);

        }else{

          union SuperType args[4];

          args[0].pointer = track;
          args[1].pointer = patch;
          args[2].int_num = voice_notenum;
          args[3].int_num = voice_velocity;
          
          SCHEDULER_add_event(time + voice->start*sample_rate/1000, RT_scheduled_stop_voice, &args[0], 4, SCHEDULER_ADD_BEFORE_SAME_TIME);
        }
      }
    }
  }
}

void PATCH_stop_note(struct Patch *patch,int notenum,int velocity,struct Tracks *track){
  printf("stopping note %d / %d\n",notenum,velocity);
  PLAYER_lock();{
    RT_PATCH_stop_note(patch,notenum,velocity,track,-1);
  }PLAYER_unlock();
}



////////////////////////////////////
// Change velocity

static void RT_change_voice_velocity(struct Patch *patch, int notenum,int velocity,struct Tracks *track,STime time){
  if(notenum < 1 || notenum>127)
    return;

  if(track!=NULL && track->volumeonoff)
    velocity = PATCH_radiumvelocity_to_patchvelocity(patch,track->volume*velocity)/MAXTRACKVOL;
  else
    velocity = PATCH_radiumvelocity_to_patchvelocity(patch,velocity);

  if(time==-1)
    time = patch->last_time;
  else
    patch->last_time = time;

  patch->changevelocity(patch,notenum,velocity,time);
}

static void RT_scheduled_change_voice_velocity(int64_t time, union SuperType *args){
  struct Tracks *track = args[0].pointer;
  struct Patch *patch = args[1].pointer;

  int notenum = args[3].int_num;
  int velocity = args[4].int_num;

  if(track!=NULL && track->patch != patch)
    return;

  //printf("stopping scheduled play note: %d. time: %d, velocity: %d\n",notenum,(int)time,velocity);
  //return;

  RT_change_voice_velocity(patch,notenum,velocity,track,time);
}

void RT_PATCH_change_velocity(struct Patch *patch,int notenum,int velocity,struct Tracks *track,STime time){
  //printf("vel: %d\n",velocity);
  if(time==-1)
    time = patch->last_time;

  float sample_rate = MIXER_get_sample_rate();

  int i;
  for(i=0;i<MAX_PATCH_VOICES;i++){
    struct PatchVoice *voice = &patch->voices[i];

    if(voice->is_on==true){

      int voice_notenum = notenum + voice->transpose;
      int voice_velocity = velocity * get_voice_velocity(voice);

      if(voice->start<0.001f){

        RT_change_voice_velocity(patch,voice_notenum,voice_velocity,track,time);

      }else{
        
        // Should improve this. It might not play anymore.

        union SuperType args[4];

        args[0].pointer = track;
        args[1].pointer = patch;
        args[2].int_num = voice_notenum;
        args[3].int_num = voice_velocity;

        SCHEDULER_add_event(time + voice->start*sample_rate/1000, RT_scheduled_change_voice_velocity, &args[0], 4, SCHEDULER_ADDORDER_DOESNT_MATTER);
      }
    }
  }
}

void PATCH_change_velocity(struct Patch *patch,int notenum,int velocity,struct Tracks *track){
  PLAYER_lock();{
    RT_PATCH_change_velocity(patch,notenum,velocity,track,-1);
  }PLAYER_unlock();
}



////////////////////////////////////
// FX

// All FX goes through this function.

void RT_FX_treat_fx(struct FX *fx,int val,struct Tracks *track,STime time,int skip){
  if(track->patch!=NULL){ // This function should take patch as first argument. The track argument is [probably not / should not be] needed.
    if(time==-1)
      time = track->patch->last_time;
    else
      track->patch->last_time = time;
  }
  fx->treatFX(fx,val,track,time,skip);
}

void FX_treat_fx(struct FX *fx,int val,struct Tracks *track,int skip){
  if(track->patch==NULL){
    RError("FX_treat_fx: track->patch==NULL");
    return;
  }

  PLAYER_lock();{
    if(track->patch!=NULL)
      RT_FX_treat_fx(fx,val,track,-1,skip);
  }PLAYER_unlock();
}


////////////////////////////////////
//

static void RT_PATCH_turn_voice_on(struct Patch *patch, int voicenum){ 
  int notenum,i;

  struct PatchVoice *voice = &patch->voices[voicenum];

  int voice_velocity = root->standardvel * get_voice_velocity(voice);

  if(voice->is_on==false){
    for(notenum=0;notenum<128;notenum++)
      for(i=0;i<patch->notes_num_ons[notenum];i++)
        RT_play_voice(patch,notenum+voice->transpose,voice_velocity,NULL,-1);
    
    voice->is_on = true;
  }
}

static void RT_PATCH_turn_voice_off(struct Patch *patch, int voicenum){ 
  int notenum,i;

  struct PatchVoice *voice = &patch->voices[voicenum];

  if(voice->is_on==true){
    for(notenum=0;notenum<128;notenum++)
      for(i=0;i<patch->notes_num_ons[notenum];i++)
        RT_stop_voice(patch,notenum+voice->transpose,root->standardvel,NULL,-1);
    
    voice->is_on = false;
  }
}

void PATCH_turn_voice_on(struct Patch *patch, int voicenum){ 
  PLAYER_lock();{
    RT_PATCH_turn_voice_on(patch,voicenum);
  }PLAYER_unlock();
}

void PATCH_turn_voice_off(struct Patch *patch, int voicenum){ 
  PLAYER_lock();{
    RT_PATCH_turn_voice_off(patch,voicenum);
  }PLAYER_unlock();
}

void PATCH_change_voice_transpose(struct Patch *patch, int voicenum, int new_transpose){
  PLAYER_lock();{
    bool was_on = patch->voices[voicenum].is_on;

    if(was_on==true)
      RT_PATCH_turn_voice_off(patch,voicenum);
    
    patch->voices[voicenum].transpose = new_transpose;
    
    if(was_on==true)
      RT_PATCH_turn_voice_on(patch,voicenum);

  }PLAYER_unlock();
}

void PATCH_stop_all_notes(struct Patch *patch){
  int notenum;

  PLAYER_lock();{

    for(notenum=0;notenum<128;notenum++){
      while(patch->num_ons[notenum]>0)
        RT_stop_voice(patch,notenum,root->standardvel,NULL,-1);
      patch->notes_num_ons[notenum] = 0;
    }

  }PLAYER_unlock();
}

void PATCH_playNoteCurrPos(struct Tracker_Windows *window,int notenum){
	struct Tracks *track=window->wblock->wtrack->track;
	struct Patch *patch=track->patch;

	if(patch==NULL || notenum<0 || notenum>127) return;

	PATCH_play_note(patch,notenum,root->standardvel,track);
}


void PATCH_stopNoteCurrPos(struct Tracker_Windows *window,int notenum){
	struct Tracks *track=window->wblock->wtrack->track;
	struct Patch *patch=track->patch;

	if(patch==NULL || notenum<0 || notenum>127) return;

	PATCH_stop_note(patch,notenum,root->standardvel,track);
}

// Must only be called if TRACK_has_peaks(track)==true.
int PATCH_get_peaks(struct Patch *patch, int notenum, int ch, float start_velocity, float end_velocity, struct Tracks *track, int64_t start_time, int64_t end_time, float *min_value, float *max_value){
  int ret = 0;
  SoundPlugin *plugin=patch->patchdata;

  if(ch==-1) {

    if(track!=NULL && track->panonoff)
      return 2;
    else
      return plugin->type->get_peaks(plugin,
                                     0,
                                     -1,
                                     0,
                                     0,0,NULL,NULL);

  }

  float sample_rate = MIXER_get_sample_rate();

  float pan = 0.0f;

  if(track!=NULL && track->panonoff)
    pan = scale(track->pan,-MAXTRACKPAN,MAXTRACKPAN,-1.0f,1.0f);
                

  float min=0.0f;
  float max=0.0f;

  int i;

  for(i=0;i<MAX_PATCH_VOICES;i++){
    struct PatchVoice *voice = &patch->voices[i];

    // This didn't turn out very pretty.
    if(voice->is_on==true){

      int voice_notenum = notenum + voice->transpose;

      if(voice_notenum > 0 && voice_notenum<128){

        int64_t voice_start_time = start_time - voice->start*sample_rate/1000;

        if(voice_start_time > 0.0f){
          float min2;
          float max2;

          int64_t voice_end_time = end_time - voice->start*sample_rate/1000;

          if(voice->length<=0.001 || ( voice_start_time < (voice->length*sample_rate/1000))){
            
            ret = plugin->type->get_peaks(plugin,
                                          voice_notenum,
                                          ch,
                                          pan,
                                          voice_start_time,
                                          voice_end_time,
                                          &min2,
                                          &max2);
            
            min2 *= get_voice_velocity(voice);
            max2 *= get_voice_velocity(voice);
            
            if(min2<min)
              min=min2;
            if(max2>max)
              max=max2;
          }
        }
      }
    }
  }

  *min_value = min;
  *max_value = max;

  return ret;
}
