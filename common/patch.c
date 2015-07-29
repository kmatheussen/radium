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
#include <string.h>


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

#include "undo.h"
#include "undo_tracks_proc.h"
#include "undo_patch_proc.h"
#include "windows_proc.h"
#include "notes_proc.h"
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

void PATCH_reset_time(void){
  VECTOR_FOR_EACH(struct Patch *patch,get_all_patches()){
    patch->last_time = 0;
  }END_VECTOR_FOR_EACH;
}

static void handle_fx_when_theres_a_new_patch_for_track(struct Tracks *track, struct Patch *old_patch, struct Patch *new_patch){

  // 1. Do instrument specific changes
  if(old_patch==NULL || new_patch==NULL)
    track->fxs = NULL;
  else if(old_patch->instrument == new_patch->instrument)
    old_patch->instrument->handle_fx_when_theres_a_new_patch_for_track(track,old_patch,new_patch);
  else
    track->fxs = NULL;

  // 2. Update fx->patch value
  struct FXs *fxs = track->fxs;
  if (fxs != NULL) {
    PLAYER_lock();{
      while(fxs!=NULL){
        struct FX *fx = fxs->fx;
        fx->patch = new_patch;
        fxs = NextFX(fxs);
      }
    }PLAYER_unlock();
  }

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

static struct Patch *PATCH_create(int instrumenttype, void *patchdata, const char *name){
  struct Patch *patch = talloc(sizeof(struct Patch));
  patch->id = PATCH_get_new_id();
  patch->is_usable = true;
  patch->forward_events = true;

  patch->name = talloc_strdup(name);
  patch->colornum = GFX_MakeRandomCustomColor(root->song->tracker_windows, -1);

  PATCH_init_voices(patch);

  if(instrumenttype==MIDI_INSTRUMENT_TYPE){
    MIDI_InitPatch(patch, patchdata);
  }else if(instrumenttype==AUDIO_INSTRUMENT_TYPE){
    AUDIO_InitPatch(patch, patchdata);
  }else{
    fprintf(stderr,"Unkown instrumenttype %d\n",instrumenttype);
    abort();
  }
  
  VECTOR_push_back(&patch->instrument->patches,patch);

  return patch;
}

struct Patch *NewPatchCurrPos(int instrumenttype, void *patchdata, const char *name){
  return PATCH_create(instrumenttype, patchdata, name);
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

  if(wtrack==NULL)
    return NULL;

  {
    struct Patch *patch=PATCH_create(instrumenttype, patchdata, name);

    {
      Undo_Track_CurrPos(window);
      handle_fx_when_theres_a_new_patch_for_track(wtrack->track,wtrack->track->patch,patch);
      wtrack->track->patch = patch;
    }

#if !USE_OPENGL
    UpdateFXNodeLines(window,wblock,wtrack);
#endif
    window->must_redraw = true;

    return patch;
  }

}

void PATCH_replace_patch_in_song(struct Patch *old_patch, struct Patch *new_patch){
  R_ASSERT_RETURN_IF_FALSE(Undo_Is_Open());

  struct Tracker_Windows *window = root->song->tracker_windows;
  struct WBlocks *wblock = window->wblocks;
    
  while(wblock!=NULL){
    
    struct WTracks *wtrack = wblock->wtracks;
    while(wtrack!=NULL){
      struct Tracks *track = wtrack->track;
      if(track->patch==old_patch){

        PlayStop();

        Undo_Track(window,wblock,wtrack,wblock->curr_realline);
        handle_fx_when_theres_a_new_patch_for_track(track,track->patch,new_patch);
        track->patch = new_patch;
        
      }
      wtrack = NextWTrack(wtrack);
    }
    wblock = NextWBlock(wblock);
  }    
}

static void remove_patch_from_song(struct Patch *patch){
  PATCH_replace_patch_in_song(patch, NULL);
}


void PATCH_delete(struct Patch *patch){

  R_ASSERT(Undo_Is_Open());

  remove_patch_from_song(patch);

  if(patch->instrument==get_audio_instrument()){
    Undo_MixerConnections_CurrPos();
    Undo_Chip_Remove_CurrPos(patch);
  }

  GFX_remove_patch_gui(patch);

  patch->instrument->remove_patch(patch);

  Undo_Patch_CurrPos();
  VECTOR_remove(&patch->instrument->patches,patch);

}

void PATCH_delete_CurrPos(struct Patch *patch){
  if(patch->instrument==get_audio_instrument())
    if(AUDIO_is_permanent_patch(patch)==true){
      GFX_Message(NULL,"Can not be deleted");
      return;
    }

  Undo_Open();{
    
    PATCH_delete(patch);

  }Undo_Close();

  DrawUpTrackerWindow(root->song->tracker_windows);
}

                   

void PATCH_select_patch_for_track(struct Tracker_Windows *window,struct WTracks *wtrack, bool use_popup){
  ReqType reqtype;

  vector_t *patches=get_all_patches();

  NInt num_patches=patches->num_elements;

  if(num_patches==0)
    return;

  if(use_popup==true)
    reqtype=NULL;
  else
    reqtype=GFX_OpenReq(window,70,(int)(num_patches+50),"Select Patch");

  vector_t v={0};
  VECTOR_push_back(&v,"<New MIDI Instrument>");
  VECTOR_push_back(&v,"<New Sample Player>");
  VECTOR_push_back(&v,"<New FluidSynth>");
  VECTOR_push_back(&v,"<New Pd Instrument>");
  VECTOR_push_back(&v,"<New Audio Instrument>");
  VECTOR_push_back(&v,"<Load New Preset>");

  VECTOR_FOR_EACH(struct Patch *patch,patches){
    VECTOR_push_back(&v,talloc_format("%d. %s",iterator666,patch->name));
  }END_VECTOR_FOR_EACH;

  {
    struct Patch *patch = NULL;

    int selection=GFX_Menu(window,reqtype,"Select Patch",&v);

    if(selection>=0){

      Undo_Open();{

        Undo_Track(window,window->wblock,wtrack,window->wblock->curr_realline);

        if(selection>=6){
          patch=patches->elements[selection-6];

        }else if(selection==0){
          patch = NewPatchCurrPos(MIDI_INSTRUMENT_TYPE, NULL, "Unnamed");
          GFX_PP_Update(patch);

        }else if(selection==1){
          SoundPlugin *plugin = add_new_audio_instrument_widget(PR_get_plugin_type_by_name(NULL, "Sample Player","Sample Player"),-100000,-100000,true,NULL,MIXER_get_bus(0),MIXER_get_bus(1));
          if(plugin!=NULL)
            patch = (struct Patch*)plugin->patch;
            
        }else if(selection==2){
          SoundPlugin *plugin = add_new_audio_instrument_widget(PR_get_plugin_type_by_name(NULL, "FluidSynth","FluidSynth"),-100000,-100000,true,NULL,MIXER_get_bus(0),MIXER_get_bus(1));
          if(plugin!=NULL)
            patch = (struct Patch*)plugin->patch;
            
        }else if(selection==3){
          SoundPlugin *plugin = add_new_audio_instrument_widget(PR_get_plugin_type_by_name(NULL, "Pd","Simple Midi Synth"),-100000,-100000,true,NULL,MIXER_get_bus(0),MIXER_get_bus(1));
          if(plugin!=NULL)
            patch = (struct Patch*)plugin->patch;

        }else if(selection==4){
          SoundPlugin *plugin = add_new_audio_instrument_widget(NULL,-100000,-100000,true,NULL,MIXER_get_bus(0),MIXER_get_bus(1));
          if(plugin!=NULL)
            patch = (struct Patch*)plugin->patch;

        }else if(selection==5){
          patch = InstrumentWidget_new_from_preset(NULL, NULL, -100000,-100000,true);

        }else
          printf("Unknown option\n");

        if(patch!=NULL){
          struct Tracks *track=wtrack->track;

          handle_fx_when_theres_a_new_patch_for_track(track,track->patch,patch);

          PLAYER_lock();{
            track->patch=patch;
          }PLAYER_unlock();

#if !USE_OPENGL
          UpdateFXNodeLines(window,window->wblock,wtrack);
#endif
          window->must_redraw = true;
              
          (*patch->instrument->PP_Update)(patch->instrument,patch);
        }

      }Undo_Close();

      if (patch==NULL)
        Undo_CancelLastUndo();
      
    } // if(selection>=0)

  }


  if(reqtype!=NULL)
    GFX_CloseReq(window,reqtype);
}


static bool has_recursive_event_connection(struct Patch *patch, struct Patch *start_patch){
  if(start_patch==patch)
    return true;

  int num_event_receivers = patch->num_event_receivers;
  int i;
  for(i=0; i<num_event_receivers; i++)
    if(has_recursive_event_connection(patch->event_receivers[i], start_patch)==true)
      return true;

  return false;
}

// Returns false if connection couldn't be made.
// MUST ONLY BE CALLED FROM mixergui/QM_chip.cpp:econnect (because PATCH_add_event_receiver must be called at the same time as SP_add_link)
bool PATCH_add_event_receiver(struct Patch *source, struct Patch *destination){
  int num_event_receivers = source->num_event_receivers;

  if(num_event_receivers==MAX_NUM_EVENT_RECEIVERS) {
    printf("No more free event receiver slots. (wow)\n");
    return false;
  }

  int i;
  for(i=0; i<num_event_receivers; i++)
    if(source->event_receivers[i]==destination){
      printf("Already connectied\n");
      return false;
    }

  if(has_recursive_event_connection(destination, source)==true) {
    GFX_Message(NULL, "Recursive event connections are not supported.\n(Send a feature request if you need it!)");
    printf("Recursive attempt\n");
    return false;
  }

  PLAYER_lock();{
    source->event_receivers[num_event_receivers] = destination;
    source->num_event_receivers = num_event_receivers+1;
  }PLAYER_unlock();

  return true;
}

// MUST ONLY BE CALLED FROM mixergui/QM_chip.cpp:PATCH_remove_event_receiver (because PATCH_add_event_receiver must be called at the same time as SP_remove_link)
void PATCH_remove_event_receiver(struct Patch *source, struct Patch *destination){
  int i;
  int num_event_receivers = source->num_event_receivers;

  PLAYER_lock();{
    for(i=0; i<num_event_receivers; i++){
      if(source->event_receivers[i]==destination){
        source->event_receivers[i] = source->event_receivers[num_event_receivers-1];
        source->num_event_receivers = num_event_receivers-1;
        //printf("Removed event receiver\n");
        break;
      }
    }
  }PLAYER_unlock();
}

void PATCH_remove_all_event_receivers(struct Patch *patch){
  int num_event_receivers = patch->num_event_receivers;

  PLAYER_lock();{
    int i;
    for(i=0; i<num_event_receivers; i++)
      patch->event_receivers[i] = NULL;

    patch->num_event_receivers = 0;
  }PLAYER_unlock();
}



void PATCH_call_very_often(void){
  struct Instruments *instrument = get_all_instruments();

  while(instrument!=NULL){

    VECTOR_FOR_EACH(struct Patch *patch, &instrument->patches){
      if (patch->widget_needs_to_be_updated) {
        patch->widget_needs_to_be_updated = false;
        GFX_update_instrument_widget(patch);
      }
    }END_VECTOR_FOR_EACH;

    instrument = NextInstrument(instrument);
  }

}


void PATCH_init(void){
  //MUTEX_INITIALIZE();
}


////////////////////////////////////
// Play note

static float get_voice_velocity(struct PatchVoice *voice){
  if(voice->volume<=35)
    return scale(voice->volume,-35,35,0,2);
  else
    return scale(voice->volume,35,70,2,7);
}

void RT_PATCH_send_play_note_to_receivers(struct Patch *patch, float notenum, int64_t note_id, float velocity, float pan, STime time){
  int i;

  if(note_id==-1)
    note_id = NotenumId(notenum);

  for(i = 0; i<patch->num_event_receivers; i++) {
    struct Patch *receiver = patch->event_receivers[i];
    R_ASSERT_RETURN_IF_FALSE(receiver!=patch); // unnecessary. We detect recursions when creating connections. (Not just once (which should have been enough) but both here and in SoundProducer.cpp)
    RT_PATCH_play_note(receiver, notenum, note_id, velocity, pan, time);
  }
}

static void RT_play_voice(struct Patch *patch, float notenum, int64_t note_id, float velocity, float pan, STime time){
  //printf("\n\n___RT_play_voice. note %d, time: %d, velocity: %d\n\n",notenum,(int)time,velocity);

  if(notenum < 1.0 || notenum>127)
    return;

#if 0
  if(track!=NULL && track->volumeonoff)
    velocity = PATCH_radiumvelocity_to_patchvelocity(patch,track->volume*velocity)/MAXTRACKVOL;
  else
    velocity = PATCH_radiumvelocity_to_patchvelocity(patch,velocity);

  float pan = 0.0f;

  if(track!=NULL && track->panonoff)
    pan = scale(track->pan,-MAXTRACKPAN,MAXTRACKPAN,-1.0f,1.0f);
#endif

  if(time < patch->last_time)
    time = patch->last_time;
  else 
    patch->last_time = time;

  Patch_addPlayingVoice(patch, notenum, note_id, pan);
  patch->playnote(patch,notenum,note_id,velocity,time,pan);

  patch->visual_note_intencity = MAX_NOTE_INTENCITY;

  if(patch->forward_events)
    RT_PATCH_send_play_note_to_receivers(patch, notenum, note_id, velocity, pan, time);
}

static void RT_scheduled_play_voice(int64_t time, const union SuperType *args){
  struct Patch *patch = args[0].pointer;

  float   notenum  = args[1].float_num;
  int64_t note_id  = args[2].int_num;
  float   velocity = args[3].float_num;
  float   pan      = args[4].float_num;

  //printf("playing scheduled play note: %d. time: %d, velocity: %d\n",notenum,(int)time,velocity);
  //return;

  RT_play_voice(patch,notenum,note_id,velocity,pan,time);
}


static void RT_scheduled_stop_voice(int64_t time_into_the_future, const union SuperType *args);

void RT_PATCH_play_note(struct Patch *patch, float notenum, int64_t note_id, float velocity, float pan, STime time){
  //printf("\n\n___Starting note %d, time: %d\n\n",notenum,(int)time);

  if(time==-1)
    time = patch->last_time;

  if(note_id==-1)
    note_id = NotenumId(notenum);

  Patch_addPlayingNote(patch, notenum, note_id, pan);

  float sample_rate = MIXER_get_sample_rate();

  int i;
  for(i=0;i<NUM_PATCH_VOICES;i++){
    struct PatchVoice *voice = &patch->voices[i];

    if(voice->is_on==true){

      float voice_notenum = notenum + voice->transpose;
      float voice_velocity = velocity * get_voice_velocity(voice);
      int64_t voice_id = note_id + i;

      union SuperType args[5];

      args[0].pointer = patch;
      args[1].float_num = voice_notenum;
      args[2].int_num = voice_id;
      args[3].float_num = voice_velocity;
      args[4].float_num = pan;

      // voice ON
      SCHEDULER_add_event(time + voice->start*sample_rate/1000, RT_scheduled_play_voice, &args[0], 5, SCHEDULER_NOTE_ON_PRIORITY);

      // voice OFF
      if(voice->length>0.001) // The voice decides when to stop by itself.
        SCHEDULER_add_event(time + (voice->start+voice->length)*sample_rate/1000, RT_scheduled_stop_voice, &args[0], 3, SCHEDULER_NOTE_OFF_PRIORITY);
    }
  }
}

//extern const char *NotesTexts3[131];

void PATCH_play_note(struct Patch *patch,float notenum,int64_t note_id,float velocity,float pan){
  //printf("** playing note %s\n",NotesTexts3[notenum]);
  PLAYER_lock();{
    RT_PATCH_play_note(patch,notenum,note_id,velocity,pan,-1);
  }PLAYER_unlock();
}


////////////////////////////////////
// Stop note

void RT_PATCH_send_stop_note_to_receivers(struct Patch *patch, float notenum,int64_t note_id,STime time){
  int i;

  if(note_id==-1)
    note_id = NotenumId(notenum);

  for(i = 0; i<patch->num_event_receivers; i++) {
    struct Patch *receiver = patch->event_receivers[i];
    RT_PATCH_stop_note(receiver, notenum, note_id, time);
  }
}

static void RT_stop_voice(struct Patch *patch, float notenum, int64_t note_id, STime time){
  if(notenum < 0.0 || notenum>127)
    return;

  if(note_id==-1)
    note_id = NotenumId(notenum);

#if 0
  if(track!=NULL && track->volumeonoff)
    velocity = PATCH_radiumvelocity_to_patchvelocity(patch,track->volume*velocity)/MAXTRACKVOL;
  else
    velocity = PATCH_radiumvelocity_to_patchvelocity(patch,velocity);
#endif

  patch->last_time = time;

  Patch_removePlayingVoice(patch, note_id);

  //printf("__stopping note: %d. time: %d\n",notenum,(int)time);
  patch->stopnote(patch, notenum, note_id, time);

  if(patch->forward_events)
    RT_PATCH_send_stop_note_to_receivers(patch, notenum, note_id, time);
}

static void RT_scheduled_stop_voice(int64_t time, const union SuperType *args){
  struct Patch *patch = args[0].pointer;

  float notenum = args[1].float_num;
  int64_t note_id = args[2].int_num;

  //printf("stopping scheduled play note: %d. time: %d, velocity: %d\n",notenum,(int)time,velocity);
  //return;

  RT_stop_voice(patch, notenum, note_id, time);
}

void RT_PATCH_stop_note(struct Patch *patch,float notenum,int64_t note_id,STime time){
  //printf("\n\nRT_PATCH_STOP_NOTE. ___Stopping note %d, time: %d\n\n",notenum,(int)time);

  if(time==-1)
    time = patch->last_time;

  if(note_id==-1)
    note_id = NotenumId(notenum);

  Patch_removePlayingNote(patch, note_id);

  float sample_rate = MIXER_get_sample_rate();

  int i;
  for(i=0;i<NUM_PATCH_VOICES;i++){
    struct PatchVoice *voice = &patch->voices[i];

    if(voice->is_on==true){

      if(voice->length<=0.001) { // i.e. this voice does not use a stopping time defined in the editor.

        float voice_notenum = notenum + voice->transpose;
        int64_t voice_id = note_id + i;

        union SuperType args[3];

        args[0].pointer = patch;
        args[1].float_num = voice_notenum;
        args[2].int_num = voice_id;
        
        SCHEDULER_add_event(time + voice->start*sample_rate/1000, RT_scheduled_stop_voice, &args[0], 3, SCHEDULER_NOTE_OFF_PRIORITY);
      }
    }
  }
}

void PATCH_stop_note(struct Patch *patch,float notenum,int64_t note_id){
  //printf("** stopping note %s\n\n",NotesTexts3[notenum]);
  PLAYER_lock();{
    RT_PATCH_stop_note(patch,notenum,note_id,-1);
  }PLAYER_unlock();
}



////////////////////////////////////
// Change velocity

void RT_PATCH_send_change_velocity_to_receivers(struct Patch *patch, float notenum, int64_t note_id, float velocity, STime time){
  int i;

  if(note_id==-1)
    note_id = NotenumId(notenum);

  for(i = 0; i<patch->num_event_receivers; i++) {
    struct Patch *receiver = patch->event_receivers[i];
    RT_PATCH_change_velocity(receiver, notenum, note_id, velocity, time);
  }
}

static void RT_change_voice_velocity(struct Patch *patch, float notenum, int64_t note_id, float velocity, STime time){
  if(notenum < 1 || notenum>127)
    return;

#if 0
  if(track!=NULL && track->volumeonoff)
    velocity = PATCH_radiumvelocity_to_patchvelocity(patch,track->volume*velocity)/MAXTRACKVOL;
  else
    velocity = PATCH_radiumvelocity_to_patchvelocity(patch,velocity);
#endif

  if(time==-1)
    time = patch->last_time;
  else
    patch->last_time = time;

  patch->changevelocity(patch,notenum,note_id,velocity,time);

  if(patch->forward_events)
    RT_PATCH_send_change_velocity_to_receivers(patch, notenum, note_id, velocity, time);
}

static void RT_scheduled_change_voice_velocity(int64_t time, const union SuperType *args){
  struct Patch *patch = args[0].pointer;

  float   notenum  = args[1].float_num;
  int64_t note_id  = args[2].int_num;
  float   velocity = args[3].float_num;

  //printf("stopping scheduled play note: %d. time: %d, velocity: %d\n",notenum,(int)time,velocity);
  //return;

  RT_change_voice_velocity(patch,notenum,note_id,velocity,time);
}

void RT_PATCH_change_velocity(struct Patch *patch,float notenum,int64_t note_id,float velocity,STime time){
  //printf("vel: %d\n",velocity);
  if(time==-1)
    time = patch->last_time;

  if(note_id==-1)
    note_id = NotenumId(notenum);

  float sample_rate = MIXER_get_sample_rate();

  int i;
  for(i=0;i<NUM_PATCH_VOICES;i++){
    struct PatchVoice *voice = &patch->voices[i];

    if(voice->is_on==true){

      float voice_notenum = notenum + voice->transpose;
      int64_t voice_id = note_id + i;
      float voice_velocity = velocity * get_voice_velocity(voice);

      // Should improve this. It might not play anymore. (???)

      union SuperType args[4];
      
      args[0].pointer = patch;
      args[1].float_num = voice_notenum;
      args[2].int_num = voice_id;
      args[3].float_num = voice_velocity;
      
      SCHEDULER_add_event(time + voice->start*sample_rate/1000, RT_scheduled_change_voice_velocity, &args[0], 4, SCHEDULER_VELOCITY_PRIORITY);
    }
  }
}

void PATCH_change_velocity(struct Patch *patch,float notenum,int64_t note_id,float velocity){
  PLAYER_lock();{
    RT_PATCH_change_velocity(patch,notenum,note_id,velocity,-1);
  }PLAYER_unlock();
}



////////////////////////////////////
// Change pitch

void RT_PATCH_send_change_pitch_to_receivers(struct Patch *patch, float notenum,int64_t note_id,float pitch,STime time){
  int i;

  if(note_id==-1)
    note_id = NotenumId(notenum);

  for(i = 0; i<patch->num_event_receivers; i++) {
    struct Patch *receiver = patch->event_receivers[i];
    RT_PATCH_change_pitch(receiver, notenum, note_id, pitch, time);
  }
}

static void RT_change_voice_pitch(struct Patch *patch, float notenum, int64_t note_id, float pitch, STime time){
  if(notenum < 1 || notenum>127)
    return;

  if(time==-1)
    time = patch->last_time;
  else
    patch->last_time = time;

  if(note_id==-1)
    note_id = NotenumId(notenum);

  //printf("Calling patch->changeptitch %d %f\n",notenum,pitch);
  patch->changepitch(patch,notenum,note_id,pitch,time);

  if(patch->forward_events)
    RT_PATCH_send_change_pitch_to_receivers(patch, notenum, note_id, pitch, time);
}

static void RT_scheduled_change_voice_pitch(int64_t time, const union SuperType *args){
  struct Patch *patch = args[0].pointer;

  float   notenum = args[1].float_num;
  int64_t note_id = args[2].int_num;
  float   pitch   = args[3].float_num;

  RT_change_voice_pitch(patch,notenum,note_id,pitch,time);
}

void RT_PATCH_change_pitch(struct Patch *patch,float notenum,int64_t note_id,float pitch,STime time){
  //printf("vel: %d\n",pitch);
  if(time==-1)
    time = patch->last_time;

  if(note_id==-1)
    note_id = NotenumId(notenum);

  float sample_rate = MIXER_get_sample_rate();

  int i;
  for(i=0;i<NUM_PATCH_VOICES;i++){
    struct PatchVoice *voice = &patch->voices[i];

    if(voice->is_on==true){

      float voice_notenum = notenum + voice->transpose;
      int64_t voice_id = note_id + i;
      float voice_pitch = pitch + voice->transpose;

      // Should improve this. It might not play anymore. (???)

      union SuperType args[4];
      
      args[0].pointer = patch;
      args[1].float_num = voice_notenum;
      args[2].int_num = voice_id;
      args[3].float_num = voice_pitch;
      
      SCHEDULER_add_event(time + voice->start*sample_rate/1000, RT_scheduled_change_voice_pitch, &args[0], 4, SCHEDULER_PITCH_PRIORITY);
    }
  }
}

void PATCH_change_pitch(struct Patch *patch,float notenum,int64_t note_id, float pitch){
  PLAYER_lock();{
    RT_PATCH_change_pitch(patch,notenum,note_id,pitch,-1);
  }PLAYER_unlock();
}



////////////////////////////////////
// Raw midi messages

void RT_PATCH_send_raw_midi_message_to_receivers(struct Patch *patch, uint32_t msg, STime time){
  int i;

  for(i = 0; i<patch->num_event_receivers; i++) {
    struct Patch *receiver = patch->event_receivers[i];
    RT_PATCH_send_raw_midi_message(receiver, msg, time);
  }
}

static void RT_send_raw_midi_message(struct Patch *patch, uint32_t msg, STime time){
  patch->sendrawmidimessage(patch,msg,time);

  if(patch->forward_events)
    RT_PATCH_send_raw_midi_message_to_receivers(patch, msg, time);
}

static void RT_scheduled_send_raw_midi_message(int64_t time, const union SuperType *args){
  struct Patch *patch = args[0].pointer;

  uint32_t msg = args[1].uint32_num;

  RT_send_raw_midi_message(patch, msg, time);
}

void RT_PATCH_send_raw_midi_message(struct Patch *patch, uint32_t msg, STime time){
  if(time==-1)
    time = patch->last_time;

  float sample_rate = MIXER_get_sample_rate();
    
  int i;
  for(i=0;i<NUM_PATCH_VOICES;i++){
    struct PatchVoice *voice = &patch->voices[i];

    if(voice->is_on==true){

      union SuperType args[2];
      
      args[0].pointer = patch;
      args[1].uint32_num = msg;
      
      SCHEDULER_add_event(time + voice->start*sample_rate/1000, RT_scheduled_send_raw_midi_message, &args[0], 2, SCHEDULER_RAWMIDIMESSAGE_PRIORITY);
    }
  }
}

void PATCH_send_raw_midi_message(struct Patch *patch, uint32_t msg){
  PLAYER_lock();{
    RT_PATCH_send_raw_midi_message(patch,msg,-1);
  }PLAYER_unlock();
}



////////////////////////////////////
// FX

// All FX goes through this function.

void RT_FX_treat_fx(struct FX *fx,int val,STime time,int skip, FX_when when){
  struct Patch *patch = fx->patch;
  R_ASSERT_RETURN_IF_FALSE(patch!=NULL);

  if(time==-1)
    time = patch->last_time;
  else
    patch->last_time = time;

  fx->treatFX(fx,val,time,skip,when);
}

void FX_treat_fx(struct FX *fx,int val,int skip){
  PLAYER_lock();{
    RT_FX_treat_fx(fx,val,-1,skip, FX_single);
  }PLAYER_unlock();
}


////////////////////////////////////
//

static void RT_PATCH_turn_voice_on(struct Patch *patch, int voicenum){ 

  struct PatchVoice *voice = &patch->voices[voicenum];

  float voice_velocity = get_voice_velocity(voice);

  if(voice->is_on==false){
    int i;

    for(i=0 ; i < patch->num_currently_playing_notes ; i++){
      PatchPlayingNote ppn = patch->playing_notes[i];

      RT_play_voice(patch,
                    ppn.note_num + voice->transpose,
                    ppn.note_id + voicenum,
                    voice_velocity,
                    ppn.pan,
                    -1
                    );
    }    
    voice->is_on = true;
  }
}

static void RT_PATCH_turn_voice_off(struct Patch *patch, int voicenum){ 

  struct PatchVoice *voice = &patch->voices[voicenum];

  if(voice->is_on==true){
    int i;

    for(i=0;i<patch->num_currently_playing_notes;i++){
      PatchPlayingNote ppn = patch->playing_notes[i];
      RT_stop_voice(patch,
                    ppn.note_num + voice->transpose,
                    ppn.note_id + voicenum,
                    -1
                    );
    }
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

  printf("STOP ALL NOTES on \"%s\".\n", patch->name);

  PLAYER_lock();{
    while(patch->num_currently_playing_voices > 0) {
      RT_stop_voice(patch,
                    patch->playing_voices[0].note_num, 
                    patch->playing_voices[0].note_id, 
                    -1
                    );
    }

    patch->num_currently_playing_notes = 0;

  }PLAYER_unlock();
}

void PATCH_playNoteCurrPos(struct Tracker_Windows *window, float notenum, int64_t note_id){
	struct Tracks *track=window->wblock->wtrack->track;
	struct Patch *patch=track->patch;

	if(patch==NULL || notenum<0 || notenum>127) return;

	PATCH_play_note(patch,notenum,note_id,TRACK_get_volume(track), TRACK_get_pan(track));
}


void PATCH_stopNoteCurrPos(struct Tracker_Windows *window,float notenum, int64_t note_id){
	struct Tracks *track=window->wblock->wtrack->track;
	struct Patch *patch=track->patch;

	if(patch==NULL || notenum<0 || notenum>127) return;

	PATCH_stop_note(patch,notenum,note_id);
}

// Must only be called if TRACK_has_peaks(track)==true.
int PATCH_get_peaks(struct Patch *patch,
                    float notenum, 
                    int ch, 
                    const struct Tracks *track, 
                    int64_t start_time, int64_t end_time, 
                    float *min_value, float *max_value
                    )
{
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

  for(i=0;i<NUM_PATCH_VOICES;i++){
    struct PatchVoice *voice = &patch->voices[i];

    // This didn't turn out very pretty.
    if(voice->is_on==true){

      float voice_notenum = notenum + (float)voice->transpose;

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
