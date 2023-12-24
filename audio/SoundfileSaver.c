/* Copyright 2012 Kjetil S. Matheussen

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

#include "pa_memorybarrier.h"

#define INCLUDE_SNDFILE_OPEN_FUNCTIONS 1

#include "../common/nsmtracker.h"
#include "../common/playerclass.h"
#include "../common/player_proc.h"
#include "../common/instruments_proc.h"

#include "SoundPlugin.h"
#include "Sampler_plugin_proc.h"
#include "Mixer_proc.h"

#include "../api/api_proc.h"


#include "SoundfileSaver_proc.h"


extern struct Root *root;
extern PlayerClass *pc;


// Ouch. This turned out to be spaghetti. Should at least unify SoundfileSaver.c and Qt_soundfilesaver_widget_callbacks.cpp.


static bool g_saving_was_successful = true;
static SNDFILE *g_sndfile;
static const wchar_t *g_filename = NULL;
static float g_post_writing_left;
static int g_num_ch;


enum SaveState{
  BEFORE_STARTING_PLAYER,
  WAITING_FOR_PLAYER_TO_START, // This state is needed when using jack transport since is_playing() doesn't return true right after starting to play when using jack transport.
  IS_WRITING,
  POST_WRITING,
  AFTER_WRITING
};

static volatile enum SaveState g_save_state;
static int g_num_times_waiting_for_player_to_start = 0;

static DEFINE_ATOMIC(bool, stop_requested) = false;

static void set_resamplers(bool set_min_type, enum ResamplerType min_type){
  vector_t patches = get_audio_instrument()->patches;
  
  VECTOR_FOR_EACH(struct Patch *patch, &patches){
    SoundPlugin *plugin=(SoundPlugin*)patch->patchdata;
    if (plugin!=NULL){
      if(!strcmp("Sample Player", plugin->type->type_name)){
        if (set_min_type){
          enum ResamplerType type  = SAMPLER_get_resampler_type(plugin);
          SAMPLER_set_temp_resampler_type(plugin, R_MAX(min_type, type));
        } else {
          SAMPLER_set_org_resampler_type(plugin);
        }
      }else{
        //R_ASSERT_NON_RELEASE(false); // ??
      }
    }
  }END_VECTOR_FOR_EACH;
}

// main thread
void SOUNDFILESAVER_writer_has_been_stopped(void){
  set_resamplers(false, 0);
}

// audio thread
static bool stop_writing(bool is_cancelled){
  char temp[1024];
  bool ret;
  int close_result = sf_close(g_sndfile);

  if(close_result==0)
    ret = true;
  else
    ret= false;

  if (is_cancelled)
	  snprintf(temp,1023,"Cancelled");
  
  else if(ret==false)
    snprintf(temp,1023,"Unable to save \"%S\": \"%s\".", g_filename, sf_error_number(close_result));

  else if(g_saving_was_successful==false)
    snprintf(temp,1023,"\"%S\" was saved, but with errors.", g_filename);

  else
    snprintf(temp,1023,"\"%S\" saved successfully.", g_filename);
    

  SOUNDFILESAVERGUI_stop(temp);

  MIXER_set_all_non_realtime(false);

  g_save_state=AFTER_WRITING;

  return ret;
}

void SOUNDFILESAVER_request_stop(void){
  printf("  REQUESTING STOP\n");
  ATOMIC_SET(stop_requested, true);
}

bool SOUNDFILESAVER_write(float **outputs, int num_ch, int num_frames){
 PaUtil_FullMemoryBarrier();

 if (ATOMIC_GET(stop_requested)){
   ATOMIC_SET(stop_requested, false);
   stop_writing(true);
   return false;
 }

  if(g_save_state==BEFORE_STARTING_PLAYER && is_playing()==false)
    return true;

  if(g_save_state==WAITING_FOR_PLAYER_TO_START){
    if (is_playing()==false && g_num_times_waiting_for_player_to_start < 5*44100/RADIUM_BLOCKSIZE){
      g_num_times_waiting_for_player_to_start++;
      return true;
    }else
      g_save_state=IS_WRITING;
  }
 
  if(g_save_state==AFTER_WRITING)
    return true;

  bool ret=true;
  int i;
  float interleaved_data[num_frames*g_num_ch];

  int pos=0;
  for(i=0;i<num_frames;i++){
    for(int ch=0;ch<g_num_ch;ch++)
      if(ch<num_ch)
        interleaved_data[pos++] = outputs[ch][i];
      else
        interleaved_data[pos++] = 0.0;
  }

  //printf("Writing %d frames\n",num_frames);

  if(sf_writef_float(g_sndfile, interleaved_data, num_frames) != num_frames){
    g_saving_was_successful = false;
    ret = false;
  }

  if(g_save_state==IS_WRITING){
    if(is_playing()==false){
      g_save_state=POST_WRITING;
    }
  }

  if(g_save_state==POST_WRITING){
    g_post_writing_left -= (float)num_frames/MIXER_get_sample_rate();
    if(g_post_writing_left <= 0.0f){
      stop_writing(false);
    }
  }

  return ret;
}

bool SOUNDFILESAVER_save(filepath_t filename, enum SOUNDFILESAVER_what what_to_save, float samplerate, int libsndfile_format, int num_ch, float post_recording_length, enum ResamplerType min_resampler_type, const char **error_string){

  PlayStop();

  g_num_ch = num_ch;
  
  set_resamplers(true, min_resampler_type);
  
  {
    SF_INFO sf_info; memset(&sf_info,0,sizeof(sf_info));
    
    sf_info.samplerate = samplerate;
    sf_info.channels = num_ch;
    sf_info.format = libsndfile_format;
    
    {
      g_sndfile = radium_sf_open(filename,SFM_WRITE,&sf_info);
      if(g_sndfile==NULL){
        printf("Why: \"%s\"\n",sf_strerror(NULL));
        if(error_string!=NULL)
          *error_string = sf_strerror(NULL);
        return false;
      }
    }
  }

  g_saving_was_successful = true;
  g_filename = talloc_wcsdup(filename.id);
  g_post_writing_left = post_recording_length;

  
  MIXER_set_all_non_realtime(true);
  

  g_save_state=BEFORE_STARTING_PLAYER; PaUtil_FullMemoryBarrier();
  {
    MIXER_start_saving_soundfile();
    if(what_to_save==SAVE_SONG)
      PlaySongFromStart();
    else if(what_to_save==SAVE_BLOCK)
      PlayBlockFromStart(root->song->tracker_windows,false);
    else
      PlayRangeFromStart(root->song->tracker_windows);
    g_num_times_waiting_for_player_to_start = 0;
  }
  PaUtil_FullMemoryBarrier(); g_save_state=WAITING_FOR_PLAYER_TO_START;

  return true;
}


