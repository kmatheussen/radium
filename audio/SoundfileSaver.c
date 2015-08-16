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
#include <sndfile.h>

#include "pa_memorybarrier.h"

#include "../common/nsmtracker.h"
#include "../common/playerclass.h"
#include "../common/player_proc.h"

#include "Mixer_proc.h"

#include "SoundfileSaver_proc.h"

extern struct Root *root;
extern PlayerClass *pc;


// Ouch. This turned out to be spaghetti. Should at least unify SoundfileSaver.c and Qt_soundfilesaver_widget_callbacks.cpp.


static bool g_saving_was_successful = true;
static SNDFILE *g_sndfile;
static const char *g_filename = NULL;
static float g_post_writing_left;


extern void SOUNDFILESAVERGUI_stop(const char *message);

enum SaveState{
  BEFORE_WRITING,
  IS_WRITING,
  POST_WRITING,
  AFTER_WRITING
};

static volatile enum SaveState g_save_state;

static bool stop_writing(void){
  char temp[1024];
  bool ret;
  int close_result = sf_close(g_sndfile);

  if(close_result==0)
    ret = true;
  else
    ret= false;

  if(ret==false)
    sprintf(temp,"Unable to save \"%s\": \"%s\".", g_filename, sf_error_number(close_result));

  else if(g_saving_was_successful==false)
    sprintf(temp,"\"%s\" was saved, but with errors.", g_filename);

  else
    sprintf(temp,"\"%s\" successfully saved.", g_filename);
    

  SOUNDFILESAVERGUI_stop(temp);

  return ret;
}

bool SOUNDFILESAVER_write(float **outputs, int num_frames){
 PaUtil_FullMemoryBarrier();

  if(g_save_state==BEFORE_WRITING && pc->isplaying==false)
    return true;

  if(g_save_state==AFTER_WRITING)
    return true;

  bool ret=true;
  int i;
  float interleaved_data[num_frames*2];

  int pos=0;
  for(i=0;i<num_frames;i++){
    interleaved_data[pos++] = outputs[0][i];
    interleaved_data[pos++] = outputs[1][i];
  }

  //printf("Writing %d frames\n",num_frames);

  if(sf_writef_float(g_sndfile, interleaved_data, num_frames) != num_frames){
    g_saving_was_successful = false;
    ret = false;
  }

  if(g_save_state==IS_WRITING){
    if(pc->isplaying==false){
      g_save_state=POST_WRITING;
    }
  }

  if(g_save_state==POST_WRITING){
    g_post_writing_left -= (float)num_frames/MIXER_get_sample_rate();
    if(g_post_writing_left <= 0.0f){
      stop_writing();
      g_save_state=AFTER_WRITING;
    }
  }

  return ret;
}

bool SOUNDFILESAVER_save(const char *filename, enum SOUNDFILESAVER_what what_to_save, float samplerate, int libsndfile_format, float post_recording_length, const char **error_string){

  PlayStop();

  {
    SF_INFO sf_info; memset(&sf_info,0,sizeof(sf_info));
    
    sf_info.samplerate = samplerate;
    sf_info.channels = 2;
    sf_info.format = libsndfile_format;
    
    {
      g_sndfile = sf_open(filename,SFM_WRITE,&sf_info);
      if(g_sndfile==NULL){
        printf("Why: \"%s\"\n",sf_strerror(NULL));
        if(error_string!=NULL)
          *error_string = sf_strerror(NULL);
        return false;
      }
    }
  }

  g_saving_was_successful = true;
  g_filename = talloc_strdup(filename);
  g_post_writing_left = post_recording_length;

  g_save_state=BEFORE_WRITING; PaUtil_FullMemoryBarrier();
  {
    MIXER_start_saving_soundfile();
    if(what_to_save==SAVE_SONG)
      PlaySongFromStart(root->song->tracker_windows);
    else
      PlayBlockFromStart(root->song->tracker_windows,false);
  }
  PaUtil_FullMemoryBarrier(); g_save_state=IS_WRITING;

  return true;
}


