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



#include "../common/nsmtracker.h"
#include "../common/disk.h"
#include "../common/vector_proc.h"
#include "../common/visual_proc.h"
#include "../common/instruments_proc.h"
#include "../common/player_proc.h"
#include "../common/OS_visual_input.h"

#include "SoundPlugin.h"
#include "Mixer_proc.h"

#include "SoundPlugin_proc.h"

#include "../mixergui/QM_MixerWidget.h"

#include "audio_instrument_proc.h"

#define MAX_FX_VAL (1<<16)
//(1<<30)

/* Audio Patch */

static void AUDIO_playnote(struct Patch *patch,int notenum,int velocity,STime time){
  SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;

  if(plugin->type->play_note != NULL)
    plugin->type->play_note(plugin, PLAYER_get_delta_time(time), notenum, velocity/(float)MAX_FX_VAL);
  //plugin->type->play_note(plugin, MIXER_get_block_delta_time(time), notenum, velocity/(float)MAX_FX_VAL);

  //printf("playing audio note %d, velocity: %d, delta time: %d. Absolute time: %d\n",notenum,velocity,(int)MIXER_get_block_delta_time(time),(int)time);
}

static void AUDIO_changevelocity(struct Patch *patch,int notenum,int velocity,STime time){
  SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;

  //printf("audio velocity changed: %d. Time: %d\n",velocity,(int)MIXER_get_block_delta_time(time));

  if(plugin->type->set_note_volume != NULL)
    plugin->type->set_note_volume(plugin, PLAYER_get_delta_time(time), notenum, velocity/(float)MAX_FX_VAL);
}

static void AUDIO_stopnote(struct Patch *patch,int notenum,int velocity,STime time){
  SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;

  //printf("stopping audio note %d\n",notenum);

  if(plugin->type->stop_note != NULL)
    plugin->type->stop_note(plugin, PLAYER_get_delta_time(time), notenum, 0.0f);//note->velocity/127.0f);
}

static void AUDIO_closePatch(struct Patch *patch){
}

static void AUDIO_changeTrackPan(int newpan,struct Tracks *track){
}


// Note that patchdata is NULL when called from disk_patches.c
void AUDIO_InitPatch(struct Patch *patch, void *patchdata) {
  patch->playnote       = AUDIO_playnote;
  patch->stopnote       = AUDIO_stopnote;
  patch->changevelocity = AUDIO_changevelocity;
  patch->closePatch     = AUDIO_closePatch;
  patch->changeTrackPan = AUDIO_changeTrackPan;

  patch->patchdata = patchdata;

  patch->instrument=get_audio_instrument();    
}



/* Audio Instrument */
#if 0
typedef struct{
  int effect_num;
} AUDIO_FX_data_t;
#endif

static int AUDIO_getMaxVelocity(struct Patch *patch){
  return MAX_FX_VAL;
}

static void AUDIO_close_FX(struct FX *fx,struct Tracks *track){
  //struct Patch *patch = track->patch;

  printf("AUDIO_close_FX called for track %d\n",track->l.num);
  //SoundPlugin *plugin = (SoundPlugin*) track->patch->patchdata;
  //AUDIO_FX_data_t *fxdata = (AUDIO_FX_data_t*)fx->fxdata;

  //OS_SLIDER_release_automation_pointers(patch,fx->effect_num);
}

static void AUDIO_treat_FX(struct FX *fx,int val,struct Tracks *track,STime time,int skip){
  SoundPlugin *plugin = (SoundPlugin*) track->patch->patchdata;
  //AUDIO_FX_data_t *fxdata = (AUDIO_FX_data_t*)fx->fxdata;

  float effect_val = val / (float)MAX_FX_VAL;

  PLUGIN_set_effect_value(plugin,PLAYER_get_delta_time(time),fx->effect_num,effect_val, PLUGIN_NONSTORED_TYPE, PLUGIN_DONT_STORE_VALUE);
}

static void AUDIO_save_FX(struct FX *fx,struct Tracks *track);
static void *AUDIO_LoadFX(struct FX *fx,struct Tracks *track);

static void init_fx(struct FX *fx, int effect_num, const char *name){

  //AUDIO_FX_data_t *fxdata = talloc_atomic(sizeof(AUDIO_FX_data_t));
  fx->effect_num      = effect_num;
  //fx->fxdata          = fxdata;

  fx->num   = effect_num;
  fx->name    = name;
  fx->min     = 0;
  fx->max     = MAX_FX_VAL;
  fx->closeFX = AUDIO_close_FX;
  fx->SaveFX  = AUDIO_save_FX;
  fx->treatFX = AUDIO_treat_FX;

#if 0
  plugin->num_automations[selection]++;
  plugin->automation_colors[selection]=fx->color;
#endif

}

static int AUDIO_getFX(struct Tracker_Windows *window,struct Tracks *track,struct FX *fx){
  struct Patch *patch = track->patch;
  SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;
  const SoundPluginType *plugin_type = plugin->type;

  const char *menutitle="Select FX";
  
  int num_effects = plugin_type->num_effects+NUM_SYSTEM_EFFECTS;
  vector_t v={0};

  if(num_effects==0){
    VECTOR_push_back(&v,"No effects available");
    GFX_Menu(window,NULL,"No FX available",&v);
    return FX_FAILED;
  }

  int i;
  for(i=0;i<num_effects;i++)
    VECTOR_push_back(&v,PLUGIN_get_effect_name(plugin_type, i));
  
  int selection=GFX_Menu(window,NULL,menutitle,&v);
  if(-1==selection)
    return FX_FAILED;

  init_fx(fx,selection,(const char*)v.elements[selection]);

  return FX_SUCCESS;
}

static void AUDIO_save_FX(struct FX *fx,struct Tracks *track){
  printf("AUDIO_save_FX called for track %d\n",track->l.num);

  DC_start("FXDATA");{

    DC_SSI("num",fx->effect_num);
    DC_SSS("name",fx->name);

  }DC_end();
}

static void *AUDIO_LoadFX(struct FX *fx,struct Tracks *track){
  static char **objs=NULL;
  static char *vars[2]={"num","name"};

  init_fx(fx,0,"Effect name was not set in file (\?\?\?)");

  GENERAL_LOAD(0,2)

var0:
	fx->effect_num = DC_LoadI(); // the effect num may change later, if the plugin has implemented get_effect_num, and it returns a different value.
	goto start;

var1:
        fx->name = DC_LoadS();
	goto start;

var2:
var3:
var4:
var5:
var6:
var7:
var8:
var9:
var10:
var11:
var12:
var13:
var14:
var15:
var16:
var17:
var18:

obj0:
obj1:
obj2:
obj3:
obj4:
obj5:
obj6:

error:
end:
        return NULL;
}

extern struct Root *root;

void DLoadAudioInstrument(void){
  struct Blocks *block = root->song->blocks;
  while(block!=NULL){
    struct Tracks *track = block->tracks;
    while(track!=NULL){
      struct Patch *patch = track->patch;
      struct FXs *fxs=track->fxs;
      while(fxs!=NULL){
        struct FX *fx = fxs->fx;

        SoundPlugin *plugin = patch->patchdata;
        if(plugin!=NULL){
          const SoundPluginType *type = plugin->type;
          if(type->get_effect_num!=NULL){
            fx->effect_num = PLUGIN_get_effect_num(type, fx->name);
            fx->num = fx->effect_num;
          }
        }

        fxs = NextFX(fxs);
      }
      track = NextTrack(track);
    }
    block = NextBlock(block);
  }
}

static int AUDIO_getPatch(struct Tracker_Windows *window,ReqType reqtype,struct Tracks *track,struct Patch *patch){
  return PATCH_SUCCESS;
}

static void AUDIO_CloseInstrument(struct Instruments *instrument){}
//static void AUDIO_InitTrack(struct Instruments *instrument,struct Tracks *track){}
static void AUDIO_StopPlaying(struct Instruments *instrument){
}

static void AUDIO_PP_Update(struct Instruments *instrument,struct Patch *patch){
  GFX_PP_Update(patch);
}

static void *AUDIO_CopyInstrumentData(struct Tracks *track){
  return NULL;
}

static void AUDIO_PlayFromStartHook(struct Instruments *instrument){}

static void AUDIO_handle_fx_when_theres_a_new_patch_for_track(struct Tracks *track, struct Patch *old_patch, struct Patch *new_patch){
  SoundPlugin *old_plugin = (SoundPlugin*) old_patch->patchdata;
  const SoundPluginType *old_type = old_plugin->type;
  int num_old_effects = old_type->num_effects;

  SoundPlugin *new_plugin = (SoundPlugin*) new_patch->patchdata;
  const SoundPluginType *new_type = new_plugin->type;
  int num_new_effects = new_type->num_effects;

  if(true
     && !strcmp(old_type->type_name, new_type->type_name)
     && !strcmp(old_type->name,      new_type->name)
     )
    return;

  struct FXs *fxs = track->fxs;
  while(fxs!=NULL){
    struct FXs *next = NextFX(fxs);
    {
      struct FX *fx = fxs->fx;
      if(fx->effect_num >= num_old_effects){
        fx->effect_num = num_new_effects + (fx->effect_num - num_old_effects);
        fx->slider_automation_value = OS_SLIDER_obtain_automation_value_pointer(new_patch,fx->effect_num);
        fx->slider_automation_color = OS_SLIDER_obtain_automation_color_pointer(new_patch,fx->effect_num);
      }else{
        ListRemoveElement1(&track->fxs, &fxs->l);
      }
    }
    fxs = next;
  }
}

static void AUDIO_remove_patch(struct Patch *patch){
  printf("AUDIO_remove_patch serves no purpose.\n");
  //SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;
  //const SoundPluginType *plugin_type = plugin->type;
  
  //PLUGIN_delete_plugin(plugin);
}

static void AUDIO_setPatchData(struct Patch *patch, char *key, char *value){}
static char *AUDIO_getPatchData(struct Patch *patch, char *key){
  return NULL;
}


extern SoundPlugin *g_system_bus;

bool AUDIO_is_permanent_patch(struct Patch *patch){
  SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;
  if(plugin==get_main_pipe() || !strcmp(plugin->type->type_name,"Bus"))
    return true;
  else
    return false;
}

#if 0
// I Don't trust this one. The patch is not always created when this function is called, and is patch->patchdata always cleared when a plugin is not used anymore? In case not, this function could return the wrong patch instead of returning NULL.
struct Patch *AUDIO_get_patch_for_plugin(SoundPlugin *plugin){
  VECTOR_FOR_EACH(struct Patch *patch,&get_audio_instrument()->patches){
    if(patch->patchdata==plugin)
      return patch;
  }END_VECTOR_FOR_EACH;
  return NULL;
}
#endif

int AUDIO_initInstrumentPlugIn(struct Instruments *instrument){
  instrument->instrumentname = "Audio instrument";

  instrument->getMaxVelocity      = AUDIO_getMaxVelocity;
  instrument->getFX               = AUDIO_getFX;
  instrument->getPatch            = AUDIO_getPatch;
  instrument->CloseInstrument     = AUDIO_CloseInstrument;
  //instrument->InitTrack           = AUDIO_InitTrack;
  instrument->StopPlaying         = AUDIO_StopPlaying;

  instrument->CopyInstrumentData = AUDIO_CopyInstrumentData;
  instrument->PlayFromStartHook  = AUDIO_PlayFromStartHook;
  instrument->LoadFX             = AUDIO_LoadFX;

  instrument->PP_Update = AUDIO_PP_Update;

  instrument->handle_fx_when_theres_a_new_patch_for_track=AUDIO_handle_fx_when_theres_a_new_patch_for_track;
  instrument->remove_patch = AUDIO_remove_patch;

  instrument->setPatchData = AUDIO_setPatchData;
  instrument->getPatchData = AUDIO_getPatchData;

  return INSTRUMENT_SUCCESS;
}
