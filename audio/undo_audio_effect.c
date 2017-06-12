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
#include "../common/undo.h"
#include "../common/OS_Player_proc.h"

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"

#include "../Qt/Qt_instruments_proc.h"
#include "../mixergui/QM_chip.h"

#include "undo_audio_effect_proc.h"


extern struct Root *root;

struct Undo_AudioEffect{
  struct Patch *patch;

  struct{
    int effect_num; // if -1, 'values' is used instead.
    float value;
  };
  
  float *values;
};


static void *Undo_Do_AudioEffect(
                                 struct Tracker_Windows *window,
                                 struct WBlocks *wblock,
                                 struct WTracks *wtrack,
                                 int realline,
                                 void *pointer
                                 );

static void Undo_AudioEffect(
                             struct Tracker_Windows *window,
                             struct WBlocks *wblock,
                             struct Patch *patch,
                             int effect_num // if -1, all values are stored.
                             )
{
  struct Undo_AudioEffect *undo_ae=talloc(sizeof(struct Undo_AudioEffect));
  SoundPlugin *plugin = patch->patchdata;
  
  undo_ae->patch = patch;
  undo_ae->effect_num = effect_num;

  int num_effects = plugin->type->num_effects+NUM_SYSTEM_EFFECTS;
    
  if (effect_num==-1)
    undo_ae->values = tcopy_atomic(plugin->savable_effect_values, sizeof(float)*num_effects);
  else
    undo_ae->value = plugin->savable_effect_values[effect_num];


  //printf("********* Storing eff undo. value: %f %d\n",undo_ae->value,plugin->comp.is_on);

  Undo_Add_dont_stop_playing(
                             window->l.num,
                             wblock->l.num,
                             wblock->wtrack->l.num,
                             wblock->curr_realline,
                             undo_ae,
                             Undo_Do_AudioEffect,
                             talloc_format("Undo audio effect %s %d",patch->name, effect_num)
                             );

}

void ADD_UNDO_FUNC(AudioEffect_CurrPos(struct Patch *patch, int effect_num)){
  struct Tracker_Windows *window = root->song->tracker_windows;
  //printf("Undo_AudioEffect_CurrPos\n");
  Undo_AudioEffect(window,window->wblock, patch, effect_num);
}

static void *Undo_Do_AudioEffect(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){

  struct Undo_AudioEffect *undo_ae=pointer;
  SoundPlugin *plugin = undo_ae->patch->patchdata;

  int num_effects = plugin->type->num_effects+NUM_SYSTEM_EFFECTS;

  if (undo_ae->effect_num >= 0 && undo_ae->effect_num < num_effects) {
    float new_value = plugin->savable_effect_values[undo_ae->effect_num];

    printf("Calling Undo_do for %d. Current value: %f. Now setting it back to %f\n",undo_ae->effect_num,new_value,undo_ae->value);

    PLUGIN_set_effect_value(plugin,
                            0,
                            undo_ae->effect_num, 
                            undo_ae->value, 
                            PLUGIN_STORED_TYPE,
                            PLUGIN_STORE_VALUE,
                            FX_single
                            );

    undo_ae->value = new_value;

    if (undo_ae->effect_num==plugin->type->num_effects+EFFNUM_INPUT_VOLUME)
      CHIP_update(plugin);

    if (undo_ae->effect_num==plugin->type->num_effects+EFFNUM_VOLUME)
      CHIP_update(plugin);

  } else {

    R_ASSERT_RETURN_IF_FALSE2(undo_ae->effect_num==-1, undo_ae);

    float *new_values = tcopy_atomic(plugin->savable_effect_values, sizeof(float)*num_effects);
        
    PLAYER_lock();{
      for(int i=0;i<num_effects;i++)
        PLUGIN_set_effect_value(plugin,
                                0,
                                i,
                                undo_ae->values[i],
                                PLUGIN_STORED_TYPE,
                                PLUGIN_STORE_VALUE,
                                FX_single);
    }PLAYER_unlock();

    undo_ae->values = new_values;

    CHIP_update(plugin);
  }
    

  GFX_update_instrument_widget(undo_ae->patch);

  return undo_ae;
}

