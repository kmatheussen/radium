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


#include <stdio.h>
#include <stdlib.h>

#include "nsmtracker.h"

#include "list_proc.h"
#include "tracks_proc.h"
#include "tempos_proc.h"
#include "time_proc.h"
#include "Beats_proc.h"
#include "visual_proc.h"

#include "../midi/midi_i_input_proc.h"
#include "../api/api_proc.h"

#include "blocks_proc.h"


void CloseBlock(NInt blocknum){
	struct Blocks *temp=(struct Blocks *)ListFindElement1(&root->song->blocks->l,blocknum);
	if(temp==NULL) return;

	ListRemoveElement1(&root->song->blocks,&temp->l);
}


void NewBlock(
	struct Blocks *block,
	NInt num_tracks,
	int num_lines,
	const char *name
){
	int lokke;
	struct TempoNodes *temponode;

	block->num_tracks=num_tracks;
	block->num_lines=num_lines;
	block->name=name;
	ATOMIC_DOUBLE_SET(block->reltempo, 1.0);
        //ATOMIC_DOUBLE_SET(block->player_time, -200.0);

	temponode=(struct TempoNodes *)talloc(sizeof(struct TempoNodes));
	temponode->l.p.dividor=1;
	temponode->reltempo=0.0f;
	block->temponodes=temponode;

	temponode=(struct TempoNodes *)talloc(sizeof(struct TempoNodes));			//Doubt its safe to make this one atomic.
	temponode->l.p.line=block->num_lines-1;
	temponode->l.p.counter=MAX_UINT32-1;
	temponode->l.p.dividor=MAX_UINT32;
	temponode->reltempo=0.0f;
	block->temponodes->l.next= &temponode->l;
	block->lasttemponode=temponode;

        //block->color = GFX_mix_colors(GFX_MakeRandomColor(), GFX_get_color(HIGH_EDITOR_BACKGROUND_COLOR_NUM), 0.82f);
        block->color = GFX_MakeRandomBlockColor();
        
        TIME_everything_in_block_has_changed(block); // Create timings for bars and so forth, needed when calling TIME_block_num_tracks_have_changed in AppendTrack.

	for(lokke=0;lokke<block->num_tracks;lokke++){
		AppendTrack(block);
	}

        block->swing_enabled = true;

        TIME_everything_in_block_has_changed(block);
          
	ListAddElement1(&root->song->blocks,&block->l);
}

struct Blocks *AppendBlock(void){
        NInt blocknum = root->song->blocks==NULL ? 0 : ListFindFirstFreePlace1(&root->song->blocks->l);

	struct Blocks *block=(struct Blocks*)talloc(sizeof(struct Blocks));

	block->l.num=blocknum;
	NewBlock(block,7,64,"NN");
	root->song->num_blocks++;

        return block;
}

void AppendBlock_spes(int num_lines,NInt num_tracks){
	NInt blocknum=ListFindFirstFreePlace1(&root->song->blocks->l);
	struct Blocks *temp=(struct Blocks*)talloc(sizeof(struct Blocks));

	temp->l.num=blocknum;
	NewBlock(temp,num_tracks,num_lines,"NN");
	root->song->num_blocks++;
}


static DEFINE_ATOMIC(double, g_midi_learned_block_multiplier) = -1.0;

namespace{

struct BlockMultiplierMidiLearn;
  
static BlockMultiplierMidiLearn *g_block_multiplier_midi_learn;


struct BlockMultiplierMidiLearn final : public radium::MidiLearn {
    
  BlockMultiplierMidiLearn(const BlockMultiplierMidiLearn&) = delete;
  BlockMultiplierMidiLearn& operator=(const BlockMultiplierMidiLearn&) = delete;
    
  BlockMultiplierMidiLearn(hash_t *state = NULL)
  {
    g_block_multiplier_midi_learn = this;
    if (state != NULL)
      init_from_state(state);
  }

  bool RT_get_automation_recording_data(struct SoundPlugin **plugin, int *effect_num) override {
    return false;
  }

  void delete_me(void) override {
    g_block_multiplier_midi_learn = NULL;
    MIDI_remove_midi_learn(this, false);
    delete this;
  }
  
  void RT_callback(float reltempo) override {

    double new_reltempo = R_BOUNDARIES(
                                       MINBLOCKRELTIME,
                                       scale_double(reltempo,0,1,MINBLOCKRELTIME,MAXBLOCKRELTIME),
                                       MAXBLOCKRELTIME
                                       );
        
    if (is_playing_song()) {
      
      ATOMIC_DOUBLE_SET(g_midi_learned_block_multiplier, new_reltempo);
      
    } else {
      
      struct Blocks *block = ATOMIC_GET(g_curr_block);
      if (block != NULL) {
        
        ATOMIC_DOUBLE_SET(block->reltempo, new_reltempo);
        GFX_ScheduleRedraw();
      }
      
    }
  }

  QString get_dest_info(void) override {
    return "Block tempo multiplier";
  }
  
  /*
  hash_t *create_state(void) override {
    hash_t *state = MidiLearn::create_state();
    return state;
  }

  void init_from_state(hash_t *state) override {
    MidiLearn::init_from_state(state);
  }
  */
};

}

void BLOCKS_add_tempo_multiplier_midi_learn(void){
  if (g_block_multiplier_midi_learn == NULL)
    MIDI_add_midi_learn(new BlockMultiplierMidiLearn);
}

void BLOCKS_remove_tempo_multiplier_midi_learn(void){
  if (g_block_multiplier_midi_learn != NULL)
    g_block_multiplier_midi_learn->delete_me();
}

bool BLOCKS_has_tempo_multiplier_midi_learn(void){
  return g_block_multiplier_midi_learn != NULL;
}

void BLOCKS_called_very_often(void){
  if (g_block_multiplier_midi_learn==NULL)
    return;
  
  double reltempo = ATOMIC_DOUBLE_SET_RETURN_OLD(g_midi_learned_block_multiplier, -1);
  if (reltempo >= 0){
    setReltempo(reltempo);
  }
}

hash_t *BLOCKS_get_state(void){
  hash_t *state = HASH_create(3);
  
  if (g_block_multiplier_midi_learn != NULL)
    HASH_put_hash(state, "tempo_multiplier_midi_learn", g_block_multiplier_midi_learn->create_state());
  
  return state;
}

void BLOCKS_create_from_state(hash_t *state){
  BLOCKS_remove_tempo_multiplier_midi_learn();
  
  if (HASH_has_key(state, "tempo_multiplier_midi_learn"))
    MIDI_add_midi_learn(new BlockMultiplierMidiLearn(HASH_get_hash(state, "tempo_multiplier_midi_learn")));
}
