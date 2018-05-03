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

#include "blocks_proc.h"


extern struct Root *root;

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

	temponode=talloc(sizeof(struct TempoNodes));
	temponode->l.p.dividor=1;
	temponode->reltempo=0.0f;
	block->temponodes=temponode;

	temponode=talloc(sizeof(struct TempoNodes));			//Doubt its safe to make this one atomic.
	temponode->l.p.line=block->num_lines-1;
	temponode->l.p.counter=MAX_UINT32-1;
	temponode->l.p.dividor=MAX_UINT32;
	temponode->reltempo=0.0f;
	block->temponodes->l.next= &temponode->l;
	block->lasttemponode=temponode;

        block->color = GFX_mix_colors(GFX_MakeRandomColor(), GFX_get_color(HIGH_EDITOR_BACKGROUND_COLOR_NUM), 0.82f);

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

	struct Blocks *block=talloc(sizeof(struct Blocks));

	block->l.num=blocknum;
	NewBlock(block,7,64,"NN");
	root->song->num_blocks++;

        return block;
}

void AppendBlock_spes(int num_lines,NInt num_tracks){
	NInt blocknum=ListFindFirstFreePlace1(&root->song->blocks->l);
	struct Blocks *temp=talloc(sizeof(struct Blocks));

	temp->l.num=blocknum;
	NewBlock(temp,num_tracks,num_lines,"NN");
	root->song->num_blocks++;
}


