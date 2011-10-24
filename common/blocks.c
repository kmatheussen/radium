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
	char *name
){
	int lokke;
	struct TempoNodes *temponode;

	block->num_tracks=num_tracks;
	block->num_lines=num_lines;
	block->name=name;
	block->reltempo=1.0f;

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
	
	for(lokke=0;lokke<block->num_tracks;lokke++){
		AppendTrack(block);
	}

	UpdateSTimes(block);

	ListAddElement1(&root->song->blocks,&block->l);
}

void AppendBlock(void){
	NInt blocknum=ListFindFirstFreePlace1(&root->song->blocks->l);
	struct Blocks *temp=talloc(sizeof(struct Blocks));

	temp->l.num=blocknum;
	NewBlock(temp,7,70,"NN");
	root->song->num_blocks++;
}

void AppendBlock_spes(int num_lines,NInt num_tracks){
	NInt blocknum=ListFindFirstFreePlace1(&root->song->blocks->l);
	struct Blocks *temp=talloc(sizeof(struct Blocks));

	temp->l.num=blocknum;
	NewBlock(temp,num_tracks,num_lines,"NN");
	root->song->num_blocks++;
}


