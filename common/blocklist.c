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


#include <string.h>
#include "nsmtracker.h"
#include "player_proc.h"

#include "blocklist_proc.h"

extern struct Root *root;


void BL_init(void){
#ifdef TRACKER_DEBUG
	int lokke;
#endif

	root->song->playlist=talloc_atomic(sizeof(struct Blocks *));
	root->song->playlist[0]=root->song->blocks;
	root->song->length=1;

	/*
#ifdef TRACKER_DEBUG
	debug("playlen: %d\n",root->song->length);
	for(lokke=0;lokke<root->song->length;lokke++){
		debug("pos: %d, block: %d\n",lokke,root->song->playlist[lokke]->l.num);
	}
#endif
	*/
}

void BL_insert(int pos,struct Blocks *block){
	int lokke;
	struct Blocks **temp;
	struct Blocks **playlist=root->song->playlist;

	PlayStop();

	root->song->length++;

	temp=talloc_atomic(root->song->length*sizeof(struct Blocks *));
	memcpy(temp,playlist,(root->song->length-1)*sizeof(struct Blocks *));

	temp[pos]=block;

	for(lokke=pos;lokke<root->song->length-1;lokke++){
		temp[lokke+1]=playlist[lokke];
	}
	root->song->playlist=temp;

#if 1
	printf("playlen: %d\n",root->song->length);
	for(lokke=0;lokke<root->song->length;lokke++){
	  printf("pos: %d, block: %d\n",lokke,temp[lokke]->l.num);
	}

#endif
}

void BL_delete(int pos){
	int lokke;

	struct Blocks **playlist=root->song->playlist;

	PlayStop();

	for(lokke=pos;lokke<root->song->length;lokke++){
		playlist[lokke]=playlist[lokke+1];
	}

	root->song->length--;

#if 1
	debug("playlen: %d\n",root->song->length);
	for(lokke=0;lokke<root->song->length;lokke++){
	  printf("pos: %d, block: %d\n",lokke,playlist[lokke]->l.num);
	}
#endif

}

struct Blocks *BL_GetBlockFromPos(int pos){
	if(pos>=root->song->length || pos<0) return NULL;
	return root->song->playlist[pos];
}

