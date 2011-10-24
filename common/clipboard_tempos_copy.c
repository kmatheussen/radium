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






#include "nsmtracker.h"
#include "placement_proc.h"
#include "list_proc.h"

#include "clipboard_tempos_copy_proc.h"


struct LPBs *cb_lpb=NULL;
struct Tempos *cb_tempo=NULL;
struct TempoNodes *cb_temponode=NULL;


struct LPBs *CB_CopyLPBs(
	struct LPBs *lpb
){
	struct LPBs *to=NULL;
	struct LPBs *new;

	while(lpb!=NULL){
		new=talloc(sizeof(struct LPBs));
		PlaceCopy(&new->l.p,&lpb->l.p);
		new->lpb=lpb->lpb;
		ListAddElement3(&to,&new->l);
		lpb=NextLPB(lpb);
	}
	return to;
}

struct Tempos *CB_CopyTempos(
	struct Tempos *tempo
){
	struct Tempos *to=NULL;
	struct Tempos *new;

	while(tempo!=NULL){
		new=talloc(sizeof(struct Tempos));
		PlaceCopy(&new->l.p,&tempo->l.p);
		new->tempo=tempo->tempo;
		ListAddElement3(&to,&new->l);
		tempo=NextTempo(tempo);
	}
	return to;
}


struct TempoNodes *CB_CopyTempoNodes(
	struct TempoNodes *temponode
){
	struct TempoNodes *to=NULL;
	struct TempoNodes *new;

	while(temponode!=NULL){
		new=talloc(sizeof(struct TempoNodes));
		PlaceCopy(&new->l.p,&temponode->l.p);
		new->reltempo=temponode->reltempo;
		ListAddElement3(&to,&new->l);
		temponode=NextTempoNode(temponode);
	}
	return to;
}






