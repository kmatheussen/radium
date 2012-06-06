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
#include "list_proc.h"
#include "memory_proc.h"
#include "realline_calc_proc.h"
#include "gfx_wblocks_proc.h"
#include "placement_proc.h"
#include "visual_proc.h"
#include "time_proc.h"
#include "undo_tempos_proc.h"
#include "gfx_window_title_proc.h"
#include "gfx_tempocolor_proc.h"
#include "player_proc.h"

#include "tempos_proc.h"




void UpdateWTempos(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){
	int realline=0;
	struct Tempos *tempo=wblock->block->tempos;
	wblock->wtempos=talloc(sizeof(struct WTempos)*wblock->num_reallines);

	while(tempo!=NULL){
		realline=FindRealLineFor(wblock,realline,&tempo->l.p);

		if(wblock->wtempos[realline].tempo!=0){
			wblock->wtempos[realline].type=TEMPO_MUL;
		}else{
			if(PlaceNotEqual(&wblock->reallines[realline]->l.p,&tempo->l.p))
				wblock->wtempos[realline].type=TEMPO_BELOW;
		}

		wblock->wtempos[realline].tempo=tempo->tempo;
		wblock->wtempos[realline].Tempo=tempo;
		tempo=NextTempo(tempo);
	}
}

struct Tempos *SetTempo(
	struct Blocks *block,
	Place *place,
	int newtempo
){
	struct Tempos *tempo;
	tempo=ListFindElement3(&block->tempos->l,place);

	if(tempo!=NULL && PlaceEqual(&tempo->l.p,place)){
		tempo->tempo=newtempo;
	}else{
		tempo=talloc(sizeof(struct Tempos));
		PlaceCopy(&tempo->l.p,place);
		tempo->tempo=newtempo;
		ListAddElement3(&block->tempos,&tempo->l);
	}

	UpdateSTimes(block);

        return tempo;
}


void SetTempoCurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;
	int curr_realline=wblock->curr_realline;
	Place *place= &wblock->reallines[curr_realline]->l.p;
	int newtempo=GFX_GetInteger(window,NULL,"New tempo: >",0,999);
	if(newtempo==-1) return;

	PlayStop();

	Undo_Tempos_CurrPos(window);

	SetTempo(wblock->block,place,newtempo);

	UpdateWTempos(window,wblock);
	DrawTempos(window,wblock,curr_realline,curr_realline);
	WBLOCK_DrawTempoColor(window,wblock,curr_realline,wblock->num_reallines);

	GFX_DrawWindowTitle(window,wblock);
}

void RemoveTempos(struct Blocks *block,Place *p1,Place *p2){
	ListRemoveElements3(&block->tempos,p1,p2);
}

void RemoveTemposCurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;
	int curr_realline=wblock->curr_realline;

	Place p1,p2;

	PlayStop();

	Undo_Tempos_CurrPos(window);

	PlaceSetReallinePlace(wblock,curr_realline,&p1);
	PlaceSetReallinePlace(wblock,curr_realline+1,&p2);

	RemoveTempos(wblock->block,&p1,&p2);

	UpdateWTempos(window,wblock);
	DrawUpTempos(window,wblock);

	UpdateSTimes(wblock->block);

	WBLOCK_DrawTempoColor(window,wblock,0,wblock->num_reallines);

	GFX_DrawWindowTitle(window,wblock);
}










