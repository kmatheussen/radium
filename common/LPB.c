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



/***********************************************************
  Lines Per Beat (LPB). File very equal to Tempo.c (which
  should have been called (BPM).
***********************************************************/



#include "nsmtracker.h"
#include "list_proc.h"
#include "memory_proc.h"
#include "realline_calc_proc.h"
#include "gfx_wblocks_proc.h"
#include "placement_proc.h"
#include "visual_proc.h"
#include "time_proc.h"
#include "undo_lpbs_proc.h"
#include "gfx_window_title_proc.h"
#include "gfx_tempocolor_proc.h"
#include "player_proc.h"

#include "LPB_proc.h"




void UpdateWLPBs(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){
	int realline=0;
	struct LPBs *lpb=wblock->block->lpbs;
	wblock->wlpbs=talloc(sizeof(struct WLPBs)*wblock->num_reallines);

	while(lpb!=NULL){
		realline=FindRealLineFor(wblock,realline,&lpb->l.p);

		if(wblock->wlpbs[realline].lpb!=0){
			wblock->wlpbs[realline].type=LPB_MUL;
		}else{
			if(PlaceNotEqual(&wblock->reallines[realline]->l.p,&lpb->l.p))
				wblock->wlpbs[realline].type=LPB_BELOW;
		}

		wblock->wlpbs[realline].lpb=lpb->lpb;
		wblock->wlpbs[realline].LPB=lpb;
		lpb=NextLPB(lpb);
	}
}


void SetLPB(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	Place *place,
	int newlpb
){
	struct LPBs *lpb;
	lpb=ListFindElement3(&wblock->block->lpbs->l,place);

	if(lpb!=NULL && PlaceEqual(&lpb->l.p,place)){
		lpb->lpb=newlpb;
	}else{
		lpb=talloc(sizeof(struct LPBs));
		PlaceCopy(&lpb->l.p,place);
		lpb->lpb=newlpb;
		ListAddElement3(&wblock->block->lpbs,&lpb->l);
	}
	UpdateSTimes(wblock->block);
}


void SetLPBCurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;
	int curr_realline=wblock->curr_realline;
	Place *place= &wblock->reallines[curr_realline]->l.p;
	int newlpb=GFX_GetInteger(window,NULL,"New LPB: >",0,99);
	if(newlpb==-1) return;

	PlayStop();

	Undo_LPBs_CurrPos(window);

	SetLPB(window,wblock,place,newlpb);

	UpdateWLPBs(window,wblock);
	DrawLPBs(window,wblock,curr_realline,curr_realline);

	WBLOCK_DrawTempoColor(window,wblock,curr_realline,wblock->num_reallines);

	GFX_DrawWindowTitle(window,wblock);
}

void RemoveLPBs(struct Blocks *block,Place *p1,Place *p2){
	ListRemoveElements3(&block->lpbs,p1,p2);
}

void RemoveLPBsCurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;
	int curr_realline=wblock->curr_realline;

	Place p1,p2;

	PlayStop();

	Undo_LPBs_CurrPos(window);

	PlaceSetReallinePlace(wblock,curr_realline,&p1);
	PlaceSetReallinePlace(wblock,curr_realline+1,&p2);

	RemoveLPBs(wblock->block,&p1,&p2);

	UpdateWLPBs(window,wblock);
	DrawUpLPBs(window,wblock);

	UpdateSTimes(wblock->block);

	WBLOCK_DrawTempoColor(window,wblock,0,wblock->num_reallines);

	GFX_DrawWindowTitle(window,wblock);
}




