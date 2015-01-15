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
#include "player_proc.h"

#include "LPB_proc.h"


extern struct Root *root;


struct WLPBs *WLPBs_get(
                        const struct Tracker_Windows *window,
                        const struct WBlocks *wblock
                        )
{
	struct WLPBs *wlpbs=talloc_atomic_clean(sizeof(struct WLPBs)*wblock->num_reallines);

	int realline=0;

	struct LPBs *lpb=wblock->block->lpbs;

	while(lpb!=NULL){

		realline=FindRealLineFor(wblock,realline,&lpb->l.p);

		if(wlpbs[realline].lpb!=0){
			wlpbs[realline].type=LPB_MUL;
		}else{
			if(PlaceNotEqual(&wblock->reallines[realline]->l.p,&lpb->l.p))
				wlpbs[realline].type=LPB_BELOW;
		}

		wlpbs[realline].lpb=lpb->lpb;
		//wlpbs[realline].LPB=lpb;

		lpb=NextLPB(lpb);
	}

        return wlpbs;
}

struct LPBs *SetLPB(
	struct Blocks *block,
	Place *place,
	int newlpb
){
	struct LPBs *lpb;
	lpb=ListFindElement3(&block->lpbs->l,place);

	if(lpb!=NULL && PlaceEqual(&lpb->l.p,place)){
		lpb->lpb=newlpb;
	}else{
		lpb=talloc(sizeof(struct LPBs));
		PlaceCopy(&lpb->l.p,place);
		lpb->lpb=newlpb;
		ListAddElement3(&block->lpbs,&lpb->l);
	}
	UpdateSTimes(block);

        return lpb;
}


void SetLPBCurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;
	int curr_realline=wblock->curr_realline;
	Place *place= &wblock->reallines[curr_realline]->l.p;
	int newlpb=GFX_GetInteger(window,NULL,"New LPB: >",0,99);
	if(newlpb==-1) return;

	PlayStop();

	Undo_LPBs_CurrPos(window);

	SetLPB(wblock->block,place,newlpb);

	//UpdateWLPBs(window,wblock);
	//DrawLPBs(window,wblock,curr_realline,curr_realline);

        wblock->block->is_dirty = true;

	//WBLOCK_DrawTempoColor(window,wblock,curr_realline,wblock->num_reallines);
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

	//UpdateWLPBs(window,wblock);
	UpdateSTimes(wblock->block);

        wblock->block->is_dirty = true;
}




