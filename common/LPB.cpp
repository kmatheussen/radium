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
#include "player_pause_proc.h"
#include "Beats_proc.h"

#include "LPB_proc.h"


extern struct Root *root;


struct WLPBs *WLPBs_get(
                        const struct Tracker_Windows *window,
                        const struct WBlocks *wblock
                        )
{
        struct WLPBs *wlpbs=(struct WLPBs *)talloc_atomic_clean(sizeof(struct WLPBs)*wblock->num_reallines);

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

QVector<LPBs*> LPBs_get(const struct WBlocks *wblock, int realline){
  QVector<LPBs*> ret;
  
  struct LPBs *lpb=wblock->block->lpbs;
  int realline2 = 0;
  
  while(lpb!=NULL){
    realline2=FindRealLineFor(wblock,realline2,&lpb->l.p);
    if(realline2>realline)
      break;
    if(realline2==realline)
      ret.push_back(lpb);
    lpb = NextLPB(lpb);
  }
  
  return ret;
}

struct LPBs *SetLPB(
	struct Blocks *block,
	const Place *place,
	int newlpb
){
  if (newlpb<=0){
    RError("Illegal lpb %d at position %s\n",newlpb,PlaceToString(place));
    newlpb = 1;
  }
        struct LPBs *lpb=(struct LPBs*)ListFindElement3(LCAST(block->lpbs),place);

        PC_Pause();{
          
          if(lpb!=NULL && PlaceEqual(&lpb->l.p,place)){
            lpb->lpb=newlpb;
          }else{
            lpb=(struct LPBs*)talloc(sizeof(struct LPBs));
            PlaceCopy(&lpb->l.p,place);
            lpb->lpb=newlpb;
            ListAddElement3(&block->lpbs,&lpb->l);
          }

          TIME_block_LPBs_have_changed(block);
          
        }PC_StopPause(NULL);
        
        return lpb;
}


void SetLPBCurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;
	int curr_realline=wblock->curr_realline;
	const Place *place= &wblock->reallines[curr_realline]->l.p;
	int newlpb=GFX_GetInteger(window,NULL,"New LPB: >",1,99,true);
	if(newlpb<=0) return;

	ADD_UNDO(LPBs_CurrPos(window));

	SetLPB(wblock->block,place,newlpb);
        
	//UpdateWLPBs(window,wblock);
	//DrawLPBs(window,wblock,curr_realline,curr_realline);

        wblock->block->is_dirty = true;

	//WBLOCK_DrawTempoColor(window,wblock,curr_realline,wblock->num_reallines);
}

void RemoveLPB(struct Blocks *block,struct LPBs *lpb){
  PC_Pause();{
    ListRemoveElement3(&block->lpbs,&lpb->l);
    TIME_block_LPBs_have_changed(block);
  }PC_StopPause(NULL);
}

void RemoveLPBs(struct Blocks *block,Place *p1,Place *p2){
  PC_Pause();{
    ListRemoveElements3(&block->lpbs,p1,p2);
    TIME_block_LPBs_have_changed(block);
  }PC_StopPause(NULL);
}

void RemoveLPBsCurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;
	int curr_realline=wblock->curr_realline;

	Place p1,p2;

	ADD_UNDO(LPBs_CurrPos(window));

	PlaceSetReallinePlace(wblock,curr_realline,&p1);
	PlaceSetReallinePlace(wblock,curr_realline+1,&p2);

	RemoveLPBs(wblock->block,&p1,&p2);

	//UpdateWLPBs(window,wblock);

        wblock->block->is_dirty = true;
}




