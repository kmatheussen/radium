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
#include "player_proc.h"
#include "player_pause_proc.h"

#include "tempos_proc.h"




struct WBPMs *WBPMs_get(
                        const struct Tracker_Windows *window,
                        const struct WBlocks *wblock
                        )
{
	int realline=0;
	struct BPMs *bpm=wblock->block->tempos;

	struct WBPMs *wbpms=talloc_atomic_clean(sizeof(struct WBPMs)*wblock->num_reallines);

	while(bpm!=NULL){
		realline=FindRealLineFor(wblock,realline,&bpm->l.p);

		if(wbpms[realline].tempo!=0){
			wbpms[realline].type=TEMPO_MUL;
		}else{
			if(PlaceNotEqual(&wblock->reallines[realline]->l.p,&bpm->l.p))
				wbpms[realline].type=TEMPO_BELOW;
		}

		wbpms[realline].tempo=bpm->tempo;
		//wbpms[realline].Tempo=tempo;
		bpm=NextBPM(bpm);
	}

        return wbpms;
}

struct Tempos *SetTempo(
	struct Blocks *block,
	Place *place,
	int newtempo
){
  if (newtempo<=0){
    RError("Illegal tempo %d at position %s\n",newtempo,PlaceToString(place));
    newtempo = 1;
  }

	struct Tempos *tempo;
	tempo=ListFindElement3(&block->tempos->l,place);

        PC_Pause();{
          
          if(tempo!=NULL && PlaceEqual(&tempo->l.p,place)){
            tempo->tempo=newtempo;
          }else{
            tempo=talloc(sizeof(struct Tempos));
            PlaceCopy(&tempo->l.p,place);
            tempo->tempo=newtempo;
            ListAddElement3(&block->tempos,&tempo->l);
          }
          
          UpdateSTimes(block);
          
        }PC_StopPause(NULL);
        
        return tempo;
}



void SetTempoCurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;
	int curr_realline=wblock->curr_realline;
	Place *place= &wblock->reallines[curr_realline]->l.p;
	int newtempo=GFX_GetInteger(window,NULL,"New tempo: >",1,999);
	if(newtempo<=0) return;

	ADD_UNDO(Tempos_CurrPos(window));

	SetTempo(wblock->block,place,newtempo);

	//UpdateWTempos(window,wblock);

#if !USE_OPENGL
	DrawTempos(window,wblock,curr_realline,curr_realline);
	WBLOCK_DrawTempoColor(window,wblock,curr_realline,wblock->num_reallines);
#endif
}

void RemoveTempos(struct Blocks *block,Place *p1,Place *p2){
  PC_Pause();{
    ListRemoveElements3(&block->tempos,p1,p2);
    UpdateSTimes(block);
  }PC_StopPause(NULL);
}

void RemoveTemposCurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;
	int curr_realline=wblock->curr_realline;

	Place p1,p2;

	ADD_UNDO(Tempos_CurrPos(window));

	PlaceSetReallinePlace(wblock,curr_realline,&p1);
	PlaceSetReallinePlace(wblock,curr_realline+1,&p2);

	RemoveTempos(wblock->block,&p1,&p2);

	//UpdateWTempos(window,wblock);

#if !USE_OPENGL
	DrawUpTempos(window,wblock);
#endif

#if !USE_OPENGL
	WBLOCK_DrawTempoColor(window,wblock,0,wblock->num_reallines);
#endif
}










