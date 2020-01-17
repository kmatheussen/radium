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

	struct WBPMs *wbpms=(struct WBPMs*)talloc_atomic_clean(sizeof(struct WBPMs)*wblock->num_reallines);

	while(bpm!=NULL){
		realline=FindRealLineFor(wblock,realline,&bpm->l.p);

		if(wbpms[realline].tempo!=0){
			wbpms[realline].type=TEMPO_MUL;                        
		}else{
			if(PlaceNotEqual(&wblock->reallines[realline]->l.p,&bpm->l.p))
				wbpms[realline].type=TEMPO_BELOW;
		}

		wbpms[realline].tempo=bpm->tempo;
                wbpms[realline].logtype = bpm->logtype;
		//wbpms[realline].Tempo=tempo;
		bpm=NextBPM(bpm);
	}

        return wbpms;
}

QVector<Tempos*> BPMs_get(const struct WBlocks *wblock, int realline){
  QVector<Tempos*> ret;
  
  struct Tempos *bpm=wblock->block->tempos;
  int realline2 = 0;
  
  while(bpm!=NULL){
    realline2=FindRealLineFor(wblock,realline2,&bpm->l.p);
    if(realline2>realline)
      break;
    if(realline2==realline)
      ret.push_back(bpm);
    bpm = NextBPM(bpm);
  }
  
  return ret;
}

struct Tempos *SetTempo(
	struct Blocks *block,
	const Place *place,
	int newtempo,
        int logtype
){
  if (newtempo<=0){
    RError("Illegal tempo %d at position %s\n",newtempo,PlaceToString(place));
    newtempo = 1;
  }

        struct Tempos *tempo = (struct Tempos*)ListFindElement3(LCAST(block->tempos),place);

        if(tempo!=NULL && PlaceEqual(&tempo->l.p,place)){
          tempo->tempo=newtempo;
          tempo->logtype = logtype;
        }else{
          tempo=(struct Tempos*)talloc(sizeof(struct Tempos));
          PlaceCopy(&tempo->l.p,place);
          tempo->tempo=newtempo;
          tempo->logtype = logtype;
          ListAddElement3(&block->tempos,&tempo->l);
        }

        TIME_block_tempos_have_changed(block);
        
        return tempo;
}


void SetTempos(
               struct Blocks *block,
               struct Tempos *tempos
               )
{
  block->tempos = tempos;
  TIME_block_tempos_have_changed(block);
}

void SetTempoCurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;
	int curr_realline=wblock->curr_realline;
	const Place *place= &wblock->reallines[curr_realline]->l.p;
	int newtempo=GFX_GetInteger(window,NULL,"New tempo: >",1,999,true);
	if(newtempo<=0) return;

	ADD_UNDO(Tempos_CurrPos(window));

	SetTempo(wblock->block,place,newtempo,LOGTYPE_HOLD);

	//UpdateWTempos(window,wblock);

#if !USE_OPENGL
	DrawTempos(window,wblock,curr_realline,curr_realline);
	WBLOCK_DrawTempoColor(window,wblock,curr_realline,wblock->num_reallines);
#endif
}

void RemoveTempo(struct Blocks *block,const struct Tempos *tempo){
  ListRemoveElement3(&block->tempos,&tempo->l);
  TIME_block_tempos_have_changed(block);
}

static void RemoveTempos(struct Blocks *block,const Place *p1,const Place *p2){
  ListRemoveElements3(&block->tempos,p1,p2);
  TIME_block_tempos_have_changed(block);
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










