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

#include <math.h>
#include <string.h>

#include "nsmtracker.h"
#include "list_proc.h"
#include "placement_proc.h"
#include "time_proc.h"
#include "reltempo_proc.h"
#include "undo_temponodes_proc.h"
#include "temponodes_legalize_proc.h"
#include "player_proc.h"
#include "player_pause_proc.h"
#include "undo.h"

#include "temponodes_proc.h"

#if !USE_OPENGL

void MakeWTempoNodesCallBack(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
        struct WTracks *wtrack,
	void *extrainfo,
	int firstlast,
	int realline,
	float u_y1,float u_y2,
	float u_x1,float u_x2
){
	struct TempoNodes *temponode=(struct TempoNodes *)extrainfo;
	WTempoNodes *wtemponode = talloc(sizeof(WTempoNodes));

	wtemponode->type=TEMPONODE_LINE;
	wtemponode->y1=u_y1;
	wtemponode->y2=u_y2;
	wtemponode->x1=u_x1;
	wtemponode->x2=u_x2;
	wtemponode->pointer=temponode;

	wtemponode->next=wblock->wtemponodes[realline];
	wblock->wtemponodes[realline]=wtemponode;

	if(firstlast==NODELINE_FIRST || firstlast==NODELINE_FIRSTANDLAST){
		WTempoNodes *wtemponode = talloc(sizeof(WTempoNodes));

		wtemponode->type=TEMPONODE_NODE;

		//		MakeBlackBox(window,u_x1,u_y1,wblock->temponodearea.width-1,wtemponode);
		wtemponode->x1=u_x1;
		wtemponode->y1=u_y1;

		wtemponode->pointer=temponode;
		wtemponode->next=wblock->wtemponodes[realline];
		wblock->wtemponodes[realline]=wtemponode;
	}

	if(NextTempoNode(temponode)==wblock->block->lasttemponode){
		if(firstlast==NODELINE_LAST || firstlast==NODELINE_FIRSTANDLAST){
			WTempoNodes *wtemponode = talloc(sizeof(WTempoNodes));
	
			wtemponode->type=TEMPONODE_NODE;

			//			MakeBlackBox(window,u_x2,u_y2,wblock->temponodearea.width-1,wtemponode);
			wtemponode->x1=u_x2;
			wtemponode->y1=u_y2;

			wtemponode->pointer=wblock->block->lasttemponode;
			wtemponode->next=wblock->wtemponodes[realline];
			wblock->wtemponodes[realline]=wtemponode;
		}
	}
}

void UpdateWTempoNodes(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){
	struct TempoNodes *prev=wblock->block->temponodes;
	struct TempoNodes *temponode=NextTempoNode(prev);

	wblock->wtemponodes=talloc(sizeof(WTempoNodes *) * wblock->num_reallines);

	while(temponode!=NULL){

		MakeNodeLines(
			window,
			wblock,
                        NULL,
			&prev->l.p,
			&temponode->l.p,
			prev->reltempo,
			temponode->reltempo,
			(float)(-wblock->reltempomax+1.0f), (float)(wblock->reltempomax-1.0f),
			prev,
			&MakeWTempoNodesCallBack
		);

		prev=temponode;
		temponode=NextTempoNode(temponode);
	}
}

#endif

struct TempoNodes *AddTempoNode(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	const Place *p,
	double reltempo
){
	struct Blocks *block=wblock->block;
	struct TempoNodes *temponode;

	temponode=talloc(sizeof(struct TempoNodes));
	temponode->reltempo=reltempo;

	PlaceCopy(&temponode->l.p,p);

        ADD_UNDO(TempoNodes_CurrPos(window));
        
        struct TempoNodes *ret = NULL;
        
        if(ListAddElement3_ns(&block->temponodes,&temponode->l)==-1) {
          UNDO_CANCEL_LAST_UNDO();
        } else {
          ret = temponode;
          TIME_block_tempos_have_changed(block);
        }  
          
        return ret;
}


void AddTempoNodeCurrPos(struct Tracker_Windows *window,float reltempo){
	struct WBlocks *wblock=window->wblock;

	AddTempoNode(
		window,wblock,
		&wblock->reallines[wblock->curr_realline]->l.p,
		reltempo
	);

#if !USE_OPENGL
	UpdateWTempoNodes(window,wblock);

	DrawUpWTempoNodes(window,wblock);
#endif
}


// TODO/FIX: Implement
void RemoveAllTempoNodesOnReallineCurrPos(struct Tracker_Windows *window){

#if !USE_OPENGL

  struct WBlocks *wblock=window->wblock;
	WTempoNodes *wtemponode;
	int realline=wblock->curr_realline;

	PlayStop();

	ADD_UNDO(TempoNodes_CurrPos(window));

	wtemponode=wblock->wtemponodes[realline];

	while(wtemponode!=NULL){
		if(wtemponode->type==TEMPONODE_NODE)
			ListRemoveElement3(&wblock->block->temponodes,(struct ListHeader3 *)wtemponode->pointer);
		wtemponode=wtemponode->next;
	}

	LegalizeTempoNodes(wblock->block);

	UpdateWTempoNodes(window,wblock);

	DrawUpWTempoNodes(window,wblock);

        TIME_block_tempos_have_changed(block);
#endif
}


float FindHighestTempoNodeVal(struct Blocks *block){
	struct TempoNodes *temponode=block->temponodes;

	float max = fabs(temponode->reltempo);
        
	temponode=NextTempoNode(temponode);

	while(temponode!=NULL){
          if(fabs(temponode->reltempo) > max){
            max=fabs(temponode->reltempo);
          }
          temponode=NextTempoNode(temponode);
	}

	return max;
}


