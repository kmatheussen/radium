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
#include "placement_proc.h"
#include "nodelines_proc.h"
#include "nodelines.h"
#include "gfx_wtracks_proc.h"
#include "visual_proc.h"
#include "realline_calc_proc.h"
#include "undo_fxs_proc.h"
#include <string.h>
#include "player_proc.h"
#include "nodeboxes_proc.h"


#include "fxlines_proc.h"



struct FXextrainfo{
	struct WTracks *wtrack;
	int color;
	void *FXs;
	void *FXNodeLine;
};


void MakeWFXNodesCallBack(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	void *extrainfo,
	int firstlast,
	int realline,
	float u_y1,float u_y2,
	float u_x1,float u_x2
){
	struct FXextrainfo *fxextrainfo=(struct FXextrainfo *)extrainfo;
	struct WTracks *wtrack=(struct WTracks *)fxextrainfo->wtrack;
	struct FXNodeLines *fxnodeline1,*fxnodeline2;

	/* To avoid displaying a long vertical line. */
	if(firstlast==NODELINE_NOTFIRSTORLAST){

		fxnodeline1=(struct FXNodeLines *)fxextrainfo->FXNodeLine;

		fxnodeline2=NextFXNodeLine(fxnodeline1);

		if(fxnodeline1->val==fxnodeline2->val) return;
	}


	WFXNodes *wfxnode2 = talloc(sizeof(WFXNodes));

	wfxnode2->type=TRE_FXLINE;
	wfxnode2->subtype=fxextrainfo->color;
	wfxnode2->y1=u_y1;
	wfxnode2->y2=u_y2;
	wfxnode2->x1=u_x1;
	wfxnode2->x2=u_x2;
	wfxnode2->pointer=fxextrainfo->FXs;

//	wfxnode2->next=wtrack->wfxnodes[realline];
//	wtrack->wfxnodes[realline]=wfxnode2;

	if(firstlast==NODELINE_FIRST || firstlast==NODELINE_FIRSTANDLAST){
		WFXNodes *wfxnode = talloc(sizeof(WFXNodes));

		wfxnode->type=TRE_FXNODE;
		wfxnode->subtype=fxextrainfo->color;

		//		MakeBlackBox(window,u_x1,u_y1,wtrack->fxwidth,wfxnode);
		wfxnode->x1=u_x1;
		wfxnode->y1=u_y1;

		wfxnode->pointer=fxextrainfo->FXNodeLine;

		wfxnode->next=wtrack->wfxnodes[realline];
		wtrack->wfxnodes[realline]=wfxnode;
	}

	if(
		NextFXNodeLine(
			NextFXNodeLine((struct FXNodeLines *)fxextrainfo->FXNodeLine)
		)==NULL
	){

		if(firstlast==NODELINE_LAST || firstlast==NODELINE_FIRSTANDLAST){
			WFXNodes *wfxnode = talloc(sizeof(WFXNodes));

			wfxnode->type=TRE_FXNODE;
			wfxnode->subtype=fxextrainfo->color;

			//			MakeBlackBox(window,u_x2,u_y2,wtrack->fxwidth,wfxnode);
			wfxnode->x1=u_x2;
			wfxnode->y1=u_y2;

			wfxnode->pointer=(((struct ListHeader3 *)(fxextrainfo->FXNodeLine))->next);
	
			wfxnode->next=wtrack->wfxnodes[realline];
			wtrack->wfxnodes[realline]=wfxnode;
		}
	}
	wfxnode2->next=wtrack->wfxnodes[realline];
	wtrack->wfxnodes[realline]=wfxnode2;
}


void UpdateFXNodeLines(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack
){
	struct FXs *fx=wtrack->track->fxs;

	struct FXNodeLines *prev;
	struct FXNodeLines *fxnode;

	struct FXextrainfo fxextrainfo={0};
	fxextrainfo.wtrack=wtrack;

        wtrack->wfxnodes=talloc(sizeof(WFXNodes *) * wblock->num_reallines);

	while(fx!=NULL){
		fxextrainfo.FXs=fx;
		prev=fx->fxnodelines;
		fxnode=NextFXNodeLine(prev);
		fxextrainfo.color=fx->fx->color;

		while(fxnode!=NULL){
			fxextrainfo.FXNodeLine=prev;
			MakeNodeLines(
				window,
				wblock,
				&prev->l.p,
				&fxnode->l.p,
				(float)prev->val,(float)fxnode->val,
				(float) fx->fx->min,(float) fx->fx->max,
				&fxextrainfo,
				&MakeWFXNodesCallBack
			);
			prev=fxnode;
			fxnode=NextFXNodeLine(fxnode);
		}
		fx=NextFX(fx);
	}

}

void UpdateSomeFXNodeLines(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	NInt starttrack,
	NInt endtrack
){
	NInt lokke;

	struct WTracks *wtrack=ListFindElement1(&wblock->wtracks->l,starttrack);
	if(wtrack==NULL) return;

	for(lokke=0;lokke<=endtrack-starttrack;lokke++){
		UpdateFXNodeLines(window,wblock,wtrack);
		wtrack=NextWTrack(wtrack);
		if(wtrack==NULL) break;
	}

}

void UpdateAllFXNodeLines(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){
	struct WTracks *wtrack=wblock->wtracks;
	while(wtrack!=NULL){
		UpdateFXNodeLines(window,wblock,wtrack);
		wtrack=NextWTrack(wtrack);
	}
}

int getNumUsedFX(struct Tracks *track){
	int ret;
	ret=ListFindNumElements1(&track->fxs->l);

	return ret;
}


/******************************************************************
  FUNCTION
    Just a way to get the same fx corresponding to 'num'.
******************************************************************/
struct FX *getTrackFX(struct Tracks *track,int num){
	struct FXs *fxs=track->fxs;
	int nnum=0;

	while(fxs!=NULL){
		if(nnum==num){
			return fxs->fx;
		}
		nnum++;
		fxs=NextFX(fxs);
	}

	RError("Error at function getTrackFX in file fxlines.c\n");
	return NULL;
}

int nextcolor=4;

struct FX *selectFX(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack
){
	char **menutext;
	struct FX *fx;
	int num_usedFX=getNumUsedFX(wtrack->track);
	int lokke,selection;

	if(num_usedFX>0){
		menutext=talloc_atomic(sizeof(char *) * (num_usedFX+1));
		for(lokke=0;lokke<num_usedFX;lokke++){
			menutext[lokke]=getTrackFX(wtrack->track,lokke)->name;
		}
		menutext[lokke]="New FX";
		selection=GFX_Menu(window,NULL,"Select FX",num_usedFX+1,menutext);
		if(selection==-1) return NULL;
		if(selection<num_usedFX) return getTrackFX(wtrack->track,selection);
	}

	fx=talloc(sizeof(struct FX));

	if(
		(*wtrack->track->instrument->getFX)(window,wtrack->track,fx)
		==
		FX_FAILED
	){
		return NULL;
	}

	if(fx->color==0){
		fx->color=nextcolor;
		nextcolor++;
		if(nextcolor==256) nextcolor=4;
	}

	return fx;
}

void AddFXNodeLine(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	struct FX *fx,
	int val,
	Place *p1
){
	int realline;
	Place p2;
	struct FXs *fxs;
	struct FXNodeLines *fxnodeline;

	fxs=ListFindElement1_r0(&wtrack->track->fxs->l,fx->l.num);
	if(fxs==NULL){
	  //		printf("new, fx->l.num: %d, wtrack->fxs->l.num:%d\n",fx->l.num,wtrack->track->fxs->l.num);
		fxs=talloc(sizeof(struct FXs));
		fxs->l.num=fx->l.num;
		fxs->fx=fx;
		ListAddElement1(&wtrack->track->fxs,&fxs->l);

		realline=FindRealLineFor(wblock,0,p1);
		if(realline==wblock->num_reallines-1){
			PlaceSetLastPos(wblock->block,&p2);
		}else{
			PlaceCopy(&p2,&wblock->reallines[wblock->curr_realline+1]->l.p);
		}
		fxnodeline=talloc(sizeof(struct FXNodeLines));
		fxnodeline->val=val;
		PlaceCopy(&fxnodeline->l.p,&p2);
		ListAddElement3(&fxs->fxnodelines,&fxnodeline->l);
	}

	fxnodeline=talloc(sizeof(struct FXNodeLines));
	fxnodeline->val=val;
	PlaceCopy(&fxnodeline->l.p,p1);
	ListAddElement3_ns(&fxs->fxnodelines,&fxnodeline->l);

}

void AddFXNodeLineCurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock;
	struct WTracks *wtrack;
	struct FX *fx;

	PlayStop();

	wblock=window->wblock;
	wtrack=wblock->wtrack;

	Undo_FXs_CurrPos(window);

	fx=selectFX(window,wblock,wtrack);
	if(fx==NULL) return;

	AddFXNodeLine(
		window,wblock,wtrack,
		fx,
		(fx->max + fx->min)/2,
		&wblock->reallines[wblock->curr_realline]->l.p
	);

	UpdateFXNodeLines(window,wblock,wtrack);
//	ClearTrack(window,wblock,wtrack,wblock->top_realline,wblock->bot_realline);
	UpdateWTrack(window,wblock,wtrack,wblock->top_realline,wblock->bot_realline);
}





