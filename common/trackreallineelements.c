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
#include <gc.h>
#include "trackreallines_proc.h"
#include "fxlines_proc.h"
#include "temponodes_proc.h"


#include "trackreallineelements_proc.h"

#define NUM_TREINHOLDER 20000

bool collectTREgarbage=false;


struct TREelementHolder{
	struct TREelementHolder *next;
//	int notused;
	int size;
	struct TrackReallineElements tre[NUM_TREINHOLDER];
};
struct TREelementHolder_small{
	struct TREelementHolder *next;
	int size;
	struct TrackReallineElements tre[2];
};
struct TREelementHolder *TREroot=NULL;

struct TrackReallineElements *nextfreeelement=NULL;

extern size_t allocated;


void ReturnRTEelement(struct TrackReallineElements *tre){
	tre->next=nextfreeelement;
//	tre->pointer=NULL;
	nextfreeelement=tre;
}


void FreeAllRTEelements_fromroot(
	struct TrackReallineElements **to
){
	struct TrackReallineElements *rte=*to;
	struct TrackReallineElements *temp;

	if(rte==NULL) return;

	for(;rte!=NULL;temp=rte,rte=rte->next);

	temp->next=nextfreeelement;
	nextfreeelement=*to;
	*to=NULL;

#if 0
	while(rte!=NULL){
		temp=rte->next;
		ReturnRTEelement(rte);
		rte=temp;
	}
	*to=NULL;
#endif
}

#if 0
void FreeAllRTEelements_fromroot(
	struct TrackReallineElements **to
){
	struct TrackReallineElements *rte= *to;
	struct TrackReallineElements *temp;

	while(rte!=NULL){
		temp=rte->next;
		ReturnRTEelement(rte);
		rte=temp;
	}
	*to=NULL;
}
#endif



void SetNextsInATreElementHolder(struct TREelementHolder *treholder){
	struct TrackReallineElements *end=&treholder->tre[treholder->size-1];

	struct TrackReallineElements *tre=&treholder->tre[0];
	struct TrackReallineElements *next=&treholder->tre[1];

	while(next<=end){
		tre->next=next;
		tre=next;
		next=next+1;
	}

	end->next=NULL;
}

bool nomoretreelementholders=false;

bool AllocateNewTREelementHolder(void){
	struct TREelementHolder *treholder;

	//	fprintf(stderr,"\n\n\nALLOCATING NEW TREELEMENTHOLDER\n\n\n\n");

	treholder=tracker_alloc_clean(sizeof(struct TREelementHolder),GC_malloc_atomic_uncollectable);
	if(treholder==NULL){
		treholder=talloc_atomic_uncollectable(sizeof(struct TREelementHolder_small));
		treholder->size=2;
	}else{
		treholder->size=NUM_TREINHOLDER;
	}

	SetNextsInATreElementHolder(treholder);

	nextfreeelement=&treholder->tre[0];

	treholder->next=TREroot;
	TREroot=treholder;

	//	fprintf(stderr,"Success\n");
	return true;
}



extern struct Root *root;

bool FreeASpesifiedWBlockTREelement(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){
	struct WTracks *wtrack;

	if( wblock->isgfxdatahere==false ) return false;

	FreeAllWTempoNodes(wblock);

	tfree(wblock->wtemponodes);
	wblock->wtemponodes=NULL;

	wtrack=wblock->wtracks;
	while(wtrack!=NULL){
		FreeAllRTEelements(wblock,wtrack);
		tfree(wtrack->trackreallines);
		wtrack->trackreallines=NULL;

		FreeAllWFXnodes(wblock,wtrack);
		tfree(wtrack->wfxnodes);
		wtrack->wfxnodes=NULL;

		wtrack=NextWTrack(wtrack);
	}
	wblock->isgfxdatahere=false;

	return true;
}

bool FreeANotShowedWBlockTREelement(void){
	struct Tracker_Windows *window=root->song->tracker_windows;
	struct WBlocks *wblock;

	while(window!=NULL){

		wblock=window->wblocks;

		while(wblock!=NULL){
			if(wblock!=window->wblock){
				if( FreeASpesifiedWBlockTREelement(window,wblock) ) return true;
			}
			wblock=NextWBlock(wblock);
		}

		window=NextWindow(window);
	}

	return false;
}


/*********************************************************************/
/* Mark and sweep gc, start. Can only be called from eventreceiver.c */
/*********************************************************************/

void TRE_markTREs(struct TrackReallineElements *tre){
	while(tre!=NULL){
		tre->type+=TRE_Max;
		tre=tre->next;
	}
}

bool TRE_collectGarbage(void){
	int lokke;
	struct TREelementHolder *TRE;
	struct TREelementHolder *TREprev=NULL;
	struct Tracker_Windows *window=root->song->tracker_windows;
	struct WBlocks *wblock;
	struct WTracks *wtrack;

//	if(1) return false;

/* Make shure this is done first. */
//	while(FreeANotShowedWBlockTREelement()==true);

//	if(1) return false;

/* Mark. */
	while(window!=NULL){

		wblock=window->wblocks;

//		if(wblock->isgfxdatahere ){

		while(wblock!=NULL){
			if(wblock->wtemponodes!=NULL){
				for(lokke=0;lokke<wblock->num_reallines;lokke++){
					TRE_markTREs(wblock->wtemponodes[lokke]);
				}
			}

			wtrack=wblock->wtracks;
			while(wtrack!=NULL){
				if(wtrack->trackreallines!=NULL){
					for(lokke=0;lokke<wblock->num_reallines;lokke++){
						TRE_markTREs(wtrack->trackreallines[lokke].trackreallineelements);
					}
				}
				if(wtrack->wfxnodes!=NULL){
					for(lokke=0;lokke<wblock->num_reallines;lokke++){
						TRE_markTREs(wtrack->wfxnodes[lokke]);
					}
				}
				wtrack=NextWTrack(wtrack);
			}
			wblock=NextWBlock(wblock);
		}
//		}

		window=NextWindow(window);
	}
	
/* Sweep. */
	nextfreeelement=NULL;
	TRE=TREroot;
	while(TRE!=NULL){
		struct TrackReallineElements *prevfree=nextfreeelement;
		bool isused=false;
		for(lokke=0;lokke<TRE->size;lokke++){
			if(TRE->tre[lokke].type<TRE_Max){
				TRE->tre[lokke].next=nextfreeelement;
				nextfreeelement=&TRE->tre[lokke];
			}else{
				TRE->tre[lokke].type-=TRE_Max;
				isused=true;
			}
		}
		if(isused==false){
			struct TREelementHolder *tretemp;
			nextfreeelement=prevfree;
			tretemp=TRE->next;
			if(TREprev==NULL){
				TREroot=tretemp;
			}else{
				TREprev->next=tretemp;
			}
			// Nah, don`t know if this is any point doing.
			GC_free(TRE);
			TRE=tretemp;
		}else{
			TREprev=TRE;
			TRE=TRE->next;
		}
	}

	collectTREgarbage=false;

	if(nextfreeelement==NULL){
//		RError("An unsuccessfull Gfx gc.\n");
		 return false;
	}

	return true;
}
/*****************************/
/*  Mark and sweep gc, end.  */
/*****************************/



struct TrackReallineElements *TRE_GetTREelementHard(void){
	struct TrackReallineElements *temp=nextfreeelement;

	if(temp==NULL){
		if(FreeANotShowedWBlockTREelement()==false){
			collectTREgarbage=true;
			AllocateNewTREelementHolder();
		}
		return TRE_GetTREelementHard();
	}

	nextfreeelement=temp->next;

	temp->next=NULL;			//This should not be necesarry. There is a bug somewhere else in the program. (probably removed now)
	return temp;

}











