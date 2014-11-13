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
#include "vector_proc.h"
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
#include "OS_visual_input.h"
#include "instruments_proc.h"

#include "fxlines_proc.h"


extern struct Root *root;

#if !USE_OPENGL
struct FXextrainfo{
	int color;
	void *FXs;
	void *FXNodeLine;
};


void MakeWFXNodesCallBack(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
        struct WTracks *wtrack,
	void *extrainfo,
	int firstlast,
	int realline,
	float u_y1,float u_y2,
	float u_x1,float u_x2
){
	struct FXextrainfo *fxextrainfo=(struct FXextrainfo *)extrainfo;
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
                                wtrack,
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
#endif

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

// Called after loading and undo. Can be called at any time.
void FX_update_all_slider_automation_visuals(void){
  struct Blocks *block = root->song->blocks;
  while(block!=NULL){
    struct Tracks *track = block->tracks;
    while(track!=NULL){
      struct FXs *fxs=track->fxs;
      while(fxs!=NULL){
        fxs->fx->slider_automation_value = OS_SLIDER_obtain_automation_value_pointer(track->patch,fxs->fx->effect_num);
        fxs->fx->slider_automation_color = OS_SLIDER_obtain_automation_color_pointer(track->patch,fxs->fx->effect_num);
        
        fxs = NextFX(fxs);
      }
      track = NextTrack(track);
    }
    block = NextBlock(block);
  }
}

void FX_min_max_have_changed_for_patch(struct Patch *patch, NInt fxnum, float old_min, float old_max, float new_min, float new_max){
  struct Blocks *block = root->song->blocks;

  while(block!=NULL){
    struct Tracks *track = block->tracks;

    while(track!=NULL){

      if (track->patch == patch) {
        struct FXs *fxs=track->fxs;
        while(fxs!=NULL){
          if (fxs->l.num == fxnum) {
            struct FX *fx = fxs->fx;
            int fx_min = fx->min;
            int fx_max = fx->max;

            struct FXNodeLines *fxnode = fxs->fxnodelines;

            Undo_FXs(root->song->tracker_windows, block, track, 0);

            while(fxnode != NULL) {
              double real_val = scale_double(fxnode->val, fx_min,  fx_max,  old_min, old_max);
              fxnode->val     = scale_double(real_val,    new_min, new_max, fx_min,  fx_max);
              fxnode = NextFXNodeLine(fxnode);
            }

            block->is_dirty=true;
          }

          fxs = NextFX(fxs);
        }
      }
      track = NextTrack(track);
    }
    block = NextBlock(block);
  }
}

static struct FX *selectFX(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack
){

  if(wtrack->track->patch==NULL)
    return NULL; // TODO: Ask for new patch.

	struct FX *fx;
	int num_usedFX=getNumUsedFX(wtrack->track);

	if(num_usedFX>0){
          int lokke;
          vector_t v={0};
          for(lokke=0;lokke<num_usedFX;lokke++)
            VECTOR_push_back(&v,getTrackFX(wtrack->track,lokke)->name);

          VECTOR_push_back(&v,"New FX");
          int selection=GFX_Menu(window,NULL,"Select FX",&v);
          if(selection==-1) return NULL;
          if(selection<num_usedFX) return getTrackFX(wtrack->track,selection);
	}

	fx=talloc(sizeof(struct FX));

        {
          if(fx->color==0){
            static int nextcolor=3;

            nextcolor++;

            if(nextcolor==3)
              nextcolor=4;
            if(nextcolor==7)
              nextcolor=8;
            if(nextcolor==9||nextcolor==10||nextcolor==11)
              nextcolor=12;
            if(nextcolor==15)
              nextcolor=1;

            fx->color=nextcolor;
          }
        }

	if(
		(*wtrack->track->patch->instrument->getFX)(window,wtrack->track,fx)
		==
		FX_FAILED
	){
		return NULL;
	}

        fx->slider_automation_value = OS_SLIDER_obtain_automation_value_pointer(wtrack->track->patch,fx->effect_num);
        fx->slider_automation_color = OS_SLIDER_obtain_automation_color_pointer(wtrack->track->patch,fx->effect_num);

	return fx;
}

int AddFXNodeLine(
                  struct Tracker_Windows *window,
                  struct WBlocks *wblock,
                  struct WTracks *wtrack,
                  int fxnum,
                  int val,
                  const Place *p1
){
	struct FXs *fxs=ListFindElement1_r0(&wtrack->track->fxs->l,fxnum);
	struct FXNodeLines *fxnodeline=talloc(sizeof(struct FXNodeLines));
        
	fxnodeline->val=val;
	PlaceCopy(&fxnodeline->l.p,p1);
	return ListAddElement3_ns(&fxs->fxnodelines,&fxnodeline->l);
}

static void AddNewTypeOfFxNodeLine(const struct WBlocks *wblock, struct WTracks *wtrack, struct FX *fx, const Place *p2, int val){
  printf("new, fxnum: %d, wtrack->fxs->l.num:%d\n",fx->num,wtrack->track->fxs==NULL?-1000:wtrack->track->fxs->l.num);
  
  struct FXs *fxs=talloc(sizeof(struct FXs));
  fxs->l.num=fx->num;
  fxs->fx=fx;
  ListAddElement1(&wtrack->track->fxs,&fxs->l);
  
  struct FXNodeLines *fxnodeline=talloc(sizeof(struct FXNodeLines));
  fxnodeline->val=val;
  PlaceCopy(&fxnodeline->l.p,p2);
  ListAddElement3(&fxs->fxnodelines,&fxnodeline->l);
}


void AddFXNodeLineCurrPos(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack){  
  PlayStop();

	struct FX *fx=selectFX(window,wblock,wtrack);
	if(fx==NULL) return;

	Undo_FXs_CurrPos(window);

        Place p1;
        PlaceCopy(&p1, &wblock->reallines[wblock->curr_realline]->l.p);

        int val = (fx->max + fx->min)/2;

        struct FXs *fxs=ListFindElement1_r0(&wtrack->track->fxs->l,fx->num);
        if (fxs==NULL){
          Place p2;

          int realline=FindRealLineFor(wblock,0,&p1);
          if(realline==wblock->num_reallines-1){
            PlaceSetLastPos(wblock->block,&p2);
          }else{
            PlaceCopy(&p2,&wblock->reallines[wblock->curr_realline+1]->l.p);
          }

          AddNewTypeOfFxNodeLine(wblock, wtrack, fx, &p2, val);
        }

	AddFXNodeLine(
                      window,wblock,wtrack,
                      fx->num,
                      val,
                      &p1
                      );

#if !USE_OPENGL
	UpdateFXNodeLines(window,wblock,wtrack);

	ClearTrack(window,wblock,wtrack,wblock->top_realline,wblock->bot_realline);
	UpdateWTrack(window,wblock,wtrack,wblock->top_realline,wblock->bot_realline);
#endif
}


void DeleteFxNodeLine(struct WTracks *wtrack, struct FXs *fxs, struct FXNodeLines *fxnodeline){

  R_ASSERT(ListFindNumElements3(&fxs->fxnodelines->l)>1);
  
  ListRemoveElement3(&fxs->fxnodelines,&fxnodeline->l);
  
  if (ListFindNumElements3(&fxs->fxnodelines->l) <= 1 ){
    struct FX *fx = fxs->fx;
    struct Tracks *track = wtrack->track;
    
    OS_SLIDER_release_automation_pointers(track->patch,fx->effect_num);
    (*fx->closeFX)(fx,track);
    ListRemoveElement1(&track->fxs,&fxs->l);
  }
}
