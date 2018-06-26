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


#include <string.h>


#include "nsmtracker.h"
#include "list_proc.h"
#include "vector_proc.h"
#include "placement_proc.h"
#include "visual_proc.h"
#include "realline_calc_proc.h"
#include "undo_fxs_proc.h"
#include "player_proc.h"
#include "player_pause_proc.h"
#include "OS_visual_input.h"
#include "instruments_proc.h"
#include "wblocks_proc.h"
#include "wtracks_proc.h"
#include "common_proc.h"
#include "OS_Player_proc.h"
#include "patch_proc.h"
#include "../api/api_proc.h"

#include "fxlines_proc.h"


extern struct Root *root;
extern struct TEvent tevent;

#if !USE_OPENGL
struct FXextrainfo{
	enum ColorNums color;
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
		fx=NextFXs(fx);
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

static int getNumUsedFX(struct Tracks *track){
	int ret;
	ret=track->fxs.num_elements;

	return ret;
}


/******************************************************************
  FUNCTION
    Just a way to get the same fx corresponding to 'num'.
******************************************************************/
static struct FX *getTrackFX(struct Tracks *track,int num){
  if (num >= track->fxs.num_elements){
    RError("Error at function getTrackFX in file fxlines.c\n");
    return NULL;
  }

  return ((struct FXs*)track->fxs.elements[num])->fx;
}


void FX_min_max_have_changed_for_patch(struct Patch *patch, NInt fxnum, float old_min, float old_max, float new_min, float new_max){
  struct Blocks *block = root->song->blocks;

  while(block!=NULL){
    struct Tracks *track = block->tracks;

    while(track!=NULL){

      if (track->patch == patch) {

        VECTOR_FOR_EACH(struct FXs *fxs, &track->fxs){

          struct FX *fx = fxs->fx;

          if (fx->effect_num == fxnum) {
            int fx_min = fx->min;
            int fx_max = fx->max;

            struct FXNodeLines *fxnode = fxs->fxnodelines;

            ADD_UNDO(FXs(root->song->tracker_windows, block, track, 0));

            while(fxnode != NULL) {
              double real_val = scale_double(fxnode->val, fx_min,  fx_max,  old_min, old_max);
              fxnode->val     = scale_double(real_val,    new_min, new_max, fx_min,  fx_max);
              fxnode = NextFXNodeLine(fxnode);
            }

            block->is_dirty=true;
          }

        }END_VECTOR_FOR_EACH;
          
      }
      track = NextTrack(track);
    }
    block = NextBlock(block);
  }
}

#if 0
enum ColorNums newFXColor(void){  
  static enum ColorNums last = AUTOMATION8_COLOR_NUM;
  
  switch(last) {
    
    case AUTOMATION1_COLOR_NUM:
      last = AUTOMATION2_COLOR_NUM;
      return last;
    case AUTOMATION2_COLOR_NUM:
      last = AUTOMATION3_COLOR_NUM;
      return last;
    case AUTOMATION3_COLOR_NUM:
      last = AUTOMATION4_COLOR_NUM;
      return last;
    case AUTOMATION4_COLOR_NUM:
      last = AUTOMATION5_COLOR_NUM;
      return last;
    case AUTOMATION5_COLOR_NUM:
      last = AUTOMATION6_COLOR_NUM;
      return last;
    case AUTOMATION6_COLOR_NUM:
      last = AUTOMATION7_COLOR_NUM;
      return last;
    case AUTOMATION7_COLOR_NUM:
      last = AUTOMATION8_COLOR_NUM;
      return last;
    case AUTOMATION8_COLOR_NUM:
      last = AUTOMATION1_COLOR_NUM;
      return last;
      
    default:
      RWarning("Unknown last color %d\n",last);
      last = AUTOMATION1_COLOR_NUM;
      return last;
      
  }
}
#endif

static const char *get_track_fx_display_name(struct Tracks *track, struct FX *fx){
  if (track->patch==fx->patch)
    return fx->name;
  else
    return talloc_format("%s (%s)", fx->name, fx->patch->name);
}

static struct FX *selectFX(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack
){

  struct Tracks *track = wtrack->track;
  struct Patch *patch = track->patch;
  
  if(patch==NULL) {
    selectInstrumentForTrack(track->l.num);
    return NULL;
  }
  
	struct FX *fx;
	int num_usedFX=getNumUsedFX(track);

	if(num_usedFX>0){
          int lokke;
          vector_t v={0};
          for(lokke=0;lokke<num_usedFX;lokke++)
            VECTOR_push_back(&v,get_track_fx_display_name(wtrack->track, getTrackFX(track,lokke)));

#if 0
          for(lokke=0;lokke<10000;lokke++)
            VECTOR_push_back(&v,talloc_format("extra %d",lokke));
#endif

          VECTOR_push_back(&v,"New FX");
          int selection=GFX_Menu(window,NULL,"Select FX",v,true);
          if(selection==-1) return NULL;
          if(selection<num_usedFX) return getTrackFX(track,selection);
	}

	fx=talloc(sizeof(struct FX));

        fx->patch = patch;
        
	if(
		patch->instrument->getFX(window,track,fx)
		==
		FX_FAILED
	){
		return NULL;
	}

	return fx;
}


int AddFXNodeLine(
                  struct Tracker_Windows *window,
                  struct WBlocks *wblock,
                  struct WTracks *wtrack,
                  struct FXs *fxs,
                  int val,
                  const Place *p1
){
	struct FXNodeLines *fxnodeline=talloc(sizeof(struct FXNodeLines));

        int ret;
        
        {
          SCOPED_PLAYER_LOCK_IF_PLAYING();
          
          fxnodeline->val=R_BOUNDARIES(fxs->fx->min, val, fxs->fx->max);
          PlaceCopy(&fxnodeline->l.p,p1);
          ret = ListAddElement3_ns(&fxs->fxnodelines,&fxnodeline->l);
        }

        return ret;
}

static void AddNewTypeOfFxNodeLine(struct Tracker_Windows *window, const struct WBlocks *wblock, struct WTracks *wtrack, struct FX *fx, const Place *p2, int val){
  //printf("new, fxnum: %d, wtrack->fx->fx->effect_num:%d\n",fx->num,wtrack->track->fxs==NULL?-1000:wtrack->track->fx->effect_num);
  
  struct FXs *fxs=talloc(sizeof(struct FXs));
  fxs->fx=fx;
  VECTOR_push_back(&wtrack->track->fxs, fxs);
  
  struct FXNodeLines *fxnodeline=talloc(sizeof(struct FXNodeLines));
  fxnodeline->val=val;
  PlaceCopy(&fxnodeline->l.p,p2);
  ListAddElement3(&fxs->fxnodelines,&fxnodeline->l);

  UpdateAllWBlockCoordinates(window);
  window->must_redraw = true;
}

static struct FXs *get_fxs_for_fx(struct Tracks *track, struct FX *fx){
  //printf("Calling get_fxs_for_fx for track %p. num_fxs: %d\n", track, track->fxs.num_elements);
  VECTOR_FOR_EACH(struct FXs *fxs, &track->fxs){
    //printf("   Comp %d/%d, %p/%p\n", fxs->fx->effect_num, fx->effect_num, fxs->fx->patch, fx->patch);
    if (fxs->fx->effect_num == fx->effect_num && fxs->fx->patch == fx->patch)
      return fxs;
  }END_VECTOR_FOR_EACH;
  
  return NULL;
}

static void AddFXNodeLineCurrPosInternal(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, struct FX *fx, const Place *place, int val){

  Place p1;
  PlaceCopy(&p1, place);
  
        struct FXs *fxs=get_fxs_for_fx(wtrack->track, fx);
        if (fxs==NULL){
          Place p2;

          //printf("    FXS 1: %p\n",fxs);
          
          PlaceCopy(&p2, &p1);

          p2.line ++;
          
          Place lastplace;
          PlaceSetLastPos(wblock->block, &lastplace);
          
          if (PlaceGreaterThan(&p2, &lastplace))
            PlaceSetLastPos(wblock->block,&p2);

          if (PlaceGreaterOrEqual(&p1, &p2))
            p1.line --;

          if (PlaceGreaterOrEqual(place, &p2)) {
            p1.line = 0;
            p1.counter = 0;
          }

          AddNewTypeOfFxNodeLine(window, wblock, wtrack, fx, &p2, val);

          fxs=get_fxs_for_fx(wtrack->track, fx);
          //printf("    FXS 2: %p\n",fxs);
          
          R_ASSERT_RETURN_IF_FALSE(fxs!=NULL);
        }

	AddFXNodeLine(
                      window,wblock,wtrack,
                      fxs,
                      val,
                      &p1
                      );

#if !USE_OPENGL
	UpdateFXNodeLines(window,wblock,wtrack);

	ClearTrack(window,wblock,wtrack,wblock->top_realline,wblock->bot_realline);
	UpdateWTrack(window,wblock,wtrack,wblock->top_realline,wblock->bot_realline);
#endif
        
        return;
}


void AddFXNodeLineCustomFxAndPos(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, struct FX *fx, const Place *p, float val){
  int scaled_val = scale(val, 0, 1, fx->min, fx->max);
                  
  AddFXNodeLineCurrPosInternal(window, wblock, wtrack, fx, p, scaled_val);
}
  
void AddFXNodeLineCurrMousePos(struct Tracker_Windows *window){
  struct WBlocks *wblock = window->wblock;
  float x = tevent.x;
  float y = tevent.y;
  struct WTracks *wtrack = WTRACK_get(wblock, x);
  if (wtrack==NULL)
    return;
  
  Place place = GetPlaceFromY(window,
                              wblock,
                              y
                              );

  struct FX *fx=selectFX(window,wblock,wtrack);
  if(fx==NULL)
    return;

  bool use_mouse_pos = false;

  if (fx->patch != NULL && fx->patch->instrument==get_MIDI_instrument())
    use_mouse_pos = true;
  
  if (use_mouse_pos){
    float val = scale(x, wtrack->fxarea.x, wtrack->fxarea.x2, 0, 1);
    ADD_UNDO(FXs_CurrPos(window));
    PC_Pause();{
      AddFXNodeLineCustomFxAndPos(window, wblock, wtrack, fx, &place, val);
    }PC_StopPause(NULL);
  } else {
    int val = fx->defaultFXValue(fx);
    ADD_UNDO(FXs_CurrPos(window));
    PC_Pause();{
      AddFXNodeLineCurrPosInternal(window, wblock, wtrack, fx, &place, val);
    }PC_StopPause(NULL);
  }
}

void AddFXNodeLineCurrPos(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack){
  
  struct FX *fx=selectFX(window,wblock,wtrack);
  if(fx==NULL) return;

  Place place;
  PlaceCopy(&place, &wblock->reallines[wblock->curr_realline]->l.p);

  ADD_UNDO(FXs_CurrPos(window));

  PC_Pause();{
    AddFXNodeLineCustomFxAndPos(window, wblock, wtrack, fx, &place, 0.5);
  }PC_StopPause(NULL);
}


void DeleteFxNodeLine(struct Tracker_Windows *window, struct WTracks *wtrack, struct FXs *fxs, struct FXNodeLines *fxnodeline){

  int num_elements = ListFindNumElements3((struct ListHeader3*)fxs->fxnodelines);

  R_ASSERT(num_elements > 1);

  if (num_elements > 2){
  
    {
      SCOPED_PLAYER_LOCK_IF_PLAYING();
      ListRemoveElement3(&fxs->fxnodelines,&fxnodeline->l);
    }

  } else if (NextFXNodeLine(fxnodeline) == NULL && fxnodeline->val != fxs->fxnodelines->val) {
    
    safe_int_write(&fxnodeline->val, fxs->fxnodelines->val);
    
  } else {
    
    PC_Pause();{
    
      ListRemoveElement3(&fxs->fxnodelines,&fxnodeline->l);
      
      struct FX *fx = fxs->fx;
      struct Tracks *track = wtrack->track;
      
      //OS_SLIDER_release_automation_pointers(track->patch,fx->effect_num);
      (*fx->closeFX)(fx,track);
      VECTOR_remove(&track->fxs, fxs);
      
    }PC_StopPause(NULL);
    
    UpdateAllWBlockCoordinates(window);
    window->must_redraw = true;
  }
}
