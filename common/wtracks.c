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

#include "nsmtracker.h"
#include "list_proc.h"
#include "gfx_subtrack_proc.h"
#include "fxlines_proc.h"
#include "windows_proc.h"
#include "undo_blocks_proc.h"
#include "clipboard_track_paste_proc.h"
#include "clipboard_track_copy_proc.h"
#include "cursor_proc.h"
#include "player_proc.h"
#include "player_pause_proc.h"
#include "visual_proc.h"
#include "wblocks_proc.h"
#include "temponodes_proc.h"
#include "playerclass.h"
#include "tracks_proc.h"
#include "notes_proc.h"
#include "swingtext_proc.h"

#include "wtracks_proc.h"

extern PlayerClass *pc;


int WTRACK_num_non_polyphonic_subtracks(const struct WTracks *wtrack){
  int ret = 0;

  if (wtrack->swingtext_on)
    ret+=3;
      
  if (wtrack->centtext_on)
    ret+=2;
      
  if (wtrack->chancetext_on)
    ret+=2;
      
  if (wtrack->veltext_on)
    ret+=3;

  if (wtrack->fxtext_on)
    ret += wtrack->track->fxs.num_elements * 3;
  
  return ret;
}

void CloseWTrack(struct WBlocks *wblock, NInt wtracknum){
	struct WTracks *temp=(struct WTracks *)ListFindElement1(&wblock->wtracks->l,wtracknum);

	ListRemoveElement1(&wblock->wtracks,&temp->l);
}

struct WTracks *WTRACK_new(void){
  struct WTracks *wtrack=talloc(sizeof(struct WTracks));

  //wtrack->pianoroll_on = true;
  wtrack->pianoroll_on = false;
  wtrack->pianoroll_lowkey = 48;
  wtrack->pianoroll_highkey = 60;
  wtrack->pianoroll_width = 240;

  //wtrack->swingtext_on = true;  
  //wtrack->centtext_on = true;
  //wtrack->veltext_on = true;
  //wtrack->fxtext_on = true;
  
  return wtrack;
}

static void NewWTrack(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	struct Tracks *track
){
	wtrack->track=track;
	wtrack->l.num=track->l.num;
	wtrack->notelength=3;
//	wtrack->fxwidth=window->fontwidth*10;
	wtrack->fxwidth=window->fontwidth*10;
	wtrack->notesonoff=1;
	wtrack->fxonoff=1;
	//wtrack->num_vel=1;

#if !USE_OPENGL
	UpdateFXNodeLines(window,wblock,wtrack);
#endif
	ListAddElement1(&wblock->wtracks,&wtrack->l);
}


/*
  FUNCTION
     Make the WTracks list the same as the Tracks list
     for window 'window'. Can be called after new window
     has been made, wblock->block has just been made, or _one_ track has been added.
*/

void UpdateWTracks(struct Tracker_Windows *window, struct WBlocks *wblock){
	struct Tracks *track=wblock->block->tracks;
	struct WTracks *wtrack=wblock->wtracks;

	struct WTracks *new;

	while(track!=NULL){
		if(wtrack==NULL){
                        wtrack=WTRACK_new();
			NewWTrack(window,wblock,(struct WTracks *)wtrack,(struct Tracks *)track);
		}

		if(track->l.num!=wtrack->l.num){
                        new=WTRACK_new();
  			NewWTrack(window,wblock,new,track);
			return;
		}

		track=NextTrack(track);
		wtrack=NextWTrack(wtrack);
	}
}

static int get_fxtextarea_width(const struct Tracker_Windows *window, const struct WTracks *wtrack){
  int num_fxs = wtrack->track->fxs.num_elements;
  return num_fxs * WTRACK_fxtrack_width(window->fontwidth);
}

static int WTRACKS_get_non_polyphonic_subtracks_width(const struct Tracker_Windows *window, const struct WTracks *wtrack){
  int ret = 0;

  if (wtrack->swingtext_on)
    ret += (4 * window->fontwidth) + 2;
  
  if (wtrack->centtext_on)
    ret += (2 * window->fontwidth) + 2;
  
  if (wtrack->chancetext_on)
    ret += (2 * window->fontwidth) + 2;
  
  if (wtrack->veltext_on)
    ret += (3 * window->fontwidth) + 2;

  if (wtrack->fxtext_on)
    ret += get_fxtextarea_width(window, wtrack);
  
  return ret;
}
    
static int WTRACK_get_pianoroll_width(
                                      const struct Tracker_Windows *window,
                                      const struct WTracks *wtrack
                                      )
{
  if (wtrack->pianoroll_on==false)
    return 0;
  
  return (wtrack->pianoroll_highkey - wtrack->pianoroll_lowkey) * 3 * ceilf(((float)window->fontheight/4.0f));
}

// Function to use when the coordinates are not calculated.
int WTRACK_getWidth(
	const struct Tracker_Windows *window,
	const struct WTracks *wtrack
	)
{
  return 
    wtrack->notesonoff*((window->fontwidth*wtrack->notelength)) +
    + WTRACKS_get_non_polyphonic_subtracks_width(window, wtrack)
    + 2 + (wtrack->fxwidth*wtrack->fxonoff)
    + WTRACK_get_pianoroll_width(window, wtrack)
    + 3
    ;
}


int WTRACKS_getWidth(const struct Tracker_Windows *window,
                     const struct WBlocks *wblock){

  struct WTracks *wtrack = wblock->wtracks;

  while(true){
    struct WTracks *next = NextWTrack(wtrack);
    if (next==NULL)
      return wtrack->x2 - wblock->skew_x - wblock->t.x1;
    wtrack = next;
  }

  R_ASSERT(false);
  return 0;
}


/* Update all wtrackcoordinates starting from "wtrack". */

static void UpdateWTrackCoordinates(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int wtrack_x
){

        int x = wtrack_x;

        // swing
        wtrack->swingtextarea.x = x;
        wtrack->swingtextarea.x2 = wtrack->swingtextarea.x + (window->fontwidth * 3);

        if (wtrack->swingtext_on==true){
          wtrack->swingtext_fits_reallines = swingtext_fits_reallines(wblock, wtrack->track->filledout_swings.array);

          if (!wtrack->swingtext_fits_reallines)
            wtrack->swingtextarea.x2 = wtrack->swingtextarea.x + (window->fontwidth * 4);

          x = wtrack->swingtextarea.x2 + 2;
        }

        // pianoroll
        if (wtrack->pianoroll_on) {
          wtrack->pianoroll_width = WTRACK_get_pianoroll_width(window, wtrack);
          wtrack->pianoroll_area.x = x;
          x = x + wtrack->pianoroll_width;
          wtrack->pianoroll_area.x2 = x;
        }

        // note area
	wtrack->notearea.x  = x;
	wtrack->notearea.x2 = wtrack->notearea.x  + wtrack->notesonoff*(wtrack->notewidth+(window->fontwidth*(0+wtrack->notelength)));
        if(wtrack->is_wide)
          wtrack->notearea.x2 += 100;

        x = wtrack->notearea.x2 + 2;

        // centtext
        wtrack->centtextarea.x = x;
        wtrack->centtextarea.x2 = wtrack->centtextarea.x + (window->fontwidth * 2);

        if (wtrack->centtext_on==true)
          x = wtrack->centtextarea.x2 + 2;

        // chancetext
        wtrack->chancetextarea.x = x;
        wtrack->chancetextarea.x2 = wtrack->chancetextarea.x + (window->fontwidth * 2);

        if (wtrack->chancetext_on==true)
          x = wtrack->chancetextarea.x2 + 2;

        // veltext
        wtrack->veltextarea.x  = x;
	wtrack->veltextarea.x2 = wtrack->veltextarea.x + (window->fontwidth * 3);

        if (wtrack->veltext_on==true)
          x = wtrack->veltextarea.x2 + 2;

        // fxtext
        wtrack->fxtextarea.x = x;
        wtrack->fxtextarea.x2 = wtrack->fxtextarea.x + get_fxtextarea_width(window, wtrack);

        if (wtrack->fxtext_on)
          x = wtrack->fxtextarea.x2 + 2;

        // fx area
        wtrack->fxarea.x  = x;
	wtrack->fxarea.x2 = wtrack->fxarea.x    + wtrack->fxonoff*wtrack->fxwidth;
        
	wtrack->x  = wtrack_x;
	wtrack->y  = wblock->a.y1;
	wtrack->y2 = wblock->a.y2;
	wtrack->x2 = wtrack->fxarea.x2;

        int y1_ = (wblock->a.y1+wblock->linearea.y)/2;
        int y2_ = (y1_+wblock->linearea.y)/2;

//	wtrack->panonoff.x1 = wtrack->notearea.x+(wtrack->notearea.x2-wtrack->notearea.x)/2 - (window->fontwidth/2);
//	wtrack->panonoff.x2 = wtrack->panonoff.x1+window->fontwidth;
        wtrack->panonoff.x1 = wtrack->notearea.x  + 1;
        wtrack->panonoff.x2 = wtrack->notearea.x2 - 1;
        if (wtrack->swingtext_on==true){
          int swingwidth = wtrack->swingtextarea.x2 - wtrack->swingtextarea.x + 2;
          wtrack->panonoff.x1 -= swingwidth;
          wtrack->panonoff.x2 -= swingwidth;
        }

	wtrack->panonoff.y1 = wtrack->pan.y1;
	wtrack->panonoff.y2 = wtrack->pan.y2;

	wtrack->pan.x1 = wtrack->panonoff.x2 + 3;
	wtrack->pan.x2 = wtrack->x2;
	//wtrack->pan.y1 = wblock->a.y1+(window->systemfontheight) + WTRACKS_SPACE*2;
	wtrack->pan.y1 = y1_;
	wtrack->pan.y2 = y2_-1;

	wtrack->volume.x1 = wtrack->pan.x1;
	wtrack->volume.x2 = wtrack->pan.x2;
	wtrack->volume.y1 = wtrack->pan.y2+2;
	//wtrack->volume.y2 = wblock->a.y1+(window->systemfontheight*2)-2;
	wtrack->volume.y2 = wtrack->volume.y1 + (wtrack->pan.y2-wtrack->pan.y1);//wblock->linearea.y-1;

	wtrack->volumeonoff.x1 = wtrack->panonoff.x1;
	wtrack->volumeonoff.x2 = wtrack->panonoff.x2;
          
	wtrack->volumeonoff.y1 = wtrack->volume.y1;
	wtrack->volumeonoff.y2 = wtrack->volume.y2;

/*
	if(wtrack->x >= wblock->a.x2){
		wblock->right_track=wtrack->l.num;
		return wtrack;
	}
*/

	if(NextWTrack(wtrack)!=NULL)
		UpdateWTrackCoordinates(window,wblock,NextWTrack(wtrack),wtrack->x2+3+1);

//	wblock->right_track=wtrack->l.num;
        
        //window->must_redraw = true;  // Can not call must_redraw here since UpdateWTrackCoordinates is called from DrawUpTrackerWindow()

	return;
}


/**********************************************************************
  FUNCTION
    Update all the visible WTracks starting from wblock->left_track,
    wblock->left_subtrack. Allso sets the wblock->right_track and
    wblock->right_subtrack variables.
**********************************************************************/

void UpdateAllWTracksCoordinates(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){
  //int leftX=0;
	//int right_subtrack;
	struct WTracks *wtrack=wblock->wtracks;

	if(wtrack==NULL) return;

        //printf("   Block %d\n", wblock->l.num);
               
	while(wtrack!=NULL){
          SetNotePolyphonyAttributes(wtrack->track); // Make sure track->polyphony is valid
	  wtrack=NextWTrack(wtrack);
	}

	//leftX = wblock->t.x1;

	wtrack=wblock->wtracks;
        /*
	while(wtrack->l.num < wblock->left_track){
	  leftX-=WTRACK_getWidth(window,wtrack);
	  wtrack=NextWTrack(wtrack);
	}
        */

        {
          struct WTracks *left_wtrack = wtrack;
          //printf("****** Setting minimum width to x2 of track %d\n",left_wtrack->l.num);
          GFX_SetMinimumWindowWidth(window, left_wtrack->x2);
        }

        /*
        // TODO: Fix with the new subtrack system
	if(wblock->left_subtrack>=0){
	  leftX -= (wtrack->fxwidth*wblock->left_subtrack/wtrack->track->polyphony)
                 + (wblock->left_subtrack>0 ? 1 : 0);
	}
        */

	//UpdateWTrackCoordinates(window,wblock,wblock->wtracks,leftX);
        UpdateWTrackCoordinates(window,wblock,wtrack,wblock->skew_x + wblock->t.x1);//wblock->t.x1);
        
#if 0
	wtrack=wblock->wtracks;
	while(wtrack->l.num < wblock->left_track){
		if(wtrack->num_vel==0) wtrack->num_vel=1;
		wtrack=NextWTrack(wtrack);
	}

	if(wblock->left_subtrack==-1)
		leftX=wblock->temponodearea.x2;
	else
		leftX=wblock->temponodearea.x2 - (
			(wtrack->fxwidth*wblock->left_subtrack/wtrack->num_vel) +
			wtrack->notesonoff*((window->fontwidth*wtrack->notelength)) + 2
			+ (wblock->left_subtrack>0 ? 1 : 0)
		);

	UpdateWTrackCoordinates(window,wblock,wtrack,leftX);
#endif

        /*
	wtrack=wblock->wtracks;

	while(wtrack!=NULL){
		wblock->right_track=wtrack->l.num;
                struct WTracks *next_wtrack = NextWTrack(wtrack);
		if(next_wtrack==NULL){
                  int num_subtracks = WTRACK_num_subtracks(wtrack);
                  wblock->right_subtrack=num_subtracks-1;
                  goto exit;
		}
		if(next_wtrack->notearea.x>=wblock->a.x2-2) break;
		wtrack=next_wtrack;
	}

	for(right_subtrack=0;;right_subtrack++){
		if(GetXSubTrack2(wtrack,right_subtrack)>=wblock->a.x2){
			wblock->right_subtrack=right_subtrack;
			//			printf("2. wtrack: %d, r_sub: %d\n",wtrack->l.num,wblock->right_subtrack);
			break;
		}
	}
        */

	//	wblock->t.x2=GetXSubTrack_B1(wblock,wblock->right_track,wblock->right_subtrack);
        // exit:
        //return;
}


void UpdateAndClearSomeTrackReallinesAndGfxWTracks(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	NInt starttrack,
	NInt endtrack
){
  //  struct WTracks *startwtrack,*endwtrack;

#if !USE_OPENGL
	UpdateSomeFXNodeLines(window,wblock,starttrack,endtrack);
	UpdateAndClearSomeWTracks(window,wblock,starttrack,endtrack,wblock->top_realline,wblock->bot_realline);
#endif

	//	startwtrack=ListFindElement1(&wblock->wtracks->l,starttrack);
	//	endwtrack=ListFindElement1(&wblock->wtracks->l,endtrack);
	/*
	GFX_P2V_bitBlt(
		       window,
		       startwtrack->x,wblock->t.y1,
		       startwtrack->x,wblock->t.y1,
		       endwtrack->x2-startwtrack->x+1,
		       wblock->t.y2-wblock->t.y1
		       );
	*/
		 /*

				struct Tracker_Windows *window,
				int from_x,int from_y,
				int to_x,int to_y,
				int width,int height
				){
		 */

}


static void SetNoteLength(
	struct Tracker_Windows *window,
	struct WTracks *wtrack,
	int size
){
	wtrack->notelength=size;
}

void ChangeNoteLength_CurrPos(
	struct Tracker_Windows *window
){
	struct WTracks *wtrack=window->wblock->wtrack;
	SetNoteLength(window,wtrack,wtrack->notelength=wtrack->notelength==3?2:3);
	window->must_redraw = true;
}

void ChangeNoteLength_Block_CurrPos(
	struct Tracker_Windows *window
){
	struct WTracks *wtrack=window->wblock->wtrack;
	int size = wtrack->notelength==3?2:3;

        wtrack=window->wblock->wtracks; // not setting the same value
	while(wtrack!=NULL){
		SetNoteLength(window,wtrack,size);
		wtrack=NextWTrack(wtrack);
	}

	window->must_redraw = true;
}

void ChangeNoteAreaWidth_CurrPos(
	struct Tracker_Windows *window
){
	struct WTracks *wtrack=window->wblock->wtrack;
        wtrack->is_wide = !wtrack->is_wide;
	window->must_redraw = true;
}

void ChangeNoteAreaWidth_Block_CurrPos(
	struct Tracker_Windows *window
){
	struct WTracks *wtrack=window->wblock->wtrack;
	bool is_wide = !wtrack->is_wide;

        wtrack=window->wblock->wtracks; // not setting the same value
	while(wtrack!=NULL){
          wtrack->is_wide = is_wide;
          wtrack=NextWTrack(wtrack);
	}

	window->must_redraw = true;
}

void MinimizeTrack_CurrPos(
                           struct Tracker_Windows *window,
                           struct WBlocks *wblock,
                           struct WTracks *wtrack                           
){

	SetNoteLength(window,wtrack,2);
	wtrack->fxwidth=window->fontwidth*wtrack->track->polyphony*2;

#if !USE_OPENGL
	UpdateFXNodeLines(window,wblock,wtrack);
#endif       

	window->must_redraw = true;
}


static bool WTRACK_allinside(struct Tracker_Windows *window, struct WBlocks *wblock){
  struct WTracks *last = ListLast1(&wblock->wtracks->l);
  if (last->x2 <= wblock->t.x2)
    return true;
  else
    return false;
}


void MinimizeBlock_CurrPos(
                           struct Tracker_Windows *window,
                           struct WBlocks *wblock
){
	struct WTracks *wtrack;
	int notelength=3;
	int inc=0;
	int startinc=0;
	int nummul=1;
	//	int orgnotelenght=wblock->wtrack->notelength;

        ADD_UNDO(Block_CurrPos(window));

        wblock->skew_x = 0;
        
	SetCursorPosConcrete(window,wblock,0,-1);

	wblock->temponodearea.width=2;
	wtrack=wblock->wtracks;
	while(wtrack!=NULL){
		SetNoteLength(window,wtrack,2);
		wtrack->fxwidth=2;
		wtrack=NextWTrack(wtrack);
	}
	UpdateWBlockCoordinates(window,wblock);

	if(WTRACK_allinside(window, wblock)==false){
		goto update;
	}


	wblock->temponodearea.width=window->fontwidth*2;
	wtrack=wblock->wtracks;
	while(wtrack!=NULL){
		SetNoteLength(window,wtrack,2);
                int polyphony = wtrack->track->polyphony;
		wtrack->fxwidth=window->fontwidth*polyphony;
		wtrack=NextWTrack(wtrack);
	}
	UpdateWBlockCoordinates(window,wblock);

	if(WTRACK_allinside(window, wblock)==false){
	  notelength=2;
	  startinc=2;
	  nummul=0;
	  goto calc;
	}

	wtrack=wblock->wtracks;
	wblock->temponodearea.width=window->fontwidth*3;
	while(wtrack!=NULL){
		SetNoteLength(window,wtrack,3);
		wtrack->fxwidth=window->fontwidth*wtrack->track->polyphony;
		wtrack=NextWTrack(wtrack);
	}
	UpdateWBlockCoordinates(window,wblock);

	if(WTRACK_allinside(window, wblock)==false){

	  wtrack=wblock->wtracks;
	  wblock->temponodearea.width=2;;
	  while(wtrack!=NULL){
	    SetNoteLength(window,wtrack,3);
	    wtrack->fxwidth=2;
	    wtrack=NextWTrack(wtrack);
	  }
	  UpdateWBlockCoordinates(window,wblock);

	  if(WTRACK_allinside(window, wblock)==false){
	    notelength=2;
	  }else{
	    nummul=0;
	  }

	}
	startinc=0;

 calc:
	inc=startinc;
	do{
		inc++;
		wblock->temponodearea.width=(window->fontwidth*notelength*nummul)+inc;
		wtrack=wblock->wtracks;
		while(wtrack!=NULL){
			SetNoteLength(window,wtrack,notelength);
			wtrack->fxwidth=(window->fontwidth*wtrack->track->polyphony*nummul)+inc;
			wtrack=NextWTrack(wtrack);
		}
		UpdateWBlockCoordinates(window,wblock);
	}while(WTRACK_allinside(window, wblock)==true);

	wblock->temponodearea.width=(window->fontwidth*notelength*nummul)+inc-1;
	wtrack=wblock->wtracks;
	while(wtrack!=NULL){
		SetNoteLength(window,wtrack,notelength);
		wtrack->fxwidth=(window->fontwidth*wtrack->track->polyphony*nummul)+inc-1;
		wtrack=NextWTrack(wtrack);
	}
	UpdateWBlockCoordinates(window,wblock);


update:
	/*
	UpdateWTempoNodes(window,wblock);
	wtrack=wblock->wtracks;
	while(wtrack!=NULL){
		UpdateFXNodeLines(window,wblock,wtrack);
		UpdateTrackReallines(window,wblock,wtrack);
		wtrack=NextWTrack(wtrack);
	}
	*/

#if 0
        // small adjustment.
        {
          struct WTracks *rightwtrack = (struct WTracks *)ListFindElement1(&wblock->wtracks->l, wblock->right_track);
          rightwtrack->x2 = wblock->a.x2;
        }
#endif

	window->must_redraw = true;

	if( ! is_playing()){
          GFX_update_instrument_patch_gui(wblock->wtrack->track->patch);
	}
}


void SwapTrack_CurrPos(
	struct Tracker_Windows *window
){
	struct WBlocks *wblock=window->wblock;
	struct WTracks *wtrack=wblock->wtrack;
	struct WTracks *next=NextWTrack(wtrack);
	struct WTracks *temp;

        PC_Pause();{
            
          if(next==NULL){
            next=wblock->wtracks;
          }

          ADD_UNDO(Block_CurrPos(window));

          temp=CB_CopyTrack(wblock,wtrack);
          
          mo_CB_PasteTrack(wblock,next,wtrack);
          mo_CB_PasteTrack(wblock,temp,next);
          
#if !USE_OPENGL       
          UpdateFXNodeLines(window,wblock,wtrack);
#endif
          
#if !USE_OPENGL
          UpdateFXNodeLines(window,wblock,next);
#endif

        }PC_StopPause(NULL);

        window->must_redraw = true;

        CursorNextTrack_CurrPos(window);
}


void AppendWTrack_CurrPos(struct Tracker_Windows *window, struct WBlocks *wblock){

	ADD_UNDO(Block_CurrPos(window));
        printf("appending tracks. Before: %d\n",wblock->block->num_tracks);

        PC_Pause();{
          AppendTrack(wblock->block);
          wblock->block->num_tracks++;
        }PC_StopPause(NULL);
        
        UpdateWTracks(window,wblock);

        printf("appending tracks. After: %d\n",wblock->block->num_tracks);

        window->must_redraw = true;
}


int WTRACK_getx1(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	NInt tracknum
){

	struct WTracks *wtrack;

	switch(tracknum){
	case LINENUMBTRACK:
		return wblock->linenumarea.x;
		break;
	case TEMPOCOLORTRACK:
		return wblock->tempocolorarea.x;
		break;
	case SWINGTRACK:
		return wblock->swingTypearea.x;
		break;
	case SIGNATURETRACK:
		return wblock->signaturearea.x;
		break;
	case LPBTRACK:
		return wblock->lpbTypearea.x;
		break;
	case TEMPOTRACK:
		return wblock->tempoTypearea.x;
		break;
	case TEMPONODETRACK:
		return wblock->temponodearea.x;
		break;
	}

	if(tracknum < 0 || tracknum>=wblock->block->num_tracks){
          RError("illegal track number %d supplied function WTRACK_getx1 in file wtracks.c",tracknum);
          return wblock->temponodearea.x2+3;
	}

	wtrack=ListFindElement1(&wblock->wtracks->l,tracknum);

	return wtrack->x;
}


int WTRACK_getx2(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	NInt tracknum
){

	struct WTracks *wtrack;

        switch(tracknum){
        case LINENUMBTRACK:
          return wblock->linenumarea.x2;
          break;
        case TEMPOCOLORTRACK:
          return wblock->tempocolorarea.x2;
          break;
        case SWINGTRACK:
          return wblock->swingarea.x2;
          break;
        case SIGNATURETRACK:
          return wblock->signaturearea.x2;
            break;
        case LPBTRACK:
          return wblock->lpbarea.x2;
          break;
        case TEMPOTRACK:
          return wblock->tempoarea.x2;
            break;
        case TEMPONODETRACK:
          return wblock->temponodearea.x2;
          break;
        }

        if(tracknum < 0 || tracknum>=wblock->block->num_tracks){
          RError("illegal track number %d supplied function WTRACK_getx2 in file wtracks.c",tracknum);
          return wblock->temponodearea.x2+3;
	}

	wtrack=ListFindElement1(&wblock->wtracks->l,tracknum);

	return wtrack->x2;

}

struct WTracks *WTRACK_get(struct WBlocks *wblock, int x){
  struct WTracks *wtrack = wblock->wtracks;

  while(wtrack!=NULL){
    if(x>=wtrack->x && x<wtrack->x2)
      break;
    wtrack = NextWTrack(wtrack);
  }

  return wtrack;  
}

