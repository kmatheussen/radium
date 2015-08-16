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
#include "visual_proc.h"
#include "wblocks_proc.h"
#include "temponodes_proc.h"
#include "playerclass.h"
#include "tracks_proc.h"
#include "notes_proc.h"

#include "wtracks_proc.h"

extern PlayerClass *pc;




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
  
  return wtrack;
}

void NewWTrack(
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

static int WTRACK_get_pianoroll_width(
                                      struct Tracker_Windows *window,
                                      struct WTracks *wtrack
                                      )
{
  if (wtrack->pianoroll_on==false)
    return 0;
  
  return (wtrack->pianoroll_highkey - wtrack->pianoroll_lowkey) * 3 * ceilf(((float)window->fontheight/4.0f));
}

// Function to use when the coordinates are not calculated.
int WTRACK_getWidth(
	struct Tracker_Windows *window,
	struct WTracks *wtrack
	)
{
  return 
    wtrack->notesonoff*((window->fontwidth*wtrack->notelength)) +
    2 + (wtrack->fxwidth*wtrack->fxonoff)
    + WTRACK_get_pianoroll_width(window, wtrack)
    + 3
    ;
}



/* Update all wtrackcoordinates starting from "wtrack". */

void UpdateWTrackCoordinates(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int wtrack_x
){

        int x = wtrack_x;

        if (wtrack->pianoroll_on) {
          wtrack->pianoroll_width = WTRACK_get_pianoroll_width(window, wtrack);
          wtrack->pianoroll_area.x = x;
          x = x + wtrack->pianoroll_width;
          wtrack->pianoroll_area.x2 = x;
        }
  
	wtrack->notearea.x  = x;
	wtrack->notearea.x2 = wtrack->notearea.x  + wtrack->notesonoff*(wtrack->notewidth+(window->fontwidth*(0+wtrack->notelength)));
        if(wtrack->is_wide)
          wtrack->notearea.x2 += 100;

	wtrack->fxarea.x    = wtrack->notearea.x2 + 2;
	wtrack->fxarea.x2   = wtrack->fxarea.x    + wtrack->fxonoff*wtrack->fxwidth;

	wtrack->x  = wtrack_x;
	wtrack->y  = wblock->a.y1;
	wtrack->y2 = wblock->a.y2;
	wtrack->x2 = wtrack->fxarea.x2;

        int y1_ = (wblock->a.y1+wblock->linearea.y)/2;
        int y2_ = (y1_+wblock->linearea.y)/2;

	wtrack->pan.x1 = wtrack->fxarea.x;
	wtrack->pan.x2 = wtrack->x2;
	//wtrack->pan.y1 = wblock->a.y1+(window->systemfontheight) + WTRACKS_SPACE*2;
	wtrack->pan.y1 = y1_;
	wtrack->pan.y2 = y2_-1;

//	wtrack->panonoff.x1 = wtrack->notearea.x+(wtrack->notearea.x2-wtrack->notearea.x)/2 - (window->fontwidth/2);
//	wtrack->panonoff.x2 = wtrack->panonoff.x1+window->fontwidth;
	wtrack->panonoff.x1 = wtrack->notearea.x+1;
	wtrack->panonoff.x2 = wtrack->notearea.x2-1;
	wtrack->panonoff.y1 = wtrack->pan.y1;
	wtrack->panonoff.y2 = wtrack->pan.y2;

	wtrack->volume.x1 = wtrack->fxarea.x;
	wtrack->volume.x2 = wtrack->x2;
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
		UpdateWTrackCoordinates(window,wblock,NextWTrack(wtrack),wtrack->x2+3);

//	wblock->right_track=wtrack->l.num;

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
	int leftX=0;
	int right_subtrack;
	struct WTracks *wtrack=wblock->wtracks;

	if(wtrack==NULL) return;

	while(wtrack!=NULL){
          SetNoteSubtrackAttributes(wtrack->track); // need track->num_subtracks variable
	  wtrack=NextWTrack(wtrack);
	}
        
	leftX = wblock->t.x1;

	wtrack=wblock->wtracks;
	while(wtrack->l.num < wblock->left_track){
	  leftX-=WTRACK_getWidth(window,wtrack);
	  wtrack=NextWTrack(wtrack);
	}

        {
          struct WTracks *left_wtrack = wtrack;
          //printf("****** Setting minimum width to x2 of track %d\n",left_wtrack->l.num);
          GFX_SetMinimumWindowWidth(window, left_wtrack->x2);
        }

	if(wblock->left_subtrack>-1){
	  leftX -= (wtrack->fxwidth*wblock->left_subtrack/wtrack->track->num_subtracks)
       	           + (wblock->left_subtrack>0 ? 1 : 0);
	}

	UpdateWTrackCoordinates(window,wblock,wblock->wtracks,leftX);

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

	wtrack=wblock->wtracks;

	while(wtrack!=NULL){
		wblock->right_track=wtrack->l.num;
		if(NextWTrack(wtrack)==NULL){
                  wblock->right_subtrack=wtrack->track->num_subtracks-1;
                  goto exit;
		}
		if(NextWTrack(wtrack)->notearea.x>=wblock->a.x2-2) break;
		wtrack=NextWTrack(wtrack);
	}

	for(right_subtrack=0;;right_subtrack++){
		if(GetXSubTrack2(wtrack,right_subtrack)>=wblock->a.x2){
			wblock->right_subtrack=right_subtrack;
			//			printf("2. wtrack: %d, r_sub: %d\n",wtrack->l.num,wblock->right_subtrack);
			break;
		}
	}

	//	wblock->t.x2=GetXSubTrack_B1(wblock,wblock->right_track,wblock->right_subtrack);
 exit:
        return;
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


void SetNoteLength(
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
	struct Tracker_Windows *window
){
	struct WBlocks *wblock=window->wblock;
	struct WTracks *wtrack=wblock->wtrack;

        int num_subtracks = GetNumSubtracks(wtrack->track);
        
	SetNoteLength(window,wtrack,2);
	wtrack->fxwidth=window->fontwidth*num_subtracks*2;

#if !USE_OPENGL
	UpdateFXNodeLines(window,wblock,wtrack);
#endif       

	window->must_redraw = true;
}


bool WTRACK_allinside(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;
	struct WTracks *rightwtrack;

	if(wblock->block->num_tracks-1 > wblock->right_track){
		return false;
	}

	if(wblock->right_track==wblock->block->num_tracks-1){
		rightwtrack=(struct WTracks *)ListFindElement1(&wblock->wtracks->l,wblock->right_track);
		if(rightwtrack->x2>wblock->a.x2){
			return false;
		}
	}
	return true;
}


void MinimizeBlock_CurrPos(
	struct Tracker_Windows *window
){
	struct WBlocks *wblock=window->wblock;
	struct WTracks *wtrack;
	int notelength=3;
	int inc=0;
	int startinc=0;
	int nummul=1;
	//	int orgnotelenght=wblock->wtrack->notelength;

        Undo_Block_CurrPos(window);

	SetCursorPosConcrete(window,wblock,0,-1);

	wblock->temponodearea.width=2;
	wtrack=wblock->wtracks;
	while(wtrack!=NULL){
		SetNoteLength(window,wtrack,2);
		wtrack->fxwidth=2;
		wtrack=NextWTrack(wtrack);
	}
	UpdateWBlockCoordinates(window,window->wblock);

	if(WTRACK_allinside(window)==false){
		goto update;
	}


	wblock->temponodearea.width=window->fontwidth*2;
	wtrack=wblock->wtracks;
	while(wtrack!=NULL){
		SetNoteLength(window,wtrack,2);
                int num_subtracks = GetNumSubtracks(wtrack->track);
		wtrack->fxwidth=window->fontwidth*num_subtracks;
		wtrack=NextWTrack(wtrack);
	}
	UpdateWBlockCoordinates(window,window->wblock);

	if(WTRACK_allinside(window)==false){
	  notelength=2;
	  startinc=2;
	  nummul=0;
	  goto calc;
	}

	wtrack=wblock->wtracks;
	wblock->temponodearea.width=window->fontwidth*3;
	while(wtrack!=NULL){
		SetNoteLength(window,wtrack,3);
                int num_subtracks = GetNumSubtracks(wtrack->track);
		wtrack->fxwidth=window->fontwidth*num_subtracks;
		wtrack=NextWTrack(wtrack);
	}
	UpdateWBlockCoordinates(window,window->wblock);

	if(WTRACK_allinside(window)==false){

	  wtrack=wblock->wtracks;
	  wblock->temponodearea.width=2;;
	  while(wtrack!=NULL){
	    SetNoteLength(window,wtrack,3);
	    wtrack->fxwidth=2;
	    wtrack=NextWTrack(wtrack);
	  }
	  UpdateWBlockCoordinates(window,window->wblock);

	  if(WTRACK_allinside(window)==false){
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
                        int num_subtracks = GetNumSubtracks(wtrack->track);
			wtrack->fxwidth=(window->fontwidth*num_subtracks*nummul)+inc;
			wtrack=NextWTrack(wtrack);
		}
		UpdateWBlockCoordinates(window,window->wblock);
	}while(WTRACK_allinside(window)==true);

	wblock->temponodearea.width=(window->fontwidth*notelength*nummul)+inc-1;
	wtrack=wblock->wtracks;
	while(wtrack!=NULL){
		SetNoteLength(window,wtrack,notelength);
                int num_subtracks = GetNumSubtracks(wtrack->track);
		wtrack->fxwidth=(window->fontwidth*num_subtracks*nummul)+inc-1;
		wtrack=NextWTrack(wtrack);
	}
	UpdateWBlockCoordinates(window,window->wblock);


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

#if 1
        // small adjustment.
        {
          struct WTracks *rightwtrack = (struct WTracks *)ListFindElement1(&wblock->wtracks->l, wblock->right_track);
          rightwtrack->x2 = wblock->a.x2;
        }
#endif

	window->must_redraw = true;

	if( ! pc->isplaying){
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

	PlayStop();

	if(next==NULL){
		next=wblock->wtracks;
	}

	Undo_Block_CurrPos(window);

	temp=CB_CopyTrack(wblock,wtrack);

	CB_PasteTrack(wblock,next,wtrack);
	CB_PasteTrack(wblock,temp,next);

#if !USE_OPENGL       
	UpdateFXNodeLines(window,wblock,wtrack);
#endif

#if !USE_OPENGL
	UpdateFXNodeLines(window,wblock,next);
#endif

	window->must_redraw = true;

	CursorNextTrack_CurrPos(window);
}


void AppendWTrack_CurrPos(struct Tracker_Windows *window, struct WBlocks *wblock){

	PlayStop();

	Undo_Block_CurrPos(window);
        printf("appending tracks. Before: %d\n",wblock->block->num_tracks);

	AppendTrack(wblock->block);
        wblock->block->num_tracks++;

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

	if(tracknum>=wblock->block->num_tracks || tracknum<0){
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

        if(tracknum>=wblock->block->num_tracks || tracknum<0){
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

