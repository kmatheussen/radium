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
#include "visual_proc.h"
#include "common_proc.h"
#include "gfx_subtrack_proc.h"
#include "list_proc.h"
#include "windows_proc.h"
#include "wtracks_proc.h"
#include "gfx_wblocks_proc.h"
#include "sliders_proc.h"
#include "blts_proc.h"
#include "wblocks_proc.h"
#include "gfx_wtrackheaders_proc.h"
#include "../midi/midi_i_input_proc.h"
#include "notes_proc.h"

#include "cursor_proc.h"


void R_SetCursorPos(struct Tracker_Windows *window){
#if !USE_OPENGL
	struct WBlocks *wblock=window->wblock;
	Blt_markAll(window,LINENUMBTRACK,wblock->right_track,wblock->curr_realline,wblock->curr_realline);
#endif
}


int CursorRight(struct Tracker_Windows *window,struct WBlocks *wblock){
	struct WTracks *wtrack=wblock->wtrack;
	struct WTracks *leftwtrack;
	struct WTracks *rightwtrack;
	int update=0;
	int x2;
        
	if(window->curr_track>=0){

		window->curr_track_sub++;
                int num_subtracks = GetNumSubtracks(wtrack);

		if(window->curr_track_sub>=num_subtracks){
			window->curr_track++;
			if(NextWTrack(wtrack)==NULL){
				window->curr_track--;
				window->curr_track_sub--;
				return 0;
			}else{
				window->curr_track_sub= -1;
				wblock->wtrack=NextWTrack(wtrack);
			}
		}

		while(
			window->curr_track>wblock->right_track
			||
			(
			 window->curr_track==wblock->right_track
			 && window->curr_track_sub>wblock->right_subtrack
			 )
		){
			leftwtrack=ListFindElement1(&wblock->wtracks->l,wblock->left_track);
                        int num_subtracks = GetNumSubtracks(leftwtrack);
			wblock->left_subtrack++;
			if(wblock->left_subtrack>=num_subtracks){
                          if (wblock->left_track < wblock->block->num_tracks-1) {
                            wblock->left_subtrack= -1;
                            wblock->left_track++;
                            //return 0;
                          } else {
                            UpdateAllWTracksCoordinates(window,wblock);
                            wblock->left_subtrack--;
                            return 1;
                          }
			}
			leftwtrack=ListFindElement1(&wblock->wtracks->l,wblock->left_track);
			if(
				wblock->left_track==wblock->block->num_tracks-1 &&
				wblock->left_subtrack==num_subtracks-1
			){
                                UpdateAllWTracksCoordinates(window,wblock);
				return 2;
			}
			UpdateAllWTracksCoordinates(window,wblock);
			update=1;
		}
		for(;;){
		  rightwtrack=ListFindElement1(&wblock->wtracks->l,window->curr_track);
                  int num_subtracks = GetNumSubtracks(rightwtrack);
		  x2=GetXSubTrack2(rightwtrack,window->curr_track_sub);
		  if(x2>wblock->a.x2){
			leftwtrack=ListFindElement1(&wblock->wtracks->l,wblock->left_track);
			wblock->left_subtrack++;
			if(wblock->left_subtrack>=num_subtracks){
                          if (wblock->left_track < wblock->block->num_tracks-1) {
				wblock->left_subtrack= -1;
				wblock->left_track++;
                          } else {
                            wblock->left_subtrack--;
                            UpdateAllWTracksCoordinates(window,wblock);
                            return 1;
                          }
			}
			leftwtrack=ListFindElement1(&wblock->wtracks->l,wblock->left_track);
			UpdateAllWTracksCoordinates(window,wblock);
			update=1;
		  }else{
		    break;
		  }
		}
	
	}else{
		window->curr_track++;

                if (window->curr_track==LPBTRACK && window->show_lpb_track==false)
                  window->curr_track++;

                if (window->curr_track==SIGNATURETRACK && window->show_signature_track==false)
                  window->curr_track++;
                
                if (window->curr_track==LINENUMBTRACK)
                  window->curr_track++;

                if (window->curr_track==TEMPONODETRACK && window->show_reltempo_track==false)
                  window->curr_track++;

		if (0==window->curr_track)
                  window->curr_track_sub= -1;
                
                
	}
	if(update==1){
		return 2;
	}else{
		return 1;
	}
}

static void set_curr_track_to_leftmost_legal_track(struct Tracker_Windows *window){
  if (false)
    printf("what?\n");
  
  else if (window->show_bpm_track==true)
    window->curr_track=TEMPOTRACK;
  
  else if (window->show_lpb_track==true)
    window->curr_track=LPBTRACK;
  
  else if (window->show_signature_track==true)
    window->curr_track=SIGNATURETRACK;
  
  else if (window->show_reltempo_track==true)
    window->curr_track=TEMPONODETRACK;
  
  else {
    window->curr_track = window->wblock->left_track;
    window->curr_track_sub = -1;
  }
}

int CursorLeft(struct Tracker_Windows *window,struct WBlocks *wblock){
	struct WTracks *wtrack=wblock->wtrack;

	if(window->curr_track>0 || (0==window->curr_track && window->curr_track_sub>=0)){

		window->curr_track_sub--;

		if(window->curr_track_sub==-2){
			wblock->wtrack=ListFindElement1(&wblock->wtracks->l,wtrack->l.num-1);
                        int num_subtracks = GetNumSubtracks(wblock->wtrack);
			window->curr_track_sub=num_subtracks-1;
			window->curr_track--;
		}

		if(
			window->curr_track<wblock->left_track ||
			(window->curr_track==wblock->left_track && window->curr_track_sub<wblock->left_subtrack)
		){
			wblock->left_subtrack=window->curr_track_sub;
			wblock->left_track=window->curr_track;
			UpdateAllWTracksCoordinates(window,wblock);
			return 2;
		}else{
			return 1;
		}
	}else{
                
                if (window->curr_track==TEMPOTRACK)
                  return 0;
                
		window->curr_track--;

                if (window->curr_track==TEMPONODETRACK && window->show_reltempo_track==false)
                  window->curr_track--;

                if (window->curr_track==LINENUMBTRACK)
                  window->curr_track--;

                if (window->curr_track==SIGNATURETRACK && window->show_signature_track==false)
                  window->curr_track--;
                
                if (window->curr_track==LPBTRACK && window->show_lpb_track==false)
                  window->curr_track--;
                
                if (window->curr_track==TEMPOTRACK && window->show_bpm_track==false)
                  set_curr_track_to_leftmost_legal_track(window);

		return 1;
	}
}

void TrackSelectUpdate(struct Tracker_Windows *window,struct WBlocks *wblock,int ret){

	switch(ret){
		case 0:
			return;
		case 1:
			R_SetCursorPos(window);
			break;
		case 2:
                  window->must_redraw = true;
#if 0
		  UpdateWBlockCoordinates(window,wblock);
		  DrawUpAllWTracks(window,wblock,NULL);
		  DrawAllWTrackHeaders(window,wblock);

                  {
                    struct WTracks *wtrack2=ListLast1(&wblock->wtracks->l);
                    if(wtrack2->fxarea.x2<wblock->a.x2){
                      GFX_FilledBox(window,0,wtrack2->fxarea.x2+2,wblock->t.y1,wblock->a.x2,wblock->t.y2,PAINT_BUFFER);
                      GFX_FilledBox(window,0,wtrack2->fxarea.x2+2,0,wblock->a.x2,wblock->t.y1,PAINT_DIRECTLY);
                    }
                  }

		  DrawBottomSlider(window);
                  DrawUpTrackerWindow(window);
#endif
                  break;
	}

        GFX_update_instrument_patch_gui(wblock->wtrack->track->patch);
        DrawAllWTrackHeaders(window,wblock);

        window->must_redraw=true;
}

void CursorRight_CurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;
	TrackSelectUpdate(window,wblock,CursorRight(window,wblock));
}



void CursorLeft_CurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;
	TrackSelectUpdate(window,wblock,CursorLeft(window,wblock));
}

int CursorNextTrack(struct Tracker_Windows *window,struct WBlocks *wblock){
	int curr_track=window->curr_track;
	int ret=0,tempret;

	while(curr_track==window->curr_track){
		tempret=CursorRight(window,wblock);
		ret=R_MAX(tempret,ret);
	}
	return ret;
}



int SetCursorPosConcrete(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	NInt tracknum,
	int subtrack
){
	struct WTracks *wtrack;
	int ret=0,tempret;

	if(tracknum>=wblock->block->num_tracks || tracknum<TEMPOTRACK) return 0;

	if(tracknum<0){
		if(tracknum==window->curr_track) return 0;

		if(tracknum>window->curr_track){
			while(window->curr_track!=tracknum){
				tempret=CursorRight(window,wblock);
				ret=R_MAX(tempret,ret);
			}
		}else{
			while(window->curr_track!=tracknum){
				tempret=CursorLeft(window,wblock);
				ret=R_MAX(tempret,ret);
			}
		}
	}else{
		wtrack=ListFindElement1(&wblock->wtracks->l,tracknum);
                int num_subtracks = GetNumSubtracks(wtrack);

		subtrack=R_MIN(num_subtracks-1,subtrack);

		if(tracknum==window->curr_track && subtrack==window->curr_track_sub) return 0;

		if(tracknum>window->curr_track || (tracknum==window->curr_track && subtrack>window->curr_track_sub)){
			while(window->curr_track!=tracknum || window->curr_track_sub!=subtrack){
				tempret=CursorRight(window,wblock);
				ret=R_MAX(tempret,ret);
			}
		}else{
			while(window->curr_track!=tracknum || window->curr_track_sub!=subtrack){
				tempret=CursorLeft(window,wblock);
				ret=R_MAX(tempret,ret);
			}
		}
	}

	return ret;
}

void SetCursorPosConcrete_CurrPos(
	struct Tracker_Windows *window,
	NInt tracknum
){
	struct WBlocks *wblock=window->wblock;
	int ret;

	if(tracknum>=wblock->block->num_tracks) return;

	ret=SetCursorPosConcrete(
		window,
		wblock,
		tracknum,
		-1
	);

	TrackSelectUpdate(window,wblock,ret);
}


void CursorNextTrack_CurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;
	struct WTracks *wtrack=wblock->wtrack;
	int ret;

	if(wtrack->l.next==NULL){
		ret=SetCursorPosConcrete(window,wblock,0,-1);
	}else{
		ret=CursorNextTrack(window,wblock);
	}

	TrackSelectUpdate(window,wblock,ret);
}

static int find_track_left(struct Tracker_Windows *window){

  if (window->curr_track==TEMPOTRACK)
    return window->wblock->block->num_tracks-1;
  
  int tracknum = window->curr_track-1;
  
  if (tracknum==TEMPONODETRACK && window->show_reltempo_track==false)
    tracknum--;

  if (tracknum==LINENUMBTRACK)
    tracknum--;

  if (tracknum==SIGNATURETRACK && window->show_signature_track==false)
    tracknum--;

  if (tracknum==LPBTRACK && window->show_lpb_track==false)
    tracknum--;

  if (tracknum==TEMPOTRACK && window->show_bpm_track==false)
    tracknum = window->wblock->block->num_tracks-1;

  return tracknum;
}

void CursorPrevTrack_CurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;
	NInt curr_track=window->curr_track;

        int new_track = find_track_left(window);
        if (new_track==curr_track) // can happen.
          return;
        
        int ret = SetCursorPosConcrete(window,wblock,new_track,-1);
        
	TrackSelectUpdate(window,wblock,ret);
}


