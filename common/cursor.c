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
#include "gfx_wtracks_proc.h"
#include "sliders_proc.h"
#include "blts_proc.h"
#include "wblocks_proc.h"
#include "gfx_wtrackheaders_proc.h"

#include "cursor_proc.h"


void SetCursorPos(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;
	//	NInt track=window->curr_track;
	//	int subtrack=window->curr_track_sub;

	Blt_markAll(window,LINENUMBTRACK,wblock->right_track,wblock->curr_realline,wblock->curr_realline);
	/*
	GFX_DrawCursorPos(
		window,

		wblock->a.x1,
		GetCursorY1Pos(window,wblock),
		wblock->a.x2,
		GetCursorY2Pos(window,wblock),

		GetXSubTrack_B1(wblock,track,subtrack),
		GetCursorY1Pos(window,wblock),
		GetXSubTrack_B2(wblock,track,subtrack),
		GetCursorY2Pos(window,wblock)
	);
	*/
}


int CursorRight(struct Tracker_Windows *window,struct WBlocks *wblock){
	struct WTracks *wtrack=wblock->wtrack;
	struct WTracks *leftwtrack;
	struct WTracks *rightwtrack;
	int update=0;
	int x2;

	if(window->curr_track>=0){

		window->curr_track_sub++;

		if(window->curr_track_sub>=wtrack->num_vel){
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
			wblock->left_subtrack++;
			if(wblock->left_subtrack>=leftwtrack->num_vel){
				wblock->left_subtrack= -1;
				wblock->left_track++;
			}
			leftwtrack=ListFindElement1(&wblock->wtracks->l,wblock->left_track);
			if(
				wblock->left_track==wblock->block->num_tracks-1 &&
				wblock->left_subtrack==leftwtrack->num_vel-1
			){
				return 2;
			}
			UpdateAllWTracksCoordinates(window,wblock);
			update=1;
		}
		for(;;){
		  rightwtrack=ListFindElement1(&wblock->wtracks->l,window->curr_track);
		  x2=GetXSubTrack2(rightwtrack,window->curr_track_sub);
		  if(x2>wblock->a.x2){
			leftwtrack=ListFindElement1(&wblock->wtracks->l,wblock->left_track);
			wblock->left_subtrack++;
			if(wblock->left_subtrack>=leftwtrack->num_vel){
				wblock->left_subtrack= -1;
				wblock->left_track++;
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
		if(0==window->curr_track) window->curr_track_sub= -1;
	}
	if(update==1){
		return 2;
	}else{
		return 1;
	}
}


int CursorLeft(struct Tracker_Windows *window,struct WBlocks *wblock){
	struct WTracks *wtrack=wblock->wtrack;

	if(window->curr_track>0 || (0==window->curr_track && window->curr_track_sub>=0)){

		window->curr_track_sub--;

		if(window->curr_track_sub==-2){
			wblock->wtrack=ListFindElement1(&wblock->wtracks->l,wtrack->l.num-1);
			window->curr_track_sub=wblock->wtrack->num_vel-1;
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
		if(LPBTRACK==window->curr_track) return 0;
		window->curr_track--;
		return 1;
	}
}

void TrackSelectUpdate(struct Tracker_Windows *window,struct WBlocks *wblock,int ret){
  struct WTracks *wtrack2;

	switch(ret){
		case 0:
			return;
		case 1:
			SetCursorPos(window);
			break;
		case 2:
		  UpdateWBlockCoordinates(window,wblock);
		  DrawUpAllWTracks(window,wblock);
		  DrawAllWTrackHeaders(window,wblock);

		  wtrack2=ListLast1(&wblock->wtracks->l);
		  if(wtrack2->fxarea.x2<wblock->a.x2){
		    GFX_P_FilledBox(window,0,wtrack2->fxarea.x2+2,wblock->t.y1,wblock->a.x2,wblock->t.y2);
		    GFX_FilledBox(window,0,wtrack2->fxarea.x2+2,0,wblock->a.x2,wblock->t.y1);
		  }

		  DrawBottomSlider(window);
		  //			DrawUpTrackerWindow(window);
			break;
	}
	(*wblock->wtrack->track->instrument->PP_Update)(
		wblock->wtrack->track->instrument,
		wblock->wtrack->track->patch
	);
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
		ret=max(tempret,ret);
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

	if(tracknum>=wblock->block->num_tracks || tracknum<LPBTRACK) return 0;

	if(tracknum<0){
		if(tracknum==window->curr_track) return 0;

		if(tracknum>window->curr_track){
			while(window->curr_track!=tracknum){
				tempret=CursorRight(window,wblock);
				ret=max(tempret,ret);
			}
		}else{
			while(window->curr_track!=tracknum){
				tempret=CursorLeft(window,wblock);
				ret=max(tempret,ret);
			}
		}
	}else{
		wtrack=ListFindElement1(&wblock->wtracks->l,tracknum);

		subtrack=min(wtrack->num_vel-1,subtrack);

		if(tracknum==window->curr_track && subtrack==window->curr_track_sub) return 0;

		if(tracknum>window->curr_track || (tracknum==window->curr_track && subtrack>window->curr_track_sub)){
			while(window->curr_track!=tracknum || window->curr_track_sub!=subtrack){
				tempret=CursorRight(window,wblock);
				ret=max(tempret,ret);
			}
		}else{
			while(window->curr_track!=tracknum || window->curr_track_sub!=subtrack){
				tempret=CursorLeft(window,wblock);
				ret=max(tempret,ret);
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


void CursorPrevTrack_CurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;
	NInt curr_track=window->curr_track;
	int ret;

	if(curr_track==LPBTRACK){
		ret=SetCursorPosConcrete(window,wblock,wblock->block->num_tracks-1,-1);
	}else{
		ret=SetCursorPosConcrete(window,wblock,curr_track-1,-1);
	}

	TrackSelectUpdate(window,wblock,ret);
}


