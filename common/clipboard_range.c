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
#include "placement_proc.h"
#include "gfx_wtracks_proc.h"
#include "wtracks_proc.h"
#include "clipboard_range.h"

#include "clipboard_range_proc.h"



struct Range *range=NULL;


void SetRange(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	NInt starttrack,
	NInt endtrack,
	int startrealline,
	int endrealline
){
	wblock->isranged=true;
	wblock->rangex1=starttrack;
	wblock->rangex2=endtrack;
	wblock->rangey1=startrealline;
	wblock->rangey2=endrealline;
}



void MarkRange(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	NInt track,
	int realline
){
	NInt starttrack,endtrack;
	int startrealline,endrealline;

	if(wblock->isranged){
		starttrack=wblock->rangex1;
		endtrack=track;
		if(starttrack>endtrack){
			starttrack=track;
			endtrack=wblock->rangex2;
		}
		
		startrealline=wblock->rangey1;
		endrealline=realline+1;
		if(startrealline>=endrealline){
			startrealline=realline;
			endrealline=wblock->rangey2;
		}

	}else{
		endtrack=starttrack=track;
		startrealline=realline;
		endrealline=realline+1;
	}

	SetRange(window,wblock,starttrack,endtrack,startrealline,endrealline);
}


void MarkRange_CurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;
	bool isranged=wblock->isranged;
	NInt y1=wblock->rangey1;
	NInt y2=wblock->rangey2;

	MarkRange(window,wblock,wblock->wtrack->l.num,wblock->curr_realline);

	if( isranged && (wblock->rangey1>y1 || wblock->rangey2<y2)){
		UpdateAndClearSomeTrackReallinesAndGfxWTracks(
			window,
			window->wblock,
			window->wblock->rangex1,
			window->wblock->rangex2
		);
	}else{
		UpdateAllWTracks(window,wblock,0,wblock->num_reallines);
	}

}


void CancelRange(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){
	wblock->isranged=false;
}

void CancelRange_CurrPos(struct Tracker_Windows *window){
	CancelRange(window,window->wblock);
	UpdateAndClearSomeTrackReallinesAndGfxWTracks(
		window,
		window->wblock,
		window->wblock->rangex1,
		window->wblock->rangex2
	);
}


/********************************************************
  FUNCTION
    Makes shure that the range is legal. Must be
    called after shrinking the number of reallines
    in a wblock, or the number of tracks.
********************************************************/
void MakeRangeLegal(
	struct WBlocks *wblock
){
	if(wblock->isranged==false) return;

	wblock->rangey2=min(wblock->rangey2,wblock->num_reallines-1);
	if(wblock->rangey2>=wblock->rangey1){
		wblock->isranged=false;
		return;
	};

	wblock->rangex2=min(wblock->rangex2,wblock->block->num_tracks-1);
	wblock->rangex1=min(wblock->rangex1,wblock->rangex2);
}





