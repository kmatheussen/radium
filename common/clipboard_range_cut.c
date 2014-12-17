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
#include "list_proc.h"
#include "clipboard_range_copy_proc.h"
#include "clipboard_range_calc_proc.h"
#include "wtracks_proc.h"
#include "undo_range_proc.h"
#include "player_proc.h"

#include "clipboard_range_cut_proc.h"



extern struct Range *range;


void CutRange_notes(
	struct Notes **tonote,
	struct Notes *fromnote,
	Place *p1,
	Place *p2
){
	struct Notes *next;
	if(fromnote==NULL) return;

	next=NextNote(fromnote);

	if(PlaceLessThan(&fromnote->l.p,p1)){
		CutRange_notes(tonote,next,p1,p2);
		return;
	}

	if(PlaceGreaterOrEqual(&fromnote->l.p,p2)) return;

	ListRemoveElement3(tonote,&fromnote->l);

	CutRange_notes(tonote,next,p1,p2);
}


void CutRange_stops(
	struct Stops **tostop,
	struct Stops *fromstop,
	Place *p1,
	Place *p2
){
	struct Stops *next;
	if(fromstop==NULL) return;

	next=NextStop(fromstop);

	if(PlaceLessThan(&fromstop->l.p,p1)){
		CutRange_stops(tostop,next,p1,p2);
		return;
	}

	if(PlaceGreaterOrEqual(&fromstop->l.p,p2)) return;

	ListRemoveElement3(tostop,&fromstop->l);

	CutRange_stops(tostop,next,p1,p2);
}


void CutRange(
	struct Blocks *block,
	NInt starttrack,
	NInt endtrack,
	Place *p1,
	Place *p2
){
	struct Tracks *track;
	int lokke;

	track=ListFindElement1(&block->tracks->l,starttrack);

	for(lokke=0;lokke<=endtrack-starttrack;lokke++){
		CutRange_notes(&track->notes,track->notes,p1,p2);
		CutRange_stops(&track->stops,track->stops,p1,p2);
		track=NextTrack(track);
	}

}

/**********************************************
  FUNCTION
    Does only remove all notes in the ranged
    area. (decided to wait with FXes)
**********************************************/
void CutRangedRange(
	struct WBlocks *wblock
){
	Place *p1,*p2;

	if(!wblock->isranged) return;

	p1=getRangeStartPlace(wblock);
	p2=getRangeEndPlace(wblock);

	CutRange(wblock->block,wblock->rangex1,wblock->rangex2,p1,p2);
	wblock->isranged=false;
}


void CutRange_CurrPos(
	struct Tracker_Windows *window
){
	if( ! window->wblock->isranged) return;

	PlayStop();

	Undo_Range(
		window,
		window->wblock->block,
		window->wblock->rangex1,
		window->wblock->rangex2,
		window->wblock->curr_realline
	);

	CopyRange(window->wblock);

	window->wblock->isranged=true;

	CutRangedRange(window->wblock);

	UpdateAndClearSomeTrackReallinesAndGfxWTracks(
		window,
		window->wblock,
		window->wblock->rangex1,
		window->wblock->rangex2
	);

        window->must_redraw = true;
}










