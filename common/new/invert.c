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





#include "../nsmtracker.h"
#include "../placement_proc.h"
#include "../clipboard_range_calc_proc.h"
#include "../list_proc.h"
#include "../wtracks_proc.h"
#include "../undo_notes_proc.h"
#include "../undo_range_proc.h"
#include "../player_proc.h"
#include "../player_pause_proc.h"

#include "invert_proc.h"



static void Invert_notes(
	struct Notes *note,
	const Place *p1,
	const Place *p2,
	bool firsttime,
        int last_org,
	int last
){
	int next=0;

	if(note==NULL) return;

	if(PlaceGreaterOrEqual(&note->l.p,p1)){

		if(PlaceGreaterOrEqual(&note->l.p,p2)) return;

		next=note->note;

     		if(firsttime==false){
                        note->note=R_MAX(1,R_MIN(127,last - (note->note - last_org)));
		}else{
			firsttime=false;
		}
	}
	Invert_notes(NextNote(note),p1,p2,firsttime,next,note->note);
}


static void InvertRange(
	struct WBlocks *wblock
){
	struct Tracks *track;
	int lokke;

	if( ! wblock->isranged) return;

	const Place *p1=&wblock->rangey1;
	const Place *p2=&wblock->rangey2;

	track=ListFindElement1(&wblock->block->tracks->l,wblock->rangex1);

	for(lokke=0;lokke<=wblock->rangex2-wblock->rangex1;lokke++){
                Invert_notes(track->notes,p1,p2,true,0,0);
		track=NextTrack(track);
	}

}

static void InvertTrack(
	struct Blocks *block,
	struct Tracks *track
){
	Place p1,p2;

	PlaceSetFirstPos(&p1);
	PlaceSetLastPos(block,&p2);

	Invert_notes(track->notes,&p1,&p2,true,0,0);
}

static void InvertBlock(
	struct Blocks *block
){
	struct Tracks *track=block->tracks;

	while(track!=NULL){
		InvertTrack(block,track);
		track=NextTrack(track);
	}
}


void InvertRange_CurrPos(
	struct Tracker_Windows *window
){
	if(!window->wblock->isranged) return;


	ADD_UNDO(Range(
                       window,
                       window->wblock,
                       window->wblock->rangex1,
                       window->wblock->rangex2+1,
                       window->wblock->curr_realline
                       ));

	InvertRange(window->wblock);

	UpdateAndClearSomeTrackReallinesAndGfxWTracks(
		window,
		window->wblock,
		window->wblock->rangex1,
		window->wblock->rangex2
	);

}

void InvertTrack_CurrPos(
	struct Tracker_Windows *window
){

  ADD_UNDO(Notes_CurrPos(window));

	InvertTrack(window->wblock->block,window->wblock->wtrack->track);

	UpdateAndClearSomeTrackReallinesAndGfxWTracks(
		window,
		window->wblock,
		window->wblock->wtrack->l.num,
		window->wblock->wtrack->l.num
	);

}


void InvertBlock_CurrPos(
	struct Tracker_Windows *window
){

	ADD_UNDO(Range(
		window,
		window->wblock,
		0,window->wblock->block->num_tracks,
		window->wblock->curr_realline
                       ));

	InvertBlock(window->wblock->block);

	UpdateAndClearSomeTrackReallinesAndGfxWTracks(
		window,
		window->wblock,
		0,
		window->wblock->block->num_tracks-1
	);

}









