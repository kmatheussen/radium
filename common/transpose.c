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
#include "clipboard_range_calc_proc.h"
#include "list_proc.h"
#include "wtracks_proc.h"
#include "undo_notes_proc.h"
#include "undo_range_proc.h"
#include "player_proc.h"
#include "player_pause_proc.h"
#include "gfx_wtracks_proc.h"
#include "blts_proc.h"

#include "transpose_proc.h"


void Transpose_note(
	struct Notes *note,
	int trans
){
	if(note->note+trans<128 && note->note+trans>0){
		note->note+=trans;
	}
}

void Transpose_notes(
	struct Notes *note,
	Place *p1,
	Place *p2,
	int trans
){
	if(note==NULL) return;

	if(PlaceGreaterOrEqual(&note->l.p,p1)){

		if(PlaceGreaterOrEqual(&note->l.p,p2)) return;

		Transpose_note(note,trans);

	}

	Transpose_notes(NextNote(note),p1,p2,trans);
}


void TransposeRange(
	struct WBlocks *wblock,
	int trans
){
	struct Tracks *track;
	int lokke;
	Place *p1,*p2;

	if( ! wblock->isranged) return;

	p1=getRangeStartPlace(wblock);
	p2=getRangeEndPlace(wblock);

	track=ListFindElement1(&wblock->block->tracks->l,wblock->rangex1);

	for(lokke=0;lokke<=wblock->rangex2-wblock->rangex1;lokke++){
		Transpose_notes(track->notes,p1,p2,trans);
		track=NextTrack(track);
	}

}

void TransposeTrack(
	struct Tracks *track,
	int trans
){
	struct Notes *note=track->notes;

	while(note!=NULL){
		if(note->note+trans<128 && note->note+trans>0){
			note->note+=trans;
		}
		note=NextNote(note);
	}

}

void TransposeBlock(
	struct Blocks *block,
	int trans
){
	struct Tracks *track=block->tracks;

	while(track!=NULL){
		TransposeTrack(track,trans);
		track=NextTrack(track);
	}
}


void TransposeRange_CurrPos(
	struct Tracker_Windows *window,
	int trans
){
	if(!window->wblock->isranged) return;


	Undo_Range(window,window->wblock->block,window->wblock->rangex1,window->wblock->rangex2,window->wblock->curr_realline);

	TransposeRange(window->wblock,trans);

	UpdateAndClearSomeTrackReallinesAndGfxWTracks(
		window,
		window->wblock,
		window->wblock->rangex1,
		window->wblock->rangex2
	);

}

void TransposeTrack_CurrPos(
	struct Tracker_Windows *window,
	int trans
){


	Undo_Notes_CurrPos(window);

	TransposeTrack(window->wblock->wtrack->track,trans);

	UpdateAndClearSomeTrackReallinesAndGfxWTracks(
		window,
		window->wblock,
		window->wblock->wtrack->l.num,
		window->wblock->wtrack->l.num
	);

}

void TransposeNote_CurrPos(
	struct Tracker_Windows *window,
	int trans
){
	struct WBlocks *wblock;
	struct WTracks *wtrack;
	struct TrackRealline *trackrealline;
	struct TrackReallineElements *element;
	struct Notes *note;

	wblock=window->wblock;
	wtrack=wblock->wtrack;
	trackrealline= &wtrack->trackreallines[wblock->curr_realline];

	Undo_Notes_CurrPos(window);

	element=trackrealline->trackreallineelements;

        while(element!=NULL && element->type!=TRE_THISNOTELINES)
	  element=element->next;

	if(element==NULL)
	  return;

	note=(struct Notes *)element->pointer;

	Transpose_note(note,trans);
	wtrack->trackreallines[wblock->curr_realline].note=note->note;
	UpdateWTrack(window,wblock,wtrack,wblock->curr_realline,wblock->curr_realline);
}


void TransposeBlock_CurrPos(
	struct Tracker_Windows *window,
	int trans
){

	Undo_Range(
		window,
		window->wblock->block,
		0,window->wblock->block->num_tracks-1,
		window->wblock->curr_realline
	);

	TransposeBlock(window->wblock->block,trans);

	UpdateAndClearSomeTrackReallinesAndGfxWTracks(
		window,
		window->wblock,
		0,
		window->wblock->block->num_tracks-1
	);

}









