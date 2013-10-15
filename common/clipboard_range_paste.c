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
#include "clipboard_range_calc_proc.h"
#include "placement_proc.h"
#include "list_proc.h"
#include "clipboard_range.h"
#include "clipboard_range_cut_proc.h"
#include "wtracks_proc.h"
#include "undo_range_proc.h"
#include "player_proc.h"

#include "clipboard_range_paste_proc.h"



extern struct Range *range;

void PasteRange_velocities(
	struct Blocks *block,
	struct Velocities **tovelocity,
	struct Velocities *fromvelocity,
	Place *place
){
	Place lastplace;
	struct Velocities *velocity;

	if(fromvelocity==NULL) return;

	velocity=talloc(sizeof(struct Velocities));

	PlaceSetLastPos(block,&lastplace);

	PlaceCopy(&velocity->l.p,&fromvelocity->l.p);
	PlaceAdd(&velocity->l.p,place);

	if(PlaceGreaterOrEqual(&velocity->l.p,&lastplace)) return;

	velocity->velocity=fromvelocity->velocity;

	ListAddElement3(tovelocity,&velocity->l);

	PasteRange_velocities(block,tovelocity,NextVelocity(fromvelocity),place);
}

void PasteRange_notes(
	struct Blocks *block,
	struct Tracks *track,
	struct Notes *fromnote,
	Place *place
){
	Place lastplace;
	struct Notes *note;

	if(fromnote==NULL) return;

	note=talloc(sizeof(struct Notes));

	note->noend=fromnote->noend;

	PlaceSetLastPos(block,&lastplace);

	PlaceCopy(&note->l.p,&fromnote->l.p);
	PlaceAdd(&note->l.p,place);

	if(PlaceGreaterOrEqual(&note->l.p,&lastplace)) return;

	PlaceCopy(&note->end,&fromnote->end);
	PlaceAdd(&note->end,place);
	if(PlaceGreaterOrEqual(&note->end,&lastplace)){
		PlaceSetLastPos(block,&note->end);
		note->noend=1;
	}

	note->note=fromnote->note;
	note->velocity=fromnote->velocity;
	note->cents=fromnote->cents;
	note->velocity_end=fromnote->velocity_end;

	ListAddElement3(&track->notes,&note->l);

	PasteRange_velocities(block,&note->velocities,fromnote->velocities,place);

	PasteRange_notes(block,track,NextNote(fromnote),place);
}


void PasteRange_stops(
	struct Blocks *block,
	struct Tracks *track,
	struct Stops *fromstop,
	Place *place
){
	Place lastplace;
	struct Stops *stop;

	if(fromstop==NULL) return;

	stop=talloc(sizeof(struct Stops));

	PlaceSetLastPos(block,&lastplace);

	PlaceCopy(&stop->l.p,&fromstop->l.p);
	PlaceAdd(&stop->l.p,place);

	if(PlaceGreaterOrEqual(&stop->l.p,&lastplace)) return;

	ListAddElement3(&track->stops,&stop->l);

	PasteRange_stops(block,track,NextStop(fromstop),place);
}


void PasteRange(
	struct Blocks *block,
	NInt tracknum,
	Place *place
){
	struct Tracks *track;
	NInt lokke;
	Place p2;

	if(range==NULL) return;

	PlaceCopy(&p2,place);
	PlaceAdd(&p2,&range->length);
	CutRange(block,tracknum,tracknum+range->num_tracks-1,place,&p2);

	track=ListFindElement1(&block->tracks->l,tracknum);
	if(track==NULL) return;

	for(lokke=0;lokke<range->num_tracks;lokke++){
		PasteRange_notes(block,track,range->notes[lokke],place);
		PasteRange_stops(block,track,range->stops[lokke],place);
		track=NextTrack(track);
		if(track==NULL) break;
	}

}

void PasteRange_CurrPos(
	struct Tracker_Windows *window
){

	struct WBlocks *wblock=window->wblock;
	struct Blocks *block=wblock->block;
	NInt curr_track=window->curr_track;
	struct LocalZooms **realline=wblock->reallines;
	int curr_realline=wblock->curr_realline;

	if(curr_track<0 || range==NULL) return;

	PlayStop();

	Undo_Range(
		window,
		block,
		curr_track,
		curr_track+range->num_tracks-1,
		wblock->curr_realline
	);

	PasteRange(block,curr_track,&realline[curr_realline]->l.p);

	UpdateAndClearSomeTrackReallinesAndGfxWTracks(
		window,
		window->wblock,
		curr_track,
		curr_track+range->num_tracks-1
	);

}


