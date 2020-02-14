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
#include "undo.h"
#include "undo_blocks_proc.h"
#include "clipboard_range.h"
#include "placement_proc.h"
#include "list_proc.h"
#include "clipboard_range_copy_proc.h"

#include "undo_range_proc.h"


void ADD_UNDO_FUNC(Range(
                         struct Tracker_Windows *window,
                         struct WBlocks *wblock,
                         NInt starttrack,
                         NInt endtrack,
                         int realline
                         )
                   )
{
  CALL_ADD_UNDO_FUNC(Block(window,
                           wblock,
                           wblock->wtrack,
                           wblock->curr_realline
                           ));
}

/*
// too flaky

void *Undo_Do_Range(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);


void Undo_Range(
	struct Tracker_Windows *window,
	struct Blocks *block,
	NInt starttrack,
	NInt endtrack,
	int realline
){
	const Place *p1=PlaceGetFirstPos();
	Place p2;
	struct Range *undo_range;
	NInt num_tracks;
	NInt lokke;
	struct Tracks *track;
	NInt num_tracks_in_block=block->num_tracks;

	if(endtrack>=num_tracks_in_block){
		num_tracks=num_tracks_in_block-1;
	}else{
		num_tracks=endtrack-starttrack+1;
	}

	undo_range=talloc(sizeof(struct Range));
	undo_range->notes=talloc((size_t)(sizeof(struct Notes *)*num_tracks));
	undo_range->stops=talloc((size_t)(sizeof(struct Stops *)*num_tracks));
	undo_range->num_tracks=num_tracks;

	PlaceSetLastPos(block,&p2);

	for(lokke=0;lokke<num_tracks;lokke++){
		track=ListFindElement1(&block->tracks->l,lokke+starttrack);
		CopyRange_stops(&undo_range->stops[lokke],track->stops,p1,&p2);
		CopyRange_notes(&undo_range->notes[lokke],track->notes,p1,&p2);
	}

	Undo_Add(window->l.num,block->l.num,starttrack,realline,
                 undo_range,Undo_Do_Range,
                 "Range",
                 LOC()
                 );
}

void *Undo_Do_Range(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
	struct Range *undo_range=(struct Range *)pointer;
	struct WTracks *mywtrack;
	struct Tracks *track;
	struct Notes *notetemp;
	struct Stops *stoptemp;
	NInt num_tracks=undo_range->num_tracks;
	NInt lokke;

	for(lokke=0;lokke<num_tracks;lokke++){
		mywtrack=ListFindElement1(&wblock->wtracks->l,lokke+wtrack->l.num);
		track=mywtrack->track;
		notetemp=track->notes;
		stoptemp=track->stops;
		track->notes=undo_range->notes[lokke];
		track->stops=undo_range->stops[lokke];
		undo_range->notes[lokke]=notetemp;
		undo_range->stops[lokke]=stoptemp;
	}

	return undo_range;
}
*/
