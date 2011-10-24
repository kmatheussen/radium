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
#include "clipboard_range_copy_proc.h"
#include "fxlines_proc.h"
#include "placement_proc.h"
#include <string.h>

#include "undo_fxs_proc.h"

struct Undo_FXs{
	struct FXs *fxs;
	void *instrumentdata;
};

void *Undo_Do_FXs(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);

void Undo_FXs(
	struct Tracker_Windows *window,
	struct Blocks *block,
	struct Tracks *track,
	int realline
){

	Place *p1=PlaceGetFirstPos();
	Place p2;
	struct Undo_FXs *undo_fxs=talloc(sizeof(struct Undo_FXs));

	PlaceSetLastPos(block,&p2);

	CopyRange_fxs(&undo_fxs->fxs,track->fxs,p1,&p2);
	if(track->instrumentdata!=NULL){
		undo_fxs->instrumentdata=(*track->instrument->CopyInstrumentData)(track);
	}

	Undo_New(
		window->l.num,
		block->l.num,
		track->l.num,
		realline,
		undo_fxs,
		Undo_Do_FXs
	);

}

void Undo_FXs_CurrPos(
	struct Tracker_Windows *window
){
	Undo_FXs(window,window->wblock->block,window->wblock->wtrack->track,window->wblock->curr_realline);
}

void *Undo_Do_FXs(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
	struct Undo_FXs *undo_fxs=(struct Undo_FXs *)pointer;
	struct FXs *temp;
	void *instrumentdata;

	struct Tracks *track=wtrack->track;

	temp=track->fxs;
	instrumentdata=track->instrumentdata;

	track->fxs=undo_fxs->fxs;
	track->instrumentdata=undo_fxs->instrumentdata;

	UpdateFXNodeLines(window,wblock,wtrack);

	undo_fxs->fxs=temp;
	undo_fxs->instrumentdata=instrumentdata;

	return undo_fxs;

}




