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
#include "placement_proc.h"
#include "clipboard_range_copy_proc.h"
#include "list_proc.h"
#include "fxlines_proc.h"
#include <string.h>
#include "trackreallines_proc.h"

#include "undo_notesandfxs_proc.h"


struct Undo_NotesAndFXs{
	struct Notes *notes;
	struct Stops *stops;
	struct FXs *fxs;
	void *instrumentdata;
};


void *Undo_Do_NotesAndFXs(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);

void Undo_NotesAndFXs(
	struct Tracker_Windows *window,
	struct Blocks *block,
	struct Tracks *track,
	int realline
){
	Place *p1=PlaceGetFirstPos();
	Place p2;
	struct Undo_NotesAndFXs *undo_notesandfxs=talloc(sizeof(struct Undo_NotesAndFXs));

	PlaceSetLastPos(block,&p2);

	CopyRange_stops(&undo_notesandfxs->stops,track->stops,p1,&p2);
	CopyRange_notes(&undo_notesandfxs->notes,track->notes,p1,&p2);
	CopyRange_fxs(&undo_notesandfxs->fxs,track->fxs,p1,&p2);
	if(track->instrumentdata!=NULL){
		undo_notesandfxs->instrumentdata=(*track->instrument->CopyInstrumentData)(track);
	}

	Undo_New(window->l.num,block->l.num,track->l.num,realline,undo_notesandfxs,Undo_Do_NotesAndFXs);

}

void *Undo_Do_NotesAndFXs(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
	struct Undo_NotesAndFXs *undo_notesandfxs=(struct Undo_NotesAndFXs *)pointer;
	struct Notes *ntemp;
	struct Stops *stemp;

	struct FXs *temp;
	void *instrumentdata;

	struct Tracks *track=wtrack->track;

	ntemp=track->notes;
	stemp=track->stops;
	temp=track->fxs;
	instrumentdata=track->instrumentdata;

	track->stops=undo_notesandfxs->stops;
	track->notes=undo_notesandfxs->notes;
	track->fxs=undo_notesandfxs->fxs;
	track->instrumentdata=undo_notesandfxs->instrumentdata;

	undo_notesandfxs->stops=stemp;
	undo_notesandfxs->notes=ntemp;
	undo_notesandfxs->fxs=temp;
	undo_notesandfxs->instrumentdata=instrumentdata;

	UpdateTrackReallines(window,wblock,wtrack);
	UpdateFXNodeLines(window,wblock,wtrack);

	return undo_notesandfxs;
}


void Undo_NotesAndFXs_CurrPos(
	struct Tracker_Windows *window
){
	Undo_NotesAndFXs(window,window->wblock->block,window->wblock->wtrack->track,window->wblock->curr_realline);
}


