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
#include "blts_proc.h"
#include "notes_proc.h"

#include "transpose_proc.h"


static float getTransposed(float val, int trans){
  if(val+trans<128 && val+trans>0)
    return val + trans;
  else
    return val;  
}

void Transpose_note(
	struct Notes *note,
	int trans
){
  note->note = getTransposed(note->note, trans);
  struct Pitches *pitch = note->pitches;
  while(pitch!=NULL){
    pitch->note = getTransposed(pitch->note, trans);
    pitch = NextPitch(pitch);
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
          Transpose_note(note, trans);
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
        struct Notes *note = FindNoteCurrPos(window);
        if (note==NULL)
          return;
        
	Undo_Notes_CurrPos(window);

	Transpose_note(note,trans);

	UpdateAndClearSomeTrackReallinesAndGfxWTracks(
		window,
		window->wblock,
		window->wblock->wtrack->l.num,
		window->wblock->wtrack->l.num
	);
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









