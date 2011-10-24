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
#include "undo_tracks_proc.h"
#include "undo_range_proc.h"
#include "wtracks_proc.h"
#include "notes_legalize_proc.h"
#include "clipboard_range_copy_proc.h"
#include "player_proc.h"

#include "quantitize_proc.h"


extern struct Root *root;


/******************************************************************
  FUNCTION
    Quantitize 'toquant' spesified by 'quant'. 'Block' is used
    to know the last zposition in the block, so that 'toquant' isn't
    placed after that.

  EXAMPLES
    toquant=1,2,3 quant=0.5 -> toquant=1,1,2
    toquant=1,6,7 quant=0.5 -> toquant=2,0,1
    toquant=5,0,1 quant=2.0 -> toquant=6,0,1
******************************************************************/

void Quantitize(
	struct Blocks *block,
	Place *toquant,
	float quant
){
	float toquantfloat;

	int upto;

	float diff;
	float out;

	if(quant==0.0 || quant/2<0.00002) return;

	toquantfloat=GetfloatFromPlace(toquant);

	upto=(int)(toquantfloat/quant);
	upto*=quant;

	diff=toquantfloat-upto;
	out=upto;

	while(diff>=quant/2){
		out+=quant;
		diff-=quant;
	}

	Float2Placement(out,toquant);

	if( ! PlaceLegal(block,toquant)){
		PlaceSetLastPos(block,toquant);
	}
}


/****************************************************************
  NOTE
    Could work better.
****************************************************************/
void Quantitize_Note(
	struct Blocks *block,
	struct Notes **notes,
	struct Notes *note,
	float quant
){
	Place tempstart,tempend;
	Place end;

	PlaceSetLastPos(block,&end);
	PlaceCopy(&tempstart,&note->l.p);

	if(PlaceEqual(&end,&note->l.p)){
		return;
	}

	Quantitize(block,&note->l.p,quant);

	if(PlaceEqual(&end,&note->l.p)){
		PlaceCopy(&note->l.p,&tempstart);
	}

	PlaceCopy(&tempend,&note->end);

	Quantitize(block,&note->end,quant);

	if(PlaceLessOrEqual(&note->end,&note->l.p)){
		PlaceCopy(&note->end,&tempend);
	}

	if(PlaceLessOrEqual(&note->end,&note->l.p)){
		PlaceCopy(&note->l.p,&tempstart);
		PlaceCopy(&note->end,&tempend);
	}

/*
	while(PlaceLessOrEqual(&note->end,&note->l.p)){
		note->end.counter+=quant*note->end.dividor;
		PlaceHandleOverflow(&note->end);		//Haven't checked this too good. A small potensial for the program to hang.
	}
*/

	if( ! PlaceLegal(block,&note->end)){
		PlaceSetLastPos(block,&note->end);
	}

	ListAddElement3(notes,&note->l);
}

void Quantitize_range(
	struct WBlocks *wblock,
	float quant
){
	struct Tracks *track;
	struct Notes *notes=NULL;
	struct Notes *note=NULL;
	struct Notes *temp;
	struct LocalZooms *realline1,*realline2;
	int lokke;
	Place first,last;

	PlaceSetFirstPos(&first);
	PlaceSetLastPos(wblock->block,&last);

	if( ! wblock->isranged) return;

	track=ListFindElement1(&wblock->block->tracks->l,wblock->rangex1);

	realline1=wblock->reallines[wblock->rangey1];
	realline2=wblock->reallines[wblock->rangey2];

	for(lokke=0;lokke<=wblock->rangex2-wblock->rangex1;lokke++){
		note=NULL;
		CopyRange_notes(&note,track->notes,&first,&last);
		for(;;){
			if(note==NULL) break;
			if(PlaceGreaterThan(&note->l.p,&realline2->l.p)) break;
			temp=NextNote(note);
			if(PlaceGreaterOrEqual(&note->l.p,&realline1->l.p)){
				Quantitize_Note(wblock->block,&notes,note,quant);
			}
			note=temp;
		}
		track->notes=notes;
		notes=NULL;
		track=NextTrack(track);

		LegalizeNotes(wblock->block,track);
	}

}

void Quantitize_track(
	struct Blocks *block,
	struct Tracks *track,
	float quant
){
	struct Notes *note=NULL;
	struct Notes *notes=NULL;
	struct Notes *temp;
	Place first,last;

	PlaceSetFirstPos(&first);
	PlaceSetLastPos(block,&last);

	CopyRange_notes(&note,track->notes,&first,&last);

	while(note!=NULL){
		temp=NextNote(note);
		Quantitize_Note(block,&notes,note,quant);
		note=temp;
	}
	track->notes=notes;

	LegalizeNotes(block,track);
}


void Quantitize_block(
	struct Blocks *block,
	float quant
){
	struct Tracks *track=block->tracks;

	while(track!=NULL){
		Quantitize_track(block,track,quant);
		track=NextTrack(track);
	}
}

	
void Quantitize_track_CurrPos(
	struct Tracker_Windows *window
){
	struct WBlocks *wblock=window->wblock;

	PlayStop();

	Undo_Track_CurrPos(window);
	Quantitize_track(wblock->block,wblock->wtrack->track,root->quantitize);

	UpdateAndClearSomeTrackReallinesAndGfxWTracks(
		window,
		wblock,
		window->curr_track,
		window->curr_track
	);
/*
	UpdateAndClearSomeTrackReallinesAndGfxWTracks(
		window,
		window->wblock,
		0,
		window->wblock->block->num_tracks-1
	);
*/
}

void Quantitize_block_CurrPos(
	struct Tracker_Windows *window
){

	PlayStop();

	Undo_Range(
		window,
		window->wblock->block,
		0,window->wblock->block->num_tracks-1,
		window->wblock->curr_realline
	);

	Quantitize_block(window->wblock->block,root->quantitize);

	UpdateAndClearSomeTrackReallinesAndGfxWTracks(
		window,
		window->wblock,
		0,
		window->wblock->block->num_tracks-1
	);
}

void Quantitize_range_CurrPos(
	struct Tracker_Windows *window
){
	if(!window->wblock->isranged) return;

	PlayStop();

	Undo_Range(window,window->wblock->block,window->wblock->rangex1,window->wblock->rangex2,window->wblock->curr_realline);

	Quantitize_range(window->wblock,root->quantitize);

	UpdateAndClearSomeTrackReallinesAndGfxWTracks(
		window,
		window->wblock,
		window->wblock->rangex1,
		window->wblock->rangex2
	);
}

