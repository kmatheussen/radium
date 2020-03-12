

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
#include "../clipboard_range_copy_proc.h"
#include "../clipboard_range_cut_proc.h"
#include "../notes_proc.h"
#include "../list_proc.h"
#include "../wtracks_proc.h"
#include "../undo_notes_proc.h"
#include "../undo_range_proc.h"
#include "../realline_calc_proc.h"
#include "../notes_legalize_proc.h"
#include "../player_proc.h"
#include "../player_pause_proc.h"

#include "backwards_proc.h"



static void BackWards_notes(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	const Place *p1,
	const Place *p2
){
	float f_p1=GetfloatFromPlace(p1);
	float f_p2=GetfloatFromPlace(p2);
	float f,newfloat,addfloat;
	Place newplace;
	int realline;

	struct Notes *notes=NULL;

	if(wtrack->track->notes==NULL) return;

	realline=FindRealLineFor(wblock,0,p1);
	if(realline==wblock->num_reallines-1) return;

	addfloat=GetfloatFromPlace(&wblock->reallines[realline+1]->l.p);
	addfloat-=GetfloatFromPlace(&wblock->reallines[realline]->l.p);

	CopyRange_notes(&notes,wtrack->track->notes,p1,p2);
	CutRange_notes(&wtrack->track->notes,wtrack->track->notes,p1,p2);

	while(notes!=NULL){
		f=GetfloatFromPlace(&notes->l.p);
		newfloat=R_MAX(f_p1,f_p2-f-addfloat);
		Float2Placement(newfloat,&newplace);

		InsertNote(wblock,wtrack,&newplace,NULL,notes->note,notes->velocity,false);

		notes=NextNote(notes);
	}
	LegalizeNotes(wblock->block,wtrack->track);
}

static void BackWardsTrack(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack
){
	Place p1,p2;

	PlaceSetFirstPos(&p1);
	PlaceSetLastPos(wblock->block,&p2);

	BackWards_notes(window,wblock,wtrack,&p1,&p2);
}


static void BackWardsRange(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){
	struct WTracks *wtrack;
	int lokke;

	if( ! wblock->isranged) return;

	const Place *p1=&wblock->rangey1;
	const Place *p2=&wblock->rangey2;

	wtrack=ListFindElement1(&wblock->wtracks->l,wblock->rangex1);

	for(lokke=0;lokke<=wblock->rangex2-wblock->rangex1;lokke++){
		BackWards_notes(window,wblock,wtrack,p1,p2);
		wtrack=NextWTrack(wtrack);
	}

}

static void BackWardsBlock(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){
	struct WTracks *wtrack=wblock->wtracks;

	while(wtrack!=NULL){
		BackWardsTrack(window,wblock,wtrack);
		wtrack=NextWTrack(wtrack);
	}
}


void BackWardsRange_CurrPos(
	struct Tracker_Windows *window
){
	if(!window->wblock->isranged) return;

	ADD_UNDO(Range(
		window,
		window->wblock,
		0,window->wblock->block->num_tracks-1,
		window->wblock->curr_realline
                       ));

        PC_Pause();{
          BackWardsRange(window,window->wblock);
        }PC_StopPause(window);
        
	UpdateAndClearSomeTrackReallinesAndGfxWTracks(
		window,
		window->wblock,
		window->wblock->rangex1,
		window->wblock->rangex2
	);

}

void BackWardsTrack_CurrPos(
	struct Tracker_Windows *window
){

        PC_Pause();{
          ADD_UNDO(Notes_CurrPos(window));

          BackWardsTrack(window,window->wblock,window->wblock->wtrack);
        }PC_StopPause(window);
        
	UpdateAndClearSomeTrackReallinesAndGfxWTracks(
		window,
		window->wblock,
		window->wblock->wtrack->l.num,
		window->wblock->wtrack->l.num
	);

}


void BackWardsBlock_CurrPos(
	struct Tracker_Windows *window
){

        PC_Pause();{
          
          ADD_UNDO(Range(
                         window,
                         window->wblock,
                         0,window->wblock->block->num_tracks-1,
                         window->wblock->curr_realline
                         ));
          
          BackWardsBlock(window,window->wblock);
          
        }PC_StopPause(window);
        
	UpdateAndClearSomeTrackReallinesAndGfxWTracks(
		window,
		window->wblock,
		0,
		window->wblock->block->num_tracks-1
	);

}
