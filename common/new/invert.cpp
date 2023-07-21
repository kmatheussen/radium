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


static void Invert_notes2(
                          struct Tracks *track,
                          const Ratio r1,
                          const Ratio r2
){
  bool firsttime = true;

  int last_org_note = 0; // Set to a value to silence wrong warning about uninitialized value in older versions of gcc.
  int last_inverted_note = 0; // Same here.
  
  r::NoteTimeData::Writer writer(track->_notes2, r::KeepOldData::KEEP_OLD_DATA);
  
  for(r::NotePtr &note : writer){
    const Ratio time = note->get_time();
    
    if (time >= r2)
      break;
    
    const int noteval = note->get_val();
    
    if (time >= r1) {
      
      if (firsttime) {
        
        firsttime = false;
        last_inverted_note = noteval;
        
      } else {
        
        r::ModifyNote new_note(note);
        
        const int delta = noteval - last_org_note;
        
        last_inverted_note = R_BOUNDARIES(1, last_inverted_note - delta, 127);

        //printf("Delta: %d. Old: %d. New: %d\n", delta, noteval, last_inverted_note);
        
        new_note->_val = last_inverted_note;
        
      }
    }
    
    last_org_note = noteval;                      
  }
}


static void InvertRange(
	struct WBlocks *wblock
){
	struct Tracks *track;
	int lokke;

	if( ! wblock->range.enabled) return;

	const Place *p1=&wblock->range.y1;
	const Place *p2=&wblock->range.y2;

	track = (struct Tracks*)ListFindElement1(&wblock->block->tracks->l,wblock->range.x1);

	for(lokke=0;lokke<=wblock->range.x2-wblock->range.x1;lokke++){
                Invert_notes(track->notes,p1,p2,true,0,0);
                Invert_notes2(track, place2ratio(*p1), place2ratio(*p2));
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

        Invert_notes2(track, make_ratio(0,1), make_ratio(block->num_lines, 1));
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
	if(!window->wblock->range.enabled) return;


	ADD_UNDO(Range(
                       window,
                       window->wblock,
                       window->wblock->range.x1,
                       window->wblock->range.x2+1,
                       window->wblock->curr_realline
                       ));

	InvertRange(window->wblock);

	UpdateAndClearSomeTrackReallinesAndGfxWTracks(
		window,
		window->wblock,
		window->wblock->range.x1,
		window->wblock->range.x2
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









