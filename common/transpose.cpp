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
#include "TimeData.hpp"
#include "placement_proc.h"
#include "clipboard_range_calc_proc.h"
#include "list_proc.h"
#include "wtracks_proc.h"
#include "undo_notes_proc.h"
#include "undo_range_proc.h"
#include "player_proc.h"
#include "player_pause_proc.h"
#include "notes_proc.h"

#include "transpose_proc.h"


static float getTransposed(float val, int trans){
  if(val+trans<128 && val+trans>0)
    return val + trans;
  else
    return val;  
}

static void Transpose_note(
	struct Notes *note,
	int trans
){
  note->note = getTransposed(note->note, trans);
  if (note->pitch_end > 0)
    note->pitch_end = getTransposed(note->pitch_end, trans);

  /*
  struct Pitches *pitch = note->pitches;
  while(pitch!=NULL){
    pitch->note = getTransposed(pitch->note, trans);
    pitch = NextPitch(pitch);
  }
  */
  
  r::PitchTimeData::Writer writer(note->_pitches);
  for(auto &pitch : writer)
    pitch._val = getTransposed(pitch._val, trans);
}

static bool Transpose_notes(
                     struct Notes *note,
                     const Place *p1,
                     const Place *p2,
                     int trans,
                     bool only_selected_notes
){
  if(note==NULL) return false;

  bool ret = false;
  
        if (!only_selected_notes) {
          
          if(PlaceGreaterOrEqual(&note->l.p,p1)){

		if(PlaceGreaterOrEqual(&note->l.p,p2)) return false;
                
                Transpose_note(note,trans);

                ret = true;
          }
          
        } else {

          if (note->pianonote_is_selected) {
            Transpose_note(note,trans);
            ret = true;
          }

        }


	if (Transpose_notes(NextNote(note),p1,p2,trans, only_selected_notes))
          ret = true;

        return ret;
}


static bool TransposeRange(
	struct WBlocks *wblock,
	int trans
){
	int lokke;

	const Place *p1=&wblock->range.y1;
	const Place *p2=&wblock->range.y2;

        if(!wblock->range.enabled){
          return Transpose_notes(wblock->wtrack->track->notes,p1,p2,trans,true);
        }


        bool ret = false;
        
	struct Tracks *track = (struct Tracks *)ListFindElement1(&wblock->block->tracks->l,wblock->range.x1);

	for(lokke=0;lokke<=wblock->range.x2-wblock->range.x1;lokke++){
          if (Transpose_notes(track->notes,p1,p2,trans, false))
            ret = true;
          track=NextTrack(track);
	}

        return ret;

}

static void TransposeTrack(
	struct Tracks *track,
	int trans
){
	struct Notes *note=track->notes;

	while(note!=NULL){
          Transpose_note(note, trans);
          note=NextNote(note);
	}

}

static void TransposeBlock(
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
  //if(!window->wblock->range.enabled) return;

  if (window->wblock->range.enabled)    
    ADD_UNDO(Range(window,window->wblock,window->wblock->range.x1,window->wblock->range.x2+1,window->wblock->curr_realline));
  else
    ADD_UNDO(Notes_CurrPos(window));
  
	if (TransposeRange(window->wblock,trans)==false)
          UNDO_CANCEL_LAST_UNDO();


	UpdateAndClearSomeTrackReallinesAndGfxWTracks(
		window,
		window->wblock,
		window->wblock->range.x1,
		window->wblock->range.x2
	);
}

void TransposeTrack_CurrPos(
	struct Tracker_Windows *window,
	int trans
){


  ADD_UNDO(Notes_CurrPos(window));

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
  vector_t notes = FindAllNotesCurrPos(window);
  if (notes.num_elements==0)
    return;
  
  ADD_UNDO(Notes_CurrPos(window));

  VECTOR_FOR_EACH(struct Notes *, note, &notes){
    Transpose_note(note,trans);
  }END_VECTOR_FOR_EACH;
  
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

	ADD_UNDO(Range(
                       window,
                       window->wblock,
                       0,window->wblock->block->num_tracks,
                       window->wblock->curr_realline
                       ));
        
	TransposeBlock(window->wblock->block,trans);

	UpdateAndClearSomeTrackReallinesAndGfxWTracks(
		window,
		window->wblock,
		0,
		window->wblock->block->num_tracks-1
	);

}
