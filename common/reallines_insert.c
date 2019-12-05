/* Copyright 1999/2000-2013 Kjetil S. Matheussen

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


#include <math.h>


#include "nsmtracker.h"
#include "placement_proc.h"
#include "list_proc.h"
#include "vector_proc.h"
#include "notes_legalize_proc.h"
#include "fxlines_proc.h"
#include "fxlines_legalize_proc.h"
#include "temponodes_legalize_proc.h"
#include "temponodes_proc.h"
#include "swing_proc.h"
#include "Signature_proc.h"
#include "LPB_proc.h"
#include "tempos_proc.h"
#include "reallines_insert_proc.h"
#include "time_proc.h"
#include "undo_tempos_proc.h"
#include "undo_temponodes_proc.h"
#include "undo_lpbs_proc.h"
#include "undo_swings_proc.h"
#include "undo_signatures_proc.h"
#include "undo_notes_proc.h"
#include "undo_notesandfxs_proc.h"
#include "player_proc.h"
#include "player_pause_proc.h"
#include "Beats_proc.h"

#include "reallines_insert_proc.h"

static void InsertPlace_notes_extra(
	struct Blocks *block,
	void *tonote,
	struct ListHeader3 *l,
	float place,
	float toplace
){
	struct Notes *note=(struct Notes *)l;
        float endplace = GetfloatFromPlacement(&note->end);
        
	if(endplace >= place){
          if (endplace+toplace > 0){
            PlaceAddfloat(&note->end,toplace);
            List_InsertPlaceLen3(block,&note->velocities,(struct ListHeader3*)note->velocities,place,toplace,NULL);
            List_InsertPlaceLen3(block,&note->pitches,(struct ListHeader3*)note->pitches,place,toplace,NULL);
          }
	}
}


static void InsertPlace_notes(
	struct Blocks *block,
	struct Tracks *track,
	float place,
	float toplace
){
	List_InsertPlaceLen3(
		block,
		&track->notes,
		(struct ListHeader3*)track->notes,
		place,
		toplace,
		InsertPlace_notes_extra
	);
	LegalizeNotes(block,track);
}

static void InsertPlace_fxs(
	struct Blocks *block,
	struct Tracks *track,
	float place,
	float toplace
){

  VECTOR_FOR_EACH(struct FXs *fxs, &track->fxs){
    List_InsertPlaceLen3(
                         block,
                         &fxs->fxnodelines,
                         (struct ListHeader3*)fxs->fxnodelines,
                         place,
                         toplace,
                         NULL
                         );
  }END_VECTOR_FOR_EACH;
  
  LegalizeFXlines(block,track);
}

static void InsertPlace_temponodes(
	struct Blocks *block,
	float place,
	float toplace
){
	List_InsertPlaceLen3(block,&block->temponodes,&block->temponodes->l,place,toplace,NULL);
	if(toplace>0.0f){
		ListAddElement3_a(&block->temponodes,&block->lasttemponode->l);
	}
	LegalizeTempoNodes(block);
}

static void InsertPlace_tempos(
	struct Blocks *block,
	float place,
	float toplace
){
	List_InsertPlaceLen3(block,&block->tempos,(struct ListHeader3*)block->tempos,place,toplace,NULL);
}

static void InsertPlace_lpbs(
	struct Blocks *block,
	float place,
	float toplace
){
	List_InsertPlaceLen3(block,&block->lpbs,(struct ListHeader3*)block->lpbs,place,toplace,NULL);
}

static void InsertPlace_swings(
	struct Blocks *block,
	float place,
	float toplace
){
	List_InsertPlaceLen3(block,&block->swings,(struct ListHeader3*)block->swings,place,toplace,NULL);
}

static void InsertPlace_signatures(
	struct Blocks *block,
	float place,
	float toplace
){
  List_InsertPlaceLen3(block,&block->signatures,(struct ListHeader3*)block->signatures,place,toplace,NULL);
}

static void InsertPlace_stops(
	struct Blocks *block,
	struct Tracks *track,
	float place,
	float toplace
){
	List_InsertPlaceLen3(block,&track->stops,(struct ListHeader3*)track->stops,place,toplace,NULL);
}


void InsertRealLines_CurrPos(
	struct Tracker_Windows *window,
	int num_reallines
){
	float place;
	float toplace;
	struct WBlocks *wblock=window->wblock;
	struct Blocks *block=wblock->block;
	const struct LocalZooms **reallines=wblock->reallines;
	int curr_realline=wblock->curr_realline;
	const struct LocalZooms *realline=reallines[curr_realline];

	if(num_reallines==0) return;

	place=GetfloatFromPlace(&realline->l.p);

	if(curr_realline==wblock->num_reallines-1){
		toplace=floor(place+1.0f);
	}else{
		toplace=GetfloatFromPlace(&reallines[curr_realline+1]->l.p);
	}

	toplace-=place;

	toplace*=num_reallines;

	if(toplace==0.0f) return;	//extra check.


        PC_Pause();
          
	switch(window->curr_track){
		case SWINGTRACK:
                  ADD_UNDO(Swings_CurrPos(window));
			InsertPlace_swings(block,place,toplace);
                        TIME_block_swings_have_changed(block);
			break;
		case SIGNATURETRACK:
                  ADD_UNDO(Signatures_CurrPos(window));
			InsertPlace_signatures(block,place,toplace);
                        TIME_block_signatures_have_changed(block);
			break;
		case LPBTRACK:
                  ADD_UNDO(LPBs_CurrPos(window));
			InsertPlace_lpbs(block,place,toplace);
			//UpdateWLPBs(window,wblock);
#if !USE_OPENGL
			DrawUpLPBs(window,wblock);
#endif
                        TIME_block_LPBs_have_changed(block);
			break;
		case TEMPOTRACK:
                  ADD_UNDO(Tempos_CurrPos(window));
			InsertPlace_tempos(block,place,toplace);
			//UpdateWTempos(window,wblock);
#if !USE_OPENGL
			DrawUpTempos(window,wblock);
#endif
                        TIME_block_tempos_have_changed(block);
			break;
		case TEMPONODETRACK:
                  ADD_UNDO(TempoNodes_CurrPos(window));
			InsertPlace_temponodes(block,place,toplace);
#if !USE_OPENGL
			UpdateWTempoNodes(window,wblock);
			DrawUpWTempoNodes(window,wblock);
#endif
                        TIME_block_tempos_have_changed(block);
			break;
		default:
			if(window->curr_track_sub>=0){
                          ADD_UNDO(NotesAndFXs_CurrPos(window));
                          InsertPlace_fxs(block,wblock->wtrack->track,place,toplace);
#if !USE_OPENGL
                          UpdateFXNodeLines(window,wblock,wblock->wtrack);
#endif
			}else{
                          ADD_UNDO(Notes_CurrPos(window));
			}
			InsertPlace_notes(block,wblock->wtrack->track,place,toplace);
			InsertPlace_stops(block,wblock->wtrack->track,place,toplace);
#if !USE_OPENGL
			ClearTrack(window,wblock,wblock->wtrack,wblock->top_realline,wblock->bot_realline);
			UpdateWTrack(window,wblock,wblock->wtrack,wblock->top_realline,wblock->bot_realline);
#endif
			break;
	}
        
        PC_StopPause(window);	

}







