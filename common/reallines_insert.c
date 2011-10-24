

#include "nsmtracker.h"
#include <math.h>
#include "placement_proc.h"
#include "list_proc.h"
#include "notes_legalize_proc.h"
#include "fxlines_proc.h"
#include "fxlines_legalize_proc.h"
#include "temponodes_legalize_proc.h"
#include "gfx_wblocks_proc.h"
#include "gfx_wtracks_proc.h"
#include "temponodes_proc.h"
#include "LPB_proc.h"
#include "tempos_proc.h"
#include "trackreallines_proc.h"
#include "reallines_insert_proc.h"
#include "time_proc.h"
#include "undo_tempos_proc.h"
#include "undo_temponodes_proc.h"
#include "undo_lpbs_proc.h"
#include "undo_notes_proc.h"
#include "undo_notesandfxs_proc.h"
#include "player_proc.h"

#include "reallines_insert_proc.h"

void InsertPlace_notes_extra(
	struct Blocks *block,
	void *tonote,
	struct ListHeader3 *l,
	float place,
	float toplace
){
	struct Notes *note=(struct Notes *)l;

	if(GetfloatFromPlacement(&note->end)>=place){
		PlaceAddfloat(&note->end,toplace);
		List_InsertPlaceLen3(block,&note->velocities,&note->velocities->l,place,toplace,NULL);
	}
}


void InsertPlace_notes(
	struct Blocks *block,
	struct Tracks *track,
	float place,
	float toplace
){
	List_InsertPlaceLen3(
		block,
		&track->notes,
		&track->notes->l,
		place,
		toplace,
		InsertPlace_notes_extra
	);
	LegalizeNotes(block,track);
}

void InsertPlace_fxs(
	struct Blocks *block,
	struct Tracks *track,
	float place,
	float toplace
){
	struct FXs *fxs=track->fxs;

	while(fxs!=NULL){
		List_InsertPlaceLen3(
			block,
			&fxs->fxnodelines,
			&fxs->fxnodelines->l,
			place,
			toplace,
			NULL
		);
		fxs=NextFX(fxs);
	}
	LegalizeFXlines(block,track);
}

void InsertPlace_temponodes(
	struct Blocks *block,
	float place,
	float toplace
){
	List_InsertPlaceLen3(block,&block->temponodes,&block->temponodes->l,place,toplace,NULL);
	if(toplace>0.0f){
		ListAddElement3(&block->temponodes,&block->lasttemponode->l);
	}
	LegalizeTempoNodes(block);
}

void InsertPlace_tempos(
	struct Blocks *block,
	float place,
	float toplace
){
	List_InsertPlaceLen3(block,&block->tempos,&block->tempos->l,place,toplace,NULL);
}

void InsertPlace_lpbs(
	struct Blocks *block,
	float place,
	float toplace
){
	List_InsertPlaceLen3(block,&block->lpbs,&block->lpbs->l,place,toplace,NULL);
}

void InsertPlace_stops(
	struct Blocks *block,
	struct Tracks *track,
	float place,
	float toplace
){
	List_InsertPlaceLen3(block,&track->stops,&track->stops->l,place,toplace,NULL);
}


void InsertRealLines_CurrPos(
	struct Tracker_Windows *window,
	int num_reallines
){
	float place;
	float toplace;
	struct WBlocks *wblock=window->wblock;
	struct Blocks *block=wblock->block;
	struct LocalZooms **reallines=wblock->reallines;
	int curr_realline=wblock->curr_realline;
	struct LocalZooms *realline=reallines[curr_realline];

	if(num_reallines==0) return;

	PlayStop();

	place=GetfloatFromPlace(&realline->l.p);

	if(curr_realline==wblock->num_reallines-1){
		toplace=floor(place+1.0f);
	}else{
		toplace=GetfloatFromPlace(&reallines[curr_realline+1]->l.p);
	}

	toplace-=place;

	toplace*=num_reallines;

	if(toplace==0.0f) return;	//extra check.


	switch(window->curr_track){
		case TEMPOTRACK:
			Undo_Tempos_CurrPos(window);
			InsertPlace_tempos(block,place,toplace);
			UpdateWTempos(window,wblock);
			DrawUpTempos(window,wblock);
			UpdateSTimes(wblock->block);
			break;
		case LPBTRACK:
			Undo_LPBs_CurrPos(window);
			InsertPlace_lpbs(block,place,toplace);
			UpdateWLPBs(window,wblock);
			DrawUpLPBs(window,wblock);
			UpdateSTimes(wblock->block);
			break;
		case TEMPONODETRACK:
			Undo_TempoNodes_CurrPos(window);
			InsertPlace_temponodes(block,place,toplace);
			UpdateWTempoNodes(window,wblock);
			DrawUpWTempoNodes(window,wblock);
			UpdateSTimes(wblock->block);
			break;
		default:
			if(window->curr_track_sub>=0){
				Undo_NotesAndFXs_CurrPos(window);
				InsertPlace_fxs(block,wblock->wtrack->track,place,toplace);
				UpdateFXNodeLines(window,wblock,wblock->wtrack);
			}else{
				Undo_Notes_CurrPos(window);
			}
			InsertPlace_notes(block,wblock->wtrack->track,place,toplace);
			InsertPlace_stops(block,wblock->wtrack->track,place,toplace);
			UpdateTrackReallines(window,wblock,wblock->wtrack);
			ClearTrack(window,wblock,wblock->wtrack,wblock->top_realline,wblock->bot_realline);
			UpdateWTrack(window,wblock,wblock->wtrack,wblock->top_realline,wblock->bot_realline);

			break;
	}

	

}







