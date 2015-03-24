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

#include "clipboard_range_copy_proc.h"
#include "clipboard_range_calc_proc.h"
#include "wtracks_proc.h"
#include <string.h>
#include "placement_proc.h"
#include "fxlines_proc.h"
#include "windows_proc.h"
#include "clipboard_tempos_copy_proc.h"
#include "list_proc.h"
#include "time_proc.h"
#include "Signature_proc.h"
#include "LPB_proc.h"
#include "temponodes_proc.h"
#include "temponodes_legalize_proc.h"
#include "tempos_proc.h"
#include "gfx_wblocks_proc.h"
#include "undo_tracks_proc.h"
#include "undo_signatures_proc.h"
#include "undo_lpbs_proc.h"
#include "undo_tempos_proc.h"
#include "undo_temponodes_proc.h"
#include "player_proc.h"
#include "fxlines_legalize_proc.h"
#include "notes_legalize_proc.h"
#include "../midi/midi_fx_proc.h"
#include "notes_proc.h"
#include "windows_proc.h"
#include "wblocks_proc.h"

#include "clipboard_track_paste_proc.h"


extern struct WTracks *cb_wtrack;

extern struct Signatures *cb_signature;
extern struct LPBs *cb_lpb;
extern struct Tempos *cb_tempo;
extern struct TempoNodes *cb_temponode;



bool CB_PasteTrackFX(
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	struct WTracks *towtrack
){
	struct Tracks *totrack;
	struct Tracks *track;
	Place *p1,p2;

        R_ASSERT_RETURN_IF_FALSE2(towtrack!=NULL, false);

	totrack=towtrack->track;
	track=wtrack->track;

	if(totrack->patch==NULL){
		totrack->patch=track->patch;
	}

	totrack->patch=track->patch;

	if(track->midi_instrumentdata!=NULL){
          totrack->midi_instrumentdata=MIDI_CopyInstrumentData(track);
	}

	totrack->fxs=NULL;

	p1=PlaceGetFirstPos();
	PlaceSetLastPos(wblock->block,&p2);

	CopyRange_fxs(&totrack->fxs,track->fxs,p1,&p2);

	LegalizeFXlines(wblock->block,totrack);

	return true;
}


bool CB_PasteTrack(
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	struct WTracks *towtrack
){
	struct Tracks *totrack;
	struct Tracks *track;
	Place *p1,p2;

	if(towtrack==NULL){
		RError("Error in function CB_PasteTrack in file clipboard_track_paste.c; towtrack=NULL\n");
		return false;
	}

	totrack=towtrack->track;
	track=wtrack->track;

	towtrack->notelength=wtrack->notelength;
	towtrack->fxwidth=wtrack->fxwidth;

	totrack->patch=track->patch;
	totrack->onoff=track->onoff;
	totrack->pan=track->pan;
	totrack->volume=track->volume;
	totrack->panonoff=track->panonoff;
	totrack->volumeonoff=track->volumeonoff;

	if(track->midi_instrumentdata!=NULL){
          totrack->midi_instrumentdata=MIDI_CopyInstrumentData(track);
	}

	totrack->trackname=talloc_strdup(track->trackname);

	totrack->notes=NULL;
	totrack->stops=NULL;
	totrack->fxs=NULL;

	p1=PlaceGetFirstPos();
	PlaceSetLastPos(wblock->block,&p2);

	CopyRange_notes(&totrack->notes,track->notes,p1,&p2);
	CopyRange_stops(&totrack->stops,track->stops,p1,&p2);
	CopyRange_fxs(&totrack->fxs,track->fxs,p1,&p2);

	LegalizeFXlines(wblock->block,totrack);
	LegalizeNotes(wblock->block,totrack);

	return true;
}

void CB_PasteTrack_CurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;
	struct Blocks *block=wblock->block;
	struct WTracks *wtrack=wblock->wtrack;
	Place lastplace;

	PlayStop();

	PlaceSetLastPos(wblock->block,&lastplace);

	switch(window->curr_track){
		case SIGNATURETRACK:
			if(cb_signature==NULL) return;
			Undo_Signatures_CurrPos(window);
			block->signatures=CB_CopySignatures(cb_signature);
			CutListAt_a(&block->signatures,&lastplace);
                        UpdateWBlockWidths(window, wblock);
			//UpdateSTimes(block);
			//UpdateWLPBs(window,wblock);
			break;
		case LPBTRACK:
			if(cb_lpb==NULL) return;
			Undo_LPBs_CurrPos(window);
			block->lpbs=CB_CopyLPBs(cb_lpb);
			CutListAt_a(&block->lpbs,&lastplace);
			UpdateSTimes(block);
			//UpdateWLPBs(window,wblock);
#if !USE_OPENGL
			DrawUpLPBs(window,wblock);
#endif
			break;
		case TEMPOTRACK:
			if(cb_tempo==NULL) return;
			Undo_Tempos_CurrPos(window);
			block->tempos=CB_CopyTempos(cb_tempo);
			CutListAt_a(&block->tempos,&lastplace);
			//UpdateWTempos(window,wblock);
#if !USE_OPENGL
			DrawUpTempos(window,wblock);
#endif
			UpdateSTimes(block);
			break;
		case TEMPONODETRACK:
			if(cb_temponode==NULL) return;
			Undo_TempoNodes_CurrPos(window);
			block->temponodes=CB_CopyTempoNodes(cb_temponode);
			CutListAt_a(&block->temponodes,&lastplace);
			LegalizeTempoNodes(block);
#if !USE_OPENGL
			///UpdateWTempoNodes(window,wblock);
			DrawUpWTempoNodes(window,wblock);
#endif
			UpdateSTimes(block);
			break;
		default:
			if(cb_wtrack==NULL) return;
			Undo_Track_CurrPos(window);
			if(window->curr_track_sub==-1){
				if(CB_PasteTrack(wblock,cb_wtrack,wtrack)){
#if !USE_OPENGL
					UpdateFXNodeLines(window,wblock,wtrack);
#endif
					window->must_redraw = true;
				}else{
#if !USE_OPENGL
					UpdateAndClearSomeTrackReallinesAndGfxWTracks(
						window,
						wblock,
						window->curr_track,
						window->curr_track
					);
#endif
				}
			}else{
				if(CB_PasteTrackFX(wblock,cb_wtrack,wtrack)){
#if !USE_OPENGL
                                  UpdateFXNodeLines(window,wblock,wtrack);
#endif
					window->must_redraw = true;
				}else{
#if !USE_OPENGL
					UpdateAndClearSomeTrackReallinesAndGfxWTracks(
						window,
						wblock,
						window->curr_track,
						window->curr_track
					);
#endif
				}
			}
			break;
	}

        SetNoteSubtrackAttributes(wtrack->track);
        ValidateCursorPos(window);
}







