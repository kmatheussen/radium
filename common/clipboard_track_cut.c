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
#include "clipboard_track_copy_proc.h"
#include "wtracks_proc.h"
#include "clipboard_tempos_copy_proc.h"
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
#include "player_pause_proc.h"
#include "windows_proc.h"
#include "notes_proc.h"
#include "wblocks_proc.h"
#include "Beats_proc.h"
#include "clipboard_track_cut_proc.h"


extern struct Signatures *cb_signature;
extern struct LPBs *cb_lpb;
extern struct Tempos *cb_tempo;
extern struct TempoNodes *cb_temponode;



extern struct WTracks *cb_wtrack;

void CB_CutTrack_Force(
	struct WBlocks *wblock,
	struct WTracks *wtrack
){
	struct FXs *fxs=wtrack->track->fxs;

	cb_wtrack=CB_CopyTrack(wblock,wtrack);
	wtrack->track->notes=NULL;
	wtrack->track->stops=NULL;

	while(fxs!=NULL){
		(*fxs->fx->closeFX)(fxs->fx,wtrack->track);
		fxs=NextFX(fxs);
	}

	wtrack->track->fxs=NULL;
}

struct WTracks *CB_CutTrack(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack
){

  struct WTracks *ret;
  
  PC_Pause();{
            
	struct FXs *fxs=wtrack->track->fxs;

	ret=CB_CopyTrack(wblock,wtrack);

        //	if(window->curr_track_sub<0){
		wtrack->track->notes=NULL;
		wtrack->track->stops=NULL;
                //	}

	while(fxs!=NULL){
		(*fxs->fx->closeFX)(fxs->fx,wtrack->track);
		fxs=NextFX(fxs);
	}

	wtrack->track->fxs=NULL;

  }PC_StopPause(window);
  
  return ret;
}

void CB_CutTrack_CurrPos(
	struct Tracker_Windows *window
){
	struct WBlocks *wblock=window->wblock;
	struct Blocks *block=wblock->block;
	struct WTracks *wtrack=wblock->wtrack;

        PC_Pause();{
          
	switch(window->curr_track){
		case SIGNATURETRACK:
                  ADD_UNDO(Signatures_CurrPos(window));
			cb_signature=CB_CopySignatures(block->signatures);
			block->signatures=NULL;
                        UpdateBeats(block);
                        UpdateWBlockWidths(window, wblock);
			//UpdateSTimes(block);
			//UpdateWLPBs(window,wblock);
			break;
		case LPBTRACK:
                  ADD_UNDO(LPBs_CurrPos(window));
			cb_lpb=CB_CopyLPBs(block->lpbs);
			block->lpbs=NULL;
			UpdateSTimes(block);
                        UpdateBeats(block);
			//UpdateWLPBs(window,wblock);
			break;
		case TEMPOTRACK:
                  ADD_UNDO(Tempos_CurrPos(window));
			cb_tempo=CB_CopyTempos(block->tempos);
			block->tempos=NULL;
			UpdateSTimes(block);
			break;
		case TEMPONODETRACK:
                  ADD_UNDO(TempoNodes_CurrPos(window));
			cb_temponode=CB_CopyTempoNodes(block->temponodes);
			block->temponodes=NULL;
			LegalizeTempoNodes(block);
			UpdateSTimes(block);
			break;
		default:
                  ADD_UNDO(Track_CurrPos(window));
			cb_wtrack = CB_CutTrack(window,wblock,wtrack);
                        //#if !USE_OPENGL
			UpdateAndClearSomeTrackReallinesAndGfxWTracks(
				window,
				window->wblock,
				window->curr_track,
				window->curr_track
			);
                        SetNotePolyphonyAttributes(wtrack->track);
                        ValidateCursorPos(window);
                        //#endif
			break;
	}

        }PC_StopPause(window);
        
        window->must_redraw=true;
}


