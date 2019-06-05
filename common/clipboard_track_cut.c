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
#include "vector_proc.h"
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
#include "undo_swings_proc.h"
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
#include "swingtext_proc.h"


extern struct Swing *cb_swing;
extern struct Signatures *cb_signature;
extern struct LPBs *cb_lpb;
extern struct Tempos *cb_tempo;
extern struct TempoNodes *cb_temponode;



extern struct WTracks *cb_wtrack;

// called from clearTrack, cut_track, CB_CutTrack_Force, and CB_ClearOrCutTrack_CurrPos.
void CB_ClearTrack_Force(
	struct Blocks *block,
	struct Tracks *track
){
	track->notes=NULL;
	track->stops=NULL;
        track->swings=NULL;

        VECTOR_FOR_EACH(struct FXs *fxs, &track->fxs){
          (*fxs->fx->closeFX)(fxs->fx,track);
        }END_VECTOR_FOR_EACH;

	VECTOR_clean(&track->fxs);

        //ValidateCursorPos(root->song->tracker_windows);
}

void CB_CutTrack_Force(
	struct WBlocks *wblock,
	struct WTracks *wtrack
){
        cb_wtrack=CB_CopyTrack(wblock,wtrack);

        CB_ClearTrack_Force(wblock->block, wtrack->track);
}

static struct WTracks *cut_track(
                                 struct Tracker_Windows *window,
                                 struct WBlocks *wblock,
                                 struct WTracks *wtrack,
                                 bool always_cut_all_fxs,
                                 bool *only_one_fxs_was_cut
){

  if (!always_cut_all_fxs)
    R_ASSERT(only_one_fxs_was_cut!=NULL);
  
  struct WTracks *ret;
  
  PC_Pause();{
            
    ret = internal_copy_track(wblock, wtrack, always_cut_all_fxs, only_one_fxs_was_cut);

    if (always_cut_all_fxs || (*only_one_fxs_was_cut)==false) {

      CB_ClearTrack_Force(wblock->block, wtrack->track);
      
    } else {

      struct FXs *fxs = ret->track->fxs.elements[0];
      
      VECTOR_FOR_EACH(struct FXs *maybe, &wtrack->track->fxs){
        if (maybe->fx->patch==fxs->fx->patch && maybe->fx->effect_num==fxs->fx->effect_num){
          (*maybe->fx->closeFX)(maybe->fx,wtrack->track);
          VECTOR_remove(&wtrack->track->fxs, maybe);
          break;
        }
      }END_VECTOR_FOR_EACH;
      
    }

  }PC_StopPause(window);
  
  return ret;
}

struct WTracks *CB_CutTrack(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack
){
  struct WTracks *ret = cut_track(window, wblock, wtrack, true, NULL);
  TIME_block_swings_have_changed(wblock->block);
  return ret;
}

static void CB_ClearOrCutTrack_CurrPos(
                                       struct Tracker_Windows *window,
                                       bool do_cut
){
	struct WBlocks *wblock=window->wblock;
	struct Blocks *block=wblock->block;
	struct WTracks *wtrack=wblock->wtrack;

        PC_Pause();{
          
	switch(window->curr_track){
		case SWINGTRACK:
                  ADD_UNDO(Swings_CurrPos(window));
                        if (do_cut) cb_swing=CB_CopySwings(block->swings, NULL);
			block->swings=NULL;
                        TIME_block_swings_have_changed(block);
			break;
		case SIGNATURETRACK:
                  ADD_UNDO(Signatures_CurrPos(window));
			if (do_cut) cb_signature=CB_CopySignatures(block->signatures);
			block->signatures=NULL;
                        TIME_block_signatures_have_changed(block);
                        UpdateWBlockWidths(window, wblock);
			//UpdateSTimes(block);
			//UpdateWLPBs(window,wblock);
			break;
		case LPBTRACK:
                  ADD_UNDO(LPBs_CurrPos(window));
			if (do_cut) cb_lpb=CB_CopyLPBs(block->lpbs);
			block->lpbs=NULL;
                        TIME_block_LPBs_have_changed(block);
			//UpdateWLPBs(window,wblock);
			break;
		case TEMPOTRACK:
                  ADD_UNDO(Tempos_CurrPos(window));
			if (do_cut) cb_tempo=CB_CopyTempos(block->tempos);
			block->tempos=NULL;
                        TIME_block_tempos_have_changed(block);
			break;
		case TEMPONODETRACK:
                  ADD_UNDO(TempoNodes_CurrPos(window));
			if (do_cut) cb_temponode=CB_CopyTempoNodes(block->temponodes);
			block->temponodes=NULL;
			LegalizeTempoNodes(block);
                        TIME_block_tempos_have_changed(block);
			break;
		default:
                  ADD_UNDO(Track_CurrPos(wblock->l.num, wtrack->l.num));
                  if (SWINGTEXT_subsubtrack(window, wtrack) != -1){
                    if (do_cut) cb_swing = wtrack->track->swings;
                    wtrack->track->swings = NULL;
                    TIME_block_swings_have_changed(block);
                  } else {
			if (do_cut)
                          cb_wtrack = cut_track(window, wblock, wtrack, false, &cb_wtrack_only_contains_one_fxs);
                        else
                          CB_ClearTrack_Force(wblock->block, wtrack->track);
                        
                        //#if !USE_OPENGL
			UpdateAndClearSomeTrackReallinesAndGfxWTracks(
				window,
				window->wblock,
				window->curr_track,
				window->curr_track
			);
                        ValidateCursorPos(window);
                        TIME_block_swings_have_changed(block);
                        //#endif
			break;
                  }
	}

        }PC_StopPause(window);
        
        window->must_redraw=true;
}



void CB_ClearTrack_CurrPos(
	struct Tracker_Windows *window
){
  CB_ClearOrCutTrack_CurrPos(window, false);
}

void CB_CutTrack_CurrPos(
	struct Tracker_Windows *window
){
  CB_ClearOrCutTrack_CurrPos(window, true);
}

