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
#include "undo_tracks_proc.h"
#include "undo_swings_proc.h"
#include "undo_signatures_proc.h"
#include "undo_lpbs_proc.h"
#include "undo_tempos_proc.h"
#include "undo_temponodes_proc.h"
#include "undo_fxs_proc.h"
#include "player_proc.h"
#include "player_pause_proc.h"
#include "windows_proc.h"
#include "notes_proc.h"
#include "wblocks_proc.h"
#include "Beats_proc.h"
#include "clipboard_track_cut_proc.h"
#include "swingtext_proc.h"
#include "fxtext_proc.h"


extern struct Swing *cb_swing;
extern struct Signatures *cb_signature;
extern struct LPBs *cb_lpb;
extern struct Tempos *cb_tempo;
extern struct TempoNodes *cb_temponode;



extern struct WTracks *cb_wtrack;

// called from clearTrack, cut_track, CB_CutTrack_Force, and CB_ClearOrCutTrack_CurrPos.
void CB_ClearTrack_Force(
	struct Blocks *block,
	struct Tracks *track,
        radium::PlayerPauseOnlyIfNeeded &pause_player,
        bool &swings_have_changed
){
  //R_ASSERT_NON_RELEASE(PLAYER_current_thread_has_lock() || is_playing()==false); // Making it NON_RELEASE because: If "track" is not alive we would get a false assertion here.

        swings_have_changed = false;
  
        if (!r::NoteTimeData::Reader(track->_notes2).is_empty() || track->notes != NULL || track->swings!=NULL) {
          
          pause_player.need_it();
          track->notes=NULL;
          //track->stops=NULL;

          r::NoteTimeData::Writer notes_cleaner(track->_notes2, r::KeepOldData::USE_CLEAN_DATA);

          if (track->swings != NULL){
            track->swings=NULL;
            swings_have_changed = true;
          }
          
        }

        r::StopTimeData::Writer stops_cleaner(track->stops2, r::KeepOldData::USE_CLEAN_DATA);
        
        VECTOR_FOR_EACH(struct FXs *, fxs, &track->fxs){
          (*fxs->fx->closeFX)(fxs->fx,track);
        }END_VECTOR_FOR_EACH;

        {
          SCOPED_PLAYER_LOCK_IF_PLAYING();
          VECTOR_clean(&track->fxs);
        }
}

/*
static void CB_CutTrack_Force(
	struct WBlocks *wblock,
	struct WTracks *wtrack
){
        cb_wtrack=CB_CopyTrack(wblock,wtrack);

        CB_ClearTrack_Force(wblock->block, wtrack->track);
}
*/

static struct WTracks *cut_track(
                                 struct Tracker_Windows *window,
                                 struct WBlocks *wblock,
                                 struct WTracks *wtrack,
                                 bool always_cut_all_fxs,
                                 bool *only_one_fxs_was_cut,
                                 radium::PlayerPauseOnlyIfNeeded &pause_player,
                                 bool &swings_have_changed
){

  if (!always_cut_all_fxs)
    R_ASSERT(only_one_fxs_was_cut!=NULL);
  
  struct WTracks *ret;
  
    ret = internal_copy_track(wblock, wtrack, always_cut_all_fxs, only_one_fxs_was_cut);

    if (always_cut_all_fxs || (*only_one_fxs_was_cut)==false) {
      
      CB_ClearTrack_Force(wblock->block, wtrack->track, pause_player, swings_have_changed);
  
    } else {

      SCOPED_PLAYER_LOCK_IF_PLAYING();
      
      struct FXs *fxs = (struct FXs*)ret->track->fxs.elements[0];
      
      VECTOR_FOR_EACH(struct FXs *, maybe, &wtrack->track->fxs){
        if (maybe->fx->patch==fxs->fx->patch && maybe->fx->effect_num==fxs->fx->effect_num){
          (*maybe->fx->closeFX)(maybe->fx,wtrack->track);
          VECTOR_remove(&wtrack->track->fxs, maybe);
          break;
        }
      }END_VECTOR_FOR_EACH;
      
    }

  return ret;
}

struct WTracks *CB_CutTrack(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack
){
  radium::PlayerPauseOnlyIfNeeded pause_player;
  bool swings_have_changed = false;

  struct WTracks *ret = cut_track(window, wblock, wtrack, true, NULL, pause_player, swings_have_changed);

  if (swings_have_changed)
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

        radium::PlayerPauseOnlyIfNeeded pause_player;
          
	switch(window->curr_track){
		case SWINGTRACK:
                        ADD_UNDO(Swings_CurrPos(window, NULL));
                        if (do_cut)
                          cb_swing=CB_CopySwings(block->swings, NULL);
                        pause_player.need_it();
			block->swings=NULL;
                        TIME_block_swings_have_changed(block);
			break;
		case SIGNATURETRACK:
                  ADD_UNDO(Signatures_CurrPos(window));
			if (do_cut)
                          cb_signature=CB_CopySignatures(block->signatures);
                        pause_player.need_it();
			block->signatures=NULL;
                        TIME_block_signatures_have_changed(block);
                        UpdateWBlockWidths(window, wblock);
			//UpdateSTimes(block);
			//UpdateWLPBs(window,wblock);
			break;
		case LPBTRACK:
                  ADD_UNDO(LPBs_CurrPos(window));
			if (do_cut)
                          cb_lpb=CB_CopyLPBs(block->lpbs);
                        pause_player.need_it();
			block->lpbs=NULL;
                        TIME_block_LPBs_have_changed(block);
			//UpdateWLPBs(window,wblock);
			break;
		case TEMPOTRACK:
                  ADD_UNDO(Tempos_CurrPos(window));
			if (do_cut)
                          cb_tempo=CB_CopyTempos(block->tempos);
                        pause_player.need_it();
			block->tempos=NULL;
                        TIME_block_tempos_have_changed(block);
			break;
		case TEMPONODETRACK:
                  ADD_UNDO(TempoNodes_CurrPos(window));
			if (do_cut)
                          cb_temponode=CB_CopyTempoNodes(block->temponodes);
                        pause_player.need_it();
			block->temponodes=NULL;
			LegalizeTempoNodes(block);
                        TIME_block_tempos_have_changed(block);
			break;
		default:
                  
                  if (SWINGTEXT_subsubtrack(window, wtrack) != -1){
                    
                    ADD_UNDO(Swings_CurrPos(window, wtrack->track));
                  
                    if (do_cut)
                      cb_swing = wtrack->track->swings;
                    
                    pause_player.need_it();
                    wtrack->track->swings = NULL;
                    TIME_block_swings_have_changed(block);
                    
                  } else {

                        {
                          struct FXs *fxs;
                          if (do_cut && FXTEXT_subsubtrack(root->song->tracker_windows, wtrack, &fxs) != -1)
                            ADD_UNDO(FXs_CurrPos(window));
                          else
                            ADD_UNDO(Track_CurrPos(wblock->l.num, wtrack->l.num));
                        }
                          
                        bool swings_have_changed = false;
			if (do_cut)
                          cb_wtrack = cut_track(window, wblock, wtrack, false, &cb_wtrack_only_contains_one_fxs, pause_player, swings_have_changed);
                        else
                          CB_ClearTrack_Force(wblock->block, wtrack->track, pause_player, swings_have_changed);

                        //#if !USE_OPENGL
			UpdateAndClearSomeTrackReallinesAndGfxWTracks(
				window,
				window->wblock,
				window->curr_track,
				window->curr_track
			);
                        ValidateCursorPos(window);

                        if (swings_have_changed)
                          TIME_block_swings_have_changed(block);
                        //#endif
			break;
                  }
	}

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

