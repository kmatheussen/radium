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
#include "vector_proc.h"
#include "time_proc.h"
#include "swing_proc.h"
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
#include "fxlines_legalize_proc.h"
#include "notes_legalize_proc.h"
#include "notes_proc.h"
#include "windows_proc.h"
#include "wblocks_proc.h"
#include "Beats_proc.h"
#include "clipboard_track_copy_proc.h"
#include "patch_proc.h"
#include "instruments_proc.h"
#include "swingtext_proc.h"

#include "../midi/midi_fx_proc.h"

#include "../api/api_proc.h"

#include "../audio/audio_instrument_proc.h"

#include "../Qt/Qt_instruments_proc.h"


#include "clipboard_track_paste_proc.h"



extern struct Swing *cb_swing;
extern struct Signatures *cb_signature;
extern struct LPBs *cb_lpb;
extern struct Tempos *cb_tempo;
extern struct TempoNodes *cb_temponode;


static void make_patches_usable(struct Tracks *track){
  struct Patch *old_patch = track->patch;
  
  if (old_patch != NULL) {
    
    struct Patch *new_patch = old_patch;
    
    if (!old_patch->is_usable) {

      if (old_patch->instrument != get_audio_instrument()) { // is_usable is not supposed to be false for MIDI instrument.
        
        R_ASSERT(false);
        
      } else {

        printf("PERMANENT_ID for %s: %d\n",old_patch->name,old_patch->permanent_id);
        
        if (old_patch->permanent_id != 0)
          new_patch = AUDIO_get_the_replacement_for_old_permanent_patch(old_patch);
        else {
          new_patch = PATCH_create_audio(NULL, NULL, old_patch->name, old_patch->state, false, 0, 0);
          connectAudioInstrumentToMainPipe(new_patch->id);
        }

      }
      
      track->patch = new_patch;
    }

    if (track->patch->patchdata==NULL){
      R_ASSERT(track->patch->instrument == get_MIDI_instrument()); // This can happen if deleting all unused midi instruments.
      if (g_currpatch == track->patch)
        g_currpatch = NULL;
      track->patch = NULL;
      return;
    }
    
    VECTOR_FOR_EACH(struct FXs *fxs, &track->fxs){
      struct FX *fx = fxs->fx;
      
      if (fx->patch == old_patch)
        fx->patch = new_patch;
      else if (!fx->patch->is_usable){
        
        if (old_patch->instrument != get_audio_instrument()) {
          
          R_ASSERT(false); // Only audio instruments may not be usable.
          
        } else {

          if (fx->patch->permanent_id != 0)
            fx->patch = AUDIO_get_the_replacement_for_old_permanent_patch(fx->patch);
          else
            fx->patch = PATCH_create_audio(NULL, NULL, fx->patch->name, fx->patch->state, false, 0, 0);
        }
        
      }
    }END_VECTOR_FOR_EACH;
    
  }
}

static bool co_CB_PasteTrackFX(
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

        make_patches_usable(track);

        if (totrack->patch == NULL)
          totrack->patch = track->patch;
                      
	if(track->midi_instrumentdata!=NULL){
          totrack->midi_instrumentdata=MIDI_CopyInstrumentData(track);
	}

	VECTOR_clean(&totrack->fxs);

	p1=PlaceGetFirstPos();
	PlaceSetLastPos(wblock->block,&p2);

	CopyRange_fxs(&totrack->fxs,&track->fxs,p1,&p2);

	LegalizeFXlines(wblock->block,totrack);

	return true;
}

static bool paste_track(
                        struct WBlocks *wblock,
                        struct WTracks *wtrack,
                        struct WTracks *towtrack
                        )
{
        struct Tracks *totrack = towtrack->track;
        struct Tracks *track = wtrack->track;
        Place *p1,p2;

	towtrack->notelength=wtrack->notelength;
	towtrack->fxwidth=wtrack->fxwidth;

	totrack->onoff=track->onoff;
	totrack->pan=track->pan;
	totrack->volume=track->volume;
	totrack->panonoff=track->panonoff;
	totrack->volumeonoff=track->volumeonoff;
        ATOMIC_SET(totrack->midi_channel, ATOMIC_GET(track->midi_channel));
        
	if(track->midi_instrumentdata!=NULL){
          totrack->midi_instrumentdata=MIDI_CopyInstrumentData(track);
	}

	totrack->trackname=talloc_strdup(track->trackname);

	totrack->notes=NULL;
	totrack->stops=NULL;
        totrack->swings=NULL;
	VECTOR_clean(&totrack->fxs);

	p1=PlaceGetFirstPos();
	PlaceSetLastPos(wblock->block,&p2);

	CopyRange_notes(&totrack->notes,track->notes,p1,&p2);
	CopyRange_stops(&totrack->stops,track->stops,p1,&p2);
	totrack->swings = CB_CopySwings(track->swings,&p2);

        if (totrack->patch != NULL)
          CopyRange_fxs(&totrack->fxs,&track->fxs,p1,&p2);

	LegalizeFXlines(wblock->block,totrack);
	LegalizeNotes(wblock->block,totrack);

	return true;

}

bool mo_CB_PasteTrack(
                      struct WBlocks *wblock,
                      struct WTracks *wtrack,
                      struct WTracks *towtrack
                      )
{
	if(towtrack==NULL){
          RError("Error in function CB_PasteTrack in file clipboard_track_paste.c; towtrack=NULL\n");
          return false;
        }


        struct Tracks *totrack = towtrack->track;
        struct Tracks *track = wtrack->track;

        totrack->patch = track->patch;

        bool ret = paste_track(wblock, wtrack, towtrack);
        TIME_block_swings_have_changed(wblock->block);
        return ret;
}


bool co_CB_PasteTrack(
                      struct WBlocks *wblock,
                      struct WTracks *wtrack,
                      struct WTracks *towtrack
                      )
{

	if(towtrack==NULL){
		RError("Error in function CB_PasteTrack in file clipboard_track_paste.c; towtrack=NULL\n");
		return false;
	}

        struct Tracks *totrack = towtrack->track;
	struct Tracks *track = wtrack->track;
        
        make_patches_usable(track);
        
        totrack->patch = track->patch;

        bool ret = paste_track(wblock, wtrack, towtrack);
        TIME_block_swings_have_changed(wblock->block);
        return ret;
}

static void remove_fxs_from_fxss(vector_t *fxss, struct FXs *fxs){
  VECTOR_FOR_EACH(struct FXs *maybe, fxss){
    if (maybe->fx->patch==fxs->fx->patch && maybe->fx->effect_num==fxs->fx->effect_num){
      VECTOR_remove(fxss, maybe);
      return;
    }
  }END_VECTOR_FOR_EACH;
}

void CB_PasteTrack_CurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;
	struct Blocks *block=wblock->block;
	struct WTracks *wtrack=wblock->wtrack;
	Place lastplace;

	PlaceSetLastPos(wblock->block,&lastplace);

        PC_Pause();
                  
	switch(window->curr_track){
		case SWINGTRACK:
                  if(cb_swing==NULL) goto exit;
			ADD_UNDO(Swings_CurrPos(window));
			block->swings=CB_CopySwings(cb_swing, &lastplace);
                        TIME_block_swings_have_changed(block);
			break;
		case SIGNATURETRACK:
                  if(cb_signature==NULL) goto exit;
			ADD_UNDO(Signatures_CurrPos(window));
			block->signatures=CB_CopySignatures(cb_signature);
			CutListAt_a(&block->signatures,&lastplace);
                        UpdateWBlockWidths(window, wblock);
                        TIME_block_signatures_have_changed(block);
			break;
		case LPBTRACK:
			if(cb_lpb==NULL) goto exit;
			ADD_UNDO(LPBs_CurrPos(window));
			block->lpbs=CB_CopyLPBs(cb_lpb);
			CutListAt_a(&block->lpbs,&lastplace);
                        TIME_block_LPBs_have_changed(block);
#if !USE_OPENGL
			DrawUpLPBs(window,wblock);
#endif
			break;
		case TEMPOTRACK:
			if(cb_tempo==NULL) goto exit;
			ADD_UNDO(Tempos_CurrPos(window));
			block->tempos=CB_CopyTempos(cb_tempo);
			CutListAt_a(&block->tempos,&lastplace);
			//UpdateWTempos(window,wblock);
#if !USE_OPENGL
			DrawUpTempos(window,wblock);
#endif
                        TIME_block_tempos_have_changed(block);
			break;
		case TEMPONODETRACK:
			if(cb_temponode==NULL) goto exit;
			ADD_UNDO(TempoNodes_CurrPos(window));
			block->temponodes=CB_CopyTempoNodes(cb_temponode);
			CutListAt_a(&block->temponodes,&lastplace);
			LegalizeTempoNodes(block);
#if !USE_OPENGL
			///UpdateWTempoNodes(window,wblock);
			DrawUpWTempoNodes(window,wblock);
#endif
                        TIME_block_tempos_have_changed(block);
			break;
		default:
                  if (SWINGTEXT_subsubtrack(window, wtrack) != -1){
                    if (cb_swing==NULL)
                      goto exit;

                    Place p2;
                    PlaceSetLastPos(wblock->block,&p2);

                    ADD_UNDO(Track_CurrPos(wblock->l.num, wtrack->l.num));
                    wtrack->track->swings = CB_CopySwings(cb_swing,&p2);
                    TIME_block_swings_have_changed(block);

                  } else {
			if(cb_wtrack==NULL) goto exit;

                        UNDO_OPEN_REC();{
                          printf("curr_track_sub: %d\n",window->curr_track_sub);
                          ADD_UNDO(Track_CurrPos(wblock->l.num, wtrack->l.num));
                          
                          if(window->curr_track_sub==-1 && cb_wtrack_only_contains_one_fxs==false){

                            // copy all
                            
                            printf("aaa\n");
                            if(co_CB_PasteTrack(wblock,cb_wtrack,wtrack)){
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
                            printf("bbb\n");

                            // only copy fx
                            
                            struct WTracks *fromwtrack = cb_wtrack;
                                                                                    
                            if (cb_wtrack_only_contains_one_fxs==true){

                              // only copy one fx
                              
                              R_ASSERT(cb_wtrack->track->fxs.num_elements==1);

                              struct FXs *fxs = cb_wtrack->track->fxs.elements[0];
                              
                              vector_t *fxss = VECTOR_copy(&wtrack->track->fxs);
                              remove_fxs_from_fxss(fxss, fxs);
                              
                              VECTOR_push_back(fxss, fxs);
                              
                              fromwtrack = CB_CopyTrack(wblock, cb_wtrack);
                              fromwtrack->track->fxs = *VECTOR_copy(fxss);
                            }
                            
                            if(co_CB_PasteTrackFX(wblock,fromwtrack,wtrack)){
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

                        }UNDO_CLOSE();
                  }                        
                  break;
	}

        SetNotePolyphonyAttributes(wtrack->track);
        ValidateCursorPos(window);


 exit:
        PC_StopPause(window);
}







