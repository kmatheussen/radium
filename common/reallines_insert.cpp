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
#include "swingtext_proc.h"
#include "fxtext_proc.h"
#include "chancetext_proc.h"
#include "centtext_proc.h"
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
#include "undo_fxs_proc.h"
#include "player_proc.h"
#include "player_pause_proc.h"
#include "Beats_proc.h"
#include "ratio_funcs.h"

#include "reallines_insert_proc.h"

static bool InsertRatio_notes_extra(
                                    struct Blocks *block,
                                    void *tonote,
                                    struct ListHeader3 *l,
                                    Ratio ratio,
                                    Ratio toratio
                                    )
{
  bool ret = false;
  
  struct Notes *note=(struct Notes *)l;
  Ratio endratio = make_ratio_from_place(note->end);
  
  if(endratio >= ratio){
    
    if (RATIO_greater_than_zero(endratio+toratio)){
      
      //RatioAddfloat(&note->end,toratio);
      note->end = make_place_from_ratio(make_ratio_from_place(note->end) + toratio);

      if (List_InsertRatioLen3(block,&note->velocities,(struct ListHeader3*)note->velocities,ratio,toratio,NULL))
        ret = true;
      
      if (List_InsertRatioLen3(block,&note->pitches,(struct ListHeader3*)note->pitches,ratio,toratio,NULL))
        ret = true;
      
    }
  }

  return ret;
}


static bool InsertRatio_notes(
                              struct Blocks *block,
                              struct Tracks *track,
                              Ratio ratio,
                              Ratio toratio
                              )
{
  if (List_InsertRatioLen3(
                           block,
                           &track->notes,
                           (struct ListHeader3*)track->notes,
                           ratio,
                           toratio,
                           InsertRatio_notes_extra
                           ))
    {
      LegalizeNotes(block,track);
      return true;
    }

  return false;
}

static bool InsertRatio_fxs(
                            struct Blocks *block,
                            struct Tracks *track,
                            struct FXs *fxs,
                            Ratio ratio,
                            Ratio toratio
                            )
{

  bool ret = List_InsertRatioLen3(
                                  block,
                                  &fxs->fxnodelines,
                                  (struct ListHeader3*)fxs->fxnodelines,
                                  ratio,
                                  toratio,
                                  NULL
                                  );
  
  if (ret)
    LegalizeFXlines(block,track);

  return ret;
}

static bool InsertRatio_all_fxs(
	struct Blocks *block,
	struct Tracks *track,
	Ratio ratio,
	Ratio toratio
){

  bool ret = false;
  
  VECTOR_FOR_EACH(struct FXs *, fxs, &track->fxs){
    if (List_InsertRatioLen3(
                             block,
                             &fxs->fxnodelines,
                             (struct ListHeader3*)fxs->fxnodelines,
                             ratio,
                             toratio,
                             NULL
                             ))
      ret = true;
  }END_VECTOR_FOR_EACH;

  if (ret)
    LegalizeFXlines(block,track);

  return ret;
}

static void InsertRatio_temponodes(
                                   struct Blocks *block,
                                   Ratio ratio,
                                   Ratio toratio
                                   )
{
  List_InsertRatioLen3(block,&block->temponodes,&block->temponodes->l,ratio,toratio,NULL);
  
  if(RATIO_greater_than_zero(toratio)){
    ListAddElement3_a(&block->temponodes,&block->lasttemponode->l);
  }
  
  LegalizeTempoNodes(block);
}

static bool InsertRatio_tempos(
	struct Blocks *block,
	Ratio ratio,
	Ratio toratio
){
  return List_InsertRatioLen3(block,&block->tempos,(struct ListHeader3*)block->tempos,ratio,toratio,NULL);
}

static bool InsertRatio_lpbs(
	struct Blocks *block,
	Ratio ratio,
	Ratio toratio
){
  return List_InsertRatioLen3(block,&block->lpbs,(struct ListHeader3*)block->lpbs,ratio,toratio,NULL);
}

static bool InsertRatio_swings(
                               struct Blocks *block,
                               Ratio ratio,
                               Ratio toratio
                               )
{
  return List_InsertRatioLen3(block,&block->swings,(struct ListHeader3*)block->swings,ratio,toratio,NULL);
}

static bool InsertRatio_track_swings(
                                     struct Blocks *block,
                                     struct Tracks *track,
                                     Ratio ratio,
                                     Ratio toratio
                                     )
{
  return List_InsertRatioLen3(block,&track->swings,(struct ListHeader3*)track->swings,ratio,toratio,NULL);
}

static bool InsertRatio_signatures(
	struct Blocks *block,
	Ratio ratio,
	Ratio toratio
){
  return List_InsertRatioLen3(block,&block->signatures,(struct ListHeader3*)block->signatures,ratio,toratio,NULL);
}


static bool InsertRatio_stops(
	struct Blocks *block,
	struct Tracks *track,
	Ratio ratio,
	Ratio toratio
){
  return r::TimeData<r::Stop>::Writer(track->stops2).insert_ratio(ratio, toratio, make_ratio(block->num_lines, 1));  
  //return List_InsertRatioLen3(block,&track->stops,(struct ListHeader3*)track->stops,ratio,toratio,NULL);
}


void InsertRealLines_CurrPos(
	struct Tracker_Windows *window,
	int num_reallines
){
	struct WBlocks *wblock=window->wblock;
	struct Blocks *block=wblock->block;
	const struct LocalZooms **reallines=wblock->reallines;
	int curr_realline=wblock->curr_realline;
	const struct LocalZooms *realline=reallines[curr_realline];

	if(num_reallines==0) return;

        Ratio ratio = make_ratio_from_place(realline->l.p);

	Ratio toratio;
	if(curr_realline==wblock->num_reallines-1){
          toratio = RATIO_floor(ratio + make_ratio(1,1)); // floor(ratio+1.0f);
	}else{
          toratio = make_ratio_from_place(reallines[curr_realline+1]->l.p); // GetfloatFromPlace(&reallines[curr_realline+1]->l.p);
	}

	toratio -= ratio;

	toratio *= num_reallines;

	if(RATIO_is_zero(toratio)) return;	//extra check.


        PC_Pause();
          
	switch(window->curr_track){
		case SWINGTRACK:
                  ADD_UNDO(Swings_CurrPos(window, NULL));
                  if (InsertRatio_swings(block,ratio,toratio))
                    TIME_block_swings_have_changed(block);
                  else
                    UNDO_CANCEL_LAST_UNDO();
                  break;
		case SIGNATURETRACK:
                  ADD_UNDO(Signatures_CurrPos(window));
                  if (InsertRatio_signatures(block,ratio,toratio))
                    TIME_block_signatures_have_changed(block);
                  else
                    UNDO_CANCEL_LAST_UNDO();
                  break;
		case LPBTRACK:
                  ADD_UNDO(LPBs_CurrPos(window));
                  if (InsertRatio_lpbs(block,ratio,toratio))
                    TIME_block_LPBs_have_changed(block);
                  else
                    UNDO_CANCEL_LAST_UNDO();
                  break;
		case TEMPOTRACK:
                  ADD_UNDO(Tempos_CurrPos(window));
                  if (InsertRatio_tempos(block,ratio,toratio))
                    TIME_block_tempos_have_changed(block);
                  else
                    UNDO_CANCEL_LAST_UNDO();
                  break;
		case TEMPONODETRACK:
                  ADD_UNDO(TempoNodes_CurrPos(window));
                  InsertRatio_temponodes(block,ratio,toratio);
                  TIME_block_tempos_have_changed(block);
                  break;
		default:
                  {
                    struct WTracks *wtrack = wblock->wtrack;
                    struct Tracks *track = wtrack->track;
                    struct FXs *fxs;

                    if (SWINGTEXT_subsubtrack(window, wtrack) >= 0){
                      
                      ADD_UNDO(Swings_CurrPos(window, track));
                      if (InsertRatio_track_swings(block,track,ratio,toratio))
                        TIME_block_swings_have_changed(block);
                      else
                        UNDO_CANCEL_LAST_UNDO();
                      
                    } else if (FXTEXT_subsubtrack(window, wtrack, &fxs) >= 0){

                      ADD_UNDO(FXs_CurrPos(window));
                      if (!InsertRatio_fxs(block,track,fxs,ratio,toratio))
                        UNDO_CANCEL_LAST_UNDO();
                      
                    } else {
                      
                      bool ret = false;

                      bool on_chancetext = CHANCETEXT_subsubtrack(window, wtrack) >= 0;
                      bool on_centtext = CENTTEXT_subsubtrack(window, wtrack) >= 0;

                      if(window->curr_track_sub>=0 && !on_chancetext && !on_centtext){
                        
                        ADD_UNDO(NotesAndFXs_CurrPos(window));
                        if (InsertRatio_all_fxs(block,track,ratio,toratio))
                          ret = true;
                        
                      }else{
                        
                        ADD_UNDO(Notes_CurrPos(window));
                        
                      }
                      
                      if (InsertRatio_notes(block,track,ratio,toratio))
                        ret = true;
                      
                      if (InsertRatio_stops(block,track,ratio,toratio))
                        ret = true;
                      
                      if (!ret)
                        UNDO_CANCEL_LAST_UNDO();
                      
                    }
                  }
                  break;
	}

        window->must_redraw_editor = true;

        PC_StopPause(window);	

}







