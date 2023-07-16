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




#include <unistd.h>
#include <inttypes.h>

#include "nsmtracker.h"
#include "TimeData.hpp"
#include "FX.hpp"
#include "placement_proc.h"
#include "list_proc.h"
#include "undo_tracks_proc.h"
#include "undo_notes_proc.h"
#include "undo_range_proc.h"
#include "undo_maintempos_proc.h"
#include "undo_fxs_proc.h"
#include "wtracks_proc.h"
#include "notes_legalize_proc.h"
#include "clipboard_range_copy_proc.h"
#include "player_proc.h"
#include "player_pause_proc.h"
#include "visual_proc.h"
#include "../Qt/Rational.h"
#include "../embedded_scheme/scheme_proc.h"
#include "settings_proc.h"
#include "ratio_funcs.h"

#include "../api/api_proc.h"

#include "quantitize_proc.h"


extern struct Root *root;

/*
static int g_quantization_type = -1;

void Quantitize_get_quant_type(void){
  if (g_quantization_type==-1)
    g_quantization_type = SETTINGS_reqd_int("quantization_type", 3);
  
  return g_quantization_type;
}

void Quantitize_set_quant_type(int type){
  R_ASSERT_RETURN_IF_FALSE(type>=1 && type<=5);
  
  g_quantization_type = type;
  SETTINGS_write_int("quantization_type", type);
}
*/

void Quantitize_fxs(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, struct FXs *fxs){

  ADD_UNDO(FXs(window, wblock->block, wtrack->track, wblock->curr_realline));

  Undo_start_ignoring_undo_operations();{

    int fxnum = getFx(fxs->fx->name, wtrack->l.num, fxs->fx->patch->id, wblock->l.num, window->l.num);

    Ratio quant = RATIO_div(make_ratio_from_static_ratio(root->quantitize_options.quant), DYN_get_ratio(getLineZoomBlockRatio(wblock->l.num, -1)));
    
    SCHEME_eval(
                talloc_format("(simple-quantitize-fxs! %d %d %d %" PRId64 "/%" PRId64 ")",
                              wblock->block->l.num,
                              wtrack->track->l.num,
                              fxnum,
                              quant.num, quant.den
                              )
                );

  }Undo_stop_ignoring_undo_operations();

}


quantitize_options_t Quantitize_get_default_options(void){
  quantitize_options_t options;
  options.quant = make_static_ratio(1,1);
  /*
  options.quantitize_start = true;
  options.quantitize_end = false;
  options.keep_note_length = false;
  */
  options.type = 3;

  return options;
}

#if 0

static bool Quantitize_Note(
                            struct Blocks *block,
                            struct Notes **notes,
                            struct Notes *note
                            )
{
  bool was_changed;
  
  bool do_add = quantitize_note(block, note, &was_changed);

  if (do_add){
    ListAddElement3_a(notes,&note->l);
    return was_changed;
  }
  
  return false;
}

static bool Quantitize_track(
                             struct Blocks *block,
                             struct Tracks *track,
                             bool only_selected_notes
                             )
{
	struct Notes *note=NULL;
	struct Notes *notes=NULL;
	struct Notes *temp;
	Place first,last;

	PlaceSetFirstPos(&first);
	PlaceSetLastPos(block,&last);

        int num_notes_before = getNumNotes(track->l.num, block->l.num, -1);
        
	CopyRange_notes(&note,track->notes,&first,&last);

        bool ret = false;
        
	while(note!=NULL){
		temp=NextNote(note);
                
                if (!only_selected_notes || note->pianonote_is_selected) {
                  
                  if (Quantitize_Note(block,&notes,note))
                    ret = true;

                } else {

                  ListAddElement3_a(&notes,&note->l);
                  
                }
                
		note=temp;
	}

        PC_Pause();{
          track->notes=notes;        
          if (LegalizeNotes(block,track))
            ret = true;
        }PC_StopPause(NULL);

        if (ret==true)
          return true;

        if (num_notes_before != getNumNotes(track->l.num, block->l.num, -1))
          return true;
        else
          return false;
}

#else

static bool Quantitize_track(
                             struct Blocks *block,
                             struct Tracks *track,
                             bool only_selected_notes
                             )
{
        bool ret = false;

        std::vector<r::NotePtr> to_remove;
        
        r::NoteTimeData::Writer writer(track->_notes2);

        for(r::NotePtr &note : writer) {
          
          if (note->d._pianonote_is_selected || !only_selected_notes) {

            bool was_changed;
  
            if (quantitize_note2(block, note, &was_changed)) {
              if (was_changed)
                ret = true;
            } else {
              to_remove.push_back(note);
              ret = true;
            }
          }
	}

        for(r::NotePtr &note : to_remove)
          writer.removeElement(note);

        writer.sortit();
        
        return ret;
}
#endif

static bool Quantitize_selected_notes(
                      struct Tracker_Windows *window,
                      struct WBlocks *wblock
){
  return Quantitize_track(wblock->block, wblock->wtrack->track, true);
}

#if 0
static bool Quantitize_range(
                             struct Tracker_Windows *window,
                             struct WBlocks *wblock
                             )
{
	Place first,last;

	PlaceSetFirstPos(&first);
	PlaceSetLastPos(wblock->block,&last);

	if( ! wblock->range.enabled)
          return Quantitize_selected_notes(window,wblock);

        bool ret = false;
        
        struct Tracks *track = (struct Tracks *)ListFindElement1(&wblock->block->tracks->l,wblock->range.x1);

        int tracknum;
	for(tracknum=0;tracknum<=wblock->range.x2-wblock->range.x1;tracknum++){

          struct Notes *new_notes=NULL;

          struct Notes *note=track->notes;

          while(note != NULL) {
            struct Notes *next=NextNote(note);
                        
            if( p_Greater_Or_Equal(note->l.p, wblock->range.y1) && p_Less_Than(note->l.p, wblock->range.y2)) {
              if (Quantitize_Note(wblock->block,&new_notes,note))
                ret = true;
            } else
              ListAddElement3_a(&new_notes,&note->l);
            
            note=next;
          }

          PC_Pause();{
            track->notes=new_notes;          
            if (LegalizeNotes(wblock->block,track))
              ret = true;
          }PC_StopPause(window);

          track=NextTrack(track);
	}

        return ret;

}
#else
static bool Quantitize_range(
                             struct Tracker_Windows *window,
                             struct WBlocks *wblock
                             )
{
	if( ! wblock->range.enabled)
          return Quantitize_selected_notes(window,wblock);

        const Ratio r1 = place2ratio(wblock->range.y1);
        const Ratio r2 = place2ratio(wblock->range.y2);
        
        bool ret = false;
        
        struct Tracks *track = (struct Tracks *)ListFindElement1(&wblock->block->tracks->l,wblock->range.x1);

        int tracknum;
	for(tracknum=0;tracknum<=wblock->range.x2-wblock->range.x1;tracknum++){

          std::vector<r::NotePtr> to_remove;
        
          r::NoteTimeData::Writer writer(track->_notes2);

          for(r::NotePtr &note : writer.get_iterator_left(r1)) {
          
            if (note->get_time() >= r1) {
              if (note->get_time() >= r2)
                break;

              bool was_changed;
              
              if (quantitize_note2(wblock->block, note, &was_changed)) {
                if (was_changed)
                  ret = true;
              } else {
                to_remove.push_back(note);
                ret = true;
              }
            }
          }

          for(r::NotePtr &note : to_remove)
            writer.removeElement(note);

          writer.sortit();

          track=NextTrack(track);
	}

        return ret;

}
#endif

static bool Quantitize_block(
	struct Blocks *block
){

        bool ret = false;
  
	struct Tracks *track=block->tracks;

	while(track!=NULL){
          if (Quantitize_track(block,track,false))
            ret = true;
          track=NextTrack(track);
	}

        return ret;
}


void Quantitize_track_CurrPos(
	struct Tracker_Windows *window
){
	struct WBlocks *wblock=window->wblock;

        ADD_UNDO(Track_CurrPos(wblock->l.num, wblock->wtrack->l.num));
	if (!Quantitize_track(wblock->block,wblock->wtrack->track,false))
          UNDO_CANCEL_LAST_UNDO();

	UpdateAndClearSomeTrackReallinesAndGfxWTracks(
		window,
		wblock,
		window->curr_track,
		window->curr_track
	);
/*
	UpdateAndClearSomeTrackReallinesAndGfxWTracks(
		window,
		window->wblock,
		0,
		window->wblock->block->num_tracks-1
	);
*/
}

void Quantitize_block_CurrPos(
	struct Tracker_Windows *window
){

	ADD_UNDO(Range(
                       window,
                       window->wblock,
                       0,window->wblock->block->num_tracks,
                       window->wblock->curr_realline
                       ));

	if (!Quantitize_block(window->wblock->block))
          UNDO_CANCEL_LAST_UNDO();

	UpdateAndClearSomeTrackReallinesAndGfxWTracks(
		window,
		window->wblock,
		0,
		window->wblock->block->num_tracks-1
	);
}

void Quantitize_range_CurrPos(
	struct Tracker_Windows *window
){
  //if(!window->wblock->range.enabled) return;

  if (window->wblock->range.enabled)    
    ADD_UNDO(Range(window,window->wblock,window->wblock->range.x1,window->wblock->range.x2+1,window->wblock->curr_realline));
  else
    ADD_UNDO(Notes_CurrPos(window));

  if (Quantitize_range(window,window->wblock)==false)
    UNDO_CANCEL_LAST_UNDO();
        
  UpdateAndClearSomeTrackReallinesAndGfxWTracks(
                                                window,
                                                window->wblock,
                                                window->wblock->range.x1,
                                                window->wblock->range.x2
                                                );
}

void SetQuantitize_CurrPos(
                           struct Tracker_Windows *window
){
    
        StaticRatio before = root->quantitize_options.quant;
        StaticRatio after;
                  
        ReqType reqtype = GFX_OpenReq(window, 100, 100, "");

        do{
          GFX_WriteString(reqtype, "Quantitize Value : ");
          GFX_SetString(reqtype, STRING_get_chars(STATIC_RATIO_as_string(before)));
          
          char temp[1024];
          GFX_ReadString(reqtype, temp, 1010, true);

          if (temp[0]==0){
            printf("no new value\n");
            GFX_CloseReq(window, reqtype);
            return;
          }
          
          after = STATIC_RATIO_from_string(STRING_create(temp));
        } while (after.denominator==0);
        
        GFX_CloseReq(window, reqtype);

        if (STATIC_RATIO_literary_equal(root->quantitize_options.quant, after))
          return;

        ADD_UNDO(MainTempo(window, window->wblock));
                       
        root->quantitize_options.quant = after;
}
