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
#include "placement_proc.h"
#include "list_proc.h"
#include "undo_tracks_proc.h"
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

    Ratio quant = RATIO_divide(root->quantitize_options.quant, DYN_get_ratio(getLineZoomBlockRatio(wblock->l.num, -1)));
    
    SCHEME_eval(
                talloc_format("(simple-quantitize-fxs! %d %d %d %d/%d)",
                              wblock->block->l.num,
                              wtrack->track->l.num,
                              fxnum,
                              (int)quant.numerator, (int)quant.denominator
                              )
                );

  }Undo_stop_ignoring_undo_operations();

}


quantitize_options_t Quantitize_get_default_options(void){
  quantitize_options_t options;
  options.quant = make_ratio(1,1);
  /*
  options.quantitize_start = true;
  options.quantitize_end = false;
  options.keep_note_length = false;
  */
  options.type = 3;

  return options;
}

static void Quantitize_Note(
                            struct Blocks *block,
                            struct Notes **notes,
                            struct Notes *note
                            )
{
  bool do_add = quantitize_note(block, note);

  if (do_add)
    ListAddElement3_a(notes,&note->l);        
}

static void Quantitize_range(
                      struct Tracker_Windows *window,
                      struct WBlocks *wblock
){
	Place first,last;

	PlaceSetFirstPos(&first);
	PlaceSetLastPos(wblock->block,&last);

	if( ! wblock->isranged) return;

        struct Tracks *track = ListFindElement1(&wblock->block->tracks->l,wblock->rangex1);

        int tracknum;
	for(tracknum=0;tracknum<=wblock->rangex2-wblock->rangex1;tracknum++){

          struct Notes *new_notes=NULL;

          struct Notes *note=track->notes;

          while(note != NULL) {
            struct Notes *next=NextNote(note);
                        
            if( p_Greater_Or_Equal(note->l.p, wblock->rangey1) && p_Less_Than(note->l.p, wblock->rangey2))
              Quantitize_Note(wblock->block,&new_notes,note);
            else
              ListAddElement3_a(&new_notes,&note->l);
            
            note=next;
          }

          PC_Pause();{
            track->notes=new_notes;          
            LegalizeNotes(wblock->block,track);
          }PC_StopPause(window);

          track=NextTrack(track);
	}

}

void Quantitize_track(
	struct Blocks *block,
	struct Tracks *track
){
	struct Notes *note=NULL;
	struct Notes *notes=NULL;
	struct Notes *temp;
	Place first,last;

	PlaceSetFirstPos(&first);
	PlaceSetLastPos(block,&last);
        
	CopyRange_notes(&note,track->notes,&first,&last);

	while(note!=NULL){
		temp=NextNote(note);
		Quantitize_Note(block,&notes,note);
		note=temp;
	}

        PC_Pause();{
          track->notes=notes;        
          LegalizeNotes(block,track);
        }PC_StopPause(NULL);

}


void Quantitize_block(
	struct Blocks *block
){
	struct Tracks *track=block->tracks;

	while(track!=NULL){
		Quantitize_track(block,track);
		track=NextTrack(track);
	}
}


void Quantitize_track_CurrPos(
	struct Tracker_Windows *window
){
	struct WBlocks *wblock=window->wblock;

        ADD_UNDO(Track_CurrPos(wblock->l.num, wblock->wtrack->l.num));
	Quantitize_track(wblock->block,wblock->wtrack->track);

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
		0,window->wblock->block->num_tracks-1,
		window->wblock->curr_realline
                       ));

	Quantitize_block(window->wblock->block);

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
	if(!window->wblock->isranged) return;

	ADD_UNDO(Range(window,window->wblock,window->wblock->rangex1,window->wblock->rangex2,window->wblock->curr_realline));

	Quantitize_range(window,window->wblock);

	UpdateAndClearSomeTrackReallinesAndGfxWTracks(
		window,
		window->wblock,
		window->wblock->rangex1,
		window->wblock->rangex2
	);
}

void SetQuantitize_CurrPos(
                           struct Tracker_Windows *window
){
    
        Ratio before = root->quantitize_options.quant;
        Ratio after;
                  
        ReqType reqtype = GFX_OpenReq(window, 100, 100, "");

        do{
          GFX_WriteString(reqtype, "Quantitize Value : ");
          GFX_SetString(reqtype, STRING_get_chars(RATIO_as_string(before)));
          
          char temp[1024];
          GFX_ReadString(reqtype, temp, 1010, true);

          if (temp[0]==0){
            printf("no new value\n");
            GFX_CloseReq(window, reqtype);
            return;
          }
          
          after = RATIO_from_string(STRING_create(temp));
        } while (after.denominator==0);
        
        GFX_CloseReq(window, reqtype);

        if (RATIO_literary_equal(root->quantitize_options.quant, after))
          return;

        ADD_UNDO(MainTempo(window, window->wblock));
                       
        root->quantitize_options.quant = after;
}
