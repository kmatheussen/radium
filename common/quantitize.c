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
#include "wtracks_proc.h"
#include "notes_legalize_proc.h"
#include "clipboard_range_copy_proc.h"
#include "player_proc.h"
#include "visual_proc.h"
#include "../Qt/Rational.h"
#include "../embedded_scheme/scheme_proc.h"
#include "settings_proc.h"

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

quantitize_options_t Quantitize_get_default_options(void){
  quantitize_options_t options;
  options.quant = ratio(1,1);
  options.quantitize_start = true;
  options.quantitize_end = false;
  options.keep_note_length = false;
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
    ListAddElement3(notes,&note->l);        
}

void Quantitize_range(
                      struct WBlocks *wblock
){
	Place first,last;

	PlaceSetFirstPos(&first);
	PlaceSetLastPos(wblock->block,&last);

	if( ! wblock->isranged) return;


	struct LocalZooms *realline1 = wblock->reallines[wblock->rangey1];
	struct LocalZooms *realline2 = wblock->reallines[wblock->rangey2];

        struct Tracks *track = ListFindElement1(&wblock->block->tracks->l,wblock->rangex1);

        int tracknum;
	for(tracknum=0;tracknum<=wblock->rangex2-wblock->rangex1;tracknum++){

          struct Notes *new_notes=NULL;

          struct Notes *note=track->notes;

          while(note != NULL) {
            struct Notes *next=NextNote(note);
                        
            if( p_Greater_Or_Equal(note->l.p, realline1->l.p) && p_Less_Than(note->l.p, realline2->l.p))
              Quantitize_Note(wblock->block,&new_notes,note);
            else
              ListAddElement3(&new_notes,&note->l);
            
            note=next;
          }

          track->notes=new_notes;          
          LegalizeNotes(wblock->block,track);

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
	track->notes=notes;

	LegalizeNotes(block,track);
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

	PlayStop();

	ADD_UNDO(Track_CurrPos(window));
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

	PlayStop();

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

	PlayStop();

	ADD_UNDO(Range(window,window->wblock,window->wblock->rangex1,window->wblock->rangex2,window->wblock->curr_realline));

	Quantitize_range(window->wblock);

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
    
        Place before = {0, root->quantitize_options.quant.numerator, root->quantitize_options.quant.denominator};
        Place after;
                  
        ReqType reqtype = GFX_OpenReq(window, 100, 100, "Quantitize");

        do{
          GFX_WriteString(reqtype, "Quantitize Value : ");
          GFX_SetString(reqtype, get_string_from_rational(&before));
          
          char temp[1024];
          GFX_ReadString(reqtype, temp, 1010);

          if (temp[0]==0){
            printf("no new value\n");
            GFX_CloseReq(window, reqtype);
            return;
          }
          
          after = get_rational_from_string(temp);
        } while (after.dividor==0);
        
        GFX_CloseReq(window, reqtype);

        if (root->quantitize_options.quant.numerator == (int)after.counter && root->quantitize_options.quant.denominator == (int)after.dividor)
          return;

        ADD_UNDO(MainTempo(window, window->wblock));
                       
        root->quantitize_options.quant.numerator = after.counter;
        root->quantitize_options.quant.denominator = after.dividor;
}
