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

#include "quantitize_proc.h"


extern struct Root *root;




enum QuantitizeNoteEndType{
  QNE_MOVE_START_TO_PREV_POINT,
  QNE_MOVE_END_TO_NEXT_POINT,
  QNE_KEEP_LENGTH,
  QNE_DONT_QUANTITIZE,
  QNE_DELETE
};

enum QuantitizeNoteEndType get_quant_type(void){
  return QNE_MOVE_END_TO_NEXT_POINT;
}

void Quantitize_Note(
	struct Blocks *block,
	struct Notes **notes,
	struct Notes *note,
	const Place quant,
        enum QuantitizeNoteEndType quant_type
){
        Place lastpos = p_Last_Pos(block);

	if(p_Equal(lastpos,note->l.p))
          return;

        Place start_org = note->l.p;
	Place end_org = note->end;

        
	note->l.p = p_Quantitize(note->l.p,quant);

        if (p_Equal(lastpos, note->l.p))
          note->l.p = start_org;


        note->end = p_Quantitize(note->end,quant);

        if (p_Less_Or_Equal(note->end, note->l.p)) {

          switch (quant_type) {

            case QNE_MOVE_START_TO_PREV_POINT:
              while (p_Less_Or_Equal(note->end, note->l.p))
                note->l.p = p_Sub(note->l.p, quant);

              if( ! p_Less_Than(note->l.p, place(0,0,1)))
                note->l.p = place(0,0,1);

              break;

            case QNE_MOVE_END_TO_NEXT_POINT:
              while (p_Less_Or_Equal(note->end, note->l.p))
                note->end = p_Add(note->end, quant);
              break;
              
            case QNE_KEEP_LENGTH:
              note->end = p_Add(note->l.p, p_Sub(note->end, note->l.p));
              break;

            case QNE_DONT_QUANTITIZE:
              note->end = end_org;
              break;
              
            case QNE_DELETE:
              return;
          }

        }

	if (p_Less_Or_Equal(note->end, note->l.p))
          return;

	if( ! PlaceLegal(block,&note->end))
          return;

	ListAddElement3(notes,&note->l);
}

void Quantitize_range(
	struct WBlocks *wblock,
	Place quant
){
	struct Tracks *track;
	struct Notes *notes=NULL;
	struct Notes *note=NULL;
	struct Notes *temp;
	struct LocalZooms *realline1,*realline2;
	int lokke;
	Place first,last;

	PlaceSetFirstPos(&first);
	PlaceSetLastPos(wblock->block,&last);

	if( ! wblock->isranged) return;

	track=ListFindElement1(&wblock->block->tracks->l,wblock->rangex1);

	realline1=wblock->reallines[wblock->rangey1];
	realline2=wblock->reallines[wblock->rangey2];

	for(lokke=0;lokke<=wblock->rangex2-wblock->rangex1;lokke++){
		note=NULL;
		CopyRange_notes(&note,track->notes,&first,&last);
		for(;;){
			if(note==NULL) break;
			if(PlaceGreaterThan(&note->l.p,&realline2->l.p)) break;
			temp=NextNote(note);
			if(PlaceGreaterOrEqual(&note->l.p,&realline1->l.p)){
                          Quantitize_Note(wblock->block,&notes,note,quant,get_quant_type());
			}
			note=temp;
		}
		track->notes=notes;
		notes=NULL;
		track=NextTrack(track);

		LegalizeNotes(wblock->block,track);
	}

}

void Quantitize_track(
	struct Blocks *block,
	struct Tracks *track,
	Place quant
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
		Quantitize_Note(block,&notes,note,quant,get_quant_type());
		note=temp;
	}
	track->notes=notes;

	LegalizeNotes(block,track);
}


void Quantitize_block(
	struct Blocks *block,
	Place quant
){
	struct Tracks *track=block->tracks;

	while(track!=NULL){
		Quantitize_track(block,track,quant);
		track=NextTrack(track);
	}
}


static Place get_quant(void){
  Place ret = {0, root->quantitize_numerator, root->quantitize_denominator};
  return ret;
}

void Quantitize_track_CurrPos(
	struct Tracker_Windows *window
){
	struct WBlocks *wblock=window->wblock;

	PlayStop();

	Undo_Track_CurrPos(window);
	Quantitize_track(wblock->block,wblock->wtrack->track,get_quant());

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

	Undo_Range(
		window,
		window->wblock->block,
		0,window->wblock->block->num_tracks-1,
		window->wblock->curr_realline
	);

	Quantitize_block(window->wblock->block,get_quant());

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

	Undo_Range(window,window->wblock->block,window->wblock->rangex1,window->wblock->rangex2,window->wblock->curr_realline);

	Quantitize_range(window->wblock,get_quant());

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
    
        Place before = {0, root->quantitize_numerator, root->quantitize_denominator};
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

        if (root->quantitize_numerator == after.counter && root->quantitize_denominator == after.dividor)
          return;

        Undo_MainTempo(window, window->wblock);
                       
        root->quantitize_numerator = after.counter;
        root->quantitize_denominator = after.dividor;
}
