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
#include "windows_proc.h"
#include "list_proc.h"
#include "vector_proc.h"
#include "reallines_proc.h"
#include "notes_proc.h"
#include "placement_proc.h"
#include "notes_legalize_proc.h"
#include "temponodes_legalize_proc.h"
#include "fxlines_legalize_proc.h"
#include "time_proc.h"
#include "visual_proc.h"
#include "undo_blocks_proc.h"
#include "undo_sequencer_proc.h"
#include "player_proc.h"
#include "player_pause_proc.h"
#include "wblocks_proc.h"
#include "block_properties_proc.h"
#include "Beats_proc.h"
#include "seqtrack_proc.h"

#include "lines_proc.h"


extern struct Root *root;
struct Blocks *blocktobelongtoforinsertlines_notes_a_terrible_hack;

void InsertLines_notes(
	void *tonote,
	struct ListHeader3 *l,
	int line,
	int toinsert
){
	struct Notes *note=(struct Notes *)l;
	struct Blocks *block=blocktobelongtoforinsertlines_notes_a_terrible_hack;
	Place p2;

	PlaceSetLastPos(block,&p2);
//	p2.line-=toinsert;

	//	printf("toinsert: %d, note->end.line: %d, p2->line: %d\n",toinsert,note->end.line,p2.line);
	if(note->end.line>=line){
//		printf("block: %d, note->end.line: %d, p2->line: %d\n",block->l.num,note->end.line,p2.line);

		if(PlaceGreaterOrEqual(&note->end,&p2) && note->l.p.line<line){

			PlaceSetLastPos(block,&p2);
			PlaceCopy(&note->end,&p2);
			note->noend=1;
		}else{
			note->end.line+=toinsert;
                        note->end.line=R_MAX(note->end.line,line);
		}

		List_InsertLines3(&note->velocities,&note->velocities->l,line,toinsert,NULL);
		List_InsertLines3(&note->pitches,&note->pitches->l,line,toinsert,NULL);
	}
}


void InsertLines_localzooms(
	void *tolocalzoom,
	struct ListHeader3 *l,
	int line,
	int toinsert
){
	struct LocalZooms *localzoom=(struct LocalZooms *)l;

	if(localzoom->Tline>=line && localzoom->level==0){
		localzoom->zoomline+=toinsert;
	}
	if(localzoom->uplevel!=NULL){
		List_InsertLines3(
			&localzoom->uplevel,
			&localzoom->uplevel->l,
			line,toinsert,
			InsertLines_localzooms
		);
	}
}


/* Argh. This one was difficult, I don`t think it works correctly
   in all situations yet either.
*/
void InsertLines(
	struct Blocks *block,
	int line,
	int toinsert
){
	int lokke;
	struct LocalZooms *localzoom;
	struct Tracker_Windows *window=root->song->tracker_windows;
	struct WBlocks *wblock;
	struct Tracks *track=block->tracks;

	int num_lines=block->num_lines;

	if(line>num_lines+toinsert){
		toinsert=line-num_lines;
	}

        if (line==num_lines-1) { // special case
          Block_Properties(block, block->num_tracks, block->num_lines + toinsert);
          return;
        }
        
	if( line<0 || line>=num_lines) return;

	if(toinsert==0 || num_lines+toinsert<2 || num_lines+toinsert>=MAX_UINT32) return;

        
        PC_Pause();{
        
          blocktobelongtoforinsertlines_notes_a_terrible_hack=block;
          
          block->num_lines=num_lines+toinsert;
          
          List_InsertLines3(&block->temponodes,block->temponodes->l.next,line,toinsert,NULL);
          LegalizeTempoNodes(block);
          List_InsertLines3(&block->signatures,&block->signatures->l,line,toinsert,NULL);
          List_InsertLines3(&block->lpbs,&block->lpbs->l,line,toinsert,NULL);
          List_InsertLines3(&block->tempos,&block->tempos->l,line,toinsert,NULL);

          TIME_block_num_lines_have_changed(block);

          while(track!=NULL){
            List_InsertLines3(&track->notes,&track->notes->l,line,toinsert,InsertLines_notes);
            LegalizeNotes(block,track);
            List_InsertLines3(&track->stops,&track->stops->l,line,toinsert,NULL);
            
            VECTOR_FOR_EACH(struct FXs *fxs, &track->fxs){
              List_InsertLines3(&fxs->fxnodelines,&fxs->fxnodelines->l,line,toinsert,NULL);
            }END_VECTOR_FOR_EACH;
            
            LegalizeFXlines(block,track);
            track=NextTrack(track);
          }

          while(window!=NULL){
            wblock=ListFindElement1(&window->wblocks->l,block->l.num);
            List_InsertLines3(
                              &wblock->localzooms,
                              &wblock->localzooms->l,
                              line,
                              toinsert,
                              InsertLines_localzooms
                              //			NULL
                              );
            for(lokke=line;lokke<line+toinsert;lokke++){
              localzoom=talloc(sizeof(struct LocalZooms));
              localzoom->Tline=lokke;
              localzoom->Tdividor=1;
              localzoom->zoomline=lokke;
              ListAddElement3(&wblock->localzooms,&localzoom->l);
            }
            UpdateWBlockWidths(window,wblock);
            UpdateRealLines_dont_change_curr_realline(window, wblock);
            UpdateReallinesDependens(window,wblock);
            if(wblock->curr_realline>=wblock->num_reallines){
              wblock->curr_realline=wblock->num_reallines-1;
            }
            window=NextWindow(window);
          }
          
          blocktobelongtoforinsertlines_notes_a_terrible_hack=NULL;

        }PC_StopPause(window);
}


void InsertLines_CurrPos(
                         struct Tracker_Windows *window,
                         int toinsert
){
	struct WBlocks *wblock=window->wblock;
	int curr_realline=wblock->curr_realline;
	int curr_line=wblock->reallines[curr_realline]->Tline;
	int num_lines=wblock->block->num_lines;

        if(toinsert==0)
	  toinsert=GFX_GetInteger(window,NULL,"Number of lines to insert\n(number can be negative): ",-(num_lines-curr_line),10000,true);

	if(toinsert==-(num_lines-curr_line)-1) return;

        UNDO_OPEN();
        ADD_UNDO(Sequencer());
	ADD_UNDO(Block_CurrPos(window));
        UNDO_CLOSE();

	InsertLines(window->wblock->block,curr_line,toinsert);

	window=root->song->tracker_windows;
	while(window!=NULL){
		window->must_redraw = true;
		window=NextWindow(window);
	}
}





