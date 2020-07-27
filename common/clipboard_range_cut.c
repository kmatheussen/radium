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
#include "vector_proc.h"
#include "clipboard_range_copy_proc.h"
#include "clipboard_range_calc_proc.h"
#include "wtracks_proc.h"
#include "undo.h"
#include "undo_range_proc.h"
#include "undo_blocks_proc.h"
#include "player_proc.h"
#include "player_pause_proc.h"
#include "../embedded_scheme/scheme_proc.h"

#include "clipboard_range_cut_proc.h"



void ClearRange_notes(
                    struct Notes **tonote,
                    const struct Notes *fromnote,
                    const Place *p1,
                    const Place *p2
                    )
{
	if(fromnote==NULL) return;

	const struct Notes *next=NextNote(fromnote);

	if(PlaceLessThan(&fromnote->l.p,p1)){
		ClearRange_notes(tonote,next,p1,p2);
		return;
	}

	if(PlaceGreaterOrEqual(&fromnote->l.p,p2)) return;

	ListRemoveElement3(tonote,&fromnote->l);

	ClearRange_notes(tonote,next,p1,p2);
}


static void ClearRange_stops(
                    struct Stops **tostop,
                    const struct Stops *fromstop,
                    const Place *p1,
                    const Place *p2
                    )
{
	struct Stops *next;
	if(fromstop==NULL) return;

	next=NextStop(fromstop);

	if(PlaceLessThan(&fromstop->l.p,p1)){
		ClearRange_stops(tostop,next,p1,p2);
		return;
	}

	if(PlaceGreaterOrEqual(&fromstop->l.p,p2)) return;

	ListRemoveElement3(tostop,&fromstop->l);

	ClearRange_stops(tostop,next,p1,p2);
}


void ClearRange(
              struct Blocks *block,
              NInt starttrack,
              NInt endtrack,
              const Place *p1,
              const Place *p2
){
	struct Tracks *track;
	int lokke;

        endtrack = R_MIN(block->num_tracks-1, endtrack);
        if (endtrack < starttrack)
          return;
        
	track=ListFindElement1(&block->tracks->l,starttrack);

        PC_Pause();{

          for(lokke=0;lokke<=endtrack-starttrack;lokke++){
            ClearRange_notes(&track->notes,track->notes,p1,p2);
            ClearRange_stops(&track->stops,track->stops,p1,p2);
            track=NextTrack(track);
            if(track==NULL) break;
          }
          
          const Place *startplace = p1;
          const Place *endplace = p2;

          SCHEME_eval(
                      talloc_format("(cut-fx-range! %d %d %d (+ %d (/ %d %d)) (+ %d (/ %d %d)))",
                                    block->l.num,
                                    starttrack,
                                    endtrack,
                                    startplace->line, startplace->counter, startplace->dividor,
                                    endplace->line, endplace->counter, endplace->dividor
                                  )
                      );
          
        }PC_StopPause(NULL);
}

void ClearRange2(
              struct Blocks *block,
              const range_t range)
{
  ClearRange(block, range.x1, range.x2, &range.y1, &range.y2);
}


void CutRange(
              struct Blocks *block,
              const range_t range,
              int rangenum
              )
{
	if(! range.enabled)
          return;

	CopyRange(block, range, rangenum);

        ClearRange2(block, range);
}


void CutRange_CurrPos(
                      struct Tracker_Windows *window,
                      int rangenum
){
        struct WBlocks *wblock = window->wblock;

	if( ! wblock->range.enabled)
          return;
        
        ADD_UNDO(Range(
                       window,
                       window->wblock,
                       window->wblock->range.x1,
                       window->wblock->range.x2+1,
                       window->wblock->curr_realline
                       ));

        CutRange(wblock->block, wblock->range, rangenum);

        wblock->range.enabled = false;
        
        window->must_redraw = true;
}






