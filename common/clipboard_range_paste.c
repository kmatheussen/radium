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
#include "clipboard_range_calc_proc.h"
#include "placement_proc.h"
#include "list_proc.h"
#include "clipboard_range.h"
#include "clipboard_range_copy_proc.h"
#include "clipboard_range_cut_proc.h"
#include "wtracks_proc.h"
#include "fxlines_legalize_proc.h"
#include "undo.h"
#include "undo_range_proc.h"
#include "undo_blocks_proc.h"
#include "player_proc.h"
#include "player_pause_proc.h"
#include "notes_proc.h"
#include "cursor_updown_proc.h"
#include "../embedded_scheme/scheme_proc.h"

#include "../api/api_proc.h"

#include "clipboard_range_paste_proc.h"



extern struct Range *range;

static void PasteRange_velocities(
                                  const struct Blocks *block,
                                  struct Velocities **tovelocity,
                                  const struct Velocities *fromvelocity,
                                  const Place *place
){
	if(fromvelocity==NULL)
          return;

        Place lastplace = p_Last_Pos(block);
                
        struct Velocities *velocity = tcopy(fromvelocity, sizeof(struct Velocities));
        PlaceAdd(&velocity->l.p,place);

        if(PlaceGreaterThan(&velocity->l.p,&lastplace))
          return;

	ListAddElement3_a(tovelocity,&velocity->l);

	PasteRange_velocities(block,tovelocity,NextVelocity(fromvelocity),place);
}

static bool PasteRange_FXs(
                           struct Blocks *block,
                           NInt starttrack,
                           const Place *startplace
){

  SCHEME_eval(
              talloc_format("(paste-fx-range! %d %d (+ %d (/ %d %d)))",
                            block->l.num,
                            starttrack,
                            startplace->line, startplace->counter, startplace->dividor
                            )
              );


  struct Tracks *track=ListFindElement1(&block->tracks->l,starttrack);
  int lokke;
  for(lokke=0;lokke<range->num_tracks;lokke++){
    LegalizeFXlines(block,track); // should not be not necessary though.
    track=NextTrack(track);
    if(track==NULL) break;
  }


  return true;
}


static void PasteRange_pitches(
                        const struct Blocks *block,
                        struct Pitches **topitch,
                        const struct Pitches *frompitch,
                        const Place *place
){

	if(frompitch==NULL) return;

        Place lastplace = p_Last_Pos(block);

	struct Pitches *pitch=tcopy(frompitch, sizeof(struct Pitches));
	PlaceAdd(&pitch->l.p,place);

	if(PlaceGreaterThan(&pitch->l.p,&lastplace))
          return;

	ListAddElement3_a(topitch,&pitch->l);

	PasteRange_pitches(block,topitch,NextPitch(frompitch),place);
}

static void PasteRange_notes(
                      const struct Blocks *block,
                      struct Tracks *track,
                      const struct Notes *fromnote,
                      const Place *place
                      )
{

	if(fromnote==NULL) return;

	struct Notes *note=CopyNote(fromnote);
	PlaceAdd(&note->l.p,place);
	PlaceAdd(&note->end,place);
        
        Place lastplace = p_Last_Pos(block);

	if(PlaceGreaterThan(&note->l.p,&lastplace))
          return;

	if(PlaceGreaterThan(&note->end,&lastplace)){
          PlaceSetLastPos(block,&note->end);
          note->noend=1;
	}

	ListAddElement3_a(&track->notes,&note->l);
        
	PasteRange_velocities(block,&note->velocities,fromnote->velocities,place);
	PasteRange_pitches(block,&note->pitches,fromnote->pitches,place);

	PasteRange_notes(block,track,NextNote(fromnote),place);
}


static void PasteRange_stops(
	struct Blocks *block,
	struct Tracks *track,
	struct Stops *fromstop,
	const Place *place
){

	if(fromstop==NULL) return;

        Place lastplace = p_Last_Pos(block);

	struct Stops *stop=tcopy(fromstop, sizeof(struct Stops));
	PlaceAdd(&stop->l.p,place);

	if(PlaceGreaterThan(&stop->l.p,&lastplace))
          return;

	ListAddElement3_a(&track->stops,&stop->l);

	PasteRange_stops(block,track,NextStop(fromstop),place);
}


static void PasteRange(
	struct Blocks *block,
	NInt tracknum,
	const Place *place
){
	NInt lokke;
	Place p2;

	if(range==NULL) return;

        struct Tracks *track=ListFindElement1(&block->tracks->l,tracknum);
        if (track==NULL)
            return;
        
	PlaceCopy(&p2,place);
	PlaceAdd(&p2,&range->length);
	CutRange(block,tracknum,tracknum+range->num_tracks-1,place,&p2);

        {
          
          for(lokke=0;lokke<range->num_tracks;lokke++){            
            if (doRangePasteCut())
              StopAllNotesAtPlace(block,track,place);

            PasteRange_notes(block,track,range->notes[lokke],place);
            PasteRange_stops(block,track,range->stops[lokke],place);

            if (doRangePasteCut()) {
              struct Notes *note = FindNextNote(track, &p2);
              printf("   p2: %d, NOTE: %d\n",p2.line, note == NULL ? -1 : note->l.p.line);
              if (note !=NULL )
                StopAllNotesAtPlace(block,track,&note->l.p);
            }

            track = NextTrack(track);
            if (track==NULL)
              break;
          }
        }
        
        PasteRange_FXs(block, tracknum, place);
}

void PasteRange_CurrPos(
	struct Tracker_Windows *window
){

	struct WBlocks *wblock=window->wblock;
	struct Blocks *block=wblock->block;
	NInt curr_track=window->curr_track;
	int curr_realline=wblock->curr_realline;

	if(curr_track<0 || range==NULL) return;

        ADD_UNDO(Range(
                       window,
                       wblock,
                       curr_track,
                       curr_track+range->num_tracks-1,
                       wblock->curr_realline
                       )
                 );

        Undo_start_ignoring_undo_operations();{
          PC_Pause();{
            PasteRange(block,curr_track,&wblock->reallines[curr_realline]->l.p);
          }PC_StopPause(window);
        }Undo_stop_ignoring_undo_operations();

	UpdateAndClearSomeTrackReallinesAndGfxWTracks(
		window,
		window->wblock,
		curr_track,
		curr_track+range->num_tracks-1
	);

        if (doRangePasteScrollDown()){
          int next_realline = curr_realline + 1;
          Place next_place = p_Add(wblock->reallines[curr_realline]->l.p, range->length);
          while(next_realline < wblock->num_reallines && p_Less_Than(wblock->reallines[next_realline]->l.p, next_place))
            next_realline++;
          ScrollEditorDown(window, next_realline - curr_realline, NULL);
        }
        
        window->must_redraw = true;
}

