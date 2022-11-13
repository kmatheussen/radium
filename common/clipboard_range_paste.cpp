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

/*
static void PasteRange_velocities(
                                  const struct Blocks *block,
                                  struct Velocities **tovelocity,
                                  const struct Velocities *fromvelocity,
                                  const Place *place
){
	if(fromvelocity==NULL)
          return;

        Place lastplace = p_Last_Pos(block);
                
        struct Velocities *velocity = (struct Velocities *)tcopy(fromvelocity);
        PlaceAdd(&velocity->l.p,place);

        if(PlaceGreaterThan(&velocity->l.p,&lastplace))
          return;

	ListAddElement3_a(tovelocity,&velocity->l);

	PasteRange_velocities(block,tovelocity,NextVelocity(fromvelocity),place);
}
*/
static void PasteRange_velocities2(
                                   const struct Blocks *block,
                                   r::VelocityTimeData *to,
                                   const r::VelocityTimeData *from,
                                   const Place *place
                                   )
{
  r::VelocityTimeData::Reader reader(from);
  
  if (reader.size()==0)
    return;
  
  Ratio ratio = ratio_from_place(*place);
  
  r::VelocityTimeData::Writer writer(to, true);
  
  for(r::Velocity velocity : reader){
    
    velocity._time += ratio;
    
    if (velocity._time > block->num_lines)
      break;
    
    writer.add(velocity);
  }
}

static bool PasteRange_FXs(
                           struct Blocks *block,
                           NInt starttrack,
                           const Place *startplace,
                           struct RangeClip *range_clip
){

  SCHEME_eval(
              talloc_format("(paste-fx-range! %d %d (+ %d (/ %d %d)) %d)",
                            block->l.num,
                            starttrack,
                            startplace->line, startplace->counter, startplace->dividor,
                            range_clip->rangenum
                            )
              );


  // Below code should not be necessary, but just in case.
  
  struct Tracks *track=(struct Tracks *)ListFindElement1(&block->tracks->l,starttrack);
  int lokke;
  for(lokke=0;lokke<range_clip->num_tracks;lokke++){

    std::vector<struct FXs*> to_remove;
    
    VECTOR_FOR_EACH(struct FXs *, fxs, &track->fxs){
      r::FXTimeData::Writer writer(fxs->_fxnodes);

      if (!LegalizeFXlines2(block->num_lines, fxs->fx, writer)){
        R_ASSERT_NON_RELEASE(false);
        writer.cancel();
        to_remove.push_back(fxs);        
      }
    }END_VECTOR_FOR_EACH;

    for(auto *fxs : to_remove)
      VECTOR_remove(&track->fxs, fxs);
    
    track=NextTrack(track);
    if(track==NULL) break;
  }


  return true;
}

/*
static void PasteRange_pitches(
                        const struct Blocks *block,
                        struct Pitches **topitch,
                        const struct Pitches *frompitch,
                        const Place *place
){

	if(frompitch==NULL) return;

        Place lastplace = p_Last_Pos(block);

	struct Pitches *pitch=(struct Pitches *)tcopy(frompitch);
	PlaceAdd(&pitch->l.p,place);

	if(PlaceGreaterThan(&pitch->l.p,&lastplace))
          return;

	ListAddElement3_a(topitch,&pitch->l);

	PasteRange_pitches(block,topitch,NextPitch(frompitch),place);
}
*/

static void PasteRange_pitches2(
                                const struct Blocks *block,
                                r::PitchTimeData *to,
                                const r::PitchTimeData *from,
                                const Place *place
                                )
{
  r::PitchTimeData::Reader reader(from);
  
  if (reader.size()==0)
    return;
  
  Ratio ratio = ratio_from_place(*place);
  
  r::PitchTimeData::Writer writer(to, true);
  
  for(r::Pitch pitch : reader){
    
    pitch._time += ratio;
    
    if (pitch._time > block->num_lines)
      break;
    
    writer.add(pitch);
  }
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
	//PlaceAdd(&note->end,place);
        note->end += place2ratio(*place);
        
        Place lastplace = p_Last_Pos(block);
        
	if(PlaceGreaterThan(&note->l.p,&lastplace))
          return;

        Ratio r_lastplace = place2ratio(lastplace);

	if(note->end > r_lastplace) {
          note->end = r_lastplace;
          note->noend=1;
	}

	ListAddElement3_a(&track->notes,&note->l);
        
	//PasteRange_velocities(block,&note->velocities,fromnote->velocities,place);
	PasteRange_velocities2(block,note->_velocities,fromnote->_velocities,place);
	//PasteRange_pitches(block,&note->pitches,fromnote->pitches,place);
	PasteRange_pitches2(block,note->_pitches,fromnote->_pitches,place);

	PasteRange_notes(block,track,NextNote(fromnote),place);
}


static void PasteRange_stops(
	struct Blocks *block,
        r::StopTimeData *to_stop,
        const r::StopTimeData *from_stop,
	//struct Tracks *track,
	//struct Stops *fromstop,
	const Place *place
){

        Ratio lastplace = place2ratio(p_Last_Pos(block));
        

        r::StopTimeData::Reader reader(from_stop);

        r::StopTimeData::Writer writer(to_stop);

        Ratio how_much = place2ratio(*place);
        
        for(r::Stop stop : reader){
          stop._time += how_much;
          
          if (stop._time >= lastplace)
            break;
          
          writer.add(stop);
        }
}


void PasteRange(
	struct Blocks *block,
	NInt tracknum,
	const Place *place,
        struct RangeClip *range_clip
){
	NInt lokke;
	Place p2;

	if(range_clip==NULL) return;

        struct Tracks *track=(struct Tracks *)ListFindElement1(&block->tracks->l,tracknum);
        if (track==NULL)
            return;
        
	PlaceCopy(&p2,place);
	PlaceAdd(&p2,&range_clip->length);
	ClearRange(block,tracknum,tracknum+range_clip->num_tracks-1,place,&p2);

        {
          
          for(lokke=0;lokke<range_clip->num_tracks;lokke++){            
            if (doRangePasteCut())
              StopAllNotesAtPlace(block,track,place);

            PasteRange_notes(block,track,range_clip->notes[lokke],place);

            if (range_clip->stops[lokke] != NULL)
              PasteRange_stops(block,track->stops2,(const r::StopTimeData *)range_clip->stops[lokke],place);

            if (doRangePasteCut()) {
              struct Notes *note = FindNextNote(track, &p2);
              //printf("   p2: %d, NOTE: %d\n",p2.line, note == NULL ? -1 : note->l.p.line);
              if (note !=NULL )
                StopAllNotesAtPlace(block,track,&note->l.p);
            }

            track = NextTrack(track);
            if (track==NULL)
              break;
          }
        }
        
        PasteRange_FXs(block, tracknum, place, range_clip);
}

void PasteRange_CurrPos(
                        struct Tracker_Windows *window,
                        struct RangeClip *range_clip
                        )
{
  
	struct WBlocks *wblock=window->wblock;
	struct Blocks *block=wblock->block;
	NInt curr_track=window->curr_track;
	int curr_realline=wblock->curr_realline;

	if(curr_track<0 || range_clip==NULL) return;

        ADD_UNDO(Range(
                       window,
                       wblock,
                       curr_track,
                       curr_track+range_clip->num_tracks,
                       wblock->curr_realline
                       )
                 );

        Undo_start_ignoring_undo_operations();{
          PC_Pause();{
            PasteRange(block,curr_track,&wblock->reallines[curr_realline]->l.p, range_clip);
          }PC_StopPause(window);
        }Undo_stop_ignoring_undo_operations();

	UpdateAndClearSomeTrackReallinesAndGfxWTracks(
		window,
		window->wblock,
		curr_track,
		curr_track+range_clip->num_tracks-1
	);

        if (doRangePasteScrollDown()){
          int next_realline = curr_realline + 1;
          Place next_place = p_Add(wblock->reallines[curr_realline]->l.p, range_clip->length);
          while(next_realline < wblock->num_reallines && p_Less_Than(wblock->reallines[next_realline]->l.p, next_place))
            next_realline++;
          ScrollEditorDown(window, next_realline - curr_realline, -1);
        }
        
        window->must_redraw = true;
}

