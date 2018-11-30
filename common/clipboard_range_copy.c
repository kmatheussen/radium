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

#include <string.h>

#include "nsmtracker.h"
#include "clipboard_range_calc_proc.h"
#include "placement_proc.h"
#include "list_proc.h"
#include "vector_proc.h"
#include "clipboard_range.h"
#include "wtracks_proc.h"
#include "notes_proc.h"
#include "patch_proc.h"
#include "../embedded_scheme/scheme_proc.h"

#include "clipboard_range_copy_proc.h"



extern struct Range *range;


static void CopyRange_velocities(
                          struct Velocities **tovelocity,
                          const struct Velocities *fromvelocity,
                          const Place *p1,
                          const Place *p2
){
	struct Velocities *velocity;

	if(fromvelocity==NULL) return;

	if(PlaceGreaterOrEqual(&fromvelocity->l.p,p2)) return;

	velocity=tcopy(fromvelocity, sizeof(struct Velocities));

	PlaceSub(&velocity->l.p,p1);

	ListAddElement3_a(tovelocity,&velocity->l);

	CopyRange_velocities(tovelocity,NextVelocity(fromvelocity),p1,p2);
}


static void CopyRange_pitches(
                       struct Pitches **topitch,
                       const struct Pitches *frompitch,
                       const Place *p1,
                       const Place *p2
){
	struct Pitches *pitch;

	if(frompitch==NULL) return;

	if(PlaceGreaterOrEqual(&frompitch->l.p,p2)) return;

	pitch=tcopy(frompitch, sizeof(struct Pitches));

	PlaceSub(&pitch->l.p,p1);

	ListAddElement3_a(topitch,&pitch->l);

	CopyRange_pitches(topitch,NextPitch(frompitch),p1,p2);
}


void CopyRange_notes(
                     struct Notes **tonote,
                     const struct Notes *fromnote,
                     const Place *p1,
                     const Place *p2
){

	struct Notes *note;

	if(fromnote==NULL){
		return;
	}

        R_ASSERT_RETURN_IF_FALSE(*tonote != fromnote);

	if(PlaceLessThan(&fromnote->l.p,p1)){
		CopyRange_notes(tonote,NextNote(fromnote),p1,p2);
		return;
	}

	if(PlaceGreaterOrEqual(&fromnote->l.p,p2)){
		return;
	}

	note=CopyNote(fromnote);
        note->pitches = NULL;
        note->velocities = NULL;
        NOTE_init(note);

	PlaceSub(&note->l.p,p1);
	PlaceSub(&note->end,p1);

	ListAddElement3_a(tonote,&note->l);

	CopyRange_velocities(&note->velocities,fromnote->velocities,p1,p2);
	CopyRange_pitches(&note->pitches,fromnote->pitches,p1,p2);

	CopyRange_notes(tonote,NextNote(fromnote),p1,p2);
}


void CopyRange_stops(
                     struct Stops **tostop,
                     const struct Stops *fromstop,
                     const Place *p1,
                     const Place *p2
){
	if(fromstop==NULL) return;

	if(PlaceLessThan(&fromstop->l.p,p1)){
		CopyRange_stops(tostop,NextStop(fromstop),p1,p2);
		return;
	}

	if(PlaceGreaterOrEqual(&fromstop->l.p,p2)) return;

	struct Stops *stop=tcopy(fromstop, sizeof(struct Stops));
	PlaceSub(&stop->l.p,p1);

	ListAddElement3_a(tostop,&stop->l);

	CopyRange_stops(tostop,NextStop(fromstop),p1,p2);

}

static void add_fxnodeline(
                           struct FXNodeLines **tofxnodeline,
                           const struct FXNodeLines *fromfxnodeline,
                           Place subtract
){               
  struct FXNodeLines *fxnodeline=tcopy(fromfxnodeline, sizeof(struct FXNodeLines));
  
  fxnodeline->l.p = p_Sub(fxnodeline->l.p, subtract);
  
  ListAddElement3_a(tofxnodeline,&fxnodeline->l);
}


static void add_scaled_fxnodeline(
                                  struct FXNodeLines **tofxnodeline,
                                  const struct FXNodeLines *nodeline1,
                                  const struct FXNodeLines *nodeline2,
                                  Place p,
                                  Place subtract
){
  struct FXNodeLines fxnodeline = *nodeline1;
  fxnodeline.l.p = p;

  R_ASSERT(p_Greater_Or_Equal(p,              nodeline1->l.p));
  R_ASSERT(p_Greater_Or_Equal(nodeline2->l.p, p));
  
  if (nodeline1->logtype != LOGTYPE_HOLD)
    fxnodeline.val = scale(p_float(p), p_float(nodeline1->l.p), p_float(nodeline2->l.p), nodeline1->val, nodeline2->val);

  add_fxnodeline(tofxnodeline, &fxnodeline, subtract);
}



static void CopyRange_fxnodelines(
                                  struct FXNodeLines **tofxnodeline,
                                  const struct FXNodeLines *fromfxnodeline,
                                  const struct FXNodeLines *last,
                                  Place p1,
                                  Place p2
){
	if(fromfxnodeline==NULL) return;

	if(p_Less_Than(fromfxnodeline->l.p, p1)){
          CopyRange_fxnodelines(tofxnodeline,
                                NextFXNodeLine(fromfxnodeline),
                                fromfxnodeline,
                                p1,
                                p2);
          return;
	}

        if (last!=NULL)
          if (p_Less_Than(last->l.p, p1))
            add_scaled_fxnodeline(tofxnodeline, last, fromfxnodeline, p1, p1);

        if(p_Greater_Or_Equal(fromfxnodeline->l.p, p2)) {
          if (last!=NULL)
            add_scaled_fxnodeline(tofxnodeline, last, fromfxnodeline, p2, p1);
          return;
        }

        add_fxnodeline(tofxnodeline, fromfxnodeline, p1);

	CopyRange_fxnodelines(tofxnodeline,
                              NextFXNodeLine(fromfxnodeline),
                              fromfxnodeline,
                              p1,
                              p2);
}


void CopyRange_fxs(
                   vector_t *tofxs,
                   const vector_t *das_fromfxs,
                   const Place *p1,
                   const Place *p2
                   )
{
  VECTOR_FOR_EACH(const struct FXs *fromfxs, das_fromfxs){

        R_ASSERT_RETURN_IF_FALSE(fromfxs->fx->patch->is_usable);
        
#if 0
        // This thing should perhaps be moved into co_CB_PasteTrack.
        if (!fromfxs->fx->patch->is_usable) {
          fromfxs->fx->patch = PATCH_create_audio(NULL, NULL, fromfxs->fx->patch->name, fromfxs->fx->patch->state);
          R_ASSERT_RETURN_IF_FALSE(fromfxs->fx->patch->patchdata != NULL);
        }
#endif
        
	struct FXs *fxs=talloc(sizeof(struct FXs));

	fxs->fx=tcopy(fromfxs->fx, sizeof(struct FX)); // Why not just reference the existing fx? (because fx is modified)

	VECTOR_push_back(tofxs,fxs);

	CopyRange_fxnodelines(&fxs->fxnodelines,fromfxs->fxnodelines,NULL,*p1,*p2);

  }END_VECTOR_FOR_EACH;
}


void CopyRange(
               struct WBlocks *wblock
){
	NInt num_tracks;
	int lokke;

	if( ! wblock->isranged) return;

        int starttrack = wblock->rangex1;
        int endtrack = R_MIN(wblock->block->num_tracks-1, wblock->rangex2);
        if (endtrack < starttrack)
          return;

	range=talloc(sizeof(struct Range));
	range->num_tracks = num_tracks = wblock->rangex2-wblock->rangex1+1;

	range->notes=talloc((int)sizeof(struct Notes *)*num_tracks);
        range->stops=talloc((int)sizeof(struct Stops *)*num_tracks);
	//range->instruments=talloc((size_t)(sizeof(struct Instruments *)*num_tracks));
	range->fxs=talloc((int)sizeof(vector_t)*num_tracks);

	GetRangePlaceLength(&range->length,wblock);
        
	const struct Tracks *track=ListFindElement1(&wblock->block->tracks->l,wblock->rangex1);

	for(lokke=0;lokke<=wblock->rangex2-wblock->rangex1;lokke++){
          //range->instruments[lokke]=track->instrument;
          CopyRange_notes(&range->notes[lokke], track->notes, &wblock->rangey1, &wblock->rangey2);
          CopyRange_stops(&range->stops[lokke], track->stops, &wblock->rangey1, &wblock->rangey2);
          CopyRange_fxs(&range->fxs[lokke], &track->fxs,      &wblock->rangey1, &wblock->rangey2);

          track=NextTrack(track);
          if (track==NULL)
            break;
	}

        
        const Place *startplace = &wblock->rangey1;
        const Place *endplace = &wblock->rangey2;
        
        SCHEME_eval(
                    talloc_format("(copy-fx-range! %d %d %d (+ %d (/ %d %d)) (+ %d (/ %d %d)))",
                                  wblock->block->l.num,
                                  starttrack,
                                  endtrack,
                                  startplace->line, startplace->counter, startplace->dividor,
                                  endplace->line, endplace->counter, endplace->dividor
                                  )
                    );

	wblock->isranged=false;
}

void CopyRange_CurrPos(
                       struct Tracker_Windows *window
                       )
{
	CopyRange(window->wblock);

	UpdateAndClearSomeTrackReallinesAndGfxWTracks(
		window,
		window->wblock,
		window->wblock->rangex1,
		window->wblock->rangex2
	);

}




