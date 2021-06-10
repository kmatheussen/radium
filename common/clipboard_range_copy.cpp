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
#include "TimeData.hpp"
#include "FX.hpp"
#include "clipboard_range_calc_proc.h"
#include "placement_proc.h"
#include "list_proc.h"
#include "vector_proc.h"
#include "clipboard_range.h"
#include "wtracks_proc.h"
#include "notes_proc.h"
#include "patch_proc.h"
#include "fxlines_proc.h"
#include "TallocWithDestructor.hpp"
#include "fxlines_legalize_proc.h"

#include "../embedded_scheme/scheme_proc.h"

#include "clipboard_range_copy_proc.h"


/*
static void CopyRange_velocities(
                          struct Velocities **tovelocity,
                          const struct Velocities *fromvelocity,
                          const Place *p1,
                          const Place *p2
){
	struct Velocities *velocity;

	if(fromvelocity==NULL) return;

	if(PlaceGreaterOrEqual(&fromvelocity->l.p,p2)) return;

	velocity=(struct Velocities*)tcopy(fromvelocity);

	PlaceSub(&velocity->l.p,p1);

	ListAddElement3_a(tovelocity,&velocity->l);

	CopyRange_velocities(tovelocity,NextVelocity(fromvelocity),p1,p2);
}
*/


static void CopyRange_velocities2(
                                  r::TimeData<r::Velocity> *to,
                                  const r::TimeData<r::Velocity> *from,
                                  const Place *p1,
                                  const Place *p2
                                  )
{
  r::TimeData<r::Velocity>::Reader reader(from);

  r::TimeData<r::Velocity>::Writer writer(to, true);
  
  Ratio start = place2ratio(*p1);
  Ratio end = place2ratio(*p2);
  
  R_ASSERT_NON_RELEASE(end>=start);
  
  for(r::Velocity velocity : reader){
    if (velocity._time >= start) {
      if (velocity._time >= end)
        break;
      velocity._time -= start;
      writer.add(velocity);
    }
  }
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

	pitch=(struct Pitches*)tcopy(frompitch);

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

	struct Notes *note=CopyNote(fromnote);
        note->pitches = NULL;
        //note->velocities = NULL;

	PlaceSub(&note->l.p,p1);

        note->end -= place2ratio(*p1);
	//PlaceSub(&note->end,p1);

	ListAddElement3_a(tonote,&note->l);

	//CopyRange_velocities(&note->velocities,fromnote->velocities,p1,p2);
	CopyRange_velocities2(note->_velocities,fromnote->_velocities,p1,p2);
	CopyRange_pitches(&note->pitches,fromnote->pitches,p1,p2);

	CopyRange_notes(tonote,NextNote(fromnote),p1,p2);
}


void CopyRange_stops(
                     r::TimeData<r::Stop> *to_stop,
                     //struct Stops **tostop,
                     const r::TimeData<r::Stop> *from_stop,
                     //const struct Stops *from_stop,
                     const Place *p1,
                     const Place *p2
){

  r::TimeData<r::Stop>::Reader reader(from_stop);

  r::TimeData<r::Stop>::Writer writer(to_stop, true);

  Ratio start = place2ratio(*p1);
  Ratio end = place2ratio(*p2);

  R_ASSERT_NON_RELEASE(end>=start);

  for(r::Stop stop : reader){
    if (stop._time >= start) {
      if (stop._time >= end)
        break;
      stop._time -= start;
      writer.add(stop);
    }
  }

  /*
  if(reader.size()==NULL) return;

	if(PlaceLessThan(&fromstop->l.p,p1)){
		CopyRange_stops(tostop,NextStop(fromstop),p1,p2);
		return;
	}

	if(PlaceGreaterOrEqual(&fromstop->l.p,p2)) return;

	struct Stops *stop=tcopy(fromstop);
	PlaceSub(&stop->l.p,p1);

	ListAddElement3_a(tostop,&stop->l);

	CopyRange_stops(tostop,NextStop(fromstop),p1,p2);
  */

}

/*
static void add_fxnodeline(
                           struct FXNodeLines **tofxnodeline,
                           const struct FXNodeLines *fromfxnodeline,
                           Place subtract
){               
  struct FXNodeLines *fxnodeline=(struct FXNodeLines *)tcopy(fromfxnodeline);
  
  fxnodeline->l.p = p_Sub(fxnodeline->l.p, subtract);
  
  ListAddElement3_a(tofxnodeline,&fxnodeline->l);
}
*/

static void add_fxnodeline2(r::TimeData<r::FXNode>::Writer &writer,
                            r::FXNode node,
                            const Ratio &subtract)
{
  node._time = node._time - subtract;
  writer.add(node);
}

/*
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
*/

static void add_scaled_fxnodeline2(const struct FX &fx,
                                   r::TimeData<r::FXNode>::Writer &writer,
                                   const r::FXNode &node1,
                                   const r::FXNode &node2,
                                   const Ratio &time,
                                   const Ratio &subtract
                                   )
{
  r::FXNode node = node1;
  node._time = time;
  
  R_ASSERT(time >= node1._time);
  R_ASSERT(node2._time >= time);
  
  if (node1._logtype != LOGTYPE_HOLD){
    if (node._time==node2._time)
      node._val = node2._val;
    else
      node._val = round(scale_double(make_double_from_ratio(time),
                                     make_double_from_ratio(node._time), make_double_from_ratio(node2._time),
                                     node1._val, node2._val));
    node._val = R_BOUNDARIES(fx.min, node._val, fx.max);
  }
  
  add_fxnodeline2(writer, node, subtract);
}


/*
static void CopyRange_fxnodelines(
                                  struct FXNodeLines **tofxnodeline,
                                  const struct FXNodeLines *fromfxnodeline,
                                  const struct FXNodeLines *previous,
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

        if (previous!=NULL)
          if (p_Less_Than(previous->l.p, p1))
            add_scaled_fxnodeline(tofxnodeline, previous, fromfxnodeline, p1, p1);

        if(p_Greater_Or_Equal(fromfxnodeline->l.p, p2)) {
          if (previous!=NULL)
            add_scaled_fxnodeline(tofxnodeline, previous, fromfxnodeline, p2, p1);
          return;
        }

        add_fxnodeline(tofxnodeline, fromfxnodeline, p1);

	CopyRange_fxnodelines(tofxnodeline,
                              NextFXNodeLine(fromfxnodeline),
                              fromfxnodeline,
                              p1,
                              p2);
}

*/

static void CopyRange_fxnodelines2(const struct FX &fx,
                                   r::TimeData<r::FXNode>::Writer &writer,
                                   const r::TimeData<r::FXNode>::Reader &reader,
                                   const Ratio &time1,
                                   const Ratio &time2)
{
  const r::FXNode *previous = NULL;
  
  for(const r::FXNode &from_node : reader){

    if (from_node._time >= time1) {
    
      if (previous!=NULL)
        if (previous->_time < time1)
          add_scaled_fxnodeline2(fx, writer, *previous, from_node, time1, time1);
      
      if(from_node._time >= time2) {
        if (previous!=NULL)
          add_scaled_fxnodeline2(fx, writer, *previous, from_node, time2, time1);
        return;
      }

      previous = &from_node;
      add_fxnodeline2(writer, from_node, time1);

    }
    
  }
}


void CopyRange_fxs(
                   int num_lines,
                   vector_t *tofxs,
                   const vector_t *das_fromfxs,
                   const Ratio r1,
                   const Ratio r2
                   )
{
  VECTOR_FOR_EACH(const struct FXs *, fromfxs, das_fromfxs){

        R_ASSERT_RETURN_IF_FALSE(fromfxs->fx->patch->is_usable);
        
#if 0
        // This thing should perhaps be moved into co_CB_PasteTrack.
        if (!fromfxs->fx->patch->is_usable) {
          fromfxs->fx->patch = PATCH_create_audio(NULL, NULL, fromfxs->fx->patch->name, fromfxs->fx->patch->state);
          R_ASSERT_RETURN_IF_FALSE(fromfxs->fx->patch->patchdata != NULL);
        }
#endif
        
	struct FXs *fxs = FXs_create();

	fxs->fx=(struct FX *)tcopy(fromfxs->fx);

        bool push_it = true;

        //CopyRange_fxnodelines(&fxs->fxnodelines,fromfxs->fxnodelines,NULL,*p1,*p2);

        {
          r::TimeData<r::FXNode>::Writer writer(fxs->_fxnodes);
          r::TimeData<r::FXNode>::Reader reader(fromfxs->_fxnodes);
          CopyRange_fxnodelines2(*fxs->fx, writer, reader, r1, r2);
          //printf("Reader size: %d. Writer size: %d\n", reader.size(), writer.size());
          //getchar();
          R_ASSERT_NON_RELEASE(reader.size() >= 2);
          //R_ASSERT_NON_RELEASE(writer.size() >= 2);
          if (!LegalizeFXlines2(num_lines, fxs->fx, writer)){
            writer.cancel();
            push_it = false;
          }
          
        }

        if (push_it)
          VECTOR_push_back(tofxs,fxs);
        
  }END_VECTOR_FOR_EACH;
}


void CopyRange(
               struct Blocks *block,
               range_t range,
               int rangenum
){
	NInt num_tracks;
	int lokke;

	if( ! range.enabled)
          return;

        int starttrack = range.x1;
        int endtrack = R_MIN(block->num_tracks-1, range.x2);
        if (endtrack < starttrack)
          return;

        R_ASSERT_RETURN_IF_FALSE(rangenum >= 0 && rangenum < NUM_RANGES);
        
	//struct RangeClip *range_clip=(struct RangeClip *)talloc(sizeof(struct RangeClip));
        struct RangeClip *range_clip=talloc_with_finalizer<struct RangeClip>([](struct RangeClip *range_clip){
          for(int i=0;i<range_clip->num_tracks;i++)
            delete range_clip->stops[i];
        });

        range_clip->rangenum = rangenum;
        
        g_range_clips[rangenum] = range_clip;
        
	range_clip->num_tracks = num_tracks = range.x2-range.x1+1;

	range_clip->notes=(struct Notes **)talloc((int)sizeof(struct Notes *)*num_tracks);
        range_clip->stops=(r::TimeData<r::Stop>**)talloc((int)sizeof(r::TimeData<r::Stop> *)*num_tracks);

	//range_clip->instruments=talloc((size_t)(sizeof(struct Instruments *)*num_tracks));
	range_clip->fxs=(vector_t*)talloc(sizeof(vector_t)*num_tracks);

	range_clip->length = p_Sub(range.y2, range.y1);
        
	const struct Tracks *track=(const struct Tracks *)ListFindElement1(&block->tracks->l,range.x1);

	for(lokke=0;lokke<=range.x2-range.x1;lokke++){
          
          //range_clip->instruments[lokke]=track->instrument;
          CopyRange_notes(&range_clip->notes[lokke], track->notes, &range.y1, &range.y2);
          
          range_clip->stops[lokke] = new r::TimeData<r::Stop>;
          CopyRange_stops(range_clip->stops[lokke], track->stops2, &range.y1, &range.y2);

          Place p2;
          PlaceSetLastPos(block,&p2);
          
          Ratio r2;
          if (p_Equal(range.y2, p2))
            r2 = make_ratio(block->num_lines, 1);
          else
            r2 = place2ratio(range.y2);
          
          CopyRange_fxs(block->num_lines, &range_clip->fxs[lokke], &track->fxs, place2ratio(range.y1), r2);

          track=NextTrack(track);
          if (track==NULL)
            break;
	}

        
        const Place *startplace = &range.y1;
        const Place *endplace = &range.y2;
        
        SCHEME_eval(
                    talloc_format("(copy-fx-range! %d %d %d (+ %d (/ %d %d)) (+ %d (/ %d %d)) %d)",
                                  block->l.num,
                                  starttrack,
                                  endtrack,
                                  startplace->line, startplace->counter, startplace->dividor,
                                  endplace->line, endplace->counter, endplace->dividor,
                                  rangenum
                                  )
                    );

}

void CopyRange_CurrPos(
                       struct Tracker_Windows *window,
                       int rangenum
                       )
{
        CopyRange(window->wblock->block, window->wblock->range, rangenum);

        window->wblock->range.enabled = false;
        window->must_redraw = true;
}

