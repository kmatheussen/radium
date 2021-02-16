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
#include "TallocWithDestructor.hpp"
#include "list_proc.h"
#include "vector_proc.h"
#include "placement_proc.h"
#include "visual_proc.h"
#include "realline_calc_proc.h"
#include "undo_fxs_proc.h"
#include "player_proc.h"
#include "player_pause_proc.h"
#include "OS_visual_input.h"
#include "instruments_proc.h"
#include "windows_proc.h"
#include "wblocks_proc.h"
#include "wtracks_proc.h"
#include "common_proc.h"
#include "OS_Player_proc.h"
#include "patch_proc.h"
#include "time_proc.h"
#include "sequencer_proc.h"
#include "../api/api_proc.h"

#include "fxlines_proc.h"


extern struct Root *root;
extern struct TEvent tevent;

struct FXs *FXs_create(void){  
  struct FXs *fxs = talloc_with_finalizer<struct FXs>([](struct FXs *fxs){
      delete fxs->_fxnodes;
    });
  
  fxs->_fxnodes = new r::TimeData<r::FXNode>;
  return fxs;
}

static int getNumUsedFX(struct Tracks *track){
	int ret;
	ret=track->fxs.num_elements;

	return ret;
}


/******************************************************************
  FUNCTION
    Just a way to get the same fx corresponding to 'num'.
******************************************************************/
static struct FX *getTrackFX(struct Tracks *track,int num){
  if (num >= track->fxs.num_elements){
    RError("Error at function getTrackFX in file fxlines.c\n");
    return NULL;
  }

  return ((struct FXs*)track->fxs.elements[num])->fx;
}


void FX_min_max_have_changed_for_patch(struct Patch *patch, NInt fxnum, float old_min, float old_max, float new_min, float new_max){
  struct Blocks *block = root->song->blocks;

  while(block!=NULL){
    struct Tracks *track = block->tracks;

    while(track!=NULL){

      if (track->patch == patch) {

        VECTOR_FOR_EACH(struct FXs *, fxs, &track->fxs){

          struct FX *fx = fxs->fx;

          if (fx->effect_num == fxnum) {
            int fx_min = fx->min;
            int fx_max = fx->max;

            ADD_UNDO(FXs(root->song->tracker_windows, block, track, 0));

            /*
            struct FXNodeLines *fxnode = fxs->fxnodelines;

            while(fxnode != NULL) {
              double real_val = scale_double(fxnode->val, fx_min,  fx_max,  old_min, old_max);
              fxnode->val     = scale_double(real_val,    new_min, new_max, fx_min,  fx_max);
              fxnode = NextFXNodeLine(fxnode);
            }
            */

            r::TimeData<r::FXNode>::Writer writer(fxs->_fxnodes);
            for(r::FXNode &node : writer){
              double real_val = scale_double(node._val, fx_min,  fx_max,  old_min, old_max);
              node._val       = scale_double(real_val,  new_min, new_max, fx_min,  fx_max);
            }
            
            block->is_dirty=true;
          }

        }END_VECTOR_FOR_EACH;
          
      }
      track = NextTrack(track);
    }
    block = NextBlock(block);
  }
}

#if 0
enum ColorNums newFXColor(void){  
  static enum ColorNums last = AUTOMATION8_COLOR_NUM;
  
  switch(last) {
    
    case AUTOMATION1_COLOR_NUM:
      last = AUTOMATION2_COLOR_NUM;
      return last;
    case AUTOMATION2_COLOR_NUM:
      last = AUTOMATION3_COLOR_NUM;
      return last;
    case AUTOMATION3_COLOR_NUM:
      last = AUTOMATION4_COLOR_NUM;
      return last;
    case AUTOMATION4_COLOR_NUM:
      last = AUTOMATION5_COLOR_NUM;
      return last;
    case AUTOMATION5_COLOR_NUM:
      last = AUTOMATION6_COLOR_NUM;
      return last;
    case AUTOMATION6_COLOR_NUM:
      last = AUTOMATION7_COLOR_NUM;
      return last;
    case AUTOMATION7_COLOR_NUM:
      last = AUTOMATION8_COLOR_NUM;
      return last;
    case AUTOMATION8_COLOR_NUM:
      last = AUTOMATION1_COLOR_NUM;
      return last;
      
    default:
      RWarning("Unknown last color %d\n",last);
      last = AUTOMATION1_COLOR_NUM;
      return last;
      
  }
}
#endif

static const char *get_track_fx_display_name(struct Tracks *track, struct FX *fx) __attribute__((returns_nonnull));
static const char *get_track_fx_display_name(struct Tracks *track, struct FX *fx){
  if (track->patch==fx->patch)
    return fx->name;
  else
    return (const char*)talloc_format("%s (%s)", fx->name, fx->patch->name);
}

static void selectFX(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
        std::function<void(struct FX*)> callback
){

  struct Tracks *track = wtrack->track;
  struct Patch *patch = track->patch;
  
  if(patch==NULL) {
    selectInstrumentForTrack(track->l.num);
    return;
  }

  int num_usedFX=getNumUsedFX(track);

  if(num_usedFX>0){
    int lokke;
    vector_t v={};
    
    VECTOR_push_back(&v,talloc_format("----------New FX (track #%d)", wtrack->l.num));
    int add = VECTOR_push_back(&v,"Add");
    
    VECTOR_push_back(&v,talloc_format("----------Existing FX (track #%d)", wtrack->l.num));

    int first = -1;
    
    for(lokke=0;lokke<num_usedFX;lokke++){
      auto *fx = getTrackFX(track,lokke);
      if (fx != NULL){
        int n = VECTOR_push_back(&v,get_track_fx_display_name(wtrack->track, fx));
        if (first==-1)
          first = n;
      }
    }

#if 0
    for(lokke=0;lokke<10000;lokke++)
      VECTOR_push_back(&v,talloc_format("extra %d",lokke));
#endif
    
    GFX_Menu3(v,[add, first, num_usedFX, callback, window, track, patch](int selection, bool onoff){
        if (selection==-1)
          return;

        if (selection==add)
          patch->instrument->getFX(window,track,callback);
        else
          callback(getTrackFX(track,selection-first));
      });

    return;
    
  } else {

    patch->instrument->getFX(window,track,callback);

  }
}


int AddFXNodeLine(
                  struct Tracker_Windows *window,
                  struct WBlocks *wblock,
                  struct WTracks *wtrack,
                  struct FXs *fxs,
                  int val,
                  const Place *p1
){

        int ret;

        /*
        struct FXNodeLines *fxnodeline=(struct FXNodeLines *)talloc(sizeof(struct FXNodeLines));
        {
          SCOPED_PLAYER_LOCK_IF_PLAYING();
          
          fxnodeline->val=R_BOUNDARIES(fxs->fx->min, val, fxs->fx->max);
          PlaceCopy(&fxnodeline->l.p,p1);
          ret = ListAddElement3_ns(&fxs->fxnodelines,&fxnodeline->l);
        }
        */
        
        R_ASSERT_RETURN_IF_FALSE2(fxs->_fxnodes!=NULL, 0);

        {
          r::TimeData<r::FXNode>::Writer writer(fxs->_fxnodes);
          
          Ratio ratio = ratio_from_place(*p1);
          
          if (!writer.has_element_at_ratio(ratio)) {
            auto node = r::FXNode(*fxs->fx, ratio, val);
            writer.add(node);
          }

          ret=writer.find_element_at_ratio(ratio);
        }
        
        return ret;
}

static struct FXs *get_fxs_for_fx(struct Tracks *track, struct FX *fx){
  //printf("Calling get_fxs_for_fx for track %p. num_fxs: %d\n", track, track->fxs.num_elements);
  VECTOR_FOR_EACH(struct FXs *, fxs, &track->fxs){
    //printf("   Comp %d/%d, %p/%p\n", fxs->fx->effect_num, fx->effect_num, fxs->fx->patch, fx->patch);
    if (fxs->fx->effect_num == fx->effect_num && fxs->fx->patch == fx->patch)
      return fxs;
  }END_VECTOR_FOR_EACH;
  
  return NULL;
}

static void AddFXNodeLineCurrPosInternal(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, struct FX *fx, const Place *place, int val){

  Place p1;
  PlaceCopy(&p1, place);

  bool created_new_fxs = false;
  Place p2;
            
  struct FXs *fxs=get_fxs_for_fx(wtrack->track, fx);
  
  if (fxs==NULL){
    
          //printf("    FXS 1: %p\n",fxs);
          
          PlaceCopy(&p2, &p1);

          p2.line ++;
          
          Place lastplace;
          PlaceSetLastPos(wblock->block, &lastplace);
          
          if (PlaceGreaterThan(&p2, &lastplace))
            PlaceSetLastPos(wblock->block,&p2);

          if (PlaceGreaterOrEqual(&p1, &p2))
            p1.line --;

          if (PlaceGreaterOrEqual(place, &p2)) {
            p1.line = 0;
            p1.counter = 0;
          }

          fxs = FXs_create();
          fxs->fx = fx;
  
          created_new_fxs = true;
  }

  AddFXNodeLine(
                window,wblock,wtrack,
                fxs,
                val,
                &p1
                );

  if (created_new_fxs) {
    AddFXNodeLine(
                  window,wblock,wtrack,
                  fxs,
                  val,
                  &p2
                  );

    const rt_vector_t *rt_vector = VECTOR_create_rt_vector(&wtrack->track->fxs, 1);
    
    {
      SCOPED_PLAYER_LOCK_IF_PLAYING();
      RT_VECTOR_push_back(&wtrack->track->fxs, fxs, rt_vector);
    }
  }
                         
  window->must_redraw = true; // fx must always use must_redraw, not must_redraw_editor, to update lower scrollbar and legalize cursor pos.
}


void AddFXNodeLineCustomFxAndPos(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, struct FX *fx, const Place *p, float val){
  int scaled_val = scale(val, 0, 1, fx->min, fx->max);
                  
  AddFXNodeLineCurrPosInternal(window, wblock, wtrack, fx, p, scaled_val);
}

/*
// not used anymore.
void AddFXNodeLineCurrMousePos(struct Tracker_Windows *window){
  struct WBlocks *wblock = window->wblock;
  float x = tevent.x;
  float y = tevent.y;
  struct WTracks *wtrack = WTRACK_get(wblock, x);
  if (wtrack==NULL)
    return;
  
  Place place = GetPlaceFromY(window,
                              wblock,
                              y
                              );

  struct FX *fx=selectFX(window,wblock,wtrack);
  if(fx==NULL)
    return;

  bool use_mouse_pos = false;

  if (fx->patch != NULL && fx->patch->instrument==get_MIDI_instrument())
    use_mouse_pos = true;
  
  if (use_mouse_pos){
    float val = scale(x, wtrack->fxarea.x, wtrack->fxarea.x2, 0, 1);
    ADD_UNDO(FXs_CurrPos(window));
    PC_Pause();{
      AddFXNodeLineCustomFxAndPos(window, wblock, wtrack, fx, &place, val);
    }PC_StopPause(NULL);
  } else {
    int val = fx->defaultFXValue(fx);
    ADD_UNDO(FXs_CurrPos(window));
    PC_Pause();{
      AddFXNodeLineCurrPosInternal(window, wblock, wtrack, fx, &place, val);
    }PC_StopPause(NULL);
  }
}
*/

void AddFXNodeLineCurrPos(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack){

  auto callback = [window, wblock, wtrack](struct FX *fx){
    if(fx==NULL) return;

    Place place;
    PlaceCopy(&place, &wblock->reallines[wblock->curr_realline]->l.p);
    
    ADD_UNDO(FXs_CurrPos(window));
    
    AddFXNodeLineCustomFxAndPos(window, wblock, wtrack, fx, &place, 0.5);
  };
    
  selectFX(window,wblock,wtrack, callback);
}


#if 0
void DeleteFxNodeLine(struct Tracker_Windows *window, struct WTracks *wtrack, struct FXs *fxs, struct FXNodeLines *fxnodeline){

  int pos = ListPosition3((struct ListHeader3*)fxs->fxnodelines, &fxnodeline->l);

  int num_elements = ListFindNumElements3((struct ListHeader3*)fxs->fxnodelines);

  R_ASSERT(num_elements > 1);

  if (num_elements > 2){
  
    {
      SCOPED_PLAYER_LOCK_IF_PLAYING();
      ListRemoveElement3(&fxs->fxnodelines,&fxnodeline->l);
    }

  } else if (NextFXNodeLine(fxnodeline) == NULL && fxnodeline->val != fxs->fxnodelines->val) {
    
    safe_int_write(&fxnodeline->val, fxs->fxnodelines->val);
    
  } else {
    
    PC_Pause();{
    
      ListRemoveElement3(&fxs->fxnodelines,&fxnodeline->l);
      
      struct FX *fx = fxs->fx;
      struct Tracks *track = wtrack->track;
      
      //OS_SLIDER_release_automation_pointers(track->patch,fx->effect_num);
      (*fx->closeFX)(fx,track);
      VECTOR_remove(&track->fxs, fxs);
      
    }PC_StopPause(NULL);
    
  }

  {
    r::TimeData<r::FXNode>::Writer writer(fxs->_fxnodes);
    R_ASSERT(writer.remove_at_pos(pos));
    // NOTE: Must include fx->closeFX etc. above if necessary.
  }

  window->must_redraw = true; // fx must always use must_redraw, not must_redraw_editor, to update lower scrollbar and legalize cursor pos.
}
#endif

void DeleteFxNodes(struct Tracker_Windows *window, struct WTracks *wtrack, struct FXs *fxs, const std::vector<int> &fxnodenums){

  if (fxnodenums.size()==0)
    return;
  
  r::TimeData<r::FXNode>::Writer writer(fxs->_fxnodes);

  int size_before = writer.size();
  
  R_ASSERT(size_before > 1);
    
  writer.remove_at_positions(fxnodenums);

  int size = writer.size();

  R_ASSERT_NON_RELEASE(size_before-size == (int)fxnodenums.size());

  if (size==1) {

    writer.remove_at_pos(0);
    
    struct FX *fx = fxs->fx;
    struct Tracks *track = wtrack->track;
      
    (*fx->closeFX)(fx,track);

    {
      SCOPED_PLAYER_LOCK_IF_PLAYING();
      VECTOR_remove(&track->fxs, fxs);
    }
  }

  window->must_redraw = true; // fx must always use must_redraw, not must_redraw_editor, to update lower scrollbar and legalize cursor pos.
}

void DeleteFxNode(struct Tracker_Windows *window, struct WTracks *wtrack, struct FXs *fxs, int fxnodenum){
  std::vector<int> fxnodenums = {fxnodenum};
  DeleteFxNodes(window, wtrack, fxs, fxnodenums);
}

template <typename T>
template <typename TimeData, typename TimeDataVector>
template <typename ValType>
void r::TimeData<T>::ReaderWriter<TimeData, TimeDataVector>::iterate_fx(struct SeqTrack *seqtrack, const struct SeqBlock *seqblock, const struct Tracks *track, struct FX *fx, int play_id, const int64_t seqtime_start, const r::RatioPeriod &period) const {

  R_ASSERT_NON_RELEASE(period._end >= period._start);
  
  if (!period_is_inside(period))
    return;

  const int das_size = size();
  const T &first_t = _vector->at_ref(0);
  const T &last_t = _vector->at_ref(das_size-1);
  
  RT_TimeData_Player_Cache *cache = get_player_cache();

  bool has_prev_value;
  double prev_value;

  // Find previous value
  {
    if (period._start.num==0 || period._start < first_t._time) { // Note: period._start==0 when it's the first call to block.

      prev_value = 0.0; // Not necessary. Only to silence compiler error. (Usually I have the opposite problem, that it won't give error when using uninitialized value. Sigh. Why don't the gcc and clang people prioritize to get this right? It seems far more important than minor optimizations for instance.)
      has_prev_value = false;
    
    } else {
      
      if (cache!=NULL && cache->_last_play_id == play_id) {
        
        prev_value = cache->_last_value;
        
      } else {

        // FIX: Hvis det er noder mellom period._prev og period._start, set has_prev_value = false.
        // Kanskje også hvis det er tempoforandringer der, om det er praktisk enkelt å sjekke det.
        
        Ratio ratio_prev = period._start - (period._end-period._start); // approx. (this is a corner case. Even if this value is totally wrong, no one would probably notice, and if they did it would be extremely seldom.)
        if (ratio_prev < first_t._time) {
          prev_value = first_t._val;
        } else {
          prev_value = get_value_raw(ratio_prev, das_size);
        }
        
      }
      
      has_prev_value = true;
    }
  }

  int64_t value_time;
  ValType value;
  FX_when when;

  // Find value at period._start
  {
    if (period._end >= last_t._time){

      value_time = get_seqblock_place_time2(seqblock, track, make_place_from_ratio(last_t._time));
      value = last_t._val;
      when = FX_end;

    } else if (period._start.num==0 || period._start < first_t._time) { // Note: period._start==0 when it's the first call to block.

      value_time = get_seqblock_place_time2(seqblock, track, make_place_from_ratio(first_t._time));
      value = first_t._val;
      when = FX_start;
      _curr_pos = 1;
      
    } else {

      value_time = seqtime_start;
      value = get_value_raw(period._start, das_size); // get_value_raw updates _curr_pos.
      when = FX_middle;

#if !defined(RELEASE)
      int curr_pos = _curr_pos;      
      R_ASSERT_NON_RELEASE(curr_pos = find_pos_for_get_value(period._start));
#endif
    }
  }

  bool same_value_as_last_time =
    has_prev_value
    && (std::is_same<ValType, int>::value
        ? int(prev_value)==value
        : (std::is_same<ValType, float>::value
           ? equal_floats(float(prev_value), value)
           : equal_doubles(prev_value, value)
           )
        );

  if (when==FX_start || when==FX_end || !same_value_as_last_time){
    //printf("....1. %d: %f. When: %d. _curr_pos: %d\n", (int)value_time, (double)value / (double)fx->max, (int) when, _curr_pos);
    RT_FX_treat_fx(seqtrack, fx, value, value_time, 0, when);
  }

  // Send out all node values between period._start and period._end
  if (when != FX_end) {
    
    while(_curr_pos < das_size - 1) { 

      const T &node = at(_curr_pos);
      if (node._time >= period._end)
        break;
      
      value = node._val;
      
      FX_when when = _curr_pos == das_size-1 ? FX_end : FX_middle;

      int64_t time = get_seqblock_place_time2(seqblock, track, make_place_from_ratio(node._time));
      //printf("....2. %d: %f. When: %d. _curr_pos: %d\n", (int)value, (double)value / (double)fx->max, (int) when, _curr_pos);
      RT_FX_treat_fx(seqtrack, fx, value, time, 0, when);

      _curr_pos++;

    }
  }

  if (cache != NULL) {

    cache->_last_value = value;
    cache->_last_play_id = play_id;
    
  }

}

static const r::RatioPeriod get_ratio_period(const struct SeqBlock *seqblock, const struct STimes *times, const int64_t seqtime_start, const int64_t seqtime_end) {
  const struct Blocks *block = seqblock->block;
    
  const Ratio ratio_start = STime2Place4(block, seqtime_to_blocktime(seqblock, seqtime_start-seqblock->t.time), times);  
  const Ratio ratio_end = STime2Place4(block, seqtime_to_blocktime(seqblock, seqtime_end-seqblock->t.time), times);

  return r::RatioPeriod(ratio_start, ratio_end);
}

void RT_fxline_called_each_block(struct SeqTrack *seqtrack,
                                 const struct SeqBlock *seqblock,
                                 const int64_t seqtime_start,
                                 const int64_t seqtime_end
                                 )
{
  int play_id = ATOMIC_GET(pc->play_id);
  
  struct Blocks *block = seqblock->block;
  
  struct Tracks *track = block->tracks;

  const struct STimes *times = get_stimes_from_swinging_mode(block, PLUGINS_AND_JACK_TRANSPORT_SWINGING_MODE);
  
  const r::RatioPeriod period = get_ratio_period(seqblock,
                                                 times,                                                 
                                                 seqtime_start,
                                                 seqtime_end);
  
  while(track!=NULL){

    int tracknum = track->l.num;

    bool enabled = track->onoff==1 || root->song->mute_editor_automation_when_track_is_muted==false;
    
    bool doit = seqblock->track_is_disabled==NULL  // i.e. playing block
      || tracknum >= MAX_DISABLED_SEQBLOCK_TRACKS
      || !seqblock->track_is_disabled[tracknum];

    if (enabled && doit){

      const r::RatioPeriod das_period
        = track->times==times
        ? period
        : get_ratio_period(seqblock,
                           track->times,                                                 
                           seqtime_start,
                           seqtime_end);
      
      /*
      if (false && track->l.num==0)
        printf("B: %d. Ratio start: %d / %d (%f). Ratio end: %d / %d (%f). Seqtime: %d -> %d. Seqblock time: %d -> %d\n", block->l.num, (int)ratio_start.num, (int)ratio_start.den, make_double_from_ratio(ratio_start), (int)ratio_end.num, (int)ratio_end.den, make_double_from_ratio(ratio_end), (int)seqtime_start, (int)seqtime_end, (int)seqblock->t.time, (int)seqblock->t.time2);
      */
      
      VECTOR_FOR_EACH(struct FXs *, fxs, &track->fxs){

        struct FX *fx = fxs->fx;
        
        if (fx->is_enabled) {

          r::TimeData<r::FXNode>::Reader reader(fxs->_fxnodes, -1); // <- -1 means that cache is not enabled yet. Cache code: ATOMIC_GET(seqblock->timedata_player_index));

          reader.iterate_fx<int>(seqtrack, seqblock, track, fx, play_id, seqtime_start, das_period);

        }
        
      }END_VECTOR_FOR_EACH;
    }
    
    track=NextTrack(track);   
  }

}
