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


#define SEQBLOCK_USING_VECTOR 1
#include "nsmtracker.h"
#include "TimeData.hpp"
#include "vector_proc.h"
#include "notes_proc.h"
#include "list_proc.h"
#include "placement_proc.h"
#include "undo_notes_proc.h"
#include "undo_blocks_proc.h"
#include "player_pause_proc.h"
#include "player_proc.h"
#include "range_proc.h"
#include "trackreallines2_proc.h"
#include "windows_proc.h"
#include "OS_Player_proc.h"

#include "velocities_proc.h"


extern struct Root *root;


static int add_velocity(
                        int logtype,
                        int velocityvelocity,
                        const Place *placement,
                        struct Notes *note
                        )
{

  Place endplace = ratio2place(note->end);
  
  if(PlaceLessOrEqual(placement, &note->l.p)) return -1;
  if(PlaceGreaterOrEqual(placement, &endplace)) return -1;

#if 0
  struct Velocities *velocity=(struct Velocities*)talloc(sizeof(struct Velocities));
  PlaceCopy(&velocity->l.p,placement);
  velocity->velocity=R_BOUNDARIES(0,velocityvelocity,MAX_VELOCITY);
  velocity->logtype = logtype;
  
  /* ListAddElement3_ns returns -1 (and doesnt do anything else)
     if there already is an element with the same placement. */

  {
    SCOPED_PLAYER_LOCK_IF_PLAYING();
    *pos = ListAddElement3_ns(&note->velocities,&velocity->l);
  }
#endif
  
  {
    r::TimeData<r::Velocity>::Writer writer(note->_velocities);

    Ratio ratio = ratio_from_place(*placement);
    if (!writer.has_element_at_ratio(ratio))    
      writer.add2(r::Velocity(ratio, velocityvelocity, logtype));

    return writer.find_element_at_ratio(ratio);
  }
}

int AddVelocity3(
                 int logtype,
                 int velocityvelocity,
                 const Place *placement,
                 struct Notes *note
                 )
{
  return add_velocity(logtype, velocityvelocity, placement, note);
}

/*
static struct Velocities *AddVelocity2(
                                int velocityvelocity,
                                const Place *placement,
                                struct Notes *note
                                )
{
  int ret;
  return add_velocity(LOGTYPE_LINEAR, velocityvelocity, placement, note, &ret);
}
*/

int AddVelocity(
                int velocityvelocity,
                const Place *placement,
                struct Notes *note
){
  return add_velocity(LOGTYPE_LINEAR, velocityvelocity, placement, note);
}

void AddVelocityCurrPos(struct Tracker_Windows *window){

	struct WBlocks *wblock=window->wblock;
	const struct LocalZooms *realline= wblock->reallines[wblock->curr_realline];
	int subtrack=window->curr_track_sub;

        if(-1==subtrack)
          return;

	ADD_UNDO(Notes_CurrPos(window));

        struct Notes *note = FindNoteOnSubTrack(
                                                wblock->wtrack,
                                                subtrack,
                                                &realline->l.p
                                                );

	if(note==NULL)
          return;

	AddVelocity(
                    root->standardvel,
                    &realline->l.p,
                    note
                    );

#if !USE_OPENGL
	ClearTrack(window,wblock,wblock->wtrack,wblock->top_realline,wblock->bot_realline);
	UpdateWTrack(window,wblock,wblock->wtrack,wblock->top_realline,wblock->bot_realline);
#endif

}


static void increase_note_velocity(struct Notes *note, int inc){
  int maxvelocity=MAX_VELOCITY;
  
  note->velocity=R_BOUNDARIES(0,note->velocity+inc,maxvelocity);

  /*
  {
    struct Velocities *velocity = note->velocities;
    while(velocity != NULL){
      velocity->velocity = R_BOUNDARIES(0,velocity->velocity+inc,maxvelocity);
      velocity = NextVelocity(velocity);
    }
  }
  */
  
  {
    r::TimeData<r::Velocity>::Writer writer(note->_velocities);
    for(r::Velocity &velocity : writer)
      velocity._val = R_BOUNDARIES(0,velocity._val+inc,maxvelocity);
  }

  note->velocity_end=R_BOUNDARIES(0,note->velocity_end+inc,maxvelocity);
}

void IncreaseVelocityCurrPos(struct Tracker_Windows *window,int inc){

        inc = inc * MAX_VELOCITY / 100;

        vector_t notes = FindAllNotesCurrPos(window);
        
        if(notes.num_elements==0)
          return;
        
        ADD_UNDO(Block_CurrPos(window));
        
        VECTOR_FOR_EACH(struct Notes *, note, &notes){
          increase_note_velocity(note, inc);
        }END_VECTOR_FOR_EACH;
        
        window->must_redraw = true;

}



/************ RT ******************/


namespace{
  class VelocityIterateCallback : public r::IterateCallback<int> {

    struct Patch *_patch;
    struct Notes *_note;

    static_assert(std::is_same<int, typeof(_note->velocity)>::value, "hepp");

  public:
    
    VelocityIterateCallback(struct Patch *patch, struct Notes *note)
      : _patch(patch)
      , _note(note)
    {}

    void callback(struct SeqTrack *seqtrack, const struct SeqBlock *seqblock, const struct Tracks *track, int val, int64_t time, FX_when when) const override {

      _note->curr_velocity = TRACK_get_velocity(track, val);
      _note->curr_velocity_time = time;
      
      //printf("Velocity: %f. Time: %d\n", _note->curr_velocity, (int)time);

      _note->has_sent_seqblock_volume_automation_this_block = true;

      RT_PATCH_change_velocity(seqtrack,
                               _patch,
                               create_note_t(seqblock,
                                             _note->id,
                                             _note->note,
                                             _note->curr_velocity * seqblock->curr_gain * seqtrack->note_gain * seqtrack->note_gain_muted,
                                             0,
                                             ATOMIC_GET(track->midi_channel),
                                             0,
                                             0
                                             ),
                               time
                               );
    }

  };
}

template <typename ValType>
static void call_callback_between_nodes(struct SeqTrack *seqtrack,
                                        const struct SeqBlock *seqblock,
                                        const struct Tracks *track,
                                        
                                        const int64_t seqtime_start,
                                        const Ratio &ratio, // ratio for seqtime_start
                                        
                                        const r::IterateCallback<ValType> &callback,
                                        
                                        const r::TimeDataSimpleNode<ValType> &n1,
                                        const r::TimeDataSimpleNode<ValType> &n2,

                                        r::RT_TimeData_Cache_Handler<ValType> &cache
                                        )
{
  ValType value;

  if (n1._val == n2._val || n1._logtype==LOGTYPE_HOLD) { // Probably the most common situation.

    R_ASSERT_NON_RELEASE(ratio >= n1._time);
    R_ASSERT_NON_RELEASE(ratio <= n2._time);
    
    value = n1._val;
    
  } else if (ratio <= n1._time) {

    R_ASSERT_NON_RELEASE(ratio == n1._time);

    value = n1._val;

  } else if (ratio >= n2._time) {

    R_ASSERT_NON_RELEASE(ratio == n2._time);

    value = n2._val;
    
  } else if (n1._time==n2._time){
    
    R_ASSERT_NON_RELEASE(false);
    
    value = n1._val;
    
  } else {

    value = scale_double(make_double_from_ratio(ratio),
                         make_double_from_ratio(n1._time), make_double_from_ratio(n2._time),
                         n1._val, n2._val);
    
  }

  if (!cache.is_same_value(value)) {
    //printf("Has: %d. is_same: %d. Value: %d. Cached value: %d\n", cache.has_value(), cache.has_value() && cache.is_same_value(value, cache.get_value()), value, cache.get_value());
    callback.callback(seqtrack, seqblock, track, value, seqtime_start, FX_single); // Note: FX_When is currently ignored by all these callbacks.
    cache.update_value(value);
  }
}

template <typename T>
template <typename TimeData, typename TimeDataVector>
template <typename ValType>
void r::TimeData<T>::ReaderWriter<TimeData, TimeDataVector>::iterate_extended(struct SeqTrack *seqtrack,
                                                                              const struct SeqBlock *seqblock,
                                                                              const struct Tracks *track,
                                                                              int play_id,
                                                                              
                                                                              const int64_t seqtime_start,
                                                                              const r::RatioPeriod &track_period, // Note: track_period._start corresponds to seqtime_start
                                                                              
                                                                              const r::IterateCallback<ValType> &callback,

                                                                              const r::TimeDataSimpleNode<ValType> &node_start,
                                                                              const r::TimeDataSimpleNode<ValType> &node_end
                                                                              ) const
{

  if (track_period._start < node_start._time) {
    
    // Commented out. For now (before notes are converted to TimeData), this can happen if moving start position of note while playing
    //R_ASSERT_NON_RELEASE(false);
    return;
  }
  if (track_period._start > node_end._time) {

    // Commented out. For now (before notes are converted to TimeData), this can happen if moving end position of note while playing
    //R_ASSERT_NON_RELEASE(false);
    return;
  }

  /*
  printf("period start/end: %d/%d - %d/%d.\n",
         (int)track_period._start.num, (int)track_period._start.den,
         (int)track_period._end.num, (int)track_period._end.den
         );
  */
  r::RT_TimeData_Cache_Handler<ValType> cache(get_player_cache(), play_id);

  const int das_size = size();
  
  if (das_size==0) {
    
    call_callback_between_nodes(seqtrack, seqblock, track, seqtime_start, track_period._start, callback,
                                node_start,
                                node_end,
                                cache);

    return;
  }
 
  {
    const T &first_t = _vector->at_first();
    
    if (track_period._start < first_t._time) {
      
      call_callback_between_nodes(seqtrack, seqblock, track, seqtime_start, track_period._start, callback,
                                  node_start,
                                  r::TimeDataSimpleNode(first_t._time, first_t._val),
                                  cache);
      
      if (das_size==1 && track_period._end >= first_t._time) {

        int64_t node_time = get_seqblock_ratio_time2(seqblock, track, first_t._time);
        callback.callback(seqtrack, seqblock, track, first_t._val, node_time, FX_single);

        cache.update_value(first_t._val);

      }
      
    }
  }

  if (das_size > 1)
    iterate(seqtrack, seqblock, track, play_id, seqtime_start, track_period, callback);
  
  {
    const T &last_t = _vector->at_last();
    
    if (track_period._start > last_t._time) {
      
      call_callback_between_nodes(seqtrack, seqblock, track, seqtime_start, track_period._start, callback,
                                  r::TimeDataSimpleNode(last_t._time, last_t._val, last_t._logtype),
                                  node_end,
                                  cache);
      
    }

  }

}


static void RT_VELOCITIES_called_each_block_for_each_note(struct SeqTrack *seqtrack,
                                                          const int play_id,
                                                          const struct SeqBlock *seqblock,
                                                          const struct Tracks *track,
                                                          const int64_t seqtime_start,
                                                          const r::RatioPeriod &track_period,
                                                          struct Patch *patch,
                                                          struct Notes *note
                                                          )
{

  VelocityIterateCallback callback(patch, note);

  r::TimeData<r::Velocity>::Reader reader(note->_velocities, seqblock->cache_num);
      
  reader.iterate_extended<int>(seqtrack,
                               seqblock,
                               track,
                               play_id,
                               seqtime_start,
                               track_period,
                               callback,
                               r::TimeDataSimpleNode<int>(place2ratio(note->l.p), note->velocity, note->velocity_first_logtype),
                               r::TimeDataSimpleNode<int>(note->end, note->velocity_end)
                               );
}

void RT_VELOCITIES_called_each_block(struct SeqTrack *seqtrack,
                                     const int play_id,
                                     const struct SeqBlock *seqblock,
                                     const struct Tracks *track,
                                     const int64_t seqtime_start,
                                     const int64_t seqtime_end,
                                     const r::RatioPeriod &period
                                     )
{
  R_ASSERT_NON_RELEASE(period._end >= period._start);

  struct Patch *patch = track->patch;
  if (patch==NULL)
    return;
  
  int tracknum = track->l.num;

  if( tracknum >= seqblock->playing_notes->size())
    return; // The seqblock->playing_notes vector is not expanded to cover all tracks unless there are playing notes on all tracks.


  const radium::RT_NoteVector &playing_notes = *seqblock->playing_notes->at_ref(tracknum);

  
#if !defined(RELEASE)
  if (tracknum==0) {
    static radium::RT_NoteVector prev_playing_notes;
    
    if (prev_playing_notes.size() != playing_notes.size())
      goto not_equal;

    for(int i = 0 ; i < playing_notes.size() ; i++)
      if (prev_playing_notes.at_ref(i) != playing_notes.at_ref(i))
        goto not_equal;

    goto equal;
    
  not_equal:

    printf("=========== Playing notes:");
    
    prev_playing_notes.clear();
    for(auto *note : playing_notes){
      printf(" %d,", (int)note->note);
      prev_playing_notes.push_back(note);
    }
    printf("\n");
  }
 equal:
#endif // !defined(RELEASE)
  

  for(struct Notes *note : playing_notes){

    //printf("Handling velocities for note %f. Track %d\n", note->note, tracknum); 
    RT_VELOCITIES_called_each_block_for_each_note(seqtrack, play_id, seqblock, track, seqtime_start, period, patch, note);
                                     
  }
  
  return;
}


