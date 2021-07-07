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
    r::VelocityTimeData::Writer writer(note->_velocities);

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
    r::VelocityTimeData::Writer writer(note->_velocities);
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


