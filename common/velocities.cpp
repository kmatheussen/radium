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


#if 0

struct FindNum_Velstruct{
	struct ListHeader3 l;
	int startstop;		/* 1=start, -1=stop */
};
#define NextFindNum_Velstruct(a) ((struct FindNum_Velstruct *)((a)->l.next))


/*******************************************************************
  FUNCTION
    Set Maximum number of velocitylines showed simultaniously on
    track (i.e wtrack->num_vel). wtrack->num_vel is the same
    as maximum polyphony for track. The next three procedures are
    to be viewed as one function.
*******************************************************************/
static void SetNum_Vel_set(
	struct WTracks *wtrack,
	struct FindNum_Velstruct **velroot
){
	struct FindNum_Velstruct *velstruct=*velroot;
	int num_vel=0;

	wtrack->num_vel=0;

	while(velstruct!=NULL){
		num_vel+=velstruct->startstop;
		if(num_vel>wtrack->num_vel)
                  wtrack->num_vel=num_vel;
		velstruct=NextFindNum_Velstruct(velstruct);
	}
	wtrack->num_vel=R_MAX(1,wtrack->num_vel);
}

static void SetNum_Vel_rec(
	struct WTracks *wtrack,
	struct FindNum_Velstruct **velroot,
	struct Notes *note
){
	struct FindNum_Velstruct temp_start;
	struct FindNum_Velstruct temp_end;

	if(note==NULL){
		SetNum_Vel_set(wtrack,velroot);
		return;
	}

	PlaceCopy(&temp_start.l.p,&note->l.p);
	temp_start.startstop=1;
	ListAddElement3_a(velroot,&temp_start.l);

	PlaceCopy(&temp_end.l.p,&note->end);
	temp_end.startstop= -1;
	ListAddElement3(velroot,&temp_end.l);

	SetNum_Vel_rec(wtrack,velroot,NextNote(note));
}

void SetNum_Vel(
	struct WTracks *wtrack
){
	struct FindNum_Velstruct *velroot=NULL;

	SetNum_Vel_rec(wtrack,&velroot,wtrack->track->notes);
}
#endif

static struct Velocities *add_velocity(
                                int velocityvelocity,
                                const Place *placement,
                                struct Notes *note,
                                int *pos
                                )
{
  *pos = -1;
  
  if(PlaceLessOrEqual(placement, &note->l.p)) return NULL;
  if(PlaceGreaterOrEqual(placement, &note->end)) return NULL;
  struct Velocities *velocity=(struct Velocities*)talloc(sizeof(struct Velocities));
  PlaceCopy(&velocity->l.p,placement);
  velocity->velocity=R_BOUNDARIES(0,velocityvelocity,MAX_VELOCITY);
  
  /* ListAddElement3_ns returns -1 (and doesnt do anything else)
     if there already is an element with the same placement. */

  {
    SCOPED_PLAYER_LOCK_IF_PLAYING();
    *pos = ListAddElement3_ns(&note->velocities,&velocity->l);
  }

  if (*pos==-1)
    return NULL;
  else
    return velocity;
}

struct Velocities *AddVelocity2(
                                int velocityvelocity,
                                const Place *placement,
                                struct Notes *note
                                )
{
  int ret;
  return add_velocity(velocityvelocity, placement, note, &ret);
}

int AddVelocity(
                int velocityvelocity,
                const Place *placement,
                struct Notes *note
){
  int ret;

  add_velocity(velocityvelocity, placement, note, &ret);

  return ret;
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
  
  struct Velocities *velocity = note->velocities;
  while(velocity != NULL){
    velocity->velocity = R_BOUNDARIES(0,velocity->velocity+inc,maxvelocity);
    velocity = NextVelocity(velocity);
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























