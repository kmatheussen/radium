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
#include "trackreallines_proc.h"
#include "placement_proc.h"
#include "undo_notes_proc.h"
#include "undo_blocks_proc.h"
#include "player_pause_proc.h"
#include "player_proc.h"
#include "range_proc.h"
#include "windows_proc.h"

#include "velocities_proc.h"


extern struct Root *root;


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
		if(num_vel>wtrack->num_vel) wtrack->num_vel=num_vel;
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


int AddVelocity(
                int velocityvelocity,
                const Place *placement,
                struct Notes *note
){
  if(PlaceLessOrEqual(placement, &note->l.p)) return -1;
  if(PlaceGreaterOrEqual(placement, &note->end)) return -1;
  struct Velocities *velocity=talloc(sizeof(struct Velocities));
  PlaceCopy(&velocity->l.p,placement);
  velocity->velocity=R_BOUNDARIES(0,velocityvelocity,MAX_VELOCITY);
  
  /* ListAddElement3_ns returns -1 (and doesnt do anything else)
     if there allready is an element with the same placement. */
  return ListAddElement3_ns(&note->velocities,&velocity->l);
}

void AddVelocityCurrPos(struct Tracker_Windows *window){
	PC_Pause();

	struct WBlocks *wblock=window->wblock;
	struct LocalZooms *realline= wblock->reallines[wblock->curr_realline];
	int subtrack=window->curr_track_sub;

        if(-1==subtrack)
          return;

	Undo_Notes_CurrPos(window);

        struct Notes *note = FindNoteOnSubTrack(
                                                wblock->wtrack->track,
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

	UpdateTrackReallines(window,wblock,wblock->wtrack);

#if !USE_OPENGL
	ClearTrack(window,wblock,wblock->wtrack,wblock->top_realline,wblock->bot_realline);
	UpdateWTrack(window,wblock,wblock->wtrack,wblock->top_realline,wblock->bot_realline);
#endif

	PC_StopPause();
}


#if 0
static struct Notes *get_note_at_realline(struct Tracker_Windows *window,struct WBlocks *wblock, struct WTracks *wtrack, int realline){
  struct TrackRealline *trackrealline= &wtrack->trackreallines[realline];

  
}
#endif


void IncreaseVelocityCurrPos(struct Tracker_Windows *window,int inc){
	struct WBlocks *wblock;
	struct WTracks *wtrack;
	int maxvelocity;

        inc = inc * MAX_VELOCITY / 100;


	wblock=window->wblock;
	wtrack=wblock->wtrack;
	maxvelocity=MAX_VELOCITY;
        
        if(is_track_ranged(wblock,wblock->wtrack) && is_realline_ranged(wblock,wblock->curr_realline)){

          vector_t *notes = get_all_ranged_notes(wblock);
          //printf("num_elements: %d\n",notes->num_elements);

          if(notes->num_elements==0)
            return;

          PC_Pause();

          Undo_Block_CurrPos(window);

          VECTOR_FOR_EACH(struct Notes *note,notes){ 
            note->velocity=R_BOUNDARIES(0,note->velocity+inc,maxvelocity);
            if(note->velocities==NULL)
              note->velocity_end=R_BOUNDARIES(0,note->velocity_end+inc,maxvelocity);
          }END_VECTOR_FOR_EACH;

          UpdateAllTrackReallines(window,wblock);
          window->must_redraw = true;

        } else {

          PC_Pause();

          Undo_Notes_CurrPos(window);

          struct Notes *note = FindNoteCurrPos(window);

          if (note != NULL) {
            note->velocity=R_BOUNDARIES(0,note->velocity+inc,maxvelocity);
            if(note->velocities==NULL)
              note->velocity_end=R_BOUNDARIES(0,note->velocity_end+inc,maxvelocity);
            
            UpdateTrackReallines(window,wblock,wtrack);
          }
        }

	PC_StopPause();
}























