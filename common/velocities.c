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
#include "notes_proc.h"
#include "list_proc.h"
#include "trackreallines_proc.h"
#include "gfx_wtracks_proc.h"
#include "placement_proc.h"
#include "undo_notes_proc.h"
#include "player_pause_proc.h"
#include "player_proc.h"

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
void SetNum_Vel_set(
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

void SetNum_Vel_rec(
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


void AddVelocity(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int subtrack,
	int velocityvelocity,
	Place *placement,
	int realline
){
	struct Velocities *velocity;
	struct Notes *note;

	if(-1==subtrack) return;

	note=FindNoteOnSubTrack(
		window,
		wblock,
		wtrack,
		subtrack,
		realline,
		placement
	);
	if(note==NULL) return;
	if(PlaceEqual(&note->l.p,placement)) return;
	if(PlaceEqual(&note->end,placement)) return;

	velocity=talloc(sizeof(struct Velocities));
	PlaceCopy(&velocity->l.p,placement);
	velocity->velocity=velocityvelocity;

	/* ListAddElement3_ns returns NULL (and doesnt do anything else)
	   if there allready is an element with the same placement. */
	if(ListAddElement3_ns(&note->velocities,&velocity->l)==NULL) return;

}

void AddVelocityCurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock;
	struct LocalZooms *realline;
	int subtrack;

	PC_Pause();

	wblock=window->wblock;
	realline= wblock->reallines[wblock->curr_realline];
	subtrack=window->curr_track_sub;

	Undo_Notes_CurrPos(window);

	AddVelocity(
		window,
		wblock,
		wblock->wtrack,
		subtrack,
//		30,
		wblock->wtrack->track->patch==NULL?root->standardvel:wblock->wtrack->track->patch->standardvel,
		&realline->l.p,
		wblock->curr_realline
	);

	UpdateTrackReallines(window,wblock,wblock->wtrack);
	ClearTrack(window,wblock,wblock->wtrack,wblock->top_realline,wblock->bot_realline);
	UpdateWTrack(window,wblock,wblock->wtrack,wblock->top_realline,wblock->bot_realline);

	PC_StopPause();
}

void IncreaseVelocityCurrPos(struct Tracker_Windows *window,int inc){
	struct WBlocks *wblock;
	struct WTracks *wtrack;
	struct TrackRealline *trackrealline;
	struct TrackReallineElements *element;
	struct Notes *note;
	int maxvelocity;

	wblock=window->wblock;
	wtrack=wblock->wtrack;
	trackrealline= &wtrack->trackreallines[wblock->curr_realline];
	maxvelocity=(*wtrack->track->instrument->getMaxVelocity)(wtrack->track);

	if(trackrealline->note<=0 || trackrealline->note>=NOTE_MUL) return;

	PC_Pause();

	Undo_Notes_CurrPos(window);

	element=trackrealline->trackreallineelements;

	while(element->type!=TRE_THISNOTELINES) element=element->next;
	note=(struct Notes *)element->pointer;

	note->velocity=R_BOUNDARIES(0,note->velocity+inc,maxvelocity);
	if(note->velocities==NULL)
		note->velocity_end=R_BOUNDARIES(0,note->velocity_end+inc,maxvelocity);


	UpdateTrackReallines(window,wblock,wtrack);
	ClearTrack(window,wblock,wtrack,wblock->top_realline,wblock->bot_realline);
	UpdateWTrack(window,wblock,wtrack,wblock->top_realline,wblock->bot_realline);

	PC_StopPause();
}























