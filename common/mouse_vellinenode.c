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
#include "list_proc.h"
#include "placement_proc.h"
#include "common_proc.h"
#include "trackreallines_proc.h"
#include "gfx_wtracks_proc.h"
#include "gfx_subtrack_proc.h"
#include "notes_proc.h"
#include "undo_notes_proc.h"
#include "gfx_window_title_proc.h"
#include "player_proc.h"
#include "realline_calc_proc.h"

#include "mouse_vellinenode_proc.h"



extern struct Root *root;

int MoveVelNode_Mouse(
	struct Tracker_Windows *window,
	int x,int y
){
	struct WBlocks *wblock=window->wblock;
	struct MouseAction *action= &window->prevaction;
	struct WTracks *wtrack=(struct WTracks *)action->pointer1;
	struct Notes *note=(struct Notes *)action->pointer2;
	struct Velocities *velocity=(struct Velocities *)action->pointer3;
	int subtrack=action->eint1;
	Place place,prev_vel,next_vel,*temp;
	int realline;
	int maxvel=(*wtrack->track->instrument->getMaxVelocity)(wtrack->track);
	int sx1=GetXSubTrack1(wtrack,subtrack);
	int sx2=GetXSubTrack2(wtrack,subtrack);

	int start_realline,end_realline;

	/* Do a check to see if all data is still valid. */
	if(isInList1_m(
		     &window->wblocks->l,&wblock->l,
		     &wblock->wtracks->l,&wtrack->l,
		     &wtrack->track->notes->l,&note->l,
		     &note->velocities->l,&velocity->l,
		     root
		     )==false){
	  action->action=NOACTION;
	  return 0;
	}

	PlayStop();

	if(note->velocities==velocity){
		PlaceCopy(&prev_vel,&note->l.p);
	}else{
	  temp=&((struct ListHeader3 *)(ListPrevElement3(&note->velocities->l,&velocity->l)))->p;
	  PlaceCopy(&prev_vel,temp);
	}

	if(NextVelocity(velocity)==NULL){
		PlaceCopy(&next_vel,&note->end);
	}else{
		temp=&(NextVelocity(velocity)->l.p);
		PlaceCopy(&next_vel,temp);
	}

	realline=GetReallineAndPlaceFromY(window,wblock,y,&place,&prev_vel,&next_vel);

	velocity->velocity=(maxvel*(x-sx1)/(sx2-sx1));
	if(
		realline<=-window->fontheight*2 ||
		x< sx1-window->fontheight*3 ||
		x> sx2+window->fontheight*3
	){
		ListRemoveElement3(&note->velocities,&velocity->l);
		action->action=NOACTION;
	}else{
		velocity->velocity=R_BOUNDARIES(0,velocity->velocity,maxvel);
		PlaceCopy(&velocity->l.p,&place);
		GFX_SetChangeInt(window,wblock,"Velocity",velocity->velocity);
		GFX_DrawWindowTitle(window,wblock);
	}

	UpdateTrackReallines(window,wblock,wtrack);

	start_realline=FindRealLineFor(wblock,0,&prev_vel);
	end_realline=FindRealLineFor(wblock,start_realline,&next_vel);

	ClearTrack(window,wblock,wtrack,
		   start_realline,
		   end_realline
		   //		   wblock->top_realline,wblock->bot_realline
		   );

	UpdateWTrack(window,wblock,wtrack,
		   start_realline,
		   end_realline
		     //		     wblock->top_realline,wblock->bot_realline
		     );

	return 0;
}

void SetMouseActionVelline(
	struct Tracker_Windows *window,
	struct MouseAction *action,
	struct WTracks *wtrack,
	struct Velocities *velocity,
	int realline,
	int subtrack,
	int x,int y,
	int click
){
	struct Notes *note;

	note=FindNoteOnSubTrack(window,window->wblock,wtrack,subtrack,realline,&velocity->l.p);
	
	action->action=VELNODE;
	action->pointer1=wtrack;
	action->pointer2=note;
	action->pointer3=velocity;
	action->eint1=subtrack;
	action->MouseUpFunction= &MoveVelNode_Mouse;

	if(click==1){
		PlayStop();
		Undo_Notes(window,window->wblock->block,wtrack->track,window->wblock->curr_realline);
	}

	return;
}



