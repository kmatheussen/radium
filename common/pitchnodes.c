/* Copyright 2000-2013 Kjetil S. Matheussen

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
#include "list_proc.h"
#include "placement_proc.h"
#include "nodelines_proc.h"
#include "nodelines.h"
#include "gfx_wblocks_proc.h"
#include "time_proc.h"
#include "player_proc.h"

#include "pitchnodes_proc.h"


static void MakeWPitchesCallBack(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
        struct WTracks *wtrack,
	void *extrainfo,
	int firstlast,
	int realline,
	float u_y1,float u_y2,
	float u_x1,float u_x2
){
	struct Pitches *pitchnode=(struct Pitches *)extrainfo;
	WPitches *wpitch = talloc(sizeof(WPitches));

	wpitch->type=TRE_PITCHLINE;
	wpitch->y1=u_y1;
	wpitch->y2=u_y2;
	wpitch->x1=u_x1;
	wpitch->x2=u_x2;
	wpitch->pointer=pitchnode;

	wpitch->next=wtrack->wpitches[realline];
	wtrack->wpitches[realline]=wpitch;

	if(firstlast==NODELINE_FIRST || firstlast==NODELINE_FIRSTANDLAST){
		WPitches *wpitch = talloc(sizeof(WPitches));

		wpitch->type=TRE_PITCHNODE;

		wpitch->x1=u_x1;
		wpitch->y1=u_y1;

		wpitch->pointer=pitchnode;
		wpitch->next=wtrack->wpitches[realline];
		wtrack->wpitches[realline]=wpitch;
	}

        if(firstlast==NODELINE_LAST){
          WPitches *wpitch = talloc(sizeof(WPitches));
	
          wpitch->type=TRE_PITCHNODE;

          wpitch->x1=u_x2;
          wpitch->y1=u_y2;

          wpitch->pointer=pitchnode;
          wpitch->next=wtrack->wpitches[realline];
          wtrack->wpitches[realline]=wpitch;
	}
}


void UpdateWPitches(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
        struct WTracks *wtrack,
        struct Notes *note,
        float min_pitch,
        float max_pitch
){
  
#define MakeNodeLine() MakeNodeLines(                              \
                                     window,                       \
                                     wblock,                       \
                                     wtrack,                       \
                                     prev_place, curr_place,       \
                                     prev_note, curr_note,         \
                                     min_pitch, max_pitch,         \
                                     NULL,                         \
                                     &MakeWPitchesCallBack         \
                                                                   )

#define SetNextPitch()                             \
  prev_place = curr_place;                         \
  prev_note = curr_note;                           \
  curr=NextPitch(curr)


        Place *prev_place = &note->l.p;
        float prev_note = note->note;

	struct Pitches *curr=note->pitches;
        Place *curr_place = curr==NULL ? &note->end : &curr->l.p;
        float curr_note = curr==NULL ? note->note : curr->note;

        if (min_pitch==max_pitch) {
          min_pitch = curr_note - 1.0f;
          max_pitch = curr_note + 1.0f;
        }

        MakeNodeLine();

        if(curr==NULL)
          return;

        SetNextPitch();

	while(curr!=NULL){
          curr_place = &curr->l.p;
          curr_note = curr->note;

          MakeNodeLine();

          SetNextPitch();
	}

        curr_place = &note->end;
        curr_note = NextNote(note) == NULL ? wtrack->track->notes->note : NextNote(note)->note;
        MakeNodeLine();
}

