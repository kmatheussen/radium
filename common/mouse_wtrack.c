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

#include "visual_proc.h"

#include "mouse_wtrackborder_proc.h"
#include "mouse_fxarea_proc.h"
#include "mouse_pitches_proc.h"
#include "mouse_wtrackheader_proc.h"

#include "mouse_wtrack_proc.h"



void SetMouseActionWTrack(
	struct Tracker_Windows *window,
	struct MouseAction *action,
	int x,int y,
	int click
){
	struct WBlocks *wblock=window->wblock;
	NInt wtracknum=wblock->left_track;
	NInt rwtracknum=wblock->right_track;
	struct WTracks *wtrack=ListFindElement1(&wblock->wtracks->l,wtracknum);
	struct WTracks *rwtrack=ListFindElement1(&wtrack->l,rwtracknum);

	for(;;){
		if(wtrack->l.num==rwtracknum) break;
		if(x>wtrack->x && x<wtrack->x2) break;
		wtrack=NextWTrack(wtrack);
	}

        wblock->mouse_track=wtrack->l.num;

	if(wtrack->l.num==rwtracknum && x>=rwtrack->fxarea.x2 && x<=rwtrack->fxarea.x2+5){
		SetMouseActionWTrackBorder(window,action,x);
		return;
	}

	if(y<wblock->t.y1){
          SetMouseActionWTrackHeader(window,action,wtrack,x,y,click);
          return;
	}

	if(x<wtrack->fxarea.x){
          if (SetMouseActionPitches(window,action,wtrack,x,y,click)==false)
            SetMouseActionWTrackBorder(window,action,x);
          return;
	}

	SetMouseActionFXarea(window,action,wtrack,x,y,click);

	return;
}


