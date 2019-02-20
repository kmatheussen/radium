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
#include "windows_proc.h"
#include "list_proc.h"
#include "gfx_wtrackheaders_proc.h"

#include "track_onoff_proc.h"

extern struct Root *root;

static void SwitchTrackOnOff(
	struct Tracks *track
){
  if(track->onoff==1){
    track->onoff=0;
  }else{
    track->onoff=1;
  }
}

static void SetTrackOnOff(
                          struct Tracks *track,
                          bool ison
){
  if(ison){
    track->onoff=1;
  }else{
    track->onoff=0;
  }
}

static void SwitchAllTracksOnOff(
	NInt tracknum
){
	struct Blocks *block=root->song->blocks;
	struct Tracks *track;
	while(block!=NULL){
		track=ListFindElement1_r0(&block->tracks->l,tracknum);
		if(track!=NULL){
			SwitchTrackOnOff(track);
		}
		block=NextBlock(block);
	}
}

void SwitchTrackOnOff_CurrPos(
	struct Tracker_Windows *window
){
	if(window->curr_track<0) return;

	SwitchAllTracksOnOff(window->curr_track);

        window->must_redraw = true;
}

static void SoloTrack(
                      NInt tracknum
){
	struct Blocks *block=root->song->blocks;
	struct Tracks *track;

	while(block!=NULL){
		track=block->tracks;
		while(track!=NULL){
			if(track->l.num==tracknum){
                          track->onoff=1;
			}else{
                          track->onoff=0;
			}
			track=NextTrack(track);
		}
		block=NextBlock(block);
	}
}

void SoloTrack_CurrPos(
	struct Tracker_Windows *window
){
	if(window->curr_track<0) return;

	SoloTrack(window->curr_track);

        window->must_redraw = true;
}

static void AllTracksOn(
	NInt tracknum
){
	struct Blocks *block=root->song->blocks;
	struct Tracks *track;

	while(block!=NULL){
		track=block->tracks;
		while(track!=NULL){
                  printf("AllTracksOn %d\n",track->l.num);
			track->onoff=1;
			track=NextTrack(track);
		}
		block=NextBlock(block);
	}
}

void AllTracksOn_CurrPos(
	struct Tracker_Windows *window
){
	if(window->curr_track<0) return;

	AllTracksOn(window->curr_track);

        window->must_redraw = true;
}


void TRACK_OF_switch_spesified_CurrPos(
	struct Tracker_Windows *window,
	NInt tracknum
){
	if(tracknum>=window->wblock->block->num_tracks) return;

        struct WTracks *wtrack = (struct WTracks*)ListFindElement1(&window->wblock->wtracks->l,tracknum);
        
	SwitchTrackOnOff(wtrack->track);

        window->must_redraw = true;
}

void TRACK_set_on_off(
	struct Tracker_Windows *window,
	NInt tracknum,
        bool is_on
){
	if(tracknum>=window->wblock->block->num_tracks) return;

        struct WTracks *wtrack = (struct WTracks*)ListFindElement1(&window->wblock->wtracks->l,tracknum);
        
	SetTrackOnOff(wtrack->track, is_on);

        window->must_redraw = true;
}

void TRACK_OF_solo_spesified_CurrPos(
	struct Tracker_Windows *window,
	NInt tracknum
){
	if(tracknum>=window->wblock->block->num_tracks) return;

	SoloTrack(tracknum);
        window->must_redraw = true;
}


void TRACK_OF_switch_solo_spesified_CurrPos(
	struct Tracker_Windows *window,
        NInt tracknum
){
  if(tracknum>=window->wblock->block->num_tracks)
    return;

  struct WBlocks *wblock = window->wblock;
  struct Blocks *block = wblock->block;
  
  struct WTracks *wtrack = (struct WTracks*)ListFindElement1(&wblock->wtracks->l,tracknum);
  struct Tracks *track = wtrack->track;
  
  bool set_solo = true;

  if(track->onoff==true){
    if(track->l.next!=NULL && NextTrack(track)->onoff==false)
      set_solo = false;
    else if(track->l.num>0 && ((struct Tracks*)(ListPrevElement1(&block->tracks->l, &track->l)))->onoff==false)
      set_solo = false;
  }

  printf("TRACK_OF_switch_solo_specified_CurrPos\n");
   
  if(set_solo==true)
    SoloTrack(tracknum);
  else
    AllTracksOn(tracknum);

  window->must_redraw = true;
}


