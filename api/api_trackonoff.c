/* Copyright 2001 Kjetil S. Matheussen

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

#include "../common/includepython.h"

#include "../common/nsmtracker.h"
#include "../common/track_onoff_proc.h"


#include "api_common_proc.h"


#include "radium_proc.h"

extern struct Root *root;

void allTracksOn(void){
  AllTracksOn_CurrPos(root->song->tracker_windows);
}

void switchTrackOn(int tracknum,int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

	TRACK_OF_switch_spesified_CurrPos(window,tracknum==-1?window->wblock->wtrack->l.num:(NInt)tracknum);
}

void soloTrack(int tracknum,int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

	TRACK_OF_solo_spesified_CurrPos(window,tracknum==-1?window->wblock->wtrack->l.num:(NInt)tracknum);
}

void switchSoloTrack(int tracknum,int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  struct WBlocks *wblock=getWBlockFromNum(windownum,-1);
  struct WTracks *wtrack=getWTrackFromNum(windownum,-1,tracknum);

  if(window==NULL || wblock==NULL || wtrack==NULL)
    return;

  TRACK_OF_switch_solo_spesified_CurrPos(window,wblock->block,wtrack->track);
}

