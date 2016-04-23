/* Copyright 2012 Kjetil S. Matheussen

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
#include "undo.h"

#include "OS_Bs_edit_proc.h"
#include "blocklist_proc.h"

#include "undo_playlist_proc.h"

extern struct Root *root;


static void *Undo_Do_Playlist(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);


void Undo_Playlist(void){
  struct Tracker_Windows *window = root->song->tracker_windows;

  Undo_Add(
           window->l.num,
           window->wblock->l.num,
           window->curr_track,
           window->wblock->curr_realline,
           BL_copy(),
           Undo_Do_Playlist,
           "Playlist"
           );
}

void *Undo_Do_Playlist(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
  int *ret = BL_copy();

  BL_paste(pointer);
  BS_UpdateBlockList();
  BS_UpdatePlayList();

  return ret;
}
