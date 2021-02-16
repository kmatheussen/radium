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
#include "undo.h"
#include "undo_tracks_proc.h"
#include "fxlines_proc.h"
#include "clipboard_track_copy_proc.h"
#include "clipboard_track_paste_proc.h"
#include "list_proc.h"

#include "undo_tracks_proc.h"


struct Undo_Tracks{
  NInt blocknum;
  NInt tracknum;
  struct WTracks *wtrack;
};

static void *Undo_Do_Track(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);

// TODO: API should change. It should only take blocknum and tracknum as arguments.
void ADD_UNDO_FUNC(Track(
                         struct Tracker_Windows *window,
                         struct WBlocks *wblock,
                         struct WTracks *wtrack,
                         int realline,
                         NInt blocknum,
                         NInt tracknum
                         )
                   )
{
  R_ASSERT_RETURN_IF_FALSE(blocknum>=0);
  R_ASSERT_RETURN_IF_FALSE(tracknum>=0);
  
  struct WBlocks *wblock_to_undo = ListFindElement1(&window->wblocks->l, blocknum);
  struct WTracks *wtrack_to_undo = ListFindElement1(&wblock_to_undo->wtracks->l, tracknum);
  
  struct Undo_Tracks *undo_tracks = talloc(sizeof(struct Undo_Tracks));
  undo_tracks->blocknum = blocknum;
  undo_tracks->tracknum = tracknum;
  undo_tracks->wtrack = CB_CopyTrack(wblock_to_undo,wtrack_to_undo);
    
  Undo_Add(
           window->l.num,
           wblock->l.num,
           wtrack->l.num,
           realline,
           undo_tracks,
           Undo_Do_Track,
           "Track"
           );
}

// TODO: Track_CurrPos is supposed to undo current track. This made it easy to do the wrong thing, so I changed the API to take blocknum and tracknum as arguments instead of window. But that doesn't make sense since it's not a "CurrPos" function anymore. The best thing would be to remove this function, and instead change the Track undo function (above) to only take these two arguments.
void ADD_UNDO_FUNC(Track_CurrPos(
                                 NInt blocknum,
                                 NInt tracknum
                                 )
                   )
{
  R_ASSERT_RETURN_IF_FALSE(blocknum>=0);
  R_ASSERT_RETURN_IF_FALSE(tracknum>=0);
  
  struct Tracker_Windows *window = root->song->tracker_windows;

  CALL_ADD_UNDO_FUNC(Track(window,window->wblock,window->wblock->wtrack,window->wblock->curr_realline, blocknum, tracknum));
}

static void *Undo_Do_Track(
	struct Tracker_Windows *window,
	struct WBlocks *currwblock,
	struct WTracks *currwtrack,
	int realline,
	void *pointer
){
  struct Undo_Tracks *undo_tracks = pointer;

  struct WBlocks *wblock = ListFindElement1(&window->wblocks->l, undo_tracks->blocknum);
  struct WTracks *wtrack = ListFindElement1(&wblock->wtracks->l, undo_tracks->tracknum);

  struct WTracks *temp=CB_CopyTrack(wblock,wtrack);
  
  printf("*** undo_do_track called. Tracknum: %d\n",wtrack->l.num);


  mo_CB_PasteTrack(wblock, undo_tracks->wtrack, wtrack);
  
  undo_tracks->wtrack = temp;
  return undo_tracks;
}


