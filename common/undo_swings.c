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
#include "clipboard_tempos_copy_proc.h"
#include "placement_proc.h"
#include "tempos_proc.h"
#include "list_proc.h"
#include "time_proc.h"

#include "undo_swings_proc.h"

struct SwingUndo{
  struct Swing *swing;
  int tracknum;
};

static void *Undo_Do_Swings(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);


static void ADD_UNDO_FUNC(Swings(
                                 struct Tracker_Windows *window,
                                 struct Blocks *block,
                                 struct Tracks *track,
                                 NInt tracknum,
                                 int realline
                                 )
                          )
{
  struct SwingUndo *su = talloc(sizeof(struct SwingUndo));
  su->swing = CB_CopySwings(track==NULL ? block->swings : track->swings, NULL);
  su->tracknum = track==NULL ? -1 : track->l.num;
  
  Undo_Add(
           window->l.num,
           block->l.num,
           tracknum,
           realline,
           su,
           Undo_Do_Swings,
           track==NULL ? "Track Swings" : "Block Swings"
           );
}

void ADD_UNDO_FUNC(Swings_CurrPos(
                                  struct Tracker_Windows *window,
                                  struct Tracks *track
                                  )
                   )
{
  struct WBlocks *wblock = window->wblock;
  CALL_ADD_UNDO_FUNC(Swings(window,
                            wblock->block,
                            track,
                            window->curr_track,
                            wblock->curr_realline
                            )
                     );
}

static void *Undo_Do_Swings(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
  struct SwingUndo *su = (struct SwingUndo*)pointer;
  struct Swing *undo_swings = su->swing;

  struct Blocks *block = wblock->block;
  
  struct Tracks *track = su->tracknum < 0 ? NULL : ListFindElement1(&block->tracks->l,  su->tracknum);
  
  struct Swing *temp = track==NULL ? block->swings : track->swings;

  if (track==NULL)
    block->swings = undo_swings;
  else
    track->swings = undo_swings;
  
  TIME_block_swings_have_changed(wblock->block);

  su->swing = temp;
  
  return su;
}




