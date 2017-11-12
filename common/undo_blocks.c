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
#include "playerclass.h"
#include "undo.h"
#include "list_proc.h"
#include "clipboard_block_copy_proc.h"
#include "clipboard_block_paste_proc.h"
#include "../audio/Mixer_proc.h"

#include "undo_blocks_proc.h"

extern PlayerClass *pc;

static void *Undo_Do_Block(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);


void ADD_UNDO_FUNC(Block(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline
                         )
                   )
{
  static struct WBlocks *last_wblock;
  static struct WTracks *last_wtrack;
  static int last_realline;

  static double last_undo_block_time = -1000;
  double time_now = TIME_get_ms();
  
  //printf("last: %d, now: %d, diff: %d\n",(int)last_undo_block_time,(int)time_now,(int)(time_now-last_undo_block_time));

  if( (time_now-last_undo_block_time) > 500 // more than 1/2 seconds.
      || wblock!=last_wblock
      || wtrack!=last_wtrack
      || realline!=last_realline
      || Undo_get_last_function()!=Undo_Do_Block
      )
    {
      Undo_Add(
               window->l.num,
               wblock->l.num,
               wtrack->l.num,
               realline,
               CB_CopyBlock(wblock),
               Undo_Do_Block,
               "Block"
               );
      last_undo_block_time = time_now;
      last_wblock=wblock;
      last_wtrack=wtrack;
      last_realline=realline;
    }
}

void ADD_UNDO_FUNC(Block2(
                          int blocknum
                          )
                   )
{
  R_ASSERT_RETURN_IF_FALSE(blocknum>=0 && blocknum < root->song->num_blocks);
  struct Tracker_Windows *window = root->song->tracker_windows;
  struct WBlocks *wblock = ListFindElement1_r0(&window->wblocks->l, blocknum);
  R_ASSERT_RETURN_IF_FALSE(wblock!=NULL);
  CALL_ADD_UNDO_FUNC(Block(window,wblock,wblock->wtrack,wblock->curr_realline));
}

void ADD_UNDO_FUNC(Block_CurrPos(
                                 struct Tracker_Windows *window
                                 )
                   )
{
  CALL_ADD_UNDO_FUNC(Block(window,window->wblock,window->wblock->wtrack,window->wblock->curr_realline));
}

static void *Undo_Do_Block(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){

	struct WBlocks *undo_wblock=(struct WBlocks *)pointer;
	struct WBlocks *temp=CB_CopyBlock(wblock);

	CB_PasteBlock(window,undo_wblock,wblock);

	return temp;
}



