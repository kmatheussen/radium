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
#include "clipboard_block_copy_proc.h"
#include "clipboard_block_paste_proc.h"
#include "../audio/Mixer_proc.h"

#include "undo_blocks_proc.h"

extern PlayerClass *pc;

void *Undo_Do_Block(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);


void Undo_Block(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline
){
  static struct WBlocks *last_wblock;
  static struct WTracks *last_wtrack;
  static int last_realline;

  static int64_t last_undo_block_time = -1000;
  int64_t time_now = MIXER_get_time();

  //printf("last: %d, now: %d, diff: %d\n",(int)last_undo_block_time,(int)time_now,(int)(time_now-last_undo_block_time));

  if( (time_now-last_undo_block_time) > pc->pfreq*3 // more than 3 seconds.
      || wblock!=last_wblock
      || wtrack!=last_wtrack
      || realline!=last_realline
      || Undo_get_last_function()!=Undo_Do_Block)
    Undo_Add(
             window->l.num,
             wblock->l.num,
             wtrack->l.num,
             realline,
             CB_CopyBlock(wblock),
             Undo_Do_Block
             );

  last_undo_block_time = time_now;
  last_wblock=wblock;
  last_wtrack=wtrack;
  last_realline=realline;
}

void Undo_Block_CurrPos(
	struct Tracker_Windows *window
){
	Undo_Block(window,window->wblock,window->wblock->wtrack,window->wblock->curr_realline);
}

void *Undo_Do_Block(
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



