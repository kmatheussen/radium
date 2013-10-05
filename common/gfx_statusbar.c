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
#include "visual_proc.h"
#include "time_proc.h"
#include "playerclass.h"
#include <string.h>
#include "blocklist_proc.h"

#include "gfx_statusbar_proc.h"

extern struct Root *root;
extern PlayerClass *pc;

char firstringinstatusbar[512]={0};

void GFX_DrawStatusBar(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){
	int blocklength=getBlockSTimeLength(wblock->block)/pc->pfreq;
	int minutes=blocklength/60;
	int seconds=blocklength%60;

	sprintf(
		wblock->title,
		"%s%d*(%d~%d) - %d:%02d - %d/%d/%d - %d/%d:%.64s",
		firstringinstatusbar,
		wblock->block->num_tracks,
		wblock->block->num_lines,
		wblock->num_reallines,
		minutes,
		seconds,
		BL_GetBlockFromPos(root->curr_playlist-1)==NULL?-1:BL_GetBlockFromPos(root->curr_playlist-1)->l.num,
		BL_GetBlockFromPos(root->curr_playlist)==NULL?-1:BL_GetBlockFromPos(root->curr_playlist)->l.num,
		BL_GetBlockFromPos(root->curr_playlist+1)==NULL?-1:BL_GetBlockFromPos(root->curr_playlist+1)->l.num,
		wblock->l.num,
		root->song->num_blocks-1,
		wblock->block->name
	);

	GFX_SetStatusBar(window,wblock->title);
}

char *GFX_GetChangeString(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){
  return firstringinstatusbar;
}

void GFX_SetChangeInt(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	const char *title,
	int Int
){
	sprintf(firstringinstatusbar,"%s%s%d - ",title,title==NULL?"":": ",Int);
}

void GFX_SetChangeFloat(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	const char *title,
	float Float
){
	sprintf(
		firstringinstatusbar,
		"%s%s%.2f - ",
		title,title==NULL?"":": ",
		Float
	);
}



