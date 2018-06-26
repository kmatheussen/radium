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
#include "reallines_proc.h"
#include "clipboard_localzooms_proc.h"
#include "OS_Player_proc.h"

#include "undo_reallines_proc.h"

struct Undo_Reallines{
  struct LocalZooms *localzooms;
  int line_zoom;
};


static void *Undo_Do_Reallines(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);


static void ADD_UNDO_FUNC(Reallines(
                             struct Tracker_Windows *window,
                             struct WBlocks *wblock,
                             NInt tracknum,
                             int realline
                             ))
{
  struct Undo_Reallines *u_r = talloc(sizeof(struct Undo_Reallines));

  u_r->localzooms=NULL;
  CB_CopyLocalZoomsRec(&u_r->localzooms,wblock->localzooms);

  u_r->line_zoom = GetLineZoomBlock(wblock);
  
  Undo_Add_dont_stop_playing(
                             window->l.num,
                             wblock->l.num,
                             tracknum,
                             realline,
                             u_r,
                             Undo_Do_Reallines,
                             "Reallines (localzooms)"
                             );
}

void ADD_UNDO_FUNC(Reallines_CurrPos(
                                     struct Tracker_Windows *window
                                     )
                   )
{
  CALL_ADD_UNDO_FUNC(Reallines(window,window->wblock,window->curr_track,window->wblock->curr_realline));
}

static void *Undo_Do_Reallines(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
  struct Undo_Reallines *u_r = pointer;

  int old_line_zoom = GetLineZoomBlock(wblock);
  struct LocalZooms *old_localzooms=wblock->localzooms;

  {
    wblock->localzooms=u_r->localzooms;
    wblock->num_expand_lines = u_r->line_zoom;
    
    UpdateRealLines(window,wblock);
    
    UpdateReallinesDependens(window,wblock);
  }

  u_r->line_zoom = old_line_zoom;
  u_r->localzooms = old_localzooms;
  
  return u_r;
}





