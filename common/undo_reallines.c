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

#include "undo_reallines_proc.h"

void *Undo_Do_Reallines(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);


void Undo_Reallines(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	NInt tracknum,
	int realline
){
	struct LocalZooms *tolocalzoom=NULL;
	CB_CopyLocalZoomsRec(&tolocalzoom,wblock->localzooms);

	Undo_New(
		window->l.num,
		wblock->l.num,
		tracknum,
		realline,
		tolocalzoom,
		Undo_Do_Reallines
	);
}

void Undo_Reallines_CurrPos(
	struct Tracker_Windows *window
){
	Undo_Reallines(window,window->wblock,window->curr_track,window->wblock->curr_realline);
}

void *Undo_Do_Reallines(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
	struct LocalZooms *undo_localzooms=(struct LocalZooms *)pointer;
	struct LocalZooms *temp=wblock->localzooms;

	wblock->localzooms=undo_localzooms;

	UpdateRealLines(window,wblock);

	UpdateReallinesDependens(window,wblock);

	return temp;
}





