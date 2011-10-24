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
#include "fxlines_proc.h"
#include "trackreallines_proc.h"
#include "clipboard_track_copy_proc.h"
#include "clipboard_track_paste_proc.h"

#include "undo_tracks_proc.h"


void *Undo_Do_Track(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);


void Undo_Track(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline
){
	Undo_New(
		window->l.num,
		wblock->l.num,
		wtrack->l.num,
		realline,
		CB_CopyTrack(wblock,wtrack),
		Undo_Do_Track
	);
}

void Undo_Track_CurrPos(
	struct Tracker_Windows *window
){
	Undo_Track(window,window->wblock,window->wblock->wtrack,window->wblock->curr_realline);
}

void *Undo_Do_Track(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
	struct WTracks *undo_wtrack=(struct WTracks *)pointer;
	struct WTracks *temp=CB_CopyTrack(wblock,wtrack);

	CB_PasteTrack(wblock,undo_wtrack,wtrack);

	UpdateFXNodeLines(window,wblock,wtrack);
	UpdateTrackReallines(window,wblock,wtrack);

	undo_wtrack=wblock->wtracks;
	while(undo_wtrack!=NULL){
		undo_wtrack=NextWTrack(undo_wtrack);
	}

	return temp;
}


