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


extern void CloseWTrack(struct WBlocks *wblock, NInt wtracknum);

extern void CloseAllWTracks(struct WBlocks *wblock);

extern void UpdateWTracks(struct Tracker_Windows *window, struct WBlocks *wblock);

extern void UpdateWTracksCoordinates(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
);

/*
extern struct WTracks *UpdateWTrackCoordinates(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int wtrack_notearea_x
);
*/

extern void UpdateAllWTracksCoordinates(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
);


extern void UpdateAndClearSomeTrackReallinesAndGfxWTracks(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	NInt starttrack,
	NInt endtrack
);


extern void ChangeNoteLength_CurrPos(
	struct Tracker_Windows *window
);

void ChangeNoteLength_Block_CurrPos(
	struct Tracker_Windows *window
);

void ChangeNoteAreaWidth_CurrPos(
                                 struct Tracker_Windows *window
                                 );

void ChangeNoteAreaWidth_Block_CurrPos(
                                       struct Tracker_Windows *window
                                       );
  
void MinimizeTrack_CurrPos(
	struct Tracker_Windows *window
);

void MinimizeBlock_CurrPos(
	struct Tracker_Windows *window
);

void SwapTrack_CurrPos(
	struct Tracker_Windows *window
);

void AppendWTrack_CurrPos(struct Tracker_Windows *window, struct WBlocks *wblock);

int WTRACK_getx1(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	NInt track,
	bool onlyfx
);

int WTRACK_getx2(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	NInt track
);
