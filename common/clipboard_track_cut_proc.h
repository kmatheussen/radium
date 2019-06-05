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



void CB_ClearTrack_Force(
                         struct Blocks *block,
                         struct Tracks *track
                         );

void CB_CutTrack_Force(
	struct WBlocks *wblock,
	struct WTracks *wtrack
);

struct WTracks *CB_CutTrack(
                            struct Tracker_Windows *window,
                            struct WBlocks *wblock,
                            struct WTracks *wtrack
                            );

extern void CB_CutTrack_CurrPos(
	struct Tracker_Windows *window
);

extern void CB_ClearTrack_CurrPos(
                                  struct Tracker_Windows *window
                                  );



