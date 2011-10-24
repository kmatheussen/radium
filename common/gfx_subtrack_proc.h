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


#ifndef TRACKER_INCLUDE

extern int GetRelXSubTrack1(
	struct WTracks *wtrack,
	int subtrack
);

extern int GetXSubTrack1(
	struct WTracks *wtrack,
	int subtrack
);

extern int GetRelXSubTrack2(
	struct WTracks *wtrack,
	int subtrack
);

extern int GetXSubTrack2(
	struct WTracks *wtrack,
	int subtrack
);

extern int GetXSubTrack_B1(
	struct WBlocks *wblock,
	NInt track,
	int subtrack
);

extern int GetXSubTrack_B2(
	struct WBlocks *wblock,
	NInt track,
	int subtrack
);

extern int SubtrackBoundaries(struct WTracks *wtrack,int subtrack,int x);

extern int GetSubTrackWidth(struct WTracks *wtrack,int subtrack);

extern int GetSubTrackPos(
	struct WTracks *wtrack,
	float x,
	int maxx,
	int subtrack
);

extern int GetSubTrack(
	struct WTracks *wtrack,
	int x
);


#endif
