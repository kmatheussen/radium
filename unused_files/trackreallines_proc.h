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


#ifndef TRACKER_TRACKREALLINES
#define TRACKER_TRACKREALLINES

#ifdef TRACKER_INCLUDE

void UpdateTrackReallines(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack
);

void UpdateAllTrackReallines(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
);
#endif

#ifndef TRACKER_INCLUDE


extern void FreeAllRTEelements(
	struct WBlocks *wblock,
	struct WTracks *wtrack
);

extern void NewTrackRealLines(
	struct WBlocks *wblock,
	struct WTracks *wtrack
);

extern void UpdateTrackReallines(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack
);

extern void UpdateAllTrackReallines(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
);

extern LANGSPEC void TRACKREALLINES_update_peak_tracks(struct Tracker_Windows *window, struct Patch *patch);

extern LANGSPEC void TRACKREALLINES_call_very_often(struct Tracker_Windows *window);
extern LANGSPEC void RT_TRACKREALLINES_schedule_update_peak_tracks(struct Patch *patch);

extern void UpdateSomeTrackReallines(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	NInt starttrack,
	NInt endtrack
);

void TR_scaleTrackReallines(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int oldwidth
	);
#endif

#endif
