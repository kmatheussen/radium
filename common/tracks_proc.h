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

#include "nsmtracker.h"

extern LANGSPEC float TRACK_get_min_pitch(const struct Tracks *track);
extern LANGSPEC float TRACK_get_max_pitch(const struct Tracks *track);
extern LANGSPEC bool TRACK_get_min_and_max_pitches(const struct Tracks *track, float *ret_min_pitch, float *ret_max_pitch);
extern LANGSPEC bool TRACK_has_peaks(struct Tracks *track);
extern LANGSPEC void CloseTrack(struct Blocks *block, NInt tracknum);
extern LANGSPEC void CloseAllTracks(struct Blocks *block);
extern LANGSPEC struct Tracks *TRACK_create(int tracknum);
extern LANGSPEC void AppendTrack(struct Blocks *block);
extern LANGSPEC void TRACK_make_monophonic_destructively(struct Tracks *track);
extern LANGSPEC bool TRACK_split_into_monophonic_tracks(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack);

#endif
