
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

#ifndef RADIUM_COMMON_CLIPBOARD_RANGE_H
#define RADIUM_COMMON_CLIPBOARD_RANGE_H

#ifdef __cplusplus
//#include "TimeData.hpp" // don't want to do this since it can screw up the call "./grep_touch_files.sh TimeData.hpp"
#endif

#define NUM_RANGES 5 // see "rangetype" under api/protos.conf

struct RangeClip{
        int rangenum;
	NInt num_tracks;
	struct Notes **notes;
#ifdef __cplusplus
        r::TimeData<r::Stop> **stops;
#else
        void **stops;
#endif

  //struct Stops **stops;
  //struct Instruments **instruments;
        vector_t *fxs;
	Place length;
};

extern struct RangeClip *g_range_clips[NUM_RANGES]; // see "rangetype" under api/protos.conf


#endif
