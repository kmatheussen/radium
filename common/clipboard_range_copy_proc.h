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

#pragma once


#include "clipboard_range.h"

extern LANGSPEC void CopyRange_notes(
                            struct Notes **tonote,
                            const struct Notes *fromnote,
                            const Place *p1,
                            const Place *p2
);

/*
extern LANGSPEC void CopyRange_stops(
	struct Stops **tostop,
	const struct Stops *fromstop,
	const Place *p1,
	const Place *p2
);
*/

#ifdef __cplusplus
void CopyRange_stops(
                     r::TimeData<r::Stop> *to_stop,
                     //struct Stops **tostop,
                     const r::TimeData<r::Stop> *from_stop,
                     //const struct Stops *from_stop,
                     const Place *p1,
                     const Place *p2);
#endif
                     
extern LANGSPEC void CopyRange_fxs(
                                   int num_lines,
                                   vector_t *tofxs,
                                   const vector_t *fromfxs,
                                   const Place *p1,
                                   const Place *p2
);

extern LANGSPEC void CopyRange(
                               struct Blocks *wblock,
                               range_t range,
                               int rangenum
);

extern LANGSPEC void CopyRange_CurrPos(
                                       struct Tracker_Windows *window,
                                       int rangenum
                                       );

