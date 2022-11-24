/* Copyright 2014 Kjetil S. Matheussen

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


#ifndef COMMON_TRACKREALLINES2_PROC_H
#define COMMON_TRACKREALLINES2_PROC_H

extern LANGSPEC int find_realline_for_end_pitch(const struct WBlocks *wblock, const Place *p);

// Returns a pointer to an array of vectors (one vector for each realline), not a pointer to a vector.
//extern LANGSPEC vector_t *TRS_get(const struct WBlocks *wblock, const struct WTracks *wtrack);
//extern LANGSPEC vector_t *TR_get(const struct WBlocks *wblock, const struct WTracks *wtrack, int realline);

#ifdef USE_QT4
const Trss TRSS_get(const struct WBlocks *wblock, const struct WTracks *wtrack);
const Trs TRS_get(const struct WBlocks *wblock, const struct WTracks *wtrack, int realline);
#endif

#endif


