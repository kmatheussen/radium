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

extern LANGSPEC void SetRange(
                              struct Tracker_Windows *window,
                              struct WBlocks *wblock,
                              NInt starttrack,
                              NInt endtrack,
                              Place startplace,
                              Place endplace
                              );

extern LANGSPEC void MarkRange_CurrPos(struct Tracker_Windows *window);

extern LANGSPEC void CancelRange(
                                 struct Tracker_Windows *window,
                                 struct WBlocks *wblock
                                 );
extern LANGSPEC void CancelRange_CurrPos(struct Tracker_Windows *window);

extern LANGSPEC void MakeRangeLegal(
	struct WBlocks *wblock
);

static inline bool range_is_legal(const struct WBlocks *wblock, const Place p1, const Place p2, int start_track, int end_track){
  return
    end_track > start_track &&
    start_track >= 0 &&
    end_track <= wblock->block->num_tracks &&
    p_Greater_Than(p2, p1) &&
    p_Greater_Or_Equal(p1, p_Create(0,0,1)) &&
    p_Less_Or_Equal(p2, p_Create(wblock->block->num_lines, 0, 1));
}

static inline bool range_is_legal2(const struct WBlocks *wblock){
  return range_is_legal(wblock, wblock->rangey1, wblock->rangey2, wblock->rangex1, wblock->rangex2+1);
}
  

