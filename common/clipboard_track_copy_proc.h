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



extern LANGSPEC struct WTracks *internal_copy_track(
                                    const struct WBlocks *wblock,
                                    const struct WTracks *wtrack,
				    const bool only_copy_current_fx_if_possible
                                    );

extern LANGSPEC struct WTracks *CB_CopyTrack(
                                    const struct WBlocks *wblock,
                                    const struct WTracks *wtrack
);

extern LANGSPEC void CB_CopyTrack_CurrPos(
                                 const struct Tracker_Windows *window
);

extern struct WTracks *cb_wtrack;
extern bool cb_wtrack_only_contains_one_fxs;



