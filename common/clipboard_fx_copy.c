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



#include <string.h>



#include "nsmtracker.h"
#include "clipboard_range_copy_proc.h"
#include "clipboard_range_calc_proc.h"
#include "placement_proc.h"
#include "clipboard_tempos_copy_proc.h"
#include "wtracks_proc.h"

#include "clipboard_track_copy_proc.h"


Don''t think this file is used.


struct FXs *cb_fxs=NULL;

struct FXs *CB_CopyFx(
                      const struct WBlocks *wblock,
                      const struct WTracks *wtrack
){

	const struct Tracks *track=wtrack->track;

	const Place *p1=PlaceGetFirstPos();
	Place p2;
	PlaceSetLastPos(wblock->block,&p2);

	CopyRange_fxs(&cb_fxs,track->fxs,p1,&p2);

	return towtrack;
}


void CB_CopyFx_CurrPos(
                       const struct Tracker_Windows *window
){
  const struct WBlocks *wblock=window->wblock;
  const struct Blocks *block=wblock->block;
  const struct WTracks *wtrack=wblock->wtrack;

        if (wtrack != NULL)
          cb_fxs = CB_CopyFx(wblock, wtrack);
}



