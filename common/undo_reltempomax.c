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







#include "nsmtracker.h"
#include "undo.h"
#include "temponodes_proc.h"

#include "undo_reltempomax_proc.h"


struct Undo_RelTempoMax{
	float reltempomax;
};

void *Undo_Do_RelTempoMax(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);

void Undo_RelTempoMax(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){
	struct Undo_RelTempoMax *u_rt=talloc_atomic(sizeof(struct Undo_RelTempoMax));
	u_rt->reltempomax=wblock->reltempomax;

	Undo_New(
		window->l.num,
		wblock->l.num,
		wblock->wtrack->l.num,
		wblock->curr_realline,
		u_rt,
		Undo_Do_RelTempoMax
	);

}

void *Undo_Do_RelTempoMax(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
	struct Undo_RelTempoMax *u_rt=(struct Undo_RelTempoMax *)pointer;
	float reltempomax=wblock->reltempomax;

	wblock->reltempomax=u_rt->reltempomax;
	UpdateWTempoNodes(window,wblock);

	u_rt->reltempomax=reltempomax;

	return u_rt;
}


