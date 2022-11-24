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

extern LANGSPEC void UpdateWTempoNodes(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
);

extern LANGSPEC struct TempoNodes *AddTempoNode(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	const Place *p,
	float reltempo
);

extern LANGSPEC void AddTempoNodeCurrPos(struct Tracker_Windows *window,float reltempo);

extern LANGSPEC void RemoveAllTempoNodesOnReallineCurrPos(struct Tracker_Windows *window);

extern LANGSPEC void FreeAllWTempoNodes(
	struct WBlocks *wblock
);

extern LANGSPEC float FindHighestTempoNodeVal(struct Blocks *block);

static inline void adjust_reltempomax(struct WBlocks *wblock, float new_value){
  if ( (new_value+1) > wblock->reltempomax) {
    wblock->reltempomax = new_value+1;      
  } else if ( (new_value-1) < -wblock->reltempomax) {
    wblock->reltempomax = -1*(new_value -1);
  }
}

#endif
