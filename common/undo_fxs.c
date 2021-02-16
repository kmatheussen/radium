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
#include "vector_proc.h"
#include "undo.h"
#include "clipboard_range_copy_proc.h"
#include "fxlines_proc.h"
#include "placement_proc.h"
#include "../midi/midi_fx_proc.h"

#include "undo_fxs_proc.h"


struct Undo_FXs{
  vector_t fxss;
  void *midi_instrumentdata;
};

static void *Undo_Do_FXs(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);

void ADD_UNDO_FUNC(
                   FXs(
                       struct Tracker_Windows *window,
                       struct Blocks *block,
                       struct Tracks *track,
                       int realline
                       )
                   )
{

	const Place *p1=PlaceGetFirstPos();
	Place p2;
	struct Undo_FXs *undo_fxs=talloc(sizeof(struct Undo_FXs));

	PlaceSetLastPos(block,&p2);

	CopyRange_fxs(block->num_lines, &undo_fxs->fxss,&track->fxs,p1,&p2);
	if(track->midi_instrumentdata!=NULL){
		undo_fxs->midi_instrumentdata=MIDI_CopyInstrumentData(track);
	}

	Undo_Add(
                 window->l.num,
                 block->l.num,
                 track->l.num,
                 realline,
                 undo_fxs,
                 Undo_Do_FXs,
                 "Track fxs"
                 );

}

void ADD_UNDO_FUNC(FXs_CurrPos(
                               struct Tracker_Windows *window
                               )
                   )
{
  CALL_ADD_UNDO_FUNC(FXs(window,window->wblock->block,window->wblock->wtrack->track,window->wblock->curr_realline));
}

static void *Undo_Do_FXs(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
	struct Undo_FXs *undo_fxs=(struct Undo_FXs *)pointer;

	struct Tracks *track=wtrack->track;

	vector_t *temp = VECTOR_copy(&track->fxs);
	void *midi_instrumentdata=track->midi_instrumentdata;

	track->fxs = *(VECTOR_copy(&undo_fxs->fxss));
	track->midi_instrumentdata=undo_fxs->midi_instrumentdata;
        
	undo_fxs->fxss = *temp;
	undo_fxs->midi_instrumentdata=midi_instrumentdata;

	return undo_fxs;

}
