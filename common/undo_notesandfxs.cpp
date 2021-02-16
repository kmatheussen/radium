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
#include "TimeData.hpp"
#include "undo.h"
#include "placement_proc.h"
#include "clipboard_range_copy_proc.h"
#include "list_proc.h"
#include "vector_proc.h"
#include "fxlines_proc.h"
#include "../midi/midi_fx_proc.h"
#include "TallocWithDestructor.hpp"

#include "undo_notesandfxs_proc.h"


struct Undo_NotesAndFXs : radium::GC_able{
	struct Notes *notes;
        r::TimeData<r::Stop> stops;
        vector_t fxss;
	void *midi_instrumentdata;
};


static void *Undo_Do_NotesAndFXs(
                                 struct Tracker_Windows *window,
                                 struct WBlocks *wblock,
                                 struct WTracks *wtrack,
                                 int realline,
                                 void *pointer
                                 );

static void ADD_UNDO_FUNC(
                   NotesAndFXs(
                               struct Tracker_Windows *window,
                               struct Blocks *block,
                               struct Tracks *track,
                               int realline
                               )
                   )
{
	const Place *p1=PlaceGetFirstPos();
	Place p2;
	struct Undo_NotesAndFXs *undo_notesandfxs=new Undo_NotesAndFXs;
        
	PlaceSetLastPos(block,&p2);

	CopyRange_stops(&undo_notesandfxs->stops,track->stops2,p1,&p2);
	CopyRange_notes(&undo_notesandfxs->notes,track->notes,p1,&p2);
	CopyRange_fxs(block->num_lines, &undo_notesandfxs->fxss,&track->fxs,p1,&p2);
	if(track->midi_instrumentdata!=NULL){
          undo_notesandfxs->midi_instrumentdata=MIDI_CopyInstrumentData(track);
	}

	Undo_Add(
                 window->l.num,
                 block->l.num,
                 track->l.num,
                 realline,
                 undo_notesandfxs,
                 Undo_Do_NotesAndFXs,
                 "Track notes and fxs"
                 );

}

void ADD_UNDO_FUNC(NotesAndFXs_CurrPos(
	struct Tracker_Windows *window
                                       ))
{
  CALL_ADD_UNDO_FUNC(NotesAndFXs(window,window->wblock->block,window->wblock->wtrack->track,window->wblock->curr_realline));
}



static void *Undo_Do_NotesAndFXs(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
	struct Undo_NotesAndFXs *undo_notesandfxs=(struct Undo_NotesAndFXs *)pointer;
	struct Notes *ntemp;

	void *midi_instrumentdata;

	struct Tracks *track=wtrack->track;

	ntemp=track->notes;
	vector_t *temp=VECTOR_copy(&track->fxs);
	midi_instrumentdata=track->midi_instrumentdata;

	track->notes=undo_notesandfxs->notes;
	track->fxs=*(VECTOR_copy(&undo_notesandfxs->fxss));
	track->midi_instrumentdata=undo_notesandfxs->midi_instrumentdata;

	undo_notesandfxs->notes=ntemp;
	undo_notesandfxs->fxss=*temp;
	undo_notesandfxs->midi_instrumentdata=midi_instrumentdata;

        {
          r::TimeData<r::Stop> stops_temp;
          
          stops_temp.move_from(track->stops2);
          
          track->stops2->move_from(&undo_notesandfxs->stops);
           
          undo_notesandfxs->stops.move_from(&stops_temp);

        }
        
	return undo_notesandfxs;
}


