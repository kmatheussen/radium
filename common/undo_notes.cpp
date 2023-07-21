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
#include "TimeData.hpp"
#include "undo.h"
#include "placement_proc.h"
#include "clipboard_range_copy_proc.h"
#include "list_proc.h"
#include "TallocWithDestructor.hpp"

#include "undo_notes_proc.h"


namespace{
  
struct Undo_Notes : radium::GC_able{
  struct Notes *notes;
  r::StopTimeData stops;
  r::NoteTimeData notes2;
};
 
}


static void *Undo_Do_Notes(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);

void ADD_UNDO_FUNC(
                   Notes(
                         struct Tracker_Windows *window,
                         struct Blocks *block,
                         struct Tracks *track,
                         int realline
                         )
                   )
{
        const Place *p1=PlaceGetFirstPos();
	Place p2;

        /*
	struct Undo_Notes *undo_notes=talloc_with_finalizer<struct Undo_Notes>([](struct Undo_Notes *undo_notes){
          //printf("     UNDOING STOPS\n");
          delete undo_notes->stops;
        });
        */
        Undo_Notes *undo_notes = new Undo_Notes;

	PlaceSetLastPos(block,&p2);

	CopyRange_stops(&undo_notes->stops,track->stops2,p1,&p2);
	//CopyRange_notes(&undo_notes->notes,track->notes,p1,&p2);
	CopyRange_notes2(&undo_notes->notes2,track->_notes2,p1,&p2);

	Undo_Add_dont_stop_playing(
                 window->l.num,
                 block->l.num,
                 track->l.num,
                 realline,
                 undo_notes,
                 Undo_Do_Notes,
                 "Track notes and stops"
                 );
}

void ADD_UNDO_FUNC(Notes_CurrPos(
                                 struct Tracker_Windows *window
                                 )
                   )
{
  CALL_ADD_UNDO_FUNC(Notes(window,window->wblock->block,window->wblock->wtrack->track,window->wblock->curr_realline));
}


static void *Undo_Do_Notes(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){

	struct Undo_Notes *undo_notes=(struct Undo_Notes *)pointer;

	struct Tracks *track=wtrack->track;

        {
          struct Notes *ntemp=track->notes;
          //struct Stops *stemp=track->stops;
          
          {
            SCOPED_PLAYER_LOCK_IF_PLAYING();
            
            track->notes=undo_notes->notes;
            //track->stops=undo_notes->stops;
          }
          
          undo_notes->notes=ntemp;
        }

        track->stops2->swap(&undo_notes->stops);
        track->_notes2->swap(&undo_notes->notes2);
                
	return undo_notes;
}
