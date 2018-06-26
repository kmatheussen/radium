/* Copyright 2001 Kjetil S. Matheussen

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


#include "../nsmtracker.h"
#include "../placement_proc.h"
#include "../notes_proc.h"

#include "../wtracks_proc.h"
#include "../undo_notes_proc.h"
#include "../player_proc.h"
#include "../player_pause_proc.h"

#include "glissando_proc.h"


static void Glissando(
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	struct Notes *note1
){
//	struct Notes *note;
	struct Notes *note2=NextNote(note1);
	float f1,f,f2;
	Place p;
	int notenote;
	int notediff;

	if(note2==NULL) return;

	notediff=R_ABS(note2->note-note1->note);
	if(notediff==0 || notediff==1) return;

	f1=GetfloatFromPlacement(&note1->l.p);
	f2=GetfloatFromPlacement(&note2->l.p);

        bool up = note2->note > note1->note;

	for(notenote=note1->note;;){

          if (up)
            notenote++;
          else
            notenote--;
          
                if (up){
                  if (notenote>=note2->note)
                    break;
                } else {
                  if (notenote<=note2->note)
                    break;
                }

		f=f1+(
				R_ABS(note1->note-notenote)*(f2-f1)
				/
				notediff
				);
                if (f<0)
                  f=0;
                  
                if (f>=wblock->block->num_lines)
                  break;
                
                Float2Placement(f,&p);                  
                  
		InsertNote(
                           wblock,
                           wtrack,
                           &p,NULL,
                           notenote,
                           (int)(note1->velocity+(
                                                  ((f-f1)*(note2->velocity-note1->velocity))/(f2-f1)
                                                  )),
                           false
                           );

	}

}

void Glissando_CurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;
	struct WTracks *wtrack=wblock->wtrack;
	struct Notes *note=wtrack->track->notes;
	struct Notes *nextnote;
        int realline = wblock->curr_realline;
	const Place *p=&wblock->reallines[realline]->l.p;

        while(note!=NULL){
          nextnote=NextNote(note);
          if(nextnote==NULL) return;
          
          if(PlaceIsBetween2(p,&note->l.p,&nextnote->l.p)){
            ADD_UNDO(Notes(window, wblock->block, wtrack->track, realline));
            Glissando(wblock,wtrack,note);
            /*
            UpdateAndClearSomeTrackReallinesAndGfxWTracks(
                                                          window,
                                                          wblock,
                                                          wtrack->l.num,
                                                          wtrack->l.num
                                                          );
            */
            return;
          }
          note=nextnote;
        }

}




