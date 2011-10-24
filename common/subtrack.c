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
#include "placement_proc.h"

#include "subtrack_proc.h"


extern char *NotesTexts3[131];


/**********************************************************************
  FUNCTION
    Finds the first free subtrack for the placement at realline
    'realline'.
**********************************************************************/
int FindFirstFreeSubTrack(
	struct WTracks *wtrack,
	int realline,
	Place *placement

){
	int lokke;
	struct Notes *note=NULL;
	struct TrackReallineElements *element;

	for(lokke=0;lokke<wtrack->num_vel;lokke++){
		element=wtrack->trackreallines[realline].trackreallineelements;
		if(element==NULL) return lokke;
		for(;;){
			if(element->type<=TRE_VELLINEEND && element->type!=TRE_VELLINENODE && element->subtype==lokke){
				note=(struct Notes *)element->pointer;
				if(PlaceGreaterThan(&note->end,placement)) break;
			}
			element=element->next;
			if(element==NULL) return lokke;
		}
	}

	RError("\n\nERROR! In function 'FindSubTrack' in file 'trackreallines.c'.\n\n");
	RError("realline: %d, wtrack->l.num: %d, placement->line: %d, placement->counter: %d, placement->dividor: %d\n\n",
		realline,wtrack->l.num,placement->line,placement->counter,placement->dividor);

	if(note==NULL){RError("\nnote=NULL\n");}else{
		RError("\nprevnote: %s line: %d, counter: %d, dividor: %d\n\n",
			NotesTexts3[note->note],realline,&note->end.line,&note->end.counter,&note->end.dividor);
	}

	return -1; /* Should never happen. */
}







