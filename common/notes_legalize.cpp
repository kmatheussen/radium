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
#include "placement_proc.h"
#include "list_proc.h"

#include "notes_legalize_proc.h"


/****************************************************
  FUNCTION
    Make sure that:

    -No end-note is placed before or on the start of the
     note or below the last legal position.

    -No velocity is placed on the same position
     as the start (or before) or the end of the note
     (or after) or another velocity.

    -No stops is below last legal position.
****************************************************/

bool LegalizeNotes(struct Blocks *block,struct Tracks *track){
	Place *p1,*p2,*p;
	Place endplace;
	struct Notes *note=track->notes;
	struct Notes *notetemp;
	//struct Stops *stop=track->stops;
	//struct Stops *stoptemp;

	PlaceSetLastPos(block,&endplace);

        bool ret = false;
        
	while(note!=NULL){
                p1=&note->l.p;
		p2=&note->end;

		if(PlaceGreaterOrEqual(p2,&endplace)){
			PlaceCopy(p2,&endplace);
                        ret = true;
		}

		if(PlaceLessOrEqual(p2,p1)){
			notetemp=NextNote(note);
			ListRemoveElement3(&track->notes,&note->l);
			note=notetemp;
                        ret = true;
			continue;
		}

		struct Velocities *velocity=note->velocities;
		while(velocity!=NULL){
			p=&velocity->l.p;
			if(
				PlaceLessThan(p,p1) ||
				PlaceGreaterThan(p,p2)
			){
				struct Velocities *velocitytemp=NextVelocity(velocity);
				ListRemoveElement3(&note->velocities,&velocity->l);
				velocity=velocitytemp;
                                ret = true;
				continue;
			}
			p1=p;
			velocity=NextVelocity(velocity);
		}

                p1=&note->l.p;
		p2=&note->end;
                
		struct Pitches *pitch=note->pitches;
		while(pitch!=NULL){
			p=&pitch->l.p;
			if(
				PlaceLessThan(p,p1) ||
				PlaceGreaterThan(p,p2)
			){
				struct Pitches *pitchtemp=NextPitch(pitch);
				ListRemoveElement3(&note->pitches,&pitch->l);
				pitch=pitchtemp;
                                ret = true;
				continue;
			}
			p1=p;
			pitch=NextPitch(pitch);
		}

		note=NextNote(note);
	}

        r::TimeData<r::Stop>::Writer(track->stops2).remove_everything_after(make_ratio_from_place(endplace));
        /*
	while(stop!=NULL){
		if(PlaceGreaterOrEqual(&stop->l.p,&endplace)){
			stoptemp=NextStop(stop);
			ListRemoveElement3(&track->stops,&stop->l);
			stop=stoptemp;
                        ret = true;
			continue;
		}

		stop=NextStop(stop);
	}
        */

        return ret;
}





