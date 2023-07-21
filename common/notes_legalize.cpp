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
	Place endplace;
	struct Notes *note=track->notes;
	struct Notes *notetemp;
	//struct Stops *stop=track->stops;
	//struct Stops *stoptemp;

	PlaceSetLastPos(block,&endplace);
        //const Ratio r_endplace = place2ratio(endplace);
          
        bool ret = false;
        
	while(note!=NULL){
                Place *p1=&note->l.p;
                Place endnote = ratio2place(note->end);
		Place *p2=&endnote;

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

                /*
		struct Velocities *velocity=note->velocities;
		while(velocity!=NULL){
			Place *p=&velocity->l.p;
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
                */
                
                {
                  r::VelocityTimeData::Writer writer(note->_velocities);
                  Ratio r1 = ratio_from_place(*p1);
                  Ratio r2 = ratio_from_place(*p2);
                  writer.remove([r1, r2, &ret](int i, r::Velocity &velocity){
                      if ((velocity._time < r1) || (velocity._time > r2)){
                        ret = true;
                        return true;
                      } else {
                        return false;
                      }
                    });
                }
                

                endnote = ratio2place(note->end);
                
                /*
                p1=&note->l.p;
		p2=&endnote;

		struct Pitches *pitch=note->pitches;
		while(pitch!=NULL){
			Place *p=&pitch->l.p;
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
                */
                
                p1=&note->l.p;
		p2=&endnote;

                {
                  r::PitchTimeData::Writer writer(note->_pitches, track->_notes2);
                  Ratio r1 = ratio_from_place(*p1);
                  Ratio r2 = ratio_from_place(*p2);
                  writer.remove([r1, r2, &ret](int i, r::Pitch &pitch){
                      if ((pitch._time < r1) || (pitch._time > r2)){
                        ret = true;
                        return true;
                      } else {
                        return false;
                      }
                    });
                }
                
		note=NextNote(note);
	}

        r::StopTimeData::Writer(track->stops2).remove_everything_after(place2ratio(endplace));
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





