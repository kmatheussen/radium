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
#include "list_proc.h"
#include "vector_proc.h"

#include "fxlines_legalize_proc.h"



/****************************************************
  FUNCTION
    Make sure that:

    -No fxnodeline is placed on the same position
     as the start (or before) of the end of the note
     (or after) or another fxnodeline.

    -There isn't any fxs that has only one fxnodeline.

****************************************************/

void LegalizeFXlines(struct Blocks *block, struct Tracks *track){
	Place p2;

	PlaceSetLastPos(block,&p2);

 again:
        VECTOR_FOR_EACH(struct FXs *fxs, &track->fxs){
                struct FXNodeLines *fxnodeline=fxs->fxnodelines;
		const Place *p1=PlaceGetFirstPos();

 		while(fxnodeline!=NULL){
			Place *p=&fxnodeline->l.p;
			if(
				PlaceLessThan(p,p1)
				|| PlaceGreaterThan(p,&p2)
				|| (p1 != PlaceGetFirstPos() && PlaceEqual(p,p1))
			){
				struct FXNodeLines *temp2=NextFXNodeLine(fxnodeline);
				ListRemoveElement3(&fxs->fxnodelines,&fxnodeline->l);
				fxnodeline=temp2;
				continue;
			}
			p1=p;
			fxnodeline=NextFXNodeLine(fxnodeline);
		}

		fxnodeline=fxs->fxnodelines;
		if(fxnodeline==NULL || fxnodeline->l.next==NULL){
                  VECTOR_remove(&track->fxs, fxs);
                  goto again;
		}

	}END_VECTOR_FOR_EACH;

}





