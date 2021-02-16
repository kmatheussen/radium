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

#if 0
void LegalizeFXlines(struct Blocks *block, struct Tracks *track){
	Place p2;

	PlaceSetLastPos(block,&p2);

 again:
        VECTOR_FOR_EACH(struct FXs *, fxs, &track->fxs){
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
#endif

static void LegalizeStart(r::TimeData<r::FXNode>::Writer &writer){

  std::vector<int> to_remove;
  
  for(int i=0;i<writer.size();i++){
    
    r::FXNode &node = writer.at_ref(i);
    
    if (node._time < 0){
      
      if (i+1<writer.size()){

        r::FXNode &node2 = writer.at_ref(i+1);
        if (node2._time <= 0){
          
          to_remove.push_back(i);
          
        } else {

          node._val = round(scale_double(0, make_double_from_ratio(node._time), make_double_from_ratio(node2._time), node._val, node2._val));
          node._time = make_ratio(0,1);
          
        }

      } else {
        
        to_remove.push_back(i);
        
      }
      
    } else {

      break;

    }
  }

  writer.remove_at_positions(to_remove);
}

static void LegalizeEnd(int num_lines, r::TimeData<r::FXNode>::Writer &writer){
  
  std::vector<int> to_remove;
  
  for(int i=writer.size()-1 ; i >= 0 ; i--){
    
    r::FXNode &node = writer.at_ref(i);
    
    if (node._time > num_lines){
      
      if (i > 0) {
        
        r::FXNode &node2 = writer.at_ref(i-1);
        
        if (node2._time >= num_lines) {
          
          to_remove.push_back(i);
          
        } else {
          
          node._val = round(scale_double(num_lines, make_double_from_ratio(node2._time), make_double_from_ratio(node._time), node2._val, node._val));
          node._time = make_ratio(num_lines,1);
          
        }
        
      } else {
        
        to_remove.push_back(i);
        
      }
      
    } else {

      break;
      
    }
  }

  writer.remove_at_positions(to_remove);
}

static void LegalizeValues(const struct FX *fx, r::TimeData<r::FXNode>::Writer &writer){
  for(r::FXNode &node : writer){
    if (node._val < fx->min)
      node._val = fx->min;
    else if (node._val > fx->max)
      node._val = fx->max;
  }
}

bool LegalizeFXlines2(int num_lines, const struct FX *fx, r::TimeData<r::FXNode>::Writer &writer){
  writer.sortit();

  LegalizeStart(writer);
  LegalizeEnd(num_lines, writer);
  LegalizeValues(fx, writer);
  
  if (writer.size() < 2)
    return false;
  else
    return true;
}
