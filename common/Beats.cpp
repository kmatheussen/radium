/* Copyright 2015 Kjetil S. Matheussen

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
#include "player_pause_proc.h"
#include "../api/api_timing_proc.h"

#include "Beats_proc.h"


namespace{
  
  struct LPBHolder{
    LPBs root_lpb;
    LPBs *lpb;
    LPBHolder(Blocks *block, int default_lpb) {
      root_lpb.l.next = (ListHeader3*)block->lpbs;
      root_lpb.l.p = p_Create(0,0,1);
      root_lpb.lpb = default_lpb;
      lpb = &root_lpb;
    }

    int get_lpb_at_place(Place p){ // p(n) >= p(n-1)
      LPBs *next = NextLPB(lpb);
      
      if (next!=NULL && p_Greater_Or_Equal(p, next->l.p)) {
        lpb = next;
        return get_lpb_at_place(p);
      }
      
      return lpb->lpb;
    }
  };
}

static Place get_measure_length_in_quarters(Ratio signature, int lpb){
  Place ratio = p_Create(0,(int)signature.numerator,(int)signature.denominator);
  Place four_lpb = p_Create(4*lpb, 0, 1);

  return p_Mul(ratio,
               four_lpb
               );
}

static Place get_beat_length_in_measurement(Ratio signature, int lpb){
  return p_Div(get_measure_length_in_quarters(signature, lpb),
               p_Create((int)signature.numerator,0,1)
               );
}


struct Beats *Beats_get(struct Blocks *block, Ratio default_signature, int default_lpb){
  Beats *beats = NULL;

  int bar_num = 1;
  
  Signatures first;
  first.l.p = p_Create(0,0,1);
  first.signature = default_signature;

  LPBHolder lpb_holder(block, default_lpb);
  
  Signatures *signature = &first;
  
  Signatures *last_drawn_signature = signature; // The root signature is already drawn in the signature track header.

  while(signature!=NULL){

    Signatures *next;
    if (signature == &first)
      next = block->signatures;
    else
      next = NextSignature(signature);

    Place nextplace;
    if (next==NULL)
      nextplace = p_Last_Pos(block);
    else
      nextplace = next->l.p;

    {
      int beat_num = 1;
      Place beat_pos = signature->l.p;
      
      while (p_Less_Than(beat_pos, nextplace)) {
        
        Beats *beat = (Beats*)talloc(sizeof(Beats));        
        beat->l.p = beat_pos;

        if (signature != last_drawn_signature) {
          beat->signature = signature->signature;
          last_drawn_signature = signature;
        }

        beat->valid_signature = signature->signature;
        
        beat->bar_num = bar_num;
        beat->beat_num = beat_num;
        
        ListAddElement3(&beats, &beat->l);

        beat_num++;
        if (beat_num==signature->signature.numerator+1) {
          beat_num=1;
          bar_num++;
        }
        //printf("********* %d/%d         **********\n",bar_num,beat_num);
        int lpb = lpb_holder.get_lpb_at_place(beat_pos);
          
        Place beat_length = get_beat_length_in_measurement(signature->signature, lpb); // This walue will be incorrect if there are lpb changes betwen beat_pos and beat_pos+beat_length. TODO.

        beat_pos = p_Add(beat_pos, beat_length);
      }

      // If there aren't enough beats to fill the time signature, we must manually start a new bar. (we don't change time signature in the middle of a bar)
      if (beat_num!=1){
        beat_num=1;
        bar_num++;
      }
    }

    signature = next;
  }

  
  
  return beats;
}

