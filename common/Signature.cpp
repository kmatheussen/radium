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



/***********************************************************
  Time signautre. File very equal to Tempo.c (which
  should have been called (BPM) and LPB.c.
***********************************************************/



#include "nsmtracker.h"
#include "list_proc.h"
#include "memory_proc.h"
#include "realline_calc_proc.h"
#include "gfx_wblocks_proc.h"
#include "placement_proc.h"
#include "visual_proc.h"
#include "time_proc.h"
#include "undo_signatures_proc.h"
#include "player_proc.h"
#include "wblocks_proc.h"
#include "../Qt/Rational.h"

#include "Signature_proc.h"

extern struct Root *root;

namespace{
  
  struct Beats{
    struct ListHeader3 l;
    Ratio signature;
    int beat_num;
  };
#define NextBeat(a) (struct Beats *)((a)->l.next)
  
  struct WBeats{
    int beat_num;  // In a 4/4 measure, this value is either 0, 1, 2 or 3, or 4. (0 means that there is no beat placed on this realline)
  };

  struct LPBHolder{
    LPBs root_lpb;
    LPBs *lpb;
    LPBHolder(Blocks *block) {
      root_lpb.l.next = (ListHeader3*)block->lpbs;
      root_lpb.l.p = place(0,0,1);
      root_lpb.lpb = root->lpb;
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
  Place ratio = place(0,signature.numerator,signature.denominator);
  Place four_lpb = place(4*lpb, 0, 1);

  return p_Mul(ratio,
               four_lpb
               );
}

static Place get_beat_length_in_measurement(Ratio signature, int lpb){
  return p_Div(get_measure_length_in_quarters(signature, lpb),
               place(signature.denominator,0,1)
               );
}


struct Beats *Beats_get(struct Blocks *block){
  Beats *beats = NULL;

  Signatures first;
  first.l.p = place(0,0,1);
  first.signature = root->signature;

  LPBHolder lpb_holder(block);
  
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
        beat->beat_num = beat_num;
        
        ListAddElement3(&beats, &beat->l);

        beat_num++;
        if (beat_num==signature->signature.denominator+1)
          beat_num=1;

        int lpb = lpb_holder.get_lpb_at_place(beat_pos);
          
        Place beat_length = get_beat_length_in_measurement(signature->signature, lpb); // This walue will be incorrect if there are lpb changes betwen beat_pos and beat_pos+beat_length. TODO.

        beat_pos = p_Add(beat_pos, beat_length);
      }
    }

    signature = next;
  }

  
  
  return beats;
}



struct WSignatures *WSignatures_get(
                                    const struct Tracker_Windows *window,
                                    const struct WBlocks *wblock
                                    )
{
        struct WSignatures *wsignatures = (struct WSignatures *)talloc_atomic_clean(sizeof(struct WSignatures)*wblock->num_reallines);

	int realline=0;

        struct Beats *beat = Beats_get(wblock->block);

	while(beat!=NULL){

		realline=FindRealLineFor(wblock,realline,&beat->l.p);

		if(wsignatures[realline].signature.numerator!=0){
			wsignatures[realline].type=SIGNATURE_MUL;
		}else{
			if(PlaceNotEqual(&wblock->reallines[realline]->l.p,&beat->l.p))
				wsignatures[realline].type=SIGNATURE_BELOW;
		}

                if (wsignatures[realline].type != SIGNATURE_MUL){ // Unlike the multi-behavior for other wxxx-types, we show the first element here, and not the last.
                  wsignatures[realline].signature = beat->signature;
                  wsignatures[realline].beat_num  = beat->beat_num;
                }

		beat = NextBeat(beat);
	}

        return wsignatures;
}


struct Signatures *SetSignature(
	struct Blocks *block,
	const Place *place,
        Ratio ratio
){
        struct Signatures *signature=(struct Signatures *)ListFindElement3(&block->signatures->l,place);

	if(signature!=NULL && PlaceEqual(&signature->l.p,place)){
               signature->signature=ratio;
	}else{
                signature=(struct Signatures*)talloc(sizeof(struct Signatures));
		PlaceCopy(&signature->l.p,place);
		signature->signature=ratio;
		ListAddElement3(&block->signatures,&signature->l);
	}
        
	//UpdateSTimes(block);

        return signature;
}


void SetSignatureCurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;
	int curr_realline=wblock->curr_realline;
	Place *place= &wblock->reallines[curr_realline]->l.p;
	char *signaturestring = GFX_GetString(window,NULL,"New Time Signature: >");
	if(signaturestring==NULL) return;

        Rational rational = create_rational_from_string(signaturestring);
        if (rational.is_valid()==false || rational.numerator<=0 || rational.denominator<=0)
          return;
        
	PlayStop();

	Undo_Signatures_CurrPos(window);

	SetSignature(wblock->block,place,rational.get_ratio());

        //UpdateWBlockWidths(window, wblock);
        
	//UpdateWSignatures(window,wblock);
	//DrawSignatures(window,wblock,curr_realline,curr_realline);

        wblock->block->is_dirty = true;

	//WBLOCK_DrawTempoColor(window,wblock,curr_realline,wblock->num_reallines);
}

static void RemoveSignatures(struct Blocks *block,Place *p1,Place *p2){
	ListRemoveElements3(&block->signatures,p1,p2);
}

void RemoveSignaturesCurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;
	int curr_realline=wblock->curr_realline;

	Place p1,p2;

	PlayStop();

	Undo_Signatures_CurrPos(window);

	PlaceSetReallinePlace(wblock,curr_realline,&p1);
	PlaceSetReallinePlace(wblock,curr_realline+1,&p2);

	RemoveSignatures(wblock->block,&p1,&p2);

        //UpdateWBlockWidths(window, wblock);
        
	//UpdateWSignatures(window,wblock);
	//UpdateSTimes(wblock->block);

        wblock->block->is_dirty = true;
}




