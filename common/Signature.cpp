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


struct WSignatures *WSignatures_get(
                        const struct Tracker_Windows *window,
                        const struct WBlocks *wblock
                        )
{
        struct WSignatures *wsignatures = (struct WSignatures *)talloc_atomic_clean(sizeof(struct WSignatures)*wblock->num_reallines);

	int realline=0;

	struct Signatures *signature=wblock->block->signatures;

	while(signature!=NULL){

		realline=FindRealLineFor(wblock,realline,&signature->l.p);

		if(wsignatures[realline].signature.numerator!=0){
			wsignatures[realline].type=SIGNATURE_MUL;
		}else{
			if(PlaceNotEqual(&wblock->reallines[realline]->l.p,&signature->l.p))
				wsignatures[realline].type=SIGNATURE_BELOW;
		}

		wsignatures[realline].signature=signature->signature;
		//wsignatures[realline].Signature=signature;

		signature=NextSignature(signature);
	}

        return wsignatures;
}

struct Signatures *SetSignature(
	struct Blocks *block,
	Place *place,
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

        UpdateWBlockWidths(window, wblock);
        
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

        UpdateWBlockWidths(window, wblock);
        
	//UpdateWSignatures(window,wblock);
	//UpdateSTimes(wblock->block);

        wblock->block->is_dirty = true;
}




