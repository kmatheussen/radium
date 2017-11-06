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
#include "disk.h"
#include "disk_song_proc.h"

#include "disk_root_proc.h"

extern struct Root *root;


void SaveRoot(struct Root *theroot){
DC_start("ROOT");

//	DC_SSN("def_instrument",theroot->def_instrument->l.num);
        DC_SSN("curr_block",0); //ATOMIC_GET(theroot->curr_blocknum));
	DC_SSI("tempo",theroot->tempo);
	DC_SSI("lpb",theroot->lpb);
        DC_SSL("signature_numerator",theroot->signature.numerator);
        DC_SSL("signature_denominator",theroot->signature.denominator);
	DC_SSL("quantitize_numerator",theroot->quantitize_options.quant.numerator);
        DC_SSL("quantitize_denominator",theroot->quantitize_options.quant.denominator);
        DC_SSF("quantitize",(double)theroot->quantitize_options.quant.numerator / (double)theroot->quantitize_options.quant.denominator);
	DC_SSL("grid_numerator",theroot->grid_numerator);
	DC_SSL("grid_denominator",theroot->grid_denominator);
	DC_SSI("keyoct",theroot->keyoct); // not used anymore. Still saved though so that new songs can be opened in older radiums.
	DC_SSI("min_standardvel",theroot->min_standardvel);
	DC_SSI("standardvel",theroot->standardvel);

	SaveSong(theroot->song);

DC_end();
}



/*********** Start Load song *************************/

struct Root *LoadRoot(void){
          
	static char *objs[1]={
		"SONG"
	};
	static char *vars[14]={
		"def_instrument",
		"curr_block",
		"tempo",
		"lpb",
                "signature_numerator",
                "signature_denominator",                
		"quantitize",
                "quantitize_numerator",
                "quantitize_denominator",
                "grid_numerator",
                "grid_denominator",
		"keyoct",
		"min_standardvel",
		"standardvel"
	};
	struct Root *ret=DC_alloc(sizeof(struct Root));
	ret->keyoct = root->keyoct;
        ret->quantitize_options = root->quantitize_options;
        ret->min_standardvel=MAX_VELOCITY*40/100;
        ATOMIC_SET(ret->editonoff, true);
        ret->grid_numerator=1;
        ret->grid_denominator=1;
        ret->signature.numerator=4;
        ret->signature.denominator=4;
        
        ATOMIC_SET(ret->play_cursor_onoff, ATOMIC_GET(root->play_cursor_onoff));
        ATOMIC_SET(ret->editor_follows_play_cursor_onoff, ATOMIC_GET(root->editor_follows_play_cursor_onoff));
        
	GENERAL_LOAD(1,14);



obj0:
	ret->song=LoadSong();
	goto start;
var0:
	goto start;			// Don't bother with instruments yet.
var1:
        DC_LoadN(); // not used anymore
	//ATOMIC_SET(ret->curr_blocknum, DC_LoadN());
	goto start;
var2:
	ret->tempo=DC_LoadI();
	goto start;
var3:
	ret->lpb=DC_LoadI();
	goto start;

var4:
	ret->signature.numerator=DC_LoadI();
	goto start;

var5:
	ret->signature.denominator=DC_LoadI();
	goto start;

var6:
        DC_LoadF();
	goto start;

var7:
        ret->quantitize_options.quant.numerator = DC_LoadI();
	goto start;

var8:
        ret->quantitize_options.quant.denominator = DC_LoadI();
	goto start;

var9:
        ret->grid_numerator=DC_LoadI();
        goto start;

var10:
        ret->grid_denominator=DC_LoadI();
        goto start;

var11:
	DC_LoadI();
        //ret->keyoct=DC_LoadI();
	goto start;

var12:
	ret->min_standardvel=DC_LoadI();
	goto start;

var13:
	ret->standardvel=DC_LoadI();
	goto start;

var14:
var15:
var16:
var17:
var18:
var19:
 var20:
 var21:
        
obj1:
obj2:
obj3:
obj4:
obj5:
obj6:
debug("loadroot, very wrong\n");

error:
debug("loadroot, goto error\n");

end:

	return ret;
}


extern struct Root *root;

void DLoadRoot(struct Root *theroot){
	DLoadSong(theroot,theroot->song);
}


/*********************** End Load Song **************************/


