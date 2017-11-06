/* Copyright 2017 Kjetil S. Matheussen

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
#include "disk_placement_proc.h"

#include "disk_swing_proc.h"



void SaveSwings(struct Swing *swing){
if(swing==NULL) return;
DC_start("SWING");

        DC_start("PLACE");{
          SavePlace(&swing->l.p);
        }DC_end();

	DC_SSI("weight", swing->weight);
	DC_SSI("logtype", swing->logtype);

DC_end();
SaveSwings(NextSwing(swing));
}



struct Swing *LoadSwing(void){
        static char *objs[1]={
          "PLACE"
        };
	static char *vars[2]={
		"weight",
		"logtype"
	};
	struct Swing *swing=DC_alloc(sizeof(struct Swing));

	GENERAL_LOAD(1,2)

var0:
	swing->weight=DC_LoadI();
	goto start;
var1:
	swing->logtype=DC_LoadI();
	goto start;
        
obj0:
        LoadPlace(&swing->l.p);
        DC_Next();
	goto start;

var2:
var3:
var4:
var5:
var6:
var7:
var8:
var9:
var10:
var11:
var12:
var13:
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

error:
end:
	return swing;
}

