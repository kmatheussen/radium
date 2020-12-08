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
#include "disk_placement_proc.h"

#include "disk_stops_proc.h"



void SaveStops(struct Stops *stop){
if(stop==NULL) return;

DC_start("STOPS");

	do{
		SavePlace(&stop->l.p);
		stop=NextStop(stop);
	}while(stop!=NULL);

DC_end();
}


void LoadStops(struct Stops **to){

	struct Stops *stop;

	while(dc.success){
		DC_fgets();
		if(dc.ret[0]=='/') return;
		stop=DC_alloc(sizeof(struct Stops));
		stop->Tline=atoi(dc.ret);
		stop->Tcounter=DC_LoadU32();
		stop->Tdividor=DC_LoadU32();
		ListAddElement3_a(to,&stop->l);
	}

error:
	return;
}

