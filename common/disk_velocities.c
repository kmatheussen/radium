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

#include "disk_velocities_proc.h"



void SaveVelocities(struct Velocities *velocity){
if(velocity==NULL) return;
DC_start("VELOCITIES");

	do{
		SavePlace(&velocity->l.p);
		DC_SaveI(velocity->velocity);
		velocity=NextVelocity(velocity);
	}while(velocity!=NULL);

DC_end();
}


void LoadVelocities(struct Velocities **to){

	struct Velocities *velocity;


	while(dc.success){
		DC_fgets();
		if(dc.ret[0]=='/') return;
		velocity=DC_alloc(sizeof(struct Velocities));
		velocity->Tline=atoi(dc.ret);
		velocity->Tcounter=DC_LoadU32();
		velocity->Tdividor=DC_LoadU32();
		velocity->velocity=DC_LoadI();
		ListAddElement3(to,&velocity->l);
	}


error:
	return;
}





