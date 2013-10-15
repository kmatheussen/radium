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
DC_start("VELOCITIES");
 
	while(velocity!=NULL){
		SavePlace(&velocity->l.p);
		DC_SaveI(velocity->velocity);
		velocity=NextVelocity(velocity);
	}

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
		velocity->velocity=DC_LoadI();  if(disk_load_version<0.67) velocity->velocity=velocity->velocity*MAX_VELOCITY/127;
		ListAddElement3(to,&velocity->l);
	}


error:
	return;
}


void SavePitches(struct Pitches *pitch){
DC_start("PITCHES");
 
	while(pitch!=NULL){
		SavePlace(&pitch->l.p);
		DC_SaveF(pitch->note);
		pitch=NextPitch(pitch);
	}

DC_end();
}


void LoadPitches(struct Pitches **to){

	struct Pitches *pitch;


	while(dc.success){
		DC_fgets();
		if(dc.ret[0]=='/') return;
		pitch=DC_alloc(sizeof(struct Pitches));
		pitch->Tline=atoi(dc.ret);
		pitch->Tcounter=DC_LoadU32();
		pitch->Tdividor=DC_LoadU32();
		pitch->note=DC_LoadF();
		ListAddElement3(to,&pitch->l);
	}


error:
	return;
}





