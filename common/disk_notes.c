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

#include "disk_notes_proc.h"



void SaveNotes(struct Notes *note){
if(note==NULL) return;
DC_start("NOTE");

	SavePlace(&note->l.p);

	DC_SaveI(note->note);
	DC_SaveI(note->cents);
	DC_SaveI(note->velocity);
	SavePlace(&note->end);
	DC_SaveI(note->velocity_end);
	DC_SaveI(note->noend);

	SaveVelocities(note->velocities);

DC_end();
SaveNotes(NextNote(note));
}


struct Notes *LoadNote(void){
	struct Notes *note=DC_alloc(sizeof(struct Notes));

	LoadPlace(&note->l.p);
	note->note=DC_LoadI();
	note->cents=DC_LoadI();
	note->velocity=DC_LoadI();
	LoadPlace(&note->end);
	note->velocity_end=DC_LoadI();
	note->noend=DC_LoadI();

	if(DC_Next()==LS_OBJECT){
		LoadVelocities(&note->velocities);
		DC_Next();
	}


error:
	if(!dc.success) debug("note not okey\n");else debug("note okey\n");
	return note;
}




