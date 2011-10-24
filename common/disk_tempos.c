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

#include "disk_tempos_proc.h"




void SaveTempos(struct Tempos *tempo){
if(tempo==NULL) return;
DC_start("TEMPOS");

	do{
		SavePlace(&tempo->l.p);
		DC_SaveI(tempo->tempo);
		tempo=NextTempo(tempo);
	}while(tempo!=NULL);


DC_end();
}


void LoadTempos(struct Tempos **to){

	struct Tempos *tempo;

	while(dc.success){
		DC_fgets();
		if(dc.ret[0]=='/') return;
		tempo=DC_alloc(sizeof(struct Tempos));
		tempo->Tline=atoi(dc.ret);
		tempo->Tcounter=DC_LoadU32();
		tempo->Tdividor=DC_LoadU32();
		tempo->tempo=DC_LoadI();
		ListAddElement3(to,&tempo->l);
	}

error:
	return;
}

