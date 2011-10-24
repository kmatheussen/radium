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

#include "disk_lpbs_proc.h"



void SaveLPBs(struct LPBs *lpb){
if(lpb==NULL) return;
DC_start("LPBs");

	do{
		SavePlace(&lpb->l.p);
		DC_SaveI(lpb->lpb);
		lpb=NextLPB(lpb);
	}while(lpb!=NULL);

DC_end();
}



void LoadLPBs(struct LPBs **to){

	struct LPBs *lpb;

	while(dc.success){
		DC_fgets();
		if(dc.ret[0]=='/') return;
		lpb=DC_alloc(sizeof(struct LPBs));
		lpb->Tline=atoi(dc.ret);
		lpb->Tcounter=DC_LoadU32();
		lpb->Tdividor=DC_LoadU32();
		lpb->lpb=DC_LoadI();
		ListAddElement3(to,&lpb->l);
	}

error:
	return;
}

