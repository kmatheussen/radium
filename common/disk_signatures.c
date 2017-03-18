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

#include "disk_signatures_proc.h"



void SaveSignatures(struct Signatures *signature){
if(signature==NULL) return;
DC_start("SIGNATURES");

	do{
		SavePlace(&signature->l.p);
		DC_SaveL(signature->signature.numerator);
                DC_SaveL(signature->signature.denominator);
		signature=NextSignature(signature);
	}while(signature!=NULL);

DC_end();
}



void LoadSignatures(struct Signatures **to){

	struct Signatures *signature;

	while(dc.success){
		DC_fgets();
		if(dc.ret[0]=='/') return;
		signature=DC_alloc(sizeof(struct Signatures));
		signature->Tline=atoi(dc.ret);
		signature->Tcounter=DC_LoadU32();
		signature->Tdividor=DC_LoadU32();
		signature->signature.numerator=DC_LoadI();
                signature->signature.denominator=DC_LoadI();
		ListAddElement3_a(to,&signature->l);
	}

error:
	return;
}

