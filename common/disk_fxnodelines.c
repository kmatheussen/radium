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

#include "disk_fxnodelines_proc.h"



void SaveFXNodeLines(struct FXNodeLines *fxnodeline){
if(fxnodeline==NULL) return;
DC_start("FXNODELINES");

	do{
		SavePlace(&fxnodeline->l.p);
		DC_SaveI(fxnodeline->val);
                SaveLogType(fxnodeline->logtype);
		fxnodeline=NextFXNodeLine(fxnodeline);
	}while(fxnodeline!=NULL);


DC_end();
}


void LoadFXNodeLines(struct FXNodeLines **to){

	struct FXNodeLines *fxnodeline;

	printf("\t\tLoadFXNodeLines_start\n");
	while(dc.success){
		DC_fgets();
		if(dc.ret[0]=='/') return;
		fxnodeline=DC_alloc(sizeof(struct FXNodeLines));
		fxnodeline->Tline=atoi(dc.ret);
		fxnodeline->Tcounter=DC_LoadU32();
		fxnodeline->Tdividor=DC_LoadU32();
		fxnodeline->val=DC_LoadI();

                if (disk_load_version >= 0.775)
                  fxnodeline->logtype = LoadLogType();

		ListAddElement3_a(to,&fxnodeline->l);
	}

	printf("\t\tLoadFXNodeLines_error_end\n");
	return;
error:
	printf("\t\tLoadFXNodeLines_end\n");
	return;
}

