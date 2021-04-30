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
#include "TimeData.hpp"
#include "FX.hpp"
#include "placement_proc.h"
#include "disk.h"
#include "disk_placement_proc.h"

#include "disk_fxnodelines_proc.h"



void SaveFXNodeLines(struct FXs *fxs){
#if 1

  r::TimeData<r::FXNode>::Reader reader(fxs->_fxnodes);
  
  if (reader.size()==0)
    return;

  DC_start("FXNODELINES");

  for(const r::FXNode &fxnode : reader){
    Place p = make_place_from_ratio(fxnode._time);
    SavePlace(&p);
    DC_SaveI(fxnode._val);
    SaveLogType(fxnode._logtype);
  }

  DC_end();
  
#else
  
if(fxnodeline==NULL) return;
DC_start("FXNODELINES");

	do{
		SavePlace(&fxnodeline->l.p);
		DC_SaveI(fxnodeline->val);
                SaveLogType(fxnodeline->logtype);
		fxnodeline=NextFXNodeLine(fxnodeline);
	}while(fxnodeline!=NULL);


DC_end();
 
 #endif
}


void LoadFXNodeLines(struct FXs *fxs){

        r::TimeData<r::FXNode>::Writer writer(fxs->_fxnodes);
        
	printf("\t\tLoadFXNodeLines_start\n");
	while(dc.success){
		DC_fgets();
		if(dc.ret[0]=='/') return;

                Place p;
                p.line = atoi(dc.ret);
                p.counter = DC_LoadU32();
                p.dividor = DC_LoadU32();

                int val = DC_LoadI();
                int logtype = LOGTYPE_LINEAR;
                if (disk_load_version >= 0.775)
                  logtype = LoadLogType();

                /*
                {
                  struct FXNodeLines *fxnodeline=(struct FXNodeLines *)DC_alloc(sizeof(struct FXNodeLines));
                  fxnodeline->l.p = p;
                  fxnodeline->val=val;
                  fxnodeline->logtype = logtype;

                  ListAddElement3_a(&fxs->fxnodelines,&fxnodeline->l);
                }
                */
                
                auto node = r::FXNode(*fxs->fx, ratio_from_place(p), val, logtype);
                writer.add(node);

	}

	printf("\t\tLoadFXNodeLines_end\n");
	return;
}

