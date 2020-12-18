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
#include "placement_proc.h"
#include "disk.h"
#include "disk_placement_proc.h"

#include "disk_stops_proc.h"



void SaveStops(struct Tracks *track){

#if 1

  r::TimeData<r::Stop>::Reader reader(track->stops2);
  if (reader.size()==0)
    return;
  
  DC_start("STOPS");
  
  for(const r::Stop &stop : reader){
    Place p = make_place_from_ratio(stop._time);
    SavePlace(&p);
  }

#else
        if(track->stops==NULL) return;
        DC_start("STOPS");
	do{
		SavePlace(&stop->l.p);
		stop=NextStop(stop);
	}while(stop!=NULL);
#endif
DC_end();
}


void LoadStops(struct Tracks *track){

        r::TimeData<r::Stop>::Writer writer(track->stops2);
  
        R_ASSERT_NON_RELEASE(writer.size()==0);
        
	while(dc.success){
		DC_fgets();
		if(dc.ret[0]=='/') return;
                Place p;
                p.line = atoi(dc.ret);
                p.counter = DC_LoadU32();
                p.dividor = DC_LoadU32();
                r::Stop stop2(ratio_from_place(p));
                writer.add(stop2);
	}
}

