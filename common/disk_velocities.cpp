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
#include "disk.h"
#include "disk_placement_proc.h"

#include "disk_velocities_proc.h"



void SaveVelocities(struct Notes *note){
DC_start("VELOCITIES2");

  r::TimeData<r::Velocity>::Reader reader(note->_velocities);
  
  for(const r::Velocity &velocity : reader){
    DC_SaveRatio(velocity._time);
    
    DC_SaveI(velocity._val);
    SaveLogType(velocity._logtype);
  }

DC_end();
}


void LoadVelocities(struct Notes *note){

  r::TimeData<r::Velocity>::Writer writer(note->_velocities);
  

	while(dc.success){
		DC_fgets();
		if(dc.ret[0]=='/') return;

                Ratio ratio = DC_LoadRatio(atoll(dc.ret), true);
                int velocity = DC_LoadI();
                int logtype = LoadLogType();

                auto node = r::Velocity(ratio, velocity, logtype);
                writer.add(node);
	}

}


void LoadVelocities_oldformat(struct Notes *note){

  r::TimeData<r::Velocity>::Writer writer(note->_velocities);

	while(dc.success){
		DC_fgets();
		if(dc.ret[0]=='/') return;

                Place p;
                p.line = atoi(dc.ret);
		p.counter=DC_LoadU32();
		p.dividor=DC_LoadU32();
                
		int velocity=DC_LoadI();
                int logtype = LOGTYPE_LINEAR;
                
                if(disk_load_version<0.67)
                  velocity=velocity*MAX_VELOCITY/127;
                else if (disk_load_version >= 0.775)
                  logtype = LoadLogType();
                    
                auto node = r::Velocity(place2ratio(p), velocity, logtype);
                writer.add(node);
	}

	return;
}


void SavePitches(struct Pitches *pitch){
DC_start("PITCHES");
 
	while(pitch!=NULL){
		SavePlace(&pitch->l.p);
		DC_SaveF(pitch->note);
                SaveLogType(pitch->logtype);
                DC_SaveI(pitch->chance);
		pitch=NextPitch(pitch);
	}

DC_end();
}


void LoadPitches(struct Pitches **to){

	struct Pitches *pitch;


	while(dc.success){
		DC_fgets();
		if(dc.ret[0]=='/') return;
		pitch=(struct Pitches*)DC_alloc(sizeof(struct Pitches));
		pitch->Tline=atoi(dc.ret);
		pitch->Tcounter=DC_LoadU32();
		pitch->Tdividor=DC_LoadU32();
		pitch->note=DC_LoadF();
                
                if (disk_load_version >= 0.775)
                  pitch->logtype = LoadLogType();

                if (disk_load_version > 0.835)
                  pitch->chance = DC_LoadI();
                else
                  pitch->chance = 0x100;
                
		ListAddElement3_a(to,&pitch->l);
	}


error:
	return;
}





