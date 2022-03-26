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
#include "TimeData.hpp"
#include "disk.h"
#include "disk_placement_proc.h"

#include "disk_velocities_proc.h"



void SaveVelocities(const struct Notes *note){
DC_start("VELOCITIES2");

 const r::VelocityTimeData::Reader reader(note->_velocities);
 
 for(const r::Velocity &velocity : reader){
   DC_SaveRatio(velocity._time);
   
   DC_SaveI(velocity._val);
   SaveLogType(velocity._logtype);
 }

DC_end();
}


void LoadVelocities(struct Notes *note){

  r::VelocityTimeData::Writer writer(note->_velocities);
  

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

        r::VelocityTimeData::Writer writer(note->_velocities);

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


void SavePitches(const struct Notes *note){
DC_start("PITCHES");

 #if 0
	while(pitch!=NULL){
		SavePlace(&pitch->l.p);
		DC_SaveF(pitch->note);
                SaveLogType(pitch->logtype);
                DC_SaveI(pitch->chance);
		pitch=NextPitch(pitch);
	}
#endif
        
        const r::PitchTimeData::Reader reader(note->_pitches);
 
        for(const r::Pitch &pitch : reader){
          DC_SaveRatio(pitch._time);
          
          DC_SaveF(pitch._val);
          SaveLogType(pitch._logtype);
          DC_SaveI(pitch._chance);
        }

DC_end();
}


void LoadPitches(struct Notes *note){

  r::PitchTimeData::Writer writer(note->_pitches);
    
  while(dc.success){
    DC_fgets();
    if(dc.ret[0]=='/') return;
    
    Ratio ratio = DC_LoadRatio(atoll(dc.ret), true);
    float val = DC_LoadF();
    int logtype = LoadLogType();
    int chance = DC_LoadI();
    
    writer.add(ratio, val, logtype, chance);
  }

}


void LoadPitches_oldformat(struct Notes *note) {

        r::PitchTimeData::Writer writer(note->_pitches);

        /*
        struct Pitches **to = &note->pitches;
        
	struct Pitches *pitch;
        */
        
	while(dc.success){
		DC_fgets();
		if(dc.ret[0]=='/') return;

                /*
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
                  pitch->chance = MAX_PATCHVOICE_CHANCE;
                
		ListAddElement3_a(to,&pitch->l);
                */

                Place p;
                p.line=atoi(dc.ret);
		p.counter=DC_LoadU32();
		p.dividor=DC_LoadU32();
                
		float note=DC_LoadF();

                int logtype = 0;
                if (disk_load_version >= 0.775)
                  logtype = LoadLogType();

                int chance;
                if (disk_load_version > 0.835)
                  chance = DC_LoadI();
                else
                  chance = MAX_PATCHVOICE_CHANCE;

                {
                  auto node = r::Pitch(place2ratio(p), note, logtype);
                  node._chance = chance;
                
                  writer.add(node);
                }
	}

        /*
error:
	return;
        */
}





