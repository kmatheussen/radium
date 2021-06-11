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
#include "placement_proc.h"
#include "disk.h"
#include "disk_placement_proc.h"
#include "disk_velocities_proc.h"
#include "notes_proc.h"

#include "disk_notes_proc.h"



void SaveNotes(struct Notes *note){
if(note==NULL) return;
DC_start("NOTE");

	SavePlace(&note->l.p);

	DC_SaveF(note->note);
        SaveLogType(note->pitch_first_logtype);
        
	DC_SaveI(0); // cents (never used, inserted into pitch instead)
        
	DC_SaveI(note->velocity);
        SaveLogType(note->velocity_first_logtype);
        
        DC_SaveRatio(note->end);
        
	DC_SaveI(note->velocity_end);
        DC_SaveF(note->pitch_end);
	DC_SaveI(note->noend);

        DC_SaveI(note->chance);
        
	SaveVelocities(note);
	SavePitches(note->pitches);

DC_end();
SaveNotes(NextNote(note));
}


struct Notes *LoadNote(void){
        struct Notes *note = NewNote();

	LoadPlace(&note->l.p);
        
	note->note=DC_LoadF();
        if (disk_load_version >= 0.775) note->pitch_first_logtype = LoadLogType();

	DC_LoadI(); // cents
        
	note->velocity=DC_LoadI();
        
        if(disk_load_version<0.67)
          note->velocity=note->velocity*MAX_VELOCITY/127;        
        else if (disk_load_version >= 0.775)
          note->velocity_first_logtype = LoadLogType();

        if (disk_load_version < 1.225) {
          Place noteend;
          LoadPlace(&noteend);
          note->end = place2ratio(noteend);
        } else {
          note->end = DC_LoadRatio();
        }
        
	note->velocity_end=DC_LoadI();
        
        if(disk_load_version<0.67)
          note->velocity_end=note->velocity_end*MAX_VELOCITY/127;
        else if (disk_load_version>0.75)
          note->pitch_end = DC_LoadF();
        
	note->noend=DC_LoadI();

        if (disk_load_version > 0.835)
          note->chance = DC_LoadI();
        else
          note->chance = 0x100;
        
        if(disk_load_version<0.69) {

          if(DC_Next()==LS_OBJECT){
            LoadVelocities_oldformat(note);
            DC_Next();
          }

        } else {

            DC_Next();

            if (disk_load_version < 1.225) {
              LoadVelocities_oldformat(note);
            } else
              LoadVelocities(note);
            
            DC_Next();
            LoadPitches(&note->pitches);
            DC_Next();
        }

        // Workaround for bug in old Radium (<= V6.9.75). We do this to make sure old songs sound the same.
        //
        if (disk_load_version > 0.88 && disk_load_version < 1.225) {

          /* In these old versions of Radium, if the following test was true:

                (note->velocities==NULL && equal_floats(note->velocity_end, 0.0)

             ...then Radium didn't handle velocities for that note. Of course, the test should have looked like this:
 
                note->velocities==NULL && (note->velocity_first_logtype==LOGTYPE_HOLD || equal_floats(note->velocity, note->velocity_end)

             ...But nonetheless, old songs should sound the same, so we just set velocity_end to the same value as velocity_first when
             velocity_end==0 and velocities.size()==0.
          */

          if (r::TimeData<r::Velocity>::Reader(note->_velocities).size()==0)
            if (equal_floats(note->velocity_end, 0.0))
              note->velocity_end = note->velocity;
        }
        
        /*
error:
        */
	if(!dc.success) debug("note not okay\n");else debug("note okay\n");
	return note;
}


void DLoadNotes(const struct Root *newroot,struct Tracks *track, struct Notes *notes){
  if(notes==NULL) return;

  // Fix pitch_end for older songs.
  if(notes->pitches != NULL && equal_floats(notes->pitch_end, 0)){
    struct Notes *next_note = NextNote(notes);
    if (next_note==NULL)
      next_note = track->notes;
    notes->pitch_end = next_note->note;
  }
  
  DLoadNotes(newroot, track, NextNote(notes));
}
