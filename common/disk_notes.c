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
        
	SavePlace(&note->end);
	DC_SaveI(note->velocity_end);
        DC_SaveF(note->pitch_end);
	DC_SaveI(note->noend);

        DC_SaveI(note->chance);
        
	SaveVelocities(note->velocities);
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
        
	note->velocity=DC_LoadI(); if(disk_load_version<0.67) note->velocity=note->velocity*MAX_VELOCITY/127;
        if (disk_load_version >= 0.775) note->velocity_first_logtype = LoadLogType();
                
	LoadPlace(&note->end);
	note->velocity_end=DC_LoadI(); if(disk_load_version<0.67) note->velocity_end=note->velocity_end*MAX_VELOCITY/127;
        if (disk_load_version>0.75)
          note->pitch_end = DC_LoadF();
	note->noend=DC_LoadI();

        if (disk_load_version > 0.835)
          note->chance = DC_LoadI();
        else
          note->chance = 0x100;
        
        if(disk_load_version<0.69) {

          if(DC_Next()==LS_OBJECT){
            LoadVelocities(&note->velocities);
            DC_Next();
          }

        } else {

            DC_Next();
            LoadVelocities(&note->velocities);

            DC_Next();
            LoadPitches(&note->pitches);
            DC_Next();
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
