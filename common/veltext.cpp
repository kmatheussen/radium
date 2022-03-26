/* Copyright 2016 Kjetil S. Matheussen

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



#include <math.h>

#include "nsmtracker.h"
#include "TimeData.hpp"
#include "vector_proc.h"
#include "realline_calc_proc.h"
#include "undo.h"
#include "undo_notes_proc.h"
#include "list_proc.h"
#include "notes_proc.h"
#include "velocities_proc.h"
#include "data_as_text_proc.h"
#include "atomic.h"

#include "../api/api_proc.h"
#include "../api/api_common_proc.h"

#include "veltext_proc.h"


static void add_veltext(const struct WBlocks *wblock, VelText_trss &veltexts, VelText &veltext, struct Notes *note, int velocity){
  int realline = FindRealLineFor(wblock, 0, &veltext.p);
  VelText_trs &trs = veltexts[realline];
  
  veltext.value = round(scale_double(velocity, 0, MAX_VELOCITY, 0, 0x100));
  if (veltext.value >= 0x100)
    veltext.value = 0xff;
  
  veltext.note = note;

  veltext.velocity_value = velocity;

  TRS_INSERT_PLACE(trs, veltext);
}
                   

static void add_velocity(const struct WBlocks *wblock, VelText_trss &veltexts, struct Notes *note, const r::Velocity &velocity, int velnum){
  VelText tr = {};
  tr.p = ratio2place(velocity._time);//velocity->l.p;
  //tr.velocity = velocity;
  tr.logtype = velocity._logtype;
  tr.velnum = velnum;
  add_veltext(wblock, veltexts, tr, note, velocity._val);
}

static void add_note(const struct WBlocks *wblock, VelText_trss &veltexts, struct Notes *note){

  int last_velocity = note->velocity;
  int last_logtype = note->velocity_first_logtype;
  
  {
    VelText tr = {};
    tr.p = note->l.p;
    tr.logtype = note->velocity_first_logtype;
    tr.is_first_velocity = true;
    tr.velnum=-1;
    add_veltext(wblock, veltexts, tr, note, note->velocity);
  }


  {
    r::VelocityTimeData::Reader reader(note->_velocities);
    
    int velnum = 0;
    for(const r::Velocity &velocity : reader){
      add_velocity(wblock, veltexts, note, velocity, velnum);
      last_velocity = velocity._val;
      last_logtype = velocity._logtype;
      velnum++;
    }
  }

  if (last_velocity != note->velocity_end && last_logtype!=LOGTYPE_HOLD)  {
    VelText tr = {};
    tr.p = ratio2place(note->end);
    tr.logtype = LOGTYPE_IRRELEVANT;
    tr.is_last_velocity = true;
    tr.velnum=-2;
    add_veltext(wblock, veltexts, tr, note, note->velocity_end);
  }
}


static void move_veltexts_to_unique_reallines(const struct WBlocks *wblock, VelText_trss &veltexts){
  for(int realline=0;realline<wblock->num_reallines-1;realline++){
    
    if (veltexts.contains(realline) && !veltexts.contains(realline+1)) {
      
      VelText_trs &trs1 = veltexts[realline];
      
      if (trs1.size() > 1) {
        VelText_trs &trs2 = veltexts[realline+1];
      
        trs2.push_back(trs1.takeFirst());
        
        std::swap(veltexts[realline], veltexts[realline+1]);
      }
    }
  }
}

// Returns a pointer to AN ARRAY of vectors (one vector for each realline), not a pointer to a vector (as one would think).
const VelText_trss VELTEXTS_get(const struct WBlocks *wblock, const struct WTracks *wtrack){
  VelText_trss veltexts;

  struct Notes *note = wtrack->track->notes;
  while(note!=NULL){
    add_note(wblock, veltexts, note);
    note = NextNote(note);
  }

  move_veltexts_to_unique_reallines(wblock, veltexts);
    
  return veltexts;
}

int VELTEXT_subsubtrack(struct Tracker_Windows *window, struct WTracks *wtrack){
  int curr_track_sub = window->curr_track_sub;

  if (wtrack->veltext_on == false)
    return -1;

  if (wtrack->swingtext_on == true)
    curr_track_sub -= 3;
  
  if (wtrack->centtext_on)
    curr_track_sub -= 2;
  
  if (wtrack->chancetext_on)
    curr_track_sub -= 2;
  
  if (curr_track_sub < 0)
    return -1;

  if (curr_track_sub > 2)
    return -1;

  return curr_track_sub;
}



bool VELTEXT_keypress(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, int realline, const Place *place, int key){
  int subsubtrack = VELTEXT_subsubtrack(window, wtrack);

  if (subsubtrack==-1)
    return false;
  
  const VelText_trss &veltexts = VELTEXTS_get(wblock, wtrack);

  const VelText_trs &veltext = veltexts[realline];

  //  if (veltext->num_elements == 0 && val==0)
  //   return true;
  
  ADD_UNDO(Notes_CurrPos(window));  

  if (veltext.size() > 1) {

    // MORE THAN ONE ELEMENT
    
    if (key == EVENT_DEL){

      /*
      PLAYER_lock();{
        for (auto vt : veltext){
          struct Notes *note = vt.note;
          if (vt.velocity != NULL)
            ListRemoveElement3(&note->velocities, &vt.velocity->l);
        }
      }PLAYER_unlock();
      */
      
      for (auto vt : veltext){
        struct Notes *note = vt.note;
        if (vt.velnum >= 0) {
          r::VelocityTimeData::Writer writer(note->_velocities);
          writer.remove_at_pos(vt.velnum);
        }
      }

    } else {
      
      UNDO_CANCEL_LAST_UNDO();
      
    }
    
  } else if (veltext.size() == 0) {

    // NO ELEMENTS
    
    struct Notes *note = FindNote(wtrack->track, place);

    if (note == NULL){
      
      UNDO_CANCEL_LAST_UNDO();
      
    } else {

      data_as_text_t dat = DAT_get_newvalue(subsubtrack, key, NOTE_get_velocity(wtrack->track), LOGTYPE_LINEAR, 0, 0xff, 0, MAX_VELOCITY, true, true, true);

      if (dat.is_valid==false)
        return false;

      AddVelocity3(dat.logtype, dat.value, place, note);
    }

  } else {

    // ONE ELEMENT
    
    const VelText &vt = veltext.at(0);
    struct Notes *note = vt.note;
    //struct Velocities *velocity = vt.velocity;

    if (key == EVENT_DEL) {

      if (false && subsubtrack == 2) {
        
        r::VelocityTimeData::Writer writer(note->_velocities);
        writer.at_ref(vt.velnum)._logtype = LOGTYPE_LINEAR;

      } else {

        int velocity_num;
        
        if (vt.is_first_velocity)
          velocity_num = 0;
        else if (vt.is_last_velocity)
          velocity_num = r::VelocityTimeData::Reader(note->_velocities).size() + 1;
        else
          velocity_num = vt.velnum + 1;
        
        deleteVelocity(velocity_num, DYN_create_string_from_chars(GetNoteIdAsCharString(note->id)), wtrack->l.num, wblock->l.num, window->l.num);
        
      }
      
    } else {

      data_as_text_t dat = DAT_get_overwrite(vt.value, vt.logtype, subsubtrack, key, 0, 0xff, 0, MAX_VELOCITY, true, true);

      if (dat.is_valid==false)
        return false;

      int num_velocities = r::VelocityTimeData::Reader(note->_velocities).size();
        
      if (vt.is_first_velocity){

        bool is_equal = false;

        if (false
            || num_velocities==0
            || (num_velocities==1 && dat.logtype==LOGTYPE_HOLD)
            )
          is_equal = note->velocity==note->velocity_end; // Note: not using equal_floats since note->velocity is an integer.

        safe_int_write(&note->velocity, dat.value);
        safe_int_write(&note->velocity_first_logtype, dat.logtype);

        if (is_equal)
          safe_int_write(&note->velocity_end, dat.value);
        
      } else if (vt.is_last_velocity){

        safe_int_write(&note->velocity_end, dat.value);
        
      } else {

        bool is_equal = false;

        if (dat.logtype==LOGTYPE_HOLD && num_velocities==1)
          is_equal = vt.velocity_value==note->velocity_end;

        /*
        safe_int_write(&velocity->velocity, dat.value);
        safe_int_write(&velocity->logtype, dat.logtype);
        */
        
        {
          r::VelocityTimeData::Writer writer(note->_velocities);
          writer.at_ref(vt.velnum)._val = dat.value;
          writer.at_ref(vt.velnum)._logtype = dat.logtype;
        }
        
        if (is_equal)
          safe_int_write(&note->velocity_end, dat.value);

      }
      
    }    
  }

  return true;
}
  
