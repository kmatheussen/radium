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
#include "list_proc.h"
#include "realline_calc_proc.h"
#include "undo.h"
#include "undo_notes_proc.h"
#include "trackreallines2_proc.h"
#include "data_as_text_proc.h"
#include "atomic.h"

#include "centtext_proc.h"

int CENTTEXT_subsubtrack(const struct Tracker_Windows *window, const struct WTracks *wtrack){
  int curr_track_sub = window->curr_track_sub;
  if (wtrack->centtext_on == false)
    return -1;

  if (wtrack->swingtext_on == true)
    curr_track_sub -= 3;
  
  if (curr_track_sub < 0)
    return -1;

  if (curr_track_sub >= 2)
    return -1;

  return curr_track_sub;
}

bool CENTTEXT_keypress(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, int realline, const Place *place, int key){
  int subsubtrack = CENTTEXT_subsubtrack(window, wtrack);

  if (subsubtrack==-1)
    return false;
  
  const Trs &trs = TRS_get(wblock, wtrack, realline);

  ADD_UNDO(Notes_CurrPos(window));  

  if (trs.size()  > 1) {

    // MORE THAN ONE ELEMENT
    
    if (key == EVENT_DEL){

      for(const TrackRealline2 &tr2 : trs){
        const r::NotePtr &note = tr2.note;

        switch(tr2.type){
          case TR2_NOTE_START:
            safe_float_write(&note->_val, floorf(note->_val));
            break;
          case TR2_NOTE_END:
            safe_float_write(&note->d._pitch_end, floorf(note->d._pitch_end));
            break;
          case TR2_PITCH:
            {
              r::PitchTimeData::Writer writer(&note->_pitches, wtrack->track->_notes2);
              if (tr2.pitchnum < 0 || tr2.pitchnum >= writer.size())
                R_ASSERT(false);
              else 
                writer.at_ref(tr2.pitchnum)._val = float(writer.at_ref(tr2.pitchnum)._val);
            }
            break;
          case TR2_STOP:
            break;
        }

      }
      
    } else {
      
      UNDO_CANCEL_LAST_UNDO();
      
    }
    
  } else if (trs.size() == 0) {

    // NO ELEMENTS
    
    UNDO_CANCEL_LAST_UNDO();

  } else {

    // ONE ELEMENT
    
    const TrackRealline2 &tr2 = trs[0];
    const r::NotePtr &dasnote = tr2.note;

    if (key == EVENT_DEL) {

      switch(tr2.type){
        case TR2_NOTE_START:
          safe_float_write(&dasnote->_val, floorf(dasnote->_val));
          break;
        case TR2_NOTE_END:
          safe_float_write(&dasnote->d._pitch_end, floorf(dasnote->d._pitch_end));
          break;
        case TR2_PITCH:
          {
            r::PitchTimeData::Writer writer(&dasnote->_pitches, wtrack->track->_notes2);

            //float bef = writer.at_ref(tr2.pitchnum)._val;
            
            if (tr2.pitchnum < 0 || tr2.pitchnum >= writer.size())
              R_ASSERT(false);
            else 
              writer.at_ref(tr2.pitchnum)._val = floor(writer.at_ref(tr2.pitchnum)._val);

            //printf("  Bef: %f. Aft: %f\n", bef, writer.at_ref(tr2.pitchnum)._val);
          }
          break;
        case TR2_STOP:
          UNDO_CANCEL_LAST_UNDO();
          return false;
          break;
      }

    } else {

      double note;

      switch(tr2.type){
        case TR2_NOTE_START:
          note = dasnote->_val;
          break;
        case TR2_NOTE_END:
          note = dasnote->d._pitch_end;
          break;
        case TR2_PITCH:
          {
            const r::PitchTimeData::Reader reader(&dasnote->_pitches);
            if (tr2.pitchnum < 0 || tr2.pitchnum >= reader.size()){
              R_ASSERT(false);
              return true;
            }

            note = reader.at_ref(tr2.pitchnum)._val;
          }
          break;
        case TR2_STOP:
          UNDO_CANCEL_LAST_UNDO();
          return false;
          break;
        default:
          R_ASSERT_NON_RELEASE(false);
          return false;
      }
      
      int cents = round((note - floor(note)) * 100);
        
      data_as_text_t dat = DAT_get_overwrite(cents, 0, subsubtrack, key, 0, 99, 0, 99, false, false);

      if (dat.is_valid==false){
        UNDO_CANCEL_LAST_UNDO();
        return false;
      }

      double new_note = floor(note) + ((double)dat.value / 100.0);
      //printf("new_note: %f, dat.value: %d / %f\n",new_note,dat.value,((float)dat.value / 100.0f));

      switch(tr2.type){
        case TR2_NOTE_START:
          safe_float_write(&dasnote->_val, new_note);
          break;
        case TR2_NOTE_END:
          safe_float_write(&dasnote->d._pitch_end, new_note);
          break;
        case TR2_PITCH:
          {
            r::PitchTimeData::Writer writer(&dasnote->_pitches, wtrack->track->_notes2);
            if (tr2.pitchnum < 0 || tr2.pitchnum >= writer.size())
              R_ASSERT(false);
            else 
              writer.at_ref(tr2.pitchnum)._val = new_note;
          }
          break;
        case TR2_STOP:
          R_ASSERT_NON_RELEASE(false);
          break;
      }
      
    }    
  }

  return true;
}
