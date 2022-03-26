/* Copyright 2013 Kjetil S. Matheussen

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

#include <string.h>

#include "nsmtracker.h"
#include "TimeData.hpp"
#include "list_proc.h"
#include "placement_proc.h"
#include "notes_proc.h"
#include "player_pause_proc.h"
#include "player_proc.h"
#include "visual_proc.h"
#include "OS_Player_proc.h"

#include "pitches_proc.h"

/*
struct Notes *GetNextPitchNote(const struct Notes *note){
  R_ASSERT(note->pitches != NULL);

  struct Notes *note2 = (struct Notes*)note;
  
  Place *end = &note2->end;
  
  note2 = NextNote(note2);
  while(note2 != NULL){
    if (PlaceGreaterOrEqual(&note2->l.p, end))
      return note2;
    note2 = NextNote(note2);
  }

  return NULL;
}
*/

int AddPitch(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, struct Notes *note, const Place *place, float notenum){
  /*
  struct Pitches *pitch = (struct Pitches *)talloc(sizeof(struct Pitches));
  PlaceCopy(&pitch->l.p,place);
  pitch->note = notenum;
  //pitch->note_note = note;

  pitch->chance = MAX_PATCHVOICE_CHANCE;
  */
  
  int pos2;
  
  {
    r::PitchTimeData::Writer writer(note->_pitches);
    
    Ratio ratio = ratio_from_place(*place);
    if (!writer.has_element_at_ratio(ratio)) {
      
      writer.add2(r::Pitch(ratio, notenum));

      pos2 = writer.find_element_at_ratio(ratio);
      
    } else {
      
      pos2 = -1;
      
    }

  }

  if(pos2 >= 0)
    if (equal_floats(note->pitch_end, 0.0f))
      note->pitch_end = notenum;

  return pos2;
}

int AddPitch2(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, r::NotePtr &note, const Ratio &ratio, float notenum){
  
  int pos2;
  
  {
    r::PitchTimeData::Writer writer(&note->_pitches);
    
    if (!writer.has_element_at_ratio(ratio)) {
      
      writer.add2(r::Pitch(ratio, notenum));

      pos2 = writer.find_element_at_ratio(ratio);
      
    } else {
      
      pos2 = -1;
      
    }

  }

  if(pos2 >= 0)
    if (equal_floats(note->d._pitch_end, 0.0f))
      note->d._pitch_end = notenum;

  return pos2;
}

void DeletePitch(struct Tracks *track, struct Notes *note, int pitchnum){
  r::PitchTimeData::Writer writer(note->_pitches);
  if (!writer.remove_at_pos(pitchnum)){
    R_ASSERT_NON_RELEASE(false);
  }
}
