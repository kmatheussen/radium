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
#include "list_proc.h"
#include "placement_proc.h"
#include "notes_proc.h"
#include "player_pause_proc.h"
#include "player_proc.h"
#include "visual_proc.h"
#include "OS_Player_proc.h"

#include "pitches_proc.h"


struct Pitches *AddPitch(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, struct Notes *note, Place *place, float notenum){
  struct Pitches *pitch = talloc(sizeof(struct Pitches));
  PlaceCopy(&pitch->l.p,place);
  pitch->note = notenum;
  //pitch->note_note = note;

  int pos;
  PC_Pause(); {
    pos=ListAddElement3_ns(&note->pitches, &pitch->l);
  }PC_StopPause();

  if(pos==-1)
    return NULL;
  
#if !USE_OPENGL
  ClearTrack(window,wblock,wtrack,wblock->top_realline,wblock->bot_realline);
  UpdateWTrack(window,wblock,wtrack,wblock->top_realline,wblock->bot_realline);
#endif
  return pitch;
}

void DeletePitch(struct Tracks *track, struct Notes *note, struct Pitches *pitch){
  PLAYER_lock();{
    ListRemoveElement3(&note->pitches, &pitch->l);
  }PLAYER_unlock();
}
