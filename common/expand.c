/* Copyright 2014 Kjetil S. Matheussen

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.
-
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */

#include <string.h>

#include "nsmtracker.h"
#include "player_proc.h"
#include "placement_proc.h"
#include "time_proc.h"
#include "reallines_proc.h"


static void expand_place(Place *place, int start, int end, int num_lines){
  if (place->line < start)
    return;
  if (place->line >= end)
    return;

  const Place place_start   = {start,             0, 1};
  const Place place_end     = {end,               0, 1};
  const Place place_new_end = {start + num_lines, 0, 1};

  Place *new_place = PlaceScale(place, &place_start, &place_end, &place_start, &place_new_end);

  memcpy(place, new_place, sizeof(Place));
}

static void expand_list3(struct ListHeader3 *l, int start, int end, int num_lines){
  while(l!=NULL){
    expand_place(&l->p, start, end, num_lines);
    l=l->next;
  }
}

static void expand_note(struct Notes *note, int start, int end, int num_lines){
  expand_place(&note->l.p, start, end, num_lines);
  expand_place(&note->end, start, end, num_lines);
  expand_list3(&note->velocities->l, start, end, num_lines);
  expand_list3(&note->pitches->l, start, end, num_lines);
}

static void expand_track(struct Tracks *track, int start, int end, int num_lines){

  struct Notes *note = track->notes;
  while(note!=NULL){
    expand_note(note, start, end, num_lines);
    note = NextNote(note);
  }
  
  expand_list3(&track->stops->l, start, end, num_lines);
  
  struct FXs *fxs = track->fxs;
  while(fxs!=NULL){
    expand_list3(&fxs->fxnodelines->l, start, end, num_lines);
    fxs = NextFX(fxs);
  }
}

void EXPAND_Block(struct Blocks *block, int start, int end, int num_lines){
  expand_list3(&block->lpbs->l, start, end, num_lines);
  expand_list3(&block->tempos->l, start, end, num_lines);
  expand_list3(&block->temponodes->l, start, end, num_lines);

  struct Tracks *track = block->tracks;
  while(track!=NULL){
    expand_track(track, start, end, num_lines);
    track=NextTrack(track);
  }

  block->num_lines = num_lines;

  UpdateSTimes(block);
}

void EXPAND_Block_full_control_CurrPos(struct Tracker_Windows *window, struct WBlocks *wblock, int start_line, int end_line, int num_lines){
  PlayStop();

  Undo_Block_CurrPos(window);

  EXPAND_Block(wblock->block, start_line, end_line, num_lines);
  UpdateRealLines(window,wblock);

  window->must_redraw = true;
  wblock->block->is_dirty = true;
}

void EXPAND_Block_CurrPos(struct Tracker_Windows *window, struct WBlocks *wblock, int num_lines){
  EXPAND_Block_full_control_CurrPos(window, wblock, 0, wblock->block->num_lines, num_lines);
}
