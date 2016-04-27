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
#include "list_proc.h"
#include "undo_blocks_proc.h"
#include "temponodes_legalize_proc.h"
#include "visual_proc.h"
#include "Beats_proc.h"

#include "expand_proc.h"


static void expand_place(Place *place, int start, int end, int num_lines){
  if (place->line < start)
    return;

  if (place->line >= end){
    int num_new_lines = num_lines-(end-start);
    place->line += num_new_lines;
    return;
  }

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

static void expand_block(struct Blocks *block, int start, int end, int num_lines){

  expand_list3(&block->signatures->l, start, end, num_lines);
  expand_list3(&block->lpbs->l, start, end, num_lines);
  expand_list3(&block->tempos->l, start, end, num_lines);
  expand_list3(&block->temponodes->l, start, end, num_lines);

  struct Tracks *track = block->tracks;
  while(track!=NULL){
    expand_track(track, start, end, num_lines);
    track=NextTrack(track);
  }

  block->num_lines = block->num_lines - (end-start) + num_lines;

  LegalizeTempoNodes(block);
  UpdateSTimes(block);
  UpdateBeats(block);
}

static void incLocalZoomLine(struct LocalZooms *localzoom, int num_lines){
  if (localzoom!=NULL){
    localzoom->Tline += num_lines;
    incLocalZoomLine(localzoom->uplevel, num_lines);
    incLocalZoomLine(NextLocalZoom(localzoom), num_lines);
  }
}

static void expand_localzooms(struct WBlocks *wblock, int start, int end, int num_lines){
  struct LocalZooms *prev=NULL;
  struct LocalZooms *localzoom=wblock->localzooms;

  while (localzoom->l.p.line < start){
    prev = localzoom;
    localzoom=NextLocalZoom(localzoom);
  }

  struct LocalZooms *localzoom_before = prev;

  while(localzoom!=NULL && localzoom->l.p.line < end)
    localzoom=NextLocalZoom(localzoom);

  // remove all localzooms between start and end
  //
  if(localzoom_before==NULL)
    wblock->localzooms=localzoom;
  else
    localzoom_before->l.next = &localzoom->l;

  // Increase Tline for all localzooms after end
  //
  int num_lines_before = end-start;
  int lines_added = num_lines - num_lines_before;

  incLocalZoomLine(localzoom, lines_added);

  // Insert new localzooms betweeen start and end
  //
  int lokke;
  for(lokke=start;lokke<start+num_lines;lokke++){
    localzoom=talloc(sizeof(struct LocalZooms));
    localzoom->Tline=lokke;
    localzoom->Tdividor=1;
    localzoom->zoomline=lokke;
    ListAddElement3(&wblock->localzooms,&localzoom->l);
  }

}

void EXPAND_Block(struct Tracker_Windows *window, struct WBlocks *wblock, int start, int end, int num_lines){
  expand_block(wblock->block, start, end, num_lines);
  expand_localzooms(wblock, start, end, num_lines);
  UpdateRealLines(window,wblock);
}

void EXPAND_Block_full_control_CurrPos(struct Tracker_Windows *window, struct WBlocks *wblock, int start_line, int end_line, int num_lines){
  PlayStop();

  ADD_UNDO(Block_CurrPos(window));

  EXPAND_Block(window, wblock, start_line, end_line, num_lines);

  window->must_redraw = true;
  wblock->block->is_dirty = true;
}

void EXPAND_Block_CurrPos(struct Tracker_Windows *window, struct WBlocks *wblock, int num_lines){
  EXPAND_Block_full_control_CurrPos(window, wblock, 0, wblock->block->num_lines, num_lines);
}

void EXPAND_Block_from_range_CurrPos(struct Tracker_Windows *window, struct WBlocks *wblock, int num_lines_after){
  if(num_lines_after==0){
    GFX_Message(NULL, "Can not expand down to 0 lines");
    return;
  }
    
  if (wblock->isranged==false){
    GFX_Message(NULL, "No range in block");
    return;
  }

  int realline1 = wblock->rangey1;
  int realline2 = wblock->rangey2;

  int line1 = wblock->reallines[realline1]->l.p.line;
  int line2 = wblock->reallines[realline2]->l.p.line;

  int num_lines_before = line2 - line1 + 1;

  if(num_lines_before==0)
    num_lines_before++;

  printf("line1: %d, line2: %d, num_lines_before: %d\n",line1,line2,num_lines_before);

  EXPAND_Block_full_control_CurrPos(window, wblock, line1, line2, num_lines_after);

  int realline = 0;

  // set new rangey1
  while(wblock->reallines[realline]->l.p.line<line1)
    realline++;
  wblock->rangey1=realline;

  // set new rangey2
  while(wblock->reallines[realline]->l.p.line <= line1+num_lines_after){
    wblock->rangey2=realline;
    realline++;
  }
    

}
