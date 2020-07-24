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
#include "player_pause_proc.h"
#include "placement_proc.h"
#include "time_proc.h"
#include "reallines_proc.h"
#include "list_proc.h"
#include "vector_proc.h"
#include "undo_blocks_proc.h"
#include "temponodes_legalize_proc.h"
#include "visual_proc.h"
#include "Beats_proc.h"
#include "sequencer_proc.h"

#include "expand_proc.h"


static Place expand_place(const Place place, const Place start, const Place end, const Place new_end, const Place last_place){
  if (p_Less_Or_Equal(place, start))
    return place;

  Place ret;

  if (p_Greater_Or_Equal(place, end)){

    if (p_Less_Than(new_end, end))
      ret = p_Sub(place, p_Sub(end, new_end));
    else
      ret = p_Add(place, p_Sub(new_end, end));

  } else {

    ret = p_Scale(place, start, end, start, new_end);

  }

  if (p_Greater_Than(ret, last_place)){
    R_ASSERT_NON_RELEASE(false);
    ret = last_place;
  }

  return ret;
}

static void expand_list3(struct ListHeader3 *l, const Place start, const Place end, const Place new_end, const Place last_place){
  while(l!=NULL){
    if (p_Greater_Or_Equal(l->p, start)) {
      l->p = expand_place(l->p, start, end, new_end, last_place);
    }
    l=l->next;
  }
}

static void expand_note(struct Notes *note, const Place start, const Place end, const Place new_end, const Place last_place){
  note->l.p = expand_place(note->l.p, start, end, new_end, last_place);
  note->end = expand_place(note->end, start, end, new_end, last_place);
  if(note->velocities!=NULL)
    expand_list3(&note->velocities->l, start, end, new_end, last_place);
  if(note->pitches!=NULL)
    expand_list3(&note->pitches->l, start, end, new_end, last_place);
}

static void expand_track(struct Tracks *track, const Place start, const Place end, const Place new_end, const Place last_place){

  struct Notes *note = track->notes;
  while(note!=NULL){
    expand_note(note, start, end, new_end, last_place);
    note = NextNote(note);
  }

  if(track->swings!=NULL)
    expand_list3(&track->swings->l, start, end, new_end, last_place);

  if(track->stops!=NULL)
    expand_list3(&track->stops->l, start, end, new_end, last_place);

  VECTOR_FOR_EACH(struct FXs *fxs, &track->fxs){
    if(fxs->fxnodelines!=NULL)
      expand_list3(&fxs->fxnodelines->l, start, end, new_end, last_place);
  }END_VECTOR_FOR_EACH;
}

static void expand_block(struct WBlocks *wblock, struct Blocks *block, const Place start, const Place end, const Place new_end){

  if (p_Equal(end, new_end))
    return;

  // Set new block->num_lines value.
  {
    Place last_place = p_Create(block->num_lines, 0, 1);
    if (p_Less_Than(new_end, end))
      last_place = p_Sub(last_place, p_Sub(end, new_end));
    else
      last_place = p_Add(last_place, p_Sub(new_end, end));
    
    if (last_place.counter==0)
      block->num_lines = last_place.line;
    else
      block->num_lines = last_place.line + 1;
  }

  const Place last_place = p_Last_Pos(block);

  if (p_Greater_Than(start, last_place)){
    R_ASSERT(false);
    return;
  }

  if(block->swings!=NULL)
    expand_list3(&block->swings->l, start, end, new_end, last_place);
  
  if(block->signatures!=NULL)
    expand_list3(&block->signatures->l, start, end, new_end, last_place);
  
  if(block->lpbs!=NULL)
    expand_list3(&block->lpbs->l, start, end, new_end, last_place);
  
  if(block->tempos!=NULL)
    expand_list3(&block->tempos->l, start, end, new_end, last_place);
  
  expand_list3(&block->temponodes->l, start, end, new_end, last_place);

  wblock->range.y1 = expand_place(wblock->range.y1, start, end, new_end, last_place);
  wblock->range.y2 = expand_place(wblock->range.y2, start, end, new_end, p_Create(wblock->block->num_lines, 0, 1));

  struct Tracks *track = block->tracks;
  while(track!=NULL){
    expand_track(track, start, end, new_end, last_place);
    track=NextTrack(track);
  }

  LegalizeTempoNodes(block);

  TIME_everything_in_block_has_changed(block);
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

void EXPAND_Block(struct Tracker_Windows *window, struct WBlocks *wblock, const Place start, const Place end, const Place new_end){
  expand_block(wblock, wblock->block, start, end, new_end);

  expand_localzooms(wblock, start.line, end.line, p_Sub(new_end, start).line);

  UpdateRealLines(window,wblock);
}

void EXPAND_Block2(struct Tracker_Windows *window, struct WBlocks *wblock, int new_num_lines){
  const Place start = p_Create(0,0,1);
  const Place end = p_Create(wblock->block->num_lines, 0, 1);
  const Place new_end = p_Create(new_num_lines, 0, 1);
  EXPAND_Block(window, wblock, start, end, new_end);
}

void EXPAND_Block_full_control_CurrPos(struct Tracker_Windows *window, struct WBlocks *wblock, const Place start, const Place end, const Place new_end){

  PC_Pause();{
    ADD_UNDO(Block_CurrPos(window));
    EXPAND_Block(window, wblock, start, end, new_end);
  }PC_StopPause(NULL);
  
  window->must_redraw = true;
  wblock->block->is_dirty = true;
}

void EXPAND_Block_CurrPos(struct Tracker_Windows *window, struct WBlocks *wblock, int num_lines){
  const Place start = p_Create(0,0,1);
  const Place end = p_Create(wblock->block->num_lines, 0, 1);
  const Place new_end = p_Create(num_lines, 0, 1);
  EXPAND_Block_full_control_CurrPos(window, wblock, start, end, new_end);
}

void EXPAND_Block_from_range_CurrPos(struct Tracker_Windows *window, struct WBlocks *wblock, const Place range_duration_after){
  if (wblock->range.enabled==false){
    GFX_Message2(NULL, true,"No range in block");
    return;
  }

  if(p_Less_Or_Equal(range_duration_after, p_Create(0,0,1))){
    GFX_Message2(NULL, true,"Can not expand down to 0 lines");
    return;
  }

  Place start = wblock->range.y1;
  Place end = wblock->range.y2;
  Place new_end = p_Add(start, range_duration_after);

  //printf("line1: %d, line2: %d, num_lines_before: %d\n",line1,line2,num_lines_before);

  EXPAND_Block_full_control_CurrPos(window, wblock, start, end, new_end);
}
