/* Copyright 2014 Kjetil S. Matheussen

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

#include <Python.h>


#include "../common/nsmtracker.h"
#include "../common/placement_proc.h"
#include "../common/list_proc.h"
#include "../common/undo_reltemposlider_proc.h"
#include "../common/gfx_wblocks_reltempo_proc.h"
#include "../common/gfx_statusbar_proc.h"
#include "../common/trackreallines_proc.h"
#include "../common/time_proc.h"
#include "../common/common_proc.h"
#include "../common/temponodes_proc.h"
#include "../common/undo_temponodes_proc.h"
#include "../common/realline_calc_proc.h"
#include "../common/notes_proc.h"
#include "../common/pitches_proc.h"
#include "../common/undo_notes_proc.h"

#include "api_common_proc.h"

#include "radium_proc.h"


extern struct Root *root;

extern volatile float scroll_pos;


extern struct ListHeader3 *current_node;

void cancelCurrentNode(void){
  current_node = NULL;
}

static int get_realline_y1(const struct Tracker_Windows *window, int realline){
  return window->fontheight*realline - scroll_pos + window->wblock->t.y1;
}

static int get_realline_y2(const struct Tracker_Windows *window, int realline){
  return window->fontheight*(realline+1) - scroll_pos + window->wblock->t.y1;
}


// placement (block time)

float getPlaceFromY(float y, int blocknum, int windownum) {
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if (wblock==NULL) {
    RError("getPlaceFromY: No block %d in window %d",blocknum,windownum);
    return 0.0;
  }

  struct Blocks *block = wblock->block;
  
  Place *prevplace=PlaceGetFirstPos();
  Place place;
  Place nextplace;
  PlaceSetLastPos(block,&nextplace);
      
  GetReallineAndPlaceFromY(window,
                           wblock,
                           y,
                           &place,
                           prevplace,
                           &nextplace
                           );
  
  return GetFloatFromPlace(&place);
}

static void setPlace(struct Blocks *block, Place *prevplace, struct ListHeader3 *node, float floatplace){

  if (prevplace==NULL)
    prevplace=PlaceGetFirstPos();

  // find next place
  
  Place temp;
  Place *nextplace;
  bool nextplace_is_lastplace = false;
  struct ListHeader3 *next = node->next;

  if(next==NULL) {
    PlaceSetLastPos(block,&temp);
    nextplace = &temp;
    nextplace_is_lastplace = true;
  }else
    nextplace = &next->p;


  // legalize place
  
  Place place;
  Float2Placement(floatplace, &place);

  if (PlaceLessOrEqual(&place, prevplace))
    PlaceFromLimit(&place, prevplace);

  if (nextplace_is_lastplace) {
    if (PlaceGreaterThan(&place, nextplace))
      PlaceCopy(&place, nextplace);
  } else {
    if (PlaceGreaterOrEqual(&place, nextplace))
      PlaceTilLimit(&place, nextplace);
  }

  
  // set place
  PlaceCopy(&node->p, &place);
}

static struct ListHeader3 *setPlaceFromNum(struct Blocks *block, struct ListHeader3 *list, int num, float floatplace){
  struct ListHeader3 *node;

  // Find prevplace and node
  
  Place *prevplace;
  
  if (num==0) {
    prevplace=NULL;
    node = list;
  } else {
    struct ListHeader3 *prevnode = ListFindElement3_num(list, num-1);
    if (prevnode==NULL)
      return NULL;
    prevplace = &prevnode->p;
    node = prevnode->next;
  }


  if (node==NULL)
    return NULL;


  setPlace(block, prevplace, node, floatplace);

  
  return node;
}






// reltempo

int getReltempoSliderX1(void){
  return root->song->tracker_windows->wblock->reltempo.x1;
}
int getReltempoSliderY1(void){
  return root->song->tracker_windows->wblock->reltempo.y1;
}
int getReltempoSliderX2(void){
  return root->song->tracker_windows->wblock->reltempo.x2;
}
int getReltempoSliderY2(void){
return root->song->tracker_windows->wblock->reltempo.y2;
}

float getReltempo(int blocknum, int windownum){
  struct WBlocks *wblock = getWBlockFromNum(windownum, blocknum);
  if (wblock==NULL)
    return 0.0f;
  else
    return wblock->block->reltempo;
}

void undoReltempo(void){
  struct Tracker_Windows *window = root->song->tracker_windows;
  Undo_RelTempoSlider(window,window->wblock);
}

static void update_statusbar(struct Tracker_Windows *window){
  struct WBlocks *wblock = window->wblock;
  GFX_SetChangeInt(window,wblock,"Block RelTempo 0.001*",(int)(wblock->block->reltempo*1000));
  GFX_DrawStatusBar(window,wblock);
}

void setReltempo(float reltempo){
  struct Tracker_Windows *window = root->song->tracker_windows;
  struct WBlocks *wblock = window->wblock;
  wblock->block->reltempo=R_BOUNDARIES(
    MINBLOCKRELTIME,
    reltempo,
    MAXBLOCKRELTIME
  );

  update_statusbar(window);
  DrawBlockRelTempo(window,wblock);

  wblock->block->is_dirty = true;
}

void showReltempoInStatusbar(void){
  struct Tracker_Windows *window = root->song->tracker_windows;
  update_statusbar(window);
}

float getMinReltempo(void){
  return MINBLOCKRELTIME;
}

float getMaxReltempo(void){
  return MAXBLOCKRELTIME;
}



// tracks positions
///////////////////////////////////////////////////

float getTrackX1(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->x;
}

float getTrackY1(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->y;
}

float getTrackX2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->x2;
}

float getTrackY2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->y2;
}

float getTrackNotesX1(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->notearea.x;
}

float getTrackNotesY1(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->y;
}

float getTrackNotesX2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->notearea.x2;
}

float getTrackNotesY2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->y2;
}


// temponodearea
//////////////////////////////////////////////////

int getTemponodeAreaX1(void){
  return root->song->tracker_windows->wblock->temponodearea.x;
}
int getTemponodeAreaY1(void){
  return root->song->tracker_windows->wblock->t.y1;
}
int getTemponodeAreaX2(void){
  return root->song->tracker_windows->wblock->temponodearea.x2;
}
int getTemponodeAreaY2(void){
  return root->song->tracker_windows->wblock->t.y2;
}

float getTemponodeMax(int blocknum, int windownum){
  struct WBlocks *wblock = getWBlockFromNum(windownum, blocknum);
  if (wblock==NULL)
    return 0.0f;
  else
    return wblock->reltempomax;
}

static struct Node *get_temponodeline(int boxnum){
  vector_t *nodes = root->song->tracker_windows->wblock->reltempo_nodes;
  if (boxnum < 0 || boxnum>=nodes->num_elements) {
    RError("There is no temponode %d",boxnum);
    return NULL;
  }else
    return nodes->elements[boxnum];
}

float getTemponodeX(int num){
  struct Node *nodeline = get_temponodeline(num);
  return nodeline==NULL ? 0 : nodeline->x;
}

float getTemponodeY(int num){
  struct Node *nodeline = get_temponodeline(num);
  return nodeline==NULL ? 0 : nodeline->y-scroll_pos;
}

float getTemponodeValue(int num, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if (wblock==NULL) {
    RError("getTemponodeValue: No block %d in window %d",blocknum,windownum);
    return 0.0;
  }

  struct Blocks *block = wblock->block;
  struct TempoNodes *temponode = ListFindElement3_num(&block->temponodes->l, num);
  if (temponode==NULL) {
    RError("No temponode %d in block %d%s",num,blocknum,blocknum==-1?" (i.e. current block)":"");
    return 0.0;
  }

  return temponode->reltempo;
}

void undoTemponodes(void){
  struct Tracker_Windows *window = root->song->tracker_windows;
  Undo_TempoNodes_CurrPos(window);
}

void setCurrentTempoNode(int num, int blocknum){
  struct Blocks *block = blocknum==-1 ? root->song->tracker_windows->wblock->block : getBlockFromNum(blocknum);
  if (block==NULL) {
    RError("setCurrentTemponode: No block %d",blocknum);
    return;
  }
  
  struct TempoNodes *temponode = ListFindElement3_num(&block->temponodes->l, num);

  if (current_node != &temponode->l) {
    current_node = &temponode->l;
    block->is_dirty = true;
  }
}

void setTemponode(int num, float value, float place, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if (wblock==NULL) {
    RError("setTemponode: No block %d in window %d",blocknum,windownum);
    return;
  }
  
  struct Blocks *block = wblock->block;

  struct TempoNodes *temponode;
  if (num==0)
    temponode = block->temponodes; // don't want to set placement for the first node. It's always at top.
  else if (num==wblock->reltempo_nodes->num_elements-1)
    temponode = ListLast3(&block->temponodes->l); // don't want to set placement for the last node. It's always at bottom.
  else
    temponode = (struct TempoNodes *)setPlaceFromNum(block, &block->temponodes->l, num, place);

  current_node = &temponode->l;
    
  if (temponode==NULL){

    RError("No temponode %d in block %d%s",num,blocknum,blocknum==-1?" (i.e. current block)":"");

  } else {

    if ( (value+1) > wblock->reltempomax) {
      wblock->reltempomax = value+1;      
    } else if ( (value-1) < -wblock->reltempomax) {
      wblock->reltempomax = -1*(value -1);
    }

    temponode->reltempo = value;

    UpdateSTimes(wblock->block);    
    UpdateAllTrackReallines(window,wblock);
  
    block->is_dirty = true;

  }
}

float getTemponodeWidth(void){
  return root->song->tracker_windows->fontheight;
}

int getNumTemponodes(int blocknum, int windownum){
  struct WBlocks *wblock = getWBlockFromNum(windownum, blocknum);
  if (wblock==NULL)
    return 0.0f;
  else
    return wblock->reltempo_nodes->num_elements;
}

void deleteTemponode(int num, int blocknum){
  
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(-1, &window, blocknum);
  if (wblock==NULL) {
    RError("deleteTemponode: No block %d",blocknum);
    return;
  }

  if (num >= wblock->reltempo_nodes->num_elements){
    RError("deleteTemponode: No temponode %d in block %d",num,blocknum);
    return;
  }

  if (num==0){
    wblock->block->temponodes->reltempo = 0.0f;
  } else if (num==wblock->reltempo_nodes->num_elements-1) {
    struct TempoNodes *last = ListLast3(&wblock->block->temponodes->l);
    last->reltempo = 0.0f;
  } else {
    ListRemoveElement3_fromNum(&wblock->block->temponodes,num);
  }

  wblock->block->is_dirty = true;
}

int createTemponode(float value, float floatplace, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if (wblock==NULL) {
    RError("createTemponode: No block %d in window %d",blocknum,windownum);
    return -1;
  }

  struct Blocks *block = wblock->block;
  
  if (floatplace<=0.0f)
    return -1;

  if (floatplace>=block->num_lines)
    return -1;
  
  Place place;
  Float2Placement(floatplace, &place);

  if ( (value+1) > wblock->reltempomax) {
    wblock->reltempomax = value+1;      
  } else if ( (value-1) < -wblock->reltempomax) {
    wblock->reltempomax = -1*(value -1);
  }
  
  struct TempoNodes *temponode = AddTempoNode(window,wblock,&place,value);
  current_node = &temponode->l;

  if (temponode==NULL)
    return -1;
  
  //GFX_SetChangeFloat(window,wblock,"Reltempo",RelTempo2RealRelTempo(Gfx2RelTempo(wblock,dx)));
  //UpdateSTimes(wblock->block);
  //GFX_DrawStatusBar(window,wblock);

  UpdateSTimes(block);
  UpdateAllTrackReallines(window,wblock);

  block->is_dirty = true;

  return ListFindElementPos3(&block->temponodes->l, &temponode->l);
}


// pitches
//////////////////////////////////////////////////

static int getPitchNum(struct Tracks *track, struct Notes *note, struct Pitches *pitch){
  int num = 0;
  struct Notes *note2 = track->notes;
  
  while(note2!=NULL){

    if (note==note2 && pitch==NULL)
      return num;

    num++;
    
    struct Pitches *pitch2 = note2->pitches;
    while(pitch2!=NULL){
      if (note==note && pitch==pitch2)
        return num;

      num++;

      pitch2 = NextPitch(pitch2);
    }

    if (note==note2) {
      RError("getPitchNum: Could not find pitch in note.");
      return 0;
    }

    note2 = NextNote(note2);
  }

  RError("getPitchNum: Could not find it");
  return 0;
}

int getNumPitches(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(-1, blocknum, tracknum);

  if (wtrack==NULL)
    return 0;

  struct Tracks *track = wtrack->track;
  
  int num = 0;
  struct Notes *notes = track->notes;
  
  while(notes!=NULL){

    num++;
    
    struct Pitches *pitches = notes->pitches;
    while(pitches!=NULL){
      num++;
      pitches = NextPitch(pitches);
    }
      
    notes = NextNote(notes);
  }

  return num;
}

void deletePitch(int pitchnum, int tracknum, int blocknum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(-1, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;

  struct Tracks *track = wtrack->track;
  
  int num = 0;
  struct Notes *notes = track->notes;
  
  while(notes!=NULL){

    if (pitchnum==num) {
      RemoveNote(wblock->block, track, notes);
      goto gotit;
    }
    
    num++;
    
    struct Pitches *pitches = notes->pitches;
    while(pitches!=NULL){
      if (pitchnum==num){
        ListRemoveElement3(&notes->pitches,&pitches->l);
        goto gotit;
      }
      
      num++;
      pitches = NextPitch(pitches);
    }
      
    notes = NextNote(notes);
  }

  RError("no pitch %d in track %d in block %d\n",pitchnum,tracknum,blocknum);
  return;
  
 gotit:
  UpdateTrackReallines(window,wblock,wtrack);
  wblock->block->is_dirty = true;
}



static bool getPitch(int pitchnum, struct Pitches **pitch, struct Notes **note, struct Tracks *track){
  int num = 0;
  struct Notes *notes = track->notes;
  while(notes!=NULL){

    if(num==pitchnum) {
      *note = notes;
      *pitch = NULL;
      return true;
    }

    num++;
    
    struct Pitches *pitches = notes->pitches;
    while(pitches!=NULL){
      if(num==pitchnum) {
        *note = notes;
        *pitch = pitches;
        return true;
      }

      num++;
      pitches = NextPitch(pitches);
    }
      
    notes = NextNote(notes);
  }

  RError("Pitch #%d in track #%d does not exist",pitchnum,track->l.num);
  return false;
}
  

static int getReallineForPitch(struct WBlocks *wblock, struct Pitches *pitch, struct Notes *note){
  if( pitch!=NULL)
    return FindRealLineFor(wblock,pitch->Tline,&pitch->l.p);
  else
    return FindRealLineFor(wblock,note->Tline,&note->l.p);
}

static float getPitchInfo(int what_to_get, int pitchnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);

  if (wtrack==NULL)
    return 0;

  struct Notes *note;
  struct Pitches *pitch;

  if (getPitch(pitchnum, &pitch, &note, wtrack->track)==false)
    return 0;
  
  switch (what_to_get){
  case 0:
    return get_realline_y1(window, getReallineForPitch(wblock, pitch, note));
  case 1:
    return get_realline_y2(window, getReallineForPitch(wblock, pitch, note));
  case 2:
    {
      if (pitch!=NULL)
        return pitch->note;
      else
        return note->note;
    }
  }

  RError("internal error (getPitchInfo)\n");
  return 0;
}

float getPitchY1(int pitchnum, int tracknum, int blocknum, int windownum){
  return getPitchInfo(0, pitchnum, tracknum, blocknum, windownum);
}

float getPitchY2(int pitchnum, int tracknum, int blocknum, int windownum){
  return getPitchInfo(1, pitchnum, tracknum, blocknum, windownum);
}

float getPitchX1(int pitchnum, int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0.0f : wtrack->notearea.x;
}

float getPitchX2(int pitchnum, int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0.0f : wtrack->notearea.x2;
}

float getPitchValue(int pitchnum, int tracknum, int blocknum, int windownum){
  float pitch = getPitchInfo(2, pitchnum, tracknum, blocknum, windownum);
  if (pitch>=NOTE_PITCH_START) // not sure I like this hack very much.
    pitch -= NOTE_PITCH_START;
  return pitch;
}

void undoPitches(int tracknum, int blocknum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(-1, &window, blocknum, &wblock, tracknum);
  Undo_Notes(window,window->wblock->block,wtrack->track,window->wblock->curr_realline);
}

static Place *getPrevLegalNotePlace(struct Tracks *track, struct Notes *note){
  Place *end = PlaceGetFirstPos(); // small bug here, cant move pitch to first position, only almost to first position.

  struct Notes *prev = ListPrevElement3(&track->notes->l, &note->l);

  if (prev != NULL) {
    end = &prev->l.p;
    if (prev->velocities!=NULL)
      end = ListLastPlace3(&prev->velocities->l);
    if (prev->pitches!=NULL)
      end = PlaceMax(end, ListLastPlace3(&prev->pitches->l));
  }
  
  return end;
}

static Place *getNextLegalNotePlace(struct Notes *note){
  Place *end = &note->end;

  if (note->velocities != NULL)
    end = PlaceMin(end, &note->velocities->l.p);

  if (note->pitches != NULL)
    end = PlaceMin(end, &note->pitches->l.p);

  return end;
}

static Place *getPrevLegalPitchPlace(struct Notes *note, struct Pitches *pitch){
  Place *end = &note->l.p;

  struct Pitches *prev = ListPrevElement3(&note->pitches->l, &pitch->l);

  if (prev != NULL)
    end = PlaceMax(end, &prev->l.p);

  return end;
}

static Place *getNextLegalPitchPlace(struct Notes *note, struct Pitches *pitch){
  Place *end = &note->end;

  if (NextPitch(pitch) != NULL)
    end = PlaceMin(end, &NextPitch(pitch)->l.p);

  return end;
}


void setPitch(int num, float value, float floatplace, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);

  if (wtrack==NULL)
    return;
  
  struct Blocks *block = wblock->block;

  value = R_BOUNDARIES(1,value,127);

  Place place;
  Float2Placement(floatplace, &place);

  struct Notes *note;
  struct Pitches *pitch;

  if (getPitch(num, &pitch, &note, wtrack->track)==false)
    return;

  if (pitch != NULL) {
    
    pitch->note = value;
    pitch->l.p = *PlaceBetween(getPrevLegalPitchPlace(note, pitch), &place, getNextLegalPitchPlace(note, pitch));
    
  } else {
    
    note->note = value;    
    Place old_place = note->l.p;
    
    if (PlaceLessThan(&place, &old_place)) {
      Place *prev_legal = getPrevLegalNotePlace(wtrack->track, note);
      if (PlaceLessOrEqual(&place, prev_legal))
        PlaceFromLimit(&place, prev_legal);
    } else {
      Place *next_legal = getNextLegalNotePlace(note);
      if (PlaceGreaterOrEqual(&place, next_legal))
        PlaceTilLimit(&place, next_legal);
    }

    note->l.p = place;
    ReplaceNoteEnds(block, wtrack->track, &old_place, &place);
    
  }

  UpdateTrackReallines(window,wblock,wtrack);
  block->is_dirty = true;
}

static struct Notes *getNoteAtPlace(struct Tracks *track, Place *place){
  struct Notes *note = track->notes;

  while(note != NULL){
    if (PlaceIsBetween3(place, &note->l.p, &note->end))
      return note;
    else
      note = NextNote(note);
  }

  return NULL;
}

static int addNote2(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, Place *place, float value){

  printf("adding NOTE. num before: %d\n",getNumPitches(wtrack->l.num, wblock->l.num, window->l.num));

  struct Notes *note = InsertNote(wblock, wtrack, place, NULL, value, NOTE_get_velocity(wtrack->track), 0);

  UpdateTrackReallines(window,wblock,wtrack);
  wblock->block->is_dirty = true;
             
  return getPitchNum(wtrack->track, note, NULL);
}

static int addPitch(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, struct Notes *note, Place *place, float value){

  printf("adding pitch. num before: %d\n",getNumPitches(wtrack->l.num, wblock->l.num, window->l.num));

  struct Pitches *pitch = AddPitch(window, wblock, wtrack, note, place, note->note);

  printf("num after: %d\n\n",getNumPitches(wtrack->l.num, wblock->l.num, window->l.num));

  UpdateTrackReallines(window,wblock,wtrack);
  wblock->block->is_dirty = true;

  return getPitchNum(wtrack->track, note, pitch);
}

int createPitch(float value, float floatplace, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);

  if (wtrack==NULL)
    return 0;

  Place place;
  Float2Placement(floatplace, &place);
  
  struct Notes *note = getNoteAtPlace(wtrack->track, &place);

  if(note==NULL)
    return addNote2(window, wblock, wtrack, &place, value);
  else
    return addPitch(window, wblock, wtrack, note, &place, value);
}
  

// ctrl / shift keys
//////////////////////////////////////////////////

extern struct TEvent tevent;

bool ctrlPressed(void){
  return AnyCtrl(tevent.keyswitch);
}

bool shiftPressed(void){
  return AnyShift(tevent.keyswitch);
}

