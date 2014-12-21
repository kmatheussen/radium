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
#include "../common/vector_proc.h"
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
#include "../common/gfx_subtrack_proc.h"
#include "../common/velocities_proc.h"
#include "../common/visual_proc.h"
#include "../common/undo_fxs_proc.h"
#include "../common/fxlines_proc.h"
#include "../common/nodelines_proc.h"
#include "../common/instruments_proc.h"
#include "../common/wblocks_proc.h"
#include "../common/OS_Player_proc.h"
#include "../common/player_proc.h"
#include "../common/undo_trackheader_proc.h"

#include "../OpenGL/Render_proc.h"

#include "api_common_proc.h"

#include "radium_proc.h"


extern struct Root *root;

extern volatile float scroll_pos;


extern const struct ListHeader3 *current_node;
extern const struct ListHeader3 *indicator_node;
extern int indicator_velocity_num;
extern int indicator_pitch_num;


// various
///////////////////////////////////////////////////

static void setCurrentNode(struct ListHeader3 *new_current_node){
  if (current_node != new_current_node){
    current_node = new_current_node;
    root->song->tracker_windows->must_redraw = true;
    //printf("current node dirty\n");
  }
}

void cancelCurrentNode(void){
  setCurrentNode(NULL);
}

static void setIndicatorNode(const struct ListHeader3 *new_indicator_node){
  if (indicator_node != new_indicator_node){
    indicator_node = new_indicator_node;
    root->song->tracker_windows->must_redraw = true;
    //printf("indicator node dirty\n");
  }
}

void cancelIndicatorNode(void){
  setIndicatorNode(NULL);
  indicator_velocity_num = -1;
  indicator_pitch_num = -1;
}


float getHalfOfNodeWidth(void){
  return root->song->tracker_windows->fontheight / 1.5; // if changing 1.5 here, also change 1.5 in Render.cpp
}

static float get_mouse_realline_y1(const struct Tracker_Windows *window, int realline){
  //printf("fontheight: %f\n",(float)window->fontheight);
  //printf("wblock->t.y1: %f. scroll_pos: %f\n",(float)window->wblock->t.y1,(float)scroll_pos);
  return window->fontheight*realline - scroll_pos + window->wblock->t.y1;
}

static float get_mouse_realline_y2(const struct Tracker_Windows *window, int realline){
  return window->fontheight*(realline+1) - scroll_pos + window->wblock->t.y1;
}

float getTopVisibleY(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if(wblock==NULL)
    return 0;

  return get_mouse_realline_y1(window, R_MAX(0, wblock->top_realline));
}

float getBotVisibleY(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if(wblock==NULL)
    return 0;

  return get_mouse_realline_y2(window, R_MIN(wblock->num_reallines-1, wblock->bot_realline));
}

void setMouseTrack(int tracknum){
  struct Tracker_Windows *window = root->song->tracker_windows;
  struct WBlocks *wblock = window->wblock;

  if(tracknum != wblock->mouse_track){
    wblock->mouse_track = tracknum;
    window->must_redraw = true;
  }
}

void setNoMouseTrack(void){
  setMouseTrack(NOTRACK);
}

void setMouseTrackToReltempo(void){
  setMouseTrack(TEMPONODETRACK);
}


// placement (block time)
///////////////////////////////////////////////////

float getPlaceFromY(float y, int blocknum, int windownum) {
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if (wblock==NULL)
    return 0.0;

  Place place;

  GetReallineAndPlaceFromY(window,
                           wblock,
                           y,
                           &place,
                           NULL,
                           NULL
                           );
  
  return GetFloatFromPlace(&place);
}


static double get_gridded_abs_y(struct Tracker_Windows *window, float abs_y){
  double grid = (double)root->grid_numerator / (double)root->grid_denominator;

  float abs_realline = abs_y / window->fontheight;
  
  double rounded = round(abs_realline / grid);
    
  return rounded * grid * window->fontheight;
}

float getPlaceInGridFromY(float y, int blocknum, int windownum) {
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if (wblock==NULL)
    return 0.0;

  // This is a hack, but it's a relatively clean one, and modifying GetReallineAndPlaceFromY is a quite major work (old code, hard to understand, it should be rewritten).
  float abs_y = (y-wblock->t.y1) + wblock->top_realline*window->fontheight;
  float gridded_abs_y = get_gridded_abs_y(window, abs_y);
  float gridded_y = gridded_abs_y - wblock->top_realline*window->fontheight + wblock->t.y1;
  
  Place place;
  
  GetReallineAndPlaceFromY(window,
                           wblock,
                           gridded_y,
                           &place,
                           NULL,
                           NULL
                           );
  
  return GetFloatFromPlace(&place);
}




// reltempo
///////////////////////////////////////////////////

float getReltempoSliderX1(void){
  return root->song->tracker_windows->wblock->reltempo.x1;
}
float getReltempoSliderY1(void){
  return root->song->tracker_windows->wblock->reltempo.y1;
}
float getReltempoSliderX2(void){
  return root->song->tracker_windows->wblock->reltempo.x2;
}
float getReltempoSliderY2(void){
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

void setReltempo(float reltempo){
  struct Tracker_Windows *window = root->song->tracker_windows;
  struct WBlocks *wblock = window->wblock;
  wblock->block->reltempo=R_BOUNDARIES(
    MINBLOCKRELTIME,
    reltempo,
    MAXBLOCKRELTIME
  );

  //update_statusbar(window);
  DrawBlockRelTempo(window,wblock);

  window->must_redraw = true;
}


float getMinReltempo(void){
  return MINBLOCKRELTIME;
}

float getMaxReltempo(void){
  return MAXBLOCKRELTIME;
}



// track panning slider
///////////////////////////////////////////////////

float getTrackPanSliderX1(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0.0f;

  return wtrack->pan.x1;
}
float getTrackPanSliderY1(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0.0f;

  return wtrack->pan.y1;
}
float getTrackPanSliderX2(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0.0f;

  return wtrack->pan.x2;
}
float getTrackPanSliderY2(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0.0f;

  return wtrack->pan.y2;
}

float getTrackPan(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0.0f;

  return scale(wtrack->track->pan, -MAXTRACKPAN, MAXTRACKPAN, -1.0, 1.0);
}

void undoTrackPan(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;

  Undo_TrackHeader(window, wblock->block, wtrack->track, wblock->curr_realline);
}

// void setTrackPan(float pan, int tracknum, int blocknum, int windownum)
// was already (more correctly) implemented in api_various.c



// track volumening slider
///////////////////////////////////////////////////

float getTrackVolumeSliderX1(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0.0f;

  return wtrack->volume.x1;
}
float getTrackVolumeSliderY1(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0.0f;

  return wtrack->volume.y1;
}
float getTrackVolumeSliderX2(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0.0f;

  return wtrack->volume.x2;
}
float getTrackVolumeSliderY2(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0.0f;

  return wtrack->volume.y2;
}

float getTrackVolume(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0.0f;

  return scale(wtrack->track->volume, 0, MAXTRACKVOL, 0.0, 1.0);
}

void undoTrackVolume(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;

  Undo_TrackHeader(window, wblock->block, wtrack->track, wblock->curr_realline);
}

// void setTrackVolume(float volume, int tracknum, int blocknum, int windownum)
// was already (more correctly) implemented in api_various.c



// block positions
///////////////////////////////////////////////////

float getBlockHeaderY2(int blocknum, int windownum){
  struct WBlocks *wblock = getWBlockFromNum(windownum, blocknum);
  return wblock==NULL ? 0 : wblock->t.y1;
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
  return getBlockHeaderY2(blocknum, windownum);
}

float getTrackNotesX2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->notearea.x2;
}

float getTrackNotesY2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->y2;
}

float getTrackFxX1(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->fxarea.x;
}

float getTrackFxY1(int tracknum, int blocknum, int windownum){
  return getBlockHeaderY2(blocknum, windownum);
}

float getTrackFxX2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->fxarea.x2;
}

float getTrackFxY2(int tracknum, int blocknum, int windownum){
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

static const struct Node *get_temponode(int boxnum){
  return VECTOR_get(GetTempoNodes(root->song->tracker_windows, root->song->tracker_windows->wblock),
                    boxnum,
                    "temponode"
                    );
}

float getTemponodeX(int num){
  const struct Node *nodeline = get_temponode(num);
  return nodeline==NULL ? 0 : nodeline->x;
}

float getTemponodeY(int num){
  const struct Node *nodeline = get_temponode(num);
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

void setCurrentTemponode(int num, int blocknum){
  struct Blocks *block = blocknum==-1 ? root->song->tracker_windows->wblock->block : getBlockFromNum(blocknum);
  if (block==NULL) {
    RError("setCurrentTemponode: No block %d",blocknum);
    return;
  }
  
  struct TempoNodes *temponode = ListFindElement3_num(&block->temponodes->l, num);

  setCurrentNode(&temponode->l);
}

void setIndicatorTemponode(int num, int blocknum){
  struct Blocks *block = blocknum==-1 ? root->song->tracker_windows->wblock->block : getBlockFromNum(blocknum);
  if (block==NULL) {
    RError("setCurrentTemponode: No block %d",blocknum);
    return;
  }
  
  struct TempoNodes *temponode = ListFindElement3_num(&block->temponodes->l, num);

  setIndicatorNode(&temponode->l);
}

void setTemponode(int num, float value, float floatplace, int blocknum, int windownum){

  PlayStop();

  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if (wblock==NULL) {
    RError("setTemponode: No block %d in window %d",blocknum,windownum);
    return;
  }

  //printf("Set temponode. value: %f. Place: %f\n",value,floatplace);
  
  struct Blocks *block = wblock->block;

  struct TempoNodes *temponode;

  const vector_t *tempo_nodes = GetTempoNodes(window, wblock);
  
  if (num==0)
    temponode = block->temponodes; // don't want to set placement for the first node. It's always at top.
  
  else if (num==tempo_nodes->num_elements-1)
    temponode = ListLast3(&block->temponodes->l); // don't want to set placement for the last node. It's always at bottom.

  else if (num>=tempo_nodes->num_elements) {
    RError("No temponode %d in block %d%s",num,blocknum,blocknum==-1?" (i.e. current block)":"");
    return;
    
  } else if (floatplace < 0) {
    temponode = ListFindElement3_num(&block->temponodes->l, num);

  } else {
    Place place;
    Float2Placement(floatplace, &place);
    temponode = (struct TempoNodes *)ListMoveElement3_FromNum_ns(&block->temponodes, num, &place, NULL, NULL);
  }
  
  if ( (value+1) > wblock->reltempomax) {
    wblock->reltempomax = value+1;      
  } else if ( (value-1) < -wblock->reltempomax) {
    wblock->reltempomax = -1*(value -1);
  }

  temponode->reltempo = value;

  UpdateSTimes(wblock->block);    

  UpdateAllTrackReallines(window,wblock); // sure?

  //printf("before: %f, now: %f\n",floatplace, GetfloatFromPlace(&temponode->l.p));

  window->must_redraw = true;
}

int getNumTemponodes(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if (wblock==NULL)
    return 0;
  
  const vector_t *tempo_nodes = GetTempoNodes(window, wblock);
  return tempo_nodes->num_elements;
}

void deleteTemponode(int num, int blocknum){
  PlayStop();

  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(-1, &window, blocknum);
  if (wblock==NULL)
    return;

  const vector_t *tempo_nodes = GetTempoNodes(window, wblock);

  if (num >= tempo_nodes->num_elements){
    RError("deleteTemponode: No temponode %d in block %d",num,blocknum);
    return;
  }

  if (num==0){
    wblock->block->temponodes->reltempo = 0.0f;
  } else if (num==tempo_nodes->num_elements-1) {
    struct TempoNodes *last = ListLast3(&wblock->block->temponodes->l);
    last->reltempo = 0.0f;
  } else {
    ListRemoveElement3_fromNum(&wblock->block->temponodes,num);
  }

  UpdateSTimes(wblock->block);    

  window->must_redraw = true;
}

int createTemponode(float value, float floatplace, int blocknum, int windownum){
  PlayStop();

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

  if (temponode==NULL)
    return -1;
  
  //GFX_SetChangeFloat(window,wblock,"Reltempo",RelTempo2RealRelTempo(Gfx2RelTempo(wblock,dx)));
  //UpdateSTimes(wblock->block);
  //GFX_DrawStatusBar(window,wblock);

  UpdateSTimes(block);

  UpdateAllTrackReallines(window,wblock); // sure?

  window->must_redraw = true;

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
      if (note2==note && pitch==pitch2)
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
      PLAYER_lock();{
        RemoveNote(wblock->block, track, notes);
      }PLAYER_unlock();
      goto gotit;
    }
    
    num++;
    
    struct Pitches *pitches = notes->pitches;
    while(pitches!=NULL){
      if (pitchnum==num){
        PLAYER_lock();{
          ListRemoveElement3(&notes->pitches,&pitches->l);
        }PLAYER_unlock();
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
  window->must_redraw = true;
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

enum PitchInfoWhatToGet {
  PITCH_INFO_Y1,
  PITCH_INFO_Y2,
  PITCH_INFO_VALUE,
};

static float getPitchInfo(enum PitchInfoWhatToGet what_to_get, int pitchnum, int tracknum, int blocknum, int windownum){
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
  case PITCH_INFO_Y1:
    return get_mouse_realline_y1(window, getReallineForPitch(wblock, pitch, note));
  case PITCH_INFO_Y2:
    return get_mouse_realline_y2(window, getReallineForPitch(wblock, pitch, note));
  case PITCH_INFO_VALUE:
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
  return getPitchInfo(PITCH_INFO_Y1, pitchnum, tracknum, blocknum, windownum);
}

float getPitchY2(int pitchnum, int tracknum, int blocknum, int windownum){
  return getPitchInfo(PITCH_INFO_Y2, pitchnum, tracknum, blocknum, windownum);
}

float getPitchX1(int pitchnum, int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0.0f : wtrack->notearea.x;
}

float getPitchX2(int pitchnum, int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0.0f : wtrack->notearea.x2;
}

static struct Node *get_pitchnodeline(int pitchnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return NULL;

  struct Notes *note;
  struct Pitches *pitch;
  if (getPitch(pitchnum, &pitch, &note, wtrack->track)==false)
    return NULL;

  int note_pitchnum;

  if (pitch==NULL)
    note_pitchnum = 0;
  else
    note_pitchnum = ListPosition3(&note->pitches->l, &pitch->l) + 1;

  const vector_t *nodes = GetPitchNodes(window, wblock, wtrack, note);

  return nodes->elements[note_pitchnum];
}


float getPitchX(int num,  int tracknum, int blocknum, int windownum){
  struct Node *nodeline = get_pitchnodeline(num, tracknum, blocknum, windownum);
  return nodeline==NULL ? 0 : nodeline->x;
}

float getPitchY(int num, int tracknum, int blocknum, int windownum){
  struct Node *nodeline = get_pitchnodeline(num, tracknum, blocknum, windownum);
  return nodeline==NULL ? 0 : nodeline->y-scroll_pos;
}


float getPitchValue(int pitchnum, int tracknum, int blocknum, int windownum){
  return getPitchInfo(PITCH_INFO_VALUE, pitchnum, tracknum, blocknum, windownum);
}

void setCurrentPitch(int num, int tracknum, int blocknum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(-1, &window, blocknum, &wblock, tracknum);
  if(wtrack==NULL)
    return;

  struct Notes *note;
  struct Pitches *pitch;
  if (getPitch(num, &pitch, &note, wtrack->track)==false)
    return;

  struct ListHeader3 *listHeader3 = pitch!=NULL ? &pitch->l : &note->l;
  setCurrentNode(listHeader3);
}

void setIndicatorPitch(int num, int tracknum, int blocknum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(-1, &window, blocknum, &wblock, tracknum);
  if(wtrack==NULL)
    return;

  struct Notes *note;
  struct Pitches *pitch;
  if (getPitch(num, &pitch, &note, wtrack->track)==false)
    return;

  setIndicatorNode(&note->l);

  if (pitch==NULL)
    indicator_pitch_num = 0;
  else {
    int pitchnum = ListPosition3(&note->pitches->l, &pitch->l);
    indicator_pitch_num = pitchnum + 1;
  }
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

static void MoveEndNote(struct Blocks *block, struct Tracks *track, struct Notes *note, Place *place){
  Place firstLegal, lastLegal;

  struct Notes *next = NextNote(note);
  
  if (next!=NULL)
    PlaceCopy(&lastLegal, &next->l.p);
  else
    PlaceSetLastPos(block, &lastLegal);

  Place *last_pitch = ListLastPlace3(&note->pitches->l);
  Place *last_velocity = ListLastPlace3(&note->velocities->l);
  Place *startPlace = &note->l.p;

  if (last_pitch==NULL)
    last_pitch = startPlace;
  if (last_velocity==NULL)
    last_velocity = startPlace;

  Place *firstLegalConst = PlaceMax(last_pitch, last_velocity);
  PlaceFromLimit(&firstLegal, firstLegalConst);

  PLAYER_lock();{
    note->end = *PlaceBetween(&firstLegal, place, &lastLegal);
  }PLAYER_unlock();

  R_ASSERT(PlaceLessOrEqual(&note->end, &lastLegal));
}

static void MoveNote(struct Blocks *block, struct Tracks *track, struct Notes *note, Place *place){
  Place old_place = note->l.p;

  if (PlaceLessThan(place, &old_place)) {
    Place *prev_legal = getPrevLegalNotePlace(track, note);
    if (PlaceLessOrEqual(place, prev_legal))
      PlaceFromLimit(place, prev_legal);
  } else {
    Place *next_legal = getNextLegalNotePlace(note);
    if (PlaceGreaterOrEqual(place, next_legal))
      PlaceTilLimit(place, next_legal);
  }

  PLAYER_lock();{
    note->l.p = *place;
    ReplaceNoteEnds(block, track, &old_place, place);
  }PLAYER_unlock();
}

void setPitch(int num, float value, float floatplace, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);

  if (wtrack==NULL)
    return;
  
  struct Tracks *track = wtrack->track;
  struct Blocks *block = wblock->block;

  value = R_BOUNDARIES(1,value,127);

  struct Notes *note;
  struct Pitches *pitch;

  if (getPitch(num, &pitch, &note, track)==false)
    return;

  if (pitch != NULL) {
    
    pitch->note = value;

    if (floatplace >= 0.0f) {
      Place firstLegalPlace,lastLegalPlace;
      PlaceFromLimit(&firstLegalPlace, &note->l.p);
      PlaceTilLimit(&lastLegalPlace, &note->end);

      Place place;
      Float2Placement(floatplace, &place);

      PLAYER_lock();{
        ListMoveElement3_ns(&note->pitches, &pitch->l, &place, &firstLegalPlace, &lastLegalPlace);
      }PLAYER_unlock();
    }
                        
  } else {
    
    note->note = value;

    if (floatplace >= 0)
      MoveNote(block, track, note, PlaceCreate2(floatplace));
  }

  UpdateTrackReallines(window,wblock,wtrack);
  window->must_redraw = true;
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

  struct Notes *note = InsertNote(wblock, wtrack, place, NULL, value, NOTE_get_velocity(wtrack->track), 0);

  UpdateTrackReallines(window,wblock,wtrack);
  window->must_redraw = true;
  
  return getPitchNum(wtrack->track, note, NULL);
}

static int addPitch(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, struct Notes *note, Place *place, float value){

  struct Pitches *pitch = AddPitch(window, wblock, wtrack, note, place, note->note);

  if(pitch==NULL)
    return -1;
  
  window->must_redraw = true;

  return getPitchNum(wtrack->track, note, pitch);
}

int createPitch(float value, float floatplace, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);

  if (wtrack==NULL)
    return -1;

  Place place;
  Float2Placement(floatplace, &place);
  
  struct Notes *note = getNoteAtPlace(wtrack->track, &place);

  value = R_BOUNDARIES(0,value,127);

  Undo_Notes(window,wblock->block,wtrack->track,window->wblock->curr_realline);
    
  if(note==NULL)
    return addNote2(window, wblock, wtrack, &place, value);
  else
    return addPitch(window, wblock, wtrack, note, &place, value);
}
  

// subtracks
///////////////////////////////////////////////////
int getNumSubtracks(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 1;

  return wtrack->num_vel;
}

static struct WTracks *getSubtrackWTrack(int subtracknum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return NULL;

  if (subtracknum>=wtrack->num_vel) {
    RError("No subtrack %d in track %d in block %d (only %d subtracks in this track)\n", subtracknum, tracknum, blocknum, wtrack->num_vel);
    return 0;
  }

  return wtrack;
}

float getSubtrackX1(int subtracknum, int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getSubtrackWTrack(subtracknum, tracknum, blocknum, windownum);
  if (wtrack==NULL)
    return 0.0f;
  else
    return GetXSubTrack1(wtrack,subtracknum);
}

float getSubtrackX2(int subtracknum, int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getSubtrackWTrack(subtracknum, tracknum, blocknum, windownum);
  if (wtrack==NULL)
    return 0.0f;
  else
    return GetXSubTrack2(wtrack,subtracknum);
}

// notes
//////////////////////////////////////////////////

void undoNotes(int tracknum, int blocknum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(-1, &window, blocknum, &wblock, tracknum);
  if(wtrack==NULL)
    return;
  Undo_Notes(window,window->wblock->block,wtrack->track,window->wblock->curr_realline);
}

float getNoteStart(int notenum, int tracknum, int blocknum, int windownum){
  struct Notes *note=getNoteFromNum(windownum,blocknum,tracknum,notenum);

  if(note==NULL)
    return -1.0f;

  return GetfloatFromPlace(&note->l.p);
}

float getNoteEnd(int notenum, int tracknum, int blocknum, int windownum){
  struct Notes *note=getNoteFromNum(windownum,blocknum,tracknum,notenum);

  if(note==NULL)
    return -1.0f;

  return GetfloatFromPlace(&note->end);
}

int getNoteSubtrack(int notenum, int tracknum, int blocknum, int windownum){
  struct Notes *note=getNoteFromNum(windownum,blocknum,tracknum,notenum);

  if(note==NULL)
    return 0;

  return note->subtrack;
}

void setNoMouseNote(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if(wblock==NULL)
    return;
  
  if (wblock->mouse_note != NULL){
    wblock->mouse_note = NULL;
    window->must_redraw = true;
    //printf("no mouse note dirty\n");
  }
}

void setMouseNote(int notenum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, notenum);
  if (note==NULL)
    return;
  else if (wblock->mouse_note != note){
    wblock->mouse_note = note;
    window->must_redraw = true;
    //printf("mouse note dirty\n");
  }
}


// velocities
//////////////////////////////////////////////////

static struct Node *get_velocitynode(int velocitynum, int notenum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, notenum);
  if (note==NULL)
    return NULL;

  const vector_t *nodes = GetVelocityNodes(window, wblock, wtrack, note);
  if (velocitynum < 0 || velocitynum>=nodes->num_elements) {
    RError("There is no velocity %d in note %d in track %d in block %d",velocitynum, notenum, tracknum, blocknum);
    return NULL;
  }
  
  return nodes->elements[velocitynum];
}


float getVelocityX(int num, int notenum, int tracknum, int blocknum, int windownum){
  struct Node *node = get_velocitynode(num, notenum, tracknum, blocknum, windownum);
  return node==NULL ? 0 : node->x;
}

float getVelocityY(int num, int notenum, int tracknum, int blocknum, int windownum){
  struct Node *node = get_velocitynode(num, notenum, tracknum, blocknum, windownum);
  return node==NULL ? 0 : node->y-scroll_pos;
}


float getVelocityValue(int velocitynum, int notenum, int tracknum, int blocknum, int windownum){
  struct Node *node = get_velocitynode(velocitynum, notenum, tracknum, blocknum, windownum);
  if (node==NULL)
    return 0;

  struct Velocities *velocity = (struct Velocities*)node->element;
  return velocity->velocity / (float)MAX_VELOCITY;
}

int getNumVelocities(int notenum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, notenum);
  if (note==NULL)
    return 0;

  const vector_t *nodes = GetVelocityNodes(window, wblock, wtrack, note);

  return nodes->num_elements;
}

int createVelocity(float value, float floatplace, int notenum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, notenum);
  if (note==NULL)
    return -1;

  Place place;
  Float2Placement(floatplace, &place);

  if (PlaceLessOrEqual(&place, &note->l.p)) {
    RError("createVelocity: placement before note start for note #%d", notenum);
    return -1;
  }

  if (PlaceGreaterOrEqual(&place, &note->end)) {
    RError("createVelocity: placement after note end for note #%d", notenum);
    return -1;
  }

  Undo_Notes(window,wblock->block,wtrack->track,window->wblock->curr_realline);

  int ret;
  PLAYER_lock();{
    ret = AddVelocity(value*MAX_VELOCITY, &place, note);
  }PLAYER_unlock();

  if (ret==-1){
    RError("createVelocity: Can not create new velocity with the same position as another velocity");
    return -1;
  }


  window->must_redraw = true;

  return ret+1;
}
  
void setVelocity(int velocitynum, float value, float floatplace, int notenum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, notenum);
  if (note==NULL)
    return;

  const vector_t *nodes = GetVelocityNodes(window, wblock, wtrack, note);
  if (velocitynum < 0 || velocitynum>=nodes->num_elements) {
    RError("There is no velocity %d in note %d in track %d in block %d",velocitynum, notenum, tracknum, blocknum);
    return;
  }

  if (velocitynum==0) {
    note->velocity = R_BOUNDARIES(0,value*MAX_VELOCITY,MAX_VELOCITY);
    if (floatplace>=0) {
      MoveNote(wblock->block, wtrack->track, note, PlaceCreate2(floatplace));
      UpdateTrackReallines(window,wblock,wtrack);
    }
  } else if (velocitynum==nodes->num_elements-1) {
    note->velocity_end = R_BOUNDARIES(0,value*MAX_VELOCITY,MAX_VELOCITY);
    if (floatplace>=0)
      MoveEndNote(wblock->block, wtrack->track, note, PlaceCreate2(floatplace));

  } else {

    struct Velocities *velocity;

    if (floatplace < 0 ) {
      velocity = ListFindElement3_num(&note->velocities->l, velocitynum-1);
    } else {
      Place firstLegalPlace,lastLegalPlace;
      PlaceFromLimit(&firstLegalPlace, &note->l.p);
      PlaceTilLimit(&lastLegalPlace, &note->end);
      
      Place place;
      Float2Placement(floatplace, &place);

      PLAYER_lock();{
        velocity = (struct Velocities*)ListMoveElement3_FromNum_ns(&note->velocities, velocitynum-1, &place, &firstLegalPlace, &lastLegalPlace);
      }PLAYER_unlock();
    }
    
    velocity->velocity=R_BOUNDARIES(0,value*MAX_VELOCITY,MAX_VELOCITY);
  }

  window->must_redraw = true;
}

void deleteVelocity(int velocitynum, int notenum, int tracknum, int blocknum, int windownum){
 struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, notenum);
  if (note==NULL)
    return;

  struct Blocks *block=wblock->block;
  struct Tracks *track=wtrack->track;

  const vector_t *nodes = GetVelocityNodes(window, wblock, wtrack, note);
  if (velocitynum < 0 || velocitynum>=nodes->num_elements) {
    RError("There is no velocity %d in note %d in track %d in block %d",velocitynum, notenum, tracknum, blocknum);
    return;
  }

  bool is_first                      = velocitynum==0;
  bool is_last                       = velocitynum==nodes->num_elements-1;
  bool is_last_and_no_velocities     = is_last && nodes->num_elements==2;
  bool is_last_and_there_are_pitches = is_last && note->pitches!=NULL;

  if (is_first || is_last_and_no_velocities || is_last_and_there_are_pitches){
    PLAYER_lock();{
      RemoveNote(block, track, note);
    }PLAYER_unlock();
    UpdateTrackReallines(window,wblock,wtrack);

  } else if (velocitynum==nodes->num_elements-1) {
    struct Velocities *last = (struct Velocities*)ListLast3(&note->velocities->l);
    PLAYER_lock();{
      note->end = last->l.p;
      note->velocity_end = last->velocity;
      ListRemoveElement3(&note->velocities, &last->l);
    }PLAYER_unlock();

  } else {
    PLAYER_lock();{
      ListRemoveElement3_fromNum(&note->velocities, velocitynum-1);
    }PLAYER_unlock();
  }

  window->must_redraw = true;
}


void setCurrentVelocityNode(int velocitynum, int notenum, int tracknum, int blocknum, int windownum){
 struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, notenum);
  if (note==NULL)
    return;

  const vector_t *nodes = GetVelocityNodes(window, wblock, wtrack, note);
  if (velocitynum < 0 || velocitynum>=nodes->num_elements) {
    RError("There is no velocity %d in note %d in track %d in block %d",velocitynum, notenum, tracknum, blocknum);
    return;
  }

  struct Node *node = nodes->elements[velocitynum];
  struct Velocities *current = (struct Velocities*)node->element;

  setCurrentNode(&current->l);
}

void setIndicatorVelocityNode(int velocitynum, int notenum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, notenum);
  if (note==NULL)
    return;

  setIndicatorNode(&note->l);

  const vector_t *nodes = GetVelocityNodes(window, wblock, wtrack, note);
  if (velocitynum < 0 || velocitynum>=nodes->num_elements) {
    RError("There is no velocity %d in note %d in track %d in block %d",velocitynum, notenum, tracknum, blocknum);
    return;
  }

  indicator_velocity_num = velocitynum;
}


// fxes
//////////////////////////////////////////////////

void addFX(int windownum, int blocknum, int tracknum){
  struct Tracker_Windows *window=NULL;
  struct WTracks *wtrack;
  struct WBlocks *wblock;

  wtrack=getWTrackFromNumA(
                           windownum,
                           &window,
                           blocknum,
                           &wblock,
                           tracknum
                           );

  if(wtrack==NULL) return;

  //printf("x: %f, y: %f\n",tevent.x, tevent.y);
  
  AddFXNodeLineCurrPos(window, wblock, wtrack);
}

void addFXMousePos(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL)
    return;

  AddFXNodeLineCurrMousePos(window);
}

static struct Node *get_fxnode(int fxnodenum, int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return NULL;
  
  const vector_t *nodes = GetFxNodes(window, wblock, wtrack, fxs);
  return VECTOR_get(nodes, fxnodenum, "fx node");
}


float getFxnodeX(int num, int fxnum, int tracknum, int blocknum, int windownum){
  struct Node *node = get_fxnode(num, fxnum, tracknum, blocknum, windownum);
  return node==NULL ? 0 : node->x;
}

float getFxnodeY(int num, int fxnum, int tracknum, int blocknum, int windownum){
  struct Node *node = get_fxnode(num, fxnum, tracknum, blocknum, windownum);
  return node==NULL ? 0 : node->y-scroll_pos;
}


float getFxnodeValue(int fxnodenum, int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return 0.0f;

  const vector_t *nodes = GetFxNodes(window, wblock, wtrack, fxs);
  struct Node *node = VECTOR_get(nodes, fxnodenum, "fx node");
  if (node==NULL)
    return 0.0f;

  int max = fxs->fx->max;
  int min = fxs->fx->min;

  struct FXNodeLines *fxnodeline = (struct FXNodeLines*)node->element;

  return scale(fxnodeline->val, min, max, 0.0f, 1.0f);
}

const char* getFxName(int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return NULL;

  return fxs->fx->name;
}
  
char* getFxString(int fxnodenum, int fxnum, int tracknum, int blocknum, int windownum){   
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return NULL;

  const vector_t *nodes = GetFxNodes(window, wblock, wtrack, fxs);
  struct Node *node = VECTOR_get(nodes, fxnodenum, "fx node");
  if (node==NULL)
    return "<fxnode not found>";

  struct FXNodeLines *fxnodeline = (struct FXNodeLines *)node->element;

  float val = fxnodeline->val;

  // Turned out this was a lot of work. Fix later, hopefully.
  //return fx->getFXstring(fx, wtrack->track, val);

  // instead we just do this:
  struct FX *fx = fxs->fx;
  static char ret[512];

  if (wtrack->track->patch->instrument==get_MIDI_instrument())
    snprintf(ret, 511, "%s: %d", fx->name, (int)val);
  else
    snprintf(ret, 511, "%s: %.02f", fx->name, scale(val, fx->min, fx->max, 0.0f, 1.0f));

  return ret;
}

int getNumFxes(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  if (wtrack == NULL)
    return 0;

  return ListFindNumElements1(&wtrack->track->fxs->l);
}

int getNumFxnodes(int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return 0;

  return ListFindNumElements3(&fxs->fxnodelines->l);
}

float getFxMinValue(int fxnum, int tracknum, int blocknum, int windownum){
  return 0.0f;
#if 0
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fx = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fx==NULL)
    return 0;

  return fx->fx->min;
#endif
}

float getFxMaxValue(int fxnum, int tracknum, int blocknum, int windownum){
  return 1.0f;
#if 0
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fx = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fx==NULL)
    return 1;

  return fx->fx->max;
#endif
}

int createFxnode(float value, float floatplace, int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fx = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fx==NULL)
    return -1;

  Place lastplace;
  PlaceSetLastPos(wblock->block, &lastplace);
                  
  Place place;
  Float2Placement(floatplace, &place);

  if (PlaceLessOrEqual(&place, PlaceGetFirstPos())){
    RError("createFx: placement before top of block for fx #%d", fxnum);
    return -1;
  }

  if (PlaceGreaterOrEqual(&place, &lastplace)) {
    RError("createFx: placement after fx end for fx #%d", fxnum);
    return -1;
  }

  Undo_FXs(window, wblock->block, wtrack->track, wblock->curr_realline);

  int max = fx->fx->max;
  int min = fx->fx->min;

  int ret;
  PLAYER_lock();{
    ret = AddFXNodeLine(
                        window,
                        wblock,
                        wtrack,
                        fx->l.num,
                        scale(value, 0,1, min, max),
                        &place
                        );
  }PLAYER_unlock();

  if (ret==-1){
    RError("createFx: Can not create new fx with the same position as another fx");
    return -1;
  }

  window->must_redraw = true;

  return ret;
}
  
void setFxnode(int fxnodenum, float value, float floatplace, int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fx = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fx==NULL)
    return;

  const vector_t *nodes = GetFxNodes(window, wblock, wtrack, fx);
  if (fxnodenum < 0 || fxnodenum>=nodes->num_elements) {
    RError("There is no fx node %d for fx %d in track %d in block %d",fxnodenum, fxnum, tracknum, blocknum);
    return;
  }

  struct Node *node = nodes->elements[fxnodenum];
  struct FXNodeLines *fxnodeline = (struct FXNodeLines *)node->element;
  
  if (floatplace >= 0.0f){
    Place place;
    Float2Placement(floatplace, &place);
  
    PLAYER_lock();{
      ListMoveElement3_FromNum_ns(&fx->fxnodelines, fxnodenum, &place, PlaceGetFirstPos(), PlaceGetLastPos(wblock->block));
    }PLAYER_unlock();
  }
  
  int max = fx->fx->max;
  int min = fx->fx->min;
  
  fxnodeline->val=scale(value, 0.0f, 1.0f, min, max); //R_BOUNDARIES(min,value,max);

  window->must_redraw = true;
}

void deleteFxnode(int fxnodenum, int fxnum, int tracknum, int blocknum, int windownum){
 struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return;

  const vector_t *nodes = GetFxNodes(window, wblock, wtrack, fxs);
  if (fxnodenum < 0 || fxnodenum>=nodes->num_elements) {
    RError("There is no fx node %d for fx %d in track %d in block %d",fxnodenum, fxnum, tracknum, blocknum);
    return;
  }

  Undo_FXs(window, wblock->block, wtrack->track, wblock->curr_realline);

  struct Node *node = nodes->elements[fxnodenum];
  struct FXNodeLines *fxnodeline = (struct FXNodeLines *)node->element;
  
  DeleteFxNodeLine(wtrack, fxs, fxnodeline); // DeleteFxNodeLine locks player / stops playing

  window->must_redraw = true;
}


void setCurrentFxnode(int fxnodenum, int fxnum, int tracknum, int blocknum, int windownum){
 struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fx = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fx==NULL)
    return;

  const vector_t *nodes = GetFxNodes(window, wblock, wtrack, fx);
  if (fxnodenum < 0 || fxnodenum>=nodes->num_elements) {
    RError("There is no fx node %d for fx %d in track %d in block %d",fxnodenum, fxnum, tracknum, blocknum);
    return;
  }

  struct Node *node = nodes->elements[fxnodenum];
  struct FXNodeLines *current = (struct FXNodeLines*)node->element;

  setCurrentNode(&current->l);
}

void setIndicatorFxnode(int fxnodenum, int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fx = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fx==NULL)
    return;

  const vector_t *nodes = GetFxNodes(window, wblock, wtrack, fx);
  if (fxnodenum < 0 || fxnodenum>=nodes->num_elements) {
    RError("There is no fx node %d for fx %d in track %d in block %d",fxnodenum, fxnum, tracknum, blocknum);
    return;
  }

  struct Node *node = nodes->elements[fxnodenum];
  setIndicatorNode(node->element);
}

void setNoMouseFx(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if(wblock==NULL)
    return;
  
  if (wblock->mouse_fxs != NULL){
    wblock->mouse_fxs = NULL;
    window->must_redraw = true;
    //printf("no mouse fx dirty\n");
  }
}

void setMouseFx(int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return;
  else if (wblock->mouse_fxs != fxs){
    wblock->mouse_fxs = fxs;
    window->must_redraw = true;
    //printf("mouse fx dirty\n");
  }
}

void undoFxs(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;

  Undo_FXs(window, wblock->block, wtrack->track, wblock->curr_realline);
}


// track widths
//////////////////////////////////////////////////

float getTrackWidth(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
    
  if (tracknum==-1){
    wblock = getWBlockFromNumA(windownum, &window, blocknum);
    if (wblock==NULL)
      return 0.0f;
    return wblock->temponodearea.width;
  } else {
    struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
    if (wtrack==NULL)
      return 0.0f;
    return wtrack->fxwidth;
  }
}

void setTrackWidth(float new_width, int tracknum, int blocknum, int windownum){
  if (new_width < 2) {
#if 0
    RError("Can not set width smaller than 2");
    return;
#else
    new_width = 2;
#endif
  }

  struct Tracker_Windows *window;
  struct WBlocks *wblock;
    
  if (tracknum==-1){
    wblock = getWBlockFromNumA(windownum, &window, blocknum);
    if (wblock==NULL)
      return;
    wblock->temponodearea.width = new_width;
  } else {
    struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
    if (wtrack==NULL)
      return;
    printf("new width: %d, old: %d\n",(int)new_width,wtrack->fxwidth);
    wtrack->fxwidth = new_width;
  }

  UpdateWBlockCoordinates(window,wblock);
  window->must_redraw=true;
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

/*
// Doesn't work to check right extra
bool extraPressed(void){
  return AnyExtra(tevent.keyswitch);
}
*/

bool leftExtraPressed(void){
  return LeftExtra(tevent.keyswitch);
}

bool altPressed(void){
  return AnyAlt(tevent.keyswitch);
}



// mouse pointer
//////////////////////////////////////////////////

/*
enum MousePointerType {
  MP_NORMAL,
  MP_BLANK,
  MP_DIAGONAL,
  MP_HORIZONTAL,  
};
*/

void setNormalMousePointer(int windownum){
  struct Tracker_Windows *window = getWindowFromNum(windownum);
  if (window!=NULL)
    SetNormalPointer(window);
}
void setBlankMousePointer(int windownum){
  struct Tracker_Windows *window = getWindowFromNum(windownum);
  if (window!=NULL)
    SetBlankPointer(window);
}
void setDiagonalResizeMousePointer(int windownum){
  struct Tracker_Windows *window = getWindowFromNum(windownum);
  if (window!=NULL)
    SetDiagResizePointer(window);
}
void setHorizontalResizeMousePointer(int windownum){
  struct Tracker_Windows *window = getWindowFromNum(windownum);
  if (window!=NULL)
    SetHorizResizePointer(window);
}
void moveMousePointer(float x, float y, int windownum){
  struct Tracker_Windows *window = getWindowFromNum(windownum);
  if (window!=NULL)
    MovePointer(window, x, y);
}
float getMousePointerX(int windownum){
  struct Tracker_Windows *window = getWindowFromNum(windownum);
  if (window==NULL)
    return 0;

  WPoint ret = GetPointerPos(window);
  return ret.x;
}
float getMousePointerY(int windownum){
  struct Tracker_Windows *window = getWindowFromNum(windownum);
  if (window==NULL)
    return 0;

  WPoint ret = GetPointerPos(window);
  return ret.y;
}
