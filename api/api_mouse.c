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


#include "../common/nsmtracker.h"
#include "../common/placement_proc.h"
#include "../common/list_proc.h"
#include "../common/undo_reltemposlider_proc.h"
#include "../common/gfx_wblocks_reltempo_proc.h"
#include "../common/gfx_statusbar_proc.h"
#include "../common/trackreallines_proc.h"
#include "../common/time_proc.h"
#include "../common/common_proc.h"

#include "api_common_proc.h"

extern struct Root *root;

extern volatile float scroll_pos;


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

static struct ListHeader3 *setPlace(struct Blocks *block, struct ListHeader3 *list, int num, float floatplace){
  struct ListHeader3 *node;

  // Find prevplace and node
  
  Place *prevplace;
  
  if (num==0) {
    prevplace=PlaceGetFirstPos();
    node = list;
  } else {
    struct ListHeader3 *prevnode = ListFindElement3_num(&block->temponodes->l, num-1);
    if (prevnode==NULL)
      return NULL;
    prevplace = &prevnode->p;
    node = prevnode->next;
  }


  if (node==NULL)
    return NULL;


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



// temponodearea


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
    RError("There is no temponode box %d",boxnum);
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

void setTemponode(int num, float value, float place, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if (wblock==NULL)
    return;

  struct Blocks *block = wblock->block;

  struct TempoNodes *temponode;
  if (num==0)
    temponode = block->temponodes; // don't want to set placement for the first node. It's always at top.
  else if (num==wblock->reltempo_nodes->num_elements-1)
    temponode = ListLast3(&block->temponodes->l); // don't want to set placement for the last node. It's always at bottom.
  else
    temponode = (struct TempoNodes *)setPlace(block, &block->temponodes->l, num, place);
  
  if (temponode==NULL){

    RError("No temponode %d in block %d%s",num,blocknum,blocknum==-1?" (i.e. current block)":"");

  } else {

    if ( (value+1) > wblock->reltempomax) {
      wblock->reltempomax = value+1;      
    } else if ( (value-1) < -wblock->reltempomax) {
      wblock->reltempomax = -1*(value -1);
    }

    temponode->reltempo = value;
  
    UpdateAllTrackReallines(window,wblock);
  
    UpdateSTimes(wblock->block);

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



// ctrl / shift keys

extern struct TEvent tevent;

bool ctrlPressed(void){
  return AnyCtrl(tevent.keyswitch);
}

bool shiftPressed(void){
  return AnyShift(tevent.keyswitch);
}

