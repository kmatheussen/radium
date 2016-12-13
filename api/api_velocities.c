#include "Python.h"

#include "../common/nsmtracker.h"
#include "../common/list_proc.h"
#include "../common/placement_proc.h"
#include "../common/velocities_proc.h"
#include "../common/nodelines_proc.h"
#include "../common/undo.h"
#include "../common/undo_notes_proc.h"
#include "../common/notes_proc.h"

#include "api_mouse_proc.h"

#include "api_common_proc.h"
#include "api_support_proc.h"
#include "radium_proc.h"



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
    handleError("There is no velocity %d in note %d in track %d in block %d",velocitynum, notenum, tracknum, blocknum);
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
  return node==NULL ? 0 : node->y-get_scroll_pos();
}


float getVelocityValue(int velocitynum, int notenum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = NULL;

  struct Velocities *velocity = getVelocityFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, notenum, &note, velocitynum);

  if (velocity==NULL){

    if (note==NULL)
      return 0;

    else if (velocitynum==0)
      return (double)note->velocity / (double)MAX_VELOCITY;

    else
      return (double)note->velocity_end / (double)MAX_VELOCITY;

  }

  return (double)velocity->velocity / (double)MAX_VELOCITY;
}

Place getVelocityPlace(int velocitynum, int notenum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = NULL;

  struct Velocities *velocity = getVelocityFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, notenum, &note, velocitynum);

  if (velocity==NULL){

    if (note==NULL)
      return place(0,0,1);

    else if (velocitynum==0)
      return note->l.p;

    else
      return note->end;

  }

  return velocity->l.p;
}

int getVelocityLogtype(int velocitynum, int notenum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = NULL;

  struct Velocities *velocity = getVelocityFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, notenum, &note, velocitynum);

  if (velocity==NULL){

    if (note==NULL)
      return 0;

    else if (velocitynum==0)
      return note->velocity_first_logtype;

    else
      return 0; // Last logtype. Always 0. Irrelevant.
  }

  return velocity->logtype;
}

int getNumVelocities(int notenum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, notenum);
  if (note==NULL)
    return 0;

  return 2+ListFindNumElements3(&note->velocities->l);
}

int addVelocity(float value, Place place, int notenum, int tracknum, int blocknum, int windownum){

  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, notenum);
  if (note==NULL)
    return -1;

  if (PlaceLessOrEqual(&place, &note->l.p)) {
    //if (notenum>0)
    //  handleError("addVelocity: placement before note start for note #%d", notenum);
    return -1;
  }

  if (PlaceGreaterOrEqual(&place, &note->end)) {
    //handleError("addVelocity: placement after note end for note #%d", notenum);
    return -1;
  }

  ADD_UNDO(Notes(window,wblock->block,wtrack->track,window->wblock->curr_realline));

  int ret = AddVelocity(value*MAX_VELOCITY, &place, note);

  if (ret==-1){
    //handleError("addVelocity: Can not create new velocity with the same position as another velocity");
    return -1;
  }

  
  window->must_redraw_editor = true;

  return ret+1;
}

int addVelocityF(float value, float floatplace, int notenum, int tracknum, int blocknum, int windownum){
  Place place;
  Float2Placement(floatplace, &place);
  return addVelocity(value, place, notenum, tracknum, blocknum, windownum);
}

int setVelocity(int velocitynum, float value, Place place, int notenum, int tracknum, int blocknum, int windownum){
  
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, notenum);
  if (note==NULL)
    return notenum;

  struct Blocks *block = wblock->block;
  struct Tracks *track = wtrack->track;
  
  const vector_t *nodes = GetVelocityNodes(window, wblock, wtrack, note);
  if (velocitynum < 0 || velocitynum>=nodes->num_elements) {
    handleError("There is no velocity %d in note %d in track %d in block %d",velocitynum, notenum, tracknum, blocknum);
    return notenum;
  }

  window->must_redraw_editor = true;

  //printf("velocitynum==%d. floatplace: %f\n",velocitynum,floatplace);

  if (velocitynum==0) {
    
    note->velocity = R_BOUNDARIES(0,value*MAX_VELOCITY,MAX_VELOCITY);
    if (place.line>=0)
      return MoveNote(block, track, note, &place, true);
    
  } else if (velocitynum==nodes->num_elements-1) {
    
    note->velocity_end = R_BOUNDARIES(0,value*MAX_VELOCITY,MAX_VELOCITY);
    if (place.line>=0)
      MoveEndNote(block, track, note, &place, true);

  } else {

    struct Velocities *velocity;

    if (place.line < 0 ) {
      velocity = ListFindElement3_num(&note->velocities->l, velocitynum-1);
    } else {
      Place firstLegalPlace,lastLegalPlace;
      PlaceFromLimit(&firstLegalPlace, &note->l.p);
      PlaceTilLimit(&lastLegalPlace, &note->end);
      
      PLAYER_lock();{
        velocity = (struct Velocities*)ListMoveElement3_FromNum_ns(&note->velocities, velocitynum-1, &place, &firstLegalPlace, &lastLegalPlace);
        NOTE_validate(block, track, note);
      }PLAYER_unlock();
    }
    
    velocity->velocity=R_BOUNDARIES(0,value*MAX_VELOCITY,MAX_VELOCITY);
  }

  return notenum;
}

int setVelocityF(int velocitynum, float value, float floatplace, int notenum, int tracknum, int blocknum, int windownum){
  Place place;
  
  if (floatplace < 0) {
    place.line=-1;
    place.counter=0;
    place.dividor=1;
  }else
    Float2Placement(floatplace, &place);
  return setVelocity(velocitynum, value, place, notenum, tracknum, blocknum, windownum);
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
    handleError("There is no velocity %d in note %d in track %d in block %d",velocitynum, notenum, tracknum, blocknum);
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

  } else if (velocitynum==nodes->num_elements-1) {
    struct Velocities *last = (struct Velocities*)ListLast3(&note->velocities->l);
    PLAYER_lock();{
      note->end = last->l.p;
      note->velocity_end = last->velocity;
      ListRemoveElement3(&note->velocities, &last->l);
      NOTE_validate(block, track, note);
    }PLAYER_unlock();

  } else {
    PLAYER_lock();{
      ListRemoveElement3_fromNum(&note->velocities, velocitynum-1);
      NOTE_validate(block, track, note);
    }PLAYER_unlock();
  }

  window->must_redraw_editor = true;
}

void setVelocityLogtype(int logtype, int velocitynum, int notenum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, notenum);
  if (note==NULL)
    return;

  //struct Blocks *block=wblock->block;
  //struct Tracks *track=wtrack->track;

  const vector_t *nodes = GetVelocityNodes(window, wblock, wtrack, note);
  if (velocitynum < 0 || velocitynum>=nodes->num_elements) {
    handleError("There is no velocity %d in note %d in track %d in block %d",velocitynum, notenum, tracknum, blocknum);
    return;
  }
    
  bool is_first                      = velocitynum==0;
  bool is_last                       = velocitynum==nodes->num_elements-1;

  if (is_last) {
    handleError("Can not set logtype for last velocity (doesn't make any sense)");
    return;
  }

  if (is_first)
    note->velocity_first_logtype = logtype;
  else {
    struct Node *node = nodes->elements[velocitynum];
    struct Velocities *velocity = (struct Velocities*)node->element;

    velocity->logtype = logtype;
  }

  window->must_redraw_editor = true;
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
    handleError("There is no velocity %d in note %d in track %d in block %d",velocitynum, notenum, tracknum, blocknum);
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

  const vector_t *nodes = GetVelocityNodes(window, wblock, wtrack, note);
  if (velocitynum < 0 || velocitynum>=nodes->num_elements) {
    handleError("There is no velocity %d in note %d in track %d in block %d",velocitynum, notenum, tracknum, blocknum);
    return;
  }

  setIndicatorNode(&note->l);
  indicator_velocity_num = velocitynum;
}



