#include "Python.h"

#include "../common/nsmtracker.h"
#include "../common/TimeData.hpp"
#include "../common/list_proc.h"
#include "../common/placement_proc.h"
#include "../common/velocities_proc.h"
#include "../common/nodelines_proc.h"
#include "../common/undo.h"
#include "../common/undo_notes_proc.h"
#include "../common/notes_proc.h"

#include "../OpenGL/Render_proc.h"

#include "api_mouse_proc.h"

#include "api_common_proc.h"
#include "api_support_proc.h"
#include "radium_proc.h"



// velocities
//////////////////////////////////////////////////

static struct Node2 *get_velocitynode(int velocitynum, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note==NULL)
    return NULL;

  const vector_t *nodes = GetVelocityNodes(window, wblock, wtrack, note);
  if (velocitynum < 0 || velocitynum>=nodes->num_elements) {
    handleError("There is no velocity #%d in note %d in track #%d in block #%d",velocitynum, (int)note->id, tracknum, blocknum);
    return NULL;
  }

  R_ASSERT_NON_RELEASE(r::VelocityTimeData::Reader(note->_velocities).size()==nodes->num_elements-2);
  
  return (struct Node2*)nodes->elements[velocitynum];
}


float getVelocityX(int num, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Node2 *node = get_velocitynode(num, dynnote, tracknum, blocknum, windownum);
  return node==NULL ? 0 : node->x;
}

float getVelocityY(int num, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Node2 *node = get_velocitynode(num, dynnote, tracknum, blocknum, windownum);
  return node==NULL ? 0 : node->y-get_scroll_pos();
}


float getVelocityValue(int velocitynum, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = NULL;

  const r::Velocity velocity = getVelocityFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote, &note, velocitynum);

  if (note==NULL)
    return 0;

  return (double)velocity._val / (double)MAX_VELOCITY;
}

Place getVelocityPlace(int velocitynum, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = NULL;

  const r::Velocity velocity = getVelocityFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote, &note, velocitynum);

  if (note==NULL)
    return p_Create(0,0,1);

  return ratio2place(velocity._time);
}

int getVelocityLogtype(int velocitynum, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = NULL;

  const r::Velocity velocity = getVelocityFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote, &note, velocitynum);

  if (note==NULL)
    return 0;

  return velocity._logtype;
}

int getNumVelocities(dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note==NULL)
    return 0;

  return r::VelocityTimeData::Reader(note->_velocities).size() + 2;
}

static int addVelocity2(float value, Place place, dyn_t dynnote, int tracknum, int blocknum, int windownum, bool show_errors){

  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note==NULL)
    return -1;

  if (PlaceLessThan(&place, &note->l.p)) {
    handleError("addVelocity: placement before note start for note. velocity: %s. note: %s", p_ToString(place), p_ToString(note->l.p));
    return -1;
  }

  if (PlaceEqual(&place, &note->l.p)) {
    if (show_errors)
      handleError("addVelocity: placement at note start for note. velocity: %s. note: %s", p_ToString(place), p_ToString(note->l.p));
    return -1;
  }

  if (place2ratio(place)==note->end){
    if (show_errors)
      handleError("addVelocity: placement at note end for note. velocity: %s. note end: %s", p_ToString(place), p_ToString(ratio2place(note->end)));
    return -1;
  }

  if (place2ratio(place) > note->end) {
    handleError("addVelocity: placement after note end for note. velocity: %s. note end: %s", p_ToString(place), p_ToString(ratio2place(note->end)));
    return -1;
  }

  int ret = AddVelocity(value*MAX_VELOCITY, &place, note);

  if (ret==-1){
    if (show_errors)
      handleError("addVelocity: Can not create new velocity with the same position as another velocity");
    return -1;
  }

  
  window->must_redraw_editor = true;

  return ret+1;
}

int addVelocity(float value, Place place, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  return addVelocity2(value, place, dynnote, tracknum, blocknum, windownum, true);
}
int addVelocityDontDisplayErrors(float value, Place place, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  return addVelocity2(value, place, dynnote, tracknum, blocknum, windownum, false);
}
  
int addVelocityF(float value, float floatplace, int notenum, int tracknum, int blocknum, int windownum){
  if (floatplace < 0){
    handleError("Place can not be negative: %f", floatplace);
    return -1;
  }

  Place place;
  Float2Placement(floatplace, &place);
  return addVelocity(value, place, DYN_create_int(notenum), tracknum, blocknum, windownum);
}

dyn_t setVelocity(float value, Place place, int velocitynum, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note==NULL)
    return dynnote;

  struct Blocks *block = wblock->block;
  struct Tracks *track = wtrack->track;

  int num_velocities = getNumVelocities(dynnote, tracknum, blocknum, windownum);
  if (velocitynum < 0 || velocitynum>=num_velocities) {
    handleError("There is no velocity #%d in note %d in track #%d in block# %d",velocitynum, (int)note->id, tracknum, blocknum);
    return dynnote;
  }

  window->must_redraw_editor = true;

  //printf("velocitynum==%d. floatplace: %f\n",velocitynum,floatplace);

  int value2 = R_BOUNDARIES(0,value*MAX_VELOCITY,MAX_VELOCITY);
  
  if (velocitynum==0) {
    
    note->velocity = value2;
    if (!p_is_same_place(place)){
      if(place.line < 0){handleError("Negative place");return dynnote;}
      return MoveNote(block, track, note, &place, true);
    }
    
  } else if (velocitynum==num_velocities-1) {
    
    note->velocity_end = value2;
    if (!p_is_same_place(place)){
      if(place.line < 0){handleError("Negative place");return dynnote;}
      MoveEndNote(block, track, note, &place, true);
    }
    
  } else {

    r::VelocityTimeData::Writer writer(note->_velocities);
    r::Velocity &vel = writer.at_ref(velocitynum-1);
    
    vel._val = value2;
    
    //printf("Value for %d: %d (%p)\n", velocitynum-1, vel._val, &vel);
    
    if (!p_is_same_place(place)){
      
      Ratio ratio = place2ratio(place);
      
      if (ratio < 0) {
        
        handleError("Position before start of block");
        
      } else if (ratio > wblock->block->num_lines) {
        
        handleError("Position after end of block");
        
      } else {
        
        writer.constraint_move(velocitynum-1,
                               R_BOUNDARIES(place2ratio(note->l.p), ratio, note->end),
                               wblock->block->num_lines
                               );
        
      }
      
    }
  }

  return dynnote;
}

dyn_t setVelocityF(float value, float floatplace, int velocitynum, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  Place place;
  
  if (floatplace < 0) {
    R_ASSERT_NON_RELEASE(false); // Don't know if this is a legal situation.
    place = g_same_place;
  }else
    Float2Placement(floatplace, &place);
  return setVelocity(value, place, velocitynum, dynnote, tracknum, blocknum, windownum);
}

void deleteVelocity(int velocitynum, dyn_t dynnote, int tracknum, int blocknum, int windownum){
 struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note==NULL)
    return;

  struct Blocks *block=wblock->block;
  struct Tracks *track=wtrack->track;

  int num_velocities = getNumVelocities(dynnote, tracknum, blocknum, windownum);

  if (velocitynum < 0 || velocitynum>=num_velocities) {
    handleError("There is no velocity #%d in note %d in track #%d in block #%d",velocitynum, (int)note->id, tracknum, blocknum);
    return;
  }

  bool is_first          = velocitynum==0;
  bool is_last           = velocitynum==num_velocities-1;
  bool no_velocities     = num_velocities==2;

  if (is_first || (is_last && no_velocities)){

    SCOPED_PLAYER_LOCK_IF_PLAYING();
    RemoveNote(block, track, note);

  } else if (velocitynum==num_velocities-1) {
    int new_last_velocity;
    Ratio new_last_ratio;

    {
      r::VelocityTimeData::Writer writer(note->_velocities);
      new_last_velocity = writer.at_last()._val;
      new_last_ratio = writer.at_last()._time;
      writer.remove_at_pos(velocitynum-2);
    }
    
    {
      SCOPED_PLAYER_LOCK_IF_PLAYING();
      note->end = new_last_ratio;
      note->velocity_end = new_last_velocity;
    }

  } else {
    r::VelocityTimeData::Writer writer(note->_velocities);
    writer.remove_at_pos(velocitynum-1);
  }

  window->must_redraw_editor = true;
}

void setVelocityLogtype(int logtype, int velocitynum, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note==NULL)
    return;

  //struct Blocks *block=wblock->block;
  //struct Tracks *track=wtrack->track;

  int num_velocities = getNumVelocities(dynnote, tracknum, blocknum, windownum);
  if (velocitynum < 0 || velocitynum>=num_velocities) {
    handleError("There is no velocity #%d in note %d in track #%d in block# %d",velocitynum, (int)note->id, tracknum, blocknum);
    return;
  }

  bool is_first                      = velocitynum==0;
  bool is_last                       = velocitynum==num_velocities-1;

  if (is_last) {
    handleError("Can not set logtype for last velocity (doesn't make any sense)");
    return;
  }

  if (is_first) {
    
    note->velocity_first_logtype = logtype;
    
  } else {

    r::VelocityTimeData::Writer writer(note->_velocities);
    R_ASSERT_RETURN_IF_FALSE(writer.size() > 0);
    writer.at_ref(velocitynum-1)._logtype = logtype;
  
  }

  window->must_redraw_editor = true;
}
  

void setCurrentVelocityNode(int velocitynum, dyn_t dynnote, int tracknum, int blocknum, int windownum){
 struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note==NULL)
    return;

  int num_velocities = getNumVelocities(dynnote, tracknum, blocknum, windownum);
  if (velocitynum < 0 || velocitynum>=num_velocities) {
    handleError("There is no velocity #%d in note %d in track #%d in block# %d",velocitynum, (int)note->id, tracknum, blocknum);
    return;
  }

  if (velocitynum <= 0) {
    R_ASSERT_NON_RELEASE(velocitynum == 0);
    API_setCurrentNode2(NODETYPE_FIRST);
  } else if (velocitynum < num_velocities-1) {
    const r::VelocityTimeData::Reader reader(note->_velocities);
    API_setCurrentNode2(reader.at_ref(velocitynum-1)._id);
  }
  else {
    R_ASSERT_NON_RELEASE(velocitynum == num_velocities -1);
    API_setCurrentNode2(NODETYPE_LAST);
  }

}



void setIndicatorVelocityNode(int velocitynum, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note==NULL)
    return;

  int num_velocities = getNumVelocities(dynnote, tracknum, blocknum, windownum);
  if (velocitynum < 0 || velocitynum>=num_velocities) {
    handleError("There is no velocity #%d in note %d in track #%d in block# %d",velocitynum, (int)note->id, tracknum, blocknum);
    return;
  }

  API_setIndicatorNode(&note->l);
  g_indicator_velocity_num = velocitynum;
  //printf("   indicator vel: %d. Note: %p\n", velocitynum, &note->l);
  
  if (velocitynum > 0 && velocitynum < num_velocities-1)
    {
      const r::VelocityTimeData::Reader reader(note->_velocities);
      API_setIndicatorNode2(reader.at_ref(velocitynum-1)._id);
    }

}

