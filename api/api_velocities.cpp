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
#if 0
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
#else
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  const r::NotePtr note = getNoteFromNumA2(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  
  if (!note)
    return NULL;

  const vector_t *nodes = GetVelocityNodes3(window, wblock, wtrack, note.get());
  if (velocitynum < 0 || velocitynum>=nodes->num_elements) {
    handleError("There is no velocity #%d in note %d in track #%d in block #%d",velocitynum, (int)note->_id, tracknum, blocknum);
    return NULL;
  }

  R_ASSERT_NON_RELEASE(r::VelocityTimeData::Reader(&note->_velocities).size()==nodes->num_elements-2);
  
  return (struct Node2*)nodes->elements[velocitynum];
#endif
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
  r::NotePtr note;

  const r::Velocity velocity = getVelocityFromNumA2(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote, note, velocitynum);

  if (!note)
    return 0;

  return (double)velocity._val / (double)MAX_VELOCITY;
}

Place getVelocityPlace(int velocitynum, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  r::NotePtr note;

  const r::Velocity velocity = getVelocityFromNumA2(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote, note, velocitynum);

  if (!note)
    return p_Create(0,0,1);

  return ratio2place(velocity._time);
}

int getVelocityLogtype(int velocitynum, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  r::NotePtr note;

  const r::Velocity velocity = getVelocityFromNumA2(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote, note, velocitynum);

  if (!note)
    return 0;

  return velocity._logtype;
}

static int get_num_velocities(const r::NotePtr &note){
  return r::VelocityTimeData::Reader(&note->_velocities).size() + 2;
}

int getNumVelocities(dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  r::NotePtr note = getNoteFromNumA2(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (!note)
    return 0;

  return get_num_velocities(note);
}

static int addVelocity2(float value, Place place, dyn_t dynnote, int tracknum, int blocknum, int windownum, bool show_errors){

  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  r::NotePtr note = getNoteFromNumA2(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (!note)
    return -1;

  Ratio ratio = place2ratio(place);
  
  if (ratio < note.get_time()){
    handleError("addVelocity: placement before note start for note. velocity: %s. note: %s", p_ToString(place), p_ToString(ratio2place(note->get_time())));
    return -1;
  }

  if (ratio > note->d._end) {
    handleError("addVelocity: placement after note end for note. velocity: %s. note end: %s", p_ToString(place), p_ToString(ratio2place(note->d._end)));
    return -1;
  }

  int ret = AddVelocity4(value*MAX_VELOCITY, ratio, note.get_mutable());

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
  r::NotePtr note = getNoteFromNumA2(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (!note)
    return dynnote;

  struct Blocks *block = wblock->block;
  struct Tracks *track = wtrack->track;

  int num_velocities = get_num_velocities(note);
  if (velocitynum < 0 || velocitynum>=num_velocities) {
    handleError("There is no velocity #%d in note %d in track #%d in block# %d",velocitynum, (int)note->_id, tracknum, blocknum);
    return dynnote;
  }

  window->must_redraw_editor = true;
  
  const Ratio ratio = place2ratio(place);
      
  //printf("velocitynum==%d. floatplace: %f\n",velocitynum,floatplace);

  int value2 = R_BOUNDARIES(0,value*MAX_VELOCITY,MAX_VELOCITY);
  
  if (velocitynum==0) {
    
    note->d._velocity = value2;
    if (!p_is_same_place(place)){
      if(place.line < 0){handleError("Negative place");return dynnote;}
      int64_t id = MoveNote2(block, track, note, ratio, true);
      fprintf(stderr, "api_velocities.cpp: Exit. Note: %p. id: %d\n", note.get(), int(id));
      if (id >= 0)
        return GetNoteIdFromNoteId(id);
    }
    
  } else if (velocitynum==num_velocities-1) {
    
    note->d._velocity_end = value2;
    if (!p_is_same_place(place)){
      if(place.line < 0){handleError("Negative place");return dynnote;}
      int64_t id = MoveEndNote2(block, track, note, ratio, true);
      if (id >= 0)
        return GetNoteIdFromNoteId(id);
    }
    
  } else {

    r::VelocityTimeData::Writer writer(&note->_velocities);

    int num_velocities = get_num_velocities(note);
    if (velocitynum < 0 || velocitynum>=num_velocities) {
      handleError("There is no velocity #%d in note %d in track #%d in block# %d",velocitynum, (int)note->_id, tracknum, blocknum);
      return dynnote;
    }

    r::Velocity &vel = writer.at_ref(velocitynum-1);
    
    vel._val = value2;
    
    //printf("Value for %d: %d (%p)\n", velocitynum-1, vel._val, &vel);
    
    if (!p_is_same_place(place)){
      
      if (ratio < 0) {
        
        handleError("Position before start of block");
        
      } else if (ratio > wblock->block->num_lines) {
        
        handleError("Position after end of block");
        
      } else {
        
        writer.constraint_move(velocitynum-1,
                               R_BOUNDARIES(note->get_time(), ratio, note->d._end),
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
  r::NotePtr note = getNoteFromNumA2(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (!note)
    return;

  struct Blocks *block=wblock->block;
  struct Tracks *track=wtrack->track;

  int num_velocities = get_num_velocities(note);

  if (velocitynum < 0 || velocitynum>=num_velocities) {
    handleError("There is no velocity #%d in note %d in track #%d in block #%d",velocitynum, (int)note->_id, tracknum, blocknum);
    return;
  }

  bool is_first          = velocitynum==0;
  bool is_last           = velocitynum==num_velocities-1;
  bool no_velocities     = num_velocities==2;

  if (is_first || (is_last && no_velocities)){

    r::NoteTimeData::Writer writer(wtrack->track->_notes2);
    RemoveNote2(block, track, writer, note);

  } else if (velocitynum==num_velocities-1) {
    int new_last_velocity;
    Ratio new_last_ratio;

    r::NoteTimeData::Writer writer(wtrack->track->_notes2);

    r::ModifyNote new_note(writer, note);
    
    {
      r::VelocityTimeData::Writer writer(&new_note->_velocities);
      new_last_velocity = writer.at_last()._val;
      new_last_ratio = writer.at_last()._time;
      writer.remove_at_pos(velocitynum-2);
    }
    
    new_note->d._end = new_last_ratio;
    new_note->d._velocity_end = new_last_velocity;

  } else {
    r::VelocityTimeData::Writer writer(&note->_velocities);

    int num_velocities = get_num_velocities(note);

    if (velocitynum < 0 || velocitynum>=num_velocities) {
      handleError("There is no velocity #%d in note %d in track #%d in block #%d",velocitynum, (int)note->_id, tracknum, blocknum);
      return;
    }

    writer.remove_at_pos(velocitynum-1);
  }

  window->must_redraw_editor = true;
}

void setVelocityLogtype(int logtype, int velocitynum, dyn_t dynnote, int tracknum, int blocknum, int windownum){
#if 0
  // DO_LATER
  
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
    R_ASSERT_RETURN_IF_FALSE(velocitynum < writer.size());
    writer.at_ref(velocitynum-1)._logtype = logtype;
  }

  window->must_redraw_editor = true;

#else
  
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  const r::NotePtr note = getNoteFromNumA2(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  
  if (!note)
    return;

  int num_velocities = get_num_velocities(note);
  
  if (velocitynum < 0 || velocitynum>=num_velocities) {
    handleError("There is no velocity #%d in note %d in track #%d in block #%d",velocitynum, (int)note->_id, tracknum, blocknum);
    return;
  }

  bool is_first                      = velocitynum==0;
  bool is_last                       = velocitynum==num_velocities-1;

  if (is_last) {
    handleError("Can not set logtype for last velocity (doesn't make any sense)");
    return;
  }

  if (is_first) {
    
    note->d._velocity_first_logtype = logtype;
    
  } else {

    r::VelocityTimeData::Writer writer(&note->_velocities);
    R_ASSERT_RETURN_IF_FALSE(velocitynum-1 < writer.size());
    writer.at_ref(velocitynum-1)._logtype = logtype;
    
  }

  window->must_redraw_editor = true;

#endif
}
  

void setCurrentVelocityNode(int velocitynum, dyn_t dynnote, int tracknum, int blocknum, int windownum){
 struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  r::NotePtr note = getNoteFromNumA2(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (!note)
    return;

  int num_velocities = get_num_velocities(note);
  if (velocitynum < 0 || velocitynum>=num_velocities) {
    handleError("There is no velocity #%d in note %d in track #%d in block# %d",velocitynum, (int)note->_id, tracknum, blocknum);
    return;
  }

  if (velocitynum <= 0) {
    R_ASSERT_NON_RELEASE(velocitynum == 0);
    API_setCurrentNode2(NODETYPE_FIRST);
  } else if (velocitynum < num_velocities-1) {
    const r::VelocityTimeData::Reader reader(&note->_velocities);
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
  r::NotePtr note = getNoteFromNumA2(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (!note)
    return;

  int num_velocities = get_num_velocities(note);
  if (velocitynum < 0 || velocitynum>=num_velocities) {
    handleError("There is no velocity #%d in note %d in track #%d in block# %d",velocitynum, (int)note->_id, tracknum, blocknum);
    return;
  }

  API_setIndicatorNode2(note->get_node_id());
  
  g_indicator_velocity_num = velocitynum;
  //printf("   indicator vel: %d. Note: %p\n", velocitynum, &note->l);
  
  if (velocitynum > 0 && velocitynum < num_velocities-1)
    {
      const r::VelocityTimeData::Reader reader(&note->_velocities);
      API_setIndicatorNode2(reader.at_ref(velocitynum-1)._id);
    }

}

