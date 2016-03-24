
#include <math.h>

#include "nsmtracker.h"
#include "vector_proc.h"
#include "realline_calc_proc.h"
#include "undo.h"
#include "undo_notes_proc.h"
#include "list_proc.h"
#include "notes_proc.h"
#include "velocities_proc.h"
#include "cursor_updown_proc.h"

extern int g_downscroll;


static void add_veltext(const struct WBlocks *wblock, vector_t *veltexts, VelText *veltext, struct Notes *note, int velocity){
  int realline = FindRealLineFor(wblock, 0, &veltext->p);      
  vector_t *v = &veltexts[realline];
  
  veltext->value = round(scale_double(velocity, 0, MAX_VELOCITY, 0, 0xff));
  veltext->note = note;
  
  VECTOR_insert_place(v, &veltext->p);
}
                   

static void add_velocity(const struct WBlocks *wblock, vector_t *veltexts, struct Notes *note, struct Velocities *velocity){
  VelText *tr = talloc(sizeof(VelText));
  tr->p = velocity->l.p;
  tr->velocity = velocity;
  tr->logtype = velocity->logtype;
  add_veltext(wblock, veltexts, tr, note, velocity->velocity);
}

static void add_note(const struct WBlocks *wblock, vector_t *veltexts, struct Notes *note){

  int last_velocity = note->velocity;

  {
    VelText *tr = talloc(sizeof(VelText));
    tr->p = note->l.p;
    tr->logtype = note->velocity_first_logtype;
    tr->is_first_velocity = true;
    
    add_veltext(wblock, veltexts, tr, note, note->velocity);
  }

  struct Velocities *velocity = note->velocities;
  while(velocity != NULL){
    add_velocity(wblock, veltexts, note, velocity);
    last_velocity = velocity->velocity;
    velocity = NextVelocity(velocity);
  }

  if (last_velocity != note->velocity_end)  {
    VelText *tr = talloc(sizeof(VelText));
    tr->p = note->end;
    tr->logtype = LOGTYPE_IRRELEVANT;
    tr->is_last_velocity = true;
    add_veltext(wblock, veltexts, tr, note, note->velocity_end);
  }
}


// Returns a pointer to AN ARRAY of vectors (one vector for each realline), not a pointer to a vector (as one would think).
vector_t *VELTEXTS_get(const struct WBlocks *wblock, const struct WTracks *wtrack){
  int num_reallines = wblock->num_reallines;
  vector_t *veltexts = talloc(sizeof(vector_t) * num_reallines);

  struct Notes *note = wtrack->track->notes;
  while(note!=NULL){
    add_note(wblock, veltexts, note);
    note = NextNote(note);
  }

  return veltexts;
}


// We circumvent the normal keyboard configuration system here.
bool VELTEXT_keypress(struct Tracker_Windows *window, int key, bool is_keydown){
  if (window->curr_track < 0)
    return false;

  struct WBlocks *wblock = window->wblock;
  struct WTracks *wtrack = wblock->wtrack;
  
  if (wtrack->veltext_on == false)
    return false;

  int curr_track_sub = window->curr_track_sub;
    
  if (curr_track_sub < 0)
    return false;

  if (curr_track_sub > 2)
    return false;

  int val = -1;
  
  switch (key){ 
  case EVENT_DEL: val = 0; break;
  case EVENT_RETURN: val = 8; break;
  case EVENT_0: val = 0; break;
  case EVENT_1: val = 1; break;
  case EVENT_2: val = 2; break;
  case EVENT_3: val = 3; break;
  case EVENT_4: val = 4; break;
  case EVENT_5: val = 5; break;
  case EVENT_6: val = 6; break;
  case EVENT_7: val = 7; break;
  case EVENT_8: val = 8; break;
  case EVENT_9: val = 9; break;    
  case EVENT_A: val = 10; break;
  case EVENT_B: val = 11; break;
  case EVENT_C: val = 12; break;
  case EVENT_D: val = 13; break;
  case EVENT_E: val = 14; break;
  case EVENT_F: val = 15; break;
  case EVENT_X: val = 15; break;
  }

  if (val==-1)
    return false;

  if (is_keydown==false)
    return true;

  vector_t *veltexts = VELTEXTS_get(wblock, wtrack);

  int realline = wblock->curr_realline;
  Place *place = &wblock->reallines[realline]->l.p;

  vector_t *veltext = &veltexts[realline];

  if (veltext->num_elements == 0 && val==0)
    return true;
  
  Undo_Notes_CurrPos(window);  

  if (veltext->num_elements > 1) {
    if (key == EVENT_DEL){
      
      VECTOR_FOR_EACH(VelText *vt, veltext){
        struct Notes *note = vt->note;
        if (vt->velocity != NULL)
          ListRemoveElement3(&note->velocities, &vt->velocity->l);
      }END_VECTOR_FOR_EACH;
      
    } else {
      
      Undo_CancelLastUndo();
      
    }
    
  } else if (veltext->num_elements == 0){
    
    struct Notes *note = FindNote(wtrack->track, place);

    if (note == NULL){
      
      Undo_CancelLastUndo();
      
    } else {
      int logtype = LOGTYPE_LINEAR;
        
      int value = 0;
      if (curr_track_sub == 0) {
        value = scale(val * 0x10, 0, 0xff, 0, MAX_VELOCITY);
      } else if (curr_track_sub == 1) {
        value = scale(val, 0, 0xff, 0, MAX_VELOCITY);
      } else if (curr_track_sub == 2) {
        value = NOTE_get_velocity(wtrack->track);
        logtype=LOGTYPE_HOLD;
      } else
        RError("Unknown curr_track_sub: %d",curr_track_sub);

      struct Velocities *velocity = AddVelocity2(value, place, note);
      velocity->logtype = logtype;
    }

  } else {

    VelText *vt = veltext->elements[0];
    struct Notes *note = vt->note;
    struct Velocities *velocity = vt->velocity;
    int logtype = -10000;
  
    if (key == EVENT_DEL && velocity != NULL) {

      if (curr_track_sub == 2)
        velocity->logtype = LOGTYPE_LINEAR;
      else
        ListRemoveElement3(&note->velocities, &vt->velocity->l);
      
    } else {
    
      int v1 = (vt->value & 0xf0) / 0x10;
      int v2 = vt->value & 0x0f;

    
      if (curr_track_sub == 0) {
        v1 = val;
      } else if (curr_track_sub == 1) {
        v2 = val;
      } else if (curr_track_sub == 2) {
        //printf("todo\n");
        if (val == 0)
          logtype=LOGTYPE_LINEAR;
        else
          logtype=LOGTYPE_HOLD;
      } else
        RError("Unknown curr_track_sub: %d",curr_track_sub);
      
      int v = v1 * 0x10 + v2;
      int scaled = round(scale_double(v, 0, 0xff, 0, MAX_VELOCITY));
      
      printf("v1: %x, v2: %x, val: %x, v: %x\n",v1,v2,val,v);
      
      if (vt->is_first_velocity){
        
        note->velocity = scaled;
        if (logtype!=-10000)
          note->velocity_first_logtype = logtype;
        
      } else if (vt->is_last_velocity){
        
        note->velocity_end = scaled;
        
      } else {
        
        velocity->velocity = scaled;
        if (logtype!=-10000)
          velocity->logtype = logtype;
        
      }
    }
    
  }

  window->must_redraw_editor = true;

  if(!is_playing())
    ScrollEditorDown(window,g_downscroll);

  return true;  
}

