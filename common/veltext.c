
#include "nsmtracker.h"
#include "vector_proc.h"
#include "realline_calc_proc.h"


static void add_veltext(const struct WBlocks *wblock, vector_t *veltexts, const VelText *veltext){
  int realline = FindRealLineFor(wblock, 0, &veltext->p);      
  vector_t *v = &veltexts[realline];
  VECTOR_insert_place(v, &veltext->p);
}
                   

static void add_velocity(const struct WBlocks *wblock, vector_t *veltexts, const struct Notes *note, struct Velocities *velocity){
  VelText *tr = talloc(sizeof(VelText));
  tr->p = velocity->l.p;
  tr->value = velocity->velocity;
  tr->logtype = velocity->logtype;
  add_veltext(wblock, veltexts, tr);
}

static void add_note(const struct WBlocks *wblock, vector_t *veltexts, struct Notes *note){

  int last_velocity = note->velocity;

  {
    VelText *tr = talloc(sizeof(VelText));
    tr->p = note->l.p;
    tr->value = note->velocity;
    tr->logtype = note->velocity_first_logtype;
    tr->is_first_velocity = true;
    
    add_veltext(wblock, veltexts, tr);
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
    tr->note = note;
    tr->value = note->velocity_end;
    tr->logtype = LOGTYPE_IRRELEVANT;
    tr->is_last_velocity = true;
    add_veltext(wblock, veltexts, tr);
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
