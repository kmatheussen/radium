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



// pitches
//////////////////////////////////////////////////

float getPitchValue(int pitchnum, int notenum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = NULL;

  struct Pitches *pitch = getPitchFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, notenum, &note, pitchnum);

  if (pitch==NULL){
    if (note==NULL)
      return 0;

    else if (pitchnum==0)
      return note->note;

    else if (note->pitch_end==0 && note->pitches==NULL)
      return note->note;

    else
      return note->pitch_end;
  }

  return pitch->note;
}

Place getPitchPlace(int pitchnum, int notenum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = NULL;

  struct Pitches *pitch = getPitchFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, notenum, &note, pitchnum);

  if (pitch==NULL){

    if (note==NULL)
      return place(0,0,1);

    else if (pitchnum==0)
      return note->l.p;

    else
      return note->end;

  }

  return pitch->l.p;
}

int getPitchLogtype(int pitchnum, int notenum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = NULL;

  struct Pitches *pitch = getPitchFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, notenum, &note, pitchnum);

  if (pitch==NULL){

    if (note==NULL)
      return 0;

    else if (pitchnum==0)
      return note->pitch_first_logtype;

    else
      return 0; // Last logtype. Always 0. Irrelevant.
  }

  return pitch->logtype;
}

float getPitchChance(int pitchnum, int notenum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = NULL;

  struct Pitches *pitch = getPitchFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, notenum, &note, pitchnum);

  if (pitch==NULL){

    if (note==NULL)
      return 0;

    else if (pitchnum==0)
      return (double)note->chance / 256.0;

    else
      return 1; // Last chance. Always 1.
  }

  return (double)pitch->chance / 256.0;
}

int getNumPitches(int notenum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, notenum);
  if (note==NULL)
    return 0;

  return 2+ListFindNumElements3(&note->pitches->l);
}

void deletePitch(int pitchnum, int notenum, int tracknum, int blocknum, int windownum){
  deletePianonote(pitchnum, notenum, tracknum, blocknum, windownum); // Think this is correct.
}

void setPitchLogtypeHolding(bool is_holding, int pitchnum, int notenum, int tracknum, int blocknum, int windownum){
  setPianonoteLogtypeHolding(is_holding, pitchnum, notenum, tracknum, blocknum, windownum); // Think this is correct
}




