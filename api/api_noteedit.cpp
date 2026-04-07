/* Copyright 2001 Kjetil S. Matheussen

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

#include "Python.h"

#include "../common/nsmtracker.h"
#include "../common/TimeData.hpp"
#include "../common/list_proc.h"
#include "../common/placement_proc.h"
#include "../common/velocities_proc.h"
#include "../common/windows_proc.h"
#include "../common/wtracks_proc.h"
#include "../common/tracks_proc.h"
#include "../common/notes_proc.h"
#include "../common/player_proc.h"
#include "../common/player_pause_proc.h"
#include "../common/wblocks_proc.h"
#include "../common/settings_proc.h"
#include "../common/undo_notes_proc.h"
#include "../common/visual_proc.h"

#include "../embedded_scheme/s7extra_proc.h"

#include "api_common_proc.h"
#include "api_support_proc.h"
#include "radium_proc.h"

#include "api_noteedit_proc.h"



extern char *NotesTexts2[];
extern char *NotesTexts3[];


const_char *getNoteName2(int notenum){
  if (notenum<0 || notenum>127)
    return "";
  else
    return NotesTexts2[notenum];
}

const_char *getNoteName3(int notenum){
  if (notenum<0 || notenum>127)
    return "";
  else
    return NotesTexts3[notenum];
}

float getNoteNameValue(const_char *notename){
  return notenum_from_notetext(notename);
}

int getPianorollLowKey(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0;
  return wtrack->pianoroll_lowkey;
}

int getPianorollHighKey(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 127;
  return wtrack->pianoroll_highkey;
}

void setPianorollRange(int minkey, int maxkey, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;
  
  int diff = maxkey - minkey;
  if (diff < 5)
    minkey = maxkey - 5;

  if (minkey < 0) {
    minkey = 0;
    maxkey = 5;
  }

  if (maxkey > 127) {
    minkey = 122;
    maxkey = 127;
  }
  
  wtrack->pianoroll_lowkey = minkey;
  wtrack->pianoroll_highkey = maxkey;

  UpdateWBlockCoordinates(window,wblock);
  window->must_redraw=true;
}
  
void setPianorollLowKey(int key, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;

  int newkey = R_BOUNDARIES(0, key, 127);

  if (wtrack->pianoroll_highkey - newkey < 5)
    return;
  
  wtrack->pianoroll_lowkey = newkey;
  
  UpdateWBlockCoordinates(window,wblock);
  window->must_redraw=true;
}

void setPianorollHighKey(int key, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;

  int newkey = R_BOUNDARIES(0, key, 127);

  if (newkey - wtrack->pianoroll_lowkey < 5)
    return;
  
  wtrack->pianoroll_highkey = newkey;

  UpdateWBlockCoordinates(window,wblock);
  window->must_redraw=true;
}

void setPianorollAutoRange(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;

  S7CALL2(void_int_int,"FROM_C-set-pianoroll-autorange", wtrack->l.num, wblock->l.num);
}

float getLowestKey(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return -1;

  return TRACK_get_min_pitch(wtrack->track);
}

float getHighestKey(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return -1;

  return TRACK_get_max_pitch(wtrack->track);
}


void incNoteVolume(int incvolume,int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

	IncreaseVelocityCurrPos(window,incvolume);
}

static bool g_scrollplay = false;

bool doScrollPlay(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_scrollplay = SETTINGS_read_bool("scroll_play", true);
    has_inited = true;
  }

  return g_scrollplay;
}

void setScrollPlay(bool doit){
  g_scrollplay = doit;
  SETTINGS_write_bool("scroll_play", doit);
}
                          
void switchScrollPlayOnOff(void){
  setScrollPlay(!doScrollPlay());
}



static bool g_do_scroll_edit_lines = true;

bool doScrollEditLines(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_do_scroll_edit_lines = SETTINGS_read_bool("arrow_keys_scroll_edit_lines", g_do_scroll_edit_lines);
    has_inited = true;
  }

  return g_do_scroll_edit_lines;
}

void setScrollEditLines(bool doit){
  g_do_scroll_edit_lines = doit;
  SETTINGS_write_bool("arrow_keys_scroll_edit_lines", doit);
}


static bool g_do_autorepeat = false;

bool doAutoRepeat(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_do_autorepeat = SETTINGS_read_bool("keyboard_autorepeat", false);
    has_inited = true;
  }

  return g_do_autorepeat;
}

void setAutoRepeat(bool doit){
  g_do_autorepeat = doit;
  SETTINGS_write_bool("keyboard_autorepeat", doit);
}


static bool g_do_addNotesWhenReleasingKeys = true;

bool doAddNotesWhenReleasingKeys(void){
  static bool has_inited = false;

  if (has_inited==false)
  {
	  g_do_addNotesWhenReleasingKeys = SETTINGS_read_bool("add_notes_when_releasing_keys", true);
	  has_inited = true;
  }

  return g_do_addNotesWhenReleasingKeys;
}

void setAddNotesWhenReleasingKeys(bool doit){
  g_do_addNotesWhenReleasingKeys = doit;
  SETTINGS_write_bool("add_notes_when_releasing_keys", doit);
}


static bool g_do_range_paste_cut = true;

bool doRangePasteCut(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_do_range_paste_cut = SETTINGS_read_bool("range_paste_cut", true);
    has_inited = true;
  }

  return g_do_range_paste_cut;
}

void setRangePasteCut(bool doit){
  g_do_range_paste_cut = doit;
  SETTINGS_write_bool("range_paste_cut", doit);
}


static bool g_do_range_paste_scroll_down = true;

bool doRangePasteScrollDown(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_do_range_paste_scroll_down = SETTINGS_read_bool("range_paste_scroll_down", true);
    has_inited = true;
  }

  return g_do_range_paste_scroll_down;
}

void setRangePasteScrollDown(bool doit){
  g_do_range_paste_scroll_down = doit;
  SETTINGS_write_bool("range_paste_scroll_down", doit);
}



int g_downscroll = 1;

void setNoteScrollLength(int l){
  if (l<0){
    handleError("setNoteScrollLength: argument must be 0 or higher. Found %d", l);
    return;
  }
  
  if (l != g_downscroll){
  
    g_downscroll = l;

    GFX_OS_update_bottombar();
  }
}

int getNoteScrollLength(void){
  return g_downscroll;
}




/****************** notes **********************/

int getNoteVolume(int windownum,int blocknum,int tracknum,dyn_t dynnote){
	struct Notes *note=getNoteFromNum(windownum,blocknum,tracknum,dynnote);

	if(note==NULL) return -1;

  return note->velocity;
}

int getNumNotes(int tracknum,int blocknum,int windownum){
	struct WTracks *wtrack=getWTrackFromNum(windownum,blocknum,tracknum);

	if(wtrack==NULL) return 0;
	if(wtrack->track->notes==NULL) return 0;

	return ListFindNumElements3((struct ListHeader3*)wtrack->track->notes);
}

bool noteContinuesNextBlock(dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note==NULL)
    return false;

  return note_continues_next_block(wblock->block, note);
}

void setNoteContinueNextBlock(bool continuenextblock, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note==NULL)
    return;
  
  ADD_UNDO(Notes(window,
                 wblock->block,
                 wtrack->track,
                 wblock->curr_realline
                 )
           );

  note->noend = continuenextblock?1:0;
}

int addNote(float notevalue,
            float velocity,
            Place start,
            Place end,
            int tracknum, int blocknum, int windownum)
{
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);

  if(wtrack==NULL)
    return -1;

  if (notevalue < 0.001){
    handleError("addNote: Pitch less than 0.001: %f\n", notevalue);
    return -1;
  }

  if (start.line==0 && start.counter==1 && start.dividor==MAX_UINT32)
    start.counter = 0;

  if (validate_place(start)==false)
    return -1;
        
  if (!PlaceLegal(wblock->block, &start)) {
    handleError("addNote: Start place %d + %d/%d is not legal", start.line, start.counter, start.dividor);
    return -1;
  }

  if (validate_place(end)==false)
    return -1;
  
  if (end.line==wblock->block->num_lines && end.counter==0)
    PlaceSetLastPos(wblock->block, &end);
  
  if (!PlaceLegal(wblock->block, &end)) {
    handleError("addNote: End place %d + %d/%d is not legal", end.line, end.counter, end.dividor);
    return -1;
  }
  
  if (PlaceLessOrEqual(&end, &start)){
    handleError("addNote: Note end (%s) positioned before or at note start (%s)", PlaceToString(&end), PlaceToString(&start));
    return -1;
  }

  struct Notes *note = InsertNote(wblock,
                                  wtrack,
                                  &start,
                                  &end,
                                  notevalue,
                                  MAX_VELOCITY*velocity,
                                  true);

  window->must_redraw=true; // must use must_redraw and not must_redraw_editor since polyphony might have changed.

  return ListFindElementPos3(&wtrack->track->notes->l,&note->l);
}

int addNote3(float notevalue,float velocity,
             int line,int counter,int dividor,
             int end_line,int end_counter,int end_dividor,
             int tracknum, int blocknum, int windownum)
{
  Place start = p_Create(line,counter,dividor);

  Place end = p_Create(end_line, end_counter, end_dividor);

  return addNote(notevalue, velocity, start, end, tracknum, blocknum, windownum);
}

void cutNote(Place place, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note==NULL)
    return;

  if (place2ratio(place) >= note->end)
    return;
  
  if (PlaceLessOrEqual(&place, &note->l.p))
    return;

  CutNoteAt(wblock->block, wtrack->track, note, &place);
}

void deleteNote(dyn_t dynnote, int tracknum, int blocknum, int windownum){
  deletePianonote(0, dynnote, tracknum, blocknum, windownum);
}

void deleteAllNotesInTrack(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;

  undoNotes(tracknum, blocknum);
  
  {
    SCOPED_PLAYER_LOCK_IF_PLAYING();
    wtrack->track->notes = NULL;
  }


  window->must_redraw=true; // must use must_redraw and not must_redraw_editor since polyphony might have changed.
}

void deleteAllNotesAndStopsInTrack(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;

  undoNotes(tracknum, blocknum);
  
  {
    SCOPED_PLAYER_LOCK_IF_PLAYING();
    wtrack->track->notes = NULL;    
    //wtrack->track->stops = NULL;
  }

  r::StopTimeData::Writer writer(wtrack->track->stops2, true);

  window->must_redraw=true; // must use must_redraw and not must_redraw_editor since polyphony might have changed.
}

void deleteAllStopsInTrack(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;

  undoNotes(tracknum, blocknum);

  /*
  {
    SCOPED_PLAYER_LOCK_IF_PLAYING();
    wtrack->track->stops = NULL;
  }
  */
  
  r::StopTimeData::Writer writer(wtrack->track->stops2, true);

  window->must_redraw_editor=true;
}

// notes
//////////////////////////////////////////////////

void undoNotes(int tracknum, int blocknum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(-1, &window, blocknum, &wblock, tracknum);
  if(wtrack==NULL)
    return;
  ADD_UNDO(Notes(window,wblock->block,wtrack->track,wblock->curr_realline));
}

const_char* getNoteId(int notenum, int tracknum, int blocknum, int windownum){
  struct Notes *note=getNoteFromNum(windownum,blocknum,tracknum,DYN_create_int(notenum));
  if (note==NULL)
    return "";
  
  return GetNoteIdAsCharString(note->id);
}

int getNoteNum(dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(-1, &window, blocknum, &wblock, tracknum);
  if(wtrack==NULL)
    return -1;
  
  struct Notes *note=getNoteFromNum(windownum,blocknum,tracknum,dynnote);
  if (note==NULL)
    return -1;

  int i = 0;
  struct Notes *notes = wtrack->track->notes;
  while(notes != NULL){
    if (notes==note)
      return i;
    i++;
    notes = NextNote(notes);
  }

  handleError("getNoteNum: Note not found in track");
  return -1;
}

Place getNoteStart(dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Notes *note=getNoteFromNum(windownum,blocknum,tracknum,dynnote);

  if(note==NULL)
    return p_Create(0,0,1);

  return note->l.p;
}

Place getNoteEnd(dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Notes *note=getNoteFromNum(windownum,blocknum,tracknum,dynnote);

  if(note==NULL)
    return p_Create(1,0,1);

  return ratio2place(note->end);
}

float getNoteValue(dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Notes *note=getNoteFromNum(windownum,blocknum,tracknum,dynnote);

  if(note==NULL)
    return 64.0f;

  return note->note;
}

float getNoteEndPitch(dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Notes *note=getNoteFromNum(windownum,blocknum,tracknum,dynnote);

  if(note==NULL)
    return 0;

  return note->pitch_end;
}

int getNoteSubtrack(dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note==NULL)
    return 0;

  return GetNoteSubtrack(wtrack, note);
}

void setNoMouseNote(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if(wblock==NULL)
    return;
  
  if (wblock->mouse_note != NULL){
    wblock->mouse_note = NULL;
    window->must_redraw_editor = true;
    //printf("no mouse note dirty\n");
  }
}

void setMouseNote(dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note==NULL)
    return;
  else if (wblock->mouse_note != note){
    wblock->mouse_note = note;
    window->must_redraw_editor = true;
    //printf("mouse note dirty\n");
  }
}

void updateNotesInPlayer(void){
  PC_PauseNoMessage();
  PC_StopPause(NULL);
}


dynvec_t getAllNotes(int tracknum, int blocknum, int windownum){
  dynvec_t ret = {};

  const struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  if (wtrack!=NULL){

    struct Notes *note = wtrack->track->notes;
    while(note != NULL){
      DYNVEC_push_back(&ret, GetNoteId(note));
      note = NextNote(note);
    }
  }
  
  return ret;
}


/* Select/unselect notes */

#include <QHash>
#include <QSet>

void unselectAllNotes(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  if (wtrack==NULL)
    return;

  struct Notes *notes = wtrack->track->notes;
  while(notes!=NULL){
    notes->pianonote_is_selected = false;
    notes = NextNote(notes);
  }

  root->song->tracker_windows->must_redraw=true;
}

bool API_note_is_selected(const struct WBlocks *wblock, const struct Notes *note){
  R_ASSERT_RETURN_IF_FALSE2(wblock!=NULL,false);
  R_ASSERT_RETURN_IF_FALSE2(note!=NULL,false);
  return note->pianonote_is_selected;
}

bool noteIsSelected(dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  const struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note==NULL)
    return false;

  return note->pianonote_is_selected;
  //return API_note_is_selected(wblock, note);
}

void selectNote(dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note==NULL)
    return;
  
  if (note->pianonote_is_selected==true)
    return;
  
  note->pianonote_is_selected = true;
  
  window->must_redraw=true;
}

void unselectNote(dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note==NULL)
    return;

  if (note->pianonote_is_selected==false)
    return;
  
  note->pianonote_is_selected = false;

  window->must_redraw=true;
}

dynvec_t getSelectedNotes(int tracknum, int blocknum, int windownum){
  dynvec_t ret = {};

  const struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  if (wtrack!=NULL){

    struct Notes *note = wtrack->track->notes;
    while(note != NULL){
      if (note->pianonote_is_selected)
        DYNVEC_push_back(&ret, GetNoteId(note));
      note = NextNote(note);
    }
  }
  
  return ret;
}

bool hasSelectedNotes(int tracknum, int blocknum, int windownum){
  const struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  if (wtrack!=NULL){

    struct Notes *note = wtrack->track->notes;
    while(note != NULL){
      if (note->pianonote_is_selected)
        return true;

      note = NextNote(note);
    }
  }
  
  return false;
}
