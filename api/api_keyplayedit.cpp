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

//#include "Python.h"


#if defined(__GNUC__) && !defined(__clang__)
#  include "../Qt/Qt_precompiled.hpp"
#endif

#include <stdbool.h>

#include <vector>

#include "../common/nsmtracker.h"
#include "../common/list_proc.h"
#include "../common/placement_proc.h"
#include "../common/patch_proc.h"
#include "../common/notes_proc.h"
#include "../common/cursor_updown_proc.h"
#include "../common/visual_proc.h"

#include "../Qt/Qt_bottom_bar_widget_proc.h"

#include "api_proc.h"
#include "api_common_proc.h"
#include "api_keyplayedit_proc.h"

#include "radium_proc.h"


extern struct TEvent tevent;
extern bool tevent_autorepeat;


/*******************************************
  Shared step-recording note input state
*******************************************/

namespace {

struct HeldNote {
  int source;
  int windownum;
  float notenum;
  float velocity;
  bool polyphonic;
};

static std::vector<HeldNote> g_held_notes;

static bool g_inserted_any = false;
static bool g_scroll_pending = false;
static int g_num_inserted = 0;
static struct Notes *g_last_inserted_note = NULL;
static struct WBlocks *g_group_wblock = NULL;
static struct Tracks *g_group_track = NULL;

static void reset_note_input_group(void){
  g_held_notes.clear();
  g_inserted_any = false;
  g_scroll_pending = false;
  g_num_inserted = 0;
  g_last_inserted_note = NULL;
  g_group_wblock = NULL;
  g_group_track = NULL;
}

static HeldNote *find_held_note(int source, float notenum){
  for(auto &note : g_held_notes)
    if (note.source==source && equal_floats(note.notenum, notenum))
      return &note;

  return NULL;
}

static void insert_held_note(struct Tracker_Windows *window, HeldNote &held){
  bool polyphonic = held.polyphonic || g_inserted_any;

  struct Notes *note = InsertNoteCurrPosNoScroll(window, held.notenum, polyphonic, held.velocity);

  if (note != NULL){
    g_inserted_any = true;
    g_scroll_pending = true;
    g_num_inserted++;
    g_last_inserted_note = note;
  }
}

} // anonymous namespace

void NoteInput_cancel_all(void){
  reset_note_input_group();
}

void NoteInput_note_on(int source, struct Tracker_Windows *window, float notenum, float velocity, bool polyphonic, bool is_repeat){
  if (ATOMIC_GET(root->editonoff)==false)
    return;

  if (window==NULL || window->curr_track<0 || window->wblock==NULL || window->wblock->wtrack==NULL)
    return;

  if (notenum<0.001 || notenum>127.9)
    return;

  if (doPolyphonicNoteInput()==false){
    NoteInput_cancel_all();
    InsertNoteCurrPos(window, notenum, polyphonic, velocity);
    return;
  }

  HeldNote *existing = find_held_note(source, notenum);
  if (existing != NULL){
    existing->velocity = velocity;
    existing->polyphonic = existing->polyphonic || polyphonic;

    if (is_repeat){
      HeldNote repeated = *existing;
      repeated.polyphonic = true;
      insert_held_note(window, repeated);
    }

    return;
  }

  bool start_group = g_held_notes.empty();

  if (!start_group && (window->wblock != g_group_wblock || window->wblock->wtrack->track != g_group_track)){
    // The cursor was moved to another block or track while notes were held.
    // Cancel the old group instead of inserting the held notes at the wrong place.
    reset_note_input_group();
    start_group = true;
  }

  if (start_group){
    reset_note_input_group();
    g_group_wblock = window->wblock;
    g_group_track = window->wblock->wtrack->track;
  }

  HeldNote held;
  held.source = source;
  held.windownum = window->l.num;
  held.notenum = notenum;
  held.velocity = velocity;
  held.polyphonic = polyphonic;

  g_held_notes.push_back(held);
  insert_held_note(window, g_held_notes.back());
}

void NoteInput_note_off(int source, struct Tracker_Windows *window, float notenum){
  if (doPolyphonicNoteInput()==false){
    NoteInput_cancel_all();
    return;
  }

  HeldNote *held = find_held_note(source, notenum);
  if (held == NULL)
    return;

  if (window==NULL)
    window = getWindowFromNum(held->windownum);

  if (window==NULL || window->wblock==NULL || window->wblock->wtrack==NULL){
    NoteInput_cancel_all();
    return;
  }

  if (g_group_wblock != NULL && (window->wblock != g_group_wblock || window->wblock->wtrack->track != g_group_track)){
    NoteInput_cancel_all();
    return;
  }

  for(auto it = g_held_notes.begin() ; it != g_held_notes.end() ; ++it){
    if (it->source==source && equal_floats(it->notenum, notenum)){
      g_held_notes.erase(it);
      break;
    }
  }

  if (g_held_notes.empty()){
    if (g_scroll_pending){
      const bool play_notes = g_num_inserted==1;
      MaybeScrollEditorDownAfterEditing(window,
                                        play_notes ? g_last_inserted_note : NULL,
                                        play_notes);
    }

    reset_note_input_group();
  }
}


/*******************************************
  Computer Keyboard Note Playing/Editing 
*******************************************/

void keyDownPlay(int notenum,int windownum)
{
	//printf("DOWN: doautorepeat: %d, autorepeat: %d\n", doAutoRepeat(), AutoRepeat(tevent.keyswitch));
  
	if (!doAutoRepeat() && tevent_autorepeat)
		return;

	struct Tracker_Windows *window=getWindowFromNum(windownum);

	notenum+=root->keyoct;

	if(notenum<=0 || notenum>127) return;
	if(window==NULL || window->curr_track<0) return;

	bool do_edit = ATOMIC_GET_RELAXED(root->editonoff);

	float velocity = -1.0;
	float note_velocity = -1.0;
	if (do_edit)
	{
		struct Tracks *track = window->wblock->wtrack->track;
		note_velocity = VELOCITY_get(NOTE_get_velocity(track));
		velocity = TRACK_get_volume(track) * note_velocity;
	}

	PATCH_playNoteCurrPos(window,notenum,-1, velocity);
	if(do_edit)
		NoteInput_note_on(NOTEINPUT_KEYBOARD, window, notenum, note_velocity, false, tevent_autorepeat);
}

void polyKeyDownPlay(int notenum,int windownum)
{
	//printf("POLY: doautorepeat: %d, autorepeat: %d\n", doAutoRepeat(), AutoRepeat(tevent.keyswitch));

	if (!doAutoRepeat() && tevent_autorepeat)
		return;

	struct Tracker_Windows *window=getWindowFromNum(windownum);

	notenum+=root->keyoct;

	if(notenum<=0 || notenum>127) return;
	if(window==NULL || window->curr_track<0) return;

	bool do_edit = ATOMIC_GET(root->editonoff);

	float velocity = -1.0;
	float note_velocity = -1.0;
	if (do_edit)
	{
		struct Tracks *track = window->wblock->wtrack->track;
		note_velocity = VELOCITY_get(NOTE_get_velocity(track));
		velocity = TRACK_get_volume(track) * note_velocity;
	}

	PATCH_playNoteCurrPos(window,notenum,-1, velocity);
	if(do_edit)
		NoteInput_note_on(NOTEINPUT_KEYBOARD, window, notenum, note_velocity, true, tevent_autorepeat);
}

void keyUpPlay(int notenum,int windownum)
{
	//printf("UP: doautorepeat: %d, autorepeat: %d\n", doAutoRepeat(), AutoRepeat(tevent.keyswitch));
	struct Tracker_Windows *window=getWindowFromNum(windownum);

	notenum+=root->keyoct;

	if(notenum<=0 || notenum>127){
		NoteInput_cancel_all();
		return;
	}

	if(window!=NULL && window->curr_track>=0)
		PATCH_stopNoteCurrPos(window,notenum,-1);

	NoteInput_note_off(NOTEINPUT_KEYBOARD, window, notenum);
}

void setKeyAdd(int addnum)
{
#if 0
	if(root->keyoct==addnum)
		root->keyoct=addnum+12;
	else if(root->keyoct==addnum+12 && addnum==72)
		root->keyoct=addnum+24;
	else if(root->keyoct==addnum+24 && addnum==72)
		root->keyoct=addnum+36;
	else if(root->keyoct==addnum+36 && addnum==72)
		root->keyoct=addnum+48;
	else
#endif
		root->keyoct=addnum;
    GFX_OS_UpdateKeyOctave();
}

void incKeyAdd(int incaddnum)
{
	int keyoct = root->keyoct + incaddnum;
	
	if(keyoct>127 || keyoct<0)
		return;

	setKeyAdd(keyoct);
}

void decKeyAdd(int decaddnum)
{
	incKeyAdd(-decaddnum);
}

bool switchDrunkVelocityOnOff(void)
{
	return EDITOR_switch_drunk_velocity();
}

void switchEditOnOff(void)
{
	struct Tracker_Windows *window=getWindowFromNum(-1);
	NoteInput_cancel_all();
	ATOMIC_SET(root->editonoff, ATOMIC_GET(root->editonoff)?false:true);
	char temp[1000];
	snprintf(temp,999,"Edit %s",ATOMIC_GET(root->editonoff)?"On":"Off");
	GFX_SetStatusBar(temp);
	window->must_redraw=true;
}

void switchSoundScrollOnOff(int windownum)
{
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

	window->playalong=window->playalong?false:true;
}

