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
#include "../common/list_proc.h"
#include "../common/placement_proc.h"
#include "../common/velocities_proc.h"
#include "../common/windows_proc.h"
#include "../common/wtracks_proc.h"
#include "../common/tracks_proc.h"
#include "../common/notes_proc.h"
#include "../common/tempos_proc.h"
#include "../common/LPB_proc.h"
#include "../common/Signature_proc.h"
#include "../common/time_proc.h"
#include "../advanced/ad_noteadd_proc.h"
#include "../common/player_proc.h"
#include "../common/undo_maintempos_proc.h"
#include "../common/Beats_proc.h"
#include "../common/wblocks_proc.h"
#include "../common/settings_proc.h"

#include "api_common_proc.h"
#include "api_support_proc.h"




extern struct Root *root;

extern char *NotesTexts2[];
extern char *NotesTexts3[];


char *getNoteName2(int notenum){
  if (notenum<0 || notenum>127)
    return "";
  else
    return NotesTexts2[notenum];
}

char *getNoteName3(int notenum){
  if (notenum<0 || notenum>127)
    return "";
  else
    return NotesTexts3[notenum];
}

float getNoteNameValue(char *notename){
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

int getNoteNote(int windownum,int blocknum,int tracknum,int notenum){
	struct Notes *note=getNoteFromNum(windownum,blocknum,tracknum,notenum);

	if(note==NULL) return -1;

  return note->note;
}

void putNoteNote(int notenote,int windownum,int blocknum,int tracknum,int notenum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  struct Notes *note=getNoteFromNum(windownum,blocknum,tracknum,notenum);

  if(window==NULL) return;
  if(note==NULL) return;

  note->note=notenote;

  UpdateAndClearSomeTrackReallinesAndGfxWTracks(
						window,
						window->wblock,
						0,
						window->wblock->block->num_tracks-1
						);
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



static bool g_do_scroll_edit_lines = false;

bool doScrollEditLines(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_do_scroll_edit_lines = SETTINGS_read_bool("arrow_keys_scroll_edit_lines", false);
    has_inited = true;
  }

  return g_do_scroll_edit_lines;
}

void setScrollEditLines(bool doit){
  g_do_scroll_edit_lines = doit;
  SETTINGS_write_bool("arrow_keys_scroll_edit_lines", doit);
}
                          
extern int g_downscroll;
void setNoteScrollLength(int l){
  R_ASSERT_RETURN_IF_FALSE(l>=0);
  g_downscroll = l;
}

int getMaxVolume(){
  return MAX_VELOCITY;
}

int getNoteVolume(int windownum,int blocknum,int tracknum,int notenum){
	struct Notes *note=getNoteFromNum(windownum,blocknum,tracknum,notenum);

	if(note==NULL) return -1;

  return note->velocity;
}

// deprecated
float getNotePlace(int windownum,int blocknum,int tracknum,int notenum){
  struct Notes *note=getNoteFromNum(windownum,blocknum,tracknum,notenum);

  if(note==NULL)
    return -1.0f;

  return GetfloatFromPlace(&note->l.p);
}

// deprecated
float getNoteEndPlace(int windownum,int blocknum,int tracknum,int notenum){
  struct Notes *note=getNoteFromNum(windownum,blocknum,tracknum,notenum);

  if(note==NULL)
    return -1.0f;

  return GetfloatFromPlace(&note->end);
}


int getNumNotes(int tracknum,int blocknum,int windownum){
	struct WTracks *wtrack=getWTrackFromNum(windownum,blocknum,tracknum);

	if(wtrack==NULL) return 0;
	if(wtrack->track->notes==NULL) return 0;

	return ListFindNumElements3(&wtrack->track->notes->l);
}

void setSignature(int numerator, int denominator){
  if (numerator<=0 || denominator<=0)
    return;
  if (numerator==root->signature.numerator && denominator==root->signature.denominator)
    return;
  
  PlayStop();

  struct Tracker_Windows *window = root->song->tracker_windows;
  struct WBlocks *wblock = window->wblock;

  Undo_MainTempo(window,wblock);
  
  root->signature = ratio(numerator, denominator);
  UpdateAllBeats();
  
  window->must_redraw = true;
}

void setLPB(int lpb_value){
  if (lpb_value <=1)
    return;
  if (lpb_value == root->lpb)
    return;
  
  PlayStop();

  struct Tracker_Windows *window = root->song->tracker_windows;
  struct WBlocks *wblock = window->wblock;

  printf("Undo MainTempo lpb: %d\n",lpb_value);
  Undo_MainTempo(window,wblock);
  
  root->lpb=lpb_value;
  UpdateAllSTimes();
  UpdateAllBeats();
  
  //UpdateAllWLPBs(window);
  window->must_redraw = true;
}

void setBPM(int bpm_value){
  if (bpm_value <=1)
    return;
  if (bpm_value == root->tempo)
    return;

  PlayStop();

  struct Tracker_Windows *window = root->song->tracker_windows;
  struct WBlocks *wblock = window->wblock;
  
  Undo_MainTempo(window,wblock);
  
  root->tempo=bpm_value;
  UpdateAllSTimes();
}

int addSignature(int numerator, int denominator,
                 int line,int counter,int dividor,
                 int blocknum)
{
  struct WBlocks *wblock=getWBlockFromNum(-1,blocknum);
  if(wblock==NULL) {
    RError("unknown block(%p)",blocknum);
    return -1;
  }

  Place dasplace = place(line,counter,dividor);

  struct Signatures *signature = SetSignature(wblock->block,&dasplace,ratio(numerator, denominator));

  wblock->block->is_dirty = true;

  return ListFindElementPos3(&wblock->block->signatures->l,&signature->l);
}

int addLPB(int lpb_value,
           int line,int counter,int dividor,
           int blocknum)
{
  struct WBlocks *wblock=getWBlockFromNum(-1,blocknum);
  if(wblock==NULL) {
    RError("unknown block(%p)",blocknum);
    return -1;
  }

  Place *place = PlaceCreate(line,counter,dividor);

  struct LPBs *lpb = SetLPB(wblock->block,place,lpb_value);

  wblock->block->is_dirty = true;

  return ListFindElementPos3(&wblock->block->lpbs->l,&lpb->l);
}

int addBPM(int bpm,
           int line,int counter,int dividor,
           int blocknum)
{
  struct WBlocks *wblock=getWBlockFromNum(-1,blocknum);
  if(wblock==NULL) {
    RError("unknown block(%p)",blocknum);
    return -1;
  }

  Place *place = PlaceCreate(line,counter,dividor);

  struct Tempos *tempo = SetTempo(wblock->block,place,bpm);

  wblock->block->is_dirty = true;

  return ListFindElementPos3(&wblock->block->tempos->l,&tempo->l);
}


void setNoteEndPlace(int line,int counter,int dividor,int windownum,int blocknum,int tracknum,int notenum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  struct Notes *note=getNoteFromNum(windownum,blocknum,tracknum,notenum);

  if(window==NULL) return;
  if(note==NULL) return;

  PlaceCopy(&note->end, PlaceCreate(line,counter,dividor));
}

int addNote(int notenum,int velocity,
            int line,int counter,int dividor,
            int end_line,int end_counter,int end_dividor, 
            int windownum, int blocknum, int tracknum)
{
  struct WBlocks *wblock=getWBlockFromNum(windownum,blocknum);
  struct WTracks *wtrack=getWTrackFromNum(windownum,blocknum,tracknum);
  if(wblock==NULL || wtrack==NULL) {
    RError("unknown wblock(%p) or wtrack(%p) %d/%d/%d\n",wblock,wtrack,windownum,blocknum,tracknum);
    return -1;
  }

  struct Notes *note = InsertNote(wblock,
                                  wtrack,
                                  PlaceCreate(line,counter,dividor),
                                  end_line==-1 ? NULL : PlaceCreate(end_line,end_counter,end_dividor),
                                  notenum,
                                  velocity,
                                  true);

  wblock->block->is_dirty = true;

  return ListFindElementPos3(&wtrack->track->notes->l,&note->l);
}

void addNoteAdds(
	PyObject *noteadds,
	int windownum,
	int blocknum,
	int tracknum,
	float startplace,
	int sort
){
	struct Tracker_Windows *window;
	struct WBlocks *wblock;

	PyObject **pyobjects;
	int num_pyobjects;

	struct NoteAdds_track *nats;
	struct NoteAdds_track_do *nat_do=NULL;
	struct NoteAdds_block *nab;
	int num_nats;
	int lokke;

	const int attrformat[]={1,0,1,1};
	char *attrnames[]={"place","notenum","volume","endplace"};

//	printf("tracknum: %d\n",tracknum);

	wblock=getWBlockFromNumA(
		windownum,
		&window,
		blocknum
	);
	if(wblock==NULL){
		printf("wblock==NULL\n");
		return;
	}

	pyobjects=PYR_getPYOArray(&num_pyobjects,noteadds);
	if(num_pyobjects==-1){
		printf("Somethings wrong 1\n");
		return;
	}
	if(num_pyobjects==0) return;


	nab=talloc(sizeof(struct NoteAdds_block));
	nab->blocknum=blocknum;
	nab->num_nats_do=num_pyobjects;
	nab->nats_do=talloc(sizeof(struct NoteAdds_track_do *)*num_pyobjects);

	for(lokke=0;lokke<num_pyobjects;lokke++){
		nats=PYR_getObjArray(&num_nats,pyobjects[lokke],4,attrformat,attrnames);

		if(num_nats==-1){
			printf("Somethings wrong 2\n");
			return;
		}

		nat_do=talloc(sizeof(struct NoteAdds_track_do));
		nat_do->tracknum=tracknum;
		nat_do->num_nats=num_nats;
		nat_do->nats=nats;
		nat_do->startplace=startplace;
		nat_do->sort=sort;

		nab->nats_do[lokke]=nat_do;
	}

//	printf("num_pyobjects: %d\n",num_pyobjects);
	if(num_pyobjects==1){
		AD_installNoteAdds_track_do(
			window,
			wblock,
			nat_do
		);
	}else{
		AD_insertNoteAdds_block_do(window,nab);
	}
}


