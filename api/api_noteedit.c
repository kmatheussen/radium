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
#include "../common/undo_signatures_proc.h"
#include "../common/undo_lpbs_proc.h"
#include "../common/undo_tempos_proc.h"
#include "../common/time_proc.h"
#include "../advanced/ad_noteadd_proc.h"
#include "../common/player_proc.h"
#include "../common/player_pause_proc.h"
#include "../common/undo_maintempos_proc.h"
#include "../common/Beats_proc.h"
#include "../common/wblocks_proc.h"
#include "../common/settings_proc.h"
#include "../common/undo_notes_proc.h"
#include "../common/visual_proc.h"

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
  R_ASSERT_RETURN_IF_FALSE(l>=0);

  if (l != g_downscroll){
  
    g_downscroll = l;

    GFX_OS_update_bottombar();
  }
}

int getNoteScrollLength(void){
  return g_downscroll;
}

int getMaxVolume(){
  return MAX_VELOCITY;
}



/********** Signatures  **********/

void setMainSignature(int numerator, int denominator){
  if (numerator<=0 || denominator<=0)
    return;
  if (numerator==root->signature.numerator && denominator==root->signature.denominator)
    return;
  
  struct Tracker_Windows *window = root->song->tracker_windows;
  struct WBlocks *wblock = window->wblock;

  ADD_UNDO(MainTempo(window,wblock));

  PC_Pause();{
    root->signature = ratio(numerator, denominator);
    UpdateAllBeats();
  }PC_StopPause(window);
  
  window->must_redraw = true;
}

Place getMainSignature(void){
  return place(0,root->signature.numerator,root->signature.denominator);
}

int numSignatures(int blocknum, int windownum){
  struct WBlocks *wblock=getWBlockFromNum(windownum,blocknum);
  if(wblock==NULL)
    return 0;

  return ListFindNumElements3(&wblock->block->signatures->l);
}

int addSignature(int numerator, int denominator,
                 Place place,
                 int blocknum)
{
  struct Tracker_Windows *window;
  struct WBlocks *wblock=getWBlockFromNumA(-1,&window,blocknum);
  if(wblock==NULL) {
    GFX_Message(NULL, "unknown block(%p)",blocknum);
    return -1;
  }

  if (!PlaceLegal(wblock->block, &place)) {
    GFX_Message(NULL, "Place %s is not legal", PlaceToString(&place));
    return -1;
  }

  ADD_UNDO(Signatures_CurrPos(window));
        
  struct Signatures *signature = SetSignature(wblock->block,&place,ratio(numerator, denominator));

  window->must_redraw=true;

  return ListFindElementPos3(&wblock->block->signatures->l,&signature->l);
}

int addSignature3(int numerator, int denominator,
                  int line,int counter,int dividor,
                  int blocknum)
{
  Place place = {line, counter, dividor};
  return addSignature(numerator, denominator, place, blocknum);
}

// Return a place, for convenience. The line part of the returned value is always 0 (even if numerator > denominator). Returns -1 on error.
Place getSignature(int signaturenum, int blocknum, int windownum){
  struct Signatures *signature = getSignatureFromNum(windownum, blocknum, signaturenum);
  if (signature==NULL)
    return place(0,-1,1);
  else
    return place(0, signature->signature.numerator, signature->signature.denominator);
}

/******************* LPBs *************************/

void setMainLPB(int lpb_value){
  if (lpb_value <=1)
    return;
  if (lpb_value == root->lpb)
    return;
  
  struct Tracker_Windows *window = root->song->tracker_windows;
  struct WBlocks *wblock = window->wblock;

  printf("Undo MainTempo lpb: %d\n",lpb_value);
  ADD_UNDO(MainTempo(window,wblock));

  PC_Pause();{
    root->lpb=lpb_value;
    UpdateAllSTimes();
    UpdateAllBeats();
  }PC_StopPause(window);
  
  //UpdateAllWLPBs(window);
  window->must_redraw = true;
}

int getMainLPB(void){
  return root->lpb;
}

int numLPBs(int blocknum, int windownum){
  struct WBlocks *wblock=getWBlockFromNum(windownum,blocknum);
  if(wblock==NULL)
    return 0;

  return ListFindNumElements3(&wblock->block->lpbs->l);
}

int addLPB(int lpb_value,
           Place place,
           int blocknum)
{
  struct Tracker_Windows *window;
  struct WBlocks *wblock=getWBlockFromNumA(-1,&window,blocknum);
  if(wblock==NULL)
    return -1;

  if (!PlaceLegal(wblock->block, &place)) {
    GFX_Message(NULL, "Place %s is not legal", PlaceToString(&place));
    return -1;
  }

  ADD_UNDO(LPBs_CurrPos(window));
  
  struct LPBs *lpb = SetLPB(wblock->block,&place,lpb_value);

  window->must_redraw=true;

  return ListFindElementPos3(&wblock->block->lpbs->l,&lpb->l);
}

int addLPB3(int lpb,
            int line,int counter,int dividor,
            int blocknum
            )
{
  Place place = {line, counter, dividor};

  return addLPB(lpb, place, blocknum);
}

int getLPB(int num, int blocknum, int windownum){
  struct LPBs *lpb = getLPBFromNum(windownum, blocknum, num);
  if (lpb==NULL)
    return -1;
  else
    return lpb->lpb;
}


/***************** BPMs *************************/

void setMainBPM(int bpm_value){
  if (bpm_value <=1)
    return;
  if (bpm_value == root->tempo)
    return;

  struct Tracker_Windows *window = root->song->tracker_windows;
  struct WBlocks *wblock = window->wblock;
  
  ADD_UNDO(MainTempo(window,wblock));

  PC_Pause();{
    root->tempo=bpm_value;
    UpdateAllSTimes();
  }PC_StopPause(window);
}

int getMainBPM(void){
  return root->tempo;
}

int numBPMs(int blocknum, int windownum){
  struct WBlocks *wblock=getWBlockFromNum(windownum,blocknum);
  if(wblock==NULL)
    return 0;

  return ListFindNumElements3(&wblock->block->tempos->l);
}


int addBPM(int bpm,
           Place place,
           int blocknum)
{
  struct Tracker_Windows *window;
  struct WBlocks *wblock=getWBlockFromNumA(-1,&window,blocknum);
  if(wblock==NULL)
    return -1;

  if (!PlaceLegal(wblock->block, &place)) {
    GFX_Message(NULL, "Place %s is not legal", PlaceToString(&place));
    return -1;
  }

  ADD_UNDO(Tempos_CurrPos(window));

  struct Tempos *tempo = SetTempo(wblock->block,&place,bpm);

  window->must_redraw=true;

  return ListFindElementPos3(&wblock->block->tempos->l,&tempo->l);
}

int addBPM3(int bpm,
            int line,int counter,int dividor,
            int blocknum
            )
{
  Place place = {line, counter, dividor};

  return addBPM(bpm, place, blocknum);
}
           
int getBPM(int num, int blocknum, int windownum){
  struct BPMs *bpm = getBPMFromNum(windownum, blocknum, num);
  if (bpm==NULL)
    return -1;
  else
    return bpm->tempo;
}

Place getBPMPlace(int num, int blocknum, int windownum){
  struct BPMs *bpm = getBPMFromNum(windownum, blocknum, num);
  if (bpm==NULL)
    return place(-1,0,1);
  else
    return bpm->l.p;
}


/****************** notes **********************/

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

void setNoteEndPlace(int line,int counter,int dividor,int windownum,int blocknum,int tracknum,int notenum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, notenum);
  if (note==NULL)
    return;

  Place *place = PlaceCreate(line,counter,dividor);

  if (!PlaceLegal(wblock->block, place)) {
    GFX_Message(NULL, "Place %d + %d/%d is not legal", line, counter, dividor);
    return;
  }

  PlaceCopy(&note->end, place);
}

bool noteContinuesNextBlock(int notenum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, notenum);
  if (note==NULL)
    return false;

  return note_continues_next_block(wblock->block, note);
}

void setNoteContinueNextBlock(bool continuenextblock, int notenum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, notenum);
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

int addNote2(float notenum,int velocity,
             int line,int counter,int dividor,
             int end_line,int end_counter,int end_dividor, 
             int windownum, int blocknum, int tracknum)
{
  struct Tracker_Windows *window;
  struct WBlocks *wblock=getWBlockFromNumA(-1,&window,blocknum);
  struct WTracks *wtrack=getWTrackFromNum(windownum,blocknum,tracknum);
  if(wblock==NULL || wtrack==NULL) {
    GFX_Message(NULL, "unknown wblock(%p) or wtrack(%p) %d/%d/%d\n",wblock,wtrack,windownum,blocknum,tracknum);
    return -1;
  }

  Place *place = PlaceCreate(line,counter,dividor);

  ValidatePlace(place);

  if (!PlaceLegal(wblock->block, place)) {
    GFX_Message(NULL, "Place %d + %d/%d is not legal", line, counter, dividor);
    return -1;
  }

  Place *end_place = end_line==-1 ? NULL : PlaceCreate(end_line,end_counter,end_dividor);

  ValidatePlace(end_place);

  if (end_place != NULL && !PlaceLegal(wblock->block, end_place)) {
    GFX_Message(NULL, "Place %d + %d/%d is not legal", end_line, end_counter, end_dividor);
    return -1;
  }

  struct Notes *note = InsertNote(wblock,
                                  wtrack,
                                  place,
                                  end_place,
                                  notenum,
                                  velocity,
                                  true);

  window->must_redraw=true;

  return ListFindElementPos3(&wtrack->track->notes->l,&note->l);
}

int addNote(int notenum,int velocity,
             int line,int counter,int dividor,
             int end_line,int end_counter,int end_dividor, 
             int windownum, int blocknum, int tracknum)
{
  return addNote2(notenum, velocity,
                  line, counter, dividor,
                  end_line, end_counter, end_dividor,
                  windownum, blocknum, tracknum
                  );
}

void cutNote(float floatplace, int notenum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, notenum);
  if (note==NULL)
    return;

  Place place;
  Float2Placement(floatplace, &place);

  if (PlaceGreaterOrEqual(&place, &note->end))
    return;
  
  if (PlaceLessOrEqual(&place, &note->l.p))
    return;

  CutNoteAt(wblock->block, wtrack->track, note, &place);
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


