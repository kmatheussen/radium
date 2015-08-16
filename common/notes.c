/* Copyright 2000 Kjetil S. Matheussen

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


#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#include "nsmtracker.h"
#include "list_proc.h"
#include "placement_proc.h"
#include "wtracks_proc.h"
#include "player_pause_proc.h"
#include "undo_notes_proc.h"
#include "cursor_updown_proc.h"
#include "blts_proc.h"
#include "playerclass.h"
#include "pitches_proc.h"
#include "trackreallines2_proc.h"
#include "OS_Player_proc.h"
#include "windows_proc.h"
#include "visual_proc.h"
#include "OS_settings_proc.h"

#include "notes_proc.h"


#ifndef TEST_NOTES

extern struct Root *root;
extern PlayerClass *pc;

static const int end_places_size = 1024*32;
static Place **end_places = NULL;

static int last_free_subtrack;

static int FindFirstFreeSubTrack(Place *p){
  int i;
  for(i=0 ; i < end_places_size ; i++){
    if (i==last_free_subtrack) {
      last_free_subtrack++;
      return i;
    }
    if (PlaceGreaterOrEqual(p, end_places[i]))
      return i;
  }

  return 0; // A polyphony of 32*1024 voices. Impressive.
}

// Also sets the track->num_subtracks attribute.
void SetNoteSubtrackAttributes(struct Tracks *track){
  last_free_subtrack = 0; // reset
  
  if (end_places==NULL)
    end_places = calloc(end_places_size,sizeof(Place*)); // Using calloc since this memory is only used temporarily in here, so it's not necessary for the GC to know about it in any way.

  track->num_subtracks = 1;
  
  struct Notes *note = track->notes;
  while(note != NULL){
    int subtrack = FindFirstFreeSubTrack(&note->l.p);
    note->subtrack = subtrack;
    
    if (subtrack+1 > track->num_subtracks)
      track->num_subtracks = subtrack+1;
    
    end_places[subtrack] = &note->end;
    note = NextNote(note);
  }
}

int GetNoteSubtrack(struct Tracks *track, struct Notes *note){
  SetNoteSubtrackAttributes(track);
  return note->subtrack;
}

int GetNumSubtracks(struct Tracks *track){
  SetNoteSubtrackAttributes(track);
  return track->num_subtracks;
}

/**************************************************************
  FUNCTION
    Returns the current note (under the cursor).
**************************************************************/
struct Notes *GetCurrNote(struct Tracker_Windows *window){
	struct WBlocks       *wblock        = window->wblock;
	struct WTracks       *wtrack        = wblock->wtrack;

        vector_t *tr = TR_get(wblock, wtrack, wblock->curr_realline);
        
        if (tr->num_elements==0)
          return NULL;

        TrackRealline2 *tr2 = tr->elements[0];

        return (struct Notes*)tr2->note;
}



/**************************************************************
  FUNCTION
    Set the _end attributes for note 'note'.
**************************************************************/
void SetEndAttributes(
	const struct Blocks *block,
	const struct Tracks *track,
	struct Notes *note
){
	Place *place;
	Place *p1=NULL,*p2=NULL;

        bool endSetEarlier = PlaceGreaterThan(&note->end, &note->l.p);
        Place *earliest = endSetEarlier ? &note->end : &note->l.p;

        struct ListHeader3 *nextnote=note->l.next;
        while(nextnote!=NULL){
          if(PlaceGreaterThan(&nextnote->p, earliest)){
            p1 = &nextnote->p;
            break;
          }
          nextnote=nextnote->next;
        }

        struct ListHeader3 *stop= &track->stops->l;
        while(stop!=NULL){
          if(PlaceGreaterThan(&stop->p, earliest)){
            p2 = &stop->p;
            break;
          }
          stop=stop->next;
        }

	place=PlaceMin(p1,p2);

	if(place!=NULL){
		note->end.line=place->line;
		note->end.counter=place->counter;
		note->end.dividor=place->dividor;
	}else{
        	PlaceSetLastPos(block, &note->end);
        	note->noend=1;
	}

}

/**************************************************************
  FUNCTION
    Stops all notes before line+(counter/dividor) at
    line+(counter/dividor, if they last that long.
**************************************************************/
static void StopAllNotesAtPlace(
                                struct WBlocks *wblock,
                                struct WTracks *wtrack,
                                Place *placement
){
        R_ASSERT(PLAYER_current_thread_has_lock());
          
	struct Tracks *track=wtrack->track;
	struct Notes *temp;

	temp=track->notes;

	while(temp!=NULL && PlaceLessThan(&temp->l.p,placement)){
		if(PlaceGreaterThan(&temp->end,placement)){
			CutListAt(&temp->velocities,placement);
			CutListAt(&temp->pitches,placement);
			PlaceCopy(&temp->end,placement);
		}
		temp=NextNote(temp);
	}
}

struct Notes *NewNote(void){
  static int64_t curr_id = -1;

  if(curr_id==-1)
    curr_id = NotenumId(1024);

  struct Notes *note=talloc(sizeof(struct Notes));
  note->id = curr_id;
  curr_id += NUM_PATCH_VOICES; // Temp hack. Maybe.
  //printf("note->id: %d\n",(int)note->id);

  return note;
}

static struct Notes *sort_notes_by_pitch_a_little_bit(struct Notes *notes){
  struct Notes *ret = notes;
  struct Notes *note_to_place_after = NULL;
  
  while(notes != NULL){
    struct Notes *next = NextNote(notes);

    R_ASSERT_RETURN_IF_FALSE2(next!=NULL, NULL);
    
    if (PlaceEqual(&notes->l.p, &next->l.p))
      if (notes->note > next->note) {
        note_to_place_after = notes;
        break;
      }
    
    notes = next;
  }

  ListRemoveElement3(&ret, &note_to_place_after->l);
  ListAddElement3_a(&ret, &note_to_place_after->l);
  
  return ret;
}

bool NOTES_sorted_by_pitch_questionmark(struct Notes *notes){
  while(notes != NULL){
    struct Notes *next = NextNote(notes);    
    if (next==NULL)
      return true;
    if (PlaceEqual(&notes->l.p, &next->l.p))
      if (notes->note > next->note)
        return false;
    notes = next;
  }
  return true;
}

struct Notes *NOTES_sort_by_pitch(struct Notes *notes){
  while(NOTES_sorted_by_pitch_questionmark(notes)==false) {
    struct Notes *better = sort_notes_by_pitch_a_little_bit(notes);
    if (better==NULL) // should not be possible, but in case the impossible happens, we don't want to lose all notes
      return notes;
    
    notes = better;
  }

  return notes;
}

static void set_new_position(struct Tracks *track, struct Notes *note, Place *start, Place *end){

  bool has_lock = PLAYER_current_thread_has_lock();

  if (track==NULL && has_lock)
    RError("track==NULL && has_lock");

  
  if (track!=NULL) {
    if (has_lock==false)
      PLAYER_lock();
    ListRemoveElement3(&track->notes, &note->l);
  }

  if (start!=NULL && start!=&note->l.p)
    note->l.p = *start;

  if (end!=NULL && end!=&note->end)
    note->end = *end;

  if (track!=NULL){
    ListAddElement3(&track->notes, &note->l);
    if (has_lock==false)
      PLAYER_unlock();
  }
}

static void set_legal_start_and_end_pos(const struct Blocks *block, struct Tracks *track, struct Notes *note){
  Place *start = &note->l.p;
  Place *end = &note->end;
  Place endplace;

  PlaceSetLastPos(block,&endplace);
  
  if(PlaceGreaterOrEqual(start,&endplace)) {
    RError("note is placed after block end. start: %f, end: %f", GetfloatFromPlace(&note->l.p), GetfloatFromPlace(&note->end));
    set_new_position(track, note, PlaceCreate(block->num_lines - 2, 0, 1), NULL);
  }
  
  if (start->line < 0) {
    RError("note is placed before block start. start: %f, end: %f", GetfloatFromPlace(&note->l.p), GetfloatFromPlace(&note->end));
    set_new_position(track, note, PlaceCreate(0,1,1), NULL);
  }
  
  if(PlaceGreaterThan(end,&endplace)) {
    RError("note end is placed after block end. start: %f, end: %f", GetfloatFromPlace(&note->l.p), GetfloatFromPlace(&note->end));
    set_new_position(track, note, NULL, &endplace);
  }

  if(PlaceLessOrEqual(end,start)) {
    RError("note end is placed before (or on) note end. start: %f, end: %f", GetfloatFromPlace(&note->l.p), GetfloatFromPlace(&note->end));
    float e = p_float(*start);
    e += 0.01;
    Place new_end;
    Float2Placement(e, &new_end);
    set_new_position(track, note, NULL, &new_end);
  }

}

void NOTE_validate(const struct Blocks *block, struct Tracks *track, struct Notes *note){
  R_ASSERT_RETURN_IF_FALSE(block!=NULL);
  R_ASSERT_RETURN_IF_FALSE(note!=NULL);
  
  R_ASSERT(track==NULL || PLAYER_current_thread_has_lock());
  
  if (note->note<=0.0f){
    RError("notenum<=0.0f: %f. Setting to 0.01",note->note);
    note->note=0.01f;
  }
  if(note->note>=128.0f){
    RError("notenum>=128.0f: %f. Setting to 127.99",note->note);
    note->note=127.99;
  }

  set_legal_start_and_end_pos(block, track, note);
}



struct Notes *InsertNote(
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	Place *placement,
        Place *end_placement,
	float notenum,
	int velocity,
	bool polyphonic
){
	struct Blocks *block=wblock->block;
	struct Tracks *track=wtrack->track;

	struct Notes *note=NewNote();

	PlaceCopy(&note->l.p,placement);

	note->note=notenum;
	note->velocity=velocity;
//	note->velocity=(*wtrack->track->instrument->getStandardVelocity)(wtrack->track);
	note->velocity_end=note->velocity;

        PLAYER_lock();
        {
          ListAddElement3(&track->notes,&note->l);

          if(polyphonic==false)
            StopAllNotesAtPlace(wblock,wtrack,placement);

          if (end_placement==NULL)
            SetEndAttributes(block,track,note);
          else
            PlaceCopy(&note->end, end_placement);

          track->notes = NOTES_sort_by_pitch(track->notes);
        }
        PLAYER_unlock();

        NOTE_validate(block, NULL, note);

        return note;
}

bool drunk_velocity=false;
static int64_t last_velocity = MAX_VELOCITY / 2;

int NOTE_get_velocity(struct Tracks *track){

  if(drunk_velocity==false)
    return root->standardvel;

  int64_t new_velocity = last_velocity + scale(rand()/100.0f,0,RAND_MAX/100.0f,-(MAX_VELOCITY/3), MAX_VELOCITY/3);
  if(new_velocity>=root->standardvel)
    new_velocity = root->standardvel - MAX_VELOCITY/50;
  if(new_velocity<root->min_standardvel)
    new_velocity = root->min_standardvel + MAX_VELOCITY / 4;

  last_velocity = new_velocity;

  //printf("returning %d\n",(int)new_velocity);

  return new_velocity;
}


int g_downscroll = 1;

static void MaybeScrollEditorDown(struct Tracker_Windows *window){
  if(window->curr_track_sub==-1 && !pc->isplaying){
    ScrollEditorDown(window,g_downscroll);
  }
}


void InsertNoteCurrPos(struct Tracker_Windows *window, float notenum, bool polyphonic, float velocity){
  if(notenum<0.001 || notenum>127.9) return;

  Undo_Notes_CurrPos(window);

  struct WBlocks *wblock        = window->wblock;
  struct WTracks *wtrack        = wblock->wtrack;
  struct Tracks  *track         = wtrack->track;
  int             curr_realline = wblock->curr_realline;

  vector_t *tr = TR_get(wblock, wtrack, curr_realline);

  if (polyphonic==false && tr->num_elements > 0) {
    TrackRealline2 *tr2 = tr->elements[0];

    if (tr2->pitch != NULL) {
      tr2->pitch->note = notenum; // lock not necessary
      MaybeScrollEditorDown(window);
      return;
    }

    if (tr2->note != NULL) {
      tr2->note->note = notenum; // lock not necessary
      MaybeScrollEditorDown(window);
      return;
    }

    const struct Stops *stop = tr2->stop;
    PLAYER_lock();{
      ListRemoveElement3(&track->stops, &stop->l);
    }PLAYER_unlock();
  }

  struct LocalZooms *realline = wblock->reallines[curr_realline];
  
  InsertNote(
             wblock,wtrack,&realline->l.p,NULL,notenum,
             velocity < 0.0 ? NOTE_get_velocity(wtrack->track) : velocity*MAX_VELOCITY,
             polyphonic
             );

  if(wtrack->l.num==wblock->right_track && polyphonic)
    UpdateAllWTracksCoordinates(window,wblock);

  if (!polyphonic)
    MaybeScrollEditorDown(window);
}

static void InsertStop(
                       struct Tracker_Windows *window,
                       struct WBlocks *wblock,
                       struct WTracks *wtrack,
                       Place *placement
){
	struct Stops *stop;

        stop=talloc(sizeof(struct Stops));
	PlaceCopy(&stop->l.p,placement);

        PLAYER_lock();{
          StopAllNotesAtPlace(wblock,wtrack,placement);
  	  ListAddElement3(&wtrack->track->stops,&stop->l);
        }PLAYER_unlock();
}

/**********************************************************************
  FUNCTION
    Set the end attributes of all notes that previously was stopped
    at position 'placement' to the next stop wherever that may be.
**********************************************************************/
void LengthenNotesTo(
	struct Blocks *block,
	struct Tracks *track,
	Place *placement
){
	struct Notes *note=track->notes;
	while(note!=NULL){
		if(PlaceGreaterThan(&note->l.p,placement)) break;
		if(PlaceEqual(&note->end,placement))
			SetEndAttributes(block,track,note);
		note=NextNote(note);
	}
}

/**********************************************************************
  FUNCTION
    Set the end attributes of all notes that previously was stopped
    at position 'old_placement' to 'new_placement'.
**********************************************************************/
void ReplaceNoteEnds(
	struct Blocks *block,
	struct Tracks *track,
	Place *old_placement,
        Place *new_placement
){
	struct Notes *note=track->notes;
	while(note!=NULL){
		if(PlaceGreaterThan(&note->l.p,old_placement)) break;
		if(PlaceEqual(&note->end,old_placement))
                  note->end = *new_placement;
		note=NextNote(note);
	}
}

void RemoveNote(
	struct Blocks *block,
	struct Tracks *track,
	struct Notes *note
){
	ListRemoveElement3(&track->notes,&note->l);
        LengthenNotesTo(block,track,&note->l.p);
}

void RemoveNoteCurrPos(struct Tracker_Windows *window){
  struct WBlocks       *wblock        = window->wblock;
  struct WTracks       *wtrack        = wblock->wtrack;
  struct Tracks        *track         = wtrack->track;
  struct LocalZooms    *realline      = wblock->reallines[wblock->curr_realline];
  int                   curr_realline = wblock->curr_realline;
  
  vector_t *tr = TR_get(wblock, wtrack, curr_realline);

  Undo_Notes_CurrPos(window);

  if (tr->num_elements==0) {
    InsertStop(window,wblock,wtrack,&realline->l.p);
    MaybeScrollEditorDown(window);
    return;
  }

  
  TrackRealline2 *tr2 = tr->elements[0];

  if (tr2->pitch != NULL) {
    DeletePitch(track, tr2->note, tr2->pitch);
    if (tr->num_elements==1)
      MaybeScrollEditorDown(window);
    return;
  }

  if (tr2->note != NULL) {
    PLAYER_lock();{
      ListRemoveElement3(&track->notes,&tr2->note->l);
      LengthenNotesTo(wblock->block,track,&realline->l.p);
    }PLAYER_unlock();
    SetNoteSubtrackAttributes(wtrack->track);
    ValidateCursorPos(window);
    if (tr->num_elements==1)
      MaybeScrollEditorDown(window);
    return;
  }

  const struct Stops *stop = tr2->stop;
  PLAYER_lock();{
    ListRemoveElement3(&track->stops, &stop->l);
    LengthenNotesTo(wblock->block,track,&realline->l.p);
  }PLAYER_unlock();
  
  if (tr->num_elements==1)
    MaybeScrollEditorDown(window);
}

struct Notes *FindPrevNoteOnSameSubTrack(struct Tracks *track, struct Notes *note){
  struct Notes *notes = track->notes;
  struct Notes *prev = NULL;
  
  while(notes != note){
    if (notes->subtrack == note->subtrack)
      prev = notes;
    notes = NextNote(notes);
  }

  return prev;
}

struct Notes *FindNextNoteOnSameSubtrack(struct Notes *note){
  int subtrack = note->subtrack;

  note = NextNote(note);
  while(note!=NULL){
    if (note->subtrack==subtrack)
      return note;
    else
      note = NextNote(note);
  }
  return NULL;
}


struct Notes *FindNoteOnSubTrack(
                                 struct Tracks *track,
                                 int subtrack,
                                 Place *placement
){
        SetNoteSubtrackAttributes(track);
  
        struct Notes *note = track->notes;
        
        while (note != NULL) {
          if(PlaceIsBetween(placement,&note->l.p,&note->end))
            if (note->subtrack==subtrack)
              return note;
          
          note = NextNote(note);
        }

        return NULL;
}

struct Notes *FindNote(
                       struct Tracks *track,
                       Place *placement
                       )
{
  struct Notes *note = track->notes;
  while(note != NULL) {
    if (PlaceIsBetween2(placement, &note->l.p, &note->end))
      break;
    note = NextNote(note);
  }
  return note;
}

struct Notes *FindNoteCurrPos(struct Tracker_Windows *window){
  struct WBlocks    *wblock   = window->wblock;
  struct Tracks     *track    = wblock->wtrack->track;
  struct LocalZooms *realline = wblock->reallines[wblock->curr_realline];

  return FindNote(track, &realline->l.p);
}

#endif // TEST_NOTES


/******************/
/* General RETURN */
/******************/

static int get_chroma(char chromachar){
  chromachar = tolower(chromachar);

  switch(chromachar){
  case 'c':
    return 0;
  case 'd':
    return 2;
  case 'e':
    return 4;
  case 'f':
    return 5;
  case 'g':
    return 7;
  case 'a':
    return 9;
  case 'h':
    return 11;
  case 'b':
    return 11;
  default:
    return -1;
  }
}

static char *chroma_to_string(int chroma){
  switch(chroma){
  case 0:
    return "c";
  case 1:
    return "c#";
  case 2:
    return "d";
  case 3:
    return "d#";
  case 4:
    return "e";
  case 5:
    return "f";
  case 6:
    return "f#";
  case 7:
    return "g";
  case 8:
    return "g#";
  case 9:
    return "a";
  case 10:
    return "a#";
  case 11:
    return "b";
  default:
    return talloc_format("<error: \"%d\" is outside note range>", chroma);
  }
}


static int get_octave(char octavechar){
  octavechar = tolower(octavechar);

  switch(octavechar){
  case '0':
    return 0;
  case '1':
    return 1;
  case '2':
    return 2;
  case '3':
    return 3;
  case '4':
    return 4;
  case '5':
    return 5;
  case '6':
    return 6;
  case '7':
    return 7;
  case '8':
    return 8;
  case '9':
    return 9;
  case 'a':
    return 10;
  case 'b':
    return 11;
  case 'c':
    return 12;
  default:
    return -1;
  }
}

static char *octave_to_string(int octave){
  switch(octave){
  case 0:
    return "0";
  case 1:
    return "1";
  case 2:
    return "2";
  case 3:
    return "3";
  case 4:
    return "4";
  case 5:
    return "5";
  case 6:
    return "6";
  case 7:
    return "7";
  case 8:
    return "8";
  case 9:
    return "9";
  case 10:
    return "a";
  case 11:
    return "b";
  case 12:
    return "c";
  default:
    return talloc_format("<error: \"%d\" is outside octave range>", octave);   
  }
}


static int get_sharp(char sharptext){
  if(sharptext=='#')
    return 1;
  else if(sharptext=='b')
    return -1;
  else if(sharptext=='B')
    return -1;
  else if(sharptext=='-')
    return 0;
  else
    return -2;
}

static char *substring(char *s,int start,int end){
  char *ret       = talloc(end-start+1);
  int   read_pos  = start;
  int   write_pos = 0;

  while(read_pos<end)
    ret[write_pos++] = s[read_pos++];

  return ret;
}

static int string_charpos(char *s, char c){
  int pos=0;
  while(s[pos]!=0){
    if(s[pos]==c)
      return pos;
    pos++;
  }
  return -1;
}

static char *strip_whitespace(char *s){
  char *ret=s;

  // strip before
  while(isspace(ret[0]))
    ret++;


  // strip after
  int pos=strlen(ret)-1;
  while(isspace(ret[pos])){
    ret[pos]=0;
    pos--;
  }


  return ret;
}

float notenum_from_notetext(char *notetext){
  int chroma, octave, sharp;

  char *stripped = strip_whitespace(notetext);

  char *notename;
    
  int comma_pos = string_charpos(stripped, ',');
  if (comma_pos != -1)
    notename = substring(stripped,0,comma_pos);
  else
    notename = stripped;

  printf("stripped: -%s-, notename: -%s-\n",stripped,notename);
  
  if(strlen(notename)==2){

    chroma = get_chroma(notename[0]);
    sharp = 0;
    octave = get_octave(notename[1]);

  } else if(strlen(notename)==3){

    chroma = get_chroma(notename[0]);
    sharp = get_sharp(notename[1]);
    octave = get_octave(notename[2]);

  } else
    return -1;

  if(chroma==-1 || sharp==-2 || octave==-1)
    return -1;

  float decimals = 0.0f;

  if (comma_pos != -1) {
    char *decimaltext = stripped + comma_pos + 1;
#ifdef TEST_NOTES
    decimals = atof(decimaltext) / 100.0f;
#else
    decimals = OS_get_double_from_string(decimaltext) / 100.0f;
#endif
    printf("decimaltext: -%s-, dec: %f\n",decimaltext,decimals);
  }
  
  float notenum = octave*12 + chroma + sharp + decimals;

  if(notenum<=0 || notenum>127.99)
    return -1;
  else
    return notenum;
}

char *notetext_from_notenum(float notenumf){
  int notenum = notenumf;
  int cents = 0;
  
  if (notenumf!=floorf(notenum)){
    float decimals = notenumf - floorf(notenum);
    printf("___ decimals: %f\n",decimals);
    cents = roundf(decimals*100.0f);
  }
  
  int octave = notenum / 12;
  int chroma = notenum - (octave*12);

  printf("************************************************************************** octave: %d, chroma: %d, cents: %d\n",octave,chroma,cents);
  char *wholenotestring = talloc_format("%s%s",chroma_to_string(chroma), octave_to_string(octave));

  if (cents != 0)
    return talloc_format("%s,%d", wholenotestring, cents);
  else
    return wholenotestring;
}


#ifndef TEST_NOTES

static float request_notenum(struct Tracker_Windows *window, char *title, float old_notenum){
  float notenum = -1;
  
  ReqType reqtype=GFX_OpenReq(window,30,12,"Set Pitch");

  if (old_notenum>0.0f)
    GFX_SetString(reqtype, notetext_from_notenum(old_notenum));

  char temp[1024];
  sprintf(temp, "%s (for example: \"c4\" or \"c#5,50\") >", title);
  
  while(notenum <= 0.0f){
    char *notetext = GFX_GetString(
                                   window,
                                   reqtype,
                                   temp
                                   );
    if(notetext==NULL)
      break;
    
    notenum = notenum_from_notetext(notetext);
  }
  
  GFX_CloseReq(window,reqtype);

  printf("notenum: %f\n",notenum);
  
  return notenum;
}

static void r_add_pitch(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, struct Notes *note, Place *p){
  float notenum = request_notenum(window, "Add pitch", -1);
  if(notenum > 0.0f)
    AddPitch(window, wblock, wtrack, note, p, notenum);
}

static void r_add_note(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, Place *p){
  float notenum = request_notenum(window, "Add note", -1);
  
  if(notenum > 0.0f)
    InsertNote(wblock, wtrack, p, NULL, notenum, NOTE_get_velocity(wtrack->track), false);
}

static void r_edit_pitch(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, struct Pitches *pitch){
  float notenum = request_notenum(window, "Edit pitch", pitch->note);
  
  if(notenum > 0.0f)
    pitch->note = notenum; // lock not necessary
}

static void r_edit_note(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, struct Notes *note){
  float notenum = request_notenum(window, "Edit note", note->note);
  
  if(notenum > 0.0f)
    note->note = notenum; // lock not necessary
}


void EditNoteCurrPos(struct Tracker_Windows *window){
  struct WBlocks       *wblock        = window->wblock;
  struct WTracks       *wtrack        = wblock->wtrack;    
  int                   curr_realline = wblock->curr_realline;  
  vector_t             *tr            = TR_get(wblock, wtrack,curr_realline);

  if (tr->num_elements==0) {

    struct LocalZooms *realline = wblock->reallines[curr_realline];
    Place             *p        = &realline->l.p;
    struct Notes      *note     = FindNote(wtrack->track, p);
      
    if (note != NULL)
      r_add_pitch(window, wblock, wtrack, note, p);
    else
      r_add_note(window, wblock, wtrack, p);
      
  } else {

    TrackRealline2 *tr2 = tr->elements[0];

    if (tr2->pitch != NULL)
      r_edit_pitch(window, wblock, wtrack, tr2->pitch);

    else if (tr2->note != NULL)
      r_edit_note(window, wblock, wtrack, tr2->note);
    
  }
  
}


  /******************************/
 /* Not General RETURN anymore */
/******************************/

void StopVelocityCurrPos(struct Tracker_Windows *window,int noend){
	struct WBlocks *wblock;
	struct WTracks *wtrack;
	int reallinerealline;
	struct LocalZooms *realline;
	struct Notes *note;
	int subtrack;

	PC_Pause();

	wblock=window->wblock;
	wtrack=wblock->wtrack;
	reallinerealline=wblock->curr_realline;
	realline=wblock->reallines[reallinerealline];
	subtrack=window->curr_track_sub;

	note=FindNoteOnSubTrack(wtrack->track,subtrack,&realline->l.p);
	if(note==NULL){
		PC_StopPause();
		return;
	}

        Undo_Notes_CurrPos(window);

	if(PlaceGreaterOrEqual(&note->l.p,&realline->l.p)){
		RemoveNote(wblock->block,wtrack->track,note);
                SetNoteSubtrackAttributes(wtrack->track);
                ValidateCursorPos(window);                        
	}else{
		CutListAt(&note->velocities,&realline->l.p);
		PlaceCopy(&note->end,&realline->l.p);
		note->noend=noend;
	}

#if !USE_OPENGL
	ClearTrack(window,wblock,wtrack,wblock->top_realline,wblock->bot_realline);
	UpdateWTrack(window,wblock,wtrack,wblock->top_realline,wblock->bot_realline);
#endif
	PC_StopPause();

        window->must_redraw=true;
}
#endif // TEST_NOTES



#ifdef TEST_NOTES

#include <stdarg.h>
#include <assert.h>

void EndProgram(void){
  printf("ENDPROGRAM called\n");
}

void RError(const char *fmt,...){
  char message[1000];
  va_list argp;
  
  va_start(argp,fmt);
  /*	vfprintf(stderr,fmt,argp); */
  vsprintf(message,fmt,argp);
  va_end(argp);

  fprintf(stderr,"error: %s\n",message);
}

static void cmp(char *text, float value){
  fprintf(stderr,"\n\nComparing \"%s\" against %f\n",text,value);
  
  float from_text = notenum_from_notetext(text);

  char *from_value = notetext_from_notenum(value);
  float from_text_from_value = notenum_from_notetext(from_value);

  fprintf(stderr,"from_text: %f, from_value: \"%s\", from_text_from_value: %f (value: %f)\n",
          from_text,
          from_value,
          from_text_from_value,
          value
          );
  
  assert(abs(from_text-value) < 0.001);
  assert(abs(from_text_from_value-value) < 0.001);
}

int main(void){

  assert(notenum_from_notetext("") < 0);
  assert(notenum_from_notetext("y5") < 0);
  assert(notenum_from_notetext("cd") < 0);

  assert(notenum_from_notetext("cb0") < 0);

  cmp("c0,1",0.01);
  cmp("c0,10",0.1);
  cmp("c#0",1);
  cmp("c#0,99",1.99);
  cmp("ga",127);
  cmp("ga,99",127.99);
  cmp("g-a",127);
  cmp("g-a,99",127.99);

  cmp("C0,1",0.01);
  cmp("C0,10",0.1);
  cmp("C#0",1);
  cmp("C#0,99",1.99);
  cmp("G-A",127);
  cmp("G-A,99",127.99);

  int cents;
  for(cents=0;cents<100;cents++){
    char *a = talloc_format("c9,%d",cents);
    cmp(a,108.0 + (float)cents/100.0f);
  }

  printf("Success, no errors\n");

  return 0;
}

#endif
