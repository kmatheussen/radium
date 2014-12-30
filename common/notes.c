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

#include "notes_proc.h"



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

  // move cursor in case it is placed on a non-existing subtrack.
  if (root!=NULL && root->song!=NULL && root->song->tracker_windows!=NULL){
    struct Tracker_Windows *window = root->song->tracker_windows;
    struct WBlocks *wblock = window->wblock;
    if (wblock!=NULL){
      struct WTracks *wtrack = wblock->wtrack;
      if (wtrack!=NULL && wtrack->track!=NULL){
        if (wtrack->track==track){
          if (window->curr_track_sub >= track->num_subtracks)
            window->curr_track_sub = track->num_subtracks - 1;
        }
      }
    }
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
	struct Blocks *block,
	struct Tracks *track,
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
void StopAllNotesAtPlace(
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	Place *placement
){
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

        if (notenum<=0.0f){
          RError("notenum<=0.0f: %f. Setting to 0.1",notenum);
          notenum=0.1f;
        }
        if(notenum>=128.0f){
          RError("notenum>=128.0f: %f. Setting to 127.9",notenum);
          notenum=127.9;
        }
        
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

        }
        PLAYER_unlock();

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

void InsertNoteCurrPos(struct Tracker_Windows *window, float notenum, bool polyphonic){
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
             NOTE_get_velocity(wtrack->track),
             polyphonic
             );

  if(wtrack->l.num==wblock->right_track && polyphonic)
    UpdateAllWTracksCoordinates(window,wblock);

  if (!polyphonic)
    MaybeScrollEditorDown(window);
}

void InsertStop(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	Place *placement
){
	struct Stops *stop;

	StopAllNotesAtPlace(wblock,wtrack,placement);

	stop=talloc(sizeof(struct Stops));
	PlaceCopy(&stop->l.p,placement);

	ListAddElement3(&wtrack->track->stops,&stop->l);

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
    PLAYER_lock();{
      InsertStop(window,wblock,wtrack,&realline->l.p);
    }PLAYER_unlock();
    MaybeScrollEditorDown(window);
    return;
  }

  
  TrackRealline2 *tr2 = tr->elements[0];

  if (tr2->pitch != NULL) {
    PLAYER_lock();{
      DeletePitch(track, tr2->pitch);
    }PLAYER_unlock();
    if (tr->num_elements==1)
      MaybeScrollEditorDown(window);
    return;
  }

  if (tr2->note != NULL) {
    PLAYER_lock();{
      ListRemoveElement3(&track->notes,&tr2->note->l);
      LengthenNotesTo(wblock->block,track,&realline->l.p);
    }PLAYER_unlock();
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

