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

#include "nsmtracker.h"
#include "list_proc.h"
#include "common_proc.h"
#include "tracks_proc.h"
#include "gfx_wtracks_proc.h"
#include "trackreallines_proc.h"
#include "undo_notes_proc.h"
#include "gfx_statusbar_proc.h"
#include "player_proc.h"
#include "placement_proc.h"
#include "pitches_proc.h"
#include "visual_proc.h"

#include "mouse_fxarea_proc.h"

#define PIXELS_PER_NOTE 5.0f

extern struct Root *root;

/*********************************************
 * Move Note
 *********************************************/
static Place *getPrevLegalNotePlace(struct Tracks *track, struct Notes *note){
  Place *end = PlaceGetFirstPos();

  struct Notes *prev = ListPrevElement3(&track->notes->l, &note->l);

  if (prev != NULL)
    end = PlaceMax(end, &prev->end);

  return end;
}

static Place *getNextLegalNotePlace(struct Notes *note){
  Place *end = &note->end;

  if (note->velocities != NULL)
    end = PlaceMin(end, &note->velocities->l.p);

  if (note->pitches != NULL)
    end = PlaceMin(end, &note->pitches->l.p);

  return end;
}



static int MoveNote(
	struct Tracker_Windows *window,
	float x,float y
){
  struct WBlocks *wblock=window->wblock;
  struct MouseAction *action= &window->prevaction;

  struct WTracks *wtrack=(struct WTracks *)action->pointer1;
  //struct Tracks *track=wtrack->track;
  struct Notes *note=(struct Notes *)action->pointer2;

  float org_x = action->efloat1;
  float delta_y = action->efloat2;

  PlayStop();

  /* Do a check to see if all data is still valid. */
  if(isInList1_m(
                 &window->wblocks->l,&wblock->l,
                 &wblock->wtracks->l,&wtrack->l,
                 &wtrack->track->notes->l,&note->l,
                 root
                 )==false){
    action->action=NOACTION;
    return 0;
  }
  
  
  Place place;
  GetReallineAndPlaceFromY(window, wblock, y-delta_y, &place, NULL, NULL);

  float org_pitch = action->efloat3;

  float pitch=org_pitch + (x-org_x)/PIXELS_PER_NOTE;
  note->note = R_BOUNDARIES(1,pitch,127);
  //printf("note->note: %f, pitch: %f\n",note->note, pitch);

  //printf("org_pitch: %f. pitch: %f. x: %f, org_x: %f\n",org_pitch,pitch,x,org_x);
  

  PlaceCopy(&note->l.p, PlaceBetween(getPrevLegalNotePlace(wtrack->track, note), &place, getNextLegalNotePlace(note)));

  UpdateTrackReallines(window,wblock,wtrack);

#if !USE_OPENGL
  DrawUpWTrack(window,wblock,wtrack);
#endif

  return 0;
}


static struct Notes *GetNoteUnderMouse(struct Tracker_Windows *window, struct WTracks *wtrack, int realline, int x, int y){
  struct TrackRealline *trackrealline= &wtrack->trackreallines[realline];
  struct TrackReallineElements *element;

  for(element=trackrealline->trackreallineelements;element!=NULL;element=element->next){
    switch(element->type){
    case TRE_THISNOTELINES:
      return element->pointer;
    }
  }

  return NULL;
}



/*********************************************
 * Move Pitch
 *********************************************/

static Place *getPrevLegalPitchPlace(struct Notes *note, struct Pitches *pitch){
  Place *end = &note->l.p;

  struct Pitches *prev = ListPrevElement3(&note->pitches->l, &pitch->l);

  if (prev != NULL)
    end = PlaceMax(end, &prev->l.p);

  return end;
}

static Place *getNextLegalPitchPlace(struct Notes *note, struct Pitches *pitch){
  Place *end = &note->end;

  if (NextPitch(pitch) != NULL)
    end = PlaceMin(end, &NextPitch(pitch)->l.p);

  return end;
}

static int MovePitch(
	struct Tracker_Windows *window,
	float x,float y
){
  struct WBlocks *wblock=window->wblock;
  struct MouseAction *action= &window->prevaction;

  struct WTracks *wtrack=(struct WTracks *)action->pointer1;
  //struct Tracks *track=wtrack->track;
  struct Notes *note=action->pointer2;
  struct Pitches *pitch=action->pointer3;

  float org_x = action->efloat1;
  float delta_y = action->efloat2;

  PlayStop();

  /* Do a check to see if all data is still valid. */
  if(isInList1_m(
                 &window->wblocks->l,&wblock->l,
                 &wblock->wtracks->l,&wtrack->l,
                 &wtrack->track->notes->l,&note->l,
                 &note->pitches->l,&pitch->l,
                 root
                 )==false){
    action->action=NOACTION;
    return 0;
  }
  
  Place place;
  GetReallineAndPlaceFromY(window, wblock, y-delta_y, &place, NULL, NULL);

  float org_pitch = action->efloat3;

  float pitchvalue=org_pitch + (x-org_x)/PIXELS_PER_NOTE;
  pitch->note = R_BOUNDARIES(1,pitchvalue,127);

  PlaceCopy(&pitch->l.p, PlaceBetween(getPrevLegalPitchPlace(note, pitch), &place, getNextLegalPitchPlace(note, pitch)));

  UpdateTrackReallines(window,wblock,wtrack);

#if !USE_OPENGL
  DrawUpWTrack(window,wblock,wtrack);
#endif

  return 0;
}

static bool GetPitchUnderMouse(struct Tracker_Windows *window, struct WTracks *wtrack, int realline, int x, int y, bool create_new_pitch, struct Notes **note, struct Pitches **pitch){
  {
    struct TrackRealline *trackrealline= &wtrack->trackreallines[realline];
    struct TrackReallineElements *element;
    
    for(element=trackrealline->trackreallineelements;element!=NULL;element=element->next){
      switch(element->type){
      case TRE_THISPITCHLINES:
        *note = element->note;
        *pitch = element->pointer;

        if (create_new_pitch==true) {
          PlayStop();
          Undo_Notes(window,window->wblock->block,wtrack->track,window->wblock->curr_realline);
        }

        return true;
      }
    }
  }



  /*********************************************
   * Create new pitch
   *********************************************/
  if (x > wtrack->x+(wtrack->fxarea.x-wtrack->x)/5) { // 20% left for track width adjustment
    WPitches *wpitches = wtrack->wpitches[realline];

    if(wpitches != NULL){
      if (create_new_pitch==true) {
      
        PlayStop();
        Undo_Notes(window,window->wblock->block,wtrack->track,window->wblock->curr_realline);

        *note = wpitches->note;
        struct WBlocks *wblock = window->wblock;
        Place *place = &wblock->reallines[realline]->l.p;
        *pitch = AddPitch(window, wblock, wtrack, wpitches->note, place, wpitches->note->note);
      }

      return true;
    }
  }

  return false;
}






/*********************************************
 * Public function
 *********************************************/


bool SetMouseActionPitches(
	struct Tracker_Windows *window,
	struct MouseAction *action,
	struct WTracks *wtrack,
	int x,int y,
	int click
                           ){
  //RError("dead code: SetMouseActionPitches");
  //printf("dead code: SetMouseActionPitches\n");
  return false;
  
	struct WBlocks *wblock=window->wblock;
        Place place;

	int realline = GetReallineAndPlaceFromY(window,wblock,y,&place,NULL,NULL);
	if(realline<0)
          return false;

        if (click==2){
          wtrack->is_wide = !wtrack->is_wide;
          SetNormalPointer(window);
          wblock->block->is_dirty=true; // redraw.
          return true;
        }

        if (click != 1) {
          struct Notes *note = NULL;
          struct Pitches *pitch = NULL;

          if (GetNoteUnderMouse(window, wtrack, realline, x, y) != NULL ||
              GetPitchUnderMouse(window, wtrack, realline, x, y, false, &note, &pitch)==true)
          {
            SetNormalPointer(window);
          } else {
            SetHorizResizePointer(window);
          }

          return false;
        }

        int realline_y1 = Common_oldGetReallineY1Pos(window,wblock,realline);
        int realline_y2 = Common_oldGetReallineY2Pos(window,wblock,realline);

        struct Notes *note = GetNoteUnderMouse(window, wtrack, realline, x, y);
        if (note!= NULL) {
          float note_delta_y = scale((float)note->l.p.counter/(float)note->l.p.dividor,0,1,realline_y1,realline_y2);

          action->action = PITCH_NOTE;
          action->pointer1 = wtrack;
          action->pointer2 = note;

          action->efloat1 = x;
          action->efloat2 = y - note_delta_y;
          action->efloat3 = note->note;

          action->MouseUpFunction = &MoveNote;

          PlayStop();
          Undo_Notes(window,window->wblock->block,wtrack->track,window->wblock->curr_realline);

          return true;
        }

        struct Pitches *pitch = NULL;
        if (GetPitchUnderMouse(window, wtrack, realline, x, y, true, &note, &pitch)==true) {
          //printf("Got it. note: %f, pitch: %f\n",note->note,pitch->note);
          float pitch_delta_y = scale((float)pitch->l.p.counter/(float)pitch->l.p.dividor,0,1,realline_y1,realline_y2);

          action->action = PITCH_PITCH;
          action->pointer1 = wtrack;
          action->pointer2 = note;
          action->pointer3 = pitch;

          action->efloat1 = x;
          action->efloat2 = y - pitch_delta_y;
          action->efloat3 = pitch->note;

          action->MouseUpFunction = &MovePitch;

          return true;

        }

        return false;
}





