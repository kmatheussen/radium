/* Copyright 2005 Kjetil S. Matheussen

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




#include "../nsmtracker.h"
#include "../TimeData.hpp"
#include "../placement_proc.h"
#include "../clipboard_range_calc_proc.h"
#include "../list_proc.h"
#include "../wtracks_proc.h"
#include "../undo_notes_proc.h"
#include "../undo_range_proc.h"
#include "../visual_proc.h"
#include "../player_proc.h"
#include "../player_pause_proc.h"

#include "pitchexpand_proc.h"


static int minnote;
static int maxnote;


#define SCALE(x,x1,x2,y1,y2) ((y1)+(((x)-(x1))*((y2)-(y1)))/((x2)-(x1)))


static void PExpand_SetMinMax(
			      struct Notes *note,
			      const Place *p1,
			      const Place *p2,
			      bool firsttime
			      )
{
	if(note==NULL) return;

	if(PlaceGreaterOrEqual(&note->l.p,p1)){

		if(PlaceGreaterOrEqual(&note->l.p,p2)) return;

		if(firsttime==false){
		  maxnote=R_MAX(note->note,maxnote);
		  minnote=R_MIN(note->note,minnote);
		}else{
		  minnote=127;
		  maxnote=0;
		  firsttime=false;
		}

                const Ratio ratio1 = place2ratio(*p1);
                const Ratio ratio2 = place2ratio(*p2);
        
                const r::PitchTimeData::Reader reader(note->_pitches);
                for(const r::Pitch &pitch : reader){

                  if(pitch._time >= ratio2)
                    break;

                  if (pitch._time >= ratio1) {
                    maxnote=R_MAX(pitch._val,maxnote);
                    minnote=R_MIN(pitch._val,minnote);
                  }
                  
                }

                if (note->end < ratio2 && !equal_floats(note->pitch_end, 0.0)) {
                  maxnote=R_MAX(note->pitch_end,maxnote);
                  minnote=R_MIN(note->pitch_end,minnote);
                }
        
	}

	PExpand_SetMinMax(NextNote(note),p1,p2,firsttime);
}

static int expandit(float note, float scalefactor){
  
  float midnote=(minnote+maxnote)/2.0f;

  float ret = (int) (0.5f + SCALE(note, 
                                  minnote, 
                                  maxnote, 
                                  midnote - ( (midnote-minnote) * scalefactor),
                                  midnote + ( (midnote-minnote) * scalefactor)
                                  )
                     );
  
  ret = R_MAX(1,R_MIN(127,ret));

  return ret;
}

static void PExpand_DoIt(
	struct Notes *note,
	const Place *p1,
	const Place *p2,
	float scalefactor
){
	if(note==NULL) return;
        if(minnote==maxnote) return;

	if(PlaceGreaterOrEqual(&note->l.p,p1)){
          
	  if(PlaceGreaterOrEqual(&note->l.p,p2)) return;

          note->note = expandit(note->note, scalefactor);

          const Ratio ratio1 = place2ratio(*p1);
          const Ratio ratio2 = place2ratio(*p2);
          
          r::PitchTimeData::Writer writer(note->_pitches);
          for(r::Pitch &pitch : writer){

            if(pitch._time >= ratio2)
              break;

            if (pitch._time >= ratio1)
              pitch._val = expandit(pitch._val, scalefactor);
          }
          
	}

	PExpand_DoIt(NextNote(note),p1,p2,scalefactor);
}


static void PExpandRange(
		  struct WBlocks *wblock,
		  float scalefactor
){
	struct Tracks *track;
	int lokke;
	bool firsttime=true;

	if( ! wblock->range.enabled) return;

	const Place *p1=&wblock->range.y1;
	const Place *p2=&wblock->range.y2;

	track=(struct Tracks *)ListFindElement1(&wblock->block->tracks->l,wblock->range.x1);

	for(lokke=0;lokke<=wblock->range.x2-wblock->range.x1;lokke++){
	  PExpand_SetMinMax(track->notes,p1,p2,firsttime);
	  firsttime=false;
	  track=NextTrack(track);
	}

	track=(struct Tracks *)ListFindElement1(&wblock->block->tracks->l,wblock->range.x1);
	for(lokke=0;lokke<=wblock->range.x2-wblock->range.x1;lokke++){
	  PExpand_DoIt(track->notes,p1,p2,scalefactor);
	  track=NextTrack(track);
	}

}

static void PExpandTrack(
	struct Blocks *block,
	struct Tracks *track,
	float scalefactor
){
	Place p1,p2;

	PlaceSetFirstPos(&p1);
	PlaceSetLastPos(block,&p2);
	
	PExpand_SetMinMax(track->notes,&p1,&p2,true);
	
        PExpand_DoIt(track->notes,&p1,&p2,scalefactor);
}

static void PExpandBlock(
		  struct Blocks *block,
		  float scalefactor
){
	struct Tracks *track;
	bool firsttime=true;
	Place p2;
	//float midnote;
	
	//printf("Scale %f\n",scalefactor);
	const Place *p1=PlaceGetFirstPos();
	PlaceSetLastPos(block,&p2);

	track=block->tracks;

	while(track!=NULL){
	  PExpand_SetMinMax(track->notes,p1,&p2,firsttime);
	  firsttime=false;
	  track=NextTrack(track);
	}
	//midnote=(minnote+maxnote)/2.0f;	
	//printf("midnote: %f, m1:%d/%d, m2: %f/%f\n", midnote,minnote,maxnote, midnote - ( (midnote-minnote) * scalefactor), midnote + ( (midnote-minnote) * scalefactor));

	track=block->tracks;
	while(track!=NULL){
	  PExpand_DoIt(track->notes,p1,&p2,scalefactor);
	  track=NextTrack(track);
	}
}


static float GetScaleFactor(struct Tracker_Windows *window){
  ReqType reqtype = GFX_OpenReq(window, 50, 20, "");

  GFX_WriteString(reqtype, "Pitch shrink/expand\n");
  GFX_WriteString(reqtype, "Shrink range: 0-1\n");
  GFX_WriteString(reqtype, "Expand range: 1-10\n");
  
  float ret = GFX_GetFloat(window,reqtype," (0-10): ",0.0f,10.0f,true);

  GFX_CloseReq(window, reqtype);
  
  return ret;  
}


void PExpandRange_CurrPos(
			  struct Tracker_Windows *window,
			  float scalefactor
){
	if(!window->wblock->range.enabled) return;

        scalefactor=GetScaleFactor(window);
        if (scalefactor<0)
          return;	

        PC_Pause();{
          ADD_UNDO(Range(
                         window,
                         window->wblock,
                         window->wblock->range.x1,
                         window->wblock->range.x2+1,
                         window->wblock->curr_realline
                         ));
          
          PExpandRange(window->wblock,scalefactor);
          
        }PC_StopPause(window);

        UpdateAndClearSomeTrackReallinesAndGfxWTracks(
                                                      window,
                                                      window->wblock,
                                                      window->wblock->range.x1,
                                                      window->wblock->range.x2
                                                      );

}

void PExpandTrack_CurrPos(
			  struct Tracker_Windows *window,
			  float scalefactor
){

        scalefactor=GetScaleFactor(window);
	if(scalefactor<0)
          return;
        
        PC_Pause();{
          ADD_UNDO(Notes_CurrPos(window));
          
          PExpandTrack(window->wblock->block,window->wblock->wtrack->track,scalefactor);

        }PC_StopPause(window);
        
	UpdateAndClearSomeTrackReallinesAndGfxWTracks(
		window,
		window->wblock,
		window->wblock->wtrack->l.num,
		window->wblock->wtrack->l.num
	);

}


void PExpandBlock_CurrPos(
			  struct Tracker_Windows *window,
			  float scalefactor
){

	scalefactor=GetScaleFactor(window);
        if (scalefactor < 0)
          return;
        
        PC_Pause();{
          
          ADD_UNDO(Range(
                         window,
                         window->wblock,
                         0,window->wblock->block->num_tracks,
                         window->wblock->curr_realline
                         ));
          
          PExpandBlock(window->wblock->block,scalefactor);
          
        }PC_StopPause(window);
        
	UpdateAndClearSomeTrackReallinesAndGfxWTracks(
		window,
		window->wblock,
		0,
		window->wblock->block->num_tracks-1
	);

}









