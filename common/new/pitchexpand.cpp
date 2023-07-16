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


static int g_minnote;
static int g_maxnote;


#define SCALE(x,x1,x2,y1,y2) ((y1)+(((x)-(x1))*((y2)-(y1)))/((x2)-(x1)))


static void PExpand_SetMinMax(
                              struct Tracks *track,
			      const Place *p1,
			      const Place *p2,
			      bool firsttime
			      )
{
  const Ratio r1 = place2ratio(*p1);
  const Ratio r2 = place2ratio(*p2);

  const r::NoteTimeData::Reader reader(track->_notes2);
  
  for(const r::NotePtr &note : reader.get_iterator_left(r1)) {
    
    if(note->get_time() >= r1) {

      if(note->get_time() >= r2)
        return;

      if(firsttime==false){
        g_maxnote=R_MAX(note->_val,g_maxnote);
        g_minnote=R_MIN(note->_val,g_minnote);
      }else{
        g_minnote=127;
        g_maxnote=0;
        firsttime=false;
      }

      const r::PitchTimeData::Reader pitch_reader(&note->_pitches);
      for(const r::Pitch &pitch : pitch_reader){

        if(pitch._time >= r2)
          break;

        if (pitch._time >= r1) {
          g_maxnote=R_MAX(pitch._val,g_maxnote);
          g_minnote=R_MIN(pitch._val,g_minnote);
        }
                  
      }

      if (note->d._end < r2 && !equal_floats(note->d._pitch_end, 0.0)) {
        g_maxnote=R_MAX(note->d._pitch_end,g_maxnote);
        g_minnote=R_MIN(note->d._pitch_end,g_minnote);
      }
    }
  }
}

static int expandit(float note, float scalefactor){
  
  float midnote=(g_minnote+g_maxnote)/2.0f;

  float ret = (int) (0.5f + SCALE(note, 
                                  g_minnote, 
                                  g_maxnote, 
                                  midnote - ( (midnote-g_minnote) * scalefactor),
                                  midnote + ( (midnote-g_minnote) * scalefactor)
                                  )
                     );
  
  ret = R_MAX(1,R_MIN(127,ret));

  return ret;
}

static void PExpand_DoIt(
                         struct Tracks *track,
                         const Place *p1,
                         const Place *p2,
                         float scalefactor
                         )
{
  if(g_minnote==g_maxnote) return;

  const Ratio r1 = place2ratio(*p1);
  const Ratio r2 = place2ratio(*p2);
        
  r::NoteTimeData::Writer writer(track->_notes2);

  for(r::NotePtr &note_ : writer.get_iterator_left(r1)) {
          
    if(note_->get_time() >= r1) {
      
      if (note_->get_time() >= r2)
        return;

      r::ModifyNote note(note_);

      note->_val = expandit(note->_val, scalefactor);

      r::PitchTimeData::Writer pitch_writer(&note->_pitches);
      for(r::Pitch &pitch : pitch_writer){

        if(pitch._time >= r2)
          break;

        pitch._val = expandit(pitch._val, scalefactor);
      }
          
      if (note->d._end < r2 && !equal_floats(note->d._pitch_end, 0.0))
        note->d._pitch_end = expandit(note->d._pitch_end, scalefactor);
    }
  }
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
	  PExpand_SetMinMax(track,p1,p2,firsttime);
	  firsttime=false;
	  track=NextTrack(track);
	}

	track=(struct Tracks *)ListFindElement1(&wblock->block->tracks->l,wblock->range.x1);
	for(lokke=0;lokke<=wblock->range.x2-wblock->range.x1;lokke++){
	  PExpand_DoIt(track,p1,p2,scalefactor);
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
	
	PExpand_SetMinMax(track,&p1,&p2,true);
	
        PExpand_DoIt(track,&p1,&p2,scalefactor);
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
	  PExpand_SetMinMax(track,p1,&p2,firsttime);
	  firsttime=false;
	  track=NextTrack(track);
	}
	//midnote=(g_minnote+g_maxnote)/2.0f;	
	//printf("midnote: %f, m1:%d/%d, m2: %f/%f\n", midnote,g_minnote,g_maxnote, midnote - ( (midnote-g_minnote) * scalefactor), midnote + ( (midnote-g_minnote) * scalefactor));

	track=block->tracks;
	while(track!=NULL){
	  PExpand_DoIt(track,p1,&p2,scalefactor);
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
