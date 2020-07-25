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
#include "placement_proc.h"
#include "wtracks_proc.h"
#include "clipboard_range.h"

#include "clipboard_range_proc.h"



struct RangeClip *g_range_clips[NUM_RANGES] = {0};


void SetRange(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	NInt starttrack,
	NInt endtrack,
	Place startplace,
	Place endplace
){

  R_ASSERT_RETURN_IF_FALSE(p_Greater_Or_Equal(startplace, p_Create(0,0,1)));
  
  /*
    R_ASSERT_RETURN_IF_FALSE(p_Less_Or_Equal(endplace, p_Create(wblock->block->num_lines, 0, 1)));
  */

  if (p_Greater_Than(endplace, p_Absolute_Last_Pos(wblock->block)))
    endplace = p_Absolute_Last_Pos(wblock->block);

  if (p_Greater_Or_Equal(startplace, endplace))
    return;

  if (endtrack > wblock->block->num_tracks) {
    endtrack = wblock->block->num_tracks;
    if (endtrack < starttrack)
      return;
  }

                                   
  
  wblock->range.enabled=true;
  wblock->range.x1=starttrack;
  wblock->range.x2=endtrack;
  
  wblock->range.y1=startplace;
  wblock->range.y2=endplace;
}



static void MarkRange(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	NInt track,
	int realline
){
	NInt starttrack,endtrack;
	Place startplace, endplace;

        R_ASSERT_RETURN_IF_FALSE(realline>=0);
        R_ASSERT_RETURN_IF_FALSE(realline<wblock->num_reallines);

	if(wblock->range.enabled){

		starttrack=wblock->range.x1;
		endtrack=track;
		if(starttrack>endtrack){
			starttrack=track;
			endtrack=wblock->range.x2;
		}
		
		startplace=wblock->range.y1;

                if (realline+1 >= wblock->num_reallines)
                  endplace = p_Create(wblock->block->num_lines, 0, 1);
                else
                  endplace = wblock->reallines[realline+1]->l.p;

		if(p_Greater_Or_Equal(startplace, endplace)){
                  startplace = wblock->reallines[realline]->l.p;
                  endplace = wblock->range.y2;
		}

		while(p_Greater_Or_Equal(startplace, endplace)){
                  realline--;
                  R_ASSERT_RETURN_IF_FALSE(realline>=0);
                  startplace = wblock->reallines[realline]->l.p;
                }

	}else{

		endtrack=starttrack=track;

		int startrealline=realline;
		int endrealline=realline+1;

                if (startrealline < 0){
                  R_ASSERT_NON_RELEASE(false);
                  startrealline = 0;
                  endrealline = 1;
                }

                if (startrealline>=wblock->num_reallines){
                  R_ASSERT_NON_RELEASE(false);
                  startrealline = wblock->num_reallines -1 ;
                  endrealline = startrealline+1;
                }

                if (endrealline > wblock->num_reallines){
                  R_ASSERT_NON_RELEASE(false);
                  startrealline = wblock->num_reallines -1 ;
                  endrealline = startrealline+1;
                }
                
                startplace = wblock->reallines[startrealline]->l.p;

                if (endrealline >= wblock->num_reallines)
                  endplace = p_Create(wblock->block->num_lines, 0, 1);
                else
                  endplace = wblock->reallines[endrealline]->l.p;
	}

	SetRange(window,wblock,starttrack,endtrack,startplace,endplace);
}


void MarkRange_CurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;

	MarkRange(window,wblock,wblock->wtrack->l.num,wblock->curr_realline);

#if !USE_OPENGL
	bool isranged=wblock->isranged;
	NInt y1=wblock->range.y1;
	NInt y2=wblock->range.y2;

	if( isranged && (wblock->range.y1>y1 || wblock->range.y2<y2)){
		UpdateAndClearSomeTrackReallinesAndGfxWTracks(
			window,
			window->wblock,
			window->wblock->range.x1,
			window->wblock->range.x2
		);
	}else{
		UpdateAllWTracks(window,wblock,0,wblock->num_reallines);
	}
#endif

        window->must_redraw = true;
}


void CancelRange(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){
  wblock->range.enabled=false;
}

void CancelRange_CurrPos(struct Tracker_Windows *window){
	CancelRange(window,window->wblock);
#if !USE_OPENGL
	UpdateAndClearSomeTrackReallinesAndGfxWTracks(
		window,
		window->wblock,
		window->wblock->range.x1,
		window->wblock->range.x2
	);
#endif

        window->must_redraw = true;
}


/********************************************************
  FUNCTION
    Makes sure that the range is legal. Must be
    called after shrinking the number of reallines
    in a wblock, or the number of tracks.
********************************************************/
void MakeRangeLegal(
	struct WBlocks *wblock
){

	if(p_Greater_Or_Equal(wblock->range.y1, wblock->range.y2)){
          if (wblock->range.y1.line==0)
            wblock->range.y2 = p_Add(wblock->range.y1, p_Create(1,0,1));
          else
            wblock->range.y1 = p_Sub(wblock->range.y2, p_Create(1,0,1));
        };

        if (p_Greater_Than(wblock->range.y2, p_Create(wblock->block->num_lines, 0, 1)))
          wblock->range.y2=p_Create(wblock->block->num_lines, 0, 1);

        if (p_Greater_Than(wblock->range.y1, p_Create(wblock->block->num_lines, 0, 1))){
          wblock->range.y1=p_Create(wblock->block->num_lines-1, 0, 1);
          wblock->range.y2=p_Create(wblock->block->num_lines,   0, 1);
        }

        if(p_Greater_Or_Equal(wblock->range.y1, wblock->range.y2)){
          R_ASSERT_NON_RELEASE(false);
          wblock->range.y1 = p_Create(0,0,1);
          wblock->range.y1 = p_Create(1,0,1);
        }

	wblock->range.x2=R_MIN(wblock->range.x2,wblock->block->num_tracks-1);
	wblock->range.x1=R_MIN(wblock->range.x1,wblock->range.x2);
}

