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

#include "nsmtracker.h"
#include "wblocks_proc.h"
#include "pixmap_proc.h"
#include "list_proc.h"
#include "common_proc.h"
#include "visual_proc.h"
#include "wtracks_proc.h"

#include "blts_proc.h"

/*
  The various mark functions mark up areas in the window that
  needs to be blted, and PixMap_blt blits these areas (without overlapping),
  to the visual window from the pixmap. The info used to do store this is the "blts"
  attribute in struct Tracker_Windows. The Blt_blt function is called
  after each event is processed, but can be called whenever
  if preferred. It should not blt things twice.
  
  Blt_markVisible marks the area that contains graphics.
  And when Blt_clearNotUsedVisible is called, it uses that information
  to clear any visible area that was painted before but now should be
  cleared. These two functions should only be called from the
  EventReceiver function.
*/


#if !USE_OPENGL

/****************************************************************
   Blitting.								 
*****************************************************************/

void Blt_blt(struct Tracker_Windows *window){
  struct WBlocks *wblock=window->wblock;
  Blt *blt=&window->blt;
  int x1,x2;

  if(blt->blt_doheader==true){
    
    Note: WTRACK_getx1 and WTRACK_getx2 has changed.
      
    x1=WTRACK_getx1(window,wblock,blt->starttrack,false);
    x2=WTRACK_getx2(window,wblock,blt->endtrack);
    QUEUE_GFX_P2V_bitBlt(
		   window,
		   x1,wblock->a.y1,
		   x1,wblock->a.y1,
		   
		   x2-x1+1,
		   wblock->t.y1-wblock->a.y1
		   );

    blt->blt_doheader=false;
  }


  if(blt->blt_do==false) return;

  if(WBlock_legalizeStartEndReallines(wblock,&blt->startrealline,&blt->endrealline)==false){
    RError("Something strange just happened in the function Blt_blt in the file blts.c\n"
	   "One or more of the Blt_mark* functions has got faulty realline values");
    blt->blt_do=false;
    return;
  }


  PixMap_bltLines(
		  window,
		  blt->startrealline-wblock->top_realline,
		  blt->endrealline-wblock->top_realline,
		  blt->x1,R_MIN(blt->x2,wblock->t.x2)
		  );

  blt->blt_do=false;

//  printf("blitted. X: %d,%d  Realline: %d,%d\n",blt->x1,min(blt->x2,wblock->t.x2),blt->startrealline,blt->endrealline);

}



/****************************************************************
   Marking.
 ***************************************************************/
void Blt_mark(
	      struct Tracker_Windows *window,
	      int startrealline,int endrealline,
	      int x1,int x2
){
  Blt *wblt=&window->blt;
  struct WBlocks *wblock=window->wblock;

  x2 = R_MIN(wblock->a.x2,x2);

  if(x1>=wblock->a.x2)
    return;

  if(x2<=x1){
    RError("Error in function Blt_mark. x1,x2: %d,%d. min,max: %d,%d\n",x1,x2,wblock->a.x1,wblock->a.x2);
    return;
  }

  if(wblt->blt_do==false){
    wblt->x1=x1;
    wblt->x2=x2;
    wblt->startrealline=startrealline;
    wblt->endrealline=endrealline;
    wblt->blt_do=true;
    return;
  }

  wblt->x1=R_MIN(wblt->x1,x1);
  wblt->x2=R_MAX(wblt->x2,x2);

  wblt->startrealline=R_MIN(wblt->startrealline,startrealline);
  wblt->endrealline=R_MAX(wblt->endrealline,endrealline);

  wblt->blt_do=true;
}


void Blt_marktrackheader(
	struct Tracker_Windows *window,
	NInt starttrack,
	NInt endtrack
	){
  Blt *blt=&window->blt;

  if(blt->blt_doheader==false){
    blt->starttrack=starttrack;
    blt->endtrack=endtrack;
    blt->blt_doheader=true;
  }else{
    blt->starttrack=R_MIN(blt->starttrack,starttrack);
    blt->endtrack=R_MAX(blt->endtrack,endtrack);
  }
}


void Blt_marktrack(
	struct Tracker_Windows *window,
	NInt starttrack,
	NInt endtrack,
	bool starttrack_onlyfxarea,
	int startrealline,
	int endrealline
){
	struct WBlocks *wblock=window->wblock;
	struct WTracks *wtrack=NULL;
	int nx1,nx2;
    
	switch(starttrack){
	case LINENUMBTRACK:
		nx1=wblock->linenumarea.x;
		break;
	case TEMPOCOLORTRACK:
		nx1=wblock->tempocolorarea.x;
		break;
	case LPBTRACK:
		nx1=wblock->lpbTypearea.x;
		break;
	case TEMPOTRACK:
		nx1=wblock->tempoTypearea.x;
		break;
	case TEMPONODETRACK:
		nx1=wblock->temponodearea.x;
		break;
	default:
		if(starttrack>wblock->right_track){
			RError("illegal starttrack at function Blt_mar in file blts.c");
			return;
		}
		wtrack=ListFindElement1(&wblock->wtracks->l,starttrack);
		if(starttrack_onlyfxarea==false){
			nx1=wtrack->notearea.x;
		}else{
			nx1=wtrack->fxarea.x;
		}
		nx1=R_MIN(nx1,wblock->t.x2);
		break;
	}
	
	switch(endtrack){
	case LINENUMBTRACK:
		nx2=wblock->linenumarea.x2;
		break;
	case TEMPOCOLORTRACK:
		nx2=wblock->tempocolorarea.x2;
		break;
	case LPBTRACK:
		nx2=wblock->lpbarea.x2;
		break;
	case TEMPOTRACK:
		nx2=wblock->tempoarea.x2;
		break;
	case TEMPONODETRACK:
		nx2=wblock->temponodearea.x2;
		break;
	default:
		if(endtrack>wblock->right_track){
			RError("illegal endtrack at function Blt_mar in file blts.c");
			return;
		}
		if(endtrack==wblock->right_track){
			nx2=wblock->t.x2;
		}else{
			wtrack=ListFindElement1(&wblock->wtracks->l,starttrack);
			nx2=wtrack->fxarea.x2;
			nx1=R_MAX(nx1,wblock->t.x1);
		}
		break;
	}
	
	Blt_mark(window,startrealline,endrealline,nx1,nx2);
	
}


// Called after a scroll.
void Blt_scrollMark(struct Tracker_Windows *window){

  Blt *wblt=&window->blt;

  wblt->x1=window->wblock->a.x1;
  wblt->x2=window->wblock->t.x2;
  wblt->startrealline=0;
  wblt->endrealline=window->wblock->num_reallines;
  wblt->blt_do=true;

}


void Blt_markFx(
		struct Tracker_Windows *window,
		NInt starttrack,
		NInt endtrack,
		int startrealline,
		int endrealline
		){
  Blt_marktrack(
	   window,
	   starttrack,
	   endtrack,
	   true,
	   startrealline,
	   endrealline
	   );
}

void Blt_markAll(
		struct Tracker_Windows *window,
		NInt starttrack,
		NInt endtrack,
		int startrealline,
		int endrealline
		){
  Blt_marktrack(
	   window,
	   starttrack,
	   endtrack,
	   false,
	   startrealline,
	   endrealline
	   );
}

void Blt_markTrack(
			struct Tracker_Windows *window,
			NInt tracknum
			){

  Blt_marktrack(
	   window,
	   tracknum,
	   tracknum,
	   false,
	   0,
	   window->wblock->num_reallines
	   );
}

void Blt_markSTrack(
			struct Tracker_Windows *window,
			NInt tracknum,
			int startrealline,
			int endrealline
			){

  Blt_marktrack(
	   window,
	   tracknum,
	   tracknum,
	   false,
	   startrealline,
	   endrealline
	   );
}

void Blt_markFxTrack(
			struct Tracker_Windows *window,
			NInt tracknum
			){
  Blt_marktrack(
	   window,
	   tracknum,
	   tracknum,
	   true,
	   0,
	   window->wblock->num_reallines
	   );
}

void Blt_markCurrTrack(
		       struct Tracker_Windows *window,
		       int startrealline,
		       int endrealline
		       ){ 
  NInt tracknum=(int)window->wblock->wtrack->l.num;
  Blt_marktrack(
	   window,
	   tracknum,
		tracknum,
	   false,
	   startrealline,
	   endrealline
	   );
}

void Blt_markCurrFxTrack(
		       struct Tracker_Windows *window,
		       int startrealline,
		       int endrealline
		       ){ 
  NInt tracknum=(int)window->wblock->wtrack->l.num;
  Blt_marktrack(
	   window,
	   tracknum,
	   tracknum,
	   true,
	   startrealline,
	   endrealline
	   );
}


/****************************************************************
   Clearing.								 
*****************************************************************/

void Blt_clearNotUsedVisible(struct Tracker_Windows *window){

  struct WBlocks *wblock=window->wblock;
  int start_realline=wblock->top_realline;
  int end_realline=wblock->bot_realline;
  Blt *blt=&window->blt;
  int v_y1,v_y2,v_x1,v_x2;

  if(blt->clear_do==false) return;


  if(WBlock_legalizeStartEndReallines(wblock,&start_realline,&end_realline)==false){
    RError("Something strange just happened in the function Blt_clearNotUsedVisible in the file blts.c\n");
    return;
  }
  v_y1=Common_oldGetReallineY1Pos(window,wblock,start_realline);
  v_y2=Common_oldGetReallineY2Pos(window,wblock,end_realline);

  v_x1=window->wblock->a.x1;
  v_x2=window->wblock->t.x2;

  if(blt->v_y1 < v_y1){
    GFX_FilledBox(
		  window,
		  0,
		  blt->v_x1,
		  blt->v_y1,
		  blt->v_x2,
		  v_y1-1,
                  PAINT_DIRECTLY
		  );
  }

  if(v_x1 > blt->v_x1){
    RError("In function Blt_clearNotUsedVisislbe in file blts.c: v_x1 > blt->v_x1\n");
  }

  if(blt->v_x2 > v_x2){
    GFX_FilledBox(
		  window,
		  0,
		  v_x2+1,
		  blt->v_y1,
		  blt->v_x2,
		  blt->v_y2,
                  PAINT_DIRECTLY
		  );
  }

  if(v_y2 < blt->v_y2){
    GFX_FilledBox(
		  window,
		  0,
		  blt->v_x1,
		  v_y2+1,
		  blt->v_x2,
		  blt->v_y2,
                  PAINT_DIRECTLY
		  );
  }
  blt->clear_do=false;

}

void  Blt_markVisible(struct Tracker_Windows *window){
  struct WBlocks *wblock=window->wblock;
  int start_realline=wblock->top_realline;
  int end_realline=wblock->bot_realline;
  Blt *blt=&window->blt;

  if(WBlock_legalizeStartEndReallines(wblock,&start_realline,&end_realline)==false){
    //RError("Something strange just happened in the function Blt_markVisible in the file blts.c. start_realline: %d, end_realline: %d\n",start_realline,end_realline);
    fprintf(stderr,"Something strange just happened in the function Blt_markVisible in the file blts.c. start_realline: %d, end_realline: %d\n",start_realline,end_realline);
    blt->clear_do=false;
    return;
  }
  blt->v_y1=Common_oldGetReallineY1Pos(window,wblock,start_realline);
  blt->v_y2=Common_oldGetReallineY2Pos(window,wblock,end_realline);

  blt->v_x1=window->wblock->a.x1;
  blt->v_x2=window->wblock->t.x2;

  blt->clear_do=true;
}

void Blt_unMarkVisible(struct Tracker_Windows *window){
  window->blt.clear_do=false;
}



#endif // !USE_OPENGL

