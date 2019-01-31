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


/*
  Warning: This code is fragile, most likely buggy, and generally horrible.

  If new functionality is needed, the current
  code should not be edited. Instead, just implement the new functionality
  by using the current functions somehow. Alternatively, rewrite everything.
 */



/***********************************************************************
  OOPS. There was a bug here. And it wasn't possibly to fix without
  rewriting a lot. So I hacked a bit...and, well, its not very easy to
  understand the code here now I guess. And its pretty slow too.
***********************************************************************/


#include <string.h>
#include <stdlib.h>
#include <math.h>

#include "nsmtracker.h"
#include "list_proc.h"
#include "localzooms_proc.h"
#include "windows_proc.h"
#include "tempos_proc.h"
#include "LPB_proc.h"
#include "temponodes_proc.h"
#include "fxlines_proc.h"
#include "clipboard_range_proc.h"
#include "undo_reallines_proc.h"
#include "player_proc.h"
#include "wblocks_proc.h"
#include "gfx_wblocks_proc.h"
#include "pixmap_proc.h"
#include "visual_proc.h"
#include "sliders_proc.h"
#include "OS_Player_proc.h"
#include "realline_calc_proc.h"
#include "scheduler_proc.h"

#include "../OpenGL/Widget_proc.h"

#include "reallines_proc.h"





/************************************************************************
  FUNCTION
    Update all structures that depends on the reallines for the wblock
    'wblock'.
************************************************************************/
void UpdateReallinesDependens(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){  
        //UpdateWTempos(window,wblock);
	//UpdateWLPBs(window,wblock);
#if !USE_OPENGL
	//UpdateWTempoNodes(window,wblock);
	UpdateAllFXNodeLines(window,wblock);
#endif
	wblock->isgfxdatahere=true;
}


/************************************************************************
  FUNCTION
    Returns the localzoom element that has its 'uplevel' attribute
    pointing to the first element in a localzoom list that the 'realline'
    belongs to. Returns NULL if it is already placed at level 0.

  NOTES
    'realline' allso gets the realline value for the new position
    localzoom 'localzoom' will get later if we are unexpanding.

    The next two procedures are to be seen as one function.
************************************************************************/
static const struct LocalZooms *FindLocalZoomRootTheHardWay(
                                                            const struct WBlocks *wblock,
                                                            const struct LocalZooms *localzoom,
                                                            const struct LocalZooms *org
){
        const struct LocalZooms *ret;

	while(localzoom!=NULL){
		if(localzoom==org) return org;

		if(localzoom->uplevel!=NULL){
			ret=FindLocalZoomRootTheHardWay(wblock,localzoom->uplevel,org);
			if(ret==org) return localzoom;
			if(ret!=NULL) return ret;
		}
		localzoom=NextLocalZoom(localzoom);
	}
	return NULL;
}

static const struct LocalZooms *FindLocalZoomRoot(
                                                  const struct WBlocks *wblock,
                                                  int *realline
){
	const struct LocalZooms **reallines=wblock->reallines;
	const struct LocalZooms *localzoom=reallines[*realline];
	const struct LocalZooms *org=localzoom;
	int level=localzoom->level;
//	int set=0;
	if(0==level) return NULL;

	while(localzoom->level>=level){
//		if(set==0 && localzoom->zoomline==0){set=1; org=localzoom;}
		(*realline)--;
		if(*realline<0){
			localzoom=wblock->localzooms;
			goto firstline;
		}
		localzoom=reallines[*realline];
	}

        if (localzoom==NULL)
          return NULL;
        
	localzoom=NextLocalZoom(localzoom);

firstline:
        
        if (localzoom==NULL)
          return NULL;
        
	while(localzoom->level<level-1) {          
          localzoom=localzoom->uplevel;
          if (localzoom==NULL)
            return NULL;
        }

	if(localzoom==NULL || localzoom->uplevel!=org){
		localzoom=FindLocalZoomRootTheHardWay(wblock,wblock->localzooms,org);
		*realline=localzoom->realline-1;
	}


	return localzoom;
	
}

/**************************************************************************
  FUNCTION
    Find out how many reallines there is in a wblock.
**************************************************************************/
static int FindNumberOfRealLines(
                                 const struct LocalZooms *localzoom,
                                 int realline
){
	while(localzoom!=NULL){
		if(localzoom->uplevel!=NULL){
			realline=FindNumberOfRealLines(localzoom->uplevel,realline);
                }else if(false && localzoom->level==0){
                  realline++;
                  realline++;
                }else if(false && localzoom->Tline%2) {
                  
                }else{
                  realline++;
		}
		localzoom=NextLocalZoom(localzoom);
	}
	return realline;
}

/************************************************************************
  FUNCTION
    When the localzooms in the wblock has been changed, or a wblock
    has just been made. (the next two procedures are to be seen on as
    one function.)
************************************************************************/

static int UpdateRealLinesRec(
	const struct LocalZooms **reallines,
	const struct LocalZooms *localzoom,
	int realline
){
	while(localzoom!=NULL){
                ((struct LocalZooms*)localzoom)->realline=realline; // not a problem for the OpenGL thread.
		if(localzoom->uplevel!=NULL){
			realline=UpdateRealLinesRec(reallines,localzoom->uplevel,realline);
                }else if(false && localzoom->level==0){
                  struct LocalZooms *lz = talloc(sizeof(struct LocalZooms));
                  lz->l.p.line    = localzoom->l.p.line;
                  lz->l.p.counter = 0;
                  lz->l.p.dividor = 2;
                  lz->realline    = realline;
                  lz->zoomline    = 0;
                  lz->level       = 1;
                  reallines[realline++]=lz;
                  {
                    struct LocalZooms *lz = talloc(sizeof(struct LocalZooms));
                    lz->l.p.line    = localzoom->l.p.line;
                    lz->l.p.counter = 1;
                    lz->l.p.dividor = 2;
                    lz->realline    = realline;
                    lz->zoomline    = 1;
                    lz->level       = 1;
                    reallines[realline++]=lz;
                  }
                }else if(false && localzoom->Tline%2) {
		}else{
                  reallines[realline++]=localzoom;
                }
		localzoom=NextLocalZoom(localzoom);
	}
	return realline;
}

static const struct LocalZooms **get_shrinked_reallines(const struct Tracker_Windows *window, const struct WBlocks *wblock, int factor, int *num_returned_reallines){
  R_ASSERT(factor>1);

  int realline = 0;
  int line = 0;

  int array_length = wblock->block->num_lines;
  const struct LocalZooms **reallines = talloc(array_length * sizeof(struct LocalZooms *));
  
  while(line < wblock->block->num_lines){

    struct LocalZooms *lz = talloc(sizeof(struct LocalZooms));
    lz->l.p.line    = line;
    //lz->l.p.counter = 0;
    lz->l.p.dividor = 1;
    lz->realline    = realline;
    //lz->zoomline    = 0;
    lz->level       = 1;

    reallines[realline]=lz;

    line     += factor;
    realline += 1;
  }

  if (realline<2) // can not have less than 2 reallines in a block.
    return NULL;

  *num_returned_reallines = realline;

  return reallines;
}


static void set_curr_realline(struct WBlocks *wblock, int new_curr_realline){
  //printf("new_curr_realline: %d, num_reallines: %d\n",new_curr_realline,wblock->num_reallines);
  wblock->curr_realline = R_BOUNDARIES(0, new_curr_realline, wblock->num_reallines - 1);

  // Not necessary anymore since current opengl realline is set when rendering now. (this call also causes flickering)
  //GE_set_curr_realline(wblock->curr_realline);
}

static void set_curr_realline_from_place(const struct Tracker_Windows *window, struct WBlocks *wblock, const Place *curr_place){
  if (curr_place!=NULL) {

    const Place *curr_place2 = &wblock->reallines[wblock->curr_realline]->l.p;
    if (PlaceEqual(curr_place, curr_place2))
      return;
    
    set_curr_realline(wblock, roundf(FindReallineForF(wblock, 0, curr_place)));
  }
}

static const struct LocalZooms **GenerateExpandedReallines(const struct Tracker_Windows *window, const struct WBlocks *wblock, int *num_new_reallines){
  const struct LocalZooms *localzoom=wblock->localzooms;
    
  *num_new_reallines = FindNumberOfRealLines(localzoom,0);

  const struct LocalZooms **reallines = talloc(*num_new_reallines * sizeof(struct LocalZooms *));
    
  UpdateRealLinesRec(reallines,localzoom,0);
  //wblock->num_reallines=UpdateRealLinesRec(wblock->reallines,localzoom,0);

  return reallines;
}

static const struct LocalZooms **GenerateReallines(const struct Tracker_Windows *window, const struct WBlocks *wblock, int *num_new_reallines){
  if (wblock->num_expand_lines > 0)
    return GenerateExpandedReallines(window, wblock, num_new_reallines);

  const struct LocalZooms **reallines = get_shrinked_reallines(window, wblock, -wblock->num_expand_lines, num_new_reallines);
  
  if (reallines==NULL) // 'get_shrinked_reallines' fails if trying to shrink too much.
    return GenerateExpandedReallines(window, wblock, num_new_reallines);
  else
    return reallines;
}

static void UpdateRealLines_internal(struct Tracker_Windows *window,struct WBlocks *wblock, bool update_curr_realline){
  const Place *curr_place = NULL;

  // To be sure it's fine.
  LegalizeLocalZooms(wblock);
    
  if (update_curr_realline && wblock->reallines!=NULL && wblock->curr_realline<wblock->num_reallines)
    curr_place = &wblock->reallines[wblock->curr_realline]->l.p;

  {
    int num_reallines;
    const struct LocalZooms **reallines = GenerateReallines(window, wblock, &num_reallines);

    //if(num_reallines==wblock->block->num_lines)
    //  wblock->zoomlinearea.width=0;

    PLAYER_lock();{
      wblock->reallines = reallines;
      wblock->num_reallines = num_reallines;

      // don't think mutation of curr_realline needs to be locked, but it doesn't really hurt either.
      if(wblock->curr_realline >= wblock->num_reallines)        
        wblock->curr_realline = wblock->num_reallines-1;

    }PLAYER_unlock();
  }
  
  set_curr_realline_from_place(window, wblock, curr_place);

  reschedule_reallines_because_num_reallines_have_changed_in_wblock(wblock);
    
  UpdateWBlockCoordinates(window, wblock);
}



#if 0
int FindHighestLocalzoomLevel(struct WBlocks *wblock){
  int highest=0;
  int i;
  for(i=0;i<wblock->num_reallines;i++)
    highest = R_MAX(wblock->reallines[i]->level, highest);
  return highest;
}
#endif

#if 0
static int FindLargestZoomLineNum(struct WBlocks *wblock){
  int highest=0;
  int i;
  for(i=0;i<wblock->num_reallines;i++)
    if(wblock->reallines[i]->level>0)
      highest = R_MAX(wblock->reallines[i]->zoomline, highest);
  return highest;
}

void SetZoomLevelAreaWidth(const struct Tracker_Windows *window,
                           struct WBlocks *wblock)
{
  if(wblock->reallines==NULL){
    wblock->zoomlinearea.width = 0;
  }else{
    int largest = FindLargestZoomLineNum(wblock);
    if(largest==0 || largest==1)
      wblock->zoomlinearea.width = 0;
    else if(largest<10)
      wblock->zoomlinearea.width = window->fontwidth;
    else
      wblock->zoomlinearea.width = ((int)log10(largest)+1) * window->fontwidth;
  }
}
#endif

static void ExpandLineInternal(
                               const struct Tracker_Windows *window,
                               struct WBlocks *wblock,
                               int realline,
                               int num_newreallines,
                               bool autogenerated
){
        const struct LocalZooms **reallines = wblock->reallines;
	const struct LocalZooms  *localzoom = reallines[realline];
	int lokke;

	if(localzoom->Tdividor*num_newreallines>=MAX_UINT32){
		fprintf(stderr,"Too many levels, can't expand.\n");
		return;
	}
	for(lokke=0;lokke<num_newreallines;lokke++){
		NewLocalZoom(
                             (struct LocalZooms**)&localzoom->uplevel, // It's perfectly fine modifying individual fields in the localzoom elements in wblock->reallines.
                             localzoom->Tline,
                             (uint_32)lokke+(localzoom->Tcounter*num_newreallines),
                             (uint_32)num_newreallines * localzoom->Tdividor,
                             lokke,
                             localzoom->level+1,
                             lokke+localzoom->Tline,
                             autogenerated
                             );
	}
}

static bool ensure_positive_expand_lines(struct WBlocks *wblock){
  if (wblock->num_expand_lines < 0) {
    GFX_Message2(NULL, true, "Currently not possible to zoom in on single line when LZ (Line zoom) is less than 1/1");
    return false;
  }
    
  return true;
}

static void ExpandLine(
                       struct Tracker_Windows *window,
                       struct WBlocks *wblock,
                       int realline,
                       int num_newreallines
){

  if (!ensure_positive_expand_lines(wblock))
    return;

  ExpandLineInternal(window,wblock,realline,num_newreallines,false);

  UpdateRealLines_internal(window, wblock, true);

  //UpdateWBlockCoordinates(window, wblock);
  //SetZoomLevelAreaWidth(window,wblock);

  MakeRangeLegal(wblock);
}

void ExpandLineCurrPos(
                       struct Tracker_Windows *window,
                       int num_newreallines
){


	struct WBlocks *wblock=window->wblock;

        if (!ensure_positive_expand_lines(wblock))
          return;

	ADD_UNDO(Reallines_CurrPos(window));

        ExpandLine(window,wblock,wblock->curr_realline,num_newreallines);
          
        UpdateReallinesDependens(window,wblock);

	window->must_redraw = true;
}

/*
static int FindNumReallinesFor(const struct LocalZooms *localzoom){
	int ret=0;

	while(localzoom!=NULL){
		if(localzoom->uplevel!=NULL){
			ret+=FindNumReallinesFor(localzoom->uplevel);
		}else{
			ret++;
		}
		localzoom=NextLocalZoom(localzoom);
	}
	return ret;
}
*/

void Unexpand(struct Tracker_Windows *window,struct WBlocks *wblock,int realline){

  if (!ensure_positive_expand_lines(wblock))
    return;

  
	const struct LocalZooms *localzoom=FindLocalZoomRoot(wblock,&realline);
	if(localzoom==NULL) return;

	((struct LocalZooms *)localzoom)->uplevel=NULL;

	set_curr_realline(wblock, realline+1);

	UpdateRealLines_internal(window, wblock, true);


}

void UnexpandCurrPos(struct Tracker_Windows *window){

	struct WBlocks *wblock=window->wblock;
	int realline=wblock->curr_realline;

        if (!ensure_positive_expand_lines(wblock))
          return;

	ADD_UNDO(Reallines_CurrPos(window));

        Unexpand(window,wblock,realline);
          
        UpdateReallinesDependens(window,wblock);

	window->must_redraw = true;

}


void Zoom(struct Tracker_Windows *window,struct WBlocks *wblock,int numtozoom){

    if (!ensure_positive_expand_lines(wblock))
    return;

	int curr_realline_org = wblock->curr_realline;
	int curr_realline     = curr_realline_org;
	int num_reallines     = wblock->num_reallines;
	int num_toexpand;
	//int zoomlineareawidth;

	//PlayStop();

	//zoomlineareawidth=wblock->zoomlinearea.width;

	ADD_UNDO(Reallines_CurrPos(window));

        {

          Unexpand(window,wblock,curr_realline);

          curr_realline=wblock->curr_realline;

          num_toexpand=(num_reallines-wblock->num_reallines)+numtozoom+1;

          if(num_toexpand>1){

            ExpandLine(window,wblock,curr_realline,num_toexpand);

            set_curr_realline(wblock, curr_realline_org);

          }

#if 0
          if(wblock->num_reallines==wblock->block->num_lines){
		wblock->zoomlinearea.width=0;
          }
#endif

          UpdateReallinesDependens(window,wblock);
        }

        window->must_redraw = true;
}

static void LineZoomBlock_internal(struct Tracker_Windows *window, struct WBlocks *wblock, int num_lines, bool update_curr_realline){

  int realline;

  if (num_lines==-1 || num_lines==0){
    if(num_lines==0)
      RError("num_expand_lines can not be 0 (divide by zero)");
    num_lines = 1;
  }

  window->must_redraw = true;

  if (num_lines<1) {
    wblock->num_expand_lines = num_lines;
    UpdateRealLines_internal(window, wblock, update_curr_realline);
    GFX_UpdateUpperLeft(window, wblock);
    set_curr_realline(wblock, wblock->curr_realline); // legalize curr realline
    return;
  }

  if (num_lines==1)
    wblock->num_expand_lines = num_lines;

  R_ASSERT(wblock->curr_realline>=0);
  R_ASSERT(wblock->curr_realline<wblock->num_reallines);
  
  int curr_realline = wblock->curr_realline;
  Place curr_place = wblock->reallines[curr_realline]->l.p;

  {
    const struct LocalZooms *localzoom = wblock->localzooms;
    while(localzoom != NULL) {
      if (localzoom->uplevel != NULL)
        if (localzoom->uplevel->autogenerated==true)
          ((struct LocalZooms *)localzoom)->uplevel = NULL;
      localzoom = NextLocalZoom(localzoom);
    }

    UpdateRealLines_internal(window,wblock,update_curr_realline);

    for(realline = wblock->num_reallines - 1; realline>=0 ; realline--){
      const struct LocalZooms *localzoom=wblock->reallines[realline];
      
      if(localzoom->Tcounter==0 && localzoom->level==0){
        if(num_lines>1)
          ExpandLineInternal(window,wblock,realline,num_lines,true);
      }
    }

    UpdateRealLines_internal(window,wblock,update_curr_realline);
    
    //UpdateWBlockCoordinates(window, wblock);
    //SetZoomLevelAreaWidth(window,wblock);

    MakeRangeLegal(wblock);

    UpdateReallinesDependens(window,wblock);

  }

  wblock->num_expand_lines = num_lines;

  if (update_curr_realline)
    set_curr_realline(wblock, (int)floorf(FindReallineForF(wblock, 0, &curr_place)));

  if (wblock->curr_realline <= wblock->num_reallines-2)
    if (wblock->reallines[wblock->curr_realline]->l.p.counter < curr_place.counter)
      if (wblock->reallines[wblock->curr_realline]->l.p.line == wblock->reallines[wblock->curr_realline+1]->l.p.line) {
        set_curr_realline(wblock, wblock->curr_realline+1);
      }

  GFX_UpdateUpperLeft(window, wblock);
}

void LineZoomBlock(struct Tracker_Windows *window, struct WBlocks *wblock, int num_lines){
  ADD_UNDO(Reallines_CurrPos(window));
  LineZoomBlock_internal(window, wblock, num_lines, true);
}

void LineZoomBlockInc(struct Tracker_Windows *window, struct WBlocks *wblock, int inc_num_lines){
  int num_expand_lines = wblock->num_expand_lines + inc_num_lines;

  if (num_expand_lines==0 || num_expand_lines==-1){ // 0 is a very illegal value (divide by zero), and -1 is the same as 1 (1/1 vs. 1).
    if (inc_num_lines > 0)
      num_expand_lines = 1;
    else
      num_expand_lines = -2;
  }

  printf("num_expand_lines: %d\n",num_expand_lines);
  
  LineZoomBlock(window,wblock,num_expand_lines);
}

int GetLineZoomBlock(struct WBlocks *wblock){
  return wblock->num_expand_lines;
}

static void UpdateRealLines_internal2(struct Tracker_Windows *window,struct WBlocks *wblock, bool update_curr_realline){
  //printf("numexpand: %d\n", wblock->num_expand_lines);

  if (wblock->reallines==NULL) {

#if !defined(RELEASE)
    // We always end up here after creating a new wblock.
    //if (g_is_loading==false && root->song->tracker_windows!=NULL && wblock->l.num < ListFindNumElements1(&root->song->tracker_windows->wblocks->l))
    //  abort();
#endif
    UpdateRealLines_internal(window, wblock, update_curr_realline);

  } else if (wblock->num_expand_lines==1) {

    UpdateRealLines_internal(window, wblock, update_curr_realline);

  } else {

    // Force recalculation of localzoom reallines when LZ!=1.
    LineZoomBlock_internal(window, wblock, wblock->num_expand_lines, update_curr_realline);

  }
}

void UpdateRealLines(struct Tracker_Windows *window,struct WBlocks *wblock){
  UpdateRealLines_internal2(window, wblock, wblock->reallines!=NULL);
}

void UpdateRealLines_dont_change_curr_realline(struct Tracker_Windows *window,struct WBlocks *wblock){
  UpdateRealLines_internal2(window, wblock, false);
}

