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
#include "gfx_wtext_proc.h"
#include "visual_proc.h"
#include "list_proc.h"
#include "gfx_wtrackheader_volpan_proc.h"
#include "blts_proc.h"
#include "../audio/SoundPlugin.h"
#include "../Qt/Qt_instruments_proc.h"

#include "gfx_wtrackheaders_proc.h"


void DrawWTrackNames(
                     struct Tracker_Windows *window,
                     struct WBlocks *wblock,
                     int starttrack,
                     int endtrack
                     )
{
  printf("Updating from %d to %d\n",starttrack,endtrack);
  //return;
  
  struct WTracks *wtrack1 = ListFindElement1(&wblock->wtracks->l, starttrack);
  struct WTracks *wtrack2 = ListFindElement1(&wblock->wtracks->l, endtrack);

  struct Patch *patch = wtrack1->track->patch;
  
  int x1 = wtrack1->x;
  int x2 = wtrack2->x2;
  int y1 = wtrack1->y;
  int y2 = wtrack1->panonoff.y1;

  // Background
  int colornum = patch==NULL ? 15 : patch->colornum;
  bool is_current_track = get_current_instruments_gui_patch()==patch;
  
  if(is_current_track)
    GFX_SetMixColor(window, 2, colornum, 150);
  else
    GFX_SetMixColor(window, 2, colornum, 0);
    
  GFX_T_FilledBox(window, 7,
                  x1,y1,x2,y2,
                  PAINT_BUFFER);

  GFX_CancelMixColor(window); // in case track is not visible and the above filledbox call is not executed, the mixcolor will be set for the next paint operation instead. Bad stuff, caused by radium originally being written for amigaos, where painting outside the visible area would cause memory corruption (instead of being ignored). Unfortunately, the cliprect system was wrongly put into common/ instead of amiga/.
  
  // Text
  GFX_SetClipRect(window,x1, 0, x2, wblock->t.y1, PAINT_BUFFER);
  {
    static char temp[500];
    sprintf(temp,"%d->%d:", wtrack1->l.num, wtrack2->l.num);
    GFX_T_Text(
               window,1,temp,
               wtrack1->x+window->fontwidth/2,
               wtrack1->y+WTRACKS_SPACE,
               wtrack1->x2-wtrack1->x-1,
               TEXT_CLIPRECT|TEXT_BOLD,
               PAINT_BUFFER
               );
    
    int name_x = wtrack1->x+window->fontwidth/2 + GFX_get_text_width(window,temp) + window->fontwidth;
    GFX_T_Text(
                   window,8,patch==NULL ? wtrack1->track->trackname : patch->name,
                   name_x,
                   wtrack1->y+WTRACKS_SPACE,
                   wtrack2->x2 - name_x, //(wtrack2->x2-window->fontwidth/2) - name_x,
                   TEXT_SCALE, //|TEXT_CENTER,
                   PAINT_BUFFER
                   );
  }
  GFX_CancelClipRect(window,PAINT_BUFFER);
}

static void DrawAllWTrackNames(
                               struct Tracker_Windows *window,
                               struct WBlocks *wblock
                               )
{  
  struct WTracks *wtrack1 = ListFindElement1(&wblock->wtracks->l,wblock->left_track);
  if (wtrack1==NULL)
    return;

  int tracknum1 = wtrack1->l.num;
  
  struct Patch   *patch1   = wtrack1->track->patch;
  struct WTracks *wtrack2  = NextWTrack(wtrack1);
  int tracknum2            = tracknum1;
  
  for(;;){
    if (wtrack2==NULL || wtrack2->track->patch==NULL || patch1==NULL || wtrack2->track->patch != patch1){
      
      DrawWTrackNames(window, wblock, tracknum1, tracknum2);
      tracknum1 = tracknum2 = tracknum2+1;
      patch1 = wtrack2==NULL ? NULL : wtrack2->track->patch;
      
    } else {
      tracknum2++;
    }

    if (wtrack2==NULL)
      break;
    else
      wtrack2 = NextWTrack(wtrack2);
  }
  
}

static void DrawAllWTrackSliders(
                                 struct Tracker_Windows *window,
                                 struct WBlocks *wblock
                                 )
{
  struct WTracks *wtrack=ListFindElement1(&wblock->wtracks->l,wblock->left_track);
  
  while(wtrack!=NULL && wtrack->l.num<=wblock->right_track){
    if(wtrack->x >= wblock->a.x2){
      break;
    }
    
    UpdatePanSlider(window,wblock,wtrack);
    UpdateVolumeSlider(window,wblock,wtrack);
    
    wtrack=NextWTrack(wtrack);
  }
}
                                 
void DrawAllWTrackHeaders(
                          struct Tracker_Windows *window,
                          struct WBlocks *wblock
                          )
{  

	GFX_T_FilledBox(
		window, 0,
		wblock->t.x1, 0,
		window->width, wblock->t.y1,
                PAINT_BUFFER
	);

        DrawAllWTrackNames(window,wblock);

        DrawAllWTrackSliders(window, wblock);
}


