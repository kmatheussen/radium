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
#include "vector_proc.h"
#include "gfx_wtrackheader_volpan_proc.h"
#include "blts_proc.h"
#include "../audio/SoundPlugin.h"
#include "../Qt/Qt_instruments_proc.h"
#include "OS_visual_input.h"

#include "gfx_wtrackheaders_proc.h"


extern char *NotesTexts3[];

void DrawWTrackNames(
                     struct Tracker_Windows *window,
                     struct WBlocks *wblock,
                     int starttrack,
                     int endtrack
                     )
{

  struct WTracks *wtrack1 = ListFindElement1(&wblock->wtracks->l, starttrack);
  struct WTracks *wtrack2 = ListFindElement1(&wblock->wtracks->l, endtrack);

  struct Patch *patch = wtrack1->track->patch;
  
  int x1 = wtrack1->x + 2;
  int x2 = wtrack2->x2 - 1;
  int y1 = wtrack1->y;
  int y2 = wtrack1->panonoff.y1 - 1;

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

static void DrawAllWTrackOnOffs(
                                struct Tracker_Windows *window,
                                struct WBlocks *wblock
                                )
{

  struct WTracks *wtrack=ListFindElement1(&wblock->wtracks->l,wblock->left_track);
  
  while(wtrack!=NULL && wtrack->l.num<=wblock->right_track){

	if(wtrack->track->onoff==0){
		GFX_T_Line(window,2,
                           wtrack->x+2,wtrack->y+1,
                           wtrack->x2-2,wtrack->y+(window->org_fontheight*2)-1,
                           PAINT_BUFFER
		);
		GFX_T_Line(window,2,
                           wtrack->x2-2,wtrack->y+1,
                           wtrack->x+2,wtrack->y+(window->org_fontheight*2)-1,
                           PAINT_BUFFER
		);
	}

        wtrack=NextWTrack(wtrack);
  }
}

#if 0
static void DrawAllPianoRollHeaders_old(
                                    struct Tracker_Windows *window,
                                    struct WBlocks *wblock
                                    )
{  
  struct WTracks *wtrack=ListFindElement1(&wblock->wtracks->l,wblock->left_track);
  
  while(wtrack!=NULL && wtrack->l.num<=wblock->right_track){

    //struct Tracks *track = wtrack->track;
    
    int x1 = wtrack->x + 2;
    int x2 = wtrack->panonoff.x1;
    int y1 = wtrack->panonoff.y1;
    //int y2 = wtrack->panonoff.y2;

    // Text
    //    GFX_SetClipRect(window,x1, 0, x2, wblock->t.y1, PAINT_BUFFER);
    {
      //static char temp[500];
      //sprintf(temp,"%d->%d:", wtrack1->l.num, wtrack2->l.num);
      int midpos1 = scale(1,0,3,x1,x2);
      int midpos2 = scale(2,0,3,x1,x2);

      //printf("_________ T_TEXT %d: %s %d %d %d. midpos: %d, x1: %d, x2: %d\n",wtrack->l.num,NotesTexts3[wtrack->pianoroll_lowkey],x1,y1,midpos-x1,midpos,x1,x2);
      GFX_T_Text(
                 window, 8, NotesTexts3[wtrack->pianoroll_lowkey],
                 x1,
                 y1+3,
                 midpos1 - x1,
                 TEXT_CLIPRECT|TEXT_SCALE|TEXT_CENTER|TEXT_BOLD,
                 PAINT_BUFFER
                 );

      printf("%d:, x1: %d, midpos1: %d, midpos2: %d, x2: %d\n",wtrack->l.num,x1, midpos1, midpos2, x2);
      
      GFX_T_Text(
                 window, 8, NotesTexts3[wtrack->pianoroll_highkey],
                 midpos2,
                 y1+3,
                 x2 - midpos2,
                 TEXT_CLIPRECT|TEXT_SCALE|TEXT_CENTER|TEXT_BOLD,
                 PAINT_BUFFER
                 );
    }
    //GFX_CancelClipRect(window,PAINT_BUFFER);

    wtrack=NextWTrack(wtrack);
  }
}
#endif


static vector_t g_pianorollheaders = {0};

static void *get_pianorollheader(int tracknum){
  while (tracknum >= g_pianorollheaders.num_elements)
    VECTOR_push_back(&g_pianorollheaders, PIANOROLLHEADER_create());

  return g_pianorollheaders.elements[tracknum];    
}

static void UpdateAllPianoRollHeaders(
                                      struct Tracker_Windows *window,
                                      struct WBlocks *wblock
                                      )
{

  struct WTracks *wtrack=wblock->wtracks;
  
  while(wtrack!=NULL) {

    void *pianorollheader = get_pianorollheader(wtrack->l.num);
    
    if (wtrack->l.num < wblock->left_track || wtrack->l.num > wblock->right_track+1) {

      if (pianorollheader != NULL)
        PIANOROLLHEADER_hide(pianorollheader);
      
    } else {

      //struct Tracks *track = wtrack->track;
      
      int x1 = wtrack->x + 2;
      int x2 = wtrack->panonoff.x1;
      int y1 = wtrack->panonoff.y1 + 2;
      int y2 = wtrack->volumeonoff.y2;
      
      PIANOROLLHEADER_assignTrack(pianorollheader, wblock->l.num, wtrack->l.num);
      PIANOROLLHEADER_show(pianorollheader, x1, y1, x2, y2);
    }
    
    wtrack=NextWTrack(wtrack);
  }
}


void DrawAllWTrackHeaders(
                          struct Tracker_Windows *window,
                          struct WBlocks *wblock
                          )
{  
#if 1
	GFX_T_FilledBox(
		window, 11,
		wblock->t.x1, 0,
		window->width, wblock->t.y1,
                PAINT_BUFFER
	);
#endif
        DrawAllWTrackNames(window,wblock);

        DrawAllWTrackSliders(window, wblock);

        DrawAllWTrackOnOffs(window, wblock);

        UpdateAllPianoRollHeaders(window, wblock);
}


