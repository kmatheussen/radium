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

#include <QColor>

#include "nsmtracker.h"
#include "gfx_wtext_proc.h"
#include "visual_proc.h"
#include "list_proc.h"
#include "vector_proc.h"
#include "gfx_wtrackheader_volpan_proc.h"
#include "blts_proc.h"
#include "wtracks_proc.h"
#include "../audio/SoundPlugin.h"
#include "../Qt/Qt_instruments_proc.h"
#include "OS_visual_input.h"
#include "instruments_proc.h"

#include "../api/api_proc.h"
#include "../Qt/Qt_colors_proc.h"


#include "gfx_wtrackheaders_proc.h"


extern char *NotesTexts3[];

static void DrawWTrackNames(
                     struct Tracker_Windows *window,
                     const struct WBlocks *wblock,
                     int starttrack,
                     int endtrack
                     )
{

  struct WTracks *wtrack1 = (struct WTracks *)ListFindElement1(&wblock->wtracks->l, starttrack);
  struct WTracks *wtrack2 = (struct WTracks *)ListFindElement1(&wblock->wtracks->l, endtrack);

  struct Patch *patch = wtrack1->track->patch;
  
  int x1 = wtrack1->x + 2;
  int x2 = wtrack2->x2 - 1;
  int y1 = wtrack1->y;
  int y2 = wtrack1->panonoff.y1 - 2;

  // Background
  unsigned int color = patch==NULL ? GFX_get_color(HIGH_EDITOR_BACKGROUND_COLOR_NUM) : get_displayed_instrument_color(patch).rgb();
  bool is_current_track = get_current_instruments_gui_patch()==patch;
  
  if(is_current_track)
    GFX_SetMixColor2(window, WHITE_COLOR_NUM, color, 150);
  else
    GFX_SetMixColor2(window, WHITE_COLOR_NUM, color, 0);
    
  GFX_T_FilledBox(window, CURSOR_EDIT_ON_COLOR_NUM,
                  x1,y1,x2,y2,
                  PAINT_BUFFER);

  GFX_CancelMixColor(window); // in case track is not visible and the above filledbox call is not executed, the mixcolor will be set for the next paint operation instead. Bad stuff, caused by radium originally being written for amigaos, where painting outside the visible area would cause memory corruption (instead of being ignored). Unfortunately, the cliprect system was wrongly put into common/ instead of amiga/.

  // Track border
  if (endtrack > starttrack){    
    struct WTracks *wtrack = NextWTrack(wtrack1);

    while(wtrack != NULL){
      //for(int i=0;i<num_tracks;i++){
      //printf("%d: %d\n", i, draw_border[i]);

      //struct WTracks *next = NextWTrack(wtrack2);

      //printf("    Drawing track header border for track %d\n", wtrack->l.num);
      
      GFX_Line(window,TRACK_SEPARATOR2B_COLOR_NUM,
               wtrack->x, wtrack->y,
               wtrack->x, wtrack->panonoff.y1 - 1,
               PAINT_BUFFER);

      if (wtrack==wtrack2)
        break;
      
      wtrack = NextWTrack(wtrack);
    }
  }

  // Text
  GFX_SetClipRect(window,x1, 0, x2, wblock->t.y1, PAINT_BUFFER);
  {
    static char temp[500];
    int wtrack_x1 = R_MAX(wtrack1->x, wblock->t.x1) + window->fontwidth/2;

    if (wtrack1->l.num==wtrack2->l.num)
      sprintf(temp,"%d:", wtrack1->l.num);
    else
      sprintf(temp,"%d->%d:", wtrack1->l.num, wtrack2->l.num);
    GFX_T_Text(
               window,TEXT_COLOR_NUM,temp,
               wtrack_x1,
               wtrack1->y+WTRACKS_SPACE-1,
               wtrack1->x2-wtrack1->x-1,
               TEXT_CLIPRECT|TEXT_BOLD|TEXT_NOTEXT,
               PAINT_BUFFER
               );
    
    int name_x = wtrack_x1 + GFX_get_text_width(window,temp) + window->fontwidth;
    const char *name = patch==NULL ? wtrack1->track->trackname : patch->name;
    int midi_channel = ATOMIC_GET(wtrack1->track->midi_channel);
    if (midi_channel){
      if (patch==NULL || patch->instrument!=get_MIDI_instrument()){
        snprintf(temp, 498, "%s [%d]", name, midi_channel+1);
        name = (const char*)&temp;
      }
    }
    GFX_T_Text(
               window,INSTRUMENT_NAME_COLOR_NUM, name,
               name_x,
               wtrack1->y+WTRACKS_SPACE-1,
               wtrack2->x2 - name_x, //(wtrack2->x2-window->fontwidth/2) - name_x,
               TEXT_SCALE|TEXT_NOTEXT, //|TEXT_CENTER,
               PAINT_BUFFER
               );
  }
  GFX_CancelClipRect(window,PAINT_BUFFER);
}

static void DrawAllWTrackNames(
                               struct Tracker_Windows *window,
                               const struct WBlocks *wblock
                               )
{  
  //int num_tracks = ListFindNumElements1(&wblock->wtracks->l);
  //bool *draw_border = alloca(sizeof(bool) * num_tracks);
  //memset(draw_border, 0, sizeof(bool) * num_tracks);

  {
    const struct WTracks *wtrack1 = get_leftmost_visible_wtrack(wblock);
    if (wtrack1==NULL)
      return;

    const struct WTracks *rightmost_wtrack = get_rightmost_visible_wtrack(wblock, wtrack1);
    R_ASSERT_RETURN_IF_FALSE(rightmost_wtrack!=NULL);
                      
    int tracknum1 = wtrack1->l.num;
    
    int channelnum1 = ATOMIC_GET(wtrack1->track->midi_channel);
    struct Patch   *patch1   = wtrack1->track->patch;
    struct WTracks *wtrack2  = NextWTrack(wtrack1);
    int tracknum2            = tracknum1;

    for(;;){
      int channelnum2 = wtrack2==NULL ? 0 : ATOMIC_GET(wtrack2->track->midi_channel);
    
      if (wtrack2==NULL || wtrack2->track->patch==NULL || patch1==NULL || wtrack2->track->patch != patch1 || channelnum2 != channelnum1){
      
        DrawWTrackNames(window, wblock, tracknum1, tracknum2);
        tracknum1 = tracknum2 = tracknum2+1;
        channelnum1 = channelnum2;
        patch1 = wtrack2==NULL ? NULL : wtrack2->track->patch;

        if (tracknum1 > rightmost_wtrack->l.num)
          break;
      
      } else {
        tracknum2++;
      }

      if (wtrack2==NULL)
        break;

      //if (tracknum2 - tracknum1 >= 1)
      //  draw_border[wtrack2->l.num] = true;
    
      wtrack2 = NextWTrack(wtrack2);
    }
  }
  

}

static void DrawAllWTrackSliders(
                                 struct Tracker_Windows *window,
                                 const struct WBlocks *wblock
                                 )
{
  ITERATE_VISIBLE_WTRACKS(wblock) {
    UpdatePanSlider(window,wblock,wtrack);
    GFX_CancelMixColor(window); // in case track is not visible and the above filledbox call is not executed, the mixcolor will be set for the next paint operation instead. Bad stuff, caused by radium originally being written for amigaos, where painting outside the visible area would cause memory corruption (instead of being ignored). Unfortunately, the cliprect system was wrongly put into common/ instead of amiga/.
    UpdateVolumeSlider(window,wblock,wtrack);
  }

  GFX_CancelMixColor(window); // in case track is not visible and the above filledbox call is not executed, the mixcolor will be set for the next paint operation instead. Bad stuff, caused by radium originally being written for amigaos, where painting outside the visible area would cause memory corruption (instead of being ignored). Unfortunately, the cliprect system was wrongly put into common/ instead of amiga/.
}

static void DrawAllWTrackOnOffs(
                                struct Tracker_Windows *window,
                                const struct WBlocks *wblock
                                )
{

  ITERATE_VISIBLE_WTRACKS(wblock) {

	if(wtrack->track->onoff==0){
		GFX_T_Line(window,WAVEFORM_COLOR_NUM,
                           wtrack->x+2,wtrack->y+1,
                           wtrack->x2-2,wtrack->y+(window->org_fontheight*2)-1,
                           PAINT_BUFFER
		);
		GFX_T_Line(window,WAVEFORM_COLOR_NUM,
                           wtrack->x2-2,wtrack->y+1,
                           wtrack->x+2,wtrack->y+(window->org_fontheight*2)-1,
                           PAINT_BUFFER
		);
	}

  }
}

#if 0
static void DrawAllPianoRollHeaders_old(
                                    struct Tracker_Windows *window,
                                    struct WBlocks *wblock
                                    )
{  
  struct WTracks *wtrack=(struct WTracks *)ListFindElement1(&wblock->wtracks->l,wblock->left_track);
  
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


static vector_t g_pianorollheaders = {};

static void *get_pianorollheader(int tracknum, bool create_new){
  if (tracknum < 200 && tracknum>= g_pianorollheaders.num_elements)
    R_ASSERT("INIT_Pianoroll_headers not called");

  if (tracknum >= g_pianorollheaders.num_elements) {
    if (create_new)
      while (tracknum >= g_pianorollheaders.num_elements)
        VECTOR_push_back(&g_pianorollheaders, PIANOROLLHEADER_create());
    else
      return NULL;
  }
    
  return g_pianorollheaders.elements[tracknum];    
}

// Call this function first to avoid having to allocate pianorollheader widgets while playing.
void INIT_Pianoroll_headers(void){
  get_pianorollheader(200, true);
}


void UpdateAllPianoRollHeaders(
                               struct Tracker_Windows *window,
                               struct WBlocks *wblock
                               )
{
  const struct WTracks *wtrack=wblock->wtracks;

  int last_tracknum = 0;
  
  const struct WTracks *left_wtrack = get_leftmost_visible_wtrack(wblock);
  const struct WTracks *right_wtrack = get_rightmost_visible_wtrack(wblock, left_wtrack);

  if (left_wtrack!=NULL && right_wtrack!=NULL){
    while(wtrack!=NULL) {

      void *pianorollheader = get_pianorollheader(wtrack->l.num, true);
      
      if (wtrack->l.num < left_wtrack->l.num || wtrack->l.num > right_wtrack->l.num+1 || wtrack->pianoroll_area.x2 <= wblock->t.x1) {
        
        PIANOROLLHEADER_hide(pianorollheader);
        //if(wtrack->l.num==0)
        //  printf("Hiding header\n");
        
      } else {
        
        //struct Tracks *track = wtrack->track;
        
        int x1 = wtrack->x + 2;
        int x2 = wtrack->panonoff.x1 - 1;
        int y1 = wtrack->panonoff.y1 + 1;
        int y2 = wtrack->volumeonoff.y2;
        
        //if(wtrack->l.num==0)
        //  printf("Showing header\n");
        
        PIANOROLLHEADER_assignTrack(pianorollheader, wblock->l.num, wtrack->l.num);
        PIANOROLLHEADER_show(wblock, pianorollheader, x1, y1, x2, y2);
      }
      
      wtrack=NextWTrack(wtrack);
      last_tracknum++;
      if (wtrack!=NULL)
        R_ASSERT(last_tracknum==wtrack->l.num);
    }
  }

  // Make sure all pianoroll headers to the right of the rightmost track is hidden. (leftovers from earlier, for instance when showing a different block with more tracks)
  for(int tracknum = last_tracknum;;){
    void *pianorollheader = get_pianorollheader(tracknum, false);
    if (pianorollheader==NULL)
      break;
    PIANOROLLHEADER_hide(pianorollheader);
    tracknum++;
  }
}


void DrawAllWTrackHeaders(
                          struct Tracker_Windows *window,
                          const struct WBlocks *wblock
                          )
{  
#if 1
	GFX_T_FilledBox(
		window, HIGH_BACKGROUND_COLOR_NUM,
		wblock->t.x1, 0,
		window->width, wblock->t.y1,
                PAINT_BUFFER
	);
#endif
        DrawAllWTrackNames(window,wblock);

        DrawAllWTrackSliders(window, wblock);

        //UpdateAllPianoRollHeaders(window, wblock);

        DrawAllWTrackOnOffs(window, wblock);
}


