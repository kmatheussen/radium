/* Copyright 2014 Kjetil S. Matheussen

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

#include "../common/includepython.h"

#include "../common/nsmtracker.h"
#include "../common/placement_proc.h"
#include "../common/vector_proc.h"
#include "../common/list_proc.h"
#include "../common/undo.h"
#include "../common/undo_reltemposlider_proc.h"
#include "../common/gfx_wblocks_reltempo_proc.h"
#include "../common/time_proc.h"
#include "../common/trackreallines2_proc.h"
#include "../common/common_proc.h"
#include "../common/temponodes_proc.h"
#include "../common/undo_temponodes_proc.h"
#include "../common/realline_calc_proc.h"
#include "../common/notes_proc.h"
#include "../common/pitches_proc.h"
#include "../common/undo_notes_proc.h"
#include "../common/gfx_subtrack_proc.h"
#include "../common/velocities_proc.h"
#include "../common/visual_proc.h"
#include "../common/undo_fxs_proc.h"
#include "../common/fxlines_proc.h"
#include "../common/instruments_proc.h"
#include "../common/wblocks_proc.h"
#include "../common/OS_Player_proc.h"
#include "../common/player_proc.h"
#include "../common/player_pause_proc.h"
#include "../common/undo_trackheader_proc.h"
#include "../common/patch_proc.h"
#include "../common/nodelines_proc.h"
#include "../common/wtracks_proc.h"
#include "../common/undo_blocks_proc.h"
#include "../common/tempos_proc.h"
#include "../common/undo_tempos_proc.h"
#include "../common/seqtrack_proc.h"
#include "../common/sliders_proc.h"

#include "../mixergui/QM_MixerWidget.h"

#include "../OpenGL/Render_proc.h"

#include "api_common_proc.h"

#include "radium_proc.h"

#include "api_mouse_proc.h"


extern volatile float g_scroll_pos;



// various
///////////////////////////////////////////////////

void setCurrentNode(struct ListHeader3 *new_current_node){
  if (current_node != new_current_node){
    current_node = new_current_node;
    root->song->tracker_windows->must_redraw_editor = true;
    //printf("current node dirty\n");
  }
}

void cancelCurrentNode(void){
  setCurrentNode(NULL);
}

void setIndicatorNode(const struct ListHeader3 *new_indicator_node){
  if (indicator_node != new_indicator_node){
    indicator_node = new_indicator_node;
    root->song->tracker_windows->must_redraw_editor = true;
    //printf("indicator node dirty\n");
  }
}

void cancelIndicatorNode(void){
  setIndicatorNode(NULL);
  indicator_velocity_num = -1;
  indicator_pitch_num = -1;
}


float getHalfOfNodeWidth(void){
  return root->song->tracker_windows->fontheight / 1.5; // if changing 1.5 here, also change 1.5 in OpenGL/Render.cpp and common/SeqAutomation.hpp
}

float get_scroll_pos(void){
  return safe_volatile_float_read(&g_scroll_pos);
}

static float get_mouse_realline_y1(const struct Tracker_Windows *window, int realline){
  //printf("fontheight: %f\n",(float)window->fontheight);
  //printf("wblock->t.y1: %f. scroll_pos: %f\n",(float)window->wblock->t.y1,(float)scroll_pos);
  return window->fontheight*realline - get_scroll_pos() + window->wblock->t.y1;
}

static float get_mouse_realline_y2(const struct Tracker_Windows *window, int realline){
  return window->fontheight*(realline+1) - get_scroll_pos() + window->wblock->t.y1;
}

float getTopVisibleY(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if(wblock==NULL)
    return 0;

  return get_mouse_realline_y1(window, R_MAX(0, wblock->top_realline));
}

float getBotVisibleY(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if(wblock==NULL)
    return 0;

  return get_mouse_realline_y2(window, R_MIN(wblock->num_reallines-1, wblock->bot_realline));
}

void setMouseTrack(int tracknum){
  struct Tracker_Windows *window = root->song->tracker_windows;
  struct WBlocks *wblock = window->wblock;

  if(tracknum != wblock->mouse_track){
    wblock->mouse_track = tracknum;
    window->must_redraw_editor = true;
  }
}

void setNoMouseTrack(void){
  setMouseTrack(NOTRACK);
}

void setMouseTrackToReltempo(void){
  setMouseTrack(TEMPONODETRACK);
}


bool mousePointerInMixer(void){
  return MW_has_mouse_pointer();
}

// placement (block time)
///////////////////////////////////////////////////

Place getPlaceFromY(float y, int blocknum, int windownum) {
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if (wblock==NULL)
    return place(0,0,1);

  Place place;

  GetReallineAndPlaceFromY(window,
                           wblock,
                           y,
                           &place,
                           NULL,
                           NULL
                           );

  //printf("Got place %s for %f\n",PlaceToString(&place),y);
  return place;
}


static double get_gridded_abs_y(struct Tracker_Windows *window, float abs_y){
  double grid = (double)root->grid_numerator / (double)root->grid_denominator;

  float abs_realline = abs_y / window->fontheight;
  
  double rounded = round(abs_realline / grid);
    
  return rounded * grid * window->fontheight;
}

Place getPlaceInGridFromY(float y, int blocknum, int windownum) {
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if (wblock==NULL)
    return place(0,0,1);

  // This is a hack, but it's a relatively clean one, and modifying GetReallineAndPlaceFromY is a quite major work (old code, hard to understand, it should be rewritten).
  float abs_y = (y-wblock->t.y1) + wblock->top_realline*window->fontheight;
  float gridded_abs_y = get_gridded_abs_y(window, abs_y);
  float gridded_y = gridded_abs_y - wblock->top_realline*window->fontheight + wblock->t.y1;
  
  Place place;
  
  GetReallineAndPlaceFromY(window,
                           wblock,
                           gridded_y,
                           &place,
                           NULL,
                           NULL
                           );

  //printf("Got grid-place %s\n",PlaceToString(&place));
  
  return place;
}


static double get_next_gridded_abs_y(struct Tracker_Windows *window, float abs_y){
  double grid = (double)root->grid_numerator / (double)root->grid_denominator;

  float abs_realline = abs_y / window->fontheight;
  
  double rounded = round(abs_realline / grid) + 1.0;
    
  return rounded * grid * window->fontheight;
}

Place getNextPlaceInGridFromY(float y, int blocknum, int windownum) {
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if (wblock==NULL)
    return place(0,0,1);

  // This is a hack, but it's a relatively clean one, and modifying GetReallineAndPlaceFromY is a quite major work (old code, hard to understand, it should be rewritten).
  float abs_y = (y-wblock->t.y1) + wblock->top_realline*window->fontheight;
  float gridded_abs_y = get_next_gridded_abs_y(window, abs_y);
  float gridded_y = gridded_abs_y - wblock->top_realline*window->fontheight + wblock->t.y1;
  
  Place place;
  
  GetReallineAndPlaceFromY(window,
                           wblock,
                           gridded_y,
                           &place,
                           NULL,
                           NULL
                           );
  
  return place;
}


// bpm
///////////////////////////////////////////////////
int getNumBPMs(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if (wblock==NULL)
    return 0;

  return ListFindNumElements3(&wblock->block->tempos->l);
}


// reltempo
///////////////////////////////////////////////////

float getReltempoSliderX1(void){
  return root->song->tracker_windows->wblock->reltempo.x1;
}
float getReltempoSliderY1(void){
  return root->song->tracker_windows->wblock->reltempo.y1;
}
float getReltempoSliderX2(void){
  return root->song->tracker_windows->wblock->reltempo.x2;
}
float getReltempoSliderY2(void){
  return root->song->tracker_windows->wblock->reltempo.y2;
}

double getReltempo(int blocknum, int windownum){
  struct WBlocks *wblock = getWBlockFromNum(windownum, blocknum);
  if (wblock==NULL)
    return 0.0f;
  else
    return ATOMIC_DOUBLE_GET(wblock->block->reltempo);
}

void undoReltempo(void){
  struct Tracker_Windows *window = root->song->tracker_windows;
  ADD_UNDO(RelTempoSlider(window,window->wblock));
}

void setReltempo(double reltempo){
  //skew_x = scale(reltempo, 0, 6, 0, -1000);

  //printf("   skew_x: %f\n", skew_x);
  struct Tracker_Windows *window = root->song->tracker_windows;

  struct WBlocks *wblock = window->wblock;
  
  double new_reltempo = R_BOUNDARIES(
    MINBLOCKRELTIME,
    reltempo,
    MAXBLOCKRELTIME
  );
  
  ATOMIC_DOUBLE_SET(wblock->block->reltempo, new_reltempo);

  //update_statusbar(window);
  //DrawBlockRelTempo(window,wblock);

  SEQUENCER_update();

  window->must_redraw = true;
}


float getMinReltempo(void){
  return MINBLOCKRELTIME;
}

float getMaxReltempo(void){
  return MAXBLOCKRELTIME;
}



// The track scrollbar (horizontal scrollbar)
///////////////////////////////////////////////////

float getTrackSliderX1(void){
  return root->song->tracker_windows->bottomslider.x;
}
float getTrackSliderY1(void){
  return root->song->tracker_windows->wblock->reltempo.y1;
}
float getTrackSliderX2(void){
  return root->song->tracker_windows->bottomslider.x2;
}
float getTrackSliderY2(void){
  return root->song->tracker_windows->wblock->reltempo.y2;
}

float getTrackSliderScrollerX1(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(blocknum, &window, windownum);
  if(wblock==NULL)
    return 0.0;

  int inner_x1;
  int inner_x2;
  GetBottomSliderCoordinates(window, window->wblock, &inner_x1, &inner_x2);

  return inner_x1;
}

float getTrackSliderScrollerY1(void){
  return getTrackSliderY1();
}

float getTrackSliderScrollerX2(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(blocknum, &window, windownum);
  if(wblock==NULL)
    return 0.0;

  int inner_x1;
  int inner_x2;
  GetBottomSliderCoordinates(window, window->wblock, &inner_x1, &inner_x2);

  return inner_x2;
}

float getTrackSliderScrollerY2(void){
  return getTrackSliderY2();
}

float getTrackSliderPos(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(blocknum, &window, windownum);
  if(wblock==NULL)
    return 0;

  int total_width = WTRACKS_getWidth(window, wblock);

  return scale(wblock->skew_x, 0, -total_width, 0, 1);
}

void setTrackSliderPos(float pos, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(blocknum, &window, windownum);
  if(wblock==NULL)
    return;

  int visible_width = root->song->tracker_windows->bottomslider.x2 - root->song->tracker_windows->bottomslider.x;
  int total_width = WTRACKS_getWidth(window, wblock);

  if (visible_width >= total_width){
    R_ASSERT_NON_RELEASE(wblock->skew_x == 0);
    wblock->skew_x = 0;
    return;
  }

  wblock->skew_x = scale(pos, 0, 1, 0, -total_width);
  if (wblock->skew_x > 0)
    wblock->skew_x = 0;

  if (wblock->skew_x < -(total_width-visible_width))
    wblock->skew_x = -(total_width-visible_width);
  //calculateblockcoordinates must also ensure that the current track is visible.
  window->must_redraw=true;
}

void setTrackSliderIsMoving(bool is_moving, int windownum){
  struct Tracker_Windows *window = getWindowFromNum(windownum);
  if(window==NULL)
    return;

  window->track_slider_is_moving = is_moving;
  window->must_redraw=true;
}



// The line scrollbar (vertical scrollbar)
///////////////////////////////////////////////////


float getEditorScrollbarX1(void){
  return get_scrollbar_x1(root->song->tracker_windows);
}
float getEditorScrollbarY1(void){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(-1, &window, -1);
  if(wblock==NULL)
    return 0.0;
  return wblock->t.y1 + get_scrollbar_y1(window, wblock);
}
float getEditorScrollbarX2(void){
  return get_scrollbar_x2(root->song->tracker_windows);
}
float getEditorScrollbarY2(void){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(-1, &window, -1);
  if(wblock==NULL)
    return 0.0;
  return wblock->t.y1 + get_scrollbar_y2(window, wblock);
}


float getEditorScrollbarScrollerX1(void){
  return getEditorScrollbarX1();
}

float getEditorScrollbarScrollerY1(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(blocknum, &window, windownum);
  if(wblock==NULL)
    return 0.0;

  float scrollbar_height = get_scrollbar_y2(window, wblock) - get_scrollbar_y1(window, wblock);
  
  return window->wblock->t.y1 + get_scrollbar_scroller_y1(wblock->curr_realline, wblock->num_reallines, scrollbar_height, get_scrollbar_scroller_height(window, wblock));
}

float getEditorScrollbarScrollerX2(void){
  return getEditorScrollbarX2();
}

float getEditorScrollbarScrollerY2(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(blocknum, &window, windownum);
  if(wblock==NULL)
    return 0.0;

  return getEditorScrollbarScrollerY1(blocknum, windownum) + get_scrollbar_scroller_height(window, wblock);
}

void setEditorScrollbarIsMoving(bool is_moving, int windownum){
  struct Tracker_Windows *window = getWindowFromNum(windownum);
  if(window==NULL)
    return;

  window->scrollbar_is_moving = is_moving;
  window->must_redraw=true;
}


// track panning on/off
///////////////////////////////////////////////////

float getTrackPanOnOffX1(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0.0f;

  return wtrack->panonoff.x1;
}
float getTrackPanOnOffY1(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0.0f;

  return wtrack->panonoff.y1;
}
float getTrackPanOnOffX2(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0.0f;

  return wtrack->panonoff.x2;
}
float getTrackPanOnOffY2(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0.0f;

  return wtrack->panonoff.y2;
}

bool getTrackPanOnOff(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0.0f;

  return wtrack->track->panonoff;
}

void undoTrackPanOnOff(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;

  ADD_UNDO(TrackHeader(wblock->l.num, wtrack->l.num));
}

void setTrackPanOnOff(bool onoff, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;

  wtrack->track->panonoff = onoff;
        
  window->must_redraw = true;
}


// track volumening on/off
///////////////////////////////////////////////////

float getTrackVolumeOnOffX1(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0.0f;

  return wtrack->volumeonoff.x1;
}
float getTrackVolumeOnOffY1(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0.0f;

  return wtrack->volumeonoff.y1;
}
float getTrackVolumeOnOffX2(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0.0f;

  return wtrack->volumeonoff.x2;
}
float getTrackVolumeOnOffY2(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0.0f;

  return wtrack->volumeonoff.y2;
}

bool getTrackVolumeOnOff(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0.0f;

  return wtrack->track->volumeonoff;
}

void undoTrackVolumeOnOff(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;

  ADD_UNDO(TrackHeader(wblock->l.num, wtrack->l.num));
}

void setTrackVolumeOnOff(bool onoff, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;

  wtrack->track->volumeonoff = onoff;
  
  window->must_redraw = true;
}



// track panning slider
///////////////////////////////////////////////////

float getTrackPanSliderX1(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0.0f;

  return wtrack->pan.x1;
}
float getTrackPanSliderY1(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0.0f;

  return wtrack->pan.y1;
}
float getTrackPanSliderX2(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0.0f;

  return wtrack->pan.x2;
}
float getTrackPanSliderY2(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0.0f;

  return wtrack->pan.y2;
}

float getTrackPan(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0.0f;

  return scale(wtrack->track->pan, -MAXTRACKPAN, MAXTRACKPAN, -1.0, 1.0);
}

void undoTrackPan(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;

  ADD_UNDO(TrackHeader(wblock->l.num, wtrack->l.num));
}

// void setTrackPan(float pan, int tracknum, int blocknum, int windownum)
// was already (more correctly) implemented in api_various.c



// track volumening slider
///////////////////////////////////////////////////

float getTrackVolumeSliderX1(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0.0f;

  return wtrack->volume.x1;
}
float getTrackVolumeSliderY1(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0.0f;

  return wtrack->volume.y1;
}
float getTrackVolumeSliderX2(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0.0f;

  return wtrack->volume.x2;
}
float getTrackVolumeSliderY2(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0.0f;

  return wtrack->volume.y2;
}

float getTrackVolume(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0.0f;

  return scale(wtrack->track->volume, 0, MAXTRACKVOL, 0.0, 1.0);
}

void undoTrackVolume(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;

  ADD_UNDO(TrackHeader(wblock->l.num, wtrack->l.num));
}

// void setTrackVolume(float volume, int tracknum, int blocknum, int windownum)
// was already (more correctly) implemented in api_various.c



// editor positions
///////////////////////////////////////////////////

float getEditorX1(int windownum){
  return 0;
}

float getEditorX2(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL)
    return -1;

  return window->width;
}

float getEditorY1(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL)
    return -1;
  return window->wblock->t.y1;
}

float getEditorY2(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL)
    return -1;
  return window->wblock->t.y2;
}

// block positions
///////////////////////////////////////////////////

float getBlockHeaderY2(int blocknum, int windownum){
  struct WBlocks *wblock = getWBlockFromNum(windownum, blocknum);
  return wblock==NULL ? 0 : wblock->t.y1;
}


// tracks positions
///////////////////////////////////////////////////

float getTrackX1(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if(wblock==NULL)
    return 0;

  if (tracknum < LEFTMOSTTRACK || tracknum >= wblock->block->num_tracks){
    handleError("getTrackX2: No track %d", tracknum);
    return 0.0f;
  }
  
  return WTRACK_getx1(window, wblock, tracknum);
}

float getTrackY1(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->y;
}

float getTrackX2(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if(wblock==NULL)
    return 0;

  if (tracknum < LEFTMOSTTRACK || tracknum >= wblock->block->num_tracks){
    handleError("getTrackX2: No track %d", tracknum);
    return 0.0f;
  }
  
  return WTRACK_getx2(window, wblock, tracknum);
}

float getTrackY2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->y2;
}

float getTrackPianorollX1(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->pianoroll_area.x;
}

float getTrackPianorollY1(int tracknum, int blocknum, int windownum){
  return getBlockHeaderY2(blocknum, windownum);
}

float getTrackPianorollX2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->pianoroll_area.x2;
}

float getTrackPianorollY2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->y2;
}

float getTrackNotesX1(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->notearea.x;
}

float getTrackNotesY1(int tracknum, int blocknum, int windownum){
  return getBlockHeaderY2(blocknum, windownum);
}

float getTrackNotesX2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->notearea.x2;
}

float getTrackNotesY2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->y2;
}

float getTrackFxX1(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->fxarea.x;
}

float getTrackFxY1(int tracknum, int blocknum, int windownum){
  return getBlockHeaderY2(blocknum, windownum);
}

float getTrackFxX2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->fxarea.x2;
}

float getTrackFxY2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->y2;
}


/////

float getCenttextX1(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->centtextarea.x;
}

float getCenttextY1(int tracknum, int blocknum, int windownum){
  return getBlockHeaderY2(blocknum, windownum);
}

float getCenttextX2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->centtextarea.x2;
}

float getCenttextY2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->y2;
}

/////

float getSwingtextX1(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->swingtextarea.x;
}

float getSwingtextY1(int tracknum, int blocknum, int windownum){
  return getBlockHeaderY2(blocknum, windownum);
}

float getSwingtextX2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->swingtextarea.x2;
}

float getSwingtextY2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->y2;
}

/////

float getChancetextX1(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->chancetextarea.x;
}

float getChancetextY1(int tracknum, int blocknum, int windownum){
  return getBlockHeaderY2(blocknum, windownum);
}

float getChancetextX2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->chancetextarea.x2;
}

float getChancetextY2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->y2;
}

/////

float getVelocitytextX1(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->veltextarea.x;
}

float getVelocitytextY1(int tracknum, int blocknum, int windownum){
  return getBlockHeaderY2(blocknum, windownum);
}

float getVelocitytextX2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->veltextarea.x2;
}

float getVelocitytextY2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->y2;
}

/////

float getFxtextX1(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->fxtextarea.x;
}

float getFxtextY1(int tracknum, int blocknum, int windownum){
  return getBlockHeaderY2(blocknum, windownum);
}

float getFxtextX2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->fxtextarea.x2;
}

float getFxtextY2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->y2;
}

int getFxtextEffectNumFromX(float x, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return -1;

  if (fxtextVisible(tracknum, blocknum, windownum)==false)
    return -1;

  if (wtrack->track->fxs.num_elements==0)
    return -1;

  int column = 0;
  VECTOR_FOR_EACH(const struct FXs *fxs, &wtrack->track->fxs){
    float x2 = wtrack->fxtextarea.x + ((column+1) * WTRACK_fxtrack_width(window->fontwidth));
    if (x < x2)
      return fxs->fx->effect_num;
    column++;
  }END_VECTOR_FOR_EACH;

  struct FXs *fxs = VECTOR_last(&wtrack->track->fxs);
  return fxs->fx->effect_num;
}

///


int getLeftmostTrackNum(void){
  return LEFTMOSTTRACK;
}

int getRelTempoTrackNum(void){
  return TEMPONODETRACK;
}
int getTempoTrackNum(void){
  return TEMPOTRACK;
}
int getLPBTrackNum(void){
  return LPBTRACK;
}
int getSignatureTrackNum(void){
  return SIGNATURETRACK;
}
int getSwingTrackNum(void){
  return SWINGTRACK;
}
int getTempoColorTrackNum(void){
  return TEMPOCOLORTRACK;
}
int getLinenumTrackNum(void){
  return LINENUMBTRACK;
}


// temponodearea
//////////////////////////////////////////////////

int getTemponodeAreaX1(void){
  return root->song->tracker_windows->wblock->temponodearea.x;
}
int getTemponodeAreaY1(void){
  return root->song->tracker_windows->wblock->t.y1;
}
int getTemponodeAreaX2(void){
  return root->song->tracker_windows->wblock->temponodearea.x2;
}
int getTemponodeAreaY2(void){
  return root->song->tracker_windows->wblock->t.y2;
}

float getTemponodeMax(int blocknum, int windownum){
  struct WBlocks *wblock = getWBlockFromNum(windownum, blocknum);
  if (wblock==NULL)
    return 0.0f;
  else
    return wblock->reltempomax;
}

static const struct Node *get_temponode(int boxnum){
  const vector_t *nodes = GetTempoNodes(root->song->tracker_windows, root->song->tracker_windows->wblock);
  return VECTOR_get2(nodes,
                     boxnum,
                     "temponode"
                     );
}

float getTemponodeX(int num){
  const struct Node *nodeline = get_temponode(num);
  return nodeline==NULL ? 0 : nodeline->x;
}

float getTemponodeY(int num){
  const struct Node *nodeline = get_temponode(num);
  return nodeline==NULL ? 0 : nodeline->y-get_scroll_pos();
}

void setCurrentTemponode(int num, int blocknum){
  struct Blocks *block = blocknum==-1 ? root->song->tracker_windows->wblock->block : getBlockFromNum(blocknum);
  if (block==NULL) {
    handleError("setCurrentTemponode: No block %d",blocknum);
    return;
  }
  
  struct TempoNodes *temponode = ListFindElement3_num(&block->temponodes->l, num);

  setCurrentNode(&temponode->l);
}

void setIndicatorTemponode(int num, int blocknum){
  struct Blocks *block = blocknum==-1 ? root->song->tracker_windows->wblock->block : getBlockFromNum(blocknum);
  if (block==NULL) {
    handleError("setCurrentTemponode: No block %d",blocknum);
    return;
  }
  
  struct TempoNodes *temponode = ListFindElement3_num(&block->temponodes->l, num);

  setIndicatorNode(&temponode->l);
}



// pianoroll
//////////////////////////////////////////////////

int getPioanorollLowKey(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(-1, blocknum, tracknum);

  if (wtrack==NULL)
    return 0;

  return wtrack->pianoroll_lowkey;
}

int getPioanorollHighKey(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(-1, blocknum, tracknum);

  if (wtrack==NULL)
    return 127;

  return wtrack->pianoroll_highkey;
}


enum PianoNoteWhatToGet {
  PIANONOTE_INFO_X1,
  PIANONOTE_INFO_Y1,
  PIANONOTE_INFO_X2,
  PIANONOTE_INFO_Y2,
  PIANONOTE_INFO_VALUE
};

static float get_pianonote_info(enum PianoNoteWhatToGet what_to_get, int pianonotenum, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note==NULL)
    return 0;

  if (what_to_get==PIANONOTE_INFO_VALUE){
    if (pianonotenum==0)
      return note->note;
    
    //if (pianonotenum==getNumPianonotes(notenum,tracknum,blocknum,windownum))
    //  return note->end_pitch;
    
    struct Pitches *pitch = ListFindElement3_num_r0(&note->pitches->l, pianonotenum-1);
    if (pitch==NULL){
      handleError("There is no pianonote #%d in note %d in track #%d in block #%d",pianonotenum,(int)note->id,tracknum,blocknum);
      return 0;
    }
    return pitch->note;
  }
  
  const struct NodeLine *nodeline = GetPianorollNodeLines(window, wblock, wtrack, note);

  int num = -1;

  while(nodeline != NULL){
    if (nodeline->is_node)
      num++;

    if (num==pianonotenum)
      break;
    
    nodeline=nodeline->next;
  }

  if (nodeline==NULL) {
    handleError("There is no pianonote #%d in note %d in track #%d in block #%d",pianonotenum,(int)note->id,tracknum,blocknum);  
    return 0;
  }

  NodelineBox box = GetPianoNoteBox(wtrack, nodeline);
  
  switch (what_to_get){
  case PIANONOTE_INFO_X1:
    return box.x1;
  case PIANONOTE_INFO_Y1:
    return wblock->t.y1 + box.y1 - get_scroll_pos();
  case PIANONOTE_INFO_X2:
    return box.x2;
  case PIANONOTE_INFO_Y2:
    return wblock->t.y1 + box.y2 - get_scroll_pos();
  default:
    handleError("Internal error");
    return 0;
  }
}


float getPianonoteX1(int num, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  return get_pianonote_info(PIANONOTE_INFO_X1, num, dynnote, tracknum, blocknum, windownum);
}

float getPianonoteY1(int num, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  return get_pianonote_info(PIANONOTE_INFO_Y1, num, dynnote, tracknum, blocknum, windownum);
}

float getPianonoteX2(int num, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  return get_pianonote_info(PIANONOTE_INFO_X2, num, dynnote, tracknum, blocknum, windownum);
}

float getPianonoteY2(int num, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  return get_pianonote_info(PIANONOTE_INFO_Y2, num, dynnote, tracknum, blocknum, windownum);
}

float getPianonoteValue(int num, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  return get_pianonote_info(PIANONOTE_INFO_VALUE, num, dynnote, tracknum, blocknum, windownum);
}

int getNumPianonotes(dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Notes *note=getNoteFromNum(windownum,blocknum,tracknum,dynnote);
  if (note==NULL)
    return 0;

  return 1 + ListFindNumElements3(&note->pitches->l);
}

static void MOVE_PLACE(Place *place, float diff){
  if (diff < 0)
    *place = p_Sub(*place, p_FromFloat(-diff));
  else
    *place = p_Add(*place, p_FromFloat(diff));
}

static void setPianoNoteValues(float value, int pianonotenum, struct Notes *note){

  // 1. Find delta
  //
  float old_value;
  
  if (pianonotenum==0) {
    
    old_value = note->note;
    
  } else {
  
    struct Pitches *pitch = ListFindElement3_num_r0(&note->pitches->l, pianonotenum-1);
    if (pitch==NULL){
      handleError("There is no pianonote %d",pianonotenum);
      return;
    }

    old_value = pitch->note;
  }

  
  float delta = value - old_value;

  // 2. Apply
  //
  //note->note + note->note - value
    
  note->note = R_BOUNDARIES(1, note->note + delta, 127);

  if (note->pitch_end > 0)
    note->pitch_end = R_BOUNDARIES(1, note->pitch_end + delta, 127);

  struct Pitches *pitch = note->pitches;
  while(pitch != NULL){
    pitch->note = R_BOUNDARIES(1, pitch->note + delta, 127);
    pitch = NextPitch(pitch);
  }
  
}

static Place getPianoNotePlace(int pianonotenum, struct Notes *note){
  if (pianonotenum==0)
    return note->l.p;
  
  struct Pitches *pitch = ListFindElement3_num_r0(&note->pitches->l, pianonotenum-1);
  if (pitch==NULL){
    handleError("There is no pianonote %d",pianonotenum);
    return note->l.p;
  }

  return pitch->l.p;
}


static int getPitchNumFromPianonoteNum(int pianonotenum, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0;

  struct Tracks *track = wtrack->track;

  int ret = 0;

  struct Notes *note = getNoteFromNum(windownum, blocknum, tracknum, dynnote);

  struct Notes *note2 = track->notes;

  while(note2!=NULL){

    if (note2==note) {
      
      if (pianonotenum > 0) {
        struct Pitches *pitch = ListFindElement3_num_r0(&note->pitches->l, pianonotenum-1);
        if (pitch==NULL){
          handleError("There is no pianonote #%d in note %d in track #%d in block #%d",pianonotenum,(int)note->id,tracknum,blocknum);
          return 0;
        }
      }
      
      return ret + pianonotenum;
    }
      
    ret++;

    ret += ListFindNumElements3(&note2->pitches->l);

    if (note2->pitch_end > 0)
      ret++;

    note2 = NextNote(note2);
  }

  handleError("There is no pianonote %d in note %d in track %d in block %d",pianonotenum,(int)note->id,tracknum,blocknum);
  return 0;
}

static dyn_t moveNote(struct Blocks *block, struct Tracks *track, struct Notes *note, float diff){
  float old_start = GetfloatFromPlace(&note->l.p);

  if (old_start + diff < 0)
    diff = -old_start;

  //printf("new_start 1: %f\n",old_start+diff);

  float old_end   = GetfloatFromPlace(&note->end);

  Place lastplace;
  PlaceSetLastPos(block, &lastplace);
  float lastplacefloat = GetfloatFromPlace(&lastplace);

  if (old_end + diff > lastplacefloat)
    diff = lastplacefloat - old_end;

  //printf("new_start 2: %f\n",old_start+diff);

  {
    SCOPED_PLAYER_LOCK_IF_PLAYING();
    
    ListRemoveElement3(&track->notes, &note->l);
    
    //Float2Placement(new_start, &note->l.p);
    //Float2Placement(new_end, &note->end);
    MOVE_PLACE(&note->l.p, diff);
    MOVE_PLACE(&note->end, diff);
    
    struct Velocities *velocity = note->velocities;
    while(velocity != NULL){
      MOVE_PLACE(&velocity->l.p, diff);
      velocity = NextVelocity(velocity);
    }

    struct Pitches *pitch = note->pitches;
    while(pitch != NULL){
      MOVE_PLACE(&pitch->l.p, diff);
      pitch = NextPitch(pitch);
    }
    
    ListAddElement3_a(&track->notes, &note->l);

    NOTE_validate(block, track, note);
  }

  return GetNoteId(note);
}

dyn_t movePianonote(int pianonotenum, float value, Place place, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note==NULL)
    return dynnote;

  struct Blocks *block = wblock->block;
  
  setPianoNoteValues(value, pianonotenum, note);

  window->must_redraw_editor = true;

  if (place.line < 0)
    return dynnote;
  
  float floatplace = GetfloatFromPlace(&place);
  
  struct Tracks *track = wtrack->track;

  Place old_place = getPianoNotePlace(pianonotenum, note);
  float old_floatplace = GetfloatFromPlace(&old_place);
  float diff      = floatplace - old_floatplace;

  return moveNote(block, track, note, diff);
}

static int setPitchnum2(int num, float value, Place place, int tracknum, int blocknum, int windownum, bool replace_note_ends);
  
dyn_t movePianonoteStart(int pianonotenum, float value, Place place, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note==NULL)
    return dynnote;

  struct Blocks *block = wblock->block;
  struct Tracks *track = wtrack->track;

  if (note->pitches!=NULL) {
    setPitchnum2(getPitchNumFromPianonoteNum(pianonotenum, dynnote, tracknum, blocknum, windownum),
                 value, place,
                 tracknum, blocknum, windownum,
                 false
                 );
    return GetNoteId(note);
  }

  note->note = R_BOUNDARIES(1, value, 127);
    
  window->must_redraw_editor = true;
    
  if (place.line < 0)
    return dynnote;

  float floatplace = GetfloatFromPlace(&place);
  
  const float mindiff = 0.001;
    
  float lastplacefloat = GetfloatFromPlace(&note->end);
  if (floatplace+mindiff >= lastplacefloat)
    floatplace = lastplacefloat - mindiff;

  if (note->velocities != NULL) {
    float firstvelplace = GetfloatFromPlace(&note->velocities->l.p);
    if (floatplace+mindiff >= firstvelplace)
      floatplace = firstvelplace - mindiff;
  }

  // (there are no pitches here)
    
  {
    SCOPED_PLAYER_LOCK_IF_PLAYING();
    
    ListRemoveElement3(&track->notes, &note->l);
    
    Float2Placement(floatplace, &note->l.p);
    
    ListAddElement3_a(&track->notes, &note->l);

    NOTE_validate(block, track, note);
  }


  return GetNoteId(note);
}

static int getPitchnumLogtype_internal(int pitchnum, struct Tracks *track);
static void setPitchnumLogtype2(int logtype, int pitchnum, struct Tracks *track);
  
int getPianonoteLogtype(int pianonotenum, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note==NULL)
    return LOGTYPE_LINEAR;

  struct Tracks *track = wtrack->track;

  int pitchnum = getPitchNumFromPianonoteNum(pianonotenum, dynnote, tracknum, blocknum, windownum);
  return getPitchnumLogtype_internal(pitchnum, track);
}

void setPianonoteLogtype(int logtype, int pianonotenum, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note==NULL)
    return;

  struct Tracks *track = wtrack->track;

  window->must_redraw_editor=true;
  
  int pitchnum = getPitchNumFromPianonoteNum(pianonotenum, dynnote, tracknum, blocknum, windownum);
  setPitchnumLogtype2(logtype, pitchnum, track);
}
  
dyn_t movePianonoteEnd(int pianonotenum, float value, Place place, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note==NULL)
    return dynnote;

  struct Blocks *block = wblock->block;
  struct Tracks *track = wtrack->track;

  if (note->pitches != NULL) {
    
    int pitchnum = getPitchNumFromPianonoteNum(pianonotenum, dynnote, tracknum, blocknum, windownum);
    int logtype  = getPitchnumLogtype_internal(pitchnum, track);    
             
    // 1. Change pitch value
    setPitchnum2(logtype==LOGTYPE_HOLD ? pitchnum : pitchnum + 1,
                 value, p_Create(-1, 0, 1),
                 tracknum, blocknum, windownum,
                 false
                 );

    // 2. Change place of the next pianonote
    setPitchnum2(pitchnum+1,
                 -1, place,
                 tracknum, blocknum, windownum,
                 false
                 );
    
  } else {
  
  
    window->must_redraw_editor=true;
    
    if(note->pitch_end > 0 || note->pitches!=NULL)
      note->pitch_end = R_BOUNDARIES(1, value, 127);
    else
      note->note = R_BOUNDARIES(1, value, 127);
  
    if (place.line < 0)
      return dynnote;

    float floatplace = GetfloatFromPlace(&place);
      
    const float mindiff = 0.001;
  
    float firstplacefloat = GetfloatFromPlace(&note->l.p);
    if (floatplace-mindiff <= firstplacefloat)
      floatplace = firstplacefloat + mindiff;

    if (note->velocities != NULL) {
      float lastvelplace = GetfloatFromPlace(ListLastPlace3(&note->velocities->l));
      if (floatplace-mindiff <= lastvelplace)
        floatplace = lastvelplace + mindiff;
    }

    // (there are no pitches here)
    
    {
      SCOPED_PLAYER_LOCK_IF_PLAYING();
      
      Float2Placement(floatplace, &note->end);
      NOTE_validate(block, track, note);
    }

  }

  return dynnote;
}

dyn_t addPianonote(float value, Place place, Place endplace, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return DYN_create_int(-1);

  struct Blocks *block = wblock->block;
  struct Tracks *track = wtrack->track;

  value = R_BOUNDARIES(1,value,127);

  Place lastplace;
  PlaceSetLastPos(block, &lastplace);

  if (p_Less_Than(place, p_Create(0,0,1)))
    place = p_Create(0,0,1);

  if (p_Greater_Or_Equal(endplace, lastplace))
    endplace = lastplace;
  
  if (p_Greater_Or_Equal(place, endplace)){
    //handleError("Illegal parameters for addPianonote. start: %f, end: %f",floatplace, endfloatplace);
    return DYN_create_int(-1);
  }

  ADD_UNDO(Notes(window,block,track,window->wblock->curr_realline));

  printf("  Place: %d %d/%d\n",place.line,place.counter,place.dividor);
  struct Notes *note = InsertNote(wblock, wtrack, &place, &endplace, value, NOTE_get_velocity(track), true);
  
  window->must_redraw_editor = true;

  return GetNoteId(note);
}

void deletePianonote(int pianonotenum, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note==NULL)
    return;

  if (pianonotenum==0) {
    window->must_redraw_editor=true;

    {
      SCOPED_PLAYER_LOCK_IF_PLAYING();
      ListRemoveElement3(&wtrack->track->notes, &note->l);
    }
    
    return;
  }

  int pitchnum = getPitchNumFromPianonoteNum(pianonotenum, dynnote, tracknum,  blocknum, windownum);

  deletePitchnum(pitchnum, tracknum, blocknum);      
}


  
void setCurrentPianonote(int num, dyn_t dynnote, int tracknum){
  struct Notes *note=getNoteFromNum(-1, -1, tracknum, dynnote);
  if (note==NULL)
    return;

  if (
      current_piano_note.tracknum != tracknum ||
      current_piano_note.noteid != note->id || 
      current_piano_note.pianonotenum != num
      )
    {  
      current_piano_note.tracknum = tracknum;
      current_piano_note.noteid = note->id;
      current_piano_note.pianonotenum = num;
      root->song->tracker_windows->must_redraw_editor = true;
    }
}

void cancelCurrentPianonote(void){
  current_piano_note.tracknum = -1;
  current_piano_note.noteid = -1;
  current_piano_note.pianonotenum = -1;
  root->song->tracker_windows->must_redraw_editor = true;
}

static int addPitch2(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, struct Notes *note, Place *place, float value);
  
void addPianonotePitch(float value, Place place, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note==NULL)
    return;


  if (note->pitch_end == 0) {
    window->must_redraw_editor = true;
    note->pitch_end = note->note;
  }

  addPitch2(window, wblock, wtrack, note, &place, value);
}

void showPianorollEraser(float pitch1, float pitch2, Place place1, Place place2, int tracknum, int blocknum, int windownum){
  g_current_pianobar_rubber.pitch1 = pitch1;
  g_current_pianobar_rubber.pitch2 = pitch2;
  g_current_pianobar_rubber.place1 = place1;
  g_current_pianobar_rubber.place2 = place2;
  g_current_pianobar_rubber.tracknum = tracknum;
  g_current_pianobar_rubber.blocknum = blocknum;
  root->song->tracker_windows->must_redraw_editor = true;
}

void hidePianorollEraser(int windownum){
  g_current_pianobar_rubber.blocknum = -2;
  root->song->tracker_windows->must_redraw_editor = true;
}

// pitchnums
//////////////////////////////////////////////////

static int getPitchNum(struct Tracks *track, struct Notes *note, struct Pitches *pitch, bool is_end_pitch){
  int num = 0;
  struct Notes *note2 = track->notes;

  while(note2!=NULL){

    if (note==note2 && pitch==NULL && is_end_pitch==false)
      return num;

    num++;
    
    struct Pitches *pitch2 = note2->pitches;
    while(pitch2!=NULL){
      if (note2==note && pitch==pitch2)
        return num;

      num++;

      pitch2 = NextPitch(pitch2);
    }

    if (note2->pitch_end > 0){
      if (note==note2 && pitch==NULL && is_end_pitch==true)
        return num;
      
      num++;
    }

    if (note==note2) {
      handleError("getPitchNum: Could not find pitch in note.");
      return 0;
    }

    note2 = NextNote(note2);
  }

  handleError("getPitchNum: Could not find it");
  return 0;
}

int getNumPitchnums(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(-1, blocknum, tracknum);

  if (wtrack==NULL)
    return 0;

  struct Tracks *track = wtrack->track;
  
  int num = 0;
  struct Notes *notes = track->notes;
  
  while(notes!=NULL){

    num++;
    
    struct Pitches *pitches = notes->pitches;
    while(pitches!=NULL){
      num++;
      pitches = NextPitch(pitches);
    }

    if (notes->pitch_end > 0)
      num++;
    
    notes = NextNote(notes);
  }

  return num;
}

void deletePitchnum(int pitchnum, int tracknum, int blocknum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(-1, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;

  struct Blocks *block = wblock->block;
  struct Tracks *track = wtrack->track;
  
  int num = 0;
  struct Notes *notes = track->notes;
  
  while(notes!=NULL){

    if (pitchnum==num) {
      {
        SCOPED_PLAYER_LOCK_IF_PLAYING();
        RemoveNote(block, track, notes);
      }
      goto gotit;
    }
    
    num++;
    
    struct Pitches *pitches = notes->pitches;
    while(pitches!=NULL){
      if (pitchnum==num){
        {
          SCOPED_PLAYER_LOCK_IF_PLAYING();
          
          ListRemoveElement3(&notes->pitches,&pitches->l);
          NOTE_validate(block, track, notes);
        }
        goto gotit;
      }
      
      num++;
      pitches = NextPitch(pitches);
    }

    if (notes->pitch_end > 0) {
      if (pitchnum==num){
        struct Pitches *pitch = ListLast3(&notes->pitches->l);
        if (pitch!=NULL)
          notes->pitch_end = pitch->note;
        else
          notes->pitch_end = 0;
        goto gotit;
      }
      num++;
    }
    
    notes = NextNote(notes);
  }

  handleError("no pitch %d in track %d in block %d\n",pitchnum,tracknum,blocknum);
  return;
  
 gotit:
  window->must_redraw_editor = true;
}



static bool getPitch(int pitchnum, struct Pitches **pitch, struct Notes **note, bool *is_end_pitch, struct Tracks *track){
  int num = 0;
  struct Notes *notes = track->notes;
  
  *is_end_pitch = false;
  
  while(notes!=NULL){

    if(num==pitchnum) {
      *note = notes;
      *pitch = NULL;
      return true;
    }

    num++;
    
    struct Pitches *pitches = notes->pitches;
    while(pitches!=NULL){
      if(num==pitchnum) {
        *note = notes;
        *pitch = pitches;
        return true;
      }

      num++;
      pitches = NextPitch(pitches);
    }

    if (notes->pitch_end > 0) {
      if(num==pitchnum) {
        *note = notes;
        *is_end_pitch = true;
        *pitch = NULL;
        return true;
      }
      num++;
    }
    
    notes = NextNote(notes);
  }

  handleError("Pitch #%d in track #%d does not exist",pitchnum,track->l.num);
  return false;
}

/*
Place getPitchStart(int pitchnum,  dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note==NULL)
    return place(0,0,1);

  bool is_end_pitch = false;
  struct Notes *note = NULL;
  struct Pitches *pitch = NULL;
  getPitch(pitchnum, &pitch, &note, &is_end_pitch, wtrack->track);

  if (is_end_pitch)
    return note->end;
  if (pitch==NULL)
    return note->l.p;
  else
    return pitch->l.p;
}
*/

static int getPitchnumLogtype_internal(int pitchnum, struct Tracks *track){
  bool is_end_pitch = false;
  struct Notes *note = NULL;
  struct Pitches *pitch = NULL;
  if (getPitch(pitchnum, &pitch, &note, &is_end_pitch, track)==false)
    return 0;

  if (is_end_pitch)
    return LOGTYPE_IRRELEVANT;
  if (pitch==NULL)
    return note->pitch_first_logtype;
  else
    return pitch->logtype;
}

/*
Place getPitchnumLogtype(int num,  int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note==NULL)
    return place(0,0,1);

  return getPitchnumLogtype_internal(num, wtrack->track);
}
*/

static void setPitchnumLogtype2(int logtype, int pitchnum, struct Tracks *track){
  bool is_end_pitch = false;
  struct Notes *note = NULL;
  struct Pitches *pitch = NULL;

  if (getPitch(pitchnum, &pitch, &note, &is_end_pitch, track)==false)
    return;

  if (is_end_pitch) {
    handleError("Can not set logtype of end pitch. pitchnum: %d, tracknum: %d",pitchnum,track->l.num);
    return;
  }
      
  if (pitch==NULL)
    note->pitch_first_logtype = logtype;
  else
    pitch->logtype = logtype;
}


void setPitchnumLogtype(int logtype, int pitchnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);

  if (wtrack==NULL)
    return;

  setPitchnumLogtype2(logtype, pitchnum, wtrack->track);
}



static int getReallineForPitch(const struct WBlocks *wblock, struct Pitches *pitch, struct Notes *note, bool is_end_pitch){
  if( pitch!=NULL)
    return FindRealLineFor(wblock,pitch->Tline,&pitch->l.p);
  else if (is_end_pitch)
    return find_realline_for_end_pitch(wblock, &note->end);
  else
    return FindRealLineFor(wblock,note->Tline,&note->l.p);
}

enum PitchInfoWhatToGet {
  PITCH_INFO_Y1,
  PITCH_INFO_Y2,
  PITCH_INFO_VALUE,
};

static float getPitchInfo(enum PitchInfoWhatToGet what_to_get, int pitchnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);

  if (wtrack==NULL)
    return 0;

  struct Notes *note;
  struct Pitches *pitch;
  bool is_end_pitch;
  
  if (getPitch(pitchnum, &pitch, &note, &is_end_pitch, wtrack->track)==false)
    return 0;
  
  switch (what_to_get){
  case PITCH_INFO_Y1:
    return get_mouse_realline_y1(window, getReallineForPitch(wblock, pitch, note, is_end_pitch));
  case PITCH_INFO_Y2:
    return get_mouse_realline_y2(window, getReallineForPitch(wblock, pitch, note, is_end_pitch));
  case PITCH_INFO_VALUE:
    {
      if (pitch!=NULL)
        return pitch->note;
      else if (is_end_pitch)
        return note->pitch_end;
      else
        return note->note;
    }
  }

  handleError("internal error (getPitchInfo)\n");
  return 0;
}

float getPitchnumY1(int pitchnum, int tracknum, int blocknum, int windownum){
  return getPitchInfo(PITCH_INFO_Y1, pitchnum, tracknum, blocknum, windownum);
}

float getPitchnumY2(int pitchnum, int tracknum, int blocknum, int windownum){
  return getPitchInfo(PITCH_INFO_Y2, pitchnum, tracknum, blocknum, windownum);
}

float getPitchnumX1(int pitchnum, int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0.0f : wtrack->notearea.x;
}

float getPitchnumX2(int pitchnum, int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0.0f : wtrack->notearea.x2;
}

static struct Node *get_pitchnodeline(int pitchnum, int tracknum, int blocknum, int windownum, bool *is_end_pitch){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return NULL;

  struct Notes *note;
  struct Pitches *pitch;  
  
  if (getPitch(pitchnum, &pitch, &note, is_end_pitch, wtrack->track)==false)
    return NULL;

  int note_pitchnum;

  if (pitch==NULL) {
    if (*is_end_pitch) {
      note_pitchnum = note->pitches==NULL ? 1 : ListFindNumElements3(&note->pitches->l) + 1;
    } else {
      note_pitchnum = 0;
    }
  } else
    note_pitchnum = ListPosition3(&note->pitches->l, &pitch->l) + 1;

  const vector_t *nodes = GetPitchNodes(window, wblock, wtrack, note);

  return nodes->elements[note_pitchnum];
}


float getPitchnumX(int num,  int tracknum, int blocknum, int windownum){
  bool is_end_pitch;
  struct Node *nodeline = get_pitchnodeline(num, tracknum, blocknum, windownum, &is_end_pitch);
  if (nodeline==NULL)
    return 0;

  return nodeline->x;
}

float getPitchnumY(int num, int tracknum, int blocknum, int windownum){
  bool is_end_pitch;
  struct Node *nodeline = get_pitchnodeline(num, tracknum, blocknum, windownum, &is_end_pitch);
  if (nodeline==NULL)
    return 0;

  if (is_end_pitch)
    return nodeline->y-get_scroll_pos();
  else
    return nodeline->y-get_scroll_pos();
}


float getPitchnumValue(int pitchnum, int tracknum, int blocknum, int windownum){
  return getPitchInfo(PITCH_INFO_VALUE, pitchnum, tracknum, blocknum, windownum);
}

void setCurrentPitchnum(int num, int tracknum, int blocknum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(-1, &window, blocknum, &wblock, tracknum);
  if(wtrack==NULL)
    return;

  struct Notes *note;
  struct Pitches *pitch;
  bool is_end_pitch;
  if (getPitch(num, &pitch, &note, &is_end_pitch, wtrack->track)==false)
    return;

  struct ListHeader3 *listHeader3 = pitch!=NULL ? &pitch->l : &note->l;
  setCurrentNode(listHeader3);
}

void setIndicatorPitchnum(int num, int tracknum, int blocknum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(-1, &window, blocknum, &wblock, tracknum);
  if(wtrack==NULL)
    return;

  struct Notes *note;
  struct Pitches *pitch;
  bool is_end_pitch;
  
  if (getPitch(num, &pitch, &note, &is_end_pitch, wtrack->track)==false)
    return;

  setIndicatorNode(&note->l);

  if (pitch==NULL) {
    if (is_end_pitch)
      indicator_pitch_num = 1 + ListFindNumElements3(&note->pitches->l);
    else
      indicator_pitch_num = 0;
  } else {
    int pitchnum = ListPosition3(&note->pitches->l, &pitch->l);
    indicator_pitch_num = pitchnum + 1;
  }
}

static int setPitchnum2(int num, float value, Place place, int tracknum, int blocknum, int windownum, bool replace_note_ends){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);

  if (wtrack==NULL)
    return num;

  struct Blocks *block = wblock->block;  
  struct Tracks *track = wtrack->track;

  float clamped_value = R_BOUNDARIES(1,value,127);

  struct Notes *note;
  struct Pitches *pitch;
  bool is_end_pitch;
  
  if (getPitch(num, &pitch, &note, &is_end_pitch, track)==false)
    return num;

  window->must_redraw_editor = true;

  if (pitch != NULL) {

    if (value > 0)
      pitch->note = clamped_value;

    if (place.line >= 0) {
      Place firstLegalPlace,lastLegalPlace;
      PlaceFromLimit(&firstLegalPlace, &note->l.p);
      PlaceTilLimit(&lastLegalPlace, &note->end);

      {
        SCOPED_PLAYER_LOCK_IF_PLAYING();
        
        ListMoveElement3_ns(&note->pitches, &pitch->l, &place, &firstLegalPlace, &lastLegalPlace);
        NOTE_validate(block, track, note);
      }
    }
                        
  } else if (is_end_pitch){

    if (value > 0)
      note->pitch_end = clamped_value;
    
    if (place.line >= 0) {
      MoveEndNote(block, track, note, &place, true);
      return getPitchNum(track, note, NULL, true);
    }
    
  } else {

    if (value > 0)
      note->note = clamped_value;

    if (place.line >= 0) {
      MoveNote(block, track, note, &place, replace_note_ends);
      return getPitchNum(track, note, NULL, false);
    }
  }

  return num;
}

int setPitchnum(int num, float value, Place place, int tracknum, int blocknum, int windownum){
  return setPitchnum2(num, value, place, tracknum, blocknum, windownum, true);
}
  
static struct Notes *getNoteAtPlace(struct Tracks *track, Place *place){
  struct Notes *note = track->notes;

  while(note != NULL){
    if (PlaceIsBetween3(place, &note->l.p, &note->end))
      return note;
    else
      note = NextNote(note);
  }

  return NULL;
}

static int addNote4(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, Place *place, float value){

  struct Notes *note = InsertNote(wblock, wtrack, place, NULL, value, NOTE_get_velocity(wtrack->track), false);

  window->must_redraw_editor = true;

  return getPitchNum(wtrack->track, note, NULL, false);
}

static int addPitch2(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, struct Notes *note, Place *place, float value){

  struct Pitches *pitch = AddPitch(window, wblock, wtrack, note, place, value);

  if(pitch==NULL)
    return -1;

  //if (note->pitch_end==0)
  //  note->pitch_end = value;
  
  window->must_redraw_editor = true;

  return getPitchNum(wtrack->track, note, pitch, false);
}

int addPitchnum(float value, Place place, int tracknum, int blocknum, int windownum){

  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);

  if (wtrack==NULL)
    return -1;

  struct Notes *note = getNoteAtPlace(wtrack->track, &place);

  value = R_BOUNDARIES(1,value,127);

  ADD_UNDO(Notes(window,wblock->block,wtrack->track,window->wblock->curr_realline));

  int ret;
  
  if(note==NULL)
    ret = addNote4(window, wblock, wtrack, &place, value);
  else
    ret = addPitch2(window, wblock, wtrack, note, &place, value);

  if (ret==-1)
    UNDO_CANCEL_LAST_UNDO();

  //printf("\n\n\n\n ***** NUM: %d\n",ret);
  return ret;
}

int addPitchnumF(float value, float floatplace, int tracknum, int blocknum, int windownum){
  if (floatplace < 0){
    handleError("Place can not be negative: %f", floatplace);
    return -1;
  }

  Place place;
  Float2Placement(floatplace, &place);
  return addPitchnum(value, place, tracknum, blocknum, windownum);
}

bool portamentoEnabled(dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note==NULL)
    return false;

  //if (note->pitch_first_logtype==LOGTYPE_HOLD && note->pitches==NULL && note->pitch_end==note->note)
  //  return false;

  return note->pitch_end > 0;
}

void setNoteEndPitch(float value, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note==NULL)
    return;

  note->pitch_end = value;
}

void enablePortamento(dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note==NULL)
    return;

  if (note->pitch_end == 0) {
    window->must_redraw_editor = true;
    note->pitch_end = note->note;
  }
}

void disablePortamento(dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note==NULL)
    return;

  {
    SCOPED_PLAYER_LOCK_IF_PLAYING();
    note->pitches = NULL;
    note->pitch_end = 0;
  }
  
  window->must_redraw_editor = true;
}

// subtracks
///////////////////////////////////////////////////
int getNumSubtracks(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 1;

  return GetNumSubtracks(wtrack);
}

static struct WTracks *getSubtrackWTrack(int subtracknum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return NULL;

  if (subtracknum>=GetNumSubtracks(wtrack)){
    handleError("No subtrack %d in track %d in block %d (only %d subtracks in this track)\n", subtracknum, tracknum, blocknum, GetNumSubtracks(wtrack));
    return 0;
  }

  return wtrack;
}

float getSubtrackX1(int subtracknum, int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getSubtrackWTrack(subtracknum, tracknum, blocknum, windownum);
  if (wtrack==NULL)
    return 0.0f;
  else
    return GetXSubTrack1(wtrack,subtracknum);
}

float getSubtrackX2(int subtracknum, int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getSubtrackWTrack(subtracknum, tracknum, blocknum, windownum);
  if (wtrack==NULL)
    return 0.0f;
  else
    return GetXSubTrack2(wtrack,subtracknum);
}



// fxs
//////////////////////////////////////////////////

void requestFX(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window=NULL;
  struct WTracks *wtrack;
  struct WBlocks *wblock;

  wtrack=getWTrackFromNumA(
                           windownum,
                           &window,
                           blocknum,
                           &wblock,
                           tracknum
                           );

  if(wtrack==NULL) return;

  //printf("x: %f, y: %f\n",tevent.x, tevent.y);
  
  AddFXNodeLineCurrPos(window, wblock, wtrack);
}

/*
void requestFXMousePos(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL)
    return;

  AddFXNodeLineCurrMousePos(window);
}
*/

static int get_effect_num(struct Patch *patch, const char *fx_name){
  int effect_num = 0;
  VECTOR_FOR_EACH(const char *name,patch->instrument->getFxNames(patch)){
    if (!strcmp(name, fx_name))
      return effect_num;
    else
      effect_num++;
  }END_VECTOR_FOR_EACH;

  return -1;
}

int getFx(const char* fx_name, int tracknum, int64_t instrument_id, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return -1;

  struct Tracks *track = wtrack->track;

  struct Patch *patch = track->patch;

  if(patch==NULL){
    handleError("Track %d has no assigned instrument",tracknum);
    return -1;
  }

  if (instrument_id >= 0){
    if (track->patch->instrument != get_audio_instrument()){
      RError("track->patch->instrument != get_audio_instrument()");
    } else {
      patch = getPatchFromNum(instrument_id);
    }
  }
  
  if (patch==NULL)
    return -1;

  int effect_num = get_effect_num(patch, fx_name);
  if (effect_num==-1){
    handleError("No effect \"%s\" for the instrument %s.", fx_name, patch->name);
    return -1;
  }

  int num = 0;

  VECTOR_FOR_EACH(struct FXs *fxs, &wtrack->track->fxs){
    if (fxs->fx->effect_num == effect_num && fxs->fx->patch==patch)
      return num;    
    num++;
  }END_VECTOR_FOR_EACH;

  return -2;
}

int addFx(float value, Place place, const char* fx_name, int tracknum, int64_t instrument_id, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return -1;

  if(value < 0 || value > 1){
    GFX_addMessage("addFx: Value must be between 0 and 1. Found %f", value);
    value = R_BOUNDARIES(0, value, 1);
  }

  
  struct Tracks *track = wtrack->track;

  printf("\n\n    createFX track %d: %s. num_fx: %d\n\n", track->l.num, fx_name, track->fxs.num_elements);
  
  struct Patch *patch = track->patch;

  if(patch==NULL){
    handleError("Track %d has no assigned instrument",tracknum);
    return -1;
  }

  if (instrument_id >= 0){
    if (track->patch->instrument != get_audio_instrument()){
      handleError("An audio instrument is not assigned to track %d", track->l.num);
      return -1;
    } else {
      patch = getPatchFromNum(instrument_id);
    }
  }
  
  if (patch==NULL)
    return -1;

  int effect_num = get_effect_num(patch, fx_name);
  if (effect_num==-1){
    handleError("No effect \"%s\" for the instrument %s.", fx_name, patch->name);
    return -1;
  }

  VECTOR_FOR_EACH(struct FXs *fxs, &wtrack->track->fxs){
    if (fxs->fx->effect_num == effect_num && fxs->fx->patch==patch){
      GFX_Message2(NULL, true, "addFX: Effect \"%s\" has already been added to track %d", fx_name, track->l.num);
      return -1;
    }
  }END_VECTOR_FOR_EACH;

  
  {
    struct FX *fx = patch->instrument->createFX(track, patch, effect_num);

    if (fx==NULL){
      printf("   FX returned NULL\n");
      return -1;
    }

    printf("  1. p.line: %d, p.c: %d, p.d: %d\n",place.line,place.counter,place.dividor);

    AddFXNodeLineCustomFxAndPos(window, wblock, wtrack, fx, &place, value);

    printf("  2. p.line: %d, p.c: %d, p.d: %d\n",place.line,place.counter,place.dividor);
        
    int num = 0;

    VECTOR_FOR_EACH(struct FXs *fxs, &wtrack->track->fxs){
      if (fxs->fx == fx)
        return num;

      num++;
    }END_VECTOR_FOR_EACH;

#if defined(RELEASE)
    RError("Internal error: Newly created FX not found, even though it was just created");
#else
    abort();
#endif
    
    return -1;
  }
}

int addFxF(float value, float floatplace, const char* fx_name, int tracknum, int64_t instrument_id, int blocknum, int windownum){
  if (floatplace < 0){
    handleError("Place can not be negative: %f", floatplace);
    return -1;
  }

  Place place;
  Float2Placement(floatplace, &place);

  return addFx(value, place, fx_name, tracknum, instrument_id, blocknum, windownum);
}

static struct Node *get_fxnode(int fxnodenum, int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return NULL;
  
  const vector_t *nodes = GetFxNodes(window, wblock, wtrack, fxs);
  return VECTOR_get2(nodes, fxnodenum, "fx node");
}


float getFxnodeX(int num, int fxnum, int tracknum, int blocknum, int windownum){
  struct Node *node = get_fxnode(num, fxnum, tracknum, blocknum, windownum);
  return node==NULL ? 0 : node->x;
}

float getFxnodeY(int num, int fxnum, int tracknum, int blocknum, int windownum){
  struct Node *node = get_fxnode(num, fxnum, tracknum, blocknum, windownum);
  return node==NULL ? 0 : node->y-get_scroll_pos();
}

Place getFxnodePlace(int fxnodenum, int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return place(0,0,1);
  
  const vector_t *nodes = GetFxNodes(window, wblock, wtrack, fxs);
  struct Node *node = VECTOR_get2(nodes, fxnodenum, "fx node");
  if (node==NULL)
    return place(0,0,1);

  struct FXNodeLines *fxnodeline = (struct FXNodeLines*)node->element;

  return fxnodeline->l.p;
}

float getFxnodeValue(int fxnodenum, int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return 0.0f;

  const vector_t *nodes = GetFxNodes(window, wblock, wtrack, fxs);
  struct Node *node = VECTOR_get2(nodes, fxnodenum, "fx node");
  if (node==NULL)
    return 0.0f;

  int max = fxs->fx->max;
  int min = fxs->fx->min;

  struct FXNodeLines *fxnodeline = (struct FXNodeLines*)node->element;

  return scale(fxnodeline->val, min, max, 0.0f, 1.0f);
}

int getFxnodeLogtype(int fxnodenum, int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return 0.0f;

  const vector_t *nodes = GetFxNodes(window, wblock, wtrack, fxs);
  struct Node *node = VECTOR_get2(nodes, fxnodenum, "fx node");
  if (node==NULL)
    return 0.0f;

  struct FXNodeLines *fxnodeline = (struct FXNodeLines*)node->element;

  return fxnodeline->logtype;
}

const char* getFxName(int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return NULL;

  return fxs->fx->name;
}
  
int64_t getFxInstrument(int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return -1;

  if (wtrack->track->patch==NULL){
    R_ASSERT("wtrack->track->patch==NULL");
    return -1;
  }

  //if (wtrack->track->patch == fxs->fx->patch)
  //  return -1;
  
  return fxs->fx->patch->id;
}
  
const_char* getFxString(int fxnodenum, int fxnum, int tracknum, int blocknum, int windownum){   
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return NULL;

  const vector_t *nodes = GetFxNodes(window, wblock, wtrack, fxs);
  struct Node *node = VECTOR_get2(nodes, fxnodenum, "fx node");
  if (node==NULL)
    return "<fxnode not found>";

  struct FXNodeLines *fxnodeline = (struct FXNodeLines *)node->element;

  float val = fxnodeline->val;

  // Turned out this was a lot of work. Fix later, hopefully. No, it's hopeless. For VST, it can't be done without doing some type of hacking we don't want to do, and for the other types of sound plugins, it's extremely hairy.
  //return fx->getFXstring(fx, wtrack->track, val);

  // instead we just do this:
  struct FX *fx = fxs->fx;
  static char ret[512];

  if (wtrack->track->patch->instrument==get_MIDI_instrument())
    snprintf(ret, 511, "%s: %d", fx->name, (int)val);
  else if (fx->patch==wtrack->track->patch)
    snprintf(ret, 511, "%s: %.01f%%", fx->name, scale(val, fx->min, fx->max, 0, 100));
  else
    snprintf(ret, 511, "%s (%s): %.01f%%", fx->name, fx->patch->name, scale(val, fx->min, fx->max, 0, 100));
  
  return ret;
}

int getNumFxs(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  if (wtrack == NULL)
    return 0;

  return wtrack->track->fxs.num_elements;
}

int getNumFxnodes(int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return 0;

  return ListFindNumElements3(&fxs->fxnodelines->l);
}

float getFxMinValue(int fxnum, int tracknum, int blocknum, int windownum){
  return 0.0f;
#if 0
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fx = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fx==NULL)
    return 0;

  return fx->fx->min;
#endif
}

float getFxMaxValue(int fxnum, int tracknum, int blocknum, int windownum){
  return 1.0f;
#if 0
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fx = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fx==NULL)
    return 1;

  return fx->fx->max;
#endif
}

int addFxnode(float value, Place place, int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return -1;

  R_ASSERT(value >= 0.0f);
  R_ASSERT(value <= 1.0f);

  Place lastplace;
  PlaceSetLastPos(wblock->block, &lastplace);

  if (PlaceLessThan(&place, PlaceGetFirstPos())){
    handleError("addFxnode: placement before top of block for fx #%d. (%s)", fxnum, PlaceToString(&place));
    place = *PlaceGetFirstPos();
  }

  if (PlaceGreaterThan(&place, &lastplace)) {    
    handleError("addFxnode: placement after fx end for fx #%d (%s). num_lines: #%d", fxnum, PlaceToString(&place), wblock->block->num_lines);
    place = lastplace;
  }

  ADD_UNDO(FXs(window, wblock->block, wtrack->track, wblock->curr_realline));

  int max = fxs->fx->max;
  int min = fxs->fx->min;

  int ret = AddFXNodeLine(
                          window,
                          wblock,
                          wtrack,
                          fxs,
                          scale(value, 0,1, min, max),
                          &place
                          );

  if (ret==-1){
    //handleError("addFxnode: Can not create new fx with the same position as another fx");
    UNDO_CANCEL_LAST_UNDO();
    return -1;
  }

  window->must_redraw_editor = true;

  return ret;
}

int addFxnodeF(float value, float floatplace, int fxnum, int tracknum, int blocknum, int windownum){
  if (floatplace < 0){
    handleError("Place can not be negative: %f", floatplace);
    return -1;
  }

  Place place;
  Float2Placement(floatplace, &place);

  return addFxnode(value, place, fxnum, tracknum, blocknum, windownum);
}
  
void setFxnode(int fxnodenum, float value, Place place, int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fx = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fx==NULL)
    return;

  R_ASSERT(value >= 0.0f);
  R_ASSERT(value <= 1.0f);

  const vector_t *nodes = GetFxNodes(window, wblock, wtrack, fx);
  if (fxnodenum < 0 || fxnodenum>=nodes->num_elements) {
    handleError("There is no fx node %d for fx %d in track %d in block %d",fxnodenum, fxnum, tracknum, blocknum);
    return;
  }

  struct Node *node = nodes->elements[fxnodenum];
  struct FXNodeLines *fxnodeline = (struct FXNodeLines *)node->element;
  
  if (place.line >= 0){
    Place *last_pos = PlaceGetLastPos(wblock->block);
    
    {
      SCOPED_PLAYER_LOCK_IF_PLAYING();
      ListMoveElement3_FromNum_ns(&fx->fxnodelines, fxnodenum, &place, PlaceGetFirstPos(), last_pos);      
    }
  }
  
  int max = fx->fx->max;
  int min = fx->fx->min;
  
  fxnodeline->val=scale(value, 0.0f, 1.0f, min, max); //R_BOUNDARIES(min,value,max);

  window->must_redraw_editor = true;
}

void setFxnodeF(int fxnodenum, float value, float floatplace, int fxnum, int tracknum, int blocknum, int windownum){
  Place place;

  if (floatplace >= 0.0f)
    Float2Placement(floatplace, &place);
  else {
    place.line = -1;
    place.counter = 0;
    place.dividor = 1;
  }
  
  return setFxnode(fxnodenum, value, place, fxnum, tracknum, blocknum, windownum);
}

void setFxnodeLogtype(int logtype, int fxnodenum, int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return;
  
  const vector_t *nodes = GetFxNodes(window, wblock, wtrack, fxs);
  if (fxnodenum < 0 || fxnodenum>=nodes->num_elements) {
    handleError("There is no fx node %d for fx %d in track %d in block %d",fxnodenum, fxnum, tracknum, blocknum);
    return;
  }

  ADD_UNDO(FXs(window, wblock->block, wtrack->track, wblock->curr_realline));

  struct Node *node = nodes->elements[fxnodenum];
  struct FXNodeLines *fxnodeline = (struct FXNodeLines *)node->element;

  fxnodeline->logtype = logtype;

  window->must_redraw_editor = true;
}

void deleteFxnode(int fxnodenum, int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return;

  const vector_t *nodes = GetFxNodes(window, wblock, wtrack, fxs);
  if (fxnodenum < 0 || fxnodenum>=nodes->num_elements) {
    handleError("There is no fx node %d for fx %d in track %d in block %d",fxnodenum, fxnum, tracknum, blocknum);
    return;
  }

  ADD_UNDO(FXs(window, wblock->block, wtrack->track, wblock->curr_realline));

  struct Node *node = nodes->elements[fxnodenum];
  struct FXNodeLines *fxnodeline = (struct FXNodeLines *)node->element;
  
  DeleteFxNodeLine(window, wtrack, fxs, fxnodeline); // DeleteFxNodeLine locks player / stops playing

  window->must_redraw_editor = true;
}


void setCurrentFxnode(int fxnodenum, int fxnum, int tracknum, int blocknum, int windownum){
 struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fx = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fx==NULL)
    return;

  const vector_t *nodes = GetFxNodes(window, wblock, wtrack, fx);
  if (fxnodenum < 0 || fxnodenum>=nodes->num_elements) {
    handleError("There is no fx node %d for fx %d in track %d in block %d",fxnodenum, fxnum, tracknum, blocknum);
    return;
  }

  struct Node *node = nodes->elements[fxnodenum];
  struct FXNodeLines *current = (struct FXNodeLines*)node->element;

  setCurrentNode(&current->l);
}

void setIndicatorFxnode(int fxnodenum, int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fx = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fx==NULL)
    return;

  const vector_t *nodes = GetFxNodes(window, wblock, wtrack, fx);
  if (fxnodenum < 0 || fxnodenum>=nodes->num_elements) {
    handleError("There is no fx node %d for fx %d in track %d in block %d",fxnodenum, fxnum, tracknum, blocknum);
    return;
  }

  struct Node *node = nodes->elements[fxnodenum];
  setIndicatorNode(node->element);
}

void setNoMouseFx(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if(wblock==NULL)
    return;
  
  if (wblock->mouse_fxs != NULL){
    wblock->mouse_fxs = NULL;
    window->must_redraw_editor = true;
    //printf("no mouse fx dirty\n");
  }
}

void setMouseFx(int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return;
  else if (wblock->mouse_fxs != fxs){
    wblock->mouse_fxs = fxs;
    window->must_redraw_editor = true;
    //printf("mouse fx dirty\n");
  }
}

void undoFxs(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;

  ADD_UNDO(FXs(window, wblock->block, wtrack->track, wblock->curr_realline));
}


////////////////////////
// fxs, clipboard

#include "../common/clipboard_range.h"

extern struct Range *range;

Place getFxrangenodePlace(int fxnodenum, int fxnum, int rangetracknum){
  if (range==NULL)
    return place(0,0,1);

  if (rangetracknum >= range->num_tracks || rangetracknum < 0 || fxnum < 0){
    handleError("rangetracknum >= range->num_tracks: %d >= %d (fxnum: %d)",rangetracknum, range->num_tracks, fxnum);
    return place(0,0,1);
  }
  
  struct FXs *fxs = VECTOR_get2(&range->fxs[rangetracknum], fxnum, "fxs");
  if (fxs==NULL){
    return place(0,0,1);
  }
  
  struct FXNodeLines *fxnodeline = ListFindElement3_num_r0(&fxs->fxnodelines->l, fxnodenum);

  if (fxnodeline==NULL){
    handleError("getFxrangenodeValue: fxnodenum >= getNumFxrangenodes: %d >= %d",fxnodenum, getNumFxrangenodes(fxnum, rangetracknum));
    return place(0,0,1);
  }

  return fxnodeline->l.p;
}


float getFxrangenodeValue(int fxnodenum, int fxnum, int rangetracknum){
  if (range==NULL)
    return 0;

  if (rangetracknum >= range->num_tracks || rangetracknum < 0 || fxnum < 0){
    handleError("rangetracknum >= range->num_tracks: %d >= %d (fxnum: %d)",rangetracknum, range->num_tracks, fxnum);
    return 0;
  }
  
  struct FXs *fxs = VECTOR_get2(&range->fxs[rangetracknum], fxnum, "fxs");
  if (fxs==NULL){
    return 0;
  }
  
  struct FXNodeLines *fxnodeline = ListFindElement3_num_r0(&fxs->fxnodelines->l, fxnodenum);

  if (fxnodeline==NULL){
    handleError("getFxrangenodeValue: fxnodenum >= getNumFxrangenodes: %d >= %d",fxnodenum, getNumFxrangenodes(fxnum, rangetracknum));
    return 0;
  }

  int max = fxs->fx->max;
  int min = fxs->fx->min;
  
  return scale(fxnodeline->val, min, max, 0.0f, 1.0f);
}

int getFxrangenodeLogtype(int fxnodenum, int fxnum, int rangetracknum){
  if (range==NULL)
    return 0;

  if (rangetracknum >= range->num_tracks || rangetracknum < 0 || fxnum < 0){
    handleError("rangetracknum >= range->num_tracks: %d >= %d (fxnum: %d)",rangetracknum, range->num_tracks, fxnum);
    return 0;
  }
  
  struct FXs *fxs = VECTOR_get2(&range->fxs[rangetracknum], fxnum, "fxs");
  if (fxs==NULL){
    return 0;
  }
  
  struct FXNodeLines *fxnodelines = ListFindElement3_num_r0(&fxs->fxnodelines->l, fxnodenum);

  if (fxnodelines==NULL){
    handleError("getFxrangenodeLogtype: fxnodenum >= getNumFxrangenodes: %d >= %d",fxnodenum, getNumFxrangenodes(fxnum, rangetracknum));
    return 0;
  }

  return fxnodelines->logtype;
}

const char* getFxrangeName(int fxnum, int rangetracknum){
  if (range==NULL)
    return 0;

  if (rangetracknum >= range->num_tracks || rangetracknum < 0 || fxnum < 0){
    handleError("rangetracknum >= range->num_tracks: %d >= %d (fxnum: %d)",rangetracknum, range->num_tracks, fxnum);
    return 0;
  }

  struct FXs *fxs = VECTOR_get2(&range->fxs[rangetracknum], fxnum, "fxs");
  if (fxs==NULL){
    return "";
  }
  
  return fxs->fx->name;
}

int getNumFxrangenodes(int fxnum, int rangetracknum){
  if (range==NULL)
    return 0;

  if (rangetracknum >= range->num_tracks || rangetracknum < 0 || fxnum < 0){
    handleError("rangetracknum >= range->num_tracks: %d >= %d (fxnum: %d)",rangetracknum, range->num_tracks, fxnum);
    return 0;
  }

  struct FXs *fxs = VECTOR_get2(&range->fxs[rangetracknum], fxnum, "fxs");
  if (fxs==NULL){
    return 0;
  }
  
  return ListFindNumElements3(&fxs->fxnodelines->l);
}

int getNumFxsInRange(int rangetracknum){
  if (range==NULL)
    return 0;

  if (rangetracknum >= range->num_tracks || rangetracknum < 0){
    handleError("rangetracknum >= range->num_tracks: %d >= %d",rangetracknum, range->num_tracks);
    return 0;
  }

  return range->fxs[rangetracknum].num_elements;
}

void clearTrackFX(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;

  struct Tracks *track = wtrack->track;

  PC_Pause();{
    VECTOR_clean(&track->fxs);
  }PC_StopPause(window);
}


//////////////////////////////////////////////////
// track widths
//////////////////////////////////////////////////

float getTrackWidth(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
    
  if (tracknum==-1){
    wblock = getWBlockFromNumA(windownum, &window, blocknum);
    if (wblock==NULL)
      return 0.0f;
    return wblock->temponodearea.width;
  } else {
    struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
    if (wtrack==NULL)
      return 0.0f;
    return wtrack->fxwidth;
  }
}

void undoTrackWidth(void){
  struct Tracker_Windows *window=getWindowFromNum(-1);
  if(window==NULL)
    return;
  
  ADD_UNDO(Block_CurrPos(window)); // can be optimized a lot
}

void setTrackWidth (float new_width, int tracknum, int blocknum, int windownum){
  if (new_width < 2) {
#if 0
    handleError("Can not set width smaller than 2");
    return;
#else
    new_width = 2;
#endif
  }

  struct Tracker_Windows *window;
  struct WBlocks *wblock;
          
  if (tracknum==-1){
    wblock = getWBlockFromNumA(windownum, &window, blocknum);
    if (wblock==NULL)
      return;
    wblock->temponodearea.width = new_width;
  } else {
    struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
    if (wtrack==NULL)
      return;
    //printf("new width: %d, old: %d\n",(int)new_width,wtrack->fxwidth);
    wtrack->fxwidth = new_width;
  }

  //UpdateWBlockCoordinates(window,wblock);
  //GL_create(window);
  
  window->must_redraw=true;
}



// ctrl / shift keys
//////////////////////////////////////////////////

extern struct TEvent tevent;

bool controlPressed(void){
  return ControlPressed(); //AnyCtrl(tevent.keyswitch);
}

bool control2Pressed(void){
#if FOR_MACOSX
  return AltPressed();
#else
  return MetaPressed();
#endif
}

bool shiftPressed(void){
  return ShiftPressed(); //AnyShift(tevent.keyswitch);
}

/*
// Doesn't work to check right extra
bool extraPressed(void){
  return AnyExtra(tevent.keyswitch);
}
*/

bool metaPressed(void){
  //return LeftExtra(tevent.keyswitch);
  return MetaPressed();
}

bool altPressed(void){
  return AltPressed(); //AnyAlt(tevent.keyswitch);
}



// mouse pointer
//////////////////////////////////////////////////

/*
enum MousePointerType {
  MP_NORMAL,
  MP_BLANK,
  MP_DIAGONAL,
  MP_HORIZONTAL,  
};
*/

void setNormalMousePointer(int64_t guinum){
  if (guinum<0){
    handleError("guinum for cursor must be 0 or larger");
    return;
  }
    SetNormalPointer(guinum);
}
void setPointingMousePointer(int64_t guinum){
  if (guinum<0){
    handleError("guinum for cursor must be 0 or larger");
    return;
  }
    SetPointingPointer(guinum);
}
void setOpenHandMousePointer(int64_t guinum){
  if (guinum<0){
    handleError("guinum for cursor must be 0 or larger");
    return;
  }
    SetOpenHandPointer(guinum);
}
void setClosedHandMousePointer(int64_t guinum){
  if (guinum<0){
    handleError("guinum for cursor must be 0 or larger");
    return;
  }
    SetClosedHandPointer(guinum);
}
void setBlankMousePointer(int64_t guinum){
  if (guinum<0){
    handleError("guinum for cursor must be 0 or larger");
    return;
  }
    SetBlankPointer(guinum);
}
void setDiagonalResizeMousePointer(int64_t guinum){
  if (guinum<0){
    handleError("guinum for cursor must be 0 or larger");
    return;
  }
    SetDiagResizePointer(guinum);
}
void setHorizontalResizeMousePointer(int64_t guinum){
  if (guinum<0){
    handleError("guinum for cursor must be 0 or larger");
    return;
  }
    SetHorizResizePointer(guinum);
}
void setHorizontalSplitMousePointer(int64_t guinum){
  if (guinum<0){
    handleError("guinum for cursor must be 0 or larger");
    return;
  }
    SetHorizSplitPointer(guinum);
}
void setVerticalResizeMousePointer(int64_t guinum){
  if (guinum<0){
    handleError("guinum for cursor must be 0 or larger");
    return;
  }
    SetVerticalResizePointer(guinum);
}

void moveMousePointer(float x, float y, int windownum){
  struct Tracker_Windows *window = getWindowFromNum(windownum);
  if (window!=NULL)
    MovePointer(window, x, y);
}

float getMousePointerX(int windownum){
  struct Tracker_Windows *window = getWindowFromNum(windownum);
  if (window==NULL)
    return 0;

  WPoint ret = GetPointerPos(window);
  return ret.x;
}
float getMousePointerY(int windownum){
  struct Tracker_Windows *window = getWindowFromNum(windownum);
  if (window==NULL)
    return 0;

  WPoint ret = GetPointerPos(window);
  return ret.y;
}
float getGlobalMousePointerX(void){
  WPoint ret = GetPointerPos(NULL);
  return ret.x;
}
float getGlobalMousePointerY(void){
  WPoint ret = GetPointerPos(NULL);
  return ret.y;
}
