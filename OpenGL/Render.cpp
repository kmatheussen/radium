/* Copyright 2014-2016 Kjetil S. Matheussen

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


#include <math.h>

#include <vlCore/VisualizationLibrary.hpp>
#include <vlGraphics/OpenGLContext.hpp>

#include "../common/nsmtracker.h"
#include "../common/settings_proc.h"
#include "../common/list_proc.h"
#include "../common/placement_proc.h"
#include "../common/realline_calc_proc.h"
#include "../common/gfx_subtrack_proc.h"
#include "../common/wtracks_proc.h"
#include "../common/time_proc.h"
#include "../common/tracks_proc.h"
#include "../common/patch_proc.h"
#include "../common/common_proc.h"
#include "../common/notes_proc.h"
#include "../common/trackreallines2_proc.h"
#include "../common/veltext_proc.h"
#include "../common/fxtext_proc.h"
#include "../common/notes_proc.h"
#include "../common/nodelines_proc.h"
#include "../common/Signature_proc.h"
#include "../common/LPB_proc.h"
#include "../common/tempos_proc.h"
#include "../Qt/Qt_instruments_proc.h"

#include "GfxElements.h"

#include "../api/api_proc.h"

#include "Render_proc.h"


// Functions in this file are called from the main thread.

static bool g_colored_tracks = false;

static bool g_is_creating_all_GL_blocks = false; // When true, we must create as many gradients as might be shown later, to avoid linking gradients when playing.

void GL_set_colored_tracks(bool onoff){
  printf("setting safe mode to %d\n",onoff);
  SETTINGS_write_bool("colored_tracks", onoff);
  g_colored_tracks = onoff;

  struct Tracker_Windows *window = root->song->tracker_windows;
  window->must_redraw = true;
}

bool GL_get_colored_tracks(void){
  return SETTINGS_read_bool("colored_tracks", true);
}

static void init_g_colored_tracks_if_necessary(void){
  static bool has_inited = false;

  if (!has_inited){
    g_colored_tracks = GL_get_colored_tracks();
    has_inited = true;
  }
}


extern char *NotesTexts3[];
extern char *NotesTexts2[];

static float get_pitchline_width(void){
  static float width = -1;

  if (width<0)
    width = 1.2; //SETTINGS_read_double("gfx_pitchline_width", 1.2); // If changing this value, also change bin/config

  return width;
}

static float get_nodeline_width(void){
  static float width = -1;

  if (width<0)
    width = 1.8; //SETTINGS_read_double("gfx_nodeline_width", 1.0); // If changing this value, also change bin/config

  return width;
}

static float get_nodeline_width(bool is_selected){
  float width = get_nodeline_width();

  if (is_selected)
    return width * 2.3;
  else
    return width;
}

static void draw_bordered_text(
                               const struct Tracker_Windows *window,
                               GE_Context *c,
                               char *text,
                               int x,
                               int y
                               )
{
  GE_text(c, text, x, y);
  
  int z = GE_get_z(c);
  int x2=x+((int)strlen(text)*window->fontwidth);
  
  // 2. Line (the gradient one)
  QColor qc1 = GE_qcolor(LOW_BACKGROUND_COLOR_NUM).darker(96);
  QColor qc2 = GE_qcolor(LOW_BACKGROUND_COLOR_NUM).darker(113);
  GE_Context *c2 = GE_gradient_z(qc1, qc2, z, y); //GE_get_rgb(9), GE_get_rgb(11), z);

  GE_gradient_triangle_start(GradientType::HORIZONTAL);
  GE_gradient_triangle_add(c2, x,  y+0.75 - 0.5);
  GE_gradient_triangle_add(c2, x2, y+0.75 - 0.5);
  GE_gradient_triangle_add(c2, x,  y+1.25);
  GE_gradient_triangle_add(c2, x2, y+1.25);
  GE_gradient_triangle_end(c2, x,  x2);  
}


static void draw_text_num(
                          const struct Tracker_Windows *window,
                          GE_Context *c,
                          int num,
                          int length,
                          int x,
                          int y
                          )
{
  char temp[50];
  char temp2[50];
  char temp3[60];
  int length2;
  
  sprintf(temp,"%d",num);

  length2=length-(int)strlen(temp);  
  if(length2<0)
    length2=0;
  
  if(length2!=0){
    memset(temp2,' ',length2+1);
    temp2[length2]=0;
  }else
    temp2[0]=0;
  
  sprintf(temp3,"%s%s",temp2,temp);
  temp3[length+1]=0;

  GE_text(c, temp3, x, y);
}


static void draw_node_indicator(float x,
                                float y)
{  
  GE_Context *c = GE_color_alpha_z(AUTOMATION_INDICATOR_COLOR_NUM, 0.6, Z_MAX_SCROLLTRANSFORM, NOMASK_Y);
  
  float away1 = 1024;
  float away2 = 5;
  float thickness = 0.8;

  // horizontal
  GE_line(c,
          x - away1, y,
          x - away2, y,
          thickness
          );
  GE_line(c,
          x + away2, y,
          x + away1, y,
          thickness
          );

  // vertical
  GE_line(c,
          x, y - away1,
          x, y - away2,
          thickness
          );
  GE_line(c,
          x, y + away1,
          x, y + away2,
          thickness
          );
}

const struct ListHeader3 *current_node = NULL;
const struct ListHeader3 *indicator_node = NULL;
int indicator_velocity_num = -1;
int indicator_pitch_num = -1;

static void draw_skewed_box(const struct Tracker_Windows *window,
                            const struct ListHeader3 *node,
                            enum ColorNums color,
                            float x, float y
                            )
{
 
  float minnodesize = window->fontheight / 1.5; // if changing 1.5 here, also change 1.5 in getHalfOfNodeWidth in api/api_mouse.c and common/SeqAutomation.hpp
  float x1 = x-minnodesize;
  float x2 = x+minnodesize;
  float y1 = y-minnodesize;
  float y2 = y+minnodesize;
  const float width = 1.2;

  int z = Z_ABOVE(Z_ZERO);

  if (node == current_node) {
    GE_filledBox(GE_mix_alpha_z(GE_get_rgb(color), White_rgb(), 300, 0.3, z, y),
                 x1,y1,
                 x2-1,y2
                 );
  }

  // vertical left
  GE_line(GE_mix_alpha_z(GE_get_rgb(color), White_rgb(), 100, 0.3, z, y1+1),
          x1+1, y1+1,
          x1+2,y2-1,
          width);

  // horizontal bottom
  GE_line(GE_mix_alpha_z(GE_get_rgb(color), Black_rgb(), 300, 0.3, z, y2-1),
          x1+2,y2-1,
          x2-1,y2-2,
          width);

  // vertical right
  GE_line(GE_mix_alpha_z(GE_get_rgb(color), Black_rgb(), 400, 0.3, z, y1+2),
          x2-1,y2-2,
          x2-2,y1+2,
          width);

  // horizontal top
  GE_line(GE_mix_alpha_z(GE_get_rgb(color), White_rgb(), 300, 0.3, z, y1+1),
          x2-2,y1+2,
          x1+1,y1+1,
          width);
}


void create_double_border(
                          float x, int y, int y2
                          )
{
  if (false){ //ATOMIC_GET(root->editonoff)){
    // old
    GE_line(Black_color(y),x,y,x,y2,0.5);
    GE_line(GE_color(TRACK_SEPARATOR2B_COLOR_NUM,NOMASK_Y),x+1,y,x+1,y2,0.5);
  } else {
    float black_width = 1.5f;
    float white_width = 1.0f;
    float black_skew = black_width/2.0f;
    float white_skew = black_skew + black_width + -white_width/2.0f;
    GE_line(GE_color(TRACK_SEPARATOR2A_COLOR_NUM,NOMASK_Y),
            x+black_skew, y,
            x+black_skew, y2,
            black_width
            );
    GE_line(GE_color(TRACK_SEPARATOR2B_COLOR_NUM,NOMASK_Y),
            x+white_skew, y,
            x+white_skew, y2,
            white_width
            );
  }
}

void create_single_border(
                          int x, int y, int y2
                          )
{
  GE_line(GE_color(TRACK_SEPARATOR1_COLOR_NUM,NOMASK_Y),x,y,x,y2,0.5);
}


void create_single_linenum_border(
                                  int x, int y, int y2
                                  )
{
  GE_line(GE_color_alpha(TRACK_SEPARATOR1_COLOR_NUM,0.5,NOMASK_Y),x,y,x,y2,0.5);
}



static void stipled_vertical_line(GE_Context *c, float x, float y1, float y2){
  const float width = 0.3;
  const float bit = 4 + 4 * (float)qrand()/(float)RAND_MAX;

  for (float y = y1 ; y < y2 ; y += bit){
    float y22 = R_MIN(y2, y+bit/2);
    GE_line(c, x, y, x, y22, width);
  }
}


static GE_Context *drawNodeLines(const struct Tracker_Windows *window, const struct NodeLine *nodelines, enum ColorNums colnum, bool is_selected, float alpha, float alpha_selected, bool hide_vertical){
  const float cut_size1 = window->fontheight*2;
  //const float cut_size2 = 10;

  GE_Context *c = NULL;
  
  float width = get_nodeline_width(is_selected);
  
  for(const struct NodeLine *ns = nodelines ; ns!=NULL ; ns=ns->next) {
    int logtype = ns->logtype;
    float x1 = ns->x1;
    float x2 = logtype==LOGTYPE_HOLD ? ns->x1 : ns->x2;
    float y1 = ns->y1;
    float y2 = ns->y2;

    c = c!=NULL ? GE_y(c, y1) : GE_color_alpha_z(colnum, is_selected ? alpha_selected : alpha, Z_ABOVE(Z_ABOVE(Z_ZERO)), 0);

    bool paint_stipled = hide_vertical && is_selected==false && x1 == x2 && (y2 - y1) > cut_size1*3.5;
    
    if (paint_stipled) {
#if 1
      GE_line(c, x1, y1, x2, y1 + cut_size1, width);
      GE_line(c, x1, y2 - cut_size1, x2, y2, width);
      
      stipled_vertical_line(c, x1, y1+cut_size1, y2-cut_size1);
#else
      GE_line(c, x1, y1, x2, y1 + cut_size1, width);
      // arrow
      GE_line(c, x2-3, y1+cut_size1-5, x2, y1 + cut_size1, width);
      GE_line(c, x2+3, y1+cut_size1-5, x2, y1 + cut_size1, width);
              
      GE_line(c, x1, y2 - cut_size2, x2, y2, width);
      // arrow
      GE_line(c, x2-3, y2-cut_size2-5, x2, y2 - cut_size2, width);
      GE_line(c, x2+3, y2-cut_size2-5, x2, y2 - cut_size2, width);
#endif
    } else {
      GE_line(c, x1, y1, x2, y2, width);
    }

    if (logtype==LOGTYPE_HOLD)
      GE_line(c, x2, y2, ns->x2, y2, width);
  }
  
  return c;
}




/************************************
   Left slider
 ************************************/

static void create_left_slider(const struct Tracker_Windows *window, const struct WBlocks *wblock){
  GE_Context *border = GE_color_z(LINE_SLIDER_COLOR_NUM, Z_STATIC, NOMASK_Y);

  float x1 = get_scrollbar_x1(window);
  float y1 = get_scrollbar_y1(window, wblock);
  float x2 = get_scrollbar_x2(window);
  float y2 = get_scrollbar_y2(window, wblock);
  
  GE_box(border,
         x1, y1,
         x2, y2,
         1.0f);

  //GE_Context *scrollbar = GE_mix_color_z(Black_rgb(), GE_get_rgb(0), 900, Z_SCROLLBAR);
  GE_Context *scrollbar;

  if (window->scrollbar_is_moving)
    scrollbar = GE_mix_color_z(White_rgb(), GE_get_rgb(LINE_SLIDER_COLOR_NUM), 200, Z_SCROLLBAR, NOMASK_Y);
  else
    scrollbar = GE_color_z(LINE_SLIDER_COLOR_NUM, Z_SCROLLBAR, NOMASK_Y);
  
  GE_filledBox(scrollbar,
               x1+2, 0, // (does not paint at editor.y1=0, but at scrollbar_slider.y1=0)
               x2-2, get_scrollbar_scroller_height(window,wblock)
               );
}


/************************************
   Background
 ************************************/


extern int beat_opacity;
extern int first_beat_opacity;
extern int line_opacity;

/*
static bool realline_is_beat(const struct WBlocks *wblock, int realline){
  int line = wblock->reallines[realline]->l.p.line;
  return wblock->block->times[line].is_beat;
}
*/

static GE_Rgb shade_realline(const GE_Rgb rgb, bool is_current_track, const WSignature &wsignature, const struct LocalZooms *localzoom){
  const GE_Rgb rgb2 =
    is_current_track
    ? GE_mix(rgb, White_rgb(), 950)
    : rgb;
          
  if (WSIGNATURE_is_first_beat(wsignature))
    return GE_mix(rgb2, Black_rgb(), first_beat_opacity);
  
  else if (wsignature.beat_num>0)
    return GE_mix(rgb2, Black_rgb(), beat_opacity);
  
  else if(localzoom->level>0 && localzoom->zoomline>0 && localzoom->autogenerated==false)
    return GE_mix(rgb2, White_rgb(), 925); // manually splitted line
  
  else
    return rgb2;
}

static void create_background_realline(const struct Tracker_Windows *window, const struct WBlocks *wblock, const WSignature &wsignature, int realline){

  const struct WTracks *last_wtrack = (const struct WTracks*)ListLast1(&wblock->wtracks->l);
  const struct LocalZooms *localzoom = wblock->reallines[realline];
  
  //int x1 = wblock->linenumarea.x;
  int x1 = wblock->tempocolorarea.x;
  int x2 = last_wtrack->x2;
  int y1 = get_realline_y1(window, realline);
  int y2 = get_realline_y2(window, realline);


  const enum ColorNums c15 = HIGH_EDITOR_BACKGROUND_COLOR_NUM;


  // background
  {
    if(beat_opacity == -1)
      beat_opacity = SETTINGS_read_int32("beat_opacity", 950);

    if(first_beat_opacity == -1)
      first_beat_opacity = SETTINGS_read_int32("first_beat_opacity", 870);
    

    struct WTracks *wtrack=(struct WTracks*)ListFindElement1(&wblock->wtracks->l,wblock->left_track);
    
    {
      GE_Context *c = GE_z(shade_realline(GE_get_rgb(c15), false, wsignature, localzoom), Z_BACKGROUND | Z_STATIC_X, y1);
      
      if (g_colored_tracks)
        GE_filledBox(c,x1,y1,wtrack->x,y2);
      else
        GE_filledBox(c,x1,y1,x2,y2);
    }

    if (g_colored_tracks) {
      
      struct WTracks *wtrack=(struct WTracks*)ListFindElement1(&wblock->wtracks->l,wblock->left_track);
      while(wtrack!=NULL && wtrack->l.num<=wblock->right_track){

        int x1 = R_MAX(wtrack->x - 1, wblock->t.x1);
        int x2 = wtrack->x2;
        //int y1 = wtrack1->y;
        //int y2 = wtrack1->panonoff.y1 - 1;
        
        struct Patch *patch = wtrack->track->patch;
        
        if (patch != NULL){
          GE_Rgb rgb = patch==NULL ? GE_get_custom_rgb(HIGH_EDITOR_BACKGROUND_COLOR_NUM) : GE_get_rgb(patch->color);

          bool is_current_track = get_current_instruments_gui_patch()==patch;

          GE_Context *c = GE_z(
                               shade_realline(rgb,
                                              is_current_track,
                                              wsignature,
                                              localzoom
                                              )
                               ,
                               Z_BACKGROUND | Z_STATIC_X,
                               y1
                               );
          
          GE_filledBox(c,x1,y1,x2,y2);
        }
        
        wtrack=NextWTrack(wtrack);
      }
      
    }

  }

  float line_width = 0.6f;

  // realline separator line
  if(1){
    //if(line_opacity == -1)
    //  line_opacity = SETTINGS_read_int("line_opacity", R_MAX(50, beat_opacity-500));
    line_opacity = 900;
    
    if(line_opacity != 1000) {
      GE_Context *c;

      if (WSIGNATURE_is_first_beat(wsignature))
        c = GE_mix_color_z(GE_get_rgb(c15), Black_rgb(), first_beat_opacity-250, Z_ABOVE(Z_BACKGROUND) | Z_STATIC_X, y1);
      else if (wsignature.beat_num>0)
        c = GE_mix_color_z(GE_get_rgb(c15), Black_rgb(), beat_opacity-250, Z_ABOVE(Z_BACKGROUND) | Z_STATIC_X, y1);
      else
        c = GE_mix_color_z(GE_get_rgb(c15), Black_rgb(), line_opacity, Z_ABOVE(Z_BACKGROUND) | Z_STATIC_X, y1);

      if (true) {
        GE_line(c,x1,y1,x2,y1,line_width);

        for(float f : wsignature.how_much_below)
          if (fabs(f) > 0.1) {
            float y_ = scale(f, 0, 1, y1, y2);
            float x1_ = wblock->linenumarea.x;
            float x2_ = wblock->linenumarea.x2;
            GE_line(c,x1_,y_,x2_,y_,2.3);
          }

      } else {
        for(float f : wsignature.how_much_below){
          float y_ = scale(f, 0, 1, y1, y2);
          GE_line(c,x1,y_,x2,y_,line_width);
        }
      }
      //GE_line(c,x1,y1,x2,y1,line_width);
    }
  }
}


static void create_background(const struct Tracker_Windows *window, const struct WBlocks *wblock, const WSignature_trss &wsignatures_trss){
  int realline;
  for(realline = 0 ; realline<wblock->num_reallines ; realline++)
    create_background_realline(window, wblock, wsignatures_trss[realline], realline);
}





/************************************
   Linenumbers
 ************************************/

static void draw_linenumber(const struct Tracker_Windows *window, const struct WBlocks *wblock, const WSignature &wsignature, int realline){
  const struct LocalZooms *localzoom = wblock->reallines[realline];

  bool is_beatnum = wsignature.beat_num > 0;
  bool is_barnum = wsignature.bar_num>0 && WSIGNATURE_is_first_beat(wsignature);
  bool is_zoomline = localzoom->level>0 && localzoom->zoomline>0 && localzoom->autogenerated==false;

  const int z = Z_LINENUMBERS | Z_STATIC_X;

  float y1 = get_realline_y1(window, realline);
  
  if (linenumbersVisible() && localzoom->l.p.counter==0) {

    draw_text_num(
                  window,
                  GE_textcolor_z(TEXT_COLOR_NUM, z, y1),
                  localzoom->l.p.line,
                  wblock->linenumbers_max_num_characters,
                  wblock->linenumarea.x,
                  y1
                  );
    

  } else if (is_barnum) {
    
    draw_text_num(
                  window,
                  GE_textcolor_z(BAR_TEXT_COLOR_NUM, z, y1),
                  wsignature.bar_num,
                  wblock->bars_max_num_characters,
                  wblock->linenumarea.x,
                  y1
                  );

  } else if (is_beatnum) {

    draw_text_num(
                  window,
                  GE_color_alpha_z(TEXT_COLOR_NUM, 0.45, Z_ZERO, y1),
                  wsignature.beat_num,
                  wblock->beats_max_num_characters,
                  wblock->beats_x,
                  y1
                  );

  } else if (is_zoomline) {

    enum ColorNums colornum;
    
    switch(localzoom->level){
    case 1:
      colornum=ZOOMLINE_TEXT_COLOR_NUM1;
      break;
    case 2:
      colornum=ZOOMLINE_TEXT_COLOR_NUM2;
      break;
    case 3:
      colornum=ZOOMLINE_TEXT_COLOR_NUM3;
      break;
    case 4:
      colornum=ZOOMLINE_TEXT_COLOR_NUM4;
      break;
    case 5:
      colornum=ZOOMLINE_TEXT_COLOR_NUM5;
      break;
    case 6:
      colornum=ZOOMLINE_TEXT_COLOR_NUM6;
      break;
    default:
      colornum=ZOOMLINE_TEXT_COLOR_NUM7;
      break;
    }
    
    draw_text_num(
                  window,
                  GE_textcolor_z(colornum, z, y1),
                  localzoom->zoomline + 1,
                  wblock->zoomlines_max_num_characters,
                  wblock->zoomlines_x,
                  y1
                  );
    
  }
}

static void create_linenumbers(const struct Tracker_Windows *window, const struct WBlocks *wblock, const WSignature_trss &wsignatures){
  int realline;
  for(realline = 0 ; realline<wblock->num_reallines ; realline++)
    draw_linenumber(window, wblock, wsignatures[realline], realline);

#if 0
  draw_linenumber(window,
                  wblock,
                  -1,
                  wblock->block->num_lines,
                  wblock->num_reallines,
                  false
                  );
#endif
}




/************************************
   Tempograph
 ************************************/

struct TempoGraph{
  float line_period;
  int num_points;
  STime *times; // delta values
  float min;
  float max;  
};

struct TempoGraph create_TempoGraph(const struct Tracker_Windows *window, const struct WBlocks *wblock){
  TempoGraph tg = {}; //(struct TempoGraph*)talloc(sizeof(struct TempoGraph));
  
  static int times_size = 0;
  static STime *times = NULL;
  
  int TEMPOGRAPH_POINTS_PER_REALLINE = window->fontheight / 2;
  tg.line_period = window->fontheight / (float)TEMPOGRAPH_POINTS_PER_REALLINE;
  tg.num_points  = (wblock->num_reallines * TEMPOGRAPH_POINTS_PER_REALLINE) + 1;

  if (tg.num_points>times_size){
    times = (STime*)V_realloc(times, tg.num_points*sizeof(STime));
    times_size = tg.num_points;
  }
  tg.times       = times;

  STime last_time = -1;
  int pos=0;
  
  for(int realline = 0 ; realline < wblock->num_reallines ; realline++){
    float fp1=GetfloatFromPlace(&wblock->reallines[realline]->l.p);
    float fp2;
    if(realline<wblock->num_reallines-1){
      fp2=GetfloatFromPlace(&wblock->reallines[realline+1]->l.p);
    }else{
      fp2=(float)wblock->block->num_lines;
    }

    for(int n = 0 ; n<TEMPOGRAPH_POINTS_PER_REALLINE ; n++){
      Place p;
      Float2Placement(scale(n,0,TEMPOGRAPH_POINTS_PER_REALLINE,fp1,fp2), &p);
      STime time = Place2STime_from_times(wblock->block->num_lines, wblock->block->times_with_global_swings, &p);
      if(realline>0 || n>0){
        STime val = time-last_time;
        if(tg.min<val || pos==0)
          tg.min = val;
        if(tg.max>val || pos==0)
          tg.max = val;
        
        tg.times[pos++] = val; //realline*TEMPOGRAPH_POINTS_PER_REALLINE + n - 1] = time-last_time;
        //printf("Setting %d (of %d)\n",realline*TEMPOGRAPH_POINTS_PER_REALLINE + n - 1, tg.num_points);
      }
      last_time = time;
    }
  }
  {
    STime val = getBlockSTimeLength(wblock->block) - last_time;
    tg.times[pos++] = val;
    tg.times[pos++] = val;
    if(tg.min<val)
      tg.min = val;
    if(tg.max>val)
      tg.max = val;
  }

  R_ASSERT_NON_RELEASE(tg.num_points==pos);
  
  tg.num_points = pos;
  
  /*
  tg.min=tg.times[0];
  tg.max=tg.times[0];
  for(int n=1;n<tg.num_points;n++){
    STime time = tg.times[n];
    if(tg.min<time)
      tg.min = time;
    if(tg.max>time)
      tg.max = time;
  }
  */
  
  return tg;
}

static void create_tempograph(const struct Tracker_Windows *window, const struct WBlocks *wblock){
  const TempoGraph tg = create_TempoGraph(window,wblock);

  float width = 1.3;

  //printf("min/max: %d, %d\n",(int)min,(int)max);

  GE_Context *c = NULL;
    
  if(fabs(tg.min - tg.max)<20) {
    float middle = (wblock->tempocolorarea.x+wblock->tempocolorarea.x2) / 2.0f;
    float y1 = get_realline_y1(window, 0);
    c = c!=NULL ? GE_y(c, y1) : GE_color(TEMPOGRAPH_COLOR_NUM, y1);
    GE_line(c,
            middle, y1,
            middle, get_realline_y2(window, wblock->num_reallines-1),
            width);
  }else{
    for(int n=0;n<tg.num_points-1;n++){
      float y1 = n * tg.line_period;
      float y2 = (n+1) * tg.line_period;
      c = c!=NULL ? GE_y(c, y1) : GE_color(TEMPOGRAPH_COLOR_NUM, y1);
      GE_line(c, 
              scale(tg.times[n],   tg.min, tg.max, wblock->tempocolorarea.x, wblock->tempocolorarea.x2), y1,
              scale(tg.times[n+1], tg.min, tg.max, wblock->tempocolorarea.x, wblock->tempocolorarea.x2), y2,
              width);
    }
  }
 
}



/************************************
   Time signature track
 ************************************/

static void create_signature(const struct Tracker_Windows *window, const struct WBlocks *wblock, const WSignature &wsignature, int realline, int x2){
    
  if (WSIGNATURE_is_measure_change(wsignature)) {

    int x = wblock->signaturearea.x;
    int y = get_realline_y1(window, realline);
  
    const Ratio &signature = wsignature.signature;
    
    char temp[50];
    sprintf(temp, "%d/%d", (int)signature.numerator, (int)signature.denominator);
    
    GE_text(GE_textcolor_z(TEXT_COLOR_NUM, Z_ZERO, y),
            temp,
            x,
            y
            );
  }
}



static void create_signaturetrack(const struct Tracker_Windows *window, const struct WBlocks *wblock, const WSignature_trss &wsignatures_trss){

  const struct WTracks *last_wtrack = (const struct WTracks*)ListLast1(&wblock->wtracks->l);
  int x2 = last_wtrack->x2;

  int realline;
  for(realline = 0 ; realline<wblock->num_reallines ; realline++)
    create_signature(window, wblock, wsignatures_trss[realline], realline, x2);
}




/************************************
   lpb track
 ************************************/

static void create_lpb(const struct Tracker_Windows *window, const struct WBlocks *wblock, struct WLPBs *wlpbs, int realline){
  int y    = get_realline_y1(window, realline);
  int lpb  = wlpbs[realline].lpb;
  int type = wlpbs[realline].type;
  
  if(lpb!=0){
    draw_text_num(
                  window,
                  GE_textcolor_z(TEXT_COLOR_NUM, Z_ZERO, y),
                  lpb,
                  wblock->lpbarea.width/window->fontwidth,
                  wblock->lpbarea.x,
                  y
                  );
  }
  
  if(type!=LPB_NORMAL){
    const char *typetext;
    switch(type){
    case LPB_BELOW:
      typetext="d";
      break;
    case LPB_MUL:
      typetext="m";
      break;
    default:
      typetext="";
      RError("something is wrong");
    };
    
    GE_text(GE_color_alpha_z(TEXT_COLOR_NUM, 0.3, Z_ZERO, y), typetext, wblock->lpbTypearea.x, y);
  }
}



static void create_lpbtrack(const struct Tracker_Windows *window, const struct WBlocks *wblock){

  struct WLPBs *wlpbs = WLPBs_get(window, wblock);

  int realline;
  for(realline = 0 ; realline<wblock->num_reallines ; realline++)
    create_lpb(window, wblock, wlpbs, realline);
}



/************************************
   swing tracks (both global and sub)
 ************************************/

static void create_swing(const struct Tracker_Windows *window, const struct WBlocks *wblock, int realline, int weight, enum WSwingType type, int logtype, bool autogenerated, int x){
  int y    = get_realline_y1(window, realline);

  GE_Context *c = autogenerated ? GE_color_alpha_z(TEXT_COLOR_NUM, 0.3, Z_ZERO, y) : GE_textcolor_z(TEXT_COLOR_NUM, Z_ZERO, y);
  
  draw_text_num(
                window,
                c,
                weight,
                2, //wblock->swingarea.width/window->fontwidth,
                x + window->fontwidth, //wblock->swingarea.x,
                y
                );
  
  if(type!=WSWING_NORMAL){
    const char *typetext;
    switch(type){
    case WSWING_BELOW:
      typetext="d";
      break;
    case WSWING_MUL:
      typetext="m";
      break;
    default:
      typetext="";
      RError("something is wrong");
    };
    
    GE_text(GE_color_alpha_z(TEXT_COLOR_NUM, 0.3, Z_ZERO, y), typetext, x, y);
  }

  if (logtype==LOGTYPE_HOLD){
    GE_text(c, "|", x + window->fontwidth*3, y);
  }
}


static void create_swingtrack(const struct Tracker_Windows *window, const struct WBlocks *wblock, const dynvec_t *barswings, const float x){

  R_ASSERT_RETURN_IF_FALSE(wblock->block->beats!=NULL);
  R_ASSERT_RETURN_IF_FALSE(wblock->block->filledout_swings.type==ARRAY_TYPE);

  const struct Beats *beats = wblock->block->beats;
  
  //const dynvec_t *barswings = wblock->block->filledout_swings.array;

  int realline = 0;
  int curr_weight = -1;
  enum WSwingType type = WSWING_NORMAL;
  int curr_logtype = 0;
  int num_elements = 0;
  bool curr_autogenerated = false;
  
  for(int i = 0; i < barswings->num_elements ; i++){
    R_ASSERT_RETURN_IF_FALSE(barswings->elements[i].type==HASH_TYPE);
    
    const hash_t *barswing = barswings->elements[i].hash;
    
    int barnum = HASH_get_int32(barswing, ":barnum");
    const dynvec_t *swings = HASH_get_dyn(barswing, ":swings").array;
    bool autogenerated = HASH_has_key(barswing, ":auto-generated"); //HASH_get_bool(barswing, ":auto-generated");
    //int num_lines = HASH_get_int32(barswing, ":num-lines");

    while(beats!=NULL && beats->bar_num-1 < barnum)
      beats = NextBeat(beats);

    R_ASSERT_RETURN_IF_FALSE(beats!=NULL);

    for(int swingnum = 0 ; swingnum < swings->num_elements ; swingnum++){
      R_ASSERT_RETURN_IF_FALSE(swings->elements[swingnum].type==ARRAY_TYPE);
      
      const dynvec_t *swing = swings->elements[swingnum].array;
      
      const Place place = p_Add(beats->l.p, DYN_get_place(swing->elements[0]));
        
      int weight = (int)DYN_get_int64_from_number(swing->elements[1]);
      int logtype = (int)DYN_get_int64_from_number(swing->elements[2]);
      bool thisone_autogenerated = swing->num_elements==4;
        
      Place realline_place2 = realline==wblock->num_reallines-1 ? p_Last_Pos(wblock->block) :  wblock->reallines[realline+1]->l.p;

      if (p_Greater_Or_Equal(place, realline_place2)){

        if (curr_weight != -1)
          create_swing(window, wblock, realline, curr_weight, type, curr_logtype, curr_autogenerated, x);

        do{
          realline++;
          realline_place2 = realline==wblock->num_reallines-1 ? p_Last_Pos(wblock->block) :  wblock->reallines[realline+1]->l.p;
        }while(p_Greater_Or_Equal(place, realline_place2));
        
        curr_weight = -1;
        type = WSWING_NORMAL;
        num_elements = 0;
      }
      
      curr_weight = weight;
      curr_logtype = logtype;
      curr_autogenerated = autogenerated || thisone_autogenerated;
      
      num_elements++;

      const Place realline_place1 = wblock->reallines[realline]->l.p;

      if (p_Equal(place, realline_place1))
        type = WSWING_NORMAL;
      else if (num_elements==1)
        type = WSWING_BELOW;
      else
        type = WSWING_MUL;
    }
  }

  if (curr_weight != -1)
    create_swing(window, wblock, realline, curr_weight, type, curr_logtype, curr_autogenerated, x);
}




/************************************
   bpm track
 ************************************/

static void create_bpm(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WBPMs *wbpms, int realline){
  int y     = get_realline_y1(window, realline);
  int tempo = wbpms[realline].tempo;
  int type  = wbpms[realline].type;
  int logtype = wbpms[realline].logtype;

  GE_Context *c = GE_textcolor_z(TEXT_COLOR_NUM, Z_ZERO, y);

  if(tempo!=0){
    draw_text_num(
                  window,
                  c,
                  tempo,
                  3, //wblock->tempoarea.width/window->fontwidth,
                  wblock->tempoarea.x,
                  y
                  );
  }

  if(type!=TEMPO_NORMAL){
    const char *typetext;
    switch(type){
    case TEMPO_BELOW:
      typetext="d";
      break;
    case TEMPO_MUL:
      typetext="m";
      break;
    default:
      abort();
    };

    GE_text(GE_color_alpha_z(TEXT_COLOR_NUM, 0.3, Z_ZERO, y), typetext, wblock->tempoTypearea.x, y);
  }

  if (logtype==LOGTYPE_HOLD){
    GE_text(c, "|", wblock->tempoarea.x + window->fontwidth*3, y);
  }
}


static void create_bpmtrack(const struct Tracker_Windows *window, const struct WBlocks *wblock){

  struct WBPMs *wbpms = WBPMs_get(window, wblock);

  int realline;
  for(realline = 0 ; realline<wblock->num_reallines ; realline++)
    create_bpm(window, wblock, wbpms, realline);
}



/************************************
   reltempo track
 ************************************/

static void create_reltempotrack(const struct Tracker_Windows *window, struct WBlocks *wblock){

  const struct NodeLine *nodelines = GetTempoNodeLines(window, wblock);
  
  bool is_current = wblock->mouse_track==TEMPONODETRACK;

  drawNodeLines(window, nodelines, AUTOMATION2_COLOR_NUM, is_current, 0.6, 0.9, false);
  
  if (indicator_node != NULL || is_current) {
    const vector_t *nodes = get_nodeline_nodes(nodelines, wblock->t.y1);

    VECTOR_FOR_EACH(const Node *, node, nodes) {
        if(wblock->mouse_track==TEMPONODETRACK)
        draw_skewed_box(window, node->element, TEXT_COLOR_NUM, node->x, node->y - wblock->t.y1);
      if (node->element==indicator_node)
        draw_node_indicator(node->x, node->y - wblock->t.y1);
    }END_VECTOR_FOR_EACH;

  }
}




/************************************
    block borders
 ************************************/
void create_block_borders(
                          const struct Tracker_Windows *window,
                          const struct WBlocks *wblock
                          ){

  int y1=get_realline_y1(window, 0);
  int y2=get_realline_y2(window, wblock->num_reallines-1);

  if (!linenumbersVisible())
    create_single_linenum_border(
                                 wblock->beats_x - 1,
                                 y1,y2
                                 );

  /*  
  create_single_linenum_border(
                               wblock->beats_x - 1,
                               y1,y2
                               );
  */

  if (wblock->zoomlines_max_num_characters > 0)
    create_single_linenum_border(
                                 wblock->zoomlines_x - 1,
                                 y1,y2
                                 );
    
  create_double_border(
                       wblock->linenumarea.x2+1,
                       y1,y2
                       );

  create_double_border(
                       wblock->tempocolorarea.x2+1,
                       y1,y2
                       );

  if (window->show_signature_track)
    create_double_border(
                         wblock->signaturearea.x2+1,
                         y1,y2
                         );

  if (window->show_swing_track)
    create_double_border(
                         wblock->swingarea.x2+1,
                         y1,y2
                         );

  if (window->show_lpb_track)
    create_double_border(
                         wblock->lpbarea.x2+1,
                         y1,y2
                         );

  if (window->show_bpm_track)
    create_double_border(
                         wblock->tempoarea.x2+1,
                         y1,y2
                         );

  if (window->show_reltempo_track)
    create_double_border(
                         wblock->temponodearea.x2+1,
                         y1,y2
                         );
}



/************************************
   tracks
 ************************************/
void create_track_borders(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, int left_subtrack){
  int y1=get_realline_y1(window, 0);
  int y2=get_realline_y2(window, wblock->num_reallines-1);
  
  create_double_border(
                       wtrack->x2+1,
                       y1,
                       y2);

  if(left_subtrack==-1 && wtrack->notesonoff==1)
    create_single_border(
                         wtrack->notearea.x2+1,
                         y1,
                         y2);

  int num_subtracks = WTRACK_num_subtracks(wtrack); 
  int first_polyphony_subtrack = WTRACK_num_non_polyphonic_subtracks(wtrack);

  bool has_fx = wtrack->track->fxs.num_elements > 0;

  if (wtrack->swingtext_on){
    if (wtrack->centtext_on || wtrack->veltext_on || wtrack->chancetext_on || (wtrack->fxtext_on && has_fx))
      create_single_border(
                           wtrack->swingtextarea.x2+1,
                           y1,
                           y2);
  }
  
  if (wtrack->centtext_on){
    if (wtrack->veltext_on || wtrack->chancetext_on || (wtrack->fxtext_on && has_fx))
      create_single_border(
                           wtrack->centtextarea.x2+1,
                           y1,
                           y2);
  }
  
  if (wtrack->chancetext_on){
    if (wtrack->veltext_on || (wtrack->fxtext_on && has_fx))
      create_single_border(
                           wtrack->chancetextarea.x2+1,
                           y1,
                           y2);
  }
  
  if (wtrack->fxtext_on){
    
    if (wtrack->veltext_on && has_fx)
      create_single_border(
                           wtrack->veltextarea.x2+1,
                           y1,
                           y2);

    for(int i = 0 ; i < wtrack->track->fxs.num_elements-1 ; i++)
      create_single_border(                           
                           wtrack->fxtextarea.x + (1+i)*WTRACK_fxtrack_width(window->fontwidth) - 1,
                           y1,
                           y2);

  }
  
  for(int lokke=R_MAX(1, R_MAX(first_polyphony_subtrack, left_subtrack)) ; lokke < num_subtracks ; lokke++){
    create_single_border(
                         GetXSubTrack1(wtrack,lokke)-1,
                         y1,
                         y2);
  }

}


static const float pitch_split_color_1 = 50;
static const float pitch_split_color_2 = 95;


static GE_Rgb get_note_color(float notenum){
  notenum = R_BOUNDARIES(0,notenum,127);

  const float split1 = pitch_split_color_1;
  const float split2 = pitch_split_color_2;

  GE_Rgb rgb;

  if(notenum<split1)
    rgb = GE_mix(GE_get_rgb(VELOCITY1_COLOR_NUM), Black_rgb(), scale(notenum,0,split1,0,1000));
  else if(notenum<split2)
    rgb = GE_mix(GE_get_rgb(VELOCITY2_COLOR_NUM), GE_get_rgb(VELOCITY1_COLOR_NUM), scale(notenum,split1,split2,0,1000));
  else
    rgb = GE_mix(White_rgb(), GE_get_rgb(VELOCITY2_COLOR_NUM), scale(notenum,split2,160,0,1000));

  rgb = GE_alpha(rgb, 0.7);

  return rgb;
}

static GE_Context *get_note_background(float notenum, bool highlight, int y){

  GE_Rgb rgb = get_note_color(notenum);
    
  if (highlight)
    rgb = GE_mix(rgb, GE_get_rgb(WAVEFORM_COLOR_NUM), 650);

  return GE(rgb, y);
  //return GE_gradient(rgb, GE_get_rgb(0));
}

static float get_notenum(const TrackRealline2 &tr2){
  if (tr2.is_end_pitch)
    return tr2.note->pitch_end;
  
  else if (tr2.pitch != NULL)
    return tr2.pitch->note;

  else if (tr2.note != NULL)
    return tr2.note->note;

  else
    return NOTE_STP;
}
                         
static float get_notenum(const Trs &trs){
  if (trs.size()==0)
    return 0;

  if (trs.size() > 1)
    return NOTE_MUL;

  const TrackRealline2 &tr2 = trs[0];

  return get_notenum(tr2);
}

static int get_chance(const TrackRealline2 &tr2){
  if (tr2.is_end_pitch)
    return -1;
  
  else if (tr2.pitch != NULL)
    return tr2.pitch->chance;

  else if (tr2.note != NULL)
    return tr2.note->chance;

  else
    return -1;
}

static float get_chance(const Trs &trs){
  if (trs.size()==0)
    return -1;

  if (trs.size()>1)
    return -2;

  const TrackRealline2 &tr2 = trs[0];

  return get_chance(tr2);
}

static ColorNums get_colnum(const TrackRealline2 &tr2){
  if (tr2.pitch != NULL)
    return PORTAMENTO_NOTE_TEXT_COLOR_NUM;
  else if(tr2.is_end_pitch==true)
    return PORTAMENTO_END_NOTE_TEXT_COLOR_NUM;
  else
    return TEXT_COLOR_NUM;
}

static void paint_halfsize_note(GE_Context *c, int num, const char *notetext, int x, int y){
#if 0 // no room
  char temp[16];
  sprintf(temp,"%d. %s",num, notetext);
  GE_text_halfsize(c, temp, x, y);
#else
  GE_text_halfsize(c, notetext, x, y);
#endif
}

static void paint_tr2(const TrackRealline2 &tr2, char **NotesTexts, int num, int x, int y){
  GE_Context *c = GE_textcolor(get_colnum(tr2), y);
  float notenum = get_notenum(tr2);
  paint_halfsize_note(c, num, NotesTexts[(int)notenum], x, y);
  //GE_text_halfsize(foreground, NotesTexts[(int)notenum1], x, y);
}

static void paint_multinotes(const struct WTracks *wtrack, const Trs &trs, char **NotesTexts, int y1, int y2){
  int num_elements = trs.size();

  int x1 = wtrack->notearea.x;
  int y_middle = (y1+y2)/2;

  paint_tr2(trs[0], NotesTexts, 1, x1, y1);
  
  paint_tr2(trs[1], NotesTexts, 2, x1, y_middle);

  if (num_elements == 2)
    return;

  int x_middle = (wtrack->notearea.x2 + wtrack->notearea.x ) / 2;

  paint_tr2(trs[2], NotesTexts, 3, x_middle, y1);
  
  if (num_elements == 3)
    return;

  if (num_elements>4){
    GE_Context *c = GE_textcolor(TEXT_COLOR_NUM, y1);
    paint_halfsize_note(c, 4, NotesTexts[NOTE_MUL], x_middle, y_middle);
  } else {
    paint_tr2(trs[3], NotesTexts, 4, x_middle, y_middle);
  }
}

static void create_track_text(const struct Tracker_Windows *window, const struct WBlocks *wblock, struct WTracks *wtrack, const Trs &trs, int realline, bool show_notes){

  R_ASSERT_RETURN_IF_FALSE(!trs.empty());
  
  char **NotesTexts = wtrack->notelength==3?NotesTexts3:NotesTexts2;
  float  notenum    = get_notenum(trs); //trackrealline->note;
  
  int y1 = get_realline_y1(window, realline);
  int y2 = get_realline_y2(window, realline);

  const TrackRealline2 &tr2 = trs[0];

  enum ColorNums colnum = get_colnum(tr2);

  double cents_d = (notenum - floor(notenum))*100.0;
  int cents = R_BOUNDARIES(0,round(cents_d),99);
        
  if (wtrack->noteshowtype==TEXTTYPE && show_notes){

    // Paint THISNOTELINES
    if(trs.size() < 2 && tr2.note!=NULL && tr2.note->polyphony_num>0) {
      //printf("Gakk: %s (%s), %d, pointer: %p\n",NotesTexts[(int)notenum],NotesTexts[(int)note->note],note->subtrack,note);
      float y = (y1+y2) / 2.0f;
      float x1 = wtrack->notearea.x2;
      float x2 = (GetNoteX1(wtrack,tr2.note) + GetNoteX2(wtrack,tr2.note)) / 2.0f;
      GE_line(GE_color(TEXT_COLOR_NUM,y1),
              x1, y,
              x2, y,
              0.8);
    }

    bool highlight;
    
    if (tr2.pitch != NULL && &tr2.pitch->l==current_node)
      highlight = true;
    else if (tr2.note != NULL && &tr2.note->l==current_node)
      highlight = true;
    else
      highlight = false;
      
    //printf("highlight: %d %p %p\n",highlight,trackrealline->daspitch,trackrealline->dasnote);
    //printf("current_node: %p\n\n",current_node);
      

    if(notenum>0 && notenum<128)
      GE_filledBox(get_note_background(notenum, highlight, y1), wtrack->notearea.x, y1, wtrack->notearea.x2, y2);
      
    if (trs.size() > 1)
      paint_multinotes(wtrack, trs, NotesTexts, y1, y2);

    else if ((g_is_creating_all_GL_blocks==false && wblock->mouse_track == wtrack->l.num)
             || cents!=0
             || wtrack->is_wide==true
             ){
      GE_Context *foreground = GE_textcolor_z(colnum,Z_ABOVE(Z_ZERO), y1);
        
      if (cents==0 || wtrack->centtext_on==true)
        GE_text(foreground, NotesTexts[(int)notenum], wtrack->notearea.x, y1); 
      else{
        char temp[32];
        if (wtrack->is_wide)
          sprintf(temp,"%s.%d",NotesTexts[(int)notenum],cents);
        else
          sprintf(temp,"%s %d",NotesTexts[(int)notenum],cents);
        GE_text(foreground, temp, wtrack->notearea.x, y1); 
      }
      
      //GE_text(foreground, NotesTexts[(int)notenum], wtrack->notearea.x, y1);
        
    }else
      draw_bordered_text(window, GE_textcolor_z(colnum, Z_ZERO, y1), NotesTexts[(int)notenum], wtrack->notearea.x, y1);
  }

  if (wtrack->centtext_on) {

    GE_Context *foreground = GE_textcolor_z(colnum,Z_ABOVE(Z_ZERO),y1);

    if (cents != 0){
      char centtext[16];
      sprintf(centtext,"%s%d",cents<10?" ":"",cents); // Never remembers the short syntax for this.
      GE_text(foreground, centtext, wtrack->centtextarea.x, y1);
    }
  }

  if (wtrack->centtext_on==false && cents_d != 0.0 && WTRACK_num_non_polyphonic_subtracks(wtrack)>0 && wtrack->notesonoff==1){
    //printf("     %d: cents_d: %f\n",wtrack->l.num, cents_d);
    wtrack->centtext_on = true;
    GFX_ScheduleCalculateCoordinates();
  }

  if (wtrack->chancetext_on) {

    int chance  = get_chance(trs);
            
    if (chance!=-1 && chance != 0x100){
      GE_Context *foreground = GE_textcolor_z(AUTOMATION2_COLOR_NUM,Z_ABOVE(Z_ZERO),y1);

      char chancetext[16];
      if (chance==-2)
        sprintf(chancetext, "xx");
      else
        sprintf(chancetext,"%s%x",chance<0x10?" ":"",chance); // Never remembers the short syntax for this.
        
      GE_text(foreground, chancetext, wtrack->chancetextarea.x, y1);
    }
  }

}

static float next_line_y1(const struct Tracker_Windows *window, float y){
  float y1 = ceil(y / window->fontheight);
  float ret = y1*window->fontheight;
  
  if (fabsf(ret - y) < 0.001)
    return ret+window->fontheight;
  else
    return ret;
}

static void create_pitches(const struct Tracker_Windows *window, const struct WBlocks *wblock, struct WTracks *wtrack, const struct Notes *note){

  const struct NodeLine *nodelines = GetPitchNodeLines(window,
                                                       wblock,
                                                       wtrack,
                                                       note
                                                       );

  // lines
  {

    GE_Context *line_color = NULL;

    for(const struct NodeLine *nodeline=nodelines ; nodeline!=NULL ; nodeline=nodeline->next) {
      bool vertical_line = nodeline->x1==nodeline->x2;
      bool continues_next_block = nodeline->next==NULL && note_continues_next_block(wblock->block, note);
    
      if(true || !vertical_line) {
        int logtype = nodeline->logtype;
        
        float x1 = nodeline->x1;
        float x2 = logtype==LOGTYPE_HOLD ? nodeline->x1 : nodeline->x2;
        float y1 = nodeline->y1;
        float y2 = nodeline->y2;

        if (vertical_line)
          y1 = next_line_y1(window, y1);

        line_color = line_color!=NULL ? GE_y(line_color, y1) : GE_color_alpha(PITCH_LINE_COLOR_NUM, 0.5, y1);  

        int width = get_pitchline_width();
        //if (vertical_line)
        width *= 2;
        
        if (y2 > y1 + 2){
          GE_line(line_color, x1, y1, x2, y2, width);
          
          if (logtype==LOGTYPE_HOLD)
            GE_line(line_color, x2, y2, nodeline->x2, y2, width);
        }
          
        if (continues_next_block){
          float y1 = nodeline->y2;
          float y2 = nodeline->y2 + 25;
          GE_line(line_color, nodeline->x2, y1, nodeline->x2, y2, width);
          GE_line(line_color, nodeline->x2 - 5, y2 - 8, nodeline->x2, y2, width);
          GE_line(line_color, nodeline->x2 + 5, y2 - 8, nodeline->x2, y2, width);
        }
      }

    }

  }
  
  // indicator node
  if (indicator_node == &note->l && indicator_pitch_num!=-1) {
    const vector_t *nodes = get_nodeline_nodes(nodelines, wblock->t.y1);
    
    if (indicator_pitch_num < nodes->num_elements) {
      //printf("indicator_pitch_num: %d\n",indicator_pitch_num);
      struct Node *node = (struct Node *)nodes->elements[indicator_pitch_num];
      draw_node_indicator(node->x, node->y-wblock->t.y1);
    }    
  }
}


static void create_pianoroll_grid(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack){
  int y1=get_realline_y1(window, 0);
  int y2=get_realline_y2(window, wblock->num_reallines-1);

  GE_Context *octave_color = GE_color_alpha(PIANOROLL_OCTAVE_COLOR_NUM, 0.25, NOMASK_Y);
  //GE_Context *note_color = GE_color_alpha(8, 0.5);

  GE_Context *white_key_color = GE_color_alpha(WHITE_COLOR_NUM, 0.1, NOMASK_Y);
  GE_Context *black_key_color = GE_color_alpha(BLACK_COLOR_NUM, 0.1, NOMASK_Y);

  float gfx_width  = wtrack->pianoroll_area.x2 - wtrack->pianoroll_area.x;
  float notespan   = wtrack->pianoroll_highkey - wtrack->pianoroll_lowkey;
  float note_width = gfx_width / notespan;

  float x = wtrack->pianoroll_area.x;

  for(int key=wtrack->pianoroll_lowkey ; key<wtrack->pianoroll_highkey ; key++){
    int chroma = key % 12;

    if (chroma==0)
      GE_line(octave_color,x,y1,x,y2,1.0);

    float key_x1 = x;
    float key_x2 = x + note_width;

    GE_Context *key_color;
    
    if (chroma==0 || chroma==2 || chroma==4 || chroma==5 || chroma==7 || chroma==9 || chroma==11)
      key_color = white_key_color;
    else
      key_color = black_key_color;

    if (key_color==white_key_color)
      GE_filledBox(key_color,
                   key_x1, y1,
                   key_x2, y2
                   );

    x = key_x2;
  }
}

struct CurrentPianoNote current_piano_note = {-1,-1,-1};

static void draw_pianonote_text(const struct Tracker_Windows *window, float notenum, bool is_current, float midpos, float y){
  
  int cents = R_BOUNDARIES(0,round((notenum - (int)notenum)*100.0),99);
  
  char *text = NotesTexts3[(int)notenum];
  char temp[32];
  
  if (cents!=0){
    sprintf(temp,"%s.%d",NotesTexts3[(int)notenum],cents);
    text = &temp[0];
  }
  
  float textgfxlength = strlen(text) * (is_current ? window->fontwidth : window->fontwidth/2);
  float x = midpos - (textgfxlength / 2.0);
  
  y -= (is_current ? window->fontheight : window->fontheight/2) + 2;
  
  GE_Context *c = GE_color_alpha_z(PIANOROLL_NOTE_NAME_COLOR_NUM, 0.7, Z_ABOVE(Z_ZERO), y);
  
  if (is_current)
    GE_text(c,text,x,y);
  else
    paint_halfsize_note(c,0,text,x,y);
}

static void create_pianoroll(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack){
  //int x1 = 100;
  //int x2 = 200;

  //float note_width = (x2-x1) / 12.0;
  //float half_note_width = 2; //note_width / 2;

  if (wtrack->pianoroll_on==false)
    return;

  if (wtrack->pianoroll_area.x < wblock->t.x1)
    return;
  
  float gfx_width  = wtrack->pianoroll_area.x2 - wtrack->pianoroll_area.x;
  float notespan   = wtrack->pianoroll_highkey - wtrack->pianoroll_lowkey;
  float note_width = gfx_width / notespan;

  create_pianoroll_grid(window, wblock, wtrack);

  enum ColorNums colornum = PIANONOTE_COLOR_NUM;
  
  GE_set_x_scissor(wtrack->pianoroll_area.x,
                   wtrack->pianoroll_area.x2+1);

  GE_Context *border_color = NULL;
  GE_Context *current_note_color = NULL;
  GE_Context *note_color = NULL;
  
  int notenum = 0;
  const struct Notes *note=wtrack->track->notes;
  while(note != NULL){
    const struct NodeLine *nodelines = GetPianorollNodeLines(window,
                                                             wblock,
                                                             wtrack,
                                                             note
                                                             );

    int pianonotenum = -1;
    
    for(const struct NodeLine *nodeline=nodelines ; nodeline!=NULL ; nodeline=nodeline->next) {

      bool is_continuing = false;
      
      if (nodeline->next==NULL)
        if (note_continues_next_block(wblock->block, note))
          is_continuing = true;

      if (nodeline->is_node)
        pianonotenum++;      
 
      int logtype = nodeline->logtype;
      float x1 = nodeline->x1;
      float x2 = logtype==LOGTYPE_HOLD ? nodeline->x1 : nodeline->x2;
      float y1 = nodeline->y1;
      float y2 = nodeline->y2;
 
      border_color = border_color!=NULL ? GE_y(border_color, y1) : GE_color_alpha_z(PIANOROLL_NOTE_BORDER_COLOR_NUM, 0.7, Z_ABOVE(Z_ZERO), y1);
 

      bool is_current = wtrack->l.num==current_piano_note.tracknum && notenum==current_piano_note.notenum && pianonotenum==current_piano_note.pianonotenum;

      //printf("pianonotenum: %d, curr.pianonotenum: %d, is_current: %s\n",pianonotenum,current_piano_note.pianonotenum,is_current?"true":"false");

      GE_Context *c;

      if (is_current) {
        
        current_note_color = current_note_color != NULL ? GE_y(current_note_color, y1) : GE_mix_color(GE_get_rgb(colornum), White_rgb(), 500, y1);
        c = current_note_color;
        GE_unset_x_scissor();
        
      } else {
        if (note_color != NULL) {
          note_color = GE_y(note_color, y1);
        } else {
          if (wtrack->track->patch==NULL)
            note_color = GE_color_alpha(colornum, 0.4, y1);
          else
            note_color = GE_mix_color(GE_get_rgb(wtrack->track->patch->color), GE_get_rgb(colornum), 400, y1);
        }
        c = note_color;
      }

      GE_line(c,
              x1, y1,
              x2, y2,
              note_width
              );

      //float x_min = R_MIN(nodeline->x1, nodeline->x2);
      //float x_max = R_MAX(nodeline->x1, nodeline->x2);
      
      //float box_x = x_min-note_width/2.0;

#if 0
      if (box_x < wtrack->pianoroll_area.x2)
        GE_box(border_color,
               box_x, nodeline->y1,
               x_max+note_width/2.0, nodeline->y2,
               1.0
               );
#endif

      if (nodeline->is_node) {          

        const NodelineBox nodelineBox = GetPianoNoteBox(wtrack, nodeline);

        bool is_inside = false;
        
        if (nodelineBox.x1 >= wtrack->pianoroll_area.x && nodelineBox.x1 < wtrack->pianoroll_area.x2)
          is_inside = true;
        else if (nodelineBox.x2 >= wtrack->pianoroll_area.x && nodelineBox.x2 < wtrack->pianoroll_area.x2)
          is_inside = true;
        
        if (is_inside || is_current) {

          if (!is_continuing)
            GE_box(border_color,
                   nodelineBox.x1, nodelineBox.y1,
                   nodelineBox.x2, nodelineBox.y2,
                   1.0
                   );
          else { 
            // box (without the x1,y2 -> x2,y2 line)
            GE_line(border_color,
                    nodelineBox.x1,nodelineBox.y1,
                    nodelineBox.x2,nodelineBox.y1,
                    1.0);
            GE_line(border_color,
                    nodelineBox.x2,nodelineBox.y1,
                    nodelineBox.x2,nodelineBox.y2,
                    1.0);
            GE_line(border_color,
                    nodelineBox.x1,nodelineBox.y1,
                    nodelineBox.x1,nodelineBox.y2,
                    1.0);
          }
          
          struct Pitches *pitch = (struct Pitches*)nodeline->element1;
          float  notenum = pitch->note;

          float x_text = (nodeline->x1 + nodeline->x2) / 2;
          bool text_is_inside = (x_text > wtrack->pianoroll_area.x - 20) && (x_text < wtrack->pianoroll_area.x2 + 20);
            
          if (text_is_inside || is_current) {
            draw_pianonote_text(window, notenum, is_current, x_text, nodeline->y1);

            // pitch_end
            if (nodeline->next==NULL && note->pitch_end > 0) {
              struct Pitches *pitch = (struct Pitches*)nodeline->element2;
              float  notenum = pitch->note;
              draw_pianonote_text(window, notenum, is_current, (nodeline->x1 + nodeline->x2) / 2, nodeline->y2);
            }
          }

          if (is_continuing){

            float midpos = nodeline->x2;

            float dx = 5;
            
            float x1 = midpos - note_width/2; //nodelineBox.x1 - dx;
            float dx1 = x1 - dx;
            
            float x =  midpos; //nodeline->x2;
            
            float x2 = midpos + note_width/2; //nodelineBox.x2 + dx;
            float dx2 = x2 + dx;

            float y0 = nodeline->y2;
            float y1 = nodeline->y2 + 15;
            float y2 = nodeline->y2 + 25;
          
            GE_filledBox(c,
                         x1, y0,
                         x2, y1
                         );
            
            GE_trianglestrip_start();

            GE_trianglestrip_add(c,dx1,y1);
            GE_trianglestrip_add(c,dx2,y1);
            GE_trianglestrip_add(c,x,y2);
            GE_trianglestrip_end(c);

            // Connecting lines between the two objects
            GE_line(border_color,
                    x1, y0,
                    nodelineBox.x1, y0,
                    1.0);            
            GE_line(border_color,
                    x2, y0,
                    nodelineBox.x2, y0,
                    1.0);

            GE_line(border_color,
                    x1, y0,
                    x1, y1,
                    1.0);
            GE_line(border_color,
                    x2, y0,
                    x2, y1,
                    1.0);
            
            
            // arrow
            GE_line(border_color,
                    dx1,y1,
                    x1,y1,
                    1.0);
            GE_line(border_color,
                    x2,y1,
                    dx2,y1,
                    1.0);
            GE_line(border_color,
                    dx1,y1,
                    x,y2,
                    1.0);
            GE_line(border_color,
                    dx2,y1,
                    x,y2,
                    1.0);
          }
        
        }

      }

      if (is_current)
        GE_set_x_scissor(wtrack->pianoroll_area.x,
                         wtrack->pianoroll_area.x2+1);
    }

    notenum++;
    note = NextNote(note);
  }

  GE_unset_x_scissor();
}



static float subtrack_x1, subtrack_x2;

static void create_track_peaks(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const struct Notes *note, const struct NodeLine *nodelines){
  struct Patch *patch = wtrack->track->patch;
  STime note_time = Place2STime(wblock->block, &note->l.p);

  float track_volume =  wtrack->track->volumeonoff ? (float)wtrack->track->volume / MAXTRACKVOL : 1.0f;
  //float velocity = scale(n,0,num_peaks, velocity1->velocity, velocity2->velocity);

  const double reltempo = ATOMIC_DOUBLE_GET(wblock->block->reltempo);
    
  const int num_channels=PATCH_get_peaks(patch, 0,
                                         -1,
                                         wtrack->track,
                                         0,0,
                                         NULL,NULL
                                         );
    

  
  GE_Context *c = NULL;
  
  //GE_Context *c = Black_color(); //GE_mix_alpha_z(GE_get_rgb(0), Black_rgb(), 100, 0.7, Z_ZERO);
  //GE_Context *c = GE_mix_alpha_z(GE_get_rgb(0), GE_get_rgb(2), 250, 0.9, Z_ZERO);

#define NUM_LINES_PER_PEAK 1

  for(int ch=0;ch<num_channels;ch++){

    GE_Context *last_c = NULL;
    
    GE_trianglestrip_start();

    for(const struct NodeLine *ns = nodelines ; ns!=NULL ; ns=ns->next){
      int logtype = ns->logtype;
      float x1 = ns->x1;
      float x2 = logtype==LOGTYPE_HOLD ? ns->x1 : ns->x2;
      float y1 = ns->y1;
      float y2 = ns->y2;

      c = c!=NULL ? GE_y(c, y1) : GE_mix_color_z(GE_get_rgb(LOW_EDITOR_BACKGROUND_COLOR_NUM), GE_get_rgb(WAVEFORM_COLOR_NUM), 100, Z_ABOVE(Z_ZERO), y1);

      if (c != last_c){
        if (last_c != NULL) 
          GE_trianglestrip_end(last_c);

        GE_trianglestrip_start();

        last_c = c;
      }
      
      const STime time1 = Place2STime(wblock->block, &ns->element1->p) - note_time;
      const STime time2 = Place2STime(wblock->block, &ns->element2->p) - note_time;

      if (time1==time2)
        continue;
          
      if (time2 < time1){
#if !defined(RELEASE)
        abort();
#endif
        continue;
      }
          
      float velocity1 = scale(x1, subtrack_x1, subtrack_x2, 0, 1);
      float velocity2 = scale(x2, subtrack_x1, subtrack_x2, 0, 1);
      
      int num_peaks = R_MAX(1, (y2-y1) / NUM_LINES_PER_PEAK);
      
      if(num_peaks<0){
        
        RWarning("num_peaks<0: %d",num_peaks);
        continue;
        
      }

      for(int n=0;;n+=NUM_LINES_PER_PEAK){

        float min,max;

        int64_t start_time = scale(n,
                                   0,num_peaks,
                                   time1,time2
                                   );

        int64_t end_time   = scale(R_MIN(num_peaks-1, n+NUM_LINES_PER_PEAK),
                                   0,num_peaks,
                                   time1,time2
                                   );

        if (start_time>=end_time)
          break;
        
        //if (n==0)
        //printf("start_time: %d, time1: %d. end_time: %d, time2: %d\n",(int)start_time, (int)time1, (int)end_time, (int)time2);
        

        PATCH_get_peaks(patch, 
                        note->note,
                        ch,
                        wtrack->track,
                        start_time / reltempo,
                        end_time / reltempo,
                        &min,
                        &max);

        float velocity = (float)scale(n,0,num_peaks,velocity1, velocity2);

        float bound_x1 = scale(scale(ch,0,num_channels,0.0f,velocity),
                               0, 1,
                               subtrack_x1, subtrack_x2);
        float bound_x2 = scale(scale(ch+1,0,num_channels,0.0f,velocity),
                               0, 1,
                               subtrack_x1, subtrack_x2);

        float x1 = scale(min*track_volume, -1,1, bound_x1, bound_x2);
        float x2 = scale(max*track_volume, -1,1, bound_x1, bound_x2);
          
        float y = y1 + n*NUM_LINES_PER_PEAK;

#if 0
        printf("Adding %f,%f at %f. min/max: %f/%f. vel1/vel2: %f/%f. time1/time2: %f/%f\n",x1,x2,y,min,max,
               scale(n,0,num_peaks,velocity1->velocity, velocity2->velocity),
               scale(n+NUM_LINES_PER_PEAK,0,num_peaks,velocity1->velocity, velocity2->velocity),
               scale(n,0,num_peaks,time1,time2) / reltempo,
               scale(n+NUM_LINES_PER_PEAK,0,num_peaks,time1,time2) / reltempo);
#endif

        if(fabsf(x1-x2) < 0.5) {
          GE_trianglestrip_end(c);
          float x = (x1+x2)/2.0f;
          GE_line(c, x, y, x, y+NUM_LINES_PER_PEAK, 1.0);
          GE_trianglestrip_start();
        }else{
          GE_trianglestrip_add(c, x1, y);
          GE_trianglestrip_add(c, x2, y);
        }

      } // end num peaks iteration

    } // end node iteration

    if (last_c != NULL)
      GE_trianglestrip_end(last_c);

  } // end ch iteration
}

static void create_velocity_gradient_background(
                                                float area_y1,
                                                float area_y2,
                                                float start_note,
                                                float end_note,
                                                const struct NodeLine *velocity_nodelines
                                                )
{

  GE_Rgb rgb1 = get_note_color(start_note);
  GE_Rgb rgb2 = get_note_color(end_note);

  GE_Context *c = NULL;

  const struct NodeLine *nodeline = velocity_nodelines;
  while (nodeline != NULL){
    float vel_y1 = nodeline->y1;
    float vel_y2 = nodeline->y2;

    if (vel_y1>=area_y2)
      break;

    bool is_inside = vel_y1>=area_y1 || vel_y2>=area_y1;

    if (is_inside && vel_y1!=vel_y2){

      int logtype = nodeline->logtype;
      float x1 = nodeline->x1;
      float x2 = logtype==LOGTYPE_HOLD ? nodeline->x1 : nodeline->x2;
          
      float y1 = R_BOUNDARIES(area_y1, nodeline->y1, area_y2);
      float y2 = R_BOUNDARIES(area_y1, nodeline->y2, area_y2);

      float x1b = scale(y1, nodeline->y1, nodeline->y2, x1, x2);
      float x2b = scale(y2, nodeline->y1, nodeline->y2, x1, x2);

      if (c!=NULL)
        c = GE_y(c, y1);
      else {
        c = GE_gradient_z(rgb1, rgb2, Z_BELOW(Z_ZERO), y1);
        GE_gradient_triangle_start(GradientType::VELOCITY);
      }
      
      GE_gradient_triangle_add(c, subtrack_x1, y1);
      GE_gradient_triangle_add(c, x1b,         y1);
      GE_gradient_triangle_add(c, subtrack_x1, y2);
      GE_gradient_triangle_add(c, x2b,         y2);
    }
    
    nodeline = nodeline->next;
  }

  if (c != NULL)
    GE_gradient_triangle_end(c, subtrack_x1, subtrack_x2);
}


static float track_notearea_x1, track_notearea_x2;
static float track_pitch_min;
static float track_pitch_max;

static void create_velocities_gradient_background(
                                                  const struct NodeLine *pitch_nodelines,
                                                  const struct NodeLine *velocity_nodelines
                                                  )
{
  const struct NodeLine *nodeline = pitch_nodelines;

  while(nodeline != NULL){
    int logtype = nodeline->logtype;
    float x1 = nodeline->x1;
    float x2 = logtype==LOGTYPE_HOLD ? nodeline->x1 : nodeline->x2;

    float y1 = nodeline->y1;
    float y2 = nodeline->y2;

    float start_note;
    float end_note;

    if (track_notearea_x1==track_notearea_x2){
      start_note = scale(x1, 0, 1, track_pitch_min, track_pitch_max);
      end_note   = scale(x2, 0, 1, track_pitch_min, track_pitch_max);
    } else {
      start_note = scale(x1, track_notearea_x1, track_notearea_x2, track_pitch_min, track_pitch_max);
      end_note   = scale(x2, track_notearea_x1, track_notearea_x2, track_pitch_min, track_pitch_max);
    }

    float split1 = pitch_split_color_1;
    float split2 = pitch_split_color_2;

    if (start_note < end_note) {

      if (start_note<split1 && end_note>split1) {
        float y_split1 = scale(split1, start_note, end_note, y1, y2);
        create_velocity_gradient_background(y1, y_split1, start_note, split1, velocity_nodelines);
        
        y1 = y_split1;
        start_note = split1;
      }
      
      if (start_note<split2 && end_note>split2){
        float y_split2 = scale(split2, start_note, end_note, y1, y2);
        
        create_velocity_gradient_background(y1, y_split2, start_note, split2, velocity_nodelines);
        
        y1 = y_split2;
        start_note = split2;
      }

    } else  if (end_note < start_note) {

      if (start_note>split2 && end_note<split2) {
        float y_split2 = scale(split2, start_note, end_note, y1, y2);
        create_velocity_gradient_background(y1, y_split2, start_note, split2, velocity_nodelines);
        
        y1 = y_split2;
        start_note = split2;
      }
      
      if (start_note>split1 && end_note<split1){
        float y_split1 = scale(split1, start_note, end_note, y1, y2);
        
        create_velocity_gradient_background(y1, y_split1, start_note, split1, velocity_nodelines);
        
        y1 = y_split1;
        start_note = split1;
      }
  
    } 

    create_velocity_gradient_background(y1, y2, start_note, end_note, velocity_nodelines);


    nodeline = nodeline->next;
  }
}


static void create_track_velocities(const struct Tracker_Windows *window, const struct WBlocks *wblock, struct WTracks *wtrack, const struct Notes *note){

  //printf("Note: %s, pointer: %p, subtrack: %d\n",NotesTexts3[(int)note->note],note,note->subtrack);
  subtrack_x1 = GetNoteX1(wtrack,note);
  subtrack_x2 = GetNoteX2(wtrack,note);

  if(subtrack_x1==subtrack_x2)
    return;
  
  const struct NodeLine *nodelines = GetVelocityNodeLines(window, wblock, wtrack, note);
  const vector_t *nodes = get_nodeline_nodes(nodelines, wblock->t.y1);

  // background
  {

    const bool paint_vertical_velocity_gradient = false;
    //const bool paint_vertical_velocity_gradient = true;

    if(paint_vertical_velocity_gradient==false && note->pitches==NULL){

      GE_Context *last_c = NULL;      
      
      for(const struct NodeLine *ns = nodelines ; ns!=NULL ; ns=ns->next){
        int logtype = ns->logtype;
        float x1 = ns->x1;
        float x2 = logtype==LOGTYPE_HOLD ? ns->x1 : ns->x2;

        GE_Context *c = get_note_background(note->note, false, ns->y1);

        if (c != last_c) {
          if (last_c != NULL){
            GE_trianglestrip_end(last_c);
          }
          
          GE_trianglestrip_start();

          last_c = c;
        }
        
        GE_trianglestrip_add(c, subtrack_x1, ns->y1);
        GE_trianglestrip_add(c, x1, ns->y1);
        GE_trianglestrip_add(c, subtrack_x1, ns->y2);
        GE_trianglestrip_add(c, x2, ns->y2);
      }

      if (last_c != NULL)
        GE_trianglestrip_end(last_c);

    }else{
      TRACK_get_min_and_max_pitches(wtrack->track, &track_pitch_min, &track_pitch_max);
      track_notearea_x1 = wtrack->notearea.x;
      track_notearea_x2 = wtrack->notearea.x2;

      const struct NodeLine *pitch_nodelines = GetPitchNodeLines(window, wblock, wtrack, note);

      create_velocities_gradient_background(
                                            pitch_nodelines,
                                            nodelines
                                          );
    }
  }

  
  bool is_current = wblock->mouse_note==note;

  // border
  {

    GE_Context *c2 = drawNodeLines(window, nodelines, BLACK_COLOR_NUM, is_current, 0.3, 0.6, false);

    // draw horizontal line where note starts, if it doesn't start on the start of a realline.
    int realline = FindRealLineForNote(wblock, 0, note);
    if (PlaceNotEqual(&wblock->reallines[realline]->l.p, &note->l.p)) {
      GE_Context *c = GE_y(c2, nodelines->y1);
      GE_line(c, subtrack_x1, nodelines->y1, nodelines->x1, nodelines->y1, get_nodeline_width(is_current));
    }
  }

  // peaks
  if(TRACK_has_peaks(wtrack->track))
    create_track_peaks(window, wblock, wtrack, note, nodelines);

  // nodes
  if (is_current)
    VECTOR_FOR_EACH(Node *, node, nodes){
      draw_skewed_box(window, node->element, VELOCITY1_COLOR_NUM, node->x, node->y - wblock->t.y1);
    }END_VECTOR_FOR_EACH;

  if (indicator_node == &note->l && indicator_velocity_num!=-1) {
    if (indicator_velocity_num >= nodes->num_elements)
      //RError("indicator_velocity_node_num(%d) >= nodes->num_elements(%d)",indicator_velocity_num,nodes->num_elements);
      printf("indicator_velocity_node_num(%d) >= nodes->num_elements(%d)\n",indicator_velocity_num,nodes->num_elements); // TODO: Find out why this happens so often.
    else {
      struct Node *node = (struct Node *)nodes->elements[indicator_velocity_num];
      draw_node_indicator(node->x, node->y-wblock->t.y1);
    }
  }
}


static void create_track_fxs(const struct Tracker_Windows *window, const struct WBlocks *wblock, struct WTracks *wtrack, const struct FXs *fxs){
  const struct NodeLine *nodelines = GetFxNodeLines(window, wblock, wtrack, fxs);

  bool is_current = wblock->mouse_track==wtrack->l.num && wblock->mouse_fxs==fxs;

  drawNodeLines(window, nodelines, fxs->fx->color, is_current, 0.6, 1.0, true);
  
  if (indicator_node != NULL || is_current) {
    const vector_t *nodes = get_nodeline_nodes(nodelines, wblock->t.y1);

    VECTOR_FOR_EACH(const Node *, node, nodes){
      if (wblock->mouse_track==wtrack->l.num && wblock->mouse_fxs==fxs)
        draw_skewed_box(window, node->element, TEXT_COLOR_NUM, node->x, node->y - wblock->t.y1);
      if (node->element==indicator_node)
        draw_node_indicator(node->x, node->y - wblock->t.y1);
    }END_VECTOR_FOR_EACH;

  }
}

static void create_track_stops(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack){
  struct Stops *stops = wtrack->track->stops;

  float reallineF = 0.0f;

  while(stops != NULL){
    reallineF = FindReallineForF(wblock, reallineF, &stops->l.p);
    float y = get_realline_y(window, reallineF); 
    GE_Context *c = GE_color_alpha(TEXT_COLOR_NUM, 0.19, y);
    GE_line(c,
            wtrack->notearea.x, y,
            wtrack->x2, y,
            1.2
            );
    stops = NextStop(stops);
  }
}

static void create_track_is_recording(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack){
  GE_Context *c = GE_z(GE_alpha(GE_get_rgb(RED_COLOR_NUM), 0.7), Z_STATIC, NOMASK_Y);

  GE_text(c, "Rec", wtrack->x, 0);
}


static void create_track_veltext2(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, int realline, char v1, char v2, char v3){

  char text[]={v1, v2, v3, '\0'};

  float x = wtrack->veltextarea.x;
  int y1 = get_realline_y1(window, realline);

  GE_Context *c = GE_textcolor(VELOCITY_TEXT_COLOR_NUM, y1);
  
  GE_text(c, text, x, y1);
}
  
static void create_track_veltext(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const VelText_trs &trs, int realline){
  int num_elements = trs.size();
  
  if (num_elements == 0)
    return;

  if (num_elements > 1){
    create_track_veltext2(window, wblock, wtrack, realline, 'x', 'x', 'x');
    return;
  }

  const VelText &vt = trs[0];
  char v1,v2,v3;

  if (vt.value < 0x10)
    v1 = ' ';
  else
    v1 = "0123456789abcdef" [vt.value / 0x10];
  

  v2 = "0123456789abcdef" [vt.value % 0x10];

  if (vt.logtype==LOGTYPE_HOLD)
    v3 = '|';
  else
    v3 = ' ';

  create_track_veltext2(window, wblock, wtrack, realline, v1, v2, v3);
}


static void create_track_fxtext2(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, int realline, ColorNums colornum, int column, char v1, char v2, char v3){

  char text[]={v1, v2, v3, '\0'};

  float x = wtrack->fxtextarea.x + (column * WTRACK_fxtrack_width(window->fontwidth));
  int y1 = get_realline_y1(window, realline);

  GE_Context *c = GE_textcolor(colornum, y1);
  
  GE_text(c, text, x, y1);
}

static void create_track_fxtext(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const FXText_trs &trs, int realline, int column){
  int num_elements = trs.size();

  if (num_elements == 0)
    return;

  const FXText &vt = trs[0];

  if (num_elements > 1){
    create_track_fxtext2(window, wblock, wtrack, realline, vt.fx->color, column, 'x', 'x', 'x');
    return;
  }

  char v1,v2,v3;

  if (vt.value < 0x10)
    v1 = ' ';
  else
    v1 = "0123456789abcdef" [vt.value / 0x10];
  

  v2 = "0123456789abcdef" [vt.value % 0x10];

  if (vt.logtype==LOGTYPE_HOLD)
    v3 = '|';
  else
    v3 = ' ';

  create_track_fxtext2(window, wblock, wtrack, realline, vt.fx->color, column, v1, v2, v3);
}

static void create_track(const struct Tracker_Windows *window, const struct WBlocks *wblock, struct WTracks *wtrack, int left_subtrack){
  create_track_borders(window, wblock, wtrack, left_subtrack);

  SetNotePolyphonyAttributes(wtrack->track);

  // velocities and pitches
  {  
    const struct Notes *note=wtrack->track->notes;
    while(note != NULL){
      if(NOTE_subtrack(wtrack, note) >= left_subtrack) {        
        if (left_subtrack==-1 && wtrack->notesonoff==1)
          create_pitches(window, wblock, wtrack, note);
        create_track_velocities(window, wblock, wtrack, note);
      }
      note = NextNote(note);
    }
  }

  // note/pitch names / cents
  if( (left_subtrack==-1 && wtrack->notesonoff==1) || wtrack->centtext_on) {

    bool show_notes = (left_subtrack==-1 && wtrack->notesonoff==1);
    
    const Trss &trss = TRSS_get(wblock, wtrack);

    auto i = trss.constBegin();
    while (i != trss.constEnd()) {
      create_track_text(window, wblock, wtrack, i.value(), i.key(), show_notes);
      ++i;
    }
  }

  if (wtrack->swingtext_on){
    create_swingtrack(window, wblock, wtrack->track->filledout_swings.array, wtrack->swingtextarea.x);
  }
  
  // velocity text
  if (wtrack->veltext_on){
    const VelText_trss &veltexts = VELTEXTS_get(wblock, wtrack);

    auto i = veltexts.constBegin();
    while (i != veltexts.constEnd()){
      create_track_veltext(window, wblock, wtrack, i.value(), i.key());
      ++i;
    }

  }

  // fx text
  if (wtrack->fxtext_on){
    int column = 0;
    VECTOR_FOR_EACH(const struct FXs *, fxs, &wtrack->track->fxs){
      const FXText_trss &fxtexts = FXTEXTS_get(wblock, wtrack, fxs);
      auto i = fxtexts.constBegin();
      while (i != fxtexts.constEnd()){
        create_track_fxtext(window, wblock, wtrack, i.value(), i.key(), column);
        ++i;
      }
      column++;
    }END_VECTOR_FOR_EACH;

  }
  
  // fxs
  if(left_subtrack<=0){
    VECTOR_FOR_EACH(const struct FXs *, fxs, &wtrack->track->fxs){
      create_track_fxs(window, wblock, wtrack, fxs);
    }END_VECTOR_FOR_EACH;
  }

  // stop lines
  create_track_stops(window, wblock, wtrack);

  // piano roll
  create_pianoroll(window, wblock, wtrack);

  // rec.
  if (ATOMIC_GET(wtrack->track->is_recording))
    create_track_is_recording(window, wblock, wtrack);
}


static void create_tracks(const struct Tracker_Windows *window, const struct WBlocks *wblock){
  struct WTracks *wtrack=(struct WTracks*)ListFindElement1(&wblock->wtracks->l,wblock->left_track);

  while(wtrack!=NULL && wtrack->x <= wblock->a.x2){// && wtrack->l.num<=wblock->right_track){ <- Using right_track isn't quite correct since piano-roll is to the left of the cursor.
    //printf("drawing track %d\n", wtrack->l.num);
    int left_subtrack = wtrack->l.num==wblock->left_track ? wblock->left_subtrack : -1;
    create_track(window, wblock, wtrack, left_subtrack);
    wtrack=NextWTrack(wtrack);
  }
}

static void create_range(const struct Tracker_Windows *window, const struct WBlocks *wblock){

  if (!wblock->isranged)
    return;
  
  int tracknum1 = R_MIN(wblock->block->num_tracks-1, wblock->rangex1);
  int tracknum2 = R_MIN(wblock->block->num_tracks-1, wblock->rangex2);

  struct WTracks *wtrack1=(struct WTracks*)ListFindElement1(&wblock->wtracks->l,tracknum1);
  struct WTracks *wtrack2=(struct WTracks*)ListFindElement1(&wblock->wtracks->l,tracknum2);

  //const struct LocalZooms *localzoom1 = wblock->reallines[realline1];
  //const struct LocalZooms *localzoom2 = wblock->reallines[realline2];

  float x1 = wtrack1->x;
  float x2 = wtrack2->x2;

  float realline1 = FindReallineForF(wblock, 0, &wblock->rangey1);
  float realline2 = FindReallineForF(wblock, realline1, &wblock->rangey2);
  int y1 = get_realline_y(window, realline1);
  int y2 = get_realline_y(window, realline2)-1;

  //printf("realline1: %f, realline2: %f, y1: %d, y2: %d\n", realline1, realline2, y1, y2);
  
  GE_Rgb rgb = GE_get_rgb(RANGE_COLOR_NUM);
  if (rgb.a==0xff)
    rgb.a = 0x80;
  
  GE_Context *c = GE_z(rgb, Z_MAX_SCROLLTRANSFORM, y1);

  GE_filledBox(c, //GE_mix_alpha_z(GE_get_rgb(color), White_rgb(), 300, 0.3, z),
               x1,y1,
               x2,y2
               );
}

static void create_cursor(const struct Tracker_Windows *window, const struct WBlocks *wblock){
  
  GE_Context *c = GE_z(GE_alpha(GE_get_rgb(ATOMIC_GET(root->editonoff)?CURSOR_EDIT_ON_COLOR_NUM:CURSOR_EDIT_OFF_COLOR_NUM), 0.2), Z_STATIC, NOMASK_Y);
  
  NInt track    = window->curr_track;
  int  subtrack = window->curr_track_sub;
  
  int xb1 = GetXSubTrack_B1(wblock,track,subtrack)-1;
  int xb2 = GetXSubTrack_B2(wblock,track,subtrack)+1;

  int x1 = window->leftslider.width;
  int x2 = xb1;
  int x3 = xb2;
  int x4 = window->width;

  int dy = wblock->t.y1;
  int y1 = GetCursorY1Pos(window, wblock) - dy;
  int y2 = GetCursorY2Pos(window, wblock) - dy;

  GE_filledBox(c, 
               x1, y1,
               x2, y2
               );
  
  GE_filledBox(c, 
               x3, y1,
               x4, y2
               );


  c = GE_z(GE_alpha(Black_rgb(), 0.75), Z_STATIC, y1);

  float width = 0.8f;
  GE_box(c,
         x1+2,y1,
         x4-3,y2-1,
         width
         );

  c = GE_z(GE_alpha(White_rgb(), 0.05), Z_STATIC, y1);
  GE_filledBox(c, 
               x2, y1,
               x3, y2-1
               );

  /*
  GE_box(c,
         x2-0.5f,y1,
         x3,y2-1,
         width/2
         );
  */
}

static void create_playcursor(const struct Tracker_Windows *window, const struct WBlocks *wblock){
  if (ATOMIC_GET(root->play_cursor_onoff)) {
    
    GE_Context *c = GE_z(GE_alpha(GE_get_rgb(PLAY_CURSOR_COLOR_NUM), 0.3), Z_PLAYCURSOR, NOMASK_Y);
    
    int x1 = window->leftslider.width;
    int x2 = window->width;
    
    int dy = wblock->t.y1;
    int y = GetCursorY1Pos(window, wblock) - dy;
    
    GE_line(c,
            x1, y,
            x2, y,
            4.0
            );
  }
}

static void create_message(const struct Tracker_Windows *window, const char *message){

  int width = (int)strlen(message) * window->fontwidth;
  int height = window->fontheight;
  
  int middle_x = window->width / 2;
  int middle_y = window->height / 2;

  int x1 = middle_x - width;
  int x2 = middle_x + width;

  int y1 = middle_y - height;
  int y2 = middle_y + height;
  
  GE_Context *background = GE_z(Black_rgb(), Z_STATIC, NOMASK_Y);
  GE_filledBox(background, x1, y1, x2, y2);

  GE_Context *border = GE_z(White_rgb(), Z_STATIC, NOMASK_Y);
  GE_box(border, x1, y1, x2, y2, 1.0);

  int x = middle_x - width/2;
  int y = middle_y - height/2;
  
  GE_Context *text_color = GE_z(White_rgb(), Z_STATIC, NOMASK_Y);
  GE_text(text_color, message, x, y);
}

static void create_lacking_keyboard_focus_greyed_out(const struct Tracker_Windows *window){
  if (g_do_grey_editor){
    GE_Context *grey = GE_z(GE_rgba(100,100,100,120), Z_STATIC, NOMASK_Y);
    GE_filledBox(grey, 0, 0, window->width, window->height);
  }
}

/************************************
   block
 ************************************/

#include <thread>

extern bool g_gl_widget_started;

static void GL_create2(const struct Tracker_Windows *window, struct WBlocks *wblock){

  if (g_gl_widget_started==false) // This check is probably not necessary
    return;
  
  init_g_colored_tracks_if_necessary();
  
  //static int n=0; printf("GL_create called %d\n",n++);

  bool block_is_visible = wblock != NULL;
  
  int y2 = wblock==NULL ? -1 : get_realline_y2(window, wblock->num_reallines-1);

  GE_start_writing(y2, block_is_visible); {

    if (block_is_visible) {
      const WSignature_trss wsignatures_trss = WSignatures_get(window, wblock);
      
      create_left_slider(window, wblock);
      create_background(window, wblock, wsignatures_trss);
      create_block_borders(window, wblock);
      create_linenumbers(window, wblock, wsignatures_trss);
      create_tempograph(window, wblock);
      if(window->show_signature_track)
        create_signaturetrack(window, wblock, wsignatures_trss);
      if(window->show_swing_track)
        create_swingtrack(window, wblock, wblock->block->filledout_swings.array, wblock->swingTypearea.x);
      if(window->show_lpb_track)
        create_lpbtrack(window, wblock);
      if(window->show_bpm_track)
        create_bpmtrack(window, wblock);
      if(window->show_reltempo_track)
        create_reltempotrack(window, wblock);
      create_tracks(window, wblock);
      create_range(window, wblock);
      create_cursor(window, wblock);
      create_playcursor(window, wblock);
    }

    {
      const char *message = window->message;
      if (message==NULL && block_is_visible==false)
        message = "Current sequencer track is pausing.";

      if (message != NULL)
        create_message(window, message);
    }
    
    create_lacking_keyboard_focus_greyed_out(window);
    
  } GE_end_writing(GE_get_rgb(LOW_EDITOR_BACKGROUND_COLOR_NUM));
}


#define RENDER_IN_SEPARATE_THREAD 0


#if !RENDER_IN_SEPARATE_THREAD

void GL_create(const struct Tracker_Windows *window){
#if 1 //defined(RELEASE)
  GL_create2(window, window->curr_block < 0 ? NULL : window->wblock);
#else
  double start = TIME_get_ms();
  GL_create2(window, window->curr_block < 0 ? NULL : window->wblock);
  printf("   GL_create. dur: %f\n", TIME_get_ms() - start);
#endif
}

#else

#include "../common/Semaphores.hpp"
#include "../common/threading.h"

radium::Semaphore sem_req;
radium::Semaphore sem_finished;

const struct Tracker_Windows *g_window;
static struct WBlocks *g_wblock;



static void gl_create_thread(){
  THREADING_init_main_thread_type(); // Can do this since the main thread is waiting for sem_finished while this thread does it's work.
    
  while(true){
    sem_req.wait();
    {
      GL_create2(g_window, g_wblock);
    }
    sem_finished.signal();
  }
}

#include <gc.h>

extern void processEventsALittleBit(void);
extern bool g_qtgui_has_started,g_qtgui_has_stopped;

void GL_create(const struct Tracker_Windows *window){
  static std::thread t1(gl_create_thread);

  if (g_qtgui_has_started==false || g_qtgui_has_stopped==true)
    return;

  Threadsafe_GC_disable(); // Could also turn on thread support in gc for the worker thread, but this is simpler, and perhaps faster too.
  {
    g_window = window;
    g_wblock = window->wblock;

    double start = TIME_get_ms();
    sem_req.signal();

    sem_finished.wait();

    printf("   dur: %f\n", TIME_get_ms() - start);
    /*
    do{
      processEventsALittleBit();
    }while(sem_finished.tryWait()==false);
    */
    
    //sem_finished.wait();
  }
  Threadsafe_GC_enable();

  //t1.join();
}

#endif

DEFINE_ATOMIC(bool, g_is_creating_all_GL_blocks) = false;

void GL_create_all(const struct Tracker_Windows *window){
  struct WBlocks *wblock = window->wblocks;

  ATOMIC_SET(g_is_creating_all_GL_blocks, true);
  g_is_creating_all_GL_blocks = true;
  
  while(wblock!=NULL){
    if (wblock != window->wblock){
      int org_left_track = wblock->left_track;
      int org_right_track = wblock->right_track;
      wblock->left_track = 0;
      wblock->right_track = wblock->block->num_tracks-1;
      
      GL_create2(window, wblock);
      GE_wait_until_block_is_rendered();

      wblock->left_track = org_left_track;
      wblock->right_track = org_right_track;
    }
    wblock = NextWBlock(wblock);
  }

  GL_create2(window, window->wblock);
    
  g_is_creating_all_GL_blocks = false;
  ATOMIC_SET(g_is_creating_all_GL_blocks, false);
}
