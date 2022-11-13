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

#if __GNUC__ >= 5
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wsuggest-override"
#endif

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-equal"
#include <vlCore/VisualizationLibrary.hpp>
#include <vlGraphics/OpenGLContext.hpp>
#pragma GCC diagnostic pop

#if __GNUC__ >= 5
#  pragma GCC diagnostic pop
#endif

#include "../common/nsmtracker.h"
#include "../common/TimeData.hpp"
#include "../common/placement_proc.h"
//#include "../common/ratio_funcs.h"

#include "../common/settings_proc.h"
#include "../common/list_proc.h"
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
#include "../common/Beats_proc.h"
#include "../common/LPB_proc.h"
#include "../common/tempos_proc.h"
#include "../common/notestext_proc.h"
#include "../common/sequencer_proc.h"
#include "../common/window_config_proc.h"

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

static float get_thickness(float thickness){
  if (!g_has_gfx_scale)
    return thickness;
  else
    return thickness * R_MAX(1, g_gfx_scale * 0.55);
  //return thickness;
}

static float get_pitchline_width(void){
  static float width = -1;

  if (width<0)
    width = get_thickness(1.2); //SETTINGS_read_double("gfx_pitchline_width", 1.2); // If changing this value, also change bin/config

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
                               const char *text,
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
  GE_Context *c2 = GE_gradient_z(qc1, qc2, GE_Conf(z, y)); //GE_get_rgb(9), GE_get_rgb(11), z);

  
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
  char temp3[110];
  int length2;
  
  snprintf(temp,49,"%d",num);

  length2=length-(int)strlen(temp);  
  if(length2<0)
    length2=0;
  
  if(length2!=0){
    memset(temp2,' ',length2+1);
    temp2[length2]=0;
  }else
    temp2[0]=0;
  
  snprintf(temp3,109,"%s%s",temp2,temp);
  temp3[length+1]=0;
  GE_text(c, temp3, x, y);
}


static void draw_node_indicator(float x,
                                float y,
                                enum ColorNums color)
{  
  //GE_Context *c = GE_color_alpha_z(AUTOMATION_INDICATOR_COLOR_NUM, 0.6, GE_Conf(Z_MAX_SCROLLTRANSFORM, NOMASK_Y, NO_SCISSORS));
  GE_Context *c = GE_color_alpha_z(color, 0.6, GE_Conf(Z_MAX_SCROLLTRANSFORM, NOMASK_Y, NO_SCISSORS));
  
  float away1 = 1024;
  float away2 = 5;
  float thickness = get_thickness(0.8);

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

const struct ListHeader3 *g_current_node = NULL;
const struct ListHeader3 *g_indicator_node = NULL;
int g_indicator_velocity_num = -1;
int g_indicator_pitch_num = -1;

int64_t g_current_node_id = -1;
int64_t g_indicator_node_id = -1;

static void draw_skewed_box_doit(const struct Tracker_Windows *window,
                                 bool is_current_node,
                                 enum ColorNums color,
                                 float x, float y,
                                 enum UseScissors use_scissors
                                 )
{
 
  float minnodesize = R_MAX(1, get_min_node_size() - 1);
  float x1 = x-minnodesize;
  float x2 = x+minnodesize;
  float y1 = y-minnodesize;
  float y2 = y+minnodesize;
  const float width = get_thickness(1.2*g_gfx_scale);

  GE_Conf conf(Z_ABOVE(Z_ZERO), y, use_scissors);

  if (is_current_node) {
    GE_filledBox(GE_mix_alpha_z(GE_get_rgb(color), White_rgb(), 300, 0.3, conf),
                 x1,y1,
                 x2-g_gfx_scale,y2-2*g_gfx_scale
                 );
  }

  // vertical left
  GE_line(GE_mix_alpha_z(GE_get_rgb(color), White_rgb(), 100, 0.3, conf.copy(y1+g_gfx_scale)),
          x1+g_gfx_scale, y1+g_gfx_scale,
          x1+2*g_gfx_scale,y2-g_gfx_scale,
          width);

  // horizontal bottom
  GE_line(GE_mix_alpha_z(GE_get_rgb(color), Black_rgb(), 300, 0.3, conf.copy(y2-g_gfx_scale)),
          x1+2*g_gfx_scale,y2-g_gfx_scale,
          x2-g_gfx_scale,y2-2*g_gfx_scale,
          width);

  // vertical right
  GE_line(GE_mix_alpha_z(GE_get_rgb(color), Black_rgb(), 400, 0.3, conf.copy(y1+2*g_gfx_scale)),
          x2-g_gfx_scale,y2-2*g_gfx_scale,
          x2-2*g_gfx_scale,y1+2*g_gfx_scale,
          width);

  // horizontal top
  GE_line(GE_mix_alpha_z(GE_get_rgb(color), White_rgb(), 300, 0.3, conf.copy(y1+g_gfx_scale)),
          x2-2*g_gfx_scale,y1+2*g_gfx_scale,
          x1+g_gfx_scale,y1+g_gfx_scale,
          width);
}

static void draw_skewed_box(const struct Tracker_Windows *window,
                                 const struct ListHeader3 *node,
                                 enum ColorNums color,
                                 float x, float y,
                                 enum UseScissors use_scissors
                                 )
{
  draw_skewed_box_doit(window, node == g_current_node, color, x, y, use_scissors);
}

static void draw_skewed_box2(const struct Tracker_Windows *window,
                            int64_t node_id,
                            enum ColorNums color,
                            float x, float y,
                            enum UseScissors use_scissors
                            )
{
  draw_skewed_box_doit(window, node_id == g_current_node_id, color, x, y, use_scissors);
}

static void create_double_border(
                          float x, int y, int y2,
                          enum UseScissors use_scissors
                          )
{
  if (false){ //ATOMIC_GET(root->editonoff)){
    // old
    GE_line(Black_color(y,use_scissors),x,y,x,y2,0.5);
    GE_line(GE_color(TRACK_SEPARATOR2B_COLOR_NUM,NOMASK_Y,use_scissors),x+1,y,x+1,y2,0.5);
  } else {
    float black_width = get_thickness(1.5f);
    float white_width = get_thickness(1.0f);
    float black_skew = black_width/2.0f;
    float white_skew = black_skew + black_width + -white_width/2.0f;
    GE_line(GE_color(TRACK_SEPARATOR2A_COLOR_NUM,NOMASK_Y,use_scissors),
            x+black_skew, y,
            x+black_skew, y2,
            black_width
            );
    GE_line(GE_color(TRACK_SEPARATOR2B_COLOR_NUM,NOMASK_Y,use_scissors),
            x+white_skew, y,
            x+white_skew, y2,
            white_width
            );
  }
}

static void create_single_border(
                          int x, int y, int y2,
                          enum UseScissors use_scissors
                          )
{
  float thickness = get_thickness(0.5);
  GE_line(GE_color(TRACK_SEPARATOR1_COLOR_NUM,NOMASK_Y,use_scissors),x,y,x,y2,thickness);
}


static void create_single_linenum_border(
                                  int x, int y, int y2,
                                  enum UseScissors use_scissors
                                  )
{
  float thickness = get_thickness(0.5);
  GE_line(GE_color_alpha(TRACK_SEPARATOR1_COLOR_NUM,0.5,NOMASK_Y,use_scissors),x,y,x,y2,thickness);
}



static void stipled_vertical_line(GE_Context *c, float x, float y1, float y2){
  const float width = get_thickness(0.3);
  const float bit = 4 + 4 * (float)rand()/(float)RAND_MAX;

  for (float y = y1 ; y < y2 ; y += bit){
    float y22 = R_MIN(y2, y+bit/2);
    GE_line(c, x, y, x, y22, width);
  }
}


template<typename NodeLine> 
static GE_Context *drawNodeLines(const struct Tracker_Windows *window,
                                 const NodeLine *nodelines,
                                 enum ColorNums colnum,
                                 bool is_selected,
                                 float alpha,
                                 float alpha_selected,
                                 bool hide_vertical,
                                 enum UseScissors use_scissors = USE_SCISSORS
                                 )
{
  const float cut_size1 = window->fontheight*2;
  //const float cut_size2 = 10;

  GE_Context *c = NULL;
  
  float width = get_nodeline_width(is_selected);
  
  for(const NodeLine *ns = nodelines ; ns!=NULL ; ns=ns->next) {
    int logtype = ns->logtype;
    float x1 = ns->x1;
    float x2 = logtype==LOGTYPE_HOLD ? ns->x1 : ns->x2;
    float y1 = ns->y1;
    float y2 = ns->y2;

    c = c!=NULL ? GE_y(c, y1) : GE_color_alpha_z(colnum, is_selected ? alpha_selected : alpha, GE_Conf(Z_ABOVE(Z_ABOVE(Z_ZERO)), 0, use_scissors));

    bool paint_stipled = hide_vertical && is_selected==false && equal_floats(x1, x2) && (y2 - y1) > cut_size1*3.5;
    
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
  GE_Context *border = GE_color_z(LINE_SLIDER_COLOR_NUM, GE_Conf(Z_STATIC, NOMASK_Y, NO_SCISSORS));

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
    scrollbar = GE_mix_color_z(White_rgb(), GE_get_rgb(LINE_SLIDER_COLOR_NUM), 200, GE_Conf(Z_SCROLLBAR, NOMASK_Y, NO_SCISSORS));
  else
    scrollbar = GE_color_z(LINE_SLIDER_COLOR_NUM, GE_Conf(Z_SCROLLBAR, NOMASK_Y, NO_SCISSORS));
  
  GE_filledBox(scrollbar,
               x1+1.5, 1, // (does not paint at editor.y1=0, but at scrollbar_slider.y1=0)
               x2-1.5, get_scrollbar_scroller_height(window,wblock)
               );
}


/************************************
   Background
 ************************************/



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
    return GE_mix(rgb2, Black_rgb(), g_bar_opacity);
  
  else if (wsignature.beat_num>0)
    return GE_mix(rgb2, Black_rgb(), g_beat_opacity);
  
  else if(localzoom->level>0 && localzoom->zoomline>0 && localzoom->autogenerated==false)
    return GE_mix(rgb2, White_rgb(), 925); // manually splitted line
  
  else if (g_line_opacity < 1000)
    return GE_mix(rgb2, Black_rgb(), g_line_opacity); // manually splitted line

  else
    return rgb2;
    
}

static void create_background_realline(const struct Tracker_Windows *window, const struct WBlocks *wblock, const WSignature &wsignature, int realline){

  bool has_keyboard_focus = FOCUSFRAMES_has_focus(radium::KeyboardFocusFrameType::EDITOR);
  struct WTracks *curr_wtrack = window->curr_track >=0 ? wblock->wtrack : NULL;
  
  const struct WTracks *last_wtrack = (const struct WTracks*)ListLast1(&wblock->wtracks->l);
  const struct LocalZooms *localzoom = wblock->reallines[realline];
  
  //int x1 = wblock->linenumarea.x;
  int x1 = wblock->tempocolorarea.x;
  int x2 = last_wtrack->x2;
  int y1 = get_realline_y1(window, realline);
  int y2 = get_realline_y2(window, realline);


  const enum ColorNums c15 = HIGH_EDITOR_BACKGROUND_COLOR_NUM;
  
  if(g_line_opacity == -1)
    g_line_opacity = SETTINGS_read_int32("line_opacity", 1000);
  
  if(g_beat_opacity == -1)
    g_beat_opacity = SETTINGS_read_int32("beat_opacity", 950);
  
  if(g_bar_opacity == -1)
    g_bar_opacity = SETTINGS_read_int32("first_beat_opacity", 870);
  

  // background
  {    

    const struct WTracks *wtrack = get_leftmost_visible_wtrack(wblock);
    const struct WTracks *end_wtrack = get_first_not_visible_wtrack(wblock, wtrack);

    if (wtrack==NULL)
      R_ASSERT_RETURN_IF_FALSE(end_wtrack==NULL);

    if (wtrack!=NULL){
      {
        GE_Context *c = GE_z(shade_realline(GE_get_rgb(c15), false, wsignature, localzoom), GE_Conf(Z_BACKGROUND | Z_STATIC_X, y1));
        
        if (g_colored_tracks)
          GE_filledBox(c,x1,y1,wtrack->x,y2);
        else
          GE_filledBox(c,x1,y1,x2,y2);
      }
      
      if (g_colored_tracks) {
        
        while(wtrack != end_wtrack){
          
          int x1 = R_MAX(wtrack->x - 1, wblock->t.x1);
          int x2 = wtrack->x2;
          //int y1 = wtrack1->y;
          //int y2 = wtrack1->panonoff.y1 - 1;
          
          struct Patch *patch = wtrack->track->patch;
          
          if (patch != NULL){
            GE_Rgb rgb = patch==NULL ? GE_get_custom_rgb(HIGH_EDITOR_BACKGROUND_COLOR_NUM) : GE_get_rgb(patch->color, true);
            
            bool is_current_track = get_current_instruments_gui_patch()==patch;
            
            GE_Context *c = GE_z(
                                 shade_realline(rgb,
                                                is_current_track,
                                                wsignature,
                                                localzoom
                                                )
                                 ,
                                 GE_Conf(Z_BACKGROUND | Z_STATIC_X, y1)
                                 );


            if (has_keyboard_focus && wtrack==curr_wtrack){
              float width = 1; // must be same width as in create_curr_track_border
              GE_filledBox(c,x1+width,y1,x2-width,y2);

              /*
              GE_Context *c2 = GE_z(GE_get_rgb(CURR_TRACK_BORDER_COLOR_NUM),
                                    GE_Conf(Z_BACKGROUND | Z_STATIC_X, y1));
                
              GE_box(c2,x1+width/2,y1+width/2,x2-width/2,y2-width/2,width);
              */
              
            } else {
              GE_filledBox(c,x1,y1,x2,y2);
            }
          }
          
          wtrack=NextWTrack(wtrack);
        }
        
      }

    }
  }

  float line_width = get_thickness(0.6f);

  // realline separator line
  if(1){
    //if(line_opacity == -1)
    //  line_opacity = SETTINGS_read_int("line_opacity", R_MAX(50, beat_opacity-500));
    //line_opacity = 900;
    
    int opacity;
    if (WSIGNATURE_is_first_beat(wsignature))
      opacity = 0; //R_MAX(0, g_bar_opacity - 750);
    else if (wsignature.beat_num>0)
      opacity = R_MAX(250, g_beat_opacity - 500);
    else
      opacity = R_MAX(500, g_line_opacity - 250);

    if(opacity < 1000){

      const GE_Conf conf(Z_ABOVE(Z_BACKGROUND) | Z_STATIC_X, y1, NO_SCISSORS);

      GE_Context *c = GE_mix_color_z(GE_get_rgb(c15), Black_rgb(), R_MAX(0, opacity), conf);

      if (true) {
        GE_line(c,x1,y1,x2,y1,line_width);

        for(float f : wsignature.how_much_below)
          if (fabs(f) > 0.1) {
            float y_ = scale(f, 0, 1, y1, y2);
            float x1_ = wblock->linenumarea.x;
            float x2_ = wblock->linenumarea.x2;
            GE_line(c,x1_,y_,x2_,y_, get_thickness(2.3));
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

static void create_curr_track_border(const struct Tracker_Windows *window, const struct WBlocks *wblock){
  float width = 1; // must be same width as in create_background_realline
        
  int x1 = WTRACK_getx1(window, wblock, window->curr_track);
  int x2 = WTRACK_getx2(window, wblock, window->curr_track);

  x1--;
  x2 += 3;
  
  float y1 = get_scrollbar_y1(window, wblock);
  float y2 = GE_get_height();//get_scrollbar_y2(window, wblock);
  
  GE_Context *c2 = GE_z(GE_get_rgb(KEYBOARD_FOCUS_BORDER_COLOR_NUM),
                        GE_Conf(Z_BELOW(Z_STATIC), NOMASK_Y, window->curr_track >= 0 ? USE_SCISSORS : NO_SCISSORS));
  
  GE_box(c2,x1+width/2,y1+width/2,x2-width/2,y2-width/2,width);
}

static void create_background(const struct Tracker_Windows *window, const struct WBlocks *wblock, const WSignature_trss &wsignatures_trss){
  int realline;
  for(realline = 0 ; realline<wblock->num_reallines ; realline++)
    create_background_realline(window, wblock, wsignatures_trss[realline], realline);

  if (FOCUSFRAMES_has_focus(radium::KeyboardFocusFrameType::EDITOR))
    create_curr_track_border(window, wblock);
}





/************************************
   Linenumbers
 ************************************/

int g_current_bar_num = -1;
int g_current_beat_num = -1;
int g_current_barbeat_block_num = -1;


static void draw_linenumber(const struct Tracker_Windows *window, const struct WBlocks *wblock, const WSignature &wsignature, int realline){
  const struct LocalZooms *localzoom = wblock->reallines[realline];

  const bool is_beatnum = wsignature.beat_num > 0;
  const bool is_barnum = wsignature.bar_num>0 && WSIGNATURE_is_first_beat(wsignature);
  const bool is_zoomline = localzoom->level>0 && localzoom->zoomline>0 && localzoom->autogenerated==false;

  const float y1 = get_realline_y1(window, realline);
  const GE_Conf conf(Z_LINENUMBERS | Z_STATIC_X, y1, NO_SCISSORS);
  
  if (linenumbersVisible() && localzoom->l.p.counter==0) {

    draw_text_num(
                  window,
                  GE_textcolor_z(TEXT_COLOR_NUM, conf),
                  localzoom->l.p.line,
                  wblock->linenumbers_max_num_characters,
                  wblock->linenumarea.x,
                  y1
                  );
    

  } else if (is_beatnum || is_barnum) {

    const int barnum = wsignature.bar_num;
    const int beatnum = wsignature.beat_num;
      
    const bool is_current_bar = g_current_barbeat_block_num==wblock->l.num && g_current_bar_num==barnum;
    const bool is_current_beat = is_current_bar && g_current_beat_num==beatnum;

    if (is_barnum) {

      //printf("bar: %d/%d. %d/%d. %d/%d\n", g_current_barbeat_block_num, wblock->l.num, g_current_beat_num, 1, g_current_bar_num, barnum);
      
      const bool has_current_beat = g_current_beat_num > 0;
        
      draw_text_num(
                    window,
                    (is_current_bar && !has_current_beat) ? GE_textcolor_z(CURRENT_BEAT_TEXT_COLOR_NUM, conf) : GE_textcolor_z(BAR_TEXT_COLOR_NUM, conf),
                    barnum,
                    wblock->bars_max_num_characters,
                    wblock->linenumarea.x,
                    y1
                  );
      
    }

    if (is_beatnum && (!is_barnum || is_current_beat)) {

        draw_text_num(
                      window,
                      is_current_beat ? GE_textcolor_z(CURRENT_BEAT_TEXT_COLOR_NUM, conf)  : GE_color_alpha_z(TEXT_COLOR_NUM, 0.45, conf), //Z_ZERO, y1),                  
                      beatnum,
                      wblock->beats_max_num_characters,
                      wblock->beats_x,
                      y1
                      );

    }

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
                  GE_textcolor_z(colornum, conf),
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

static const struct TempoGraph create_TempoGraph(const struct Tracker_Windows *window, const struct WBlocks *wblock){
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
      STime time = Place2STime(wblock->block, &p, EDITOR_CURR_TRACK_SWINGING_MODE);

      // This version might be unnoticable faster (although it could also be unnoticable slower), but in the above version we see if the stimes[linenum].time values are wrong.
      //float floatplace = scale(n,0,TEMPOGRAPH_POINTS_PER_REALLINE,fp1,fp2);
      //STime time = Place2STime_from_times2(stimes, floatplace);
      
      if(realline>0 || n>0){
        STime val = time-last_time;
        //printf("%d.%d: Time: %f. Dur: %f\n", realline, n, (double)time/pc->pfreq, (double)val/pc->pfreq);
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

  float width = get_thickness(2.3);

  //printf("min/max: %d, %d\n",(int)min,(int)max);

  GE_Context *c = NULL;
    
  if(fabs(tg.min - tg.max)<20) {
    float middle = (wblock->tempocolorarea.x+wblock->tempocolorarea.x2) / 2.0f;
    float y1 = get_realline_y1(window, 0);
    c = c!=NULL ? GE_y(c, y1) : GE_color(TEMPOGRAPH_COLOR_NUM, y1, NO_SCISSORS);
    GE_line(c,
            middle, y1,
            middle, get_realline_y2(window, wblock->num_reallines-1),
            width);
  }else{
    for(int n=0;n<tg.num_points-1;n++){
      float y1 = n * tg.line_period;
      float y2 = (n+1) * tg.line_period;
      c = c!=NULL ? GE_y(c, y1) : GE_color(TEMPOGRAPH_COLOR_NUM, y1, NO_SCISSORS);
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
  
    const StaticRatio &signature = wsignature.signature;
    
    char temp[50];
    sprintf(temp, "%d/%d", signature.numerator, signature.denominator);
    
    GE_text(GE_textcolor_z(TEXT_COLOR_NUM, GE_Conf(Z_ZERO, y, NO_SCISSORS)),
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

  const GE_Conf conf(Z_ZERO, y, NO_SCISSORS);

  if(lpb!=0){
    draw_text_num(
                  window,
                  GE_textcolor_z(TEXT_COLOR_NUM, conf),
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
    
    GE_text(GE_color_alpha_z(TEXT_COLOR_NUM, 0.3, conf), typetext, wblock->lpbTypearea.x, y);
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

static void create_swing(const struct Tracker_Windows *window, const struct WBlocks *wblock, int realline, int weight, enum WSwingType type, int logtype, bool autogenerated, bool swingtext_fits_reallines, int x, enum UseScissors use_scissors){

  const int y    = get_realline_y1(window, realline);

  const GE_Conf conf(Z_ZERO, y, use_scissors);

  GE_Context *c = autogenerated ? GE_color_alpha_z(TEXT_COLOR_NUM, 0.3, conf) : GE_textcolor_z(TEXT_COLOR_NUM, conf);
  
  draw_text_num(
                window,
                c,
                weight,
                2, //wblock->swingarea.width/window->fontwidth,
                swingtext_fits_reallines ? x : x + window->fontwidth, //wblock->swingarea.x,
                y
                );
  
  if(!swingtext_fits_reallines){
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
      
      GE_text(GE_color_alpha_z(TEXT_COLOR_NUM, 0.3, conf), typetext, x, y);
    }
  }

  if (logtype==LOGTYPE_HOLD){
    float dx;
    if (swingtext_fits_reallines)
      dx = window->fontwidth*2;
    else
      dx = window->fontwidth*3;
    GE_text(c, "|", x + dx, y);
  }
}


static void create_swingtrack(const struct Tracker_Windows *window, const struct WBlocks *wblock, const dynvec_t *barswings, bool swingtext_fits_reallines, const float x, enum UseScissors use_scissors){

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
          create_swing(window, wblock, realline, curr_weight, type, curr_logtype, curr_autogenerated, swingtext_fits_reallines, x, use_scissors);

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

      if (type!=WSWING_NORMAL){
        R_ASSERT_NON_RELEASE(!swingtext_fits_reallines);
      }
    }
  }

  if (curr_weight != -1)
    create_swing(window, wblock, realline, curr_weight, type, curr_logtype, curr_autogenerated, swingtext_fits_reallines, x, use_scissors);
}




/************************************
   bpm track
 ************************************/

static void create_bpm(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WBPMs *wbpms, int realline){
  const int y     = get_realline_y1(window, realline);
  const int tempo = wbpms[realline].tempo;
  const int type  = wbpms[realline].type;
  const int logtype = wbpms[realline].logtype;

  const GE_Conf conf(Z_ZERO, y, NO_SCISSORS);

  GE_Context *c = GE_textcolor_z(TEXT_COLOR_NUM, conf);

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

    GE_text(GE_color_alpha_z(TEXT_COLOR_NUM, 0.3, conf), typetext, wblock->tempoTypearea.x, y);
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

  drawNodeLines(window, nodelines, AUTOMATION2_COLOR_NUM, is_current, 0.6, 0.9, false, NO_SCISSORS);
  
  if (g_indicator_node != NULL || is_current) {
    const vector_t *nodes = get_nodeline_nodes(nodelines, wblock->t.y1);

    VECTOR_FOR_EACH(const Node *, node, nodes) {
      if(wblock->mouse_track==TEMPONODETRACK)
        draw_skewed_box(window, node->element, TEXT_COLOR_NUM, node->x, node->y - wblock->t.y1, NO_SCISSORS);
      if (node->element==g_indicator_node)
        draw_node_indicator(node->x, node->y - wblock->t.y1, AUTOMATION2_COLOR_NUM);
    }END_VECTOR_FOR_EACH;

  }
}




/************************************
    block borders
 ************************************/
static void create_block_borders(
                          const struct Tracker_Windows *window,
                          const struct WBlocks *wblock
                          ){

  int y1=get_realline_y1(window, 0);
  int y2=get_realline_y2(window, wblock->num_reallines-1);

  if (!linenumbersVisible())
    create_single_linenum_border(
                                 wblock->beats_x - 1,
                                 y1,y2,
                                 NO_SCISSORS
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
                                 y1,y2,
                                 NO_SCISSORS
                                 );
    
  create_double_border(
                       wblock->linenumarea.x2+1,
                       y1,y2,
                       NO_SCISSORS
                       );

  create_double_border(
                       wblock->tempocolorarea.x2+1,
                       y1,y2,
                       NO_SCISSORS
                       );

  if (window->show_signature_track)
    create_double_border(
                         wblock->signaturearea.x2+1,
                         y1,y2,
                         NO_SCISSORS
                         );

  if (window->show_swing_track)
    create_double_border(
                         wblock->swingarea.x2+1,
                         y1,y2,
                         NO_SCISSORS
                         );

  if (window->show_lpb_track)
    create_double_border(
                         wblock->lpbarea.x2+1,
                         y1,y2,
                         NO_SCISSORS
                         );

  if (window->show_bpm_track)
    create_double_border(
                         wblock->tempoarea.x2+1,
                         y1,y2,
                         NO_SCISSORS
                         );

  if (window->show_reltempo_track)
    create_double_border(
                         wblock->temponodearea.x2+1,
                         y1,y2,
                         NO_SCISSORS
                         );
}



/************************************
   tracks
 ************************************/
static void create_track_borders(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack){
  int y1=get_realline_y1(window, 0);
  int y2=get_realline_y2(window, wblock->num_reallines-1);
  
  create_double_border(
                       wtrack->x2+1,
                       y1,
                       y2,
                       USE_SCISSORS);

  if(wtrack->notesonoff==1)
    create_single_border(
                         wtrack->notearea.x2+1,
                         y1,
                         y2,
                         USE_SCISSORS);

  int num_subtracks = WTRACK_num_subtracks(wtrack); 
  int first_polyphony_subtrack = WTRACK_num_non_polyphonic_subtracks(wtrack);

  bool has_fx = wtrack->track->fxs.num_elements > 0;
  
  bool has_fxtext = wtrack->fxtext_on && FXTEXT_has(wtrack->track);
  
  if (wtrack->swingtext_on){
    if (wtrack->centtext_on || wtrack->veltext_on || wtrack->chancetext_on || (has_fxtext && has_fx))
      create_single_border(
                           wtrack->swingtextarea.x2+1,
                           y1,
                           y2,
                           USE_SCISSORS);
  }
  
  if (wtrack->centtext_on){
    if (wtrack->veltext_on || wtrack->chancetext_on || (has_fxtext && has_fx))
      create_single_border(
                           wtrack->centtextarea.x2+1,
                           y1,
                           y2,
                           USE_SCISSORS);
  }
  
  if (wtrack->chancetext_on){
    if (wtrack->veltext_on || (has_fxtext && has_fx))
      create_single_border(
                           wtrack->chancetextarea.x2+1,
                           y1,
                           y2,
                           USE_SCISSORS);
  }
  
  if (has_fxtext){
    
    if (wtrack->veltext_on && has_fx)
      create_single_border(
                           wtrack->veltextarea.x2+1,
                           y1,
                           y2,
                           USE_SCISSORS);

    int num_fx = FXTEXT_num(wtrack->track);
    
    int i = 0;
    VECTOR_FOR_EACH(const struct FXs *, fxs, &wtrack->track->fxs){
      if(i>=num_fx-1)
        break;
      if(fxs->fx->is_enabled){
        create_single_border(                           
                             wtrack->fxtextarea.x + (1+i)*WTRACK_fxtext_track_width(window->fontwidth) - 1,
                             y1,
                             y2,
                             USE_SCISSORS);
        i++;
      }
    }END_VECTOR_FOR_EACH;
  }
  
  for(int lokke=R_MAX(1, first_polyphony_subtrack) ; lokke < num_subtracks ; lokke++){
    create_single_border(
                         GetXSubTrack1(wtrack,lokke)-1,
                         y1,
                         y2,
                         USE_SCISSORS);
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
/*
static float get_notenum(const TrackRealline2 &tr2){

  switch(tr2.type){
    case TR2_NOTE_START:
      return tr2.note->note;
    case TR2_NOTE_END:
      return tr2.note->pitch_end;
    case TR2_PITCH:
      {
        R_ASSERT_RETURN_IF_FALSE2(tr2.note != NULL, 11);
        R_ASSERT_RETURN_IF_FALSE2(tr2.note->pitches != NULL, 12);
        struct Pitches *pitch = (struct Pitches*)ListFindElement3_num(&tr2.note->pitches->l, tr2.pitchnum);
        if (pitch != NULL)
          return pitch->note;
        else{
          R_ASSERT(false);
          return 13;
        }
      }
    case TR2_STOP:
      return NOTE_STP;
  }

  R_ASSERT_NON_RELEASE(false);
  return 50; // To silence erronouous compiler warning.
}
*/

static float get_notenum(const Trs &trs){
  if (trs.size()==0)
    return 0;

  if (trs.size() > 1)
    return NOTE_MUL;

  const TrackRealline2 &tr2 = trs[0];

  return tr2.pitch;//get_notenum(tr2);
}
/*
static int get_chance(const TrackRealline2 &tr2){
  switch(tr2.type){
    case TR2_NOTE_START:
      return tr2.note->chance;
    case TR2_NOTE_END:
      return -1;
    case TR2_PITCH:
      {
        R_ASSERT_RETURN_IF_FALSE2(tr2.note != NULL, -1);
        R_ASSERT_RETURN_IF_FALSE2(tr2.note->pitches != NULL, -1);
        struct Pitches *pitch = (struct Pitches*)ListFindElement3_num(&tr2.note->pitches->l, tr2.pitchnum);
        if (pitch != NULL)
          return pitch->chance;
        else{
          R_ASSERT(false);
          return -1;
        }
      }
      break;
    case TR2_STOP:
      return -1;
  }

  R_ASSERT_NON_RELEASE(false);
  return 50; // To silence erronouous compiler warning.
}
*/
static float get_chance(const Trs &trs){
  if (trs.size()==0)
    return -1;

  if (trs.size()>1)
    return -2;

  const TrackRealline2 &tr2 = trs[0];

  return tr2.chance; //get_chance(tr2);
}

static ColorNums get_colnum(const TrackRealline2 &tr2){
  switch(tr2.type){
    case TR2_NOTE_START:
      return TEXT_COLOR_NUM;
    case TR2_NOTE_END:
      return PORTAMENTO_END_NOTE_TEXT_COLOR_NUM;
    case TR2_PITCH:
      return PORTAMENTO_NOTE_TEXT_COLOR_NUM;
    case TR2_STOP:
      return TEXT_COLOR_NUM;
  }

  R_ASSERT_NON_RELEASE(false);
  return TEXT_COLOR_NUM;  // To silence erronouous compiler warning.
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

static void paint_tr2(const TrackRealline2 &tr2, const char **NotesTexts, int num, int x, int y){
  GE_Context *c = GE_textcolor(get_colnum(tr2), y);
  float notenum = tr2.pitch;//get_notenum(tr2);
  paint_halfsize_note(c, num, get_notename(NotesTexts, notenum), x, y);
  //GE_text_halfsize(foreground, NotesTexts[(int)notenum1], x, y);
}

static void paint_multinotes(const struct WTracks *wtrack, const Trs &trs, const char **NotesTexts, int y1, int y2){
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
  
  const char **NotesTexts = wtrack->notelength==3?NotesTexts3:NotesTexts2;
  float  notenum    = get_notenum(trs); //trackrealline->note;
  
  int y1 = get_realline_y1(window, realline);
  int y2 = get_realline_y2(window, realline);

  const TrackRealline2 &tr2 = trs[0];

  enum ColorNums colnum = get_colnum(tr2);

  double cents_d = (notenum - floor(notenum))*100.0;
  int cents = R_BOUNDARIES(0,round(cents_d),99);
        
  if (wtrack->noteshowtype==TEXTTYPE && show_notes){

    // Paint THISNOTELINES
    if(trs.size() < 2 && tr2.note && tr2.note->d._polyphony_num>0) {
      //printf("Gakk: %s (%s), %d, pointer: %p\n",NotesTexts[(int)notenum],NotesTexts[(int)note->note],note->subtrack,note);
      float y = (y1+y2) / 2.0f;
      float x1 = wtrack->notearea.x2;
      float x2 = (GetNoteX1(wtrack,tr2.note->d._polyphony_num) + GetNoteX2(wtrack,tr2.note->d._polyphony_num)) / 2.0f;
      GE_line(GE_color(TEXT_COLOR_NUM,y1),
              x1, y,
              x2, y,
              get_thickness(0.8));
    }

    bool highlight;

    switch(tr2.type){
      case TR2_NOTE_START:
      case TR2_NOTE_END:
        highlight = tr2.node_id==g_current_node_id;
        break;
      case TR2_PITCH:
        highlight = tr2.node_id==g_current_node_id;
        break;
      case TR2_STOP:
        highlight = false;
        break;
      default:
        R_ASSERT_NON_RELEASE(false);
        highlight = false;
        break;
    }

    //printf("highlight: %d %p %p\n",highlight,trackrealline->daspitch,trackrealline->dasnote);
    //printf("g_current_node: %p\n\n",g_current_node);
      

    // Only paint background for real notes. (129-131=mur/---/mul)
    if(notenum>0 && notenum<128)
      GE_filledBox(get_note_background(notenum, highlight, y1), wtrack->notearea.x, y1, wtrack->notearea.x2, y2);

    const char* notestext = get_notename(NotesTexts, notenum);
      
    if (trs.size() > 1)
      paint_multinotes(wtrack, trs, NotesTexts, y1, y2);

    else if ((g_is_creating_all_GL_blocks==false && wblock->mouse_track == wtrack->l.num)
             || cents!=0
             || wtrack->is_wide==true
             ){
      GE_Context *foreground = GE_textcolor_z(colnum, GE_Conf(Z_ABOVE(Z_ZERO), y1));

      if (cents==0 || wtrack->centtext_on==true)
        GE_text(foreground, notestext, wtrack->notearea.x, y1); 
      else {
        char temp[32];
        if (wtrack->is_wide)
          sprintf(temp,"%s.%d",notestext,cents);
        else
          sprintf(temp,"%s %d",notestext,cents);
        GE_text(foreground, temp, wtrack->notearea.x, y1); 
      }
      
      //GE_text(foreground, NotesTexts[(int)notenum], wtrack->notearea.x, y1);
        
    }else
      draw_bordered_text(window, GE_textcolor_z(colnum, GE_Conf(Z_ZERO, y1)), notestext, wtrack->notearea.x, y1);
  }

  if (wtrack->centtext_on) {

    GE_Context *foreground = GE_textcolor_z(colnum,GE_Conf(Z_ABOVE(Z_ZERO),y1));

    if (cents != 0){
      char centtext[16];
      sprintf(centtext,"%s%d",cents<10?" ":"",cents); // Never remembers the short syntax for this.
      GE_text(foreground, centtext, wtrack->centtextarea.x, y1);
    }
  }

  if (!wtrack->centtext_on && wtrack->notesonoff==1 && !equal_doubles(cents_d, 0.0)) {
    if (wtrack->chancetext_on || wtrack->veltext_on || wtrack->fxtext_on){
      //printf("     %d: cents_d: %f\n",wtrack->l.num, cents_d);

      // FIX: after changing notes to TimeData, this should be done in a writer-hook, not here.
      wtrack->centtext_on = true;
      
      GFX_ScheduleCalculateCoordinates();
    }
  }

  if (wtrack->chancetext_on) {

    int chance  = get_chance(trs);
            
    if (chance!=-1 && chance != MAX_PATCHVOICE_CHANCE){
      GE_Context *foreground = GE_textcolor_z(AUTOMATION2_COLOR_NUM,GE_Conf(Z_ABOVE(Z_ZERO),y1));

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

#if 0
static void create_pitches(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const struct Notes *note, const struct NodeLine2 *nodelines){

  //printf("\nPainting nodeline:\n");
  
  // lines
  {

    GE_Context *line_color = NULL;

    for(const struct NodeLine2 *nodeline=nodelines ; nodeline!=NULL ; nodeline=nodeline->next) {
      bool vertical_line = equal_floats(nodeline->x1, nodeline->x2);
      bool continues_next_block = nodeline->next==NULL && note_continues_next_block(wblock->block, note);
    
      if(true || !vertical_line) {
        int logtype = nodeline->logtype;
        
        float x1 = nodeline->x1;
        float x2 = logtype==LOGTYPE_HOLD ? nodeline->x1 : nodeline->x2;
        float y1 = nodeline->y1;
        float y2 = nodeline->y2;

        //printf("nodeline: %f,%f -> %f,%f\n", x1, y1, x2, y2);
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
  if (g_indicator_node == &note->l && g_indicator_pitch_num!=-1) {
    const vector_t *nodes = get_nodeline_nodes2(nodelines, wblock->t.y1);
    
    if (g_indicator_pitch_num < nodes->num_elements) {
      //printf("g_indicator_pitch_num: %d\n",g_indicator_pitch_num);
      struct Node2 *node = (struct Node2 *)nodes->elements[g_indicator_pitch_num];
      draw_node_indicator(node->x, node->y-wblock->t.y1, PITCH_LINE_COLOR_NUM);
    }    
  }
}
#endif

static void create_pitches2(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const r::Note *note, const struct NodeLine2 *nodelines){

  //printf("\nPainting nodeline:\n");
  
  // lines
  {

    GE_Context *line_color = NULL;

    for(const struct NodeLine2 *nodeline=nodelines ; nodeline!=NULL ; nodeline=nodeline->next) {
      bool vertical_line = equal_floats(nodeline->x1, nodeline->x2);
      bool continues_next_block = nodeline->next==NULL && note_continues_next_block2(wblock->block, note);
    
      if(true || !vertical_line) {
        int logtype = nodeline->logtype;
        
        float x1 = nodeline->x1;
        float x2 = logtype==LOGTYPE_HOLD ? nodeline->x1 : nodeline->x2;
        float y1 = nodeline->y1;
        float y2 = nodeline->y2;

        //printf("nodeline: %f,%f -> %f,%f\n", x1, y1, x2, y2);
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
  if (note->get_node_id()==g_indicator_node_id && g_indicator_node_id!=-1 && g_indicator_pitch_num!=-1) {
    const vector_t *nodes = get_nodeline_nodes2(nodelines, wblock->t.y1);
    
    if (g_indicator_pitch_num < nodes->num_elements) {
      //printf("g_indicator_pitch_num: %d\n",g_indicator_pitch_num);
      struct Node2 *node = (struct Node2 *)nodes->elements[g_indicator_pitch_num];
      draw_node_indicator(node->x, node->y-wblock->t.y1, PITCH_LINE_COLOR_NUM);
    }    
  }
}

static float get_pianoroll_note_width(const struct WTracks *wtrack){
  float gfx_width  = wtrack->pianoroll_area.x2 - wtrack->pianoroll_area.x;
  float notespan   = wtrack->pianoroll_highkey - wtrack->pianoroll_lowkey;
  float note_width = gfx_width / notespan;
  return note_width;
}

static void create_pianoroll_grid(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack){
  int y1=get_realline_y1(window, 0);
  int y2=get_realline_y2(window, wblock->num_reallines-1);

  GE_Context *octave_color = GE_color_alpha(PIANOROLL_OCTAVE_COLOR_NUM, 0.25, NOMASK_Y);
  //GE_Context *note_color = GE_color_alpha(8, 0.5);

  GE_Context *white_key_color = GE_color_alpha(WHITE_COLOR_NUM, 0.1, NOMASK_Y);
  GE_Context *black_key_color = GE_color_alpha(BLACK_COLOR_NUM, 0.1, NOMASK_Y);

  float note_width = get_pianoroll_note_width(wtrack);

  float x = wtrack->pianoroll_area.x;

  for(int key=wtrack->pianoroll_lowkey ; key<wtrack->pianoroll_highkey ; key++){
    int chroma = key % 12;

    if (chroma==0)
      GE_line(octave_color,x,y1,x,y2, get_thickness(1.0));

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

struct CurrentPianoNote g_current_piano_note = {-1,-1,-1};
struct CurrentPianoGhostNote g_current_piano_ghost_note = {.tracknum = -1, .start={}, .end={}, .value=0.0f};

static void draw_pianonote_text(const struct Tracker_Windows *window, float notenum, bool is_current, float midpos, float y){
  
  int cents = R_BOUNDARIES(0,round((notenum - (int)notenum)*100.0),99);
  
  const char *text = get_notename(NotesTexts3, notenum);
  char temp[32];
  
  if (cents!=0){
    sprintf(temp,"%s.%d",get_notename(NotesTexts3, notenum),cents);
    text = &temp[0];
  }
  
  float textgfxlength = strlen(text) * (is_current ? window->fontwidth : window->fontwidth/2);
  float x = midpos - (textgfxlength / 2.0);
  
  y -= (is_current ? window->fontheight : window->fontheight/2) + 2;
  
  GE_Context *c = GE_color_alpha_z(PIANOROLL_NOTE_NAME_COLOR_NUM, 0.7, GE_Conf(Z_ABOVE(Z_ZERO), y));
  
  if (is_current)
    GE_text(c,text,x,y);
  else
    paint_halfsize_note(c,0,text,x,y);
}


struct PianorollRectangle g_current_pianobar_rubber = {-1,-1,{0,0,1},{0,0,1},-2,-2};
struct PianorollRectangle g_current_pianobar_selection_rectangle = {-1,-1,{0,0,1},{0,0,1},-2,-2};

static void maybe_create_pianoroll_rectangle(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack,
                                             const PianorollRectangle &rectangle,
                                             const GE_Rgb &border_color,
                                             const GE_Rgb &filled_color)
{
  if (rectangle.blocknum!=-1 &&
      rectangle.blocknum!=wblock->l.num)
    return;
    
  if (rectangle.tracknum!=-1 &&
      rectangle.tracknum!=wtrack->l.num)
    return;
    
  float reallineF1 = FindReallineForF(wblock, 0, &rectangle.place1);
  float reallineF2 = FindReallineForF(wblock, 0, &rectangle.place2);

  float y1 = get_realline_y(window, R_MIN(reallineF1, reallineF2));
  float y2 = get_realline_y(window, R_MAX(reallineF1, reallineF2));

  //float note_width = get_pianoroll_note_width(wtrack);

  float px1 = wtrack->pianoroll_area.x;
  float px2 = wtrack->pianoroll_area.x2;

  float x1 = scale(R_MIN(rectangle.pitch1, rectangle.pitch2), wtrack->pianoroll_lowkey, wtrack->pianoroll_highkey, px1, px2);
  float x2 = scale(R_MAX(rectangle.pitch1, rectangle.pitch2), wtrack->pianoroll_lowkey, wtrack->pianoroll_highkey, px1, px2);

  const float width = 1.7;
  
  //printf("Painting rubber %f %f %f %f\n", x1, y1, x2, y2);
  GE_filledBox(GE(filled_color, y1, NO_SCISSORS),
               x1, y1,
               x2, y2
               );
  
  GE_box(GE(border_color, y1, NO_SCISSORS),
         x1, y1,
         x2, y2,
         width
         );
}

static void create_pianoroll_notes(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack){
  const float note_width = get_pianoroll_note_width(wtrack);

  const enum ColorNums colornum = PIANONOTE_COLOR_NUM;
  
  GE_set_x_scissor(wtrack->pianoroll_area.x,
                   wtrack->pianoroll_area.x2+1);

  GE_Context *border_color = NULL;
  GE_Context *current_note_color = NULL;
  GE_Context *note_color = NULL;

  bool is_painting_ghost_note = false;
  r::Note ghost_note(make_ratio(0,1), 64, MAX_VELOCITY / 2);
  
  r::NoteTimeData::Reader note_reader(wtrack->track->_gfx_notes2 ? wtrack->track->_gfx_notes2 : wtrack->track->_notes2);

  for(int i = 0 ; i <= note_reader.size() ; i++) {

    const r::Note *note;
    
    if (i < note_reader.size()) {
      
      note = note_reader.at_ref(i).get();
      
    } else {
      
      if (is_painting_ghost_note==false && g_current_piano_ghost_note.tracknum==wtrack->l.num) {
        
        ghost_note._val = g_current_piano_ghost_note.value;
        ghost_note._time = place2ratio(g_current_piano_ghost_note.start);
        ghost_note.d._end = place2ratio(g_current_piano_ghost_note.end);
        
        note = &ghost_note;
        is_painting_ghost_note = true;
        
      } else {
        
        break;
        
      }
    }

    r::PitchTimeData::Reader pitch_reader(&note->_pitches);
    
    const struct NodeLine2 *nodelines = GetPianorollNodeLines2(window,
                                                               wblock,
                                                               wtrack,
                                                               note,
                                                               pitch_reader
                                                               );

    int pianonotenum = -1;
    
    for(const struct NodeLine2 *nodeline=nodelines ; nodeline!=NULL ; nodeline=nodeline->next) {

      bool is_continuing = false;
      
      if (nodeline->next==NULL)
        if (note_continues_next_block2(wblock->block, note))
          is_continuing = true;

      if (nodeline->is_node)
        pianonotenum++;      
 
      int logtype = nodeline->logtype;
      float x1 = nodeline->x1;
      float x2 = logtype==LOGTYPE_HOLD ? nodeline->x1 : nodeline->x2;
      float y1 = nodeline->y1;
      float y2 = nodeline->y2;
 
      border_color = border_color!=NULL ? GE_y(border_color, y1) : GE_color_z(PIANOROLL_NOTE_BORDER_COLOR_NUM, GE_Conf(Z_ABOVE(Z_ZERO), y1));

      const bool is_current_note = wtrack->l.num==g_current_piano_note.tracknum && note->_id==g_current_piano_note.noteid;

      bool is_current = is_current_note && pianonotenum==g_current_piano_note.pianonotenum;

      if (!is_current && is_painting_ghost_note)
        is_current = true;

      if (!is_current && is_current_note && pianonotenum==g_current_piano_note.pianonotenum)
        if (note->d._pianonote_is_selected) // && note->d._pianonote_is_selected)
          is_current = true;

      //printf("pianonotenum: %d, curr.pianonotenum: %d, is_current: %s\n",pianonotenum,g_current_piano_note.pianonotenum,is_current?"true":"false");

      enum UseScissors use_scissors = is_current ? NO_SCISSORS : USE_SCISSORS;

      GE_Context *c;

      if (is_current) {
        
        current_note_color = current_note_color != NULL ? GE_y(current_note_color, y1) : GE_mix_color(GE_get_rgb(colornum), White_rgb(), 500, y1, use_scissors);
        c = current_note_color;
        GE_unset_x_scissor();
        
      } else {
        if (note_color != NULL) {
          note_color = GE_y(note_color, y1);
        } else {
          if (wtrack->track->patch==NULL)
            note_color = GE_color_alpha(colornum, 0.4, y1, use_scissors);
          else
            note_color = GE_mix_color(GE_get_rgb(wtrack->track->patch->color, true), GE_get_rgb(colornum), 400, y1, use_scissors);
        }
        c = note_color;
      }

      bool is_selected = note->d._pianonote_is_selected;

      if (is_painting_ghost_note)
        c = GE(GE_alpha(GE_get_rgb(c), 0.2), y1);
      
      if (is_selected)
        c = GE_mix_color(GE_get_rgb(c), GE_get_rgb(PIANONOTE_SELECTED_COLOR_NUM), 500, y1, use_scissors);

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

        const NodelineBox nodelineBox = GetPianoNoteBox2(wtrack, nodeline);

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
                   get_thickness(1.0)
                   );
          else { 
            // box (without the x1,y2 -> x2,y2 line)
            GE_line(border_color,
                    nodelineBox.x1,nodelineBox.y1,
                    nodelineBox.x2,nodelineBox.y1,
                    get_thickness(1.0)
                    );
            GE_line(border_color,
                    nodelineBox.x2,nodelineBox.y1,
                    nodelineBox.x2,nodelineBox.y2,
                    get_thickness(1.0)
                    );
            GE_line(border_color,
                    nodelineBox.x1,nodelineBox.y1,
                    nodelineBox.x1,nodelineBox.y2,
                    get_thickness(1.0)
                    );
          }

          float notenum;

          if (pianonotenum==0)
            notenum = note->_val;
          else if (pianonotenum==pitch_reader.size()+1)
            notenum = note->d._pitch_end;
          else
            notenum = pitch_reader.at_ref(pianonotenum-1)._val;
          
          float x_text = (nodeline->x1 + nodeline->x2) / 2;
          bool text_is_inside = (x_text > wtrack->pianoroll_area.x - 20) && (x_text < wtrack->pianoroll_area.x2 + 20);
            
          if (text_is_inside || is_current) {
            draw_pianonote_text(window, notenum, is_current, x_text, nodeline->y1);

            // pitch_end
            if (nodeline->next==NULL && note->d._pitch_end > 0) {
              //const r::Pitch &pitch = pitch_reader.at_ref(pianonotenum+1);
              //struct Pitches *pitch = (struct Pitches*)nodeline->element2;
              float  notenum;
              
              if (pianonotenum==pitch_reader.size())
                notenum = note->d._pitch_end;
              else
                notenum = pitch_reader.at_ref(pianonotenum)._val;

              draw_pianonote_text(window, notenum, is_current, (nodeline->x1 + nodeline->x2) / 2, nodeline->y2);
            }
          }

          if (is_continuing){

            float midpos = nodeline->x2;

            float dx = get_thickness(5);
            
            float x1 = midpos - note_width/2; //nodelineBox.x1 - dx;
            float dx1 = x1 - dx;
            
            float x =  midpos; //nodeline->x2;
            
            float x2 = midpos + note_width/2; //nodelineBox.x2 + dx;
            float dx2 = x2 + dx;

            float y0 = nodeline->y2;
            float y1 = nodeline->y2 + get_thickness(15);
            float y2 = nodeline->y2 + get_thickness(25);
          
            GE_filledBox(c,
                         x1, y0,
                         x2, y1
                         );

            {
              GE_ScopedTrianglestrip trianglestrip;
              
              trianglestrip.add(c,dx1,y1);
              trianglestrip.add(c,dx2,y1);
              trianglestrip.add(c,x,y2);
            }

            // Connecting lines between the two objects
            GE_line(border_color,
                    x1, y0,
                    nodelineBox.x1, y0,
                    get_thickness(1.0));            
            GE_line(border_color,
                    x2, y0,
                    nodelineBox.x2, y0,
                    get_thickness(1.0));

            GE_line(border_color,
                    x1, y0,
                    x1, y1,
                    get_thickness(1.0));
            GE_line(border_color,
                    x2, y0,
                    x2, y1,
                    get_thickness(1.0));
            
            
            // arrow
            GE_line(border_color,
                    dx1,y1,
                    x1,y1,
                    get_thickness(1.0));
            GE_line(border_color,
                    x2,y1,
                    dx2,y1,
                    get_thickness(1.0));
            GE_line(border_color,
                    dx1,y1,
                    x,y2,
                    get_thickness(1.0));
            GE_line(border_color,
                    dx2,y1,
                    x,y2,
                    get_thickness(1.0));
          }
        
        }

      }

      if (is_current)
        GE_set_x_scissor(wtrack->pianoroll_area.x,
                         wtrack->pianoroll_area.x2+1);
    }
  }

  GE_unset_x_scissor();
}

static void create_pianoroll(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack){
  //int x1 = 100;
  //int x2 = 200;

  //float note_width = (x2-x1) / 12.0;
  //float half_note_width = 2; //note_width / 2;

  if (wtrack->pianoroll_on==false)
    return;

  //  if (wtrack->pianoroll_area.x < wblock->t.x1)
  //    return;
  
  create_pianoroll_grid(window, wblock, wtrack);

  create_pianoroll_notes(window, wblock, wtrack);

  maybe_create_pianoroll_rectangle(window, wblock, wtrack,
                                   g_current_pianobar_rubber,
                                   GE_rgba(1, 30, 60, 128),
                                   GE_rgba(0xff,0xff,0xff,0x80)
                                   );
    
  maybe_create_pianoroll_rectangle(window, wblock, wtrack,
                                   g_current_pianobar_selection_rectangle,
                                   GE_rgba(120, 30, 60, 128),
                                   GE_alpha(GE_get_rgb(SEQUENCER_TEMPO_AUTOMATION_COLOR_NUM), 0.5)
                                   );
}



static float subtrack_x1, subtrack_x2;

#if 0
static void create_track_peaks(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const struct Notes *note, const struct NodeLine2 *nodelines){
  struct Patch *patch = wtrack->track->patch;
  STime note_time = Place2STime2(wblock->block, &note->l.p, wtrack->track);

  float track_volume =  wtrack->track->volumeonoff ? (float)wtrack->track->volume / MAXTRACKVOL : 1.0f;
  //float velocity = scale(n,0,num_peaks, velocity1->velocity, velocity2->velocity);

  const double reltempo = ATOMIC_DOUBLE_GET(wblock->block->reltempo);
    
  const int num_channels=PATCH_get_peaks(patch, 0,
                                         -1,
                                         wtrack->track,
                                         0,0,
                                         NULL,NULL
                                         );
    

  const float min_width = R_MAX(0.5, (float)window->fontheight / 31.0f);
  const float num_lines_per_peak = R_MAX(2.7, (float)window->systemfontheight / 6.5f);

  GE_Context *c = NULL;
  
  //GE_Context *c = Black_color(); //GE_mix_alpha_z(GE_get_rgb(0), Black_rgb(), 100, 0.7, Z_ZERO);
  //GE_Context *c = GE_mix_alpha_z(GE_get_rgb(0), GE_get_rgb(2), 250, 0.9, Z_ZERO);


  for(int ch=0;ch<num_channels;ch++){

    GE_ScopedTrianglestrip trianglestrip;

    for(const struct NodeLine2 *ns = nodelines ; ns!=NULL ; ns=ns->next){
      int logtype = ns->logtype;
      float x1 = ns->x1;
      float x2 = logtype==LOGTYPE_HOLD ? ns->x1 : ns->x2;
      float y1 = ns->y1;
      float y2 = ns->y2;

      //printf("y1/y2: %f, %f\n", y1, y2);
      
      c = c!=NULL ? GE_y(c, y1) : GE_mix_color_z(GE_get_rgb(LOW_EDITOR_BACKGROUND_COLOR_NUM), GE_get_rgb(WAVEFORM_COLOR_NUM), 100, GE_Conf(Z_ABOVE(Z_ZERO), y1));

      const STime time1 = Ratio2STime2(wblock->block, ns->time1, wtrack->track) - note_time;
      const STime time2 = Ratio2STime2(wblock->block, ns->time2, wtrack->track) - note_time;

      R_ASSERT_NON_RELEASE(time2 >= time1);
      
      if (time1>=time2)
        continue;
          
      float velocity1 = scale(x1, subtrack_x1, subtrack_x2, 0, 1);
      float velocity2 = scale(x2, subtrack_x1, subtrack_x2, 0, 1);
      
      int num_peaks = R_MAX(2, (y2-y1) / num_lines_per_peak); // Must have at least two points in time to draw a waveform.

      int64_t last_end_time = time1;
                                
      for(int n=0; n < num_peaks ; n++){

        float min,max;
        
        int64_t start_time = last_end_time;

        int64_t end_time   = R_MIN(time2,
                                   scale(n+1,
                                         0, num_peaks-1+1,
                                         time1, time2)
                                   );
        
        
        //printf("  %d/%d: %d -> %d (diff: %d) (full: %d -> %d)\n", n, num_peaks, (int)start_time, (int)end_time, (int)(end_time-start_time), (int)time1, (int)time2);

        R_ASSERT_NON_RELEASE(end_time >= start_time);

        int64_t reltempo_start_time = start_time / reltempo;
        int64_t reltempo_end_time = end_time / reltempo;
        
        if (reltempo_start_time>=reltempo_end_time)
          continue; // Playing too fast. No audio data. (note that the current value of 'last_end_time' is kept in the next iteration)
        
        //if (n==0)
        //printf("start_time: %d, time1: %d. end_time: %d, time2: %d\n",(int)start_time, (int)time1, (int)end_time, (int)time2);
        

        PATCH_get_peaks(patch, 
                        note->note,
                        ch,
                        wtrack->track,
                        reltempo_start_time,
                        reltempo_end_time,
                        &min,
                        &max);

        float velocity = (float)scale(n,
                                      0,num_peaks-1,
                                      velocity1, velocity2);

        float bound_x1 = scale(scale(ch,
                                     0,num_channels,
                                     0.0f,velocity),
                               0, 1,
                               subtrack_x1, subtrack_x2);
        float bound_x2 = scale(scale(ch+1,
                                     0,num_channels,
                                     0.0f,velocity),
                               0, 1,
                               subtrack_x1, subtrack_x2);

        float x1 = scale(min*track_volume, -1,1, bound_x1, bound_x2);
        float x2 = scale(max*track_volume, -1,1, bound_x1, bound_x2);
          
        //float y = y1 + n*NUM_LINES_PER_PEAK;
        float y = scale(n,
                        0, num_peaks-1,
                        y1, y2);

        //auto *old_c = c;

        c = GE_y(c, y); // Optimization. Split waveforms into several slices.

        /*
        if (c != old_c){
          printf("Changing C at %f\n", y);
        }
        */
#if 0
        printf("Adding %f,%f at %f. min/max: %f/%f. vel1/vel2: %f/%f. time1/time2: %f/%f\n",x1,x2,y,min,max,
               scale(n,0,num_peaks,velocity1->velocity, velocity2->velocity),
               scale(n+NUM_LINES_PER_PEAK,0,num_peaks,velocity1->velocity, velocity2->velocity),
               scale(n,0,num_peaks,time1,time2) / reltempo,
               scale(n+NUM_LINES_PER_PEAK,0,num_peaks,time1,time2) / reltempo);
#endif

        //printf("   tr.y: %f\n", y);
        trianglestrip.add(c, R_MAX(subtrack_x1, x1-min_width), y); // Subtract a little bit (min_width) so that we see a thin line instead of nothing when there's no sound
        trianglestrip.add(c, R_MIN(subtrack_x2, x2+min_width), y); // Same here.
        
        last_end_time = end_time;
        
      } // end num peaks iteration

    } // end node iteration

  } // end ch iteration
}
#endif

static void create_track_peaks2(const struct Tracker_Windows *window,
                                const struct WBlocks *wblock,
                                const struct WTracks *wtrack,
                                const r::Note *note,
                                const struct NodeLine2 *nodelines)
{
  struct Patch *patch = wtrack->track->patch;
  STime note_time = Ratio2STime2(wblock->block, note->get_time(), wtrack->track);

  float track_volume =  wtrack->track->volumeonoff ? (float)wtrack->track->volume / MAXTRACKVOL : 1.0f;
  //float velocity = scale(n,0,num_peaks, velocity1->velocity, velocity2->velocity);

  const double reltempo = ATOMIC_DOUBLE_GET(wblock->block->reltempo);
    
  const int num_channels=PATCH_get_peaks(patch, 0,
                                         -1,
                                         wtrack->track,
                                         0,0,
                                         NULL,NULL
                                         );
    

  const float min_width = R_MAX(0.5, (float)window->fontheight / 31.0f);
  const float num_lines_per_peak = R_MAX(2.7, (float)window->systemfontheight / 6.5f);

  GE_Context *c = NULL;
  
  //GE_Context *c = Black_color(); //GE_mix_alpha_z(GE_get_rgb(0), Black_rgb(), 100, 0.7, Z_ZERO);
  //GE_Context *c = GE_mix_alpha_z(GE_get_rgb(0), GE_get_rgb(2), 250, 0.9, Z_ZERO);


  for(int ch=0;ch<num_channels;ch++){

    GE_ScopedTrianglestrip trianglestrip;

    for(const struct NodeLine2 *ns = nodelines ; ns!=NULL ; ns=ns->next){
      int logtype = ns->logtype;
      float x1 = ns->x1;
      float x2 = logtype==LOGTYPE_HOLD ? ns->x1 : ns->x2;
      float y1 = ns->y1;
      float y2 = ns->y2;

      //printf("y1/y2: %f, %f\n", y1, y2);
      
      c = c!=NULL ? GE_y(c, y1) : GE_mix_color_z(GE_get_rgb(LOW_EDITOR_BACKGROUND_COLOR_NUM), GE_get_rgb(WAVEFORM_COLOR_NUM), 100, GE_Conf(Z_ABOVE(Z_ZERO), y1));

      const STime time1 = Ratio2STime2(wblock->block, ns->time1, wtrack->track) - note_time;
      const STime time2 = Ratio2STime2(wblock->block, ns->time2, wtrack->track) - note_time;

      R_ASSERT_NON_RELEASE(time2 >= time1);
      
      if (time1>=time2)
        continue;
          
      float velocity1 = scale(x1, subtrack_x1, subtrack_x2, 0, 1);
      float velocity2 = scale(x2, subtrack_x1, subtrack_x2, 0, 1);
      
      int num_peaks = R_MAX(2, (y2-y1) / num_lines_per_peak); // Must have at least two points in time to draw a waveform.

      int64_t last_end_time = time1;
                                
      for(int n=0; n < num_peaks ; n++){

        float min,max;
        
        int64_t start_time = last_end_time;

        int64_t end_time   = R_MIN(time2,
                                   scale(n+1,
                                         0, num_peaks-1+1,
                                         time1, time2)
                                   );
        
        
        //printf("  %d/%d: %d -> %d (diff: %d) (full: %d -> %d)\n", n, num_peaks, (int)start_time, (int)end_time, (int)(end_time-start_time), (int)time1, (int)time2);

        R_ASSERT_NON_RELEASE(end_time >= start_time);

        int64_t reltempo_start_time = start_time / reltempo;
        int64_t reltempo_end_time = end_time / reltempo;
        
        if (reltempo_start_time>=reltempo_end_time)
          continue; // Playing too fast. No audio data. (note that the current value of 'last_end_time' is kept in the next iteration)
        
        //if (n==0)
        //printf("start_time: %d, time1: %d. end_time: %d, time2: %d\n",(int)start_time, (int)time1, (int)end_time, (int)time2);
        

        PATCH_get_peaks(patch, 
                        note->get_val(),
                        ch,
                        wtrack->track,
                        reltempo_start_time,
                        reltempo_end_time,
                        &min,
                        &max);

        float velocity = (float)scale(n,
                                      0,num_peaks-1,
                                      velocity1, velocity2);

        float bound_x1 = scale(scale(ch,
                                     0,num_channels,
                                     0.0f,velocity),
                               0, 1,
                               subtrack_x1, subtrack_x2);
        float bound_x2 = scale(scale(ch+1,
                                     0,num_channels,
                                     0.0f,velocity),
                               0, 1,
                               subtrack_x1, subtrack_x2);

        float x1 = scale(min*track_volume, -1,1, bound_x1, bound_x2);
        float x2 = scale(max*track_volume, -1,1, bound_x1, bound_x2);
          
        //float y = y1 + n*NUM_LINES_PER_PEAK;
        float y = scale(n,
                        0, num_peaks-1,
                        y1, y2);

        //auto *old_c = c;

        c = GE_y(c, y); // Optimization. Split waveforms into several slices.

        /*
        if (c != old_c){
          printf("Changing C at %f\n", y);
        }
        */
#if 0
        printf("Adding %f,%f at %f. min/max: %f/%f. vel1/vel2: %f/%f. time1/time2: %f/%f\n",x1,x2,y,min,max,
               scale(n,0,num_peaks,velocity1->velocity, velocity2->velocity),
               scale(n+NUM_LINES_PER_PEAK,0,num_peaks,velocity1->velocity, velocity2->velocity),
               scale(n,0,num_peaks,time1,time2) / reltempo,
               scale(n+NUM_LINES_PER_PEAK,0,num_peaks,time1,time2) / reltempo);
#endif

        //printf("   tr.y: %f\n", y);
        trianglestrip.add(c, R_MAX(subtrack_x1, x1-min_width), y); // Subtract a little bit (min_width) so that we see a thin line instead of nothing when there's no sound
        trianglestrip.add(c, R_MIN(subtrack_x2, x2+min_width), y); // Same here.
        
        last_end_time = end_time;
        
      } // end num peaks iteration

    } // end node iteration

  } // end ch iteration
}

static void create_velocity_gradient_background(
                                                float area_y1,
                                                float area_y2,
                                                float start_note,
                                                float end_note,
                                                const struct NodeLine2 *velocity_nodelines
                                                )
{

  GE_Rgb rgb1 = get_note_color(start_note);
  GE_Rgb rgb2 = get_note_color(end_note);

  GE_Context *c = NULL;

  const struct NodeLine2 *nodeline = velocity_nodelines;
  while (nodeline != NULL){
    float vel_y1 = nodeline->y1;
    float vel_y2 = nodeline->y2;

    if (vel_y1>=area_y2)
      break;

    bool is_inside = vel_y1>=area_y1 || vel_y2>=area_y1;

    if (is_inside && !equal_floats(vel_y1,vel_y2)){

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
        c = GE_gradient_z(rgb1, rgb2, GE_Conf(Z_BELOW(Z_ZERO), y1));
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

static void create_velocities_gradient_background(
                                                  const struct NodeLine2 *pitch_nodelines,
                                                  const struct NodeLine2 *velocity_nodelines,
                                                  const float track_pitch_min, const float track_pitch_max
                                                  )
{
  const struct NodeLine2 *nodeline = pitch_nodelines;

  while(nodeline != NULL){
    int logtype = nodeline->logtype;
    float x1 = nodeline->x1;
    float x2 = logtype==LOGTYPE_HOLD ? nodeline->x1 : nodeline->x2;

    float y1 = nodeline->y1;
    float y2 = nodeline->y2;

    float start_note;
    float end_note;

    if (equal_floats(track_notearea_x1, track_notearea_x2)){
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

#if 0
static void create_track_velocities(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const struct Notes *note, const struct NodeLine2 *pitch_nodelines, const r::PitchTimeData::Reader &reader, const float track_pitch_min, const float track_pitch_max) {

  //printf("Note: %s, pointer: %p, subtrack: %d\n",NotesTexts3[(int)note->note],note,note->subtrack);
  subtrack_x1 = GetNoteX1(wtrack,note->polyphony_num);
  subtrack_x2 = GetNoteX2(wtrack,note->polyphony_num);

  if(equal_floats(subtrack_x1, subtrack_x2))
    return;
  
  const struct NodeLine2 *nodelines = GetVelocityNodeLines2(window, wblock, wtrack, note);
  const vector_t *nodes = get_nodeline_nodes2(nodelines, wblock->t.y1);

  // background
  {

    const bool paint_vertical_velocity_gradient = false;
    //const bool paint_vertical_velocity_gradient = true;

    if(paint_vertical_velocity_gradient==false && reader.size()==0 && equal_floats(note->pitch_end, 0.0)) {

      GE_ScopedTrianglestrip trianglestrip;
    
      for(const struct NodeLine2 *ns = nodelines ; ns!=NULL ; ns=ns->next){
        int logtype = ns->logtype;
        float x1 = ns->x1;
        float x2 = logtype==LOGTYPE_HOLD ? ns->x1 : ns->x2;

        GE_Context *c = get_note_background(note->note, false, ns->y1);

        trianglestrip.add(c, subtrack_x1, ns->y1);
        trianglestrip.add(c, x1, ns->y1);
        trianglestrip.add(c, subtrack_x1, ns->y2);
        trianglestrip.add(c, x2, ns->y2);
      }

    }else{
      
      track_notearea_x1 = wtrack->notearea.x;
      track_notearea_x2 = wtrack->notearea.x2;

      create_velocities_gradient_background(
                                            pitch_nodelines,
                                            nodelines,
                                            track_pitch_min, track_pitch_max
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
  if (is_current || g_indicator_node_id!=-1)
    VECTOR_FOR_EACH(const Node2 *, node, nodes){
      if (is_current)
        draw_skewed_box2(window, node->id, VELOCITY1_COLOR_NUM, node->x, node->y - wblock->t.y1, USE_SCISSORS);
      if (node->id==g_indicator_node_id && g_indicator_node_id!=-1)
        draw_node_indicator(node->x, node->y - wblock->t.y1, VELOCITY_TEXT_COLOR_NUM);
    }END_VECTOR_FOR_EACH;

#if 1
  if (g_indicator_node == &note->l && g_indicator_velocity_num!=-1) {
    if (g_indicator_velocity_num >= nodes->num_elements)
      //RError("g_indicator_velocity_node_num(%d) >= nodes->num_elements(%d)",g_indicator_velocity_num,nodes->num_elements);
      printf("g_indicator_velocity_node_num(%d) >= nodes->num_elements(%d)\n",g_indicator_velocity_num,nodes->num_elements); // TODO: Find out why this happens so often.
    else {
      struct Node2 *node = (struct Node2 *)nodes->elements[g_indicator_velocity_num];
      draw_node_indicator(node->x, node->y-wblock->t.y1, VELOCITY_TEXT_COLOR_NUM);
      //printf("  Drawing vel indicator. Note: %f. velnum: %d. x: %f. y: %f\n", note->note, g_indicator_velocity_num, node->x, node->y);
    }
  }
#endif
}
#endif

static void create_track_velocities2(const struct Tracker_Windows *window,
                                     const struct WBlocks *wblock,
                                     const struct WTracks *wtrack,
                                     const r::Note *note,
                                     const struct NodeLine2 *pitch_nodelines,
                                     const r::PitchTimeData::Reader &reader,
                                     const float track_pitch_min,
                                     const float track_pitch_max)
{

  //printf("Note: %s, pointer: %p, subtrack: %d\n",NotesTexts3[(int)note->note],note,note->subtrack);
  subtrack_x1 = GetNoteX1(wtrack,note->d._polyphony_num);
  subtrack_x2 = GetNoteX2(wtrack,note->d._polyphony_num);

  if(equal_floats(subtrack_x1, subtrack_x2))
    return;
  
  const struct NodeLine2 *nodelines = GetVelocityNodeLines3(window, wblock, wtrack, note);
  const vector_t *nodes = get_nodeline_nodes2(nodelines, wblock->t.y1);

  // background
  {

    const bool paint_vertical_velocity_gradient = false;
    //const bool paint_vertical_velocity_gradient = true;

    if(paint_vertical_velocity_gradient==false && reader.size()==0 && equal_floats(note->d._pitch_end, 0.0)) {

      GE_ScopedTrianglestrip trianglestrip;
    
      for(const struct NodeLine2 *ns = nodelines ; ns!=NULL ; ns=ns->next){
        int logtype = ns->logtype;
        float x1 = ns->x1;
        float x2 = logtype==LOGTYPE_HOLD ? ns->x1 : ns->x2;

        GE_Context *c = get_note_background(note->get_val(), false, ns->y1);

        trianglestrip.add(c, subtrack_x1, ns->y1);
        trianglestrip.add(c, x1, ns->y1);
        trianglestrip.add(c, subtrack_x1, ns->y2);
        trianglestrip.add(c, x2, ns->y2);
      }

    }else{
      
      track_notearea_x1 = wtrack->notearea.x;
      track_notearea_x2 = wtrack->notearea.x2;

      create_velocities_gradient_background(
                                            pitch_nodelines,
                                            nodelines,
                                            track_pitch_min, track_pitch_max
                                            );
    }
  }

  bool is_current = wblock->mouse_note==note;
  
  // border
  {

    GE_Context *c2 = drawNodeLines(window, nodelines, BLACK_COLOR_NUM, is_current, 0.3, 0.6, false);

    // draw horizontal line where note starts, if it doesn't start on the start of a realline.
    int realline = FindReallineForRatio(wblock, 0, note->get_time());
    if (place2ratio(wblock->reallines[realline]->l.p) != note->get_time()){
      GE_Context *c = GE_y(c2, nodelines->y1);
      GE_line(c, subtrack_x1, nodelines->y1, nodelines->x1, nodelines->y1, get_nodeline_width(is_current));
    }
  }

  // peaks
  if(TRACK_has_peaks(wtrack->track))
    create_track_peaks2(window, wblock, wtrack, note, nodelines);

  // nodes
  if (is_current || g_indicator_node_id!=-1)
    VECTOR_FOR_EACH(const Node2 *, node, nodes){
      if (is_current)
        draw_skewed_box2(window, node->id, VELOCITY1_COLOR_NUM, node->x, node->y - wblock->t.y1, USE_SCISSORS);
      if (node->id==g_indicator_node_id && g_indicator_node_id!=-1)
        draw_node_indicator(node->x, node->y - wblock->t.y1, VELOCITY_TEXT_COLOR_NUM);
    }END_VECTOR_FOR_EACH;

  if (g_indicator_node_id == note->get_node_id() && g_indicator_velocity_num!=-1) {
    if (g_indicator_velocity_num >= nodes->num_elements)
      //RError("g_indicator_velocity_node_num(%d) >= nodes->num_elements(%d)",g_indicator_velocity_num,nodes->num_elements);
      printf("g_indicator_velocity_node_num(%d) >= nodes->num_elements(%d)\n",g_indicator_velocity_num,nodes->num_elements); // TODO: Find out why this happens so often.
    else {
      struct Node2 *node = (struct Node2 *)nodes->elements[g_indicator_velocity_num];
      draw_node_indicator(node->x, node->y-wblock->t.y1, VELOCITY_TEXT_COLOR_NUM);
      //printf("  Drawing vel indicator. Note: %f. velnum: %d. x: %f. y: %f\n", note->note, g_indicator_velocity_num, node->x, node->y);
    }
  }
}


static void create_track_fxs(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const struct FXs *fxs){
  const struct NodeLine2 *nodelines = GetFxNodeLines(window, wblock, wtrack, fxs);

  bool is_current = wblock->mouse_track==wtrack->l.num && wblock->mouse_fxs==fxs;

  drawNodeLines(window, nodelines, fxs->fx->color, is_current, 0.6, 1.0, true);
  
  if (g_indicator_node_id >= 0 || is_current) {
    const vector_t *nodes = get_nodeline_nodes2(nodelines, wblock->t.y1);

    VECTOR_FOR_EACH(const Node2 *, node, nodes){
      if (wblock->mouse_track==wtrack->l.num && wblock->mouse_fxs==fxs)
        draw_skewed_box2(window, node->id, TEXT_COLOR_NUM, node->x, node->y - wblock->t.y1, USE_SCISSORS);
      if (node->id==g_indicator_node_id)
        draw_node_indicator(node->x, node->y - wblock->t.y1, fxs->fx->color);
    }END_VECTOR_FOR_EACH;

  }
}

static void create_track_stops(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack){

  float reallineF = 0.0f;

#if 1
  r::StopTimeData::Reader reader(wtrack->track->stops2);
  for(const r::Stop &stop : reader) {
    Place place = ratio2place(stop._time);
    reallineF = FindReallineForF(wblock, reallineF, &place);
    float y = get_realline_y(window, reallineF); 
    GE_Context *c = GE_color_alpha(TEXT_COLOR_NUM, 0.19, y);
    GE_line(c,
            wtrack->notearea.x, y,
            wtrack->x2, y,
            get_thickness(1.2)
            );
  }
#else
  struct Stops *stops = wtrack->track->stops;
  
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
#endif
}

static void create_track_is_recording(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack){
  GE_Context *c = GE_z(GE_alpha(GE_get_rgb(RED_COLOR_NUM), 0.7), GE_Conf(Z_STATIC, NOMASK_Y));

  GE_text(c, "Rec", wtrack->x, 0);
}

static void create_track_is_disabled_in_seqblock(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack){
  GE_Context *c = GE_z(GE_alpha(GE_get_rgb(WHITE_COLOR_NUM), 0.7), GE_Conf(Z_STATIC, NOMASK_Y));

  float y2 = get_scrollbar_y2(window, wblock);
  
  GE_line(c,
          wtrack->x, 0,
          wtrack->x2, y2,
          2.3);
          
  GE_line(c,
          wtrack->x, y2,
          wtrack->x2, 0,
          2.3);
}


static void create_track_veltext2(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, int realline, char v1, char v2, char v3, bool is_first, bool is_last){

  char text[]={v1, v2, v3, '\0'};

  float x = wtrack->veltextarea.x;
  int y1 = get_realline_y1(window, realline);

  GE_Context *c;
  if (is_first)
    c = GE_textcolor(VELOCITY_TEXT_COLOR_NUM, y1);
  else if (is_last)
    c = GE_textcolor(LAST_VELOCITY_TEXT_COLOR_NUM, y1);
  else
    c = GE_textcolor(MIDDLE_VELOCITY_TEXT_COLOR_NUM, y1);
  
  GE_text(c, text, x, y1);
}
  
static void create_track_veltext(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const VelText_trs &trs, int realline){
  int num_elements = trs.size();
  
  if (num_elements == 0)
    return;

  if (num_elements > 1){
    bool is_first = false;
    bool is_last = false;
    
    for(auto tr : trs){
      if (tr.is_first_velocity){
        is_first=true;
        break;
      }
      if (tr.is_last_velocity){
        is_last=true;
      }
    }

    create_track_veltext2(window, wblock, wtrack, realline, 'x', 'x', 'x', is_first, is_last);
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

  create_track_veltext2(window, wblock, wtrack, realline, v1, v2, v3, vt.is_first_velocity, vt.is_last_velocity);
}


static void create_track_fxtext2(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, int realline, ColorNums colornum, int column, char v1, char v2, char v3){

  char text[]={v1, v2, v3, '\0'};

  float x = wtrack->fxtextarea.x + (column * WTRACK_fxtext_track_width(window->fontwidth));
  int y1 = get_realline_y1(window, realline);

  GE_Context *c = GE_textcolor(colornum, y1);
  
  GE_text(c, text, x, y1);
}

static void create_track_fxtext(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const FXText_trs &trs, int realline, int column){
  int num_elements = trs.size();

  if (num_elements == 0)
    return;

  const r::FXText &vt = trs[0];

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

static void create_track(const struct Tracker_Windows *window, const struct WBlocks *wblock, struct WTracks *wtrack){
  create_track_borders(window, wblock, wtrack);

  const float track_pitch_min = wtrack->track->_notes2->_min_display_pitch;
  const float track_pitch_max = wtrack->track->_notes2->_max_display_pitch;
  
  // velocities and pitches
#if 0
  {  
    const struct Notes *note=wtrack->track->gfx_notes!=NULL ? wtrack->track->gfx_notes : wtrack->track->notes;
    while(note != NULL){

      const r::PitchTimeData::Reader pitch_reader(note->_pitches);
      
      const struct NodeLine2 *pitch_nodelines = GetPitchNodeLines2(window, wblock, wtrack, note,track_pitch_min, track_pitch_max, pitch_reader);

      if (wtrack->notesonoff==1)
        create_pitches(window, wblock, wtrack, note, pitch_nodelines);
      
      create_track_velocities(window, wblock, wtrack, note, pitch_nodelines, pitch_reader, track_pitch_min, track_pitch_max);
      note = NextNote(note);
    }
  }
#else
  {
    r::NoteTimeData::Reader reader(wtrack->track->_notes2);
    for(const r::NotePtr &note : reader) {

      const r::PitchTimeData::Reader pitch_reader(&note->_pitches);
      
      const struct NodeLine2 *pitch_nodelines = GetPitchNodeLines3(window, wblock, wtrack, note.get(), track_pitch_min, track_pitch_max, pitch_reader);

      if (wtrack->notesonoff==1)
        create_pitches2(window, wblock, wtrack, note.get(), pitch_nodelines);
      
      create_track_velocities2(window, wblock, wtrack, note.get(), pitch_nodelines, pitch_reader, track_pitch_min, track_pitch_max);
      
    }
  }
  
#endif
  
  // note/pitch names / cents
  if( (wtrack->notesonoff==1) || wtrack->centtext_on) {

    bool show_notes = wtrack->notesonoff==1;
    
    const Trss &trss = TRSS_get(wblock, wtrack);

    auto i = trss.constBegin();
    while (i != trss.constEnd()) {
      create_track_text(window, wblock, wtrack, i.value(), i.key(), show_notes);
      ++i;
    }
  }

  if (wtrack->swingtext_on){
    create_swingtrack(window, wblock, wtrack->track->filledout_swings.array, wtrack->swingtext_fits_reallines, wtrack->swingtextarea.x, USE_SCISSORS);
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
      if (fxs->fx->is_enabled){
        const FXText_trss &fxtexts = FXTEXTS_get(wblock, wtrack, fxs);
        auto i = fxtexts.constBegin();
        while (i != fxtexts.constEnd()){
          create_track_fxtext(window, wblock, wtrack, i.value(), i.key(), column);
          ++i;
        }
        column++;
      }
    }END_VECTOR_FOR_EACH;

  }
  
  // fxs
  VECTOR_FOR_EACH(const struct FXs *, fxs, &wtrack->track->fxs){
    if(fxs->fx->is_enabled)
      create_track_fxs(window, wblock, wtrack, fxs);
  }END_VECTOR_FOR_EACH;

  // stop lines
  create_track_stops(window, wblock, wtrack);

  // piano roll
  create_pianoroll(window, wblock, wtrack);

  // rec.
  if (ATOMIC_GET(wtrack->track->is_recording))
    create_track_is_recording(window, wblock, wtrack);

  // disabled in seqblock (two white crossing diagonal lines)
  if (wtrack->l.num < MAX_DISABLED_SEQBLOCK_TRACKS){
    if (is_playing() && pc->playtype==PLAYSONG){
      struct SeqBlock *seqblock = RT_get_curr_seqblock();
      
      if(seqblock!=NULL && seqblock->block!=NULL){
        
        R_ASSERT_NON_RELEASE(seqblock->track_is_disabled!=NULL);
        
        if (seqblock->track_is_disabled[wtrack->l.num])
          create_track_is_disabled_in_seqblock(window, wblock, wtrack);
      }
    }
  }
}


static void create_tracks(const struct Tracker_Windows *window, const struct WBlocks *wblock){
  ITERATE_VISIBLE_WTRACKS(wblock)
    create_track(window, wblock, const_cast<struct WTracks*>(wtrack));
}

static void create_range(const struct Tracker_Windows *window, const struct WBlocks *wblock){

  if (!wblock->range.enabled)
    return;
  
  int tracknum1 = R_MIN(wblock->block->num_tracks-1, wblock->range.x1);
  int tracknum2 = R_MIN(wblock->block->num_tracks-1, wblock->range.x2);

  struct WTracks *wtrack1=(struct WTracks*)ListFindElement1(&wblock->wtracks->l,tracknum1);
  struct WTracks *wtrack2=(struct WTracks*)ListFindElement1(&wblock->wtracks->l,tracknum2);

  //const struct LocalZooms *localzoom1 = wblock->reallines[realline1];
  //const struct LocalZooms *localzoom2 = wblock->reallines[realline2];

  float x1 = wtrack1->x;
  float x2 = wtrack2->x2;

  float realline1 = FindReallineForF(wblock, 0, &wblock->range.y1);
  float realline2 = FindReallineForF(wblock, realline1, &wblock->range.y2);
  int y1 = get_realline_y(window, realline1);
  int y2 = get_realline_y(window, realline2)-1;

  //printf("realline1: %f, realline2: %f, y1: %d, y2: %d\n", realline1, realline2, y1, y2);
  
  GE_Rgb rgb = GE_get_rgb(RANGE_COLOR_NUM);
  if (rgb.a==0xff)
    rgb.a = 0x80;
  
  GE_Context *c = GE_z(rgb, GE_Conf(Z_MAX_SCROLLTRANSFORM, y1));

  GE_filledBox(c, //GE_mix_alpha_z(GE_get_rgb(color), White_rgb(), 300, 0.3, z),
               x1,y1,
               x2,y2
               );
}

static void create_cursor(const struct Tracker_Windows *window, const struct WBlocks *wblock){
  
  const GE_Conf conf(Z_STATIC, NOMASK_Y, NO_SCISSORS);
  GE_Context *c = GE_z(GE_alpha(GE_get_rgb(ATOMIC_GET(root->editonoff)?CURSOR_EDIT_ON_COLOR_NUM:CURSOR_EDIT_OFF_COLOR_NUM), 0.2), conf);

  NInt track    = window->curr_track;
  int  subtrack = window->curr_track_sub;

  if(window->wblock!=wblock){
    track = 0;
    subtrack = -1;
  }

  if (track >= wblock->block->num_tracks){
    R_ASSERT_NON_RELEASE(false);
    track = wblock->block->num_tracks - 1;
  }
  
  int xb1 = GetXSubTrack_B1(wblock,track,subtrack)-1;
  int xb2 = GetXSubTrack_B2(wblock,track,subtrack)+1;

  bool curr_pos_invisible = false;

  if (window->curr_track >=0){
    if (xb2 < wblock->t.x1)
      curr_pos_invisible = true;
    else{
      xb1 = R_MAX(xb1, wblock->t.x1);
      xb2 = R_MAX(xb2, wblock->t.x1);
    }
  }

  int x1 = window->leftslider.width;
  int x2 = xb1;
  int x3 = xb2;
  int x4 = window->width;

  int dy = wblock->t.y1;
  int y1 = GetCursorY1Pos(window, wblock) - dy;
  int y2 = GetCursorY2Pos(window, wblock) - dy;

  if (curr_pos_invisible){
    GE_filledBox(c, 
                 x1, y1,
                 x4, y2
                 );
  } else {
    GE_filledBox(c, 
                 x1, y1,
                 x2, y2
                 );
    
    GE_filledBox(c, 
                 x3, y1,
                 x4, y2
                 );
  }

  {
    const GE_Conf conf(Z_STATIC, y1, NO_SCISSORS);
    float width = 0.8f;

    {
      GE_Context *c = GE_z(GE_get_rgb(CURSOR_BORDER_COLOR_NUM), conf);
      
      GE_box_without_right(c,
                           x1+2,y1,
                           x2+2,y2-1,
                           width
                           );
      GE_box_without_left(c,
                          x3,y1,
                          x4-3,y2-1,
                          width
                          );
    }

    {
      const GE_Conf conf2(Z_ABOVE(Z_STATIC), y1, NO_SCISSORS);
      GE_Context *c = GE_z(GE_alpha(White_rgb(), 0.05), conf2);
      GE_filledBox(c, 
                   x2+2, y1,
                   x3, y2-1
                   );
    }

    GE_box(GE_z(GE_get_rgb(CURSOR_CURR_COLUMN_BORDER_COLOR_NUM), conf),
           x2+1,y1,
           x3,y2-1,
           width
           );

  }

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
    
    GE_Context *c = GE_z(GE_alpha(GE_get_rgb(PLAY_CURSOR_COLOR_NUM), 0.3), GE_Conf(Z_PLAYCURSOR, NOMASK_Y, NO_SCISSORS));
    
    int x1 = window->leftslider.width;
    int x2 = window->width;
    
    int dy = wblock->t.y1;
    int y = GetCursorY1Pos(window, wblock) - dy;
    
    GE_line(c,
            x1, y,
            x2, y,
            get_thickness(4.0)
            );
  }
}

static void create_current_barbeat_mark(const WSignature_trss &wsignatures_trss, const struct Tracker_Windows *window, const struct WBlocks *wblock){
  if (g_current_barbeat_block_num != wblock->l.num) // Don't need to check for more. g_current_barbeat_block_num is always -1 if there is no current bar or beat.
    return;

  Place start,end;
  if (get_barbeat_start_and_end(wblock->block, g_current_bar_num, g_current_beat_num, &start, &end)==false){
    //R_ASSERT_NON_RELEASE(false); // Happens, for instance, when deleting last bar.
    return;
  }
                            
  float start_reallineF = FindReallineForF(wblock, 0, &start);
  float y1 = get_realline_y(window, start_reallineF);

  float end_reallineF = FindReallineForF(wblock, start_reallineF, &end);
  float y2 = get_realline_y(window, end_reallineF);

  GE_Context *c = GE_color_z(CURRENT_BEAT_RANGE_COLOR_NUM,
                             GE_Conf(Z_ABOVE(Z_ABOVE(Z_ABOVE(Z_ZERO))),y1,NO_SCISSORS)
                             );
  
  //GE_Context *c = GE_y(GE_rgba(10,10,100,120), y1);
  GE_filledBox(c, 0, y1, window->width, y2);
}

static void create_message(const struct Tracker_Windows *window, QString message){

  int width = message.length() * window->fontwidth;
  int height = window->fontheight;
  
  int middle_x = window->width / 2;
  int middle_y = window->height / 2;

  int x1 = middle_x - width;
  int x2 = middle_x + width;

  int y1 = middle_y - height;
  int y2 = middle_y + height;
  
  const GE_Conf conf(Z_STATIC, NOMASK_Y, NO_SCISSORS);

  GE_Context *background = GE_z(Black_rgb(), conf);
  GE_filledBox(background, x1, y1, x2, y2);

  GE_Context *border = GE_z(White_rgb(), conf);
  GE_box(border, x1, y1, x2, y2, 1.0);

  int x = middle_x - width/2;
  int y = middle_y - height/2;
  
  GE_Context *text_color = GE_z(White_rgb(), conf);
  GE_text2(text_color, message, x, y);
}

static void create_lacking_keyboard_focus_greyed_out(const struct Tracker_Windows *window){
  if (g_do_grey_editor){
    const GE_Conf conf(Z_STATIC, NOMASK_Y, NO_SCISSORS);
    //GE_Context *grey = GE_z(GE_rgba(100,100,100,120), conf);
    GE_Context *grey = GE_z(GE_get_rgb(EDITOR_GRAYED_OUT_COLOR_NUM), conf);
    GE_filledBox(grey, 0, 0, window->width, window->height);
  }
}

/************************************
   block
 ************************************/

#include <thread>

static void GL_create2(const struct Tracker_Windows *window, struct WBlocks *wblock){

  if (g_gl_widget_started==false) // This check is probably not necessary
    return;
  
  init_g_colored_tracks_if_necessary();
  
  //static int n=0; printf("GL_create called %d\n",n++);

  const bool block_is_visible = wblock != NULL;
  
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
        create_swingtrack(window, wblock, wblock->block->filledout_swings.array, wblock->swingtext_fits_reallines, wblock->swingTypearea.x, NO_SCISSORS);
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
      create_current_barbeat_mark(wsignatures_trss, window, wblock);
    }

    {
      static const char *is_pausing_message = "Current seqtrack is pausing.";
      const char *old_message = window->message;
      const char *new_message = window->message;
      
      if ((old_message==NULL || old_message==is_pausing_message) && block_is_visible==false) {

        SeqTrack *curr_seqtrack = RT_get_curr_seqtrack();
        
#if !defined(RELEASE)
        {
          const SeqBlock *curr_seqblock = RT_get_curr_seqblock2(curr_seqtrack);
          if (curr_seqblock!=NULL)
            if (curr_seqblock->block==NULL)
              abort();
        }
#endif

        const SeqBlock *curr_sample_seqblock = RT_get_curr_sample_seqblock2(curr_seqtrack);

        if (curr_sample_seqblock != NULL){
          filepath_t filename = get_seqblock_sample_name(curr_seqtrack, curr_sample_seqblock, false);
          if (isIllegalFilepath(filename)){
            R_ASSERT(false);
            new_message = "error";
          }else{
            create_message(window, QString("Playing ") + STRING_get_qstring(filename.id));
            goto gotit;
          }
        }else
          new_message = is_pausing_message;
      }
      
      if (new_message != NULL)
        create_message(window, new_message);
    }

 gotit:
  
    create_lacking_keyboard_focus_greyed_out(window);
    
  } GE_end_writing(GE_get_rgb(LOW_EDITOR_BACKGROUND_COLOR_NUM));
}


#define RENDER_IN_SEPARATE_THREAD 0


#if !RENDER_IN_SEPARATE_THREAD

void GL_create(const struct Tracker_Windows *window){
#if 1 //defined(RELEASE)
  GL_create2(window, window->curr_block < 0 ? NULL : window->wblock);
#else
  static int num=0;
  double start = TIME_get_ms();
  GL_create2(window, window->curr_block < 0 ? NULL : window->wblock);
  printf("   GL_create %d. dur: %f\n", num++, TIME_get_ms() - start);
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

#if defined(FOR_LINUX)
  pthread_setname_np(pthread_self(), "gl_create_thread");
#endif

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
      GL_create2(window, wblock);
      GE_wait_until_block_is_rendered();
    }
    wblock = NextWBlock(wblock);
  }

  GL_create2(window, window->wblock);
    
  g_is_creating_all_GL_blocks = false;
  ATOMIC_SET(g_is_creating_all_GL_blocks, false);

}
