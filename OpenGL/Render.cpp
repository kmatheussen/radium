#include <math.h>


#include "../common/nsmtracker.h"
#include "../common/settings_proc.h"
#include "../common/list_proc.h"
#include "../common/placement_proc.h"
#include "../common/realline_calc_proc.h"
#include "../common/gfx_subtrack_proc.h"
#include "../common/time_proc.h"
#include "../common/tracks_proc.h"
#include "../common/patch_proc.h"
#include "../common/common_proc.h"
#include "../common/trackreallines2_proc.h"
#include "../common/notes_proc.h"
#include "../common/nodelines_proc.h"
#include "../common/Signature_proc.h"
#include "../common/LPB_proc.h"
#include "../common/tempos_proc.h"

#include "GfxElements.h"


#include "Render_proc.h"


// Functions in this file are called from the main thread.


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
    width = 1.3; //SETTINGS_read_double("gfx_nodeline_width", 1.0); // If changing this value, also change bin/config

  return width;
}

static float get_nodeline_width(bool is_selected){
  float width = get_nodeline_width();

  if (is_selected)
    return width * 1.8;
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
  
#if 1
  int z = GE_get_z(c);
    
  int x2=x+(strlen(text)*window->fontwidth);

#if 0
  int y2=y+window->fontheight-1;
  // 1. Line
  GE_line(GE_color_z(9, z),
          x,y,x,y2,0.5f);
#endif
  
  // 2. Line (the gradient one)
  QColor qc1 = GE_qcolor(9).darker(96);
  QColor qc2 = GE_qcolor(9).darker(113);
  GE_Context *c2 = GE_gradient_z(qc1, qc2, z); //GE_get_rgb(9), GE_get_rgb(11), z);

#if 0  
  GE_line(c2,
          x,y+1,x2,y+1,0.5f);
#else
  GE_gradient_triangle_start(GradientType::HORIZONTAL);
  GE_gradient_triangle_add(c2, x,  y+0.75 - 0.5);
  GE_gradient_triangle_add(c2, x2, y+0.75 - 0.5);
  GE_gradient_triangle_add(c2, x,  y+1.25);
  GE_gradient_triangle_add(c2, x2, y+1.25);
  GE_gradient_triangle_end(c2, x,  x2);
#endif
  
  // 3. More lines
  #if 0
  GE_Context *c3 = GE_mix_color_z(GE_get_rgb(11), GE_get_rgb(1), 800, z);

  GE_line(c3, x2,y, x2,y2, 0.5f);
  GE_line(c3, x,y2, x2,y2, 0.5f);
  #endif
#endif
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

  length2=length-strlen(temp);  
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
  GE_Context *c = GE_color_alpha_z(14, 0.6, Z_MAX_SCROLLTRANSFORM);
  
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
                            int color,
                            float x, float y
                            )
{
 
  float minnodesize = window->fontheight / 1.5; // if changing 1.5 here, also change 1.5 in getHalfOfNodeWidth in api_mouse.c
  float x1 = x-minnodesize;
  float x2 = x+minnodesize;
  float y1 = y-minnodesize;
  float y2 = y+minnodesize;
  const float width = 1.2;

  int z = Z_ABOVE(Z_ZERO);

  if (node == current_node) {
    GE_filledBox(GE_mix_alpha_z(GE_get_rgb(color), GE_get_rgb(2), 300, 0.3, z),
                 x1,y1,
                 x2-1,y2
                 );
  }

  // vertical left
  GE_line(GE_mix_alpha_z(GE_get_rgb(color), GE_get_rgb(2), 100, 0.3, z),
          x1+1, y1+1,
          x1+2,y2-1,
          width);

  // horizontal bottom
  GE_line(GE_mix_alpha_z(GE_get_rgb(color), GE_get_rgb(1), 300, 0.3, z),
          x1+2,y2-1,
          x2-1,y2-2,
          width);

  // vertical right
  GE_line(GE_mix_alpha_z(GE_get_rgb(color), GE_get_rgb(1), 400, 0.3, z),
          x2-1,y2-2,
          x2-2,y1+2,
          width);

  // horizontal top
  GE_line(GE_mix_alpha_z(GE_get_rgb(color), GE_get_rgb(2), 300, 0.3, z),
          x2-2,y1+2,
          x1+1,y1+1,
          width);
}


void create_double_border(
                          int x, int y, int y2
                          )
{
  GE_line(GE_color(1),x,y,x,y2,0.5);
  GE_line(GE_color(9),x+1,y,x+1,y2,0.5);
}

void create_single_border(
                          int x, int y, int y2
                          )
{
  GE_line(GE_color(7),x,y,x,y2,0.5);
}






static GE_Context *drawNodeLines(const struct NodeLine *nodelines, int colnum, bool is_selected, float alpha, float alpha_selected){
  float width = get_nodeline_width(is_selected);
  GE_Context *c = GE_color_alpha_z(colnum, is_selected ? alpha_selected : alpha, Z_ABOVE(Z_ABOVE(Z_ZERO)));
  
  for(const struct NodeLine *ns = nodelines ; ns!=NULL ; ns=ns->next)
    GE_line(c, ns->x1, ns->y1, ns->x2, ns->y2, width);

  return c;
}




/************************************
   Left slider
 ************************************/

static void create_left_slider(const struct Tracker_Windows *window, const struct WBlocks *wblock){
  GE_Context *border = GE_color_z(1, Z_STATIC);

  GE_box(border,
         0,                        get_scrollbar_y1(window, wblock),
         window->leftslider.width, get_scrollbar_y2(window, wblock),
         1.0f);


  GE_Context *scrollbar = GE_mix_color_z(GE_get_rgb(1), GE_get_rgb(0), 700, Z_SCROLLBAR);

  GE_filledBox(scrollbar,
               2,                            0,
               window->leftslider.width - 2, get_scrollbar_scroller_height(window,wblock)
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

static void create_background_realline(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WSignatures *wsignature, int realline){

  const struct WTracks *last_wtrack = (const struct WTracks*)ListLast1(&wblock->wtracks->l);

  int x1 = wblock->linenumarea.x;
  int x2 = last_wtrack->x2;
  int y1 = get_realline_y1(window, realline);
  int y2 = get_realline_y2(window, realline);

  // background
  {
    if(beat_opacity == -1)
      beat_opacity = SETTINGS_read_int("beat_opacity", 950);

    if(first_beat_opacity == -1)
      first_beat_opacity = SETTINGS_read_int("first_beat_opacity", 870);
    
    GE_Context *c;
 
    if (WSIGNATURE_is_first_beat(wsignature))
      c = GE_mix_color_z(GE_get_rgb(15), GE_get_rgb(1), first_beat_opacity, Z_BACKGROUND | Z_STATIC_X);
    else if (wsignature->beat_num>0)
      c = GE_mix_color_z(GE_get_rgb(15), GE_get_rgb(1), beat_opacity, Z_BACKGROUND | Z_STATIC_X);
    else
      c = GE_color_z(15, Z_BACKGROUND | Z_STATIC_X);
    
    GE_filledBox(c,x1,y1,x2,y2);
  }

  float line_width = 0.6f;

  // realline separator line
  if(1){
    if(line_opacity == -1)
      line_opacity = SETTINGS_read_int("line_opacity", 800);
    
    if(line_opacity != 1000) {
      GE_Context *c = GE_mix_color_z(GE_get_rgb(15), GE_get_rgb(1), line_opacity, Z_ABOVE(Z_BACKGROUND) | Z_STATIC_X);

      GE_line(c,x1,y1,x2,y1,line_width);
    }
  }
}


static void create_background(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WSignatures *wsignatures){
  int realline;
  for(realline = 0 ; realline<wblock->num_reallines ; realline++)
    create_background_realline(window, wblock, &wsignatures[realline], realline);
}





/************************************
   Linenumbers
 ************************************/

static void draw_linenumber(const struct Tracker_Windows *window, const struct WBlocks *wblock, int colornum, int linenumber, int realline, bool is_zoomline){
  int y = get_realline_y1(window, realline);
  const int z = Z_LINENUMBERS | Z_STATIC_X;

  GE_Context *c;

  if (colornum<0)
    c = GE_color_alpha_z(-colornum, 0.3, z);
  else
    c = GE_textcolor_z(colornum, z);
  
  int x,width;

  if (is_zoomline) {
    width = wblock->zoomlinearea.width/window->fontwidth;
    x = wblock->zoomlinearea.x;
  } else {
    width = wblock->linenumarea.width/window->fontwidth;
    x = wblock->linenumarea.x;
  }

  draw_text_num(
                window,
                c,
                linenumber,
                width,
                x,
                y
                );
}

static void draw_realline_linenumber(const struct Tracker_Windows *window, const struct WBlocks *wblock, int realline){
  struct LocalZooms *localzoom = wblock->reallines[realline];

  int colornum;

  if(localzoom->level>0){
    if(localzoom->zoomline>0){
      colornum=14; //R_MIN(7,localzoom->level+1);
    }else{
      if(localzoom->level==1){
        colornum=1;
      }else if(localzoom->level==2){
        colornum=14;
      }else{
        colornum=R_MIN(7,localzoom->level);
      }
    }
  }else{
    colornum=1;
  }

  if(localzoom->level>0 && localzoom->zoomline>0){
    if (localzoom->zoomline>1
        || (realline+1 < wblock->num_reallines && wblock->reallines[realline+1]->zoomline==localzoom->zoomline+1)  // only show subline number if there are more than one subline
        )
      draw_linenumber(window,
                      wblock,
                      colornum,
                      localzoom->zoomline,
                      realline,
                      true
                      );
  }else{
    draw_linenumber(window,
                    wblock,
                    colornum,
                    localzoom->Tline,
                    realline,
                    false
                    );
  }  
}

static void create_linenumbers(const struct Tracker_Windows *window, const struct WBlocks *wblock){
  int realline;
  for(realline = 0 ; realline<wblock->num_reallines ; realline++)
    draw_realline_linenumber(window, wblock, realline);
  
  draw_linenumber(window,
                  wblock,
                  -1,
                  wblock->block->num_lines,
                  wblock->num_reallines,
                  false
                  );
}




/************************************
   Tempograph
 ************************************/

struct TempoGraph{
  float line_period;
  int num_points;
  STime *times;
  float min;
  float max;  
};

struct TempoGraph *create_TempoGraph(const struct Tracker_Windows *window, const struct WBlocks *wblock){
  struct TempoGraph *tg = (struct TempoGraph*)talloc(sizeof(struct TempoGraph));

  int TEMPOGRAPH_POINTS_PER_REALLINE = window->fontheight / 2;
  tg->line_period = window->fontheight / (float)TEMPOGRAPH_POINTS_PER_REALLINE;
  tg->num_points  = (wblock->num_reallines * TEMPOGRAPH_POINTS_PER_REALLINE) + 1;
  tg->times       = (STime*)talloc_atomic(tg->num_points*sizeof(STime));

  STime last_time = -1;

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
      STime time = Place2STime(wblock->block,&p);
      if(realline>0 || n>0){
        tg->times[realline*TEMPOGRAPH_POINTS_PER_REALLINE + n - 1] = time-last_time;
        //printf("Setting %d (of %d)\n",realline*TEMPOGRAPH_POINTS_PER_REALLINE + n - 1, tg->num_points);
      }
      last_time = time;
    }
  }
  tg->times[tg->num_points-2] = getBlockSTimeLength(wblock->block) - last_time;
  tg->times[tg->num_points-1] = tg->times[tg->num_points-2];
  

  tg->min=tg->times[0];
  tg->max=tg->times[0];
  for(int n=1;n<tg->num_points;n++){
    STime time = tg->times[n];
    if(tg->min<time)
      tg->min = time;
    if(tg->max>time)
      tg->max = time;
  }

  return tg;
}

static void create_tempograph(const struct Tracker_Windows *window, const struct WBlocks *wblock){
  struct TempoGraph *tg = create_TempoGraph(window,wblock);

  float width = 1.3;
  GE_Context *c = GE_color(5);

  //printf("min/max: %d, %d\n",(int)min,(int)max);

  if(fabs(tg->min - tg->max)<20) {
    float middle = (wblock->tempocolorarea.x+wblock->tempocolorarea.x2) / 2.0f;
    GE_line(c,
            middle, get_realline_y1(window, 0),
            middle, get_realline_y2(window, wblock->num_reallines-1),
            width);
  }else{
    for(int n=0;n<tg->num_points-1;n++){
      GE_line(c, 
              scale(tg->times[n],tg->min,tg->max,wblock->tempocolorarea.x,wblock->tempocolorarea.x2), n * tg->line_period,
              scale(tg->times[n+1],tg->min,tg->max,wblock->tempocolorarea.x,wblock->tempocolorarea.x2), (n+1) * tg->line_period,
              width);
    }
  }
 
}



/************************************
   Time signature track
 ************************************/

static void create_signature(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WSignatures *wsignatures, int realline, int x2){
  int   y         = get_realline_y1(window, realline);
  
  Ratio signature = wsignatures[realline].signature;
  int   beat_num  = wsignatures[realline].beat_num;
  int   type      = wsignatures[realline].type;
    
  if(type!=SIGNATURE_NORMAL){
    int x = wblock->signatureTypearea.x;

    VECTOR_FOR_EACH(float*, f_pointer, &wsignatures[realline].how_much_below){
      float f = *f_pointer;
      float y_ = scale(f, 0, 1, y, get_realline_y2(window, realline));
      GE_line(GE_color_alpha_z(1, 0.4, Z_ZERO),
              x, y_,
              x2, y_,
              0.8
              );

    }END_VECTOR_FOR_EACH;
    /*
    const char *typetext;
    switch(type){
    case SIGNATURE_BELOW:
      typetext="d";
      break;
    case SIGNATURE_MUL:
      typetext="m";
      break;
    default:
      typetext="";
      RError("something is wrong");
    };
    
    GE_text(GE_color_alpha_z(1, 0.3, Z_ZERO), typetext, x, y);
    */
  }

  if(beat_num!=0){
    int x    = wblock->signaturearea.x;    
    char temp[50];

    if (WSIGNATURE_is_measure_change(&wsignatures[realline])) {
      sprintf(temp, "%d/%d", signature.numerator, signature.denominator);
      GE_text(GE_textcolor_z(1, Z_ZERO),
              temp,
              x,
              y
              );
    } else {
      sprintf(temp, " %d", beat_num);
      GE_text(GE_color_alpha_z(1, 0.3, Z_ZERO),
              temp,
              x,
              y
              );      
    }
  }

}



static void create_signaturetrack(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WSignatures *wsignatures){

  const struct WTracks *last_wtrack = (const struct WTracks*)ListLast1(&wblock->wtracks->l);
  int x2 = last_wtrack->x2;

  int realline;
  for(realline = 0 ; realline<wblock->num_reallines ; realline++)
    create_signature(window, wblock, wsignatures, realline, x2);
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
                  GE_textcolor_z(1, Z_ZERO),
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
    
    GE_text(GE_color_alpha_z(1, 0.3, Z_ZERO), typetext, wblock->lpbTypearea.x, y);
  }
}



static void create_lpbtrack(const struct Tracker_Windows *window, const struct WBlocks *wblock){

  struct WLPBs *wlpbs = WLPBs_get(window, wblock);

  int realline;
  for(realline = 0 ; realline<wblock->num_reallines ; realline++)
    create_lpb(window, wblock, wlpbs, realline);
}




/************************************
   bpm track
 ************************************/

static void create_bpm(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WBPMs *wbpms, int realline){
  int y     = get_realline_y1(window, realline);
  int tempo = wbpms[realline].tempo;
  int type  = wbpms[realline].type;

  if(tempo!=0){
    draw_text_num(
                  window,
                  GE_textcolor_z(1, Z_ZERO),
                  tempo,
                  wblock->tempoarea.width/window->fontwidth,
                  wblock->tempoarea.x,
                  y);
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

    GE_text(GE_color_alpha_z(1, 0.3, Z_ZERO), typetext, wblock->tempoTypearea.x, y);
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

  drawNodeLines(nodelines, 4, is_current, 0.6, 0.9);
  
  if (indicator_node != NULL || is_current) {
    const vector_t *nodes = get_nodeline_nodes(nodelines, wblock->t.y1);

    VECTOR_FOR_EACH(const Node *, node, nodes) {
        if(wblock->mouse_track==TEMPONODETRACK)
        draw_skewed_box(window, node->element, 1, node->x, node->y - wblock->t.y1);
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

  create_double_border(
                       wblock->linenumarea.x2+1,
                       y1,y2
                       );
  
  create_double_border(
                       wblock->zoomlinearea.x2+1,
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

  if (window->show_lpb_track)
    create_double_border(
                         wblock->lpbarea.x2+1,
                         y1,y2
                         );

  if (window->show_bpm_track)
    create_single_border(
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

  if(left_subtrack==-1)
    create_single_border(
                         wtrack->notearea.x2+1,
                         y1,
                         y2);

  for(int lokke=R_MAX(1, left_subtrack) ; lokke < wtrack->track->num_subtracks ; lokke++){
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
    rgb = GE_mix(GE_get_rgb(5), GE_get_rgb(1), scale(notenum,0,split1,0,1000));
  else if(notenum<split2)
    rgb = GE_mix(GE_get_rgb(6), GE_get_rgb(5), scale(notenum,split1,split2,0,1000));
  else
    rgb = GE_mix(GE_get_rgb(2), GE_get_rgb(6), scale(notenum,split2,160,0,1000));

  rgb = GE_alpha(rgb, 0.7);

  return rgb;
}

static GE_Context *get_note_background(float notenum, bool highlight){

  GE_Rgb rgb = get_note_color(notenum);
    
  if (highlight)
    rgb = GE_mix(rgb, GE_get_rgb(2), 650);

  return GE(rgb);
  //return GE_gradient(rgb, GE_get_rgb(0));
}

static float get_notenum(TrackRealline2 *tr2){
  if (tr2->pitch != NULL)
    return tr2->pitch->note;

  else if (tr2->note != NULL)
    return tr2->note->note;

  else
    return NOTE_STP;
}
                         
static float get_notenum(vector_t *trs){
  if (trs->num_elements==0)
    return 0;

  if (trs->num_elements>1)
    return NOTE_MUL;

  TrackRealline2 *tr2 = (TrackRealline2*)trs->elements[0];

  return get_notenum(tr2);
}

static int get_colnum(TrackRealline2 *tr2, bool isranged){
  if (tr2!=NULL && tr2->pitch != NULL)
    return 14;
  else
    return 1;
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

static void paint_tr2(TrackRealline2 *tr2, char **NotesTexts, int num, bool isranged, int x, int y){
  GE_Context *c = GE_textcolor(get_colnum(tr2, isranged));
  float notenum = get_notenum(tr2);
  paint_halfsize_note(c, num, NotesTexts[(int)notenum], x, y);
  //GE_text_halfsize(foreground, NotesTexts[(int)notenum1], x, y);
}

static void paint_multinotes(const struct WTracks *wtrack, vector_t *tr, char **NotesTexts, bool isranged, int y1, int y2){
  int num_elements = tr->num_elements;

  int x1 = wtrack->notearea.x;
  int y_middle = (y1+y2)/2;

  paint_tr2((TrackRealline2*)tr->elements[0], NotesTexts, 1, isranged, x1, y1);
  
  paint_tr2((TrackRealline2*)tr->elements[1], NotesTexts, 2, isranged, x1, y_middle);

  if (num_elements == 2)
    return;

  int x_middle = (wtrack->notearea.x2 + wtrack->notearea.x ) / 2;

  paint_tr2((TrackRealline2*)tr->elements[2], NotesTexts, 3, isranged, x_middle, y1);
  
  if (num_elements == 3)
    return;

  if (num_elements>4){
    GE_Context *c = GE_textcolor(get_colnum(NULL, isranged));
    paint_halfsize_note(c, 4, NotesTexts[NOTE_MUL], x_middle, y_middle);
  } else {
    paint_tr2((TrackRealline2*)tr->elements[3], NotesTexts, 4, isranged, x_middle, y_middle);
  }
}

static void create_track_text(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, vector_t *tr, int realline){
  char **NotesTexts = wtrack->notelength==3?NotesTexts3:NotesTexts2;
  float  notenum    = get_notenum(tr); //trackrealline->note;
  bool   isranged   = wblock->isranged && wblock->rangex1<=wtrack->l.num && wblock->rangex2>=wtrack->l.num && realline>=wblock->rangey1 && realline<wblock->rangey2;

  int y1 = get_realline_y1(window, realline);
  int y2 = get_realline_y2(window, realline);

  TrackRealline2 *tr2 = tr->num_elements==0 ? NULL : (TrackRealline2*)tr->elements[0];

  int colnum = get_colnum(tr2, isranged);
  
  if (isranged)
    GE_filledBox(GE_color(0),wtrack->notearea.x,y1,wtrack->notearea.x2,y2);

  if(tr2!=NULL && wtrack->noteshowtype==TEXTTYPE){

    // Paint THISNOTELINES
    if(tr->num_elements < 2 && tr2->note!=NULL && tr2->note->subtrack>0) {
      //printf("Gakk: %s (%s), %d, pointer: %p\n",NotesTexts[(int)notenum],NotesTexts[(int)note->note],note->subtrack,note);
      float y = (y1+y2) / 2.0f;
      float x1 = wtrack->notearea.x2;
      float x2 = (GetXSubTrack1(wtrack,tr2->note->subtrack) + GetXSubTrack2(wtrack,tr2->note->subtrack)) / 2.0f;
      GE_line(GE_color(13),
              x1, y,
              x2, y,
              1.6);
    }

    bool highlight;
    
    if (tr2->pitch != NULL && &tr2->pitch->l==current_node)
      highlight = true;
    else if (tr2->note != NULL && &tr2->note->l==current_node)
      highlight = true;
    else
      highlight = false;

    //printf("highlight: %d %p %p\n",highlight,trackrealline->daspitch,trackrealline->dasnote);
    //printf("current_node: %p\n\n",current_node);
    
    int cents = R_BOUNDARIES(0,round((notenum - (int)notenum)*100.0),99);

    if(isranged==false && notenum>0 && notenum<128)
      GE_filledBox(get_note_background(notenum, highlight), wtrack->notearea.x, y1, wtrack->notearea.x2, y2);

    if (tr->num_elements > 1)
      paint_multinotes(wtrack, tr, NotesTexts, isranged, y1, y2);
    
    else if (wblock->mouse_track == wtrack->l.num || wtrack->is_wide==true || cents!=0) {
      GE_Context *foreground = GE_textcolor(colnum);
      
      if (cents==0)
        GE_text(foreground, NotesTexts[(int)notenum], wtrack->notearea.x, y1); 
      else{
        char temp[32];
        if (wtrack->is_wide)
          sprintf(temp,"%s.%d",NotesTexts[(int)notenum],cents);
        else
          sprintf(temp,"%s %d",NotesTexts[(int)notenum],cents);
        GE_text(foreground, temp, wtrack->notearea.x, y1); 
      }
      
    }else
      draw_bordered_text(window, GE_textcolor_z(colnum, Z_ZERO), NotesTexts[(int)notenum], wtrack->notearea.x, y1);

  }
}

static void create_pitches(const struct Tracker_Windows *window, const struct WBlocks *wblock, struct WTracks *wtrack, const struct Notes *note){
  bool show_read_lines = wtrack->noteshowtype==GFXTYPE1 || wblock->mouse_track==wtrack->l.num;

  GE_Context *line_color = GE_color_alpha(7, 0.5);
  
  const struct NodeLine *nodelines = GetPitchNodeLines(window,
                                                       wblock,
                                                       wtrack,
                                                       note
                                                       );

  
  // lines
  for(const struct NodeLine *nodeline=nodelines ; nodeline!=NULL ; nodeline=nodeline->next)
    if(show_read_lines || nodeline->x1!=nodeline->x2)
      GE_line(line_color, nodeline->x1, nodeline->y1, nodeline->x2, nodeline->y2, get_pitchline_width());
  
  // indicator node
  if (indicator_node == &note->l && indicator_pitch_num!=-1) {
    const vector_t *nodes = get_nodeline_nodes(nodelines, wblock->t.y1);
    
    if (indicator_pitch_num >= nodes->num_elements)
      RError("indicator_pitch_node_num(%d) >= nodes->num_elements(%d)",indicator_pitch_num,nodes->num_elements);
    else {
      printf("indicator_pitch_num: %d\n",indicator_pitch_num);
      struct Node *node = (struct Node *)nodes->elements[indicator_pitch_num];
      draw_node_indicator(node->x, node->y-wblock->t.y1);
    }    
  }
}

static float subtrack_x1, subtrack_x2;

static void create_track_peaks(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const struct Notes *note, const struct NodeLine *nodelines){
  struct Patch *patch = wtrack->track->patch;
  float note_time = Place2STime(wblock->block, &note->l.p);

  float track_volume =  wtrack->track->volumeonoff ? (float)wtrack->track->volume / MAXTRACKVOL : 1.0f;
  //float velocity = scale(n,0,num_peaks, velocity1->velocity, velocity2->velocity);

  const int num_channels=PATCH_get_peaks(patch, 0,
                                         -1,
                                         wtrack->track,
                                         0,0,
                                         NULL,NULL
                                         );
    

  GE_Context *c = GE_mix_alpha_z(GE_get_rgb(0), GE_get_rgb(2), 700, 0.7, Z_ZERO);

#define NUM_LINES_PER_PEAK 2

  for(int ch=0;ch<num_channels;ch++){

    GE_trianglestrip_start();

    for(const struct NodeLine *ns = nodelines ; ns!=NULL ; ns=ns->next){

      float time1 = Place2STime(wblock->block, &ns->element1->p) - note_time;
      float time2 = Place2STime(wblock->block, &ns->element2->p) - note_time;
      
      float velocity1 = scale(ns->x1, subtrack_x1, subtrack_x2, 0, 1);
      float velocity2 = scale(ns->x2, subtrack_x1, subtrack_x2, 0, 1);
      
      int num_peaks = (ns->y2-ns->y1) / NUM_LINES_PER_PEAK;
      
      if(num_peaks<0){
        
        RError("num_peaks<0: %d",num_peaks);
        continue;
        
      }


      for(int n=0;n<num_peaks;n++){
        
        float min,max;
        
        int64_t start_time = scale(n,
                                   0,num_peaks,
                                   time1,time2
                                   );

        int64_t end_time   = scale(n+NUM_LINES_PER_PEAK,
                                   0,num_peaks,
                                   time1,time2
                                   );

        PATCH_get_peaks(patch, 
                        note->note,
                        ch,
                        wtrack->track,
                        start_time / wblock->block->reltempo,
                        end_time / wblock->block->reltempo,
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
          
        float y = ns->y1 + n*NUM_LINES_PER_PEAK;

#if 0
        printf("Adding %f,%f at %f. min/max: %f/%f. vel1/vel2: %f/%f. time1/time2: %f/%f\n",x1,x2,y,min,max,
               scale(n,0,num_peaks,velocity1->velocity, velocity2->velocity),
               scale(n+NUM_LINES_PER_PEAK,0,num_peaks,velocity1->velocity, velocity2->velocity),
               scale(n,0,num_peaks,time1,time2) / wblock->block->reltempo,
               scale(n+NUM_LINES_PER_PEAK,0,num_peaks,time1,time2) / wblock->block->reltempo);
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

    GE_trianglestrip_end(c);

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

  GE_Context *c = GE_gradient_z(rgb1, rgb2, Z_BELOW(Z_ZERO));

  GE_gradient_triangle_start(GradientType::VELOCITY);

  const struct NodeLine *nodeline = velocity_nodelines;
  while (nodeline != NULL){
    float vel_y1 = nodeline->y1;
    float vel_y2 = nodeline->y2;

    if (vel_y1>=area_y2)
      break;

    bool is_inside = vel_y1>=area_y1 || vel_y2>=area_y1;

    if (is_inside){

      float y1 = R_BOUNDARIES(area_y1, nodeline->y1, area_y2);
      float y2 = R_BOUNDARIES(area_y1, nodeline->y2, area_y2);

      float x1 = scale(y1, nodeline->y1, nodeline->y2, nodeline->x1, nodeline->x2);
      float x2 = scale(y2, nodeline->y1, nodeline->y2, nodeline->x1, nodeline->x2);
                   

      GE_gradient_triangle_add(c, subtrack_x1, y1);
      GE_gradient_triangle_add(c, x1,          y1);
      GE_gradient_triangle_add(c, subtrack_x1, y2);
      GE_gradient_triangle_add(c, x2,          y2);
    }
    
    nodeline = nodeline->next;
  }

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
    float x1 = nodeline->x1;
    float x2 = nodeline->x2;

    float y1 = nodeline->y1;
    float y2 = nodeline->y2;

    float start_note = scale(x1, track_notearea_x1, track_notearea_x2, track_pitch_min, track_pitch_max);
    float end_note   = scale(x2, track_notearea_x1, track_notearea_x2, track_pitch_min, track_pitch_max);

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

void create_track_velocities(const struct Tracker_Windows *window, const struct WBlocks *wblock, struct WTracks *wtrack, const struct Notes *note){

  //printf("Note: %s, pointer: %p, subtrack: %d\n",NotesTexts3[(int)note->note],note,note->subtrack);
  subtrack_x1 = GetXSubTrack1(wtrack,note->subtrack);
  subtrack_x2 = GetXSubTrack2(wtrack,note->subtrack);

  const struct NodeLine *nodelines = GetVelocityNodeLines(window, wblock, wtrack, note);
  const vector_t *nodes = get_nodeline_nodes(nodelines, wblock->t.y1);

  // background
  {

    const bool paint_vertical_velocity_gradient = false;
    //const bool paint_vertical_velocity_gradient = true;

    if(paint_vertical_velocity_gradient==false && note->pitches==NULL){
      
      GE_Context *c = get_note_background(note->note, false);

      GE_trianglestrip_start();
      
      for(const struct NodeLine *ns = nodelines ; ns!=NULL ; ns=ns->next){
        GE_trianglestrip_add(c, subtrack_x1, ns->y1);
        GE_trianglestrip_add(c, ns->x1, ns->y1);
        GE_trianglestrip_add(c, subtrack_x1, ns->y2);
        GE_trianglestrip_add(c, ns->x2, ns->y2);
      }
      
      GE_trianglestrip_end(c);

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
    GE_Context *c = drawNodeLines(nodelines, 1, is_current, 0.3, 0.6);

    // draw horizontal line where note starts, if it doesn't start on the start of a realline.
    int realline = FindRealLineForNote(wblock, 0, note);
    if (PlaceNotEqual(&wblock->reallines[realline]->l.p, &note->l.p))
      GE_line(c, subtrack_x1, nodelines->y1, nodelines->x1, nodelines->y1, get_nodeline_width(is_current));
  }

  // peaks
  if(TRACK_has_peaks(wtrack->track))
    create_track_peaks(window, wblock, wtrack, note, nodelines);

  // nodes
  if (is_current)
    VECTOR_FOR_EACH(Node *, node, nodes){
      draw_skewed_box(window, node->element, 5, node->x, node->y - wblock->t.y1);
    }END_VECTOR_FOR_EACH;

  if (indicator_node == &note->l && indicator_velocity_num!=-1) {
    if (indicator_velocity_num >= nodes->num_elements)
      RError("indicator_velocity_node_num(%d) >= nodes->num_elements(%d)",indicator_velocity_num,nodes->num_elements);
    else {
      struct Node *node = (struct Node *)nodes->elements[indicator_velocity_num];
      draw_node_indicator(node->x, node->y-wblock->t.y1);
    }
  }
}


static void create_track_fxs(const struct Tracker_Windows *window, const struct WBlocks *wblock, struct WTracks *wtrack, const struct FXs *fxs){
  const struct NodeLine *nodelines = GetFxNodeLines(window, wblock, wtrack, fxs);

  bool is_current = wblock->mouse_track==wtrack->l.num && wblock->mouse_fxs==fxs;

  drawNodeLines(nodelines, fxs->fx->color, is_current, 0.6, 1.0);
  
  if (indicator_node != NULL || is_current) {
    const vector_t *nodes = get_nodeline_nodes(nodelines, wblock->t.y1);

    VECTOR_FOR_EACH(const Node *, node, nodes){
      if (wblock->mouse_track==wtrack->l.num && wblock->mouse_fxs==fxs)
        draw_skewed_box(window, node->element, 1, node->x, node->y - wblock->t.y1);
      if (node->element==indicator_node)
        draw_node_indicator(node->x, node->y - wblock->t.y1);
    }END_VECTOR_FOR_EACH;

  }
}

static void create_track_stops(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack){
  struct Stops *stops = wtrack->track->stops;

  float reallineF = 0.0f;
  GE_Context *c = GE_color_alpha(1,0.19);

  while(stops != NULL){
    reallineF = FindReallineForF(wblock, reallineF, &stops->l.p);
    float y = get_realline_y(window, reallineF);
    GE_line(c,
            wtrack->notearea.x, y,
            wtrack->x2, y,
            1.2
            );
    stops = NextStop(stops);
  }
}

static void create_track_is_recording(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack){
  GE_Context *c = GE_z(GE_alpha(GE_get_rgb(3), 0.7), Z_STATIC);

  GE_text(c, "Rec", wtrack->x, 0);
}

void create_track(const struct Tracker_Windows *window, const struct WBlocks *wblock, struct WTracks *wtrack, int left_subtrack){
  create_track_borders(window, wblock, wtrack, left_subtrack);

  SetNoteSubtrackAttributes(wtrack->track);
      
  if(left_subtrack==-1) {

    vector_t *trs = TRS_get(wblock, wtrack);
      
    for(int realline = 0 ; realline<wblock->num_reallines ; realline++)
      create_track_text(window, wblock, wtrack, &trs[realline], realline);
  }

  {
  
    const struct Notes *note=wtrack->track->notes;
    while(note != NULL){
      if(note->subtrack >= left_subtrack) {
        if (left_subtrack==-1)
          create_pitches(window, wblock, wtrack, note);
        create_track_velocities(window, wblock, wtrack, note);
      }
      note = NextNote(note);
    }
  }


  
  if(left_subtrack<=0){
    
    const struct FXs *fxs=wtrack->track->fxs;
    while(fxs != NULL){
      create_track_fxs(window, wblock, wtrack, fxs);
      fxs = NextFX(fxs);
    }

    create_track_stops(window, wblock, wtrack);
  }

  if (wtrack->track->is_recording)
    create_track_is_recording(window, wblock, wtrack);
}


void create_tracks(const struct Tracker_Windows *window, const struct WBlocks *wblock){
  struct WTracks *wtrack=(struct WTracks*)ListFindElement1(&wblock->wtracks->l,wblock->left_track);

  while(wtrack!=NULL && wtrack->l.num<=wblock->right_track){
    int left_subtrack = wtrack->l.num==wblock->left_track ? wblock->left_subtrack : -1;
    create_track(window, wblock, wtrack, left_subtrack);
    wtrack=NextWTrack(wtrack);
  }
}

void create_cursor(const struct Tracker_Windows *window, const struct WBlocks *wblock){

  GE_Context *c = GE_z(GE_alpha(GE_get_rgb(root->editonoff?7:5), 0.2), Z_STATIC);
  
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


  c = GE_z(GE_alpha(GE_get_rgb(1), 0.75), Z_STATIC);

  float width = 0.8f;
  GE_box(c,
         x1+2,y1,
         x4-3,y2-1,
         width
         );

  c = GE_z(GE_alpha(GE_get_rgb(2), 0.05), Z_STATIC);
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


/************************************
   block
 ************************************/

void GL_create(const struct Tracker_Windows *window, struct WBlocks *wblock){
  static int level=0;
  //static int n=0; printf("GL_create called %d\n",n++);
  printf("GL_create called %d\n",level++);

  GE_start_writing(); {
    
    struct WSignatures *wsignatures = WSignatures_get(window, wblock);

    create_left_slider(window, wblock);
    create_background(window, wblock, wsignatures);
    create_block_borders(window, wblock);
    create_linenumbers(window, wblock);
    create_tempograph(window, wblock);
    if(window->show_signature_track)
      create_signaturetrack(window, wblock, wsignatures);
    if(window->show_lpb_track)
      create_lpbtrack(window, wblock);
    if(window->show_bpm_track)
      create_bpmtrack(window, wblock);
    if(window->show_reltempo_track)
      create_reltempotrack(window, wblock);
    create_tracks(window, wblock);
    create_cursor(window, wblock);

  } GE_end_writing(GE_get_rgb(0));
}
