

#include "../common/nsmtracker.h"
#include "../common/settings_proc.h"
#include "../common/list_proc.h"

#include "GfxElements.h"


#include "Render_proc.h"







static int get_realline_y1(struct Tracker_Windows *window, int realline){
  return window->fontheight*realline;
}

static int get_realline_y2(struct Tracker_Windows *window, int realline){
  return window->fontheight*(realline+1);
}




static void draw_text_num(
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






/************************************
   Background
 ************************************/


extern int lpb_opacity;
extern int line_opacity;

static void create_background_realline(struct Tracker_Windows *window, struct WBlocks *wblock, int realline){

  struct WTracks *last_wtrack = (struct WTracks*)ListLast1(&wblock->wtracks->l);

  int x1 = 0;
  int x2 = last_wtrack->x2;
  int y1 = get_realline_y1(window, realline);
  int y2 = get_realline_y2(window, realline);

  // background color
  {
    if(lpb_opacity == -1)
      lpb_opacity = SETTINGS_read_int("lpb_opacity", 900);
    
    GE_Context *c;
    if( (wblock->wlpbs[realline].is_beat))
      c = GE_static_x(GE_mix_color_z(GE_get_rgb(15), GE_get_rgb(1), lpb_opacity / 1000.0f, Z_BACKGROUND));
    else
      c = GE_static_x(GE_color_z(15, Z_BACKGROUND));
    
    GE_filledBox(c,x1,y1,x2,y2);
  }

  float line_width = 1.0f;

  // realline separator line
  {
    if(line_opacity == -1)
      line_opacity = SETTINGS_read_int("line_opacity", 800);
    
    if(line_opacity != 1000) {
      GE_Context *c = GE_static_x(GE_mix_color_z(GE_get_rgb(15), GE_get_rgb(1), line_opacity / 1000.0f, Z_LINENUMBERS - 1));

      GE_line(c,x1,y1,x2,y1,line_width);
    }
  }
}


static void create_background(struct Tracker_Windows *window, struct WBlocks *wblock){
  int realline;
  for(realline = 0 ; realline<wblock->num_reallines ; realline++)
    create_background_realline(window, wblock, realline);
}





/************************************
   Linenumbers
 ************************************/

static void create_background_linenumber(struct Tracker_Windows *window, struct WBlocks *wblock, int realline){
  int y = get_realline_y1(window, realline);

  struct LocalZooms *localzoom = wblock->reallines[realline];

  int colornum;

  if(localzoom->level>0){
    if(localzoom->zoomline>0){
      colornum=R_MIN(7,localzoom->level+1);
    }else{
      if(localzoom->level==1){
        colornum=1;
      }else{
        colornum=R_MIN(7,localzoom->level);
      }
    }
  }else{
    colornum=1;
  }

  GE_Context *c = GE_static_x(GE_textcolor_z(colornum, Z_LINENUMBERS));
  
  if(localzoom->level>0 && localzoom->zoomline>0){
    draw_text_num(
                  c,
                  localzoom->zoomline,
                  wblock->zoomlinearea.width/window->fontwidth,
                  wblock->zoomlinearea.x,
                  y);
  }else{
    draw_text_num(
                  c,
                  localzoom->Tline,
                  (wblock->linenumarea.width)/window->fontwidth,
                  wblock->linenumarea.x,
                  y);
  }  
}

static void create_linenumbers(struct Tracker_Windows *window, struct WBlocks *wblock){
  int realline;
  for(realline = 0 ; realline<wblock->num_reallines ; realline++)
    create_background_linenumber(window, wblock, realline);
}





/************************************
   lpb track
 ************************************/

static void create_lpb(struct Tracker_Windows *window, struct WBlocks *wblock,int realline){
  int y = get_realline_y1(window, realline);
  int lpb=wblock->wlpbs[realline].lpb;
  int type=wblock->wlpbs[realline].type;
  GE_Context *c = GE_color(1);

  if(lpb!=0){
    draw_text_num(
                  c,
                  lpb,
                  wblock->lpbarea.width/window->fontwidth,
                  wblock->lpbarea.x,
                  y);
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
      abort();
    };
    GE_text(c, typetext, wblock->lpbTypearea.x, y);
  }
}


static void create_lpbtrack(struct Tracker_Windows *window, struct WBlocks *wblock){
  int realline;
  for(realline = 0 ; realline<wblock->num_reallines ; realline++)
    create_lpb(window, wblock, realline);
}



/************************************
   lpb track
 ************************************/

static void create_bpm(struct Tracker_Windows *window, struct WBlocks *wblock,int realline){
  int y = get_realline_y1(window, realline);
  int tempo=wblock->wtempos[realline].tempo;
  int type=wblock->wtempos[realline].type;
  GE_Context *c = GE_color(1);
  
  if(tempo!=0){
    draw_text_num(
                  c,
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
    GE_text(c, typetext, wblock->tempoTypearea.x, y);
  }
}


static void create_bpmtrack(struct Tracker_Windows *window, struct WBlocks *wblock){
  int realline;
  for(realline = 0 ; realline<wblock->num_reallines ; realline++)
    create_bpm(window, wblock, realline);
}






/************************************
   block
 ************************************/

void GL_create(struct Tracker_Windows *window, struct WBlocks *wblock){
  GE_start_writing(); {

    create_background(window, wblock);
    create_linenumbers(window, wblock);
    create_lpbtrack(window, wblock);
    create_bpmtrack(window, wblock);
#if 0
    create_reltempotrack(window, wblock);
    create_tracks(window, wblock);
#endif

  } GE_end_writing();
}
