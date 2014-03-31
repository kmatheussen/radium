#include <math.h>


#include "../common/nsmtracker.h"
#include "../common/settings_proc.h"
#include "../common/list_proc.h"
#include "../common/placement_proc.h"
#include "../common/realline_calc_proc.h"
#include "../common/gfx_subtrack_proc.h"
#include "../common/nodelines_proc.h"
#include "../common/time_proc.h"
#include "../common/tracks_proc.h"
#include "../common/patch_proc.h"

#include "GfxElements.h"


#include "Render_proc.h"


// Functions in this file are called from the main thread.


extern char *NotesTexts3[131];
extern char *NotesTexts2[131];


static int get_realline_y1(const struct Tracker_Windows *window, int realline){
  return window->fontheight*realline;
}

static int get_realline_y2(const struct Tracker_Windows *window, int realline){
  return window->fontheight*(realline+1);
}

static float get_realline_y(const struct Tracker_Windows *window, float reallineF){
  return window->fontheight*reallineF;
}

static void draw_bordered_text(
                               const struct Tracker_Windows *window,
                               int colornum, int z,
                               char *text,
                               int x,
                               int y
                               )
{
  GE_Context *c = GE_textcolor_z(colornum, z);

  GE_text(c, text, x, y);

#if 1
  int x2=x+(strlen(text)*window->fontwidth);
  int y2=y+window->fontheight-1;

  GE_line(GE_color_z(9, z),
          x,y,x,y2,1.0f);

  QColor qc1 = GE_qcolor(9).darker(90);
  QColor qc2 = GE_qcolor(9).darker(110);
  GE_Context *c2 = GE_gradient_z(qc1, qc2, z); //GE_get_rgb(9), GE_get_rgb(11), z);

  GE_line(c2,
          x,y,x2,y,1.0f);

  GE_Context *c3 = GE_mix_color_z(GE_get_rgb(11), GE_get_rgb(1), 800, z);

  GE_line(c3, x2,y, x2,y2, 1.0f);
  GE_line(c3,x,y2,x2,y2,1.0f);
#endif
}


static void draw_text_num(
                          const struct Tracker_Windows *window,
                          int colornum, int z,
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
  
  draw_bordered_text(window, colornum, z, temp3, x, y);
}




static void draw_skewed_box(const struct Tracker_Windows *window,
                            int color,
                            int x, int y
                            )
{
 
  int x1 = x-15;
  int x2 = x+15;
  int y1 = y-15;
  int y2 = y+15;

  // vertical left
  GE_line(GE_mix_color(GE_get_rgb(color), GE_get_rgb(2), 100),
          x1+1, y1+1,
          x1+2,y2-1,
          1.0f);

  // horizontal bottom
  GE_line(GE_mix_color(GE_get_rgb(color), GE_get_rgb(1), 300),
          x1+2,y2-1,
          x2-1,y2-2,
          1.0f);

  // vertical right
  GE_line(GE_mix_color(GE_get_rgb(color), GE_get_rgb(1), 400),
          x2-1,y2-2,
          x2-2,y1+2,
          1.0f);

  // horizontal top
  GE_line(GE_mix_color(GE_get_rgb(color), GE_get_rgb(2), 300),
          x2-2,y1+2,
          x1+1,y1+1,
          1.0f);
}


void create_double_border(
                          int x, int y, int y2
                          )
{
  GE_line(GE_color(1),x,y,x,y2,1.0);
  GE_line(GE_color(9),x+1,y,x+1,y2,1.0);
}

void create_single_border(
                          int x, int y, int y2
                          )
{
  GE_line(GE_color(7),x,y,x,y2,1.0);
}




/******************************************************************************************
   NodeLine. Will replace the old type of nodelines. Placed here for convenience, for now.
*******************************************************************************************/

struct NodeLine{
  struct NodeLine *next;

  float x1,y1;
  float x2,y2;

  const struct ListHeader3 *element1;
  const struct ListHeader3 *element2;

  bool is_node;
};


// Note that 'y' can be outside the range of the nodeline. If that happens, nodelines is not modified.
static void insert_nonnode_nodeline(struct NodeLine *nodelines, const struct ListHeader3 *element, float y){

  if(y <= nodelines->y1)
    return;

  while(nodelines != NULL) {
    if(y>nodelines->y1 && y<nodelines->y2){

      // put it after

      struct NodeLine *n = (struct NodeLine *)talloc(sizeof(struct NodeLine));
      n->element1 = element;
      n->y1 = y;

      n->x1 = scale(GetfloatFromPlace(&element->p),
                    GetfloatFromPlace(&nodelines->element1->p),
                    GetfloatFromPlace(&nodelines->element2->p),
                    nodelines->x1, nodelines->x2
                    );

      //n->x1 = scale(y, nodelines->y1, nodelines->y2, nodelines->x1, nodelines->x2);

      n->next = nodelines->next ;
      nodelines->next = n;

      n->x2 = nodelines->x2;
      n->y2 = nodelines->y2;
      n->element2 = nodelines->element2;

      nodelines->x2 = n->x1;
      nodelines->y2 = n->y1;
      nodelines->element2 = n->element1;

      return;
    }

    nodelines = nodelines->next;
  }
}

struct NodeLine *create_nodelines(
                                  const struct Tracker_Windows *window,
                                  const struct WBlocks *wblock,
                                  const struct ListHeader3 *list,                                  
                                  float (*get_x)(const struct WBlocks *wblock, const struct ListHeader3 *element), // should return a value between 0 and 1.
                                  const struct ListHeader3 *last_element // may be null. may also contain more than one element.
                                  )
{
  struct NodeLine *nodelines = NULL;

  assert(list != NULL);
  assert(list->next != NULL || last_element!=NULL);


  // 1. Create straight forward nodelines from the list
  {
    float reallineF = 0.0f;
    struct NodeLine *nodelines_last = NULL;

    while(list != NULL){
      struct NodeLine *nodeline = (struct NodeLine *)talloc(sizeof(struct NodeLine));

      nodeline->x1 = get_x(wblock, list);
      reallineF = FindReallineForF(wblock, reallineF, &list->p);
      nodeline->y1 = get_realline_y(window, reallineF);
      nodeline->element1 = list;
      nodeline->is_node = true;

      if(nodelines_last==NULL)
        nodelines = nodelines_last = nodeline;
      else {
        nodelines_last->next = nodeline;
        nodelines_last = nodeline;
      }

      list = list->next;
      if (list==NULL) {
        list = last_element;
        last_element = NULL;
      }
    }
  }


  // 2. Insert x2, y2 and element2 attributes, and remove last element.
  {
    struct NodeLine *ns = nodelines;
    struct NodeLine *next = ns->next;
    for(;;){
      ns->x2 = next->x1;
      ns->y2 = next->y1;
      ns->element2 = next->element1;
      if(next->next==NULL)
        break;
      ns = next;
      next = next->next;
    }
    ns->next = NULL; // Cut the last element
  }


  // 3. Insert all non-node break-points. (caused by realline level changes)
  {
    struct LocalZooms **reallines=wblock->reallines;
    int curr_level = reallines[0]->level;
    int realline;
    float reallineF = 0.0f;
    
    for(realline = 1; realline < wblock->num_reallines ; realline++) {
          
      struct LocalZooms *localzoom = reallines[realline];
      
      if (localzoom->level != curr_level){
        reallineF = FindReallineForF(wblock, reallineF, &localzoom->l.p);
        insert_nonnode_nodeline(nodelines, &localzoom->l, get_realline_y(window, reallineF));
        curr_level = localzoom->level;
      }
    }
  }


  return nodelines;
}



/************************************
   Background
 ************************************/


extern int lpb_opacity;
extern int line_opacity;

static void create_background_realline(const struct Tracker_Windows *window, const struct WBlocks *wblock, int realline){

  const struct WTracks *last_wtrack = (const struct WTracks*)ListLast1(&wblock->wtracks->l);

  int x1 = 0;
  int x2 = last_wtrack->x2;
  int y1 = get_realline_y1(window, realline);
  int y2 = get_realline_y2(window, realline);

  // background
  {
    if(lpb_opacity == -1)
      lpb_opacity = SETTINGS_read_int("lpb_opacity", 900);
    
    GE_Context *c;
 
    if( (wblock->wlpbs[realline].is_beat))
      c = GE_mix_color_z(GE_get_rgb(15), GE_get_rgb(1), lpb_opacity, Z_BACKGROUND | Z_STATIC_X);
    else
      c = GE_color_z(15, Z_BACKGROUND | Z_STATIC_X);
    
    GE_filledBox(c,x1,y1,x2,y2);
  }

  float line_width = 1.0f;

  // realline separator line
  {
    if(line_opacity == -1)
      line_opacity = SETTINGS_read_int("line_opacity", 800);
    
    if(line_opacity != 1000) {
      GE_Context *c = GE_mix_color_z(GE_get_rgb(15), GE_get_rgb(1), line_opacity, Z_ABOVE(Z_BACKGROUND) | Z_STATIC_X);

      GE_line(c,x1,y1,x2,y1,line_width);
    }
  }
}


static void create_background(const struct Tracker_Windows *window, const struct WBlocks *wblock){
  int realline;
  for(realline = 0 ; realline<wblock->num_reallines ; realline++)
    create_background_realline(window, wblock, realline);
}





/************************************
   Linenumbers
 ************************************/

static void create_background_linenumber(const struct Tracker_Windows *window, const struct WBlocks *wblock, int realline){
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

  int z = Z_LINENUMBERS | Z_STATIC_X;
  
  if(localzoom->level>0 && localzoom->zoomline>0){
    draw_text_num(
                  window,
                  colornum, z,
                  localzoom->zoomline,
                  wblock->zoomlinearea.width/window->fontwidth,
                  wblock->zoomlinearea.x,
                  y);
  }else{
    draw_text_num(
                  window,
                  colornum, z,
                  localzoom->Tline,
                  (wblock->linenumarea.width)/window->fontwidth,
                  wblock->linenumarea.x,
                  y);
  }  
}

static void create_linenumbers(const struct Tracker_Windows *window, const struct WBlocks *wblock){
  int realline;
  for(realline = 0 ; realline<wblock->num_reallines ; realline++)
    create_background_linenumber(window, wblock, realline);
}





/************************************
   lpb track
 ************************************/

static void create_lpb(const struct Tracker_Windows *window, const struct WBlocks *wblock,int realline){
  int y = get_realline_y1(window, realline);
  int lpb=wblock->wlpbs[realline].lpb;
  int type=wblock->wlpbs[realline].type;
  
  if(lpb!=0){
    draw_text_num(
                  window,
                  1, Z_ZERO,
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
    
    GE_Context *c = GE_textcolor_z(1, Z_ZERO);
    GE_text(c, typetext, wblock->lpbTypearea.x, y);
  }
}



static void create_lpbtrack(const struct Tracker_Windows *window, const struct WBlocks *wblock){
  int realline;
  for(realline = 0 ; realline<wblock->num_reallines ; realline++)
    create_lpb(window, wblock, realline);
}




/************************************
   bpm track
 ************************************/

static void create_bpm(const struct Tracker_Windows *window, const struct WBlocks *wblock,int realline){
  int y     = get_realline_y1(window, realline);
  int tempo = wblock->wtempos[realline].tempo;
  int type  = wblock->wtempos[realline].type;
  
  if(tempo!=0){
    draw_text_num(
                  window,
                  1, Z_ZERO,
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

    GE_Context *c = GE_color(1);
    GE_text(c, typetext, wblock->tempoTypearea.x, y);
  }
}


static void create_bpmtrack(const struct Tracker_Windows *window, const struct WBlocks *wblock){
  int realline;
  for(realline = 0 ; realline<wblock->num_reallines ; realline++)
    create_bpm(window, wblock, realline);
}



/************************************
   reltempo track
 ************************************/

static float get_temponode_x(const struct WBlocks *wblock, const struct ListHeader3 *element){
  struct TempoNodes *temponode = (struct TempoNodes*)element;
  return scale(temponode->reltempo, (float)(-wblock->reltempomax+1.0f),(float)(wblock->reltempomax-1.0f), wblock->temponodearea.x, wblock->temponodearea.x2);
}

static void create_reltempotrack(const struct Tracker_Windows *window, const struct WBlocks *wblock){
  GE_Context *line_color = GE_color(4);

  struct NodeLine *nodelines = create_nodelines(window,
                                                wblock,
                                                &wblock->block->temponodes->l,
                                                get_temponode_x,
                                                NULL
                                                );
  do{

    if(nodelines->is_node && wblock->mouse_track==TEMPONODETRACK)
      draw_skewed_box(window, 1, nodelines->x1, nodelines->y1);

    GE_line(line_color, nodelines->x1, nodelines->y1, nodelines->x2, nodelines->y2, 1.5);

  }while(nodelines->next!=NULL && (nodelines=nodelines->next));

  if(wblock->mouse_track==TEMPONODETRACK)
    draw_skewed_box(window, 1, nodelines->x2, nodelines->y2);
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
                       wblock->lpbarea.x2+1,
                       y1,y2
                       );
  
  create_single_border(
                       wblock->tempoarea.x2+1,
                       y1,y2
                       );
  
  create_double_border(
                       wblock->temponodearea.x2+1,
                       y1,y2
                       );
}



/************************************
   tracks
 ************************************/
void create_track_borders(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack){
  int y1=get_realline_y1(window, 0);
  int y2=get_realline_y2(window, wblock->num_reallines-1);
  
  create_double_border(
                       wtrack->x2+1,
                       y1,
                       y2);

  create_single_border(
                       wtrack->notearea.x2+1,
                       y1,
                       y2);

  for(int lokke=1 ; lokke<wtrack->num_vel;lokke++){
    create_single_border(
                         GetXSubTrack1(wtrack,lokke)-1,
                         y1,
                         y2);
  }

}

static GE_Context *get_note_background(int notenum){
  notenum = R_BOUNDARIES(0,notenum,127);
  const int split1 = 50;
  const int split2 = 95;

  GE_Rgb rgb;

  if(notenum<split1)
    rgb = GE_mix(GE_get_rgb(5), GE_get_rgb(1), scale(notenum,0,split1,0,1000));
  else if(notenum<split2)
    rgb = GE_mix(GE_get_rgb(6), GE_get_rgb(5), scale(notenum,split1,split2,0,1000));
  else
    rgb = GE_mix(GE_get_rgb(2), GE_get_rgb(6), scale(notenum,split2,160,0,1000));

  return GE_mix_color(rgb, GE_get_rgb(15), 400);
}

void create_track_text(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, int realline){
  char                 **NotesTexts    = wtrack->notelength==3?NotesTexts3:NotesTexts2;
  struct TrackRealline  *trackrealline = &wtrack->trackreallines[realline];
  float                  notenum       = trackrealline->note;
  int                    colnum        = 1;
  bool                   isranged      = wblock->isranged && wblock->rangex1<=wtrack->l.num && wblock->rangex2>=wtrack->l.num && realline>=wblock->rangey1 && realline<wblock->rangey2;

  int y1 = get_realline_y1(window, realline);
  int y2 = get_realline_y2(window, realline);

  if(notenum>=NOTE_PITCH_START){
    //isgliding = true;
    notenum -= NOTE_PITCH_START;
    colnum = 5;
  }

  if (isranged) {
    colnum = 1;
    GE_filledBox(GE_color(0),wtrack->notearea.x,y1,wtrack->notearea.x2,y2);
  }

  if(notenum!=0 && wtrack->noteshowtype==TEXTTYPE){
    
    if(isranged==false && notenum>0 && notenum<128)
      GE_filledBox(get_note_background(notenum), wtrack->notearea.x, y1, wtrack->notearea.x2, y2);

    if (wblock->mouse_track == wtrack->l.num || wtrack->is_wide==true) {
      GE_Context *foreground = GE_textcolor(colnum);

      int cents = R_BOUNDARIES(0,(notenum - (int)notenum)*100,99);

      if (cents==0)
        GE_text(foreground, NotesTexts[(int)notenum], wtrack->notearea.x, y1); 
      else{
        char temp[32];
        sprintf(temp,"%s, %d",NotesTexts[(int)notenum],cents);
        GE_text(foreground, temp, wtrack->notearea.x, y1); 
      }
      
    }else
      draw_bordered_text(window, colnum, Z_ZERO, NotesTexts[(int)notenum], wtrack->notearea.x, y1);

  }
}

void create_track_pitches(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, int realline){
  int y1 = get_realline_y1(window, realline);
  int y2 = get_realline_y2(window, realline);

  TBox within2;
  within2.y1 = y1;
  within2.y2 = y2;
  within2.x1 = wtrack->x;
  within2.x2 = wtrack->fxarea.x - 2;

  WArea warea;
  warea.x=within2.x1;
  warea.x2=within2.x2;
  warea.width=warea.x2-warea.x;

  bool show_read_lines = wblock->mouse_track==wtrack->l.num;

  WPitches *wpitch=wtrack->wpitches[realline];
  while(wpitch!=NULL){
    if(wpitch->type==TRE_PITCHLINE)
      if(wpitch->x1 != wpitch->x2 || show_read_lines) {
        TBox get;
        GetNodeLine(wpitch,&warea,&within2,&get);
        GE_line(GE_color_alpha(7,0.5), get.x1,get.y1, get.x2,get.y2, 1.75);
      }
    wpitch=wpitch->next;
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
                                         NULL,NULL);
    

  GE_Context *c = GE_color(0);

  GE_trianglestrip_start();

#define NUM_LINES_PER_PEAK 2

  for(const struct NodeLine *ns = nodelines ; ns!=NULL ; ns=ns->next){
    int num_peaks = (ns->y2-ns->y1) / NUM_LINES_PER_PEAK;

    if(num_peaks<0){

      RError("num_peaks<0: %d",num_peaks);
      continue;

    }

    float time1 = Place2STime(wblock->block, &ns->element1->p) - note_time;
    float time2 = Place2STime(wblock->block, &ns->element2->p) - note_time;
        
    for(int n=0;n<num_peaks;n++){
      struct Velocities *velocity1 = (struct Velocities*)ns->element1;
      struct Velocities *velocity2 = (struct Velocities*)ns->element2;
        
      for(int ch=0;ch<num_channels;ch++){
        
        float min,max;
        
        PATCH_get_peaks(patch, 
                        note->note,
                        ch,
                        wtrack->track,
                        scale(n,0,num_peaks,time1,time2) / wblock->block->reltempo,
                        scale(n+NUM_LINES_PER_PEAK,0,num_peaks,time1,time2) / wblock->block->reltempo,
                        &min,
                        &max);

        float velocity = (float)scale(n,0,num_peaks,velocity1->velocity,velocity2->velocity) / (float)MAX_VELOCITY;

        float bound_x1 = scale(scale(ch,0,num_channels,0.0f,velocity),
                               0, 1, subtrack_x1, subtrack_x2);
        float bound_x2 = scale(scale(ch+1,0,num_channels,0.0f,velocity),
                               0, 1, subtrack_x1, subtrack_x2);

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

        if(fabsf(x1-x2) < 0.5)
          GE_line(c, (x1+x2)/2.0f, y, (x1+x2)/2.0f, y+NUM_LINES_PER_PEAK, 1.0);
        else{
          GE_trianglestrip_add(c, x1, y);
          GE_trianglestrip_add(c, x2, y);
        }
      }
    }
  }

  GE_trianglestrip_end(c);
}

static float get_velocity_x(const struct WBlocks *wblock, const struct ListHeader3 *element){
  struct Velocities *velocity = (struct Velocities*)element;
  return scale_double(velocity->velocity, 0, MAX_VELOCITY, subtrack_x1, subtrack_x2);
}

void create_track_velocities(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const struct Notes *note){
  struct Velocities first_velocity;
  first_velocity.l.p = note->l.p;
  first_velocity.l.next = &note->velocities->l;
  first_velocity.velocity = note->velocity;

  struct Velocities last_velocity;
  last_velocity.l.p = note->end;
  last_velocity.l.next = NULL;
  last_velocity.velocity = note->velocity_end;

  subtrack_x1 = GetXSubTrack1(wtrack,note->subtrack);
  subtrack_x2 = GetXSubTrack2(wtrack,note->subtrack);

  struct NodeLine *nodelines = create_nodelines(window,
                                                wblock,
                                                &first_velocity.l,
                                                get_velocity_x,
                                                &last_velocity.l
                                                );

  // background
  {
    GE_Context *c = get_note_background(note->note);
    
    GE_trianglestrip_start();
    
    for(struct NodeLine *ns = nodelines ; ns!=NULL ; ns=ns->next){
      GE_trianglestrip_add(c, subtrack_x1, ns->y1);
      GE_trianglestrip_add(c, ns->x1, ns->y1);
      GE_trianglestrip_add(c, subtrack_x1, ns->y2);
      GE_trianglestrip_add(c, ns->x2, ns->y2);

      if(ns->is_node && wblock->mouse_track==wtrack->l.num && wblock->mouse_note==note)
        draw_skewed_box(window, 5, ns->x1, ns->y1);
    }
    GE_trianglestrip_end(c);
  }

  // border
  {
    float width = 1.75;
    GE_Context *c = GE_mix_color(GE_get_rgb(1), GE_get_rgb(15), 300);

    for(struct NodeLine *ns = nodelines ; ns!=NULL ; ns=ns->next)
      GE_line(c, ns->x1, ns->y1, ns->x2, ns->y2, width);
  }

  // peaks
  if(TRACK_has_peaks(wtrack->track))
    create_track_peaks(window, wblock, wtrack, note, nodelines);
}


static float fx_min, fx_max, wtrackfx_x1, wtrackfx_x2;

static float get_fxs_x(const struct WBlocks *wblock, const struct ListHeader3 *element){
  struct FXNodeLines *fxnode = (FXNodeLines *)element;
  return scale(fxnode->val, fx_min, fx_max, wtrackfx_x1, wtrackfx_x2);
}


void create_track_fxs(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const struct FXs *fxs){
  fx_min = fxs->fx->min;
  fx_max = fxs->fx->max;
  wtrackfx_x1 = wtrack->fxarea.x;
  wtrackfx_x2 = wtrack->fxarea.x2;

  struct NodeLine *nodelines = create_nodelines(window,
                                                wblock,
                                                &fxs->fxnodelines->l,
                                                get_fxs_x,
                                                NULL
                                                );  

  GE_Context *line_color = GE_color(fxs->fx->color);

  do{

    if(nodelines->is_node && wblock->mouse_track==wtrack->l.num)
      draw_skewed_box(window, 1, nodelines->x1, nodelines->y1);

    GE_line(line_color, nodelines->x1, nodelines->y1, nodelines->x2, nodelines->y2, 1.5);

  }while(nodelines->next!=NULL && (nodelines=nodelines->next));

  if(wblock->mouse_track==wtrack->l.num)
    draw_skewed_box(window, 1, nodelines->x2, nodelines->y2);
}

void create_track(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack){
  create_track_borders(window, wblock, wtrack);

  for(int realline = 0 ; realline<wblock->num_reallines ; realline++) {
    create_track_text(window, wblock, wtrack, realline);
    create_track_pitches(window, wblock, wtrack, realline);
  }

  const struct Notes *note=wtrack->track->notes;
  while(note != NULL){
    create_track_velocities(window, wblock, wtrack, note);
    note = NextNote(note);
  }

  const struct FXs *fxs=wtrack->track->fxs;
  while(fxs != NULL){
    create_track_fxs(window, wblock, wtrack, fxs);
    fxs = NextFX(fxs);
  }
}


void create_tracks(const struct Tracker_Windows *window, const struct WBlocks *wblock){
  const struct WTracks *wtrack=wblock->wtracks;

  while(wtrack!=NULL){
    create_track(window, wblock, wtrack);
    wtrack=NextWTrack(wtrack);
  }
}


/************************************
   block
 ************************************/

void GL_create(const struct Tracker_Windows *window, const struct WBlocks *wblock){
  GE_start_writing(); {

    create_background(window, wblock);
    create_block_borders(window, wblock);
    create_linenumbers(window, wblock);
    create_lpbtrack(window, wblock);
    create_bpmtrack(window, wblock);
    create_reltempotrack(window, wblock);
    create_tracks(window, wblock);

  } GE_end_writing();
}
