/* Copyright 2013 Kjetil S. Matheussen

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

#include <stdio.h>

#include <QPainter>
#include <QMouseEvent>
#include <QTimer>
#include <QFileDialog>
#include <QMessageBox>

#include "Qt_compressor_widget.h"

#include "../audio/system_compressor_wrapper_proc.h"


#define GLIDING_PEAK_AREA 0

const int min_db = -40;
const int max_db = 40;

const int k_timer_interval = 35;

const int max_attack_release = 500;

const float def_threshold = 0.7f;
const float def_ratio = 0.3;
const float def_makeupgain = 0.3f;

#ifdef COMPILING_RADIUM

// I'm pretty sure it's not necessary to test for plugin!=NULL in these three functons.
// Normally, it's safe, but these functions can also be called from a timer.
//
static void set_compressor_parameter(struct Patch *patch, int num,float value){
  SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
  if(plugin!=NULL)
    PLUGIN_set_effect_value(plugin, 0, plugin->type->num_effects+EFFNUM_COMP_RATIO+num, value, PLUGIN_NONSTORED_TYPE, PLUGIN_STORE_VALUE);
}

#else // COMPILING_RADIUM

static void set_compressor_parameter(struct Patch *patch, int num,float value){
  SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
  if(plugin!=NULL)
    COMPRESSOR_set_parameter(plugin->compressor,num,value);
}

#endif // #else COMPILING_RADIUM

static float get_compressor_parameter(struct Patch *patch, int num){
  SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
  if(plugin!=NULL)
    return COMPRESSOR_get_parameter(plugin->compressor,num);
  else
    return 0.0f;
}

static float get_graph_value(struct Patch *patch, int num){
  SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
  if(plugin!=NULL)
    return COMPRESSOR_get_graph_value(plugin->compressor,num);
  else
    return 0.0;
}


namespace radium_comp{


struct Comp : public QWidget, public QTimer{
struct Patch *_patch;

  // All values have the value 0 at top, and 1 at bottom, and:
  // 0 <= val1 <= val2 <= 1
  // (sometimes < instead of <=)

  // slider 1
  double in_val1; // 0
  double in_val2; // threshold

  // slider 2
  double out_val1; // used to calculate ratio ( ratio = in_val2/(out_val1-in_val2) )
  double out_val2; // threshold

  // slider 3
  double vol_val1; // used to calculate volume ( volume = out_val1/vol_val1 )
  double vol_val2; // threshold * volume

  int curr_slider;

  struct Box{
    int x1,y1,x2,y2;
    int p1,p2;

    Box(int _x1, int _y1, int _x2, int _y2, float val1,float val2)
      : x1(_x1)
      , y1(_y1)
      , x2(_x2)
      , y2(_y2)
    {
      p1 = scale(val1,0,1,y1,y2);
      p2 = scale(val2,0,1,y1,y2);
    }

    bool inside(int x,int y){
      return x>=x1 && x<x2 && y>y1 && y<y2;
    }
    bool in_val1(int y){
      int p=(p1+p2)/2;
      return y<p;
    }
    bool in_val2(int y){
      int p=(p1+p2)/2;
      return y>=p;
    }
  };

  struct Peaks{
    const static int num_peaks = 30;//24;
    
    float peaks[num_peaks]; // in dB
    int peak_pos;
    float last_peak;

#if GLIDING_PEAK_AREA
    float min_peak_db, max_peak_db; // in dB
#endif
    int min_peak, max_peak; // in pixels
    
    Peaks()
      : peak_pos(0)
      , last_peak(0)
#if GLIDING_PEAK_AREA
      , min_peak_db(0), max_peak_db(0)
#endif
      , min_peak(0)
      , max_peak(0)
    {
      for(int i=0;i<num_peaks;i++)
        peaks[i]=0.0f;
    }

    float get_min_peak(){
      float ret=max_db;
      for(int i=0;i<num_peaks;i++)
        if(peaks[i]<ret)
          ret=peaks[i];
      return ret;
    }

    float get_max_peak(){
      float ret=min_db;
      for(int i=0;i<num_peaks;i++)
        if(peaks[i]>ret)
          ret=peaks[i];
      return ret;
    }


#if GLIDING_PEAK_AREA
    // Trying gliding area instead of array. IMO it is less clear, and it also uses more CPU because of more gfx updates.
   void update_peaks(const Box &box, float peak){
      const float inc = 0.25;

      if(peak>max_peak_db)
        max_peak_db = peak;
      else
        max_peak_db = max_peak_db - inc;
  
      if(peak<min_peak_db)
        min_peak_db = peak;
      else
        min_peak_db = min_peak_db + inc;
      
      if(min_peak_db < min_db)
        min_peak_db = min_db;

      if(max_peak_db > max_db)
        max_peak_db = max_db;

      if(min_peak_db>max_peak_db)
        min_peak_db=max_peak_db;

      //if(box.x1<50)
      //  printf("min_peak_db: %f\n",min_peak_db);

      min_peak = scale(min_peak_db,min_db,max_db, box.y2, box.y1);
      max_peak = scale(max_peak_db,min_db,max_db, box.y2, box.y1);
    }

#else

    void set_min_max(const Box &box){
      min_peak = scale(get_min_peak(),min_db,max_db, box.y2, box.y1);
      max_peak = scale(get_max_peak(),min_db,max_db, box.y2, box.y1);
    }

#endif

    const int get_y_pixel(const Box &box){
      return scale(last_peak,min_db,max_db, box.y2, box.y1);
    }

    const QLine get_line_line(const Box &box){
      const int peak = get_y_pixel(box);
      return QLine(box.x1,peak,box.x2,peak);
    }

    const QLine get_line_to_next_box(const Box &box1, Peaks *peaks2, const Box &box2){
      Peaks *peaks1 = this;
      int x1=box1.x2;
      int y1=peaks1->get_y_pixel(box1);
      int x2=box2.x1;
      int y2=peaks2->get_y_pixel(box2);

      if(y1 < box1.y1){
        x1=scale(box1.y1, y1,y2, x1,x2);
        y1=box1.y1 - 10; // FIX. why is - 10 needed?
      }

      if(y2 < box2.y1){
        x2=scale(box2.y1, y1,y2, x1,x2);
        y2=box2.y1 - 10;
      }

      if(y1 > box1.y2){
        x1=scale(box1.y2, y1,y2, x1,x2);
        y1=box1.y2 + 10;
      }

      if(y2 > box2.y2){
        x2=scale(box2.y2, y1,y2, x1,x2);
        y2=box2.y2 + 10;
      }

      if(x1<box1.x2)
        x1=10000;
      if(x1>box2.x1)
        x1=10000;
      if(x2<box1.x2)
        x1=10000;
      if(x2>box2.x1)
        x1=10000;


      return QLine(x1,y1,x2,y2);
    }

    // For lines in boxes (those lines are always horizontal)
    const QRect get_line_rect(const Box &box){
      const int peak = get_y_pixel(box);
      return QRect(box.x1,peak-2,box.x2-box.x1,5);
    }

    // For the lines between boxes.
    const QPolygon get_line_region(const Box &box1, Peaks *peaks2, const Box &box2){
      const QLine line = get_line_to_next_box(box1,peaks2,box2);

      int x1=line.x1();
      int y1=line.y1();
      int x2=line.x2();
      int y2=line.y2();

      if(x1==10000)
        return QPolygon(); // Line is not painted.

#if 0
      double degree = 3.14159265359/2.0 - atan( (double)(line2.y1() - line1.y2()) / (double)(line2.x1() - line1.x2()));
      int h = ceil( (4.0/sin(degree)) / 2.0);
      //printf("degree: %f, h: %d\n",degree*57.2957795,h);
#else
      // optimized:
      int h_square = y2 - y1;
      h_square *= h_square;

      int w_square = x2 - x1;
      w_square *= w_square;

      int h = ceil(4.0/2.0 * sqrt((double)h_square/(double)w_square + 1.0)); // "4.0" is the pen width.
#endif

      QPolygon poly(4);

      poly.setPoint(0, x1, y1-h);
      poly.setPoint(1, x1, y1+h);
      poly.setPoint(2, x2, y2+h);
      poly.setPoint(3, x2, y2-h);

      return poly;
    }

    const QRect get_area_rect(const Box &box){
      return QRect(box.x1,max_peak,
                   box.x2-box.x1, min_peak-max_peak);
    }

    void paint_peak_area(QPainter *p, const Box &box){
      QColor col(40,90,140);
      col = col.light(80);
      col.setAlpha(120);

      p->fillRect(get_area_rect(box),col);
    }

    void paint_peak_line(QPainter *p, const Box &box){
      p->drawLine(get_line_line(box));
    }

    void paint_peak_line_between_boxes(QPainter *p, const Box &box1, Peaks *peaks2, const Box &box2){
      const QLine line = get_line_to_next_box(box1, peaks2, box2); 
      if(line.x1()==10000)
        return;

      p->drawLine(line);
    }

    QRegion add_peaks(const Box &box, float peak){

      QRegion reg_before = get_area_rect(box);

      if(peak<-1000)
        peak=-1000;
      if(peak>1000)
        peak=1000;

      last_peak = peak;
      //printf("adding peak %.2f. ",peak);
      peaks[peak_pos] = peak;
      peak_pos++;
      if(peak_pos==num_peaks)
        peak_pos=0;

#if USE_GLIDING_PEAK_AREA
      update_peaks(box,peak);
#else
      set_min_max(box);
#endif

      QRegion reg_after = get_area_rect(box);

      return reg_before.xored(reg_after);
    }
  };

  Peaks peaks_in;
  Peaks peaks_out;
  Peaks peaks_vol;

  float in_peak_value;
  float out_peak_value;
  float vol_peak_value;

  void timerEvent(QTimerEvent * e){ // virtual method from QTimer
    if(isVisible())
      update_peaks();
  }

Comp(struct Patch *patch, QWidget *parent)
    : QWidget(parent)
    , _patch(patch)
    , in_val1(0.0)
    , in_val2(def_threshold)
    , out_val1(def_ratio)
    , out_val2(def_threshold)
    , vol_val1(def_makeupgain)
    , vol_val2(def_threshold)
    , curr_slider(0)
  {
    setInterval(k_timer_interval); // QTimer
    start(); // QTimer

    set_threshold(def_threshold);
    set_compressor_parameters();

    update_peaks();
  }

  int get_box_width(){
    return (width()-1) / 4;
  }

  int get_width_between_boxes(){
    return get_box_width() / 2;
  }

  Box get_slider1_parms(){
    return Box(
               0,0,
               get_box_width(),
               height()-1,
               in_val1,in_val2);
  }
  
  Box get_slider2_parms(){
    return Box(
               get_box_width() + get_width_between_boxes(), 0,
               get_box_width() + get_width_between_boxes() + get_box_width(),
               height()-1,
               out_val1,out_val2);
  }

  Box get_slider3_parms(){
    return Box(
               get_box_width() + get_width_between_boxes() + get_box_width() + get_width_between_boxes(), 0,
               get_box_width() + get_width_between_boxes() + get_box_width() + get_width_between_boxes() + get_box_width(),
               height()-1,
               vol_val1, vol_val2);
  }

  double get_threshold(){
    return scale(in_val2,
                 in_val1,1,
                 max_db,min_db);
  }

  void set_threshold_gui(){
    set_threshold(scale(get_compressor_parameter(_patch, COMP_EFF_THRESHOLD),max_db,min_db,in_val1,1));
  }

  double get_ratio(){
    double len_in = in_val2-in_val1;
    double len_out = out_val2-out_val1;

    if(len_in==0.0 && len_out==0.0)
      return 1.0;

    if(len_out==0.0)
      return 60;

    double ratio = fabs(len_in-len_out)<0.00001
                                        ? 1.0
                                        : len_in / len_out;

    if(ratio>60)
      ratio=60;
    return ratio;
  }

  void set_ratio_gui(){
    double ratio = get_compressor_parameter(_patch, COMP_EFF_RATIO);
    if(ratio<1.0){
      set_ratio(scale(ratio,1,0,0,-1));
    }else
      set_ratio(out_val2 - out_val2/ratio);
  }

  double get_makeup_gain(){
    return scale(vol_val1,0,1,max_db,min_db) - scale(out_val1,0,1,max_db,min_db);
  }

  // set_ratio must be called first.
  void set_makeup_gain_gui(){
    float vol = get_compressor_parameter(_patch, COMP_EFF_OUTPUT_VOLUME);
    float addvol = scale(out_val1,0,1,max_db,min_db) + vol;
    set_makeupgain(scale(addvol,max_db,min_db,0,1));
  }

  // After loading.
  void set_gui_parameters(){
    set_threshold_gui();
    set_ratio_gui();
    set_makeup_gain_gui();
  }

  void set_compressor_parameters(){
    //double input_volume = 1.0;
    double threshold = get_threshold();
    double ratio = get_ratio();
    double output_volume = get_makeup_gain();
      
    set_compressor_parameter(_patch, COMP_EFF_RATIO,ratio);
    set_compressor_parameter(_patch, COMP_EFF_THRESHOLD, threshold);
    //set_compressor_parameter(2,attack);
    //set_compressor_parameter(3,release);
    //set_compressor_parameter(4,input_volume);
    set_compressor_parameter(_patch, COMP_EFF_OUTPUT_VOLUME,output_volume);

#if 0
    printf("%f / %f\n%f / %f\n%f / %f\ninput_volume:\t\t %.2f\n"
           "threshold:\t\t %f.2f\n"
           "ratio:\t\t\t %f.2f\n"
           "output_volume:\t\t %.2f\n\n",in_val2,out_val2,scale(vol_val1,0,1,max_db,min_db),scale(out_val1,0,1,max_db,min_db),vol_val1,out_val1,
           input_volume,
           threshold,
           ratio,
           output_volume);
#endif
  }

  enum{
    THRESHOLD_SLIDER = 1,
    RATIO_SLIDER,
    MAKEUPGAIN_SLIDER
  };

  double p_startpos;
  int y_startpos;

  void set_makeupgain(float val){
    //double old = vol_val2;
    vol_val1=val;
#if 0 // allow this.
    if(vol_val1<0.0)
      vol_val1=0.0;
#endif
    if(vol_val1>0.9999)
      vol_val1=0.9999;
    vol_val2 = vol_val1 + (out_val2-out_val1);// scale(out_val2, out_val1, 1.0, vol_val1, 1.0);
    //printf("setting makeupgain to %f. old/new vol_val2: %f / %f\n",val,vol_val2,old);
  }

  void set_ratio(float val){
    double vol_diff = vol_val1 - out_val1;
    out_val1=val;
    //    if(out_val1<0.0)
    //    out_val1=0.0;

    if(out_val1<0){
      double threshold=get_compressor_parameter(_patch, COMP_EFF_THRESHOLD); //get_threshold();
      double threshold_scaled=scale(threshold,max_db,min_db,0,1);
      double ratio = scale(val,0,-1,1,0); //1.0 + val; // val is the out slider value, which goes from 0 to -1 when we are in expanding mode.
      if(ratio<0.001)
        ratio=0.001;
      in_val1 = ((ratio-1)*threshold_scaled) / (ratio*threshold_scaled - ratio - threshold_scaled); // From: i1 = i2*(1-ratio), i2=scale(threshold_scaled,0,1,i1,1), i1=in_va1, i2=in_val2
      out_val1=0.0;
      if(in_val1>0.999)
        in_val1=0.999;
      in_val2=scale(threshold,max_db,min_db,in_val1,1);
      out_val2=in_val2;
    }else{
      in_val1=0.0;
    }
    if(out_val1>in_val2-0.0001)
      out_val1=in_val2-0.0001;

    set_makeupgain(vol_diff + out_val1);
  }

  void set_threshold(float val){
    //double bef=in_val2;
    double old_inval1_scaled = scale(in_val1,0,in_val2,0,1);
    double old_ratio_factor = out_val1 / (in_val2-in_val1);

    in_val2 = val;
    if(in_val2<=0.0)
      in_val2=0.00001;
    if(in_val2>0.9999)
      in_val2=0.9999;
    
    out_val2 = in_val2;

    if(in_val1<=0.0){
      set_ratio(old_ratio_factor*in_val2);
    }else{
      in_val1 = scale(old_inval1_scaled,0,1,0,in_val2);
      set_makeupgain(vol_val1);
      //printf("b %.2f - %.2f. Bef: %.2f, now: %.2f\n",in_val1,old_inval1_scaled,bef,in_val2);
    }
  }

  void handle_mouse_event ( QMouseEvent * event ){
    //printf("Got mouse press event %d / %d\n",(int)event->x(),(int)event->y());

    double new_val = p_startpos + scale(event->y()-y_startpos,0,height(),0,1);
    //printf("p_startpos: %f\n",p_startpos);

    switch(curr_slider){
    case THRESHOLD_SLIDER:
      set_threshold(new_val);
      break;
    case RATIO_SLIDER:
      set_ratio(new_val);
      break;
    case MAKEUPGAIN_SLIDER:
      set_makeupgain(new_val);
      break;
    }

    set_compressor_parameters();

    update();

    event->accept();
  }

  void mousePressEvent ( QMouseEvent * event )
  {
    Box in_box = get_slider1_parms();
    Box out_box = get_slider2_parms();
    Box vol_box = get_slider3_parms();

    int x = event->x();
    int y = event->y();

    if(in_box.inside(x,y)){
#ifdef COMPILING_RADIUM
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      Undo_AudioEffect_CurrPos(_patch, plugin->type->num_effects+EFFNUM_COMP_THRESHOLD);
#endif

      curr_slider = THRESHOLD_SLIDER;
      p_startpos = in_box.p2;

    }else if(out_box.inside(x,y)){
#ifdef COMPILING_RADIUM
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      Undo_AudioEffect_CurrPos(_patch, plugin->type->num_effects+EFFNUM_COMP_RATIO);
#endif

      curr_slider = RATIO_SLIDER;
      if(in_box.p1>0){
        p_startpos = scale(in_box.p1,0,in_box.p2,0,-height());
      }else{
        p_startpos = out_box.p1;
      }

    }else if(vol_box.inside(x,y)){
#ifdef COMPILING_RADIUM
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      Undo_AudioEffect_CurrPos(_patch, plugin->type->num_effects+EFFNUM_COMP_OUTPUT_VOLUME);
#endif

      curr_slider = MAKEUPGAIN_SLIDER;
      p_startpos = vol_box.p1;

    }

    p_startpos = scale(p_startpos,0,height(),0,1);
    y_startpos = y;
  }

  void mouseMoveEvent ( QMouseEvent * event )
  {
    handle_mouse_event(event);
  }

  void mouseReleaseEvent ( QMouseEvent * event )
  {
    handle_mouse_event(event);
    curr_slider = 0;
  }

  void paint_slider(QPainter *p, const Box &b){
    p->drawRect(b.x1, b.y1,
                b.x2-b.x1, b.y2-b.y1);

    p->drawLine(b.x1,b.p1,
                b.x2,b.p1);
    p->drawLine(b.x1,b.p2,
                b.x2,b.p2);
  }

  void paint_gradient(QPainter *p, const Box &in, const Box &out, const Box &vol){
    QColor col1(30,170,33);
    QColor col2(159,58,33);
    col2=col2.light(80);
    
    col1 = col1.light(80);
    col2 = col2.light(60);

    col1.setAlpha(120);
    col2.setAlpha(180);

    if(!isEnabled()){
      col1.setAlpha(40);
      col2.setAlpha(40);
    }

#if 0
#ifdef COMPILING_RADIUM
    QColor *colors = static_cast<EditorWidget*>(root->song->tracker_windows->os_visual.widget)->colors;
#else
    QColor *colors = g_colors;
#endif
    QColor c(98,59,33);

    col2 = c.light(80);
    col1 = colors[13].light(100);

    col2.setAlpha(160);
    col1.setAlpha(200);
#endif

    // in slider
    {
      QLinearGradient gradient(0,in.p1,0,in.y2);
      gradient.setColorAt(0,col1);
      gradient.setColorAt(1,col2);
      p->fillRect(in.x1 ,in.p1, in.x2-in.x1 ,in.y2-in.p1,QBrush(gradient));
    }

    float in_val = scale(in_val2,in_val1,1.0f,0.0f,1.0f);
    QColor col = mix_colors(col1,col2,1.0f-in_val);

    // out slider top
    {
      QLinearGradient gradient(0,out.p1,0,out.p2);
      gradient.setColorAt(0,col1);
      gradient.setColorAt(1,col);
      p->fillRect(out.x1, out.p1,
                  out.x2-out.x1,
                  out.p2-out.p1,
                  QBrush(gradient));
    }

    // out slider bot.
    {
      QLinearGradient gradient(0,out.p2,0,out.y2);
      gradient.setColorAt(0,col);
      gradient.setColorAt(1,col2);
      p->fillRect(out.x1, out.p2,
                  out.x2-out.x1,
                  out.y2-out.p2,
                  QBrush(gradient));
    }

    // area between in slider and out slider, below threshold 
    {
      QLinearGradient gradient(0,out.p2,
                               0,out.y2);
      gradient.setColorAt(0,col);
      gradient.setColorAt(1,col2);
      p->fillRect(in.x2, out.p2,
                  out.x1-in.x2,
                  out.y2-out.p2,
                  QBrush(gradient));
    }

    // area between in slider and out slider, above threshold
    for(int x=in.x2;x<out.x1;x++){
      int y1 = scale(x,
                     in.x2,out.x1,
                     in.p1,out.p1);
      int y2 = scale(x,
                     in.x2,out.x1,
                     in.p2,out.p2);
      QLinearGradient gradient(x,y1,x,y2);
      gradient.setColorAt(0,col1);
      gradient.setColorAt(1,col);
      p->setPen(QPen(QBrush(gradient),1));
      p->drawLine(x,y1,x,y2);
    }

    // volume slider
    {
      int p1 = vol.p1;
      int p2 = vol.p2;//scale(out.p2, out.p1, out.y2, vol.p1, vol.y2);
      int p3 = p2 + (out.y2-out.p2);

      // top
      {
        QLinearGradient gradient(0,p1,0,p2);
        gradient.setColorAt(0,col1);
        gradient.setColorAt(1,col);
        p->fillRect(vol.x1, p1,
                    vol.x2-vol.x1,
                    p2-p1,
                    QBrush(gradient));
      }
      
      // bot.
      {
        QLinearGradient gradient(0,p2,0,p3);
        gradient.setColorAt(0,col);
        gradient.setColorAt(1,col2);
        p->fillRect(vol.x1, p2,
                    vol.x2-vol.x1,
                    p3-p2,
                    QBrush(gradient));
      }

      QColor col3=col2.light(scale(p3,vol.y2,vol.y1,100,40));

      // below bot.
      {
        QLinearGradient gradient(0,p3,0,vol.y2);
        gradient.setColorAt(0,col2);
        gradient.setColorAt(1,col3);
        p->fillRect(vol.x1,p3,
                    vol.x2-vol.x1,vol.y2-p3,
                    QBrush(gradient));
      }

      // area between out slider and vol slider, above threshold
      for(int x=out.x2;x<vol.x1;x++){
        int y1 = scale(x,
                       out.x2,vol.x1,
                       out.p1,vol.p1);
        int y2 = scale(x,
                       out.x2,vol.x1,
                       out.p2,vol.p2);
        QLinearGradient gradient(x,y1,x,y2);
        gradient.setColorAt(0,col1);
        gradient.setColorAt(1,col);
        p->setPen(QPen(QBrush(gradient),1));
        p->drawLine(x,y1,x,y2);
      }

      // area between out slider and vol slider, below threshold
      for(int x=out.x2;x<vol.x1;x++){
        int y1 = scale(x,
                       out.x2,vol.x1,
                       out.p2,vol.p2);
        int y2 = vol.y2-1;
        QLinearGradient gradient(x,y1,x,y2);
        gradient.setColorAt(0,col);
        gradient.setColorAt(1,mix_colors(col2,col3,scale(x,out.x2,vol.x1,1.0f,0.0f)));
        p->setPen(QPen(QBrush(gradient),1));
        p->drawLine(x,y1,x,y2);
      }

    }

    //p->setBrush(QBrush());
    p->setPen(QPen());
  }


  // Called regularly
  void update_peaks(){
    Box in_box = get_slider1_parms();
    Box out_box = get_slider2_parms();
    Box vol_box = get_slider3_parms();

    in_peak_value = get_graph_value(_patch, 0);
    out_peak_value = in_peak_value + get_graph_value(_patch, 1);
    vol_peak_value = out_peak_value + get_makeup_gain();

    QRegion reg;

    // update old line boxes
    reg = peaks_in.get_line_rect(in_box);
    reg = reg.united(peaks_out.get_line_rect(out_box));
    reg = reg.united(peaks_vol.get_line_rect(vol_box));

    // update old lines between boxes
    reg = reg.united(peaks_in.get_line_region(in_box,&peaks_out,out_box));
    reg = reg.united(peaks_out.get_line_region(out_box,&peaks_vol,vol_box));

    // Add new peaks, and update areas in boxes (both old and new)
    reg = reg.united(peaks_in.add_peaks(in_box, scale(scale(in_peak_value,max_db,min_db,in_val1,1),0,1,max_db,min_db)));
    reg = reg.united(peaks_out.add_peaks(out_box,scale(scale(out_peak_value,max_db,min_db,in_val1,1),0,1,max_db,min_db)));
    reg = reg.united(peaks_vol.add_peaks(vol_box,scale(scale(vol_peak_value,max_db,min_db,in_val1,1),0,1,max_db,min_db)));

    // update new lines between boxes
    reg = reg.united(peaks_in.get_line_region(in_box,&peaks_out,out_box));
    reg = reg.united(peaks_out.get_line_region(out_box,&peaks_vol,vol_box));

    // update new line boxes
    reg = reg.united(peaks_in.get_line_rect(in_box));
    reg = reg.united(peaks_out.get_line_rect(out_box));
    reg = reg.united(peaks_vol.get_line_rect(vol_box));

    update(reg);
  }

  void paintPeaks(QPainter *p, const Box &in_box, const Box &out_box, const Box &vol_box){

    {
      QColor col(0,90,180);
      col = col.light(60);
      col.setAlpha(130);
      QPen pen(col);
      pen.setWidth(4);
      p->setPen(pen);
    }

    // paint lines
    {
      // in
      peaks_in.paint_peak_line(p, in_box);
      
      // between in and out
      peaks_in.paint_peak_line_between_boxes(p, in_box, &peaks_out, out_box);
      
      // out
      peaks_out.paint_peak_line(p, out_box);
      
      // between out and vol
      peaks_out.paint_peak_line_between_boxes(p, out_box, &peaks_vol, vol_box);
      
      // vol
      peaks_vol.paint_peak_line(p, vol_box);
    }

    // paint areas
    {
      peaks_in.paint_peak_area(p,in_box);
      peaks_out.paint_peak_area(p,out_box);
      peaks_vol.paint_peak_area(p,vol_box);
    }

#if 0 // for checking that get_line_region returns correct value.
    p->setPen(QPen());
    p->drawPolygon(peaks_in.get_line_region(in_box,&peaks_out,out_box));
    p->drawPolygon(peaks_out.get_line_region(out_box,&peaks_vol,vol_box));
#endif
  }

  int get_text_width(QString text){
    const QFontMetrics fn = QFontMetrics(QFont());
    return fn.width(text);
  }

  void paintSliderText(QPainter *p, const Box &box, QString text, double value, QString unit){
#ifdef COMPILING_RADIUM
    QColor *colors = static_cast<EditorWidget*>(root->song->tracker_windows->os_visual.widget)->colors;
#else
    QColor *colors = g_colors;
#endif
    QColor c(colors[1]);
    c.setAlpha(160);
    if(!isEnabled())
      c.setAlpha(80);
    p->setPen(QPen(c,1));

    QString value_text = QString::number(value,'g',3) + unit;
    QString one_line = text + " " + value_text;
    
    QRect rect(QPoint(box.x1+2,box.y1+2),QPoint(box.x2-4,box.y2-4));

    if(get_text_width(text) < rect.width() && get_text_width(value_text) < rect.width()){
      // horizontal

      p->drawText(rect,Qt::AlignTop|Qt::AlignLeft,
                  get_text_width(one_line)<rect.width()
                    ? one_line
                    : text+"\n"+value_text);

    }else{
      // vertical

      p->save();
      QPoint point(box.x1,box.y1); 
      p->translate(box.x1,0);//point);//x, y);
      p->rotate(90); // or 270
      //p->scale((box.x2-box.x1)/12,1.0);
      //point = p->xFormDev(point);
      p->drawText(5,-5, one_line);
      p->restore();

    }
    //p->drawText(box.x1,box.y2-20,QString::number(value));
  }

  void paint_box_fill(QPainter *p, const Box &box){
    QColor col(150,190,10);
    col.setAlpha(10);
    
    p->fillRect(box.x1, box.y1,
                box.x2-box.x1, box.y2-box.y1,
                col);      
  }

  void paintEvent ( QPaintEvent * ev ){
    QPainter p(this);

    //printf("Paint\n");

    Box in_box = get_slider1_parms();
    Box out_box = get_slider2_parms();
    Box vol_box = get_slider3_parms();

    paint_gradient(&p,in_box,out_box,vol_box);

    QColor black(0,0,0);
    black.setAlpha(80);
    QPen pen(black);
    //pen.setWidth(2);

    p.setPen(pen);

    paint_slider(&p, in_box);
    paint_slider(&p, out_box);
    paint_slider(&p, vol_box);

    // bottom line between sliders
    {
      p.drawLine(in_box.x2,in_box.y2,out_box.x1,out_box.y2);
      p.drawLine(out_box.x2,out_box.y2,vol_box.x1,vol_box.y2);
    }

    // threshold line between sliders
    {
      // between in and out
      p.drawLine(in_box.x2,in_box.p2,
                 out_box.x1,out_box.p2);

      if(out_box.p2!=vol_box.p2)
        p.setRenderHints(QPainter::Antialiasing,true);
 
      // between out and vol
      p.drawLine(out_box.x2,out_box.p2,
                 vol_box.x1,vol_box.p2);

      if(out_box.p2!=vol_box.p2)
        p.setRenderHints(QPainter::Antialiasing,false);
    }

    paint_box_fill(&p,in_box);
    paint_box_fill(&p,out_box);
    paint_box_fill(&p,vol_box);

    paintPeaks(&p, in_box, out_box, vol_box);

    // border line between sliders
    {
      black.setAlpha(60);
      QPen pen(black);
      p.setPen(pen);

      // between in and out
      {
        if(in_box.p1!=out_box.p1){
          p.setRenderHints(QPainter::Antialiasing,true);
          pen.setWidth(2);
        }

        p.drawLine(in_box.x2,in_box.p1,
                   out_box.x1,out_box.p1);
        
        if(in_box.p1!=out_box.p1){
          p.setRenderHints(QPainter::Antialiasing,false);
          pen.setWidth(1);     
        }
      }

      // between out and vol
      {
        if(out_box.p1!=vol_box.p1){
          p.setRenderHints(QPainter::Antialiasing,true);
          pen.setWidth(2);
        }

        p.drawLine(out_box.x2,out_box.p1,
                   vol_box.x1,vol_box.p1);
        
        if(out_box.p1!=vol_box.p1){
          p.setRenderHints(QPainter::Antialiasing,false);
          pen.setWidth(1);
        }
      }
    }

    p.setPen(QPen());

    paintSliderText(&p,in_box,"Threshold: ",get_threshold(), "dB");
    paintSliderText(&p,out_box,"Ratio: ",get_ratio(),":1");
    paintSliderText(&p,vol_box,"Makeup Gain: ",get_makeup_gain(),"dB");
  }

};

static double OS_get_double_from_string(const char *s){
  QLocale::setDefault(QLocale::C);
  QString string(s);
  return string.toDouble();
}

static float read_float(FILE *file){
  char temp[512] = {0};

  fgets(temp,500,file);

  return OS_get_double_from_string(temp);
}

class Compressor_widget : public QWidget, public Ui::Compressor_widget{
  Q_OBJECT

 public:
  bool initing;
  Comp *comp;
  struct Patch *_patch;

 Compressor_widget(struct Patch *patch, QWidget *parent=NULL)
    : QWidget(parent)
    , _patch(patch)
  {
    initing = true;
    setupUi(this);
    comp = new Comp(patch,this);
    comp->setEnabled(false);
    verticalLayout->insertWidget(1,comp);

    // Enable MyQSlider and MyQCheckBox to take care of undo/redo.
    {
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      attack_slider->_patch = patch;
      attack_slider->_effect_num = plugin->type->num_effects+EFFNUM_COMP_ATTACK;
      release_slider->_patch = patch;
      release_slider->_effect_num = plugin->type->num_effects+EFFNUM_COMP_RELEASE;
      enable_checkbox->_patch = patch;
      enable_checkbox->_effect_num = plugin->type->num_effects+EFFNUM_COMP_ONOFF;
    }

    update_gui();
    initing = false;
  }

  float get_exp_value(double val, double max_val, double y1, double y2){
    return scale_double(exp(val/max_val),
                        exp(0.0),expf(1.0),
                        y1,y2
                        );
  }

  float get_exp_inverted_value(double y, double max_val, double y1, double y2){
    return max_val * log(scale_double(y,y1,y2,exp(0.0),exp(1.0)));
  }

  void update_gui(){
    comp->set_gui_parameters();
    attack_slider->setValue(get_exp_inverted_value(get_compressor_parameter(_patch, COMP_EFF_ATTACK),1000,0,max_attack_release));
    release_slider->setValue(get_exp_inverted_value(get_compressor_parameter(_patch, COMP_EFF_RELEASE),1000,0,max_attack_release));

    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    enable_checkbox->setChecked(plugin->comp.is_on);

    update();
  }

  void load(QString filename){
    FILE *file = fopen(filename,"r");

    if(file==NULL){
      QMessageBox msgBox;
      msgBox.setText("Could not open file \""+filename+"\".");
      msgBox.setStandardButtons(QMessageBox::Ok);
      msgBox.setDefaultButton(QMessageBox::Ok);
      msgBox.exec();
      return;
    }
    
    set_compressor_parameter(_patch, COMP_EFF_RATIO,read_float(file)); // ratio
    set_compressor_parameter(_patch, COMP_EFF_THRESHOLD,read_float(file)); // threshold
    set_compressor_parameter(_patch, COMP_EFF_ATTACK,read_float(file)); // attack
    set_compressor_parameter(_patch, COMP_EFF_RELEASE,read_float(file)); // release
    //set_compressor_parameter(INPUT_VOLUME,read_float(file)); // input volume (currently not used)
    read_float(file); // input volume (currently not used)
    set_compressor_parameter(_patch, COMP_EFF_OUTPUT_VOLUME,read_float(file)); // output volume

    fclose(file);

    update_gui();
  }

public slots:

void on_enable_checkbox_toggled(bool val){
#ifdef COMPILING_RADIUM
  SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
  PLUGIN_set_effect_value(plugin, 0, plugin->type->num_effects+EFFNUM_COMP_ONOFF, val==true?1.0:0.0, PLUGIN_NONSTORED_TYPE, PLUGIN_STORE_VALUE);

  attack_slider->setEnabled(val);
  release_slider->setEnabled(val);
  comp->setEnabled(val);
#endif
}

  void on_attack_slider_valueChanged(int val){
    float attack = get_exp_value(val,1000,0,max_attack_release);
    set_compressor_parameter(_patch, COMP_EFF_ATTACK,attack);
    char temp[512];
    sprintf(temp,"Attack: %.2fms",attack);
    SLIDERPAINTER_set_string(attack_slider->_painter, temp);
  }

  void on_release_slider_valueChanged(int val){
    float release = get_exp_value(val,1000,0,max_attack_release);
    set_compressor_parameter(_patch, COMP_EFF_RELEASE,release);
    char temp[512];
    sprintf(temp,"Release: %.2fms",release);
    SLIDERPAINTER_set_string(release_slider->_painter, temp);
  }

  void on_bypass_toggled(bool val){
    printf("bypass: %d\n",(int)val);
    set_compressor_parameter(_patch, COMP_EFF_BYPASS,val==true?1.0f:0.0f);
  }

  void on_load_button_pressed(void){
    printf("load pressed\n");

    QString filename = QFileDialog::getOpenFileName(this, "Load Effect configuration", "", "Radium Compressor Configuration (*.rcc)");

    if(filename=="")
      return;

    load(filename);
  }

  void on_save_button_pressed(void){
    printf("save pressed\n");

    QString filename = QFileDialog::getSaveFileName(this, "Save Effect configuration", "", "Radium Compressor Configuration (*.rcc)");

    if(filename=="")
      return;

    FILE *file = fopen(filename,"w");

    if(file==NULL){
      QMessageBox msgBox;
      msgBox.setText("Could not save file.");
      msgBox.setStandardButtons(QMessageBox::Ok);
      msgBox.setDefaultButton(QMessageBox::Ok);
      msgBox.exec();
      return;
    }

    fprintf(file,"%f\n%f\n%f\n%f\n%f\n%f\n",
            get_compressor_parameter(_patch, COMP_EFF_RATIO),
            get_compressor_parameter(_patch, COMP_EFF_THRESHOLD),
            get_compressor_parameter(_patch, COMP_EFF_ATTACK),
            get_compressor_parameter(_patch, COMP_EFF_RELEASE),
            //get_compressor_parameter(INPUT_VOLUME),
            0.0f, // input volume (in dB)
            get_compressor_parameter(_patch, COMP_EFF_OUTPUT_VOLUME)
            );

    fclose(file);
  }
};

} // anon. namespace

