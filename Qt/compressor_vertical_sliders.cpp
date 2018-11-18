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

//#include <string>
#include <math.h>

#define GLIDING_PEAK_AREA 0 // Set to 1 to get gliding peaks area, which most meters use. Looks good, but I think it is slightly more confusing than the array used now.

static const int min_db = -40;
static const int max_db = 40;

#if !defined(COMPILING_RADIUM)
static const int k_timer_interval = 30;
#endif

const float def_threshold = 0.7f;
const float def_ratio = 0.3;
const float def_makeupgain = 0.3f;

namespace cvs{

struct Comp
  : public MyWidget
#if !defined(COMPILING_RADIUM)
  , public MyTimer
#endif
{
  Patch *_patch;

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

  // attack/release sliders
  double attack_val;
  double release_val;

  int curr_slider;

#ifdef JUCE_API
  Atomic<int32> gui_parameters_are_dirty;
#endif

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

  struct Slider : public Box{
    Slider(int _x1, int _y1, int _x2, int _y2, float val1,float val2)
      : Box(_x1,_y1,_x2,_y2,val1,val2)
    {
      p1 = scale(val1,0,1,x1,x2);
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
      const float inc = 0.1;

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

    int get_y_pixel(const Box &box) const {
      return scale(last_peak,min_db,max_db, box.y2, box.y1);
    }

    MyLine get_line_line(const Box &box) const {
      const int peak = get_y_pixel(box);
      return MyLine(box.x1,peak,box.x2,peak);
    }

    MyLine get_line_to_next_box(const Box &box1, Peaks *peaks2, const Box &box2) const{
      const Peaks *peaks1 = this;
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
        if (y1==y2)
          x1 = 10000; // I.e. not visible
        else
          x1=scale(box1.y2, y1,y2, x1,x2);
        y1=box1.y2 + 10;
      }

      if(y2 > box2.y2){
        if (y1==y2)
          x1 = 10000; // I.e. not visible
        else
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


      return MyLine(x1,y1,x2,y2);
    }

    // For lines in boxes (those lines are always horizontal)
    MyRect get_line_rect(const Box &box) const{
      const int peak = get_y_pixel(box);
      return MyRect(box.x1,peak-2,box.x2,peak+3);
    }

    void update_line_rect(MyWidget *widget, const Box &box){
      widget->update(get_line_rect(box));
    }

    // For the lines between boxes.
    void update_line_region(MyWidget *widget, const Box &box1, Peaks *peaks2, const Box &box2){
      const MyLine line = get_line_to_next_box(box1,peaks2,box2);

      int x1=line.x1;
      int y1=line.y1;
      int x2=line.x2;
      int y2=line.y2;

      if(x1==10000)
        return; // Line is not painted.

      if (x1==x2) // I.e. nothing is visible (since it's a line from one box to another, a vertical line doesn't make sense)
        return;
      
      int h_square = y2 - y1;
      h_square *= h_square;

      int w_square = x2 - x1;
      w_square *= w_square;

      int h = ceil(4.0/2.0 * sqrt((double)h_square/(double)w_square + 1.0)); // "4.0" is the pen width.
      
      if (h < -2000 || h > 2000){
        fprintf(stderr, "h: %d, y1: %d\n",h,y1);
#if defined(RELEASE)
        h = 10;
#else
        abort();
#endif
      }
      
      int y1_1 = y1-h;
      int y1_2 = y1+h;
      int y2_1 = y2-h;
      int y2_2 = y2+h;

      const int dx = 8;

      if(y1_1 < y2_1)
        for(int x=x1;x<x2;x+=dx){
          int y1=scale(x,x1,x2,y1_1,y2_1);
          int y2=scale(x+dx,x1,x2,y1_2,y2_2)+1;
          widget->update(x,y1,x+dx,y2);
        }
      else
        for(int x=x1;x<x2;x+=dx){
          int y1=scale(x+dx,x1,x2,y1_1,y2_1);
          int y2=scale(x,x1,x2,y1_2,y2_2)+1;
          widget->update(x,y1,x+dx,y2);
        }
    }

    const MyRect get_area_rect(const Box &box){
      return MyRect(box.x1,max_peak,
                    box.x2, min_peak);
    }

    void paint_peak_area(MyPainter *p, const Box &box){
      MyColor col(40,90,140);
      col = col.lighter(80);
      col.setAlpha(120);

      p->fillRect(get_area_rect(box), col);
    }

    void paint_peak_line(MyPainter *p, const Box &box, const MyColor &col){
      p->drawLine(get_line_line(box),col);
    }

    void paint_peak_line_between_boxes(MyPainter *p, const Box &box1, Peaks *peaks2, const Box &box2, const MyColor &col){
      const MyLine line = get_line_to_next_box(box1, peaks2, box2); 
      if(line.x1==10000)
        return;

      p->drawLine(line,col);
    }

    // Only works if rect1.x1==rect2.x1 && rect2.x2==rect2.x2
    void update_xored_rects(MyWidget *widget, const MyRect &rect1, const MyRect &rect2){
      if(rect1.y1==rect2.y1 && rect1.y2==rect2.y2)
        return;

      MyRect r1=rect1;
      MyRect r2=rect2;
      int x1=rect1.x1;
      int x2=rect1.x2;

      if(r1.y1 > r2.y1){
        MyRect temp=r1;
        r1=r2;
        r2=temp;
      }

#if 0 // for testing
      widget->update(x1,r1.y1,x2,r1.y2);
      widget->update(x1,r2.y1,x2,r2.y2);
      return;
#endif

      if(r1.y1==r2.y1 && r2.y2<r1.y2){
        widget->update(x1, r2.y2, x2, r1.y2);
        return;
      }

      if(r1.y1==r2.y1){
        widget->update(x1, r1.y2, x2, r2.y2);
        return;
      }

      if(r1.y2==r2.y2){
        widget->update(x1, r1.y1, x2, r2.y1);
        return;
      }

      if(r2.y2 < r1.y2){
        widget->update(x1, r1.y1, x2, r2.y1);
        widget->update(x1, r2.y2, x2, r1.y2);
        return;
      }

      if(r2.y1 < r1.y2){
        widget->update(x1, r1.y1, x2, r2.y1);
        widget->update(x1, r1.y2, x2, r2.y2);
      }

      // i.e. they are non-overlapping 
      widget->update(x1, r1.y1, x2, r1.y2);
      widget->update(x1, r2.y1, x2, r2.y2);
    }

    void add_peaks_and_update_area(MyWidget *widget, const Box &box, float peak){

      MyRect rect_before = get_area_rect(box);

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

#if GLIDING_PEAK_AREA
      update_peaks(box,peak);
#else
      set_min_max(box);
#endif

      MyRect rect_after = get_area_rect(box);

      update_xored_rects(widget, rect_before, rect_after);
    }
  };

  Peaks peaks_in;
  Peaks peaks_out;
  Peaks peaks_vol;

  bool background_image_must_be_updated;

  MyImage *background_image;

#ifdef JUCE_API
  Image cachedImage_radium_256x256x32_transparent_png;
#endif

#if !defined(COMPILING_RADIUM)
  void timer(){ // virtual method from MyTimer

    if(isVisible()){
#ifdef JUCE_API
      if(gui_parameters_are_dirty.compareAndSetValue(0,1)){
        set_gui_parameters();
        updateBackgroundImage();
        update();
      }
#endif
      update_peaks();
    }

  }
#endif
  
  void prepare_for_deletion(void){
#if !defined(COMPILING_RADIUM)
    MyTimer::prepare_for_deletion(); // <- This causes graphics to stop updating after redoing an undo. This will be fixed later.
#endif
  }
  
  virtual ~Comp(){
    prepare_for_deletion();
    //R_ASSERT_RETURN_IF_FALSE(false);
    delete background_image;
    background_image = NULL;
  }

 Comp(Patch *patch, void *parent)
    : MyWidget(parent)
    , _patch(patch)
    , in_val1(0.0)
    , in_val2(def_threshold)
    , out_val1(def_ratio)
    , out_val2(def_threshold)
    , vol_val1(def_makeupgain)
    , vol_val2(def_threshold)
    , curr_slider(0)
    , background_image_must_be_updated(true)
    , background_image(NULL)
#ifdef JUCE_API
    , cachedImage_radium_256x256x32_transparent_png (0)
#endif
  {
#ifdef JUCE_API
#ifndef COMPILING_MULTIBAND
    gui_parameters_are_dirty.set(0);
    cachedImage_radium_256x256x32_transparent_png = ImageCache::getFromMemory (Radium_Compressor::radium_256x256x32_transparent_png, Radium_Compressor::radium_256x256x32_transparent_pngSize);
    cachedImage_radium_256x256x32_transparent_png = cachedImage_radium_256x256x32_transparent_png.rescaled(530,350,Graphics::highResamplingQuality);
#endif
#endif

    createBackgroundImage();

#if !defined(COMPILING_RADIUM)
    startTimer(k_timer_interval); // MyTimer
#endif
    
    set_gui_parameters();
    //set_threshold(def_threshold);
    //set_compressor_parameters();

    update_peaks();
  }

  void createBackgroundImage(){
    delete background_image;
    background_image = new MyImage(width(),height());
    updateBackgroundImage();
  }

  void resized() override {
    if(width()>0 && height()>0)
      createBackgroundImage();
  }

  int get_box_width(){
    return (width()-1) / 4;
  }

  int get_slider_height(){
    return 20;
  }

  int get_box_height(){
    //return height()-((get_slider_height()+1)*2);
    return height()-1;
  }

  int get_width_between_boxes(){
    return get_box_width() / 2;
  }

  Box get_slider1_parms(){
    return Box(
               0,0,
               get_box_width(),
               get_box_height(),
               in_val1,in_val2);
  }
  
  Box get_slider2_parms(){
    return Box(
               get_box_width() + get_width_between_boxes(), 0,
               get_box_width() + get_width_between_boxes() + get_box_width(),
               get_box_height(),
               out_val1,out_val2);
  }

  Box get_slider3_parms(){
    return Box(
               get_box_width() + get_width_between_boxes() + get_box_width() + get_width_between_boxes(), 0,
               get_box_width() + get_width_between_boxes() + get_box_width() + get_width_between_boxes() + get_box_width(),
               get_box_height(),
               vol_val1, vol_val2);
  }

#if 0
  Slider get_attack_slider_parms(){
    return Box(
               0,get_box_height() + 1,
               width(), get_box_height() + get_slider_height()
               attack_val,0.0f);
  }

  Slider get_release_slider_parms(){
    return Box(
               0,get_box_height() + 1,
               width(), get_box_height() + get_slider_height()
               release_val,0.0f);
  }
#endif

  double get_threshold(){
    return scale_double(in_val2,
                        in_val1,1,
                        max_db,min_db);
  }

  void set_threshold_gui(){
     //    printf("threshold: %f. (%f -> %f)\n",get_compressor_parameter(_patch, COMP_EFF_THRESHOLD),(float)max_db,(float)min_db);
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

    if(ratio<1.0/60.0)
      return 1.0/60.0;

    else if(ratio>60)
      return 60;

    else
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

#ifdef JUCE_API
  void mark_gui_parameters_as_dirty(){
    gui_parameters_are_dirty.set(1);
  }
#endif

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

    if(get_makeup_gain() < -80)
      vol_val1 = out_val1 - scale(120,max_db,min_db,0,1);

    if(get_makeup_gain() > 80)
      vol_val1 = out_val1 + scale(120,max_db,min_db,0,1);

    //    if(get_makeup_gain() > 80)
    //  vol_val1 = out_val1 + scale(80,max_db,min_db,0,1);

#if 0 // allow this.
    if(vol_val1<0.0)
      vol_val1=0.0;
#endif
#if 0 // allow this too.
    if(vol_val1>0.9999)
      vol_val1=0.9999;
#endif
    vol_val2 = vol_val1 + (out_val2-out_val1);
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
    double old_inval1_scaled = in_val2==0 ? 0 : scale(in_val1,
                                                      0,in_val2,
                                                      0,1);
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

  void handle_mouse_event (int x, int y){
    //printf("Got mouse press event %d / %d\n",(int)event->x(),(int)event->y());

    double delta = scale_double(y-y_startpos,0,height(),0,1);

    if (ctrlPressed())
      delta = delta/10.0;
    
    double new_val = p_startpos + delta;
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

    background_image_must_be_updated=true;
    update();

    y_startpos = y;
    p_startpos = new_val;
  }

  bool mousePress(int x, int y) override {
    Box in_box = get_slider1_parms();
    Box out_box = get_slider2_parms();
    Box vol_box = get_slider3_parms();

    if(in_box.inside(x,y)){
#ifdef COMPILING_RADIUM
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      ADD_UNDO(AudioEffect_CurrPos(_patch, plugin->type->num_effects+EFFNUM_COMP_THRESHOLD));
#endif

      curr_slider = THRESHOLD_SLIDER;
      p_startpos = in_box.p2;


    }else if(out_box.inside(x,y)){
#ifdef COMPILING_RADIUM
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      ADD_UNDO(AudioEffect_CurrPos(_patch, plugin->type->num_effects+EFFNUM_COMP_RATIO));
#endif

      curr_slider = RATIO_SLIDER;
      if(in_box.p1>0){
        p_startpos = scale_double(in_box.p1,0,in_box.p2,0,-height());
      }else{
        p_startpos = out_box.p1;
      }

    }else if(vol_box.inside(x,y)){
#ifdef COMPILING_RADIUM
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      ADD_UNDO(AudioEffect_CurrPos(_patch, plugin->type->num_effects+EFFNUM_COMP_OUTPUT_VOLUME));
#endif

      curr_slider = MAKEUPGAIN_SLIDER;
      p_startpos = vol_box.p1;

    } else {
      return false;
    }

    p_startpos = scale_double(p_startpos,0,height(),0,1);
    y_startpos = y;

    switch(curr_slider){
    case THRESHOLD_SLIDER:
      set_compressor_automation_start(_patch,COMP_EFF_THRESHOLD);
      break;
    case RATIO_SLIDER:
      set_compressor_automation_start(_patch,COMP_EFF_RATIO);
      break;
    case MAKEUPGAIN_SLIDER:
      set_compressor_automation_start(_patch,COMP_EFF_OUTPUT_VOLUME);
      break;
    }

    return true;
  }

  bool mouseMove (int x, int y) override
  {
    if(curr_slider==0)
      return false;

    handle_mouse_event(x,y);
    return true;
  }

  bool mouseRelease (int x, int y) override
  {
    if(curr_slider==0)
      return false;

    switch(curr_slider){
    case THRESHOLD_SLIDER:
      set_compressor_automation_end(_patch,COMP_EFF_THRESHOLD);
      break;
    case RATIO_SLIDER:
      set_compressor_automation_end(_patch,COMP_EFF_RATIO);
      break;
    case MAKEUPGAIN_SLIDER:
      set_compressor_automation_end(_patch,COMP_EFF_OUTPUT_VOLUME);
      break;
    }

    handle_mouse_event(x,y);
    curr_slider = 0;
    return true;
  }

  void paint_box(MyPainter *p, const Box &b, const MyColor &col){
    p->drawRect(b.x1, b.y1,
                b.x2, b.y2,
                col);
    p->drawLine(b.x1,b.p1,
                b.x2,b.p1,
                col);
    p->drawLine(b.x1,b.p2,
                b.x2,b.p2,
                col);
  }

#if 0
  void paint_slider(MyPainter *p, const Slider &s, const MyColor &col){
    p->drawRect(s.x1, s.y1,
                s.x2, s.y2);
    p->drawLine(s.x1,s.p1,
                s.x2,s.p1);
  }
#endif

  void paint_gradient_rectangle(MyPainter *p, const int x1, const int y1, const int x2, const int y2, const MyColor &c1, const MyColor &c2){
#if 0
    for(int y=y1;y<y2;y++){
      p->setPen(mix_colors(c1,c2,scale(y, y1, y2, 1.0f,0.0f)));
      p->drawLine(x1,y,x2,y);
    }
#else
    p->setGradient(0,y1,0,y2,c1,c2);
    p->fillRect(x1, y1, x2 ,y2, c1);
    p->unsetGradient();
#endif
  }

  void paint_gradient_trapezoid(MyPainter *p, const int x1, const int y1_1, const int y1_2, const int x2, const int y2_1, const int y2_2, const int min_y, const int max_y, const MyColor &c1, const MyColor &c2){
#if 0
    if(y1_2-y1_1 >= y2_2-y2_1)
      for(int y1=y1_1;y1<y1_2;y1++){
        int y2=scale(y1,y1_1,y1_2,y2_1,y2_2);
        p->setPen(mix_colors(c1,c2,scale(y1, y1_1,y1_2, 1.0f,0.0f)));
        p->drawLine(x1,y1,x2,y2);
      }
    else
      for(int y2=y2_1;y2<y2_2;y2++){
        int y1=scale(y2,y2_1,y2_2,y1_1,y1_2);
        p->setPen(mix_colors(c1,c2,scale(y2, y2_1,y2_2, 1.0f,0.0f)));
        p->drawLine(x1,y1,x2,y2);
      }
#else
    for(float x=x1;x<x2;x+=0.2){
      float y1=scale(x,x1,x2,y1_1,y2_1) + 0.1;
      float y2=scale(x,x1,x2,y1_2,y2_2);

      p->setGradient(x,y1,x,y2,c1,c2);
      //p->drawLine(x,R_BOUNDARIES(min_y,y1,max_y),x,R_BOUNDARIES(min_y,y2,max_y),c1);
      p->fillRect(x,y1,x+0.2,y2,c1); // using fillRect since gradient drawLine doesn't seem to work with juce.
    }
    p->unsetGradient();
#endif
  }

  void paint_gradient(MyPainter *p, const Box &in, const Box &out, const Box &vol){
#if 0 // for testing if clipping is accurate
    QColor col1(qrand()%255,qrand()%255,qrand()%255);
    QColor col2(qrand()%255,qrand()%255,qrand()%255);
#else
    MyColor col1(30,170,33);
    MyColor col2(159,58,33);
#endif
    col2=col2.lighter(80);
    
    col1 = col1.lighter(80);
    col2 = col2.lighter(60);

    //col1.setAlpha(120);
    //col2.setAlpha(180);

    if(!isEnabled()){
      //col1.setAlpha(40);
      //col2.setAlpha(40);
      col1 = mix_mycolors(col1, MyColor(), 0.4);
      col2 = mix_mycolors(col2, MyColor(), 0.4);
    }else{
      col1 = mix_mycolors(col1, MyColor(), 0.8);
      col2 = mix_mycolors(col2, MyColor(), 0.8);
    }

    // in slider
    paint_gradient_rectangle(p, in.x1,in.p1, in.x2,in.y2, col1, col2);

    float in_val = scale(in_val2,in_val1,1.0f,0.0f,1.0f);
    MyColor col = mix_mycolors(col1,col2,1.0f-in_val);

    // out slider top
    paint_gradient_rectangle(p, out.x1,out.p1, out.x2,out.p2, col1,col);

    // out slider bot.
    paint_gradient_rectangle(p, out.x1,out.p2, out.x2,out.y2, col, col2);

    // area between in slider and out slider, below threshold 
    paint_gradient_rectangle(p, in.x2,in.p2, out.x1,out.y2, col, col2);

    // area between in slider and out slider, above threshold
    paint_gradient_trapezoid(p, in.x2,in.p1,in.p2, out.x1,out.p1,out.p2, out.y1, out.y2, col1, col);

    // volume slider
    {
      int p1 = vol.p1;
      int p2 = vol.p2;//scale(out.p2, out.p1, out.y2, vol.p1, vol.y2);
      int p3 = p2 + (out.y2-out.p2);

      // top
      paint_gradient_rectangle(p, vol.x1,p1, vol.x2,p2, col1, col);
      
      // middle.
      paint_gradient_rectangle(p, vol.x1,p2, vol.x2,p3, col, col2);

      //QColor col3=col2.light(scale(p3,vol.y2,vol.y1,100,40));
      MyColor col3=mix_mycolors(col2,MyColor(1,1,1),R_BOUNDARIES(0.0f, scale_double(p3,vol.y2,vol.y1,1.0f,0.0f), 1.0f));

      // bot.
      paint_gradient_rectangle(p, vol.x1,p3, vol.x2,vol.y2, col2, col3);

      // area between out slider and vol slider, above threshold
      paint_gradient_trapezoid(p,
                               out.x2, out.p1, out.p2,
                               vol.x1, vol.p1, vol.p2,
                               vol.y1, vol.y2,
                               col1,col);

      // area between out slider and vol slider, below threshold, colors in the col-col2 range.
      paint_gradient_trapezoid(p,
                               out.x2, out.p2,out.y2,
                               vol.x1, p2,p3,
                               vol.y1, vol.y2,
                               col,col2);

      // area between out slider and vol slider, further below threshold, colors in the col2-col3 range.
      if(p3 < vol.y2)
        paint_gradient_trapezoid(p,
                                 out.x2, out.y2, out.y2 + (vol.y2-p2),
                                 vol.x1, p3, vol.y2,
                                 vol.y1, vol.y2,
                                 col2,col3);
    }
  }


  // Called regularly
  void update_peaks(){
    Box in_box = get_slider1_parms();
    Box out_box = get_slider2_parms();
    Box vol_box = get_slider3_parms();

    float in_peak_value = get_graph_value(_patch, 0);
    float out_peak_value = in_peak_value + get_graph_value(_patch, 1);
    //vol_peak_value = out_peak_value + get_makeup_gain();

    //printf("in_peak_value: %f. out: %f, vol: %f\n",in_peak_value,out_peak_value,vol_peak_value);

    // update old line boxes
    peaks_in.update_line_rect(this,in_box);
    peaks_out.update_line_rect(this,out_box);
    peaks_vol.update_line_rect(this,vol_box);

    // update old lines between boxes
    peaks_in.update_line_region(this,in_box,&peaks_out,out_box);
    peaks_out.update_line_region(this,out_box,&peaks_vol,vol_box);

    // Add new peaks, and update areas in boxes (both old and new)
    peaks_in.add_peaks_and_update_area(this,in_box, scale(scale(in_peak_value,max_db,min_db,in_val1,1),0,1,max_db,min_db));
    peaks_out.add_peaks_and_update_area(this,out_box,scale(scale(out_peak_value,max_db,min_db,in_val1,1),0,1,max_db,min_db));
    //peaks_vol.add_peaks_and_update_area(this,vol_box,scale(scale(vol_peak_value,max_db,min_db,in_val1,1),0,1,max_db,min_db));
    peaks_vol.add_peaks_and_update_area(this,vol_box,
                                        scale(scale(out_peak_value,max_db,min_db,in_val1,1), 0,1, max_db,min_db)
                                        + scale(scale(get_makeup_gain(),max_db,min_db,0,1), 0,1, max_db,min_db));

    // update new lines between boxes
    peaks_in.update_line_region(this,in_box,&peaks_out,out_box);
    peaks_out.update_line_region(this,out_box,&peaks_vol,vol_box);

    // update new line boxes
    peaks_in.update_line_rect(this,in_box);
    peaks_out.update_line_rect(this,out_box);
    peaks_vol.update_line_rect(this,vol_box);
  }

#if defined(COMPILING_RADIUM)
  void calledRegularlyByParent(void){
    //printf("   comp reg %p\n",this);
    update_peaks();
  }
#endif
  
  void paintPeaks(MyPainter *p, const Box &in_box, const Box &out_box, const Box &vol_box){

    // peak lines
    {
      MyColor col(0,90,180);
      col = col.lighter(60);
      col.setAlpha(130);
      
      p->setThickness(4);
      
      // paint lines
      {
      // in
        peaks_in.paint_peak_line(p, in_box, col);
        
        // between in and out
        peaks_in.paint_peak_line_between_boxes(p, in_box, &peaks_out, out_box, col);
        
        // out
        peaks_out.paint_peak_line(p, out_box, col);
        
        // between out and vol
        peaks_out.paint_peak_line_between_boxes(p, out_box, &peaks_vol, vol_box, col);
        
        // vol
        peaks_vol.paint_peak_line(p, vol_box, col);
      }
      
      p->setThickness(1);
    }

    // peak areas
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

  void paintSliderText(MyPainter *p, const Box &box, std::string text, double value, const std::string unit){
    MyColor c(1);

#if 1 //ndef JUCE_API
    c.setAlpha(220);
    if(!isEnabled())
      c.setAlpha(80);
#endif

    if(value==-0.0)
      value=0.0;

    char temp[32];
    snprintf(temp,31,"%.1f",value);
    const char *maybeplus = unit.compare("dB")==0 && value>0.0 ? "+" : "";
    std::string value_text = maybeplus; value_text.append(temp); value_text.append(unit);
    std::string one_line = text; one_line.append(value_text);
    
    MyRect rect(box.x1+2,box.y1+2,box.x2-4,box.y2-4);

#ifdef JUCE_API
    if(true){
#else
    if(p->getTextWidth(text) < rect.width() && p->getTextWidth(value_text) < rect.width()){
#endif
      // horizontal

#if 0
      // linebreaks in strings doesn't seem to work in juce.
      p->drawText(rect, p->getTextWidth(one_line)<rect.width()
                  ? one_line
                  : text+"\n"+value_text,
                  c);
#endif

      if(p->getTextWidth(one_line)<rect.width())
        p->drawText(rect,one_line,c);
      else{
        // Do the linebreak manually instead.
        p->drawText(rect,text,c);
        rect.y1+=p->getTextHeight();
        p->drawText(rect,value_text,c);
      }

    }else{

      p->drawVerticalText(box.x1,box.y1,one_line,c);

    }
    //p->drawText(box.x1,box.y2-20,QString::number(value));
  }

  void paint_box_fill(MyPainter *p, const Box &box){
    MyColor col(150,190,10);
    col.setAlpha(10);

    if(box.p1>box.y1)
      p->fillRect(box.x1, box.y1,
                  box.x2, box.p1,
                  col);      
    if(box.p2<box.y2)
      p->fillRect(box.x1, box.y2,
                  box.x2, box.p2,
                  col);      
  }

  void paintBackgroundImage(MyPainter *p){

    p->fillRect(0,0,width(),height(),MyColor());

    //printf("Paint\n");

    Box in_box = get_slider1_parms();
    Box out_box = get_slider2_parms();
    Box vol_box = get_slider3_parms();

    paint_gradient(p,in_box,out_box,vol_box);

    MyColor black(1,1,1);
#ifdef JUCE_API
    black.setAlpha(140);
#else
    black.setAlpha(80);
#endif

    paint_box(p, in_box, black);
    paint_box(p, out_box, black);
    paint_box(p, vol_box, black);

    // bottom line between sliders
#ifndef JUCE_API
    if(1){
#ifndef JUCE_API
      MyColor c = mix_mycolors(MyColor(1,1,1), MyColor(), 0.4); // Alpha color will not look correct here since the color it's drawn over differs.
#else
      MyColor c(1,1,1);// = mix_mycolors(MyColor(1,1,1), MyColor(), 0.8); // Alpha color will not look correct here since the color it's drawn over differs.
#endif
      p->drawLine(in_box.x2,in_box.y2,vol_box.x1,out_box.y2,c);
      p->drawLine(out_box.x2,out_box.y2,vol_box.x1,vol_box.y2,c);
    }
#else
    p->drawLine(in_box.x1,out_box.y2,vol_box.x2,vol_box.y2,MyColor(1,1,1));
#endif

    // threshold line between sliders
    if(1){
      // between in and out
      p->drawLine(in_box.x2,in_box.p2,
                  out_box.x1,out_box.p2,
                  black
                  );

      // between out and vol
      p->drawLine(out_box.x2,out_box.p2,
                  vol_box.x1,vol_box.p2,
                  black);
    }

    paint_box_fill(p,in_box);
    paint_box_fill(p,out_box);
    paint_box_fill(p,vol_box);

    // //

    // border line between sliders
    if(1){
      black.setAlpha(80);

      if(in_box.p1!=out_box.p1)
        p->setThickness(1);

      // between in and out
      p->drawLine(in_box.x2,in_box.p1,
                  out_box.x1,out_box.p1,
                  black);

      if(out_box.p1!=vol_box.p1)
        p->setThickness(1);
      else
        p->setThickness(1);

      // between out and vol
      p->drawLine(out_box.x2,out_box.p1,
                  vol_box.x1,vol_box.p1,
                  black);
        
      p->setThickness(1);
    }

    paintSliderText(p,in_box,"Threshold: ",get_threshold(), "dB");
    {
      double ratio = get_ratio();
      if(ratio<1)
        paintSliderText(p,out_box,"Ratio: 1:",1.0/ratio,"");
      else
        paintSliderText(p,out_box,"Ratio: ",ratio,":1");
    }
    paintSliderText(p,vol_box,"Makeup Gain: ",get_makeup_gain(),"dB");


#ifdef JUCE_API
  //p->drawLine(in_box.x1,out_box.y2-1,vol_box.x2,vol_box.y2-1,MyColor(1,1,1));
  p->drawLine(in_box.x1,out_box.y2,vol_box.x2,vol_box.y2,MyColor());//1,1,1));

#if 0
    p->g->setOpacity(0.035f);
    p->g->drawImageAt(cachedImage_radium_256x256x32_transparent_png,
                      (width()-cachedImage_radium_256x256x32_transparent_png.getWidth())/2,
                      (height()-cachedImage_radium_256x256x32_transparent_png.getHeight())/2);
    p->g->setOpacity(1.0f);
#endif

#endif

    background_image_must_be_updated = false;
  }

  void updateBackgroundImage(){
    MyPainter p(background_image);
    //background_image->fill(colors[11]);
    paintBackgroundImage(&p);
  }

  void repaint(MyPainter *p) override {

    if(background_image_must_be_updated==true)
      updateBackgroundImage();

    Box in_box = get_slider1_parms();
    Box out_box = get_slider2_parms();
    Box vol_box = get_slider3_parms();

    //paintBackgroundImage(&p);
    p->drawImage(0,0,background_image);

    paintPeaks(p, in_box, out_box, vol_box);
  }

};
}
