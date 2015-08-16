/* Copyright 2012 Kjetil S. Matheussen

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

#include <QWidget>
#include <QGraphicsItem>
#include <QTimer>
#include <QAbstractSlider>
#include <QTimerEvent>

#include "EditorWidget.h"
#include "../common/threading.h"

#include "Qt_SliderPainter_proc.h"

#include "Qt_MyWidget.h"
#include "MySliderPainterPainter.h"


static const int k_timer_interval = 50;

static int scale_int(int x, int x1, int x2, int y1, int y2){
  return (int)scale((float)x,(float)x1,(float)x2,(float)y1,(float)y2);
}

static float gain2db(float val){
  if(val<=0.0f)
    return -100.0f;

  return 20*log10(val);
}

static float db2linear(float db){
  if(db<-70)
    return 0.0f;
  else if(db>40)
    return 1.0f;
  else
    return scale(db,-70.0f,40.0f,0.0f,1.0f);
}

#if 0
// Function iec_scale picked from meterbridge by Steve Harris.
// db is a value between 0 and 1.

static float iec_scale(float db) {
  
  db = 20.0f * log10f(db);


         float def = 0.0f; /* Meter deflection %age */
 
         if (db < -70.0f) {
                 def = 0.0f;
         } else if (db < -60.0f) {
                 def = (db + 70.0f) * 0.25f;
         } else if (db < -50.0f) {
                 def = (db + 60.0f) * 0.5f + 5.0f;
         } else if (db < -40.0f) {
                 def = (db + 50.0f) * 0.75f + 7.5;
         } else if (db < -30.0f) {
                 def = (db + 40.0f) * 1.5f + 15.0f;
         } else if (db < -20.0f) {
                 def = (db + 30.0f) * 2.0f + 30.0f;
         } else if (db < 0.0f) {
                 def = (db + 20.0f) * 2.5f + 50.0f;
         } else {
                 def = 100.0f;
         }
 
         return def * 2.0f / 200.0f;
}
#endif




#define MAX_CHANNELS 8

//namespace{ // stupid c++ with all this useless syntax necessary to make a program appear to work. C++ is only great if you like a challenge and don't want things done.

namespace{
struct AutomationOrPeakData{
  float *value;
  int requested_pos;
  int last_drawn_pos;

  int *color;

  int ch;
  int num_ch;

  bool is_automation;
};
}

static int DATA_get_y1(AutomationOrPeakData *data, int height){
  R_ASSERT(data->num_ch>0);
  return scale_int(data->ch,0,data->num_ch,1,height-1);
}

static int DATA_get_y2(AutomationOrPeakData *data, int height){
  return scale_int(data->ch+1,0,data->num_ch,1,height-1);
}

struct SliderPainter{

  struct Timer : public QTimer{
    SliderPainter *_painter;

    void timerEvent(QTimerEvent * e){
      R_ASSERT(THREADING_is_main_thread());
      
      if(_painter->isVisible()==false)
        return;

      for(int i=0;i<(int)_painter->_data.size();i++){
        AutomationOrPeakData *data = _painter->_data.at(i);
    
        float gain;

        if(data->is_automation)
          gain = *data->value;
        else{
          float db = gain2db(*data->value);
          if(db>4.0f)
            _painter->_peak_color = 14;
          else if(db>0.0f)
            _painter->_peak_color = 6;
          else
            _painter->_peak_color = 13;

          gain = db2linear(db);
        }

        data->requested_pos = scale(gain,0.0f,1.0f,
                                    0.0f,(float)_painter->width())
                              - 1;

        if(data->last_drawn_pos != data->requested_pos){

          //printf("Painting. Last drawn: %d. requested: %d\n",data->last_drawn_pos,data->requested_pos);


          int y1 = DATA_get_y1(data,_painter->height());
          int y2 = DATA_get_y2(data,_painter->height());
          int height = y2-y1;

          //printf("y1: %d, y2: %d, height: %d. req: %d, last: %d\n",y1,y2,height,data->requested_pos,data->last_drawn_pos);
          _painter->update(data->requested_pos,
                           y1,4,height+1);
          _painter->update(data->last_drawn_pos,
                           y1,4,height+1);

          //printf("%d - %d\n",data->requested_pos,data->last_drawn_pos);
        }
      }

      //e->accept();
    }
  }; // struct Timer

  std::vector<AutomationOrPeakData*>_data;
  //QVector<AutomationOrPeakData*>_data;
  
  QAbstractSlider *_qslider;
  QGraphicsItem *_graphics_item;  // Either widget or _graphics_item must be set.

  int _x1;
  int _y1;
  int _x2;
  int _y2;

  float *_peak_values;
  bool _local_peak_values;
  float _automation_value;
  int _automation_color;
  int _peak_color;

  bool _alternative_color;

  int _value;

  Timer _timer;

  QString _display_string;

  int _num_channels;

  bool _auto_updater_has_started;


  int value(){
    if(_qslider!=NULL)
      return _qslider->value();
    else
      return _value;
  }

  int minimum(){
    if(_qslider!=NULL)
      return _qslider->minimum();
    else
      return 0;
  }

  int maximum(){
    if(_qslider!=NULL)
      return _qslider->maximum();
    else
      return 10000;
  }

  int orientation(){
    if(_qslider!=NULL)
      return _qslider->orientation();
    else
      return Qt::Horizontal;
  }

  bool isEnabled(){
    if(_qslider!=NULL)
      return _qslider->isEnabled();
    else
      return _num_channels>0;
  }

  bool isVisible(){
    if(_qslider!=NULL)
      return _qslider->isVisible() && !_qslider->isHidden();
    else
      return _graphics_item->isVisible();
  }

  void update(int x, int y, int width, int height){
    //fprintf(stderr,"x: %d. _qslider: %p\n",x,_qslider);
    if(x<0)
      x=0;
    if(y<0)
      y=0;

    //fprintf(stderr,"y: %d. _qslider: %p\n",y,_qslider);
    if(_qslider!=NULL){
      return _qslider->update(x,y,width,height);
    }else
      return _graphics_item->update(_x1+x, _y1+y, width, height);
  }

  int width(){
    if(_qslider!=NULL)
      return _qslider->width();
    else
      return _x2-_x1;
  }

  int height(){
    if(_qslider!=NULL)
      return _qslider->height();
    else
      return _y2-_y1;
  }

  void init(){
    _value = 0;
    _num_channels = 0;
    _peak_values = NULL;
    _local_peak_values=false;
    _automation_color = 13;
    _peak_color = 13;
    _alternative_color = false;
    _auto_updater_has_started = false;
    _automation_value = 0.0f;
  }

  SliderPainter(QAbstractSlider *widget)
    : _qslider(widget)
    , _graphics_item(NULL)
  {
    init();
  }

  SliderPainter(QGraphicsItem *graphics_item)
    : _qslider(NULL)
    , _graphics_item(graphics_item)
  {
    init();
  }

  ~SliderPainter(){
    if(_local_peak_values==true)
      free(_peak_values);

    for(int i=0;i<(int)_data.size();i++) // Don't like iterators. Rather free memory manually than using them.
      delete _data.at(i);
  }

  void start_auto_updater() {
    R_ASSERT(THREADING_is_main_thread());
    if (_auto_updater_has_started==false){
      _timer._painter = this;
      _timer.setInterval(k_timer_interval);
      _timer.start();
      _auto_updater_has_started = true;
    }
  }

  AutomationOrPeakData *create_automation_data(){
    R_ASSERT(THREADING_is_main_thread());
    
    AutomationOrPeakData *data = new AutomationOrPeakData;

    data->value          = &_automation_value;
    data->requested_pos  = 0;
    data->last_drawn_pos = 0;

    data->color = &_automation_color;

    data->ch     = 0;
    data->num_ch = 1;

    data->is_automation = true;

    _data.push_back(data);

    start_auto_updater();

    return data;
  }

  float *obtain_automation_value_pointer(){
    create_automation_data();
    return &_automation_value;
  }

  int *obtain_automation_color_pointer(){
    return &_automation_color;
  }

  float *obtain_peak_value_pointers(int num_channels, float *peak_values){
    R_ASSERT(THREADING_is_main_thread());
    
    if(peak_values==NULL) {
      _peak_values = (float*)calloc(sizeof(float),num_channels);

      _local_peak_values = true;

    } else {

      _peak_values = peak_values;
    } 

    
    _data.clear();

    for(int ch=0;ch<num_channels;ch++){
      AutomationOrPeakData *data = create_automation_data();
      data->ch            = ch;
      data->num_ch        = num_channels;
      data->is_automation = false;
      data->value         = &_peak_values[ch];
      data->color         = &_peak_color;
    }

    return _peak_values;
  }

  void set_peak_value_pointers(int num_channels, float *pointers){
    obtain_peak_value_pointers(num_channels, pointers);
  }

  void paint(QPainter *p){
    R_ASSERT(THREADING_is_main_thread());
        
#ifdef COMPILING_RADIUM
    QColor *colors = static_cast<EditorWidget*>(root->song->tracker_windows->os_visual.widget)->colors;
#else
    QColor *colors = g_colors;
#endif

    cvs::MyPainter mp(p);

    SLIDERPAINTERPAINTER_paint(&mp,0,0,width(),height(),
                               isEnabled(), 
                               scale(value(),minimum(),maximum(),0.0f,1.0f),
                               _display_string.toStdString(),
                               _alternative_color);

    for(int i=0;i<(int)_data.size();i++){
      AutomationOrPeakData *data = _data.at(i);

      //printf("%s: sp: %p, i: %d, size: %d (%d/%d)\n",_display_string.toUtf8().constData(),this,(int)i,(int)_data.size(),sizeof(float),sizeof(float*));
      
      int y1 = DATA_get_y1(data,height());
      int y2 = DATA_get_y2(data,height());
      int height = y2-y1;
      
      p->fillRect(data->requested_pos+1 ,y1+1,
                  2,                    height-1,
                  colors[*data->color]);
      
      p->setPen(QPen(colors[11].light(120),1));
      p->drawRect(data->requested_pos, y1,
                  3,                   height);
      
      data->last_drawn_pos = data->requested_pos;
    }
  }

};
//}

SliderPainter *SLIDERPAINTER_create(QAbstractSlider *qslider){
  return new SliderPainter(qslider);
}

SliderPainter *SLIDERPAINTER_create(QGraphicsItem *graphics_item, int x1, int y1, int x2, int y2){
  SliderPainter *painter = new SliderPainter(graphics_item);
  painter->_x1=x1;
  painter->_y1=y1;
  painter->_x2=x2;
  painter->_y2=y2;

  return painter;
}


void SLIDERPAINTER_delete(SliderPainter *painter){
  R_ASSERT(THREADING_is_main_thread());
#if 1
  delete painter;
#else
  SLIDERPAINTER_became_invisible(painter);
#endif
}

void SLIDERPAINTER_start_auto_updater(SliderPainter *painter){
  R_ASSERT(THREADING_is_main_thread());
  painter->start_auto_updater();
}

void SLIDERPAINTER_became_visible(SliderPainter *painter){
  R_ASSERT(THREADING_is_main_thread());
  if(painter->_auto_updater_has_started==true)
    painter->_timer.start();
}

void SLIDERPAINTER_became_invisible(SliderPainter *painter){
  R_ASSERT(THREADING_is_main_thread());
  if(painter->_auto_updater_has_started==true)
    painter->_timer.stop();
}

void SLIDERPAINTER_setValue(SliderPainter *painter, int value){
  painter->_value = value;
}

void SLIDERPAINTER_set_num_channels(SliderPainter *painter, int num_channels){
  painter->_num_channels = num_channels;
}

void SLIDERPAINTER_paint(SliderPainter *painter, QPainter *p){
  painter->paint(p);
}

float *SLIDERPAINTER_obtain_peak_value_pointers(SliderPainter *painter, int num_channels){
  return painter->obtain_peak_value_pointers(num_channels,NULL);
}

void SLIDERPAINTER_set_peak_value_pointers(SliderPainter *painter, int num_channels, float *pointers){
  painter->set_peak_value_pointers(num_channels, pointers);
  //painter->set_peak_value_pointers(num_channels, NULL);
}

float *SLIDERPAINTER_obtain_automation_value_pointer(SliderPainter *painter){
  return painter->obtain_automation_value_pointer();
}
int *SLIDERPAINTER_obtain_automation_color_pointer(SliderPainter *painter){
  return painter->obtain_automation_color_pointer();
}
void SLIDERPAINTER_release_automation_pointers(SliderPainter *painter){
  // Removed: 1. It's not very important to remove it.
  //          2. In order to remove the correct one, we need to know which automation to remove.
  //          3. Code was very complicated
  //          4. Peaks for the bottom bar disappeared. (very complicated code)
  //painter->release_automation_pointers();
}

// Used for chips where the slider controls input volume instead of output volume.
void SLIDERPAINTER_set_alternative_color(SliderPainter *painter){
  painter->_alternative_color = true;
}

void SLIDERPAINTER_set_string(SliderPainter *painter,QString string){
  painter->_display_string = string;
}

QString SLIDERPAINTER_get_string(SliderPainter *painter){
  return painter->_display_string;
}
