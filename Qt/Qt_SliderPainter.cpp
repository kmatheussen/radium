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

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wnull-dereference"
#include <QWidget>
#pragma GCC diagnostic pop

#include <QGraphicsItem>
#include <QAbstractSlider>

#include "EditorWidget.h"
#include "../common/threading.h"

#include "../audio/SoundPlugin.h"
#include "../audio/AudioMeterPeaks_proc.h"

#include "Qt_SliderPainter_proc.h"

#include "Qt_MyWidget.h"
#include "MySliderPainterPainter.h"

/*
static inline int scale_int(int x, int x1, int x2, int y1, int y2){
  return (int)scale((float)x,(float)x1,(float)x2,(float)y1,(float)y2);
}
*/


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
private:
  float *value;

public:
  AutomationOrPeakData(float *value)
    : value(value)
    , last_value(-100000)
  {}

  float get_automation_or_peak_value(void){
    return *value;
  }
  
  float requested_pos;
  float last_drawn_pos;

  float last_value;
  
  enum ColorNums color;

  int ch;
  int num_ch;
  
  bool is_automation;
  bool single_line_style;
  
};
}

static float DATA_get_y1(AutomationOrPeakData *data, int height, int num_channels){
  R_ASSERT(data->num_ch>0);
  
  if (num_channels==-1 || data->is_automation)
    num_channels = data->num_ch;
  else
    num_channels = R_MIN(num_channels, data->num_ch);
  
  return (float)scale_double(data->ch,0,num_channels,1,height-1);
}

static float DATA_get_y2(AutomationOrPeakData *data, int height, int num_channels){
  if (num_channels==-1 || data->is_automation)
    num_channels = data->num_ch;
  else
    num_channels = R_MIN(num_channels, data->num_ch);

  return (float)scale_double(data->ch+1,0,num_channels,1,height-1);
}

struct SliderPainter{

  std::vector<AutomationOrPeakData*>_data;
  //QVector<AutomationOrPeakData*>_data;
  
  QAbstractSlider *_qslider;
  QGraphicsItem *_graphics_item;  // Either widget or _graphics_item must be set.

  int _x1;
  int _y1;
  int _x2;
  int _y2;

  float *_peak_values;
  bool _local_peak_values; // TODO: Remove this one. Seems like it is always false.
  float _automation_value;

  bool _alternative_color;
  bool _is_hovered = false;
  bool _recording_color;
  
  int _value;

  QString _display_string;

  int _num_channels;
  int _last_num_channels;
  
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

    //printf("Calling update() 5\n");
    
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
    _alternative_color = false;
    _recording_color = false;
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
      V_free(_peak_values);

    for(int i=0;i<(int)_data.size();i++) // Don't like iterators. Rather free memory manually than using them.
      delete _data.at(i);
  }

  void call_regularly(int num_channels){
    for(int i=0;i<(int)_data.size();i++){
      AutomationOrPeakData *data = _data.at(i);

      bool doit = data->is_automation
        || num_channels == -1
        || data->ch < num_channels;
      
      if (!doit)
        continue;

      float value = data->get_automation_or_peak_value();

      if (!equal_floats(value, data->last_value) || _last_num_channels!=num_channels) {

        float gain;

        if(data->is_automation) {
          gain = value;
        } else{
          float db = value; //gain2db(value);
          if(db>4.0f)
            data->color = PEAKS_4DB_COLOR_NUM;
          else if(db>0.0f)
            data->color = PEAKS_0DB_COLOR_NUM;
          else
            data->color = PEAKS_COLOR_NUM;
          
          gain = db2linear(db, 1, 0);
        }

        if (data->single_line_style)
          data->requested_pos = scale(gain,0.0f,1.0f,
                                      0.0f,(float)width()-2)
                                - 1;
        else
          data->requested_pos = scale(gain,0.0f,1.0f,
                                      0.0f,(float)width());
          
        
        if(!equal_floats(data->last_drawn_pos, data->requested_pos) || _last_num_channels!=num_channels){
          
          //printf("Painting. Last drawn: %d. requested: %d\n",data->last_drawn_pos,data->requested_pos);

          float y1 = DATA_get_y1(data,height(),num_channels);
          y1 = floor(y1);
          
          float y2 = DATA_get_y2(data,height(),num_channels);
          y2 = ceil(y2);
          
          int height = R_MAX(1, y2-y1);
          
          //printf("y1: %d, y2: %d, height: %d. req: %d, last: %d\n",y1,y2,height,data->requested_pos,data->last_drawn_pos);
          if (data->single_line_style) {
            
            update(data->requested_pos-1,
                   y1,
                   6,height+1);
            update(data->last_drawn_pos-1,
                   y1,
                   6,height+1);
            
          } else {
            
            float x1 = R_MIN(data->last_drawn_pos-1, data->requested_pos-1);
            float x2 = R_MAX(data->last_drawn_pos-1+6, data->requested_pos-1+6);
            update(x1,    y1,
                   x2-x1, height+1);
          }
          
          //printf("%d - %d\n",data->requested_pos,data->last_drawn_pos);
        }

      }

      data->last_value = value;

    }

    _last_num_channels = num_channels;      
  }

  AutomationOrPeakData *create_automation_data(float *value = NULL){
    R_ASSERT(THREADING_is_main_thread());

    if (value==NULL)
      value = &_automation_value;
    
    AutomationOrPeakData *data = new AutomationOrPeakData(value);
    
    data->requested_pos  = 0;
    data->last_drawn_pos = 0;

    data->ch     = 0;
    data->num_ch = 1;
    
    data->is_automation = true;
    data->single_line_style = true;
    
    _data.push_back(data);

    return data;
  }

  void set_automation_value_pointer(enum ColorNums color_num, float *automation_value){
    AutomationOrPeakData *data = create_automation_data(automation_value);
    data->color = color_num;
  }

  void set_peak_value_pointers(int num_channels, float *peak_values, bool single_line_style){
    R_ASSERT(THREADING_is_main_thread());
    R_ASSERT(peak_values != NULL);

    _peak_values = peak_values;
    
    _data.clear();

    for(int ch=0;ch<num_channels;ch++){
      AutomationOrPeakData *data = create_automation_data(&_peak_values[ch]);
      data->ch            = ch;
      data->num_ch        = num_channels;
      data->is_automation = false;
      data->color         = PEAKS_COLOR_NUM;
      data->single_line_style = single_line_style;
    }
  }

  void paint_peaks_non_single_line_style(QPainter *p, AutomationOrPeakData *data, float y1, float y2, float peak_height){
    float x1 = 0.0f;
    float x4 = width();
    
    float x2 = db2linear(0.0f, x4, x1);
    float x3 = db2linear(4.0f, x4, x1);

    float x = data->requested_pos;

    if (peak_height < 1) {
      y2 = y1 + 1;
      peak_height = 1;
    }

    if (y1 < 0)
      y1 = 0;
    
    if (y2 > height()-1)
      y2 = height() - 1;
    
      
    
        
    QRect f(x1, y1, R_MIN(x, x2) - x1, peak_height);
    p->fillRect(f, get_qcolor(PEAKS_COLOR_NUM));
    
    if (x > x2) {
      QRectF f(x2, y1, R_MIN(x, x3) - x2, peak_height);
      p->fillRect(f, get_qcolor(PEAKS_0DB_COLOR_NUM));

      if (x > x3) {
        QRectF f(x3, y1, x - x3, peak_height);
        p->fillRect(f, get_qcolor(PEAKS_4DB_COLOR_NUM));
      }
    }
    
  }
  
  void paint_automation_or_peaks(QPainter *p, bool single_line_style){
    
    for(int i=0;i<(int)_data.size();i++){
      AutomationOrPeakData *data = _data.at(i);

      bool doit = data->is_automation
        || _last_num_channels == -1
        || data->ch < _last_num_channels;
      
      if (doit && (data->single_line_style==single_line_style) && data->last_value >= -100.0f){
        
        //printf("%s: sp: %p, i: %d, size: %d (%d/%d)\n",_display_string.toUtf8().constData(),this,(int)i,(int)_data.size(),sizeof(float),sizeof(float*));
        
        float y1 = DATA_get_y1(data,height(), _last_num_channels);
        float y2 = DATA_get_y2(data,height(), _last_num_channels);
        float height = y2-y1;

        if (data->single_line_style) {
          QRectF f(data->requested_pos+1, y1+1,
                   2,                     height);

          p->fillRect(f, get_qcolor(data->color));
        
          p->setPen(QPen(get_qcolor(HIGH_BACKGROUND_COLOR_NUM).lighter(120),1));
          const QRectF &f2 = f; //f.adjusted(0, 0, 0, 0);        
          p->drawRect(f2);

        } else {
        
          paint_peaks_non_single_line_style(p, data, y1, y2, height);
        
        }
      
        data->last_drawn_pos = data->requested_pos;

      }
    }
  }
  
  void paint(QPainter *p){
    R_ASSERT(THREADING_is_main_thread());

#ifdef COMPILING_RADIUM
    //QColor *colors = static_cast<EditorWidget*>(root->song->tracker_windows->os_visual.widget)->colors;
#else
    QColor *colors = g_colors;
#endif

    if (_recording_color){
      QColor c = get_qcolor(SLIDER_RECORDING_COLOR_NUM);
      p->fillRect(0,0,width(),height(),c);
    } else if (_is_hovered && _qslider->isEnabled()){
      QColor c = _qslider->palette().color(_qslider->backgroundRole());
      myFillRect(*p, QRectF(0,0,width(),height()), c.lighter(110), false);
      //myFillRect(*p, QRectF(0,0,width(),height()), c);
    }

    paint_automation_or_peaks(p, false); // chip peaks

    cvs::MyPainter mp(p);

    SLIDERPAINTERPAINTER_paint(&mp,0,0,width(),height(),
                               isEnabled(),
                               scale_double(value(),minimum(),maximum(),0.0f,1.0f),
                               _display_string.toStdString(),
                               _alternative_color
                               );

    paint_automation_or_peaks(p, true); // everything else
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

void SLIDERPAINTER_prepare_for_deletion(SliderPainter *painter){
}

void SLIDERPAINTER_delete(SliderPainter *painter){
  R_ASSERT(THREADING_is_main_thread());
#if 1
  delete painter;
#else
  SLIDERPAINTER_became_invisible(painter);
#endif
}

void SLIDERPAINTER_became_visible(SliderPainter *painter){
  R_ASSERT(THREADING_is_main_thread());
}

void SLIDERPAINTER_became_invisible(SliderPainter *painter){
  R_ASSERT(THREADING_is_main_thread());
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

void SLIDERPAINTER_set_peak_value_pointers(SliderPainter *painter, int num_channels, float *pointers, bool single_line_style){
  painter->set_peak_value_pointers(num_channels, pointers, single_line_style);
  //painter->set_peak_value_pointers(num_channels, NULL);
}

void SLIDERPAINTER_set_automation_value_pointer(SliderPainter *painter, enum ColorNums color_num, float *pointer){
  painter->set_automation_value_pointer(color_num, pointer);
}

void SLIDERPAINTER_call_regularly(SliderPainter *painter, int num_channels){
  painter->call_regularly(num_channels);
}

// Used for chips where the slider controls input volume instead of output volume.
void SLIDERPAINTER_set_alternative_color(SliderPainter *painter, bool setit){
  painter->_alternative_color = setit;
}

void SLIDERPAINTER_set_hovered(SliderPainter *painter, bool is_hovered){
  painter->_is_hovered = is_hovered;
}

void SLIDERPAINTER_set_recording_color(SliderPainter *painter, bool setit){
  painter->_recording_color = setit;
}

void SLIDERPAINTER_set_string(SliderPainter *painter,QString string){  
  painter->_display_string = string;
}

QString SLIDERPAINTER_get_string(SliderPainter *painter){
  return painter->_display_string;
}

