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


#ifndef QT_MYQSLIDER_H
#define QT_MYQSLIDER_H

#include <QSlider>
#include <QMouseEvent>
#include <QPainter>
#include <QTimer>
#include <QVector>
#include <QApplication>
#include <QFont>

#include "EditorWidget.h"

#include "../common/instruments_proc.h"
#include "../common/vector_proc.h"
#include "../common/settings_proc.h"

#include "../audio/undo_audio_effect_proc.h"
#include "../audio/SoundPlugin.h"
#include "../audio/SoundPlugin_proc.h"
#include "../audio/Pd_plugin_proc.h"

#include "Qt_instruments_proc.h"

#include "Qt_SliderPainter_proc.h"

static int scale_int(int x, int x1, int x2, int y1, int y2){
  return (int)scale((float)x,(float)x1,(float)x2,(float)y1,(float)y2);
}

struct MyQSlider;

#ifdef COMPILING_RADIUM
extern QVector<MyQSlider*> g_all_myqsliders;
extern struct Root *root;
#else
QVector<MyQSlider*> g_all_myqsliders;
#endif

static int g_minimum_height = 0;

struct MyQSlider : public QSlider{

 public:
  struct Patch *_patch;
  int _effect_num;
  
  SliderPainter *_painter;

  bool _minimum_size_set;

  bool _is_a_pd_slider;

  void init(){
    _has_mouse=false;

    _patch = NULL;
    _effect_num = 0;

    _is_a_pd_slider = false;

    if(g_minimum_height==0){
      QFontMetrics fm(QApplication::font());
      QRect r =fm.boundingRect("In Out 234234 dB");
      g_minimum_height = r.height()+4;
      printf("Minimum height: %d, family: %s, font pixelsize: %d, font pointsize: %d\n",g_minimum_height,QApplication::font().family().ascii(),QApplication::font().pixelSize(),QApplication::font().pointSize());
    }

    _minimum_size_set = false; // minimumSize must be set later. I think ui generated code overwrites it when set here.

    _painter = SLIDERPAINTER_create(this);

    g_all_myqsliders.push_back(this);
  }

  MyQSlider ( QWidget * parent = 0 ) : QSlider(parent) {init();}
  MyQSlider ( Qt::Orientation orientation, QWidget * parent = 0 ) : QSlider(orientation,parent) { init();}

  ~MyQSlider(){
    g_all_myqsliders.remove(g_all_myqsliders.indexOf(this));
    SLIDERPAINTER_delete(_painter);
  }

#if 0
  void handle_system_delay(bool down){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    const SoundPluginType *type = plugin->type;

    if(_effect_num==type->num_effects+EFFNUM_DELAY_TIME){
      if(down==true)
        plugin->delay.is_on = false;
      else
        plugin->delay.is_on = true;
    }
  }
#endif

  bool _has_mouse;

  void handle_mouse_event ( QMouseEvent * event ){
    //printf("Got mouse press event %d / %d\n",(int)event->x(),(int)event->y());
    if (orientation() == Qt::Vertical)
      setValue(scale_int(event->y(),height(),0,minimum(),maximum()));
    else
      setValue(scale_int(event->x(),0,width(),minimum(),maximum()));

    event->accept();
  }

  // mousePressEvent 
  void mousePressEvent ( QMouseEvent * event )
  {
    //printf("Got mouse pres event %d / %d\n",(int)event->x(),(int)event->y());
    if (event->button() == Qt::LeftButton){

#ifdef COMPILING_RADIUM
      if(_patch!=NULL && _patch->instrument==get_audio_instrument()){
        Undo_AudioEffect_CurrPos(_patch, _effect_num);
        //handle_system_delay(true);
      }
#endif

      handle_mouse_event(event);
      _has_mouse = true;

    }else{

#ifdef COMPILING_RADIUM
      vector_t options = {0};

      if(_is_a_pd_slider){
        /*
        VECTOR_push_back(&options, "Set Symbol Name");
        VECTOR_push_back(&options, "Set Type");
        VECTOR_push_back(&options, "Set Minimum Value");
        VECTOR_push_back(&options, "Set Maximum Value");
        */
        VECTOR_push_back(&options, "Delete");
      } else {
        VECTOR_push_back(&options, "Reset");
        //VECTOR_push_back(&options, "Set Value");
      }


      int command = GFX_Menu(root->song->tracker_windows, NULL, "", &options);

      //printf("command: %d, _patch: %p, is_audio: %d\n",command, _patch, _patch!=NULL && _patch->instrument==get_audio_instrument());

      if(command==0 && _patch!=NULL && _patch->instrument==get_audio_instrument()){
        SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
        if(_is_a_pd_slider) {
          //printf("Calling delete controller for %p / %d\n",plugin,_effect_num);
          PD_delete_controller(plugin, _effect_num);
        } else {
          PLUGIN_reset_one_effect(plugin,_effect_num);
          GFX_update_instrument_widget(_patch);
        }
      }

#if 0
      else if(command==1 && _patch!=NULL && _patch->instrument==get_audio_instrument()){
        SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
        char *s = GFX_GetString(root->song->tracker_windows,NULL, (char*)"new value");
        if(s!=NULL){
          float value = OS_get_double_from_string(s);
          Undo_AudioEffect_CurrPos(_patch, _effect_num);
          PLUGIN_set_effect_value(plugin,-1,_effect_num,value,PLUGIN_STORED_TYPE,PLUGIN_STORE_VALUE);
          GFX_update_instrument_widget(_patch);
        }
      }
#endif // 0
#endif // COMPILING_RADIUM

      event->accept();
    }
  }

  void mouseMoveEvent ( QMouseEvent * event )
  {
    if (_has_mouse){
      handle_mouse_event(event);
    }else
      QSlider::mouseMoveEvent(event);
  }

  void mouseReleaseEvent ( QMouseEvent * event )
  {
    //printf("Got mouse release event %d / %d\n",(int)event->x(),(int)event->y());
    if (_has_mouse){
#if 0
      if(_patch!=NULL && _patch->instrument==get_audio_instrument()){
        handle_system_delay(false);
      }
#endif
      handle_mouse_event(event);
      _has_mouse=false;
    }else
      QSlider::mouseReleaseEvent(event);
  }

  void paintEvent ( QPaintEvent * ev ){
    if(_minimum_size_set==false){
      _minimum_size_set=true;
      setMinimumHeight(g_minimum_height);
    }

#if 0
    {
      QFontMetrics fm(QApplication::font());
      //QRect r =fm.boundingRect(SLIDERPAINTER_get_string(_painter));
      int width = fm.width(SLIDERPAINTER_get_string(_painter)) + 20;
      if(minimumWidth() < width)
        setMinimumWidth(width);
    }

#endif

    QPainter p(this);
    SLIDERPAINTER_paint(_painter,&p);
  }

};

#endif // QT_MYQSLIDER_H
