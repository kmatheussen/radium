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
#include <QScrollBar>
#include <QMouseEvent>
#include <QPainter>
#include <QVector>
#include <QApplication>
#include <QFont>

#include "EditorWidget.h"

#include "../common/instruments_proc.h"
#include "../common/vector_proc.h"
#include "../common/settings_proc.h"
#include "../common/placement_proc.h"
#include "../common/sequencer_proc.h"
#include "../common/seqtrack_automation_proc.h"

#include "../audio/undo_audio_effect_proc.h"
#include "../audio/SoundPlugin.h"
#include "../audio/SoundPlugin_proc.h"
#include "../audio/Pd_plugin_proc.h"
#include "../audio/Modulator_plugin_proc.h"

#include "../embedded_scheme/s7extra_proc.h"

#include "../api/api_proc.h"

#include "Qt_instruments_proc.h"

#include "Qt_SliderPainter_proc.h"

struct MyQSlider;

static inline int scale_int(int x, int x1, int x2, int y1, int y2){
  return (int)scale((float)x,(float)x1,(float)x2,(float)y1,(float)y2);
}


#ifdef COMPILING_RADIUM
//extern QVector<MyQSlider*> g_all_myqsliders;
extern struct Root *root;
#else
//QVector<MyQSlider*> g_all_myqsliders;
#endif

extern struct TEvent tevent;

static int g_minimum_height = 0;

struct MyQSlider : public QSlider, public radium::MouseCycleFix {

 public:
  radium::GcHolder<struct Patch> _patch;
  int _effect_num;
  bool _is_recording;
  
  SliderPainter *_painter;

  bool _minimum_size_set;

  bool _is_a_pd_slider;

  void init(){
    setMouseTracking(true);
    
    _has_mouse=false;

    _patch.set(NULL);
    _effect_num = 0;
    _is_recording = false;
    
    _is_a_pd_slider = false;

    if(g_minimum_height==0){
      QFontMetrics fm(QApplication::font());
      QRect r =fm.boundingRect("In Out 234234 dB");
      g_minimum_height = r.height()+4;
      printf("Minimum height: %d, family: %s, font pixelsize: %d, font pointsize: %d\n",g_minimum_height,QApplication::font().family().toUtf8().constData(),QApplication::font().pixelSize(),QApplication::font().pointSize());
    }

    _minimum_size_set = false; // minimumSize must be set later. I think ui generated code overwrites it when set here.

    _painter = SLIDERPAINTER_create(this);

    //g_all_myqsliders.push_back(this);
  }

  MyQSlider ( QWidget * parent_ = 0 ) : QSlider(parent_) {init();}
  MyQSlider ( Qt::Orientation orientation_, QWidget * parent_ = 0 ) : QSlider(orientation_,parent_) { init();}

  void prepare_for_deletion(void){
    SLIDERPAINTER_prepare_for_deletion(_painter);
  }
  
  ~MyQSlider(){
    prepare_for_deletion();
    
    //R_ASSERT(false);
    //g_all_myqsliders.remove(g_all_myqsliders.indexOf(this));
    SLIDERPAINTER_delete(_painter);
    
    _painter = NULL; // quicker to discover memory corruption
    _patch.set(NULL);
  }

  void calledRegularlyByParent(void){
    R_ASSERT_RETURN_IF_FALSE(_patch.data() != NULL);
      
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

    if (plugin != NULL) {
        bool is_recording_now = PLUGIN_is_recording_automation(plugin, _effect_num);
        if (is_recording_now != _is_recording){
          SLIDERPAINTER_set_recording_color(_painter, is_recording_now);
          update();
          _is_recording = is_recording_now;
        }
    }

    SLIDERPAINTER_call_regularly(_painter, -1);
  }

  bool _popup_menu_is_visible = false;

  bool _is_hovered = false;
  
  void enterEvent(QEnterEvent *event) override {
    if (_patch.data() != NULL && isEnabled()){
      SLIDERPAINTER_set_hovered(_painter, true);
      _is_hovered = true;
      update();
      if(_patch->instrument==get_audio_instrument())
        GFX_SetStatusBar(talloc_format("\"%s\" (right-click for options)", getInstrumentEffectName(_effect_num, _patch->id)));
      else
        GFX_SetStatusBar(talloc_format("MIDI CC %d. (right-click for options)", _effect_num));
    }
  }

  void leaveEvent(QEvent *event) override {
    if (_patch.data() != NULL && isEnabled()){
      if(_popup_menu_is_visible==false){
        SLIDERPAINTER_set_hovered(_painter, false);
        _is_hovered = false;
        update();
      }
      GFX_SetStatusBar("");
    }
  }

  void hideEvent ( QHideEvent * event_ ) override {
    SLIDERPAINTER_became_invisible(_painter);
  }

  void showEvent ( QShowEvent * event_ ) override {
    SLIDERPAINTER_became_visible(_painter);
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

  float last_value;
  float last_pos;
  bool _has_mouse;

  void handle_mouse_event (radium::MouseCycleEvent &event_){
    //printf("Got mouse press event_ %d / %d\n",(int)event_.x(),(int)event_.y());

    float slider_length;
    float new_pos;

    if (orientation() == Qt::Vertical) {
      new_pos = event_.y();
      slider_length = height();
    } else {
      new_pos = event_.x();
      slider_length = width();
    }

    float dx = new_pos - last_pos;
    float per_pixel = (float)(maximum() - minimum()) / (float)slider_length;

    //printf("***** last_pos: %d, new_pos: %d, dx: %d\n",(int)last_pos,(int)new_pos,(int)dx);
    
    if ((event_.modifiers() & Qt::ControlModifier))
      per_pixel /= 10.0f;

    last_pos = new_pos;
    last_value += dx*per_pixel;

    if (last_value < minimum())
      last_value = minimum();
    if (last_value > maximum())
      last_value = maximum();

    setValue(last_value);
    //SLIDERPAINTER_setValue(_painter, last_value);

    //printf("dx: %f, per_pixel: %f, min/max: %f / %f, value: %f\n",dx,per_pixel,(float)minimum(),(float)maximum(),(float)value());

    event_.accept();
  }
  
  /*
  void handle_mouse_event ( QMouseEvent * event_ ){
    if (event_==NULL)
      return;

    radium::MouseCycleEvent event2(event_);
    handle_mouse_event(event2);
  }
  */
  
  void show_midi_slider_popup_menu(void){
    int remove_modulator=-10;
    int replace_modulator=-10;
    int add_modulator=-10;

    int64_t modulator_id = MODULATOR_get_id(_patch.data(), _effect_num);

    vector_t options = {};
    
    if(modulator_id >= 0){
      remove_modulator=VECTOR_push_back(&options, talloc_format("Remove Modulator (%s)", getModulatorDescription2(_patch->id, _effect_num)));
      replace_modulator=VECTOR_push_back(&options, talloc_format("Replace Modulator (%s)", getModulatorDescription2(_patch->id, _effect_num)));
    } else {
      add_modulator=VECTOR_push_back(&options, "Assign Modulator");
    }

    _popup_menu_is_visible = true;
    
    IsAlive is_alive(this);
    
    int64_t guinum =
      GFX_Menu3(options,[is_alive, this, modulator_id, remove_modulator, replace_modulator, add_modulator](int command, bool onoff){

        if (!is_alive || _patch->patchdata==NULL)
          return;

        _popup_menu_is_visible=false;
        SLIDERPAINTER_set_hovered(_painter, false);
        update();
        
        if (command==remove_modulator){
          MODULATOR_remove_target(modulator_id, _patch.data(), _effect_num);
          
        }else if (command==replace_modulator){
          MODULATOR_maybe_create_and_add_target(_patch.data(), _effect_num, true);
          
        }else if (command==add_modulator){
          MODULATOR_maybe_create_and_add_target(_patch.data(), _effect_num, false);

        }

        GFX_update_instrument_widget(_patch.data());
        });
          
    set_unhovered_when_popupmenu_is_closed(guinum);
  }
  
  void set_unhovered_when_popupmenu_is_closed(int64_t guinum){
    if(_popup_menu_is_visible){
      if(false==gui_isOpen(guinum)){
        _popup_menu_is_visible=false;
        SLIDERPAINTER_set_hovered(_painter, false);
        update();
      }else{
        IsAlive is_alive(this);
        QTimer::singleShot(50, [is_alive, this, guinum](){
            if (is_alive)
              set_unhovered_when_popupmenu_is_closed(guinum);
          });
      }
    }
  }

  
  // mousePressEvent 
  bool fix_mousePressEvent (radium::MouseCycleEvent &event_ ) override
  {
    if(_patch.data()!=NULL && _patch->instrument==get_audio_instrument() && _patch->patchdata == NULL) // temp fix
      return true;
    
    //printf("Got mouse pres event_ %d / %d\n",(int)event_->x(),(int)event_->y());
    if (event_.button() == Qt::LeftButton){

#ifdef COMPILING_RADIUM
      if(_patch.data()!=NULL && _patch->instrument==get_audio_instrument()){
        ADD_UNDO(AudioEffect_CurrPos(_patch.data(), _effect_num, AE_NO_FLAGS));
        //handle_system_delay(true);
      }
#endif

      last_value = value();
      if (orientation() == Qt::Vertical)
        last_pos = event_.y();
      else
        last_pos = event_.x();
      //handle_mouse_event_(event_);
      _has_mouse = true;

    } else {

      if(_patch.data()==NULL || _patch->patchdata == NULL) //  _patch->instrument!=get_audio_instrument() 
        return true;

      if (event_.button() == Qt::BackButton || shiftPressed()){

        SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
        
        PLUGIN_reset_one_effect(plugin,_effect_num);
        GFX_update_instrument_widget(_patch.data());
        
      } else {
      
#ifdef COMPILING_RADIUM

        if (_patch->instrument==get_MIDI_instrument()) {
          
          show_midi_slider_popup_menu();

        } else {
          
          SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

          _popup_menu_is_visible = true;

          dyn_t ret = S7CALL2(dyn_dyn_charpointer,"FROM_C-show-effect-popup-menu", DYN_create_instrument(_patch->id), PLUGIN_get_effect_name(plugin, _effect_num));
          if (ret.type==INT_TYPE){
            set_unhovered_when_popupmenu_is_closed(ret.int_number);
          }

        }
        
#endif // COMPILING_RADIUM
      }
      
      event_.accept();
    }

    return true;
  }

  void fix_mouseMoveEvent (radium::MouseCycleEvent &event_ ) override
  {
    if (_has_mouse){
      handle_mouse_event(event_);
    }else {
      auto *event = event_.get_qtevent();
      if (event)
        QSlider::mouseMoveEvent(event);
    }
  }

  bool fix_mouseReleaseEvent (radium::MouseCycleEvent &event_ ) override
  {
    //printf("Got mouse release event %d / %d\n",(int)event_->x(),(int)event_->y());
    if (_has_mouse){
#if 0
      if(_patch.data()!=NULL && _patch->instrument==get_audio_instrument()){
        handle_system_delay(false);
      }
#endif
      handle_mouse_event(event_);
      _has_mouse=false;
    }else {

      auto *qevent = event_.get_qtevent();
      
      if (qevent != NULL){        
        QSlider::mouseReleaseEvent(qevent);
      } else {
        //R_ASSERT_NON_RELEASE(false);
      }
    }

    return true;
  }

  MOUSE_CYCLE_CALLBACKS_FOR_QT;
    
  void paintEvent ( QPaintEvent * ev ) override {
    TRACK_PAINT();
    
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

    QColor bc = get_qcolor(SLIDER_BACKGROUND_COLOR_NUM);
    if (_is_hovered && isEnabled())
      bc = bc.lighter(110);
    
    myFillRect(p, QRectF(1,1,width()-2,height()-2), bc, false);
     
    SLIDERPAINTER_paint(_painter,&p);
  }

};



#endif // QT_MYQSLIDER_H
