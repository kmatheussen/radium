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
#include "../common/seqtrack_proc.h"
#include "../common/seqtrack_automation_proc.h"

#include "../audio/undo_audio_effect_proc.h"
#include "../audio/SoundPlugin.h"
#include "../audio/SoundPlugin_proc.h"
#include "../audio/Pd_plugin_proc.h"

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

struct MyQSlider : public QSlider {

 public:
  struct Patch *_patch;
  int _effect_num;
  bool _is_recording;
  
  SliderPainter *_painter;

  bool _minimum_size_set;

  bool _is_a_pd_slider;

  void init(){
    _has_mouse=false;

    _patch = NULL;
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
    _patch = NULL;
  }

  void calledRegularlyByParent(void){
    R_ASSERT_RETURN_IF_FALSE(_patch != NULL);
      
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

    if (plugin != NULL) {
        bool is_recording_now = PLUGIN_is_recording_automation(plugin, _effect_num);
        if (is_recording_now != _is_recording){
          SLIDERPAINTER_set_recording_color(_painter, is_recording_now);
          update();
          _is_recording = is_recording_now;
        }
    }

    SLIDERPAINTER_call_regularly(_painter);
  }
  
  void hideEvent ( QHideEvent * event_ ) {
    SLIDERPAINTER_became_invisible(_painter);
  }

  void showEvent ( QShowEvent * event_ ) {
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

  void handle_mouse_event ( QMouseEvent * event_ ){
    //printf("Got mouse press event_ %d / %d\n",(int)event_->x(),(int)event_->y());

    float slider_length;
    float new_pos;

    if (orientation() == Qt::Vertical) {
      new_pos = event_->y();
      slider_length = height();
    } else {
      new_pos = event_->x();
      slider_length = width();
    }

    float dx = new_pos - last_pos;
    float per_pixel = (float)(maximum() - minimum()) / (float)slider_length;

    //printf("***** last_pos: %d, new_pos: %d, dx: %d\n",(int)last_pos,(int)new_pos,(int)dx);
    
    if ((event_->modifiers() & Qt::ControlModifier))
      per_pixel /= 10.0f;

    last_pos = new_pos;
    last_value += dx*per_pixel;

    if (last_value < minimum())
      last_value = minimum();
    if (last_value > maximum())
      last_value = maximum();

    setValue(last_value);

    //printf("dx: %f, per_pixel: %f, min/max: %f / %f, value: %f\n",dx,per_pixel,(float)minimum(),(float)maximum(),(float)value());

    event_->accept();
  }

  // mousePressEvent 
  void mousePressEvent ( QMouseEvent * event_ ) override
  {
    if(_patch!=NULL && _patch->instrument==get_audio_instrument() && _patch->patchdata == NULL) // temp fix
      return;
    
    //printf("Got mouse pres event_ %d / %d\n",(int)event_->x(),(int)event_->y());
    if (event_->button() == Qt::LeftButton){

#ifdef COMPILING_RADIUM
      if(_patch!=NULL && _patch->instrument==get_audio_instrument()){
        ADD_UNDO(AudioEffect_CurrPos(_patch, _effect_num));
        //handle_system_delay(true);
      }
#endif

      last_value = value();
      if (orientation() == Qt::Vertical)
        last_pos = event_->y();
      else
        last_pos = event_->x();
      //handle_mouse_event_(event_);
      _has_mouse = true;

    }else{

      if(_patch==NULL || _patch->instrument!=get_audio_instrument() || _patch->patchdata == NULL)
        return;
      
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;


#ifdef COMPILING_RADIUM
      vector_t options = {}; // c++ way of zero-initialization without getting missing-field-initializers warning.

      bool has_midi_learn = PLUGIN_has_midi_learn(plugin, _effect_num);
      bool is_recording_automation = PLUGIN_is_recording_automation(plugin, _effect_num);
      bool doing_random_change = PLUGIN_get_random_behavior(plugin, _effect_num);
      
      int pd_delete=-10;
      int reset=-10;
      int remove_midi_learn=-10;
      int midi_relearn=-10;
      int midi_learn=-10;
      int record=-10;
      int add_automation_to_current_editor_track=-10;
      int add_automation_to_current_sequencer_track=-10;
      int add_random = -10;
      int remove_random = -10;
      
      if(_is_a_pd_slider){
        /*
        VECTOR_push_back(&options, "Set Symbol Name");
        VECTOR_push_back(&options, "Set Type");
        VECTOR_push_back(&options, "Set Minimum Value");
        VECTOR_push_back(&options, "Set Maximum Value");
        */
        pd_delete = VECTOR_push_back(&options, "Delete");
      } else {

        reset=VECTOR_push_back(&options, "Reset");

        //VECTOR_push_back(&options, "Set Value");
      }

      if (has_midi_learn){
        remove_midi_learn = VECTOR_push_back(&options, "Remove MIDI Learn");
        midi_relearn = VECTOR_push_back(&options, "MIDI Relearn");
      }else{
        midi_learn = VECTOR_push_back(&options, "MIDI Learn");
      }

      if (!is_recording_automation)
        record = VECTOR_push_back(&options, "Record");

      add_automation_to_current_editor_track = VECTOR_push_back(&options, "Add automation to current editor track");
      add_automation_to_current_sequencer_track = VECTOR_push_back(&options, "Add automation to current sequencer track");

      if (_effect_num < plugin->type->num_effects){
        if (doing_random_change)
          remove_random = VECTOR_push_back(&options, "Don't change value when pressing \"Random\"");
        else
          add_random = VECTOR_push_back(&options, "Change value when pressing \"Random\"");
      }
      
      //VECTOR_push_back(&options, "");
      
      //VECTOR_push_back(&options, "Set Value");

      int command = GFX_Menu(root->song->tracker_windows, NULL, "", options,true);

      //printf("command: %d, _patch: %p, is_audio: %d\n",command, _patch, _patch!=NULL && _patch->instrument==get_audio_instrument());

      if (command==pd_delete)
        PD_delete_controller(plugin, _effect_num);

      else if (command==reset)
        PLUGIN_reset_one_effect(plugin,_effect_num);

      else if (command==remove_midi_learn)
        PLUGIN_remove_midi_learn(plugin, _effect_num, true);

      else if (command==midi_relearn)
        {
          PLUGIN_remove_midi_learn(plugin, _effect_num, true);
          PLUGIN_add_midi_learn(plugin, _effect_num);
        }

      else if (command==midi_learn)
        PLUGIN_add_midi_learn(plugin, _effect_num);

      else if (command==record)
        PLUGIN_set_recording_automation(plugin, _effect_num, true);

      else if (command==remove_random)
        PLUGIN_set_random_behavior(plugin, _effect_num, false);

      else if (command==add_random)
        PLUGIN_set_random_behavior(plugin, _effect_num, true);
      
      else if (command==add_automation_to_current_editor_track) {

        int blocknum = currentBlock(-1);
        int tracknum = R_MAX(0, currentTrack(blocknum, -1));
        int64_t current_instrument_id = getInstrumentForTrack(tracknum, blocknum, -1);
        if (current_instrument_id < 0) {
          current_instrument_id = _patch->id;
          setInstrumentForTrack(_patch->id, tracknum, blocknum, -1);
        }
        
        if (!instrumentIsAudio(current_instrument_id)){
          
          GFX_addMessage("The instrument for the current track is not an audio instrument");
          
        } else {

          const char *fxname = PLUGIN_get_effect_name(plugin, _effect_num);
          float value_ = PLUGIN_get_effect_value(plugin, _effect_num, VALUE_FROM_STORAGE);

          int fxnum = getFx(fxname, tracknum, _patch->id, blocknum, -1);

          if (fxnum >= 0){

            addFxnode(value_,
                      p_Create(currentLine(blocknum, -1), 0, 1),
                      fxnum,
                      tracknum,
                      blocknum,
                      -1);
            
            
          } else {
                            
            addFx(value_,
                  p_Create(currentLine(blocknum, -1), 0, 1),
                  fxname,
                  tracknum,
                  _patch->id,
                  blocknum,
                  -1);
          }
        }

      } else if (command==add_automation_to_current_sequencer_track) {

        struct SeqTrack *seqtrack = SEQUENCER_get_curr_seqtrack();

        if (seqtrack != NULL) {

          undoSequencerAutomation();

          float value_ = PLUGIN_get_effect_value(plugin, _effect_num, VALUE_FROM_STORAGE);

          int64_t pos1 = ATOMIC_DOUBLE_GET(pc->song_abstime); //is_playing() && pc->playtype==PLAYSONG ? ATOMIC_DOUBLE_GET(pc->song_abstime) : 0;

          int64_t visible_duration = R_MAX(100, SEQUENCER_get_visible_end_time() - SEQUENCER_get_visible_start_time());
          
          int64_t pos2 = pos1 + visible_duration / 10;

          SEQTRACK_AUTOMATION_add_automation(seqtrack->seqtrackautomation, _patch, _effect_num, pos1, value_, LOGTYPE_LINEAR, pos2, value_);
        }

      }
        
        
      GFX_update_instrument_widget(_patch);
        
#if 0
      else if(command==1){
        char *s = GFX_GetString(root->song->tracker_windows,NULL, (char*)"new value");
        if(s!=NULL){
          float value = OS_get_double_from_string(s);
          printf("value: %f\n",value);
          setValue(value);
        }
      }

#else
#if 0
      else if(command==1 && _patch!=NULL && _patch->instrument==get_audio_instrument()){
        SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
        char *s = GFX_GetString(root->song->tracker_windows,NULL, (char*)"new value");
        if(s!=NULL){
          float value = OS_get_double_from_string(s);
          ADD_UNDO(AudioEffect_CurrPos(_patch, _effect_num));
          PLUGIN_set_effect_value(plugin, -1, _effect_num, value, PLUGIN_STORED_TYPE, PLUGIN_STORE_VALUE, FX_single);
          GFX_update_instrument_widget(_patch);
        }
      }
#endif
#endif
#endif // COMPILING_RADIUM

      event_->accept();
    }
  }

  void mouseMoveEvent ( QMouseEvent * event_ ) override
  {
    if (_has_mouse){
      handle_mouse_event(event_);
    }else
      QSlider::mouseMoveEvent(event_);
  }

  void mouseReleaseEvent ( QMouseEvent * event_ ) override
  {
    //printf("Got mouse release event %d / %d\n",(int)event_->x(),(int)event_->y());
    if (_has_mouse){
#if 0
      if(_patch!=NULL && _patch->instrument==get_audio_instrument()){
        handle_system_delay(false);
      }
#endif
      handle_mouse_event(event_);
      _has_mouse=false;
    }else
      QSlider::mouseReleaseEvent(event_);
  }

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
    SLIDERPAINTER_paint(_painter,&p);
  }

};



#endif // QT_MYQSLIDER_H
