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

#ifndef QT_MYQCHECKBOX_H
#define QT_MYQCHECKBOX_H

#include <QCheckBox>
#include <QMouseEvent>
#include <QString>
#include <QPainter>

#include "../Qt/EditorWidget.h"

#include "../common/instruments_proc.h"
#include "../common/vector_proc.h"
#include "../audio/SoundPlugin.h"
#include "../audio/SoundPlugin_proc.h"
#include "../audio/Pd_plugin_proc.h"
#include "../audio/Modulator_plugin_proc.h"

#include "Qt_instruments_proc.h"

#include "../audio/undo_audio_effect_proc.h"
#include "../common/undo_patchvoice_proc.h"
#include "Qt_mix_colors.h"

#ifdef COMPILING_RADIUM
extern struct Root *root;
#else
extern QColor *g_colors;
#endif


static int get_text_width(QString text){
  const QFontMetrics fn = QFontMetrics(QFont());
  return fn.width(text);
}

inline static void CHECKBOX_paint(QPainter *painter, bool is_checked, bool is_enabled, int width, int height, QString text, bool _is_implicitly_on){
#ifdef COMPILING_RADIUM
  //QColor *colors = static_cast<EditorWidget*>(root->song->tracker_windows->os_visual.widget)->colors;
#else
    QColor *colors = g_colors;
#endif

    QColor col; // on

    QColor checked_col;
    
    if(text!=""){
      col = get_qcolor(BUTTONS_COLOR_NUM);
      
      if (text=="Record")
        col = mix_colors(col, QColor(255,0,0), 0.9);
      else if (text=="Waiting for note...")
        col = mix_colors(col, QColor(255,0,0), 0.6);
      else if (text=="Recording")
        col = mix_colors(col, QColor(255,0,0), 0.1);

      if (text=="Mute")
        checked_col = mix_colors(col, Qt::green, 0.75);
      else if (text=="Solo")
        checked_col = mix_colors(col, Qt::yellow, 0.75);
      else if (text=="Bypass")
        checked_col = mix_colors(col, get_qcolor(ZOOMLINE_TEXT_COLOR_NUM1), 0.6);
      else
        checked_col = col;

      if (is_checked) {
        col = checked_col;
      }
      
#if 0
      col = mix_colors(col.light(70),QColor(98,59,33),0.55);//editor->colors[colnum].light(52);
      col.setAlpha(76);
#endif
    }else{
      //col = get_qcolor(HIGH_BACKGROUND_COLOR_NUM).light(52);
      col = get_qcolor(BUTTONS_ON_OFF_COLOR_NUM);
      //col = get_qcolor(PEAKS_COLOR_NUM);
   }


    if(is_enabled==false)
      col.setAlpha(col.alpha()/3);

    //col = QColor(106, 104, 100, 255);
    //col = QColor(0, 107, 156, 255);

    if(is_checked==true){
      painter->fillRect(1,1,width-2,height-1,col);
      //painter->setPen(editor->colors[1]);
      //p.drawRect(0,0,width()-1,height()-1);
      //p.drawRect(1,1,width()-3,height()-3);
    }else{
      if(_is_implicitly_on)
        painter->setPen(checked_col);
      else
        painter->setPen(col);

      painter->drawRect(1,1,width-3,height-2); // outer
      if(_is_implicitly_on)
        painter->setPen(col);
      painter->drawRect(2,2,width-5,height-4); // inner
    }


    if(text!=""){
      painter->setPen(get_qcolor(HIGH_EDITOR_BACKGROUND_COLOR_NUM));
      painter->drawRect(0,0,width,height);

      //QRect rect(5,3,width-5,height-3);
      QRect rect(1,1,width-2,height-1);//5,3,width-5,height-3);
      QColor button_text_color = get_qcolor(BUTTONS_TEXT_COLOR_NUM);
      if(is_checked==true)
        button_text_color.setAlpha(190);
      else
        button_text_color.setAlpha(120);
      painter->setPen(button_text_color);

      if(text.endsWith("Loop"))
        text = text + " " + QChar(8634);

      if(height>width && text.size()>2){
        painter->save();
        painter->translate(0,0);
        painter->rotate(90);
        int text_width = get_text_width(text);
        int pos = (height-text_width)/2;
        painter->drawText(pos,-5, text);
        painter->restore();
      }else{
        painter->drawText(rect, Qt::AlignCenter, text);
      }

    }
}


struct MyQCheckBox : public QCheckBox{
  bool _has_mouse;
  struct Patch *_patch;
  int _effect_num;
  bool _is_patchvoice_onoff_button;
  bool _add_undo_when_clicked = true;
  bool _is_a_pd_slider;
  bool _is_implicitly_on = false;
  QString vertical_text;

  void init(){
    _has_mouse=false;
    _patch = NULL;
    _effect_num = 0;
    _is_patchvoice_onoff_button = false;
    _is_a_pd_slider = false;
  }

  MyQCheckBox ( QWidget * parent = 0 ) : QCheckBox(parent) {init();}
  MyQCheckBox ( const QString & text, QWidget * parent = 0) : QCheckBox(text,parent) {init();}

  void mousePressEvent ( QMouseEvent * event ) override
  {
    if(_patch!=NULL && _patch->instrument==get_audio_instrument() && _patch->patchdata == NULL) // temp fix
      return;

    if (event->button() == Qt::LeftButton){      
      //setSliderDown(true);    
#ifdef COMPILING_RADIUM
      if (_add_undo_when_clicked){
        if(_is_patchvoice_onoff_button==true)
          ADD_UNDO(PatchVoice_CurrPos(_patch,_effect_num));
        else if(_patch!=NULL  && _patch->instrument==get_audio_instrument())
          ADD_UNDO(AudioEffect_CurrPos(_patch, _effect_num));
      }
#endif
      //handle_mouse_event(event);
      _has_mouse = true;
      printf("Got it %p %d. Checked: %d\n",_patch,_effect_num,!isChecked());
      setChecked(!isChecked());

    }else{
      
      if (_is_patchvoice_onoff_button==true)
        return;

      //printf("patch: %p, patchdata: %p\n",_patch,_patch==NULL?NULL:_patch->patchdata);
      if(_patch==NULL || _patch->instrument!=get_audio_instrument() || _patch->patchdata == NULL) {
        emit clicked();//rightClicked();
        return;
      }
      
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

#ifdef COMPILING_RADIUM
      vector_t options = {}; // c++ way of zero-initialization without getting missing-field-initializers warning.

      bool has_midi_learn = PLUGIN_has_midi_learn(plugin, _effect_num);
      bool doing_random_change = PLUGIN_get_random_behavior(plugin, _effect_num);
      int64_t modulator_id = MODULATOR_get_id(_patch, _effect_num);
      
      int delete_pd = -10;
      int reset = -10;

      int remove_midi_learn = -10;
      int midi_relearn = -10;
      int add_midi_learn = -10;

      int remove_modulator=-10;
      int replace_modulator=-10;
      int add_modulator=-10;

      int add_random = -10;
      int remove_random = -10;

      if(_is_a_pd_slider){
        /*
        VECTOR_push_back(&options, "Set Symbol Name");
        VECTOR_push_back(&options, "Set Type");
        VECTOR_push_back(&options, "Set Minimum Value");
        VECTOR_push_back(&options, "Set Maximum Value");
        */
        delete_pd = VECTOR_push_back(&options, "Delete");
      } else {
        reset = VECTOR_push_back(&options, "Reset");
        //VECTOR_push_back(&options, "Set Value");
      }

      VECTOR_push_back(&options, "--------------");

      if (has_midi_learn){
        remove_midi_learn = VECTOR_push_back(&options, "Remove MIDI Learn");
        midi_relearn = VECTOR_push_back(&options, "MIDI Relearn");
      }else{
        add_midi_learn = VECTOR_push_back(&options, "MIDI Learn");
      }      

      VECTOR_push_back(&options, "--------------");

      if(modulator_id >= 0){
        remove_modulator=VECTOR_push_back(&options, talloc_format("Remove Modulator (%s)", getModulatorDescription2(_patch->id, _effect_num)));
        replace_modulator=VECTOR_push_back(&options, talloc_format("Replace Modulator (%s)", getModulatorDescription2(_patch->id, _effect_num)));
      } else {
        add_modulator=VECTOR_push_back(&options, "Assign Modulator");
      }

      VECTOR_push_back(&options, "--------------");

      if (_effect_num < plugin->type->num_effects){
        if (doing_random_change)
          remove_random = VECTOR_push_back(&options, "Don't change value when pressing \"Random\"");
        else
          add_random = VECTOR_push_back(&options, "Change value when pressing \"Random\"");
      }
      
      int command = GFX_Menu(root->song->tracker_windows, NULL, "", options, true);

      //printf("command: %d, _patch: %p, is_audio: %d\n",command, _patch, _patch!=NULL && _patch->instrument==get_audio_instrument());

      if (command==delete_pd)
        PD_delete_controller(plugin, _effect_num);
      
      else if (command==reset)
        PLUGIN_reset_one_effect(plugin,_effect_num);

      else if (command==remove_random)
        PLUGIN_set_random_behavior(plugin, _effect_num, false);
      
      else if (command==add_random)
        PLUGIN_set_random_behavior(plugin, _effect_num, true);
      
      else if (command==add_midi_learn)
        PLUGIN_add_midi_learn(plugin, _effect_num);
          
      else if (command==remove_midi_learn)
        PLUGIN_remove_midi_learn(plugin, _effect_num, true);
      
      else if (command==midi_relearn) {
        PLUGIN_remove_midi_learn(plugin, _effect_num, true);
        PLUGIN_add_midi_learn(plugin, _effect_num);
      }
      else if (command==remove_modulator){
        MODULATOR_remove_target(modulator_id, _patch, _effect_num);
        
      }else if (command==replace_modulator){
        MODULATOR_maybe_create_and_add_target(_patch, _effect_num, true);
        
      }else if (command==add_modulator){
        MODULATOR_maybe_create_and_add_target(_patch, _effect_num, false);
      }

      GFX_update_instrument_widget(_patch);
      
#endif // COMPILING_RADIUM

      event->accept();
    }
  }


  void paintEvent ( QPaintEvent * ev ) override {
    TRACK_PAINT();
    
    QPainter p(this);

    if(text().startsWith("V ")){
      vertical_text = text().right(text().size()-2);
      setText("");
    }

    QString text2 = vertical_text!="" ? vertical_text : text();

    if(_patch!=NULL && _patch->instrument==get_audio_instrument() && _patch->patchdata != NULL){
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      QString b = PLUGIN_has_midi_learn(plugin, _effect_num) ? "*" : "";
      QString a = (_effect_num>=plugin->type->num_effects || PLUGIN_get_random_behavior(plugin, _effect_num)) ? "" : " [xR]";

      text2 = b + text2 + a;
    }
    
    CHECKBOX_paint(&p, isChecked(), isEnabled(), width(), height(), text2, _is_implicitly_on);
  }

#if 0  //unable to make this work. Using the "clicked" signal instead.
 signals:
  
  void rightClicked(){
    printf("Right clicked\n");
  }
#endif
  
};


#endif // QT_MYQCHECKBOX_H
