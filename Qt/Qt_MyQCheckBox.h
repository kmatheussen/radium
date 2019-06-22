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

#include "helpers.h"


#include "../common/instruments_proc.h"
#include "../common/vector_proc.h"
#include "../audio/SoundPlugin.h"
#include "../audio/SoundPlugin_proc.h"
#include "../audio/Pd_plugin_proc.h"
#include "../audio/Modulator_plugin_proc.h"

#include "../embedded_scheme/s7extra_proc.h"

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

inline static void CHECKBOX_paint(QPainter *painter, bool is_checked, bool is_enabled, int width, int height, QString text, bool _is_implicitly_on, bool is_hovered){
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
      if(is_hovered && is_enabled)
        painter->fillRect(1,1,width-2,height-1,col.lighter(110));
      else
        painter->fillRect(1,1,width-2,height-1,col);
      //painter->setPen(editor->colors[1]);
      //p.drawRect(0,0,width()-1,height()-1);
      //p.drawRect(1,1,width()-3,height()-3);
    }else{
      if(is_hovered && is_enabled){
        QColor c = get_qcolor(HIGH_BACKGROUND_COLOR_NUM); //.light(52); //_qslider->palette().color(_qslider->backgroundRole());
        myFillRect(*painter, QRectF(0,0,width,height), c.lighter(110), false);
      }
      
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

struct MyQCheckBox_OnlyCustomPainting : public QCheckBox{
  radium::GcHolder<struct Patch> _patch;
  bool _is_patchvoice_onoff_button = false;
  int _patchvoicenum = 0;
  int _effect_num = 0;
  bool _is_implicitly_on = false;

  MyQCheckBox_OnlyCustomPainting ( QWidget * parent = 0 ) : QCheckBox(parent) {}
  MyQCheckBox_OnlyCustomPainting ( const QString & text, QWidget * parent = 0) : QCheckBox(text,parent) {}

  QString vertical_text;

  // Have to do this. Just overriding the paintEvent makes mouse partly stop working.
  void mousePressEvent ( QMouseEvent * event ) override {
    setChecked(!isChecked());    
  }

  bool _is_hovered = false;
      
  bool _popup_menu_is_visible = false;
  
  void enterEvent(QEvent *event) override {
    if (_patch.data() != NULL){
      _is_hovered = true;
      update();
      if(_patch->instrument==get_audio_instrument())
        GFX_SetStatusBar(talloc_format("\"%s\" (right-click for options)", getInstrumentEffectName(_effect_num, _patch->id)));
    }
  }

  void leaveEvent(QEvent *event) override {
    if (_patch.data() != NULL){
      if(_popup_menu_is_visible==false){
        _is_hovered = false;
        update();
      }
      GFX_SetStatusBar("");
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

    if(_patch.data()!=NULL && _patch->patchdata != NULL && _is_patchvoice_onoff_button==false)
      text2 = get_parameter_prepend_text(_patch.data(), _effect_num) + text2;
    
    CHECKBOX_paint(&p, isChecked(), isEnabled(), width(), height(), text2, _is_implicitly_on, _is_hovered);
  }

  
};

struct MyQCheckBox : public MyQCheckBox_OnlyCustomPainting {
  bool _has_mouse = false;
  bool _add_undo_when_clicked = true;
  bool _is_a_pd_slider = false;

  MyQCheckBox ( QWidget * parent = 0 ) : MyQCheckBox_OnlyCustomPainting(parent) {}
  MyQCheckBox ( const QString & text, QWidget * parent = 0) : MyQCheckBox_OnlyCustomPainting(text,parent) {}

  Qt::MouseButton _last_pressed_button = Qt::NoButton;

  std::function<void(void)> _show_popup_menu;
  
  void contextMenuEvent(QContextMenuEvent *event) override {
    if (_show_popup_menu) {
      _show_popup_menu();
    } else
      MyQCheckBox_OnlyCustomPainting::contextMenuEvent(event);
  }

  void set_unhovered_when_popupmenu_is_closed(int64_t guinum){
    if(_popup_menu_is_visible){
      if(false==gui_isOpen(guinum)){
        _popup_menu_is_visible=false;
        _is_hovered = false;
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
  
  void mousePressEvent ( QMouseEvent * event ) override
  {

    _last_pressed_button = event->button();

    if(_patch.data()!=NULL && _patch->instrument==get_audio_instrument() && _patch->patchdata == NULL) // temp fix
      return;

    if (event->button() == Qt::LeftButton){

      //setSliderDown(true);    
#ifdef COMPILING_RADIUM
      if (_add_undo_when_clicked){
        if(_is_patchvoice_onoff_button==true)
          ADD_UNDO(PatchVoice_CurrPos(_patch.data(), _patchvoicenum));
        else if(_patch.data()!=NULL  && _patch->instrument==get_audio_instrument())
          ADD_UNDO(AudioEffect_CurrPos(_patch.data(), _effect_num));
      }
#endif
      //handle_mouse_event(event);
      _has_mouse = true;
      printf("Got it %p %d. Checked: %d\n",_patch.data(),_effect_num,!isChecked());
      setChecked(!isChecked());

    }else{
      
      if (_is_patchvoice_onoff_button==true)
        return;

      //printf("patch: %p, patchdata: %p\n",_patch,_patch==NULL?NULL:_patch->patchdata);
      if(_patch.data()==NULL || _patch->instrument!=get_audio_instrument() || _patch->patchdata == NULL) {
        emit clicked();//rightClicked();
        return;
      }
      
      event->accept();

      if (shiftPressed()){
        
        SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
        
        PLUGIN_reset_one_effect(plugin,_effect_num);
        GFX_update_instrument_widget(_patch.data());
        
      } else {
      
#ifdef COMPILING_RADIUM

        if (_patch->instrument==get_audio_instrument()) {
          
          SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

          _popup_menu_is_visible = true;

          dyn_t ret = S7CALL2(dyn_int_charpointer,"FROM_C-show-effect-popup-menu", _patch->id, PLUGIN_get_effect_name(plugin, _effect_num));
          if (ret.type==INT_TYPE){
            set_unhovered_when_popupmenu_is_closed(ret.int_number);
          }

        }

#endif // COMPILING_RADIUM

      }

    }
  }


#if 0  //unable to make this work. Using the "clicked" signal instead.
 signals:
  
  void rightClicked(){
    printf("Right clicked\n");
  }
#endif
  
};


#endif // QT_MYQCHECKBOX_H
