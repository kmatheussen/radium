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

#include <functional>

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
#include "../api/api_gui_proc.h"
#include "../api/api_proc.h"

#include "Qt_mix_colors.h"

#ifdef COMPILING_RADIUM
extern struct Root *root;
#else
extern QColor *g_colors;
#endif


struct MyQCheckBox_OnlyCustomPainting : public QCheckBox{
  radium::GcHolder<struct Patch> _patch;
  bool _is_patchvoice_onoff_button = false;
  int _patchvoicenum = 0;
  int _effect_num = 0;
  bool _is_implicitly_on = false;

  MyQCheckBox_OnlyCustomPainting ( QWidget * parent = 0 )
    : QCheckBox(parent)
  {
    init();
  }
  MyQCheckBox_OnlyCustomPainting ( const QString & text, QWidget * parent = 0)
    : QCheckBox(text,parent)
  {
    init();
  }

  void init(void){
    setMouseTracking(true);
  }
  
  QString vertical_text;

  // Have to do this. Just overriding the paintEvent makes mouse partly stop working.
  void mousePressEvent ( QMouseEvent * event ) override {
    setChecked(!isChecked());    
  }

  //QColor _background_color = get_qcolor(BUTTONS_COLOR_NUM);

  std::function<void(bool)> _hovered_callback;

  int x1_border = 1;
  int x2_border = 1;
  int y1_border = 1;
  int y2_border = 1;

  bool _always_gradient_when_checked = false;
  
  bool _is_hovered = false;
  bool _is_popup_hovered = false;
      
  bool _popup_menu_is_visible = false;

  bool _show_enabled_marker = true;
  
  void enterEvent(QEvent *event) override {
    if (_hovered_callback) {
      _hovered_callback(true);
    } else if (_patch.data() != NULL){
      _is_popup_hovered = true;
      if(_patch->instrument==get_audio_instrument())
        GFX_SetStatusBar(talloc_format("\"%s\" (right-click for options)", getInstrumentEffectName(_effect_num, _patch->id)));
    }
    _is_hovered = true;
    update();
  }

  void leaveEvent(QEvent *event) override {
    if (_hovered_callback) {
      _hovered_callback(false);
    } else if (_patch.data() != NULL){
      if(_popup_menu_is_visible==false){
        _is_popup_hovered = false;
        update();
      }
      GFX_SetStatusBar("");
    }
    _is_hovered = false;
    update();
  }


  radium::GcHolder<wchar_t> _text_to_draw;
  
  void paintEvent ( QPaintEvent * ev ) override {
    TRACK_PAINT();
    
    QPainter p(this);

    p.setRenderHints(QPainter::Antialiasing,true);
    
    if(text().startsWith("V ")){
      vertical_text = text().right(text().size()-2);
      setText("");
    }

    QString text2 = vertical_text!="" ? vertical_text : text();
      
    if(_patch.data()!=NULL && _patch->patchdata != NULL && _is_patchvoice_onoff_button==false)
      text2 = get_parameter_prepend_text(_patch.data(), _effect_num) + text2;

    API_run_custom_gui_paint_function(this,
                                      &p, &ev->region(),
                                      [this,text2](){

                                        if (_text_to_draw.data()==NULL || QString::fromWCharArray(_text_to_draw.data())!=text2)
                                          _text_to_draw.set(STRING_create(text2));

                                        S7EXTRA_GET_FUNC(draw_checkbox_func, "draw-button");

                                        bool do_gradient = false;

                                        if(_is_hovered) {
                                          
                                          do_gradient = true;

                                        } else if (_always_gradient_when_checked && isChecked()) {
                                          
                                          do_gradient = true;
                                          
                                        } else if (text2.isEmpty()){
                                          
                                          do_gradient = false;
                                          
                                        } else if (isChecked()){
                                          
                                          if (text2.isEmpty())
                                            do_gradient = false;
                                          
                                          else if (_show_enabled_marker)
                                            do_gradient = true;
                                          
                                        }
                                        
                                        s7extra_applyFunc_void_varargs(draw_checkbox_func,
                                                                       DYN_create_int(API_get_gui_from_widget(this)),
                                                                       DYN_create_string_dont_copy(_text_to_draw.data()),
                                                                       DYN_create_bool(isChecked()),
                                                                       DYN_create_int(x1_border),
                                                                       DYN_create_int(y1_border),
                                                                       DYN_create_int(width()-x2_border),
                                                                       DYN_create_int(height()-y2_border),
                                                                       
                                                                       DYN_create_symbol_dont_copy(":background-color"),
                                                                       DYN_create_string_dont_copy(L"check_box_unselected_v2"),
                                                                           
                                                                       DYN_create_symbol_dont_copy(":is-hovering"),
                                                                       DYN_create_bool(_is_hovered && isEnabled()),
                                                                       
                                                                       DYN_create_symbol_dont_copy(":is-enabled"),
                                                                       DYN_create_bool(isEnabled()),
                                                                       
                                                                       DYN_create_symbol_dont_copy(":prepend-checked-marker"),
                                                                       DYN_create_bool(_show_enabled_marker),
                                                                       
                                                                       DYN_create_symbol_dont_copy(":vertical-text"),
                                                                       DYN_create_bool(!vertical_text.isEmpty()),
                                                                       
                                                                       DYN_create_symbol_dont_copy(":gradient-background"),
                                                                       DYN_create_bool(do_gradient),
                                                                       //DYN_create_bool(!_is_hovered && !isChecked() && _show_enabled_marker && !text2.isEmpty()),
                                                                       
                                                                       g_uninitialized_dyn);
                                      });
  
  }

  
};

struct MyQCheckBox : public MyQCheckBox_OnlyCustomPainting, public radium::MouseCycleFix {
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
        _is_popup_hovered = false;
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

  void fix_mousePressEvent( QMouseEvent *event) override {

    _is_hovered = true;
    
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
          ADD_UNDO(AudioEffect_CurrPos(_patch.data(), _effect_num, AE_NO_FLAGS));
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

  void fix_mouseMoveEvent( QMouseEvent *qmouseevent) override {
    MyQCheckBox_OnlyCustomPainting::mouseMoveEvent(qmouseevent);
  }

  void fix_mouseReleaseEvent(radium::MouseCycleEvent &event) override{
    _is_hovered = false;
    
    auto *qevent = event.get_qtevent();
    
    if (qevent != NULL)
      MyQCheckBox_OnlyCustomPainting::mouseReleaseEvent(qevent);

    update();
  }
  
  MOUSE_CYCLE_CALLBACKS_FOR_QT;

  
#if 0  //unable to make this work. Using the "clicked" signal instead.
 signals:
  
  void rightClicked(){
    printf("Right clicked\n");
  }
#endif
  
};


#endif // QT_MYQCHECKBOX_H
