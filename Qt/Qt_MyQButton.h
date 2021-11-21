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

#ifndef QT_MYQBUTTON_H
#define QT_MYQBUTTON_H

#include <QToolButton>

#include "Qt_mix_colors.h"
#include "Qt_colors_proc.h"

#include "../audio/SoundPlugin.h"

#include "../api/api_gui_proc.h"
#include "../api/api_proc.h"

#include "../embedded_scheme/s7extra_proc.h"

//#include "Qt_MyQCheckBox.h"


struct MyQButton : public QToolButton, public radium::MouseCycleFix {

  MyQButton ( QWidget * parent = 0 ) : QToolButton(parent) {}
  MyQButton ( const QString & text, QWidget * parent = 0)
    : QToolButton(parent)
  {
    setMouseTracking(true);
    setText(text);
  }

  /*
  void mousePressEvent ( QMouseEvent * event ) override
  {
    printf("PRESSED %d\n", event->button() == Qt::LeftButton);
    QToolButton::mousePressEvent(event);
  }    
  */

  bool _is_hovered = false;

  int x1_border = 1;
  int x2_border = 1;
  int y1_border = 1;
  int y2_border = 1;
  
  bool fix_mousePressEvent(radium::MouseCycleEvent &qmouseevent) override {
    _is_hovered = true;

    auto *event = qmouseevent.get_qtevent();
    if (event)
      QToolButton::mousePressEvent(event);
    update();

    return true;
  }

  void fix_mouseMoveEvent(radium::MouseCycleEvent &qmouseevent) override {
    auto *event = qmouseevent.get_qtevent();
    if (event)
      QToolButton::mouseMoveEvent(event);
  }

  bool fix_mouseReleaseEvent(radium::MouseCycleEvent &event) override{
    _is_hovered = false;
    
    auto *qevent = event.get_qtevent();
    
    if (qevent != NULL)
      QToolButton::mouseReleaseEvent(qevent);

    update();

    return true;
  }
  
  MOUSE_CYCLE_CALLBACKS_FOR_QT;

  void enterEvent(QEvent *event) override {
    _is_hovered = true;
    update();
  }

  void leaveEvent(QEvent *event) override {
    _is_hovered = false;
    update();
  }

  std::function<void(void)> _show_popup_menu;

  // Override event instead of contextMenuEvent since contextMenuEvent is not called when widget is disabled.
  bool event(QEvent *event) override {
    
    if (event->type() == QEvent::MouseButtonPress){
      QMouseEvent *mouseEvent = dynamic_cast<QMouseEvent*>(event);
      if (mouseEvent==NULL){
        R_ASSERT(false);
      } else {
        if (mouseEvent->button()==Qt::RightButton && _show_popup_menu)
          _show_popup_menu();
      }
    }

    return QToolButton::event(event);
  }
    
  /*
  void contextMenuEvent(QContextMenuEvent *event) override {
    if (_show_popup_menu) {
      _show_popup_menu();
    } else
      QToolButton::contextMenuEvent(event);
  }
  */

  radium::GcHolder<const wchar_t> _text_to_draw;
  
  void paintEvent ( QPaintEvent * ev ) override {
    TRACK_PAINT();
    //QToolButton::paintEvent(ev);
    QPainter p(this);
    //p.eraseRect(rect());
    //printf("isdown: %d. enabled: %d, width: %d, height: %d\n", isDown(),isEnabled(), width(), height());

    p.setRenderHints(QPainter::Antialiasing,true);
    
#if 1

    //QRect rect(1,1,width()-2,height()-2);
    
    //radium::ScopedQClipRect scoped_clip_rect(p, rect);
    
    API_run_custom_gui_paint_function(this,
                                      &p, &ev->region(),
                                      [this](){

                                        static dynvec_t args = {}; // static to avoid allocating bdwgc-memory.

                                        DYNVEC_light_clean(args);
 
                                        if (_text_to_draw.data()==NULL || QString::fromWCharArray(_text_to_draw.data())!=text())
                                          _text_to_draw.set(STRING_create(text()));
                                       
                                        DYNVEC_push_back(args, DYN_create_int(API_get_gui_from_widget(this)));
                                        DYNVEC_push_back(args, DYN_create_string_dont_copy(_text_to_draw.data()));
                                        DYNVEC_push_back(args, DYN_create_bool(isDown()));
                                        DYNVEC_push_back(args, DYN_create_int(x1_border));
                                        DYNVEC_push_back(args, DYN_create_int(y1_border));
                                        DYNVEC_push_back(args, DYN_create_int(width()-x2_border));
                                        DYNVEC_push_back(args, DYN_create_int(height()-y2_border));

                                        if (isDown()){
                                          DYNVEC_push_back(args, DYN_create_symbol_dont_copy(":selected-color"));
                                          DYNVEC_push_back(args, DYN_create_string_dont_copy(L"button_pressed_v2"));
                                        } else {                                        
                                          DYNVEC_push_back(args, DYN_create_symbol_dont_copy(":background-color"));
                                          DYNVEC_push_back(args, DYN_create_string_dont_copy(L"button_v2"));
                                        }
                                            
                                        DYNVEC_push_back(args, DYN_create_symbol_dont_copy(":is-hovering"));
                                        DYNVEC_push_back(args, DYN_create_bool(_is_hovered && isEnabled()));
                                        
                                        DYNVEC_push_back(args, DYN_create_symbol_dont_copy(":is-enabled"));
                                        DYNVEC_push_back(args, DYN_create_bool(isEnabled()));
                                                                       
                                        S7EXTRA_GET_FUNC(draw_checkbox_func, "draw-button");
                                        s7extra_applyFunc_void(draw_checkbox_func, args);

                                        /*
                                        evalScheme(talloc_format("(draw-button %d \"%s\" %s %d %d %d %d :is-hovering %s)", 
                                                                 (int)API_get_gui_from_widget(this),
                                                                 text().toUtf8().constData(), isDown() ? "#t" : "#f",
                                                                 1, 1, width()-1, height()-1,
                                                                 _is_hovered ? "#t" : "#f"
                                                                 )
                                                   );
                                        */
                                        
                                        /*
                                          S7EXTRA_GET_FUNC(draw_checkbox_func, "draw-button");
                                          S7CALL(void_int_charstring_bool_float_float_float_float,
                                          draw_checkbox_func,
                                               API_get_gui_from_widget(this),
                                               text2.toUtf8().constData(), isChecked(),
                                               0, 0, width(), height()
                                               );
                                        */
                                      });
    
#else
    
    CHECKBOX_paint(&p, !isDown(), isEnabled(), width(), height(), text(), false, false);

#endif
  }
  
};


#endif // QT_MYQBUTTON_H
