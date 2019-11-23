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

#include "../audio/SoundPlugin.h"

#include "Qt_MyQCheckBox.h"

struct MyQButton : public QToolButton{

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

  void enterEvent(QEvent *event) override {
    _is_hovered = true;
    update();
  }

  void leaveEvent(QEvent *event) override {
    _is_hovered = false;
    update();
  }

  void paintEvent ( QPaintEvent * ev ) override {
    TRACK_PAINT();
    //QToolButton::paintEvent(ev);
    QPainter p(this);
    //p.eraseRect(rect());
    //printf("isdown: %d. enabled: %d, width: %d, height: %d\n", isDown(),isEnabled(), width(), height());

    p.setRenderHints(QPainter::Antialiasing,true);
    
#if 1

    QRect rect(1,1,width()-2,height()-2);
    
    radium::ScopedQClipRect scoped_clip_rect(p, rect);
    
    API_run_custom_gui_paint_function(this,
                                      &p, &ev->region(),
                                      [this](){
                                        evalScheme(talloc_format("(draw-button %d \"%s\" %s %d %d %d %d :is-hovering %s)", 
                                                                 (int)API_get_gui_from_widget(this),
                                                                 text().toUtf8().constData(), isDown() ? "#t" : "#f",
                                                                 1, 1, width()-1, height()-1,
                                                                 _is_hovered ? "#t" : "#f"
                                                                 )
                                                   );
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
