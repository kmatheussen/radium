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

#ifndef QT_MYQLABEL_H
#define QT_MYQLABEL_H

#include <QLabel>

#include "Qt_sequencer_proc.h"


struct MyQLabel : public QLabel {

  bool _use_custom_painter = false;
  
  MyQLabel(QWidget * parent = 0)
    : QLabel(parent)
  {}
  
  MyQLabel ( const QString & text, QWidget * parent = 0)
    : QLabel(text, parent)
  {}


  
  void paintEvent ( QPaintEvent * ev ) override {
    TRACK_PAINT();
      
    if (!_use_custom_painter) {

      QLabel::paintEvent(ev);
      
    } else {
      
      QPainter p(this);
      
      p.setRenderHints(QPainter::Antialiasing,true);
      
      myDrawText(p, rect(), text(), alignment(), false, 0, true, false);

    }
  }
};
  

#endif
