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

#include "../audio/undo_audio_effect_proc.h"
#include "../common/undo_patchvoice_proc.h"

extern struct Root *root;

static QColor mix_colors(const QColor &c1, const QColor &c2, float how_much){

  float a1 = how_much;
  float a2 = 1.0f-a1;

  if(c1.red()==0 && c1.green()==0 && c1.blue()==0){ // some of the black lines doesn't look look very good.
    int r = 74*a1 + c2.red()*a2;
    int g = 74*a1 + c2.green()*a2;
    int b = 74*a1 + c2.blue()*a2;

    return QColor(r,g,b);
  }else{

    int r = c1.red()*a1 + c2.red()*a2;
    int g = c1.green()*a1 + c2.green()*a2;
    int b = c1.blue()*a1 + c2.blue()*a2;

    return QColor(r,g,b);
  }
}


inline static void CHECKBOX_paint(QPainter *painter, bool is_checked, bool is_enabled, int width, int height, bool contains_text){
    EditorWidget *editor = static_cast<EditorWidget*>(root->song->tracker_windows->os_visual.widget);

    QColor col1; // off
    QColor col2; // on

    int col1num = 13;
    int col2num = 9;

    if(is_enabled==true){
      col1 = editor->colors[col1num].light(40);
      col2 = editor->colors[col2num].light(52);
    }else{
      col1 = editor->colors[col1num].light(96);
      col2 = editor->colors[col2num].light(90);
    }

    if(contains_text){
      col1.setAlpha(120);
      col2 = mix_colors(col1.light(70),QColor(98,59,33).light(90),0.35);//editor->colors[col2num].light(52);
      col2.setAlpha(70);
    }

    //col2 = QColor(106, 104, 100, 255);
    //col2 = QColor(0, 107, 156, 255);

    if(is_checked==true){
      painter->fillRect(1,1,width-2,height-1,col2);
      painter->setPen(editor->colors[1]);
      //p.drawRect(0,0,width()-1,height()-1);
      //p.drawRect(1,1,width()-3,height()-3);
    }else{
      painter->setPen(col2);
      painter->drawRect(1,1,width-3,height-2);
      painter->drawRect(2,2,width-5,height-4);
      //p.fillRect(0,0,width(),height(),col1);
    }
}

struct MyQCheckBox : public QCheckBox{
  bool _has_mouse;
  struct Patch *_patch;
  int _effect_num;
  bool _undo_patchvoice;

  void init(){
    _has_mouse=false;
    _patch = NULL;
    _effect_num = 0;
    _undo_patchvoice = false;
  }

  MyQCheckBox ( QWidget * parent = 0 ) : QCheckBox(parent) {init();}
  MyQCheckBox ( const QString & text, QWidget * parent = 0) : QCheckBox(text,parent) {init();}


  void mousePressEvent ( QMouseEvent * event )
  {
    if (event->button() == Qt::LeftButton){      
      //setSliderDown(true);    
      if(_undo_patchvoice==true)
        Undo_PatchVoice_CurrPos(_patch,_effect_num);
      else if(_patch!=NULL)
        Undo_AudioEffect_CurrPos(_patch, _effect_num);
      //handle_mouse_event(event);
      _has_mouse = true;
      printf("Got it %p %d\n",_patch,_effect_num);
      setChecked(!isChecked());
    }else
      QCheckBox::mousePressEvent(event);
  }


  void paintEvent ( QPaintEvent * ev ){
    QPainter p(this);
    CHECKBOX_paint(&p, isChecked(), isEnabled(), width(), height(), text()!="");

    if(text()!=""){
      QRect rect(5,2,width()-5,height()-2);
      QColor black(0,0,0);
      if(isChecked())
        black.setAlpha(190);
      else
        black.setAlpha(120);
      p.setPen(black);
      if(text()=="Loop")
        p.drawText(rect, Qt::AlignLeft, text() + " " + QChar(8634));
      else
   
        p.drawText(rect, Qt::AlignLeft, text());
    }
  }
};


#endif // QT_MYQCHECKBOX_H
