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


#include <QCheckBox>
#include <QMouseEvent>

#include "../audio/undo_audio_effect_proc.h"
#include "../common/undo_patchvoice_proc.h"

extern struct Root *root;

inline static void CHECKBOX_paint(QPainter *painter, bool is_checked, bool is_enabled, int width, int height){
    EditorWidget *editor = static_cast<EditorWidget*>(root->song->tracker_windows->os_visual.widget);

    QColor col1; // off
    QColor col2; // on

    int col1num = 11;
    int col2num = 10;

    if(is_enabled==true){
      col1 = editor->colors[col1num].light(40);
      col2 = editor->colors[col2num].light(52);
    }else{
      col1 = editor->colors[col1num].light(96);
      col2 = editor->colors[col2num].light(90);
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
    CHECKBOX_paint(&p, isChecked(), isEnabled(), width(), height());
  }

};

