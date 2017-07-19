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


#include <QSpinBox>
#include <QMouseEvent>

#include "../audio/undo_audio_effect_proc.h"
#include "../common/undo_patchvoice_proc.h"

#include "helpers.h"

#include "../OpenGL/Widget_proc.h"


extern void set_editor_focus(void);

  
struct MyQSpinBox : public GL_PauseCaller, public QSpinBox{
  bool _has_mouse;
  struct Patch *_patch;
  int _effect_num;
  bool _is_patchvoice_spinbox;

  void init(){
    _has_mouse=false;
    _patch = NULL;
    _effect_num = 0;
    _is_patchvoice_spinbox = false;
  }

  MyQSpinBox ( QWidget * parent = 0 ) : QSpinBox(parent) {init();}

  void focusInEvent ( QFocusEvent *e ) override {                                 
    printf("Got focusInEvent\n");
    obtain_keyboard_focus();
    GL_lock();
    QSpinBox::focusInEvent(e);                                             
    GL_unlock();
  }                                                                     
  void focusOutEvent ( QFocusEvent *e ) override {                                
    printf("Got focusOutEvent\n");
    release_keyboard_focus();
    GL_lock();
    QSpinBox::focusOutEvent(e);                                            
    GL_unlock();
  }                                                                     
  void hideEvent ( QHideEvent *e ) override {                                
    //printf("Got hideEvent\n");
    release_keyboard_focus();
    QSpinBox::hideEvent(e);                                            
  }                                                                     
  void 	wheelEvent ( QWheelEvent * event ) override {
    printf("Got wheel event\n");
    QSpinBox::wheelEvent(event);
    set_editor_focus();
  }


  void mousePressEvent ( QMouseEvent * event ) override
  {
    if (event->button() == Qt::LeftButton){      
      //setSliderDown(true);    
      if(_is_patchvoice_spinbox==true)
        ADD_UNDO(PatchVoice_CurrPos(_patch,_effect_num));

      printf("Got it %p %d\n",_patch,_effect_num);
      //setChecked(!isChecked());
    }

    QSpinBox::mousePressEvent(event);
  }

#if 0
  void paintEvent ( QPaintEvent * ev ) override {
    TRACK_PAINT();
    
    EditorWidget *editor = static_cast<EditorWidget*>(root->song->tracker_windows->os_visual.widget);

    QColor col1; // off
    QColor col2; // on

    int col1num = 11;
    int col2num = 10;

    if(isEnabled()){
      col1 = editor->colors[col1num].light(90);
      col2 = editor->colors[col2num].light(82);
    }else{
      col1 = editor->colors[col1num].light(116);
      col2 = editor->colors[col2num].light(110);
    }

    //col2 = QColor(106, 104, 100, 255);
    //col2 = QColor(0, 107, 156, 255);

    QPainter p(this);

    if(isChecked()){
      p.fillRect(1,1,width()-2,height()-2,col2);
      p.setPen(editor->colors[1]);
      //p.drawRect(0,0,width()-1,height()-1);
      //p.drawRect(1,1,width()-3,height()-3);
    }else{
      p.setPen(col2);
      p.drawRect(1,1,width()-3,height()-3);
      p.drawRect(2,2,width()-5,height()-5);
      //p.fillRect(0,0,width(),height(),col1);
    }
  }
#endif
};

