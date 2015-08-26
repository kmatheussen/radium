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

#include "../Qt/EditorWidget.h"

#include "../common/instruments_proc.h"
#include "../common/vector_proc.h"
#include "../audio/SoundPlugin_proc.h"
#include "../audio/Pd_plugin_proc.h"

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

inline static void CHECKBOX_paint(QPainter *painter, bool is_checked, bool is_enabled, int width, int height, QString text){
#ifdef COMPILING_RADIUM
  //QColor *colors = static_cast<EditorWidget*>(root->song->tracker_windows->os_visual.widget)->colors;
#else
    QColor *colors = g_colors;
#endif

    QColor col; // on

    if(text!=""){
      col = get_qcolor(BUTTONS_COLOR_NUM);
#if 1
      col = mix_colors(col.light(70),QColor(98,59,33),0.55);//editor->colors[colnum].light(52);
      col.setAlpha(76);
#endif
    }else{
      col = get_qcolor(HIGH_BACKGROUND_COLOR_NUM).light(52);
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
      painter->setPen(col);
      painter->drawRect(1,1,width-3,height-2); // outer
      painter->drawRect(2,2,width-5,height-4); // inner
    }


    if(text!=""){
      painter->setPen(get_qcolor(HIGH_EDITOR_BACKGROUND_COLOR_NUM));
      painter->drawRect(0,0,width,height);

      //QRect rect(5,3,width-5,height-3);
      QRect rect(1,1,width-2,height-1);//5,3,width-5,height-3);
      QColor black(0,0,0);
      if(is_checked==true)
        black.setAlpha(190);
      else
        black.setAlpha(120);
      painter->setPen(black);

      if(text=="Loop")
        text = text + " " + QChar(8634);

      if(height>width){
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
  bool _undo_patchvoice;

  bool _is_a_pd_slider;

  QString vertical_text;

  void init(){
    _has_mouse=false;
    _patch = NULL;
    _effect_num = 0;
    _undo_patchvoice = false;
    _is_a_pd_slider = false;
  }

  MyQCheckBox ( QWidget * parent = 0 ) : QCheckBox(parent) {init();}
  MyQCheckBox ( const QString & text, QWidget * parent = 0) : QCheckBox(text,parent) {init();}

  void mousePressEvent ( QMouseEvent * event )
  {
    if (event->button() == Qt::LeftButton){      
      //setSliderDown(true);    
#ifdef COMPILING_RADIUM
      if(_undo_patchvoice==true)
        Undo_PatchVoice_CurrPos(_patch,_effect_num);
      else if(_patch!=NULL  && _patch->instrument==get_audio_instrument())
        Undo_AudioEffect_CurrPos(_patch, _effect_num);
#endif
      //handle_mouse_event(event);
      _has_mouse = true;
      printf("Got it %p %d. Checked: %d\n",_patch,_effect_num,!isChecked());
      setChecked(!isChecked());

    }else{

#ifdef COMPILING_RADIUM
      vector_t options = {0};

      if(_is_a_pd_slider){
        /*
        VECTOR_push_back(&options, "Set Symbol Name");
        VECTOR_push_back(&options, "Set Type");
        VECTOR_push_back(&options, "Set Minimum Value");
        VECTOR_push_back(&options, "Set Maximum Value");
        */
        VECTOR_push_back(&options, "Delete");
      } else {
        VECTOR_push_back(&options, "Reset");
        //VECTOR_push_back(&options, "Set Value");
      }


      int command = GFX_Menu(root->song->tracker_windows, NULL, "", &options);

      //printf("command: %d, _patch: %p, is_audio: %d\n",command, _patch, _patch!=NULL && _patch->instrument==get_audio_instrument());

      if(command==0 && _patch!=NULL && _patch->instrument==get_audio_instrument()){
        SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
        if(_is_a_pd_slider) {
          //printf("Calling delete controller for %p / %d\n",plugin,_effect_num);
          PD_delete_controller(plugin, _effect_num);
        } else {
          PLUGIN_reset_one_effect(plugin,_effect_num);
          GFX_update_instrument_widget(_patch);
        }
      }

#endif // COMPILING_RADIUM

      event->accept();
    }
  }


  void paintEvent ( QPaintEvent * ev ){
    QPainter p(this);

    if(text().startsWith("V ")){
      vertical_text = text().right(text().size()-2);
      setText("");
    }

    CHECKBOX_paint(&p, isChecked(), isEnabled(), width(), height(), vertical_text!="" ? vertical_text : text());
  }
};


#endif // QT_MYQCHECKBOX_H
