/* Copyright 2012-2013 Kjetil S. Matheussen

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


#include "Qt_jack_plugin_widget.h"

#include "../audio/Jack_plugin_proc.h"


class Jack_Plugin_widget : public QWidget, public Ui::Jack_Plugin_widget{
  Q_OBJECT;

public:

  Patch *_patch;

  Jack_Plugin_widget(QWidget *parent, struct Patch *patch)
    : QWidget(parent)
    , _patch(patch)
  {
    setupUi(this);
    update_gui();
  }

  ~Jack_Plugin_widget() {
  }

  QLineEdit *get_line_edit(int num){
    switch(num){
    case 0: return port1Edit;
    case 1: return port2Edit;
    case 2: return port3Edit;
    case 3: return port4Edit;
    case 4: return port5Edit;
    case 5: return port6Edit;
    case 6: return port7Edit;
    case 7: return port8Edit;
    }
    return NULL;
  }
  
  QLabel *get_label(int num){
    switch(num){
    case 0: return label_1;
    case 1: return label_2;
    case 2: return label_3;
    case 3: return label_4;
    case 4: return label_5;
    case 5: return label_6;
    case 6: return label_7;
    case 7: return label_8;
    }
    return NULL;
  }

  bool has_removed_unused_widgets = false;
  
  void update_gui(void){    
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    
    int num_ports = plugin->type->num_inputs;
    if (num_ports==0)
      num_ports = plugin->type->num_outputs;
    
    for(int i=0;i<8;i++){
      QLineEdit *line_edit = get_line_edit(i);
      R_ASSERT_RETURN_IF_FALSE(line_edit!=NULL);
      if (i < num_ports)
        line_edit->setText(JACK_get_name(plugin, i));
      else if(has_removed_unused_widgets==false){
        QLabel *label = get_label(i);
        R_ASSERT_RETURN_IF_FALSE(label!=NULL);
        label->hide();
        line_edit->hide();
      }
    }

    has_removed_unused_widgets = true;
  }

  void editing_finished(int portnum){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    QLineEdit *line_edit = get_line_edit(portnum);
    JACK_set_name(plugin, portnum, line_edit->text().toUtf8().constData());
    update_gui();
    set_editor_focus();
  }
  
public slots:

  void on_port1Edit_editingFinished(){
    editing_finished(0);
  }
  void on_port2Edit_editingFinished(){
    editing_finished(1);
  }
  void on_port3Edit_editingFinished(){
    editing_finished(2);
  }
  void on_port4Edit_editingFinished(){
    editing_finished(3);
  }
  void on_port5Edit_editingFinished(){
    editing_finished(4);
  }
  void on_port6Edit_editingFinished(){
    editing_finished(5);
  }
  void on_port7Edit_editingFinished(){
    editing_finished(6);
  }
  void on_port8Edit_editingFinished(){
    editing_finished(7);
  }
};
