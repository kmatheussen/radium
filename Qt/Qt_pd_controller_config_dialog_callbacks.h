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


#include <QMessageBox>
#include <QFileDialog>
#include <QTimer>
#include <QTimerEvent>

#include "../audio/SoundPlugin_proc.h"
#include "../audio/Pd_plugin_proc.h"

#include "Qt_pd_controller_config_dialog.h"


class Pd_Controller_Config_dialog : public QDialog, public Ui::Pd_Controller_Config_dialog{
  Q_OBJECT;

public:
  QWidget *_pd_controller_widget;
  SoundPlugin *_plugin; // Pd_plugin.recreate_from_state also recreate gui, so it is safe to store this one here.
  Pd_Controller *_controller; // Pd_plugin.recreate_from_state also recreate gui, so it is safe to store this one here.
  bool _showing;

 Pd_Controller_Config_dialog(QWidget *parent, SoundPlugin *plugin, int controller_num)
    : QDialog(parent,"pd_plugin widget")
    , _pd_controller_widget(parent)
    , _plugin(plugin)
    , _controller(PD_get_controller(plugin, controller_num))
    , _showing(false)
 {
    setupUi(this);
    update_gui();
  }

  void update_gui(void){
    if(_controller->type==0 || _controller->type==1) {
      //value_slider->show();
      //onoff_widget->hide();
      min_value_widget->show();
      max_value_widget->show();

    } else {
      min_value_widget->hide();
      max_value_widget->hide();
    }

    type_selector->setCurrentIndex(_controller->type);
    name_widget->setText(_controller->name);
  }

  void closeEvent(QCloseEvent *event) {
    _showing = false;
    event->ignore(); // keep window
  }


public slots:

  void on_min_value_widget_valueChanged(double val){
    if(val!=_controller->max_value) {
      Undo_PdControllers_CurrPos(_plugin->patch);
      _controller->min_value = val;
    }
  }

  void on_max_value_widget_valueChanged(double val){
    if(val!=_controller->min_value) {
       Undo_PdControllers_CurrPos(_plugin->patch);
      _controller->max_value = val;
    }
  }

  void on_type_selector_currentIndexChanged( int val){
    if(val!=_controller->type) {
      Undo_PdControllers_CurrPos(_plugin->patch);
      _controller->type = val;
      update_gui();
    }
    //value_slider->update();
  }

  void on_name_widget_returnPressed(){
    _showing = false;
    hide();
  }

  void on_name_widget_editingFinished(){
    printf("name: -%s-\n",name_widget->text().ascii());
    QString name = name_widget->text();
    if(name != _controller->name) {
      Undo_PdControllers_CurrPos(_plugin->patch);
      PD_set_controller_name(_plugin, _controller->num, name.ascii());
    }
    //set_editor_focus();
    //_pd_controller_widget->update();
  }

  void on_hide_button_released(){
    _showing = false;
    hide();
  }

  void on_delete_button_released(){
    //Undo_PdControllers_CurrPos(_plugin->patch);
    PD_delete_controller(_plugin, _controller->num);
  }
};

