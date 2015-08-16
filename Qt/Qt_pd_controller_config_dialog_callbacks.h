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

#include "../common/fxlines_proc.h"

#include "../audio/SoundPlugin_proc.h"
#include "../audio/Pd_plugin_proc.h"

#include "Qt_pd_controller_config_dialog.h"

static const int k_timer_interval_here4 = 50;

extern LANGSPEC void QT_UpdateEditor(struct Tracker_Windows *window);
extern LANGSPEC void QT_RepaintEditor(struct Tracker_Windows *window);

class Pd_Controller_Config_dialog : public QDialog, public Ui::Pd_Controller_Config_dialog {
  Q_OBJECT;

public:
  struct Patch *_patch;
  int _controller_num;
  bool _is_updating_gui;

  QByteArray _my_geometry;
  int _type;
  QString _name;
  const wchar_t *_display_name;
  
  struct Timer : QTimer{
    Pd_Controller_Config_dialog *my;
    void timerEvent(QTimerEvent * e){
      my->timerCallback();
    }
  };

  Timer _timer;

 Pd_Controller_Config_dialog(QWidget *parent, struct Patch *patch, int controller_num)
    : QDialog(parent,"pd_plugin widget")
    , _patch(patch)
    , _controller_num(controller_num)
    , _is_updating_gui(true)
    , _type(0)
    , _display_name(NULL)
 {
    setupUi(this);
    update_gui();

    //if(controller_num==0)
    //  printf("*** New controller dialog for %s. min: %f, val: %f, max: %f\n", _controller->name, _controller->min_value, _controller->value, _controller->max_value);
    
    _timer.setInterval(k_timer_interval_here4);
    _timer.my = this;
  }

  Pd_Controller *get_controller(){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if(plugin==NULL)
      return NULL;

    return PD_get_controller(plugin, _controller_num);
  }

  void hide_and_remember_position() {
    _my_geometry = saveGeometry();
    hide();
  }

  void timerCallback(void) {
    Pd_Controller *controller = get_controller();
    if(controller==NULL)
      return;

    if (isVisible() && controller->config_dialog_visible==false) {
      hide_and_remember_position();
    } if (!isVisible() && controller->config_dialog_visible==true) {
      show();
      restoreGeometry((const QByteArray &)_my_geometry);
    }

    if (controller->type != _type) {
      _type = controller->type;
      update_gui();
    }

    if (controller->display_name != NULL && controller->display_name!=_display_name){
      _name = STRING_get_qstring(controller->display_name);
      _display_name = controller->display_name;
        
    } else if (controller->display_name==NULL && strcmp(controller->name, _name.toUtf8().constData())) {
      _name = controller->name;
      update_gui();
    }

    if(controller->type != EFFECT_FORMAT_BOOL){
      if (min_value_widget->value() != controller->min_value)
        update_gui();
      if (max_value_widget->value() != controller->max_value)
        update_gui();
    }
  }

  void parent_is_hiding() {
    hide_and_remember_position();
    _timer.stop();
  }

  void parent_is_showing() {
    if(! _timer.isActive())
      _timer.start();
  }

  void update_gui(void){
    Pd_Controller *controller = get_controller();
    if(controller==NULL){
      printf("controller==NULL (\?\?\?)\n");
      return;
    }

    _is_updating_gui = true;

    if(controller->type==EFFECT_FORMAT_FLOAT || controller->type==EFFECT_FORMAT_INT) {
      //value_slider->show();
      //onoff_widget->hide();
      min_value_widget->show();
      max_value_widget->show();

      min_value_widget->setValue(controller->min_value);
      max_value_widget->setValue(controller->max_value);

    } else {
      min_value_widget->hide();
      max_value_widget->hide();
    }

    type_selector->setCurrentIndex(controller->type);
    if (controller->display_name != NULL)
      name_widget->setText(STRING_get_qstring(controller->display_name));
    else
      name_widget->setText(controller->name);

    _is_updating_gui = false;
  }

  void closeEvent(QCloseEvent *event) {
    Pd_Controller *controller = get_controller();
    if(controller!=NULL)
      controller->config_dialog_visible = false;
    event->ignore(); // Only hide the window, dont close it.
  }

  void set_min_max(double new_min, double new_max){
    Pd_Controller *controller = get_controller();
    if(controller==NULL){
      printf("controller==NULL (\?\?\?)\n");
      return;
    }

    Undo_Open();{

      Undo_PdControllers_CurrPos(_patch);

      float old_min = controller->min_value;
      float old_max = controller->max_value;

      controller->min_value = new_min;
      controller->max_value = new_max;

      FX_min_max_have_changed_for_patch(_patch, controller->num, old_min, old_max, new_min, new_max);

    }Undo_Close();
  }


public slots:

  void on_min_value_widget_valueChanged(double val){
    Pd_Controller *controller = get_controller();
    if(controller==NULL){
      printf("controller==NULL (\?\?\?)\n");
      return;
    }

    if(_is_updating_gui==false)
      if(val!=controller->max_value) {
        set_min_max(val, controller->max_value);
      }
  }

  void on_max_value_widget_valueChanged(double val){
    Pd_Controller *controller = get_controller();
    if(controller==NULL){
      printf("controller==NULL (\?\?\?)\n");
      return;
    }

    if(_is_updating_gui==false)
      if(val!=controller->min_value) {
        set_min_max(controller->min_value, val);
      }
  }

  void on_type_selector_currentIndexChanged( int val){
    Pd_Controller *controller = get_controller();
    if(controller==NULL){
      printf("controller==NULL (\?\?\?)\n");
      return;
    }

    if(_is_updating_gui==false)
      if(val!=controller->type) {
        Undo_PdControllers_CurrPos(_patch);
        controller->type = val;
        update_gui();
      }
    //value_slider->update();
  }

  void on_name_widget_returnPressed(){
    Pd_Controller *controller = get_controller();
    if(controller==NULL){
      printf("controller==NULL (\?\?\?)\n");
      return;
    }

    controller->config_dialog_visible = false;
    hide_and_remember_position();
  }

  void on_name_widget_editingFinished(){
    Pd_Controller *controller = get_controller();
    if(controller==NULL){
      printf("controller==NULL (\?\?\?)\n");
      return;
    }

    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

    if(_is_updating_gui==false) {
      printf("name: -%s-\n",name_widget->text().toUtf8().constData());
      QString name = name_widget->text();
      if(name != controller->name) {
        Undo_PdControllers_CurrPos(_patch);
        PD_set_controller_name(plugin, controller->num, STRING_create(name));
      }
      //set_editor_focus();
      //_pd_controller_widget->update();
    }
  }

  void on_hide_button_released(){
    Pd_Controller *controller = get_controller();
    if(controller==NULL){
      printf("controller==NULL (\?\?\?)\n");
      return;
    }

    controller->config_dialog_visible = false;
    hide_and_remember_position();
  }

  void on_delete_button_released(){
    Pd_Controller *controller = get_controller();
    if(controller==NULL){
      printf("controller==NULL (\?\?\?)\n");
      return;
    }

    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

    PD_delete_controller(plugin, controller->num);
  }
};

