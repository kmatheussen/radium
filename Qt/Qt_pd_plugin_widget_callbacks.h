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


#include <QTimer>
#include <QTimerEvent>

#include "../audio/SoundPlugin_proc.h"
#include "../audio/Pd_plugin.h"
#include "../audio/Pd_plugin_proc.h"

#include "Qt_plugin_widget_callbacks_proc.h"

#include "mQt_pd_controller_widget_callbacks.h"

#include "Qt_pd_plugin_widget_callbacks_proc.h"

#include "Qt_pd_plugin_widget.h"


static const int k_timer_interval_here3 = 50;

namespace{
  
class Pd_Plugin_widget : public QWidget, public Ui::Pd_Plugin_widget{
  Q_OBJECT;

public:
  radium::GcHolder<struct Patch> _patch;
  DEFINE_ATOMIC(bool, _gui_is_visible);

  int _num_controllers;
  std::vector<Pd_Controller_widget*> _controllers;
  Pd_Controller_Config_dialog *_conf_dialogs[NUM_PD_CONTROLLERS];

  struct Timer : public QTimer{
    Pd_Plugin_widget *_pd_plugin_widget;
    int _last_cleared_generation;
    std::atomic<int> _clearing_generation;
    bool _last_sent_gui_is_visible;

    void timerEvent(QTimerEvent * e) override {
      RETURN_IF_DATA_IS_INACCESSIBLE();

      SoundPlugin *plugin = (SoundPlugin*)_pd_plugin_widget->_patch->patchdata;
      if(plugin!=NULL) { // dont think plugin can be NULL here though.

	int clearing_generation = _clearing_generation.load();
        if(_last_cleared_generation != clearing_generation){
          _last_cleared_generation = clearing_generation;
          _pd_plugin_widget->clear();
        }

        PD_set_qtgui(plugin, _pd_plugin_widget);

        if(_pd_plugin_widget->isVisible()==false)
          return;

        bool gui_is_visible = ATOMIC_GET(_pd_plugin_widget->_gui_is_visible);
        if (_last_sent_gui_is_visible != gui_is_visible) {
          QWidget *parent = _pd_plugin_widget->parentWidget();
          if(gui_is_visible)
            PLUGINWIDGET_gui_is_visible(parent);
          else
            PLUGINWIDGET_gui_is_hidden(parent);

          _last_sent_gui_is_visible = gui_is_visible;
        }

        _pd_plugin_widget->update_gui();
      }
    }
  };

  Timer _timer;

  Pd_Plugin_widget(QWidget *parent, struct Patch *patch)
    : QWidget(parent)
    , _patch(patch)
    , _num_controllers(0)
  {
    ATOMIC_SET(_gui_is_visible, false);
    setupUi(this);

    for(int i=0;i<NUM_PD_CONTROLLERS;i++)
      _conf_dialogs[i] = new Pd_Controller_Config_dialog(this, patch, i);

    //update_gui();

    PD_set_qtgui((SoundPlugin*)patch->patchdata, this);

    //printf("\n\n\n\n          ************************************ CONSTRUCTOR *************************\n\n\n\n\n");

    _timer._pd_plugin_widget = this;
    _timer._last_sent_gui_is_visible = false;
    _timer._last_cleared_generation = 0;
    _timer._clearing_generation = 0;
    _timer.setInterval(k_timer_interval_here);
    _timer.start();

  }

  ~Pd_Plugin_widget() {
    for(int i=0;i<NUM_PD_CONTROLLERS;i++)
      delete _conf_dialogs[i];
  }

  void hideEvent ( QHideEvent * event ) override {
    for(int i=0;i<NUM_PD_CONTROLLERS;i++)
      _conf_dialogs[i]->parent_is_hiding();

    _timer.stop();
  }

  void showEvent ( QShowEvent * event ) override {
    for(int i=0;i<NUM_PD_CONTROLLERS;i++)
      _conf_dialogs[i]->parent_is_showing();

    _timer.start();
  }

  void update_gui(void){
    if(_controllers.empty()) {
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      //test if plugin==NULL here?
      for(int controller_num=0; controller_num < NUM_PD_CONTROLLERS; controller_num++){
        Pd_Controller *controller = PD_get_controller(plugin, controller_num);
        if(controller->has_gui)
          new_controller(controller_num);
      }
    }
  }

  void new_controller(int controller_num){
    if(_num_controllers>=NUM_PD_CONTROLLERS)
      return;

    if(_num_controllers != controller_num)
      RWarning("Something is probably wrong in the order of creating pd controllers. %d/%d",_num_controllers,controller_num);

    Pd_Controller_widget *controller_widget = new Pd_Controller_widget(this, (SoundPlugin*)_patch->patchdata, controller_num);

    int x = _num_controllers / 8;
    int y = _num_controllers - (8*x);

    gridLayout->addWidget(controller_widget,y,x);
    _num_controllers++;

    _controllers.push_back(controller_widget);
  }

  void new_controller() {
    new_controller(_num_controllers);
  }

  
  void clear(){
    for(unsigned int i=0; i<_controllers.size(); i++) {
      Pd_Controller_widget *c = _controllers[i];
      gridLayout->removeWidget(c);
      c->hide(); // Probably not necessary, but this stops the timer.

      // Todo: c is never deleted.

      //c->deleteLater(); // This can crash radium
      //delete c; // This is very likely to crash radium
    }

    _controllers.clear();

    _num_controllers = 0;
  }

  
public slots:

};

}


void PDGUI_is_hidden(void *gui){
  if(gui!=NULL){
    Pd_Plugin_widget *pd_widget = (Pd_Plugin_widget *)gui;
    ATOMIC_SET(pd_widget->_gui_is_visible, false);
  }
}

void PDGUI_is_visible(void *gui){
  if(gui!=NULL){
    Pd_Plugin_widget *pd_widget = (Pd_Plugin_widget *)gui;
    ATOMIC_SET(pd_widget->_gui_is_visible, true);
  }
}

void PDGUI_clear(void *gui){
  if(gui!=NULL){
    Pd_Plugin_widget *pw = (Pd_Plugin_widget *)gui;
    pw->clear();
  }
}

// Called from an audio thread
void PDGUI_schedule_clearing(void *gui){
  if(gui!=NULL){
    Pd_Plugin_widget *pw = (Pd_Plugin_widget *)gui;
    pw->_timer._clearing_generation++;
  }
}
