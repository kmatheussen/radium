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
#include "../audio/Pd_plugin.h"
#include "../audio/Pd_plugin_proc.h"

#include "mQt_pd_controller_widget_callbacks.h"

#include "Qt_pd_plugin_widget_callbacks_proc.h"

#include "Qt_pd_plugin_widget.h"


class Pd_Plugin_widget : public QWidget, public Ui::Pd_Plugin_widget{
  Q_OBJECT;

public:
  struct Patch *_patch;
  int _num_controllers;
  std::vector<Pd_Controller_widget*> _controllers;

  Pd_Plugin_widget(QWidget *parent, struct Patch *patch)
    : QWidget(parent,"pd_plugin widget")
    , _patch(patch)
    , _num_controllers(0)
  {
    setupUi(this);

    PD_get_controller((SoundPlugin*)patch->patchdata, 0)->gui = this;

    update_gui();
  }

  void update_gui(void){
  }

  void new_controller(int controller_num){
    printf("hepp\n");
    if(_num_controllers>=40)
      return;

    if(_num_controllers != controller_num)
      RWarning("Something is probably wrong in the order of creating pd controllers. %d/%d",_num_controllers,controller_num);

    Pd_Controller_widget *controller_widget = new Pd_Controller_widget(this, (SoundPlugin*)_patch->patchdata, controller_num);

    int x = _num_controllers / 8;
    int y = _num_controllers - (8*x);

    printf("x: %d. y: %d\n",x,y);

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
      c->deleteLater();
      //delete c;
    }

    _controllers.clear();

    _num_controllers = 0;
  }

public slots:

};

void PDGUI_clear(void *gui) {
  Pd_Plugin_widget *pw = (Pd_Plugin_widget*)gui;
  pw->clear();
}

void PDGUI_add_controller(void *gui, int controller_num){
  Pd_Plugin_widget *pw = (Pd_Plugin_widget*)gui;
  pw->new_controller(controller_num);
}

/*
void PDGUI_new_controller() {
}
*/
