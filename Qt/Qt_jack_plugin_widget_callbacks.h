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

  void update_gui(void){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    port1Edit->setText(JACK_get_name(plugin, 0));
    port2Edit->setText(JACK_get_name(plugin, 1));
  }

public slots:

  void on_port1Edit_editingFinished(){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    JACK_set_name(plugin, 0, port1Edit->text().toUtf8().constData());
    update_gui();
    set_editor_focus();
  }

  void on_port2Edit_editingFinished(){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    JACK_set_name(plugin, 1, port2Edit->text().toUtf8().constData());
    update_gui();
    set_editor_focus();
  }
};
