/* Copyright 2013 Kjetil S. Matheussen

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

#include "mQt_tools_callbacks.h"

//#include "../common/nsmtracker.h"

#include "helpers.h"

struct Root;
extern struct Root *root;

static QPointer<MidiLearnPrefs> midilearn_prefs_widget;

static void ensure_open(void){
  if (midilearn_prefs_widget.isNull()){
    midilearn_prefs_widget = new MidiLearnPrefs(g_main_window);
    g_static_toplevel_widgets.push_back(midilearn_prefs_widget.data());
  }
}

void MIDILEARN_PREFS_open(void){
  ensure_open();
  safeShowOrExec(midilearn_prefs_widget.data(), true);
}

void MIDILEARN_PREFS_add(MidiLearn *midi_learn){
  ensure_open();
  midilearn_prefs_widget->add(midi_learn);
}

void MIDILEARN_PREFS_remove(MidiLearn *midi_learn){
  ensure_open();
  midilearn_prefs_widget->remove(midi_learn);
}

void TOOLS_open(void){
  static QPointer<Tools> widget=NULL;
  if(widget==NULL){
    widget = new Tools(g_main_window);
    //widget->setWindowModality(Qt::ApplicationModal);
    g_static_toplevel_widgets.push_back(widget.data());
  }

  safeShowOrExec(widget, true);
}



