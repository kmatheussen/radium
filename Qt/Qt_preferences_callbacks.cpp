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


#include <stdio.h>
#include <sndfile.h>
#include <unistd.h>

#include <QMessageBox>

#include "../common/nsmtracker.h"
#include "../common/hashmap_proc.h"
#include "../common/OS_string_proc.h"
#include "../common/OS_settings_proc.h"
#include "../common/settings_proc.h"
#include "../OpenGL/Widget_proc.h"
#include "../audio/MultiCore_proc.h"
#include "../midi/midi_i_input_proc.h"
#include "../midi/midi_i_plugin_proc.h"
#include "../midi/midi_menues_proc.h"

extern "C" {
  struct PyObject;
#include "../api/radium_proc.h"
}

#include "../Qt/Qt_MyQSpinBox.h"
#include <FocusSniffers.h>
#include "helpers.h"

#include "mQt_vst_paths_widget_callbacks.h"

#include "Qt_preferences.h"



extern struct Root *root;

namespace{

class Preferences : public QDialog, public Ui::Preferences {
  Q_OBJECT

 public:
  bool _initing;

 Preferences(QWidget *parent=NULL)
   : QDialog(parent, "Preferences")
  {
    _initing = true;

    setupUi(this);

    updateWidgets();

    // VST
    {    
      Vst_paths_widget *vst_widget = new Vst_paths_widget;
      vst_widget->buttonBox->hide();
      
      tabWidget->addTab(vst_widget, "VST");
    }

    tabWidget->setCurrentIndex(0);

    _initing = false;
  }

  void updateWidgets(){

    // OpenGL
    {
      vsyncOnoff->setChecked(GL_get_vsync());
      
      switch(GL_get_multisample()){
      case 1:
        mma1->setChecked(true);
        break;
        
      case 2:
        mma2->setChecked(true);
        break;
        
      case 4:
        mma4->setChecked(true);
        break;
        
      case 8:
        mma8->setChecked(true);
        break;
        
      case 16:
        mma16->setChecked(true);
        break;
        
      case 32:
        mma32->setChecked(true);
        break;
      }
      
      
      QString vblankbuttontext = QString("Erase Estimated Vertical Blank (")+QString::number(1000.0/GL_get_estimated_vblank())+" Hz)";
      eraseEstimatedVBlankInterval->setText(vblankbuttontext);
    }

    // CPU
    {
      numCPUs->setValue(MULTICORE_get_num_threads());
    }

    // Edit
    {
      scrollplay_onoff->setChecked(doScrollPlay());
      multiplyscrollbutton->setChecked(doScrollEditLines());
    }

    // MIDI
    {
      const char *name = MIDI_get_input_port();
      input_port_name->setText(name==NULL ? "" : name);
        
      use0x90->setChecked(MIDI_get_use_0x90_for_note_off());
      
      if (MIDI_get_record_accurately())
        record_sequencer_style->setChecked(true);
      else
        record_tracker_style->setChecked(true);
      
      if(MIDI_get_record_velocity())
        record_velocity_on->setChecked(true);
      else
        record_velocity_off->setChecked(true);
    }
    
  }

public slots:

  void on_buttonBox_clicked(QAbstractButton * button){
    if (button->text() == QString("Close")){
      printf("close\n");
      this->hide();
    } else
      RError("Unknown button \"%s\"\n",button->text().toUtf8().constData());
  }

  void on_eraseEstimatedVBlankInterval_clicked(){
    printf("erasing\n");
    GL_erase_estimated_vblank();
  }

  void on_vsyncOnoff_toggled(bool val){
    GL_set_vsync(val);
  }

  void on_mma1_toggled(bool val){
    if (val)
      GL_set_multisample(1);
  }

  void on_mma2_toggled(bool val){
    if (val)
      GL_set_multisample(2);
  }

  void on_mma4_toggled(bool val){
    if (val)
      GL_set_multisample(4);
  }

  void on_mma8_toggled(bool val){
    if (val)
      GL_set_multisample(8);
  }

  void on_mma16_toggled(bool val){
    if (val)
      GL_set_multisample(16);
  }

  void on_mma32_toggled(bool val){
    if (val)
      GL_set_multisample(32);
  }

  // cpu

  void on_numCPUs_valueChanged(int val){
    printf("cpus: %d\n",val);
    MULTICORE_set_num_threads(val);
    
    //set_editor_focus();
    //numCPUs->setFocusPolicy(Qt::NoFocus);
    //on_numCPUs_editingFinished();
  }
  void on_numCPUs_editingFinished(){
    set_editor_focus();

    GL_lock();{
      numCPUs->clearFocus();
    }GL_unlock();

    //numCPUs->setFocusPolicy(Qt::NoFocus);
  }

  // edit

  void on_scrollplay_onoff_toggled(bool val){
    setScrollPlay(val);
  }
  void on_multiplyscrollbutton_toggled(bool val){
    setScrollEditLines(val);
  }

  // MIDI

  void on_set_input_port_clicked(){
    MIDISetInputPort();
  }

  void on_use0x90_toggled(bool val){
    MIDI_set_use_0x90_for_note_off(val);
  }

  void on_record_sequencer_style_toggled(bool val){
    MIDI_set_record_accurately(val);
  }

  void on_record_velocity_on_toggled(bool val){
    MIDI_set_record_velocity(val);
  }
};
}


extern int num_users_of_keyboard;


static void ensure_widget_is_created(void){
}

static Preferences *g_preferences_widget=NULL;

void PREFERENCES_open(void){
  if(g_preferences_widget==NULL){
    g_preferences_widget = new Preferences(NULL);
    //widget->setWindowModality(Qt::ApplicationModal);
  }

  safeShow(g_preferences_widget);

  /*  
  num_users_of_keyboard++;
  safeExec(msgBox);
  num_users_of_keyboard--;
  */
}

void PREFERENCES_open_MIDI(void){
  PREFERENCES_open();
  g_preferences_widget->tabWidget->setCurrentIndex(3);
}

void PREFERENCES_update(void){
  g_preferences_widget->updateWidgets();
}

#include "mQt_preferences_callbacks.cpp"

