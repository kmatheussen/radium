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
#include <FocusSniffers.h>
#include "helpers.h"

#include "../common/nsmtracker.h"
#include "../common/hashmap_proc.h"
#include "../common/OS_string_proc.h"
#include "../common/OS_settings_proc.h"
#include "../common/settings_proc.h"
#include "../OpenGL/Widget_proc.h"

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

    _initing = false;
  }

  void updateWidgets(){
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
};
}


extern int num_users_of_keyboard;


static void ensure_widget_is_created(void){
}

void PREFERENCES_open(void){
  static Preferences *widget=NULL;
  if(widget==NULL){
    widget = new Preferences(NULL);
    //widget->setWindowModality(Qt::ApplicationModal);
  }

  safeShow(widget);

  /*  
  num_users_of_keyboard++;
  safeExec(msgBox);
  num_users_of_keyboard--;
  */
}


#include "mQt_preferences_callbacks.cpp"

