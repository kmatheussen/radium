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
//#include <sndfile.h>
#include <unistd.h>

#include <FocusSniffers.h>

#include "../common/nsmtracker.h"
#include "../common/hashmap_proc.h"
#include "../common/OS_string_proc.h"

//namespace{
#include "Qt_MyQCheckBox.h"
//}

#include "helpers.h"

#include "Qt_song_properties.h"


class song_properties : public RememberGeometryQDialog, public Ui::Song_properties {
  Q_OBJECT

 public:
  bool _initing;

 song_properties(QWidget *parent=NULL)
    : RememberGeometryQDialog(parent)
  {
    _initing = true;

    setupUi(this);
    set_linear_accelerando_and_ritardando(ATOMIC_GET(root->song->linear_accelerando), ATOMIC_GET(root->song->linear_ritardando));
    
    _initing = false;
  }

  void set_linear_accelerando_and_ritardando(bool linear_accelerando, bool linear_ritardando){
    if (linear_accelerando)
      acc_linear->setChecked(true);
    else
      acc_faster_and_faster->setChecked(true);

    if (linear_ritardando)
      rit_linear->setChecked(true);
    else
      rit_slower_and_slower->setChecked(true);
  }

public slots:

  void on_acc_linear_toggled(bool val){
    if (_initing==true)
      return;
    
    ATOMIC_SET(root->song->linear_accelerando, val);
    TIME_global_tempos_have_changed();

    root->song->tracker_windows->must_redraw_editor = true;
  }
  
  void on_rit_linear_toggled(bool val){
    if (_initing==true)
      return;
    
    ATOMIC_SET(root->song->linear_ritardando, val);
    TIME_global_tempos_have_changed();
    
    root->song->tracker_windows->must_redraw_editor = true;
  }
  
  void on_buttonBox_clicked(QAbstractButton * button){
    this->hide();
  }

};

