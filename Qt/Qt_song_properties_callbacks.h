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

#include "../api/api_proc.h"

//namespace{
#include "Qt_MyQCheckBox.h"
//}

#include "helpers.h"

#include "Qt_song_properties.h"

namespace{
  
class song_properties : public RememberGeometryQDialog, public Ui::Song_properties {
  Q_OBJECT

 public:
  bool _initing;

 song_properties(QWidget *parent=NULL)
   : RememberGeometryQDialog(parent, radium::NOT_MODAL)
  {
    _initing = true;

    setupUi(this);

    update_widgets(root->song);
    
    _initing = false;
  }

  void update_widgets(struct Song *song){
    R_ASSERT(_initing==true);
    
    set_linear_accelerando_and_ritardando(ATOMIC_GET(song->linear_accelerando), ATOMIC_GET(song->linear_ritardando));
    send_swing_to_plugins->setChecked(song->plugins_should_receive_swing_tempo);
    mixer_comments_visible->setChecked(mixerStripCommentsVisible());
    swing_along->setChecked(song->editor_should_swing_along);
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

  void on_mixer_comments_visible_toggled(bool val){
    if (_initing==true)
      return;

    setMixerStripCommentsVisible(val);
  }
  
  void on_send_swing_to_plugins_toggled(bool val){
    if (_initing==true)
      return;
    
    root->song->plugins_should_receive_swing_tempo = val;
    TIME_global_tempos_have_changed();
  }

  void on_swing_along_toggled(bool val){
    if (_initing==true)
      return;

    root->song->editor_should_swing_along = val;
    root->song->tracker_windows->must_redraw_editor = true;
  }
  
  void on_buttonBox_clicked(QAbstractButton * button){
    this->hide();
  }

};

}

