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
#include "../common/OS_Player_proc.h"
#include "../common/sequencer_proc.h"

#include "../mixergui/QM_MixerWidget.h"

#include "../api/api_gui_proc.h"
#include "../api/api_proc.h"

//namespace{
#include "Qt_MyQCheckBox.h"
//}

#include "Qt_song_properties.h"

bool g_is_replacing_main_pipe = false;

namespace{
  
class song_properties : public RememberGeometryQDialog, public Ui::Song_properties {
  Q_OBJECT

 public:
  bool _initing;

 song_properties(QWidget *parent)
   : RememberGeometryQDialog(parent, radium::NOT_MODAL)
  {
    _initing = true;

    setupUi(this);

    update_widgets(root->song);

    adjustSizeAndMoveWindowToCentre(this);
    
    _initing = false;
  }

  void update_widgets(struct Song *song){
    R_ASSERT(_initing==true);
    
    set_linear_accelerando_and_ritardando(song->linear_accelerando, song->linear_ritardando);
    send_swing_to_plugins->setChecked(song->plugins_should_receive_swing_tempo);
    use_swinging_beats_in_sequencer->setChecked(song->use_swinging_beats_in_sequencer);
    display_swinging_beats_in_seqblocks_in_sequencer->setChecked(song->display_swinging_beats_in_seqblocks_in_sequencer);
    mute_automation->setChecked(song->mute_editor_automation_when_track_is_muted);
    swing_along->setChecked(song->editor_should_swing_along);

    mixer_comments_visible->setChecked(mixerStripCommentsVisible());
    two_channels_in_main_pipe->setChecked(song->num_channels_in_main_pipe==2);
    include_pan_and_dry_in_wet->setChecked(includePanAndDryInWetSignal());
    
    embed_samples->setChecked(g_curr_song_contains_embedded_samples);
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

  void set_num_channels_in_main_pipe(int num_channels){
    R_ASSERT_RETURN_IF_FALSE(num_channels==2 || num_channels==8);
    if (num_channels == root->song->num_channels_in_main_pipe)
      return;

    struct SoundPlugin *plugin = get_main_pipe();
    struct Patch *patch = plugin->patch;

    const_char *description = getAudioInstrumentDescription(toBase64(""), toBase64("Pipe"), toBase64(num_channels==2 ? "Pipe" : "Pipe8"));

    g_is_replacing_main_pipe = true;
    {
      requestReplaceInstrument(patch->id, description, API_get_gui_from_existing_widget(this));
    }
    // g_is_replacing_main_pipe is set to false in internalReplaceMainPipe in api/api_instruments.c
  }

public slots:

  void on_acc_linear_toggled(bool val){
    if (_initing==true)
      return;
    
    {
      SCOPED_PLAYER_LOCK_IF_PLAYING();
      root->song->linear_accelerando = val;
    }
    
    TIME_global_tempos_have_changed();

    root->song->tracker_windows->must_redraw_editor = true;
  }
  
  void on_rit_linear_toggled(bool val){
    if (_initing==true)
      return;

    {
      SCOPED_PLAYER_LOCK_IF_PLAYING();
      root->song->linear_ritardando = val;
    }
    
    TIME_global_tempos_have_changed();
    
    root->song->tracker_windows->must_redraw_editor = true;
  }

  void on_two_channels_in_main_pipe_toggled(bool val){
    if (_initing==true)
      return;

    set_num_channels_in_main_pipe(val==true ? 2 : 8);
  }

  void on_mixer_comments_visible_toggled(bool val){
    if (_initing==true)
      return;

    setMixerStripCommentsVisible(val);
  }
  
  void on_include_pan_and_dry_in_wet_toggled(bool val){
    if (_initing==true)
      return;

    setIncludePanAndDryInWetSignal(val);
  }
  
  void on_mute_automation_toggled(bool val){
    if (_initing==true)
      return;

    {
      SCOPED_PLAYER_LOCK_IF_PLAYING();
      root->song->mute_editor_automation_when_track_is_muted = val;
    }
  }
  
  void on_send_swing_to_plugins_toggled(bool val){
    if (_initing==true)
      return;
    
    root->song->plugins_should_receive_swing_tempo = val;
    TIME_global_tempos_have_changed();
  }

  void on_use_swinging_beats_in_sequencer_toggled(bool val){
    if (_initing==true)
      return;
    
    root->song->use_swinging_beats_in_sequencer = val;
    SEQUENCER_update(SEQUPDATE_TIME|SEQUPDATE_TIMELINE);
  }

  void on_display_swinging_beats_in_seqblocks_in_sequencer_toggled(bool val){
    if (_initing==true)
      return;
    
    root->song->display_swinging_beats_in_seqblocks_in_sequencer = val;
    SEQUENCER_update(SEQUPDATE_TIME);
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

  void on_embed_samples_toggled(bool val){
    g_curr_song_contains_embedded_samples = val;
  }
};

}

