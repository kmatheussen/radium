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

#include "Qt_MyQCheckBox.h"
#include "../Qt/Qt_MyQSpinBox.h"
#include "../Qt/Qt_MyQLabel.h"


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

  // weird workaround to prevent closing the window when pressing the return key when editing num bus channels.
  void keyPressEvent(QKeyEvent *event) override {
    printf("Gakk!\n");
    set_editor_focus();
    GL_lock();{
      num_bus_ch->clearFocus();
    }GL_unlock();
    event->ignore();
  }
  
  void update_widgets(struct Song *song){
    R_ASSERT(_initing==true);
    
    set_linear_accelerando_and_ritardando(song->linear_accelerando, song->linear_ritardando);
    send_swing_to_plugins->setChecked(song->plugins_should_receive_swing_tempo);
    use_swinging_beats_in_sequencer->setChecked(song->use_swinging_beats_in_sequencer);
    display_swinging_beats_in_seqblocks_in_sequencer->setChecked(song->display_swinging_beats_in_seqblocks_in_sequencer);
    mute_automation->setChecked(song->mute_editor_automation_when_track_is_muted);
    swing_along->setChecked(song->editor_should_swing_along);

    switch(song->glissando_behavior) {
      case 0: glissando0->setChecked(true); break;
      case 1: glissando1->setChecked(true); break;
      case 2: glissando2->setChecked(true); break;
      default: R_ASSERT_NON_RELEASE(false);
    }
    
    mixer_comments_visible->setChecked(mixerStripCommentsVisible());
    
    //two_channel_buses->setChecked(song->default_num_bus_channels==2);
    num_bus_ch->setValue(song->default_num_bus_channels);
    
    include_pan_and_dry_in_wet->setChecked(includePanAndDryInWetSignal());
    mute_system_buses_when_bypassed->setChecked(muteSystemBusesWhenBypassed());

    mute_plugin_MIDI->setChecked(song->RT_mute_plugin_MIDI_when_muted);
    send_plugin_MIDI_through->setChecked(song->RT_send_plugin_MIDI_through_when_bypassed);
    implicitly_mute_MIDI->setChecked(song->RT_implicitly_mute_plugin_MIDI);
    
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

#if 0  
  void set_num_bus_channels(int num_channels){
    R_ASSERT_RETURN_IF_FALSE(num_channels==2 || num_channels==8);
    if (num_channels == root->song->default_num_bus_channels)
      return;

    int old_channels = root->song->default_num_bus_channels;
    int new_channels = num_channels;
    
    auto setit = [this](int num_channels){

      int64_t guinum = API_get_gui_from_existing_widget(this);
      
      // main pipe
      /*
      if(0){
        struct SoundPlugin *plugin = get_main_pipe();
        struct Patch *patch = plugin->patch;
        
        g_is_replacing_main_pipe = true;
        {
          requestReplaceInstrument(patch->id, description, guinum);
        }
        // g_is_replacing_main_pipe is set to false in internalReplaceMainPipe in api/api_instruments.c
      }
      */
      
      // other buses
      {
        //kan heller bare iterere seqtracks.
        
        std::vector<SoundProducer*> sp_all = MIXER_get_all_SoundProducers().to_std_vector(); // Make a copy because we modify the content.
        
        for (SoundProducer *sp : sp_all) {
          SoundPlugin *plugin = SP_get_plugin(sp);

          if (!strcmp(plugin->type->type_name, "Bus")) {

            int bus_num = PLUGIN_get_bus_num(plugin->type);
                             
            const char *plugin_name = toBase64(PLUGIN_get_bus_plugin_name(bus_num, num_channels));
            const char *description = getAudioInstrumentDescription(toBase64(""), toBase64("Bus"), plugin_name);
      
            struct Patch *patch = plugin->patch;
            requestReplaceInstrument(patch->id, description, guinum);
          }
        }
      }

      root->song->default_num_bus_channels = num_channels;

      {
        R_ASSERT(_initing==false);
        _initing=true;
        two_channel_buses->setChecked(root->song->default_num_bus_channels==2);
        _initing=false;
      }
    };
    
    UNDO_functions(
                   "Set number of bus channels",
                   [setit, new_channels](){
                     setit(new_channels);
                   },
                   [setit, old_channels](){
                     setit(old_channels);
                   });

  }
#endif

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

  void on_num_bus_ch_valueChanged(int val){
    if (_initing==true)
      return;
    
    printf("Setting to %d\n", val);
    root->song->default_num_bus_channels = val;
  }
  
  void on_num_bus_ch_editingFinished(int val){
    set_editor_focus();
    GL_lock();{
      num_bus_ch->clearFocus();
    }GL_unlock();
  }
  /*
  void on_two_channel_buses_toggled(bool val){
    if (_initing==true)
      return;

    set_num_bus_channels(val==true ? 2 : 8);
  }
  */
 
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
  
  void on_mute_system_buses_when_bypassed_toggled(bool val){
    if (_initing==true)
      return;

    setMuteSystemBusesWhenBypassed(val);
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
  
  void on_glissando0_toggled(bool val){
    if (_initing==true || !val)
      return;

    radium::PlayerLock lock;
    root->song->glissando_behavior = 0;
  }
  
  void on_glissando1_toggled(bool val){
    if (_initing==true || !val)
      return;

    radium::PlayerLock lock;
    root->song->glissando_behavior = 1;
  }
  
  void on_glissando2_toggled(bool val){
    if (_initing==true || !val)
      return;

    radium::PlayerLock lock;
    root->song->glissando_behavior = 2;
  }
  
  void on_buttonBox_clicked(QAbstractButton * button){
    this->hide();
  }
  
  void on_mute_plugin_MIDI_toggled(bool val){
    if (_initing==true)
      return;

    {
      radium::PlayerLock lock;
      root->song->RT_mute_plugin_MIDI_when_muted = val;
    }
  }
  
  void on_send_plugin_MIDI_through_toggled(bool val){
    if (_initing==true)
      return;
    
    {
      radium::PlayerLock lock;
      root->song->RT_send_plugin_MIDI_through_when_bypassed = val;
    }

    SP_call_me_after_solo_has_changed();
  }
  
  void on_implicitly_mute_MIDI_toggled(bool val){
    if (_initing==true)
      return;
    
    {
      radium::PlayerLock lock;
      root->song->RT_implicitly_mute_plugin_MIDI = val;      
    }
    
    SP_call_me_after_solo_has_changed();
  }
  
  void on_embed_samples_toggled(bool val){
    g_curr_song_contains_embedded_samples = val;
  }
};

}

