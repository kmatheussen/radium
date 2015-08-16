/* Copyright 2012 Kjetil S. Matheussen

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


#if USE_QT4
//#include <QCleanlooksStyle>
//#include <QOxygenStyle>
#include <QPlastiqueStyle>
#endif

#include "../common/patch_proc.h"
#include "../common/instruments_proc.h"

#include "../audio/Mixer_proc.h"

#include "Qt_no_instrument_widget.h"

extern "C" void set_font_thickness(int val);
extern QString default_style_name;

// TODO: Remove all of this. Not used.

class No_instrument_widget : public QWidget, public Ui::No_instrument_widget{
  Q_OBJECT

 public:
  bool initing;

 No_instrument_widget(QWidget *parent=NULL)
    : QWidget(parent)
  {
    initing = true;
    setupUi(this);
    use_system_colors->setChecked(!SETTINGS_read_bool("override_default_qt_colors",true));
    use_system_style->setChecked(!SETTINGS_read_bool("override_default_qt_style",true));
    initing = false;
  }

public slots:
  void on_create_midi_instrument_clicked()
  {
    printf("Got it midi\n");
    if(initing==true)
      return;

    static int midipatchnum=0;
    char patchname[200];
    sprintf(patchname,"MIDI %d",++midipatchnum);

    struct Patch *patch = NewPatchCurrPos(MIDI_INSTRUMENT_TYPE, NULL, patchname);
    GFX_PP_Update(patch);

#if 0
    add_midi_instrument(patch);
    MIDI_instrument_widget *instrument = get_midi_instrument_widget(patch);
    instruments_widget->tabs->showPage(instrument);
#endif
  }

  void on_create_audio_instrument_clicked()
  {
    printf("Got it audio\n");
    if(initing==true)
      return;

    add_new_audio_instrument_widget(NULL,-100000,-100000,true,NULL,MIXER_get_buses());
  }

  void on_create_sample_instrument_clicked()
  {
    printf("Got it sample\n");
    if(initing==true)
      return;

    add_new_audio_instrument_widget(PR_get_plugin_type_by_name(NULL, "Sample Player","Sample Player"),-100000,-100000,true,NULL,MIXER_get_buses());
  }

  void on_create_fluidsynth_clicked()
  {
    printf("Got it fluidsynth\n");
    if(initing==true)
      return;

    add_new_audio_instrument_widget(PR_get_plugin_type_by_name(NULL, "FluidSynth","FluidSynth"),-100000,-100000,true,NULL,MIXER_get_buses());
  }

  void on_use_system_colors_stateChanged(int state){
    if(initing==true)
      return;

    bool override;

    if(state==Qt::Unchecked)
      override = true;
    else if(state==Qt::Checked)
      override = false;
    else
      return;

    SETTINGS_write_bool("override_default_qt_colors",override);

    setApplicationColors(qapplication);
  }

  void on_use_system_style_stateChanged(int state){
    if(initing==true)
      return;

    bool override;

    if(state==Qt::Unchecked)
      override = true;
    else if(state==Qt::Checked)
      override = false;
    else
      return;

    SETTINGS_write_bool("override_default_qt_style",override);

    printf("default: %s\n",default_style_name.toUtf8().constData());

    if(override==true){
      QApplication::setStyle("plastique");
      QApplication::setEffectEnabled(Qt::UI_AnimateMenu,true);
      QApplication::setEffectEnabled(Qt::UI_AnimateCombo,true);
    }else
      QApplication::setStyle(default_style_name);
  }

  void on_midi_input_stateChanged(int state){
    if(initing==true)
      return;

    if(state==Qt::Unchecked)
      root->editonoff = false;
    else if(state==Qt::Checked)
      root->editonoff = true;
    else
      return;

    struct Tracker_Windows *window = root->song->tracker_windows;
    char temp[1000];
    sprintf(temp,"Midi Input %s",root->editonoff?"On":"Off");
    GFX_SetStatusBar(window,temp);
  }

  /*
  void on_font_thickness_slider_valueChanged( int val){
    set_font_thickness(val);
  }
  */
};
