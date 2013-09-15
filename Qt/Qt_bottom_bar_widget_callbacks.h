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




#include "../common/patch_proc.h"
#include "../common/instruments_proc.h"
#include "../common/undo.h"
#include "../common/OS_visual_input.h"

#include "../audio/SoundPlugin.h"
#include "../audio/SoundPlugin_proc.h"

#include "../Qt/Qt_MyQButton.h"
#include <QTimer>

#include "FocusSniffers.h"

#include "Qt_audio_instrument_widget.h"

#include "Qt_bottom_bar_widget.h"

class Bottom_bar_widget;

Bottom_bar_widget *g_bottom_bar_widget = NULL;
Ui::Audio_instrument_widget *g_system_audio_instrument_widget = NULL;
struct Patch *g_system_out_patch = NULL;

extern bool drunk_velocity;
extern float g_cpu_usage;

class Bottom_bar_widget : public QWidget, public Ui::Bottom_bar_widget {
  Q_OBJECT

  struct Timer : public QTimer{
    Bottom_bar_widget *bottom_bar_widget;
    void timerEvent(QTimerEvent * e){
      QString usage;
      usage.sprintf("CPU: %.1f",g_cpu_usage);
      //printf("Usage: %f\n",g_cpu_usage);
      bottom_bar_widget->cpu_label->setText(usage);
    }
  };

 public:
  bool _initing;
  Timer _timer;

 Bottom_bar_widget(QWidget *parent=NULL)
    : QWidget(parent)
  {
    _initing = true;
    setupUi(this);

    if(g_bottom_bar_widget != NULL)
      RError("g_bottom_bar_widget!=NULL");

    g_bottom_bar_widget = this;

    min_velocity_slider->setValue(4000);
    velocity_slider->setValue(8000);
    update_velocity_sliders();

    // Adjust cpu label width
    {
      QFontMetrics fm(QApplication::font());
      //QRect r =fm.boundingRect(SLIDERPAINTER_get_string(_painter));
      int width = fm.width("CPU: 100.5") + 5;
      cpu_label->setMinimumWidth(width);
    }

    // Adjust velocity slider widths
    {
      QFontMetrics fm(QApplication::font());
      //QRect r =fm.boundingRect(SLIDERPAINTER_get_string(_painter));
      int width = fm.width("Min. Vel: 100%")+20;
      min_velocity_slider->setMinimumWidth(width);
      velocity_slider->setMinimumWidth(width);
    }

    _initing = false;

    _timer.bottom_bar_widget = this;
    _timer.setInterval(1000);
    _timer.start();
  }

  void update_velocity_sliders(){
    if(drunk_velocity==true)
      SLIDERPAINTER_set_string(velocity_slider->_painter, QString("Max Vel: ") + QString::number(velocity_slider->value()*100/10000) + "%");
    else
      SLIDERPAINTER_set_string(velocity_slider->_painter, QString("Vel: ") + QString::number(velocity_slider->value()*100/10000) + "%");

    SLIDERPAINTER_set_string(min_velocity_slider->_painter, QString("Min. Vel: ") + QString::number(min_velocity_slider->value()*100/10000) + "%");

    min_velocity_slider->setMaximum(velocity_slider->value());

    if(drunk_velocity==true)
      velocity_slider->setMinimum(min_velocity_slider->value());
    else
      velocity_slider->setMinimum(0);
  }

public slots:

  void on_system_volume_slider_valueChanged(int val){
    g_system_audio_instrument_widget->input_volume_slider->setValue(val);
    
    SoundPlugin *plugin = (SoundPlugin*)g_system_out_patch->patchdata;
    const SoundPluginType *type = plugin->type;
    
    char buf[64]={0};
    PLUGIN_get_display_value_string(plugin, type->num_effects+EFFNUM_INPUT_VOLUME, buf, 64);
    
    SLIDERPAINTER_set_string(system_volume_slider->_painter, buf);
  }

  void on_octave_up_button_pressed(){
    incKeyAdd(12);
  }

  void on_octave_down_button_pressed(){
    incKeyAdd(-12);
  }

  void on_drunk_velocity_onoff_toggled(bool val){
    drunk_velocity = val;
    update_velocity_sliders();
    if(drunk_velocity==true)
      min_velocity_slider->setEnabled(true);
    else
      min_velocity_slider->setEnabled(false);

    velocity_slider->update();
  }

  void on_min_velocity_slider_valueChanged(int val){
    if(_initing==true)
      return;
    root->min_standardvel = val*MAX_VELOCITY / 10000;
    update_velocity_sliders();
  }

  void on_velocity_slider_valueChanged(int val){
    if(_initing==true)
      return;
    root->standardvel = val*MAX_VELOCITY / 10000;
    update_velocity_sliders();
  }

  void on_undo_button_pressed(){
    Undo();
  }

  void on_redo_button_pressed(){
    Redo();
  }

  void on_midi_input_onoff_toggled(bool val){
    if(val!=root->editonoff)
      switchEditOnOff();
  }

  void on_scrollplay_onoff_toggled(bool val){
    root->scrollplayonoff = val;
  }
};

extern "C"{
  void GFX_OS_set_system_volume_peak_pointers(float *pointers, int num_channels){
    SLIDERPAINTER_set_peak_value_pointers(g_bottom_bar_widget->system_volume_slider->_painter, num_channels, pointers);
  }

  void GFX_OS_UpdateKeyOctave(void){
    g_bottom_bar_widget->octave_label->setText(QString("Octave: ")+QString::number(root->keyoct/12,16));
  }

  void OS_GFX_NumUndosHaveChanged(int num_undos, bool redos_are_available, bool has_unsaved_undos){
    g_bottom_bar_widget->num_undos_label->setText(QString::number(num_undos));
    g_bottom_bar_widget->unsaved_undos->setText(has_unsaved_undos?"*":" ");
    g_bottom_bar_widget->undo_button->setEnabled(num_undos>0);
    g_bottom_bar_widget->redo_button->setEnabled(redos_are_available);
  }

  void OS_GFX_SetVolume(int value){
    g_bottom_bar_widget->system_volume_slider->setValue(value);
  }
}

void BottomBar_set_system_audio_instrument_widget_and_patch(Ui::Audio_instrument_widget *system_audio_instrument_widget, struct Patch *system_out_patch){
  g_system_audio_instrument_widget = system_audio_instrument_widget;
  g_system_out_patch = system_out_patch;

  SoundPlugin *plugin = (SoundPlugin*)g_system_out_patch->patchdata;
  const SoundPluginType *type = plugin->type;

  { // for undo
    g_bottom_bar_widget->system_volume_slider->_patch = g_system_out_patch;
    g_bottom_bar_widget->system_volume_slider->_effect_num = EFFNUM_INPUT_VOLUME;
    SLIDERPAINTER_set_num_channels(g_bottom_bar_widget->system_volume_slider->_painter, type->num_inputs);
  }
}
    
