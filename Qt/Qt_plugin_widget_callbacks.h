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


#include <QMessageBox>
#include <QFileDialog>

#include "Qt_PluginWidget.h"

#include "../audio/Sampler_plugin_proc.h"
#include "../audio/undo_pd_controllers_proc.h"

#include "Qt_plugin_widget_callbacks_proc.h"

#include "Qt_plugin_widget.h"

#include "mQt_pd_plugin_widget_callbacks.h"
#include "mQt_jack_plugin_widget_callbacks.h"


class Plugin_widget : public QWidget, public Ui::Plugin_widget{
  Q_OBJECT;

public:
  struct Patch *_patch;
  Pd_Plugin_widget *_pd_plugin_widget;
  Jack_Plugin_widget *_jack_plugin_widget;
  bool _ignore_show_gui_checkbox_stateChanged;

private:
  PluginWidget *_plugin_widget;

public:

  Plugin_widget(QWidget *parent, struct Patch *patch)
    : QWidget(parent,"plugin widget")
    , _patch(patch)
    , _pd_plugin_widget(NULL)
    , _jack_plugin_widget(NULL)
    , _ignore_show_gui_checkbox_stateChanged(false)
    , _plugin_widget(NULL)
    {
    setupUi(this);

    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    const SoundPluginType *type = plugin->type;

    if(QString("Sample Player") != plugin->type->type_name)
      interpolation_type->hide();
    else{
      interpolation_type->setToolTip("Selects resampling interpolation type for this instrument.\n"
                                     "None   = No interpolation. Gives a metallic sound.\n"
                                     "Linear = Quick interpolation. Has a much less metallic sound than no interpolation.\n"
                                     "Cubic  = Also quick, but has a slightly less metallic sound than linear interpolation.\n"
                                     "Sinc1  = Excellent sound: \"The fastest bandlimited interpolator, providing a Signal-to-Noise Ratio (SNR) of 97dB and a bandwidth of 80%.\" (libsamplerate)\n"
                                     "Sinc2  = Excellent sound: \"The highest quality sinc based converter, providing a worst case SNR of 97dB at a bandwidth of 97%.\" (libsamplerate)");
      interpolation_type->setCurrentIndex(SAMPLER_get_resampler_type(plugin));

      {
        EditorWidget *editor = static_cast<EditorWidget*>(root->song->tracker_windows->os_visual.widget);
        QColor c = editor->colors[13];
        c=mix_colors(c.light(70),QColor(98,59,33),0.55);
        c.setAlpha(76);
        QPalette pal(interpolation_type->palette());
        pal.setColor( QPalette::Active, QColorGroup::Button, c);
        pal.setColor( QPalette::Inactive, QColorGroup::Button, c);
        pal.setColor( QPalette::Disabled, QColorGroup::Button, c.light(80));
        interpolation_type->setPalette(pal);
      }
    }

    {
      QString info;
      
      if(!strcmp(type->type_name,type->name))
        info = type->type_name;
      else
        info = QString(type->type_name) + ": " + type->name;
      
      //info = info.sprintf(". Inputs: %d. Output: %d",type->num_inputs,type->num_outputs);
      
      info_button->setText(info);

      if(type->info==NULL)
        info_button->setEnabled(false);
    }

    {
      QString info;
      if(false){//!strcmp(type->type_name,"FluidSynth") || !strcmp(type->name,"Sample Player")){
        //info.sprintf("%d / %d",type->num_inputs,type->num_outputs);
        plugin_info->hide();
      }else{
        info.sprintf("Ins: %d. Outs: %d.",type->num_inputs,type->num_outputs);
        plugin_info->setText(info);
      }
    }

    if(strcmp(type->type_name,"Faust") || strcmp(type->name,"Multiband Compressor"))
      delete limiter_bypass_button;

    if(strcmp(type->type_name,"Sample Player") && strcmp(type->type_name,"FluidSynth"))
      delete sample_name_label;

    //instrument->effects_frame->addWidget(PluginWidget_create(NULL, plugin), 0, 3, 2, 1);

    //   Pd:
    if(!strcmp(plugin->type->type_name, "Pd")) {
      _pd_plugin_widget = new Pd_Plugin_widget(this,_patch);
      vertical_layout->insertWidget(1,_pd_plugin_widget);

      // Jack:
    }else if(!strcmp(plugin->type->type_name, "Jack")) {
      new_pd_controller_button->hide();
      load_button->hide();
      save_button->hide();
      reset_button->hide();
      random_button->hide();
      _jack_plugin_widget = new Jack_Plugin_widget(this,_patch);
      vertical_layout->insertWidget(1,_jack_plugin_widget);

      // Others:
    } else {
      new_pd_controller_button->hide();
      _plugin_widget=PluginWidget_create(NULL, _patch);
      vertical_layout->insertWidget(1,_plugin_widget);
    }

    if(plugin->type->show_gui==NULL || plugin->type->hide_gui==NULL)
      show_gui_checkbox->hide();

    if(plugin->type->show_gui==NULL)
      show_gui_button->hide();

    if(plugin->type->show_gui!=NULL && plugin->type->hide_gui!=NULL)
      show_gui_button->hide();
  }

  void update_widget() {
    if(_plugin_widget != NULL)
      for(unsigned int i=0;i<_plugin_widget->_param_widgets.size();i++){
        ParamWidget *param_widget = _plugin_widget->_param_widgets.at(i);
        param_widget->update_gui_element();
      }

    if(_pd_plugin_widget != NULL)
      _pd_plugin_widget->update_gui();

    if(_jack_plugin_widget != NULL)
      _jack_plugin_widget->update_gui();
  }

  public slots:

  void on_new_pd_controller_button_released() {
    Undo_PdControllers_CurrPos(_patch);
    _pd_plugin_widget->new_controller();  
  }

  void on_show_gui_checkbox_stateChanged(int val){
    if (_ignore_show_gui_checkbox_stateChanged==false) {
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      if (val==Qt::Checked){
        plugin->type->show_gui(plugin);
      }else if(val==Qt::Unchecked){
        plugin->type->hide_gui(plugin);
      }
    }
  }

  void on_show_gui_button_released(){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    plugin->type->show_gui(plugin);
  }

    void on_limiter_bypass_button_toggled(bool val){
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      int effect_num = PLUGIN_get_effect_num(plugin, "Limiter Bypass");

      PLUGIN_set_effect_value(plugin, -1, effect_num, val==true ? 1.0 : 0.0, PLUGIN_NONSTORED_TYPE, PLUGIN_STORE_VALUE, FX_single);

      //update_limiter_sliders();
    }

    void on_interpolation_type_currentIndexChanged( int val){
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      printf("Setting resampler type to %d\n",val);
      SAMPLER_set_resampler_type(plugin, val);
    }

    void on_save_button_pressed(){
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

      QString filename;

      {
        GL_ScopedLock lock;  // GL_lock is needed when using intel gfx driver to avoid crash caused by opening two opengl contexts simultaneously from two threads.
        filename = QFileDialog::getSaveFileName(this, "Save Effect configuration", "", "Radium Effect Configuration (*.rec)");
      }

      if(filename=="")
        return;

      disk_t *file = DISK_open_for_writing(filename);

      if(file==NULL){
        QMessageBox msgBox;
        msgBox.setText("Could not save file.");
        msgBox.setStandardButtons(QMessageBox::Ok);
        msgBox.setDefaultButton(QMessageBox::Ok);
        safeExec(msgBox);
        return;
      }

      hash_t *state = PLUGIN_get_effects_state(plugin);

      HASH_save(state, file);

      DISK_close_and_delete(file);
    }

    void on_load_button_pressed(){
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      const SoundPluginType *type = plugin->type;

      QString filename;
      
      {
        GL_ScopedLock lock;  // GL_lock is needed when using intel gfx driver to avoid crash caused by opening two opengl contexts simultaneously from two threads.
        filename = QFileDialog::getOpenFileName(this, "Load Effect configuration", "", "Radium Effect Configuration (*.rec)");
      }

      num_users_of_keyboard--;

      if(filename=="")
        return;

      disk_t *file = DISK_open_for_reading(filename);
      if(file==NULL){
        QMessageBox msgBox;
        msgBox.setText("Could not open file.");
        msgBox.setStandardButtons(QMessageBox::Ok);
        msgBox.setDefaultButton(QMessageBox::Ok);
        safeExec(msgBox);
        return;
      }

      hash_t *state = HASH_load(file);
      DISK_close_and_delete(file);

      if(state==NULL){
        QMessageBox msgBox;
        msgBox.setText("File does not appear to be a valid effects settings file");
        msgBox.setStandardButtons(QMessageBox::Ok);
        msgBox.setDefaultButton(QMessageBox::Ok);
        safeExec(msgBox);
        return;
      }

      Undo_Open();{
        for(int i=0;i<type->num_effects+NUM_SYSTEM_EFFECTS;i++)
          Undo_AudioEffect_CurrPos(plugin->patch, i);
      }Undo_Close();

      PLUGIN_set_effects_from_state(plugin, state);
      GFX_update_instrument_widget(plugin->patch);
    }

    void on_reset_button_pressed(){
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      PLUGIN_reset(plugin);
      GFX_update_instrument_widget(plugin->patch);
    }

    void on_random_button_pressed(){
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      PLUGIN_random(plugin);
      GFX_update_instrument_widget(plugin->patch);
    }

    void on_info_button_pressed(){
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      const SoundPluginType *type = plugin->type;

      QMessageBox msgBox;
      if(type->info!=NULL)
        msgBox.setText(type->info);
      else
        msgBox.setText("No information about this plugin."); // This message box should never show.
      safeExec(msgBox);
    }
};

void PLUGINWIDGET_gui_is_hidden(void *w){
  Plugin_widget *plugin_widget = static_cast<Plugin_widget*>(w);
  plugin_widget->_ignore_show_gui_checkbox_stateChanged = true; {
    plugin_widget->show_gui_checkbox->setChecked(false);
  } plugin_widget->_ignore_show_gui_checkbox_stateChanged = false;
}

void PLUGINWIDGET_gui_is_visible(void *w){
  Plugin_widget *plugin_widget = static_cast<Plugin_widget*>(w);
  plugin_widget->_ignore_show_gui_checkbox_stateChanged = true; {
    plugin_widget->show_gui_checkbox->setChecked(true);
  } plugin_widget->_ignore_show_gui_checkbox_stateChanged = false;
}

