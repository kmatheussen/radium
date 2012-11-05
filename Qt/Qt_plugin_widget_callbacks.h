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

#include "Qt_plugin_widget.h"

class Plugin_widget : public QWidget, public Ui::Plugin_widget{
  Q_OBJECT;

public:

  struct Patch *_patch;
  PluginWidget *_plugin_widget;


  Plugin_widget(QWidget *parent, struct Patch *patch)
    : QWidget(parent,"plugin widget")
    , _patch(patch)
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
      if(!strcmp(type->type_name,"FluidSynth") || !strcmp(type->name,"Sample Player"))
        //info.sprintf("%d / %d",type->num_inputs,type->num_outputs);
        plugin_info->hide();
      else{
        info.sprintf("Inputs: %d. Outputs: %d.",type->num_inputs,type->num_outputs);
        plugin_info->setText(info);
      }
    }

    if(strcmp(type->type_name,"Faust") || strcmp(type->name,"Multiband Compressor"))
      delete limiter_bypass_button;

    //instrument->effects_frame->addWidget(PluginWidget_create(NULL, plugin), 0, 3, 2, 1);
    _plugin_widget=PluginWidget_create(NULL, _patch);

    vertical_layout->insertWidget(3,_plugin_widget);
  }

  public slots:

    void on_limiter_bypass_button_toggled(bool val){
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      const SoundPluginType *type = plugin->type;
      int effect_num = PLUGIN_get_effect_num(type, "Limiter Bypass");

      PLUGIN_set_effect_value(plugin, -1, effect_num, val==true ? 1.0 : 0.0, PLUGIN_NONSTORED_TYPE, PLUGIN_STORE_VALUE);

      //update_limiter_sliders();
    }

    void on_interpolation_type_currentIndexChanged( int val){
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      printf("Setting resampler type to %d\n",val);
      SAMPLER_set_resampler_type(plugin, val);
    }

    void on_save_button_pressed(){
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

      num_users_of_keyboard++;
      QString filename = QFileDialog::getSaveFileName(this, "Save Effect configuration", "", "Radium Effect Configuration (*.rec)");
      num_users_of_keyboard--;

      if(filename=="")
        return;

      FILE *file = fopen(filename,"w");

      if(file==NULL){
        QMessageBox msgBox;
        msgBox.setText("Could not save file.");
        msgBox.setStandardButtons(QMessageBox::Ok);
        msgBox.setDefaultButton(QMessageBox::Ok);
        msgBox.exec();
        return;
      }

      hash_t *state = PLUGIN_get_effects_state(plugin);

      HASH_save(state, file);

      fclose(file);
    }

    void on_load_button_pressed(){
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      const SoundPluginType *type = plugin->type;

      num_users_of_keyboard++;
      QString filename = QFileDialog::getOpenFileName(this, "Load Effect configuration", "", "Radium Effect Configuration (*.rec)");
      num_users_of_keyboard--;

      if(filename=="")
        return;

      FILE *file = fopen(filename,"r");
      if(file==NULL){
        QMessageBox msgBox;
        msgBox.setText("Could not open file.");
        msgBox.setStandardButtons(QMessageBox::Ok);
        msgBox.setDefaultButton(QMessageBox::Ok);
        msgBox.exec();
        return;
      }

      hash_t *state = HASH_load(file);
      fclose(file);

      if(state==NULL){
        QMessageBox msgBox;
        msgBox.setText("File does not appear to be a valid effects settings file");
        msgBox.setStandardButtons(QMessageBox::Ok);
        msgBox.setDefaultButton(QMessageBox::Ok);
        msgBox.exec();
        return;
      }

      Undo_Open();{
        for(int i=0;i<type->num_effects+NUM_SYSTEM_EFFECTS;i++)
          Undo_AudioEffect_CurrPos(plugin->patch, i);
      }Undo_Close();

      PLUGIN_create_effects_from_state(plugin, state);
      GFX_update_instrument_widget(plugin->patch);
    }

    void on_reset_button_pressed(){
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      PLUGIN_reset(plugin);
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
      msgBox.exec();
    }
};
