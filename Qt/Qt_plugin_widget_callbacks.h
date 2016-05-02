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
#include "helpers.h"

#include "../audio/Sampler_plugin_proc.h"
#include "../audio/undo_pd_controllers_proc.h"
#include "../audio/Juce_plugins_proc.h"

#include "Qt_plugin_widget_callbacks_proc.h"

#include "Qt_plugin_widget.h"

#include "mQt_pd_plugin_widget_callbacks.h"
#include "mQt_jack_plugin_widget_callbacks.h"


static QString last_fxb_preset_path = "";


class Plugin_widget : public QWidget, public Ui::Plugin_widget{
  Q_OBJECT;

public:
  struct Patch *_patch;
  Pd_Plugin_widget *_pd_plugin_widget;
  Jack_Plugin_widget *_jack_plugin_widget;
  bool _ignore_show_gui_checkbox_stateChanged;

  QMessageBox infoBox;

  PluginWidget *_plugin_widget;

public:

  Plugin_widget(QWidget *parent, struct Patch *patch)
    : QWidget(parent)
    , _patch(patch)
    , _pd_plugin_widget(NULL)
    , _jack_plugin_widget(NULL)
    , _ignore_show_gui_checkbox_stateChanged(false)
    , _plugin_widget(NULL)
    {
      R_ASSERT(_patch!=NULL);
        
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
        QColor c = get_qcolor(BUTTONS_COLOR_NUM);
        c=mix_colors(c.light(70),QColor(98,59,33),0.55);
        c.setAlpha(76);
        QPalette pal(interpolation_type->palette());
        pal.setColor( QPalette::Active, QPalette::Button, c);
        pal.setColor( QPalette::Inactive, QPalette::Button, c);
        pal.setColor( QPalette::Disabled, QPalette::Button, c.light(80));
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

    if(strcmp(type->type_name,"VST"))
      delete fxbp_button;

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
      _plugin_widget=PluginWidget_create(this, _patch);
      vertical_layout->insertWidget(1,_plugin_widget);
    }

    if(plugin->type->show_gui==NULL || plugin->type->hide_gui==NULL)
      show_gui_checkbox->hide();

    if(plugin->type->show_gui==NULL)
      show_gui_button->hide();

    if(plugin->type->show_gui!=NULL && plugin->type->hide_gui!=NULL)
      show_gui_button->hide();

    if (type->get_num_presets==NULL || type->get_num_presets(plugin)==0){
      preset_selector->hide();
      preset_button->hide();
    } else {
      preset_selector->setMinimum(1);
      preset_selector->setMaximum(type->get_num_presets(plugin));
    }

    update_widget();
  }

  // only called when visible
  void calledRegularlyByParent(void){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

    if (plugin != NULL) {
      const SoundPluginType *type = plugin->type;
      
      if(type->gui_is_visible!=NULL){
        bool checkbox = show_gui_checkbox->isChecked();
        bool gui = type->gui_is_visible(plugin);
        if (checkbox==false && gui==true)
          show_gui_checkbox->setChecked(true);
        else if(checkbox==true && gui==false)
          show_gui_checkbox->setChecked(false);
      }
      
      update_preset_widgets();
    }
  }
  
  void prepare_for_deletion(void){
    if (_plugin_widget != NULL)
      _plugin_widget->prepare_for_deletion();
  }
  
  ~Plugin_widget(){
    prepare_for_deletion();
  }
      
  void update_widget() {
    if(_plugin_widget != NULL)
      for(ParamWidget *param_widget : _plugin_widget->_param_widgets)
        param_widget->update_gui_element();
    
    if(_pd_plugin_widget != NULL)
      _pd_plugin_widget->update_gui();

    if(_jack_plugin_widget != NULL)
      _jack_plugin_widget->update_gui();

    update_preset_widgets();
  }

  void update_preset_widgets(){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    const SoundPluginType *type = plugin->type;

    if (type->get_num_presets != NULL){

      int preset_num = type->get_current_preset(plugin);
      if (preset_num != preset_selector->value())
        preset_selector->setValue(preset_num+1);

      const char *preset_name = type->get_preset_name(plugin, preset_num);
      if (strcmp(preset_name, preset_button->text().toUtf8().constData()))
        preset_button->setText(preset_name);

    }
  }
  
private:
  
  void SaveFXBP(bool is_fxb){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

    obtain_keyboard_focus();

    QString filename;
    
    GL_lock();{ // GL_lock is needed when using intel gfx driver to avoid crash caused by opening two opengl contexts simultaneously from two threads.
      filename = QFileDialog::getSaveFileName(
                                              g_mixer_widget,
                                              is_fxb ? "Save VST FXB file" : "Save VST FXP file",
                                              last_fxb_preset_path,
#if FOR_WINDOWS
                                              is_fxb ? "*.fxb ;; All files (*)" : "*.fxp ;; All files (*)",
#else
                                              is_fxb ? "VST FXB (*.fxb) ;; All files (*)" : "VST FXP (*.fxp) ;; All files (*)",
#endif
                                              0,
                                              useNativeFileRequesters() ? (QFileDialog::Option)0 : QFileDialog::DontUseNativeDialog
                                              );
    }GL_unlock();

    release_keyboard_focus();
    
    if(filename=="")
      return;
    
    last_fxb_preset_path = QFileInfo(filename).absoluteDir().path();

    if (is_fxb)
      PLUGINHOST_save_fxb(plugin, STRING_create(filename));
    else
      PLUGINHOST_save_fxp(plugin, STRING_create(filename));
  }

  void LoadFXBP(void){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

    obtain_keyboard_focus();

    QString filename;
    
    GL_lock();{ // GL_lock is needed when using intel gfx driver to avoid crash caused by opening two opengl contexts simultaneously from two threads.
      filename = QFileDialog::getOpenFileName(
                                              g_mixer_widget,
                                              "Load VST FXB or FXP file",
                                              last_fxb_preset_path,
#if FOR_WINDOWS
                                              "*.fxb *.fxp ;; All files (*)",
#else
                                              "VST FXB/FXP files (*.fxb *.fxp) ;; All files (*)",
#endif
                                              0,
                                              useNativeFileRequesters() ? (QFileDialog::Option)0 : QFileDialog::DontUseNativeDialog
                                              );
    }GL_unlock();

    release_keyboard_focus();
    
    if(filename=="")
      return;
    
    last_fxb_preset_path = QFileInfo(filename).absoluteDir().path();
    
    PLUGINHOST_load_fxbp(plugin, STRING_create(filename));
    
    GFX_update_instrument_widget((struct Patch*)_patch);
  }


  
public slots:

  void on_new_pd_controller_button_released() {
    ADD_UNDO(PdControllers_CurrPos(_patch));
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

      PLAYER_lock();{
        PLUGIN_set_effect_value(plugin, -1, effect_num, val==true ? 1.0 : 0.0, PLUGIN_NONSTORED_TYPE, PLUGIN_STORE_VALUE, FX_single);
      }PLAYER_unlock();

      //update_limiter_sliders();
    }

    void on_interpolation_type_currentIndexChanged( int val){
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      printf("Setting resampler type to %d\n",val);
      SAMPLER_set_resampler_type(plugin, val);
    }

  void on_fxbp_button_clicked(){
    vector_t v = {}; // c++ way of zero-initialization without getting missing-field-initializers warning.
    VECTOR_push_back(&v, "Load FXB or FXP file");
    VECTOR_push_back(&v, "Save FXB (standard VST bank format)");
    VECTOR_push_back(&v, "Save FXP (standard VST preset format)");
    
    switch(GFX_Menu(root->song->tracker_windows, NULL, "", &v)){
    case 0: LoadFXBP(); break;
    case 1: SaveFXBP(true); break;
    case 2: SaveFXBP(false); break;
    default: break;
    }
  }
  
    void on_save_button_clicked(){
      InstrumentWidget_save_preset(_patch);      
    }

    void on_load_button_clicked(){
      loadInstrumentPreset(_patch->id, "");
    }

    void on_replace_button_clicked(){
      replaceInstrument(_patch->id, "");
    }
    
    void on_reset_button_clicked(){
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      PLUGIN_reset(plugin);
      GFX_update_instrument_widget(_patch);
    }

    void on_random_button_clicked(){
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      PLUGIN_random(plugin);

      GFX_update_instrument_widget(_patch);
    }

    void on_info_button_clicked(){
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      const SoundPluginType *type = plugin->type;

      if(type->info!=NULL)
        infoBox.setText(type->info);
      else
        infoBox.setText("No information about this plugin."); // This message box should never show.

      safeShowOrExec(&infoBox);
    }

    void on_preset_button_clicked(){
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      const SoundPluginType *type = plugin->type;

      int num_presets = type->get_num_presets(plugin);
      
      vector_t v = {}; // c++ way of zero-initialization without getting missing-field-initializers warning.

      for(int i=0;i<num_presets;i++){
        VECTOR_push_back(&v, talloc_format("%d: %s", i+1, type->get_preset_name(plugin, i)));
      }

      VECTOR_push_back(&v, "--------------");
      VECTOR_push_back(&v, "<set new name>");
      
      int num = GFX_Menu(root->song->tracker_windows, NULL, "", &v);
      if (num == num_presets+1) {
        char *new_name = GFX_GetString(NULL, NULL, "new name: ");
        if (new_name != NULL){
          type->set_preset_name(plugin, type->get_current_preset(plugin), new_name);
          update_widget();
        }
      } else if (num >= 0 && num<num_presets) {
        type->set_current_preset(plugin, num);
        update_widget();
      }
    }
    
    void on_preset_selector_editingFinished(){
      int num = preset_selector->value() - 1;
      printf("num: %d\n",num);
      
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      const SoundPluginType *type = plugin->type;

      type->set_current_preset(plugin, num);
      update_widget();
      
      set_editor_focus();
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

