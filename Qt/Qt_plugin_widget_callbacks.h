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

#define SHOW_SOLO_BUTTON 0

#include <QFileDialog>

#include "../common/disk.h"

#include "Qt_PluginWidget.h"
#include "helpers.h"

#include "../audio/Sampler_plugin_proc.h"
#include "../audio/undo_pd_controllers_proc.h"
#include "../audio/Juce_plugins_proc.h"
#include "../audio/CpuUsage.hpp"
#include "../audio/Presets_proc.h"

#include "Qt_plugin_widget_callbacks_proc.h"

#include "Qt_plugin_widget.h"

#include "mQt_auto_suspend_menu_callbacks.h"


#include "mQt_pd_plugin_widget_callbacks.h"
#ifdef WITH_FAUST_DEV
#include "mQt_faust_plugin_widget_callbacks.h"
#endif
#include "mQt_jack_plugin_widget_callbacks.h"

static QString last_fxb_preset_path = "";


namespace{
  
class Plugin_widget : public QWidget, public Ui::Plugin_widget{
  Q_OBJECT;

public:
  radium::GcHolder<struct Patch> _patch;
  Pd_Plugin_widget *_pd_plugin_widget;
  Jack_Plugin_widget *_jack_plugin_widget;
#ifdef WITH_FAUST_DEV
  Faust_Plugin_widget *_faust_plugin_widget;
#endif
  bool _ignore_checkbox_stateChanged;

  PluginWidget *_plugin_widget;

  SizeType _size_type;
  //int _last_height;

  Auto_Suspend_Menu _auto_suspend_menu;

  bool _is_initing;
  
public:

  Plugin_widget(QWidget *parent, struct Patch *patch)
    : QWidget(parent)
    , _patch(patch)
    , _pd_plugin_widget(NULL)
    , _jack_plugin_widget(NULL)
#ifdef WITH_FAUST_DEV
    , _faust_plugin_widget(NULL)
#endif
    , _ignore_checkbox_stateChanged(false)
    , _plugin_widget(NULL)
    , _size_type(SIZETYPE_NORMAL)
      //, _last_height(10)
    , _auto_suspend_menu(this, patch)
  {
    R_ASSERT(patch!=NULL);
    
    _is_initing = true;
    
    setupUi(this);
    
    set_cpu_usage_font_and_width(plugin_info, true, true);
    
    ab_reset->hide(); // The exact same button is placed only a few hundred pixels to the left anyway.
      
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    const SoundPluginType *type = plugin->type;

#if SHOW_SOLO_BUTTON
    solo_checkbox->_patch = _patch;
    solo_checkbox->_effect_num = type->num_effects + EFFNUM_SOLO_ONOFF;
    solo_checkbox->_add_undo_when_clicked = false;
#else
    solo_checkbox->hide();
#endif
    
    bool will_always_autosuspend = type->will_always_autosuspend;

    if(will_always_autosuspend || type->will_never_autosuspend)
      auto_bypass_menu_button->hide();
    
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

      info = info + "  " + small_number(type->num_inputs) + " \u208b " + small_number(type->num_outputs);
    
      //info = info.sprintf(". Inputs: %d. Output: %d",type->num_inputs,type->num_outputs);
      
      info_button->setText(info);

      //if(type->info==NULL)
      //info_button->setEnabled(false);
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
      _pd_plugin_widget = new Pd_Plugin_widget(this,_patch.data());
      vertical_layout->insertWidget(1,_pd_plugin_widget);
      faust_load_button->hide();
      faust_save_button->hide();
      faust_compilation_status->hide();
      faust_revert_button->hide();
      faust_show_button->hide();
      faust_options_button->hide();
      
      // Jack:
    }else if(!strcmp(plugin->type->type_name, "Jack")) {
      new_pd_controller_button->hide();
      faust_load_button->hide();
      faust_save_button->hide();
      faust_compilation_status->hide();
      faust_revert_button->hide();
      faust_show_button->hide();
      faust_options_button->hide();
      load_button->hide();
      save_button->hide();
      reset_button->hide();
      random_button->hide();
      _jack_plugin_widget = new Jack_Plugin_widget(this,_patch.data());
      vertical_layout->insertWidget(1,_jack_plugin_widget);

#ifdef WITH_FAUST_DEV
      // Faust:
    }else if(!strcmp(plugin->type->type_name, "Faust Dev")) {
      new_pd_controller_button->hide();
      _faust_plugin_widget = new Faust_Plugin_widget(this, faust_compilation_status, _patch.data());
      vertical_layout->insertWidget(1,_faust_plugin_widget);
      //_plugin_widget=PluginWidget_create(this, _patch.data());
#endif

      // Others:
    } else {
      new_pd_controller_button->hide();
      faust_load_button->hide();
      faust_save_button->hide();
      faust_compilation_status->hide();
      faust_revert_button->hide();
      faust_show_button->hide();
      faust_options_button->hide();
      _plugin_widget=PluginWidget_create(this, _patch.data(), SIZETYPE_NORMAL);
      vertical_layout->insertWidget(1,_plugin_widget);
    }

    
    if(plugin->type!=PR_get_plugin_type_by_name(NULL, "Sample Player","Sample Player") && plugin->type!=PR_get_plugin_type_by_name(NULL, "Sample Player","Click"))
      record_button->hide();

    
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

    //_plugin_widget->set_automation_value_pointers(plugin);

    update_widget();

    _is_initing = false;
  }

  QString small_number(int n){
    const QString a[10] = {
#ifdef FOR_WINDOWS
      QString("0"),
      QString("1"),
      QString("2"),
      QString("3"),
      QString("4"),
      QString("5"),
      QString("6"),
      QString("7"),
      QString("8"),
      QString("9")
#else
      QString("\u2080"),
      QString("\u2081"),
      QString("\u2082"),
      QString("\u2083"),
      QString("\u2084"),
      QString("\u2085"),
      QString("\u2086"),
      QString("\u2087"),
      QString("\u2088"),
      QString("\u2089")
#endif
    };

    QString source = QString::number(n);
    QString ret = "";

    for(int i = 0 ; i < source.size(); i++)
      ret += a[QString(source[i]).toInt()];

    return ret;
  }

  void update_cpu_usage(bool force){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    
    if (plugin != NULL) {

      CpuUsage *cpu_usage = (CpuUsage*)ATOMIC_GET(plugin->cpu_usage);
      
      if (cpu_usage==NULL){
        
        ATOMIC_SET(plugin->cpu_usage, new CpuUsage);

      } else {

        if (SP_is_autosuspending(plugin->sp)) {
          if (cpu_usage->should_update())
            plugin_info->setText(AUTOSUSPENDING_STRING);
        } else {
          QString new_text = cpu_usage->update_and_get_string();
          if (new_text != plugin_info->text())
            plugin_info->setText(new_text);
        }
        
      }
    }
  }
  
  // only called when visible
  void calledRegularlyByParent(void){
    _is_initing = true;

#ifdef WITH_FAUST_DEV
    if (_faust_plugin_widget!=NULL)
      _faust_plugin_widget->calledRegularlyByParent();
#endif    

    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    
    if (plugin != NULL) {
      const SoundPluginType *type = plugin->type;

      if (!strcmp("Sample Player", type->type_name)){
        QString recording_status = SAMPLER_get_recording_status(plugin);
        if (recording_status != record_button->text())
          record_button->setText(recording_status);
      }
      
      if(type->gui_is_visible!=NULL){
        bool checkbox = show_gui_checkbox->isChecked();
        bool gui = PLUGIN_gui_is_visible(plugin, API_get_gui_from_widget(this));
        
        _ignore_checkbox_stateChanged=true;{
          
          if (checkbox==false && gui==true)
            show_gui_checkbox->setChecked(true);
          else if(checkbox==true && gui==false)
            show_gui_checkbox->setChecked(false);
          
        }_ignore_checkbox_stateChanged=false;
      }
      
      update_preset_widgets();

      callSliderpainterUpdateCallbacks();

      update_cpu_usage(false);

    }

#ifdef WITH_FAUST_DEV
    if (_faust_plugin_widget != NULL){
      
      if (faust_show_button->isChecked() && !_faust_plugin_widget->_cpp_dialog->isVisible())
        faust_show_button->setChecked(false);
      
      if (faust_options_button->isChecked() && !_faust_plugin_widget->_options_dialog->isVisible())
        faust_options_button->setChecked(false);
      
    }
#endif

    _is_initing = false;
  }

  void callSliderpainterUpdateCallbacks(void){
    PluginWidget *plugin_widget = NULL;

    if (_plugin_widget != NULL)
      plugin_widget = _plugin_widget;

#ifdef WITH_FAUST_DEV
    else if (_faust_plugin_widget != NULL)
      plugin_widget = _faust_plugin_widget->_plugin_widget;
#endif
    
    if (plugin_widget != NULL)
      plugin_widget->calledRegularlyByParent();
    
    if (_pd_plugin_widget != NULL){
      
      for(unsigned int i=0; i<_pd_plugin_widget->_controllers.size(); i++) {
        Pd_Controller_widget *c = _pd_plugin_widget->_controllers[i];
        
        MyQSlider *slider = c->value_slider;
        if (slider != NULL){
          SLIDERPAINTER_call_regularly(slider->_painter, -1);
        }
      }
    }
#if 0
    int height = g_main_window->height();
    if (height!=_last_height && _is_large && editor_has_keyboard_focus()){
      on_large_checkbox_toggled(false);
      on_large_checkbox_toggled(true);
    }
    _last_height = height;
#endif
  }
  
  void prepare_for_deletion(void){
    if (_plugin_widget != NULL)
      _plugin_widget->prepare_for_deletion();
  }
  
  ~Plugin_widget(){
    prepare_for_deletion();
  }
      
  void update_widget(void) {
    if(_plugin_widget != NULL)
      _plugin_widget->update_gui();
    
    if(_pd_plugin_widget != NULL)
      _pd_plugin_widget->update_gui();

    if(_jack_plugin_widget != NULL)
      _jack_plugin_widget->update_gui();

#ifdef WITH_FAUST_DEV
    if(_faust_plugin_widget != NULL)
      _faust_plugin_widget->update_gui();
#endif

    QFontMetrics fm(QApplication::font());
    int header_height = fm.height() * 4 / 3;
    header->setMinimumHeight(header_height);
    header->setMaximumHeight(header_height);
    
    update_preset_widgets();

    {
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      if (plugin != NULL){

#if SHOW_SOLO_BUTTON
        {
          bool is_solo = ATOMIC_GET(plugin->solo_is_on);
          if (solo_checkbox->isChecked() != is_solo)
            solo_checkbox->setChecked(is_solo);
        }
#endif
        
        {
          enum AutoSuspendBehavior auto_suspend_behavior = PLUGIN_get_autosuspend_behavior(plugin);
          
          if (auto_suspend_behavior==DEFAULT_AUTOSUSPEND_BEHAVIOR)
            auto_bypass_menu_button->setText(" ");
          else if (auto_suspend_behavior==AUTOSUSPEND_ENABLED)
            auto_bypass_menu_button->setText("✔");
          else
            auto_bypass_menu_button->setText("✘");
        }
        
        #if 0
        {
          CpuUsage *cpu_usage = (CpuUsage*)ATOMIC_GET(plugin->cpu_usage);
          if (cpu_usage != NULL)
            cpu_usage->reset(); // If not, max value has an unusually high value during the first second. I don't know why that is.
        }
        #endif

        update_ab_buttons();

        if (plugin->type->play_note==NULL)
          enable_sample_seek->hide();
        else
          enable_sample_seek->setChecked(ATOMIC_GET(plugin->enable_sample_seek));
      }

    }
    
  }

  void update_preset_widgets(){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    const SoundPluginType *type = plugin->type;

    if (type->get_num_presets != NULL && preset_selector->isVisible()){

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

    R_ASSERT(g_radium_runs_custom_exec==false);

    QString filename;

    {
      radium::ScopedExec scopedExec(true);

      
      filename = QFileDialog::getSaveFileName(
                                              this,
                                              is_fxb ? "Save VST FXB file" : "Save VST FXP file",
                                              last_fxb_preset_path,
#if FOR_WINDOWS
                                              is_fxb ? "*.fxb ;; All files (*)" : "*.fxp ;; All files (*)",
#else
                                              is_fxb ? "VST FXB (*.fxb) ;; All files (*)" : "VST FXP (*.fxp) ;; All files (*)",
#endif
                                              0,
                                              QFileDialog::DontUseCustomDirectoryIcons | (useNativeFileRequesters() ? (QFileDialog::Option)0 : QFileDialog::DontUseNativeDialog)
                                              );
      
      if(filename=="")
        return;
    }

    last_fxb_preset_path = QFileInfo(filename).absoluteDir().path();
    
    if (is_fxb)
      PLUGINHOST_save_fxb(plugin, STRING_create(filename));
    else
      PLUGINHOST_save_fxp(plugin, STRING_create(filename));
  }

  void LoadFXBP(void){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    
    R_ASSERT(g_radium_runs_custom_exec==false);

    QString filename;

    {
      radium::ScopedExec scopedExec(true);
    
      filename = QFileDialog::getOpenFileName(
                                              this,
                                              "Load VST FXB or FXP file",
                                              last_fxb_preset_path,
#if FOR_WINDOWS
                                              "*.fxb *.fxp ;; All files (*)",
#else
                                              "VST FXB/FXP files (*.fxb *.fxp) ;; All files (*)",
#endif
                                              0,
                                              QFileDialog::DontUseCustomDirectoryIcons | (useNativeFileRequesters() ? (QFileDialog::Option)0 : QFileDialog::DontUseNativeDialog)
                                              );
      
      if(filename=="")
        return;
    }

    last_fxb_preset_path = QFileInfo(filename).absoluteDir().path();
    
    PLUGINHOST_load_fxbp(plugin, STRING_create(filename));
    
    GFX_update_instrument_widget(_patch.data());
  }

  void change_height(SizeType type){
    if (_size_type==type)
      return;

    if (_plugin_widget != NULL){
      for(ParamWidget *paramWidget : _plugin_widget->_param_widgets)
        paramWidget->_size_type = type;
    }

    _size_type=type;
    
    AUDIOWIDGET_change_height(_patch.data(), type);

#ifdef WITH_FAUST_DEV
    if (_faust_plugin_widget != NULL)
      _faust_plugin_widget->change_height(type);
#endif
    //    else
      //#endif
#if 0
    // ?? This code was enabled before.
      {
        delete _plugin_widget;
        _plugin_widget=PluginWidget_create(this, _patch.data(), type);
        vertical_layout->insertWidget(1,_plugin_widget);
      }
#endif
    if (type==SIZETYPE_NORMAL)
      evalScheme("(minimize-lowertab)");
  }

  int _ab_checkbox_width = -1;

public:
  
  void update_ab_buttons(void){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    int curr=plugin->curr_ab_num;

    if (_ab_checkbox_width==-1){
      ab_a->adjustSize();
      _ab_checkbox_width = ab_a->width();
    }

    ab_a->setMinimumWidth(_ab_checkbox_width);
    ab_a->setMaximumWidth(_ab_checkbox_width);
    ab_b->setMinimumWidth(_ab_checkbox_width);
    ab_b->setMaximumWidth(_ab_checkbox_width);

    if (curr==0 || plugin->ab_is_valid[0])
      ab_a->setText("A*");
    else
      ab_a->setText("A");
    
    if (curr==1 || plugin->ab_is_valid[1])
      ab_b->setText("B*");
    else
      ab_b->setText("B");
  }

  void ab_rightclicked(int num){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    int curr=plugin->curr_ab_num;

    IsAlive is_alive(this);
    
    GFX_SimpleMenu(talloc_format("%sReset",curr==num?"[disabled]":""), [is_alive, this, plugin, num](int command, bool onoff){

        if (!is_alive || _patch->patchdata==NULL)
          return;

        PLUGIN_reset_ab(plugin, num);
        update_ab_buttons();
        AUDIOWIDGET_redraw_ab(_patch.data());
      });
  }


public slots:

  // ab-testing
  void on_ab_a_clicked(){if(ab_a->_last_pressed_button==Qt::RightButton) ab_rightclicked(0);}
  void on_ab_b_clicked(){if(ab_b->_last_pressed_button==Qt::RightButton) ab_rightclicked(1);}
  /*
  void on_ab_c_clicked(){ab_rightclicked(2);}
  void on_ab_d_clicked(){ab_rightclicked(3);}
  void on_ab_e_clicked(){ab_rightclicked(4);}
  void on_ab_f_clicked(){ab_rightclicked(5);}
  void on_ab_g_clicked(){ab_rightclicked(5);}
  void on_ab_h_clicked(){ab_rightclicked(6);}
  */
  
  void on_ab_reset_clicked(){
    if (_is_initing)
      return;
    
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    PLUGIN_reset_ab(plugin,-1);
    update_ab_buttons();
    AUDIOWIDGET_redraw_ab(_patch.data());
  }

  void on_ab_a_toggled(bool val){
    if (_is_initing)
      return;
    
    if (val && _ignore_checkbox_stateChanged==false)
      AUDIOWIDGET_set_ab(_patch.data(), 0);
    update_ab_buttons();
  }

  void on_ab_b_toggled(bool val){
    if (_is_initing)
      return;
    
    if (val && _ignore_checkbox_stateChanged==false)
      AUDIOWIDGET_set_ab(_patch.data(), 1);
    update_ab_buttons();
  }

  // auto-bypass
  //
  void on_auto_bypass_menu_button_released() {
    if (_is_initing)
      return;
    
    _auto_suspend_menu.myExec();
    update_widget();
  }

  // sample-seek

  void on_enable_sample_seek_toggled(bool val){
    if (_is_initing)
      return;
    
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    ATOMIC_SET(plugin->enable_sample_seek, val);
  }
  
  // faust
  //
#if 0
  void on_faust_compile_button_released() {
    printf("Got it\n");
#ifdef WITH_FAUST_DEV // <-- #ifdef must be on the inside of the function for moc to produce code.
    if (_faust_plugin_widget != NULL)
      _faust_plugin_widget->start_compilation();
#endif
  }
#endif

  void on_faust_revert_button_released(){
    if (_is_initing)
      return;
    
#ifdef WITH_FAUST_DEV
    if (_faust_plugin_widget != NULL)
      _faust_plugin_widget->revert_to_latest_working_version();
#endif
  }

  void on_faust_load_button_released(){
    if (_is_initing)
      return;
    
#ifdef WITH_FAUST_DEV
    if (_faust_plugin_widget != NULL){

      QString filename;

      R_ASSERT(g_radium_runs_custom_exec==false);

      {
        radium::ScopedExec scopedExec(true);

        filename = QFileDialog::getOpenFileName(this,
                                                "Load Faust source code",
                                                "",
                                                "*.dsp ;; All files (*)",
                                                0,
                                                QFileDialog::DontUseCustomDirectoryIcons | (useNativeFileRequesters() ? (QFileDialog::Option)0 : QFileDialog::DontUseNativeDialog)
                                                );
      }
        
      if(filename != "")
        _faust_plugin_widget->load_source(filename);
    }
#endif
  }
    
  void on_faust_save_button_released(){
    if (_is_initing)
      return;
    
#ifdef WITH_FAUST_DEV
    if (_faust_plugin_widget != NULL){

      QString filename;

      R_ASSERT(g_radium_runs_custom_exec==false);

      {
        radium::ScopedExec scopedExec(true);

        filename = QFileDialog::getSaveFileName(this,
                                                "Save Faust source code",
                                                "",
                                                "*.dsp ;; All files (*)",
                                                0,
                                                QFileDialog::DontUseCustomDirectoryIcons | (useNativeFileRequesters() ? (QFileDialog::Option)0 : QFileDialog::DontUseNativeDialog)
                                                );
        
      }

      if(filename != "")
        _faust_plugin_widget->save_source(filename);      
    }
#endif
  }

  void on_faust_show_button_toggled(bool val){
    if (_is_initing)
      return;
    
#ifdef WITH_FAUST_DEV
    if (_faust_plugin_widget != NULL){
      if (val)
        _faust_plugin_widget->show_cpp_source();
      else
        _faust_plugin_widget->_cpp_dialog->hide();
    }
#endif
  }

  void on_faust_options_button_toggled(bool val){
    if (_is_initing)
      return;
    
#ifdef WITH_FAUST_DEV
    if (_faust_plugin_widget != NULL){
      if (val)
        _faust_plugin_widget->edit_options();
      else
        _faust_plugin_widget->_options_dialog->hide();
    }
#endif
  }

  void on_half_checkbox_toggled(bool val){
    if (_is_initing)
      return;
    
    //addMessage(talloc_format("Half %d\n", val));
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin==NULL)
      return;
    
    if (max_checkbox->isChecked())
      return;

    if (val)
      change_height(SIZETYPE_HALF);
    else
      change_height(SIZETYPE_NORMAL);
  }
    
  void on_max_checkbox_toggled(bool val){
    if (_is_initing)
      return;
    
    //addMessage(talloc_format("Full %d\n", val));
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin==NULL)
      return;
    
    if (val){
      change_height(SIZETYPE_FULL);
    }else{
      if (half_checkbox->isChecked())
        change_height(SIZETYPE_HALF);
      else
        change_height(SIZETYPE_NORMAL);
    }
  }
    
  
  // pd
  //
  void on_new_pd_controller_button_released() {
    if (_is_initing)
      return;
    
    ADD_UNDO(PdControllers_CurrPos(_patch.data()));
    _pd_plugin_widget->new_controller();  
  }

  // general
  void on_show_gui_checkbox_toggled(bool val){
    if (_is_initing)
      return;
    
    if (_ignore_checkbox_stateChanged==false) {
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      if (val){
        PLUGIN_open_gui(plugin, API_get_gui_from_widget(this));
      }else{
        PLUGIN_close_gui(plugin);
      }
    }
  }
  
    void on_limiter_bypass_button_toggled(bool val){
      if (_is_initing)
        return;
    
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      int effect_num = PLUGIN_get_effect_num(plugin, "Limiter Bypass", NULL);
      R_ASSERT_RETURN_IF_FALSE(effect_num != -1);
      
      PLUGIN_set_effect_value(plugin, -1, effect_num, val==true ? 1.0 : 0.0, STORE_VALUE, FX_single, EFFECT_FORMAT_SCALED);

      //update_limiter_sliders();
    }

    void on_interpolation_type_currentIndexChanged( int val){
      if (_is_initing)
        return;

      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      printf("Setting resampler type to %d\n",val);
      SAMPLER_set_resampler_type(plugin, (enum ResamplerType)val);
    }

  void on_fxbp_button_clicked(){
    if (_is_initing)
      return;

    vector_t v = {};
    VECTOR_push_back(&v, "Load FXB or FXP file");
    VECTOR_push_back(&v, "Save FXB (standard VST bank format)");
    VECTOR_push_back(&v, "Save FXP (standard VST preset format)");

    IsAlive is_alive(this);
    
    GFX_Menu3(v,[is_alive, this](int num, bool onoff){
        if (!is_alive || _patch->patchdata==NULL)
          return;

        switch(num){
          case 0: LoadFXBP(); break;
          case 1: SaveFXBP(true); break;
          case 2: SaveFXBP(false); break;
          default: break;
        }
      });

    //switch(GFX_Menu(root->song->tracker_windows, NULL, "", v, true)){
    //}
  }

  void on_record_button_clicked(){
    if (_is_initing)
      return;

    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

    vector_t v = {}; // c++ way of zero-initialization without getting missing-field-initializers warning.

    int mono_main   = VECTOR_push_back(&v, "Mono from main input");
    int stereo_main = VECTOR_push_back(&v, "Stereo from main inputs");

    VECTOR_push_back(&v, "--------------");
    
    int mono   = VECTOR_push_back(&v, "Mono from input connection");
    int stereo = VECTOR_push_back(&v, "Stereo from input connections");

    QString pathdir;

    if (saveRecordedAudioFilesInBrowserPath() || dc.filename == NULL){

      auto *sample_requester_widget = AUDIOWIDGET_get_sample_requester_widget(_patch.data());
      //printf("dir: %p\n", sample_requester_widget->_dir);
      pathdir = SAMPLEREQUESTER_get_path(sample_requester_widget);

    } else {

      QString filename = STRING_get_qstring(dc.filename).replace(QRegExp(".rad$"), "_rad");      

      QDir dir(QFileInfo(filename).absoluteFilePath() + "_audio");

      if (!dir.exists()){

        // Qt has a really confusing api to make a directory.
        QDir base(QDir::root());
        if (!base.mkdir(dir.absolutePath())){
          GFX_Message(NULL, "Unable to create directory %s.", dir.absolutePath().toUtf8().constData());
          return;
        }

      }
      
      pathdir = dir.absolutePath();

    }

    IsAlive is_alive(this);
    
    GFX_Menu3(v,[is_alive, this, plugin, pathdir, mono_main, stereo_main, mono, stereo](int sel, bool onoff){

        if (!is_alive || _patch->patchdata==NULL)
          return;

        if (sel==mono_main)
          SAMPLER_start_recording(plugin, STRING_create(pathdir), 1, true);
        else if (sel==stereo_main)
          SAMPLER_start_recording(plugin, STRING_create(pathdir), 2, true);
        else if (sel==mono)
          SAMPLER_start_recording(plugin, STRING_create(pathdir), 1, false);
        else if (sel==stereo)
          SAMPLER_start_recording(plugin, STRING_create(pathdir), 2, false);
      });
    
  }

  void saveit(void){
    vector_t patches = {};
    VECTOR_push_back(&patches, _patch.data());
    PRESET_save(&patches, true, API_get_gui_from_existing_widget(this->window()));    
  }
  
  void on_save_button_clicked(){
    if (_is_initing)
      return;

    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    
    bool can_embed = QString("Sample Player")==plugin->type->type_name || QString("FluidSynth")==plugin->type->type_name;
    
    if (can_embed) {

      vector_t v = {};
      VECTOR_push_back(&v, "Save");
      int save_embedded = VECTOR_push_back(&v, "Save, with embedded sample");

      IsAlive is_alive(this);
      
      GFX_Menu3(v, [save_embedded, is_alive, this](int num, bool onoff){
                  
          if (!is_alive || _patch->patchdata==NULL || num < 0)
            return;

          //printf("NUM: %d. save_mebedded: %d\n", num, save_embedded);
          
          bool old_embed = g_embed_samples;
          
          if (num==save_embedded)
            g_embed_samples = true;
          
          saveit();

          if (num==save_embedded)
            g_embed_samples = old_embed;
          
        });
      
    } else {

      saveit();

    }
    
  }

  void on_load_button_clicked(){      
    if (_is_initing)
      return;
    
    requestLoadInstrumentPreset(_patch->id, "", API_get_gui_from_existing_widget(this->window()));
  }

  void on_replace_button_clicked(){
    if (_is_initing)
      return;
    
    requestReplaceInstrument(_patch->id, "", API_get_gui_from_existing_widget(this->window()));
  }
    
  void on_reset_button_clicked(){
    if (_is_initing)
      return;
    
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    PLUGIN_reset(plugin);
    GFX_update_instrument_widget(_patch.data());
  }

  void on_random_button_clicked(){
    if (_is_initing)
      return;
    
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    PLUGIN_random(plugin);

    GFX_update_instrument_widget(_patch.data());
  }

  void on_info_button_clicked(){
    if (_is_initing)
      return;
    
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin!=NULL)
      PLUGIN_show_info_window(plugin->type, plugin, API_get_gui_from_existing_widget(this->window()));
  }

  void on_preset_button_clicked(){
    if (_is_initing)
      return;
    
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    const SoundPluginType *type = plugin->type;
    int num_presets = type->get_num_presets(plugin);
      
    vector_t v = {}; // c++ way of zero-initialization without getting missing-field-initializers warning.

    for(int i=0;i<num_presets;i++){
      VECTOR_push_back(&v, talloc_format("%d: %s", i+1, type->get_preset_name(plugin, i)));
    }

    VECTOR_push_back(&v, "--------------");
    VECTOR_push_back(&v, "<set new name>");

    IsAlive is_alive(this);

    GFX_Menu3(v,

              [is_alive,num_presets,type,plugin,this](int num, bool onoff){
                
                if (!is_alive || _patch->patchdata==NULL)
                  return;

                R_ASSERT_RETURN_IF_FALSE(_patch->patchdata==plugin);

                printf("I'm here, actually\n");
                
                if (num == num_presets+1) {
                  char *new_name = GFX_GetString(NULL, NULL, "new name: ", true);
                  if (new_name != NULL){
                    type->set_preset_name(plugin, type->get_current_preset(plugin), new_name);
                    update_widget();
                  }
                } else if (num >= 0 && num<num_presets) {
                  type->set_current_preset(plugin, num);
                  update_widget();
                }
              });
  }
    
  void on_preset_selector_editingFinished(){
    if (_is_initing)
      return;
    
    int num = preset_selector->value() - 1;
    printf("num: %d\n",num);
      
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    const SoundPluginType *type = plugin->type;

    type->set_current_preset(plugin, num);
    update_widget();
      
    set_editor_focus();
  }

#if SHOW_SOLO_BUTTON
  void on_solo_checkbox_toggled(bool val){
    if (_is_initing)
      return;
    
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin != NULL){
      int num_effects = plugin->type->num_effects;
      //ADD_UNDO(AudioEffect_CurrPos(_patch, num_effects+EFFNUM_SOLO_ONOFF));
      PLUGIN_set_effect_value(plugin, -1, num_effects+EFFNUM_SOLO_ONOFF, val ? 1 : 0, STORE_VALUE, FX_single, EFFECT_FORMAT_SCALED);
      //}
      //ATOMIC_SET(plugin->solo_is_on, val);
      //CHIP_update(plugin);
    }
  }
#endif
};

}


// These two functions can probably be deleted. The GUI polls constantly now.

void PLUGINWIDGET_gui_is_hidden(void *w){
  Plugin_widget *plugin_widget = static_cast<Plugin_widget*>(w);
  plugin_widget->_ignore_checkbox_stateChanged = true; {
    plugin_widget->show_gui_checkbox->setChecked(false);
  } plugin_widget->_ignore_checkbox_stateChanged = false;
}

void PLUGINWIDGET_gui_is_visible(void *w){
  Plugin_widget *plugin_widget = static_cast<Plugin_widget*>(w);
  plugin_widget->_ignore_checkbox_stateChanged = true; {
    plugin_widget->show_gui_checkbox->setChecked(true);
  } plugin_widget->_ignore_checkbox_stateChanged = false;
}

