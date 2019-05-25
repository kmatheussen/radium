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


#include "../common/sequencer_proc.h"

#include "../audio/SoundPlugin_proc.h"
#include "../audio/SoundPluginRegistry_proc.h"

#include "Qt_MyQScrollBar.hpp"

/*
class Pd_Plugin_widget;
class Audio_instrument_wigdet;
static Pd_Plugin_widget *AUDIOWIDGET_get_pd_plugin_widget(Audio_instrument_widget *audio_instrument_widget);
*/

#include "Qt_audio_instrument_widget.h"

#include "mQt_sample_requester_widget_callbacks.h"
#include "mQt_plugin_widget_callbacks.h"
#include "mQt_compressor_widget_callbacks.h"

#include "../mixergui/QM_chip.h"
#include "../audio/Bus_plugins_proc.h"


//extern void BottomBar_set_system_audio_instrument_widget_and_patch(Ui::Audio_instrument_widget *system_audio_instrument_widget, struct Patch *patch);

namespace{
  
class Audio_instrument_widget : public QWidget, public Ui::Audio_instrument_widget{
  Q_OBJECT;

public:

  bool is_starting;
  
  radium::GcHolder<struct Patch> _patch;

  Patch_widget *_patch_widget;
  Plugin_widget *_plugin_widget;
  
  Sample_requester_widget *_sample_requester_widget;
  Compressor_widget *_comp_widget;

  SizeType _size_type;
  SizeType _size_type_before_hidden;
  //bool _was_large_before_hidden;

  static void set_arrow_style(QWidget *arrow, bool set_size_policy=true){
    QPalette palette2;

    QBrush brush1(QColor(106, 104, 100, 255));
    brush1.setStyle(Qt::SolidPattern);

    QBrush brush3(QColor(85, 85, 0, 255));
    //QBrush brush3(QColor(0, 107, 156, 255));
    brush3.setStyle(Qt::SolidPattern);
    palette2.setBrush(QPalette::Active, QPalette::WindowText, brush3);
    palette2.setBrush(QPalette::Inactive, QPalette::WindowText, brush3);
    palette2.setBrush(QPalette::Disabled, QPalette::WindowText, brush1);
    palette2.setBrush(QPalette::Active, QPalette::Text, brush3);
    palette2.setBrush(QPalette::Inactive, QPalette::Text, brush3);
    palette2.setBrush(QPalette::Disabled, QPalette::Text, brush1);
    arrow->setPalette(palette2);
    //static_cast<QLabel*>(arrow)->setText("gakk");

#if 1
    if(set_size_policy){
      QSizePolicy sizePolicy4(QSizePolicy::Fixed, QSizePolicy::Fixed);
      sizePolicy4.setHorizontalStretch(0);
      sizePolicy4.setVerticalStretch(0);
      sizePolicy4.setHeightForWidth(arrow->sizePolicy().hasHeightForWidth());
      arrow->setSizePolicy(sizePolicy4);

      //arrow->setFrameShape(QFrame::Box);
    }
#endif
  }


 Audio_instrument_widget(QWidget *parent,struct Patch *patch)
    : QWidget(parent)
    , is_starting(true)
    , _patch(patch)
    , _plugin_widget(NULL)
    , _sample_requester_widget(NULL)
      //, _is_large(false)
    , _size_type(SIZETYPE_NORMAL)
    , _size_type_before_hidden(SIZETYPE_NORMAL)
  {

    setupUi(this);    

    scrollArea->setHorizontalScrollBar(new Qt_MyQScrollBar(Qt::Horizontal));
    scrollArea->setVerticalScrollBar(new Qt_MyQScrollBar(Qt::Vertical));

    time_of_last_minheight_inc.start();

    solo_button->_add_undo_when_clicked = false;
    bypass_button->_add_undo_when_clicked = false;
    
    /*
    {
      //QFontMetrics fm(font());
      _ab_checkbox_width = ab_reset_button->width(); //fm.width("A") * 4;
      ab_reset_button->setMinimumWidth(_ab_checkbox_width);
      ab_reset_button->setMaximumWidth(_ab_checkbox_width);
    }
    */
              
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

    _patch_widget = new Patch_widget(this,patch);
    voiceBox_layout->insertWidget(0,_patch_widget, 1);
    //if(false && plugin->type->play_note==NULL)
    //  _patch_widget->voices_widget->setDisabled(true);

    SLIDERPAINTER_set_string(input_volume_slider->_painter, "In");
    SLIDERPAINTER_set_string(volume_slider->_painter, "Volume");
    SLIDERPAINTER_set_string(output_volume_slider->_painter, "Out");
    SLIDERPAINTER_set_string(panning_slider->_painter, "Panning");
    //velocity_slider->_painter, "Edit vel.";

    SLIDERPAINTER_set_string(drywet_slider->_painter, "Dry/Wet");
    SLIDERPAINTER_set_string(bus1_slider->_painter, "Reverb");
    SLIDERPAINTER_set_string(bus2_slider->_painter, "Chorus");
    SLIDERPAINTER_set_string(bus3_slider->_painter, "Aux 1");
    SLIDERPAINTER_set_string(bus4_slider->_painter, "Aux 2");
    SLIDERPAINTER_set_string(bus5_slider->_painter, "Aux 3");

    /*
    if(!strcmp(plugin->type->type_name,"Jack") && !strcmp(plugin->type->name,"System Out")){
      _i_am_system_out = true;
      BottomBar_set_system_audio_instrument_widget_and_patch(this, _patch.data());
    }
    */
    
#if 0
    if(plugin->type->num_inputs==0){
      drywet_slider->setDisabled(true);
      effects_onoff->setDisabled(true);
    }

    if(plugin->type->num_inputs==0){
      input_volume_slider->setDisabled(true);
      input_volume_onoff->setDisabled(true);
    }
#endif

    if(plugin->type->num_outputs==0){
      filters_widget->setDisabled(true);
      outputs_widget->setDisabled(true);
    }

    if(plugin->type->num_outputs<2){
      panning_onoff->setDisabled(true);
      panning_slider->setDisabled(true);
      rightdelay_onoff->setDisabled(true);
      rightdelay_slider->setDisabled(true);
    }

#if 0
    //instrument->effects_frame->addWidget(PluginWidget_create(NULL, plugin), 0, 3, 2, 1);
    _plugin_widget=PluginWidget_create(NULL, _patch.data());
    if(_plugin_widget->_param_widgets.size() > 0){
      delete(pipe_label);
      pipe_label = NULL;
    }
#else
    _plugin_widget = new Plugin_widget(this,_patch.data());

    delete(pipe_label);
    pipe_label = NULL;
#endif

    effects_layout->insertWidget(4,_plugin_widget);
    _plugin_widget->setVisible(plugin->show_controls_gui);
    spacer_holder->setVisible(!plugin->show_controls_gui);
    

    if(plugin->type==PR_get_plugin_type_by_name(NULL, "Sample Player","Sample Player") || plugin->type==PR_get_plugin_type_by_name(NULL, "Sample Player","Click") || plugin->type==PR_get_plugin_type_by_name(NULL, "FluidSynth","FluidSynth")){
      _sample_requester_widget = new Sample_requester_widget(this, _patch_widget->name_widget, _plugin_widget->sample_name_label, _patch.data());
      effects_layout->insertWidget(3,_sample_requester_widget);
      show_browser->setFixedWidth(browserArrow->width());

      /*
      QLabel *arrow = new QLabel("=>",this);
      arrow->setFrameShape(QFrame::Box);

      set_arrow_style(arrow);

      effects_layout->insertWidget(3, arrow, 0, Qt::AlignTop);
      arrow->show();
      */

      //MyQCheckBox *checkbox = new MyQCheckBox("hello", this);
      //effects_layout->

    } else
      browserWidget->hide();

    // Add compressor
    {
      _comp_widget = new Compressor_widget(patch, this);
      _comp_widget->setMinimumWidth(150);
      effects_layout->insertWidget(effects_layout->count()-2, _comp_widget);

      // these widgets are only used in the standalone version
      _comp_widget->load_button->hide();
      _comp_widget->save_button->hide();
      _comp_widget->radium_url->hide();
      _comp_widget->bypass->hide();

      GL_lock(); {
        _comp_widget->show();
        _comp_widget->setVisible(plugin->show_compressor_gui);
      }GL_unlock();
    }

    
    filters_widget->setVisible(plugin->show_equalizer_gui);


    // Adjust output widget widths
    {
      QFontMetrics fm(QApplication::font());
      //QRect r =fm.boundingRect(SLIDERPAINTER_get_string(_painter));
      int width = fm.width("Dry: 100% Wet: 0% Gak") + 10;
      lowpass_freq_slider->setMinimumWidth(width);
      highpass_freq_slider->setMinimumWidth(width);
      drywet_slider->setMinimumWidth(width);
    }


#if 0
    if(plugin->type->num_inputs==0){
      input_volume_layout->setDisabled(true);
    }
#endif


    // set the scroll bar itself to size 10.
    scrollArea->horizontalScrollBar()->setFixedHeight(10);
#ifdef USE_QT5
    //scrollArea->verticalScrollBar()->hide();
#endif
    
    updateWidgets();
    setupPeakAndAutomationStuff();

    is_starting = false;
  }

  void enterEvent(QEvent *event) override{
    setCursor(Qt::ArrowCursor);
  }
  
  void prepare_for_deletion(void){
    _comp_widget->prepare_for_deletion();
    _plugin_widget->prepare_for_deletion();
  }

  ~Audio_instrument_widget(){
    if(_size_type != SIZETYPE_NORMAL)
      change_height(SIZETYPE_NORMAL);

    prepare_for_deletion();
  }
     
  MyQSlider *get_system_slider(int system_effect){
    switch(system_effect){

    case EFFNUM_INPUT_VOLUME:
      return input_volume_slider; 
    case EFFNUM_VOLUME:
      return volume_slider; 
    case EFFNUM_OUTPUT_VOLUME:
      return output_volume_slider;

    case EFFNUM_BUS1:
      return bus1_slider; 
    case EFFNUM_BUS2:
      return bus2_slider; 
    case EFFNUM_BUS3:
      return bus3_slider; 
    case EFFNUM_BUS4:
      return bus4_slider; 
    case EFFNUM_BUS5:
      return bus5_slider; 

    case EFFNUM_DRYWET:
      return drywet_slider; 

    case EFFNUM_PAN:
      return panning_slider; 

    case EFFNUM_LOWPASS_FREQ:
      return lowpass_freq_slider; 

    case EFFNUM_HIGHPASS_FREQ:
      return highpass_freq_slider; 

    case EFFNUM_EQ1_FREQ:
      return eq1_freq_slider; 
    case EFFNUM_EQ1_GAIN:
      return eq1_gain_slider; 

    case EFFNUM_EQ2_FREQ:
      return eq2_freq_slider; 
    case EFFNUM_EQ2_GAIN:
      return eq2_gain_slider; 

    case EFFNUM_LOWSHELF_FREQ:
      return lowshelf_freq_slider; 
    case EFFNUM_LOWSHELF_GAIN:
      return lowshelf_gain_slider; 

    case EFFNUM_HIGHSHELF_FREQ:
      return highshelf_freq_slider; 
    case EFFNUM_HIGHSHELF_GAIN:
      return highshelf_gain_slider; 

    case EFFNUM_DELAY_TIME:
      return rightdelay_slider; 

    default:
      return NULL;
    }
  }

  MyQCheckBox *get_system_checkbox(int system_effect){
    switch(system_effect){
    case EFFNUM_INPUT_VOLUME_ONOFF:
      return input_volume_onoff;
    case EFFNUM_VOLUME_ONOFF:
      return mute_button;
    case EFFNUM_OUTPUT_VOLUME_ONOFF:
      return output_volume_onoff;
    case EFFNUM_PAN_ONOFF:
      return panning_onoff;
    case EFFNUM_EFFECTS_ONOFF:
      return bypass_button;
    case EFFNUM_SOLO_ONOFF:
      return solo_button;
    case EFFNUM_LOWPASS_ONOFF:
      return lowpass_onoff;
    case EFFNUM_HIGHPASS_ONOFF:
      return highpass_onoff;

    case EFFNUM_EQ1_ONOFF:
      return eq1_onoff;
    case EFFNUM_EQ2_ONOFF:
      return eq2_onoff;
    case EFFNUM_LOWSHELF_ONOFF:
      return lowshelf_onoff;
    case EFFNUM_HIGHSHELF_ONOFF:
      return highshelf_onoff;

    case EFFNUM_DELAY_ONOFF:
      return rightdelay_onoff;

    default:
      return NULL;
    }
  }

  void updateSliderString(int system_effect){
    MyQSlider *slider = get_system_slider(system_effect);
    if(slider==NULL)
      return;

    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    const SoundPluginType *type = plugin->type;
    int effect_num = type->num_effects + system_effect;

    SLIDERPAINTER_set_recording_color(slider->_painter, PLUGIN_is_recording_automation(plugin, effect_num));
    
    QString p = get_parameter_prepend_text(_patch.data(), effect_num);
    
    char buf[64]={0};
    PLUGIN_get_display_value_string(plugin, effect_num, buf, 64);
    if(system_effect==EFFNUM_DRYWET)
      SLIDERPAINTER_set_string(slider->_painter, p + QString(buf));
    else if(system_effect==EFFNUM_BUS1)
      SLIDERPAINTER_set_string(slider->_painter, p + QString(BUS_get_bus_name(0)) + ": " + QString(buf));
    else if(system_effect==EFFNUM_BUS2)
      SLIDERPAINTER_set_string(slider->_painter, p + QString(BUS_get_bus_name(1)) + ": " + QString(buf));
    else if(system_effect==EFFNUM_BUS3)
      SLIDERPAINTER_set_string(slider->_painter, p + QString(BUS_get_bus_name(2)) + ": " + QString(buf));
    else if(system_effect==EFFNUM_BUS4)
      SLIDERPAINTER_set_string(slider->_painter, p + QString(BUS_get_bus_name(3)) + ": " + QString(buf));
    else if(system_effect==EFFNUM_BUS5)
      SLIDERPAINTER_set_string(slider->_painter, p + QString(BUS_get_bus_name(4)) + ": " + QString(buf));
    else {
      SLIDERPAINTER_set_string(slider->_painter, p + QString(PLUGIN_get_effect_name(plugin, effect_num)+strlen("System ")) + ": " + QString(buf));
    }
      
    slider->update();
  }

  void updateSlider(int system_effect){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    const SoundPluginType *type = plugin->type;
    int effect_num = type->num_effects + system_effect;

    MyQSlider* slider = get_system_slider(system_effect);
    float value = PLUGIN_get_effect_value(plugin, effect_num, VALUE_FROM_STORAGE);
    //printf("            %s, value: %f\n",PLUGIN_get_effect_name(plugin, effect_num), value);
    slider->setValue(value * 10000);

    updateSliderString(system_effect);
  }

  void updateChecked(QAbstractButton* checkwidget, int system_effect){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    const SoundPluginType *type = plugin->type;

    int effect_num = type->num_effects + system_effect;

    bool val = PLUGIN_get_effect_value(plugin,effect_num, VALUE_FROM_PLUGIN) >= 0.5f;

    if(system_effect==EFFNUM_VOLUME_ONOFF || system_effect==EFFNUM_EFFECTS_ONOFF)
      checkwidget->setChecked(!val);
    else
      checkwidget->setChecked(val);

    if (checkwidget==mute_button){
      if (PATCH_get_from_id(_patch->id)==NULL) // happens during initialization.
        mute_button->_is_implicitly_on = false;
      else
        mute_button->_is_implicitly_on = instrumentIsImplicitlyMuted(_patch->id);
    }
    
    if(system_effect==EFFNUM_INPUT_VOLUME_ONOFF)
      input_volume_slider->setEnabled(val);
    //if(system_effect==EFFNUM_VOLUME_ONOFF)
    //  volume_slider->setEnabled(val);
    if(system_effect==EFFNUM_OUTPUT_VOLUME_ONOFF)
      output_volume_slider->setEnabled(val);
    if(system_effect==EFFNUM_LOWPASS_ONOFF)
      lowpass_freq_slider->setEnabled(val);
    if(system_effect==EFFNUM_HIGHPASS_ONOFF)
      highpass_freq_slider->setEnabled(val);
    if(system_effect==EFFNUM_EQ1_ONOFF){
      eq1_freq_slider->setEnabled(val);
      eq1_gain_slider->setEnabled(val);
    }
    if(system_effect==EFFNUM_EQ2_ONOFF){
      eq2_freq_slider->setEnabled(val);
      eq2_gain_slider->setEnabled(val);
    }
    if(system_effect==EFFNUM_LOWSHELF_ONOFF){
      lowshelf_freq_slider->setEnabled(val);
      lowshelf_gain_slider->setEnabled(val);
    }
    if(system_effect==EFFNUM_HIGHSHELF_ONOFF){
      highshelf_freq_slider->setEnabled(val);
      highshelf_gain_slider->setEnabled(val);
    }

    if(system_effect==EFFNUM_DELAY_ONOFF){
      rightdelay_slider->setEnabled(val);
    }
  }

  void updateBusChecked(QAbstractButton* checkwidget, QSlider* bus_slider, SoundProducer *bus){
    if(bus==NULL){
      R_ASSERT_NON_RELEASE(g_is_loading);
      return;
    }

    struct Patch *from = _patch.data();
    struct Patch *to = const_cast<Patch*>(SP_get_plugin(bus)->patch);

    if(instrumentIsOpenAndAudio(from->id)==false){
      R_ASSERT_NON_RELEASE(false);
      return;
    }
    
    if(instrumentIsOpenAndAudio(to->id)==false){
      R_ASSERT_NON_RELEASE(false);
      return;
    }
    
    bool are_connected = MW_are_connected(from, to);
    checkwidget->setChecked(are_connected);
    bus_slider->setEnabled(are_connected);

    if(are_connected)
      checkwidget->setEnabled(true);
    else{
      bool can_connect = CONNECTION_can_connect(from, to);
      //printf("   %s => %s legal: %d\n", from->name, to->name, can_connect);
      checkwidget->setEnabled(can_connect);
    }
  }
    
  QTime time_of_last_minheight_inc;
  int number_of_minheight_incs = 0;
  
  void calledRegularlyByParent(void){
    
    //printf("hello %p\n", this);
    
    //SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

    if (_plugin_widget->isVisible())
      _plugin_widget->calledRegularlyByParent();
    
    callSystemSliderpainterUpdateCallbacks();
    
    if (_comp_widget->isVisible())
      _comp_widget->calledRegularlyByParent();

    int curr_minimum_height = minimumHeight();
    
    bool is_visible = scrollArea->verticalScrollBar()->isVisible();
    
    if (is_visible) {

      printf("  **UP** MINIMIZING UP to %d\n", curr_minimum_height+5);
      setMinimumHeight(curr_minimum_height+5);
      time_of_last_minheight_inc.restart();
      number_of_minheight_incs++;
      
    } else if (_size_type != SIZETYPE_HALF && curr_minimum_height > 5){

      if (time_of_last_minheight_inc.elapsed() > 1000 && number_of_minheight_incs < 3) { // Wait at least 1 second before trying to lower it again. If not, it won't stabilize. (might not stabilize now either though, but it won't be that visible now)
        printf("  **DOWN** MINIMIZING DOWN to %d\n", curr_minimum_height-5);
        setMinimumHeight(curr_minimum_height-5);
        number_of_minheight_incs = 0;
      }
    }
  }

  void callSystemSliderpainterUpdateCallbacks(void){
    
    for(int system_effect=0 ; system_effect<NUM_SYSTEM_EFFECTS ; system_effect++){
      MyQSlider *slider = get_system_slider(system_effect);
      if (slider != NULL) // effect can be a checkbox.
        slider->calledRegularlyByParent();
    }

  }

  void setupPeakAndAutomationStuff(void){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    const SoundPluginType *type = plugin->type;

    // peaks
    
    int num_inputs = type->num_inputs;
    int num_outputs = type->num_outputs;

    if(num_outputs>0){
      SLIDERPAINTER_set_peak_value_pointers(volume_slider->_painter, num_outputs, plugin->volume_peaks.decaying_dbs, true);

      SLIDERPAINTER_set_peak_value_pointers(output_volume_slider->_painter, num_outputs, plugin->output_volume_peaks.decaying_dbs, true);

      SLIDERPAINTER_set_peak_value_pointers(bus1_slider->_painter,2, plugin->bus0_volume_peaks.decaying_dbs, true);
      SLIDERPAINTER_set_peak_value_pointers(bus2_slider->_painter,2, plugin->bus1_volume_peaks.decaying_dbs, true);
      SLIDERPAINTER_set_peak_value_pointers(bus3_slider->_painter,2, plugin->bus2_volume_peaks.decaying_dbs, true);
      SLIDERPAINTER_set_peak_value_pointers(bus4_slider->_painter,2, plugin->bus3_volume_peaks.decaying_dbs, true);
      SLIDERPAINTER_set_peak_value_pointers(bus5_slider->_painter,2, plugin->bus4_volume_peaks.decaying_dbs, true);
    }

    if(num_inputs>0)
      SLIDERPAINTER_set_peak_value_pointers(input_volume_slider->_painter, num_inputs, plugin->input_volume_peaks.decaying_dbs, true);
    else if (num_outputs>0)
      SLIDERPAINTER_set_peak_value_pointers(input_volume_slider->_painter, num_outputs, plugin->input_volume_peaks.decaying_dbs, true);

    // automation

    for(int system_effect=0 ; system_effect<NUM_SYSTEM_EFFECTS ; system_effect++){
      int effect_num = type->num_effects + system_effect;
      
      MyQSlider *slider = get_system_slider(system_effect);
      if (slider != NULL) // effect can be a checkbox.
        SLIDERPAINTER_set_automation_value_pointer(slider->_painter, get_effect_color(plugin, effect_num), &plugin->slider_automation_values[effect_num]);
    }

#if 0
    PluginWidget *plugin_widget = NULL;

    if (_plugin_widget->_plugin_widget != NULL)
      plugin_widget = _plugin_widget->_plugin_widget;

#ifdef WITH_FAUST_DEV
    else if (_plugin_widget->_faust_plugin_widget != NULL)
      plugin_widget = _plugin_widget->_faust_plugin_widget->_plugin_widget;
#endif

    //if (plugin_widget != NULL)
    //  plugin_widget->set_automation_value_pointers(plugin);
#endif
    
    if (_plugin_widget->_pd_plugin_widget != NULL){

      for(unsigned int i=0; i<_plugin_widget->_pd_plugin_widget->_controllers.size(); i++) {
        Pd_Controller_widget *c = _plugin_widget->_pd_plugin_widget->_controllers[i];

        MyQSlider *slider = c->value_slider;
        if (slider != NULL){
          int effect_num = slider->_effect_num;
          SLIDERPAINTER_set_automation_value_pointer(slider->_painter, get_effect_color(plugin, effect_num), &plugin->slider_automation_values[effect_num]);
        }
      }
    }
  }

  void updateWidgets(void){
    //printf("updateWidgets %s\n", _patch->name);
    set_arrow_style(controlsArrow, false);
    set_arrow_style(arrow2, false);
    set_arrow_style(arrow3);
    set_arrow_style(arrow4);
    set_arrow_style(arrow5);
    set_arrow_style(arrow6);
    set_arrow_style(arrow8);
    set_arrow_style(arrow9);
    set_arrow_style(arrow10);
    set_arrow_style(arrow7, false);
    set_arrow_style(browserArrow, false);
    if(pipe_label!=NULL)
      set_arrow_style(pipe_label,false);

    QFontMetrics fm(QApplication::font());
    int fontheight = fm.height() + 3;

    browserWidget->setFixedWidth(fontheight);
    controlsWidget->setFixedWidth(fontheight);
    compressorWidget->setFixedWidth(fontheight);
    equalizerWidget->setFixedWidth(fontheight);
    
    /*
    show_compressor->setFixedWidth(fontheight);
    show_equalizer->setFixedWidth(fontheight);
    show_browser->setFixedWidth(fontheight);
    show_controls->setFixedWidth(fontheight);
    */
    /*
    show_compressor->setFixedWidth(controlsArrow->width());
    show_equalizer->setFixedWidth(controlsArrow->width());
    show_browser->setFixedWidth(browserArrow->width());
    show_controls->setFixedWidth(controlsArrow->width());
    */
    

    updateSlider(EFFNUM_BUS1);
    updateSlider(EFFNUM_BUS2);
    updateSlider(EFFNUM_BUS3);
    updateSlider(EFFNUM_BUS4);
    updateSlider(EFFNUM_BUS5);
    updateSlider(EFFNUM_DRYWET);
    updateSlider(EFFNUM_INPUT_VOLUME);
    updateSlider(EFFNUM_VOLUME);
    updateSlider(EFFNUM_OUTPUT_VOLUME);
    updateSlider(EFFNUM_PAN);

    updateSlider(EFFNUM_LOWPASS_FREQ);
    updateSlider(EFFNUM_HIGHPASS_FREQ);
    updateSlider(EFFNUM_EQ1_FREQ);
    updateSlider(EFFNUM_EQ1_GAIN);
    updateSlider(EFFNUM_EQ2_FREQ);
    updateSlider(EFFNUM_EQ2_GAIN);
    updateSlider(EFFNUM_LOWSHELF_FREQ);
    updateSlider(EFFNUM_LOWSHELF_GAIN);
    updateSlider(EFFNUM_HIGHSHELF_FREQ);
    updateSlider(EFFNUM_HIGHSHELF_GAIN);
    updateSlider(EFFNUM_DELAY_TIME);

    updateChecked(input_volume_onoff, EFFNUM_INPUT_VOLUME_ONOFF);
    updateChecked(mute_button, EFFNUM_VOLUME_ONOFF);
    updateChecked(output_volume_onoff, EFFNUM_OUTPUT_VOLUME_ONOFF);

    Buses buses = MIXER_get_buses();
    updateBusChecked(bus1_onoff, bus1_slider, buses.bus1);
    updateBusChecked(bus2_onoff, bus2_slider, buses.bus2);
    updateBusChecked(bus3_onoff, bus3_slider, buses.bus3);
    updateBusChecked(bus4_onoff, bus4_slider, buses.bus4);
    updateBusChecked(bus5_onoff, bus5_slider, buses.bus5);
    
    updateChecked(panning_onoff, EFFNUM_PAN_ONOFF);
    updateChecked(bypass_button, EFFNUM_EFFECTS_ONOFF);
    updateChecked(solo_button, EFFNUM_SOLO_ONOFF);

    updateChecked(lowpass_onoff, EFFNUM_LOWPASS_ONOFF);
    updateChecked(highpass_onoff, EFFNUM_HIGHPASS_ONOFF);
    updateChecked(eq1_onoff, EFFNUM_EQ1_ONOFF);
    updateChecked(eq2_onoff, EFFNUM_EQ2_ONOFF);
    updateChecked(lowshelf_onoff, EFFNUM_LOWSHELF_ONOFF);
    updateChecked(highshelf_onoff, EFFNUM_HIGHSHELF_ONOFF);
    updateChecked(rightdelay_onoff, EFFNUM_DELAY_ONOFF);

    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    const SoundPluginType *type = plugin->type;

    for(int system_effect=EFFNUM_INPUT_VOLUME;system_effect<NUM_SYSTEM_EFFECTS;system_effect++){
      MyQSlider *slider = get_system_slider(system_effect);
      if(slider!=NULL){
        int effect_num = type->num_effects + system_effect;
        slider->_effect_num = effect_num;
        slider->_patch.set(_patch.data());
      }

      MyQCheckBox *checkbox = get_system_checkbox(system_effect);
      if(checkbox!=NULL){
        int effect_num = type->num_effects + system_effect;
        checkbox->_effect_num = effect_num;
        checkbox->_patch.set(_patch.data());
      }

      if(slider!=NULL && checkbox!=NULL)
        RError("weird");
    }

    update_all_ab_buttons();

#if 0 //ndef RELEASE
    static int checknum;
    //    PLAYER_lock();
    fprintf(stderr,"\n\n     STARTING CHECK %d\n",checknum);
        
    const radium::Vector2<SoundProducer*> *org_all_sp = MIXER_get_all_SoundProducers();
    //void *elements1=org_all_sp->elements;
    int size = org_all_sp->size();
    QVector<SoundProducer*> sps;

    for(int n=0;n<size;n++)
      //    for (auto *sp : *all_sp)
      sps.push_back(org_all_sp->at(n));//[n]);//->elements[n]);

    for(int i = 0 ; i < 50000 ; i ++){
      const radium::Vector2<SoundProducer*> *all_sp = MIXER_get_all_SoundProducers();
      //void *elements2=all_sp->elements;
      R_ASSERT(all_sp->size() == size);
      
      for(int n=0;n<size;n++){
        SoundProducer *sp1 = sps[n];
        //SoundProducer *sp2 = all_sp->at(n);//[n];//((SoundProducer**)elements1)[n];//org_all_sp->elements[n];
        SoundProducer *sp2 = all_sp->elements[n];//at(n);//[n];//((SoundProducer**)elements1)[n];//org_all_sp->elements[n];
        if (sp1 != sp2){
          SoundPlugin *p1 = SP_get_plugin(sp1);
          SoundPlugin *p2 = SP_get_plugin(sp2);
          fprintf(stderr,"org_all_sp: %p, all_sp: %p, sp1: %p, sp2: %p. plug1: %s, plug2: %s\n",sp1,sp2,sp1,sp2,p1==NULL?"(null)":p1->patch->name,p2==NULL?"(null)":p2->patch->name);
          abort();
        }
        R_ASSERT(sps[n]==all_sp->at(n));//elements[n]);
      }
      
      //SP_get_SoundProducer(plugin);
    }
    fprintf(stderr,"\n\n     FINISHING CHECK %d\n",checknum++);
    //    PLAYER_unlock();
#endif
    
    //int num_inputs = type->num_inputs;
    int num_outputs = type->num_outputs;

    if(num_outputs>0){
      bool effects_are_on = ATOMIC_GET(plugin->effects_are_on);
      
      input_volume_layout->setEnabled(effects_are_on);
      
      if(plugin->type->num_inputs>0)
        _plugin_widget->setEnabled(effects_are_on);
      
      filters_widget->setEnabled(effects_are_on);
      
      volume_widget->setEnabled(effects_are_on);
      
      if (root->song->include_pan_and_dry_in_wet_signal){
        bool pan_is_on = effects_are_on && PLUGIN_get_effect_value(plugin, type->num_effects + EFFNUM_PAN_ONOFF, VALUE_FROM_PLUGIN) >= 0.5f;
        bool width_is_on = effects_are_on && PLUGIN_get_effect_value(plugin, type->num_effects + EFFNUM_DELAY_ONOFF, VALUE_FROM_PLUGIN) >= 0.5f;
        
        panning_slider->setEnabled(pan_is_on);
        rightdelay_slider->setEnabled(width_is_on);

        panning_onoff->setEnabled(effects_are_on);
        rightdelay_onoff->setEnabled(effects_are_on);
      }
      
      drywet_slider->setEnabled(effects_are_on);
    }

    _comp_widget->update_gui();

    show_browser->setChecked(plugin->show_browser_gui);
    show_controls->setChecked(plugin->show_controls_gui);
    show_equalizer->setChecked(plugin->show_equalizer_gui);
    show_compressor->setChecked(plugin->show_compressor_gui);

    _patch_widget->updateWidgets();

    if(_sample_requester_widget != NULL){
      _sample_requester_widget->updateWidgets();
    }

    update();
  }

  void set_plugin_value(int sliderval, int system_effect){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    const SoundPluginType *type = plugin->type;
    int effect_num = type->num_effects + system_effect;

    if (is_starting==false)
      PLUGIN_set_effect_value(plugin, -1, effect_num, sliderval/10000.0f, STORE_VALUE, FX_single, EFFECT_FORMAT_SCALED);

    updateSliderString(system_effect);
  }

  void set_bus_value(SoundProducer *bus, bool val){
    if (is_starting==true)
      return;

    if (bus==NULL){
      R_ASSERT_NON_RELEASE(g_is_starting_up);
      return;
    }

    ADD_UNDO(MixerConnections_CurrPos());
    
    if (val)
      MW_connect(_patch.data(), const_cast<Patch*>(SP_get_plugin(bus)->patch), ConnectionType::IS_SEND);
    else
      MW_disconnect(_patch.data(), const_cast<Patch*>(SP_get_plugin(bus)->patch));
  }
  
private:
  
  QVector<QWidget*> hidden_widgets;
  int height_before_large;

  // enable for faust and multiband
  void show_large(SizeType new_size_type){
    printf("show_large\n");

    R_ASSERT_RETURN_IF_FALSE(new_size_type!=_size_type);
    R_ASSERT_RETURN_IF_FALSE(new_size_type!=SIZETYPE_NORMAL);

    if (_size_type==SIZETYPE_NORMAL){
      hidden_widgets.clear();

      height_before_large = height();
      
      for (int i = 0; i < effects_layout->count(); ++i){
        QWidget *widget = effects_layout->itemAt(i)->widget();
        if (widget != NULL){
          if (widget != _plugin_widget && widget != _sample_requester_widget && widget->isVisible()){
            widget->hide();
            hidden_widgets.push_back(widget);
        }
        }
      }
    }

    if (instrumentWidgetIsInMixer())
      MW_instrument_widget_set_size(this, _size_type, new_size_type);

    if (new_size_type==SIZETYPE_HALF) {

      if (!instrumentWidgetIsInMixer()) {
        int size = g_main_window->height() / 2;
        setMinimumHeight(size);
        setMaximumHeight(size);
      }
      
    } else {
      setMinimumHeight(0);
      setMaximumHeight(16777214);//g_main_window->height());
    }

    _size_type = new_size_type;
  }

  void show_small(void){
    printf("show_small\n");

    R_ASSERT_RETURN_IF_FALSE(_size_type!=SIZETYPE_NORMAL);

    if (instrumentWidgetIsInMixer())
      MW_instrument_widget_set_size(this, _size_type, SIZETYPE_NORMAL);
    
    _size_type=SIZETYPE_NORMAL;
    
    setMinimumHeight(10);
    setMaximumHeight(16777214);
    //resize(width(), height_before_large);
    
    for (auto *widget : hidden_widgets)
      widget->show();
    
    hidden_widgets.clear();
  }

  void hide_non_instrument_widgets(void){
    /*
    GFX_HideEditor();
    GFX_HideMixer();
    GFX_PlayListWindowToBack();
    */
    if (instrumentWidgetIsInMixer()){
      MW_hide_non_instrument_widgets();
    } else {
      hideUpperPartOfMainWindow();
    }

    MW_disable_include_instrument_checkbox();
  }

  void show_non_instrument_widgets(void){

    if (instrumentWidgetIsInMixer()){
      MW_show_non_instrument_widgets();
    } else {
      showUpperPartOfMainWindow();
    }

    MW_enable_include_instrument_checkbox();
    
    /*
    GFX_ShowEditor();
    GFX_ShowMixer();
    GFX_PlayListWindowToFront();
    */
  }

  
public:

  // Note: May be called from the destructor (new_size_type=SIZETYPE_NORMAL when called from the destructor)
  void change_height(SizeType new_size_type){
    if (new_size_type==_size_type)
      return;

    if (new_size_type==SIZETYPE_FULL)
      hide_non_instrument_widgets();    
    else if (_size_type == SIZETYPE_FULL)
      show_non_instrument_widgets();

    if (new_size_type==SIZETYPE_NORMAL)
      SEQUENCER_show_because_instrument_widget_is_large();
    else {
      if (!instrumentWidgetIsInMixer())
        SEQUENCER_hide_because_instrument_widget_is_large();
    }
    
    if (new_size_type==SIZETYPE_NORMAL)
      show_small();
    else
      show_large(new_size_type);
  }

  void hideEvent(QHideEvent * event) override {
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin != NULL)
      ATOMIC_SET(plugin->is_visible, false);
    
    _size_type_before_hidden = _size_type;
    
    if(_size_type != SIZETYPE_NORMAL)
      change_height(SIZETYPE_NORMAL);
      //show_small();  // If not, all instrument widgets will have large height.
  }
  
  void showEvent(QShowEvent * event) override {
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin != NULL)
      ATOMIC_SET(plugin->is_visible, true);
    
    if (_size_type_before_hidden != SIZETYPE_NORMAL)
      change_height(_size_type_before_hidden);
    //      show_large();
  }


  void update_all_ab_buttons(void){
    update_ab_button(ab0, 0);
    update_ab_button(ab1, 1);
    update_ab_button(ab2, 2);
    update_ab_button(ab3, 3);
    update_ab_button(ab4, 4);
    update_ab_button(ab5, 5);
    update_ab_button(ab6, 6);
    update_ab_button(ab7, 7);
    
    if(_plugin_widget != NULL)
      _plugin_widget->update_ab_buttons();
  }

  int _ab_checkbox_width = -1;

  void update_ab_button(QCheckBox *checkbox, int num){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

    QString c = QString("ABCDEFGH"[num]);

    bool is_selected=plugin->curr_ab_num==num || plugin->ab_is_valid[num];
    
    if (is_selected)
      checkbox->setText(c+"*");
    else
      checkbox->setText(c);

    if (!is_selected && _ab_checkbox_width==-1){
      checkbox->adjustSize();
      _ab_checkbox_width = checkbox->width();
    }

    if (_ab_checkbox_width != -1){
      checkbox->setMinimumWidth(_ab_checkbox_width);
      checkbox->setMaximumWidth(_ab_checkbox_width);
    }
  }

  MyQCheckBox *get_ab_checkbox(int num){
    if (num<0 || num >= 8){
      R_ASSERT(false);
      num=0;
    }
    MyQCheckBox *array[] = {ab0,ab1,ab2,ab3,ab4,ab5,ab6,ab7};
    return array[num];
  }
  
  bool arrgh = false;
  
  void ab_pressed(int num, bool val){
    MyQCheckBox *checkbox = get_ab_checkbox(num);
    
    if (arrgh==false) {

      arrgh=true;
      
      //printf("   AB_PRESSED. Num: %d. Val: %d\n", num, val);

      if (val==true){

        SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

        PLUGIN_change_ab(plugin, num);

        if (_plugin_widget != NULL){
          _plugin_widget->_ignore_checkbox_stateChanged = true; { // Note to Qt developers: 'setChecked' should have an optional arguments whether to send signal to callbacks.
            
            _plugin_widget->ab_a->setChecked(num==0);
            _plugin_widget->ab_b->setChecked(num==1);
            /*          
                        ab0->setChecked(num==0);
                        ab1->setChecked(num==1);
                        ab2->setChecked(num==2);
                        ab3->setChecked(num==3);
                        ab4->setChecked(num==4);
                        ab5->setChecked(num==5);
                        ab6->setChecked(num==6);
                        ab7->setChecked(num==7);
            */
          }_plugin_widget->_ignore_checkbox_stateChanged = false;      
        }
        
      }

      update_ab_button(checkbox, num);

      arrgh=false;
    }
    
  }

  void ab_rightclicked(int num){
    if (get_ab_checkbox(num)->_last_pressed_button==Qt::RightButton){
      _plugin_widget->ab_rightclicked(num);
      update_all_ab_buttons();
    }
  }

public slots:

  void on_ab0_clicked(){ab_rightclicked(0);}
  void on_ab1_clicked(){ab_rightclicked(1);}
  void on_ab2_clicked(){ab_rightclicked(2);}
  void on_ab3_clicked(){ab_rightclicked(3);}
  void on_ab4_clicked(){ab_rightclicked(4);}
  void on_ab5_clicked(){ab_rightclicked(5);}
  void on_ab6_clicked(){ab_rightclicked(5);}
  void on_ab7_clicked(){ab_rightclicked(6);}

  void on_ab_reset_button_clicked(){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    PLUGIN_reset_ab(plugin,-1);
    update_all_ab_buttons();
  }

  void on_ab0_toggled(bool val){ab_pressed(0, val);}
  void on_ab1_toggled(bool val){ab_pressed(1, val);}
  void on_ab2_toggled(bool val){ab_pressed(2, val);}
  void on_ab3_toggled(bool val){ab_pressed(3, val);}
  void on_ab4_toggled(bool val){ab_pressed(4, val);}
  void on_ab5_toggled(bool val){ab_pressed(5, val);}
  void on_ab6_toggled(bool val){ab_pressed(6, val);}
  void on_ab7_toggled(bool val){ab_pressed(7, val);}
  
  
  
#if 0
  void on_arrow1_toggled(bool val){
    _plugin_widget->setVisible(val);
  }

  void on_arrow2_toggled(bool val){
    filters_widget->setVisible(val);
  }

  void on_arrow3_toggled(bool val){
    outputs_widget1->setVisible(val);
    outputs_widget2->setVisible(val);
  }
#endif

  void on_show_browser_toggled(bool val){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    const SoundPluginType *type = plugin->type;
    int effect_num = type->num_effects + EFFNUM_BROWSER_SHOW_GUI;
    PLUGIN_set_effect_value(plugin, -1, effect_num, val==true?1.0:0.0, STORE_VALUE, FX_single, EFFECT_FORMAT_SCALED);
    if(_sample_requester_widget!=NULL)
      _sample_requester_widget->setVisible(val);
  }

  void on_show_controls_toggled(bool val){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    const SoundPluginType *type = plugin->type;
    int effect_num = type->num_effects + EFFNUM_CONTROLS_SHOW_GUI;
    PLUGIN_set_effect_value(plugin, -1, effect_num, val==true?1.0:0.0, STORE_VALUE, FX_single, EFFECT_FORMAT_SCALED);
    _plugin_widget->setVisible(val);
    spacer_holder->setVisible(!val);
  }

  void on_show_equalizer_toggled(bool val){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    const SoundPluginType *type = plugin->type;
    int effect_num = type->num_effects + EFFNUM_EQ_SHOW_GUI;
    PLUGIN_set_effect_value(plugin, -1, effect_num, val==true?1.0:0.0, STORE_VALUE, FX_single, EFFECT_FORMAT_SCALED);
    filters_widget->setVisible(val);
  }

  void on_show_compressor_toggled(bool val){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    const SoundPluginType *type = plugin->type;
    int effect_num = type->num_effects + EFFNUM_COMP_SHOW_GUI;
    PLUGIN_set_effect_value(plugin, -1, effect_num, val==true?1.0:0.0, STORE_VALUE, FX_single, EFFECT_FORMAT_SCALED);
    _comp_widget->setVisible(val);
  }

  void on_input_volume_onoff_toggled(bool val){
    set_plugin_value(val==true ? 10000 : 0, EFFNUM_INPUT_VOLUME_ONOFF);
    input_volume_slider->setEnabled(val);
  }

  void on_mute_button_toggled(bool val){
    set_plugin_value(val==false ? 10000 : 0, EFFNUM_VOLUME_ONOFF);
    //volume_slider->setEnabled(!val);
    CHIP_update((SoundPlugin*)_patch->patchdata);
  }

  void on_output_volume_onoff_toggled(bool val){
    set_plugin_value(val==true ? 10000 : 0, EFFNUM_OUTPUT_VOLUME_ONOFF);
    output_volume_slider->setEnabled(val);
  }

  
  void on_bus1_onoff_toggled(bool val){
    set_bus_value(MIXER_get_buses().bus1, val);
    bus1_slider->setEnabled(val);
  }
  void on_bus2_onoff_toggled(bool val){
    set_bus_value(MIXER_get_buses().bus2, val);
    bus2_slider->setEnabled(val);
  }
  void on_bus3_onoff_toggled(bool val){
    set_bus_value(MIXER_get_buses().bus3, val);
    bus3_slider->setEnabled(val);
  }
  void on_bus4_onoff_toggled(bool val){
    set_bus_value(MIXER_get_buses().bus4, val);
    bus4_slider->setEnabled(val);
  }
  void on_bus5_onoff_toggled(bool val){
    set_bus_value(MIXER_get_buses().bus5, val);
    bus5_slider->setEnabled(val);
  }

  
  void on_bus1_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_BUS1);
  }
  void on_bus2_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_BUS2);
  }
  void on_bus3_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_BUS3);
  }
  void on_bus4_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_BUS4);
  }
  void on_bus5_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_BUS5);
  }

  // effects onoff / dry_wet

  void on_bypass_button_toggled(bool val){
    if(is_starting==false)
      setInstrumentBypass(_patch->id, val);
    /*
    SoundPlugin *plugin = (SoundPlugin*)(_patch->patchdata);
        
    if (doUndoBypass()){
      const SoundPluginType *type = plugin->type;
      ADD_UNDO(AudioEffect_CurrPos(_patch.data(), type->num_effects + EFFNUM_EFFECTS_ONOFF));
    }
    
    set_plugin_value(val==false ? 10000 : 0, EFFNUM_EFFECTS_ONOFF);
    drywet_slider->setEnabled(!val);
    CHIP_update(plugin);
    updateWidgets();
    */
  }

  void on_solo_button_toggled(bool val){
    if(is_starting==false)
      setInstrumentSolo(_patch->id, val);
  }

  void on_drywet_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_DRYWET);
  }

  void on_input_volume_slider_valueChanged(int val){
    if(_patch->patchdata != NULL) { // temp fix
      set_plugin_value(val, EFFNUM_INPUT_VOLUME);

      if (GFX_OS_patch_is_system_out(_patch.data()))
        OS_GFX_SetVolume(val);

      CHIP_update((SoundPlugin*)_patch->patchdata);
    }
  }

  void on_volume_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_VOLUME);
    CHIP_update((SoundPlugin*)_patch->patchdata);
  }

  void on_output_volume_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_OUTPUT_VOLUME);
  }

  // lowpass 
  void on_lowpass_onoff_toggled(bool val){
    set_plugin_value(val==true ? 10000 : 0, EFFNUM_LOWPASS_ONOFF);
    lowpass_freq_slider->setEnabled(val);
  }

  void on_lowpass_freq_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_LOWPASS_FREQ);
  }

  // highpass 
  void on_highpass_onoff_toggled(bool val){
    set_plugin_value(val==true ? 10000 : 0, EFFNUM_HIGHPASS_ONOFF);
    highpass_freq_slider->setEnabled(val);
  }

  void on_highpass_freq_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_HIGHPASS_FREQ);
  }

  // eq1
  void on_eq1_onoff_toggled(bool val){
    set_plugin_value(val==true ? 10000 : 0, EFFNUM_EQ1_ONOFF);
    eq1_freq_slider->setEnabled(val);
    eq1_gain_slider->setEnabled(val);
  }

  void on_eq1_freq_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_EQ1_FREQ);
  }

  void on_eq1_gain_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_EQ1_GAIN);
  }

  // eq2
  void on_eq2_onoff_toggled(bool val){
    set_plugin_value(val==true ? 10000 : 0, EFFNUM_EQ2_ONOFF);
    eq2_freq_slider->setEnabled(val);
    eq2_gain_slider->setEnabled(val);
  }

  void on_eq2_freq_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_EQ2_FREQ);
  }

  void on_eq2_gain_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_EQ2_GAIN);
  }

  // lowshelf
  void on_lowshelf_onoff_toggled(bool val){
    set_plugin_value(val==true ? 10000 : 0, EFFNUM_LOWSHELF_ONOFF);
    lowshelf_freq_slider->setEnabled(val);
    lowshelf_gain_slider->setEnabled(val);
  }

  void on_lowshelf_freq_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_LOWSHELF_FREQ);
  }

  void on_lowshelf_gain_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_LOWSHELF_GAIN);
  }

  // highshelf
  void on_highshelf_onoff_toggled(bool val){
    set_plugin_value(val==true ? 10000 : 0, EFFNUM_HIGHSHELF_ONOFF);
    highshelf_freq_slider->setEnabled(val);
    highshelf_gain_slider->setEnabled(val);
  }

  void on_highshelf_freq_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_HIGHSHELF_FREQ);
  }

  void on_highshelf_gain_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_HIGHSHELF_GAIN);
  }

  // rightdelay
  void on_rightdelay_onoff_toggled(bool val){
    set_plugin_value(val==true ? 10000 : 0, EFFNUM_DELAY_ONOFF);
    rightdelay_slider->setEnabled(val);
  }

  void on_rightdelay_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_DELAY_TIME);
  }

#if 0
  void on_rightdelay_slider_sliderPressed(){
    rightdelay_slider->setEnabled(false);
    printf("Turning off slider\n");
  }

  void on_rightdelay_slider_sliderReleased(){
    rightdelay_slider->setEnabled(true);
    printf("Turning on slider\n");
  }
#endif

  // Volume

#if 0
  // Velocity

  void on_velocity_slider_valueChanged(int val)
  {
    _patch->standardvel = val*MAX_VELOCITY/10000;
  }
#endif


#if 0
  void on_velocity_onoff_stateChanged( int val)
  {
    if (val==Qt::Checked){
      velocity_slider->setEnabled(true);
      velocity_spin->setEnabled(true);;
    }else if(val==Qt::Unchecked){
      velocity_slider->setEnabled(false);
      velocity_spin->setEnabled(false);
    }
  }
#endif

  // Panning

  void on_panning_slider_valueChanged( int val)
  {
    set_plugin_value(val, EFFNUM_PAN);
  }

  void on_panning_onoff_toggled(bool val)
  {
    set_plugin_value(val==true ? 10000 : 0, EFFNUM_PAN_ONOFF);
    panning_slider->setEnabled(val);
  }
};

}

 
/*
static Pd_Plugin_widget *AUDIOWIDGET_get_pd_plugin_widget(Audio_instrument_widget *audio_instrument_widget){
  return audio_instrument_widget->_plugin_widget->_pd_plugin_widget;
}
*/

#ifdef WITH_FAUST_DEV
static Faust_Plugin_widget *AUDIOWIDGET_get_faust_plugin_widget(Audio_instrument_widget *audio_instrument_widget){
  return audio_instrument_widget->_plugin_widget->_faust_plugin_widget;
}
#endif

Sample_requester_widget *AUDIOWIDGET_get_sample_requester_widget(struct Patch *patch){
  return get_audio_instrument_widget(patch)->_sample_requester_widget;
}

void AUDIOWIDGET_change_height(struct Patch *patch, SizeType type){
  get_audio_instrument_widget(patch)->change_height(type);
}

/*
SizeType AUDIOWIDGET_get_size_type(struct Patch *patch){
  return get_audio_instrument_widget(patch)->_size_type;
}
*/

void AUDIOWIDGET_set_ab(struct Patch *patch, int ab_num){
  auto *w = get_audio_instrument_widget(patch);

  switch(ab_num){
    case 0: w->ab0->setChecked(true); break;
    case 1: w->ab1->setChecked(true); break;
    case 2: w->ab2->setChecked(true); break;
    case 3: w->ab3->setChecked(true); break;
    case 4: w->ab4->setChecked(true); break;
    case 5: w->ab5->setChecked(true); break;
    case 6: w->ab6->setChecked(true); break;
    case 7: w->ab7->setChecked(true); break;
  }
}

void AUDIOWIDGET_redraw_ab(struct Patch *patch){
  get_audio_instrument_widget(patch)->update_all_ab_buttons();
}
  
#if 0
void AUDIOWIDGET_show_large(struct Patch *patch){
  get_audio_instrument_widget(patch)->show_large();
}

void AUDIOWIDGET_show_small(struct Patch *patch){
  get_audio_instrument_widget(patch)->show_small();
}
#endif

