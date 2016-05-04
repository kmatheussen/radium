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


#include "../audio/SoundPlugin_proc.h"
#include "../audio/SoundPluginRegistry_proc.h"

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

class Audio_instrument_widget : public QWidget, public Ui::Audio_instrument_widget{
  Q_OBJECT;

public:

  bool is_starting;
  
  struct Patch *_patch;

  Patch_widget *_patch_widget;
  Plugin_widget *_plugin_widget;
  
  Sample_requester_widget *_sample_requester_widget;
  Compressor_widget *_comp_widget;

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
  {
    setupUi(this);    

    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

    _patch_widget = new Patch_widget(this,patch);
    voiceBox_layout->insertWidget(0,_patch_widget);
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

    /*
    if(!strcmp(plugin->type->type_name,"Jack") && !strcmp(plugin->type->name,"System Out")){
      _i_am_system_out = true;
      BottomBar_set_system_audio_instrument_widget_and_patch(this, _patch);
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
    _plugin_widget=PluginWidget_create(NULL, _patch);
    if(_plugin_widget->_param_widgets.size() > 0){
      delete(pipe_label);
      pipe_label = NULL;
    }
#else
    _plugin_widget = new Plugin_widget(this,_patch);

    delete(pipe_label);
    pipe_label = NULL;
#endif

    effects_layout->insertWidget(3,_plugin_widget);
    _plugin_widget->setVisible(plugin->show_controls_gui);
    spacer_holder->setVisible(!plugin->show_controls_gui);
    

    if(plugin->type==PR_get_plugin_type_by_name(NULL, "Sample Player","Sample Player") || plugin->type==PR_get_plugin_type_by_name(NULL, "Sample Player","Click") || plugin->type==PR_get_plugin_type_by_name(NULL, "FluidSynth","FluidSynth")){
      _sample_requester_widget = new Sample_requester_widget(this, _patch_widget->name_widget, _plugin_widget->sample_name_label, _patch);
      effects_layout->insertWidget(2,_sample_requester_widget);
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
      drywet_slider->setMinimumWidth(width);
    }


#if 0
    if(plugin->type->num_inputs==0){
      input_volume_layout->setDisabled(true);
    }
#endif


    // set the scroll bar itself to size 10.
    scrollArea->horizontalScrollBar()->setFixedHeight(10);

    updateWidgets();
    setupPeakAndAutomationStuff();

    is_starting = false;
  }

  void prepare_for_deletion(void){
    _comp_widget->prepare_for_deletion();
    _plugin_widget->prepare_for_deletion();
  }

  ~Audio_instrument_widget(){
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

    case EFFNUM_DRYWET:
      return drywet_slider; 

    case EFFNUM_PAN:
      return panning_slider; 

    case EFFNUM_LOWPASS_FREQ:
      return lowpass_freq_slider; 

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
      return volume_onoff;
    case EFFNUM_OUTPUT_VOLUME_ONOFF:
      return output_volume_onoff;
    case EFFNUM_BUS1_ONOFF:
      return bus1_onoff;
    case EFFNUM_BUS2_ONOFF:
      return bus2_onoff;
    case EFFNUM_PAN_ONOFF:
      return panning_onoff;
    case EFFNUM_EFFECTS_ONOFF:
      return effects_onoff;
    case EFFNUM_LOWPASS_ONOFF:
      return lowpass_onoff;

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

    char buf[64]={0};
    PLUGIN_get_display_value_string(plugin, effect_num, buf, 64);
    if(system_effect==EFFNUM_DRYWET)
      SLIDERPAINTER_set_string(slider->_painter, QString(buf));
    else if(system_effect==EFFNUM_BUS1)
      SLIDERPAINTER_set_string(slider->_painter, QString(BUS_get_bus_name(0)) + ": " + QString(buf));
    else if(system_effect==EFFNUM_BUS2)
      SLIDERPAINTER_set_string(slider->_painter, QString(BUS_get_bus_name(1)) + ": " + QString(buf));
    else
      SLIDERPAINTER_set_string(slider->_painter, QString(PLUGIN_get_effect_name(plugin, effect_num)+strlen("System ")) + ": " + QString(buf));
      
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

    bool val = PLUGIN_get_effect_value(plugin,effect_num,VALUE_FROM_STORAGE) > 0.5f;

    checkwidget->setChecked(val);

    show_compressor->setFixedWidth(controlsArrow->width());
    show_equalizer->setFixedWidth(controlsArrow->width());
    show_browser->setFixedWidth(browserArrow->width());
    show_controls->setFixedWidth(controlsArrow->width());

    if(system_effect==EFFNUM_INPUT_VOLUME_ONOFF)
      input_volume_slider->setEnabled(val);
    if(system_effect==EFFNUM_VOLUME_ONOFF)
      volume_slider->setEnabled(val);
    if(system_effect==EFFNUM_OUTPUT_VOLUME_ONOFF)
      output_volume_slider->setEnabled(val);
    if(system_effect==EFFNUM_BUS1_ONOFF)
      bus1_slider->setEnabled(val);
    if(system_effect==EFFNUM_BUS2_ONOFF)
      bus2_slider->setEnabled(val);

    if(system_effect==EFFNUM_LOWPASS_ONOFF)
      lowpass_freq_slider->setEnabled(val);
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


  void calledRegularlyByParent(void){
    
    //printf("hello %p\n", this);

    if (_plugin_widget->isVisible())
      _plugin_widget->calledRegularlyByParent();
    
    callSliderpainterUpdateCallbacks();
    
    if (_comp_widget->isVisible())
      _comp_widget->calledRegularlyByParent();

    adjustWidgetHeight();
  }

  // horror
  void adjustWidgetHeight(void){
    static bool shrinking = false;
    static int num_times_horizontal_is_not_visible;
    static bool can_shrink = true;
    static bool last_time_shrank = false;

    bool is_visible = scrollArea->verticalScrollBar()->isVisible();
    
    if (scrollArea->horizontalScrollBar()->isVisible())
      num_times_horizontal_is_not_visible=0;
    else
      num_times_horizontal_is_not_visible++;
    
    if (is_visible){
      if (last_time_shrank)
        can_shrink = false;
      else
        can_shrink = true;
      setMinimumHeight(height()+1);
      //printf(" adjust 1\n");
      shrinking = false;
    } else if (is_visible==false && num_times_horizontal_is_not_visible>50 && can_shrink==true){
      shrinking = true;
    }
    
    if (shrinking){
      int old_size = minimumHeight();
      int new_size = old_size-1;
      if(new_size > 50){
        setMinimumHeight(new_size);
        //printf(" adjust 2\n");
      }
      last_time_shrank = true;
    }else
      last_time_shrank = false;
    
  }
                                      
  void callSliderpainterUpdateCallbacks(void){
    
    for(int system_effect=0 ; system_effect<NUM_SYSTEM_EFFECTS ; system_effect++){
      MyQSlider *slider = get_system_slider(system_effect);
      if (slider != NULL) // effect can be a checkbox.
        SLIDERPAINTER_call_regularly(slider->_painter);
    }

    if (_plugin_widget->_plugin_widget != NULL){

      bool has_been_visible = false;
      
      for(ParamWidget *paramWidget : _plugin_widget->_plugin_widget->_param_widgets){
        MyQSlider *slider = paramWidget->_slider;
        if (slider != NULL){
          bool is_visible = slider->isVisible();
          if (is_visible==true)
            has_been_visible = true;
          
          if (is_visible==false && has_been_visible ==true) // Optimize a bit. some vst plugins have thousands of parameters.
            break;

          if (is_visible) {
            //printf(" redraing effect %d\n", paramWidget->_effect_num);
            SLIDERPAINTER_call_regularly(slider->_painter);
          }
        }
      }
    }
    
    if (_plugin_widget->_pd_plugin_widget != NULL){
      
      for(unsigned int i=0; i<_plugin_widget->_pd_plugin_widget->_controllers.size(); i++) {
        Pd_Controller_widget *c = _plugin_widget->_pd_plugin_widget->_controllers[i];
        
        MyQSlider *slider = c->value_slider;
        if (slider != NULL){
          SLIDERPAINTER_call_regularly(slider->_painter);
        }
      }
    }
  }

  void setupPeakAndAutomationStuff(void){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    const SoundPluginType *type = plugin->type;

    // peaks
    
    int num_inputs = type->num_inputs;
    int num_outputs = type->num_outputs;

    if(num_outputs>0){
      SLIDERPAINTER_set_peak_value_pointers(volume_slider->_painter, num_outputs, plugin->volume_peak_values);

      SLIDERPAINTER_set_peak_value_pointers(output_volume_slider->_painter, num_outputs, plugin->output_volume_peak_values);

      SLIDERPAINTER_set_peak_value_pointers(bus1_slider->_painter,2, plugin->bus_volume_peak_values0);

      SLIDERPAINTER_set_peak_value_pointers(bus2_slider->_painter,2, plugin->bus_volume_peak_values1);
    }

    if(num_inputs>0 || num_outputs>0){//plugin->input_volume_peak_values==NULL){
      if(num_inputs>0)
        SLIDERPAINTER_set_peak_value_pointers(input_volume_slider->_painter, num_inputs, plugin->input_volume_peak_values);
      else
        SLIDERPAINTER_set_peak_value_pointers(input_volume_slider->_painter, num_outputs, plugin->input_volume_peak_values);
    }

    // automation

    for(int system_effect=0 ; system_effect<NUM_SYSTEM_EFFECTS ; system_effect++){
      int effect_num = type->num_effects + system_effect;
      
      MyQSlider *slider = get_system_slider(system_effect);
      if (slider != NULL) // effect can be a checkbox.
        SLIDERPAINTER_set_automation_value_pointer(slider->_painter, get_effect_color(plugin, effect_num), &plugin->automation_values[effect_num]);
    }

    if (_plugin_widget->_plugin_widget != NULL){
    
      for(ParamWidget *paramWidget : _plugin_widget->_plugin_widget->_param_widgets){
        int effect_num = paramWidget->_effect_num;
        
        MyQSlider *slider = paramWidget->_slider;
        if (slider != NULL){
          SLIDERPAINTER_set_automation_value_pointer(slider->_painter, get_effect_color(plugin, effect_num), &plugin->automation_values[effect_num]);
        }
      }
    }
    
    if (_plugin_widget->_pd_plugin_widget != NULL){

      for(unsigned int i=0; i<_plugin_widget->_pd_plugin_widget->_controllers.size(); i++) {
        Pd_Controller_widget *c = _plugin_widget->_pd_plugin_widget->_controllers[i];

        MyQSlider *slider = c->value_slider;
        if (slider != NULL){
          int effect_num = slider->_effect_num;
          SLIDERPAINTER_set_automation_value_pointer(slider->_painter, get_effect_color(plugin, effect_num), &plugin->automation_values[effect_num]);
        }
      }
    }
  }
  
  void updateWidgets(){
    set_arrow_style(controlsArrow, false);
    set_arrow_style(arrow2, false);
    set_arrow_style(arrow3);
    set_arrow_style(arrow4);
    set_arrow_style(arrow5);
    set_arrow_style(arrow6);
    set_arrow_style(arrow7, false);
    set_arrow_style(browserArrow, false);
    if(pipe_label!=NULL)
      set_arrow_style(pipe_label,false);

    updateSlider(EFFNUM_BUS1);
    updateSlider(EFFNUM_BUS2);
    updateSlider(EFFNUM_DRYWET);
    updateSlider(EFFNUM_INPUT_VOLUME);
    updateSlider(EFFNUM_VOLUME);
    updateSlider(EFFNUM_OUTPUT_VOLUME);
    updateSlider(EFFNUM_PAN);

    updateSlider(EFFNUM_LOWPASS_FREQ);
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
    updateChecked(volume_onoff, EFFNUM_VOLUME_ONOFF);
    updateChecked(output_volume_onoff, EFFNUM_OUTPUT_VOLUME_ONOFF);
    updateChecked(bus1_onoff, EFFNUM_BUS1_ONOFF);
    updateChecked(bus2_onoff, EFFNUM_BUS2_ONOFF);

    updateChecked(panning_onoff, EFFNUM_PAN_ONOFF);
    updateChecked(effects_onoff, EFFNUM_EFFECTS_ONOFF);

    updateChecked(lowpass_onoff, EFFNUM_LOWPASS_ONOFF);
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
        slider->_patch = _patch;
      }

      MyQCheckBox *checkbox = get_system_checkbox(system_effect);
      if(checkbox!=NULL){
        int effect_num = type->num_effects + system_effect;
        checkbox->_effect_num = effect_num;
        checkbox->_patch = _patch;
      }

      if(slider!=NULL && checkbox!=NULL)
        RError("weird");
    }


    bus1_widget->setEnabled(SP_get_bus_descendant_type(SP_get_SoundProducer(plugin))==IS_BUS_PROVIDER);
    bus2_widget->setEnabled(SP_get_bus_descendant_type(SP_get_SoundProducer(plugin))==IS_BUS_PROVIDER);

    //int num_inputs = type->num_inputs;
    int num_outputs = type->num_outputs;

    if(num_outputs>0){
      input_volume_layout->setEnabled(ATOMIC_GET(plugin->effects_are_on));
      if(plugin->type->num_inputs>0)
        _plugin_widget->setEnabled(ATOMIC_GET(plugin->effects_are_on));
      filters_widget->setEnabled(ATOMIC_GET(plugin->effects_are_on));
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

  }

  void set_plugin_value(int sliderval, int system_effect){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    const SoundPluginType *type = plugin->type;
    int effect_num = type->num_effects + system_effect;

    if (is_starting==false)
      PLUGIN_set_effect_value(plugin, -1, effect_num, sliderval/10000.0f, PLUGIN_NONSTORED_TYPE, PLUGIN_STORE_VALUE, FX_single); // Don't need to lock player for setting system effects, I think.

    updateSliderString(system_effect);
  }

public slots:

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
    PLUGIN_set_effect_value(plugin, -1, effect_num, val==true?1.0:0.0, PLUGIN_NONSTORED_TYPE, PLUGIN_STORE_VALUE, FX_single);
    if(_sample_requester_widget!=NULL)
      _sample_requester_widget->setVisible(val);
  }

  void on_show_controls_toggled(bool val){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    const SoundPluginType *type = plugin->type;
    int effect_num = type->num_effects + EFFNUM_CONTROLS_SHOW_GUI;
    PLUGIN_set_effect_value(plugin, -1, effect_num, val==true?1.0:0.0, PLUGIN_NONSTORED_TYPE, PLUGIN_STORE_VALUE, FX_single);
    _plugin_widget->setVisible(val);
    spacer_holder->setVisible(!val);
  }

  void on_show_equalizer_toggled(bool val){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    const SoundPluginType *type = plugin->type;
    int effect_num = type->num_effects + EFFNUM_EQ_SHOW_GUI;
    PLUGIN_set_effect_value(plugin, -1, effect_num, val==true?1.0:0.0, PLUGIN_NONSTORED_TYPE, PLUGIN_STORE_VALUE, FX_single);
    filters_widget->setVisible(val);
  }

  void on_show_compressor_toggled(bool val){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    const SoundPluginType *type = plugin->type;
    int effect_num = type->num_effects + EFFNUM_COMP_SHOW_GUI;
    PLUGIN_set_effect_value(plugin, -1, effect_num, val==true?1.0:0.0, PLUGIN_NONSTORED_TYPE, PLUGIN_STORE_VALUE, FX_single);
    _comp_widget->setVisible(val);
  }

  void on_input_volume_onoff_toggled(bool val){
    set_plugin_value(val==true ? 10000 : 0, EFFNUM_INPUT_VOLUME_ONOFF);
    input_volume_slider->setEnabled(val);
  }

  void on_volume_onoff_toggled(bool val){
    set_plugin_value(val==true ? 10000 : 0, EFFNUM_VOLUME_ONOFF);
    volume_slider->setEnabled(val);
    CHIP_update((SoundPlugin*)_patch->patchdata);
  }

  void on_output_volume_onoff_toggled(bool val){
    set_plugin_value(val==true ? 10000 : 0, EFFNUM_OUTPUT_VOLUME_ONOFF);
    output_volume_slider->setEnabled(val);
  }

  void on_bus1_onoff_toggled(bool val){
    set_plugin_value(val==true ? 10000 : 0, EFFNUM_BUS1_ONOFF);
    bus1_slider->setEnabled(val);
  }

  void on_bus2_onoff_toggled(bool val){
    set_plugin_value(val==true ? 10000 : 0, EFFNUM_BUS2_ONOFF);
    bus2_slider->setEnabled(val);
  }

  void on_bus1_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_BUS1);
  }

  void on_bus2_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_BUS2);
  }

  // effects onoff / dry_wet

  void on_effects_onoff_toggled(bool val){
    set_plugin_value(val==true ? 10000 : 0, EFFNUM_EFFECTS_ONOFF);
    drywet_slider->setEnabled(val);
    CHIP_update((SoundPlugin*)_patch->patchdata);
    updateWidgets();
  }

  void on_drywet_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_DRYWET);
  }

  void on_input_volume_slider_valueChanged(int val){
    if(_patch->patchdata != NULL) { // temp fix
      set_plugin_value(val, EFFNUM_INPUT_VOLUME);

      if (GFX_OS_patch_is_system_out(_patch))
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

/*
static Pd_Plugin_widget *AUDIOWIDGET_get_pd_plugin_widget(Audio_instrument_widget *audio_instrument_widget){
  return audio_instrument_widget->_plugin_widget->_pd_plugin_widget;
}
*/
