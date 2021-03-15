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
#include "../common/sequencer_proc.h"

#include "../audio/SoundPlugin.h"
#include "../audio/SoundPlugin_proc.h"
#include "../audio/SoundProducer_proc.h"
#include "../audio/CpuUsage.hpp"

#include "../Qt/Qt_MyQButton.h"
#include "../Qt/Qt_MyQSpinBox.h"
#include <QTimer>

#ifdef USE_QT5
#include <QStyleFactory>
#else
#include <QCleanlooksStyle>
#endif

#include "FocusSniffers.h"

#include "Qt_instruments_proc.h"

#include "Qt_audio_instrument_widget.h"

#include "Qt_bottom_bar_widget.h"

#include "Qt_bottom_bar_widget_proc.h"


class Bottom_bar_widget;

//static QVector<Bottom_bar_widget*> g_bottom_bars;
static Ui::Audio_instrument_widget *g_system_audio_instrument_widget = NULL;
static struct Patch *g_system_out_patch = NULL;
static struct SoundPlugin *g_system_out_plugin = NULL;

extern bool drunk_velocity;

extern int scrolls_per_second;
extern int default_scrolls_per_second;

class Bottom_bar_widget : public QWidget, public Ui::Bottom_bar_widget {
  Q_OBJECT

  bool has_midi_learn = false;

  void frequent_timer_callback(void){

    if (_include_navigator && _navigator_widget==NULL) {
      
      _navigator_widget = SEQUENCER_create_navigator_widget();
      
      if (_navigator_widget !=NULL) {
        horizontalLayout_2->insertWidget(3, _navigator_widget);
        _navigator_widget->show();
      }
      
    }


    if (this->edit_onoff->isChecked() != ATOMIC_GET(root->editonoff))
      this->edit_onoff->setChecked(ATOMIC_GET(root->editonoff));

    bool clickonoff = ATOMIC_GET_RELAXED(root->clickonoff);
    if (this->click_onoff->isChecked() != clickonoff)
      this->click_onoff->setChecked(clickonoff);
    
    if (this->play_cursor_onoff->isChecked() != ATOMIC_GET(root->play_cursor_onoff))
      this->play_cursor_onoff->setChecked(ATOMIC_GET(root->play_cursor_onoff));
    
    if (this->editor_follows_play_cursor_onoff->isChecked() != ATOMIC_GET(root->editor_follows_play_cursor_onoff))
      this->editor_follows_play_cursor_onoff->setChecked(ATOMIC_GET(root->editor_follows_play_cursor_onoff));

    if (g_system_out_plugin != NULL){
      const SoundPluginType *type = g_system_out_plugin->type;
      int effect_num = type->num_effects+EFFNUM_INPUT_VOLUME;
      float val = PLUGIN_get_effect_value(g_system_out_plugin, effect_num, VALUE_FROM_STORAGE);
      this->_triggered_by_user = false;
      this->system_volume_slider->setValue(val * 10000);
      this->_triggered_by_user = true;
        
      bool has_midi_learn_now = PLUGIN_has_midi_learn(g_system_out_plugin, effect_num);
      if (has_midi_learn_now != has_midi_learn){
        //printf("hepp %d\n",has_midi_learn_now);
        has_midi_learn = has_midi_learn_now;
        this->set_volume_slider_string();
        this->system_volume_slider->update();
      }
      
      //SLIDERPAINTER_set_recording_color(this->system_volume_slider->_painter, PLUGIN_is_recording_automation(g_system_out_plugin, effect_num));
    }

    this->system_volume_slider->_patch.set(g_system_out_patch);
    
    if (this->system_volume_slider->_patch.data() != NULL)
      this->system_volume_slider->calledRegularlyByParent();
    
    //SLIDERPAINTER_call_regularly(this->system_volume_slider->_painter);
  }
  
  struct Timer2 : public QTimer{
    
    void timerEvent(QTimerEvent * e) override {

      RETURN_IF_DATA_IS_INACCESSIBLE();

      if (g_system_out_patch==NULL && g_system_out_plugin != NULL)
        g_system_out_patch = (struct Patch*)g_system_out_plugin->patch;
      
      if (g_system_audio_instrument_widget == NULL && g_system_out_patch != NULL) {
        
        g_system_audio_instrument_widget = InstrumentWidget_get_audio_instrument_widget(g_system_out_patch);
        
        // Commented out. We poll the plugin instead. (see below)
        //if (g_system_audio_instrument_widget != NULL)
        //  this->system_volume_slider->setValue(g_system_audio_instrument_widget->input_volume_slider->value());
      }
    
      for(auto *bottom_bar_widget : g_bottom_bars)
        bottom_bar_widget->frequent_timer_callback();
    }
  };

 public:
  bool _initing;
  Timer2 _timer2;

  bool _triggered_by_user;
  bool _include_navigator;
  
  Bottom_bar_widget(QWidget *parent, bool include_navigator)
    : QWidget(parent)
    , _triggered_by_user(true)
    , _include_navigator(include_navigator)
  {
    _initing = true;
    setupUi(this);
#ifdef USE_QT5
#else
    setStyle(new QCleanlooksStyle());
#endif

    QFontMetrics fm(QApplication::font());
        
    setMinimumHeight(fm.height()*3/2);
    setMaximumHeight(fm.height()*3/2);
    //frame->setMinimumHeight(fm.height()*3/2);
    //frame->setMaximumHeight(fm.height()*3/2);

    min_velocity_slider->setValue(4000);
    velocity_slider->setValue(8000);
    update_velocity_sliders();

    drunk_velocity_onoff->setChecked(drunk_velocity);

    if(scrolls_per_second==-1)
      scrolls_per_second = SETTINGS_read_int32("scrolls_per_second", default_scrolls_per_second);

    editlines->setValue(1);
    
    sps->setValue(scrolls_per_second);
    
    // For now, I haven't tried any machine where smooth scrolling doesn't work better than non-smooth scrolling. Anyay, just hide the sps options for now.
    sps->hide();
    sps_label->hide();
    sps_line->hide();

    // Adjust velocity slider widths
    {
      QFontMetrics fm(QApplication::font());
      //QRect r =fm.boundingRect(SLIDERPAINTER_get_string(_painter));
      int width = fm.boundingRect("Min. Vel: 100%").width()+20;
      min_velocity_slider->setMinimumWidth(width);
      velocity_slider->setMinimumWidth(width);
    }

    // cpu / xruns
    {
      int64_t guinum = S7CALL2(int_void,"FROM_C-create-cpu-usage-widget");
      QWidget *cpuusage = API_gui_get_widget(guinum);
      if (cpuusage != NULL)
        horizontalLayout_2->addWidget(cpuusage);
      else{
        R_ASSERT_NON_RELEASE(false);
      }
    }

    
    _initing = false;

    
    // set up timers, but only in the first created bottom bar.
    if (g_bottom_bars.size() == 0){

      _timer2.setInterval(50);
      _timer2.start();
    }

    g_bottom_bars.push_back(this);

    if (g_bottom_bars.size() > 1 && g_system_out_plugin!=NULL)
      GFX_OS_set_system_volume_plugin(g_system_out_plugin);

    // Set up custom popup menues for the time widgets
    {
      bpm->setContextMenuPolicy(Qt::CustomContextMenu);
      connect(bpm, SIGNAL(customContextMenuRequested(const QPoint&)),
              this, SLOT(ShowBPMPopup(const QPoint&)));

      bpm_label->setContextMenuPolicy(Qt::CustomContextMenu);
      connect(bpm_label, SIGNAL(customContextMenuRequested(const QPoint&)),
              this, SLOT(ShowBPMPopup(const QPoint&)));

      lpb->setContextMenuPolicy(Qt::CustomContextMenu);
      connect(lpb, SIGNAL(customContextMenuRequested(const QPoint&)),
              this, SLOT(ShowLPBPopup(const QPoint&)));
      
      lpb_label->setContextMenuPolicy(Qt::CustomContextMenu);
      connect(lpb_label, SIGNAL(customContextMenuRequested(const QPoint&)),
              this, SLOT(ShowLPBPopup(const QPoint&)));
      
      signature->setContextMenuPolicy(Qt::CustomContextMenu);
      connect(signature, SIGNAL(customContextMenuRequested(const QPoint&)),
              this, SLOT(ShowSignaturePopup(const QPoint&)));

      signature_label->setContextMenuPolicy(Qt::CustomContextMenu);
      connect(signature_label, SIGNAL(customContextMenuRequested(const QPoint&)),
              this, SLOT(ShowSignaturePopup(const QPoint&)));
    }

    
    {
      QColor system_color = get_qcolor(HIGH_BACKGROUND_COLOR_NUM);
      QPalette pal(palette());
      pal.setColor( QPalette::Active, QPalette::Dark, system_color);
      pal.setColor( QPalette::Active, QPalette::Light, system_color);
      pal.setColor( QPalette::Inactive, QPalette::Dark, system_color);
      pal.setColor( QPalette::Inactive, QPalette::Light, system_color);
      pal.setColor( QPalette::Disabled, QPalette::Dark, system_color);
      pal.setColor( QPalette::Disabled, QPalette::Light, system_color);
      setPalette(pal);
    }

    
    octave_down_button->_show_popup_menu = [](){
      S7CALL2(void_void,"FROM_C-show-bottom-bar-octave-down-popup-menu");
    };

    octave_up_button->_show_popup_menu = [](){
      S7CALL2(void_void,"FROM_C-show-bottom-bar-octave-up-popup-menu");
    };

    undo_button->_show_popup_menu = [](){
      S7CALL2(void_void,"FROM_C-show-bottom-bar-undo-popup-menu");
    };
    
    redo_button->_show_popup_menu = [](){
      S7CALL2(void_void,"FROM_C-show-bottom-bar-redo-popup-menu");
    };
    
    drunk_velocity_onoff->_show_popup_menu = [](){
      S7CALL2(void_void,"FROM_C-show-bottom-bar-switch-drunk_velocity-popup-menu");
    };
    
    edit_onoff->_show_popup_menu = [](){
      S7CALL2(void_void,"FROM_C-show-bottom-bar-switch-edit-popup-menu");
    };
    
    click_onoff->_show_popup_menu = [](){
      S7CALL2(void_void,"FROM_C-show-bottom-bar-switch-click-popup-menu");
    };
    
    play_cursor_onoff->_show_popup_menu = [](){
      S7CALL2(void_void,"FROM_C-show-bottom-bar-switch-play-cursor-popup-menu");
    };

    editor_follows_play_cursor_onoff->_show_popup_menu = [](){
      S7CALL2(void_void,"FROM_C-show-bottom-bar-switch-editor-follows-play-cursor-popup-menu");
    };
  }

  ~Bottom_bar_widget(){
    g_bottom_bars.removeOne(this);
    delete _navigator_widget;
  }

  QWidget *_navigator_widget = NULL;
      
  void remove_editor_elements(void){

    velocity_slider->hide();
    min_velocity_slider->hide();
    drunk_velocity_onoff->hide();
    
    edit_onoff->hide();
    
    octave_label->hide();
    octave_up_button->hide();
    octave_down_button->hide();
    
    lpb->hide();
    lpb_label->hide();

    play_cursor_onoff->hide();
    editor_follows_play_cursor_onoff->hide();

    editlines_label->hide();
    editlines->hide();

    line_5->hide();
    line_6->hide();
    line_8->hide();
    line_9->hide();
    line_10->hide();
    line_11->hide();
    el_line->hide();
    sps_line->hide();
    tempo_line->hide();
  }

  /*
  void resizeEvent( QResizeEvent *qresizeevent) override {
    if(_navigator_widget != NULL){
      _navigator_widget->setMinimumHeight(frame->height());
      _navigator_widget->setMaximumHeight(frame->height());
    }
  }
  */
  void enterEvent(QEvent *event) override {
    setCursor(Qt::ArrowCursor);
  }

  void updateWidgets(void){
    signature->setText(Rational(root->signature).toString());
    lpb->setValue(root->lpb);
    bpm->setValue(root->tempo);
    editlines->setValue(getNoteScrollLength());
    editor_follows_play_cursor_onoff->setVisible(ATOMIC_GET(root->play_cursor_onoff));
  }

  void update_velocity_sliders(){
    if(drunk_velocity==true){

      SLIDERPAINTER_set_string(velocity_slider->_painter, QString("Max. Vel: ") + QString::number(velocity_slider->value()*100/10000) + "%");
      //min_velocity_slider->setEnabled(true);
      min_velocity_slider->show();
      SLIDERPAINTER_set_string(min_velocity_slider->_painter, QString("Min. Vel: ") + QString::number(min_velocity_slider->value()*100/10000) + "%");

    }else{

      SLIDERPAINTER_set_string(velocity_slider->_painter, QString("Vel: ") + QString::number(velocity_slider->value()*100/10000) + "%");
      //min_velocity_slider->setEnabled(false);
      min_velocity_slider->hide();

    }

    min_velocity_slider->setMaximum(velocity_slider->value());

    if(drunk_velocity==true)
      velocity_slider->setMinimum(min_velocity_slider->value());
    else
      velocity_slider->setMinimum(0);
  }

  void set_volume_slider_string(void){
    if (g_system_out_plugin != NULL){
      const SoundPluginType *type = g_system_out_plugin->type;
      
      int effect_num = type->num_effects+EFFNUM_INPUT_VOLUME;
      
      char buf[64]={0};
      PLUGIN_get_display_value_string(g_system_out_plugin, effect_num, buf, 64);
      
      QString p = PLUGIN_has_midi_learn(g_system_out_plugin, effect_num) ? "*" : "";
      
      SLIDERPAINTER_set_string(system_volume_slider->_painter, p + buf);
    }
  }

public slots:
  
  void ShowBPMPopup(const QPoint& pos)
  {
    printf("GOTIT bpm\n");
    GFX_SimpleMenu("show BPM track", [](int command, bool onoff){
        showHideBPMTrack(-1);
      });
  }

  void ShowLPBPopup(const QPoint& pos)
  {
    printf("GOTIT lpb\n");
    GFX_SimpleMenu("show LPB track", [](int command, bool onoff){
        showHideLPBTrack(-1);
      });
  }

  void ShowSignaturePopup(const QPoint& pos)
  {
    printf("GOTIT signature\n");
    GFX_SimpleMenu("show time signature track", [](int command, bool onoff){
        showHideSignatureTrack(-1);
      });
  }

  void on_signature_editingFinished(){
    printf("signature bottombar\n");
    
    Rational rational(signature->text());
    signature->pushValuesToRoot(rational);

    signature->setText(Rational(root->signature).toString());
    set_editor_focus();
  }

  void on_lpb_editingFinished(){
    printf("lpb bottombar\n");
    setMainLPB(lpb->value());
    lpb->clearFocus();
    set_editor_focus();
  }

  void on_bpm_editingFinished(){
    printf("bpm bottombar\n");
    setMainBPM(bpm->value());
    bpm->clearFocus();
    set_editor_focus();
  }
  
  void on_system_volume_slider_valueChanged(int val){
    //printf("%d val: %d %p / %p / %p\n",_triggered_by_user,val,g_system_audio_instrument_widget,g_system_out_patch,g_system_out_plugin);
    //GFX_SetBrightness(root->song->tracker_windows, scale(val,0,10000,0,1));

    if (_triggered_by_user == true && g_system_audio_instrument_widget != NULL)
      g_system_audio_instrument_widget->input_volume_slider->setValue(val);
    
    set_volume_slider_string();
  }

  void on_octave_up_button_clicked(){
    incKeyAdd(12);
  }

  void on_octave_down_button_clicked(){
    incKeyAdd(-12);
  }

  void on_editlines_valueChanged(int val){
    setNoteScrollLength(val);
  }

  void on_editlines_editingFinished(){
    editlines->clearFocus();
    set_editor_focus();
  }

  void on_sps_valueChanged(int val){
    scrolls_per_second = val;
    SETTINGS_write_int("scrolls_per_second", val);
  }

  void on_sps_editingFinished(){
    set_editor_focus();
  }

  void on_drunk_velocity_onoff_toggled(bool val){
    if(_initing==true)
      return;
    
    drunk_velocity = val;
    update_velocity_sliders();
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

  void on_undo_button_clicked(){
    Undo();
  }

  void on_redo_button_clicked(){
    Redo();
  }

  void on_edit_onoff_toggled(bool val){
    if(val!=ATOMIC_GET(root->editonoff))
      switchEditOnOff();
  }

  void on_click_onoff_toggled(bool val){
    enableMetronome(val);
  }

  void on_play_cursor_onoff_toggled(bool val){
    enablePlayCursor(val);
    editor_follows_play_cursor_onoff->setVisible(val);
  }

  void on_editor_follows_play_cursor_onoff_toggled(bool val){
    enableEditorFollowsPlayCursor(val);
  }
};

extern "C"{
  /*
  void GFX_OS_set_system_volume_peak_pointers(float *pointers, int num_channels){
    SLIDERPAINTER_set_peak_value_pointers(g_bottom_bar_widget->system_volume_slider->_painter, num_channels, pointers);
  }
  */

  // Note: plugin might be NULL.
  void GFX_OS_set_system_volume_plugin(struct SoundPlugin *plugin){
    {
      radium::PlayerLock lock;
      g_system_out_plugin = plugin;
      g_RT_system_out_input_latency = 0;
    }
    
    g_system_out_patch = NULL;
    g_system_audio_instrument_widget = NULL;
        
    for(auto *bottom_bar_widget : g_bottom_bars){
      
      auto *system_volume_slider = bottom_bar_widget->system_volume_slider;
      
      system_volume_slider->_patch.set(NULL);
      system_volume_slider->_effect_num = EFFNUM_INPUT_VOLUME;
      
      if (plugin == NULL){
        
        static float nullfloats[2] = {0.0f, 0.0f};
        SLIDERPAINTER_set_peak_value_pointers(system_volume_slider->_painter, 2, nullfloats, true);
        
      } else {
        
        const SoundPluginType *type = plugin->type;
        
        SLIDERPAINTER_set_peak_value_pointers(system_volume_slider->_painter, type->num_inputs, plugin->input_volume_peaks.decaying_dbs, true);
        
      }
    }
      
  }

  void GFX_OS_UpdateKeyOctave(void){
    if(g_is_starting_up==false)
      for(auto *bottom_bar_widget : g_bottom_bars)
        bottom_bar_widget->octave_label->setText(QString("Oct.: ")+QString::number(root->keyoct/12,16));
  }

  void GFX_OS_update_bottombar(void){
    if(g_is_starting_up==false)
      for(auto *bottom_bar_widget : g_bottom_bars)
        bottom_bar_widget->updateWidgets();
  } 
 
  void OS_GFX_NumUndosHaveChanged(int num_undos, bool redos_are_available, bool has_unsaved_undos){
    for(auto *bottom_bar_widget : g_bottom_bars){
      bottom_bar_widget->num_undos_label->setText(QString::number(num_undos));
      bottom_bar_widget->unsaved_undos->setText(has_unsaved_undos?"*":" ");
      bottom_bar_widget->undo_button->setEnabled(num_undos>0);
      bottom_bar_widget->redo_button->setEnabled(redos_are_available);
    }
  }

  // This function could be deleted since the timer polls the system out plugin now. But it's a little bit smoother to call this function directly, and it probably doesn't hurt to keep it.
  void OS_GFX_SetVolume(int value){
    for(auto *bottom_bar_widget : g_bottom_bars){
      bottom_bar_widget->_triggered_by_user=false;
      bottom_bar_widget->system_volume_slider->setValue(value);
      bottom_bar_widget->_triggered_by_user=true;
    }
  }

  void OS_GFX_IncVolume(int how_much){
    for(auto *bottom_bar_widget : g_bottom_bars){
      int new_value = how_much + bottom_bar_widget->system_volume_slider->value();
      if(new_value<0)
        new_value=0;
      if(new_value>10000)
        new_value=10000;
      
      bottom_bar_widget->system_volume_slider->setValue(new_value);
    }
  }
}

bool GFX_OS_patch_is_system_out(struct Patch *patch){
  return patch==g_system_out_patch;
}

struct Patch *GFX_OS_get_system_out(void){
  if (g_system_out_patch==NULL)
    return NULL;
  
  if (PATCH_get_from_id(g_system_out_patch->id)==NULL){
    R_ASSERT_NON_RELEASE(false);
    return NULL;
  }

  return g_system_out_patch;
}

struct SoundPlugin *RT_get_system_out_plugin(void){
  return g_system_out_plugin;
}

bool EDITOR_switch_drunk_velocity(void){
  drunk_velocity = !drunk_velocity;
  
  for(auto *bottom_bar_widget : g_bottom_bars){
    bottom_bar_widget->_initing = true;
    bottom_bar_widget->update_velocity_sliders();
    bottom_bar_widget->velocity_slider->update();
    bottom_bar_widget->drunk_velocity_onoff->setChecked(drunk_velocity);
    bottom_bar_widget->_initing = false;
  }
  
  return drunk_velocity;
}
  
QWidget *BottomBar_create(QWidget *parent, bool include_editor_elements, bool include_navigator){
  auto *ret = new Bottom_bar_widget(parent, include_navigator);
  if(!include_editor_elements)
    ret->remove_editor_elements();
  
  {
    const QFontMetrics fn = QFontMetrics(QApplication::font());
    ret->status_label->setMinimumWidth(fn.boundingRect("Main Pipe/System Compression Release").width()); // sion Release: 9.99
    ret->status_label->_use_custom_painter = true;
  }
  
  {
    QColor system_color = get_qcolor(HIGH_BACKGROUND_COLOR_NUM);
    QPalette pal(ret->palette());
    pal.setColor( QPalette::Active, QPalette::Dark, system_color);
    pal.setColor( QPalette::Active, QPalette::Light, system_color);
    pal.setColor( QPalette::Inactive, QPalette::Dark, system_color);
    pal.setColor( QPalette::Inactive, QPalette::Light, system_color);
    pal.setColor( QPalette::Disabled, QPalette::Dark, system_color);
    pal.setColor( QPalette::Disabled, QPalette::Light, system_color);
    ret->setPalette(pal);
  }

  ret->setStyleSheet("QStatusBar::item { border: 0px solid black }; ");

  /*
  {
    QColor system_color(SETTINGS_read_string("system_color","#d2d0d5"));
    ret->setStyleSheet("#frame { border: 1px solid " + system_color.darker(150).name(QColor::HexArgb) + "; }");
  }
  */
  return ret;
}


/*
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

  GFX_OS_UpdateKeyOctave();
}
  
*/

