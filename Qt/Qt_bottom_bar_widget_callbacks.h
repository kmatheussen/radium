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
#include "../audio/CpuUsage.hpp"

#include "../Qt/Qt_MyQButton.h"
#include "../Qt/Qt_MyQSpinBox.h"
#include <QTimer>

#ifndef USE_QT5
#include <QCleanlooksStyle>
#endif

#include "FocusSniffers.h"

#include "Qt_instruments_proc.h"

#include "Qt_audio_instrument_widget.h"

#include "Qt_bottom_bar_widget.h"


class Bottom_bar_widget;

Bottom_bar_widget *g_bottom_bar_widget = NULL;
Ui::Audio_instrument_widget *g_system_audio_instrument_widget = NULL;
struct Patch *g_system_out_patch = NULL;
struct SoundPlugin *g_system_out_plugin = NULL;

extern bool drunk_velocity;
extern CpuUsage g_cpu_usage;

extern int scrolls_per_second;
extern int default_scrolls_per_second;

class Bottom_bar_widget : public QWidget, public Ui::Bottom_bar_widget {
  Q_OBJECT

  struct Timer : public QTimer{
    Bottom_bar_widget *bottom_bar_widget;
    void timerEvent(QTimerEvent * e){
      QString usage;

      float mincpu = g_cpu_usage.min();
      float maxcpu = g_cpu_usage.max();
      float avgcpu = g_cpu_usage.avg();
      
      usage.sprintf("%s%.1f / %s%.1f / %s%.1f",
                    mincpu < 10 ? " " : "", mincpu,
                    avgcpu<10?" ":"", avgcpu,
                    maxcpu < 10?" ":"", maxcpu
                    );

      g_cpu_usage.reset();

      bottom_bar_widget->cpu_label->setText(usage);
    }
  };

  struct Timer2 : public QTimer{
    
    Bottom_bar_widget *bottom_bar_widget;
    
    void timerEvent(QTimerEvent * e){
      
      if (bottom_bar_widget->edit_onoff->isChecked() != ATOMIC_GET(root->editonoff))
        bottom_bar_widget->edit_onoff->setChecked(ATOMIC_GET(root->editonoff));
      
      if (bottom_bar_widget->click_onoff->isChecked() != ATOMIC_GET(root->clickonoff))
        bottom_bar_widget->click_onoff->setChecked(ATOMIC_GET(root->clickonoff));
      
      if (bottom_bar_widget->play_cursor_onoff->isChecked() != ATOMIC_GET(root->play_cursor_onoff))
        bottom_bar_widget->play_cursor_onoff->setChecked(ATOMIC_GET(root->play_cursor_onoff));
      
      if (bottom_bar_widget->editor_follows_play_cursor_onoff->isChecked() != ATOMIC_GET(root->editor_follows_play_cursor_onoff))
        bottom_bar_widget->editor_follows_play_cursor_onoff->setChecked(ATOMIC_GET(root->editor_follows_play_cursor_onoff));

      if (g_system_out_patch==NULL && g_system_out_plugin != NULL)
        g_system_out_patch = (struct Patch*)g_system_out_plugin->patch;
        
      if (g_system_audio_instrument_widget == NULL && g_system_out_patch != NULL) {
        g_bottom_bar_widget->system_volume_slider->_patch = g_system_out_patch;
                
        g_system_audio_instrument_widget = InstrumentWidget_get_audio_instrument_widget(g_system_out_patch);
        if (g_system_audio_instrument_widget != NULL)
          bottom_bar_widget->system_volume_slider->setValue(g_system_audio_instrument_widget->input_volume_slider->value());
      }

      SLIDERPAINTER_call_regularly(bottom_bar_widget->system_volume_slider->_painter);
    }
  };

 public:
  bool _initing;
  Timer _timer;
  Timer2 _timer2;

 Bottom_bar_widget(QWidget *parent=NULL)
    : QWidget(parent)
  {
    _initing = true;
    setupUi(this);
#ifndef USE_QT5
    setStyle(new QCleanlooksStyle());
#endif
    
    if(g_bottom_bar_widget != NULL)
      RError("g_bottom_bar_widget!=NULL");

    g_bottom_bar_widget = this;

    min_velocity_slider->setValue(4000);
    velocity_slider->setValue(8000);
    update_velocity_sliders();

    drunk_velocity_onoff->setChecked(drunk_velocity);

    if(scrolls_per_second==-1)
      scrolls_per_second = SETTINGS_read_int("scrolls_per_second", default_scrolls_per_second);

    editlines->setValue(1);
    
    sps->setValue(scrolls_per_second);
    
    // For now, I haven't tried any machine where smooth scrolling doesn't work better than non-smooth scrolling. Anyay, just hide the sps options for now.
    sps->hide();
    sps_label->hide();
    sps_line->hide();

    // Adjust cpu label width
    set_cpu_usage_font_and_width(cpu_label, false);

    // Adjust velocity slider widths
    {
      QFontMetrics fm(QApplication::font());
      //QRect r =fm.boundingRect(SLIDERPAINTER_get_string(_painter));
      int width = fm.width("Min. Vel: 100%")+20;
      min_velocity_slider->setMinimumWidth(width);
      velocity_slider->setMinimumWidth(width);
    }

    _initing = false;

    // set up timers
    {
      _timer.bottom_bar_widget = this;
      _timer.setInterval(1000);
      _timer.start();
      
      _timer2.bottom_bar_widget = this;
      _timer2.setInterval(50);
      _timer2.start();
    }

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


public slots:
  
  void ShowBPMPopup(const QPoint& pos)
  {
    printf("GOTIT bpm\n");
    if (popupMenu((char*)"show BPM track")==0)
      showHideBPMTrack(-1);
  }

  void ShowLPBPopup(const QPoint& pos)
  {
    printf("GOTIT lpb\n");
    if (popupMenu((char*)"show LPB track")==0)
      showHideLPBTrack(-1);
  }

  void ShowSignaturePopup(const QPoint& pos)
  {
    printf("GOTIT signature\n");
    if (popupMenu((char*)"show time signature track")==0)
      showHideSignatureTrack(-1);
  }

  void on_signature_editingFinished(){
    printf("signature bottombar\n");
    
    Rational rational = create_rational_from_string(signature->text());
    signature->pushValuesToRoot(rational);

    signature->setText(Rational(root->signature).toString());
    set_editor_focus();
  }

  void on_lpb_editingFinished(){
    printf("lpb bottombar\n");
    setMainLPB(lpb->value());
    set_editor_focus();
  }

  void on_bpm_editingFinished(){
    printf("bpm bottombar\n");
    setMainBPM(bpm->value());
    set_editor_focus();
  }
  
  void on_system_volume_slider_valueChanged(int val){
    //printf("val: %d %p / %p / %p\n",val,g_system_audio_instrument_widget,g_system_out_patch,g_system_out_plugin);
    GFX_SetBrightness(root->song->tracker_windows, scale(val,0,10000,0,1));

    if (g_system_audio_instrument_widget != NULL)
      g_system_audio_instrument_widget->input_volume_slider->setValue(val);
    
    if (g_system_out_plugin != NULL) {
      const SoundPluginType *type = g_system_out_plugin->type;
        
      char buf[64]={0};
      PLUGIN_get_display_value_string(g_system_out_plugin, type->num_effects+EFFNUM_INPUT_VOLUME, buf, 64);

      SLIDERPAINTER_set_string(system_volume_slider->_painter, buf);
    }
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
  
  void GFX_OS_set_system_volume_plugin(struct SoundPlugin *plugin){
    g_system_out_plugin = plugin;
    g_system_out_patch = NULL;
    g_system_audio_instrument_widget = NULL;

    g_bottom_bar_widget->system_volume_slider->_patch = NULL;
    g_bottom_bar_widget->system_volume_slider->_effect_num = EFFNUM_INPUT_VOLUME;

    if (plugin == NULL){
      
      static float nullfloats[2] = {0.0f, 0.0f};
      SLIDERPAINTER_set_peak_value_pointers(g_bottom_bar_widget->system_volume_slider->_painter, 2, nullfloats);
      
    } else {

      const SoundPluginType *type = plugin->type;

      SLIDERPAINTER_set_peak_value_pointers(g_bottom_bar_widget->system_volume_slider->_painter, type->num_inputs, plugin->input_volume_peak_values);
      
    }    
  }

  void GFX_OS_UpdateKeyOctave(void){
    if(ATOMIC_GET(is_starting_up)==false)
      g_bottom_bar_widget->octave_label->setText(QString("Oct.: ")+QString::number(root->keyoct/12,16));
  }

  void GFX_OS_update_bottombar(void){
    if(ATOMIC_GET(is_starting_up)==false)
      g_bottom_bar_widget->updateWidgets();
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

  void OS_GFX_IncVolume(int how_much){
    int new_value = how_much + g_bottom_bar_widget->system_volume_slider->value();
    if(new_value<0)
      new_value=0;
    if(new_value>10000)
      new_value=10000;

    g_bottom_bar_widget->system_volume_slider->setValue(new_value);
  }
}

bool GFX_OS_patch_is_system_out(struct Patch *patch){
  return patch==g_system_out_patch;
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

