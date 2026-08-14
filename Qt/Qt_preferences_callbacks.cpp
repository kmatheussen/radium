/* Copyright 2013 Kjetil S. Matheussen

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


#if defined(__GNUC__) && !defined(__clang__)
#  include "../Qt/Qt_precompiled.hpp"
#endif

#define __STDC_FORMAT_MACROS 1
#include <inttypes.h>

#include <stdio.h>
#include <unistd.h>

#include <gc.h>

#include <QPushButton>
#include <QColorDialog>
#include <QCloseEvent>
#include <QHideEvent>
#include <QPainter>
#include <QVector>
#include <QFile>
#include <QTextStream>
#include <QDir>

#include "../common/nsmtracker.h"
#include "../common/hashmap_proc.h"
#include "../common/OS_string_proc.h"
#include "../common/OS_settings_proc.h"
#include "../common/OS_visual_input.h"
#include "../common/settings_proc.h"
#include "../common/window_config_proc.h"
#include "../OpenGL/Widget_proc.h"
#include "../OpenGL/Render_proc.h"
#include "../audio/MultiCore_proc.h"
#include "../audio/Mixer_proc.h"
#include "../midi/midi_i_input_proc.h"
#include "../midi/midi_instrument_proc.h"
#include "../midi/midi_ports_proc.h"
#include "../midi/OS_midi_proc.h"

#include "../api/api_various_proc.h"
#include "../api/api_proc.h"

#include "Qt_MyQSpinBox.h"
#include "Qt_MyQScrollBar.hpp"
#include <FocusSniffers.h>
#include "helpers.h"
#include "Qt_sequencer_proc.h"
#include "Qt_SaveRestoreWindows_proc.h"

#include "Qt_colors_proc.h"


#include "mQt_vst_paths_widget_callbacks.h"

#include "Qt_preferences.h"


static void minimizeRecursively(QObject *object){

  if (object==NULL)
    return;
  
  QWidget *widget = qobject_cast<QWidget*>(object);
  
  if (widget != NULL){
    widget->resize(widget->width()+1, widget->height()+1);
    widget->adjustSize();
    widget->updateGeometry();

  }
  
  for(auto *c : object->children()){
    minimizeRecursively(c);
  }
  if (widget != NULL){
    widget->resize(widget->width()+1, widget->height()+1);
    widget->adjustSize();
    widget->updateGeometry();

  }
   
}

static void save_color_dialog_custom_colors(void){
  QFile file(OS_get_dot_radium_path() + QDir::separator() + "color_dialog_custom_colors.conf");
  if (!file.open(QIODevice::WriteOnly | QIODevice::Text))
    return;
  QTextStream out(&file);
  for(int i = 0; i < QColorDialog::customCount(); i++){
    QColor color = QColorDialog::customColor(i);
    out << (color.isValid() ? color.name(QColor::HexArgb) : "") << "\n";
  }
}

static void restore_color_dialog_custom_colors(void){
  QFile file(OS_get_dot_radium_path() + QDir::separator() + "color_dialog_custom_colors.conf");
  if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
    return;
  QTextStream in(&file);
  for(int i = 0; i < QColorDialog::customCount() && !in.atEnd(); i++){
    QString line = in.readLine().trimmed();
    if (!line.isEmpty())
      QColorDialog::setCustomColor(i, QColor(line));
  }
}

extern struct Root *root;
bool g_show_key_codes = false;

extern bool g_gc_is_incremental;

#if PUT_ON_TOP
int RememberGeometryQDialog::num_open_dialogs;
#endif

namespace{

struct ColorButton;
static radium::Vector<ColorButton*> all_buttons;

static enum ColorNums g_current_colornum = LOW_EDITOR_BACKGROUND_COLOR_NUM;

enum {
  SLIDER_INSTRUMENT_BRIGHTNESS = 0,
  SLIDER_INSTRUMENT_SATURATION,
  SLIDER_INSTRUMENT_BRIGHTNESS_IN_EDITOR,
  SLIDER_INSTRUMENT_SATURATION_IN_EDITOR,
  SLIDER_BLOCK_BRIGHTNESS,
  SLIDER_BLOCK_SATURATION,
  SLIDER_GRADIENT,
  NUM_SLIDERS
};

struct PreferencesUndoEntry {
  bool is_slider;
  enum ColorNums colornum;
  QColor color;
  int slider_id;
  float slider_value;
};

static QVector<PreferencesUndoEntry> g_color_undo_stack;
static QVector<PreferencesUndoEntry> g_color_redo_stack;
static QColor g_pending_old_color;
static bool g_pending_change_committed = false;
static bool g_slider_committed[NUM_SLIDERS] = {false}; // [NO_STATIC_ARRAY_WARNING]
static int g_pending_old_slider_values[NUM_SLIDERS] = {0}; // [NO_STATIC_ARRAY_WARNING]
static bool g_in_undo_redo = false;

struct Separator : public QWidget{
  QString _text;
  Separator(QString text)
    : _text(text)
  {
    setMinimumHeight(root->song->tracker_windows->systemfontheight);
    setMaximumHeight(root->song->tracker_windows->systemfontheight);

    QSizePolicy policy = QSizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::MinimumExpanding);

    setSizePolicy(policy);
  }

  void paintEvent ( QPaintEvent * ev ) override {
    TRACK_PAINT();
    
    //QToolButton::paintEvent(ev);
    QPainter p(this);
    p.fillRect(0,0,width(),height(),QColor("black"));
    //p.eraseRect(rect());

    QRect rect(1,1,width()-2,height()-2);

    QColor text_color = get_qcolor(TEXT_COLOR_NUM); //black(0,0,0);
    
    p.setPen(text_color);

    myDrawText(p, rect, _text, Qt::AlignHCenter | Qt::AlignVCenter, false, 0, true, false);
  }
  
};
 
struct MyColorDialog : public QColorDialog
{

public: 

	MyColorDialog()
	{
		setOption(QColorDialog::NoButtons); // Avoid crash on macos sierra. (https://bugreports.qt.io/browse/QTBUG-56448)
	}

	void mouseReleaseEvent(QMouseEvent *event) override
	{
		QColorDialog::mouseReleaseEvent(event);

		if (g_pending_change_committed) {
			g_pending_old_color = get_qcolor(g_current_colornum);
			g_pending_change_committed = false;
		}
	}

	void keyReleaseEvent(QKeyEvent *event) override
	{
		QColorDialog::keyReleaseEvent(event);

		if (g_pending_change_committed) {
			g_pending_old_color = get_qcolor(g_current_colornum);
			g_pending_change_committed = false;
		}
	}

	
#if FOR_MACOSX && !USE_QT5
	void closeEvent(QCloseEvent *event) override
	{
		hide();
		event->ignore(); // Only hide the window, dont close it.
	}
	
	void myshow()
	{
		safeShow(this);
		raise();
	}
#endif
};
 
struct ColorButton : public QPushButton{
  Q_OBJECT

public:
  
  enum ColorNums colornum;
  bool is_current;

  MyColorDialog *color_dialog;
  
  
  ColorButton(QString name, enum ColorNums colornum, MyColorDialog *color_dialog)
    : QPushButton(name)
    , colornum(colornum)
    , is_current(colornum==g_current_colornum)
    , color_dialog(color_dialog)
  {
    setCheckable(true);

    all_buttons.push_back(this);
    
    connect(this, SIGNAL(pressed()), this, SLOT(color_pressed()));
    //connect(this, SIGNAL(released()), this, SLOT(color_released()));
    //connect(this, SIGNAL(clicked(bool)), this, SLOT(color_clicked(bool)));
    //connect(this, SIGNAL(toggled(bool)), this, SLOT(color_toggled(bool)));
  }

  ~ColorButton(){
    all_buttons.remove(this);
  }

  /*
  bool is_current(void){
    return isChecked() || isDown();
  }
  */
  
  void paintEvent ( QPaintEvent * ev ) override {
    TRACK_PAINT();
    
    //QToolButton::paintEvent(ev);
    QPainter p(this);
    p.eraseRect(rect());
    //printf("********** isdown: %d. enabled: %d, width: %d, height: %d\n", isDown(),isEnabled(), width(), height());
    //CHECKBOX_paint(&p, !isDown(), isEnabled(), width(), height(), text());

    int split = 100;
    int text_width = width() - split;

    QColor text_color = get_qcolor(TEXT_COLOR_NUM);

    QColor swatch_color = get_qcolor(colornum);
    printf("COLOR_BUTTON_PAINT: name='%s' colornum=%d is_current=%d swatch=%s\n",
           text().toUtf8().constData(), colornum, is_current, swatch_color.name(QColor::HexArgb).toUtf8().constData());

    QRect rect(split+10,1,text_width-2,height()-1);

    p.setPen(text_color);
    p.drawText(rect, Qt::AlignLeft | Qt::AlignVCenter, text());

    // Left 50px: opaque
    QColor opaque_color = swatch_color;
    opaque_color.setAlpha(255);
    p.fillRect(0, 0, split/2, height(), opaque_color);

    // Right 50px: with alpha
    p.fillRect(split/2, 0, split - split/2, height(), swatch_color);

    if (is_current) {
      p.drawRect(0,0,width()-1,height()-1);
      p.drawRect(1,1,width()-3,height()-3);
    }
  }

  void set_current(void){
    for(auto button : all_buttons){
      if (button != this) {
        if (button->is_current == true) {
          button->is_current = false;
          button->update();
        }
      }
    }
    is_current = true;

    printf("set_current: colornum=%d  old=%s  committed=%d\n", colornum, get_qcolor(colornum).name().toUtf8().constData(), g_pending_change_committed);
    g_pending_old_color = get_qcolor(colornum);
    g_pending_change_committed = false;

    g_current_colornum = colornum;
    color_dialog->setCurrentColor(get_qcolor(colornum));
#if FOR_MACOSX && !USE_QT5
    color_dialog->myshow();
#endif
    
    update();
  }

  public slots:

  void color_pressed(){
    printf("color_pressed: colornum=%d is_current=%d  old=%s\n", colornum, is_current, get_qcolor(colornum).name().toUtf8().constData());
    g_pending_old_color = get_qcolor(colornum);
    g_pending_change_committed = false;
    if (is_current==false)
      set_current();
  }
  void color_released(){
    printf("Color %d released to %d\n",colornum,is_current);
  }
  void color_clicked(bool checked){
    printf("Color %d clicked to %d %d\n",colornum,is_current,checked);
  }
  void color_toggled(bool checked){
    printf("Color %d toggled to %d %d\n",colornum,is_current,checked);
  }

};

struct MidiInput : public QWidget{
  Q_OBJECT

public:

  QString name;
  QHBoxLayout layout;

  MidiInput(QWidget *parent, QString name)
    : QWidget(parent)
    , name(name)
    , layout(this)
  {
    layout.setSpacing(1);
    layout.setContentsMargins(1,1,1,1);
                       
    QLabel *label = new QLabel(name, this);
    label->setFrameStyle(QFrame::StyledPanel | QFrame::Sunken);
    label->setSizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::MinimumExpanding);
    layout.addWidget(label);

    layout.addSpacing(10);
    
    QPushButton *button = new QPushButton("Delete", this);
    button->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::MinimumExpanding);
    layout.addWidget(button);

    layout.addSpacing(10);
        
    connect(button, SIGNAL(released()), this, SLOT(delete_released()));

  }
    

  public slots:

  void delete_released(){
    printf("%s deleted\n",name.toUtf8().constData());
    MIDI_remove_editor_input_port(name.toUtf8().constData());
    PREFERENCES_update();
  }
};
  
class Preferences : public RememberGeometryQDialog, public Ui::Preferences {
  Q_OBJECT

 public:
  bool _initing;
  bool _is_updating_widgets;
  bool _needs_to_update = false;
  MyColorDialog _color_dialog;
  Vst_paths_widget *vst_widget = new Vst_paths_widget;
  
 Preferences(QWidget *parent=NULL)
   : RememberGeometryQDialog(parent, radium::MAY_BE_MODAL)
   , _is_updating_widgets(false)
  {
    R_ASSERT(parent!=NULL);
    _initing = true;

    setupUi(this);

    testColorInRealtime(ALTERNATIVE_LABEL_COLOR_NUM, get_qcolor(ALTERNATIVE_LABEL_COLOR_NUM)); // Update label colors.
    
    scrollArea->setHorizontalScrollBar(new Qt_MyQScrollBar(Qt::Horizontal));
    scrollArea->setVerticalScrollBar(new Qt_MyQScrollBar(Qt::Vertical));
    scrollArea_2->setHorizontalScrollBar(new Qt_MyQScrollBar(Qt::Horizontal));
    scrollArea_2->setVerticalScrollBar(new Qt_MyQScrollBar(Qt::Vertical));

    updateWidgets();

    // VST
    {    
      vst_widget->buttonBox->hide();
      
      tabWidget->insertTab(tabWidget->count()-5, vst_widget, "Plugins");
    }

    // Colors
    {
      _color_dialog.setOption(QColorDialog::NoButtons, true);
      
#if FOR_MACOSX && !USE_QT5
      //_color_dialog.hide();
      _color_dialog.setOption(QColorDialog::DontUseNativeDialog, true);
      
#else
      _color_dialog.setOption(QColorDialog::DontUseNativeDialog, true);
      _color_dialog.setOption(QColorDialog::ShowAlphaChannel, true);

      colorlayout_right->insertWidget(0, &_color_dialog);
      g_pending_old_color = get_qcolor(g_current_colornum);
#endif

      restore_color_dialog_custom_colors();

      connect(&_color_dialog, SIGNAL(currentColorChanged(const QColor &)), this, SLOT(color_changed(const QColor &)));
      connect(tabWidget, SIGNAL(currentChanged(int)), this, SLOT(current_tab_changed(int)));

      scrollArea->setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOn);
      scrollArea->setWidgetResizable(true);

      QWidget *contents = scrollArea->widget();      
      QVBoxLayout *layout = new QVBoxLayout(contents);
      layout->setSpacing(1);

      contents->setLayout(layout);
      
      for(int i=START_CONFIG_COLOR_NUM;i<END_CONFIG_COLOR_NUM;i++) {
        
        QString name = get_color_display_name((enum ColorNums)i);
        
        if (is_qcolor_separator(i)){

          Separator *s = new Separator(name);
          layout->addWidget(s);
          safeShow(s);
    
        } else {
            
          ColorButton *l = new ColorButton(name, (enum ColorNums)i, &_color_dialog);
          
          layout->addWidget(l);
          //l->move(0, i*20);
          safeShow(l);
          //contents->resize(contents->width(), 200*20);
        }
      }
      
      //contents->adjustSize();

      populate_color_selector();

      {
        static bool has_stored_checkpoint = false;
        if (has_stored_checkpoint==false){
          has_stored_checkpoint = true;
          GFX_StoreCheckpoint();
        }
      }
    }

    gui_tab_widget->setCurrentIndex(0);
    opengl_tab_widget->setCurrentIndex(0);
    tabWidget->setCurrentIndex(0);

    _initing = false;

    resize(10,10); // as small as possible
    //adjustSize();
  }

  /*
  // Can't override showEvent when remember geometry.
  void showEvent(QShowEvent *event) override {    
    if (tabWidget->currentWidget() == colors)
      obtain_keyboard_focus_without_greying();

    if(g_radium_runs_custom_exec) return;

    if (_needs_to_update)
      updateWidgets();
  }
  */

  // Must override setVisible instead.
  virtual void setVisible(bool visible) override {
    if (visible && isVisible()==false){
      if (tabWidget->currentWidget() == colors)
        obtain_keyboard_focus_without_greying();
      
      if(g_radium_runs_custom_exec==false){
        if (_needs_to_update)
          updateWidgets();
      }
    }

    if (visible==false && isVisible()==true){
      GFX_reload_qt_stylesheets(false);
      GFX_SaveAllColorConfigurations();
	  GFX_SaveColors(createIllegalFilepath());
      save_color_dialog_custom_colors();
    }

    RememberGeometryQDialog::setVisible(visible);
  }

  
#if FOR_MACOSX && !USE_QT5
  void hideEvent(QHideEvent *event) override {
    _color_dialog.close();
    release_keyboard_focus();
    event->accept();
  }

  #if 0
  void changeEvent(QEvent *event) {

    if (tabWidget->currentWidget() != colors)
      _color_dialog.close();
    else
      _color_dialog.myshow();
    event->accept();
  }
  #endif

#else
  void hideEvent(QHideEvent *event) override {
    //printf("        HIDEVENT1\n");
    RememberGeometryQDialog::hideEvent(event);
    release_keyboard_focus();
    event->accept();
  }
#endif
  
  void updateWidgets(){
    _needs_to_update = false;
      
    _is_updating_widgets = true;
  
    // OpenGL
    {
      vsyncOnoff->setChecked(GL_get_vsync());
      
      switch(GL_get_multisample()){
      case 1:
        mma1->setChecked(true);
        break;
        
      case 2:
        mma2->setChecked(true);
        break;
        
      case 4:
        mma4->setChecked(true);
        break;
        
      case 8:
        mma8->setChecked(true);
        break;
        
      case 16:
        mma16->setChecked(true);
        break;
        
      case 32:
        mma32->setChecked(true);
        break;
      }
      {
        const char *supported = GL_get_supported_msaa_samples();
        label_msaa_supported->setText(QString("Supported by current backend: ") + supported);
      }

#if 0
      QString w="999999999";
      adjustWidthToFitText(mma1, w);
      adjustWidthToFitText(mma2, w);
      adjustWidthToFitText(mma4, w);
      adjustWidthToFitText(mma8, w);
      adjustWidthToFitText(mma16, w);
      adjustWidthToFitText(mma32, w);
#endif
      
#if USE_QT5
      eraseEstimatedVBlankInterval->hide();
      erase_vblank_group_box_layout->removeItem(erase_estimated_vblank_spacer);
#else
      QString vblankbuttontext = QString("Erase Estimated Vertical Blank (")+QString::number(1000.0/GL_get_estimated_vblank())+" Hz)";
      eraseEstimatedVBlankInterval->setText(vblankbuttontext);
#endif

#if 0
      safeModeOnoff->setChecked(GL_get_safe_mode());
      safeModeOnoff->setEnabled(false);
#endif

      high_priority_render_thread->setChecked(GL_get_high_render_thread_priority());

      clampTextRendering->setChecked(GL_get_clamp_text_rendering());

      // GPU Backend
      {
        const char *rhi_backend = GL_get_backend();
        if (!strcmp(rhi_backend, "null"))
          rhi_null->setChecked(true);
        else if (!strcmp(rhi_backend, "opengl"))
          rhi_opengl->setChecked(true);
        else if (!strcmp(rhi_backend, "vulkan"))
          rhi_vulkan->setChecked(true);
        else if (!strcmp(rhi_backend, "d3d11"))
          rhi_d3d11->setChecked(true);
        else if (!strcmp(rhi_backend, "d3d12"))
          rhi_d3d12->setChecked(true);
        else if (!strcmp(rhi_backend, "metal"))
          rhi_metal->setChecked(true);

#if FOR_WINDOWS
        rhi_metal->hide();
#elif FOR_MACOSX
        rhi_opengl->hide();
        rhi_vulkan->hide();
        rhi_d3d11->hide();
        rhi_d3d12->hide();
#elif FOR_LINUX
        rhi_d3d11->hide();
        rhi_d3d12->hide();
        rhi_metal->hide();
#else
#  error "unknown arch."
#endif
      }
    }


    // Instruments
    {
      enable_sample_seek_by_default->setChecked(enableSampleSeekByDefault());
    }

    
    // Various
    {

      // gc
      {
        gcOnOff->setChecked(true);

#if 0 // defined(FOR_MACOSX) && (defined (__arm64__) || defined (__aarch64__))
	bool incremental_gc = true;
#else
        bool incremental_gc = SETTINGS_read_bool("incremental_gc",false);
#endif
	
        incrementalGcNextTime->setChecked(false);
        
        incrementalGc->setChecked(incremental_gc);
        
        if (g_gc_is_incremental==false)
          incrementalGc->setDisabled(true);
        
        if (incremental_gc)
          incrementalGcNextTime->setDisabled(true);
	
#if 0 // defined(FOR_MACOSX) && (defined (__arm64__) || defined (__aarch64__))
	R_ASSERT(incremental_gc);
	gcOnOff->setDisabled(true);
	incrementalGcNextTime->setDisabled(true);
	incrementalGc->setDisabled(true);
#endif// macos(arm) -> !macos(arm)
      }

      // audio meter update
      {
        cpu_friendly_audio_meter_updates->setChecked(useCPUFriendlyAudiometerUpdates());
      }

      // NSM
      {
        nsm_switch_enabled->setChecked(supportsSwitchNsmCapability());
      }

      // Remote access
      {
        enable_remote_control->setChecked(controlPortOpenForRemoteConnections());
      }
    }

    // Audio
    {
      numCPUs->setValue(MULTICORE_get_num_threads());
      numCPUs->setMaximum(MAX_NUM_CPUS);
      
      enable_autobypass->setChecked(autobypassEnabled());
      autobypass_delay->setValue(getAutoBypassDelay());
      undo_solo->setChecked(doUndoSolo());
      undo_bypass->setChecked(doUndoBypass());

      switch(RADIUM_BLOCKSIZE){
      case 64: b64->setChecked(true); break;
      case 128: b128->setChecked(true); break;
      case 256: b256->setChecked(true); break;
      case 512: b512->setChecked(true); break;
      case 1024: b1024->setChecked(true); break;
      case 2048: b2048->setChecked(true); break;
      case 4096: b4096->setChecked(true); break;
      case 8192: b8192->setChecked(true); break;
      }

      use_jack_if_jack_server_is_running->setChecked(SETTINGS_read_bool("use_jack_if_jack_server_is_running", true));

      {
        enable_latency_compensation->setChecked(latencyCompensationEnabled());
        
        if (getRecordingLatencyFromSystemInputIsAutomaticallyDetermined())
          auto_recording_latency->setChecked(true);
        else
          custom_recording_latency->setChecked(true);
        
        auto_recording_latency->setText(QString("System Out input-latency + Soundcard input+output latency. (")
                                        + QString::number(frames_to_ms(MIXER_get_latency_for_main_system_out()), 'f', 2) + "ms"
                                        + " + "
                                        + QString::number(frames_to_ms(g_audio_system_input_latency + g_audio_system_output_latency), 'f', 2)
                                        + "ms)");
        
        custom_recording_latency->setText(QString("System Out input-latency + Custom recording latency. (")
                                          + QString::number(frames_to_ms(MIXER_get_latency_for_main_system_out()), 'f', 2) + "ms"
                                          + " + "
                                          + QString::number(getCustomRecordingLatencyFromSystemInput(), 'f', 2)
                                          + "ms)");
        
        custom_recording_latency_value->setValue(getCustomRecordingLatencyFromSystemInput());
        
        //custom_recording_latency_layout->setEnabled(!getRecordingLatencyFromSystemInputIsAutomaticallyDetermined());
        //custom_recording_latency_label->setEnabled(!getRecordingLatencyFromSystemInputIsAutomaticallyDetermined());
        //custom_recording_latency_value->setEnabled(!getRecordingLatencyFromSystemInputIsAutomaticallyDetermined());
      }

      {
        switch(getMidiInstrumentLatencyType()){
          case 0: no_midi_instrument_latency_compensation->setChecked(true); break;
          case 1: only_system_out_input_latency->setChecked(true); break;
          case 2: auto_midi_instrument_latency->setChecked(true); break;
          case 3: custom_midi_instrument_latency->setChecked(true); break;
          default: break;
        }

        //auto_midi_instrument_latency->setText("System Out input latency + Soundcard output latency. (" + QString::number(frames_to_ms(g_audio_system_output_latency), 'f', 2) + "ms)");
        
        custom_midi_instrument_latency_value->setValue(getCustomMidiInstrumentLatency());
        
      }

      if (keepOldLoopWhenLoadingNewSample()) {
        
        if (useSameLoopFramesWhenLoadingNewSample())
          keep_old_loop_data_frame->setChecked(true);
        else
          keep_old_loop_data_percentage->setChecked(true);
        
      } else {
        
        discard_loop_data->setChecked(true);
        
      }

      check_abnormal_signals->setChecked(getCheckAbnormalSignals());
    }

    /*
    {
      embedded_audio_files->setText(STRING_get_qstring(getEmbeddedAudioFilesPath().id));
      embedded_audio_group->hide(); // not used yet.
    }
    */
    
    // Disk
    {
      stop_playing_when_saving->setChecked(doStopPlayingWhenSavingSong());

      save_audio_in_song_folder->setChecked(!saveRecordedAudioFilesInBrowserPath());
      save_audio_in_browser_folder->setChecked(saveRecordedAudioFilesInBrowserPath());

      autobackup_onoff->setChecked(doAutoBackups());
      save_backup_while_playing->setChecked(doSaveBackupWhilePlaying());
      autobackup_interval->setValue(autobackupIntervalInMinutes());

      autodelete_takes_when_undoing->setChecked(doAutoDeleteSequencerRecordings());
      treatment0->setChecked(unusedRecordingTakesTreatment()==URTT_NEVER);
      treatment1->setChecked(unusedRecordingTakesTreatment()==URTT_ASK);
      treatment2->setChecked(unusedRecordingTakesTreatment()==URTT_ALWAYS);
    }
    
    // Editor
    {
      pauseRenderingOnoff->setChecked(GL_get_pause_rendering_on_off());
      showKeyCodesOnoff->setChecked(false);

	  use_qwerty_09_editor_subtracks->setChecked(useQwerty09EditorSubtracks());
	  use_qwerty_af_editor_subtracks->setChecked(useQwertyAfEditorSubtracks());
	  
      colorTracksOnoff->setChecked(GL_get_colored_tracks());

      update_waveforms_during_playback->setChecked(SETTINGS_read_bool("enable_editor_rerendering_during_playback",true));

      line_opacity->setValue(g_line_opacity);
      beat_opacity->setValue(g_beat_opacity);
      bar_opacity->setValue(g_bar_opacity);
      
      scrollplay_onoff->setChecked(doScrollPlay());

      multiplyscrollbutton->setChecked(doScrollEditLines());

      autorepeatbutton->setChecked(doAutoRepeat());

      add_notes_on_release->setChecked(doAddNotesWhenReleasingKeys());

      range_paste_cut_button->setChecked(doRangePasteCut());

      range_paste_scroll_down_button->setChecked(doRangePasteScrollDown());

      if (linenumbersVisible())
        showLineNumbers->setChecked(true);
      else
        showBarsAndBeats->setChecked(true);
    }

    // Preset Browser
    {
      presetBrowserRootFolder->setText(SETTINGS_read_qstring("preset_root_folder", QDir::homePath() + QString::fromUtf8("/Radium Presets")));
    }

    // Sequencer
    {

      if (smoothSequencerScrollingEnabled())
        button_everything_else_moving->setChecked(true);
      else
        button_cursor_moving->setChecked(true);

      autoscroll_cursor->setChecked(autoscrollSequencerToMakePlaycursorVisible());
        
      
      if (useJackTransport())
        jack_transport->setChecked(true);
      else
        internal_transport->setChecked(true);

      is_timebase_master->setChecked(isJackTimebaseMaster());
      
      
      default_fadeout->setValue(getDefaultAudiofileFadeout());
      auto_crossfades->setChecked(doAutoCrossfades());

      autoselect_seqtrack_under_mouse->setChecked(autoselectSeqtrackUnderMouse());
      
      autoselect_editor_block_under_mouse->setChecked(autoselectEditorBlockUnderMouse());
      autoselect_editor_block_when_changing_seqtrack->setChecked(autoselectEditorBlockWhenChangingSeqtrack());

      if (sequencerMouseScrollWheelStartsStopsPlaying())
        sequencer_scroll_wheel_starts_stops_playing->setChecked(true);
      else
        sequencer_scroll_wheel_scrolls_up_down->setChecked(true);

      alt_as_horizonal_scroll_modifier->setChecked(SETTINGS_read_bool("alt_as_horizonal_scroll_modifier", false));
    }

    // Windows
    {
	    use_previous_window_positions_during_startup->setChecked(getDoSaveRestoreWindows());
	    
      show_playlist_during_startup->setChecked(showPlaylistDuringStartup());
      show_mixer_strip_during_startup->setChecked(showMixerStripDuringStartup());

      if(showMixerStripOnLeftSide())
        show_mixer_strip_on_the_left->setChecked(true);
      else
        show_mixer_strip_on_the_right->setChecked(true);

      sequencer_window_is_child_of_main_window->setChecked(sequencerWindowIsChildOfMainWindow());
      mixer_window_is_child_of_main_window->setChecked(mixerWindowIsChildOfMainWindow());
      help_window_is_child_of_main_window->setChecked(helpWindowIsChildOfMainWindow());

#if FOR_MACOSX
      swap_ctrl_and_cmd->setChecked(swapCtrlAndCmd());
#else      
      line_swap_ctrl_and_cmd->hide();      
      swap_ctrl_and_cmd->hide();      
#endif
        
      max_num_menu_elements->setValue(getMaxSubmenuEntries());
      tab_bar_height->setValue(getTabBarHeight());
      
      modal_windows->setChecked(doModalWindows());
#if FOR_WINDOWS
      native_file_requesters->hide();
#else
      native_file_requesters->setChecked(useNativeFileRequesters());
#endif
    }

    // Colors
    {
      save_color_file->hide();
      load_color_file->hide();
      
      instrument_brightness->setValue(getInstrumentBrightness()*1000);
      instrument_saturation->setValue(getInstrumentSaturation()*1000);
      instrument_brightness_in_editor->setValue(getInstrumentBrightnessInEditor()*1000);
      instrument_saturation_in_editor->setValue(getInstrumentSaturationInEditor()*1000);
      block_brightness->setValue(getBlockBrightness()*1000);
      block_saturation->setValue(getBlockSaturation()*1000);
      gradient_slider->setValue(getAmountOfGradient()*1000);
    }
    
    // MIDI
    {
      use0x90->setChecked(MIDI_get_use_0x90_for_note_off());
      
      if(MIDI_get_record_velocity())
        record_velocity_on->setChecked(true);
      else
        record_velocity_off->setChecked(true);

      split_into_monophonic_tracks_after_recording->setChecked(doSplitIntoMonophonicTracksAfterRecordingFromMidi());

      use_current_track_midi_channel->setChecked(doUseTrackChannelForMidiInput());
      send_midi_input_to_current_instrument->setChecked(isSendingMidiInputToCurrentInstrument());
        
      while(midi_input_layout->count() > 0)
        delete midi_input_layout->itemAt(0)->widget();

      int num_input_ports;
      const char **input_port_names = MIDIPORT_get_editor_input_ports(&num_input_ports);
      for(int i=0;i<num_input_ports;i++){
        MidiInput *l = new MidiInput(this, input_port_names[i]);
        midi_input_layout->addWidget(l);
      }
              
      /*
      {
        static int a = 0;
        a++;
        MidiInput *l = new MidiInput(this, "hello1 "+QString::number(a));
        midi_input_layout->addWidget(l);
        
        MidiInput *l2 = new MidiInput(this, "hello2");
        midi_input_layout->addWidget(l2);
        
        MidiInput *l3 = new MidiInput(this, "hello3");
        midi_input_layout->addWidget(l3);
      }
      */
    }

    // VST
    vst_widget->updateWidgets();
    
    // Faust
    {
      const char *style = getFaustGuiStyle();
      faust_blue_button->setChecked(!strcmp(style, "Blue"));
      faust_salmon_button->setChecked(!strcmp(style, "Salmon"));
      faust_grey_button->setChecked(!strcmp(style, "Grey"));
      faust_default_button->setChecked(!strcmp(style, "Default"));

#ifdef WITH_FAUST_DEV
      faust_optimization_level->setValue(getFaustOptimizationLevel());
      adjustWidthToFitText(faust_optimization_level, "999999");
#else
      faust_llvm_opt_level_box->hide();
#endif
    }

    minimizeRecursively(this->window());
    
    _is_updating_widgets = false;
  }

  void populate_color_selector(void){
    color_selector->blockSignals(true);

    color_selector->clear();

    QVector<QString> names = GFX_GetColorConfigurationNames();
    for(const auto &name : names)
      color_selector->addItem(name);

    delete_color->setEnabled(names.size() > 1);
    color_selector->setEnabled(names.size() > 1);

    QString current = GFX_GetCurrentColorConfigurationName();
    int index = color_selector->findText(current);
    if (index >= 0)
      color_selector->setCurrentIndex(index);

    color_selector->blockSignals(false);
    color_selector->update();
  }

public slots:

  void on_buttonBox_clicked(QAbstractButton * button){
    //printf("button text: -%s-\n", button->text().toUtf8().constData());
    //if (button->text() == QString("Close")){
    //  printf("close\n");
      this->hide();
      //}// else
    //RError("Unknown button \"%s\"\n",button->text().toUtf8().constData());
  }

  void on_eraseEstimatedVBlankInterval_clicked(){
#if !USE_QT5
    if (_initing==false){
      printf("erasing\n");
      GL_erase_estimated_vblank();
    }
#endif
  }
  
  void on_vsyncOnoff_toggled(bool val){
    if (_initing==false){
#if !USE_QT5
      if (!_is_updating_widgets)
        GL_erase_estimated_vblank(); // makes sense
#endif
      GL_set_vsync(val);
    }
  }

  void on_clampTextRendering_toggled(bool val){
    if (_initing==false)
      GL_set_clamp_text_rendering(val);
  }

#if 0
  void on_safeModeOnoff_toggled(bool val){
    if (_initing==false)
      GL_set_safe_mode(val);
  }
#endif

  void on_high_priority_render_thread_toggled(bool val){
    if (_initing==false)
      GL_set_high_render_thread_priority(val);
  }
  

  void on_rhi_null_toggled(bool val){
    if (_initing==false && val)
      GL_request_setting_backend("null");
  }
  void on_rhi_opengl_toggled(bool val){
    if (_initing==false && val)
      GL_request_setting_backend("opengl");
  }
  void on_rhi_vulkan_toggled(bool val){
    if (_initing==false && val)
      GL_request_setting_backend("vulkan");
  }
  void on_rhi_d3d11_toggled(bool val){
    if (_initing==false && val)
      GL_request_setting_backend("d3d11");
  }
  void on_rhi_d3d12_toggled(bool val){
    if (_initing==false && val)
      GL_request_setting_backend("d3d12");
  }
  void on_rhi_metal_toggled(bool val){
    if (_initing==false && val)
      GL_request_setting_backend("metal");
  }
  
  void on_enable_sample_seek_by_default_toggled(bool val){
    if (_initing==false)
      setEnableSampleSeekByDefault(val);
  }
  
  void on_pauseRenderingOnoff_toggled(bool val){
    if (_initing==false)
      GL_set_pause_rendering_on_off(val);
  }

  void on_showKeyCodesOnoff_toggled(bool val){
    g_show_key_codes = val;
    if (g_show_key_codes==false && _initing==false) {
      root->song->tracker_windows->message=NULL;
      root->song->tracker_windows->must_redraw = true;
    }
  }

  void on_use_qwerty_09_editor_subtracks_toggled(bool val){
    if (_initing==false)
		setUseQwerty09EditorSubtracks(val);
  }
	
  void on_use_qwerty_af_editor_subtracks_toggled(bool val){
    if (_initing==false)
		setUseQwertyAfEditorSubtracks(val);
  }
	
  void on_colorTracksOnoff_toggled(bool val){
    if (_initing==false)
      GL_set_colored_tracks(val);
  }
  
  void on_update_waveforms_during_playback_toggled(bool val){
    if (_initing==false){
      SETTINGS_write_bool("enable_editor_rerendering_during_playback",val);
      g_rt_do_rerendering = val;
    }
  }

  void on_line_opacity_valueChanged(int val){
    if (_initing==false){
      g_line_opacity = val;
      root->song->tracker_windows->must_redraw_editor = true;
      SETTINGS_write_int("line_opacity", val);
    }
  }
  void on_line_opacity_editingFinished(){
    set_editor_focus();

    GL_lock();{
      line_opacity->clearFocus();
    }GL_unlock();
  }

  void on_beat_opacity_valueChanged(int val){
    if (_initing==false){
      g_beat_opacity = val;
      root->song->tracker_windows->must_redraw_editor = true;
      SETTINGS_write_int("beat_opacity", val);
    }
  }
  void on_beat_opacity_editingFinished(){
    set_editor_focus();

    GL_lock();{
      beat_opacity->clearFocus();
    }GL_unlock();
  }

  void on_bar_opacity_valueChanged(int val){
    if (_initing==false){
      g_bar_opacity = val;
      root->song->tracker_windows->must_redraw_editor = true;
      SETTINGS_write_int("first_beat_opacity", val);
    }
  }
  void on_bar_opacity_editingFinished(){
    set_editor_focus();

    GL_lock();{
      bar_opacity->clearFocus();
    }GL_unlock();
  }

  void on_use_previous_window_positions_during_startup_toggled(bool val)
  {
	  if (_initing==false)
		  setDoSaveRestoreWindows(val);
  }

  void on_show_playlist_during_startup_toggled(bool val){
    if (_initing==false)
      setShowPlaylistDuringStartup(val);
  }

  void on_show_mixer_strip_during_startup_toggled(bool val){
    if (_initing==false)
      setShowMixerStripDuringStartup(val);
  }

  void on_show_mixer_strip_on_the_left_toggled(bool val){
    if (_initing==false) 
      setShowMixerStripOnLeftSide(val);
  }

  void on_sequencer_window_is_child_of_main_window_toggled(bool val){
    if (_initing==false)
      setSequencerWindowIsChildOfMainWindow(val);
  }
  
  void on_mixer_window_is_child_of_main_window_toggled(bool val){
    if (_initing==false)
      setMixerWindowIsChildOfMainWindow(val);
  }
  
  void on_help_window_is_child_of_main_window_toggled(bool val){
    if (_initing==false)
      setHelpWindowIsChildOfMainWindow(val);
  }
  
  void on_swap_ctrl_and_cmd_toggled(bool val){
    if (_initing==false)
      setSwapCtrlAndCmd(val);
  }
  
  void on_gcOnOff_toggled(bool val){
    if (_initing==false){
      if (val) {
        printf("   setting ON\n");
        Threadsafe_GC_enable();
      } else {
        printf("   setting OFF\n");
        Threadsafe_GC_disable();      
      }
    }
  }

  void on_incrementalGcNextTime_toggled(bool val){
    if (_initing==false)
      SETTINGS_write_bool("try_incremental_gc",val);
  }

  void on_incrementalGc_toggled(bool val){
    if (_initing==false)
      SETTINGS_write_bool("incremental_gc",val);
  }

  void on_cpu_friendly_audio_meter_updates_toggled(bool val){
    //printf("val: %d\n",val);
    if (_initing==false)
      setUseCPUFriendlyAudiometerUpdates(val);
  }

  void on_nsm_switch_enabled_toggled(bool val){
    //printf("val: %d\n",val);
    if (_initing==false)
      setSupportsSwitchNsmCapability(val);
  }

  void on_enable_remote_control_toggled(bool val){
    //printf("val: %d\n",val);
    if (_initing==false)
      setControlPortOpenForRemoteConnections(val);
  }

  void on_mma1_toggled(bool val){
    if (_initing==false)
      if (val)
        GL_set_multisample(1);
  }

  void on_mma2_toggled(bool val){
    if (_initing==false)
      if (val)
        GL_set_multisample(2);
  }

  void on_mma4_toggled(bool val){
    if (_initing==false)
      if (val)
        GL_set_multisample(4);
  }

  void on_mma8_toggled(bool val){
    if (_initing==false)
      if (val)
        GL_set_multisample(8);
  }

  void on_mma16_toggled(bool val){
    if (_initing==false)
      if (val)
        GL_set_multisample(16);
  }

  void on_mma32_toggled(bool val){
    if (_initing==false)
      if (val)
        GL_set_multisample(32);
  }

  // cpu

  void on_numCPUs_valueChanged(int val){
    printf("cpus: %d\n",val);
    if (_initing==false){
      MULTICORE_set_num_threads(val);
    }
    //set_editor_focus();
    //numCPUs->setFocusPolicy(Qt::NoFocus);
    //on_numCPUs_editingFinished();
  }
  void on_numCPUs_editingFinished(){
    set_editor_focus();

    GL_lock();{
      numCPUs->clearFocus();
    }GL_unlock();

    //numCPUs->setFocusPolicy(Qt::NoFocus);
  }


  // auto-bypass

  void on_enable_autobypass_toggled(bool val){
    if (_initing==false)
      setAutobypassEnabled(val);
  }

  void on_autobypass_delay_valueChanged(int val){
    if (_initing==false)
      setAutobypassDelay(val);
  }

  void on_enable_latency_compensation_toggled(bool val){
    if (_initing==false)
      setLatencyCompensationEnabled(val);
  }
  
  void on_auto_recording_latency_toggled(bool val){
    if (_initing==false && val){
      setRecordingLatencyFromSystemInputIsAutomaticallyDetermined(true);
      /*
      custom_recording_latency_layout->setEnabled(false);
      custom_recording_latency_label->setEnabled(false);
      custom_recording_latency_value->setEnabled(false);
      */
    }
  }
  
  void on_custom_recording_latency_toggled(bool val){
    if (_initing==false && val){
      setRecordingLatencyFromSystemInputIsAutomaticallyDetermined(false);
      /*
      custom_recording_latency_layout->setEnabled(true);
      custom_recording_latency_label->setEnabled(true);
      custom_recording_latency_value->setEnabled(true);
      */
    }
  }

  void on_custom_recording_latency_value_valueChanged(double val){
    if (_initing==false)
      setCustomRecordingLatencyFromSystemInput(val);
  }

  void on_custom_recording_latency_value_editingFinished(){
    set_editor_focus();
    GL_lock();{
      custom_recording_latency_value->clearFocus();
    }GL_unlock();
  }

  void on_tabWidget_currentChanged(int tabnum){
    PREFERENCES_update();
  }
  

  void on_no_midi_instrument_latency_compensation_toggled(bool val){
    if (_initing==false && val)
      setMidiInstrumentLatencyType(0);
  }
  
  void on_only_system_out_input_latency_toggled(bool val){
    if (_initing==false && val)
      setMidiInstrumentLatencyType(1);
  }
  
  void on_auto_midi_instrument_latency_toggled(bool val){
    if (_initing==false && val)
      setMidiInstrumentLatencyType(2);
  }
  
  void on_custom_midi_instrument_latency_toggled(bool val){
    if (_initing==false && val)
      setMidiInstrumentLatencyType(3);
  }
  
  void on_custom_midi_instrument_latency_value_valueChanged(double val){
    if (_initing==false)
      setCustomMidiInstrumentLatency(val);
  }
  
  void on_custom_midi_instrument_latency_value_editingFinished(void){
    set_editor_focus();
    GL_lock();{
      custom_recording_latency_value->clearFocus();
    }GL_unlock();
  }
  
  
  void on_undo_solo_toggled(bool val){
    if (_initing==false)
      setUndoSolo(val);
  }

  void on_undo_bypass_toggled(bool val){
    if (_initing==false)
      setUndoBypass(val);
  }

  void on_autobypass_delay_editingFinished(){
    set_editor_focus();
    GL_lock();{
      autobypass_delay->clearFocus();
    }GL_unlock();
  }

  void on_b64_toggled(bool val){ if (_initing==false)  SETTINGS_write_int("audio_block_size", 64);  }
  void on_b128_toggled(bool val){ if (_initing==false)  SETTINGS_write_int("audio_block_size", 128);  }
  void on_b256_toggled(bool val){ if (_initing==false)  SETTINGS_write_int("audio_block_size", 256);  }
  void on_b512_toggled(bool val){ if (_initing==false)  SETTINGS_write_int("audio_block_size", 512);  }
  void on_b1024_toggled(bool val){ if (_initing==false)  SETTINGS_write_int("audio_block_size", 1024);  }
  void on_b2048_toggled(bool val){ if (_initing==false)  SETTINGS_write_int("audio_block_size", 2048);  }
  void on_b4096_toggled(bool val){ if (_initing==false)  SETTINGS_write_int("audio_block_size", 4096);  }
  void on_b8192_toggled(bool val){ if (_initing==false)  SETTINGS_write_int("audio_block_size", 8192);  }

  void on_use_jack_if_jack_server_is_running_toggled(bool val){
    if (_initing==false)
      SETTINGS_write_bool("use_jack_if_jack_server_is_running", val);
  }
  
  void on_check_abnormal_signals_toggled(bool val){
    if (_initing==false)
      setCheckAbnormalSignals(val);
  }
  
  void on_discard_loop_data_toggled(bool val){
    if (_initing==false && val)
      setKeepOldLoopWhenLoadingNewSample(false);
  }
  void on_keep_old_loop_data_frame_toggled(bool val){
    if (_initing==false && val){
      setUseSameLoopFramesWhenLoadingNewSample(true);
      setKeepOldLoopWhenLoadingNewSample(true);
    }
  }
  void on_keep_old_loop_data_percentage_toggled(bool val){
    if (_initing==false && val){
      setUseSameLoopFramesWhenLoadingNewSample(false);
      setKeepOldLoopWhenLoadingNewSample(true);
    }
  }


  /*
  // embedded audio file paths
  void on_embedded_audio_files_editingFinished(){
    setEmbeddedAudioFilesPath(make_filepath(embedded_audio_files->text()));
    set_editor_focus();
    
    GL_lock();{
      embedded_audio_files->clearFocus();
    }GL_unlock();
  }
  */
  
  // Disk
  //
  void on_stop_playing_when_saving_toggled(bool val){
    //printf("val: %d\n",val);
    if (_initing==false)
      setStopPlayingWhenSavingSong(val);
  }
  
  void on_save_audio_in_song_folder_toggled(bool val){
    //printf("val: %d\n",val);
    if (_initing==false)
      setSaveRecordedAudioFilesInBrowserPath(!val);
  }
  
  void on_save_audio_in_browser_folder_toggled(bool val){
    //printf("val: %d\n",val);
    if (_initing==false)
      setSaveRecordedAudioFilesInBrowserPath(val);
  }
  
  void on_autobackup_onoff_toggled(bool val){
    if (_initing==false)
      setDoAutoBackups(val);
  }

  void on_save_backup_while_playing_toggled(bool val){
    printf("val2: %d\n",val);
    if (_initing==false)
      setSaveBackupWhilePlaying(val);
  }
  
  void on_autobackup_interval_valueChanged(int val){
    printf("val: %d\n",val);
    if (_initing==false)
      setAutobackupIntervalInMinutes(val);
  }
  void on_autobackup_interval_editingFinished(){
    set_editor_focus();

    GL_lock();{
      autobackup_interval->clearFocus();
    }GL_unlock();
  }


  void on_autodelete_takes_when_undoing_toggled(bool val){
    if (_initing==false)
      setDoAutoDeleteSequencerRecordings(val);
  }

  void on_treatment0_toggled(bool val){
    if (_initing==false && val)
      setUnusedRecordingTakesTreatment(URTT_NEVER);
  }
  
  void on_treatment1_toggled(bool val){
    if (_initing==false && val)
      setUnusedRecordingTakesTreatment(URTT_ASK);
  }
  
  void on_treatment2_toggled(bool val){
    if (_initing==false && val)
      setUnusedRecordingTakesTreatment(URTT_ALWAYS);
  }
  
  // editor
  //
  
  void on_scrollplay_onoff_toggled(bool val){
    if (_initing==false)
      setScrollPlay(val);
  }
  void on_multiplyscrollbutton_toggled(bool val){
    if (_initing==false)
      setScrollEditLines(val);
  }
  void on_autorepeatbutton_toggled(bool val){
    if (_initing==false)
      setAutoRepeat(val);
  }
  void on_add_notes_on_release_toggled(bool val){
    if (_initing==false)
      setAddNotesWhenReleasingKeys(val);
  }
  void on_range_paste_cut_button_toggled(bool val){
    if (_initing==false)
      setRangePasteCut(val);
  }
  void on_range_paste_scroll_down_button_toggled(bool val){
    if (_initing==false)
      setRangePasteScrollDown(val);
  }
  void on_showLineNumbers_toggled(bool val){
    if (_initing==false)
      setLinenumbersVisible(val);
  }

  void on_presetBrowserRootFolder_editingFinished(){
    SETTINGS_write_string("preset_root_folder", presetBrowserRootFolder->text());
      
    set_editor_focus();

    GL_lock();{
      presetBrowserRootFolder->clearFocus();
    }GL_unlock();
  }


  // sequencer
  //
  void on_button_everything_else_moving_toggled(bool val){
    if (_initing==false)
      setSmoothSequencerScrollingEnabled(val);
  }

  void on_autoscroll_cursor_toggled(bool val){
    if (_initing==false)
      setAutoscrollSequencerToMakePlaycursorVisible(val);
  }

  void on_jack_transport_toggled(bool val){
    if (_initing==false)
      setUseJackTransport(val);
  }

  void on_is_timebase_master_toggled(bool val){
    if (_initing==false)
      setIsJackTimebaseMaster(val);
  }
  
  void on_default_fadeout_valueChanged(double val){
    if (_initing==false)
      setDefaultAudiofileFadeout(val);
  }

  void on_default_fadeout_editingFinished(){
    set_editor_focus();

    GL_lock();{
      default_fadeout->clearFocus();
    }GL_unlock();
  }

  void on_auto_crossfades_toggled(bool val){
    if (_initing==false)
      setDoAutoCrossfades(val);
  }

  void on_autoselect_editor_block_under_mouse_toggled(bool val){
    if (_initing==false)
      setAutoselectEditorBlockUnderMouse(val);
  }

  void on_autoselect_editor_block_when_changing_seqtrack_toggled(bool val){
    if (_initing==false)
      setAutoselectEditorBlockWhenChangingSeqtrack(val);
  }
  
  void on_autoselect_seqtrack_under_mouse_toggled(bool val){
    if (_initing==false)
      setAutoselectSeqtrackUnderMouse(val);
  }

  void on_sequencer_scroll_wheel_starts_stops_playing_toggled(bool val){
    if (_initing==false)
      setSequencerMouseScrollWheelStartsStopsPlaying(val);
  }

  void on_alt_as_horizonal_scroll_modifier_toggled(bool val){
    if (_initing==false)
      SETTINGS_write_bool("alt_as_horizonal_scroll_modifier", val);
  }
  
  // colors
  void color_changed(const QColor &col){
    printf("COLOR_CHANGED: g_current_colornum=%d new_color=%s\n", g_current_colornum, col.name(QColor::HexArgb).toUtf8().constData());
    testColorInRealtime(g_current_colornum, col);
    
    if (g_in_undo_redo){
      printf("color_changed: skipped (in_undo_redo)\n");
      goto color_changed_end;
    }
    
    if (g_pending_change_committed==false && col==g_pending_old_color){
      printf("color_changed: skipped (same_color colornum=%d)\n", g_current_colornum);
      goto color_changed_end;
    }
    
    if (g_pending_change_committed==false){
      printf("color_changed: PUSH colornum=%d restore_to=%s\n", g_current_colornum, g_pending_old_color.name().toUtf8().constData());
      g_color_undo_stack.push_back({false, g_current_colornum, g_pending_old_color, 0, 0.0f});
      g_pending_change_committed = true;
    } else {
      printf("color_changed: consecutive colornum=%d (already committed)\n", g_current_colornum);
    }
    g_color_redo_stack.clear();
    update_color_undo_redo_buttons();

  color_changed_end:
    for(auto button : all_buttons){
      if (button->colornum == LOW_EDITOR_BACKGROUND_COLOR_NUM || button->colornum == HIGH_EDITOR_BACKGROUND_COLOR_NUM)
        printf("color_changed_end: updating colornum=%d isVisible=%d isHidden=%d\n", button->colornum, button->isVisible(), button->isHidden());
      button->update();
    }
  }

  void current_tab_changed(int tabnum){
#if FOR_MACOSX && !USE_QT5
    printf("   CHangeEvent called %d\n",tabnum);
    if (tabWidget->currentWidget() != colors)
      _color_dialog.close();
    else
      _color_dialog.myshow();
#endif
    if (tabWidget->currentWidget() == colors)
      obtain_keyboard_focus_without_greying();
    else
      release_keyboard_focus();
  }

  void update_color_undo_redo_buttons(void){
    printf("==== update_buttons: undo_stack=%d  redo_stack=%d\n", (int)g_color_undo_stack.size(), (int)g_color_redo_stack.size());
    color_undo->setEnabled(g_color_undo_stack.isEmpty()==false);
    color_redo->setEnabled(g_color_redo_stack.isEmpty()==false);
  }

  void on_color_undo_clicked(){
    printf("UNDO clicked: undo_stack=%d  redo_stack=%d\n", (int)g_color_undo_stack.size(), (int)g_color_redo_stack.size());
    if (g_color_undo_stack.isEmpty())
      return;

    PreferencesUndoEntry entry = g_color_undo_stack.last();
    g_color_undo_stack.pop_back();

    if (entry.is_slider) {
      int current_value = get_slider_value(entry.slider_id);
      printf("UNDO slider: slider_id=%d  restoring=%d  was=%d\n", entry.slider_id, (int)entry.slider_value, current_value);
      g_color_redo_stack.push_back({true, (enum ColorNums)0, QColor(), entry.slider_id, (float)current_value});

      g_in_undo_redo = true;
      set_slider_value(entry.slider_id, (int)entry.slider_value);
      g_in_undo_redo = false;
    } else {    
      QColor current_value = get_qcolor(entry.colornum);
      printf("UNDO: colornum=%d  restoring=%s  was=%s\n", entry.colornum, entry.color.name().toUtf8().constData(), current_value.name().toUtf8().constData());
      g_color_redo_stack.push_back({false, entry.colornum, current_value, 0, 0.0f});

      g_in_undo_redo = true;
      testColorInRealtime(entry.colornum, entry.color);

      if (g_current_colornum==entry.colornum)
        _color_dialog.setCurrentColor(entry.color);

      g_in_undo_redo = false;

      for(auto button : all_buttons)
        button->update();
    }

    update_color_undo_redo_buttons();
  }

  void on_color_redo_clicked(){
    printf("REDO clicked: undo_stack=%d  redo_stack=%d\n", (int)g_color_undo_stack.size(), (int)g_color_redo_stack.size());
    if (g_color_redo_stack.isEmpty())
      return;

    PreferencesUndoEntry entry = g_color_redo_stack.last();
    g_color_redo_stack.pop_back();

    if (entry.is_slider) {
      int current_value = get_slider_value(entry.slider_id);
      printf("REDO slider: slider_id=%d  restoring=%d  was=%d\n", entry.slider_id, (int)entry.slider_value, current_value);
      g_color_undo_stack.push_back({true, (enum ColorNums)0, QColor(), entry.slider_id, (float)current_value});

      g_in_undo_redo = true;
      set_slider_value(entry.slider_id, (int)entry.slider_value);
      g_in_undo_redo = false;
    } else {
      QColor current_value = get_qcolor(entry.colornum);
      printf("REDO: colornum=%d  restoring=%s  was=%s\n", entry.colornum, entry.color.name().toUtf8().constData(), current_value.name().toUtf8().constData());
      g_color_undo_stack.push_back({false, entry.colornum, current_value, 0, 0.0f});

      g_in_undo_redo = true;
      testColorInRealtime(entry.colornum, entry.color);

      if (g_current_colornum==entry.colornum)
        _color_dialog.setCurrentColor(entry.color);

      g_in_undo_redo = false;

      for(auto button : all_buttons)
        button->update();
    }

    update_color_undo_redo_buttons();
  }

  void on_color_store_snapshot_clicked(){
    GFX_StoreCheckpoint();
  }

  void on_color_revert_color_clicked(){
    GFX_RevertColorFromCheckpoint(g_current_colornum);
    _color_dialog.setCurrentColor(get_qcolor(g_current_colornum));

    for(auto button : all_buttons)
      button->update();
  }

  void on_color_revert_all_colors_clicked(){
    GFX_RevertAllToCheckpoint();
    _color_dialog.setCurrentColor(get_qcolor(g_current_colornum));
    update_color_sliders();

    for(auto button : all_buttons)
      button->update();
  }

  void on_color_set_default_color_clicked(){
    GFX_SetDefaultColor(g_current_colornum);
    _color_dialog.setCurrentColor(get_qcolor(g_current_colornum));

    for(auto button : all_buttons)
      button->update();
  }

  void on_color_set_all_default_colors_clicked(){
    GFX_SetDefaultColors1(root->song->tracker_windows);

    instrument_brightness->setValue(1000 * DEFAULT_INSTRUMENT_BRIGHTNESS);
    instrument_saturation->setValue(1000 * DEFAULT_INSTRUMENT_SATURATION);
    instrument_brightness_in_editor->setValue(1000 * DEFAULT_INSTRUMENT_BRIGHTNESS_IN_EDITOR);
    instrument_saturation_in_editor->setValue(1000 * DEFAULT_INSTRUMENT_SATURATION_IN_EDITOR);
    block_brightness->setValue(1000 * DEFAULT_BLOCK_BRIGHTNESS);
    block_saturation->setValue(1000 * DEFAULT_BLOCK_SATURATION);
    gradient_slider->setValue(1000 * DEFAULT_AMOUNT_OF_GRADIENT);

    _color_dialog.setCurrentColor(get_qcolor(g_current_colornum));

    for(auto button : all_buttons)
      button->update();
  }


  void on_save_color_file_clicked(){
    filepath_t filename = GFX_GetSaveFileName(root->song->tracker_windows,
                                              NULL,
                                              "Select file",
                                              createIllegalFilepath(),
                                              NULL,
                                              NULL,
                                              "",
                                              true);
    if (isLegalFilepath(filename))
      GFX_SaveColors(filename);
  }

  void update_color_sliders(void){
    instrument_brightness->setValue(getInstrumentBrightness()*1000);
    instrument_saturation->setValue(getInstrumentSaturation()*1000);
    instrument_brightness_in_editor->setValue(getInstrumentBrightnessInEditor()*1000);
    instrument_saturation_in_editor->setValue(getInstrumentSaturationInEditor()*1000);
    block_brightness->setValue(getBlockBrightness()*1000);
    block_saturation->setValue(getBlockSaturation()*1000);
    gradient_slider->setValue(getAmountOfGradient()*1000);
  }

  void on_rename_color_clicked(){
    QString old_name = GFX_GetCurrentColorConfigurationName();

    ReqType reqtype = GFX_OpenReq(root->song->tracker_windows, 50, 4, "");
    GFX_SetString(reqtype, old_name.toUtf8().constData());

    const char *new_name = GFX_GetString(root->song->tracker_windows, reqtype, "New name: ", true);

    GFX_CloseReq(root->song->tracker_windows, reqtype);

    if (new_name != NULL && strlen(new_name) > 0 && old_name != new_name){
      GFX_RenameColorConfiguration(old_name, new_name);
      populate_color_selector();
    }
  }

  void on_add_color_clicked(){
    const char *name = GFX_GetString(root->song->tracker_windows, NULL, "New color configuration:", true);

    if (name != NULL && strlen(name) > 0){
      GFX_NewColorConfiguration(name);
      populate_color_selector();
      update_color_sliders();
      _color_dialog.setCurrentColor(get_qcolor(g_current_colornum));
    }
  }

  void on_delete_color_clicked(){
    QString name = GFX_GetCurrentColorConfigurationName();
    GFX_DeleteColorConfiguration(name);
    populate_color_selector();
    update_color_sliders();
    _color_dialog.setCurrentColor(get_qcolor(g_current_colornum));
  }

  void on_color_selector_activated(int index){
    QString name = color_selector->itemText(index);
    GFX_SetColorConfiguration(name);
    update_color_sliders();
    _color_dialog.setCurrentColor(get_qcolor(g_current_colornum));
  }


  int get_slider_value(int slider_id){
    switch(slider_id){
      case SLIDER_INSTRUMENT_BRIGHTNESS: return instrument_brightness->value();
      case SLIDER_INSTRUMENT_SATURATION: return instrument_saturation->value();
      case SLIDER_INSTRUMENT_BRIGHTNESS_IN_EDITOR: return instrument_brightness_in_editor->value();
      case SLIDER_INSTRUMENT_SATURATION_IN_EDITOR: return instrument_saturation_in_editor->value();
      case SLIDER_BLOCK_BRIGHTNESS: return block_brightness->value();
      case SLIDER_BLOCK_SATURATION: return block_saturation->value();
      case SLIDER_GRADIENT: return gradient_slider->value();
      default: return 0;
    }
  }

  void set_slider_value(int slider_id, int value){
    switch(slider_id){
      case SLIDER_INSTRUMENT_BRIGHTNESS: instrument_brightness->setValue(value); break;
      case SLIDER_INSTRUMENT_SATURATION: instrument_saturation->setValue(value); break;
      case SLIDER_INSTRUMENT_BRIGHTNESS_IN_EDITOR: instrument_brightness_in_editor->setValue(value); break;
      case SLIDER_INSTRUMENT_SATURATION_IN_EDITOR: instrument_saturation_in_editor->setValue(value); break;
      case SLIDER_BLOCK_BRIGHTNESS: block_brightness->setValue(value); break;
      case SLIDER_BLOCK_SATURATION: block_saturation->setValue(value); break;
      case SLIDER_GRADIENT: gradient_slider->setValue(value); break;
    }
  }

  void on_instrument_brightness_sliderPressed(){
    if (_initing==false){
      g_pending_old_slider_values[SLIDER_INSTRUMENT_BRIGHTNESS] = instrument_brightness->value();
      g_slider_committed[SLIDER_INSTRUMENT_BRIGHTNESS] = false;
    }
  }
  void on_instrument_brightness_valueChanged(int val){
    if (_initing==false){
      setInstrumentBrightness((float)val/1000.0);
      if (!g_in_undo_redo && !g_slider_committed[SLIDER_INSTRUMENT_BRIGHTNESS] && val!=g_pending_old_slider_values[SLIDER_INSTRUMENT_BRIGHTNESS]){
        g_color_undo_stack.push_back({true, (enum ColorNums)0, QColor(), SLIDER_INSTRUMENT_BRIGHTNESS, (float)g_pending_old_slider_values[SLIDER_INSTRUMENT_BRIGHTNESS]});
        g_color_redo_stack.clear();
        g_slider_committed[SLIDER_INSTRUMENT_BRIGHTNESS] = true;
        update_color_undo_redo_buttons();
      }
    }
  }
  void on_instrument_brightness_sliderReleased(){
    g_pending_old_slider_values[SLIDER_INSTRUMENT_BRIGHTNESS] = instrument_brightness->value();
    g_slider_committed[SLIDER_INSTRUMENT_BRIGHTNESS] = false;
  }
  void on_reset_instrument_brightness_clicked(){
    instrument_brightness->setValue(1000 * DEFAULT_INSTRUMENT_BRIGHTNESS);
  }

  void on_instrument_saturation_sliderPressed(){
    if (_initing==false){
      g_pending_old_slider_values[SLIDER_INSTRUMENT_SATURATION] = instrument_saturation->value();
      g_slider_committed[SLIDER_INSTRUMENT_SATURATION] = false;
    }
  }
  void on_instrument_saturation_valueChanged(int val){
    if (_initing==false){
      setInstrumentSaturation((float)val/1000.0);
      if (!g_in_undo_redo && !g_slider_committed[SLIDER_INSTRUMENT_SATURATION] && val!=g_pending_old_slider_values[SLIDER_INSTRUMENT_SATURATION]){
        g_color_undo_stack.push_back({true, (enum ColorNums)0, QColor(), SLIDER_INSTRUMENT_SATURATION, (float)g_pending_old_slider_values[SLIDER_INSTRUMENT_SATURATION]});
        g_color_redo_stack.clear();
        g_slider_committed[SLIDER_INSTRUMENT_SATURATION] = true;
        update_color_undo_redo_buttons();
      }
    }
  }
  void on_instrument_saturation_sliderReleased(){
    g_pending_old_slider_values[SLIDER_INSTRUMENT_SATURATION] = instrument_saturation->value();
    g_slider_committed[SLIDER_INSTRUMENT_SATURATION] = false;
  }
  void on_reset_instrument_saturation_clicked(){
    instrument_saturation->setValue(1000 * DEFAULT_INSTRUMENT_SATURATION);
  }

  void on_instrument_brightness_in_editor_sliderPressed(){
    if (_initing==false){
      g_pending_old_slider_values[SLIDER_INSTRUMENT_BRIGHTNESS_IN_EDITOR] = instrument_brightness_in_editor->value();
      g_slider_committed[SLIDER_INSTRUMENT_BRIGHTNESS_IN_EDITOR] = false;
    }
  }
  void on_instrument_brightness_in_editor_valueChanged(int val){
    if (_initing==false){
      setInstrumentBrightnessInEditor((float)val/1000.0);
      if (!g_in_undo_redo && !g_slider_committed[SLIDER_INSTRUMENT_BRIGHTNESS_IN_EDITOR] && val!=g_pending_old_slider_values[SLIDER_INSTRUMENT_BRIGHTNESS_IN_EDITOR]){
        g_color_undo_stack.push_back({true, (enum ColorNums)0, QColor(), SLIDER_INSTRUMENT_BRIGHTNESS_IN_EDITOR, (float)g_pending_old_slider_values[SLIDER_INSTRUMENT_BRIGHTNESS_IN_EDITOR]});
        g_color_redo_stack.clear();
        g_slider_committed[SLIDER_INSTRUMENT_BRIGHTNESS_IN_EDITOR] = true;
        update_color_undo_redo_buttons();
      }
    }
  }
  void on_instrument_brightness_in_editor_sliderReleased(){
    g_pending_old_slider_values[SLIDER_INSTRUMENT_BRIGHTNESS_IN_EDITOR] = instrument_brightness_in_editor->value();
    g_slider_committed[SLIDER_INSTRUMENT_BRIGHTNESS_IN_EDITOR] = false;
  }
  void on_reset_instrument_brightness_in_editor_clicked(){
    instrument_brightness_in_editor->setValue(1000 * DEFAULT_INSTRUMENT_BRIGHTNESS_IN_EDITOR);
  }

  void on_instrument_saturation_in_editor_sliderPressed(){
    if (_initing==false){
      g_pending_old_slider_values[SLIDER_INSTRUMENT_SATURATION_IN_EDITOR] = instrument_saturation_in_editor->value();
      g_slider_committed[SLIDER_INSTRUMENT_SATURATION_IN_EDITOR] = false;
    }
  }
  void on_instrument_saturation_in_editor_valueChanged(int val){
    if (_initing==false){
      setInstrumentSaturationInEditor((float)val/1000.0);
      if (!g_in_undo_redo && !g_slider_committed[SLIDER_INSTRUMENT_SATURATION_IN_EDITOR] && val!=g_pending_old_slider_values[SLIDER_INSTRUMENT_SATURATION_IN_EDITOR]){
        g_color_undo_stack.push_back({true, (enum ColorNums)0, QColor(), SLIDER_INSTRUMENT_SATURATION_IN_EDITOR, (float)g_pending_old_slider_values[SLIDER_INSTRUMENT_SATURATION_IN_EDITOR]});
        g_color_redo_stack.clear();
        g_slider_committed[SLIDER_INSTRUMENT_SATURATION_IN_EDITOR] = true;
        update_color_undo_redo_buttons();
      }
    }
  }
  void on_instrument_saturation_in_editor_sliderReleased(){
    g_pending_old_slider_values[SLIDER_INSTRUMENT_SATURATION_IN_EDITOR] = instrument_saturation_in_editor->value();
    g_slider_committed[SLIDER_INSTRUMENT_SATURATION_IN_EDITOR] = false;
  }
  void on_reset_instrument_saturation_in_editor_clicked(){
    instrument_saturation_in_editor->setValue(1000 * DEFAULT_INSTRUMENT_SATURATION_IN_EDITOR);
  }

  void on_block_brightness_sliderPressed(){
    if (_initing==false){
      g_pending_old_slider_values[SLIDER_BLOCK_BRIGHTNESS] = block_brightness->value();
      g_slider_committed[SLIDER_BLOCK_BRIGHTNESS] = false;
    }
  }
  void on_block_brightness_valueChanged(int val){
    if (_initing==false){
      setBlockBrightness((float)val/1000.0);
      if (!g_in_undo_redo && !g_slider_committed[SLIDER_BLOCK_BRIGHTNESS] && val!=g_pending_old_slider_values[SLIDER_BLOCK_BRIGHTNESS]){
        g_color_undo_stack.push_back({true, (enum ColorNums)0, QColor(), SLIDER_BLOCK_BRIGHTNESS, (float)g_pending_old_slider_values[SLIDER_BLOCK_BRIGHTNESS]});
        g_color_redo_stack.clear();
        g_slider_committed[SLIDER_BLOCK_BRIGHTNESS] = true;
        update_color_undo_redo_buttons();
      }
    }
  }
  void on_block_brightness_sliderReleased(){
    g_pending_old_slider_values[SLIDER_BLOCK_BRIGHTNESS] = block_brightness->value();
    g_slider_committed[SLIDER_BLOCK_BRIGHTNESS] = false;
  }
  void on_reset_block_brightness_clicked(){
    block_brightness->setValue(1000 * DEFAULT_BLOCK_BRIGHTNESS);
  }

  void on_block_saturation_sliderPressed(){
    if (_initing==false){
      g_pending_old_slider_values[SLIDER_BLOCK_SATURATION] = block_saturation->value();
      g_slider_committed[SLIDER_BLOCK_SATURATION] = false;
    }
  }
  void on_block_saturation_valueChanged(int val){
    if (_initing==false){
      setBlockSaturation((float)val/1000.0);
      if (!g_in_undo_redo && !g_slider_committed[SLIDER_BLOCK_SATURATION] && val!=g_pending_old_slider_values[SLIDER_BLOCK_SATURATION]){
        g_color_undo_stack.push_back({true, (enum ColorNums)0, QColor(), SLIDER_BLOCK_SATURATION, (float)g_pending_old_slider_values[SLIDER_BLOCK_SATURATION]});
        g_color_redo_stack.clear();
        g_slider_committed[SLIDER_BLOCK_SATURATION] = true;
        update_color_undo_redo_buttons();
      }
    }
  }
  void on_block_saturation_sliderReleased(){
    g_pending_old_slider_values[SLIDER_BLOCK_SATURATION] = block_saturation->value();
    g_slider_committed[SLIDER_BLOCK_SATURATION] = false;
  }
  void on_reset_block_saturation_clicked(){
    block_saturation->setValue(1000 * DEFAULT_BLOCK_SATURATION);
  }

  void on_gradient_slider_sliderPressed(){
    if (_initing==false){
      g_pending_old_slider_values[SLIDER_GRADIENT] = gradient_slider->value();
      g_slider_committed[SLIDER_GRADIENT] = false;
    }
  }
  void on_gradient_slider_valueChanged(int val){
    if (_initing==false){
      setAmountOfGradient((float)val/1000.0);
      if (!g_in_undo_redo && !g_slider_committed[SLIDER_GRADIENT] && val!=g_pending_old_slider_values[SLIDER_GRADIENT]){
        g_color_undo_stack.push_back({true, (enum ColorNums)0, QColor(), SLIDER_GRADIENT, (float)g_pending_old_slider_values[SLIDER_GRADIENT]});
        g_color_redo_stack.clear();
        g_slider_committed[SLIDER_GRADIENT] = true;
        update_color_undo_redo_buttons();
      }
    }
  }
  void on_gradient_slider_sliderReleased(){
    g_pending_old_slider_values[SLIDER_GRADIENT] = gradient_slider->value();
    g_slider_committed[SLIDER_GRADIENT] = false;
  }
  void on_reset_gradient_button_clicked(){
    gradient_slider->setValue(1000 * DEFAULT_AMOUNT_OF_GRADIENT);
  }
  
  // windows

  void on_max_num_menu_elements_valueChanged(int val){
    printf("max menu entries: %d\n",val);
    if (_initing==false)
      setMaxSubmenuEntries(val);
  }
  void on_max_num_menu_elements_editingFinished(){
    set_editor_focus();

    GL_lock();{
      max_num_menu_elements->clearFocus();
    }GL_unlock();
  }

  void on_tab_bar_height_valueChanged(double val){
    if (_initing==false)
      setTabBarHeight(val);
  }
  void on_tab_bar_height_editingFinished(){
    set_editor_focus();

    GL_lock();{
      tab_bar_height->clearFocus();
    }GL_unlock();
  }

  void on_modal_windows_toggled(bool val){
    if (_initing==false)
      setModalWindows(val);
  }

  void on_native_file_requesters_toggled(bool val){
    if (_initing==false)
      setUseNativeFileRequesters(val);
  }

  
  // MIDI

  void on_set_input_port_clicked(){
    MIDISetInputPort(true);
  }

  void on_use0x90_toggled(bool val){
    if (_initing==false)
      MIDI_set_use_0x90_for_note_off(val);
  }

  void on_record_velocity_on_toggled(bool val){
    if (_initing==false)
      MIDI_set_record_velocity(val);
  }

  void on_split_into_monophonic_tracks_after_recording_toggled(bool val){
    if (_initing==false)
      setSplitIntoMonophonicTracksAfterRecordingFromMidi(val);
  }
  
  void on_use_current_track_midi_channel_toggled(bool val){
    if (_initing==false)
      setUseTrackChannelForMidiInput(val);
  }
  
  void on_send_midi_input_to_current_instrument_toggled(bool val){
    if (_initing==false)
      setSendMidiInputToCurrentInstrument(val);
  }
  
  // Faust

  void on_faust_blue_button_toggled(bool val){
    if (_initing==false && val)
      setFaustGuiStyle("Blue");
  }
  void on_faust_salmon_button_toggled(bool val){
    if (_initing==false && val)
      setFaustGuiStyle("Salmon");
  }
  void on_faust_grey_button_toggled(bool val){
    if (_initing==false && val)
      setFaustGuiStyle("Grey");
  }
  void on_faust_default_button_toggled(bool val){
    if (_initing==false && val)
      setFaustGuiStyle("Default");
  }
  
  void on_faust_optimization_level_valueChanged(int val){
    if (_initing==false)
      setFaustOptimizationLevel(val);
  }
  
  void on_faust_optimization_level_editingFinished(){
    set_editor_focus();

    GL_lock();{
      faust_optimization_level->clearFocus();
    }GL_unlock();
  }
  
};
}



/*
static void ensure_widget_is_created(void){
}
*/

static QPointer<Preferences> g_preferences_widget;

void PREFERENCES_open(void){
  if(g_preferences_widget.isNull()){
    g_preferences_widget = new Preferences(g_main_window);
    
    g_static_toplevel_widgets.push_back(g_preferences_widget.data());
  }

  safeShowOrExec(g_preferences_widget, true);
}

void PREFERENCES_open_MIDI(void){
  PREFERENCES_open();
  g_preferences_widget->tabWidget->setCurrentWidget(g_preferences_widget->MIDI);
}

void PREFERENCES_open_sequencer(void){
  PREFERENCES_open();
  g_preferences_widget->tabWidget->setCurrentWidget(g_preferences_widget->sequencer);
}

void PREFERENCES_update(void){
  if (false==g_preferences_widget.isNull()){
    g_preferences_widget->_needs_to_update = true;
  
    if (g_preferences_widget->isVisible())
        g_preferences_widget->updateWidgets();
  }
}

namespace{
  struct VST_paths_dialog : public RememberGeometryQDialog {
    VST_paths_dialog(QWidget *parent)
      : RememberGeometryQDialog(parent, radium::MAY_BE_MODAL)
    {
      setWindowTitle("VST preferences");

      auto *child = new Vst_paths_widget;

      QVBoxLayout *mainLayout = new QVBoxLayout;
      setLayout(mainLayout);
      mainLayout->addWidget(child);

      connect(child->buttonBox,SIGNAL(accepted()),this,SLOT(hide()));
    }
  };
}

static QPointer<VST_paths_dialog> g_vst_paths_dialog;

void OS_VST_config(struct Tracker_Windows *window){
#if defined(FOR_MACOSX)
  GFX_addMessage("No VST options to edit on OSX");
#else
  //EditorWidget *editor=(EditorWidget *)window->os_visual.widget;
  if (g_vst_paths_dialog.isNull())
    g_vst_paths_dialog=new VST_paths_dialog(g_main_window);

  safeShow(g_vst_paths_dialog.data());
  
#endif  
  printf("Ohjea\n");
}

bool OS_VST_config_visible(void){
  if (g_vst_paths_dialog.isNull())
    return false;

  return g_vst_paths_dialog->isVisible();
}
  
#include "mQt_preferences_callbacks.cpp"

