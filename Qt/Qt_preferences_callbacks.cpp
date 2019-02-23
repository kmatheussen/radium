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

#include "../common/nsmtracker.h"
#include "../common/hashmap_proc.h"
#include "../common/OS_string_proc.h"
#include "../common/OS_settings_proc.h"
#include "../common/settings_proc.h"
#include "../OpenGL/Widget_proc.h"
#include "../OpenGL/Render_proc.h"
#include "../audio/MultiCore_proc.h"
#include "../midi/midi_i_input_proc.h"
#include "../midi/midi_instrument_proc.h"
#include "../midi/midi_menues_proc.h"
#include "../midi/OS_midi_proc.h"

#include "../api/api_proc.h"

#include "Qt_MyQSpinBox.h"
#include "Qt_MyQScrollBar.hpp"
#include <FocusSniffers.h>
#include "helpers.h"

#include "Qt_colors_proc.h"

#include "mQt_vst_paths_widget_callbacks.h"

#include "Qt_preferences.h"



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

struct MyColorDialog : public QColorDialog {

public: 

  MyColorDialog(){
    setOption(QColorDialog::NoButtons); // Avoid crash on macos sierra. (https://bugreports.qt.io/browse/QTBUG-56448)
  }

#if FOR_MACOSX && !USE_QT5
  void closeEvent(QCloseEvent *event) {
    hide();
    event->ignore(); // Only hide the window, dont close it.
  }
  void myshow(){
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

    QColor text_color = get_qcolor(TEXT_COLOR_NUM); //black(0,0,0);

    /*
    QColor white(255,255,255);
    QColor col;
    if (is_current){
      col = black;
      p.setPen(white);
    } else {
      col = white;
      p.setPen(black);
    }
    
    
    p.fillRect(half_width,0,half_width,height(),col);
    */

    QRect rect(split+1,1,text_width-2,height()-1);

    p.setPen(text_color);
    p.drawText(rect, Qt::AlignCenter, text());

    p.fillRect(0,0,split,height(),get_qcolor(colornum));

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

    g_current_colornum = colornum;
    color_dialog->setCurrentColor(get_qcolor(colornum));
#if FOR_MACOSX && !USE_QT5
    color_dialog->myshow();
#endif
    
    update();
  }

  public slots:

  void color_pressed(){
    printf("Color %d pressed to %d\n",colornum,is_current);
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
    MIDI_OS_RemoveInputPort(name.toUtf8().constData());
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

    scrollArea->setHorizontalScrollBar(new Qt_MyQScrollBar(Qt::Horizontal));
    scrollArea->setVerticalScrollBar(new Qt_MyQScrollBar(Qt::Vertical));
    scrollArea_2->setHorizontalScrollBar(new Qt_MyQScrollBar(Qt::Horizontal));
    scrollArea_2->setVerticalScrollBar(new Qt_MyQScrollBar(Qt::Vertical));

    updateWidgets();

    // VST
    {    
      vst_widget->buttonBox->hide();
      
      tabWidget->insertTab(tabWidget->count()-2, vst_widget, "Plugins");
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
#endif


      connect(&_color_dialog, SIGNAL(currentColorChanged(const QColor &)), this, SLOT(color_changed(const QColor &)));
      connect(tabWidget, SIGNAL(currentChanged(int)), this, SLOT(current_tab_changed(int)));

      scrollArea->setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOn);
      scrollArea->setWidgetResizable(true);

      QWidget *contents = scrollArea->widget();      
      QVBoxLayout *layout = new QVBoxLayout(contents);
      layout->setSpacing(1);

      contents->setLayout(layout);
      
      for(int i=START_CONFIG_COLOR_NUM;i<END_CONFIG_COLOR_NUM;i++) {
        ColorButton *l = new ColorButton(get_color_display_name((enum ColorNums)i), (enum ColorNums)i, &_color_dialog);
      
        layout->addWidget(l);
        //l->move(0, i*20);
        safeShow(l);
        //contents->resize(contents->width(), 200*20);
      }
      
      //contents->adjustSize();
    }

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
      

#if USE_QT5
      eraseEstimatedVBlankInterval->hide();
      erase_vblank_group_box_layout->removeItem(erase_estimated_vblank_spacer);
#else
      QString vblankbuttontext = QString("Erase Estimated Vertical Blank (")+QString::number(1000.0/GL_get_estimated_vblank())+" Hz)";
      eraseEstimatedVBlankInterval->setText(vblankbuttontext);
#endif

#if defined(FOR_MACOSX)
      // Seems like Apple goes to great lengths to make it impossible to avoid high CPU in OpenGL applications when a window is not visible, unless we accept some stuttering in the graphics.      
      high_cpu_protection->setChecked(false);
      high_cpu_protection->setEnabled(false);
#else      
      high_cpu_protection->setChecked(doHighCpuOpenGlProtection());
#endif

#if 0 // FOR_MACOSX
      // Of course it doesn't work on OSX.
      bool draw_in_separate_process = false;
      draw_in_separate_process_onoff->setEnabled(false);
#else
      bool draw_in_separate_process = SETTINGS_read_bool("opengl_draw_in_separate_process",true);//GL_using_nvidia_card());
#endif
      draw_in_separate_process_onoff->setChecked(draw_in_separate_process);
      
      safeModeOnoff->setChecked(GL_get_safe_mode());
      safeModeOnoff->setEnabled(false);
    }

    
    // Various
    {

      // gc
      {
        gcOnOff->setChecked(true);
        
        bool incremental_gc = SETTINGS_read_bool("incremental_gc",false);
        
        incrementalGcNextTime->setChecked(false);
        
        incrementalGc->setChecked(incremental_gc);
        
        if (g_gc_is_incremental==false)
          incrementalGc->setDisabled(true);
        
        if (incremental_gc)
          incrementalGcNextTime->setDisabled(true);
      }

      // audio meter update
      {
        cpu_friendly_audio_meter_updates->setChecked(useCPUFriendlyAudiometerUpdates());
      }
    }

    // Audio
    {
      numCPUs->setValue(MULTICORE_get_num_threads());
      buzy_looping->setChecked(doAudioBuzyLoop());
      enable_autobypass->setChecked(autobypassEnabled());
      autobypass_delay->setValue(getAutoBypassDelay());
      undo_solo->setChecked(doUndoSolo());
      undo_bypass->setChecked(doUndoBypass());
      recalculate_bus_latency_onoff->setChecked(doAlwaysRunBuses());
    }

    {
      embedded_audio_files->setText(getEmbeddedAudioFilesPath());
      embedded_audio_group->hide(); // not used yet.
    }

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

      colorTracksOnoff->setChecked(GL_get_colored_tracks());

      scrollplay_onoff->setChecked(doScrollPlay());

      multiplyscrollbutton->setChecked(doScrollEditLines());

      autorepeatbutton->setChecked(doAutoRepeat());

      range_paste_cut_button->setChecked(doRangePasteCut());

      range_paste_scroll_down_button->setChecked(doRangePasteScrollDown());

      if (linenumbersVisible())
        showLineNumbers->setChecked(true);
      else
        showBarsAndBeats->setChecked(true);
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
    }

    // Windows
    {
      show_playlist_during_startup->setChecked(showPlaylistDuringStartup());
      show_mixer_strip_during_startup->setChecked(showMixerStripDuringStartup());

      if(showMixerStripOnLeftSide())
        show_mixer_strip_on_the_left->setChecked(true);
      else
        show_mixer_strip_on_the_right->setChecked(true);

      sequencer_window_is_child_of_main_window->setChecked(sequencerWindowIsChildOfMainWindow());
      mixer_window_is_child_of_main_window->setChecked(mixerWindowIsChildOfMainWindow());
      
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
    }
    
    // MIDI
    {
      use0x90->setChecked(MIDI_get_use_0x90_for_note_off());
      
      if (MIDI_get_record_accurately())
        record_sequencer_style->setChecked(true);
      else
        record_tracker_style->setChecked(true);
      
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
      const char **input_port_names = MIDI_OS_get_connected_input_ports(&num_input_ports);
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
#else
      faust_llvm_opt_level_box->hide();
#endif
    }
    
    _is_updating_widgets = false;
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

  void on_high_cpu_protection_toggled(bool val){
    if (_initing==false)
      setHighCpuOpenGlProtection(val);
  }
  
  void on_draw_in_separate_process_onoff_toggled(bool val){
    if (_initing==false)
      SETTINGS_write_bool("opengl_draw_in_separate_process",val);
  }
  
  void on_safeModeOnoff_toggled(bool val){
    if (_initing==false)
      GL_set_safe_mode(val);
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

  void on_colorTracksOnoff_toggled(bool val){
    if (_initing==false)
      GL_set_colored_tracks(val);
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
    if (_initing==false)
      MULTICORE_set_num_threads(val);
    
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

  void on_buzy_looping_toggled(bool val){
    if (_initing==false)
      setAudioBuzyLoop(val);
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

  void on_recalculate_bus_latency_onoff_toggled(bool val){
    if (_initing==false)
      setAlwaysRunBuses(val);
  }
  
  // embedded audio file paths
  void on_embedded_audio_files_editingFinished(){
    setEmbeddedAudioFilesPath(embedded_audio_files->text().toUtf8().constData());
    set_editor_focus();
    
    GL_lock();{
      embedded_audio_files->clearFocus();
    }GL_unlock();
  }

  
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

  // colors
  void color_changed(const QColor &col){
    //printf("HAPP! %s\n",col.name(QColor::HexArgb).toUtf8().constData());
    testColorInRealtime(g_current_colornum, col);
    //printf("  alpha1: %d\n",col.alpha());
    
    for(auto button : all_buttons){
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
  
  void on_color_reset_button_clicked(){
    printf("HHH");
    GFX_ResetColor(g_current_colornum);
    _color_dialog.setCurrentColor(get_qcolor(g_current_colornum));

    for(auto button : all_buttons){
      button->update();
    }

  }

  void on_color_reset_all_button_clicked(){
    printf("AAAxHHH");
    GFX_ResetColors();
    _color_dialog.setCurrentColor(get_qcolor(g_current_colornum));

    for(auto button : all_buttons){
      button->update();
    }

  }

  void on_color_save_button_clicked(){
    GFX_SaveColors(NULL);
  }


  void on_save_color_file_clicked(){
    const wchar_t *filename = GFX_GetSaveFileName(root->song->tracker_windows,
                                                  NULL,
                                                  "Select file",
                                                  NULL,
                                                  NULL,
                                                  NULL,
                                                  true);
    if (filename != NULL)
      GFX_SaveColors(filename);
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

  void on_record_sequencer_style_toggled(bool val){
    if (_initing==false)
      MIDI_set_record_accurately(val);
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

