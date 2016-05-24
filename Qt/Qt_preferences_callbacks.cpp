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


#include <stdio.h>
#include <sndfile.h>
#include <unistd.h>

#include <gc.h>

#include <QMessageBox>
#include <QColorDialog>
#include <QCloseEvent>
#include <QHideEvent>

#include "../common/nsmtracker.h"
#include "../common/hashmap_proc.h"
#include "../common/OS_string_proc.h"
#include "../common/OS_settings_proc.h"
#include "../common/settings_proc.h"
#include "../OpenGL/Widget_proc.h"
#include "../OpenGL/Render_proc.h"
#include "../audio/MultiCore_proc.h"
#include "../midi/midi_i_input_proc.h"
#include "../midi/midi_i_plugin_proc.h"
#include "../midi/midi_menues_proc.h"
#include "../midi/OS_midi_proc.h"

#include "../api/api_proc.h"

#include "../Qt/Qt_MyQSpinBox.h"
#include <FocusSniffers.h>
#include "helpers.h"

#include "Qt_colors_proc.h"

#include "mQt_vst_paths_widget_callbacks.h"

#include "Qt_preferences.h"



extern struct Root *root;
bool g_show_key_codes = false;

extern bool g_gc_is_incremental;


namespace{

struct ColorButton;
static radium::Vector<ColorButton*> all_buttons;

static enum ColorNums g_current_colornum = LOW_EDITOR_BACKGROUND_COLOR_NUM;

struct MyColorDialog : public QColorDialog {

public: 
    
#if FOR_MACOSX
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

    all_buttons.add(this);
    
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
  
  void paintEvent ( QPaintEvent * ev ){
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
#if FOR_MACOSX
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
  
class Preferences : public QDialog, public Ui::Preferences {
  Q_OBJECT

 public:
  bool _initing;
  bool _is_updating_widgets;
  MyColorDialog _color_dialog;

 Preferences(QWidget *parent=NULL)
   : QDialog(parent)
   , _is_updating_widgets(false)
  {
    _initing = true;

    setupUi(this);

    updateWidgets();

    // VST
    {    
      Vst_paths_widget *vst_widget = new Vst_paths_widget;
      vst_widget->buttonBox->hide();
      
      tabWidget->addTab(vst_widget, "VST");
    }

    // Colors
    {
      _color_dialog.setOption(QColorDialog::NoButtons, true);
      
#if FOR_MACOSX
      //_color_dialog.hide();
      _color_dialog.setOption(QColorDialog::DontUseNativeDialog, true);
#if 0
      _color_dialog.setWindowFlags(Qt::Widget);
      _color_dialog.setParent(this);
      colorlayout_right->insertWidget(0, &_color_dialog);
#endif
      
#else
      _color_dialog.setOption(QColorDialog::DontUseNativeDialog, true);
      //_color_dialog.setOption(QColorDialog::ShowAlphaChannel, true);

      colorlayout_right->insertWidget(0, &_color_dialog);
#endif


      connect(&_color_dialog, SIGNAL(currentColorChanged(const QColor &)), this, SLOT(color_changed(const QColor &)));
      connect(tabWidget, SIGNAL(currentChanged(int)), this, SLOT(current_tab_changed(int)));

      scrollArea->setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOn);
      scrollArea->setWidgetResizable(true);

      QWidget *contents = scrollArea->widget();
      QVBoxLayout *layout = new QVBoxLayout(contents);
      layout->setSpacing(1);
      
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
  }

#if FOR_MACOSX
  void hideEvent(QHideEvent *event) {
    _color_dialog.close();
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
  
#endif
  
  void updateWidgets(){
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
      
      
      QString vblankbuttontext = QString("Erase Estimated Vertical Blank (")+QString::number(1000.0/GL_get_estimated_vblank())+" Hz)";
      eraseEstimatedVBlankInterval->setText(vblankbuttontext);

      safeModeOnoff->setChecked(GL_get_safe_mode());
      pauseRenderingOnoff->setChecked(GL_get_pause_rendering_on_off());
      showKeyCodesOnoff->setChecked(false);

      colorTracksOnoff->setChecked(GL_get_colored_tracks());

      gcOnOff->setChecked(true);

      bool incremental_gc = SETTINGS_read_bool("incremental_gc",false);
    
      incrementalGcNextTime->setChecked(false);

      incrementalGc->setChecked(incremental_gc);

      if (g_gc_is_incremental==false)
        incrementalGc->setDisabled(true);

      if (incremental_gc)
        incrementalGcNextTime->setDisabled(true);
    }

    // CPU
    {
      numCPUs->setValue(MULTICORE_get_num_threads());
    }

    // Edit
    {
      scrollplay_onoff->setChecked(doScrollPlay());

      multiplyscrollbutton->setChecked(doScrollEditLines());

      autorepeatbutton->setChecked(doAutoRepeat());

      range_paste_cut_button->setChecked(doRangePasteCut());

      range_paste_scroll_down_button->setChecked(doRangePasteScrollDown());

      if (linenumbersVisible())
        showLineNumbers->setChecked(true);
      else
        showBarsAndBeats->setChecked(true);

      autobackup_onoff->setChecked(doAutoBackups());
      autobackup_interval->setValue(autobackupIntervalInMinutes());
    }

    // Windows
    {
      modal_windows->setChecked(doModalWindows());
#if FOR_WINDOWS
      native_file_requesters->hide();
#else
      native_file_requesters->setChecked(useNativeFileRequesters());
#endif
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

      while(midi_input_layout->count() > 0)
        delete midi_input_layout->itemAt(0)->widget();

      int num_input_ports;
      char **input_port_names = MIDI_OS_get_connected_input_ports(&num_input_ports);
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

    _is_updating_widgets = false;
  }

public slots:

  void on_buttonBox_clicked(QAbstractButton * button){
    if (button->text() == QString("Close")){
      printf("close\n");
      this->hide();
    }// else
    //RError("Unknown button \"%s\"\n",button->text().toUtf8().constData());
  }

  void on_eraseEstimatedVBlankInterval_clicked(){
    printf("erasing\n");
    GL_erase_estimated_vblank();
  }

  void on_vsyncOnoff_toggled(bool val){
    if (_initing==false){
      if (!_is_updating_widgets)
        GL_erase_estimated_vblank(); // makes sense
      GL_set_vsync(val);
    }
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

  void on_gcOnOff_toggled(bool val){
    if (_initing==false){
      if (val) {
        printf("   setting ON\n");
        GC_enable();
      } else {
        printf("   setting OFF\n");
        GC_disable();      
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

  // edit

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

  void on_autobackup_onoff_toggled(bool val){
    if (_initing==false)
      setDoAutoBackups(val);
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

  
  // colors
  void color_changed(const QColor &col){
    //printf("HAPP! %s\n",col.name().toUtf8().constData());
    testColorInRealtime(g_current_colornum, col);

    for(auto button : all_buttons){
      button->update();
    }
  }

  void current_tab_changed(int tabnum){
#if FOR_MACOSX
    printf("   CHangeEvent called %d\n",tabnum);
    if (tabWidget->currentWidget() != colors)
      _color_dialog.close();
    else
      _color_dialog.myshow();
#endif
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
    GFX_SaveColors();
  }


  // windows

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
    MIDISetInputPort();
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
};
}



static void ensure_widget_is_created(void){
}

static Preferences *g_preferences_widget=NULL;

void PREFERENCES_open(void){
  if(g_preferences_widget==NULL){
    g_preferences_widget = new Preferences(NULL);
    //widget->setWindowModality(Qt::ApplicationModal);
  }

  safeShowOrExec(g_preferences_widget);
}

void PREFERENCES_open_MIDI(void){
  PREFERENCES_open();
  g_preferences_widget->tabWidget->setCurrentWidget(g_preferences_widget->MIDI);
}

void PREFERENCES_update(void){
  g_preferences_widget->updateWidgets();
}

void OS_VST_config(struct Tracker_Windows *window){
#if defined(FOR_MACOSX)
  GFX_Message(NULL,"No VST options to edit on OSX");
#else
  //EditorWidget *editor=(EditorWidget *)window->os_visual.widget;
  Vst_paths_widget *vst_paths_widget=new Vst_paths_widget(NULL); // I'm not quite sure i it's safe to make this one static. It seems to work, but shouldn't the dialog be deleted when destroying the window? Not having it static is at least safe, although it might leak some memory.
  GL_lock();{
    vst_paths_widget->show();
  } GL_unlock();
#endif  
  printf("Ohjea\n");
}


#include "mQt_preferences_callbacks.cpp"

