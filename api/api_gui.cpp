/* Copyright 2017 Kjetil S. Matheussen

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

#include "../common/includepython.h"

#include <inttypes.h>


#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wshorten-64-to-32"
#include <QVector> // Shortening warning in the QVector header. Temporarily turned off by the surrounding pragmas.
#include <QQueue>
#pragma clang diagnostic pop


#include <QWidget>
#include <QCloseEvent>
#include <QPushButton>
#include <QCheckBox>
#include <QRadioButton>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QFormLayout>
#include <QGroupBox>
#include <QPlainTextEdit>
#include <QScrollArea>
#include <QUiLoader>
#include <QToolTip>
#include <QHeaderView>
#include <QDesktopWidget>
#include <QDir>
#include <QFileDialog>
#include <QFontDialog>
#include <QTabWidget>
#include <QTextBrowser>
#include <QTextDocumentFragment>
#include <QSplitter>
#include <QHideEvent>
#include <QWindow>
#include <QScreen>
#include <QMimeData>
#include <QDrag>

#include <QDesktopServices>
#include <QtWebKitWidgets/QWebView>
#include <QtWebKitWidgets/QWebFrame>


#include "../common/nsmtracker.h"
#include "../common/patch_proc.h"

#include "../audio/SoundPlugin.h"
#include "../audio/AudioMeterPeaks_proc.h"
#include "../audio/Juce_plugins_proc.h"

#include "../Qt/FocusSniffers.h"
#include "../Qt/ScrollArea.hpp"
#include "../Qt/Qt_MyQCheckBox.h"
#include "../Qt/flowlayout.h"
#include "../Qt/lzqlineedit.h"
#include "../Qt/Timer.hpp"
#include "../Qt/MySplitter.hpp"
#include "../Qt/Qt_sequencer_proc.h"
#include "../Qt/Qt_bottom_bar_widget_proc.h"
#include "../Qt/Qt_MyQScrollBar.hpp"

#include "../common/visual_proc.h"
#include "../common/seqtrack_proc.h"
#include "../embedded_scheme/s7extra_proc.h"
#include "../embedded_scheme/scheme_proc.h"

#include "../common/OS_system_proc.h"

#include "../Qt/Qt_MyQSlider.h"
#include "../Qt/Qt_mix_colors.h"
#include "../Qt/Qt_Fonts_proc.h"

#include "api_common_proc.h"

#include "radium_proc.h"
#include "api_gui_proc.h"


static QByteArray g_filedialog_geometry;
static QByteArray g_fontdialog_geometry;

int g_num_running_resize_events = 0;

// Keep track of code that might be waiting for callbacks. We could get strange error messages when a widget is closed then.
namespace{
  namespace myprivate{
    static int g_num_visiting_event_handlers = 0;
  }
  struct ScopedEventHandlerTracker{
    ScopedEventHandlerTracker(){
      myprivate::g_num_visiting_event_handlers++;
    }
    ~ScopedEventHandlerTracker(){
      --myprivate::g_num_visiting_event_handlers;
    }
  };
}

static bool safe_to_close_widget(void){
  R_ASSERT(myprivate::g_num_visiting_event_handlers >= 0);
  return myprivate::g_num_visiting_event_handlers==0 && g_radium_runs_custom_exec==false;
}

// To make sure we don't sent double click events to a widget that has just been opened. (seems like a minor qt quirk/bug)
static QPointer<QWidget> g_last_pressed_widget = NULL;
static QPointer<QWidget> g_last_released_widget = NULL;

#define CALL1_PARENT_MOUSEPRESS(classname) classname::mousePressEvent(event)
#define CALL1_PARENT_MOUSEMOVE(classname) classname::mouseMoveEvent(event)
#define CALL1_PARENT_MOUSERELEASE(classname) {if(event.is_real_event()) classname::mouseReleaseEvent(event.get_qtevent());}

#define CALL2_PARENT_MOUSEPRESS(classname) classname::fix_mousePressEvent(event)
#define CALL2_PARENT_MOUSEMOVE(classname) classname::fix_mouseMoveEvent(event)
#define CALL2_PARENT_MOUSERELEASE(classname) classname::fix_mouseReleaseEvent(event)

#define CALL_PARENT_MOUSEPRESS(classname) CALL1_PARENT_MOUSEPRESS(classname)
#define CALL_PARENT_MOUSEMOVE(classname) CALL1_PARENT_MOUSEMOVE(classname)
#define CALL_PARENT_MOUSERELEASE(classname) CALL1_PARENT_MOUSERELEASE(classname)


#define MOUSE_OVERRIDERS(classname)                                     \
  void fix_mousePressEvent(QMouseEvent *event) override{                \
    ScopedEventHandlerTracker event_handler_tracker;                    \
    g_last_pressed_widget = this;                                       \
    if (_mouse_callback.v==NULL){                                         \
      CALL_PARENT_MOUSEPRESS(classname);                                \
      return;                                                           \
    }                                                                   \
    RETURN_IF_DATA_IS_INACCESSIBLE_SAFE2();                             \
    if (!Gui::mousePressEvent(event)){                                  \
      CALL_PARENT_MOUSEPRESS(classname);                                \
    }                                                                   \
  }                                                                     \
                                                                        \
  void fix_mouseMoveEvent(QMouseEvent *event) override{                 \
    ScopedEventHandlerTracker event_handler_tracker;                    \
    if (_mouse_callback.v==NULL){                                         \
      CALL_PARENT_MOUSEMOVE(classname);                                 \
      return;                                                           \
    }                                                                   \
    RETURN_IF_DATA_IS_INACCESSIBLE_SAFE2();                             \
    if (!Gui::mouseMoveEvent(event)){                                   \
      CALL_PARENT_MOUSEMOVE(classname);                                 \
    }                                                                   \
  }                                                                     \
                                                                        \
  void fix_mouseReleaseEvent(radium::MouseCycleEvent &event) override { \
    ScopedEventHandlerTracker event_handler_tracker;                    \
    g_last_released_widget = this;                                      \
    if (_mouse_callback.v==NULL){                                         \
      CALL_PARENT_MOUSERELEASE(classname);                              \
      return;                                                           \
    }                                                                   \
    RETURN_IF_DATA_IS_INACCESSIBLE_SAFE2();                             \
    if (!Gui::mouseReleaseEvent(event)){                                \
      CALL_PARENT_MOUSERELEASE(classname);                              \
    }                                                                   \
  }                                                                     \
                                                                        \
  MOUSE_CYCLE_CALLBACKS_FOR_QT;                                         \
                                                                        \
  void leaveEvent(QEvent *event) override{                              \
    ScopedEventHandlerTracker event_handler_tracker;                    \
    if (_mouse_callback.v==NULL){                                         \
      classname::leaveEvent(event); return;}                            \
    RETURN_IF_DATA_IS_INACCESSIBLE_SAFE2();                             \
    Gui::mouseLeaveEvent(event);                                        \
    classname::leaveEvent(event);                                       \
  }                                                                     \
  void enterEvent(QEvent *event) override{                              \
    ScopedEventHandlerTracker event_handler_tracker;                    \
    if (_mouse_callback.v==NULL){                                         \
      classname::enterEvent(event); return;}                            \
    RETURN_IF_DATA_IS_INACCESSIBLE_SAFE2();                             \
    Gui::mouseEnterEvent(event);                                        \
    classname::enterEvent(event);                                       \
  }    


#define DOUBLECLICK_OVERRIDER(classname)                                \
  void mouseDoubleClickEvent(QMouseEvent *event) override{              \
    ScopedEventHandlerTracker event_handler_tracker;                    \
    if (_doubleclick_callback.v==NULL){                                   \
      classname::mouseDoubleClickEvent(event);return;}                  \
    RETURN_IF_DATA_IS_INACCESSIBLE_SAFE2();                             \
    if(this==g_last_pressed_widget && this==g_last_released_widget)     \
      Gui::mouseDoubleClickEvent(event);                                \
  }                                                                     


#define MOUSE_WHEEL_OVERRIDER(classname)                                \
  void wheelEvent(QWheelEvent *event) override{                         \
    ScopedEventHandlerTracker event_handler_tracker;                    \
    if (!Gui::wheelEvent(event))                                        \
      classname::wheelEvent(event);                                     \
  }                                                                     


#define KEY_OVERRIDERS(classname)                                       \
  void keyPressEvent(QKeyEvent *event) override{                        \
    ScopedEventHandlerTracker event_handler_tracker;                    \
    if (!Gui::keyPressEvent(event))                                     \
      classname::keyPressEvent(event);                                  \
  }                                                                     \
                                                                        \
  void keyReleaseEvent(QKeyEvent *event) override{                      \
    ScopedEventHandlerTracker event_handler_tracker;                    \
    if (!Gui::keyReleaseEvent(event))                                   \
      classname::keyReleaseEvent(event);                                \
  }


#define FOCUSIN_OVERRIDER(classname)                    \
  void focusInEvent ( QFocusEvent *event ) override {   \
    ScopedEventHandlerTracker event_handler_tracker;    \
    RETURN_IF_DATA_IS_INACCESSIBLE_SAFE2();             \
    Gui::focusInEvent(event);                           \
    classname::focusInEvent(event);                     \
  }


#define CLOSE_OVERRIDER(classname)                                      \
  void closeEvent(QCloseEvent *ev) override {                           \
    if (Gui::closeEvent(ev))                                            \
      classname::closeEvent(ev);                                        \
  }                                                                     



#define RESIZE_OVERRIDER(classname)                                     \
  void resizeEvent( QResizeEvent *event) override {                     \
    ScopedEventHandlerTracker event_handler_tracker;                    \
    radium::ScopedResizeEventTracker resize_event_tracker;              \
    if (_image!=NULL)                                                   \
      setNewImage(event->size().width(), event->size().height());       \
    if (_resize_callback.v!=NULL)                                         \
      Gui::resizeEvent(event);                                          \
    resizeEvent2(event);                                                \
    classname::resizeEvent(event);                                      \
  }                                                                     

#define PAINT_OVERRIDER(classname)                                      \
  void paintEvent(QPaintEvent *ev) override {                           \
    ScopedEventHandlerTracker event_handler_tracker;                    \
    if (Gui::paintEvent(ev, QRegion())==false)                          \
      classname::paintEvent(ev);                                        \
  }
  
#define SETVISIBLE_OVERRIDER(classname)                                 \
  void setVisible(bool visible) override {                              \
    if (_has_been_opened_before == false)                               \
      _has_been_opened_before = true;                                   \
    Gui::visibility_changed(visible);                                   \
    remember_geometry.setVisible_override<classname>(this, visible);    \
  }

  /*
#define SHOW_OVERRIDER(classname)                                       \
  void showEvent(QShowEvent *event) override {                           \
    if (window()==this){                                                \
      printf("     SHOWEVENT\n");                                       \
      remember_geometry.remember_geometry_setVisible_override_func(this, true); \
    }                                                                   \
  }
  */
#define HIDE_OVERRIDER(classname)                                       \
  void hideEvent(QHideEvent *event_) override {                         \
    ScopedEventHandlerTracker event_handler_tracker;                    \
    remember_geometry.hideEvent_override(this);                         \
    Gui::hideEvent(event_);                                             \
  }

#define CHANGE_OVERRIDER(classname)              \
  void changeEvent(QEvent *event) override {                            \
    ScopedEventHandlerTracker event_handler_tracker;                    \
    Gui::changeEvent(event);                                            \
    classname::changeEvent(event);                                      \
  }


#define OVERRIDERS_WITHOUT_KEY_AND_MOUSE_WHEEL(classname)               \
  MOUSE_OVERRIDERS(classname)                                           \
  SETVISIBLE_OVERRIDER(classname)                                       \
  HIDE_OVERRIDER(classname)                                             \
  DOUBLECLICK_OVERRIDER(classname)                                      \
  CLOSE_OVERRIDER(classname)                                            \
  RESIZE_OVERRIDER(classname)                                           \
  PAINT_OVERRIDER(classname)                                            \
  CHANGE_OVERRIDER(classname)                                           \
  FOCUSIN_OVERRIDER(classname)                                          \

  
#define OVERRIDERS(classname)                                           \
  OVERRIDERS_WITHOUT_KEY_AND_MOUSE_WHEEL(classname)                     \
  MOUSE_WHEEL_OVERRIDER(classname)                                      \
  KEY_OVERRIDERS(classname)
  

//CHANGE_OVERRIDER(classname)                                             
  /*
  SHOW_OVERRIDER(classname)                                            \

  */


#if 0
static inline QColor getQColor(int64_t color){
#if QT_VERSION >= 0x056000
  return QColor(QRgba64::fromRgba64(color));
#else
  QColor col((uint32_t)color);
  col.setAlpha(int(color>>24));
  return col;
#endif
}
#endif

static QColor getQColor(const_char* colorname){
#if !defined(RELEASE)
  if(strlen(colorname) > 9 && colorname[0]=='#')
    abort();
#endif

#if DEBUG_COLORS
  return QColor(generateNewColor(1.0));
#endif
  
  QColor color = get_config_qcolor(colorname);
  if (!color.isValid()){
#if !defined(RELEASE)
    abort();
#endif
    handleError("Color \"%s\" is not valid", colorname);
    color = Qt::blue;
  }
  
  return color;
}

static QPen getPen(const_char* color){
  QPen pen(getQColor(color));
  
  return pen;
}

static void setDefaultSpacing(QLayout *layout){
  layout->setSpacing(0);
  layout->setContentsMargins(0,0,0,0);
}
  

//static bool g_currently_painting = false;


namespace radium_gui{

struct FullScreenParent;
struct Gui;

static QVector<Gui*> g_valid_guis;

static int64_t g_highest_guinum = 0;
static QHash<int64_t, Gui*> g_guis;

static QHash<const QWidget*, Gui*> g_gui_from_widgets; // Q: What if a QWidget is deleted, and later a new QWidget gets the same pointer value? 
                                                       // A: Shouldn't be a problem. Gui->_widget is immediately set to NULL when the widget is deleted,
                                                       //    and we always check if Gui->_widget!=NULL when getting a Gui from g_gui_from_widgets.

static QHash<int64_t, const char*> g_guis_can_not_be_closed; // The string contains the reason that this gui can not be closed.

static QHash<const FullScreenParent*, Gui*> g_gui_from_full_screen_widgets;

static bool g_delayed_resizing_timer_active = false;
static QQueue<Gui*> g_delayed_resized_guis; // ~Gui removes itself from this one, if present. We could have used QPointer<Gui> instead, but that would make it harder to check if gui is already scheduled for later resizing.


  class VerticalAudioMeterPainter;

  static QVector<VerticalAudioMeterPainter*> g_active_vertical_audio_meters;

  static int64_t g_vamp_id = 0;

  class VerticalAudioMeterPainter {
    
    friend struct Gui;
    
    radium::GcHolder<struct Patch> _patch;
    radium::GcHolder<struct Patch> _note_event_patch;
    float *_pos = NULL;
    float *_falloff_pos = NULL;

    int _num_channels = 0;
    bool _is_input = false;
    bool _is_output = false;

    float _last_peak = -100.0f;
    radium::ProtectedS7Extra<func_t*> _peak_callback;

  public:
    int64_t _guinum;

  private:
    
    float _x1, _y1, _x2, _y2;
    float _width, _height;
    QRect _rect;
    
  public:

    int64_t _id = g_vamp_id++;

    void setPos(double x1, double y1, double x2, double y2){
      _x1=x1;
      _y1=y1;
      _x2=x2;
      _y2=y2;
      _width=x2-x1;
      _height=y2-y1;
      _rect = QRectF(x1, y1, _width, _height).toAlignedRect();
    }

  private:
    
    VerticalAudioMeterPainter(struct Patch *patch, struct Patch *note_event_patch, int64_t guinum, double x1, double y1, double x2, double y2)
      : _patch(patch)
      , _note_event_patch(note_event_patch)
      , _guinum(guinum)
    {
      R_ASSERT_RETURN_IF_FALSE(patch->instrument==get_audio_instrument());

      setPos(x1, y1, x2, y2);

      /*
      // autofill black bacground color
      setAutoFillBackground(true);
      QPalette pal = palette();
      pal.setColor(QPalette::Window, QColor("black"));
      setPalette(pal);
      */
                            
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      R_ASSERT_RETURN_IF_FALSE(plugin!=NULL);

      if(plugin->type->num_outputs > 0)
        _is_output = true;
      else if(plugin->type->num_inputs > 0)
        _is_input = true;

      if (_is_output)
        _num_channels = plugin->type->num_outputs;
      else if (_is_input)
        _num_channels = plugin->type->num_inputs;

      if (_num_channels > 0){
        _pos = (float*)calloc(_num_channels, sizeof(float));
        _falloff_pos = (float*)calloc(_num_channels, sizeof(float));
      }

      R_ASSERT_RETURN_IF_FALSE(get_audio_meter_peaks().num_channels == _num_channels);

      g_active_vertical_audio_meters.push_back(this);
    }

    ~VerticalAudioMeterPainter(){
      R_ASSERT(g_active_vertical_audio_meters.removeOne(this));
      
      free(_pos);
      free(_falloff_pos);
    }

  public:
    
    void addPeakCallback(func_t *func, int64_t guinum){
      if (_peak_callback.v != NULL){
        handleError("Audio Meter #%d already have a peak callback", (int)guinum);
        return;
      }

      _peak_callback.set(func);

      callPeakCallback();
    }

    AudioMeterPeaks _fallback_peaks = {};

    AudioMeterPeaks &get_audio_meter_peaks(void){
      if(_patch->instrument==get_MIDI_instrument()){
#if !defined(RELEASE)
        abort(); // want to know if this can happen.
#endif
        return _fallback_peaks;
      }
        
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

      if(plugin==NULL)
        return _fallback_peaks;

      // Testing backtrace.
      //if (ATOMIC_GET(root->editonoff)==false) // If removing this line, there is no s7 backtrace in the crashreport when the program stops during startup. Really strange.
      //  R_ASSERT(false);
            
      if (_is_output)
        return plugin->volume_peaks;

      if (_is_input)
        return plugin->input_volume_peaks;

      R_ASSERT(false);
      return _fallback_peaks;
    }

    void callPeakCallback(void){
      if (_peak_callback.v != NULL)
        S7CALL(void_double,_peak_callback.v, _last_peak);
    }

    void resetPeak(void){
      _last_peak = -100;
      const AudioMeterPeaks &peaks = get_audio_meter_peaks();
      for(int ch = 0 ; ch < _num_channels ; ch++)
        peaks.peaks[ch] = -100;
    }

    void get_x1_x2(int ch, float &x1, float &x2, const int num_channels) const {

      int num_borders = num_channels - 1;
      float border_width = 0;

      float meter_area_width = _width;
      float start_x = _x1;

      float total_meter_space = meter_area_width - num_borders * border_width;

      float meter_width = total_meter_space / num_channels;

      x1 = start_x + border_width + (ch * (border_width+meter_width));

      if (ch==num_channels-1)
        x2 = _x2;
      else
        x2 = x1 + meter_width;

      // Calling 'round' to get rid of think, almost invisible line, that sometimes can be seen between the channels.
      x1 = round(x1);
      x2 = round(x2);
    }

    float get_pos_y1(void) const {
      return _y1; //upper_border - 1;
    }
    
    float get_pos_y2(void) const {
      return _y2; //- down_border + 2;
    }
    
    const float falloff_height = 1.5;

    int get_num_visible_channels(void) const {
      int num_channels = _num_channels;
      
      if (_is_output){
        if(_patch->instrument==get_MIDI_instrument()){
#if !defined(RELEASE)
          abort(); // want to know if this can happen.
#endif
          return 0;
        }
        
        SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
        if (plugin != NULL){
          if (plugin->num_visible_outputs >= 0)
            num_channels = R_MIN(num_channels, plugin->num_visible_outputs);
        } else {
          //R_ASSERT_NON_RELEASE(false); // Happens when loading song. (more accurately, right after, so can't assert g_is_loading_song)
        }
      }

      return num_channels;
    }

    QRectF get_falloff_rect(double x1, double x2, double falloff_pos) const {
      return QRectF(x1, falloff_pos-falloff_height/2.0,
                    x2-x1, falloff_height);      
    }

    float get_indicator_width(void) const {
      return R_MIN(root->song->tracker_windows->systemfontheight / 2, _x2-_x1-2);
    }
      
    QRectF get_indicator_rect(void) const {
      float width = get_indicator_width();
      
      float x1 = ((_x1+_x2) / 2.0) - (width/2.0);
      //float y1 = scale(pitch, 0, 128, _y2-width/2.0, width/2.0); //_y1 + (x1 - _x1);
      float y1 = _y1 + width/2.0;

      return QRectF(x1,y1,width,width);
    }
    
    void myupdate(QWidget *widget, float x1, float y1, float x2, float y2){
      int x1_i = floor(R_MAX(_x1, x1));
      int y1_i = floor(R_MAX(_y1, y1));
      int x2_i = ceil(R_MIN(_x2, x2));
      int y2_i = ceil(R_MIN(_y2, y2));
      widget->update(x1_i,
                     y1_i,
                     x2_i - x1_i,
                     y2_i - y1_i);
    }

    void myupdate(QWidget *widget, const QRectF &rect){
      qreal x1, y1, x2, y2;
      rect.getCoords(&x1, &y1, &x2, &y2);
      myupdate(widget, x1, y1, x2, y2);
    }

    void myupdate(QWidget *widget, const QRect &rect){
      int x1, y1, x2, y2;
      rect.getCoords(&x1, &y1, &x2, &y2);
      myupdate(widget, x1, y1, x2, y2);
    }

  private:

    int _note_intencity = -1;
    
    // NOTE. This function can be called from a custom exec().
    // This means that _patch->plugin might be gone, and the same goes for soundproducer.
    // (_patch is never gone, never deleted, except when loading song)
    void call_regularly(QWidget *widget){
      ScopedEventHandlerTracker event_handler_tracker;

      if(_patch->instrument==get_MIDI_instrument()){
#if !defined(RELEASE)
          abort(); // want to know if this can happen.
#endif
          return;
      }
            
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      if(plugin==NULL){
        return;
      }

      {
        int old_intencity = _note_intencity;
        
        _note_intencity = ATOMIC_GET_RELAXED(_note_event_patch->visual_note_intencity);

        if(_note_intencity>0 || old_intencity != -1)
          myupdate(widget, get_indicator_rect());
      }
      
      const AudioMeterPeaks &peaks = get_audio_meter_peaks();

      R_ASSERT_NON_RELEASE(plugin->num_visible_outputs <= _num_channels);
      
      int num_channels = R_MIN(peaks.num_channels, get_num_visible_channels());

      for(int ch=0 ; ch < num_channels ; ch++){
        float prev_pos = _pos[ch];
        float prev_falloff_pos = _falloff_pos[ch];

        float db;
        float db_falloff;
        float db_peak;

        db = peaks.decaying_dbs[ch];
        db_falloff = peaks.falloff_dbs[ch];
        db_peak = peaks.peaks[ch];
        
        if (db_peak > _last_peak){
          _last_peak = db_peak;
          callPeakCallback();
        }

        float pos = db2linear(db, get_pos_y1(), get_pos_y2());
        _pos[ch] = pos;

        float x1,x2;
        get_x1_x2(ch, x1,x2, num_channels);

        if (pos < prev_pos){
          myupdate(widget,
                   x1-2,   pos-2,
                   x2+4,   prev_pos+4);
        } else if (pos > prev_pos){
          myupdate(widget,
                   x1-2,   prev_pos-2,
                   x2+4,   pos+4);
        }

        //update();

        float falloff_pos = db2linear(db_falloff, get_pos_y1(), get_pos_y2());
        _falloff_pos[ch] = falloff_pos;

        if (falloff_pos != prev_falloff_pos){
          myupdate(widget, get_falloff_rect(x1, x2, prev_falloff_pos).toAlignedRect().adjusted(-1,-1,1,1)); // I don't understand why it's necessary to add .adjusted() here.
          myupdate(widget, get_falloff_rect(x1, x2, falloff_pos).toAlignedRect().adjusted(-1,-1,1,1)); // I don't understand why it's necessary to add .adjusted() here.
        }
      }
    }
    
  public:
    
    void paintMeter(QPainter &p) {
      if(_patch->instrument==get_MIDI_instrument()){
#if !defined(RELEASE)
        abort(); // Not necessarily anything wrong. Just want to know if this can happen.
#endif
        return;
      }

      p.setRenderHints(QPainter::Antialiasing,true);

      QColor qcolor1("black");
      QColor color4db = get_qcolor(PEAKS_4DB_COLOR_NUM);
      QColor color0db = get_qcolor(PEAKS_0DB_COLOR_NUM);
      QColor colorgreen  = get_qcolor(PEAKS_COLOR_NUM);
      QColor colordarkgreen  = get_qcolor(PEAKS_COLOR_NUM).darker(150);

      float posm20db = db2linear(-20, get_pos_y1(), get_pos_y2());
      float pos0db = db2linear(0, get_pos_y1(), get_pos_y2());
      float pos4db = db2linear(4, get_pos_y1(), get_pos_y2());

      p.setPen(Qt::NoPen);

      if (_note_intencity==0) {
        
        p.setBrush(qcolor1);
        p.drawRect(get_indicator_rect());

        _note_intencity = -1;
      }
          

      int num_channels = get_num_visible_channels();
      
      for(int ch=0 ; ch < num_channels ; ch++){
        float x1,x2;
        get_x1_x2(ch, x1,x2, num_channels);

        float pos = _pos[ch];

        // Background
        {
          QRectF rect1(x1,     get_pos_y1(),
                       x2-x1,  pos-get_pos_y1());
          p.setBrush(qcolor1);
          p.drawRect(rect1);
        }

        const bool do_gradient = false;

        float ratio = (_y2-_y1) / (x2-x1);
        int how_much_gradient = R_BOUNDARIES(5, scale(ratio, 1, 15, 0, 15), 15);
        
        // decaying meter
        {
          float gx = (x1+x2) / 2.0;

            // Do the dark green
            {
              float dark_green_pos = R_MAX(pos, posm20db);
              QRectF rect(x1, dark_green_pos,
                          x2-x1,  get_pos_y2()-dark_green_pos);

              if(do_gradient){
                QLinearGradient gradient(gx, posm20db, gx, get_pos_y2());
                gradient.setColorAt(1, colordarkgreen);
                gradient.setColorAt(0, colorgreen);
                p.setBrush(gradient);
                p.drawRect(rect);
              } else {
                //p.setBrush(colordarkgreen);
                myFillRectHorizontalGradient(p, rect, colordarkgreen, true, how_much_gradient);
              }
            }

            // Do the green
            if (pos <= posm20db){
              float green_pos = R_MAX(pos, pos0db);
              QRectF rect(x1, green_pos,
                          x2-x1,  posm20db-green_pos);
              
              if(do_gradient){
                QLinearGradient gradient(gx, pos0db, gx, posm20db);
                gradient.setColorAt(1, colorgreen);
                gradient.setColorAt(0, color0db);
                p.setBrush(gradient);
                p.drawRect(rect);
              } else {
                p.setBrush(colorgreen);
                myFillRectHorizontalGradient(p, rect, colorgreen, true, how_much_gradient);
              }
            }

            // Do the yellow
            if (pos <= pos0db){
              float yellow_pos = R_MAX(pos, pos4db);
              QRectF rect(x1, yellow_pos,
                           x2-x1,  pos0db-yellow_pos);
              
              if(do_gradient){
                QLinearGradient gradient(gx, pos4db, gx, pos0db);
                gradient.setColorAt(1, color0db);
                gradient.setColorAt(0, color4db);
                p.setBrush(gradient);
                p.drawRect(rect);
              } else {
                //p.setBrush(color0db);
                myFillRectHorizontalGradient(p, rect, color0db, true, how_much_gradient);
              }
            }

            // Do the red
            if (pos <= pos4db){
              float red_pos = pos;
              QRectF rect(x1, red_pos,
                           x2-x1,  pos4db-red_pos);
              if(do_gradient){
                QLinearGradient gradient(gx, get_pos_y1(), gx, pos4db);
                gradient.setColorAt(1, color4db);
                gradient.setColorAt(0, color4db.darker());
                p.setBrush(gradient);
                p.drawRect(rect);
              } else {
                //p.setBrush(color4db);
                myFillRectHorizontalGradient(p, rect, color4db, true, how_much_gradient);
              }
            }
        }

        // falloff meter
        {
          //SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
          //if(plugin!=NULL)
          //  printf("Fallof pos: %f, y2: %f, falloffdb: %f\n", _falloff_pos[ch], get_pos_y2(), plugin->volume_peaks.falloff_dbs[ch]);
          
          if (_falloff_pos[ch] < get_pos_y2()){
            float falloff_pos = _falloff_pos[ch];
            /*
            QRectF falloff_rect(x1, falloff_pos-falloff_height/2.0,
                                x2-x1, falloff_height);
            */
            QRectF falloff_rect = get_falloff_rect(x1, x2, falloff_pos);
            
            if (falloff_pos > posm20db)
              p.setBrush(colordarkgreen);
            else if (falloff_pos > pos0db)
              p.setBrush(colorgreen);
            else if (falloff_pos > pos4db)
              p.setBrush(color0db);
            else
              p.setBrush(color4db);

            p.drawRect(falloff_rect);
          }
        }

        /*        
        p.setBrush(qcolor1);
        p.drawRoundedRect(QRectF(get_x1(ch), 1,
                                 get_x2(ch), height()-1),
                          5,5);

        p.setBrush(qcolor2);
        p.drawRoundedRect(rect2,5,5);
        */
      }


      //int intencity = ATOMIC_GET_RELAXED(_patch->visual_note_intencity);
      if(_note_intencity > 0){

        QColor c = get_qcolor(NOTE_EVENT_INDICATOR_COLOR_NUM);
        c.setAlphaF(::scale(_note_intencity, 0, MAX_NOTE_INTENCITY, 0.0, 1.0));
        p.setBrush(QBrush(c,Qt::SolidPattern));
        
        QColor border_color = get_qcolor(NOTE_EVENT_INDICATOR_BORDER_COLOR_NUM);
        p.setPen(border_color);

        float width = get_indicator_width();
        p.drawRoundedRect(get_indicator_rect(), width, width);

      }

      p.setBrush(Qt::NoBrush);
      
      //border
#if 0
      QPen pen(QColor("#202020"));
      pen.setWidth(2.0);
      p.setPen(pen);
      p.drawRoundedRect(0,0,width()-1,height()-1, 5, 5);
#endif
    }
  };


  
  
  struct Callback : QObject {
    Q_OBJECT;

    QWidget *_widget;
      
    QString _last_string_value = "__________________________________"; // Workaround for https://bugreports.qt.io/browse/QTBUG-40
    int64_t _last_int_value = INT64_MIN; // an int normally can't (or at least won't) hold this value.
    double _last_double_value = -DBL_MAX;
    
  public:

    radium::ProtectedS7Extra<func_t*> _func;
    
    Callback(func_t *func, QWidget *widget)
      : _widget(widget)
      , _func(func)
    {
    }

  public slots:
    void clicked(bool checked){
      ScopedEventHandlerTracker event_handler_tracker;
      S7CALL(void_void,_func.v);
    }
    
    void toggled(bool checked){
      ScopedEventHandlerTracker event_handler_tracker;
      S7CALL(void_bool, _func.v, checked);
    }

    void editingFinished(){
      ScopedEventHandlerTracker event_handler_tracker;

      QLineEdit *line_edit = dynamic_cast<QLineEdit*>(_widget);
      R_ASSERT_RETURN_IF_FALSE(line_edit!=NULL);
      
      QString value = line_edit->text();
            
      set_editor_focus();

      GL_lock();{
        line_edit->clearFocus();
      }GL_unlock();

      if (value != _last_string_value){
        _last_string_value = value;
        S7CALL(void_charpointer, _func.v, value.toUtf8().constData());
      }
    }

    void intTextEditingFinished(){
      ScopedEventHandlerTracker event_handler_tracker;

      QSpinBox *spinbox = dynamic_cast<QSpinBox*>(_widget);
      R_ASSERT_RETURN_IF_FALSE(spinbox!=NULL);
      
      int value = spinbox->value();
            
      set_editor_focus();

      GL_lock();{
        spinbox->clearFocus();
      }GL_unlock();

      if (value != _last_int_value){
        QPointer<Callback> mythis(this);
        
        S7CALL(void_int, _func.v, value);
        if (mythis.isNull()==false)
          _last_int_value = value;
      }
    }

    void doubleTextEditingFinished(){
      ScopedEventHandlerTracker event_handler_tracker;

      QDoubleSpinBox *spinbox = dynamic_cast<QDoubleSpinBox*>(_widget);
      R_ASSERT_RETURN_IF_FALSE(spinbox!=NULL);
      
      double value = spinbox->value();
            
      set_editor_focus();

      GL_lock();{
        spinbox->clearFocus();
      }GL_unlock();

      if (value != _last_int_value){
        QPointer<Callback> mythis(this);
        S7CALL(void_double, _func.v, value);
        if (mythis.isNull()==false)
          _last_double_value = value;
      }
    }


    void textChanged(QString text){
      ScopedEventHandlerTracker event_handler_tracker;
      S7CALL(void_charpointer, _func.v, text.toUtf8().constData());
    }
    
    void currentFontChanged(const QFont &font){
      ScopedEventHandlerTracker event_handler_tracker;
      S7CALL(void_dyn, _func.v, DYN_create_string(font.toString()));//.toUtf8().constData()));
    }

    void intValueChanged(int val){
      ScopedEventHandlerTracker event_handler_tracker;
      S7CALL(void_int,_func.v, val);
    }

    void doubleValueChanged(double val){
      ScopedEventHandlerTracker event_handler_tracker;
      S7CALL(void_double,_func.v, val);
    }

    void textChanged(){
      ScopedEventHandlerTracker event_handler_tracker;
      QTextEdit *text_edit = dynamic_cast<QTextEdit*>(_widget);
      S7CALL(void_charpointer,_func.v, text_edit->toPlainText().toUtf8().constData());
    }
    
    void plainTextChanged(){
      ScopedEventHandlerTracker event_handler_tracker;
      QPlainTextEdit *text_edit = dynamic_cast<QPlainTextEdit*>(_widget);
      S7CALL(void_charpointer,_func.v, text_edit->toPlainText().toUtf8().constData());
    }
    
    void itemSelectionChanged(){
      ScopedEventHandlerTracker event_handler_tracker;
      //QTableWidget *table = dynamic_cast<QTableWidget*>(_widget);
      S7CALL(void_void,_func.v);
    }
    
    void cellDoubleClicked(int row, int column){
      ScopedEventHandlerTracker event_handler_tracker;
      S7CALL(void_int_int,_func.v, column, row);
    }
    
    void fileSelected(const QString &file){
      ScopedEventHandlerTracker event_handler_tracker;
      S7CALL(void_charpointer,_func.v, path_to_w_path(STRING_create(file)));
    }

    void currentChanged(int index){
      ScopedEventHandlerTracker event_handler_tracker;
      S7CALL(void_int,_func.v, index);
    }
  };


  /*
    FullScreenParent should/must:
    1. When asked to close, it must ask child if it wants to close. Only if child wants to close, we can close.
    2. When child is hidden, we must also hide instead.
    3. When child is becoming visible, we must also become visible.
    4. When child is deleted, we must delete too.
    5. When the original parent of the child is deleted, we must delete too.
    6. When the child changes parent, we must delete, or preferably, monitor when to hide and so forth.
    */
  struct FullScreenParent : public QWidget, radium::Timer {

    QPointer<QWidget> _child;
    
    bool _had_original_parent;
    QPointer<QObject> _original_parent;
    Qt::WindowFlags _original_flags;
    QRect _original_geometry;
    
    FullScreenParent(QWidget *child, Gui *gui)
      : radium::Timer(15, true)
      , _child(child)
    {
      R_ASSERT(child->isWindow());

      _original_parent = child->parent();
      
      _had_original_parent = _original_parent != NULL;
      
      _original_flags = child->windowFlags();
      _original_geometry = child->geometry();

      g_gui_from_full_screen_widgets[this] = gui;

      QVBoxLayout *mainLayout = new QVBoxLayout(this);
      mainLayout->setSpacing(0);
      mainLayout->setContentsMargins(0,0,0,0);
      
      setLayout(mainLayout);
      
      mainLayout->addWidget(child);

      showFullScreen();
      child->show();
      show();

      //printf("   CREATED FULL SCREEN %p\n", this->parent());

#if defined(FOR_WINDOWS)
      OS_WINDOWS_set_key_window((void*)winId()); // To avoid losing keyboard focus
#endif
    }
    
    ~FullScreenParent(){
      g_gui_from_full_screen_widgets.remove(this);
    }

    void setNewParent(QWidget *new_parent){
      _original_parent = new_parent;
    }
      
    void resetChildToOriginalState(void){
      R_ASSERT_RETURN_IF_FALSE(_child != NULL);

      layout()->removeWidget(_child);

      QWidget *parentWidget = dynamic_cast<QWidget*>(_original_parent.data()); // Might be null, not only if _original_parent is not a qwidget, but also if the original parent was deleted.
      _child->setParent(parentWidget, _original_flags); // Must set a different parent to the child. If not the child will be deleted in the destructor of this class.
      
      _child->setGeometry(_original_geometry);
    }

    bool _someone_else_has_become_parent = false;
    
    void calledFromTimer(void) override {

      if (_child==NULL){
        // 4.
        //printf("   CHILD==NULL\n");
        delete this;
        return;
      }

      if (_had_original_parent && _original_parent==NULL){
        // 5.
        //printf("   ORIGINAL PARENT==NULL\n");
        delete this;
        return;
      }

      if (_someone_else_has_become_parent==true) {
        
        if (_child->parent() != this && _child->isWindow()) {
            // 6.
            //printf("   I AM STEALING CHILD BACK\n");
            _someone_else_has_become_parent = false;
            _original_parent = _child->parent();
            _had_original_parent = _original_parent != NULL;
            layout()->addWidget(_child);
            show();
            return;
        }

      } else {
          
        if (_child->parent() != this) {
          // 6.
          //printf("   SOMEONE ELSE HAS BECOME PARENT\n");
          hide();
          _original_parent = _child->parent();
          _someone_else_has_become_parent = true;
          _had_original_parent = true;
          return;
        }

        if (_child->isVisibleTo(this)==false){
          if (isVisible()==true){
            // 2.
            //printf("   HIDE\n");
            hide();
          }
        } else {
          if (isVisible()==false){
            // 3.
            //printf("   SHOW\n");
            show();
          }
        }
        
      }

    }
    
    void closeEvent(QCloseEvent *ev) override {
      //printf("   CLOSING\n");
      ev->ignore(); // We only react to what the child might do.

      // 1.
      if (_child != NULL)
        _child->close();
    }
  };

  struct Gui {
    
    QVector<Callback*> _callbacks;
 
  public:
    int64_t _gui_num;
    int _valid_guis_pos;

    const QWidget *_widget_as_key; // Need a way to get hold of the original widget's address in the destructor, even after the original widget has been deleted. (Note that _widget_as_key might have been deleted. Only _widget can be considered to have a valid widget value.)
    const QPointer<QWidget> _widget; // Stored in a QPointer since we need to know if the widget has been deleted.
    bool _created_from_existing_widget; // Is false if _widget was created by using one of the gui_* functions (except gui_child()). Only used for validation.

    // Has value when running full screen.
    //bool _is_full_screen = false;
    QPointer<FullScreenParent> _full_screen_parent = NULL;
    Qt::WindowFlags _original_flags;
    QPointer<QWidget> _original_parent;
    QRect _original_geometry;

    bool _take_keyboard_focus;
    bool _has_keyboard_focus = false;

    radium::ProtectedS7FuncVector _deleted_callbacks;
    
    RememberGeometry remember_geometry;

    QSet<int64_t> _child_plugin_gui_patch_ids; // Plugins that have been opened with this GUI as parent. We identify the plugin by using Patch::id since Patch::id requires less bookkeeping (no need to do anything when patch or plugin is deleted).
    
    bool is_full_screen(void) const {
      //return _is_full_screen;
      //return _widget->isFullScreen();
      return _full_screen_parent!=NULL;
    }
    
    int64_t get_gui_num(void) const {
      return _gui_num;
    }

    bool _has_been_closed = false;
    bool _delayed_deletion = false;

    radium::Modality _modality = radium::MAY_BE_MODAL; // We need to remember whether modality should be on or not, since modality is a parameter for set_window_parent.
    bool _have_set_size = false;
    bool _has_been_opened_before = false;

    QString _class_name;
    bool _is_pure_qwidget;

    QColor _background_color;

    //QFont _paintfont;
    QHash<QString,QFont> _cached_fonts;
    QString _fontstring;

#if !defined(RELEASE)
    // for debugging. Use "call puts(gui->scheme_backtrace)" in gdb to print.
    //const char *backtrace = strdup(JUCE_get_backtrace()); // Extremely slow.
    //const char *scheme_backtrace = strdup(SCHEME_get_history());
    const char *scheme_backtrace = "";
#endif
                   
    Gui(QWidget *widget, bool created_from_existing_widget = false)
      : _widget_as_key(widget)
      , _widget(widget)
      , _created_from_existing_widget(created_from_existing_widget)
      , _take_keyboard_focus(created_from_existing_widget==false)
#if defined(RELEASE)
      , _deleted_callbacks(false)
#else
      , _deleted_callbacks(true)
#endif
      , _class_name(widget->metaObject()->className())
    {
      _is_pure_qwidget = _class_name=="QWidget";

      R_ASSERT(_background_color.isValid()==false);
    
      update_modality_attribute();
      
      g_gui_from_widgets[_widget] = this;
      
      _gui_num = g_highest_guinum++;
      g_guis[_gui_num] = this;

      _valid_guis_pos = g_valid_guis.size();
      g_valid_guis.push_back(this);
      
      if (!_created_from_existing_widget){
#if 0
        // screws up mixer. Don't know why. (Press "M" two times)
        auto policy = _widget->sizePolicy();
        policy.setRetainSizeWhenHidden(true);
        _widget->setSizePolicy(policy);
#endif
        _widget->setAttribute(Qt::WA_DeleteOnClose);
      }
      //_widget->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
      //_widget->setAttribute(Qt::WA_OpaquePaintEvent);
    }

    virtual ~Gui(){
      ScopedEventHandlerTracker event_handler_tracker;

      R_ASSERT(g_valid_guis[_valid_guis_pos] == this);
      R_ASSERT(g_guis.contains(_gui_num));
      R_ASSERT(g_guis.value(_gui_num) != NULL);

      R_ASSERT_RETURN_IF_FALSE(_widget!=g_main_window);

      if(g_static_toplevel_widgets.contains(_widget)){
        R_ASSERT_NON_RELEASE(false==g_static_toplevel_widgets.contains(_widget)); // Use _widget instead of widget since the static toplevel widgets might be deleted before all gui widgets. The check is good enough anyway.
      }

      // delete meters
      while(_vamps.size() > 0)
        deleteVamp(_vamps.value(0));
      
      // Close plugin GUI children
      {
        QList<int64_t> patch_ids = _child_plugin_gui_patch_ids.toList(); // Work on a copy since elements are removed while iterating.
        
        for(int64_t patch_id : patch_ids){
        
          struct Patch *patch = PATCH_get_from_id(patch_id);
          if (patch->instrument==get_audio_instrument()){
            if (patch!=NULL){
              SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
              if (plugin!=NULL){
                PLUGIN_close_gui(plugin);
              }
            }
          } else {
            R_ASSERT(false);
          }
        }
      }
      
      //printf("Deleting Gui %p (%d) (classname: %s)\n",this,(int)get_gui_num(), _class_name.toUtf8().constData());

      if (g_delayed_resized_guis.contains(this))
        g_delayed_resized_guis.removeOne(this);

      apply_deleted_callbacks();

      for(Callback *callback : _callbacks)
        delete callback;

      delete _image_painter;
      delete _image;

      g_guis_can_not_be_closed.remove(_gui_num); // Can call QHash::remove() even when the table doesn't contain the key.
        
      g_gui_from_widgets.remove(_widget_as_key); // Can't use _widget since it might have been set to NULL.
        
      g_guis.remove(_gui_num);

      {
        R_ASSERT(g_valid_guis[_valid_guis_pos] == this);
        
        if (_valid_guis_pos < g_valid_guis.size()-1){
          g_valid_guis.last()->I_am_the_last_pos_of_valid_guis_move_me_somewhere_else(_valid_guis_pos);

          R_ASSERT(g_valid_guis[_valid_guis_pos] != this);
        }
        
        g_valid_guis.removeLast();
      }
    }

    void apply_deleted_callbacks(void){
      _deleted_callbacks.safe_for_all(false, [](func_t *func){
          S7CALL(void_bool,func, g_radium_runs_custom_exec);
          return true;
        });

      _deleted_callbacks.clear();
    }
    
    void I_am_the_last_pos_of_valid_guis_move_me_somewhere_else(int new_pos) {
      R_ASSERT_RETURN_IF_FALSE(g_valid_guis.size()-1 == _valid_guis_pos);

      _valid_guis_pos = new_pos;
      g_valid_guis[new_pos] = this;
    }

    bool size_is_valid(void){
      if (_have_set_size==false && _has_been_opened_before==false)
        return false;
      else
        return true;
    }

    template<typename T>
    T *mycast(const_char* funcname, int argnum = 1) const {
      QWidget *widget = _widget.data();

      T *ret = qobject_cast<T*>(widget);
      
      if (ret==NULL)
        handleError("%s: argument #%d (gui #%d) is wrong type. Expected %s, got %s.", funcname, argnum, (int)get_gui_num(), T::staticMetaObject.className(), widget==NULL ? "(deleted)" : widget->metaObject()->className());
      
      return ret;
    }

    void update_modality_attribute(void) {
      QWidget *widget = _widget.data();
      if(widget==NULL)
        return;

      if (widget->isModal())
        _modality = radium::IS_MODAL;
      else
        _modality = radium::NOT_MODAL;
    }

    void changeEvent(QEvent *event){
      if(_widget->isWindow() && _widget->window()!=g_main_window->window()){
        if (_take_keyboard_focus==true){
          if (event->type()==QEvent::ActivationChange){
            //printf("  ActitionChange. Is Active: %d. Classname: %s. Widget: %p. Window: %p\n", _widget->isActiveWindow(), _class_name.toUtf8().constData(),_widget.data(),_widget->window());
            if(_widget->isActiveWindow()){
              obtain_keyboard_focus();
              _has_keyboard_focus = true;
            } else {
              release_keyboard_focus();
              _has_keyboard_focus = false;
            }
          } else if (event->type()==QEvent::Close || event->type()==QEvent::Hide){
            //printf("  Close/Hide. Is Active: %d. Classname: %s. Widget: %p. Window: %p\n", _widget->isActiveWindow(), _class_name.toUtf8().constData(),_widget.data(),_widget->window());
            release_keyboard_focus();
            _has_keyboard_focus = false;
          }
        }
      }
    }

    void hideEvent(QHideEvent *event){
      //printf("  Hide event 1.\n");
      if(_widget->isWindow() && _widget->window()!=g_main_window->window()){
        if (_take_keyboard_focus==true){
          //printf("  Hide event 2. Is Active: %d. Classname: %s. Widget: %p. Window: %p\n", _widget->isActiveWindow(), _class_name.toUtf8().constData(),_widget.data(),_widget->window());
          release_keyboard_focus();
          _has_keyboard_focus = false;
        }
      }
    }

    bool _was_visible = false;
    
    radium::ProtectedS7Extra<func_t*> _visibility_change_callback;

    void visibility_changed(bool visible) {
      if (visible == _was_visible)
        return;

      if(_visibility_change_callback.v != NULL)
        S7CALL(void_bool, _visibility_change_callback.v, visible);
      
      _was_visible = visible;
    }
      
    void addVisiblityChangeCallback(func_t* func){
      if (_visibility_change_callback.v!=NULL){
        handleError("Gui %d already has a visibility change callback.", (int)_gui_num);
        return;
      }

      _visibility_change_callback.set(func);
    }
    
    /************ AUDIO METERS *******************/
    
    QVector<VerticalAudioMeterPainter*> _vamps;

    VerticalAudioMeterPainter *createVamp(struct Patch *patch, struct Patch *note_event_patch, double x1, double y1, double x2, double y2){
      auto *vamp = new VerticalAudioMeterPainter(patch, note_event_patch, get_gui_num(), x1, y1, x2, y2);

#if !defined(RELEASE)
      for(const auto *vamp2 : _vamps){
        if (vamp->_rect.intersects(vamp2->_rect))
          handleError("Error: Vertical audio meters overlaps with antoher vertical audio meter 1: (%d, %d -> %d, %d) 2: (%d, %d -> %d, %d)", vamp->_rect.x(), vamp->_rect.y(), vamp->_rect.right(), vamp->_rect.top(), vamp2->_rect.x(), vamp2->_rect.y(), vamp2->_rect.right(), vamp2->_rect.top());
      }
#endif

      _vamps.push_back(vamp);
      callVampRegularly(vamp);
      return vamp;
    }
    
    void deleteVamp(VerticalAudioMeterPainter *vamp){
      R_ASSERT_RETURN_IF_FALSE(_vamps.contains(vamp)==true);
      R_ASSERT(_vamps.removeAll(vamp)==1);
      delete vamp;
    }

    void callVampRegularly(VerticalAudioMeterPainter *vamp){
      if(_widget.data()==NULL){
        R_ASSERT_NON_RELEASE(false);
        return;
      }

      vamp->call_regularly(_widget);
    }

    // Returns true if only vamps needs to be painted.
    bool getVampsToPaint(const QPaintEvent *event, bool *vamps_to_paint, const QRegion &already_painted_areas, int &num_vamps_to_paint) const {
      num_vamps_to_paint = 0;

      if(_vamps.size()==0)
        return false;
      
      bool all_rects_are_contained_in_vamps = true;

      //int i2=0;
      for(const QRect &rect : event->region()){

#if 0
        printf("  Rect %d: %d, %d -> %d, %d. Covered: %d\n", i2++, rect.x(), rect.y(), rect.x() + rect.width(), rect.y() + rect.height(), already_painted_areas.contains(rect));

        if(already_painted_areas.rects().size()>0){
          QRect rect2 = already_painted_areas.rects().at(0);
          printf("   t_X: %d,%d -> %d, %d. Size: %d\n", rect2.x(), rect2.y(), rect2.right(), rect2.bottom(), already_painted_areas.rects().size());
        }
#endif

        if (workingQRegionContains(already_painted_areas, rect))
          continue;

        bool rect_is_contained_in_vamp = false;

        int i=0;

        for(const auto *vamp : _vamps){

          //printf("            VampRect %d: %d, %d -> %d, %d. Intersects: %d\n", i2++, vamp->_rect.x(), vamp->_rect.y(), vamp->_rect.right(), vamp->_rect.bottom(), rect.intersects(vamp->_rect));
                  
          if (rect.intersects(vamp->_rect)){
            
            if(vamps_to_paint[i]==false){
              num_vamps_to_paint++;
              vamps_to_paint[i] = true;
            }

            if (vamp->_rect.contains(rect)==true){
              rect_is_contained_in_vamp = true;
              break;
            }
          }
          
          i++;
        }

        if (rect_is_contained_in_vamp==false)
          all_rects_are_contained_in_vamps = false;
      }
      
      return all_rects_are_contained_in_vamps;
    }

    void paintVamps(QPainter &p, const bool *vamps_to_paint){
      QWidget *widget = _widget.data();
      if(widget==NULL){
        R_ASSERT_NON_RELEASE(false);
        return;
      }

      bool do_sequencer_clipping = widget==SEQUENCER_WIDGET_get_widget();
            
      if (do_sequencer_clipping){
        // Hack to avoid vamps to be painted on top of the "+A a" button, and the dragger.
        float y1 = SEQTIMELINE_get_y2() - SEQUENCER_get_y1();
        float y2 = SEQNAV_get_y2() - SEQUENCER_get_y1() - 1.5*(4 + root->song->tracker_windows->systemfontheight); // If changing 1.5, also change 1.5 in "ty2" in <sequencer-left-part> in seqtrack-headers.scm. And if changing 4+systemfontheight, change get-fontheight in mixer-strips.scm and (get-fontheight) in mixer-strips.scm.
        p.setClipRect(QRectF(0, y1, widget->width(), y2-y1));
        p.setClipping(true);
      }
        
      int i=0;        
      for(auto *vamp : _vamps){

        //printf("            Painting vamp %d: %d\n", i, vamps_to_paint[i]);
        if (vamps_to_paint[i]==true)
          vamp->paintMeter(p);
        
        i++;
      }

      if (do_sequencer_clipping)
        p.setClipping(false);
    }
    
    /************ MOUSE *******************/
    
    radium::ProtectedS7Extra<func_t*> _mouse_callback;
    int _currentButton = 0;
    double _mouse_event_failed = -1;
    QPoint _last_mouse_pos = QPoint(0,0);
    
    int getMouseButtonEventID(QMouseEvent *qmouseevent) const {
      if(qmouseevent->button()==Qt::LeftButton)
        return TR_LEFTMOUSEDOWN;
      else if(qmouseevent->button()==Qt::RightButton)
        return TR_RIGHTMOUSEDOWN;
      else if(qmouseevent->button()==Qt::MiddleButton)
        return TR_MIDDLEMOUSEDOWN;
      else
        return 0;
    }

    bool mousePressEvent(QMouseEvent *event){
      R_ASSERT_RETURN_IF_FALSE2(_mouse_callback.v!=NULL, false);

      if ((_mouse_event_failed - TIME_get_ms()) > 0){
        printf("mouse_event failed last time. Won't try again for a couple of seconds\n");
        return false;
      }
      
      event->accept();

      _currentButton = getMouseButtonEventID(event);
      _last_mouse_pos = event->pos();

      int64_t guinum = get_gui_num(); // gui might be closed when calling _mouse_callback
            
      int ret = S7CALL(bool_int_int_float_float,_mouse_callback.v, _currentButton, API_MOUSE_PRESSING, _last_mouse_pos.x(), _last_mouse_pos.y());
      if (g_scheme_failed==true && gui_isOpen(guinum))
        _mouse_event_failed = TIME_get_ms() + 5000;
      
      return ret;
      
      //printf("  Press. x: %d, y: %d. This: %p\n", point.x(), point.y(), this);
    }

    bool mouseReleaseEvent(radium::MouseCycleEvent &event) {
      R_ASSERT_RETURN_IF_FALSE2(_mouse_callback.v!=NULL, false);

      if ((_mouse_event_failed - TIME_get_ms()) > 0){
        printf("mouse_event failed last time. Won't try again\n");
        return false;
      }
      
      event.accept();

      int64_t guinum = get_gui_num(); // gui might be closed when calling _mouse_callback
      
      _last_mouse_pos = event.pos();
            
      bool ret = S7CALL(bool_int_int_float_float, _mouse_callback.v, _currentButton, API_MOUSE_RELEASING, _last_mouse_pos.x(), _last_mouse_pos.y());
      if (g_scheme_failed==true && gui_isOpen(guinum))
        _mouse_event_failed = TIME_get_ms() + 5000;
      
      _currentButton = 0;
      //printf("  Release. x: %d, y: %d. This: %p\n", point.x(), point.y(), this);
      return ret;
    }

    bool mouseMoveEvent(QMouseEvent *event){
      R_ASSERT_RETURN_IF_FALSE2(_mouse_callback.v!=NULL, false);

      if ((_mouse_event_failed - TIME_get_ms()) > 0){
        printf("mouse_event failed last time. Won't try again\n");
        return false;
      }
      
      event->accept();

      _last_mouse_pos = event->pos();
      
      int64_t guinum = get_gui_num(); // gui might be closed when calling _mouse_callback
      
      bool ret = S7CALL(bool_int_int_float_float,_mouse_callback.v, _currentButton, API_MOUSE_MOVING, _last_mouse_pos.x(), _last_mouse_pos.y());
      if (g_scheme_failed==true && gui_isOpen(guinum))
        _mouse_event_failed = TIME_get_ms() + 5000;

      //hmm.
      //throwExceptionIfError();
        
      return ret;
      
      //printf("    move. x: %d, y: %d. This: %p\n", point.x(), point.y(), this);
    }

    bool mouseLeaveEvent(QEvent *event){
      R_ASSERT_RETURN_IF_FALSE2(_mouse_callback.v!=NULL, false);

      if ((_mouse_event_failed - TIME_get_ms()) > 0){
        printf("mouse_event failed last time. Won't try again\n");
        return false;
      }
      
      event->accept();

      int64_t guinum = get_gui_num(); // gui might be closed when calling _mouse_callback
      
      bool ret = S7CALL(bool_int_int_float_float,_mouse_callback.v, _currentButton, API_MOUSE_LEAVING, _last_mouse_pos.x(), _last_mouse_pos.y());
      if (g_scheme_failed==true && gui_isOpen(guinum))
        _mouse_event_failed = TIME_get_ms() + 5000;
      
      return ret;
      
      //printf("    move. x: %d, y: %d. This: %p\n", point.x(), point.y(), this);
    }

    bool mouseEnterEvent(QEvent *event){
      R_ASSERT_RETURN_IF_FALSE2(_mouse_callback.v!=NULL, false);

      if ((_mouse_event_failed - TIME_get_ms()) > 0){
        printf("mouse_event failed last time. Won't try again\n");
        return false;
      }
      
      event->accept();

      int64_t guinum = get_gui_num(); // gui might be closed when calling _mouse_callback
      
      bool ret = S7CALL(bool_int_int_float_float,_mouse_callback.v, _currentButton, API_MOUSE_ENTERING, _last_mouse_pos.x(), _last_mouse_pos.y());
      if (g_scheme_failed==true && gui_isOpen(guinum))
        _mouse_event_failed = TIME_get_ms() + 5000;
      
      return ret;
      
      //printf("    move. x: %d, y: %d. This: %p\n", point.x(), point.y(), this);
    }

    void addMouseCallback(func_t* func){      
      if (_mouse_callback.v!=NULL){
        handleError("Gui %d already has a mouse callback.", (int)_gui_num);
        return;
      }

      _mouse_callback.set(func);
      _widget->setMouseTracking(true);
    }


    /************ MOUSE Wheel *******************/

    radium::ProtectedS7Extra<func_t*> _mouse_wheel_callback;

    bool is_child_of_scroll_area(QObject *object){
      if (object==NULL)
        return false;
      
      QWidget *widget = qobject_cast<QWidget*>(object);
      
      if (widget != NULL){

        if (qobject_cast<QScrollArea*>(widget) != NULL)
          return true;
        
        if (widget->isWindow())
          return false;
      }
      
      if (is_child_of_scroll_area(object->parent()))
        return true;
      else
        return false;
    }
    
    bool wheelEvent(QWheelEvent *event) {
      if(can_internal_data_be_accessed_questionmark_safer()==false)
        return is_child_of_scroll_area(_widget.data());

      if (_mouse_wheel_callback.v==NULL)
        return is_child_of_scroll_area(_widget.data());

      event->accept();

      const QPoint &point = event->pos();

      return S7CALL(bool_bool_float_float,_mouse_wheel_callback.v, event->delta() > 0, point.x(), point.y());
    }
    
    void addMouseWheelCallback(func_t* func){      
      if (_mouse_wheel_callback.v!=NULL){
        handleError("Gui %d already has a mouse wheel callback.", (int)_gui_num);
        return;
      }

      _mouse_wheel_callback.set(func);
    }

    void removeMouseWheelCallback(void){
      if (_mouse_wheel_callback.v==NULL){
        handleError("Gui %d doesn't have a mouse wheel callback.", (int)_gui_num);
        return;
      }
      
      _mouse_wheel_callback.set(NULL);
    }

    /************ KEY *******************/

    radium::ProtectedS7Extra<func_t*> _key_callback;

    bool keyEvent(QKeyEvent *event, int keytype){
      if(can_internal_data_be_accessed_questionmark_safer()==false)
        return false;

      if (_key_callback.v==NULL)
        return false;

      QString s = false ? ""
        : event->key()==Qt::Key_Enter ? "\n"
        : event->key()==Qt::Key_Return ? "\n"
        : event->key()==Qt::Key_Escape ? "ESC"
        : event->key()==Qt::Key_Home ? "HOME"
        : event->key()==Qt::Key_End ? "END"
        : event->text();

      //printf("  GOt key: %d. Auto: %d\n", event->key(), event->isAutoRepeat());
      
      event->accept();
      
      return S7CALL(bool_int_charpointer,_key_callback.v, keytype, talloc_strdup(s.toUtf8().constData()));
    }
    
    bool keyPressEvent(QKeyEvent *event){
      if (event->key()==Qt::Key_F11){
        event->accept();
        GFX_toggleCurrWindowFullScreen();
        /*
          fprintf(stderr,"\n\n\n api_gui.cpp / KeyPressEvent: F11 Pressed.   IS MAIN WINDOW: %d. %p / %p\n\n\n", parentwindow==g_main_window, _widget, _widget->window());
          //int64_t window_gui = gui_getParentWindow(get_gui_num());
          int64_t window_gui = API_get_gui_from_widget(parentwindow);
          gui_setFullScreen(window_gui, !gui_isFullScreen(window_gui));
          */
        return true;
      }

      /*
      if (_created_from_existing_widget)
        return false;

      QWidget *parentwindow = _widget->window();

      if (parentwindow!=g_main_window && !_created_from_existing_widget){  // Happens when a gui is created here and placed into the main window. For instance the Edit tab.
      }
      */
      
      return keyEvent(event, 0);
    }
    
    bool keyReleaseEvent(QKeyEvent *event){
      return keyEvent(event, 1);
    }

    void addKeyCallback(func_t* func){      
      if (_key_callback.v!=NULL){
        handleError("Gui %d already has a key callback.", (int)_gui_num);
        return;
      }

      _key_callback.set(func);
    }

    
    /************ FOCUS IN *******************/

    radium::ProtectedS7Extra<func_t*> _focus_in_callback;
    
    void focusInEvent(QFocusEvent *event){
      if (_focus_in_callback.v==NULL)
        return;

      S7CALL(void_void, _focus_in_callback.v);
    }


    void addFocusInCallback(func_t* func){      
      if (_focus_in_callback.v!=NULL){
        handleError("Gui %d already has a focus in callback.", (int)_gui_num);
        return;
      }

      _focus_in_callback.set(func);
    }

    
    /************ DOUBLECLICK *******************/

    radium::ProtectedS7Extra<func_t*> _doubleclick_callback;

    void mouseDoubleClickEvent(QMouseEvent *event){
      R_ASSERT_RETURN_IF_FALSE(_doubleclick_callback.v!=NULL);
      event->accept();

      const QPoint &point = event->pos();

      S7CALL(void_int_float_float,_doubleclick_callback.v, getMouseButtonEventID(event), point.x(), point.y());
    }

    // Returns true if it uses signal system.
    // Should add everyting here.
    virtual bool addDoubleClickCallback(func_t* func){
      QTableWidget *qtableWidget = dynamic_cast<QTableWidget*>(_widget.data());

      if (qtableWidget!=NULL){
        
        Callback *callback = new Callback(func, _widget);
        qtableWidget->connect(qtableWidget, SIGNAL(cellDoubleClicked(int,int)), callback, SLOT(cellDoubleClicked(int,int)));
        _callbacks.push_back(callback);

        return true;
        
      } else {
        
        if (_doubleclick_callback.v!=NULL){
          handleError("Gui %d already has a doubleclick callback.", (int)_gui_num);
        } else {
          _doubleclick_callback.set(func);
        }
        
        return false;
      }
    }

    
    /************ CLOSE *******************/

    radium::ProtectedS7Extra<func_t*> _close_callback;

    // Some scheme code is running (which might be run from an event handler), so we let the garbage collector delete us instead to avoid memory corruption in those event handlers.
    void setDelayedDeletion(QCloseEvent *event){

      _delayed_deletion = true;
      
      event->ignore();
      
      if(_widget != NULL)
        _widget->hide();
#if !defined(RELEASE)
      else
        abort();
#endif
    }

    int _num_close_calls = 0;

    bool run_close_callback(void) const {
      ScopedEventHandlerTracker event_handler_tracker; // To avoid another closeEvent to be called from this closeEvent. Qt likes crashing.
      return S7CALL(bool_bool,_close_callback.v, g_radium_runs_custom_exec);
    }
    
    bool closeEvent(QCloseEvent *event){

      _num_close_calls++;

      
      if (_close_callback.v != NULL){
        
        R_ASSERT(_num_close_calls==1);

        int64_t guinum = get_gui_num(); // gui might be closed when calling _mouse_callback (not supposed to happen though)
      
        bool result = run_close_callback();

        if (!gui_isOpen(guinum)){
          R_ASSERT_NON_RELEASE(false);
          return false; // return false so that we don't run the closeEvent method in the super class.
        }

        if (result==false){

          // Closing was cancelled, so we clean the "close-state".
          _num_close_calls = 0;          
          event->ignore();

          return false;
          
        } else {

          _close_callback.set(NULL);
          
        }
      }

      R_ASSERT(_close_callback.v==NULL);

      apply_deleted_callbacks(); // Do this as early as possible

      _has_been_closed = true;


      /*
       + We test for "_num_close_call==1" here to avoid mysterious crashes when running Qt 5.12.0.
         The crashes happened in later events and at apparently random places within Qt. It took
         two days work to track it down, and I only managed to find a workaround for the problem.
         The bug might have been fixed in later versions of Qt, but there's probably no good reason to remove
         the test since a delayed deletion shouldn't degrade performance.
       + We test for g_scheme_nested_level > 0 just in case. Seems like a sane precaution, but
         it can probably be removed.
       + We test for safe_to_close_widget() since Qt crashes if closeEvent is called from another event.
         (Qt might not crash anymore, haven't checked this in a long time, but there's probably no reason to remove the test.)
      */
      if(_num_close_calls==1 || g_scheme_nested_level > 0 || safe_to_close_widget()==false){
        
        setDelayedDeletion(event);
        
        return false; // delay closing        
      }

      
      return true; // close it.
    }

    void addCloseCallback(func_t* func){      
      if (_close_callback.v!=NULL){
        handleError("Gui %d already has a close callback.", (int)_gui_num);
        return;
      }

      _close_callback.set(func);
    }

    
    /************ RESIZE *******************/
    
    radium::ProtectedS7Extra<func_t*> _resize_callback = NULL;
    double _resize_callback_failed = -1;

    void do_the_resize(int width, int height){
      if(gui_isOpen(_gui_num)){ // && _widget->isVisible()){ //  && _widget->width()>0 && _widget->height()>0)
        int64_t guinum = get_gui_num(); // gui might be closed when calling _mouse_callback
        S7CALL(void_int_int,_resize_callback.v, width, height);
        if (g_scheme_failed==true && gui_isOpen(guinum))
          _resize_callback_failed = TIME_get_ms() + 5000;
      }
    }
    
    static void do_all_the_resizing(void){
      g_delayed_resizing_timer_active = false;
            

      //R_ASSERT_NON_RELEASE(g_delayed_resized_guis.isEmpty()==false); // This happens. I guess calls to "do_the_resize" triggers new resizes.
    
      if(g_radium_runs_custom_exec && g_and_its_not_safe_to_paint){
        g_delayed_resizing_timer_active = true;
        QTimer::singleShot(100, do_all_the_resizing); // try again later
        return;
      }

      //printf("do_the_resize called\n");
      while(!g_delayed_resized_guis.isEmpty()){
        Gui *gui = g_delayed_resized_guis.first();
        g_delayed_resized_guis.pop_front();
        //printf("   Aiai. Resizing %p\n", gui);
        gui->do_the_resize(gui->_widget->width(), gui->_widget->height());
      }
    }
    
    virtual void resizeEvent2(QResizeEvent *event){
    }

    void resizeEvent(QResizeEvent *event){
      R_ASSERT_RETURN_IF_FALSE(_resize_callback.v!=NULL);

      if ((_resize_callback_failed - TIME_get_ms()) > 0){
        printf("resize_event failed last time. Won't try again to avoid a debug output bonanza (%s).\n", _class_name.toUtf8().constData());
        return;
      }

      if(g_delayed_resized_guis.contains(this))
        return; // already scheduled.

      if(g_num_running_resize_events > 1 || myprivate::g_num_visiting_event_handlers>1 || (g_radium_runs_custom_exec && g_and_its_not_safe_to_paint)){
#if !defined(RELEASE)
        printf("  Aiai: %d %d %d %d\n", g_num_running_resize_events, myprivate::g_num_visiting_event_handlers, g_radium_runs_custom_exec, g_and_its_not_safe_to_paint);
#endif
        g_delayed_resized_guis.push_back(this);
        if (g_delayed_resizing_timer_active==false){
          g_delayed_resizing_timer_active = true;
          QTimer::singleShot(100, do_all_the_resizing);
        }
        return;
      }

      do_the_resize(event->size().width(), event->size().height());
    }

    void addResizeCallback(func_t* func){
      if (_resize_callback.v!=NULL){
        handleError("Gui %d already has a resize callback.", (int)_gui_num);
        return;
      }

      _resize_callback.set(func);
    }


    /************ DRAWING *******************/

    radium::ProtectedS7Extra<func_t*> _paint_callback;

    QImage *_image = NULL;
    QPainter *_image_painter = NULL;
    QPainter *_current_painter = NULL;
    double _paint_callback_failed = -1;

    const QRegion *_current_region = NULL;
    
    bool maybePaintBackgroundColor(QPaintEvent *event, QPainter &p) const {
      if (_background_color.isValid()==false)
        return false;
      
      p.fillRect(_widget->rect(), _background_color);

      event->accept();

      return true;
    }

    bool maybePaintBackgroundColor(QPaintEvent *event) const {
      if (_background_color.isValid()==false)
        return false;

      QPainter p(_widget);
      return maybePaintBackgroundColor(event, p);
    }

    bool paintEvent3(QPainter *p, const QRegion *region, std::function<void(void)> func){
      bool ret = true;
      
      _current_painter = p;
      _current_region = region;
      
      int64_t guinum = get_gui_num(); // gui might be closed when calling _mouse_callback

      func();
      
      if (g_scheme_failed==true && gui_isOpen(guinum))
        _paint_callback_failed = TIME_get_ms() + 5000;
      
      ret = gui_isOpen(guinum);  // Check if we have been deleted in the meantime.
      
      if (ret){
        _current_painter = NULL;
        _current_region = NULL;
      }

      return ret;
    }
    
    // Returns true if 'this' is still alive. (sometimes the paint callback triggers deletion.)
    bool paintEvent2(QPaintEvent *event, const bool *vamps_to_paint) {
      TRACK_PAINT();

      bool ret = true;
      
      R_ASSERT_RETURN_IF_FALSE2(_paint_callback.v!=NULL || _background_color.isValid(), ret);

      if(!can_internal_data_be_accessed_questionmark()){
        maybePaintBackgroundColor(event);
        return ret;
      }
          
      if ((_paint_callback_failed - TIME_get_ms()) > 0){
        printf("paint_event failed last time. Won't try again to avoid a debug output bonanza (%s).\n", _class_name.toUtf8().constData());
        maybePaintBackgroundColor(event);
        return ret;
      }

      QWidget *widget = _widget.data();

      if (widget==NULL){
        R_ASSERT_NON_RELEASE(false);
        return false;
      }

      QPainter p(widget);
      
      if(maybePaintBackgroundColor(event, p)==false)
        event->accept();
      
      if (_paint_callback.v != NULL) {

        p.setRenderHints(QPainter::Antialiasing,true);
        
        ret = paintEvent3(&p, &event->region(),
                          [this,widget](){
                            S7CALL(void_int_int,_paint_callback.v, widget->width(), widget->height());
                          });
        
      }

      if (ret)
        paintVamps(p, vamps_to_paint);

      return ret;
    }

    bool paintEvent(QPaintEvent *event, const QRegion &already_painted_areas) {
      bool ret = true;

      if(_image!=NULL) {
        
        TRACK_PAINT();                  
        QPainter p(_widget);
        p.drawImage(event->rect().topLeft(), *_image, event->rect());
        
      } else {

        int num_vamps = _vamps.size();
        bool vamps_to_paint[R_MAX(1, num_vamps)];
        memset(vamps_to_paint, 0, R_MAX((size_t)1, sizeof(bool)*num_vamps));
        
        int num_vamps_to_paint;
        bool only_vamps_needs_to_be_painted = getVampsToPaint(event, vamps_to_paint, already_painted_areas, num_vamps_to_paint);

        if (only_vamps_needs_to_be_painted){

          //printf("Only vamp painting %d\n", vamps_to_paint[0]);

          if (num_vamps_to_paint > 0){
            TRACK_PAINT();
            QPainter p(_widget);
            paintVamps(p, vamps_to_paint);
          }

        } else {

          if (_paint_callback.v!=NULL || _background_color.isValid())
            paintEvent2(event, vamps_to_paint);
          else
            ret = false;
          
        }

      }
      
      return ret;
    }


    void addPaintCallback(func_t* func){
      if (_paint_callback.v!=NULL){
        handleError("Gui %d already has a paint callback.", (int)_gui_num);
        return;
      }

      _paint_callback.set(func);
    }


    void setNewImage(int width, int height){
      width = R_MAX(1,width);
      height = R_MAX(1, height);

      if (_image!=NULL && _image->width() >= width && _image->height() >= height)
        return;

      if (_image!=NULL){
        width = R_MAX(_image->width(), width);
        height = R_MAX(_image->height(), height);
      }

      //width*=2;
      //height*=2;

      //printf("   %d: num_calls to setNewImage: %d. %d >= %d, %d >= %d\n", (int)_gui_num, num_calls++, _image==NULL ? -1 : _image->width(), width, _image==NULL ? -1 : _image->height(), height);

      auto *new_image = new QImage(width, height, QImage::Format_ARGB32);
      auto *new_image_painter = new QPainter(new_image);

      //new_image_painter->fillRect(QRect(0,0,width,height), get_qcolor(LOW_BACKGROUND_COLOR_NUM));
      
      if (_image!=NULL)
        new_image_painter->drawImage(QPoint(0,0), *_image);
      
      new_image_painter->setRenderHints(QPainter::Antialiasing,true);

      delete _image_painter;
      delete _image;
      _image_painter = new_image_painter;
      _image = new_image;

      if(_widget==NULL){
        R_ASSERT_NON_RELEASE(false);
      } else {
        set_widget_takes_care_of_painting_everything(_widget);
      }
    }
    
    
    QPainter *get_painter(void) {
      QPainter *painter = _current_painter;

      if (painter==NULL){

        if (_image==NULL){
          QWidget *widget = _widget.data();
          if (widget==NULL){
            R_ASSERT_NON_RELEASE(false);
            setNewImage(500,500);
          } else {
            setNewImage(widget->width(), widget->height());
          }
        }

        painter=_image_painter;
      }

      return painter;
    }

    void setPen(const_char* color){
      get_painter()->setPen(getQColor(color));
    }

    void myupdate(float x1, float y1, float x2, float y2, float extra=0.0f){
      if (_current_painter == NULL){
        float min_x = R_MIN(x1, x2) - extra;
        float max_x = R_MAX(x1, x2) + extra;
        float min_y = R_MIN(y1, y2) - extra;
        float max_y = R_MAX(y1, y2) + extra;
        _widget->update(min_x-1, min_y-1, max_x-min_x+2, max_y-min_y+2);
      }
    }

    void drawLine(const_char* color, float x1, float y1, float x2, float y2, float width) {
      QPainter *painter = get_painter();
      
      QLineF line(x1, y1, x2, y2);

      QPen pen = getPen(color);
      pen.setWidthF(width);
      painter->setPen(pen);
      
      //printf("Color: %x, %s\n", (unsigned int)color, pen.color().name(QColor::HexArgb).toUtf8().constData());

      painter->drawLine(line);

      if (_current_painter==NULL)
        myupdate(x1, y1, x2, y2, width);
    }

    void drawBox(const_char* color, float x1, float y1, float x2, float y2, float width, float round_x, float round_y) {
      QPainter *painter = get_painter();

      QRectF rect(x1, y1, x2-x1, y2-y1);

      QPen pen = getPen(color);
      pen.setWidthF(width);
      painter->setPen(pen);

      if (round_x>0 && round_y>0)
        painter->drawRoundedRect(rect, round_x, round_y);
      else
        painter->drawRect(rect);

      float halfwidth = 2*width/3;
      if (_current_painter==NULL)
        myupdate(x1-halfwidth, y1-halfwidth, x2+halfwidth, y2+halfwidth);
    }

    void filledBox(const_char* color, float x1, float y1, float x2, float y2, float round_x, float round_y, float do_gradient) {
      QPainter *painter = get_painter();

      QRectF rect(x1, y1, x2-x1, y2-y1);

      QColor qcolor = getQColor(color);

      //QPen pen = _image_rect.pen();

      painter->setPen(Qt::NoPen);

      if(do_gradient){        
        //QLinearGradient gradient((x1+x2)/2.0, y1, (x1+x2)/2.0, y2);
        QLinearGradient gradient(x1, y1, x1, y2);
        gradient.setColorAt(0, qcolor.lighter(125));
        gradient.setColorAt(1, qcolor.darker(125));
        painter->setBrush(gradient);
      } else {
        painter->setBrush(qcolor);
      }

      if (round_x>0 && round_y>0)
        painter->drawRoundedRect(rect, round_x, round_y);
      else
        painter->drawRect(rect);

      painter->setBrush(Qt::NoBrush);
      //_image_painter->setPen(pen);

      if (_current_painter==NULL)
        myupdate(x1, y1, x2, y2);
    }

    void drawPolygon(const_char* color, const dynvec_t *points, bool do_fill, float width = 1.0){
      QPainter *painter = get_painter();

      if (points->num_elements<2)
        return;

      QPointF qpoints[points->num_elements];

      bool is_first = true;
      float min_x=0,max_x=0,min_y=0,max_y=0;
      for(int i=0;i<points->num_elements;i+=2){

        const dyn_t &x_point = points->elements[i];
        const dyn_t &y_point = points->elements[i+1];
        
        if (DYN_is_number(x_point)==false){
          handleError("gui_%sPolygon: Expected a number for points[%d], found %s", do_fill ? "filled" : "draw", i, DYN_type_name(x_point.type));
          return;
        }

        if (DYN_is_number(y_point)==false){  
          handleError("gui_%sPolygon: Expected a number for points[%d], %s", do_fill ? "filled" : "draw", i+1, DYN_type_name(y_point.type));
          return;
        }

        double x = DYN_get_double_from_number(x_point);
        double y = DYN_get_double_from_number(y_point);

        qpoints[i/2].setX(x);
        qpoints[i/2].setY(y);
        
        if (is_first==true){
          min_x = x;
          max_x = x;
          min_y = y;
          max_y = y;
          is_first = false;
        }else{
          min_x = R_MIN(x, min_x);
          max_x = R_MAX(x, max_x);
          min_y = R_MIN(y, min_y);
          max_y = R_MAX(y, max_y);
        }

      }

      bool do_gradient = true;

      QColor qcolor = getQColor(color);

      QPen pen;

      if (do_fill){
        painter->setPen(Qt::NoPen);
        if(do_gradient){
          QLinearGradient gradient((min_x+max_x)/2.0, min_y, (min_x+max_x)/2.0, max_y);
          gradient.setColorAt(0, qcolor.lighter(125));
          gradient.setColorAt(1, qcolor.darker(125));
          painter->setBrush(gradient);
        } else {
          painter->setBrush(qcolor);
        }
      } else {
        pen = QPen(qcolor);
        pen.setWidthF(width);
        painter->setPen(pen);
      }

      painter->drawPolygon(qpoints, points->num_elements/2);

      if(do_fill)
        painter->setBrush(Qt::NoBrush);

      if (_current_painter==NULL){
        float halfwidth = 2*width/3;
        myupdate(min_x-halfwidth, min_y-halfwidth, max_x+halfwidth, max_y+halfwidth);
      }
    }

    void filledEllipse(const_char* color, float x1, float y1, float x2, float y2) {
      QPainter *painter = get_painter();

      QRectF rect(x1, y1, x2-x1, y2-y1);

      QColor qcolor = getQColor(color);

      //QPen pen = _image_rect.pen();

      painter->setPen(Qt::NoPen);
      painter->setBrush(qcolor);
      painter->drawEllipse(rect);
      painter->setBrush(Qt::NoBrush);
      //_image_painter->setPen(pen);

      if (_current_painter==NULL)
        myupdate(x1, y1, x2, y2);
    }

    void drawEllipse(const_char* color, float x1, float y1, float x2, float y2, float width) {
      QPainter *painter = get_painter();

      QRectF rect(x1, y1, x2-x1, y2-y1);

      QPen pen = getPen(color);

      pen.setWidthF(width);
      painter->setPen(pen);
      painter->drawEllipse(rect);

      if (_current_painter==NULL)
        myupdate(x1, y1, x2, y2);
    }

    void setOpacity(double opacity){
      QPainter *painter = get_painter();
      painter->setOpacity(opacity);
    }
    
    bool drawText(const_char* color, const_char *chartext, float x1, float y1, float x2, float y2, bool wrap_lines, bool align_top, bool align_left, int rotate, bool cut_text_to_fit, bool scale_font_size) {
      QPainter *painter = get_painter();

      QString text = QString(chartext); //.replace(" ", "\n");
      
      QRectF rect(x1, y1, x2-x1, y2-y1);

      setPen(color);
      
      int flags = 0; //wrap_lines ? Qt::TextWrapAnywhere : 0;

      if(align_top)
        flags |= Qt::AlignTop;
      else
        flags |= Qt::AlignVCenter;

      if(align_left)
        flags |= Qt::AlignLeft;
      else
        flags |= Qt::AlignHCenter;

      bool ret = myDrawText(painter, rect, text, flags, wrap_lines, rotate, scale_font_size, cut_text_to_fit);
      
      if (_current_painter==NULL)
        myupdate(x1, y1, x2, y2);

      return ret;
    }


    /************ VIRTUAL METHODS *******************/
    
    virtual QLayout *getLayout(void) const {
      return _widget->layout();
    }

    // Try to put as much as possible in here, since GUIs created from ui files does not use the subclasses
    virtual void setGuiText(const_char *text){
      {
        QAbstractButton *button = dynamic_cast<QAbstractButton*>(_widget.data());
        if (button!=NULL){
          button->setText(text);
          return;
        }
      }

      {
        QLabel *label = dynamic_cast<QLabel*>(_widget.data());
        if (label!=NULL){
          label->setText(text);
          return;
        }
      }

      handleError("Gui #%d does not have a setGuiText method", (int)_gui_num);
      return;
    }

    // Try to put as much as possible in here, since GUIs created from ui files does not use the subclasses
    virtual void appendGuiValue(dyn_t val){
      {
        QTextEdit *text_edit = dynamic_cast<QTextEdit*>(_widget.data());
        if (text_edit!=NULL){ 
          if(val.type==STRING_TYPE){
            
            QString s = STRING_get_qstring(val.string);
            text_edit->moveCursor(QTextCursor::End);
            
            if (text_edit->isReadOnly()){
              text_edit->insertHtml(s);
            } else
              text_edit->insertPlainText(s);
            
          }else {
            
            handleError("Text->setValue received %s, expected STRING_TYPE", DYN_type_name(val.type));
            
          }
          
          if (text_edit->isReadOnly())
            text_edit->moveCursor(QTextCursor::End);
          return;
        }
      }

      handleError("Gui #%d does not have an appendValue method", (int)_gui_num);
    }
    
    // Try to put as much as possible in here, since GUIs created from ui files does not use the subclasses
    virtual void setGuiValue(dyn_t val){

      {
        QTableWidget *qtableWidget = dynamic_cast<QTableWidget*>(_widget.data());
        if (qtableWidget!=NULL){
          if(val.type==INT_TYPE)
            qtableWidget->setCurrentCell((int)val.int_number, 1);//qtableWidget->currentColumn());
          else
            handleError("Table->setValue received %s, expected INT_TYPE", DYN_type_name(val.type));
          return;
        }
      }
      

      {
        QAbstractButton *button = dynamic_cast<QAbstractButton*>(_widget.data());
        if (button!=NULL){
          if(val.type==BOOL_TYPE)
            button->setChecked(val.bool_number);
          else
            handleError("Button->setValue received %s, expected BOOL_TYPE", DYN_type_name(val.type));
          return;
        }
      }

      {
        QAbstractSlider *slider = dynamic_cast<QAbstractSlider*>(_widget.data());
        if (slider!=NULL){
          if(val.type==INT_TYPE)
            slider->setValue(val.bool_number);
          else
            handleError("Slider->setValue received %s, expected INT_TYPE", DYN_type_name(val.type));
          return;
        }
      }

      {
        QLabel *label = dynamic_cast<QLabel*>(_widget.data());
        if (label!=NULL){
          if(val.type==STRING_TYPE)
            label->setText(STRING_get_qstring(val.string));
          else
            handleError("Text->setValue received %s, expected STRING_TYPE", DYN_type_name(val.type));
          return;
        }
      }

      {
        RatioSnifferQLineEdit *ratioedit = dynamic_cast<RatioSnifferQLineEdit*>(_widget.data());
        if (ratioedit != NULL){
          if (DYN_is_liberal_ratio(val))
            ratioedit->setText(STATIC_RATIO_as_qstring(DYN_get_static_ratio(val)));
          else
            handleError("Ratio->setValue received %s, expected a number or a string", DYN_type_name(val.type));
          return;
        }
      }
      
      {
        QLineEdit *line_edit = dynamic_cast<QLineEdit*>(_widget.data());
        if (line_edit!=NULL){
          if(val.type==STRING_TYPE)
            line_edit->setText(STRING_get_qstring(val.string));
          else
            handleError("Line->setValue received %s, expected STRING_TYPE", DYN_type_name(val.type));
          return;
        }
      }

      {
        QTextEdit *text_edit = dynamic_cast<QTextEdit*>(_widget.data());
        if (text_edit!=NULL){ 
          if(val.type==STRING_TYPE){
            QString s = STRING_get_qstring(val.string);
            if (text_edit->isReadOnly()){
              text_edit->setText(s);
            }
            else
              text_edit->setPlainText(s);
          }else
            handleError("Text->setValue received %s, expected STRING_TYPE", DYN_type_name(val.type));
          if (text_edit->isReadOnly())
            text_edit->moveCursor(QTextCursor::End);
          return;
        }
      }

      {
        QSpinBox *spinbox = dynamic_cast<QSpinBox*>(_widget.data());
        if (spinbox!=NULL){
          if (val.type==INT_TYPE)
            spinbox->setValue((int)val.int_number);
          else
            handleError("IntText->setValue received %s, expected INT_TYPE", DYN_type_name(val.type));
          return;
        }
      }

      {
        QDoubleSpinBox *doublespinbox = dynamic_cast<QDoubleSpinBox*>(_widget.data());
        if (doublespinbox!=NULL){
          if (val.type==FLOAT_TYPE)
            doublespinbox->setValue(val.float_number);
          else
            handleError("FloatText->setValue received %s, expected FLOAT_TYPE", DYN_type_name(val.type));
          return;
        }
      }
                  
      handleError("Gui #%d does not have a setValue method", (int)_gui_num);
    }

    // Should put as much as possible in here since ui widgets does not use the subclasses. (Actually, the ui widgets can probably use the subclasses by extending the "createWidget" function, but qwidgets created elsewhere can not). The code is cleaner this way too than to add virtual methods into all subclasses. (Virtual methods in subclasses are somewhat faster though, but performance doesn't matter here.))
    virtual dyn_t getGuiValue(void){

      QTableWidget *qtableWidget = dynamic_cast<QTableWidget*>(_widget.data());
      if (qtableWidget!=NULL){
        dynvec_t ret = {};
        for(const auto *item : qtableWidget->selectedItems())
          DYNVEC_push_back(&ret, DYN_create_string(item->text().toUtf8().constData()));
        return DYN_create_array(ret);
      }
      
      QAbstractButton *button = dynamic_cast<QAbstractButton*>(_widget.data());
      if (button!=NULL){
        if(button->isCheckable())
          return DYN_create_bool(button->isChecked());
        else
          return DYN_create_bool(button->isDown());
      }
      
      QAbstractSlider *slider = dynamic_cast<QAbstractSlider*>(_widget.data());
      if (slider!=NULL)
        return DYN_create_int(slider->value());

      QLabel *label = dynamic_cast<QLabel*>(_widget.data());
      if (label!=NULL)
        return DYN_create_string(label->text());

      /*
        // gui_ratio returns a string now since denominators are not preserved in rational types.
      RatioSnifferQLineEdit *ratio_edit = dynamic_cast<RatioSnifferQLineEdit*>(_widget.data());
      if (ratio_edit != NULL)
        return DYN_create_ratio(ratio_edit->get_ratio());
      */
      
      QLineEdit *line_edit = dynamic_cast<QLineEdit*>(_widget.data());
      if (line_edit!=NULL)
        return DYN_create_string(line_edit->text());

      QTextEdit *text_edit = dynamic_cast<QTextEdit*>(_widget.data());
      if (text_edit!=NULL)
        return DYN_create_string(text_edit->toPlainText());

      QSpinBox *spinbox = dynamic_cast<QSpinBox*>(_widget.data());
      if (spinbox!=NULL)
        return DYN_create_int(spinbox->value());

      QDoubleSpinBox *doublespinbox = dynamic_cast<QDoubleSpinBox*>(_widget.data());
      if (doublespinbox!=NULL)
        return DYN_create_float(doublespinbox->value());
      
                  
      handleError("Gui #%d does not have a getValue method", (int)_gui_num);
      return DYN_create_bool(false);
    }

    virtual void addGuiRealtimeCallback(func_t* func){
      Callback *callback = new Callback(func, _widget);

      {
        QLineEdit *line_edit = dynamic_cast<QLineEdit*>(_widget.data());
        if (line_edit!=NULL){
          line_edit->connect(line_edit, SIGNAL(textChanged(QString)), callback, SLOT(textChanged(QString)));
          goto gotit;
        }
      }
      
      handleError("Gui #%d does not have an addRealtimeCallback method", (int)_gui_num);
      delete callback;
      return;
      
    gotit:
      _callbacks.push_back(callback);
      return;      
    }
    
    virtual void addGuiCallback(func_t* func){
      ScopedEventHandlerTracker event_handler_tracker;

      Callback *callback = new Callback(func, _widget);

      int64_t guinum = get_gui_num(); // gui might be closed when calling _mouse_callback

      {
        QTabWidget *tabs = dynamic_cast<QTabWidget*>(_widget.data());
        if (tabs != NULL){
          tabs->connect(tabs, SIGNAL(currentChanged(int)), callback, SLOT(currentChanged(int)));
          goto gotit;
        }
      }
      
      {
        QFileDialog *file_dialog = dynamic_cast<QFileDialog*>(_widget.data());
        if (file_dialog!=NULL){
          file_dialog->connect(file_dialog, SIGNAL(fileSelected(const QString &)), callback, SLOT(fileSelected(const QString &)));
          goto gotit;
        }
      }

      {
        QFontDialog *font_dialog = dynamic_cast<QFontDialog*>(_widget.data());
        if(font_dialog != NULL){
          font_dialog->connect(font_dialog, SIGNAL(fontSelected(const QFont&)), callback, SLOT(currentFontChanged(const QFont&)));
          font_dialog->connect(font_dialog, SIGNAL(currentFontChanged(const QFont&)), callback, SLOT(currentFontChanged(const QFont&)));
          goto gotit;
        }
      }
      
      {
        QCheckBox *button = dynamic_cast<QCheckBox*>(_widget.data());
        if (button!=NULL){
          button->connect(button, SIGNAL(toggled(bool)), callback, SLOT(toggled(bool)));
          S7CALL(void_bool,func, button->isChecked());
          goto gotit;
        }
      }

      {
        QRadioButton *button = dynamic_cast<QRadioButton*>(_widget.data());
        if (button!=NULL){
          button->connect(button, SIGNAL(toggled(bool)), callback, SLOT(toggled(bool)));
          S7CALL(void_bool,func, button->isChecked());
          goto gotit;
        }
      }

      {
        QAbstractButton *button = dynamic_cast<QAbstractButton*>(_widget.data());
        if (button!=NULL){
          button->connect(button, SIGNAL(clicked(bool)), callback, SLOT(clicked(bool)));
          goto gotit;
        }
      }

      {
        QAbstractSlider *slider = dynamic_cast<QAbstractSlider*>(_widget.data());
        if (slider!=NULL){
          slider->connect(slider, SIGNAL(intValueChanged(int)), callback, SLOT(intValueChanged(int)));
          S7CALL(void_int,func, slider->value());
          goto gotit;
        }
      }
      
      {
        QLineEdit *line_edit = dynamic_cast<QLineEdit*>(_widget.data());
        if (line_edit!=NULL){
          line_edit->connect(line_edit, SIGNAL(editingFinished()), callback, SLOT(editingFinished()));
          if (dynamic_cast<RatioSnifferQLineEdit*>(_widget.data())==NULL) // We just ignore calling this callback entirely for ratios. TODO: Remove all of these initial calls to the callbacks.
            S7CALL(void_charpointer,func, line_edit->text().toUtf8().constData()); // Calling the callbacks here was a really bad idea. TODO: Fix that.
          goto gotit;
        }
      }

      {
        QSpinBox *spinbox = dynamic_cast<QSpinBox*>(_widget.data());
        if (spinbox!=NULL){
          //spinbox->connect(spinbox, SIGNAL(valueChanged(int)), callback, SLOT(spinboxIntValueChanged(int)));
          spinbox->connect(spinbox, SIGNAL(editingFinished()), callback, SLOT(intTextEditingFinished()));
          S7CALL(void_int,func, spinbox->value());
          goto gotit;
        }
      }
      
      {
        QDoubleSpinBox *spinbox = dynamic_cast<QDoubleSpinBox*>(_widget.data());
        if (spinbox!=NULL){
          //spinbox->connect(spinbox, SIGNAL(valueChanged(double)), callback, SLOT(doubleValueChanged(double)));
          spinbox->connect(spinbox, SIGNAL(editingFinished()), callback, SLOT(doubleTextEditingFinished()));
          S7CALL(void_double,func, spinbox->value());
          goto gotit;
        }
      }

      {
        QTextEdit *text_edit = dynamic_cast<QTextEdit*>(_widget.data());
        if (text_edit!=NULL){
          text_edit->connect(text_edit, SIGNAL(textChanged()), callback, SLOT(textChanged()));
          S7CALL(void_charpointer,func, text_edit->toPlainText().toUtf8().constData());
          goto gotit;
        }
      }
            
      {
        QPlainTextEdit *text_edit = dynamic_cast<QPlainTextEdit*>(_widget.data());
        if (text_edit!=NULL){
          text_edit->connect(text_edit, SIGNAL(plainTextChanged()), callback, SLOT(plainTextChanged()));
          S7CALL(void_charpointer,func, text_edit->toPlainText().toUtf8().constData());
          goto gotit;
        }
      }

      {
        QTableWidget *table = dynamic_cast<QTableWidget*>(_widget.data());
        if (table != NULL){
          table->connect(table, SIGNAL(itemSelectionChanged()), callback, SLOT(itemSelectionChanged()));
          S7CALL(void_void,func);
          goto gotit;          
        }
      }

      handleError("Gui #%d does not have an addCallback method", (int)_gui_num);
      delete callback;
      return;

    gotit:
      if (gui_isOpen(guinum))
        _callbacks.push_back(callback);
      return;      
    }
  };
  

  struct Widget : QWidget, Gui, public radium::MouseCycleFix {
    Q_OBJECT;

  public:
    
    Widget(int width, int height)
      : Gui(this)
    {
      /*
      setAutoFillBackground(true);
      QPalette pal = palette();
      pal.setColor(QPalette::Window, QColor("black"));
      setPalette(pal);
      */
      
      if (width>=0 || height>=0){
        if(width<=0)
          width=R_MAX(1, QWidget::width());
        if(height<=0)
          height=R_MAX(1, QWidget::height());
        resize(width,height);
      }
    }

    ~Widget() {
    }

    OVERRIDERS(QWidget);
  };

  struct Popup : QWidget, Gui, public radium::MouseCycleFix {
    Q_OBJECT;

  public:
    
    Popup(int width, int height)
      : Gui(this)
    {
      
      setWindowFlags(Qt::FramelessWindowHint | Qt::Popup);
      //setWindowFlags(Qt::Popup);

      remember_geometry.move_window_to_centre_first_time_its_opened = false;
      
      if (width>=0 || height>=0){
        if(width<=0)
          width=R_MAX(1, QWidget::width());
        if(height<=0)
          height=R_MAX(1, QWidget::height());
        resize(width,height);
      }
    }

    ~Popup() {
    }

    OVERRIDERS(QWidget);
  };

  /*
  struct Frame : QFrame, Gui{
    Q_OBJECT;

  public:
    Frame()
      : Gui(this)
    {
    }
    
    OVERRIDERS(QFrame);
  };
  */

    
  struct VerticalAudioMeter : QWidget, public Gui, public radium::MouseCycleFix {
    Q_OBJECT;

  public:
    
    VerticalAudioMeter(struct Patch *patch, struct Patch *note_event_patch)
      : Gui(this)
    {
      set_widget_takes_care_of_painting_everything(this);
      createVamp(patch, note_event_patch, 0, 0, width(), height());
    }

  private:

    OVERRIDERS(QWidget);
    
    void resizeEvent2(QResizeEvent *event) override {
      _vamps.at(0)->setPos(0, 0, width(), height());
      callVampRegularly(_vamps.at(0));
      update();
    }
  };

  
  struct PushButton : QPushButton, Gui, public radium::MouseCycleFix {
    Q_OBJECT;
    
  public:
    
    PushButton(const char *text)
      : QPushButton(text)
      , Gui(this)
    {      
    }

    OVERRIDERS(QPushButton);
  };

  
  struct CheckBox : QCheckBox, Gui, public radium::MouseCycleFix {
    Q_OBJECT;
    
  public:
    
    CheckBox(const char *text, bool is_checked)
      : QCheckBox(text)
      , Gui(this)
    {
      setChecked(is_checked);
    }

    OVERRIDERS(QCheckBox);
  };

  struct RadiumCheckBox : MyQCheckBox_OnlyCustomPainting, Gui, public radium::MouseCycleFix {
    Q_OBJECT;
    
  public:
    
    RadiumCheckBox(const char *text, bool is_checked)
      : MyQCheckBox_OnlyCustomPainting(text)
      , Gui(this)
    {
      setChecked(is_checked);
    }

    OVERRIDERS(MyQCheckBox_OnlyCustomPainting);
  };
  
  struct RadioButton : QRadioButton, Gui, public radium::MouseCycleFix {
    Q_OBJECT;
    
  public:
    
    RadioButton(const char *text, bool is_checked)
      : QRadioButton(text)
      , Gui(this)
    {
      setChecked(is_checked);
    }

    OVERRIDERS(QRadioButton);
  };

  struct VerticalLayout : QWidget, Gui, public radium::MouseCycleFix {
    VerticalLayout()
      : Gui(this)
    {
      QVBoxLayout *mainLayout = new QVBoxLayout;
      setDefaultSpacing(mainLayout);

      setLayout(mainLayout);
    }

    OVERRIDERS(QWidget);
  };
  
  struct HorizontalLayout : QWidget, Gui, public radium::MouseCycleFix {
    HorizontalLayout()
      : Gui(this)
    {
      QHBoxLayout *mainLayout = new QHBoxLayout;      
      setDefaultSpacing(mainLayout);
      
      setLayout(mainLayout);
    }

    OVERRIDERS(QWidget);
  };

  struct MyGridLayout : QGridLayout{
    int _num_columns;
    int _x=0,_y=0;
    
    MyGridLayout(int num_columns)
      : _num_columns(num_columns)
    {
      setDefaultSpacing(this);
    }
    
    void addItem(QLayoutItem *item) override {
      QGridLayout::addItem(item, _y, _x);
      _x++;
      if (_x==_num_columns){
        _x = 0;
        _y++;
      }
    }
  };
  
  struct TableLayout : QWidget, Gui, public radium::MouseCycleFix {
    TableLayout(int num_columns)
      : Gui(this)
    {
      setLayout(new MyGridLayout(num_columns));
      setDefaultSpacing(layout());
    }
    
    OVERRIDERS(QWidget);
  };
  
  struct MyFlowLayout : QWidget, Gui, public radium::MouseCycleFix {
    MyFlowLayout()
      : Gui(this)
    {
      setLayout(new FlowLayout());
      setDefaultSpacing(layout());
    }
    
    OVERRIDERS(QWidget);
  };
  
  struct GroupBox : QGroupBox, Gui, public radium::MouseCycleFix {
    GroupBox(const char *title)
      : QGroupBox(title)
      , Gui(this)        
    {
      QVBoxLayout *mainLayout = new QVBoxLayout;
      //setDefaultSpacing(mainLayout); // Doesn't make sense to remove spacing from group boxes. Without spacking, they will just look like a vertical layout with a header.
      setLayout(mainLayout);
    }

    OVERRIDERS(QGroupBox);
  };
  
  struct ScrollArea : radium::ScrollArea, Gui, public radium::MouseCycleFix {
    QWidget *contents;
    const char *magic = "magic";

    ScrollArea(bool scroll_horizontal, bool scroll_vertical, bool listen_to_mouse_wheel)
      : radium::ScrollArea(NULL, listen_to_mouse_wheel)
      , Gui(this)        
    {
      horizontalScrollBar()->setObjectName("horizontalScrollBar");
      verticalScrollBar()->setObjectName("verticalScrollBar");

      if (!scroll_horizontal)
        setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
      //else
      //  setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOn);

      if (!scroll_vertical)
        setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
      //else
      //  setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOn);

      //setWidgetResizable(true);

      contents = getWidget();//(this);
      //contents->resize(500,500);
      //contents->show();

      QLayout *layout = new QVBoxLayout;
      layout->setSpacing(0);
      layout->setContentsMargins(0,0,0,0);

      contents->setLayout(layout);

      //setWidget(contents);
    }

    QLayout *getLayout(void) const override {
      return contents->layout();
    }

    OVERRIDERS(radium::ScrollArea);
  };

  struct VerticalScroll : radium::ScrollArea, Gui, public radium::MouseCycleFix {
    QWidget *contents;
    const char *magic = "magic2";
    QLayout *mylayout;

    VerticalScroll(bool listen_to_mouse_wheel)
      : radium::ScrollArea(NULL, listen_to_mouse_wheel)
      , Gui(this)        
    {
      horizontalScrollBar()->setObjectName("horizontalScrollBar");
      verticalScrollBar()->setObjectName("verticalScrollBar");

      setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);

      //setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
      //setWidgetResizable(true);
      
      contents = getWidget(); //new QWidget;//(this);
      //contents = new QWidget(this);

      mylayout = new QVBoxLayout(contents);
      setDefaultSpacing(mylayout);
      //mylayout->setSpacing(1);
      //mylayout->setContentsMargins(1,1,1,1);

      contents->setLayout(mylayout);
      
      //setWidget(contents);    
    }

    QLayout *getLayout(void) const override {
      return mylayout;
    }

    OVERRIDERS(radium::ScrollArea);
  };

  struct HorizontalScroll : radium::ScrollArea, Gui, public radium::MouseCycleFix {
    QWidget *contents;
    const char *magic = "magic3";
    QLayout *mylayout;

    HorizontalScroll(bool listen_to_mouse_wheel)
      : radium::ScrollArea(NULL, listen_to_mouse_wheel)
      , Gui(this)        
    {
      horizontalScrollBar()->setObjectName("horizontalScrollBar");
      verticalScrollBar()->setObjectName("verticalScrollBar");

      //setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
      //setWidgetResizable(true);

      contents = getWidget();
      //QWidget *contents = new QWidget(this);

      mylayout = new QHBoxLayout(contents);
      setDefaultSpacing(mylayout);
      //mylayout->setSpacing(1);
      //mylayout->setContentsMargins(1,1,1,1);

      contents->setLayout(mylayout);
      
      //setWidget(contents);    
    }

    QLayout *getLayout(void) const override {
      return mylayout;
    }

    OVERRIDERS(radium::ScrollArea);
  };

  struct Slider : MyQSlider, Gui {
    Q_OBJECT;

    bool _is_int;
    QString _text;
    radium::ProtectedS7Extra<func_t*> _get_text;
    double _min,_max;
    radium::ProtectedS7FuncVector _funcs;
    
  public:
    
    Slider(Qt::Orientation orientation, const wchar_t *text, func_t *get_text, double min, double curr, double max, bool is_int)
      : MyQSlider(orientation)
      , Gui(this)
      , _is_int(is_int)
      , _text(STRING_get_qstring(text))
      , _get_text(get_text)
      , _min(min)
      , _max(max)
#if defined(RELEASE)
      , _funcs(false)
#else
      , _funcs(true)
#endif
    {

      R_ASSERT(min!=max);

      if (orientation==Qt::Vertical){ // Weird Qt vertical slider behavor.
        double temp = max;
        max = min;
        min = temp;
        _max = max;
        _min = min;
      }
        
      if (is_int) {
        setMinimum(0);
        setMaximum(fabs(min-max));
        setTickInterval(1);
      } else {
        setMinimum(0);
        setMaximum(10000);
      }

      setGuiValue(DYN_create_float(curr));
      connect(this, SIGNAL(valueChanged(int)), this, SLOT(valueChanged(int)));      

      value_setted(value()); // In case value wasn't changed when calling setValue above.
    }

#undef CALL_PARENT_MOUSEPRESS
#undef CALL_PARENT_MOUSEMOVE
#undef CALL_PARENT_MOUSERELEASE

#define CALL_PARENT_MOUSEPRESS(classname) CALL2_PARENT_MOUSEPRESS(classname)
#define CALL_PARENT_MOUSEMOVE(classname) CALL2_PARENT_MOUSEMOVE(classname)
#define CALL_PARENT_MOUSERELEASE(classname) CALL2_PARENT_MOUSERELEASE(classname)

    OVERRIDERS(MyQSlider);

#undef CALL_PARENT_MOUSEPRESS
#undef CALL_PARENT_MOUSEMOVE
#undef CALL_PARENT_MOUSERELEASE

#define CALL_PARENT_MOUSEPRESS(classname) CALL1_PARENT_MOUSEPRESS(classname)
#define CALL_PARENT_MOUSEMOVE(classname) CALL1_PARENT_MOUSEMOVE(classname)
#define CALL_PARENT_MOUSERELEASE(classname) CALL1_PARENT_MOUSERELEASE(classname)


    void set_get_text_func(func_t *get_text){
      _get_text.set(get_text);
    }
    
    void value_setted(int value){
      ScopedEventHandlerTracker event_handler_tracker;

      double scaled_value = scale_double(value, minimum(), maximum(), _min, _max);

      if (_get_text.v != NULL)
        SLIDERPAINTER_set_string(_painter, S7CALL(charpointer_dyn, _get_text.v, DYN_create_float(scaled_value)));
      
      if (_is_int) {
        if (_get_text.v == NULL)
          SLIDERPAINTER_set_string(_painter, _text + QString::number(scaled_value));
        
        _funcs.safe_for_all(false, [scaled_value](func_t *func){
            S7CALL(void_int,func, scaled_value);
            return true;
          });
        
      } else {
                              
        if (_get_text.v == NULL)
          SLIDERPAINTER_set_string(_painter, _text + QString::number(scaled_value, 'f', 2));

        _funcs.safe_for_all(false, [scaled_value](func_t *func){
            S7CALL(void_double,func, scaled_value);
            return true;
          });
        
      }
    }

    virtual void addGuiCallback(func_t* func) override {
      _funcs.push_back(func);
      value_setted(value());
    }
    
    virtual void setGuiValue(dyn_t val) override {
      if(val.type!=INT_TYPE && val.type!=FLOAT_TYPE){
        handleError("Slider->setValue received %s, expected INT_TYPE or FLOAT_TYPE", DYN_type_name(val.type));
        return;
      }

      if (val.type==INT_TYPE)
        setValue(scale_double(val.int_number, _min, _max, minimum(), maximum()));
      else
        setValue(scale_double(val.float_number, _min, _max, minimum(), maximum()));
    }

    virtual dyn_t getGuiValue(void) override {
      double scaled_value = scale_double(value(), minimum(), maximum(), _min, _max);
      if (_is_int)
        return DYN_create_int((int)scaled_value);
      else
        return DYN_create_float(scaled_value);
    }

  public slots:
    void valueChanged(int value){
      value_setted(value);
    }
  };

  struct Text : QLabel, Gui, public radium::MouseCycleFix {
    Text(QString text, const_char* colorname, bool align_top, bool align_left)
      : Gui(this)
    {
      Qt::Alignment flags = Qt::AlignJustify;

      if(align_top)
        flags |= Qt::AlignTop;
      else
        flags |= Qt::AlignVCenter;

      if(align_left)
        flags |= Qt::AlignLeft;
      else
        flags |= Qt::AlignHCenter;

      setAlignment(flags);

      if (!strcmp(colorname,""))
        setText(text);
      else {
        QColor color = getQColor(colorname);
        if(color.isValid())
          setText("<span style=\" color:" + color.name(QColor::HexArgb) + ";\">" + text + "</span>");
      }
    }

    OVERRIDERS(QLabel);
  };

  static int64_t create_slider(Qt::Orientation orientation, dyn_t textorfunc, double min, double curr, double max, bool is_int){
    const wchar_t *text = L"";
    func_t *get_text = NULL;
    
    if (textorfunc.type==STRING_TYPE)
      text = textorfunc.string;
    else if (textorfunc.type==FUNC_TYPE)
      get_text = textorfunc.func;
    else {
      handleError("Create slider: 'textorfunc' must be text or function. Found %s", DYN_type_name(textorfunc.type));
      return -1;
    }
    return (new radium_gui::Slider(orientation, text, get_text, min, curr, max, is_int))->get_gui_num();
  }
  
  struct MyFocusSnifferQLineEdit : FocusSnifferQLineEdit{
    Q_OBJECT;
  public:
    
    MyFocusSnifferQLineEdit(QWidget *parent = NULL)
      : FocusSnifferQLineEdit(parent)
    {
      connect(this, SIGNAL(editingFinished()), this, SLOT(editingFinished()));
    }

  public slots:
    void editingFinished(){
      set_editor_focus();
      GL_lock();{
        clearFocus();
      }GL_unlock();
    }
  };

  MakeFocusSnifferClass(QTextBrowser);
  
  struct Line : MyFocusSnifferQLineEdit, Gui, public radium::MouseCycleFix {
    Q_OBJECT;

  public:
    
    Line(QString content, const_char* textcolor)
      : Gui(this)
    {
      setText(content);
      setCursorPosition(0);
      setContextMenuPolicy(Qt::NoContextMenu);

      if (strcmp(textcolor,"")){
        QColor color = getQColor(textcolor);
        QPalette palette;
        palette.setColor(QPalette::Text, color);
        setPalette(palette);
      }

    }

    OVERRIDERS(MyFocusSnifferQLineEdit);
  };

  struct MyRatioSnifferQLineEdit : RatioSnifferQLineEdit, Gui, public radium::MouseCycleFix {
    Q_OBJECT;
  public:
    
    MyRatioSnifferQLineEdit(QWidget *parent, StaticRatio ratio, bool wheelMainlyChangesNumerator = true, bool wheelDecrasesDenominatorIfNumeratorIsOne = true)
      : RatioSnifferQLineEdit(parent, wheelMainlyChangesNumerator, wheelDecrasesDenominatorIfNumeratorIsOne)
      , Gui(this)
    {
      setText(Rational(ratio).toString());
      connect(this, SIGNAL(editingFinished()), this, SLOT(editingFinished()));
    }
    
    OVERRIDERS(RatioSnifferQLineEdit);
                                     
  public slots:
    void editingFinished(){
      set_editor_focus();
      GL_lock();{
        clearFocus();
      }GL_unlock();
    }
  };
  
  //  struct TextEdit : FocusSnifferQTextEdit, Gui{
  struct TextBrowser : FocusSnifferQTextBrowser, Gui, public radium::MouseCycleFix {
    Q_OBJECT;

  public:
    
    TextBrowser(QString content)
      : Gui(this)
    {
      setText(content);
      setOpenExternalLinks(true);
    }

    OVERRIDERS(FocusSnifferQTextBrowser);
  };


  struct TextEdit : FocusSnifferQTextEdit, Gui, public radium::MouseCycleFix {
    Q_OBJECT;

  public:
    
    TextEdit(QString content)
      : Gui(this)
    {
      setPlainText(content);
      setLineWrapMode(QTextEdit::NoWrap);
    }

    OVERRIDERS(FocusSnifferQTextEdit);
  };


  struct MyFocusSnifferQSpinBox : FocusSnifferQSpinBox{
    Q_OBJECT;
  public:
    
    MyFocusSnifferQSpinBox(QWidget *parent = NULL)
      : FocusSnifferQSpinBox(parent)
    {
      connect(this, SIGNAL(editingFinished()), this, SLOT(editingFinished()));
    }
  public slots:
    void editingFinished(){
      set_editor_focus();
      GL_lock();{
        clearFocus();
      }GL_unlock();
    }
  };
  

  struct IntText : MyFocusSnifferQSpinBox, Gui, public radium::MouseCycleFix {
    Q_OBJECT;
    
  public:
    
    IntText(int min, int curr, int max)
      : Gui(this)
    {
      setMinimum(R_MIN(min, max));
      setMaximum(R_MAX(min, max));
      setValue(curr);
    }

    void stepBy(int steps) override{
      QSpinBox::stepBy(steps);
      printf("  STEPBY: %d\n", steps);

      emit editingFinished();
    }

    OVERRIDERS(MyFocusSnifferQSpinBox);
  };
  
  struct MyFocusSnifferQDoubleSpinBox : FocusSnifferQDoubleSpinBox{
    Q_OBJECT;
  public:
    
    MyFocusSnifferQDoubleSpinBox(QWidget *parent = NULL)
      : FocusSnifferQDoubleSpinBox(parent)
    {
      connect(this, SIGNAL(editingFinished()), this, SLOT(editingFinished()));
    }

  public slots:
    void editingFinished(){
      set_editor_focus();
      GL_lock();{
        clearFocus();
      }GL_unlock();
    }
  };
  

  struct FloatText : MyFocusSnifferQDoubleSpinBox, Gui, public radium::MouseCycleFix {
    Q_OBJECT;
    
  public:
    
    FloatText(double min, double curr, double max, int num_decimals, double step_interval)
      : Gui(this)
    {
      setMinimum(R_MIN(min, max));
      setMaximum(R_MAX(min, max));
      setDecimals(num_decimals);
      if (step_interval <= 0)
        setSingleStep(fabs(max-min) / 20.0);
      else
        setSingleStep(step_interval);
      setValue(curr);
    }

    void stepBy(int steps) override{
      MyFocusSnifferQDoubleSpinBox::stepBy(steps);
      printf("  STEPBY: %d\n", steps);

      emit editingFinished();
    }

    OVERRIDERS(MyFocusSnifferQDoubleSpinBox);
  };


  MakeFocusSnifferClass(QWebView);

  static QUrl getUrl(QString stringurl){
    if (stringurl.startsWith("http") || stringurl.startsWith("file:"))
      return stringurl;
    else if (QFileInfo(stringurl).isAbsolute())
      return QUrl::fromLocalFile(QDir::fromNativeSeparators(stringurl));
    else if (OS_has_full_program_file_path(stringurl))
      return QUrl::fromLocalFile(QDir::fromNativeSeparators(OS_get_full_program_file_path(stringurl)));
    else
      return QUrl::fromLocalFile(QDir::fromNativeSeparators(stringurl));
  }
  
  struct Web : FocusSnifferQWebView, Gui, public radium::MouseCycleFix {
    Q_OBJECT;
    
  public:
    
    Web(QString url)
      : Gui(this)
    {
      //setWindowTitle(url);
      setUrl(getUrl(url));
      //connect(page(),SIGNAL(downloadRequested(QNetworkRequest)),this,SLOT(download(QNetworkRequest)));
      connect(this,SIGNAL(urlChanged(const QUrl &)),this,SLOT(urlChanged(const QUrl &)));
    }

    /*
    QSize sizeHint() const override {
      return QSize(-1,-1);
    }
    */

    // https://forum.qt.io/topic/23736/qwebview-qwebpage-need-help-with-context-menu/4
    void contextMenuEvent(QContextMenuEvent * ev) override {
      ScopedEventHandlerTracker event_handler_tracker;
      
      auto *main_frame = page()->mainFrame();
      auto rel_pos = ev->pos();
      auto hit_test = main_frame->hitTestContent(rel_pos);
      auto hit_url = hit_test.linkUrl();
      if(hit_url.isEmpty()){
        //printf("NOT LINK URL\n");
        FocusSnifferQWebView::contextMenuEvent(ev);
        return;
      }

      int64_t parentgui = -1;
      QWidget *parent_widget = parentWidget();
      if (parent_widget!=NULL)
        parentgui = API_get_gui_from_widget(parent_widget);

      const char *code = talloc_format("(popup-menu \"Open Link\" (lambda ()\n"
                                       "                           (<gui> :set-url %" PRId64 " \"%s\"))\n"
                                       "            \"Open in New Window\" (lambda ()\n"
                                       "                                      (define web (<gui> :web \"%s\"))\n"
                                       "                                      (<gui> :set-parent web %" PRId64 ")\n"
                                       "                                      (<gui> :show web))\n"
                                       ")",
                                       get_gui_num(),
                                       hit_url.toString().toUtf8().constData(),
                                       hit_url.toString().toUtf8().constData(),
                                       parentgui
                                       );
      //printf("Evaling -%s-\n", code);
      evalScheme(code);
    }
    
    QString _last_search_text;

    void searchForward(void){
      if(_last_search_text != "")
        findText(_last_search_text, QWebPage::FindWrapsAroundDocument);
    }

    void zoom(bool zoom_in){
      float zoom = zoomFactor();      
      float newzoom;
      if (zoom_in)
        newzoom = zoom * 1.2;
      else
        newzoom = zoom / 1.2;
      
      if (newzoom > 0.85 && newzoom < 1.15)
        newzoom = 1.0;
      
      if (newzoom > 0.05)
        page()->mainFrame()->setZoomFactor(newzoom);
    }

    void keyPressEvent(QKeyEvent *event) override{

      // back / forward
      if (event->modifiers() & Qt::AltModifier){
        if (event->key()==Qt::Key_Left){
          back();
          event->accept();
          return;
        }
        if (event->key()==Qt::Key_Right){
          forward();
          event->accept();
          return;
        }
      }

      // reload
      if (event->key()==Qt::Key_F5 || (event->key()==Qt::Key_R && event->modifiers()&Qt::ControlModifier)){
        reload();
        event->accept();
        return;
      }

      // zoom
      if (event->modifiers()&Qt::ControlModifier){

        if (event->key()==Qt::Key_Minus || event->key()==Qt::Key_Plus) { 
          zoom(event->key()==Qt::Key_Plus);
          event->accept();
          return;
        }
      }

      // search
      if (event->key()==Qt::Key_F && event->modifiers()&Qt::ControlModifier){
        char *s = GFX_GetString(root->song->tracker_windows, NULL, "Search for (F3 to repeat): ", true);
        if (s!=NULL && strlen(s)>0){
          _last_search_text = s;
          searchForward();
        }

        // Some hacking required to get focus since FocusSniffer calls obtain_keyboard_focus/release_keyboard_focus instead of obtain_keyboard_focus_counting/release_keyboard_focus_counting. At least I think it got something to do with that.
        set_editor_focus();
        activateWindow();
        QTimer::singleShot(50, this, SLOT(setFocus()));
        QTimer::singleShot(100, obtain_keyboard_focus);
        QTimer::singleShot(250, set_editor_focus);
        QTimer::singleShot(500, this, SLOT(setFocus()));
        QTimer::singleShot(1000, obtain_keyboard_focus);

        event->accept();
        return;
      }

      // repeat search
      if (event->key()==Qt::Key_F3){
        searchForward();
        event->accept();
        return;
      }

      // cancel search
      // (Must also catch Key_Escape since the focussniffer gives up focus when receiving escape.)
      if (event->key()==Qt::Key_Escape){
        findText(""); // Cancel search highlightning
        event->accept();
        return;
      }
      
      if (!Gui::keyPressEvent(event)){
        FocusSnifferQWebView::keyPressEvent(event);
        return;
      }
    }

    // TODO: We should probably not call FocusSnifferQWebView::keyReleaseEvent if we ate the keypress event.
    void keyReleaseEvent(QKeyEvent *event) override{
      if (!Gui::keyReleaseEvent(event)){
        FocusSnifferQWebView::keyReleaseEvent(event);
        return;
      }
    }

    void wheelEvent(QWheelEvent *qwheelevent) override {
      if (qwheelevent->modifiers() & Qt::ControlModifier){

        zoom(qwheelevent->delta() > 0);

      } else {

        Qt::Orientation orientation;
          
        if (HorizontalModifierPressed(qwheelevent->modifiers())) {
          orientation = Qt::Horizontal;
          page()->mainFrame()->setScrollBarValue(orientation, page()->mainFrame()->scrollBarValue(orientation) + qwheelevent->delta()/2);
        } else {
          //orientation = Qt::Vertical;
          FocusSnifferQWebView::wheelEvent(qwheelevent);
        }
      }
    }

    // Implement "Open link in new window"
    QWebView *createWindow(QWebPage::WebWindowType type) override{
      auto *ret = new Web("");
      ret->show();
      return ret;
    }
                     
    OVERRIDERS_WITHOUT_KEY_AND_MOUSE_WHEEL(FocusSnifferQWebView);

  public slots:
    
    void urlChanged(const QUrl &url){
      setWindowTitle(url.toString());
    }
    
    /*

    void download(const QNetworkRequest &request){
      qDebug()<<"Download Requested: "<<request.url();
    } 
    */   
  };


  struct FileRequester : QFileDialog, Gui, public radium::MouseCycleFix {
    Q_OBJECT;
    
  public:

    FileRequester(QString header_text, QString dir, QString filetypename, QString postfixes, bool for_loading)
      : QFileDialog(NULL, header_text, dir, FileRequester::get_postfixes_filter(filetypename, postfixes))
      , Gui(this)
    {
      /*
      setWindowTitle(header_text);
      setDirectory(dir);
      setSupportedSchemes(FileRequester::get_postfixes_filter(filetypename, postfixes).split(";;"));
      */

#if FOR_MACOSX
      printf("          WORKAROUND: Always set DontUseNativeDialog on OSX to avoid partly GUI freeze.\n");
      setOption(QFileDialog::DontUseNativeDialog, true);
#endif
      
      if (for_loading)
        setAcceptMode(QFileDialog::AcceptOpen);
      else
        setAcceptMode(QFileDialog::AcceptSave);

      if (!g_filedialog_geometry.isEmpty()){
        restoreGeometry(g_filedialog_geometry);
      }
      
      _have_set_size = true; // shouldn't this one be set only if we restore geometry?
    }

    ~FileRequester(){
      g_filedialog_geometry = saveGeometry();
    }
    
    static QString get_postfixes_filter(QString type, QString postfixes){
      QString postfixes2 = postfixes==NULL ? "*.rad *.mmd *.mmd2 *.mmd3 *.MMD *.MMD2 *.MMD3" : QString(postfixes);
      
#if FOR_WINDOWS
      return postfixes2 + " ;; All files (*)";
#else
      type = type==NULL ? "Song files" : type;
      return QString(type) + " (" + postfixes2 + ") ;; All files (*)";
#endif
    }

    OVERRIDERS(QFileDialog);
  };

  
  struct FontRequester : QFontDialog, Gui, public radium::MouseCycleFix {
    Q_OBJECT;
    
  public:

    FontRequester(QFont font)
      : Gui(this)
    {
      // setOption(QFontDialog::NoButtons, true);
      setCurrentFont(font);
      if (!g_fontdialog_geometry.isEmpty()){
        restoreGeometry(g_fontdialog_geometry);
        _have_set_size = true;
      }
    }

    ~FontRequester(){
      g_fontdialog_geometry = saveGeometry();
    }
    
    void done(int result) override{
      ScopedEventHandlerTracker event_handler_tracker;

      printf("  DONE: %d\n", result);
      for (auto *callback : _callbacks)
        S7CALL(void_dyn, callback->_func.v, DYN_create_bool(result==0 ? false : 1));
      
      deleteLater(); // Must do this to close the dialog. Qt does something strange here. Seems like QFontDialog overrides the close callback.
    }

    OVERRIDERS(QFontDialog);
  };

  struct TabBar : QTabBar, Gui, public radium::MouseCycleFix {
    Q_OBJECT;

  public:
    
    TabBar(QTabBar::Shape shape, QWidget *parent = NULL)
      : QTabBar(parent)
      , Gui(this)
    {
      setShape(shape);
      setUsesScrollButtons(false); // The "my-tabs" function scales the text.

      QFontMetrics fm(QApplication::font());
      int systemfontheight = round(fm.height()*getTabBarHeight());
      QString pix = QString::number(systemfontheight);

      setStyleSheet(QString("QTabBar::tab { height: ") + pix + "px; width: " + pix + "px; }");
      
      //setExpanding(true); // Doesn't work. Instead there is some crazy code in gui.scm/my-tabs to do this manually.
      setExpanding(false); // Setting it explicitly to false. In case setExpanding starts working, things could look strange.
    }

    OVERRIDERS(QTabBar);
  };


  struct Tabs : QTabWidget, Gui, public radium::MouseCycleFix {
    Q_OBJECT;

  public:
    
    Tabs(int tab_pos, QWidget *parent = NULL)
      : QTabWidget(parent)
      , Gui(this)
    {
      setTabPosition((QTabWidget::TabPosition)tab_pos);

      setDocumentMode(false); // Remove border
      
      setTabBar(new TabBar((QTabBar::Shape)tab_pos, this));
      
      tabBar()->setDocumentMode(false); // Remove border
    }
    
    /*
    virtual QSize sizeHint() const override {
      if (currentWidget()==NULL)
        return QSize(10,10);
      
      return currentWidget()->sizeHint();
    }
    */
    
    OVERRIDERS(QTabWidget);
  };


  struct Splitter : radium::Splitter, Gui, public radium::MouseCycleFix {
    Q_OBJECT;
    
  public:
    
    Splitter(bool horizontal, bool childrenCollappsible)
      : radium::Splitter(horizontal ? Qt::Horizontal : Qt::Vertical)
      , Gui(this)
    {
      setChildrenCollapsible(childrenCollappsible);
    }
    /*
    virtual QSize sizeHint() const override {
      if (currentWidget()==NULL)
        return QSize(10,10);
      
      return currentWidget()->sizeHint();
    }
    */
    OVERRIDERS(radium::Splitter);
  };

  // QRubberBand doesn't work. I've also searched the internet, and no one seems to have made it work.
  // Use gui-rubber-band from gui.scm instead (workaround)
  /*
  struct RubberBand : QWidget, Gui{
    Q_OBJECT;

  public:

    RubberBand(float opacity)
    //: QWidgetRubberBand(QRubberBand::Rectangle)
      , Gui(this)
    {
      setWindowOpacity(opacity);
      setWindowFlags(Qt::FramelessWindowHint);
    }

    //OVERRIDERS(QRubberBand);
    void paintEvent(QPaintEvent *)
    {
      QColor backgroundColor = palette().background().color();
      backgroundColor.setAlpha(216); // Use Alphachannel u want
      QPainter painter(this);
      painter.fillRect(rect(),backgroundColor);
    }

  };
  */

  struct Table : QTableWidget, Gui, public radium::MouseCycleFix {
    Q_OBJECT;

  public:

    int _sorting_disabled = 1; // sorting is disabled if _sorting_disabled > 0
    
    Table(QStringList headers)
      : Gui(this)
    {
      setColumnCount(headers.size());
      setHorizontalHeaderLabels(headers);

      setSelectionBehavior(QAbstractItemView::SelectRows);
      setSelectionMode(QAbstractItemView::SingleSelection);

      for(int y=0;y<headers.size();y++)
        horizontalHeader()->setSectionResizeMode(y, QHeaderView::Interactive);
      //horizontalHeader()->setSectionResizeMode(y, QHeaderView::ResizeToContents);
      //      horizontalHeader()->setSectionResizeMode(y, QHeaderView::Stretch);
      
      //horizontalHeader()->setSectionResizeMode(0, QHeaderView::Stretch);
      //horizontalHeader()->setSectionResizeMode(0, QHeaderView::Interactive);
                          
      //horizontalHeader()->setSectionResizeMode(1, QHeaderView::Interactive);

      setHorizontalScrollBar(new Qt_MyQScrollBar(Qt::Horizontal));
      setVerticalScrollBar(new Qt_MyQScrollBar(Qt::Vertical));
      setCornerWidget(NULL);
      
      //horizontalScrollBar()->setCursor(Qt::OpenHandCursor);
      //verticalScrollBar()->setCursor(Qt::OpenHandCursor);
    }

    OVERRIDERS(QTableWidget);
  };


  struct MyUiLoader : public QUiLoader{
    MyUiLoader(){
      clearPluginPaths();
    }

    QWidget *createWidget(const QString &className, QWidget *parent = Q_NULLPTR, const QString &name = QString()) override {
      ScopedEventHandlerTracker event_handler_tracker;
      
      QWidget *ret = NULL;

      //printf("\n\n  CLASSNAME: -%s-\n\n", className.toUtf8().constData());
      if (className=="QTextEdit" || className=="QPlainTextEdit")
        ret = new FocusSnifferQTextEdit(parent);
      else if (className=="QLineEdit")
        ret = new MyFocusSnifferQLineEdit(parent);
      else if (className=="RatioQLineEdit")
        ret = new MyRatioSnifferQLineEdit(parent, make_static_ratio(1,1));
      else if (className=="QSpinBox")
        ret = new MyFocusSnifferQSpinBox(parent);
      else if (className=="QDoubleSpinBox")
        ret = new MyFocusSnifferQDoubleSpinBox(parent);
      else if (className=="QTabWidget")
        ret = new Tabs(0, parent);
      else
        return QUiLoader::createWidget(className, parent, name);

      ret->setObjectName(name);
      return ret;
    }
  };

  static MyUiLoader *get_uiloader(void){
    static MyUiLoader *uiloader = NULL;
    if (uiloader==NULL)
      uiloader = new MyUiLoader;
    return uiloader;
  }
}


using namespace radium_gui;

int gui_getSystemFontheight(void){
  return root->song->tracker_windows->systemfontheight;
}

void gui_toolTip(const_char* text){
  QToolTip::showText(QCursor::pos(),text,NULL,QRect());
}

const_char* gui_mixColors(const_char* color1, const_char* color2, float how_much_color1){
  QColor col1 = getQColor(color1);
  QColor col2 = getQColor(color2);
  return talloc_strdup(mix_colors(col1, col2, how_much_color1).name(QColor::HexArgb).toUtf8().constData());
}

const_char* gui_setAlphaForColor(const_char* color, float how_much_alpha){
  QColor col1 = getQColor(color);
  //printf(" Before: %S\n", STRING_create(col1.name(QColor::HexArgb)));
  col1.setAlphaF(how_much_alpha);
  //printf(" After: %S\n\n", STRING_create(col1.name(QColor::HexArgb)));
  return talloc_strdup(col1.name(QColor::HexArgb).toUtf8().constData());
}

const_char* gui_makeColorLighter(const_char* color, float how_much){
  QColor col1 = getQColor(color);
  col1 = col1.lighter(how_much*100);
  return talloc_strdup(col1.name(QColor::HexArgb).toUtf8().constData());
}

const_char* gui_makeColorDarker(const_char* color, float how_much){
  QColor col1 = getQColor(color);
  col1 = col1.darker(how_much*100);
  return talloc_strdup(col1.name(QColor::HexArgb).toUtf8().constData());
}


static Gui *get_gui_maybeclosed(int64_t guinum){
  if (guinum < 0 || guinum > g_highest_guinum){
    handleError("There has never been a Gui #%d", (int)guinum);
    return NULL;
  }

  return g_guis.value(guinum);
}


static Gui *get_gui(int64_t guinum){

  if (guinum==-4) {
    
    QWidget *parent = QApplication::widgetAt(QCursor::pos());
    if (parent != NULL)
      guinum = API_get_gui_from_widget(parent);
    
  } else if (guinum < 0) {
    
    QWidget *parent = API_gui_get_parentwidget(NULL, guinum);
    if (parent != NULL)
      guinum = API_get_gui_from_existing_widget(parent);
    
  }

  Gui *gui = get_gui_maybeclosed(guinum);

  if (gui==NULL){
    if (guinum>=0 && guinum<g_highest_guinum)
      handleError("Gui #%d has been closed and can not be used.", (int)guinum);
    return NULL;
  }
  
  if (gui->_widget==NULL) {
    R_ASSERT(gui->_created_from_existing_widget); // GUI's that are not created from an existing widget are (i.e. should be) automatically removed when the QWidget is deleted.
    handleError("Gui #%d (class %s), created from an existing widget, has been closed and can not be used.", (int)gui->get_gui_num(), gui->_class_name.toUtf8().constData());
    delete gui;
    return NULL;
  }

  return gui;
}

float gui_textWidth(const_char* text, int64_t guinum){
  QFont font;

  if (guinum >= 0){
    Gui *gui = get_gui(guinum);
    if (gui != NULL) {
      QPainter *painter = gui->get_painter();
      if (painter == NULL)
        font = gui->_widget->font();
      else
        font = painter->font();
    } else {
      font = QApplication::font();
    }
  } else {
    font = QApplication::font();
  }

  const QFontMetrics fn = QFontMetrics(font);
  return fn.boundingRect(text).width();
}

int64_t gui_ui(const_char *filename){
  
  QFile file(filename);
  if (file.open(QFile::ReadOnly)==false){
    handleError("Unable to open \"%s\": %s", filename, file.errorString().toUtf8().constData());
    return -1;
  }
  
  QWidget *widget = get_uiloader()->load(&file);
  file.close();

  if (widget==NULL){
    handleError("Unable to open \"%s\": %s", filename, get_uiloader()->errorString().toUtf8().constData());
    return -1;
  }
  
  Gui *gui = new VerticalLayout();
  gui->_widget->layout()->addWidget(widget);
  
  return gui->get_gui_num();
}

int gui_numOpenGuis(void){
  return g_valid_guis.size();
}

int64_t gui_random(void){
  return g_valid_guis[qrand() % g_valid_guis.size()]->get_gui_num();
}

int64_t gui_child(int64_t guinum, const_char* childname){
  Gui *gui = get_gui(guinum);

  if (gui==NULL)
    return -1;

  QWidget *child = gui->_widget->findChild<QWidget*>(childname);

  if (child==NULL){
    handleError("gui_child: Could not find child \"%s\" in gui #%d.", childname, (int)guinum);
    return -1;
  }

  return API_get_gui_from_widget(child);
}

void gui_setName(int64_t guinum, const_char* guiname){
  Gui *gui = get_gui(guinum);

  if (gui==NULL)
    return;

  gui->_widget->setObjectName(guiname);
}

static void perhaps_collect_a_little_bit_of_gui_garbage(int num_guis_to_check){
  static int pos = 0;

  while(g_valid_guis.size() > 0 && num_guis_to_check > 0){
    if (pos >= g_valid_guis.size())
      pos = 0;

    Gui *gui = g_valid_guis[pos];

    //printf("Checking %d/%d (guinum: %d)\n", pos, g_valid_guis.size(), gui->get_gui_num());
    
    R_ASSERT_RETURN_IF_FALSE(gui!=NULL);
    
    if (gui->_widget==NULL){

      printf("        GUI GC: COLLECTING gui garbage. Pos: %d, guinum: %d\n", pos, (int)gui->get_gui_num());
      delete gui;

    } else {
    
      if (gui->_has_been_closed && gui->_delayed_deletion && safe_to_close_widget()) {
        printf("        GUI GC: Delayed closing of GUI. Pos: %d, guinum: %d\n", pos, (int)gui->get_gui_num());
        gui->_widget->close(); // Try to close again. Last time some scheme code was running, so we delayed closing the widget.
      }

    }


      

    pos++;
    num_guis_to_check--;
  }
}

void *API_get_native_gui_handle(int64_t guinum){
  Gui *gui = get_gui(guinum);
  
  if (gui==NULL)
    return NULL;

  return (void*)gui->_widget->effectiveWinId();
}


void API_add_child_plugin_gui(int64_t guinum, struct Patch *patch){
  R_ASSERT_RETURN_IF_FALSE(patch!=NULL);
  R_ASSERT_RETURN_IF_FALSE(patch->instrument==get_audio_instrument());
    
  SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
  R_ASSERT_RETURN_IF_FALSE(plugin!=NULL);
  R_ASSERT_RETURN_IF_FALSE(plugin->type->show_gui!=NULL);
  R_ASSERT_RETURN_IF_FALSE(plugin->type->hide_gui!=NULL);

  Gui *gui = get_gui(guinum);
  
  if (gui==NULL){
    R_ASSERT_NON_RELEASE(false);
    return;
  }

  gui->_child_plugin_gui_patch_ids << patch->id;
}

void API_remove_child_plugin_gui(int64_t guinum, struct Patch *patch){
  R_ASSERT_RETURN_IF_FALSE(patch!=NULL);
  R_ASSERT_RETURN_IF_FALSE(patch->instrument==get_audio_instrument());
  
  SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
  R_ASSERT_RETURN_IF_FALSE(plugin!=NULL);
  R_ASSERT_RETURN_IF_FALSE(plugin->type->show_gui!=NULL);
  R_ASSERT_RETURN_IF_FALSE(plugin->type->hide_gui!=NULL);

  Gui *gui = get_gui(guinum);
  
  if (gui==NULL){
    printf("API_remove_child_plugin_gui. Note: No GUI #%d. Perhaps it has been closed? (Patch: %s)\n", (int)guinum, patch->name);
    //R_ASSERT_NON_RELEASE(false);
    return;
  }

  if(gui->_child_plugin_gui_patch_ids.remove(patch->id)==false){
    printf("API_remove_child_plugin_gui. Note: No patch %s in gui #%d\n", patch->name, (int)guinum);
    R_ASSERT_NON_RELEASE(false);
  }
}
  

// Widget can have been created any type of way. Both using one of the gui_* functions, or other places.
// A new Gui will be created if the widget is not included in g_guis/etc. already.
int64_t API_get_gui_from_widget(QWidget *widget){
  R_ASSERT_RETURN_IF_FALSE2(widget!=NULL, -1);

  {
    Gui *gui = g_gui_from_widgets.value(widget);
    if (gui==NULL)
      gui = g_gui_from_full_screen_widgets.value(dynamic_cast<FullScreenParent*>(widget));
  
    if (gui != NULL){
      if (gui->_widget == NULL){
        //handleError("Gui #%d, created from existing widget, has been closed and can not be used.", gui->get_gui_num()); // It probably doesn't happen very often, but it's not an error. (It means that a new QWidget instance has the same pointer value as an old deleted QWidget.)
        delete gui;
      }else
        return gui->get_gui_num();
    }
  }

  return (new Gui(widget, true))->get_gui_num();
}

int64_t API_get_gui_from_existing_widget(QWidget *widget){
  R_ASSERT_RETURN_IF_FALSE2(widget!=NULL, -1);

#if !defined(RELEASE)
  // Check that it's hasn't been added as normal GUI. (this assertion is the only difference between API_get_gui_from_existing_widget and get_gui_from_widget).
  const Gui *gui = g_gui_from_widgets.value(widget);
  if (gui==NULL)
    gui = g_gui_from_full_screen_widgets.value(dynamic_cast<FullScreenParent*>(widget));
  
  if (gui!=NULL)
    R_ASSERT(gui->_created_from_existing_widget);
#endif

  return API_get_gui_from_widget(widget);
}

void API_run_paint_event_for_custom_widget(QWidget *widget, QPaintEvent *ev, const QRegion &already_painted_areas){
  ScopedEventHandlerTracker event_handler_tracker;

  Gui *gui = g_gui_from_widgets.value(widget);
  if (gui==NULL){
    fprintf(stderr, "     API_run_paint_event_for_custom_widget: No Gui created for widget %p\n", widget);
    R_ASSERT_NON_RELEASE(false);
    return;
  }

  gui->paintEvent(ev, already_painted_areas);
}

void API_gui_set_curr_painter(QWidget *widget, QPainter *p){
  Gui *gui = g_gui_from_widgets.value(widget);

  if(p==NULL)
    R_ASSERT(gui->_current_painter!=NULL);

  gui->_current_painter = p;
}

bool API_run_custom_gui_paint_function(QWidget *widget, QPainter *p, const QRegion *region, std::function<void(void)> func){
  Gui *gui = g_gui_from_widgets.value(widget);
  if (gui==NULL){
    API_get_gui_from_existing_widget(widget);
    gui = g_gui_from_widgets.value(widget);
    if (gui==NULL){
      fprintf(stderr, "     API_run_custom_gui_paint_function: No Gui created for widget %p\n", widget);
      R_ASSERT_NON_RELEASE(false);
      return false;
    }
  }

  QRegion clip_region = p->clipRegion();
  bool has_clipping = p->hasClipping();
  
  //printf("seq guinum: %d\n",(int)gui->_gui_num);
  bool ret = gui->paintEvent3(p, region, func);

  p->setClipRegion(clip_region); // restore old clip region
  p->setClipping(has_clipping);
  
  p->setFont(QFont()); // 'drawText' doesn't clean up font type afterwards.
  
  return ret;
}


bool API_run_mouse_press_event_for_custom_widget(QWidget *widget, QMouseEvent *ev){
  ScopedEventHandlerTracker event_handler_tracker;

  Gui *gui = g_gui_from_widgets.value(widget);
  if (gui==NULL){
    fprintf(stderr, "     API_run_mouse_press_event_for_custom_widget: No Gui created for widget %p\n", widget);
    R_ASSERT_NON_RELEASE(false);
    return false;
  }

  if (gui->_mouse_callback.v != NULL)
    return gui->mousePressEvent(ev);
  else
    return false;
}

bool API_run_mouse_move_event_for_custom_widget(QWidget *widget, QMouseEvent *ev){
  ScopedEventHandlerTracker event_handler_tracker;

  Gui *gui = g_gui_from_widgets.value(widget);
  if (gui==NULL){
    fprintf(stderr, "     API_run_mouse_press_event_for_custom_widget: No Gui created for widget %p\n", widget);
    R_ASSERT_NON_RELEASE(false);
    return false;
  }

  if (gui->_mouse_callback.v != NULL)
    return gui->mouseMoveEvent(ev);
  else
    return false;
}

bool API_run_mouse_release_event_for_custom_widget(QWidget *widget, radium::MouseCycleEvent &event){
  ScopedEventHandlerTracker event_handler_tracker;

  Gui *gui = g_gui_from_widgets.value(widget);
  if (gui==NULL){
    fprintf(stderr, "     API_run_mouse_press_event_for_custom_widget: No Gui created for widget %p\n", widget);
    R_ASSERT_NON_RELEASE(false);
    return false;
  }

  if (gui->_mouse_callback.v != NULL)
    return gui->mouseReleaseEvent(event);
  else
    return false;
}

bool API_run_mouse_wheel_event_for_custom_widget(QWidget *widget, QWheelEvent *event){
  ScopedEventHandlerTracker event_handler_tracker;

  Gui *gui = g_gui_from_widgets.value(widget);
  if (gui==NULL){
    fprintf(stderr, "     API_run_mouse_press_event_for_custom_widget: No Gui created for widget %p\n", widget);
    R_ASSERT_NON_RELEASE(false);
    return false;
  }

  if (gui->_mouse_wheel_callback.v != NULL)
    return gui->wheelEvent(event);
  else
    return false;
}

bool API_run_mouse_leave_event_for_custom_widget(QWidget *widget, QEvent *ev){
  ScopedEventHandlerTracker event_handler_tracker;

  Gui *gui = g_gui_from_widgets.value(widget);
  if (gui==NULL){
    fprintf(stderr, "     API_run_mouse_press_event_for_custom_widget: No Gui created for widget %p\n", widget);
    R_ASSERT_NON_RELEASE(false);
    return false;
  }

  if (gui->_mouse_callback.v != NULL)
    return gui->mouseLeaveEvent(ev);
  else
    return false;
}

void API_run_resize_event_for_custom_widget(QWidget *widget, QResizeEvent *ev){
  ScopedEventHandlerTracker event_handler_tracker;

  Gui *gui = g_gui_from_widgets.value(widget);
  if (gui==NULL){
    fprintf(stderr, "     API_run_mouse_press_event_for_custom_widget: No Gui created for widget %p\n", widget);
    R_ASSERT_NON_RELEASE(false);
    return;
  }

  if (gui->_resize_callback.v != NULL)
    gui->resizeEvent(ev);
}

int64_t gui_getMainXSplitter(void){
  EditorWidget *editor = static_cast<EditorWidget*>(root->song->tracker_windows->os_visual.widget);
  return API_get_gui_from_existing_widget(editor->xsplitter);
}

int64_t gui_getEditorGui(void){
  static int64_t guinum = -100;

  if (guinum==-100){
    EditorWidget *editor = static_cast<EditorWidget*>(root->song->tracker_windows->os_visual.widget);
    guinum = API_get_gui_from_existing_widget(editor);
  }

  return guinum;
}

int64_t gui_getSequencerGui(void){
  return API_get_gui_from_existing_widget(SEQUENCER_getWidget());
}

int64_t gui_getInstrumentGui(void){
  return API_get_gui_from_existing_widget(getInstrumentsWidget());
}

double gui_getEditorDistanceX(int64_t guinum){
  Gui *gui = get_gui(guinum);
  
  if (gui==NULL){
    R_ASSERT_NON_RELEASE(false);
    return 0;
  }

  QPoint p1 = mapToEditor(gui->_widget, QPoint(0, 0));
  return p1.x();
}

double gui_getEditorDistanceY(int64_t guinum){
  Gui *gui = get_gui(guinum);
  
  if (gui==NULL){
    R_ASSERT_NON_RELEASE(false);
    return 0;
  }

  QPoint p1 = mapToEditor(gui->_widget, QPoint(0, 0));
  return p1.y();
}

/////// Callbacks
//////////////////////////

// Callbacks probably don't work in existing widgets since they are not subclasses of both QWidget and Gui.
static bool check_existing(Gui *gui){
  if(gui->_created_from_existing_widget){
    
    handleError("Can not add callback. Gui #%d of type \"%s\" is created from an existing qt widget, and is not created by the API.\n", (int)gui->_gui_num, gui->_class_name.toUtf8().constData());

    return false;
    
  } else {
    
    return true;
    
  }
}

void gui_addDeletedCallback(int64_t guinum, func_t* func){
  Gui *gui = get_gui(guinum);

  if (gui==NULL)
    return;

  if(check_existing(gui)==false)
    return;
  
  gui->_deleted_callbacks.push_back(func);
}
                      
void gui_addRealtimeCallback(int64_t guinum, func_t* func){
  Gui *gui = get_gui(guinum);

  if (gui==NULL)
    return;

  // Uses signal system.
  //if(check_existing(gui)==false)
  //  return;

  gui->addGuiRealtimeCallback(func);
}

void gui_addCallback(int64_t guinum, func_t* func){
  Gui *gui = get_gui(guinum);

  if (gui==NULL)
    return;

  // This callback works on non-existing widgets since it uses the signal system of Qt and does not depend on virtual methods.
  //if(check_existing(gui)==false)
  //  return;

  gui->addGuiCallback(func);
}

void gui_addMouseCallback(int64_t guinum, func_t* func){
  Gui *gui = get_gui(guinum);

  if (gui==NULL)
    return;

  if (gui->_widget!=SEQUENCER_getWidget_r0()) // we call the mouse callback manually for the sequencer gui
    if(check_existing(gui)==false)
      return;
  
  gui->addMouseCallback(func);
}

void gui_addMouseWheelCallback(int64_t guinum, func_t* func){
  Gui *gui = get_gui(guinum);

  if (gui==NULL)
    return;

  if (gui->_widget!=SEQUENCER_getWidget_r0()) // we call the mouse callback manually for the sequencer gui
    if(check_existing(gui)==false)
      return;
  
  gui->addMouseWheelCallback(func);
}

void gui_removeMouseWheelCallback(int64_t guinum){
  Gui *gui = get_gui(guinum);

  if (gui==NULL)
    return;

  if(check_existing(gui)==false)
    return;
  
  gui->removeMouseWheelCallback();
}

void gui_addKeyCallback(int64_t guinum, func_t* func){
  Gui *gui = get_gui(guinum);

  if (gui==NULL)
    return;

  if(check_existing(gui)==false)
    return;
  
  gui->addKeyCallback(func);
}

void gui_addFocusInCallback(int64_t guinum, func_t* func){
  Gui *gui = get_gui(guinum);

  if (gui==NULL)
    return;

  if(check_existing(gui)==false)
    return;
  
  gui->addFocusInCallback(func);
}

void gui_addDoubleClickCallback(int64_t guinum, func_t* func){
  Gui *gui = get_gui(guinum);

  if (gui==NULL)
    return;

  
  if (gui->addDoubleClickCallback(func)==false)
    if(check_existing(gui)==false)
      return;
}

void gui_addCloseCallback(int64_t guinum, func_t* func){
  Gui *gui = get_gui(guinum);

  if (gui==NULL)
    return;

  if(check_existing(gui)==false)
    return;
  
  gui->addCloseCallback(func);
}

void gui_addResizeCallback(int64_t guinum, func_t* func){
  Gui *gui = get_gui(guinum);

  if (gui==NULL)
    return;

  if (gui->_widget!=SEQUENCER_getWidget_r0()) // we call the mouse callback manually for the sequencer gui
    if(check_existing(gui)==false)
      return;
  
  gui->addResizeCallback(func);
}

void gui_addPaintCallback(int64_t guinum, func_t* func){
  Gui *gui = get_gui(guinum);

  if (gui==NULL)
    return;

  if (gui->_widget!=SEQUENCER_getWidget_r0()) // we call the mouse callback manually for the sequencer gui
    if(check_existing(gui)==false)
      return;
  
  gui->addPaintCallback(func);
}

void gui_updateRecursively(int64_t guinum){
  Gui *gui = get_gui(guinum);

  if (gui==NULL)
    return;

  R_ASSERT_RETURN_IF_FALSE(g_qt_is_painting==false);

  updateWidgetRecursively(gui->_widget);
}

void gui_update(int64_t guinum, double x1, double y1, double x2, double y2){

  Gui *gui = get_gui(guinum);

  if (gui==NULL)
    return;

  R_ASSERT_RETURN_IF_FALSE(g_qt_is_painting==false);

  if (x1 < 0)
    gui->_widget->update();
  else
    gui->_widget->update(QRectF(x1, y1, x2-x1, y2-y1).toAlignedRect());
}

void gui_setClipRect(int64_t guinum, double x1, double y1, double x2, double y2){
  Gui *gui = get_gui(guinum);

  if (gui==NULL)
    return;

  QPainter *painter = gui->get_painter();

  painter->setClipRect(QRectF(x1, y1, x2-x1, y2-y1));
  painter->setClipping(true);  
}

void gui_cancelClipRect(int64_t guinum){
  Gui *gui = get_gui(guinum);

  if (gui==NULL)
    return;

  QPainter *painter = gui->get_painter();

  painter->setClipping(false);
}

void gui_dontAutofillBackground(int64_t guinum){
  Gui *gui = get_gui(guinum);

  if (gui==NULL)
    return;

  set_widget_takes_care_of_painting_everything(gui->_widget);
}

/////// Widgets
//////////////////////////


int64_t gui_widget(int width, int height){
  return (new Widget(width, height))->get_gui_num();
}

int64_t gui_popup(int width, int height){
  return (new Popup(width, height))->get_gui_num();
}

/*
int64_t gui_frame(void){
  return (new Frame())->get_gui_num();
}
*/

int64_t gui_button(const_char *text){
  return (new PushButton(text))->get_gui_num();
}

int64_t gui_checkbox(const_char *text, bool is_checked, bool radium_style){
  //return -1;
  if (radium_style)
    return (new RadiumCheckBox(text, is_checked))->get_gui_num();
  else
    return (new CheckBox(text, is_checked))->get_gui_num();
}

int64_t gui_radiobutton(const_char *text, bool is_checked){
      //return -1;
  return (new RadioButton(text, is_checked))->get_gui_num();
}

int64_t gui_horizontalIntSlider(dyn_t textorfunc, int min, int curr, int max){
  if(min==max){
    handleError("Gui slider: minimum and maximum value is the same");
    return -1;
  }
  //return -1;
  return create_slider(Qt::Horizontal, textorfunc, min, curr, max, true);
}
int64_t gui_horizontalSlider(dyn_t textorfunc, double min, double curr, double max){
  if(min==max){
    handleError("Gui slider: minimum and maximum value is the same");
    return -1;
  }
  //return -1;
  return create_slider(Qt::Horizontal, textorfunc, min, curr, max, false);
}
int64_t gui_verticalIntSlider(dyn_t textorfunc, int min, int curr, int max){
  if(min==max){
    handleError("Gui slider: minimum and maximum value is the same");
    return -1;
  }
  //return -1;
  return create_slider(Qt::Vertical, textorfunc, min, curr, max, true);
}
int64_t gui_verticalSlider(dyn_t textorfunc, double min, double curr, double max){
  if(min==max){
    handleError("Gui slider: minimum and maximum value is the same");
    return -1;
  }
  //return -1;
  return create_slider(Qt::Vertical, textorfunc, min, curr, max, false);
}


int64_t gui_verticalLayout(void){
  //return -1;
  return (new VerticalLayout())->get_gui_num();
}

int64_t gui_horizontalLayout(void){
  //return -1;
  return (new HorizontalLayout())->get_gui_num();
}

int64_t gui_tableLayout(int num_columns){
  //return -1;
  return (new TableLayout(num_columns))->get_gui_num();
}

int64_t gui_flowLayout(void){
  return (new MyFlowLayout())->get_gui_num();
}

int64_t gui_group(const_char* title){
  //return -1;
  return (new GroupBox(title))->get_gui_num();
}

int64_t gui_scrollArea(bool scroll_horizontal, bool scroll_vertical, bool listen_to_mouse_wheel){
  return (new ScrollArea(scroll_horizontal, scroll_vertical, listen_to_mouse_wheel))->get_gui_num();
}

int64_t gui_verticalScroll(bool listen_to_mouse_wheel){
  return (new VerticalScroll(listen_to_mouse_wheel))->get_gui_num();
}

int64_t gui_horizontalScroll(bool listen_to_mouse_wheel){
  return (new HorizontalScroll(listen_to_mouse_wheel))->get_gui_num();
}

int64_t gui_text(const_char* text, const_char* color, bool align_top, bool align_left){
  return (new Text(text, color, align_top, align_left))->get_gui_num();
}

int64_t gui_textEdit(const_char* content, bool read_only){
  //return -1;
  if (read_only)
    return (new TextBrowser(content))->get_gui_num();
  else
    return (new TextEdit(content))->get_gui_num();
}

int64_t gui_ratio(dyn_t ratio, bool wheelMainlyChangesNumerator, bool wheelDecrasesDenominatorIfNumeratorIsOne){
  if (!DYN_is_liberal_ratio(ratio)){
    handleError("gui_ratio: Expected a number or string as 'ratio' argument. Found %s", DYN_type_name(ratio.type));
    return -1;
  }

  return (new MyRatioSnifferQLineEdit(NULL, DYN_get_static_ratio(ratio), wheelMainlyChangesNumerator, wheelDecrasesDenominatorIfNumeratorIsOne))->get_gui_num();
}

int64_t gui_line(const_char* content, const_char* textcolor){
  //return -1;
  return (new Line(content, textcolor))->get_gui_num();
}

int64_t gui_intText(int min, int curr, int max){
  //return -1;
  return (new IntText(min, curr, max))->get_gui_num();
}

int64_t gui_floatText(double min, double curr, double max, int num_decimals, double step_interval){
  //return -1;
  return (new FloatText(min, curr, max, num_decimals, step_interval))->get_gui_num();
}

int64_t gui_web(const_char* stringurl){
  return (new Web(stringurl))->get_gui_num();
}

void gui_setUrl(int64_t guinum, const_char* url){
  Gui *web_gui = get_gui(guinum);
  if (web_gui==NULL)
    return;

  QWebView *web = web_gui->mycast<QWebView>(__FUNCTION__);
  
  if (web != NULL){
    //web->setWindowTitle(url);
    web->setUrl(getUrl(url));
  }
}

void openExternalWebBrowser(const_char *stringurl){
  QDesktopServices::openUrl(getUrl(stringurl));
}

int64_t gui_fileRequester(const_char* header_text, const_char* dir, const_char* filetypename, const_char* postfixes, bool for_loading){
  return (new FileRequester(header_text, dir, filetypename, postfixes, for_loading))->get_gui_num();
}

int64_t gui_fontRequester(const_char* fontdescr){
  QFont font;
  font.fromString(fontdescr);
  return (new FontRequester(font))->get_gui_num();
}

int64_t gui_bottomBar(bool include_editor_elements, bool include_navigator){
  QWidget *bottom_bar = BottomBar_create(NULL, include_editor_elements, include_navigator);
  
  return API_get_gui_from_existing_widget(bottom_bar);
}


/************* Tabs ***************************/

int64_t gui_tabs(int tab_pos){
  return (new Tabs(tab_pos))->get_gui_num();
}

int64_t gui_getTabBar(int64_t tabs_guinum){
  Gui *tabs_gui = get_gui(tabs_guinum);
  if (tabs_gui==NULL)
    return -1;

  QTabWidget *tabs = tabs_gui->mycast<QTabWidget>(__FUNCTION__);
  if (tabs==NULL)
    return -1;

  TabBar *tab_bar = dynamic_cast<TabBar*>(tabs->tabBar());
  if (tab_bar != NULL)
    return tab_bar->get_gui_num();

  printf("Warning. tabBar of %d is not a TabBar instance. Can not override paint and mouse methods, etc.\n", (int)tabs_guinum);
         
  return API_get_gui_from_widget(tabs->tabBar()); 
}

void gui_removeTab(int64_t tabs_guinum, int pos){
  Gui *tabs_gui = get_gui(tabs_guinum);
  if (tabs_gui==NULL)
    return;

  QTabWidget *tabs = tabs_gui->mycast<QTabWidget>(__FUNCTION__);
  if (tabs==NULL)
    return;

  tabs->removeTab(pos);
}

int gui_addTab(int64_t tabs_guinum, const_char* name, int64_t tab_guinum, int pos){ // if pos==-1, tab is append. (same as if pos==num_tabs)
  Gui *tabs_gui = get_gui(tabs_guinum);
  if (tabs_gui==NULL)
    return -1;

  Gui *tab_gui = get_gui(tab_guinum);
  if (tab_gui==NULL)
    return -1;

  QTabWidget *tabs = tabs_gui->mycast<QTabWidget>(__FUNCTION__);
  if (tabs==NULL)
    return -1;
  
  int num = tabs->insertTab(pos, tab_gui->_widget, name);
  
  return num;
}

int gui_currentTab(int64_t tabs_guinum){
  Gui *tabs_gui = get_gui(tabs_guinum);
  if (tabs_gui==NULL)
    return -1;

  QTabWidget *tabs = tabs_gui->mycast<QTabWidget>(__FUNCTION__);
  if (tabs==NULL)
    return -1;

  return tabs->currentIndex();
}

void gui_setCurrentTab(int64_t tabs_guinum, int pos){
  Gui *tabs_gui = get_gui(tabs_guinum);
  if (tabs_gui==NULL)
    return;

  QTabWidget *tabs = tabs_gui->mycast<QTabWidget>(__FUNCTION__);
  if (tabs==NULL)
    return;

  return tabs->setCurrentIndex(pos);
}

int gui_getTabPos(int64_t tabs_guinum, int64_t tab_guinum){
  Gui *tabs_gui = get_gui(tabs_guinum);
  if (tabs_gui==NULL)
    return -1;

  Gui *tab_gui = get_gui(tab_guinum);
  if (tab_gui==NULL)
    return -1;

  QTabWidget *tabs = tabs_gui->mycast<QTabWidget>(__FUNCTION__);
  if (tabs==NULL)
    return -1;

  int ret = tabs->indexOf(tab_gui->_widget);
  if (ret<0)
    return ret-1;
  else
    return ret;
}

int gui_numTabs(int64_t tabs_guinum){
  Gui *tabs_gui = get_gui(tabs_guinum);
  if (tabs_gui==NULL)
    return -1;

  QTabWidget *tabs = tabs_gui->mycast<QTabWidget>(__FUNCTION__);
  if (tabs==NULL)
    return -1;

  return tabs->count();
}

const_char* gui_tabName(int64_t tabs_guinum, int pos){
  Gui *tabs_gui = get_gui(tabs_guinum);
  if (tabs_gui==NULL)
    return "";

  QTabWidget *tabs = tabs_gui->mycast<QTabWidget>(__FUNCTION__);
  if (tabs==NULL)
    return "";

  if (pos==-1)
    pos = tabs->currentIndex();

  if (pos < 0 || pos >= tabs->count()){
    handleError("No tab %d in tabs #%d", pos, (int)tabs_guinum);
    return "";
  }
  
  return talloc_strdup(tabs->tabText(pos).toUtf8().constData());
}





/************* Splitter ***************************/

int64_t gui_verticalSplitter(bool childrenCollappsible){
  return (new Splitter(false, childrenCollappsible))->get_gui_num();
}

int64_t gui_horizontalSplitter(bool childrenCollappsible){
  return (new Splitter(true, childrenCollappsible))->get_gui_num();
}

int64_t gui_getSplitterHandle(int64_t splitter_guinum, int pos){
  Gui *gui = get_gui(splitter_guinum);
  if (gui==NULL)
    return -1;

  QSplitter *splitter = dynamic_cast<QSplitter*>(gui->_widget.data());
  if (splitter==NULL){
    handleError("gui_getSplitterHandle: Gui is not a splitter");
    return -1;
  }

  if (pos < 0 || pos >= splitter->count()){
    handleError("No splitter handle %d in splitter #%d", pos, (int)splitter_guinum);
    return -1;
  }

  auto *handle = splitter->handle(pos);
  if (handle==NULL){
    handleError("Qt returned NULL when asking for splitter handle %d in splitter #%d. (that's very strange)", pos, (int)splitter_guinum);
    return -1;
  }
  
  return API_get_gui_from_widget(handle);
}

dyn_t gui_getSplitterSizes(int64_t splitter_guinum){
  dynvec_t ret = {};
  
  const Gui *gui = get_gui(splitter_guinum);
  if (gui==NULL)
    goto exit;

  {
    const QSplitter *splitter = dynamic_cast<QSplitter*>(gui->_widget.data());
    if (splitter==NULL){
      handleError("gui_getSplitterSizes: Gui is not a splitter");
      goto exit;
    }
    
    {
      const auto &sizes = splitter->sizes();
      for(int size : sizes){
        DYNVEC_push_back(&ret, DYN_create_int(size));
      }
    }
  }

 exit:
  return DYN_create_array(ret);
}

void gui_setSplitterSizes(int64_t splitter_guinum, dyn_t splitter_sizes){
  Gui *gui = get_gui(splitter_guinum);
  if (gui==NULL)
    return;

  QSplitter *splitter = dynamic_cast<QSplitter*>(gui->_widget.data());
  if (splitter==NULL){
    handleError("gui_setSplitterSizes: Gui is not a splitter");
    return;
  }
  
  if (splitter_sizes.type != ARRAY_TYPE){
    handleError("gui_table: Argument 'splitter_sizes' must be a list or a vector, found %s", DYN_type_name(splitter_sizes.type));
    return;
  }

  const dynvec_t *array = splitter_sizes.array;
  
  if (array->num_elements != splitter->sizes().size()){
    handleError("gui_setSplitterSizes: Expected %d elements in splitter_sizes, found %d", splitter->sizes().size(), array->num_elements);
    return;
  }
  
  QList<int> sizes;

  int num = 0;
  for(const dyn_t &el : array){
    
    if (DYN_is_number(el)==false){  
      handleError("gui_setSplitterSizes: Expected a number for splitter_sizes[%d], found %s", num, DYN_type_name(el.type));
      return;
    }

    int64_t size = DYN_get_int64_from_number(el);

    if(size < 0 || size > 100000){
      handleError("gui_setSplitterSizes: Invalid number for splitter_sizes[%d]: %d", num, (int)size);
      return;      
    }

    sizes.push_back((int)size);
    num++;
  }

  splitter->setSizes(sizes);
}


/*
int64_t gui_rubberBand(float opacity){
  return (new RubberBand(opacity))->get_gui_num();
}
*/

/************** Table **********************/

int64_t gui_table(dyn_t header_names){
  if (header_names.type != ARRAY_TYPE){
    handleError("gui_table: Argument must be a list or vector of strings, found %s", DYN_type_name(header_names.type));
    return -1;
  }

  QStringList headers;

  for(int i=0;i<header_names.array->num_elements;i++){
    dyn_t el = header_names.array->elements[i];
    if(el.type != STRING_TYPE){
      handleError("gui_table: Element %d in header_names is not a string: %s", i, DYN_type_name(el.type));
      return -1;
    }
    
    headers << STRING_get_qstring(el.string);
  }

  return (new Table(headers))->get_gui_num();
}

namespace{

  // these classes are used to make sorting table rows work.
  
  struct Pri{
    QString text;
    double _pri;
    Pri(double pri)
      :_pri(pri)
    {}
  };
  
  struct MyNumItem : public QTableWidgetItem, public Pri{
    MyNumItem(double num, bool is_int)
      : QTableWidgetItem(is_int ? QString::number(int(num)) : QString::number(num))
      , Pri(num)
    {}
    bool operator<(const QTableWidgetItem &other) const override{
      const Pri *myother = dynamic_cast<const Pri*>(&other);
      if (myother==NULL)
        return false;
      else
        return _pri < myother->_pri;
    }
  };
  struct MyStringItem : public QTableWidgetItem, public Pri{
    QString _name;
    MyStringItem(QString name)
      : QTableWidgetItem(name)
      , Pri(DBL_MIN)
      , _name(name)
    {}
    bool operator<(const QTableWidgetItem &other) const override{
      const MyStringItem *myother = dynamic_cast<const MyStringItem*>(&other);
      if (myother==NULL){
        const Pri *mypriother = dynamic_cast<const Pri*>(&other);
        if (mypriother!=NULL)
          return _pri < mypriother->_pri; //false; //_name < other.text(); //QTableWidgetItem::operator<(other);
        else
          return true;
      }else if (myother->_name=="")
        return true;
      else if (_name=="")
        return false;
      else
        return _name < myother->_name;
    }
  };

  // If w contains a layout, return the first widget (if any) in the layout that does not have a layout. Returns NULL if no such widget is found.
  // Note: Depth first search.
  static QWidget *get_first_widget_in_widget(QWidget *w){
    if (w->layout()==NULL)
      return w;

    for(auto *c : w->children()){
      QWidget *maybe = dynamic_cast<QWidget*>(c);
      if (maybe != NULL){
        maybe = get_first_widget_in_widget(maybe);
        if (maybe != NULL)
          return maybe;
      }
    }

    return NULL;
  }

  struct MyGuiItem : public QTableWidgetItem, public Pri{
    int64_t _guinum;
    QPointer<QWidget> _widget;
    
    MyGuiItem(int64_t guinum)
      : Pri(DBL_MAX)
      , _guinum(guinum)
    {
      Gui *cell_gui = get_gui(guinum);
      R_ASSERT_RETURN_IF_FALSE(cell_gui!=NULL);  
      _widget = cell_gui->_widget.data();
    }
    
    bool operator<(const QTableWidgetItem &other) const override{
      bool fallback = true;
      
      {
        const Pri *other_pri = dynamic_cast<const Pri*>(&other);
        if (other_pri!=NULL)
          fallback = _pri < other_pri->_pri; //false; //_name < other.text(); //QTableWidgetItem::operator<(other);
      }
      
      const MyGuiItem *other_gui_item = dynamic_cast<const MyGuiItem*>(&other);
      
      if (other_gui_item != NULL){

        fallback = _guinum < other_gui_item->_guinum;
        
        const QWidget *w1 = get_first_widget_in_widget(_widget);
        if (w1==NULL) w1 = _widget;

        const QWidget *w2 = get_first_widget_in_widget(other_gui_item->_widget);
        if (w2==NULL) w2 = other_gui_item->_widget;

        // line edits
        {
          const QLineEdit *b1 = dynamic_cast<const QLineEdit*>(w1);
          const QLineEdit *b2 = dynamic_cast<const QLineEdit*>(w2);
          
          if (b1!=NULL && b2!=NULL) {
            
            if (b1->text()==b2->text())
              return fallback;
            
            return b1->text().compare(b2->text(), Qt::CaseInsensitive) < 0;
          }
        }

        // Buttons
        {
          const QAbstractButton *b1 = dynamic_cast<const QAbstractButton*>(w1);
          const QAbstractButton *b2 = dynamic_cast<const QAbstractButton*>(w2);
          
          if (b1!=NULL && b2!=NULL) {
            
            if (b1->isCheckable() && b2->isCheckable()){
              
              if (b1->isChecked()==b2->isChecked())
                return fallback;
              
              return !b1->isChecked();          
            }
            
            if (b1->text()==b2->text())
              return fallback;
            
            return b1->text().compare(b2->text(), Qt::CaseInsensitive) < 0;
          }
        }

        // Spinboxes
        {
          const QAbstractSpinBox *b1 = dynamic_cast<const QAbstractSpinBox*>(w1);
          const QAbstractSpinBox *b2 = dynamic_cast<const QAbstractSpinBox*>(w2);
          
          if (b1!=NULL && b2!=NULL) {
            
            double val1 = STRING_get_double(STRING_create(b1->text()));
            double val2 = STRING_get_double(STRING_create(b2->text()));

            if (val1==val2)
              return fallback;
            else
              return val1 < val2;
          }
        }
      }
      
      return fallback;
    }
  };
}

// Same as mid-horizontal-layout in gui.scm
static QWidget *create_mid_widget(Gui *gui){//QWidget *content_widget){
  //printf("HORPOL: %d\n", (int)gui->_widget->sizePolicy().horizontalPolicy());
  if (gui->_widget->sizePolicy().horizontalPolicy() != QSizePolicy::Fixed){
    return gui->_widget; // is stretching.
  }
      
  int64_t content = gui->get_gui_num(); //API_get_gui_from_widget(content_widget);
  
  int64_t layout = gui_horizontalLayout();
  gui_addLayoutSpace(layout, 1, 1, true, false);
  gui_add(layout, content, -1, -1, -1, -1);
  gui_addLayoutSpace(layout, 1, 1, true, false);

  return API_gui_get_widget(layout);
}

static int64_t add_table_cell(int64_t table_guinum, Gui *cell_gui, QTableWidgetItem *item, int x, int y, bool enabled){
  Gui *table_gui = get_gui(table_guinum);
  if (table_gui==NULL)
    return -1;

  
  QTableWidget *table = table_gui->mycast<QTableWidget>("gui_addTableCell");
  if (table==NULL)
    return -1;
  
  if (enabled)
    item->setFlags(Qt::ItemIsSelectable|Qt::ItemIsEnabled);
  else
    item->setFlags(Qt::NoItemFlags);
  
  if (y >= table->rowCount())
    table->setRowCount(y+1);

  table->setItem(y, x, item);

  if (cell_gui==NULL){
    
    if (table->cellWidget(y,x) != NULL)
      table->removeCellWidget(y,x); // is the cell widget deleted now? (yes)
    
    return -1;
  }

  // Workaround for Qt bug.
  g_guis_can_not_be_closed[cell_gui->get_gui_num()] = "Gui is placed inside a Table. Qt seems to crash with if you close a QTableWidget cell widget manually. However, table cells are deleted automatically when being replaced, or a row is deleted, or a table is cleared, so it's probably never necessary to close them manually.";

  //table->setCellWidget(y, x, API_gui_get_widget(S7CALL2(int_int, "mid-horizontal-layout", cell_gui->get_gui_num()))); // "
  table->setCellWidget(y, x, create_mid_widget(cell_gui));
  
  return cell_gui->get_gui_num();
}
                              
int64_t gui_addTableGuiCell(int64_t table_guinum, int64_t cell_gui_num, int x, int y, bool enabled){
  Gui *cell_gui = get_gui(cell_gui_num);
  if (cell_gui==NULL)
    return -1;

  auto *item = new MyGuiItem(cell_gui_num);
  
  return add_table_cell(table_guinum, cell_gui, item, x, y, enabled);
}

int64_t gui_addTableStringCell(int64_t table_guinum, const_char* string, int x, int y, bool enabled){
  QString name(string);
  auto *item = new MyStringItem(name);
    
  //Gui *cell_gui = new Text(name, "");

  return add_table_cell(table_guinum, NULL, item, x, y, enabled);
}

int64_t gui_addTableIntCell(int64_t table_guinum, int64_t num, int x, int y, bool enabled){
  QString name = QString::number(num);
  auto *item = new MyNumItem(num, true);
  item->setFlags(Qt::ItemIsSelectable|Qt::ItemIsEnabled);
    
  //Gui *cell_gui = new Text(name, "");

  return add_table_cell(table_guinum, NULL, item, x, y, enabled);
}

int64_t gui_addTableFloatCell(int64_t table_guinum, double num, int x, int y, bool enabled){
  QString name = QString::number(num);
  auto *item = new MyNumItem(num, false);
  item->setFlags(Qt::ItemIsSelectable|Qt::ItemIsEnabled);
    
  //Gui *cell_gui = new Text(name, "");

  return add_table_cell(table_guinum, NULL, item, x, y, enabled);
}

int gui_getTableRowNum(int64_t table_guinum, int64_t cell_guinum){
  Gui *table_gui = get_gui(table_guinum);
  if (table_gui==NULL)
    return -1;

  Gui *cell_gui = get_gui(cell_guinum);
  if (cell_gui==NULL)
    return -1;

  QTableWidget *table = table_gui->mycast<QTableWidget>(__FUNCTION__);
  if (table==NULL)
    return -1;

  for(int x=0;x<table->columnCount();x++){
    for(int y=0;y<table->rowCount();y++){
      if(table->cellWidget(y,x)==cell_gui->_widget)
        return y;
    }
  }

  return -1;
}


void gui_addTableRows(int64_t table_guinum, int pos, int how_many){
  Gui *table_gui = get_gui(table_guinum);
  if (table_gui==NULL)
    return;

  QTableWidget *table = table_gui->mycast<QTableWidget>(__FUNCTION__);
  if (table==NULL)
    return;

  int num_rows = table->rowCount();

  if (pos < -num_rows){
    handleError("gui_addTableRows: pos (%d) too large. There's only %d rows in this table", pos, table->rowCount());
    pos = -num_rows;
  }
  
  if(pos==num_rows && how_many > 0){
    table->setRowCount(num_rows+how_many); // Optimization. Calling table->insertRow() several times in a row sometimes stalls Qt for up to a second (224 rows). Calling setRowCount doesn't, for some reason.
    
  }else if(how_many<0 && pos>=num_rows+how_many){
    if (pos>num_rows+how_many)
      handleError("gui_addTableRows: Illegal 'pos' argument for Gui #%d. pos: %d, how_many: %d, number of rows: %d. Last legal position: %d",
                  (int)table_guinum, pos, how_many, num_rows, num_rows+how_many);
    table->setRowCount(num_rows+how_many);
    
  }else if (how_many > 0){
    for(int i=0;i<how_many;i++)
      table->insertRow(pos);

    /*
    // This case is covered in the second if above.
  }else if (pos==0 && how_many==-table->rowCount()){
    printf("   CLEARNGING\n");
    table->clearContents();
    table->setRowCount(0);
    */
    
  }else
    for(int i=0;i<-how_many;i++)
      table->removeRow(pos);

  R_ASSERT(table->rowCount() == num_rows+how_many);
}

int gui_getNumTableRows(int64_t table_guinum){
  Gui *table_gui = get_gui(table_guinum);
  if (table_gui==NULL)
    return 0;

  QTableWidget *table = table_gui->mycast<QTableWidget>(__FUNCTION__);
  if (table==NULL)
    return 0;

  return table->rowCount();
}

int gui_currTableRow(int64_t table_guinum){
  Gui *table_gui = get_gui(table_guinum);
  if (table_gui==NULL)
    return 0;

  QTableWidget *table = table_gui->mycast<QTableWidget>(__FUNCTION__);
  if (table==NULL)
    return 0;

  if (table->rowCount()==0){
    handleError("gui_currTableRow: There are no rows in this table");
    return 0;
  }

  return table->currentRow();
}


void gui_enableTableSorting(int64_t table_guinum, bool do_sort){
  Gui *table_gui = get_gui(table_guinum);
  if (table_gui==NULL)
    return;


  QTableWidget *table = table_gui->mycast<QTableWidget>(__FUNCTION__);
  if (table==NULL)
    return;

  table->setSortingEnabled(do_sort);
  
  /*
  if (do_sort)
    table->_sorting_disabled--;
  else
    table->_sorting_disabled++;

  if (table->_sorting_disabled==0)
    table->setSortingEnabled(true);
  if (table->_sorting_disabled==1)
    table->setSortingEnabled(false);
  */
}

void gui_sortTableBy(int64_t table_guinum, int x, bool sort_ascending){
  Gui *table_gui = get_gui(table_guinum);
  if (table_gui==NULL)
    return;

  QTableWidget *table = table_gui->mycast<QTableWidget>(__FUNCTION__);
  if (table==NULL)
    return;

  if (table->columnCount()==0){
    handleError("gui_addTableRows: There are no columns in this table");
    return;
  }

  if (x < 0){
    handleError("gui_addTableRows: x is negative");
    return;
  }
  
  if (x>table->columnCount()){
    handleError("gui_addTableRows: x (%d) too large. There's only %d columns in this table", x, table->columnCount());
    return;
  }

  table->sortItems(x, sort_ascending ? Qt::AscendingOrder : Qt::DescendingOrder);
}

void gui_stretchTable(int64_t table_guinum, int x, bool do_stretch, int size){
  Gui *table_gui = get_gui(table_guinum);
  if (table_gui==NULL)
    return;

  QTableWidget *table = table_gui->mycast<QTableWidget>(__FUNCTION__);
  if (table==NULL)
    return;

  if (table->columnCount()==0){
    handleError("gui_addTableRows: There are no columns in this table");
    return;
  }

  if (x < 0){
    handleError("gui_addTableRows: x is negative");
    return;
  }
  
  if (x>table->columnCount()){
    handleError("gui_addTableRows: x (%d) too large. There's only %d columns in this table", x, table->columnCount());
    return;
  }

  if (do_stretch)
    table->horizontalHeader()->setSectionResizeMode(x, QHeaderView::Stretch);
  
  table->horizontalHeader()->resizeSection(x, size);
}


////////////////////////////
// various operations on guis


// Returns Qt classname. Should not be used for dispatching. Probably only useful for debugging.
const_char* gui_className(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return "(not found)";

  return talloc_strdup(gui->_class_name.toUtf8().constData()); //widget->metaObject()->className(); // ->metaObject()->className() is always supposed to work though.
}

void gui_setWindowTitle(int64_t guinum, const_char *value){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->_widget->setWindowTitle(value);
}

void gui_setToolTip(int64_t guinum, const_char *value){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->_widget->setToolTip(value);
}
  
void gui_setText(int64_t guinum, const_char *value){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->setGuiText(value);
}

void gui_setValue(int64_t guinum, dyn_t value){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->setGuiValue(value);
}

void gui_appendValue(int64_t guinum, dyn_t value){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;
  
  gui->appendGuiValue(value);
}

dyn_t gui_getValue(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return DYN_create_bool(false);

  return gui->getGuiValue();
}

void gui_add(int64_t parentnum, int64_t childnum, int x1_or_stretch, int y1, int x2, int y2){
  Gui *parent = get_gui(parentnum);
  if (parent==NULL)
    return;

  Gui *child = get_gui(childnum);  
  if (child==NULL)
    return;

  //printf("Child parent: %p\n", child->_widget->parent());
#if !defined(RELEASE)
  if(child->_widget->parent()!=NULL){
    //printf("Error: gui_add: child->_widget->parent()!=NULL\n");
    handleError("gui_add: child->_widget->parent()!=NULL");
    return;
  }
#endif
  
  radium::Splitter *splitter = dynamic_cast<radium::Splitter*>(parent);
  
  QLayout *layout = parent->getLayout();

  // Automatically add layout to widget, if necessary.
  if (splitter==NULL && layout==NULL && y1==-1){
    layout = new QHBoxLayout;
    setDefaultSpacing(layout);
    parent->_widget->setLayout(layout);
  }
  
  if(splitter!=NULL || layout==NULL || y1!=-1) {

    int x1 = x1_or_stretch;

    if (x1<0)
      x1 = 0;
    if (y1<0)
      y1 = 0;

    if (splitter != NULL) {

      splitter->addWidget(child->_widget);
      
      //int stretch = x1_or_stretch == -1 ? 0 : x1_or_stretch;
      //splitter->setStretchFactor(splitter->count()-1, stretch);
      splitter->setSizes({900000,1}); // Hack
      
    } else {
    
      ScrollArea *scroll_area = dynamic_cast<ScrollArea*>(parent); // Think this one should be changed to parent->_widget.
      if (scroll_area != NULL){
        //printf("      Adding to scroll child\n");
        child->_widget->setParent(scroll_area->contents);
        child->_widget->move(x1,y1);
        if (parent->_widget->isVisible())
          child->_widget->show();
        
        int new_width = R_MAX(parent->_widget->width(), x2);
        int new_height = R_MAX(parent->_widget->height(), y2);
        
        //parent->_widget->resize(new_width, new_height);
        scroll_area->contents->resize(new_width, new_height);
        
      }else{

        QWidget *child_window =  child->_widget->window();
        QWidget *parent_window = parent->_widget->window();

        if (child_window!=NULL && parent_window!=NULL){
          if (child_window==parent_window){
            handleError("gui_add: Will not set gui #%d as a child of gui #%d since they both belong to the same window. (Qt often freezes if trying to do that plus that it's likely that there is a bug somewhere since this call was made. If this is not a bug, as a workaround, you need to remove the old parent of the child before calling gui_add. And if the child is already a window, then it's definitely a bug since it means that parent is a child of the child.). %p %p", (int)child->get_gui_num(), (int)parent->get_gui_num(), child->_widget->parent(), parent->_widget->parent());
            return;
          }
        }
        
        child->_widget->setParent(parent->_widget);
        child->_widget->move(x1,y1);
        if (parent->_widget->isVisible())
          child->_widget->show();
      }

      if (x2>x1 && y2 > y1)
        child->_widget->resize(x2-x1, y2-y1);

      
      /*
        int new_width = R_MAX(parent->_widget->width(), x2);
        int new_height = R_MAX(parent->_widget->height(), y2);
        
        parent->_widget->resize(new_width, new_height);
      */
    }

  } else {

    QBoxLayout *box_layout = dynamic_cast<QBoxLayout*>(layout);
    
    if (box_layout!=NULL){

      int stretch = x1_or_stretch == -1 ? 0 : x1_or_stretch;

      box_layout->addWidget(child->_widget, stretch);

    } else {

      if (x1_or_stretch != -1)
        handleError("Warning: Parent gui #%d does not have a box layout", (int)parentnum);

      layout->addWidget(child->_widget);

    }
    
  }

  /*
    // A failed attempt to minimize the qt flicker bug.

  parent->_widget->adjustSize();
  parent->_widget->updateGeometry();
  parent->_widget->adjustSize();
  parent->_widget->updateGeometry();
  
  child->_widget->adjustSize();
  child->_widget->updateGeometry();
  child->_widget->adjustSize();
  child->_widget->updateGeometry();

  parent->_widget->adjustSize();
  parent->_widget->updateGeometry();
  */
}

void gui_replace(int64_t parentnum, int64_t oldchildnum, int64_t newchildnum){
  Gui *parent = get_gui(parentnum);
  if (parent==NULL)
    return;

  Gui *oldchild = get_gui(oldchildnum);
  if (oldchild==NULL)
    return;

  Gui *newchild = get_gui(newchildnum);  
  if (newchild==NULL)
    return;

  if (g_static_toplevel_widgets.contains(newchild->_widget)){
    handleError("gui_replace: Trying to put a permanent widget into a layout. This is probably a bug.");
    return;
  }

  // Check that parent is not a child of newchild
  {
    QObject *parentparent = parent->_widget->parent();
    while(parentparent != NULL){
      if (parentparent==newchild->_widget){
        handleError("gui_replace: Can not replace #%d with #%d in #%d since #%d is a parent of #%d", (int)oldchildnum, (int)newchildnum, (int)parentnum, (int)newchildnum, (int)parentnum);
        return;
      }
      parentparent = parentparent->parent();
    }
  }
  
  QLayout *layout = parent->getLayout();
  if(layout==NULL){
    handleError("gui_replace: Gui #%d does not have a layout", (int)parentnum);
    return;
  }

  // Trying to see if this prevents Qt from sometimes freezing. (Stuck in QWidgetPrivate::invalidateGraphicsEffectsRecursively())
  // (don't remove, it seems to work.)
  if (newchild->_widget->parent() != NULL)
    newchild->_widget->setParent(NULL);
  
  QLayoutItem *old_item = layout->replaceWidget(oldchild->_widget, newchild->_widget, Qt::FindDirectChildrenOnly);

  if(old_item==NULL){
    handleError("gui_replace: Gui #%d not found in #%d", (int)oldchildnum, (int)parentnum);
    return;
  }

  delete old_item;
}

bool gui_isVisible(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return false;

  return gui->_widget->isVisible();
}

void gui_addVisibilityChangeCallback(int64_t guinum, func_t* func){
  Gui *gui = get_gui(guinum);

  if (gui==NULL)
    return;

  gui->addVisiblityChangeCallback(func);
}



void gui_show(int64_t guinum){
  if (g_qt_is_painting){
    handleError("Can not call gui_show from a paint callback");
    return;
  }
  
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  QWidget *w = gui->_widget;

  if (dynamic_cast<Popup*>(w) != NULL) {
    printf("   Showing POPUP\n");

    w->adjustSize();
    w->updateGeometry();

    QScreen *screen = QGuiApplication::primaryScreen();
    auto pos = QCursor::pos(screen);
  
    int swidth = screen->size().width();
    int sheight = screen->size().height();
    
    if (pos.x() >= 0){
      if (pos.x() + w->width() > swidth)
        pos.setX(R_MAX(0, swidth - w->width()));
    } else {
      pos.setX(0);
    }
                   
    if (pos.y() >= 0){
      if (pos.y() + w->height() > sheight)
        pos.setY(R_MAX(0, sheight - w->height()));
    } else {
      pos.setY(0);
    }
    
    w->move(pos);
    safeShowPopup(w);

    return;
  }

  
  if (!gui->size_is_valid()){
    //printf("ADJUSTING SIZE AND GEOMETRY\n");
    w->adjustSize();
    w->updateGeometry();
  }
  
  gui->_has_been_opened_before = true;
  
  //pauseUpdates(w);

  //if (w->isWindow())
  safeShow(w);

  /*
  if (w->isWindow()){
    w->setFocusPolicy(Qt::StrongFocus);
    w->setFocus();
  }
  */
}

void gui_hide(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->_widget->hide();
}


void gui_close(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  /*
  // Removed this limitation. It makes sense to do this when removing a child widget from a ui.
  //
  if (gui->_created_from_existing_widget){ //g_gui_from_existing_widgets.contains(gui->_widget)){
    handleError("Can not close Gui #%d since it was not created via the API");
    return;
  }
  */
  
  if (g_static_toplevel_widgets.contains(gui->_widget)){
    handleError("Can not close Gui #%d since it is marked as a static toplevel widget", (int)guinum);
    return;
  }

  const char *can_not_be_closed_reason = g_guis_can_not_be_closed.value(guinum);
  if (can_not_be_closed_reason != NULL){
    handleError("Gui #%d can not be closed.\nReason: %s", (int)guinum, can_not_be_closed_reason);
    return;
  }

  if (gui->_has_been_closed){
    handleError("Gui #%d has already been closed", (int)guinum);
    return;
  }
  
  gui->_widget->close();
}

bool gui_isOpen(int64_t guinum){
  Gui *gui = get_gui_maybeclosed(guinum);

  if (gui==NULL || gui->_widget==NULL)
    return false;
  else
    return true;
}

int64_t gui_getParentWindow(int64_t guinum){
  if (guinum < 0)
    return guinum;
  
  QWidget *w = API_gui_get_parentwidget(NULL, guinum);

  if (w==NULL)
    return -3;

  return API_get_gui_from_widget(w);
}

int64_t gui_getParentGui(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return false;

  QWidget *w = gui->_widget->parentWidget();

  if (w == NULL){
    handleError("Gui #%d has no parent gui\n", (int)guinum);
    return -1;
  }

  return API_get_gui_from_widget(w);
}

static bool gui_setParent2(int64_t guinum, int64_t parentgui, bool mustBeWindow){
  //return false;
  
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return false;

  if (mustBeWindow){
    bool is_window = gui->_widget->isWindow() || gui->_widget->parent()==NULL || gui->is_full_screen();
    if(!is_window){
      handleError("gui_setParent: Gui #%d is not a window. (className: %s)", (int)guinum, gui_className(guinum));
      return false;
    }
  }
  
  QWidget *parent = API_gui_get_parentwidget(gui->_widget, parentgui);
  printf("**parent is main_window: %d\n", parent==g_main_window);
  
  if (parent==gui->_widget){
    printf("Returned same as me\n");
    return false;
  }
  
  //  else if (gui->is_full_screen())
  //   gui->_full_screen_parent->setNewParent(parent);

  else if (gui->_widget->parent() == parent)
    return false;
  
  else {
    bool isvisible = gui->_widget->isVisible();
    if (isvisible)
      gui->_widget->hide();

    {

      // sanity checks
      
      QWidget *child_window =  gui->_widget->window();
      QWidget *parent_window = parent==NULL ? NULL : parent->window();
      
      if (child_window!=NULL && parent_window!=NULL){
        if (child_window==parent_window){
          handleError("gui_setParent2: Will not set gui #%d as a child of gui #%d (parentgui parameter: %d) since they both belong to the same window. (Qt often freezes if trying to do that plus that it's likely that there is a bug somewhere since this call was made. If this is not a bug, you can, as a workaround, remove the old parent of the child before calling gui_add. If the child is already a window, then it's definitely a bug since it means that parent is a child of the child.)", (int)gui->get_gui_num(), (int)API_get_gui_from_widget(parent), (int)parentgui);
          return false;
        }
      }
    }

    if (g_static_toplevel_widgets.contains(gui->_widget)){
      handleError("gui_setParent2: Trying to set parent of a permanent widget. This is probably a bug.");
      return false;
    }
      
    // Trying to see if this prevents Qt from sometimes freezing the program when calling QWidget::setParent() in Qt/helpers.h. (Stuck in QWidgetPrivate::invalidateGraphicsEffectsRecursively())
    // (don't remove, it seems to work.)
    if (gui->_widget->parent() != NULL)
      gui->_widget->setParent(NULL);

    set_window_parent(gui->_widget, parent, gui->_modality);

    if (isvisible)
      gui->_widget->show();
  }
    

  return true;
}

bool gui_setAsWindow(int64_t guinum, int64_t parentgui){
  return gui_setParent2(guinum, parentgui, false);
}

bool gui_setParent(int64_t guinum, int64_t parentgui){
  return gui_setParent2(guinum, parentgui, true);
}

bool gui_removeParent(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return false;

  if (gui->_widget->parent()==NULL)
    return false;

  gui->_widget->setParent(NULL);
  return true;
}

void gui_setModal(int64_t guinum, bool set_modal){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  bool was_modal = gui->_widget->isModal();

  gui->_modality = set_modal ? radium::IS_MODAL : radium::NOT_MODAL;
  gui->_widget->setWindowModality(set_modal ? Qt::ApplicationModal : Qt::NonModal);

  if(set_modal && !was_modal)
    register_modal_qwidget(gui->_widget);
}

bool gui_isModal(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return false;

  gui->update_modality_attribute();
  
  if (gui->_modality==radium::NOT_MODAL)
    return false;
  else if (gui->_modality==radium::MAY_BE_MODAL)  // MAY_BE_MODAL is just a hack to make sure grandchild windows stays on top of child windows. (will be removed when moving over to MDI)
    return false;
  else if (gui->_modality==radium::IS_MODAL)
    return true;
  else
    R_ASSERT_NON_RELEASE(false);

  return false;
}

void gui_disableUpdates(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->_widget->setUpdatesEnabled(false);
}

void gui_enableUpdates(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->_widget->setUpdatesEnabled(true);
}

int gui_width(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return 0;

  if (!gui->size_is_valid())
    gui->_widget->adjustSize();

  return gui->_widget->width();
}

int gui_height(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return 0;

  if (!gui->size_is_valid())
    gui->_widget->adjustSize();

  QScrollArea *area = dynamic_cast<QScrollArea*>(gui->_widget.data());
  if (area!=NULL){
    QScrollBar *scrollbar = area->horizontalScrollBar();
    if (scrollbar->isVisible()){
      //printf("  IS VISIBLE %d\n",scrollbar->height());
      return area->widget()->height() - scrollbar->height();
    }else{
      //printf("    NOT NOTNOT IS VISIBLE\n");
      return area->widget()->height();
    }
  }else{
    //printf("   NOT A SCROLL AREA\n");

    return gui->_widget->height();
  }
}

int gui_getX(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return 0;

  return gui->_widget->x();
}

int gui_getY(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return 0;

  return gui->_widget->y();
}

void gui_moveToCentreOf(int64_t guinum, int64_t window_to_move_in_centre_of){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  Gui *of = get_gui(window_to_move_in_centre_of);
  if (of==NULL)
    return;

  QWidget *w = gui->_widget;

  w->updateGeometry();
    
  moveWindowToCentre(w, of->_widget->window()->geometry());
}

void gui_moveToParentCentre(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  QWidget *w = gui->_widget;

  w->updateGeometry();

  moveWindowToCentre(w);
}

void gui_raise(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  QWidget *w = gui->_widget;

  w->raise();
  //w->activateWindow();
}

void gui_activate(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  QWidget *w = gui->_widget;

  w->activateWindow();
}

void gui_setPos(int64_t guinum, int x, int y){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->_widget->move(x,y);
}

void gui_setSize(int64_t guinum, int width, int height){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->_have_set_size = true;

  radium::Splitter *splitter = dynamic_cast<radium::Splitter*>(gui->_widget->parent());
  if(splitter != NULL){
    
    int count = splitter->count();
    int pos = 0;
    
    for(int i=0;i<count;i++)
      if(splitter->widget(i)==gui->_widget.data()){
        pos = i;
        break;
      }

    QList<int> sizes = splitter->sizes();

    int new_height = height;
    int old_height = sizes[pos];
    int diff_height = new_height - old_height;

    /*
    printf("\n\npos: %d, old: %d, new: %d, diff: %d. 0: %d -> %d, 1: %d -> %d\n",
           pos,
           old_height, new_height, diff_height,
           sizes[0], sizes[0] - diff_height/(count-1),
           sizes[1], new_height);
    */
    
    for(int i=0;i<count;i++)
      if (i==pos)
        sizes[i] = new_height;
      else
        sizes[i] -= diff_height/(count-1);
    
    splitter->setSizes(sizes);

    //printf("Actual: %d %d   (req: %d, %d, new_height: %d)\n\n", splitter->sizes()[0], splitter->sizes()[1], sizes[0], sizes[1], new_height);
    
  } else {
    gui->_widget->resize(width, height);
  }
}

bool gui_mousePointsMainlyAt(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return false;

  return gui->_widget->window()==QApplication::topLevelAt(QCursor::pos());
}

void gui_setFullScreen(int64_t guinum, bool enable){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  if (gui->_widget->window()==g_main_window){
    R_ASSERT_NON_RELEASE(false);
    return;
  }
  
  QPointer<QWidget> focusWidget = gui->_widget->focusWidget(); // Seems like focus is always lost when we change parent of the widget

  if(enable){

    if(gui->is_full_screen())
      return;

#if 0    
#if FOR_WINDOWS
    gui->_widget->hide(); // To prevent a warning in Windows about illegal geometry. (must be called before calling the FullScreenParent constructor) (doesn't really work)
#endif
#endif

    fprintf(stderr, "  gui_setFullScreen: Setting FULLSCREEN\n");
    
    // I've spent xx hours trying to show full screen work by calling showFullScreen() directly on the widget, but it's not working very well.
    // Creating a new full screen parent works much better.

    gui->_full_screen_parent = new FullScreenParent(gui->_widget, gui);
    
  }else{

    if(!gui->is_full_screen())
      return;

    fprintf(stderr, "  gui_setFullScreen: Setting BACK fullscreen. NOT fullscreen.\n");
    R_ASSERT_NON_RELEASE(gui->_widget != NULL);
    
    if (gui->_full_screen_parent.data() != NULL && gui->_widget != NULL) {
      gui->_full_screen_parent->resetChildToOriginalState();
    
      fprintf(stderr, "  Hiding full\n\n");
      delete gui->_full_screen_parent;
      
      gui->_widget->show();
      gui->_widget->setFocus();
    }
      
  }

  if (focusWidget.data() != NULL)
    focusWidget->setFocus();
}
  
bool gui_isFullScreen(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return false;

  //return gui->_widget->isFullScreen();
  return gui->is_full_screen();
}

void gui_setTakesKeyboardFocus(int64_t guinum, bool take_it){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  if (gui->_has_keyboard_focus && take_it==false) {
    release_keyboard_focus();
    gui->_has_keyboard_focus = false;
  }
    
  
  gui->_take_keyboard_focus = take_it;
}

bool gui_takesKeyboardFocus(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return false;

  return gui->_take_keyboard_focus;
}

void gui_setBackgroundColor(int64_t guinum, const_char* color){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  QColor c = getQColor(color);

  QWidget *w;
  
  ScrollArea *scroll_area = dynamic_cast<ScrollArea*>(gui); // Think this one should be changed to parent->_widget.
  if (scroll_area != NULL)
    w = scroll_area->contents;
  else
    w = gui->_widget;

  if (gui->_is_pure_qwidget){

    // FIX: There's more gui types than pure QWidget were Background doesn't work. "QLabel" for instance.
    gui->_background_color = c; // Setting Background/Base of a pure QWidget doesn't work. Must paint manually.
    set_widget_takes_care_of_painting_everything(gui->_widget);
    
  } else {
    
    QPalette pal = w->palette();
    pal.setColor(QPalette::Background, c);
    pal.setColor(QPalette::Base, c);
    //gui->_widget->setAutoFillBackground(true);
    w->setPalette(pal);
    
  }
}

const_char* gui_getBackgroundColor(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return "black";

#if DEBUG_COLORS
  return generateNewColor(1.0);
#endif

  return talloc_strdup(gui->_widget->palette().color(gui->_widget->backgroundRole()).name(QColor::HexArgb).toUtf8().constData());
}

const_char* generateNewColor(float mix_background){
  return GFX_get_colorname_from_color(GFX_mix_colors(GFX_MakeRandomColor(),
                                                     GFX_get_color(HIGH_EDITOR_BACKGROUND_COLOR_NUM),
                                                     mix_background)
                                      );
}

const_char* generateNewBlockColor(float mix_background){
  return GFX_get_colorname_from_color(GFX_mix_colors(GFX_MakeRandomBlockColor(),
                                                     GFX_get_color(HIGH_EDITOR_BACKGROUND_COLOR_NUM),
                                                     mix_background)
                                      );
}

const_char* gui_getFont(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return "";

  QPainter *painter = gui->get_painter();

  if (painter != NULL)
    return talloc_strdup(painter->font().toString().toUtf8().constData());

  QFont font = gui->_widget->font();
  QString fontstring = font.toString();

  if(!gui->_cached_fonts.contains(fontstring))
    gui->_cached_fonts[fontstring] = font;

  return talloc_strdup(fontstring.toUtf8().constData());
}

void gui_setFont(int64_t guinum, const_char* char_fontdescr){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  QString fontdescr(char_fontdescr);

  QFont font;

  if (gui->_cached_fonts.contains(fontdescr))
    font = gui->_cached_fonts[fontdescr];
  else{    
    font.fromString(fontdescr);
    gui->_cached_fonts[fontdescr] = font;
  }

  QPainter *painter = gui->get_painter();

  if(painter==NULL) {
    gui->_widget->setFont(font);
    return;
  }

  if(fontdescr==gui->_fontstring)
    return;
  
  gui->_fontstring = fontdescr;
  painter->setFont(font);
}

static inline void setStyleSheetRecursively(QWidget *widget, const_char* stylesheet){
  if (widget != NULL){
    widget->setStyleSheet(stylesheet);
    
    for(auto *c : widget->children())
      setStyleSheetRecursively(dynamic_cast<QWidget*>(c), stylesheet);
  }
}

void gui_setStyleSheetRecursively(int64_t guinum, const_char* stylesheet){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  setStyleSheetRecursively(gui->_widget, stylesheet);
}

void gui_setStyleSheet(int64_t guinum, const_char* stylesheet){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->_widget->setStyleSheet(stylesheet);
}

void gui_setLayoutSpacing(int64_t guinum, int spacing, int left, int top, int right, int bottom){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  QLayout *layout = gui->getLayout();
  if (layout==NULL){
    handleError("Gui #%d doesn't have a layout", (int)guinum);
    return;
  }

  layout->setSpacing(spacing);
  layout->setContentsMargins(left, top, right, bottom);
}

static inline QSizePolicy::Policy get_grow_policy_from_bool(bool grow){
  return grow ? QSizePolicy::MinimumExpanding : QSizePolicy::Fixed;
}

void gui_addLayoutSpace(int64_t guinum, int width, int height, bool grow_horizontally, bool grow_vertically){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  QLayout *layout = gui->getLayout();
  if (layout==NULL){
    handleError("Gui #%d doesn't have a layout", (int)guinum);
    return;
  }

  layout->addItem(new QSpacerItem(width,height,
                                  get_grow_policy_from_bool(grow_horizontally),get_grow_policy_from_bool(grow_vertically)
                                  )
                  );
}

void gui_setSizePolicy(int64_t guinum, bool grow_horizontally, bool grow_vertically){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  QSizePolicy policy = QSizePolicy(get_grow_policy_from_bool(grow_horizontally),
                                   get_grow_policy_from_bool(grow_vertically));
  
  //policy.setRetainSizeWhenHidden(true);  // screws up mixer. Don't know why.  (Press "M" two times)
  
  auto *scroll = dynamic_cast<VerticalScroll*>(gui->_widget.data());
  if (scroll!=NULL)
    scroll->contents->setSizePolicy(policy);
  else
    gui->_widget->setSizePolicy(policy);
}

void gui_setMinWidth(int64_t guinum, int minwidth){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  auto *scroll = dynamic_cast<VerticalScroll*>(gui->_widget.data());
  if (scroll!=NULL)
    scroll->contents->setMinimumWidth(minwidth);
  else
    gui->_widget->setMinimumWidth(minwidth);
}

void gui_setMinHeight(int64_t guinum, int minheight){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->_widget->setMinimumHeight(minheight);
}

void gui_setMaxWidth(int64_t guinum, int minwidth){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  auto *scroll = dynamic_cast<VerticalScroll*>(gui->_widget.data());
  if (scroll!=NULL)
    scroll->contents->setMaximumWidth(minwidth);
  else
    gui->_widget->setMaximumWidth(minwidth);
}

void gui_setMaxHeight(int64_t guinum, int minheight){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->_widget->setMaximumHeight(minheight);
}

#if 0
// Attempt to workaround THE Qt bug (i.e. flickering when calculating geometries of widgets in a layout)
// Didn't work though. Qt insists on waiting to update geometry until the widget is visible (but why?) (the widget system in Qt is probably 99% messy.
// Probably almost all, or all, Qt programs that resize widgets with layouts have this problem).
static void minimizeRecursively(QWidget *widget){
  if (widget != NULL){
    widget->resize(10,10);
    widget->adjustSize();
    widget->updateGeometry();

    for(auto *c : widget->children()){
      QWidget *w = dynamic_cast<QWidget*>(c);      
      if (w && w->isWindow()==false){
        minimizeRecursively(w);
      }
    }
  }
}
#endif

void gui_minimizeAsMuchAsPossible(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  QWidget *widget = gui->_widget;
  
#if 1
  widget->resize(10,10);
  widget->adjustSize();
  widget->updateGeometry();
#else
  minimizeRecursively(widget);
#endif
}


void gui_setEnabled(int64_t guinum, bool is_enabled){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->_widget->setEnabled(is_enabled);
}

bool gui_isEnabled(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return false;

  return gui->_widget->isEnabled();
}

void gui_setStaticToplevelWidget(int64_t guinum, bool add){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;  
  
  bool is_included = g_static_toplevel_widgets.contains(gui->_widget);

  if (add){
    
    if (is_included){
      handleError("Gui #%d is already set as static toplevel widget", (int)guinum);
      return;
    }
    if (gui->_widget->window() != gui->_widget){
      handleError("Gui #%d is not a toplevel widget. (class name: %s)", (int)guinum, gui_className(guinum));
      return;
    }
    g_static_toplevel_widgets.push_back(gui->_widget);
    
  } else {
#if defined(RELEASE)
    R_ASSERT(false);
#endif

    if (false==is_included){
      handleError("Gui #%d has not been added as a static toplevel widget", (int)guinum);
      return;
    }
    g_static_toplevel_widgets.remove(g_static_toplevel_widgets.indexOf(gui->_widget));
  }
  
}

// Note! This function is called from the error handler.
const_char *getDateString(const_char* date_format){
  return talloc_strdup(QDate::currentDate().toString(date_format).toUtf8().constData());
}

// Note! This function is called from the error handler.
const_char *getTimeString(const_char* time_format){
  return talloc_strdup(QTime::currentTime().toString(time_format).toUtf8().constData());
}

const_char *getHtmlFromText(const_char* text){
  QString html = QTextDocumentFragment::fromPlainText(text).toHtml("UTF-8");

  // Remove <!--StartFragment--> and <!--EndFragment-->
  html.remove("<!--StartFragment-->");
  html.remove("<!--EndFragment-->");
  
  // Only keep what's inbetween <body>
  int pos1 = html.indexOf("<body>");
  int pos2 = html.lastIndexOf("</body>");

  if (pos1>= 0 && pos2>5){
    pos1 += QString("<body>").size();
    if (pos2>pos1){
      auto ref = html.midRef(pos1, pos2-pos1).toString();
      return talloc_strdup(ref.toUtf8().constData());
    }
  }

  printf("html: %s\n", html.toUtf8().constData());
  R_ASSERT_NON_RELEASE(false);

  return talloc_strdup(html.toUtf8().constData());
}

// audio meter
////////////////

int64_t gui_verticalAudioMeter(int64_t instrument_id, int64_t note_event_instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return -1;

  SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
  R_ASSERT_RETURN_IF_FALSE2(plugin!=NULL, -1);
  
  if(plugin->type->num_outputs==0 && plugin->type->num_inputs==0){
    handleError("gui_verticalAudioMeter: instrument %s has no audio inputs or audio outputs", patch->name);
    return -1;
  }

  struct Patch *note_event_patch = note_event_instrument_id==-1 ? patch : getAudioPatchFromNum(note_event_instrument_id);
  if(note_event_patch==NULL)
    return -1;

  return (new VerticalAudioMeter(patch, note_event_patch))->get_gui_num();
}

int64_t gui_addVerticalAudioMeter(int64_t guinum, int64_t instrument_id, double x1, double y1, double x2, double y2, int64_t note_event_instrument_id){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return -1;

  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return -1;

  struct Patch *note_event_patch = note_event_instrument_id==-1 ? patch : getAudioPatchFromNum(note_event_instrument_id);
  if(note_event_patch==NULL)
    return -1;
  
  // We get garbage graphics when the coordinates are not integers. Don't bother to investigate why.
  x1 = floor(x1);
  y1 = floor(y1);
  x2 = floor(x2);
  y2 = floor(y2);
  
  auto *vamp = gui->createVamp(patch, note_event_patch, x1, y1, x2, y2);
  gui_update(guinum, x1, y1, x2, y2);
                       
  return vamp->_id;
}

bool gui_removeVerticalAudioMeter(int64_t vap){
  for(auto *vamp : g_active_vertical_audio_meters){
    if (vamp->_id == vap){
      Gui *gui = g_guis.value(vamp->_guinum);
      if(gui==NULL)
        R_ASSERT(false);
      else {
        gui->deleteVamp(vamp);
        return true;
      }
    }
  }

  handleError("gui_removeVerticalAudioMeter: vertical audio meter %d not found", (int)vap);
  return false;
}
                               

int gui_removeAllVerticalAudioMeters(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return 0;

  int ret = gui->_vamps.size();

  while(gui->_vamps.size() > 0)
    gui->deleteVamp(gui->_vamps.value(0));

  return ret;
}
                               
void gui_addAudioMeterPeakCallback(int64_t guinum, func_t* func){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  VerticalAudioMeter *meter = gui->mycast<VerticalAudioMeter>(__FUNCTION__);
  if (meter==NULL)
    return;

  meter->_vamps.at(0)->addPeakCallback(func, guinum);
}

void gui_resetAudioMeterPeak(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;
  
  VerticalAudioMeter *meter = gui->mycast<VerticalAudioMeter>(__FUNCTION__);
  if (meter==NULL)
    return;

  meter->_vamps.at(0)->resetPeak();
}


///////////////// Mixer strips
///////////////////////////////

static QVector<int64_t> g_mixerstrip_guinums;
void informAboutGuiBeingAMixerStrips(int64_t guinum){
  g_mixerstrip_guinums.push_back(guinum);
}

int64_t gui_createMixerStrips(int num_rows, dyn_t instrument_ids){
  if (instrument_ids.type==UNINITIALIZED_TYPE)
    instrument_ids = DYN_create_bool(false);
  
  return S7CALL2(int_int_dyn, "create-mixer-strips-gui", num_rows, instrument_ids);
}

int gui_getNumRowsInMixerStrips(int64_t guinum){
  return (int)S7CALL2(int_int, "mixer-strips-get-num-rows", guinum);
}

void gui_setNumRowsInMixerStrips(int64_t guinum, int num_rows){
  if (num_rows < 1){
    handleError("gui_setNumRowsInMixerStrips: num_rows < 1: %d < 1", num_rows);
    return;
  }
  S7CALL2(void_int_int, "mixer-strips-change-num-rows", guinum, num_rows);
}

// Called after loading.
void gui_resetAllMixerStrips(void){
  for(int64_t guinum : g_mixerstrip_guinums)
    S7CALL2(void_int, "mixer-strips-reset-configuration!", guinum);
}

dyn_t gui_getMixerStripsConfiguration(int64_t guinum){
  return S7CALL2(dyn_int, "mixer-strips-get-configuration", guinum);
}

void gui_setMixerStripsConfiguration(int64_t guinum, dyn_t configuration){
  return S7CALL2(void_int_dyn, "mixer-strips-set-configuration!", guinum, configuration);
}

int64_t showMixerStrips2(int num_rows, dyn_t instrument_ids){
  int64_t gui = gui_createMixerStrips(num_rows, instrument_ids);
  if (gui!=-1)
    gui_show(gui);
  return gui;
}

int64_t showMixerStrips(int num_rows){
  return showMixerStrips2(num_rows, g_uninitialized_dyn);
}

QVector<QWidget*> MIXERSTRIPS_get_all_widgets(void){ 
  QVector<QWidget*> ret;
  QVector<int64_t> to_remove;

  for(int64_t guinum : g_mixerstrip_guinums){

    Gui *gui = g_guis.value(guinum);
    
    if (gui==NULL)     
      to_remove.push_back(guinum);
    else if (gui->_full_screen_parent != NULL)
      ret.push_back(gui->_full_screen_parent);
    else
      ret.push_back(gui->_widget);
  }

  for(int64_t guinum : to_remove)
    g_mixerstrip_guinums.removeOne(guinum);

  return ret;
}


QWidget *MIXERSTRIPS_get_curr_widget(void){
  QVector<int64_t> to_remove;

  QWidget *ret = NULL;
  
  for(int64_t guinum : g_mixerstrip_guinums){

    Gui *gui = g_guis.value(guinum);
    
    if (gui==NULL) {
     
      to_remove.push_back(guinum);

    } else {

      QWidget *widget = gui->_widget;
      if (gui->_full_screen_parent != NULL)
        widget = gui->_full_screen_parent;
      
      QPoint p = widget->mapFromGlobal(QCursor::pos());
      int x = p.x();
      int y = p.y();

      //printf("                  MIXERSTRIPS_get_curr_widget. x: %d, y: %d, width: %d, height: %d\n", x,y,widget->width(),widget->height());
      
      if (true
          && x >= 0
          && x <  widget->width()
          && y >= 0
          && y <  widget->height()
          )
        {
          ret = widget;
          break;
        }
    }
  }

  for(int64_t guinum : to_remove)
    g_mixerstrip_guinums.removeOne(guinum);
  
  return ret;  
}

bool MIXERSTRIPS_has_mouse_pointer(void){
  return MIXERSTRIPS_get_curr_widget() != NULL;  
}

///////////////// Drawing
/////////////////////////////

void gui_drawLine(int64_t guinum, const_char* color, float x1, float y1, float x2, float y2, float width){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->drawLine(color, x1, y1, x2, y2, width);
}

void gui_drawBox(int64_t guinum, const_char* color, float x1, float y1, float x2, float y2, float width, float round_x, float round_y){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->drawBox(color, x1, y1, x2, y2, width, round_x, round_y);
}

void gui_filledBox(int64_t guinum, const_char* color, float x1, float y1, float x2, float y2, float round_x, float round_y, bool do_gradient){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->filledBox(color, x1, y1, x2, y2, round_x, round_y, do_gradient);
}

void gui_filledPolygon(int64_t guinum, const_char* color, dyn_t points){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  if (points.type != ARRAY_TYPE){
    handleError("gui_filledPolygon: Argument 'points' must be a list or a vector, found %s", DYN_type_name(points.type));
    return;
  }

  const dynvec_t *array = points.array;

  if (array->num_elements == 0){
    return;
  }

  if (array->num_elements % 2 == 1){
    handleError("gui_filledPolygon: Number of points must be an even number, found %d", array->num_elements);
    return;
  }

  gui->drawPolygon(color, array, true);
}

void gui_drawPolygon(int64_t guinum, const_char* color, dyn_t points, float width) {
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  if (points.type != ARRAY_TYPE){
    handleError("gui_drawPolygon: Argument 'points' must be a list or a vector, found %s", DYN_type_name(points.type));
    return;
  }

  const dynvec_t *array = points.array;

  if (array->num_elements == 0){
    return;
  }

  if (array->num_elements % 2 == 1){
    handleError("gui_drawPolygon: Number of points must be an even number, found %d", array->num_elements);
    return;
  }

  gui->drawPolygon(color, array, false, width);
}

void gui_filledEllipse(int64_t guinum, const_char* color, float x1, float y1, float x2, float y2){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->filledEllipse(color, x1, y1, x2, y2);
}

void gui_drawEllipse(int64_t guinum, const_char* color, float x1, float y1, float x2, float y2, float width) {
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->drawEllipse(color, x1, y1, x2, y2, width);
}

void gui_setPaintOpacity(int64_t guinum, double opacity){
  Gui *gui = get_gui(guinum);

  if (gui==NULL)
    return;

  opacity = R_BOUNDARIES(0,opacity,1);

  gui->setOpacity(opacity);
}

bool gui_drawText(int64_t guinum, const_char* color, const_char *text, float x1, float y1, float x2, float y2, bool wrap_lines, bool align_top, bool align_left, int rotate, bool cut_text_to_fit, bool scale_font_size) {
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return false;

  return gui->drawText(color, text, x1, y1, x2, y2, wrap_lines, align_top, align_left, rotate, cut_text_to_fit, scale_font_size);
}

void gui_drawVerticalText(int64_t guinum, const_char* color, const_char *text, float x1, float y1, float x2, float y2, bool wrap_lines, bool align_top, bool align_left) {
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;
  
  gui->drawText(color, text, x1, y1, x2, y2, wrap_lines, align_top, align_left, 90, true, true);
}

bool gui_areaNeedsPainting(int64_t guinum, float x1, float y1, float x2, float y2){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return false;

  const QRegion *region = gui->_current_region;
  
  if (region==NULL){
    R_ASSERT(gui->_current_painter==NULL);
    handleError("gui_areaNeedsPainting: Function must be called from paint callback.");
    return false;
  }

  R_ASSERT(gui->_current_painter!=NULL);
  
  const QRect rect = QRectF(x1, y1, x2-x1, y2-y1).toAlignedRect();

  return region->intersects(rect);
}


///////////// drag and drop

namespace{
  struct DragTempWidget : QWidget, Gui{

    func_t *_paint_icon_callback;

    DragTempWidget(func_t* paint_icon_callback, int width, int height)
      : Gui(this)
      , _paint_icon_callback(paint_icon_callback)
    {
      setMinimumSize(width, height);
      resize(width,height);
      QColor c = Qt::transparent;
    QPalette pal = palette();
    pal.setColor(QPalette::Background, c);
    pal.setColor(QPalette::Base, c);
    //gui->_widget->setAutoFillBackground(true);
    setPalette(pal);

    }

    void paintEvent(QPaintEvent *ev) override{
      ScopedEventHandlerTracker event_handler_tracker;
      TRACK_PAINT();
      QPainter p(this);
      _current_painter = &p;
      p.setRenderHints(QPainter::Antialiasing,true);    
      p.fillRect(rect(), Qt::transparent);
      S7CALL(void_int_int_int, _paint_icon_callback, get_gui_num(), width(), height());
      _current_painter = NULL;
    }
  };
}


static void create_drag_icon(int64_t parent, int width, int height, float hotspot_x, float hotspot_y, const_char* w_path, int blocknum, func_t *paint_icon_callback){

  Gui *gui = get_gui(parent);
  
  if (gui==NULL)
    return;

  QMimeData *mimeData = new QMimeData;

  if (w_path != NULL){
    R_ASSERT(blocknum==-1);
    printf("   w_path: -%s-.   path: -%s-\n", w_path, w_to_qstring(w_path).toUtf8().constData());
    QUrl url = QUrl::fromLocalFile(w_to_qstring(w_path));
    QList<QUrl> urls;
    urls << url;
    mimeData->setUrls(urls);
  } else {
    mimeData->setText("block:" + QString::number(blocknum));
  }

  mimeData->setData(QStringLiteral("application/x-hotspot"),
                    QByteArray::number((int)hotspot_x) + ' ' + QByteArray::number((int)hotspot_y)
                    );
  //mimeData->setText(mime_data); //child->text());
  //mimeData->setData(hotSpotMimeDataKey(),
  //                  QByteArray::number(hotSpot.x()) + ' ' + QByteArray::number(hotSpot.y()));

  qreal dpr = gui->_widget->window()->windowHandle()->devicePixelRatio();
  QPixmap pixmap(width * dpr, height * dpr);
  pixmap.setDevicePixelRatio(dpr);
  pixmap.fill(Qt::transparent);

  {
    DragTempWidget temp_widget(paint_icon_callback, width, height);
    temp_widget.render(&pixmap);
  }

  QDrag *drag = new QDrag(gui->_widget);
  drag->setMimeData(mimeData);
  drag->setPixmap(pixmap);
  drag->setHotSpot(QPoint(hotspot_x, hotspot_y));

  IsAlive is_alive(drag);

  // schedule drag->exec to run in the next qt cycle to avoid nested calls to scheme eval. Want to avoid that complication.
  QTimer::singleShot(3,[is_alive, drag]{
      if (is_alive){
        printf("\n\n\n  ------------- CALLING EXEC\n\n\n");
        drag->exec(Qt::CopyAction | Qt::MoveAction | Qt::LinkAction);
        printf("----- EXEC finished\n\n\n");
      } else {
        R_ASSERT_NON_RELEASE(false);
      }

      //if (dropAction == Qt::MoveAction)
      //  child->close();

    });
}

void gui_createBlockDragIcon(int64_t parent, int width, int height, float hotspot_x, float hotspot_y, int blocknum, func_t *paint_icon_callback){
  create_drag_icon(parent, width, height, hotspot_x, hotspot_y, NULL, blocknum, paint_icon_callback);
}

void gui_createFileDragIcon(int64_t parent, int width, int height, float hotspot_x, float hotspot_y, const_char* w_path, func_t *paint_icon_callback){
  create_drag_icon(parent, width, height, hotspot_x, hotspot_y, w_path, -1, paint_icon_callback);
}

///////////// keyboard

void obtainKeyboardFocus(int64_t guinum){
  obtain_keyboard_focus_counting();

  if (guinum >= 0){
    Gui *gui = get_gui(guinum);
    if (gui==NULL)
      return;
    gui->_widget->setFocusPolicy(Qt::StrongFocus);
    gui->_widget->setFocus();
  }
}

void releaseKeyboardFocus(void){
  release_keyboard_focus_counting();
}

bool gui_hasKeyboardFocus(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return false;

  return gui->_widget->hasFocus();
}

////////////

// NOTE. This function can be called from a custom exec().
// This means that _patch->plugin might be gone, and the same goes for soundproducer.
// (_patch is never gone, never deleted)
void API_gui_call_regularly(void){
  for(auto *vamp : g_active_vertical_audio_meters){
    Gui *gui = g_guis.value(vamp->_guinum);
    if(gui==NULL)
      R_ASSERT(false);
    else
      gui->callVampRegularly(vamp);
  }
  
  perhaps_collect_a_little_bit_of_gui_garbage(20);
}

QWidget *API_gui_get_widget(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return NULL;

  return gui->_widget;
}

static QWidget *get_parentwidget(QWidget *child, int64_t parentnum, bool must_be_window){
  QWidget *parent;
  
  if (parentnum==-1)
    parent = g_main_window;

  else if (parentnum==-2)
    // get_current_parent() can return anything, but I think the worst thing that could happen if the parent is deleted,
    // at least in this case, is that some warning messages would be displayed. The base case (and I hope only case) is
    // just that the window closes, and that closing the window was the natural thing to happen, since the parent was closed.
    parent = get_current_parent(child, false);
  
  else if (parentnum==-3)
    parent = NULL;

  else if (parentnum==-4)
    parent = QApplication::widgetAt(QCursor::pos());
  
  else {    
    Gui *parentgui = get_gui(parentnum);
    if (parentgui==NULL)
      return g_main_window;
    
    parent = parentgui->_widget;
  }

  if (parent != NULL && must_be_window) {
    QWidget *window = parent->window();
    if (parent != window){
      //printf("API_gui_get_parentwidget: #%d is not a window gui. (automatically fixed)\n", (int)parentnum); // Remove printf since this function is sometimes called quite often.
      parent = window;
    }
  }

  return parent;
}

QWidget *API_gui_get_parentwidget(QWidget *child, int64_t parentnum){
  return get_parentwidget(child, parentnum, true);
}

/*
bool API_gui_is_painting(void){
  return g_currently_painting;
}
*/


///////////////// main y splitter
/////////////////////////////////

QWidget *API_get_main_ysplitter(void){
  int64_t gui = S7CALL2(int_void,"FROM-C-get-main-y-splitter");
  return API_gui_get_widget(gui);
}


///////////////// Lower tabs
//////////////////////////////

QWidget *API_get_lowertabs(void){
  int64_t gui = S7CALL2(int_void,"FROM-C-get-lowertab-gui");
  return API_gui_get_widget(gui);
}

void API_setLowertabIncludesInstrument(bool includeit){
  S7CALL2(void_bool, "FROM-C-set-lowertab-includes-instrument", includeit);
}

void API_showInstrumentGui(void){
  S7CALL2(void_void, "FROM-C-show-instrument-gui");
}

void API_hideInstrumentGui(void){
  S7CALL2(void_void, "FROM-C-hide-instrument-gui");
}

// Returns true if the instrument widget is the current tab in the lower tabs. NOT the same as 'instrumentGuiIsVisible'
bool API_instrumentGuiIsVisibleInLowerTab(void){
  return S7CALL2(bool_void, "FROM-C-instrument-gui-is-visible");
}

///////////////////////////////////////////////

void showEditGui(void){
  S7CALL2(void_void, "FROM-C-show-edit-gui");
}

void hideEditGui(void){
  S7CALL2(void_void, "FROM-C-hide-edit-gui");
}

// Returns true if the instrument widget is the current tab in the lower tabs.
bool editGuiIsVisible(void){
  return S7CALL2(bool_void, "FROM-C-edit-gui-is-visible");
}


///////////////////////////////////////////////

void API_showSequencerGui(void){
  S7CALL2(void_void, "FROM-C-show-sequencer-gui");
}

void API_hideSequencerGui(void){
  S7CALL2(void_void, "FROM-C-hide-sequencer-gui");
}

bool GFX_SequencerIsVisible(void){
  return S7CALL2(bool_void, "FROM-C-sequencer-gui-is-visible");
}


///////////////// Mixer strips
//////////////////////////////


// Returns a single mixer strip. (Currently, this function is only used in MIXERSTRIP_call_regularly() in Qt_instruments.cpp)
int64_t gui_createSingleMixerStrip(int64_t instrument_id, int width, int height){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return -1;

  return S7CALL2(int_int_int_int,"create-standalone-mixer-strip", instrument_id, width, height);
}


///////////////// Triangle collision detection
//////////////////////////////////////////////

// Based on https://gist.github.com/TimSC/5ba18ae21c4459275f90
//2D Triangle-Triangle collisions in C++
//Release by Tim Sheerman-Chase 2016 under CC0


//#include <vector>
//#include <iostream>
//#include <stdexcept>
//using namespace std;

typedef std::pair<double, double> TriPoint;

static inline double Det2D(TriPoint &p1, TriPoint &p2, TriPoint &p3) 
{
	return +p1.first*(p2.second-p3.second)
		+p2.first*(p3.second-p1.second)
		+p3.first*(p1.second-p2.second);
}

static bool CheckTriWinding(TriPoint &p1, TriPoint &p2, TriPoint &p3, bool allowReversed)
{
	double detTri = Det2D(p1, p2, p3);
	if(detTri < 0.0)
	{
		if (allowReversed)
		{
			TriPoint a = p3;
			p3 = p2;
			p2 = a;
		}
		else{
                  handleError("triangle has wrong winding direction");
                  return false;
                }
	}
        return true;
}

static bool BoundaryCollideChk(TriPoint &p1, TriPoint &p2, TriPoint &p3, double eps)
{
	return Det2D(p1, p2, p3) < eps;
}

static bool BoundaryDoesntCollideChk(TriPoint &p1, TriPoint &p2, TriPoint &p3, double eps)
{
	return Det2D(p1, p2, p3) <= eps;
}

static bool TriTri2D(TriPoint *t1,
                     TriPoint *t2,
                     double eps = 0.0, bool allowReversed = true, bool onBoundary = true)
{
  //Trangles must be expressed anti-clockwise
  if (CheckTriWinding(t1[0], t1[1], t1[2], allowReversed)==false)
    return false;
  
  if (CheckTriWinding(t2[0], t2[1], t2[2], allowReversed)==false)
    return false;

	bool (*chkEdge)(TriPoint &, TriPoint &, TriPoint &, double) = NULL;
	if(onBoundary) //Points on the boundary are considered as colliding
		chkEdge = BoundaryCollideChk;
	else //Points on the boundary are not considered as colliding
		chkEdge = BoundaryDoesntCollideChk;

	//For edge E of trangle 1,
	for(int i=0; i<3; i++)
	{
		int j=(i+1)%3;

		//Check all points of trangle 2 lay on the external side of the edge E. If
		//they do, the triangles do not collide.
		if (chkEdge(t1[i], t1[j], t2[0], eps) &&
			chkEdge(t1[i], t1[j], t2[1], eps) &&
			chkEdge(t1[i], t1[j], t2[2], eps))
			return false;
	}

	//For edge E of trangle 2,
	for(int i=0; i<3; i++)
	{
		int j=(i+1)%3;

		//Check all points of trangle 1 lay on the external side of the edge E. If
		//they do, the triangles do not collide.
		if (chkEdge(t2[i], t2[j], t1[0], eps) &&
			chkEdge(t2[i], t2[j], t1[1], eps) &&
			chkEdge(t2[i], t2[j], t1[2], eps))
			return false;
	}

	//The triangles collide
	return true;
}

bool trianglesIntersects(float a_x1, float a_y1, float a_x2, float a_y2, float a_x3, float a_y3, float b_x1, float b_y1, float b_x2, float b_y2, float b_x3, float b_y3, bool allowReversed, bool onBoundary){
  TriPoint t1[] = {TriPoint(a_x1,a_y1),TriPoint(a_x2,a_y2),TriPoint(a_x3, a_y3)};
  TriPoint t2[] = {TriPoint(b_x1,b_y1),TriPoint(b_x2,b_y2),TriPoint(b_x3, b_y3)};
  return TriTri2D(t1, t2, 0.0, allowReversed, onBoundary);
}






#include "mapi_gui.cpp"


