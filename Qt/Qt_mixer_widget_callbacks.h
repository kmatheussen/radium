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


#include <qmath.h>
#include <assert.h>

#include "../api/api_gui_proc.h"
#include "../api/api_proc.h"

#include <QSplitter>

#include "Qt_bottom_bar_widget_proc.h"
#include "Timer.hpp"


bool g_pause_scroll_area_updates_when_resizing = false;




static QSlider *g_zoom_slider = NULL;
//static QWidget *g_view = NULL;

extern bool g_pause_scroll_area_updates_when_resizing;


class MyQGraphicsView : public QGraphicsView{
public:
  MyQGraphicsView(QWidget *parent)
    : QGraphicsView(parent)
  {
    //g_view = this;
  }

  // http://stackoverflow.com/questions/1355446/get-visible-rectangle-of-qgraphicsview
  QRectF visibleRect(){
    QPointF A = mapToScene( QPoint(0,0) );
    QPointF B = mapToScene( QPoint( 
                                    viewport()->width(), 
                                    viewport()->height()));
    return QRectF( A, B );
  }

  void enterEvent(QEvent *event) override {
    setCursor(Qt::ArrowCursor);
  }

  void keyPressEvent ( QKeyEvent * event ) override {
    event->ignore();
  }
  
  void wheelEvent(QWheelEvent *e) override
  {
    if(g_zoom_slider!=NULL){
      if (e->modifiers() & Qt::ControlModifier) {
        if (e->delta() > 0)
          g_zoom_slider->setValue(g_zoom_slider->value() + 6);
        else
          g_zoom_slider->setValue(g_zoom_slider->value() - 6);
      } else if (e->modifiers() & Qt::ShiftModifier) {
        QScrollBar *scrollbar = horizontalScrollBar();
        if(scrollbar!=NULL){
          if (e->delta() > 0)
            scrollbar->setValue(scrollbar->value()-70);
          else
            scrollbar->setValue(scrollbar->value()+70);
        }
      } else {
        QScrollBar *scrollbar = verticalScrollBar();
        if(scrollbar!=NULL){
          if (e->delta() > 0)
            scrollbar->setValue(scrollbar->value()-70);
          else
            scrollbar->setValue(scrollbar->value()+70);
        }
        //QGraphicsView::wheelEvent(e);
      }
    }

    e->accept();
  }
};

#define QGraphicsView MyQGraphicsView
#include "Qt_mixer_widget.h"

#include "mQt_mixer_direction_menu_callbacks.h"


extern EditorWidget *g_editor;

class Mixer_widget;
static Mixer_widget *g_mixer_widget2;

class Mixer_widget : public QWidget, public Ui::Mixer_widget, radium::Timer{
  Q_OBJECT

 public:
  bool initing;

  qreal _rotate;

  qreal _middle_zoom;

  Mixer_Direction_Menu _mixer_direction_menu;

  dyn_t *_mixer_strip_configuration = NULL;
  int64_t _mixer_strips_gui = -1;
  int _num_rows = 2;

  QWidget *_bottom_bar;

 Mixer_widget(QWidget *parent=NULL)
    : QWidget(parent)
    , radium::Timer(100, true)
    , _rotate(0)
    , _middle_zoom(1.0)
    , _mixer_direction_menu(this)
    , _mytimer(this)
  {
    initing = true;
    
    setupUi(this);
    g_zoom_slider = zoom_slider;
    zoom_slider->hide();
    
    // Tro to find default zoom level based on system font
    QFont font = g_editor->main_window->font();
    _middle_zoom = 230 - (font.pointSize()-12) * 4.0;
    
    on_zoom_slider_valueChanged(zoom_slider->value());

    // don't need the zoom buttons.
    zoomin_button->hide();
    zoomout_button->hide();
    
    update_ab_buttons(true);
    show_modular_mixer_widgets(true);
    
    connections_visibility->setChecked(MW_get_connections_visibility());

    if (instrumentWidgetIsInMixer()){
      verticalLayout->addWidget(getInstrumentsWidget(), 0);
      include_instrument_widget->setChecked(instrumentWidgetIsInMixer());
      //getInstrumentsWidget()->show();
    }
      
    //connect(ab_a, SIGNAL(rightClicked()), this, SLOT(on_ab_a_rightClicked()));
      
    g_mixer_widget2 = this;

    _bottom_bar = BottomBar_create(this);
    verticalLayout->insertWidget(-1, _bottom_bar, 0);
    _bottom_bar->hide();
    
    initing = false;
  }

  void enterEvent(QEvent *event) override {
    setCursor(Qt::ArrowCursor);
  }

  void set_rotate(qreal rotate){
    _rotate = rotate;

    qreal scale = qPow(qreal(2), (zoom_slider->value() - 250) / qreal(50));

    QMatrix matrix;
    matrix.scale(scale, scale);
    matrix.rotate(_rotate);

    view->setMatrix(matrix);    
  }

  void update_ab_buttons(bool update_current_button){
    int curr = MW_get_curr_ab();

    MyQCheckBox *buttons[MW_NUM_AB] = {ab_a, ab_b, ab_c, ab_d, ab_e, ab_f, ab_g, ab_h};
    const QString names[MW_NUM_AB]    = {"A",  "B",  "C",  "D",  "E",  "F",  "G",  "H"};
    const QString selnames[MW_NUM_AB] = {"A*", "B*", "C*", "D*", "E*", "F*", "G*", "H*"};

    static int _ab_checkbox_width=-1;
    
    if (_ab_checkbox_width==-1){
      ab_a->adjustSize();
      _ab_checkbox_width = ab_a->width() * 2 / 3;
    }

    for(int i=0;i<MW_NUM_AB;i++){
      
      buttons[i]->setMinimumWidth(_ab_checkbox_width);
      buttons[i]->setMaximumWidth(_ab_checkbox_width);

      if(curr==i || MW_is_ab_valid(i))
        buttons[i]->setText(selnames[i]);
      else
        buttons[i]->setText(names[i]);

      if (curr==i){
        if (buttons[i]->isChecked()==false && update_current_button)
          buttons[i]->setChecked(true);
      }
    }
  }

  void ab_rightclicked(int num){
    if (simplePopupMenu(talloc_format("%sReset",MW_get_curr_ab()==num?"[disabled]":""))==0){
      MW_reset_ab(num);
      update_ab_buttons(false);
    }
  }

  
  void show_modular_mixer_widgets(bool is_modular){
    zoomreset_button->setVisible(is_modular);
    //zoom_slider->setVisible(is_modular);
    mixer_direction_menu_button->setVisible(is_modular);
    show_cpu_usage->setVisible(is_modular);
    connections_visibility->setVisible(is_modular);

    rows1->setVisible(!is_modular);
    rows2->setVisible(!is_modular);
    rows3->setVisible(!is_modular);
    rows4->setVisible(!is_modular);
  }

  void change_num_mixerstrips_rows(int num_rows){
    if(!initing && _mixer_strips_gui!=-1 && num_rows!=_num_rows){
      gui_setNumRowsInMixerStrips(_mixer_strips_gui, num_rows);
      /*
      int64_t old_gui = _mixer_strips_gui;
      int64_t guinum, int num_rows){
      _mixer_strips_gui = gui_createMixerStrips(num_rows, g_uninitialized_dyn);
      QWidget *w = API_gui_get_widget(_mixer_strips_gui);
      auto *old_item = verticalLayout->replaceWidget(API_gui_get_widget(old_gui), w);
      verticalLayout->setStretchFactor(w,1);
      delete old_item;
      gui_close(old_gui);
      */
    }

    _num_rows = num_rows;
  }
    

  struct MyTimer : public QTimer{
    Mixer_widget *_parent;
    QTime time;

    MyTimer(Mixer_widget *parent)
      : _parent(parent)
    {
      //setSingleShot(true);
      setInterval(50);
    }

    void timerEvent(QTimerEvent *e) override {
      if (time.elapsed() > 30){ // singleshot is messy since we might get deleted at any time.
        printf("TEIMERERINE EVENT\n");
        // _parent->adjustSize();
        _parent->setUpdatesEnabled(true);
        //g_mixer_widget->setUpdatesEnabled(true);
        g_pause_scroll_area_updates_when_resizing = false;
        //mixer_layout->setUpdatesEnabled(true);
        // _parent->mixer_layout->update();
        /*
        if(_parent->_mixer_strips_gui!=-1){
          QWidget *w = API_gui_get_widget(_parent->_mixer_strips_gui);
          w->setUpdatesEnabled(true);
        }
        */
        //_parent->verticalLayout->update();
        stop();
      }
    }

    void startit(void){
      time.restart();
      if (!isActive()){
        //_parent->setUpdatesEnabled(false);
        start();
      }
    }

  };

  MyTimer _mytimer;

  
  void pauseUpdatesALittleBit(void){
    _mytimer.startit();
#if 0
    //mixer_layout->update();
    if (!_mytimer.isActive()){
      //setUpdatesEnabled(false);
      //mixer_layout->setUpdatesEnabled(false);
      /*
      if(_mixer_strips_gui!=-1){
        QWidget *w = API_gui_get_widget(_mixer_strips_gui);
        w->setUpdatesEnabled(false);
      }
      */
      _mytimer.time.restart();
      _mytimer.start();
      printf("                 Started timer\n");
    }
#endif
  }

  /*
  void resizeEvent( QResizeEvent *qresizeevent) override{
  radium::ScopedResizeEventTracker resize_event_tracker;
    //pauseUpdatesALittleBit();
    //Mixer_widget->update();
  }
  */

  void calledFromTimer(void) override {
    if (_mixer_strips_gui >= 0){

      int new_num_rows = gui_getNumRowsInMixerStrips(_mixer_strips_gui);
      if (new_num_rows != _num_rows){

        initing = true;{

          if(new_num_rows==1)
            rows1->setChecked(true);
          else if (new_num_rows==2)
            rows2->setChecked(true);
          else if (new_num_rows==3)
            rows3->setChecked(true);
          else if (new_num_rows==4)
            rows4->setChecked(true);

        }initing = false;

      }
    }
  }

public slots:

  void on_ab_a_clicked(){ab_rightclicked(0);}
  void on_ab_b_clicked(){ab_rightclicked(1);}
  void on_ab_c_clicked(){ab_rightclicked(2);}
  void on_ab_d_clicked(){ab_rightclicked(3);}
  void on_ab_e_clicked(){ab_rightclicked(4);}
  void on_ab_f_clicked(){ab_rightclicked(5);}
  void on_ab_g_clicked(){ab_rightclicked(5);}
  void on_ab_h_clicked(){ab_rightclicked(6);}
    
  void on_ab_a_toggled(bool val){
    printf("  CHECK A. val: %d\n", val);
    if (val && !initing){
      MW_change_ab(0);
      update_ab_buttons(false);
    }
  }
  
  void on_ab_b_toggled(bool val){
    printf("  CHECK B. val: %d\n", val);
    if (val && !initing){
      MW_change_ab(1);
      update_ab_buttons(false);
    }
  }
  
  void on_ab_c_toggled(bool val){
    if (val && !initing){
      MW_change_ab(2);
      update_ab_buttons(false);
    }
  }
  
  void on_ab_d_toggled(bool val){
    if (val && !initing){
      MW_change_ab(3);
      update_ab_buttons(false);
    }
  }
  
  void on_ab_e_toggled(bool val){
    if (val && !initing){
      MW_change_ab(4);
      update_ab_buttons(false);
    }
  }
  
  void on_ab_f_toggled(bool val){
    if (val && !initing){
      MW_change_ab(5);
      update_ab_buttons(false);
    }
  }
  
  void on_ab_g_toggled(bool val){
    if (val && !initing){
      MW_change_ab(6);
      update_ab_buttons(false);
    }
  }
  
  void on_ab_h_toggled(bool val){
    if (val && !initing){
      MW_change_ab(7);
      update_ab_buttons(false);
    }
  }
  
  void on_ab_reset_clicked(){    
    MW_reset_ab(-1);
    update_ab_buttons(true);
  }

  void on_window_mode_toggled(bool show_window){
    if(initing)
      return;

    QWidget *w = get_qwidget(g_mixer_widget);
    
    //static QWidget *xsplitter = NULL;
    if(show_window){
      //if(xsplitter!=NULL)
      //  xsplitter = (QWidget*)g_mixer_widget->parent();
      pauseUpdates(w, 15); // Prevent some flickering.
      pauseUpdates(g_main_window, 15); // Prevent some flickering.
            
      w->hide();
      _bottom_bar->show();
      convert_widget_to_window(w, g_main_window, radium::NOT_MODAL);

      w->adjustSize();

      //w->show();
    } else {
      EditorWidget *editor = static_cast<EditorWidget*>(root->song->tracker_windows->os_visual.widget);
      QSplitter *splitter = editor->xsplitter;

      int pos = splitter->count();

      if (pos>0 && g_mixerstripparent == splitter->widget(pos-1))
        pos--;

      /*
      if (splitter->count() > 0){
        
        int64_t curr_mixerstrip_guinum = MIXERSTRIP_get_curr_mixerstrip_guinum();
        
        if (curr_mixerstrip_guinum >= 0 && gui_isOpen(curr_mixerstrip_guinum)) {

          QWidget *mixerstrip_widget = API_gui_get_widget(curr_mixerstrip_guinum);

          printf("pos: %d, count: %d. strip: %p, w-2: %p, w-1: %p.\n", pos, splitter->count(), mixerstrip_widget, splitter->widget(pos-2), splitter->widget(pos-1));
        }
      }
      */
      
      splitter->insertWidget(pos, w);

      _bottom_bar->hide();

#if defined(FOR_WINDOWS)
      OS_WINDOWS_set_key_window((void*)g_main_window->winId()); // Don't know why.
#endif
    }
    
    if(include_instrument_widget->isChecked())
      GFX_update_current_instrument_widget(); // Fix arrow colors, etc.
  }
  
  void on_show_modular_toggled(bool show_modular){
    if (initing)
      return;

    g_pause_scroll_area_updates_when_resizing = true;
    //g_mixer_widget->setUpdatesEnabled(false); // <- This causes graphics not to be updated after switching when running in separate window. Need to resize a little bit first.
    setUpdatesEnabled(false);
    
    pauseUpdatesALittleBit(); // Prevent some flickering.

    if (show_modular){

      modular_widget->show();

      if(_mixer_strips_gui != -1){
        QWidget *w = API_gui_get_widget(_mixer_strips_gui);
        //mixer_layout->removeWidget(w); // I've tried very hard to use replaceWidget instead of showing/hiding the 'view' widget, but Qt refuses to behave as I want.
        w->hide();
        //gui_close(_mixer_strips_gui);
        //_mixer_strips_gui = -1;
        
      }
      show_modular_mixer_widgets(true);
      
    } else {

      if (_mixer_strips_gui == -1){

        _mixer_strips_gui = gui_createMixerStrips(_num_rows, g_uninitialized_dyn);

        if (_mixer_strips_gui != -1){
          show_modular_mixer_widgets(false);
          QWidget *w = API_gui_get_widget(_mixer_strips_gui);
          //w->setParent(bottom_widget);
          //w->setFixedSize(width(), height()-50);
          verticalLayout->insertWidget(1, w, 1);
          modular_widget->hide();
          /*
            mixer_layout->update();
            verticalLayout->update();
            updateGeometry();
          */
        }

      } else {

        QWidget *w = API_gui_get_widget(_mixer_strips_gui);
        w->setUpdatesEnabled(false);
        show_modular_mixer_widgets(false);        
        modular_widget->hide();
        w->show();
        w->setUpdatesEnabled(true);

      }

      if (_mixer_strip_configuration != NULL){
        gui_setMixerStripsConfiguration(_mixer_strips_gui, *_mixer_strip_configuration);
        remove_gc_root(_mixer_strip_configuration);
        _mixer_strip_configuration = NULL;
      }
    }
    
    //setUpdatesEnabled(true); // It's a flaw in Qt that we need to call this function. And it doesn't even work very well.

    if(include_instrument_widget->isChecked())
      GFX_update_current_instrument_widget(); // Fix arrow colors, etc.
  }

  void on_include_instrument_widget_toggled(bool include_instrument_widget){
    if (initing)
      return;
    
    if(include_instrument_widget){
      API_setLowertabIncludesInstrument(false);
      verticalLayout->insertWidget(verticalLayout->count()-1, getInstrumentsWidget(), 0);
    }

    setInstrumentWidgetInMixer(include_instrument_widget);
      
    GFX_update_current_instrument_widget(); // Fix arrow colors, etc.
    
    if (include_instrument_widget && getInstrumentsWidget()->isVisible()==false)
      GFX_InstrumentWindowToFront();

    if (!include_instrument_widget)
      API_setLowertabIncludesInstrument(true);
  }

  void on_rows1_toggled(bool val){
    if(val)
      change_num_mixerstrips_rows(1);
  }
  
  void on_rows2_toggled(bool val){
    if(val)
      change_num_mixerstrips_rows(2);
  }
  
  void on_rows3_toggled(bool val){
    if(val)
      change_num_mixerstrips_rows(3);
  }
  
  void on_rows4_toggled(bool val){
    if(val)
      change_num_mixerstrips_rows(4);
  }
  
  void on_show_cpu_usage_toggled(bool val){
    ATOMIC_SET(g_show_cpu_usage_in_mixer, val);
    MW_update_all_chips();
  }

  void on_connections_visibility_toggled(bool val){
    if (initing==false)
      MW_set_connections_visibility(val);
  }

  void on_mixer_direction_menu_button_released() {
    _mixer_direction_menu.myExec();
    
    if (_rotate>=(270+180)/2)
      mixer_direction_menu_button->setText("\342\207\221");
    else if (_rotate>=(180+90)/2)
      mixer_direction_menu_button->setText("\342\207\220");
    else if (_rotate>=(90/2))
      mixer_direction_menu_button->setText("\342\207\223");
    else
      mixer_direction_menu_button->setText("\342\207\222");
    
    //update_widget();
  }

  /*    
  void on_up_button_toggled(bool val){
    if(val==true){
      set_rotate(270);
    }
  }
  void on_down_button_toggled(bool val){
    if(val==true){
      set_rotate(90);
    }
  }
  void on_left_button_toggled(bool val){
    if(val==true){
      set_rotate(180);
    }
  }
  void on_right_button_toggled(bool val){
    if(val==true){
      set_rotate(0);
    }
  }
  */
  
  void on_zoom_slider_valueChanged(int val){
    
    qreal scale = qPow(qreal(2), (val - _middle_zoom) / qreal(50));

    QMatrix matrix;
    matrix.scale(scale, scale);
    matrix.rotate(_rotate);

    view->setMatrix(matrix);
  }

  void on_zoomin_button_clicked(){
    zoom_slider->setValue(zoom_slider->value() + 6);
  }
  void on_zoomout_button_clicked(){
    zoom_slider->setValue(zoom_slider->value() - 6);
  }
  void on_zoomreset_button_clicked(){
    zoom_slider->setValue(250);
  }

  void on_help_button_clicked(){
    showMixerHelpWindow();
  }

};


void MW_set_rotate(float rotate){
  g_mixer_widget2->set_rotate(rotate);
}

void MW_update_mixer_widget(void){
  g_mixer_widget2->update_ab_buttons(true);  
  g_mixer_widget2->update();
}

void MW_disable_include_instrument_checkbox(void){
  g_mixer_widget2->include_instrument_widget->setEnabled(false);
}
void MW_enable_include_instrument_checkbox(void){
  g_mixer_widget2->include_instrument_widget->setEnabled(true);
}

void MW_instrument_widget_set_size(QWidget *audio_widget, SizeType old_size_type, SizeType new_size_type){
  R_ASSERT_RETURN_IF_FALSE(instrumentWidgetIsInMixer());

  if (new_size_type==SIZETYPE_HALF){
    g_mixer_widget2->verticalLayout->setStretchFactor(getInstrumentsWidget(),1);
  } else {
    g_mixer_widget2->verticalLayout->setStretchFactor(getInstrumentsWidget(),0);
  }
}

void MW_hide_non_instrument_widgets(void){
  if (g_mixer_widget2->_mixer_strips_gui != -1){
    QWidget *w = API_gui_get_widget(g_mixer_widget2->_mixer_strips_gui);
    w->hide();
  }else
    g_mixer_widget2->modular_widget->hide();
  
  g_mixer_widget2->bar_widget->hide();
}

void MW_show_non_instrument_widgets(void){
  if (g_mixer_widget2->_mixer_strips_gui != -1){
    QWidget *w = API_gui_get_widget(g_mixer_widget2->_mixer_strips_gui);
    w->show();
  }else
    g_mixer_widget2->modular_widget->show();
  
  g_mixer_widget2->bar_widget->show();
}

dyn_t MW_get_mixer_strips_state(void){
  if (g_mixer_widget2->_mixer_strips_gui==-1)
    return DYN_create_bool(false);
  else
    return gui_getMixerStripsConfiguration(g_mixer_widget2->_mixer_strips_gui);
}

void MW_apply_mixer_strips_state(dyn_t state){
  if (state.type==BOOL_TYPE){
    R_ASSERT(state.bool_number==false);
    return;
  }

  if (g_mixer_widget2->show_modular->isChecked()==false){
    
    R_ASSERT_RETURN_IF_FALSE(g_mixer_widget2->_mixer_strips_gui >= 0);
    gui_setMixerStripsConfiguration(g_mixer_widget2->_mixer_strips_gui, state);

  } else {

    g_mixer_widget2->_mixer_strip_configuration =  (dyn_t*)add_gc_root(tcopy(&state, sizeof(dyn_t)));

  }
}

#if 0
extern "C"{ void GFX_showHideMixerWidget(void);}

void GFX_showHideMixerWidget(void){
  if(g_view!=NULL)
    g_view->hide();
}
#endif

