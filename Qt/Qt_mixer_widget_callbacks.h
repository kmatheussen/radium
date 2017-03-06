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

#include "../api/api_proc.h"


static QSlider *g_zoom_slider = NULL;
//static QWidget *g_view = NULL;

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

  void enterEvent(QEvent *event) override{
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

class Mixer_widget : public QWidget, public Ui::Mixer_widget{
  Q_OBJECT

 public:
  bool initing;

  qreal _rotate;

  qreal _middle_zoom;

  Mixer_Direction_Menu _mixer_direction_menu;

  int64_t _mixer_strips_gui = -1;
  int _num_rows = 2;
  
 Mixer_widget(QWidget *parent=NULL)
    : QWidget(parent)
    , _rotate(0)
    , _middle_zoom(1.0)
    , _mixer_direction_menu(this)
    , _mytimer(this)
  {
    initing = true;
    setupUi(this);
    g_zoom_slider = zoom_slider;
    initing = false;

    // Tro to find default zoom level based on system font
    QFont font = g_editor->main_window->font();
    _middle_zoom = 230 - (font.pointSize()-12) * 4.0;
    
    on_zoom_slider_valueChanged(zoom_slider->value());

    // don't need the zoom buttons.
    zoomin_button->hide();
    zoomout_button->hide();
    
    update_ab_buttons();
    show_modular_mixer_widgets(true);
    
    connections_visibility->setChecked(MW_get_connections_visibility());
      
    //connect(ab_a, SIGNAL(rightClicked()), this, SLOT(on_ab_a_rightClicked()));
      
    g_mixer_widget2 = this;
  }

  void enterEvent(QEvent *event) override{
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

  void update_ab_buttons(void){
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

      if (curr==i)
        buttons[i]->setChecked(true);
    }
  }

  void ab_rightclicked(int num){
    if (popupMenu(talloc_format("%sReset",MW_get_curr_ab()==num?"[disabled]":""))==0){
      MW_reset_ab(num);
      update_ab_buttons();
    }
  }

  void show_modular_mixer_widgets(bool is_modular){
    zoomreset_button->setVisible(is_modular);
    view->setVisible(is_modular);
    zoom_slider->setVisible(is_modular);
    mixer_direction_menu_button->setVisible(is_modular);
    show_cpu_usage->setVisible(is_modular);
    connections_visibility->setVisible(is_modular);

    help_button->setVisible(is_modular);

    rows1->setVisible(!is_modular);
    rows2->setVisible(!is_modular);
    rows3->setVisible(!is_modular);
    rows4->setVisible(!is_modular);
    
    mixerstrips_help->setVisible(!is_modular);
  }

  void change_num_mixerstrips_rows(int num_rows){
    if(!initing && _mixer_strips_gui!=-1 && num_rows!=_num_rows){
      _num_rows = num_rows;
      int64_t old_gui = _mixer_strips_gui;
      if (old_gui != -1){
        _mixer_strips_gui = createMixerStripsWindow(num_rows);
        auto *old_item = mixer_layout->replaceWidget(API_gui_get_widget(old_gui), API_gui_get_widget(_mixer_strips_gui));
        delete old_item;
        gui_close(old_gui);
      }
    }
  }
    

  struct MyTimer : public QTimer{
    Mixer_widget *_parent;

    MyTimer(Mixer_widget *parent)
      : _parent(parent)
    {
      setSingleShot(true);
      setInterval(50);
    }

    void timerEvent(QTimerEvent *e) override{
      printf("TEIMERERINE EVENT\n");
      // _parent->adjustSize();
      _parent->setUpdatesEnabled(true);
      //mixer_layout->setUpdatesEnabled(true);
      // _parent->mixer_layout->update();
      if(_parent->_mixer_strips_gui!=-1){
        QWidget *w = API_gui_get_widget(_parent->_mixer_strips_gui);
        w->setUpdatesEnabled(true);
      }

      stop();
    }
  };

  MyTimer _mytimer;

  void pauseUpdatesALittleBit(void){
    //mixer_layout->update();
    if (!_mytimer.isActive()){
      setUpdatesEnabled(false);
      //mixer_layout->setUpdatesEnabled(false);
      if(_mixer_strips_gui!=-1){
        QWidget *w = API_gui_get_widget(_mixer_strips_gui);
        w->setUpdatesEnabled(false);
      }
      _mytimer.start();
      printf("                 Started timer\n");
    }
  }

  void resizeEvent( QResizeEvent *qresizeevent) override{
    pauseUpdatesALittleBit();
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
    if (val && !initing)
      MW_change_ab(0);
    update_ab_buttons();
  }
  
  void on_ab_b_toggled(bool val){
    if (val && !initing)
      MW_change_ab(1);
    update_ab_buttons();
  }
  
  void on_ab_c_toggled(bool val){
    if (val && !initing)
      MW_change_ab(2);
    update_ab_buttons();
  }
  
  void on_ab_d_toggled(bool val){
    if (val && !initing)
      MW_change_ab(3);
    update_ab_buttons();
  }
  
  void on_ab_e_toggled(bool val){
    if (val && !initing)
      MW_change_ab(4);
    update_ab_buttons();
  }
  
  void on_ab_f_toggled(bool val){
    if (val && !initing)
      MW_change_ab(5);
    update_ab_buttons();
  }
  
  void on_ab_g_toggled(bool val){
    if (val && !initing)
      MW_change_ab(6);
    update_ab_buttons();
  }
  
  void on_ab_h_toggled(bool val){
    if (val && !initing)
      MW_change_ab(7);
    update_ab_buttons();
  }
  
  void on_ab_reset_clicked(){
    MW_reset_ab(-1);
    update_ab_buttons();
  }

  void on_show_modular_toggled(bool val){
    if (initing)
      return;

    pauseUpdatesALittleBit();
    //setUpdatesEnabled(false);

    if (val){
      
      if(_mixer_strips_gui != -1){
        mixer_layout->removeWidget(API_gui_get_widget(_mixer_strips_gui));
        gui_close(_mixer_strips_gui);
        _mixer_strips_gui = -1;
      }
      show_modular_mixer_widgets(true);
      
    } else {
      
      _mixer_strips_gui = createMixerStripsWindow(_num_rows);
      if (_mixer_strips_gui != -1){
        show_modular_mixer_widgets(false);
        QWidget *w = API_gui_get_widget(_mixer_strips_gui);
        //w->setFixedSize(width(), height()-50);
        mixer_layout->addWidget(w);
        mixer_layout->update();
        verticalLayout->update();
        updateGeometry();
      }
      
    }
    
    //setUpdatesEnabled(true); // It's a flaw in Qt that we need to call this function. And it doesn't even work very well.
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
    GFX_showMixerHelpWindow();
  }

  void on_mixerstrips_help_clicked(){
    GFX_showMixerStripsHelpWindow();
  }

};


void MW_set_rotate(float rotate){
  g_mixer_widget2->set_rotate(rotate);
}

void MW_update_mixer_widget(void){
  g_mixer_widget2->update_ab_buttons();
  g_mixer_widget2->update();
}

#if 0
extern "C"{ void GFX_showHideMixerWidget(void);}

void GFX_showHideMixerWidget(void){
  if(g_view!=NULL)
    g_view->hide();
}
#endif

