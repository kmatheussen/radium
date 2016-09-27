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
  
 Mixer_widget(QWidget *parent=NULL)
    : QWidget(parent)
    , _rotate(0)
    , _middle_zoom(1.0)
    , _mixer_direction_menu(this)
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

    //connect(ab_a, SIGNAL(rightClicked()), this, SLOT(on_ab_a_rightClicked()));
      
    g_mixer_widget2 = this;
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
    }
  }

  void ab_rightclicked(int num){
    if (popupMenu(talloc_format("%sReset",MW_get_curr_ab()==num?"[disabled]":""))==0){
      MW_reset_ab(num);
      update_ab_buttons();
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
  
  void on_show_cpu_usage_toggled(bool val){
    ATOMIC_SET(g_show_cpu_usage_in_mixer, val);
    MW_update_all_chips();
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

};


void MW_set_rotate(float rotate){
  g_mixer_widget2->set_rotate(rotate);
}


#if 0
extern "C"{ void GFX_showHideMixerWidget(void);}

void GFX_showHideMixerWidget(void){
  if(g_view!=NULL)
    g_view->hide();
}
#endif

