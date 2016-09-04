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

extern EditorWidget *g_editor;


class Mixer_widget : public QWidget, public Ui::Mixer_widget{
  Q_OBJECT

 public:
  bool initing;

  qreal _rotate;

  qreal _middle_zoom;
  
 Mixer_widget(QWidget *parent=NULL)
    : QWidget(parent)
    , _rotate(0)
    , _middle_zoom(1.0)
  {
    initing = true;
    setupUi(this);
    g_zoom_slider = zoom_slider;
    initing = false;

    // Tro to find default zoom level based on system font
    QFont font = g_editor->main_window->font();
    _middle_zoom = 230 - (font.pointSize()-12) * 4.0;
    
    on_zoom_slider_valueChanged(zoom_slider->value());
  }

  void set_rotate(qreal rotate){
    _rotate = rotate;

    qreal scale = qPow(qreal(2), (zoom_slider->value() - 250) / qreal(50));

    QMatrix matrix;
    matrix.scale(scale, scale);
    matrix.rotate(_rotate);

    view->setMatrix(matrix);    
  }

  public slots:

  void on_show_cpu_usage_toggled(bool val){
    ATOMIC_SET(g_show_cpu_usage_in_mixer, val);
    MW_update_all_chips();
  }
    
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

#if 0
extern "C"{ void GFX_showHideMixerWidget(void);}

void GFX_showHideMixerWidget(void){
  if(g_view!=NULL)
    g_view->hide();
}
#endif

