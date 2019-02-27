
#ifndef _RADIUM_QT_QT_MYQSCROLLBAR_HPP
#define _RADIUM_QT_QT_MYQSCROLLBAR_HPP

#include <QScrollBar>

#include "Qt_colors_proc.h"

extern int g_default_slider_height;

struct Qt_MyQScrollBar : public QScrollBar, public radium::MouseCycleFix {

  Qt_MyQScrollBar(QWidget *parent = nullptr)
    : QScrollBar(parent)
  {
    init();
  }
      
  Qt_MyQScrollBar(Qt::Orientation orientation, QWidget *parent = nullptr)
    : QScrollBar(orientation, parent)
  {
    init();
  }

  void init(void){
    setCursor(Qt::OpenHandCursor);
  }

  QSize getSize(int pref_size) const {
    int width = g_default_slider_height; //root->song->tracker_windows->bottomslider_height;
    
    if (orientation()==Qt::Vertical)
      return QSize(width, pref_size);
    else {
      return QSize(pref_size, width);
    }
  }
  
  QSize sizeHint() const override {
    QSize prefSize = QScrollBar::sizeHint();

    if (orientation()==Qt::Vertical)
      return getSize(prefSize.height());
    else
      return getSize(prefSize.width());
  }
  
  QSize minimumSizeHint() const override {
    QSize prefSize = QScrollBar::minimumSizeHint();

    if (orientation()==Qt::Vertical)
      return getSize(prefSize.height());
    else
      return getSize(prefSize.width());
  }

  bool _is_moving = false;
  int _start_pos;
  int _start_val;

  const float b = 2;

  
  void fix_mousePressEvent(QMouseEvent *event) override{
    event->accept();
    
    if (orientation()==Qt::Vertical)
      _start_pos = event->pos().y();
    else
      _start_pos = event->pos().x();

    _start_val = value();

    setCursor(Qt::ClosedHandCursor);
    
    _is_moving = true;
    update();
  }
  void fix_mouseMoveEvent(QMouseEvent *event) override{
    event->accept();
    if (_is_moving){
      int dx;
      if (orientation()==Qt::Vertical)
        dx = event->pos().y() - _start_pos;
      else
        dx = event->pos().x() - _start_pos;

      const int page_step = pageStep();

      //const int val = value();
      const int min_val = minimum();
      const int max_val = maximum();

      const int dx_width = (orientation()==Qt::Vertical ? height() : width()) - b*2;
      
      if(dx_width==0){
        printf("dx_width: %d\n", dx_width);
        return;
      }
      
      setValue(_start_val + 
               scale(dx,
                     0, dx_width,
                     0, max_val-min_val+page_step)
               );
    }
  }
  void fix_mouseReleaseEvent(radium::MouseCycleEvent &event) override{    
    event.accept();
    _is_moving = false;
    
    setCursor(Qt::OpenHandCursor);
    
    update();
  }

  MOUSE_CYCLE_CALLBACKS_FOR_QT;
  
  void paintEvent ( QPaintEvent * ev ) override {
    TRACK_PAINT();

    QPainter p(this);

    const int page_step = pageStep();

    const int val = value();
    const int min_val = minimum();
    const int max_val = maximum();

    p.fillRect(0,0,width(),height(),  get_qcolor(HIGH_BACKGROUND_COLOR_NUM));

    /*
    QColor background = QWidget::palette().color(QWidget::backgroundRole());
    float lightness = background.lightnessF();
    float inv_lightness = lightness < 0.3 ? lightness+0.2 : lightness-0.2;
    QColor foreground(lightness*255,lightness*255,lightness*255);

    printf("lightneess: %f. Inv: %f\n", background.lightnessF(), inv_lightness);
    */

    QColor foreground = get_qcolor(SCROLLBAR_COLOR_NUM);//(70,60,20);
    //foreground.setAlpha(128);

    QPen pen(foreground);
    pen.setWidth(b);
    p.setPen(foreground);
    p.drawRect(0, 0, width()-b/2, height()-b/2);
    
    float x1 = b;
    float x2 = width()-b;
    float y1 = b;
    float y2 = height()-b;

    float xy1 = orientation()==Qt::Vertical ? y1 : x1;
    float xy2 = orientation()==Qt::Vertical ? y2 : x2;

    if(max_val+page_step-min_val==0){
      printf("gakk1: %d\n",max_val+page_step-min_val);
      return;
    }
    
    const float slider_xy1 = scale(val,
                                   min_val, max_val+page_step,
                                   xy1, xy2);
    const float slider_xy2 = scale(val + page_step,
                                   min_val, max_val+page_step,
                                   xy1, xy2);

    if (_is_moving)
      foreground = foreground.darker(150);

    if (orientation()==Qt::Vertical) {
      
      myFillRect(p, QRectF(x1, slider_xy1, x2-x1, slider_xy2-slider_xy1), foreground);
      
    } else {
      
      myFillRect(p, QRectF(slider_xy1, y1, slider_xy2-slider_xy1, y2-y1), foreground);
      
    }
    
  }
};


#endif
