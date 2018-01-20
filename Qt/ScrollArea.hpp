
#ifndef _RADIUM_QT_SCROLLAREA_HPP
#define _RADIUM_QT_SCROLLAREA_HPP

#include <stdio.h>

#include <QWidget>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QScrollBar>
#include <QScrollArea>
#include <QResizeEvent>
#include <QTimer>

#if TEST_MAIN
#include <QMouseEvent>
#include <QPushButton>
#endif

extern bool g_pause_scroll_area_updates_when_resizing;

namespace radium{

// Replacement/wrapper for QScrollArea. It's too hard trying to force QScrollArea to do what I want.
//
class ScrollArea : public QWidget {

  QVBoxLayout *_vertical_layout;
  QHBoxLayout *_horizontal_layout;

  Qt::ScrollBarPolicy _vertical_scrollbar_policy = Qt::ScrollBarAsNeeded;
  Qt::ScrollBarPolicy _horizontal_scrollbar_policy = Qt::ScrollBarAsNeeded;

  struct MyQScrollBar;

  MyQScrollBar *_vertical_scroll_bar;
  MyQScrollBar *_horizontal_scroll_bar;

  struct InnerScrollArea : public QScrollArea {

    ScrollArea *_scroll_area;

    struct Widget : public QWidget {

      struct InnerScrollArea *_inner_scroll_area;

      Widget(InnerScrollArea *inner_scroll_area)
        : QWidget(inner_scroll_area)
        , _inner_scroll_area(inner_scroll_area)
      {
      }

      void resizeEvent(QResizeEvent * event) override {
        //fprintf(stderr, " Widget resized. width: %d, height: %d\n", event->size().width(), event->size().height());
        _inner_scroll_area->_scroll_area->updateScrollbars();
      }

      int getPreferredWidth(void){
        return childrenRect().width();
      }

      int getPreferredHeight(void){
        return childrenRect().height();
      }

    };

    Widget *_widget;

    InnerScrollArea(ScrollArea *scroll_area)
      : QScrollArea(scroll_area)
      , _scroll_area(scroll_area)
    {
      _widget = new Widget(this);
      setWidget(_widget);
      setWidgetResizable(true);
      setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
      setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
    }

    void resizeEvent(QResizeEvent * event) override {
      //fprintf(stderr, " InnerScrollArea resized. width: %d, height: %d\n", event->size().width(), event->size().height());

      if(_scroll_area->_vertical_scrollbar_policy==Qt::ScrollBarAlwaysOff){
        _widget->setMinimumHeight(height());
        _widget->setMaximumHeight(height());
      }

      if(_scroll_area->_horizontal_scrollbar_policy==Qt::ScrollBarAlwaysOff){
        _widget->setMinimumWidth(width());
        _widget->setMaximumWidth(width());
      }

      _scroll_area->updateScrollbars();
      _widget->adjustSize();
      if (_widget->layout() != NULL)
        _widget->layout()->update(); // If not, content don't always update.
    }

  };

  InnerScrollArea *_inner_scroll_area;

  void updateScrollbars(void){

    //QWidget *widget = getWidget();

    // vertical scroll bar
    {
      int outer_height = _inner_scroll_area->height();
      int inner_height = _inner_scroll_area->_widget->getPreferredHeight();
      int maximum = inner_height - outer_height;

      _vertical_scroll_bar->setMinimum(0);
      _vertical_scroll_bar->setMaximum(maximum);
      _vertical_scroll_bar->setPageStep(outer_height);

      //printf("                Vertical outer: %d. inner: %d. maximum: %d\n", outer_height, inner_height, maximum);

      if (_vertical_scrollbar_policy==Qt::ScrollBarAlwaysOff)
        _vertical_scroll_bar->hide();
      else if (_vertical_scrollbar_policy==Qt::ScrollBarAlwaysOn)
        _vertical_scroll_bar->show();
      else if (maximum <= 0)
        _vertical_scroll_bar->hide();
      else
        _vertical_scroll_bar->show();
    }

    // horizontal scroll bar
    {
      int outer_width = _inner_scroll_area->width();
      int inner_width = _inner_scroll_area->_widget->getPreferredWidth(); //width();
      int maximum = inner_width - outer_width;

      _horizontal_scroll_bar->setMinimum(0);
      _horizontal_scroll_bar->setMaximum(maximum);
      _horizontal_scroll_bar->setPageStep(outer_width);
      //printf("   Horizontal max: %d\n", inner_width - outer_width);

      //printf("                Horizontal outer: %d. inner: %d. maximum: %d\n", outer_width, inner_width, maximum);

      if (_horizontal_scrollbar_policy==Qt::ScrollBarAlwaysOff)
        _horizontal_scroll_bar->hide();
      else if (_horizontal_scrollbar_policy==Qt::ScrollBarAlwaysOn)
        _horizontal_scroll_bar->show();
      else if (maximum <= 0)
        _horizontal_scroll_bar->hide();
      else
        _horizontal_scroll_bar->show();
    }

    //widget->adjustSize();

    //printf("   x: %d, y: %d, width: %d, height: %d\n", widget->x(), widget->y(), widget->width(), widget->height());
  }

  struct MyQScrollBar : public QScrollBar {

    ScrollArea *_scroll_area;

    MyQScrollBar(Qt::Orientation orientation, ScrollArea *scroll_area)
      : QScrollBar(orientation, scroll_area)
      , _scroll_area(scroll_area)
    {
    }

    void sliderChange(SliderChange change) override {
      if (change==QAbstractSlider::SliderValueChange) {
        //printf("New value: %d\n", value());
        if (orientation()==Qt::Vertical){
          //_scroll_area->_inner_scroll_area->move(_scroll_area->_inner_scroll_area->x(), -value());
          _scroll_area->_inner_scroll_area->verticalScrollBar()->setValue(value());
        }else if (orientation()==Qt::Horizontal){
          //printf("   Setting horizontal value\n");
          //_scroll_area->_inner_scroll_area->move(-value(), _scroll_area->_inner_scroll_area->y());
          _scroll_area->_inner_scroll_area->horizontalScrollBar()->setValue(value());
        }
      }

      QScrollBar::sliderChange(change);
    }
  };

#if 1  
  struct MyTimer : public QTimer{
    ScrollArea *_scroll_area;
    QTime time;

    MyTimer(ScrollArea *scroll_area)
      : _scroll_area(scroll_area)
      {
        setInterval(15);
      }

    void timerEvent(QTimerEvent *e) override {
      if (time.elapsed() > 15){ // Singleshot is more messy since 1. we don't 'moc' this file, and 2. we might get deleted at any time so using lambda/etc. leads to memory corruption unless we are careful.
        _scroll_area->setUpdatesEnabled(true);
        //printf("TimerEvent ____ gakkgakk %p\n",_scroll_area);
        
        _scroll_area->updateScrollbars();
        _scroll_area->getWidget()->adjustSize();
        if (_scroll_area->getWidget()->layout() != NULL)
          _scroll_area->getWidget()->layout()->update(); // If not, content don't always update.

        stop();
      }
    }

    void startit(void){
      time.restart();
      if (!isActive()){
        start();
      }
    }
  };

  MyTimer _mytimer;
#endif

  void pauseUpdatesALittleBit(void){
    if(g_pause_scroll_area_updates_when_resizing)
      setUpdatesEnabled(false);     // <--- This causes flickering when resizing.
    _mytimer.startit();
  }

public:

  void resizeEvent(QResizeEvent * event) override {
    pauseUpdatesALittleBit();
  }


  ScrollArea(QWidget *parent = NULL)
    : QWidget(parent)
    , _mytimer(this)
  {
    _inner_scroll_area = new InnerScrollArea(this);
    _vertical_scroll_bar = new MyQScrollBar(Qt::Vertical, this);
    _horizontal_scroll_bar = new MyQScrollBar(Qt::Horizontal, this);

    _horizontal_layout = new QHBoxLayout;
    _vertical_layout = new QVBoxLayout;

    _horizontal_layout->setSpacing(0);
    _vertical_layout->setSpacing(0);

    _horizontal_layout->setContentsMargins(0,0,0,0);
    _vertical_layout->setContentsMargins(0,0,0,0);

    _horizontal_layout->addWidget(_inner_scroll_area);
    _horizontal_layout->addWidget(_vertical_scroll_bar);

    _vertical_layout->addLayout(_horizontal_layout);
    _vertical_layout->addWidget(_horizontal_scroll_bar);

    setLayout(_vertical_layout);

    updateScrollbars();
  }

  void setVerticalScrollBarPolicy(Qt::ScrollBarPolicy policy){
    _vertical_scrollbar_policy = policy;

    if(_vertical_scrollbar_policy==Qt::ScrollBarAlwaysOff)
      _inner_scroll_area->_widget->setSizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::Fixed);

    updateScrollbars();
  }

  void setHorizontalScrollBarPolicy(Qt::ScrollBarPolicy policy){
    _horizontal_scrollbar_policy = policy;

    if(_horizontal_scrollbar_policy==Qt::ScrollBarAlwaysOff)
      _inner_scroll_area->_widget->setSizePolicy(QSizePolicy::Fixed,QSizePolicy::MinimumExpanding);

    updateScrollbars();
  }

  QScrollBar *verticalScrollBar(void){
    return _vertical_scroll_bar;
  }

  QScrollBar *horizontalScrollBar(void){
    return _horizontal_scroll_bar;
  }

  QWidget *getWidget(void){
    return _inner_scroll_area->widget();
  }

#ifdef TEST_MAIN
  void mousePressEvent(QMouseEvent *qmouseevent) override {
    QPushButton *push = new QPushButton(this);
    push->setMinimumHeight(30);
    push->setMinimumWidth(300);
    getWidget()->layout()->addWidget(push);
  }
#endif

};


} // namespace radium

#ifdef TEST_MAIN



/*
echo '#include "ScrollArea.hpp"' >gakk.cpp
g++ `pkg-config --cflags --libs Qt5Gui Qt5Widgets` gakk.cpp -Wall -fPIC -DTEST_MAIN -std=gnu++11 && ./a.out
*/

#include <QApplication>
#include <QLabel>

int main(int argc, char **argv){

  QApplication app(argc,argv);

  radium::ScrollArea *scroll_area = new radium::ScrollArea;

  QWidget *main_widget = scroll_area->getWidget();

  QGridLayout *layout = new QGridLayout;

  QLabel *label = new QLabel("hello hello\n\n\nhello.");
  layout->addWidget(label);

  main_widget->setLayout(layout);
  scroll_area->show();

  return app.exec();
}


#endif // TEST_MAIN


#endif // _RADIUM_QT_SCROLLAREA_HPP
