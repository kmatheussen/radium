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



// MyColor / MyPainter / MyWidget / etc. for Qt. (The same classes also exists for Juce. (Why not subclass a superclass with empty virtual functions then? Because in this case, that would only involve extra typing and no advantages.))

#ifndef QT_MYWIDGET_H
#define QT_MYWIDGET_H

#include <string>

#include "MyLineMyRect.h"

#include <QApplication>
#include <QPainter>
#include <QMouseEvent>
#include <QTimer>

#include "Qt_mix_colors.h"

extern struct Root *root;

namespace cvs{

  struct MyColor{
    QColor col;
    MyColor(int r,int g,int b)
      : col(r,g,b)
    {}

    MyColor(QColor col)
      : col(col)
    {}

    MyColor(enum ColorNums colnum)
#ifdef COMPILING_RADIUM
        : col(get_qcolor(colnum))
#else
        : col(g_colors[colnum])
#endif
    {}

    MyColor()
#ifdef COMPILING_RADIUM
      : col(get_qcolor(HIGH_BACKGROUND_COLOR_NUM))
#else
      : col(g_colors[11])
#endif
    {}

    MyColor lighter(int p){
      return MyColor(col.lighter(p));
    }

    MyColor darker(int p){
      return MyColor(col.darker(p));
    }

    void setAlpha(int a){
      col.setAlpha(a);
    }
  };

  static inline MyColor mix_mycolors(const MyColor &c1, const MyColor &c2, float how_much){
    return MyColor(mix_colors(c1.col,c2.col,how_much));
  }

  struct MyImage{
    QImage image;
    QPainter qp;
    MyImage(int width,int height)
      : image(width,height,QImage::Format_RGB32)
      , qp(&image)
    {
    }
  };

  struct MyPainter{
    QPainter *p;
    MyColor col;
    QPen pen;
    QLinearGradient gradient;
    bool use_gradient;
    int _thickness;

    MyPainter(QPainter *p) 
      : p(p) 
      , use_gradient(false)
      , _thickness(1)
    {
      p->setRenderHints(QPainter::Antialiasing,true); // This line makes no difference. Qt turns it off at some point in the program.
      p->setPen(pen);
    }

    MyPainter(MyImage *image)
      : p(&image->qp)
      , use_gradient(false)
      , _thickness(1)
    {
      p->setPen(pen);
    }

      ~MyPainter(){
        p->setPen(QPen(QBrush(),1));
      }

    void setGradient(int x1, int y1, int x2, int y2, const MyColor &c1, const MyColor &c2){
      gradient = QLinearGradient(x1,y1,x2,y2);
      gradient.setColorAt(0,c1.col);
      gradient.setColorAt(1,c2.col);
      p->setPen(QPen(QBrush(gradient),_thickness));
      use_gradient = true;
    }

    void unsetGradient(){
      use_gradient=false;
    }

    void drawImage(int x,int y,MyImage *image){
      p->drawImage(x,y,image->image);
    }

    void drawRect(const int x1,const int y1,const int x2,const int y2, const MyColor &col){
      if(use_gradient==false){
        pen.setColor(col.col);
        p->setPen(pen);
      }
      p->drawRect(x1,y1,x2-x1,y2-y1);
    }
    void drawRect(const MyRect &rect,const MyColor &col){
      drawRect(rect.x1,rect.y1,rect.x2,rect.y2,col);
    }


    void fillRect(const double x1,const double y1,const double x2,const double y2, const MyColor &col){
      QRectF rect(x1,y1,x2-x1,y2-y1);


      if(use_gradient==false)
        p->setBrush(col.col);
      else
        p->setBrush(gradient);

      QPen old_pen = p->pen();

      p->setPen(Qt::NoPen);
      p->drawRect(rect);

      p->setPen(old_pen);
      p->setBrush(Qt::NoBrush);

          /*

      if(use_gradient==false)
        p->fillRect(x1,y1,x2-x1,y2-y1,col.col);
      else
        p->fillRect(x1,y1,x2-x1,y2-y1,QBrush(gradient));
          */
    }

    void fillRect(const MyRect &rect,const MyColor &col){
      fillRect((double)rect.x1,(double)rect.y1,(double)rect.x2,(double)rect.y2,col);
    }

    void setThickness(int thickness){
      _thickness=thickness;
      pen.setWidth(thickness);
    }

    void drawLine(const double x1,const double y1,const double x2,const double y2, const MyColor &col){
      if(use_gradient==false){
        pen.setColor(col.col);
        p->setPen(pen);
      }

      //bool anti = x1!=x2 && y1!=y2;

      //if(anti)
      p->setRenderHints(QPainter::Antialiasing,true); // I have no idea why, but antialiasing is turned off at some point. Must be a bug in Qt.

      p->drawLine(x1,y1,x2,y2);

      //if(anti)
      //p->setRenderHints(QPainter::Antialiasing,false);
    }

    void drawLine(const MyLine &l, const MyColor &col){
      drawLine((double)l.x1,(double)l.y1,(double)l.x2,(double)l.y2, col);
    }

    int getTextWidth(std::string text){
      const QFontMetrics fn = QFontMetrics(QFont());
      return fn.boundingRect(QString::fromStdString(text)).width();
    }

    int getTextHeight(){
      const QFontMetrics fn = QFontMetrics(QFont());
      return fn.height();
    }

    void drawText(const MyRect &rect, std::string text, const MyColor &col){
      if(use_gradient==false){
        pen.setColor(col.col);
        p->setPen(pen);
      }

      p->drawText(rect.x1,rect.y1,rect.width(),rect.height(),
                  //Qt::AlignTop
                  Qt::AlignVCenter|Qt::AlignLeft,
                  QString::fromStdString(text));
    }

    void drawVerticalText(int x, int y, std::string text, const MyColor &col){
      
      if(use_gradient==false){
        pen.setColor(col.col);
        p->setPen(pen);
      }
      p->save();
      QPoint point(x,y); 
      p->translate(x,0);//point);//x, y);
      p->rotate(90); // or 270
      //p->scale((box.x2-box.x1)/12,1.0);
      //point = p->xFormDev(point);
      p->drawText(5,-5,QString::fromStdString(text));
      p->restore();
      
    }
  };

  struct MyWidget {
    struct MyQWidget2 : public QWidget{

      MyWidget *mywidget;
      MyQWidget2(QWidget *parent, MyWidget *mywidget) : QWidget(parent),mywidget(mywidget){
      }

      void mousePressEvent ( QMouseEvent * event ) override {
        if(mywidget->mousePress(event->x(),event->y()))
          event->accept();
      }
      void mouseMoveEvent ( QMouseEvent * event ) override {
        if(mywidget->mouseMove(event->x(),event->y()))
          event->accept();
      }
      void mouseReleaseEvent ( QMouseEvent * event ) override {
        if(mywidget->mouseRelease(event->x(),event->y()))
          event->accept();
      }
      void paintEvent ( QPaintEvent * ev ) override {
        TRACK_PAINT();

        RETURN_IF_DATA_IS_INACCESSIBLE();
        
        QPainter qp(this);
        MyPainter p(&qp);

        //qp.setRenderHints(QPainter::Antialiasing,true);

        mywidget->repaint(&p);
      }
      void resizeEvent ( QResizeEvent * event ) override {
        radium::ScopedResizeEventTracker resize_event_tracker;
        RETURN_IF_DATA_IS_INACCESSIBLE();
        mywidget->resized();
      }
    };

    MyQWidget2 *w;

    MyWidget(void *parent)
      : w(new MyQWidget2(static_cast<QWidget*>(parent),this))
    {}

	  virtual ~MyWidget() = default;
	  
    void update(int x1,int y1,int x2,int y2){
      w->update(x1,y1,x2-x1,y2-y1);
    }
    void update(const MyRect &r){
      update(r.x1,r.y1,r.x2,r.y2);
    }

    void update(){
      w->update();
    }

    int width(){return w->width();}
    int height(){return w->height();}

    bool isVisible(){return w->isVisible();}
    bool isEnabled(){
      //printf("w is %s\n",w.isEnabled()?"enabled":"disabled");
      return w->isEnabled();
    }

    bool ctrlPressed(void){
      return QApplication::keyboardModifiers() & Qt::ControlModifier;
    }
    
    virtual void resized() = 0;

    //virtual void repaint(MyPainter *p){}
    virtual void repaint(MyPainter *p) = 0;

    virtual bool mousePress(int x, int y) = 0;

    virtual bool mouseMove(int x, int y) = 0;

    virtual bool mouseRelease(int x, int y) = 0;
  };

  // Used by the compressor
  struct MyTimer{

    struct MyQTimer : public QTimer{
      MyTimer *mytimer;

      MyQTimer(MyTimer *mytimer) : mytimer(mytimer) {}

      void timerEvent(QTimerEvent * e) override { // virtual method from QTimer
        RETURN_IF_DATA_IS_INACCESSIBLE();
        mytimer->timer();
      }
    };

    MyQTimer *qtimer;

    MyTimer()
      : qtimer(new MyQTimer(this))
    {}

    void prepare_for_deletion(void){
      qtimer->stop(); // Is it okay to call this one without first calling start?
    }
    
    virtual ~MyTimer(){
      prepare_for_deletion();
    }

    virtual void timer() = 0;

    void startTimer(int interval_in_milliseconds){
      qtimer->setInterval(interval_in_milliseconds);
      qtimer->start();
    }
  };

} // namespace cvs

#endif // QT_MYWIDGET_H

