/* Copyright 2003 Kjetil S. Matheussen

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

#ifndef EDITOR_WIDGET_H
#define EDITOR_WIDGET_H

#ifdef USE_QT3
#include <qmenubar.h>
#include <qmainwindow.h>
#include <qslider.h>
#include <qtimer.h>

#  if USE_GTK_VISUAL
#    include "qtxembed-1.3-free/src/qtxembed.h"
#  endif

#include <qapplication.h>
#include <qpixmap.h>
#include <qimage.h>
#include <qpointarray.h>
#include <qlabel.h>
#endif // USE_QT3

#ifdef USE_QT4
//Added by qt3to4:
#include <QWidget>
#include <QLabel>
#include <Q3PointArray>
#include <QPainter>
#ifdef FOR_WINDOWS
#  include <QKeyEvent>
#endif
#endif

#ifdef USE_QT3
# define Q3PointArray QPointArray
# define Q3PopupMenu QPopupMenu
#endif

#define GFX_DONTSHRINK
#include "../common/nsmtracker.h"
#include "../common/windows_proc.h"

#include "../common/visual_proc.h"
#include "../common/visual_op_queue_proc.h"


// Don't paint on the frame.
//#define XOFFSET 5
//#define YOFFSET 2

#define NUM_LINESBUFFERS 8

class QMainWindow;
class QSplitter;

#if USE_QIMAGE_BUFFER
typedef QImage PaintBuffer;
#else
typedef QPixmap PaintBuffer;
#endif

class EditorWidget : public QWidget //QFrame
//class EditorWidget : public QtXEmbedContainer //QWidget //QFrame
{

public:
  EditorWidget(QWidget *parent=0, const char *name="no name" );
  ~EditorWidget();

#if USE_QT_VISUAL && USE_QT4
  //const QPaintEngine* paintEngine();
#endif

    QColor     colors[16];				// color array

#if USE_QT_VISUAL
    PaintBuffer *paintbuffer;
    PaintBuffer *cursorbuffer;
    PaintBuffer *linesbuffer[NUM_LINESBUFFERS];
#endif

    //void timerEvent(QTimerEvent *);

    struct Tracker_Windows *window; // Not sure if this one is used.

    QMainWindow *main_window;

#if USE_QT_VISUAL
    QPainter *painter;
    QPainter *paintbuffer_painter;
    QPainter *cursorbuffer_painter;
    QPainter *linesbuffer_painter[NUM_LINESBUFFERS];

    QFont font;
#endif

    //QFrame *status_frame;
    QLabel *status_label;

    QSplitter *xsplitter;
    QSplitter *ysplitter;

    int get_editor_width(){
      //return this->width()-XOFFSET-2; // Fine tuned. No logical reason behind it. (2 is probably just the frame border width)
      return this->width();
    }

    int get_editor_height(){
      //return this->height()-YOFFSET-2; // Fine tuned. No logical reason behind it. (2 is probably just the frame border width)
      return this->height();
    }

    void start_vertical_blank_callback();

    Q3PointArray qpa;

#if USE_QT_VISUAL && USE_QIMAGE_BUFFER
    void init_buffers(){
       const QImage::Format image_format = QImage::Format_RGB32;

       if(this->paintbuffer==NULL || this->cursorbuffer==NULL || this->paintbuffer->width()<this->width() || this->paintbuffer->height()<this->height()){
         delete this->paintbuffer_painter;
         delete this->cursorbuffer_painter;
         for(int i=0;i<NUM_LINESBUFFERS;i++)
           delete this->linesbuffer_painter[i];

         delete this->paintbuffer;
         delete this->cursorbuffer;
         for(int i=0;i<NUM_LINESBUFFERS;i++)
           delete this->linesbuffer[i];
         this->paintbuffer = new QImage(this->width(), this->height(), image_format);
         this->cursorbuffer = new QImage(this->width(), this->height(), image_format);
         for(int i=0;i<NUM_LINESBUFFERS;i++)
           this->linesbuffer[i] =  new QImage(this->width(), this->height(), image_format);

         this->paintbuffer_painter = new QPainter(this->paintbuffer);
         this->cursorbuffer_painter = new QPainter(this->cursorbuffer);
         for(int i=0;i<NUM_LINESBUFFERS;i++) {
           this->linesbuffer_painter[i] = new QPainter(this->linesbuffer[i]);
           this->linesbuffer_painter[i]->setRenderHints(QPainter::Antialiasing,true);
         }

         this->paintbuffer_painter->setFont(this->font);
         for(int i=0;i<NUM_LINESBUFFERS;i++)
           this->linesbuffer_painter[i]->setFont(this->font);
       }

#if 1
      {
        this->cursorbuffer_painter->fillRect(0,0,this->width(),this->height(),this->colors[15]);
      }
#if 0
      {
        this->paintbuffer_painter->fillRect(0,0,this->width(),this->height(),this->colors[0]);
      }
#endif
#endif
    }

#else
    void init_buffers(){
#if USE_QT_VISUAL
       if(this->paintbuffer==NULL || this->cursorbuffer==NULL){
          this->paintbuffer=new QPixmap(editor->width(),editor->height());
          this->cursorbuffer=new QPixmap(editor->width(),editor->height());
#ifdef USE_QT3
          this->paintbuffer->setOptimization(QPixmap::BestOptim);
          this->cursorbuffer->setOptimization(QPixmap::BestOptim);
#endif
       }else{
          this->paintbuffer->resize(this->width(), this->height());
          this->cursorbuffer->resize(this->width(), this->height());
       }
#endif
    }
#endif

protected:
    //    bool        event(QEvent *);
#if 1 //USE_QT_VISUAL
    void	paintEvent( QPaintEvent * );
#endif

#if 0 // Using X11filter for keys
    void        keyPressEvent(QKeyEvent *);
    void        keyReleaseEvent(QKeyEvent *qkeyevent);
#endif

#if 0
#ifdef FOR_WINDOWS
    void keyPressEvent(QKeyEvent *qkeyevent){
      qkeyevent->accept();
    }
    void keyReleaseEvent(QKeyEvent *qkeyevent){
      qkeyevent->accept();
    }
#endif
#endif

#if USE_QT_VISUAL
    void	mousePressEvent( QMouseEvent *);
    void	mouseReleaseEvent( QMouseEvent *);
    void	mouseMoveEvent( QMouseEvent *);
#endif
    void        resizeEvent( QResizeEvent *);
    void        closeEvent(QCloseEvent *);
};

#endif

