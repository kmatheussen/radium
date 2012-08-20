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
#include <qpointarray.h>
#include <qlabel.h>
#endif

#ifdef USE_QT4
//Added by qt3to4:
#include <QWidget>
#include <QCustomEvent>
#include <QPaintEvent>
#include <QResizeEvent>
#include <QMouseEvent>
#include <QKeyEvent>
#include <QWheelEvent>
#include <QCloseEvent>
#include <Q3PointArray>
#include <QFrame>
#include <QLabel>
#include <QMenuBar>
#include <QApplication>
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
#define XOFFSET 5
#define YOFFSET 2

class QMainWindow;
class QSplitter;

class EditorWidget : public QWidget //QFrame
//class EditorWidget : public QtXEmbedContainer //QWidget //QFrame
{
public:
  EditorWidget(QWidget *parent=0, const char *name="no name" );
  ~EditorWidget();

#if USE_QT_VISUAL && USE_QT4
  const QPaintEngine* paintEngine();
#endif

    QColor     colors[16];				// color array

#if USE_QT_VISUAL
    QPixmap    *qpixmap;
    QPixmap    *cursorpixmap;
#endif

    //void timerEvent(QTimerEvent *);

    struct Tracker_Windows *window; // Not sure if this one is used.

    QMainWindow *main_window;

#if USE_QT_VISUAL
    QPainter *painter; // Set in paintEvent
    QPainter *qpixmap_painter; // Set in paintEvent
    QPainter *cursorpixmap_painter; // Set in paintEvent

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

    Q3PointArray qpa;

protected:
    //    bool        event(QEvent *);
#if 1 //USE_QT_VISUAL
    void	paintEvent( QPaintEvent * );
#endif

#if 0 // Using X11filter for keys
    void        keyPressEvent(QKeyEvent *);
    void        keyReleaseEvent(QKeyEvent *qkeyevent);
#endif

    void        wheelEvent(QWheelEvent *);

#if USE_QT_VISUAL
    void	mousePressEvent( QMouseEvent *);
    void	mouseReleaseEvent( QMouseEvent *);
    void	mouseMoveEvent( QMouseEvent *);
#endif
    void        resizeEvent( QResizeEvent *);
    void        closeEvent(QCloseEvent *);
    void        customEvent(QEvent *);
};

#endif

