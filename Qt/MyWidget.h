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



#include <qapplication.h>
#include <qpushbutton.h>
#include <qcanvas.h>
#include <qpixmap.h>
#include <qmenubar.h>

#define GFX_DONTSHRINK
#include "../common/nsmtracker.h"
#include "../common/windows_proc.h"

#include "../common/visual_proc.h"

#define INITIALPOOLSIZE 64

class RPoints{
public:
  RPoints();
  ~RPoints();

  void addPoint(int x,int y);
  void drawPoints(QPainter *qp);

private:
  QPointArray *qpa;
  uint num_points;
};

class MyWidget : public QWidget
{
public:
  MyWidget(struct Tracker_Windows *window, QWidget *parent=0, const char *name=0 );
  ~MyWidget();

    QColor     colors[8];				// color array
    RPoints    *rpoints[8];
    QPixmap    *qpixmap;
    QPixmap    *cursorpixmap;

    //void timerEvent(QTimerEvent *);

protected:
    //    bool        event(QEvent *);
    void	paintEvent( QPaintEvent * );
#if 0
    void        keyPressEvent(QKeyEvent *);
    void        keyReleaseEvent(QKeyEvent *qkeyevent);
#endif
    void	mousePressEvent( QMouseEvent *);
    void	mouseReleaseEvent( QMouseEvent *);
    void	mouseMoveEvent( QMouseEvent *);
    void        resizeEvent( QResizeEvent *);
    void        closeEvent(QCloseEvent *);
    void        wheelEvent(QWheelEvent *);
    void        customEvent(QCustomEvent *);
private:
    QPoint     *points;				// point array

    int		count;				// count = number of points
    bool	down;				// TRUE if mouse down

    struct Tracker_Windows *window;
};

