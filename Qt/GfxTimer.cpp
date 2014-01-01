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




#include <stdio.h>

#include <QGLWidget>
#include <QTimer>
#include <QTime>

#if GFXTIMER_TESTING
#  define LANGSPEC "C"
#else
  #include "../common/nsmtracker.h"
#endif

#include "GfxTimer_proc.h"



static QTime dastime;

namespace{
class VBlankCallback : public QGLWidget {

  VBlank_callback _function;
  void *_data;

  QGLFormat desiredFormat(){
    QGLFormat fmt;
    fmt.setSwapInterval(1);
    return fmt;
  }

  virtual void initializeGL()
  {
    // Set the clear color to blue
    //glClearColor( 0.0f, 0.0f, 1.0f, 1.0f );
  }

  virtual void paintGL(){
    printf("PaintEvent. Since last: %d\n",dastime.elapsed());
    dastime.restart();

    if(_function != NULL)
      _function(_data);

    /*
    glClear( GL_COLOR_BUFFER_BIT );

    glClearColor( 0.0f, 0.0f, 1.0f, 1.0f );

    glColor3f( 1.0f, 0.0f, 0.0f );

    // Draw a filled rectangle with the current color
    glRectf(20,30,40,50);
    */

  }

  virtual void resizeGL( int w, int h ){
    /*
    if ( h == 0 )
      h = 1;
    glViewport( 0, 0, w, h);
    glMatrixMode( GL_MODELVIEW );
    glLoadIdentity();
    */
  }

public:

  VBlankCallback(VBlank_callback function, void *data)
    : QGLWidget(desiredFormat())
    , _function(function)
    , _data(data)
  {
    QTimer* timer = new QTimer( this );
    connect(timer, SIGNAL(timeout()), this, SLOT(updateGL()));
    //connect(timer, SIGNAL(timeout()), this, SLOT(step()));

    if(format().swapInterval() == -1){
      // V_blank synchronization not available (tearing likely to happen)
      printf("Swap Buffers at v_blank not available: refresh at approx 60fps.");
      timer->setInterval(16);

    }else{

      // V_blank synchronization available
      printf("Swap Buffers interval: %d\n",format().swapInterval());
      timer->setInterval(0);
    }

    //show();
    timer->start();

  }
};
}

void call_function_at_vertical_blank(VBlank_callback function, void *data){
  new VBlankCallback(function, data);
}


#if GFXTIMER_TESTING
#include <QtGui/QApplication>

static void dasfunction(void *data){
  static int n = 0;
  printf("hepp. dasfunction %d\n",n++);
}

int main(int argc, char *argv[]) {

    QApplication a(argc, argv);

    call_function_at_vertical_blank(dasfunction, NULL);

    return a.exec();
}
#endif
