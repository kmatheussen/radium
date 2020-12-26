/**************************************************************************************/
/*                                                                                    */
/*  Visualization Library                                                             */
/*  http://www.visualizationlibrary.org                                               */
/*                                                                                    */
/*  Copyright (c) 2005-2010, Michele Bosi                                             */
/*  All rights reserved.                                                              */
/*                                                                                    */
/*  Redistribution and use in source and binary forms, with or without modification,  */
/*  are permitted provided that the following conditions are met:                     */
/*                                                                                    */
/*  - Redistributions of source code must retain the above copyright notice, this     */
/*  list of conditions and the following disclaimer.                                  */
/*                                                                                    */
/*  - Redistributions in binary form must reproduce the above copyright notice, this  */
/*  list of conditions and the following disclaimer in the documentation and/or       */
/*  other materials provided with the distribution.                                   */
/*                                                                                    */
/*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND   */
/*  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED     */
/*  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE            */
/*  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR  */
/*  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES    */
/*  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;      */
/*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON    */
/*  ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT           */
/*  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS     */
/*  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                      */
/*                                                                                    */
/**************************************************************************************/

#ifndef Qt5ThreadedWindow_INCLUDE_ONCE
#define Qt5ThreadedWindow_INCLUDE_ONCE

#include <vlQt5/link_config.hpp>
#include <vlCore/VisualizationLibrary.hpp>
#include <vlGraphics/OpenGLContext.hpp>
#include <vlCore/Log.hpp>

#include <QApplication>
#include <QMouseEvent>
#include <QWidget>
#include <QUrl>
#include <QTimer>
#include <QTime>
#include <QObject>
#include <QThread>
#include <QMutex>
#include <QQueue>
#include <QAtomicInt>
#include <QSemaphore>
#include <QGLWidget>
#include <QGLFormat>


namespace vlQt5
{

  class VLQT5_EXPORT Qt5ThreadedWidget : public QGLWidget {
    //Q_OBJECT

    struct Event{
      Event()
        :button(vl::LeftButton),x(0),y(0),wheel(0),unicode_ch(0),key(vl::Key_None)
      {}
        
      enum{
        MOUSEPRESS,
        MOUSERELEASE,
        MOUSEMOVE,
        WHEEL,
        KEYPRESS,
        KEYRELEASE,
        RESIZE,
        DESTROY        
      } type;
      vl::EMouseButton button;
      int x;
      int y;
      float wheel;
      unsigned short unicode_ch;
      vl::EKey key;
    };


  public:

    struct MyThread : public QThread, vl::OpenGLContext {
      Qt5ThreadedWidget *_widget;

      QMutex mutex;
      QQueue<Event> queue;

#if 0
      QAtomicInt widget_width;
      QAtomicInt widget_height;
#endif
      //QTime time;

      QSemaphore *wait_semaphore;
      
      MyThread(Qt5ThreadedWidget* widget)
        : _widget(widget)
#if 0
        , widget_width(10)
        , widget_height(10)
#endif
      {
      }

      bool handle_events(){
        mutex.lock();

        while (!queue.isEmpty()){
          Event e = queue.dequeue();
          mutex.unlock();

          switch(e.type){
          case Event::MOUSEPRESS:
            dispatchMouseDownEvent(e.button, e.x, e.y);
            break;
          case Event::MOUSERELEASE:
            dispatchMouseUpEvent(e.button, e.x, e.y);
            break;
          case Event::MOUSEMOVE:
            dispatchMouseMoveEvent(e.x,e.y);
            break;
          case Event::WHEEL:
            dispatchMouseWheelEvent(e.wheel);
            break;
          case Event::KEYPRESS:
            dispatchKeyPressEvent(e.unicode_ch,e.key);
            break;
          case Event::KEYRELEASE:
            dispatchKeyReleaseEvent(e.unicode_ch,e.key);
            break;
          case Event::RESIZE:
            dispatchResizeEvent(e.x, e.y);
            break;
          case Event::DESTROY:
            printf("Calling destroy\n");
            dispatchDestroyEvent();
            return false;
          default:
            abort();
          }

          mutex.lock();
        }

        mutex.unlock();

        return true;
      }

      void push_event(Event e){
        QMutexLocker locker(&mutex);
        queue.enqueue(e);
      }

      void update(){
        //printf("update callled\n");
      }

      void swapBuffers()
      {
        _widget->swapBuffers();
      }

      void makeCurrent()
      {
        // _widget->makeCurrent() is already called in run().
        // Calling it more often sometimes causes crashes using the catalyst driver on Linux.
        
        //_widget->makeCurrent();
      }

      void startThread(void){
        wait_semaphore = new QSemaphore;

        start();

        wait_semaphore->acquire();
        delete wait_semaphore;
      }

      void run() {
#if 0
        int width = 0;
        int height = 0;
#endif
        
        Qt5ThreadedWidget *widget = _widget;

        _widget->makeCurrent();
        //makeCurrent();

        widget->init_vl(this);

        wait_semaphore->release();
        
        //dispatchResizeEvent(2000,1024);

        while(handle_events()==true){

#if 0
          int new_height = widget_height.load();
          int new_width  = widget_width.load();
          
          if(new_height!=height || new_width!=width){
            width = new_width;
            height = new_height;
            printf("&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& resizing to %d/%d\n",width,height);
            dispatchResizeEvent(width,height);
          }
#endif
          
          //widget->swapBuffers();    
          
          dispatchRunEvent();

          //printf("width/height: %d/%d\n",width(),height());
          
          //if(time.elapsed()>18)
          //  printf("hepp %d\n",(int)time.elapsed());
          //time.restart();
          //QApplication::processEvents(QEventLoop::AllEvents, 1);
        }

        _widget->doneCurrent();
      }
    };

    MyThread *mythread;

    void is_patched(){
    }
    
    void startRendering() //override
    {
    }

    void 	paintEvent ( QPaintEvent *  ){
    }

    void 	resizeEvent ( QResizeEvent *  ) {
#if 0
      mythread->widget_width = width();
      mythread->widget_height = height();
#endif
      Event e;
      e.type=Event::RESIZE;
      e.x = width();
      e.y = height();
      mythread->push_event(e);
    }
    //virtual void 	updateGL () {}
    //virtual void 	paintGL () {}
    //virtual bool 	event ( QEvent * e ){ return true;    }

    void mouseMoveEvent(QMouseEvent* ev)
    {
      Event e;
      e.type=Event::MOUSEMOVE;
      e.x=ev->x();
      e.y=ev->y();
      mythread->push_event(e);
    }

    void mousePressEvent(QMouseEvent* ev)
    {
      vl::EMouseButton bt = vl::NoButton;
      switch(ev->button())
      {
      case Qt::LeftButton:  bt = vl::LeftButton; break;
      case Qt::RightButton: bt = vl::RightButton; break;
      case Qt::MidButton:   bt = vl::MiddleButton; break;
      default:
        bt = vl::UnknownButton; break;
      }

      Event e;
      e.type=Event::MOUSEPRESS;
      e.button = bt;
      e.x=ev->x();
      e.y=ev->y();
      mythread->push_event(e);
    }

    void mouseReleaseEvent(QMouseEvent* ev)
    {
      vl::EMouseButton bt = vl::NoButton;
      switch(ev->button())
      {
      case Qt::LeftButton:  bt = vl::LeftButton; break;
      case Qt::RightButton: bt = vl::RightButton; break;
      case Qt::MidButton:   bt = vl::MiddleButton; break;
      default:
        bt = vl::UnknownButton; break;
      }
      Event e;
      e.type=Event::MOUSERELEASE;
      e.button = bt;
      e.x=ev->x();
      e.y=ev->y();
      mythread->push_event(e);
    }

    void wheelEvent(QWheelEvent* ev)
    {
      Event e;
      e.type=Event::WHEEL;
      e.wheel = (double)ev->delta() / 120.0;
      mythread->push_event(e);
    }

    void keyPressEvent(QKeyEvent*)
    {
      unsigned short unicode_ch = 0;
      vl::EKey key = vl::Key_None;
      //translateKeyEvent(ev, unicode_ch, key);
      Event e;
      e.type = Event::KEYPRESS;
      e.key = key;
      e.unicode_ch = unicode_ch;
      mythread->push_event(e);
    }

    void keyReleaseEvent(QKeyEvent*)
    {
      unsigned short unicode_ch = 0;
      vl::EKey key = vl::Key_None;
      //translateKeyEvent(ev, unicode_ch, key);
      Event e;
      e.type = Event::KEYRELEASE;
      e.key = key;
      e.unicode_ch = unicode_ch;

      mythread->push_event(e); 
    }

    QGLFormat getQGLFormat(vl::OpenGLContextFormat vlFormat){
      QGLFormat qtFormat;
      
      qtFormat.setDoubleBuffer( vlFormat.doubleBuffer() );
        
      // color buffer
      qtFormat.setRedBufferSize( vlFormat.rgbaBits().r() );
      qtFormat.setGreenBufferSize( vlFormat.rgbaBits().g() );
      qtFormat.setBlueBufferSize( vlFormat.rgbaBits().b() );
      // setAlpha == true makes the create() function alway fail
      // even if the returned format has the requested alpha channel
      qtFormat.setAlphaBufferSize( vlFormat.rgbaBits().a() );
      qtFormat.setAlpha( vlFormat.rgbaBits().a() != 0 );
        
      // accumulation buffer
      int accum = vl::max( vlFormat.accumRGBABits().r(), vlFormat.accumRGBABits().g() );
      accum = vl::max( accum, vlFormat.accumRGBABits().b() );
      accum = vl::max( accum, vlFormat.accumRGBABits().a() );
      qtFormat.setAccumBufferSize( accum );
      qtFormat.setAccum( accum != 0 );
        
      // multisampling
      if (vlFormat.multisample())
        qtFormat.setSamples( vlFormat.multisampleSamples() );
      qtFormat.setSampleBuffers( vlFormat.multisample() );
        
      // depth buffer
      qtFormat.setDepthBufferSize( vlFormat.depthBufferBits() );
      qtFormat.setDepth( vlFormat.depthBufferBits() != 0 );

      // stencil buffer
      qtFormat.setStencilBufferSize( vlFormat.stencilBufferBits() );
      qtFormat.setStencil( vlFormat.stencilBufferBits() != 0 );
        
      // stereo
      qtFormat.setStereo( vlFormat.stereo() );
        
      // swap interval / v-sync
      qtFormat.setSwapInterval( vlFormat.vSync() ? 1 : 0 );

#ifndef NDEBUG
      printf("--------------------------------------------\n");
      printf("REQUESTED OpenGL Format:\n");
      printf("--------------------------------------------\n");
      printf("rgba = %d %d %d %d\n", qtFormat.redBufferSize(), qtFormat.greenBufferSize(), qtFormat.blueBufferSize(), qtFormat.alphaBufferSize() );
      printf("double buffer = %d\n", (int)qtFormat.doubleBuffer() );
      printf("depth buffer size = %d\n", qtFormat.depthBufferSize() );
      printf("depth buffer = %d\n", qtFormat.depth() );
      printf("stencil buffer size = %d\n", qtFormat.stencilBufferSize() );
      printf("stencil buffer = %d\n", qtFormat.stencil() );
      printf("accum buffer size %d\n", qtFormat.accumBufferSize() );
      printf("accum buffer %d\n", qtFormat.accum() );
      printf("stereo = %d\n", (int)qtFormat.stereo() );
      printf("swap interval = %d\n", qtFormat.swapInterval() );
      printf("multisample = %d\n", (int)qtFormat.sampleBuffers() );
      printf("multisample samples = %d\n", (int)qtFormat.samples() );
#endif

      return qtFormat;
    }

    vl::OpenGLContextFormat vlFormat;
    
    Qt5ThreadedWidget(vl::OpenGLContextFormat vlFormat, QWidget *parent=0)
      : QGLWidget(getQGLFormat(vlFormat), parent)
      , mythread(new MyThread(this))
      , vlFormat(vlFormat)
    {
    }


    void start(void){
      doneCurrent();

      init_qt(vlFormat);

      context()->moveToThread(mythread);
      
      mythread->startThread();
    }
    
    ~Qt5ThreadedWidget()
    {
      stop();
      delete mythread;
    }

    void stop(){
      if (mythread->isRunning()) {
        Event e;
        e.type = Event::DESTROY;
        
        mythread->push_event(e); 
        printf("waiting\n");
        mythread->wait();
        printf("got it\n");
      }
    }

    virtual void init_vl(vl::OpenGLContext *glContext)  = 0;

  private:
    
    void init_qt(vl::OpenGLContextFormat vlFormat) {

#ifndef NDEBUG
      QGLFormat qtFormat = format();

      printf("--------------------------------------------\n");
      printf("OBTAINED OpenGL Format:\n");
      printf("--------------------------------------------\n");
      printf("rgba = %d %d %d %d\n", qtFormat.redBufferSize(), qtFormat.greenBufferSize(), qtFormat.blueBufferSize(), qtFormat.alphaBufferSize() );
      printf("double buffer = %d\n", (int)qtFormat.doubleBuffer() );
      printf("depth buffer size = %d\n", qtFormat.depthBufferSize() );
      printf("depth buffer = %d\n", qtFormat.depth() );
      printf("stencil buffer size = %d\n", qtFormat.stencilBufferSize() );
      printf("stencil buffer = %d\n", qtFormat.stencil() );
      printf("accum buffer size %d\n", qtFormat.accumBufferSize() );
      printf("accum buffer %d\n", qtFormat.accum() );
      printf("stereo = %d\n", (int)qtFormat.stereo() );
      printf("swap interval = %d\n", qtFormat.swapInterval() );
      printf("multisample = %d\n", (int)qtFormat.sampleBuffers() );
      printf("multisample samples = %d\n", (int)qtFormat.samples() );
      printf("--------------------------------------------\n");
#endif

      if(vlFormat.fullscreen())
        QGLWidget::setWindowState(QGLWidget::windowState() | Qt::WindowFullScreen);        
      else
        QGLWidget::setWindowState(QGLWidget::windowState() & (~Qt::WindowFullScreen));
    }
  };
}

#endif
