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

#ifndef Qt5Window_INCLUDE_ONCE
#define Qt5Window_INCLUDE_ONCE

#include <vlQt5/link_config.hpp>
#include <vlCore/VisualizationLibrary.hpp>
#include <vlGraphics/OpenGLContext.hpp>
//#include <QtWidgets>
//#include <QtGui/QApplication>
#include <QApplication>
#include <QtGui/QMouseEvent>
//#include <QtGui/QWidget>
#include <QWidget>
#include <QtCore/QUrl>
#include <QtCore/QTimer>
#include <QtCore/QObject>
#include <QtOpenGL/QGLWidget>
#include <QtOpenGL/QGLFormat>
#include <QMimeData>

namespace vlQt5
{
//-----------------------------------------------------------------------------
// Qt5Widget
//-----------------------------------------------------------------------------
  /** The Qt5Widget class implements an OpenGLContext using the Qt5 API. */
  class VLQT5_EXPORT Qt5Widget : public QGLWidget, public vl::OpenGLContext
  {
    //Q_OBJECT

  public:
    using vl::Object::setObjectName;
    using QObject::setObjectName;
  
    Qt5Widget(QWidget* parent=NULL, const QGLWidget* shareWidget=NULL, Qt::WindowFlags f=0)
    :QGLWidget(parent,shareWidget,f)
    {
      setContinuousUpdate(true);
      setMouseTracking(true);
      setAutoBufferSwap(false);
      setAcceptDrops(true);
      // let Qt take care of object destruction.
      vl::OpenGLContext::setAutomaticDelete(false);
    }

    ~Qt5Widget()
    {
      dispatchDestroyEvent();
    }

    void dragEnterEvent(QDragEnterEvent *ev) 
    { 
      if (ev->mimeData()->hasUrls()) 
        ev->acceptProposedAction(); 
    }

    void dropEvent(QDropEvent* ev)
    {
      if ( ev->mimeData()->hasUrls() )
      {
        std::vector<vl::String> files;
        QList<QUrl> list = ev->mimeData()->urls();
        for(int i=0; i<list.size(); ++i)
        {
          if (list[i].path().isEmpty())
            continue;
          #ifdef WIN32
            if (list[i].path()[0] == '/')
              files.push_back( list[i].path().toStdString().c_str()+1 );
            else
              files.push_back( list[i].path().toStdString().c_str() );
          #else
            files.push_back( list[i].path().toStdString().c_str() );
          #endif
        }
        dispatchFileDroppedEvent(files);
      }
    }

    bool initQt5Widget(const vl::String& title, const vl::OpenGLContextFormat& info, const QGLContext* shareContext=0, int x=0, int y=0, int width=640, int height=480)
    {
      // setFormat(fmt) is marked as deprecated so we use this other method
      QGLContext* glctx = new QGLContext(context()->format(), this);
      QGLFormat fmt = context()->format();

      fmt.setProfile(QGLFormat::CompatibilityProfile);
      // double buffer
      fmt.setDoubleBuffer( info.doubleBuffer() );

      // color buffer
      fmt.setRedBufferSize( info.rgbaBits().r() );
      fmt.setGreenBufferSize( info.rgbaBits().g() );
      fmt.setBlueBufferSize( info.rgbaBits().b() );
      // setAlpha == true makes the create() function alway fail
      // even if the returned format has the requested alpha channel
      fmt.setAlphaBufferSize( info.rgbaBits().a() );
      fmt.setAlpha( info.rgbaBits().a() != 0 );

      // accumulation buffer
      int accum = vl::max( info.accumRGBABits().r(), info.accumRGBABits().g() );
      accum = vl::max( accum, info.accumRGBABits().b() );
      accum = vl::max( accum, info.accumRGBABits().a() );
      fmt.setAccumBufferSize( accum );
      fmt.setAccum( accum != 0 );

      // multisampling
      if (info.multisample())
        fmt.setSamples( info.multisampleSamples() );
      fmt.setSampleBuffers( info.multisample() );

      // depth buffer
      fmt.setDepthBufferSize( info.depthBufferBits() );
      fmt.setDepth( info.depthBufferBits() != 0 );

      // stencil buffer
      fmt.setStencilBufferSize( info.stencilBufferBits() );
      fmt.setStencil( info.stencilBufferBits() != 0 );

      // stereo
      fmt.setStereo( info.stereo() );

      // swap interval / v-sync
      fmt.setSwapInterval( info.vSync() ? 1 : 0 );

      glctx->setFormat(fmt);
      // this function returns false when we request an alpha buffer
      // even if the created context seem to have the alpha buffer
      /*bool ok = */glctx->create(shareContext);
      setContext(glctx);

      initGLContext();

      framebuffer()->setWidth(width);
      framebuffer()->setHeight(height);

      #ifndef NDEBUG
        printf("--------------------------------------------\n");
        printf("REQUESTED OpenGL Format:\n");
        printf("--------------------------------------------\n");
        printf("rgba = %d %d %d %d\n", fmt.redBufferSize(), fmt.greenBufferSize(), fmt.blueBufferSize(), fmt.alphaBufferSize() );
        printf("double buffer = %d\n", (int)fmt.doubleBuffer() );
        printf("depth buffer size = %d\n", fmt.depthBufferSize() );
        printf("depth buffer = %d\n", fmt.depth() );
        printf("stencil buffer size = %d\n", fmt.stencilBufferSize() );
        printf("stencil buffer = %d\n", fmt.stencil() );
        printf("accum buffer size %d\n", fmt.accumBufferSize() );
        printf("accum buffer %d\n", fmt.accum() );
        printf("stereo = %d\n", (int)fmt.stereo() );
        printf("swap interval = %d\n", fmt.swapInterval() );
        printf("multisample = %d\n", (int)fmt.sampleBuffers() );
        printf("multisample samples = %d\n", (int)fmt.samples() );
        printf("opengl version:  = %s\n", glGetString(GL_VERSION) );

        fmt = format();

        printf("--------------------------------------------\n");
        printf("OBTAINED OpenGL Format:\n");
        printf("--------------------------------------------\n");
        printf("rgba = %d %d %d %d\n", fmt.redBufferSize(), fmt.greenBufferSize(), fmt.blueBufferSize(), fmt.alphaBufferSize() );
        printf("double buffer = %d\n", (int)fmt.doubleBuffer() );
        printf("depth buffer size = %d\n", fmt.depthBufferSize() );
        printf("depth buffer = %d\n", fmt.depth() );
        printf("stencil buffer size = %d\n", fmt.stencilBufferSize() );
        printf("stencil buffer = %d\n", fmt.stencil() );
        printf("accum buffer size %d\n", fmt.accumBufferSize() );
        printf("accum buffer %d\n", fmt.accum() );
        printf("stereo = %d\n", (int)fmt.stereo() );
        printf("swap interval = %d\n", fmt.swapInterval() );
        printf("multisample = %d\n", (int)fmt.sampleBuffers() );
        printf("multisample samples = %d\n", (int)fmt.samples() );
        printf("opengl version:  = %s\n", glGetString(GL_VERSION) );
        printf("--------------------------------------------\n");
      #endif

      setWindowTitle(title);
      move(x,y);
      resize(width,height);

      if (info.fullscreen())
        setFullscreen(true);

      return true;
    }

    virtual void setContinuousUpdate(bool continuous)
    {
      mContinuousUpdate = continuous;
      if (continuous)
      {
        disconnect(&mUpdateTimer, SIGNAL(timeout()), this, SLOT(updateGL()));
        connect(&mUpdateTimer, SIGNAL(timeout()), this, SLOT(updateGL()));
        mUpdateTimer.setSingleShot(false);
        mUpdateTimer.start(0);
      }
      else
      {
        disconnect(&mUpdateTimer, SIGNAL(timeout()), this, SLOT(updateGL()));
        mUpdateTimer.stop();
      }
    }

    void initializeGL()
    {
      // OpenGL extensions initialization
      dispatchInitEvent();
    }

    void resizeGL(int width, int height)
    {
      dispatchResizeEvent(width, height);
    }

    void paintGL()
    {
      dispatchRunEvent();
    }

    void update()
    {
      QGLWidget::update();
      // QGLWidget::updateGL();
    }

    virtual void setWindowTitle(const vl::String& title)
    {
      QGLWidget::setWindowTitle( QString::fromStdString(title.toStdString()) );
    }

    virtual bool setFullscreen(bool fullscreen)
    {
      mFullscreen = fullscreen;
      if (fullscreen)
        QGLWidget::setWindowState(QGLWidget::windowState() | Qt::WindowFullScreen);
      else
        QGLWidget::setWindowState(QGLWidget::windowState() & (~Qt::WindowFullScreen));
      return true;
    }

    virtual void quitApplication()
    {
      eraseAllEventListeners();
      QApplication::quit();
    }

    virtual void show()
    {
      QGLWidget::show();
    }

    virtual void hide()
    {
      QGLWidget::hide();
    }

    virtual void setPosition(int x, int y)
    {
      QGLWidget::move(x,y);
    }

    virtual vl::ivec2 position() const
    {
      return vl::ivec2(QGLWidget::pos().x(), QGLWidget::pos().y());
    }

    virtual void setSize(int w, int h)
    {
      // this already excludes the window's frame so it's ok for Visualization Library standards
      QGLWidget::resize(w,h);
    }

    virtual vl::ivec2 size() const
    {
      // this already excludes the window's frame so it's ok for Visualization Library standards
      return vl::ivec2(QGLWidget::size().width(), QGLWidget::size().height());
    }

    void swapBuffers()
    {
      QGLWidget::swapBuffers();
    }

    void makeCurrent()
    {
      QGLWidget::makeCurrent();
    }

    void setMousePosition(int x, int y)
    {
      QCursor::setPos( mapToGlobal(QPoint(x,y)) );
    }

    void mouseMoveEvent(QMouseEvent* ev)
    {
      if (!mIgnoreNextMouseMoveEvent)
        dispatchMouseMoveEvent(ev->x(), ev->y());
      mIgnoreNextMouseMoveEvent = false;
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
      dispatchMouseDownEvent(bt, ev->x(), ev->y());
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
      dispatchMouseUpEvent(bt, ev->x(), ev->y());
    }

    void wheelEvent(QWheelEvent* ev)
    {
      dispatchMouseWheelEvent(ev->delta() / 120);
    }

    void keyPressEvent(QKeyEvent* ev)
    {
      unsigned short unicode_ch = 0;
      vl::EKey key = vl::Key_None;
      translateKeyEvent(ev, unicode_ch, key);
      dispatchKeyPressEvent(unicode_ch, key);
    }

    void keyReleaseEvent(QKeyEvent* ev)
    {
      unsigned short unicode_ch = 0;
      vl::EKey key = vl::Key_None;
      translateKeyEvent(ev, unicode_ch, key);
      dispatchKeyReleaseEvent(unicode_ch, key);
    }

    virtual void setMouseVisible(bool visible)
    {
      mMouseVisible=visible;
      if (visible)
        QGLWidget::setCursor(Qt::ArrowCursor);
      else
        QGLWidget::setCursor(Qt::BlankCursor);
    }

    virtual void getFocus()
    {
      QGLWidget::setFocus(Qt::OtherFocusReason);
    }

  protected:
    void translateKeyEvent(QKeyEvent* ev, unsigned short& unicode_out, vl::EKey& key_out);

  protected:
    QTimer mUpdateTimer;
  };
  //-----------------------------------------------------------------------------
}

#endif
