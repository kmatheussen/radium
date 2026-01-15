/**************************************************************************************/
/*                                                                                    */
/*  Visualization Library                                                             */
/*  http://visualizationlibrary.org                                                   */
/*                                                                                    */
/*  Copyright (c) 2005-2020, Michele Bosi                                             */
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

#ifndef Qt6Window_INCLUDE_ONCE
#define Qt6Window_INCLUDE_ONCE

#include <vlQt6/link_config.hpp>
#include <vlCore/VisualizationLibrary.hpp>
#include <vlGraphics/OpenGLContext.hpp>
#include <QApplication>
#include <QMouseEvent>
#include <QWidget>
#include <QUrl>
#include <QTimer>
#include <QObject>
#include <QMimeData>
#include <QOpenGLWidget>
#include <QOpenGLContext>
//#include <QGLFormat>

namespace vlQt6
{
  //-----------------------------------------------------------------------------
  // Qt6Widget
  //-----------------------------------------------------------------------------
  /** The Qt6Widget class implements an OpenGLContext using the Qt6 API. */
  class VLQT6_EXPORT Qt6Widget : public QOpenGLWidget, public vl::OpenGLContext
  {
    Q_OBJECT

  public:
    using QObject::setObjectName;
    using vl::Object::setObjectName;

    Qt6Widget(
      QWidget *parent = NULL, 
      Qt::WindowFlags f = Qt::WindowFlags()
    ):
      QOpenGLWidget(parent, f), 
      mRefresh(10) // 100 fps
    {
      setContinuousUpdate(true);
      setMouseTracking(true);
      //setAutoBufferSwap(false); MIC FIXME?
      setAcceptDrops(true);
      // let Qt take care of object destruction.
      vl::OpenGLContext::setAutomaticDelete(false);
    }

    ~Qt6Widget()
    {
      dispatchDestroyEvent();
    }

    void dragEnterEvent(QDragEnterEvent *ev)
    {
      if (ev->mimeData()->hasUrls())
        ev->acceptProposedAction();
    }

    void dropEvent(QDropEvent *ev)
    {
      if (ev->mimeData()->hasUrls())
      {
        std::vector<vl::String> files;
        QList<QUrl> list = ev->mimeData()->urls();
        for (int i = 0; i < list.size(); ++i)
        {
          if (list[i].path().isEmpty())
            continue;
#ifdef WIN32
          if (list[i].path()[0] == '/')
            files.push_back(list[i].path().toStdString().c_str() + 1);
          else
            files.push_back(list[i].path().toStdString().c_str());
#else
          files.push_back(list[i].path().toStdString().c_str());
#endif
        }
        dispatchFileDroppedEvent(files);
      }
    }

    bool initQt6Widget(const vl::String &title, const vl::OpenGLContextFormat &info, int x = 0, int y = 0, int width = 640, int height = 480)
    {
#if 0
      QSurfaceFormat fmt = format();

      switch (info.openGLProfile())
      {
      case vl::GLP_Compatibility:
        fmt.setProfile(QSurfaceFormat::CompatibilityProfile);
        if (info.majVersion())
        {
          fmt.setVersion(info.majVersion(), info.minVersion());
        }
        break;
      case vl::GLP_Core:
        fmt.setProfile(QSurfaceFormat::CoreProfile);
        if (info.majVersion())
        {
          fmt.setVersion(info.majVersion(), info.minVersion());
        }
        break;
      case vl::GLP_Default:
        // Don't care
        break;
      }

      // double buffer
      fmt.setSwapBehavior( info.doubleBuffer() ? QSurfaceFormat::DoubleBuffer : QSurfaceFormat::SingleBuffer );
      //fmt.setDoubleBuffer();

      // color buffer
      fmt.setRedBufferSize(info.rgbaBits().r());
      fmt.setGreenBufferSize(info.rgbaBits().g());
      fmt.setBlueBufferSize(info.rgbaBits().b());
      // setAlpha == true makes the create() function alway fail
      // even if the returned format has the requested alpha channel
      fmt.setAlphaBufferSize(info.rgbaBits().a());

      // accumulation buffer - not supported in Qt 6?
      /*
      int accum = vl::max(info.accumRGBABits().r(), info.accumRGBABits().g());
      accum = vl::max(accum, info.accumRGBABits().b());
      accum = vl::max(accum, info.accumRGBABits().a());
      fmt.setAccumBufferSize(accum);
      fmt.setAccum(accum != 0);
      */

      // multisampling
      if ( info.multisample() ) 
      {
        fmt.setSamples( info.multisampleSamples() );
      }

      // depth buffer
      fmt.setDepthBufferSize(info.depthBufferBits());

      // stencil buffer
      fmt.setStencilBufferSize(info.stencilBufferBits());

      // stereo
      fmt.setStereo(info.stereo());

      // swap interval / v-sync
      fmt.setSwapInterval( info.vSync() ? 1 : 0 );

      this->QOpenGLWidget::setFormat( fmt ); // must be called before the widget or its parent window gets shown
#endif

	  initGLContext();
			
      framebuffer()->setWidth(width);
      framebuffer()->setHeight(height);

#ifndef NDEBUG
      printf("--------------------------------------------\n");
      printf("REQUESTED OpenGL Format:\n");
      printf("--------------------------------------------\n");
      printf("rgba = %d %d %d %d\n", fmt.redBufferSize(), fmt.greenBufferSize(), fmt.blueBufferSize(), fmt.alphaBufferSize());
      printf("double buffer = %d\n", (int)fmt.swapBehavior() == QSurfaceFormat::DoubleBuffer );
      printf("depth buffer size = %d\n", fmt.depthBufferSize());
      printf("depth buffer = %d\n", fmt.depthBufferSize());
      printf("stencil buffer size = %d\n", fmt.stencilBufferSize());
      printf("stencil buffer = %d\n", fmt.stencilBufferSize());
      printf("accum buffer size %d\n", 0);
      printf("accum buffer %d\n", 0);
      printf("stereo = %d\n", (int)fmt.stereo());
      printf("swap interval = %d\n", fmt.swapInterval());
      printf("multisample = %d\n", (int)fmt.samples() != 0);
      printf("multisample samples = %d\n", (int)fmt.samples());

      fmt = format();

      printf("--------------------------------------------\n");
      printf("OBTAINED OpenGL Format:\n");
      printf("--------------------------------------------\n");
      printf("rgba = %d %d %d %d\n", fmt.redBufferSize(), fmt.greenBufferSize(), fmt.blueBufferSize(), fmt.alphaBufferSize());
      printf("double buffer = %d\n", (int)fmt.swapBehavior() == QSurfaceFormat::DoubleBuffer );
      printf("depth buffer size = %d\n", fmt.depthBufferSize());
      printf("depth buffer = %d\n", fmt.depthBufferSize());
      printf("stencil buffer size = %d\n", fmt.stencilBufferSize());
      printf("stencil buffer = %d\n", fmt.stencilBufferSize());
      printf("accum buffer size %d\n", 0);
      printf("accum buffer %d\n", 0);
      printf("stereo = %d\n", (int)fmt.stereo());
      printf("swap interval = %d\n", fmt.swapInterval());
      printf("multisample = %d\n", (int)fmt.samples() != 0);
      printf("multisample samples = %d\n", (int)fmt.samples());
      printf("--------------------------------------------\n");
#endif

      setWindowTitle(title);
      move(x, y);
      resize(width, height);

      if (info.fullscreen())
        setFullscreen(true);

      return true;
    }

    virtual void setContinuousUpdate(bool continuous)
    {
      mContinuousUpdate = continuous;
      if (continuous)
      {
        disconnect(&mUpdateTimer, SIGNAL(timeout()), this, SLOT(update()));
        connect(&mUpdateTimer, SIGNAL(timeout()), this, SLOT(update()));
        mUpdateTimer.setSingleShot(false);
        mUpdateTimer.setInterval(mRefresh);
        mUpdateTimer.start(0);
      }
      else
      {
        disconnect(&mUpdateTimer, SIGNAL(timeout()), this, SLOT(update()));
        mUpdateTimer.stop();
      }
    }

    void setRefreshRate(int msec)
    {
      mRefresh = msec;
      mUpdateTimer.setInterval(mRefresh);
    }

    int refreshRate()
    {
      return mRefresh;
    }

	virtual void init_vl(vl::OpenGLContext *glContext) = 0;
	
    void initializeGL()
    {
      // OpenGL extensions initialization
      initGLContext();

#if !THREADED_OPENGL
      init_vl(this);
#endif

      // Important! Qt 6 instead of using the default framebuffer 0 uses its own framebuffer for offscreen rendering which also changes at every resize.
      // We set to FBO to externally managed so VL does not override the FBO settings prepared by Qt upon pain event.
	  //      vl::OpenGLContext::framebuffer()->setExternallyManaged( true );

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
      QOpenGLWidget::update();
    }

    virtual void setWindowTitle(const vl::String &title)
    {
      QOpenGLWidget::setWindowTitle(QString::fromStdString(title.toStdString()));
    }

    virtual bool setFullscreen(bool fullscreen)
    {
      mFullscreen = fullscreen;
      if (fullscreen)
        QOpenGLWidget::setWindowState(QOpenGLWidget::windowState() | Qt::WindowFullScreen);
      else
        QOpenGLWidget::setWindowState(QOpenGLWidget::windowState() & (~Qt::WindowFullScreen));
      return true;
    }

    virtual void quitApplication()
    {
      eraseAllEventListeners();
      QApplication::quit();
    }

    virtual void show()
    {
      QOpenGLWidget::show();
    }

    virtual void hide()
    {
      QOpenGLWidget::hide();
    }

    virtual void setPosition(int x, int y)
    {
      QOpenGLWidget::move(x, y);
    }

    virtual vl::ivec2 position() const
    {
      return vl::ivec2(QOpenGLWidget::pos().x(), QOpenGLWidget::pos().y());
    }

    virtual void setSize(int w, int h)
    {
      // this already excludes the window's frame so it's ok for Visualization Library standards
      QOpenGLWidget::resize(w, h);
    }

    virtual vl::ivec2 size() const
    {
      // this already excludes the window's frame so it's ok for Visualization Library standards
      return vl::ivec2(QOpenGLWidget::size().width(), QOpenGLWidget::size().height());
    }

    void swapBuffers()
    {
      // Not supported anymore in Qt 6. Docs say to use update() instead.
      // QOpenGLWidget::swapBuffers();
    }

    void makeCurrent()
    {
      QOpenGLWidget::makeCurrent();
    }

    void setMousePosition(int x, int y)
    {
      QCursor::setPos(mapToGlobal(QPoint(x, y)));
    }

    void mouseMoveEvent(QMouseEvent *ev)
    {
      if (!mIgnoreNextMouseMoveEvent)
        dispatchMouseMoveEvent(ev->position().x(), ev->position().y());
      mIgnoreNextMouseMoveEvent = false;
    }

    void mousePressEvent(QMouseEvent *ev)
    {
      vl::EMouseButton bt = vl::NoButton;
      switch (ev->button())
      {
      case Qt::LeftButton:
        bt = vl::LeftButton;
        break;
      case Qt::RightButton:
        bt = vl::RightButton;
        break;
      case Qt::MiddleButton:
        bt = vl::MiddleButton;
        break;
      default:
        bt = vl::UnknownButton;
        break;
      }
      dispatchMouseDownEvent(bt, ev->position().x(), ev->position().y());
    }

    void mouseReleaseEvent(QMouseEvent *ev)
    {
      vl::EMouseButton bt = vl::NoButton;
      switch (ev->button())
      {
      case Qt::LeftButton:
        bt = vl::LeftButton;
        break;
      case Qt::RightButton:
        bt = vl::RightButton;
        break;
      case Qt::MiddleButton:
        bt = vl::MiddleButton;
        break;
      default:
        bt = vl::UnknownButton;
        break;
      }
      dispatchMouseUpEvent(bt, ev->position().x(), ev->position().y());
    }

    void wheelEvent(QWheelEvent *ev)
    {
      dispatchMouseWheelEvent(ev->angleDelta().y() / 120);
    }

    void keyPressEvent(QKeyEvent *ev)
    {
      unsigned short unicode_ch = 0;
      vl::EKey key = vl::Key_None;
      translateKeyEvent(ev, unicode_ch, key);
      dispatchKeyPressEvent(unicode_ch, key);
    }

    void keyReleaseEvent(QKeyEvent *ev)
    {
      unsigned short unicode_ch = 0;
      vl::EKey key = vl::Key_None;
      translateKeyEvent(ev, unicode_ch, key);
      dispatchKeyReleaseEvent(unicode_ch, key);
    }

    virtual void setMouseVisible(bool visible)
    {
      mMouseVisible = visible;
      if (visible)
        QOpenGLWidget::setCursor(Qt::ArrowCursor);
      else
        QOpenGLWidget::setCursor(Qt::BlankCursor);
    }

    virtual void getFocus()
    {
      QOpenGLWidget::setFocus(Qt::OtherFocusReason);
    }

  protected:
    void translateKeyEvent(QKeyEvent *ev, unsigned short &unicode_out, vl::EKey &key_out);

  protected:
    int mRefresh;
    QTimer mUpdateTimer;
  };
  //-----------------------------------------------------------------------------
} // namespace vlQt6

#endif
