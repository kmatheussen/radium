/* Copyright 2014-2016 Kjetil S. Matheussen

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


#include <sys/types.h>
#include <unistd.h>
#include <errno.h>

#include <bitset>

#include <QWidget>

#if USE_QT5
#include <QWindow>
#include <QScreen>
#endif

#include <math.h>
#include <stdio.h>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wundef"
#pragma clang diagnostic ignored "-Wnon-virtual-dtor"
#pragma clang diagnostic ignored "-Wimplicit-int-conversion"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-equal"
#include <vlCore/VisualizationLibrary.hpp>
#include <vlVG/VectorGraphics.hpp>
#include <vlGraphics/Rendering.hpp>
#pragma GCC diagnostic pop

#pragma clang diagnostic pop


/*
  Note: !THREADED_OPENGL should compile and run (at least without too many modifications), but it hasn't been tested much.
  Performance is also significantly worse, but it can probably be handy for debugging.
*/

static double g_last_resize_time = -1;

#if defined(FOR_MACOSX)
#define USE_LAYOUT_COVER 1
#else
#define USE_LAYOUT_COVER 0
#endif

#include <vlGraphics/SceneManagerActorTree.hpp>
#include <vlVG/SceneManagerVectorGraphics.hpp>
#include <vlGraphics/OcclusionCullRenderer.hpp>
#include <vlGraphics/Actor.hpp>

#include <vlCore/Matrix4.hpp>

#if NEW_VL
#include <vlQt5/QtDirectory.cpp>
#include <vlQt5/QtFile.cpp>
#endif

#include <QOpenGLWidget>
#include <QOpenGLContext>
#include <QTextEdit>
#include <QMessageBox>
#include <QApplication>
#include <QAbstractButton>
#include <QGLFormat>
#include <QDebug>
#include <QElapsedTimer>
#include <QOperatingSystemVersion>

#define GE_DRAW_VL

#include "../common/nsmtracker.h"
#include "../common/windows_proc.h"
#include "../common/playerclass.h"
#include "../common/list_proc.h"
#include "../common/realline_calc_proc.h"
#include "../common/time_proc.h"
#include "../common/disk.h"
#include "../common/sequencer_proc.h"
#include "../common/settings_proc.h"
#include "../common/OS_Semaphores.h"
#include "../common/OS_Player_proc.h"
#include "../common/Semaphores.hpp"
#include "../common/Mutex.hpp"
#include "../common/Vector.hpp"
#include "../common/MovingAverage.hpp"
#include "../common/player_proc.h"
#include "../common/visual_proc.h"

#include "../mixergui/QM_MixerWidget.h"

#include "../Qt/Qt_Bs_edit_proc.h"
#include "../Qt/Timer.hpp"
#include "../Qt/Qt_Fonts_proc.h"
#include "../Qt/Qt_mix_colors.h"

#include "../audio/Juce_plugins_proc.h"

#include "../api/api_gui_proc.h"

#include "GfxElements.h"
#include "T2.hpp"
#include "Timing.hpp"
#include "Render_proc.h"
#include "CheckOpenGL_proc.h"

#define USE_QOPENGL 0

#if USE_QT5
#  if THREADED_OPENGL
#    if USE_QOPENGL
#      include "Qt6ThreadedWidget.hpp"
#    else
#      include "Qt5ThreadedWidget.hpp"
#    endif
#    define USE_COVER 1
#  else
#  if USE_QOPENGL
#    include "Qt6Widget.hpp" // Note Q_OBJECT must be removed from Qt6Widget
#  else
#    include "Qt5Widget.hpp" // Note Q_OBJECT must be removed from Qt5Widget
#    include <gui/vlQt5/Qt5Widget.cpp>
#  endif
#    define USE_COVER 0
//#    error "what?"
#  endif
#else
#  error "error"
#  include <vlQt4/Qt4ThreadedWidget.hpp>
#endif

#if USE_QOPENGL
#  define Q_QL_Widget QOpenGLWidget
#else
#  define Q_QL_Widget QGLWidget
#endif

#include "Widget_proc.h"


/*
Example on how to use threaded QOpenGLWidget instead of QGLWidget:
https://gitlab.com/pteam/korvins-qtbase/blob/6f7bc2a7074b7f8c9dacd997d4af597396bbc8d0/examples/opengl/threadedqopenglwidget/glwidget.cpp
*/


/*
=================================================================
==9461==ERROR: AddressSanitizer: new-delete-type-mismatch on 0x614001109440 in thread T0 (radium):
  object passed to delete has wrong type:
  size of the allocated type:   400 bytes;
  size of the deallocated type: 16 bytes.
    #0 0x7ffff7126812 in operator delete(void*, unsigned long) ../../.././libsanitizer/asan/asan_new_delete.cc:108
    #1 0x7ffff1107b0e in QOpenGLVersionFunctionsStorage::~QOpenGLVersionFunctionsStorage() opengl/qopenglversionfunctions.cpp:81
    #2 0x7ffff0e2eda4 in QOpenGLContextPrivate::~QOpenGLContextPrivate() kernel/qopenglcontext_p.h:219
    #3 0x7ffff0e2eda4 in QOpenGLContextPrivate::~QOpenGLContextPrivate() kernel/qopenglcontext_p.h:222
    #4 0x7ffff04e674b in QScopedPointerDeleter<QObjectData>::cleanup(QObjectData*) ../../include/QtCore/../../src/corelib/tools/qscopedpointer.h:60
    #5 0x7ffff04e674b in QScopedPointer<QObjectData, QScopedPointerDeleter<QObjectData> >::~QScopedPointer() ../../include/QtCore/../../src/corelib/tools/qscopedpointer.h:107
    #6 0x7ffff04e674b in QObject::~QObject() kernel/qobject.cpp:900
    #7 0x7ffff0e2e798 in QOpenGLContext::~QOpenGLContext() kernel/qopenglcontext.cpp:696
    #8 0x7ffff18cbeeb in QOpenGLWidgetPrivate::reset() kernel/qopenglwidget.cpp:678
    #9 0x7ffff18cc762 in QOpenGLWidgetPrivate::~QOpenGLWidgetPrivate() kernel/qopenglwidget.cpp:576
    #10 0x7ffff18cc762 in QOpenGLWidgetPrivate::~QOpenGLWidgetPrivate() kernel/qopenglwidget.cpp:577
    #11 0x7ffff04e674b in QScopedPointerDeleter<QObjectData>::cleanup(QObjectData*) ../../include/QtCore/../../src/corelib/tools/qscopedpointer.h:60
    #12 0x7ffff04e674b in QScopedPointer<QObjectData, QScopedPointerDeleter<QObjectData> >::~QScopedPointer() ../../include/QtCore/../../src/corelib/tools/qscopedpointer.h:107
    #13 0x7ffff04e674b in QObject::~QObject() kernel/qobject.cpp:900
    #14 0x7ffff18a91fb in QWidget::~QWidget() kernel/qwidget.cpp:1548
    #15 0x7ffff6fe3ca9 in QtWebEngineCore::RenderWidgetHostViewQtDelegateWidget::~RenderWidgetHostViewQtDelegateWidget() .moc/../render_widget_host_view_qt_delegate_widget.h:58
    #16 0x7ffff6fe3ca9 in QtWebEngineCore::RenderWidgetHostViewQtDelegateWidget::~RenderWidgetHostViewQtDelegateWidget() .moc/../render_widget_host_view_qt_delegate_widget.h:58
    #17 0x7ffff23e605e in std::default_delete<QtWebEngineCore::RenderWidgetHostViewQtDelegate>::operator()(QtWebEngineCore::RenderWidgetHostViewQtDelegate*) const /home/kjetil/site_gcc/include/c++/5.3.0/bits/unique_ptr.h:76
    #18 0x7ffff23e605e in base::internal::scoped_ptr_impl<QtWebEngineCore::RenderWidgetHostViewQtDelegate, std::default_delete<QtWebEngineCore::RenderWidgetHostViewQtDelegate> >::reset(QtWebEngineCore::RenderWidgetHostViewQtDelegate*) /newhd/fedora19stuff/qt-everywhere-opensource-src-5.7.0/qtwebengine/src/3rdparty/chromium/base/memory/scoped_ptr.h:177
    #19 0x7ffff23e605e in base::internal::scoped_ptr_impl<QtWebEngineCore::RenderWidgetHostViewQtDelegate, std::default_delete<QtWebEngineCore::RenderWidgetHostViewQtDelegate> >::~scoped_ptr_impl() /newhd/fedora19stuff/qt-everywhere-opensource-src-5.7.0/qtwebengine/src/3rdparty/chromium/base/memory/scoped_ptr.h:166
    #20 0x7ffff23e605e in scoped_ptr<QtWebEngineCore::RenderWidgetHostViewQtDelegate, std::default_delete<QtWebEngineCore::RenderWidgetHostViewQtDelegate> >::~scoped_ptr() /newhd/fedora19stuff/qt-everywhere-opensource-src-5.7.0/qtwebengine/src/3rdparty/chromium/base/memory/scoped_ptr.h:240
    #21 0x7ffff23e605e in QtWebEngineCore::RenderWidgetHostViewQt::~RenderWidgetHostViewQt() /newhd/fedora19stuff/qt-everywhere-opensource-src-5.7.0/qtwebengine/src/core/render_widget_host_view_qt.cpp:250
    #22 0x7ffff23e61f8 in QtWebEngineCore::RenderWidgetHostViewQt::~RenderWidgetHostViewQt() /newhd/fedora19stuff/qt-everywhere-opensource-src-5.7.0/qtwebengine/src/core/render_widget_host_view_qt.cpp:255
    #23 0x7ffff3051080 in content::RenderWidgetHostImpl::Destroy(bool) /newhd/fedora19stuff/qt-everywhere-opensource-src-5.7.0/qtwebengine/src/3rdparty/chromium/content/browser/renderer_host/render_widget_host_impl.cc:1460
    #24 0x7ffff3041e7e in content::RenderViewHostImpl::ShutdownAndDestroy() /newhd/fedora19stuff/qt-everywhere-opensource-src-5.7.0/qtwebengine/src/3rdparty/chromium/content/browser/renderer_host/render_view_host_impl.cc:958
    #25 0x7ffff31aa364 in content::FrameTree::ReleaseRenderViewHostRef(content::RenderViewHostImpl*) /newhd/fedora19stuff/qt-everywhere-opensource-src-5.7.0/qtwebengine/src/3rdparty/chromium/content/browser/frame_host/frame_tree.cc:382
    #26 0x7ffff2fb6d9c in content::RenderFrameHostImpl::~RenderFrameHostImpl() /newhd/fedora19stuff/qt-everywhere-opensource-src-5.7.0/qtwebengine/src/3rdparty/chromium/content/browser/frame_host/render_frame_host_impl.cc:294
    #27 0x7ffff2fb72b8 in content::RenderFrameHostImpl::~RenderFrameHostImpl() /newhd/fedora19stuff/qt-everywhere-opensource-src-5.7.0/qtwebengine/src/3rdparty/chromium/content/browser/frame_host/render_frame_host_impl.cc:295
    #28 0x7ffff2fbb952 in linked_ptr<content::RenderFrameHostImpl>::depart() /newhd/fedora19stuff/qt-everywhere-opensource-src-5.7.0/qtwebengine/src/3rdparty/chromium/base/memory/linked_ptr.h:144
    #29 0x7ffff2fbb952 in linked_ptr<content::RenderFrameHostImpl>::~linked_ptr() /newhd/fedora19stuff/qt-everywhere-opensource-src-5.7.0/qtwebengine/src/3rdparty/chromium/base/memory/linked_ptr.h:82
    #30 0x7ffff2fbb952 in std::_List_node<linked_ptr<content::RenderFrameHostImpl> >::~_List_node() /home/kjetil/site_gcc/include/c++/5.3.0/bits/stl_list.h:106
    #31 0x7ffff2fbb952 in void __gnu_cxx::new_allocator<std::_List_node<linked_ptr<content::RenderFrameHostImpl> > >::destroy<std::_List_node<linked_ptr<content::RenderFrameHostImpl> > >(std::_List_node<linked_ptr<content::RenderFrameHostImpl> >*) /home/kjetil/site_gcc/include/c++/5.3.0/ext/new_allocator.h:124
    #32 0x7ffff2fbb952 in std::__cxx11::list<linked_ptr<content::RenderFrameHostImpl>, std::allocator<linked_ptr<content::RenderFrameHostImpl> > >::_M_erase(std::_List_iterator<linked_ptr<content::RenderFrameHostImpl> >) /home/kjetil/site_gcc/include/c++/5.3.0/bits/stl_list.h:1777
    #33 0x7ffff2fbb952 in std::__cxx11::list<linked_ptr<content::RenderFrameHostImpl>, std::allocator<linked_ptr<content::RenderFrameHostImpl> > >::erase(std::_List_const_iterator<linked_ptr<content::RenderFrameHostImpl> >) /home/kjetil/site_gcc/include/c++/5.3.0/bits/list.tcc:156
    #34 0x7ffff2fbb952 in content::RenderFrameHostManager::DeleteFromPendingList(content::RenderFrameHostImpl*) /newhd/fedora19stuff/qt-everywhere-opensource-src-5.7.0/qtwebengine/src/3rdparty/chromium/content/browser/frame_host/render_frame_host_manager.cc:797
    #35 0x7ffff2fb0521 in content::RenderFrameHostImpl::OnSwappedOut() /newhd/fedora19stuff/qt-everywhere-opensource-src-5.7.0/qtwebengine/src/3rdparty/chromium/content/browser/frame_host/render_frame_host_impl.cc:1295
    #36 0x7ffff2fba62a in bool IPC::Message::Dispatch<content::RenderFrameHostImpl, content::RenderFrameHostImpl, void>(IPC::Message const*, content::RenderFrameHostImpl*, content::RenderFrameHostImpl*, void*, void (content::RenderFrameHostImpl::*)()) /newhd/fedora19stuff/qt-everywhere-opensource-src-5.7.0/qtwebengine/src/3rdparty/chromium/ipc/ipc_message.h:158
    #37 0x7ffff2fba62a in content::RenderFrameHostImpl::OnMessageReceived(IPC::Message const&) /newhd/fedora19stuff/qt-everywhere-opensource-src-5.7.0/qtwebengine/src/3rdparty/chromium/content/browser/frame_host/render_frame_host_impl.cc:522
    #38 0x7ffff3038b05 in content::RenderProcessHostImpl::OnMessageReceived(IPC::Message const&) /newhd/fedora19stuff/qt-everywhere-opensource-src-5.7.0/qtwebengine/src/3rdparty/chromium/content/browser/renderer_host/render_process_host_impl.cc:1723
    #39 0x7ffff39bdc7f in IPC::ChannelProxy::Context::OnDispatchMessage(IPC::Message const&) /newhd/fedora19stuff/qt-everywhere-opensource-src-5.7.0/qtwebengine/src/3rdparty/chromium/ipc/ipc_channel_proxy.cc:293
    #40 0x7ffff3d78e88 in base::Callback<void ()>::Run() const /newhd/fedora19stuff/qt-everywhere-opensource-src-5.7.0/qtwebengine/src/3rdparty/chromium/base/callback.h:394
    #41 0x7ffff3d78e88 in base::debug::TaskAnnotator::RunTask(char const*, base::PendingTask const&) /newhd/fedora19stuff/qt-everywhere-opensource-src-5.7.0/qtwebengine/src/3rdparty/chromium/base/debug/task_annotator.cc:51
    #42 0x7ffff3d25788 in base::MessageLoop::RunTask(base::PendingTask const&) /newhd/fedora19stuff/qt-everywhere-opensource-src-5.7.0/qtwebengine/src/3rdparty/chromium/base/message_loop/message_loop.cc:486
    #43 0x7ffff3d26388 in base::MessageLoop::DeferOrRunPendingTask(base::PendingTask const&) /newhd/fedora19stuff/qt-everywhere-opensource-src-5.7.0/qtwebengine/src/3rdparty/chromium/base/message_loop/message_loop.cc:495
    #44 0x7ffff3d2684c in base::MessageLoop::DoWork() /newhd/fedora19stuff/qt-everywhere-opensource-src-5.7.0/qtwebengine/src/3rdparty/chromium/base/message_loop/message_loop.cc:607
    #45 0x7ffff23c0b84 in handleScheduledWork /newhd/fedora19stuff/qt-everywhere-opensource-src-5.7.0/qtwebengine/src/core/content_browser_client_qt.cpp:193
    #46 0x7ffff23c0b84 in customEvent /newhd/fedora19stuff/qt-everywhere-opensource-src-5.7.0/qtwebengine/src/core/content_browser_client_qt.cpp:175
    #47 0x7ffff04dfbda in QObject::event(QEvent*) kernel/qobject.cpp:1285
    #48 0x7ffff186989b in QApplicationPrivate::notify_helper(QObject*, QEvent*) kernel/qapplication.cpp:3799
    #49 0x7ffff1870cb5 in QApplication::notify(QObject*, QEvent*) kernel/qapplication.cpp:3556
    #50 0x7ffff04b5c97 in QCoreApplication::notifyInternal2(QObject*, QEvent*) kernel/qcoreapplication.cpp:988
    #51 0x7ffff04b828a in QCoreApplication::sendEvent(QObject*, QEvent*) ../../include/QtCore/../../src/corelib/kernel/qcoreapplication.h:231
    #52 0x7ffff04b828a in QCoreApplicationPrivate::sendPostedEvents(QObject*, int, QThreadData*) kernel/qcoreapplication.cpp:1649
    #53 0x7ffff0506182 in postEventSourceDispatch kernel/qeventdispatcher_glib.cpp:276
    #54 0x3b4b047824 in g_main_context_dispatch (/lib64/libglib-2.0.so.0+0x3b4b047824)
    #55 0x3b4b047b57  (/lib64/libglib-2.0.so.0+0x3b4b047b57)
    #56 0x3b4b047c13 in g_main_context_iteration (/lib64/libglib-2.0.so.0+0x3b4b047c13)
    #57 0x7ffff050658e in QEventDispatcherGlib::processEvents(QFlags<QEventLoop::ProcessEventsFlag>) kernel/qeventdispatcher_glib.cpp:423
    #58 0x7ffff04b4209 in QEventLoop::exec(QFlags<QEventLoop::ProcessEventsFlag>) kernel/qeventloop.cpp:210
    #59 0x7ffff04bc23c in QCoreApplication::exec() kernel/qcoreapplication.cpp:1261
    #60 0x6f18d9 in radium_main Qt/Qt_Main.cpp:1287
    #61 0x820fae in init_radium api/api_common.c:63
    #62 0x8205c6 in _wrap_init_radium api/radium_wrap.c:577
    #63 0x3b5d4dd0e0 in PyEval_EvalFrameEx (/lib64/libpython2.7.so.1.0+0x3b5d4dd0e0)
    #64 0x3b5d4ddb1e in PyEval_EvalCodeEx (/lib64/libpython2.7.so.1.0+0x3b5d4ddb1e)
    #65 0x3b5d4ddbf1 in PyEval_EvalCode (/lib64/libpython2.7.so.1.0+0x3b5d4ddbf1)
    #66 0x3b5d4f6b99  (/lib64/libpython2.7.so.1.0+0x3b5d4f6b99)
    #67 0x3b5d4f7991 in PyRun_FileExFlags (/lib64/libpython2.7.so.1.0+0x3b5d4f7991)
    #68 0x3b5d4d4b75  (/lib64/libpython2.7.so.1.0+0x3b5d4d4b75)
    #69 0x3b5d4dd0e0 in PyEval_EvalFrameEx (/lib64/libpython2.7.so.1.0+0x3b5d4dd0e0)
    #70 0x3b5d4ddb1e in PyEval_EvalCodeEx (/lib64/libpython2.7.so.1.0+0x3b5d4ddb1e)
    #71 0x3b5d4ddbf1 in PyEval_EvalCode (/lib64/libpython2.7.so.1.0+0x3b5d4ddbf1)
    #72 0x3b5d4f6b99  (/lib64/libpython2.7.so.1.0+0x3b5d4f6b99)
    #73 0x3b5d4f7767 in PyRun_StringFlags (/lib64/libpython2.7.so.1.0+0x3b5d4f7767)
    #74 0x3b5d4f827a in PyRun_SimpleStringFlags (/lib64/libpython2.7.so.1.0+0x3b5d4f827a)
    #75 0x6f28c2 in main Qt/Qt_Main.cpp:1618
    #76 0x3b49421734 in __libc_start_main (/lib64/libc.so.6+0x3b49421734)
    #77 0x4e7860  (/home/kjetil/radium/bin/radium+0x4e7860)

0x614001109440 is located 0 bytes inside of 400-byte region [0x614001109440,0x6140011095d0)
allocated by thread T0 (radium) here:
    #0 0x7ffff7125d8a in operator new(unsigned long) ../../.././libsanitizer/asan/asan_new_delete.cc:60
    #1 0x7ffff1107d11 in QOpenGLVersionFunctionsStorage::backend(QOpenGLContext*, QOpenGLVersionFunctionsBackend::Version) opengl/qopenglversionfunctions.cpp:108

SUMMARY: AddressSanitizer: new-delete-type-mismatch ../../.././libsanitizer/asan/asan_new_delete.cc:108 operator delete(void*, unsigned long)
==9461==HINT: if you don't care about these warnings you may set ASAN_OPTIONS=new_delete_type_mismatch=0
==9461==ABORTING

*/


#ifdef FOR_LINUX
static int set_pthread_priority(pthread_t pthread,int policy,int priority,const char *message,const char *name){
  struct sched_param par={};
  par.sched_priority=priority;

  //if((sched_setscheduler(pid,policy,&par)!=0)){
  if ((pthread_setschedparam(pthread, policy, &par)) != 0) {
    fprintf(stderr,message,policy==SCHED_FIFO?"SCHED_FIFO":policy==SCHED_RR?"SCHED_RR":policy==SCHED_OTHER?"SCHED_OTHER":"SCHED_UNKNOWN",priority,getpid(),name,strerror(errno));
    //abort();
    return 0;
  }
  return 1;
}

static void set_realtime(int type, int priority){
  //bound_thread_to_cpu(0);
  set_pthread_priority(pthread_self(),type,priority,"Unable to set %s/%d for %d (\"%s\"). (%s)\n", "a gc thread");
}
#endif

static DEFINE_ATOMIC(bool, g_has_updated_at_least_once) = false;
static DEFINE_ATOMIC(bool, g_has_resized_at_least_once) = false;

DEFINE_ATOMIC(char *, GE_vendor_string) = NULL;
DEFINE_ATOMIC(char *, GE_renderer_string) = NULL;
DEFINE_ATOMIC(char *, GE_version_string) =NULL;
DEFINE_ATOMIC(uint32_t, GE_opengl_version_flags) = 0;


bool GL_using_nvidia_card(void){
  const char *c1 = ATOMIC_GET(GE_vendor_string);
  const char *c2 = ATOMIC_GET(GE_renderer_string);
  const char *c3 = ATOMIC_GET(GE_version_string);
  
  bool a = c1!=NULL && QString(c1).contains("nvidia", Qt::CaseInsensitive);
  bool b = c2!=NULL && QString(c2).contains("nvidia", Qt::CaseInsensitive);
  bool c = c3!=NULL && QString(c3).contains("nvidia", Qt::CaseInsensitive);
  
  return a || b || c;
}



static bool g_safe_mode = false;

#if USE_QT5
static const bool USE_GL_LOCK = false; // Seems like this is safe... (the lock is used to avoid simultaneous access to openglcontext, but in qt5 we specifically tell qt that we use opengl from another thread, so I guess/hope qt is not accessing the openglcontext in qt5 when we do that. (this was also the main reason for switching to qt5, since the gl_lock doesn't always work perfectly in qt4.))
#else
static const bool USE_GL_LOCK = true;
#endif

#define USE_SWAP_MUTEX 0 // For debugging. Lowers gfx performance significantly.

static radium::Mutex mutex;
#if USE_SWAP_MUTEX
static radium::Mutex swap_mutex;
#endif
static radium::Mutex make_current_mutex;
static radium::Mutex draw_mutex(true); // recursive mutex

static __thread int g_gl_lock_visits = 0; // simulate a recursive mutex this way instead of using the QThread::Recursive option since QWaitCondition doesn't work with recursive mutexes. We need recursive mutexes since calls to qsometing->exec() (which are often executed inside the gl lock) can process qt events, which again can call some function which calls gl_lock. I don't know why qt processes qt events inside exec() though. That definitely seems like the wrong desing, or maybe it's even a bug in qt. If there is some way of turning this peculiar behavior off, I would like to know about it. I don't feel that this behaviro is safe.

static QImage g_cover_image;
static radium::Mutex g_image_mutex(true); // recursive mutex

void GL_lock(void){

  g_gl_lock_visits++;  
  if (g_gl_lock_visits>1)
    return;

  if (USE_GL_LOCK)
    mutex.lock();

#if 0
  if (g_safe_mode)
    GL_draw_lock(); //something may deadlock
#endif
}

void GL_unlock(void){
  R_ASSERT_RETURN_IF_FALSE(g_gl_lock_visits>0);

  g_gl_lock_visits--;
  if (g_gl_lock_visits>0)
    return;

  return;

#if 0
  if (g_safe_mode)
    GL_draw_unlock();
#endif

  if (USE_GL_LOCK)
    mutex.unlock();
}

bool GL_maybeLock(void){
  
  if (g_gl_lock_visits>1)
    return false;

  GL_lock();
  return true;
}

void GL_draw_lock(void){
#if !defined(RELEASE)
  abort(); // don't think this lock is used anymore.
#endif
  draw_mutex.lock();
}

void GL_draw_unlock(void){
  draw_mutex.unlock();
}


namespace{
  struct PreventHighCpuInSwap{

  private:
    QElapsedTimer _swap_timer;
    QElapsedTimer _first_sleep_timer;
    bool is_sleeping = false;
    bool is_waiting_before_sleeping = false;
    bool is_waiting_before_turning_off_waiting_before_sleeping = false;

#define _num_swap_times 40

    radium::MovingAverage _moving_average;

  public:

    const int _num_checks_before_sleeping = 20;
    const int _time_before_first_sleep = 1000;
    
    PreventHighCpuInSwap()
      :_moving_average(_num_swap_times, 200)
    {
      _swap_timer.start();
    }

    void check(double vblank, double swap_duration){
      
      double duration = _swap_timer.restart();
      double average_swap_time = _moving_average.get(duration);

      if (average_swap_time >= vblank*0.8){
        if (is_sleeping){
          //printf("Turning off sleeping. dur: %f, avg: %f\n", duration, average_swap_time);
          is_sleeping=false;
        }

        if (is_waiting_before_sleeping){

          if (!is_waiting_before_turning_off_waiting_before_sleeping){
            is_waiting_before_turning_off_waiting_before_sleeping = true;
            _first_sleep_timer.restart();
          }
          
          if (_first_sleep_timer.elapsed() >= 20000){
            //printf("  Turning off wating before sleeping. dur: %f, avg: %f\n", duration, average_swap_time);
            is_waiting_before_turning_off_waiting_before_sleeping = false;
            is_waiting_before_sleeping = false;
          }
        }

      } else {

        if (is_sleeping){

          // todo: Print message to editor window.
          
          //printf("OpenGL quickswapsleeping. Duration: %f. Vblank: %f. average: %f\n", duration, vblank, average_swap_time);
          msleep(40 * vblank);
          _moving_average.reset(vblank/2);
          //_swap_timer.restart();

        } else if (!is_waiting_before_sleeping){

          //printf(" Starting to wait 1 second. Duration: %f\n", duration);
          _first_sleep_timer.restart();
          is_waiting_before_sleeping = true;

        } else {

          if (_first_sleep_timer.elapsed() >= _time_before_first_sleep){
            is_sleeping = true;
          }

        }

      }
      
    }
    
#undef _num_swap_times
  };

  PreventHighCpuInSwap prevent_high_cpu_in_swap;
}

bool g_gl_widget_started = false;

static bool g_should_do_modal_windows = false;

bool GL_should_do_modal_windows(void){
  return g_should_do_modal_windows;
}

static DEFINE_ATOMIC(bool, g_is_currently_pausing) = false;
static radium::Semaphore g_order_pause_gl_thread;
static radium::CondWait g_ack_pause_gl_thread;

static radium::Semaphore g_order_make_current;
static radium::CondWait g_ack_make_current;

static DEFINE_ATOMIC(int, g_curr_realline);

// TS (called from both main thread and opengl thread)
void GE_set_curr_realline(int curr_realline){
  //printf("  ############      Setting g_curr_realline to %d\n", curr_realline);
  ATOMIC_SET(g_curr_realline, curr_realline);
}

// OpenGL thread
static float GE_scroll_pos(const SharedVariables *sv, double realline){
  double extra = sv->top_realline - sv->curr_realline;
  return
    (   (realline+extra) * sv->fontheight  );
}


extern int scrolls_per_second;
extern int default_scrolls_per_second;


// The time_estimator is used to estimate the screen refresh rate according to the soundcard clock.
// I.e. 60.0 hz when using the gfx card clock is unlikely to be 60.0 hz when using the soundcard clock.
//
// However, since we put a high pass filter on the scroll position, it doesn't really matter, so
// we only estimate using QT4. In Qt5, we just use the widget->windowHandle()->screen()->refreshRate() value instead.
//
static TimeEstimator time_estimator;


// OpenGL thread
static double get_realline_stime(const SharedVariables *sv, int realline){
  double blocktime;
  if(realline==sv->num_reallines)
    blocktime = sv->block_duration;
  else
    blocktime = Place2STime_from_times2(sv->times, p_getDouble(sv->reallines[realline]->l.p));
  
  return blocktime_to_seqtime_double(sv->seqblock_stretch, blocktime);
}


// OpenGL thread
static bool need_to_reset_timing(const SharedVariables *sv, double stime, int last_used_i_realline, const struct Blocks *last_used_block, double last_used_stime, double blocktime){
  if (stime < 0){
    fprintf(stderr,"Error: stime: %f, pc->blocktime: %f\n",stime,blocktime);
    #if 0
      #if !defined(RELEASE)
        abort();
      #endif
    #endif
    return true;
  }

  if (last_used_block != sv->block)    
    return true;
  
  if(last_used_i_realline>=sv->num_reallines) // First check that i_realline is within the range of the block. (block might have changed number of lines)
    return true;
    
  // TODO: Make the "last_stime < stime"-check configurable.
  if (stime < last_used_stime)
    return true;
  
  if(stime < get_realline_stime(sv, last_used_i_realline)) // Time is now before the line we were at when we left last time. Start searching from 0 again. (Not sure if is correct. It might be last_used_i_realline+1 instead)
    return true;

  return false;
}


// OpenGL thread
static double find_current_realline_while_playing(const SharedVariables *sv, double blocktime){

  double time_in_ms = blocktime * 1000.0 / (double)pc->pfreq; // I'm not entirely sure reading pc->start_time_f instead of pc->start_time is unproblematic.
  double stime      = time_estimator.get(time_in_ms, sv->reltempo * ATOMIC_DOUBLE_GET(g_curr_song_tempo_automation_tempo)) * (double)pc->pfreq / 1000.0; // Could this value be slightly off because we just changed block, and because of that we skipped a few calles to time_estimator.get ? (it shouldn't matter though, timing is resetted when that happens. 'time_in_ms' should always be valid)

  //stime      = time_in_ms* (double)pc->pfreq / 1000.0;

  static double last_stime = stime;
    
    
  //Strictly speaking, we need to atomically get current block + pc->blocktime. But it is uncertain how important this is is.
  // Maybe store blocktime in the block itself?
  
  static int i_realline = 0; // Note that this one is static. The reason is that we are usually placed on the same realline as last time, so we remember last position and start searching from there.
  static const struct Blocks *block = NULL; // Remember block used last time. If we are not on the same block now, we can't use the i_realline value used last time.

  R_ASSERT(i_realline>=0);
  
  //                                    Common situation. We are usually on the same line as the last visit,
  if (i_realline > 0) i_realline--; //  but we need to go one step back to reload prev_line_stime.
  //                                    (storing last used 'stime1' and/or 'stime2' would be an optimization which would make my head hurt and make no difference in cpu usage)

  if (need_to_reset_timing(sv, stime, i_realline, block, last_stime, blocktime)) {
    i_realline = 0;
    block = sv->block;
    time_estimator.set_time(time_in_ms);
    stime = time_in_ms * (double)pc->pfreq / 1000.0; // Convert the current block time into number of frames.
  }

  //  stime -= 24000;
      
  last_stime = stime;
  
  double stime2 = get_realline_stime(sv, i_realline);
  
  while(true){

    double stime1 = stime2;
    for(;;){ // This for loop is here to handle a very special situation where we play so fast that stime1==stime2. In normal songs, this should not happen.
      stime2 = get_realline_stime(sv, i_realline+1);

#if 0
      if (stime1==stime2){ // Could probably happen if playing really fast... Not sure. (yes, it happens if playing really fast)
#if !defined(RELEASE)
        /abort();
#endif
        return i_realline;
      }
#endif
      
      if (i_realline==sv->num_reallines)
        return sv->num_reallines;
      if (equal_doubles(stime1, stime2))
        i_realline++;
      else
        break;
    }
      
    if (stime >= stime1 && stime <= stime2){
      return scale_double(stime,
                          stime1, stime2,
                          i_realline, i_realline+1
                          );
    }

    i_realline++;

    if (i_realline==sv->num_reallines)
      break;
  }

  return sv->num_reallines;
}




#if 0
static void UpdateReallineByLines(struct Tracker_Windows *window, struct WBlocks *wblock, double d_till_curr_realline){
  static STime last_time = 0;
  static struct WBlocks *last_wblock = NULL;

  int till_curr_realline = (int)d_till_curr_realline;

  if(scrolls_per_second==-1)
    scrolls_per_second = SETTINGS_read_int("scrolls_per_second", default_scrolls_per_second);

  bool do_scrolling = false;
  
  if(wblock != last_wblock)
    do_scrolling = true;
  
  else if (last_time > pc->therealtime)
    do_scrolling = true;
  
  else if(till_curr_realline < wblock->curr_realline)
    do_scrolling = true;
  
  else if(till_curr_realline > wblock->curr_realline){
    STime from_time = (STime) ((double)Place2STime(wblock->block, &wblock->reallines[wblock->curr_realline]->l.p) / wblock->block->reltempo);
    STime to_time   = (STime) ((double)Place2STime(wblock->block, &wblock->reallines[till_curr_realline]->l.p) / wblock->block->reltempo);
    
    STime time = to_time - from_time;
    
    STime time_necessary_to_scroll = pc->pfreq / scrolls_per_second;
    
    if(time>=time_necessary_to_scroll)
      do_scrolling = true;
  }
  
  if(do_scrolling==true) {
    ScrollEditorToRealLine(window,till_curr_realline);
    last_time = pc->therealtime;
    last_wblock = wblock;
  }
}
#endif


// Main thread
static Tracker_Windows *get_window(void){
  return root->song->tracker_windows;
}

// Main thread
static EditorWidget *get_editorwidget(void){
  return (EditorWidget *)get_window()->os_visual.widget;
}


volatile float g_scroll_pos = 0.0f;

// Main thread
/*
static QMouseEvent translate_qmouseevent(const QMouseEvent *qmouseevent){
  const QPoint p = qmouseevent->pos();

  return QMouseEvent(qmouseevent->type(),
                     QPoint(p.x(), p.y() + root->song->tracker_windows->wblock->t.y1),
                     qmouseevent->globalPos(),
                     qmouseevent->button(),
                     qmouseevent->buttons(),
                     Qt::NoModifier
                     );
}
*/

#define TEST_TIME 0

#if defined(RELEASE) && TEST_TIME==1
#error "oops"
#endif

#if USE_LAYOUT_COVER // this mad stuff seems to prevent the apple opengl library from crashing when resizing.

namespace{
  class MyCoverWidget : public QWidget {
  public:
    MyCoverWidget(){
      //setMouseTracking(true);
    }
  private:
    void resizeEvent(QResizeEvent *qresizeevent) override {
      g_last_resize_time = TIME_get_ms();
      radium::ScopedResizeEventTracker resize_event_tracker;
      QSize size = qresizeevent->size();
      THREADING_run_on_main_thread_async([size]{
          g_editor->gl_widget->resize(size);
        });
    }
     
    void paintEvent( QPaintEvent *e ){
      TRACK_PAINT();
      QPainter p(this);

      p.fillRect(rect(), get_qcolor(LOW_EDITOR_BACKGROUND_COLOR_NUM));
      
      if (true){ //if ((TIME_get_ms() - _start_time) > 50){
        
        QRect cover_rect = rect().adjusted(rect().width()/4, rect().height()/4, -rect().width()/4, -rect().height()/4);
        
        p.fillRect(cover_rect.adjusted(-20,-20,20,20), mix_colors(get_qcolor(LOW_EDITOR_BACKGROUND_COLOR_NUM), Qt::black, 0.5));
        
        QRect text_rect(0, 0, width(), cover_rect.y()-20);
        p.drawText(text_rect, Qt::AlignCenter, "Resizing OpenGL");

        /*
        if(!_cover->_image.isNull()){
          p.drawImage(cover_rect, _cover->_image);
        }
        */
      }
    }
  };
}

static bool g_cover_is_visible = false;

static void show_layout_cover(void){
  if (g_cover_is_visible)
    return;
  g_cover_is_visible = true;
  
  printf("               SHOW_COVER\n");
  if (g_editor->gl_cover==NULL){
    g_editor->gl_cover = new MyCoverWidget;
    static_cast<QBoxLayout*>(g_editor->editor_layout_widget->layout())->insertWidget(1,g_editor->gl_cover);    
  }

  g_editor->editor_layout_widget->setUpdatesEnabled(false);
  
  //g_editor->editor_layout_widget->layout()->replaceWidget(g_editor->gl_widget, g_editor->gl_cover, Qt::FindDirectChildrenOnly);
  
  g_editor->gl_widget->hide();
  g_editor->gl_cover->show();

  //g_editor->gl_widget->setMouseTracking(true);

  /*
  g_editor->gl_cover->adjustSize();
  g_editor->gl_cover->updateGeometry();
  */

  QTimer::singleShot(100, []{
      g_editor->editor_layout_widget->setUpdatesEnabled(true);
      //g_editor->editor_layout_widget->setMouseTracking(true);
      //g_editor->gl_widget->setMouseTracking(true);
    });
}

static void hide_layout_cover(void){
  if (!g_cover_is_visible)
    return;
  g_cover_is_visible = false;
  
  printf("           HIDE_COVER\n");
  g_editor->editor_layout_widget->setUpdatesEnabled(false);

  //g_editor->editor_layout_widget->layout()->replaceWidget(g_editor->gl_cover, g_editor->gl_widget, Qt::FindDirectChildrenOnly);
  
  g_editor->gl_cover->hide();
  g_editor->gl_widget->show();
  //g_editor->gl_widget->setMouseTracking(true);
  
  /*
  g_editor->gl_widget->adjustSize();
  g_editor->gl_widget->updateGeometry();
  */

  QTimer::singleShot(100, []{
      g_editor->editor_layout_widget->setUpdatesEnabled(true);
      //g_editor->gl_widget->setMouseTracking(true);
    });

  //g_editor->editor_layout_widget->setUpdatesEnabled(true);
}

static void schedule_hide_layout_cover(void){
  //if (g_editor->gl_widget->isVisible())
  //  return;
  
  if ( (TIME_get_ms() - g_last_resize_time) > 100){
    
    hide_layout_cover();
    
    /*
      g_editor->gl_widget->show();
      
      {
      radium::ScopedMutex lock(_cover_mutex);
      delete _cover;
      _cover = NULL;
      }
    */
    
  } else {
    
    QTimer::singleShot(1000, schedule_hide_layout_cover);
    
  }
}

#endif

static QThread *g_render_thread = NULL;


class MyQtThreadedWidget
#if USE_QT5
#  if THREADED_OPENGL
  : public vlQt5::Qt5ThreadedWidget
#  else
  : public vlQt5::Qt5Widget
#  endif
#else
  : public vlQt4::Qt4ThreadedWidget
#endif
  , public vl::UIEventListener
  , public radium::MouseCycleFix
{
    
public:

#if defined(FOR_MACOSX)
  const bool _use_cover = QOperatingSystemVersion::current() < QOperatingSystemVersion::MacOSBigSur;
#else
  const bool _use_cover = true;
#endif
  
  int current_width;
  int current_height;
  
  vl::ref<vl::Rendering> _rendering;

  T2_data *t2_data;
  bool t2_data_can_be_used;
  radium::Vector<T2_data*> old_t2_datas; // Waiting to be sent back to t2.
  
  vl::ref<vl::SceneManagerVectorGraphics> vgscene;

#if !USE_QT5
  DEFINE_ATOMIC(bool, is_training_vblank_estimator);
  bool has_overridden_vblank_value;
#endif
  
  DEFINE_ATOMIC(double, override_vblank_value);

  float last_scroll_pos;
  double last_current_realline_while_playing;
  
  bool sleep_when_not_painting = true;  //SETTINGS_read_bool("opengl_sleep_when_not_painting", false))

  DEFINE_ATOMIC(bool, _main_window_is_exposed);

#if USE_SWAP_MUTEX
  struct MyTimer : public radium::Timer {
    MyTimer()
      : radium::Timer(5)
    {
      swap_mutex.lock();
    }

    void calledFromTimer(void) override {
      swap_mutex.unlock();
      msleep(5);
      swap_mutex.lock();
    }
      
  };

  MyTimer mytimer;
#endif
  
  // Main thread
  MyQtThreadedWidget(vl::OpenGLContextFormat vlFormat, QWidget *parent=0)
#if USE_QT5
#  if THREADED_OPENGL
    : Qt5ThreadedWidget(vlFormat, parent)
#  else
      : Qt5Widget(vlFormat, parent)
#  endif
#else
    : Qt4ThreadedWidget(vlFormat, parent)
#endif
    , current_width(500)
    , current_height(500)
    , t2_data(NULL)
    , t2_data_can_be_used(false)
#if !USE_QT5
    , has_overridden_vblank_value(false)
#endif
      //, painting_data(NULL)
    , last_scroll_pos(-1.0f)
    , last_current_realline_while_playing(-1.0f)
  {
    vlQt5::Qt5ThreadedWidget::setObjectName("MyQtThreadedWidget");
            
    ATOMIC_SET(_main_window_is_exposed, false);

#if 0 //!THREADED_OPENGL
    initQt5Widget("hello", vlFormat);
#endif
    
#if USE_QT5
    ATOMIC_DOUBLE_SET(override_vblank_value, 1000.0 / 60.0);
#else
    ATOMIC_DOUBLE_SET(override_vblank_value, -1.0);
    ATOMIC_SET(is_training_vblank_estimator, false);
#endif
    
    setMouseTracking(true);
    //setAttribute(Qt::WA_PaintOnScreen);

    setAttribute(Qt::WA_NoSystemBackground); // makes no difference.
    setAttribute(Qt::WA_OpaquePaintEvent);
    
    _update_event_counter_timer.start();

  }

  // Main thread
  ~MyQtThreadedWidget(){
    fprintf(stderr,"Exit myqtthreaderwidget\n");
  }

  vl::ref<vl::OcclusionCullRenderer> mOcclusionRenderer;
  vl::ref<vl::Scissor> _scissor;

  // OpenGL thread
  void init_vl(vl::OpenGLContext *glContext)
#if THREADED_OPENGL
    override
#endif
  {

    printf("init_vl\n");

    msleep(200); // Maybe this prevents opengl from crashing during startup on osx.

    glContext->initGLContext(); // Sometimes, the first gl* call crashes inside here on OSX.

#if USE_QOPENGL
    glContext->vl::OpenGLContext::framebuffer()->setExternallyManaged( true );
    //dispatchInitEvent();
#endif
      

    // Important! Qt 6 instead of using the default framebuffer 0 uses its own framebuffer for offscreen rendering which also changes at every resize.
    // We set to FBO to externally managed so VL does not override the FBO settings prepared by Qt upon pain event.
    //glContext->vl::OpenGLContext::framebuffer()->setExternallyManaged( true );

#if THREADED_OPENGL
    g_render_thread = QThread::currentThread();
    if (GL_get_high_render_thread_priority()){
      g_render_thread->setPriority(QThread::HighestPriority);
    }
#endif
    
    _rendering = new vl::Rendering;
        
    _rendering->renderer()->setFramebuffer(glContext->framebuffer() );
    
    // A gigantic speedup, but rendering isn't right.
    if(0){
      vl::Renderer* regular_renderer = _rendering->renderer();
      // creates our occlusion renderer
      mOcclusionRenderer = new vl::OcclusionCullRenderer;
      mOcclusionRenderer->setWrappedRenderer( regular_renderer );
      // installs the occlusion renderer in place of the regular one
      _rendering->setRenderer( mOcclusionRenderer.get() );
    }
    
    //_rendering->setCullingEnabled(true); // What does this method do? It seems to make no difference whether we use the OcclusionCullRenderer or not.

    
    glContext->addEventListener(this);


    //printf("FLAGS: %d\n",QGLFormat::openGLVersionFlags());
    //gets(NULL);

#ifdef FOR_LINUX
    if(0)set_realtime(SCHED_FIFO,1); // TODO: Add priority inheritance to all locks. Setting the OpenGL thread to a higher priority might not make a difference because of priority inversion.
#endif
  }
  
  /** Event generated when the bound OpenGLContext bocomes initialized or when the event listener is bound to an initialized OpenGLContext. */
  // OpenGL thread
  void initEvent(void) override {
    //printf("initEvent\n");
    _rendering->sceneManagers()->clear();
    vgscene = new vl::SceneManagerVectorGraphics;
    _rendering->sceneManagers()->push_back(vgscene.get());
  }
  
private:
  
  // OpenGL thread
  bool canDraw(){
    return vgscene.get() != NULL;    
  }
  
public:

  // Main thread
  bool fix_mousePressEvent(radium::MouseCycleEvent &qmouseevent) override {
    return get_editorwidget()->handle_mouse_press(qmouseevent, qmouseevent.x(), qmouseevent.y() + root->song->tracker_windows->wblock->t.y1);
  }

  // Main thread
  void fix_mouseMoveEvent(radium::MouseCycleEvent &qmouseevent) override {
    //printf("fix_mouseMoveEvent %d %d\n", (int)qmouseevent.x(), (int)qmouseevent.y());
    get_editorwidget()->handle_mouse_move(qmouseevent.button(), qmouseevent.x(), qmouseevent.y() + root->song->tracker_windows->wblock->t.y1);
  }

  // Main thread
  bool fix_mouseReleaseEvent(radium::MouseCycleEvent &event) override{
    return get_editorwidget()->handle_mouse_release(event.button(), event.x(), event.y() + root->song->tracker_windows->wblock->t.y1);
  }
  
  MOUSE_CYCLE_CALLBACKS_FOR_QT;
  
  void wheelEvent(QWheelEvent *qwheelevent) override {
    //QMouseEvent event = translate_qmouseevent(qmouseevent);
    get_editorwidget()->wheelEvent(qwheelevent);
    //GL_create(get_window(), get_window()->wblock);
  }
    
  void leaveEvent(QEvent * event) override {
    //fprintf(stderr, "LEAVING\n");
    //setNormalMousePointer(-1); // This should not be necessary anymore.
  }
  
  /** Event generated right before the bound OpenGLContext is destroyed. */
  void destroyEvent() override {
    fprintf(stderr,"destroyEvent\n");
  }

  /*
    // No need to override paintEvent. OpenGL takes care of that by itself.

  // Only called when parent needs to be repainted. For instances if another window has covered it, or it is resized.
  void paintEvent( QPaintEvent *e ) override {
    //fprintf(stderr,"GLWindow paintEvent\n");

    // Doesn't seem to make any difference whether to call GL_create or set must_redraw=true.
    // I guess calling GL_create as soon as possible is best to avoid flickering.
    //GL_create(get_window());
    //get_window()->must_redraw=true;
  }
  */
  
private:

  void send_back_old_t2_data(bool keep_last_element){
#if !defined(RELEASE)
    if (old_t2_datas.size() > 0)
      R_ASSERT(T3_use_t2_thread());
#endif
    
    int when_to_stop = keep_last_element ? 1 : 0;
    
    while(old_t2_datas.size() > when_to_stop){
      if (T3_send_back_old_t2_data(old_t2_datas.at(0)) == false)
        break;
      old_t2_datas.remove_pos(0, true);
    }
  }
  
  int64_t draw_counter = 0;
  
  // OpenGL thread
  bool draw(void){
  

    
#if TEST_TIME
    double start1 = TIME_get_ms();
    //double draw_dur = 0.0;
    double dur1=0,dur2=0,dur25=0,dur3=0;
#endif

    int player_id = ATOMIC_GET(pc->play_id);
    bool is_playing = ATOMIC_GET(pc->player_state)==PLAYER_STATE_PLAYING;

    static bool use_t2_thread = T3_use_t2_thread(); // Minor optimization to set static.
    
    T2_data *new_t2_data = NULL;

    //if (!is_playing || (draw_counter % 6 == 0))
    new_t2_data = T3_maybe_get_t2_data();

    T3_create_gradienttriangles_if_needed(new_t2_data != NULL);
        
    draw_counter++;
    
    if (new_t2_data != NULL){

      //prevent_high_cpu_in_swap.reset(); // On OSX, swap doesn't take very much time for a while after redrawing.

      t2_data_can_be_used = false;
      
      T2_data *old_t2_data = t2_data;
      t2_data = new_t2_data;

      //_rendering = new vl::Rendering;  
      //_rendering->renderer()->setFramebuffer(openglContext()->framebuffer() );

      auto viewport = _rendering->camera()->viewport();

      viewport->setWidth(current_width * g_opengl_scale_ratio);
      viewport->setHeight(current_height * g_opengl_scale_ratio);

      if(!equal_doubles(g_opengl_scale_ratio, 1.0)){

#if 1
        // I think this is correct. Not sure.
        _rendering->camera()->setProjectionOrtho(-0.5f,
                                                 current_width-0.5f,
                                                 -0.5f,
                                                 current_height-0.5f,
                                                 -1.0, +1.0);
#else
        // Alternative way. Might be better. Don't know.
        _rendering->camera()->setProjectionOrtho(-0.5f / g_opengl_scale_ratio, 
                                                 (current_width - 0.5f) / g_opengl_scale_ratio, 
                                                 -0.5f / g_opengl_scale_ratio, 
                                                 (current_height - 0.5f) / g_opengl_scale_ratio, 
                                                 -1.0, +1.0);
#endif
      } else {

        _rendering->camera()->setProjectionOrtho(-0.5f);

      }


      viewport->setClearColor(get_vec4(t2_data->background_color));
      //glClear(GL_STENCIL_BUFFER_BIT|GL_ACCUM_BUFFER_BIT|GL_DEPTH_BUFFER_BIT|GL_COLOR_BUFFER_BIT); makes no difference.
      
#if TEST_TIME
      dur1 = TIME_get_ms();
#endif
      
      _rendering->transform()->eraseAllChildren();
      
      _rendering->transform()->addChild(t2_data->scroll_transform.get());
      //_rendering->transform()->addChild(t2_data->linenumbers_transform.get());
      _rendering->transform()->addChild(t2_data->scrollbar_transform.get());
      _rendering->transform()->addChild(t2_data->playcursor_transform.get());

      //vgscene = new vl::SceneManagerVectorGraphics;
          
      vgscene->vectorGraphicObjects()->clear();
      vgscene->vectorGraphicObjects()->push_back(t2_data->vg.get());

#if TEST_TIME
      dur2 = TIME_get_ms();
#endif
      
      vgscene->computeBounds(); // This should always be a no-op. If not, we could create a new vgscene in T2 each time.

#if TEST_TIME
      dur25 = TIME_get_ms();
#endif

      if (use_t2_thread)
        old_t2_datas.push_back(old_t2_data);
      else
        delete old_t2_data;

#if !defined(RELEASE)
      if (old_t2_datas.size() > 1)
        printf("  Warning: old_t2_data is growing: %d\n", old_t2_datas.size());
#endif
      
#if TEST_TIME
      dur3 = TIME_get_ms();
#endif
    }
        
    if (t2_data==NULL){
      //printf("Retfalse1\n");
      return false;
    }

    const SharedVariables *sv = GE_get_shared_variables(t2_data->painting_data);
    if (is_playing)
      if (sv->block_is_visible==false || sv->block!=sv->curr_playing_block)
        is_playing = false; // I.e. we are not rendering the block that is currently playing (if any).

    //printf("sv->curr_playing_block: %d\n", sv->curr_playing_block==NULL ? -1 : sv->curr_playing_block->l.num);
    
    if (new_t2_data!=NULL)// && !is_playing)
      GE_set_curr_realline(sv->curr_realline);
      
    double blocktime = 0.0;

    int playing_blocknum = -1;
  
    if (is_playing){
      
      if ((sv->curr_playing_block==NULL || sv->block!=sv->curr_playing_block)) { // Check that our blocktime belongs to the block that is rendered.
        
        if (new_t2_data!=NULL && use_t2_thread)
          T3_t2_data_picked_up_but_old_data_will_be_sent_back_later();
        
        if (t2_data_can_be_used){
          //printf("Waiting...\n");
          _rendering->render();
          return true;
        }else{

          //printf("Retfalse2. old_t2_datas.size: %d. sv->curr_playing_block==NULL (%d) || sv->block!=sv->curr_playing_block (%d)\n",old_t2_datas.size(), sv->curr_playing_block==NULL, sv->block!=sv->curr_playing_block);
          //printf("  Wait.gakk\n");
          return false; // Returning false uses 100% CPU on Intel gfx / Linux, and could possibly cause jumpy graphics, but here we are just waiting for the block to be rendered.
        }
      }

      playing_blocknum = sv->curr_playing_block->l.num;
        
      blocktime = ATOMIC_DOUBLE_GET(sv->curr_playing_block->player_time);
      //if (blocktime < -50)
      //  printf("blocktime: %f\n",blocktime);
      
#if !defined(RELEASE)
      if (equal_doubles(blocktime, -100) || blocktime>=0.0){
      }else{
        fprintf(stderr,"blocktime: %f\n",blocktime);
        //abort();
      }
#endif

      //R_ASSERT_NON_RELEASE(blocktime==-100 || blocktime>=0.0);

      //printf("blocktime: %f\n",blocktime);
      
      if (blocktime < 0.0) {  // Either the block hasn't started playing yet (sequencer cursor is inside a pause), or we just switched block and waiting for a proper blocktime to be calculated.
          
        if (new_t2_data!=NULL && use_t2_thread)
          T3_t2_data_picked_up_but_old_data_will_be_sent_back_later();
        
        if (t2_data_can_be_used  || !equal_doubles(blocktime, -100.0)){
          _rendering->render();
          //printf("   rettrue1\n");
          return true;
        } else {
          //printf("Retfalse3\n");
          return false;
        }
      }

    }

    double current_realline_while_playing =
      is_playing
      ? find_current_realline_while_playing(sv, blocktime)
      : 0.0;
    
    R_ASSERT_NON_RELEASE(current_realline_while_playing >= 0);

    int current_realline_while_not_playing = ATOMIC_GET(g_curr_realline);
    
    double till_realline =
      ATOMIC_GET_RELAXED(sv->root->play_cursor_onoff)
      ? current_realline_while_not_playing
      : is_playing
        ? current_realline_while_playing
        : current_realline_while_not_playing;

    Play_set_curr_playing_realline(
                                   is_playing ? (int)current_realline_while_playing : current_realline_while_not_playing,
                                   playing_blocknum
                                   );
    
    float scroll_pos = GE_scroll_pos(sv, till_realline);

    
    if (player_id != ATOMIC_GET(pc->play_id)) {// In the very weird and unlikely case that the player has stopped and started since the top of this function (the computer is really struggling), we return false
      
      if (new_t2_data!=NULL && use_t2_thread)
        T3_t2_data_picked_up_but_old_data_will_be_sent_back_later();

      //printf("Retfalse4\n");
      return false;
    }

    if (!is_playing && equal_floats(scroll_pos, last_scroll_pos) && new_t2_data==NULL) {
      if (t2_data_can_be_used){
        _rendering->render();
        //printf("   rettrue2\n");
        return true;
      }else{
        //printf("Retfalse5\n");
        return false; // TODO: Check if this still uses 100% cpu on Intel/linux. It's a little bit wasteful to render the same frame again and again while not playing just because of one driver on one platform.
      }
    }


    //printf("scrolling\n");

    //_scissor->enable(_rendering->camera()->viewport());

    // Set render mask
    if(1){
#if 1
      float upper = scroll_pos;
      float lower = scroll_pos + current_height;
#else
      // testing
      float middle = scroll_pos + current_height/2;
      float upper = middle - 100;
      float lower = middle + 100;
#endif

      uint32_t mask;
            
      if (sv->block_is_visible) 
        mask = getMask(upper,lower,GE_get_slice_size(t2_data->painting_data));
      else
        mask = (uint32_t)-1; // i.e. paint everything.
      
      _rendering->setEnableMask(mask);
    }
    

    // scroll
    {
      vl::mat4 mat = vl::mat4::getRotation(0.0f, 0, 0, 1);
      mat.translate(0,scroll_pos,0);
      t2_data->scroll_transform->setLocalAndWorldMatrix(mat);
    }

#if 0
    // linenumbers
    {
      vl::mat4 mat = vl::mat4::getRotation(0.0f, 0, 0, 1);
      mat.translate(0,scroll_pos,0);
      t2_data->linenumbers_transform->setLocalAndWorldMatrix(mat);
    }
#endif
    
    // scrollbar
    {
      vl::mat4 mat = vl::mat4::getRotation(0.0f, 0, 0, 1);
      float y1 = get_scrollbar_scroller_y1(till_realline, sv->num_reallines - (is_playing?0:1), sv->scrollbar_height, sv->scrollbar_scroller_height);
      float scrollbarpos = scale(y1,
                                 0, sv->scrollbar_height,
                                 0, -sv->scrollbar_height);
      /*
      float scrollbarpos = scale(till_realline,
                                 0, sv->num_reallines - (is_playing?0:1),
                                 -2, -(sv->scrollbar_height - sv->scrollbar_scroller_height - 1)
                                 );
      */
      //printf("bar_length: %f, till_realline: %f. scrollpos: %f, pos: %f, max: %d\n",bar_length,till_realline, scrollpos, pos, window->leftslider.x2);
      mat.translate(0,scrollbarpos,0);
      t2_data->scrollbar_transform->setLocalAndWorldMatrix(mat);
    }
    
    // playcursor (i.e. the red horizontal line which is moving down when playing and the "PC" button is pressed)
    {
      vl::mat4 mat = vl::mat4::getRotation(0.0f, 0, 0, 1);

      float playcursorpos = scroll_pos - GE_scroll_pos(sv, current_realline_while_playing);

      mat.translate(0,playcursorpos,0);
      t2_data->playcursor_transform->setLocalAndWorldMatrix(mat);
    }

          
#if TEST_TIME
    double dur4 = TIME_get_ms();
#endif
    
    GE_update_triangle_gradient_shaders(t2_data->painting_data, scroll_pos);

#if TEST_TIME
    double dur5 = TIME_get_ms();
#endif

    t2_data_can_be_used = true;
    _rendering->render();

    // [ref 1]
    // Must do this after calling render() (and possibly after transform->setLocalAndWorldMatrix() is called)
    // Could seem like some objects are cached and not removed until render() is called.
    //
    // If we don't do it here, we get tsan hits when vl::Object->decReferences() is called,
    // and the objects may also be freed here instead of in the T2 thread.
    //
    send_back_old_t2_data(false);
    
    
#if TEST_TIME
    double dur6 = TIME_get_ms();
    double total = dur6-start1;

    if (true) //false && total>15)// || new_t2_data != NULL)
      printf("%f: mask: %s  Total: %f. s-1: %f, 1-2: %f, 2-3: %f - %f, 3-4: %f, 4-5: %f, 5-6: %f. Num actors: %d\n",
             scroll_pos,
             std::bitset<32>(_rendering->enableMask()).to_string().c_str(),
             total,
             dur1-start1,
             dur2-dur1,
             dur25-dur2,
             dur3-dur25,
             dur4-dur3,
             dur5-dur4,
             dur6-dur5,
             t2_data->vg->actors()->size());
#endif
    
    safe_volatile_float_write(&g_scroll_pos, scroll_pos);
    
    last_scroll_pos = scroll_pos;
    last_current_realline_while_playing = current_realline_while_playing;

    //printf("    GOTIT\n");
    return true;
  }


  int _num_update_event_calls = 0;
  QElapsedTimer _update_event_counter_timer;

  // OpenGL thread
  void assertHealthyVBlank(void){
    _num_update_event_calls++;
    
    if (_update_event_counter_timer.elapsed() >= 1000){
      int measured_vblank = _num_update_event_calls;

      double system_vblank = GL_get_vblank();

      int max_healthy_vblank;
      if (system_vblank >= 0)
        max_healthy_vblank = round(1000.0/system_vblank) + 20;
      else
        max_healthy_vblank = 260;

      //printf("vblank: %d\n",measured_vblank);
      
      if (measured_vblank > max_healthy_vblank)
        RT_message("Warning: Very high refresh rate detected.\n"
                   "Got %dHz. Expected maximum %dHz.\n\n"
                   "You are probably experience very slow graphical performance now.\n\n"
                   "Please check that VSync is turned on both under\n"
                   "Edit -> Preferences -> OpenGL, and globally in your system.",
                   measured_vblank,
                   max_healthy_vblank);

      _num_update_event_calls = 0;
      _update_event_counter_timer.restart();
    }
  }

#if defined(RELEASE)
#define USE_JUCE_CPU_PROTECTION_LOGIC 0 // The JUCE logic causes stuttering in the graphics for a few seconds after returning to the program.
#else  
#define USE_JUCE_CPU_PROTECTION_LOGIC 1 // Uses less CPU
#endif

#if USE_JUCE_CPU_PROTECTION_LOGIC && !defined(FOR_MACOSX)
  double lastSwapTime = monotonic_seconds() * 1000.0;
  int underrunCounter = 0;
#endif

  DEFINE_ATOMIC(bool, _dont_swap_right_now) = false; // written to in main thread. Both written to and read from in the opengl thread.
  int _dont_swap_right_now_downcount = 0; // only accessed from opengl thread
  //double _dont_swap_right_now_last_time = 0.0; // only accessed from opengl thread. Need to use a timer in case waiting for vblank isn't working (and it isn't always working on OSX).

  bool maybe_dont_swap_right_now(void) {
    //printf("    ... maybe_dont_swap called. _dont_swap_right_now: %d. _dont_swap_right_now_downcount: %d\n", ATOMIC_GET(_dont_swap_right_now), _dont_swap_right_now_downcount);
    if(ATOMIC_GET(_dont_swap_right_now)){
      msleep(17);
      return true;
    }
    
    if(_dont_swap_right_now_downcount > 0){
      msleep(17);
      _dont_swap_right_now_downcount--;
      if(_dont_swap_right_now_downcount==0){
#if !USE_LAYOUT_COVER
        hide_cover();
#else
        THREADING_run_on_main_thread_async([]{
            g_last_resize_time = TIME_get_ms();
            schedule_hide_layout_cover();
          });
#endif
      }
      
      /*
      double time = TIME_get_ms();
      if ((time - _dont_swap_right_now_last_time) > 15){
        _dont_swap_right_now_downcount--;
        _dont_swap_right_now_last_time = time;
      }
      */
      return true;
    }

    return false;
  }
  
  // OpenGL thread
  void swap(void){

    if (maybe_dont_swap_right_now())
      return;

    if (USE_GL_LOCK)
      mutex.lock();

#if USE_SWAP_MUTEX
    radium::ScopedMutex swap_mutex_lock(swap_mutex);
#endif
    
    void *juce_lock = NULL;

    if (doLockJuceWhenSwappingOpenGL())
      juce_lock = JUCE_lock();

    // Swap to the newly rendered buffer
    if (openglContext()->hasDoubleBuffer()) {

#if USE_JUCE_CPU_PROTECTION_LOGIC
      double now;
#endif

      //printf("Swapping\n");

      if (juce_lock==NULL){
        radium::ScopedMutex lock(JUCE_show_hide_gui_lock);
#if USE_JUCE_CPU_PROTECTION_LOGIC
        now = monotonic_seconds() * 1000.0;
#endif
        
        if(!maybe_dont_swap_right_now()) // Check again in case it was set to false in the meantime.
          openglContext()->swapBuffers();

      }else{
#if USE_JUCE_CPU_PROTECTION_LOGIC
        now = monotonic_seconds() * 1000.0;
#endif
        if(!maybe_dont_swap_right_now()) // Check again in case it was set to false in the meantime.
          openglContext()->swapBuffers();
      }

      //printf("update: %fms\n", time_estimator.get_vblank());

#if !defined(FOR_MACOSX)
      if (doHighCpuOpenGlProtection()) {
        double vblank = GL_get_vblank();
        if (equal_doubles(vblank, -1))
          vblank = time_estimator.get_vblank();

#if !USE_JUCE_CPU_PROTECTION_LOGIC
        prevent_high_cpu_in_swap.check(vblank, 0);
#else
        // This code is copied from JUCE.
        
        const double swapTime = (monotonic_seconds() * 1000.0) - now;
        const int frameTime = (int) (now - lastSwapTime);
        const int minSwapTimeMs = (int)vblank;
        //printf("minSwapTimeMs: %d\n", minSwapTimeMs);
        
        const bool maybe = false; //scale(rand(), 0, RAND_MAX, 0, 10) < 3;
          
        if (maybe || (swapTime < 0.5 && frameTime < minSwapTimeMs - 3)) {

          if (underrunCounter > 3) {

              double how_long = 2 * (minSwapTimeMs - frameTime);
              //Thread::sleep (2 * (minSwapTimeMs - frameTime));
#if 0 //!defined(RELEASE)
              printf("Sleeping to avoid CPU lock. How_long: %f, underrunCounter: %d, swapTime: %f, frameTime: %d, minSwapTimeMs: %d. now: %f\n",
                     how_long,
                     underrunCounter,
                     swapTime,
                     frameTime,
                     minSwapTimeMs,
                     monotonic_seconds());
#endif
              if (how_long > 0)
                msleep(how_long);
              
              now = monotonic_seconds() * 1000.0; //Time::getMillisecondCounterHiRes();

          } else {

            ++underrunCounter;

          }

        } else {
          
          if (underrunCounter > 0)
            --underrunCounter;
          
        }

        lastSwapTime = now;
#endif
      }
#endif // !defined(FOR_MACOSX)
    }

    if (juce_lock != NULL)
      JUCE_unlock(juce_lock);

    if (USE_GL_LOCK)
      mutex.unlock();

    /*
    'Remove vsync health check since swapBuffers returns immediately when window is not visible on at least two separate opengl drivers. Instead write a warning in capitol letters to the vsync option in the preferences. If someone still wants to disable vsync, its really their own fault.'
    */
    //assertHealthyVBlank();
  }

#if USE_COVER
  
  class Cover : QObject {
    QImage &_image = g_cover_image;
    radium::Mutex &_image_mutex = g_image_mutex;

    struct CoverWidget : public QWidget {

      //QWidget *_parent;
      Cover *_cover ;
      double _start_time = TIME_get_ms();

      CoverWidget(QWidget *parent, Cover *cover)
        : QWidget(parent)
          //, _parent(parent)
        , _cover(cover)
      {
        setMouseTracking(true);
      }
    
      void paintEvent( QPaintEvent *e ){
        TRACK_PAINT();
        QPainter p(this);
        
        {
          radium::ScopedMutex lock(_cover->_image_mutex);

          p.fillRect(rect(), get_qcolor(LOW_EDITOR_BACKGROUND_COLOR_NUM));

          if (true){ //if ((TIME_get_ms() - _start_time) > 50){

            QRect cover_rect = rect().adjusted(rect().width()/4, rect().height()/4, -rect().width()/4, -rect().height()/4);
            
            p.fillRect(cover_rect.adjusted(-20,-20,20,20), mix_colors(get_qcolor(LOW_EDITOR_BACKGROUND_COLOR_NUM), Qt::black, 0.5));
            
            QRect text_rect(0, 0, width(), cover_rect.y()-20);
            p.drawText(text_rect, Qt::AlignCenter, "Resizing OpenGL");
            
            if(!_cover->_image.isNull()){
              p.drawImage(cover_rect, _cover->_image);
            }
          }
        }
      }

    };

  public:

    CoverWidget *_widget = NULL;

    QSize _size;
    
    Cover(QWidget *parent, QSize size)
      : _size(size)
    {

#if 1
        IsAlive is_alive(this);
      // We are in the main thread here, but since we are called from the resize event, we don't want to create a new QWidget now.
      // Doing Qt stuff inside event handlers has led to strange and untracable crashes and other strange behaviors in the past.
      THREADING_run_on_main_thread_async([is_alive, this, parent, size]{

          if (!is_alive)
            return;

          _widget = new CoverWidget(parent, this);
          _widget->resize(size);
          _widget->move(0,0);

          _widget->show();
        },
        true);
#endif
    }

    ~Cover(){
      delete _widget;
    }

    bool _has_grabbed_image = false;
    
    bool has_grabbed_image(void){      
      radium::ScopedMutex lock(_image_mutex);
      return _has_grabbed_image;
      //return !_image.isNull();
    }

    bool maybe_grab_frame_buffer(Q_QL_Widget *parent){
      radium::ScopedMutex lock(_image_mutex);
      
      if(_has_grabbed_image==false){
        
        if(true){
          
          printf("                GRABBING OpenGL frame buffer\n");
#if USE_QOPENGL
          _image = parent->grabFramebuffer();
#else
          _image = parent->grabFrameBuffer();
#endif
          //_image = parent->renderPixmap().toImage();
          _has_grabbed_image = true;
          
        }else{
          
          IsAlive is_alive(this);
          
          THREADING_run_on_main_thread_async([is_alive, this, parent]{
              if (!is_alive)
                return;
              
              radium::ScopedMutex lock(_image_mutex);
              if(_has_grabbed_image==false){
                _image = parent->grab().toImage();
                _has_grabbed_image = true;
              }
            });
        }
#if 0
        IsAlive is_alive(this);

        // We are in the main thread here, but since we are called from the resize event, we don't want to create a new QWidget now.
        // Doing Qt stuff inside event handlers has led to strange and untracable crashes and other strange behaviors in the past.
        THREADING_run_on_main_thread_async([is_alive, this, parent]{
            
            if (!is_alive)
              return;
            
            _widget = new CoverWidget(parent, this);
            _widget->resize(_size);
            _widget->move(0,0);
            _widget->show();
          });
#endif
        
        return true;
        
      } else {
        
        return false;
        
      }
      
    }

  };

  radium::Mutex _cover_mutex;
  Cover *_cover = NULL;
#endif

  
  void hide_cover(void){

#if !defined(FOR_MACOSX)
    if (!_use_cover)
      return;
#endif
    

#if USE_COVER
    IsAlive is_alive(this);

    THREADING_run_on_main_thread_async([is_alive, this]{

#if defined(FOR_MACOSX)
        // Workaround for sequencer, playlist and mixer not being updated when moving dragger.
        // Doesn't seem like Qt cares very much about macos. There are lots of these types of bugs coming and going for each new version of Qt.
        
        //printf(".......Updating it\n");
        //SEQUENCER_update(SEQUPDATE_EVERYTHING); // This one is also updated in gui_setSplitterSizes, which should take care of most updates.

        
        //gui_update(gui_getMainXSplitter(),-1,-1,-1,-1);
        QTimer::singleShot(50, [](){
            //printf(".......Updating 50\n");
            updateWidgetRecursively(API_get_main_ysplitter()); // This updates everything in the main window. Do this to update the Y splitter widget itself.
            //gui_updateRecursively(gui_getMainXSplitter()); // Yes, it's necessary to update recursively.
            //SEQUENCER_update(SEQUPDATE_EVERYTHING);
          });
        
        QTimer::singleShot(250, [](){
            //printf(".......Updating 250\n");
            updateWidgetRecursively(API_get_main_ysplitter()); // This updates everything in the main window. Do this to update the Y splitter widget itself.
            //          gui_updateRecursively(gui_getMainXSplitter());
            //SEQUENCER_update(SEQUPDATE_EVERYTHING);
          });
        
        QTimer::singleShot(450, [](){
            //printf(".......Updating 450\n");
            updateWidgetRecursively(API_get_main_ysplitter()); // This updates everything in the main window. Do this to update the Y splitter widget itself.
            //SEQUENCER_update(SEQUPDATE_EVERYTHING); // Must have this one though, in case the y splitter is not updated from the dragger. (not needed to call this when updating the y splitter)
          });
#endif

        if (_use_cover) {

          QTimer::singleShot(80, [is_alive, this](){
              if (!is_alive)
                return;
              
              {
                radium::ScopedMutex lock(_cover_mutex);
                delete _cover;
                _cover = NULL;
              }
            });
        }
        
      });
#else
    
#endif
  }

  void show_cover(void){

    if (!_use_cover)
      return;
        
#if USE_COVER
    radium::ScopedMutex lock(_cover_mutex);

    if (_cover == NULL){

      _cover = new Cover(this, size());

#if 0
      if (_cover->_widget != NULL)
        _cover->_widget->update();
#else
      for(int i=0;i<200/30;i++){
        if(true || _cover->has_grabbed_image()){
          if (_cover->_widget != NULL)
            _cover->_widget->update();
          break;
        }
        
        {
          _cover_mutex.unlock();
          msleep(30);
          _cover_mutex.lock();
        }
      }
#endif
    } else {
      if (_cover->_widget != NULL){

        IsAlive is_alive(this);

        // We are in the main thread, but we don't want to call resize() inside a qt event handler.
        THREADING_run_on_main_thread_async([is_alive, this]{
            if (!is_alive)
              return;

            radium::ScopedMutex lock(_cover_mutex);
            if (_cover!=NULL && _cover->_widget != NULL){
              _cover->_widget->resize(size());
            }

            if (_cover!=NULL && _cover->_widget != NULL)
              _cover->_widget->update();
          },
          true);
      }
    }
#endif
  }

public:

  /** Event generated when the bound OpenGLContext does not have any other message to process 
      and OpenGLContext::continuousUpdate() is set to \p true or somebody calls OpenGLContext::update(). */
  // OpenGL thread
  void updateEvent() override {
    //{static double last_time = 0; static int counter =0; double nowtime = TIME_get_ms(); printf("   Counter: %d. Time: %f\n", counter++, nowtime-last_time);last_time = nowtime;}

#if !defined(RELEASE)
    if (Q_QL_Widget::context()->isValid()==false){
      // hmm.
      abort();
    }
#endif
    
#if THREADED_OPENGL
    const bool handle_current = true;
#endif
    
#if USE_COVER
    if (_use_cover) {
      radium::ScopedMutex lock(_cover_mutex);
      if (_cover != NULL){
        if(_cover->maybe_grab_frame_buffer(this)){
          msleep(15);
          return;
        }
      }
    }
#endif

#ifdef FOR_MACOSX
    if (maybe_dont_swap_right_now()) // Have to do this before doing anything on OSX to avoid crash.
      return;
#endif
    
    radium::ScopedMutex lock(make_current_mutex);
#if THREADED_OPENGL
    
    if (handle_current)
      Q_QL_Widget::makeCurrent();  // Not sure about this. updateEvent() is called immediately again after it returns.
#endif

    if (g_order_pause_gl_thread.tryWait()) {
      g_ack_pause_gl_thread.notify_one();
      OS_WaitAtLeast(200);
    }
    
    if (g_order_make_current.tryWait()) {
#if THREADED_OPENGL
      if (!handle_current)
        Q_QL_Widget::makeCurrent();
#endif
      g_ack_make_current.notify_one();
    }

    ATOMIC_SET(g_is_currently_pausing, false);
    
    if (ATOMIC_GET_RELAXED(GE_version_string)==NULL) {
      ATOMIC_SET(GE_vendor_string, V_strdup((const char*)glGetString(GL_VENDOR)));
      ATOMIC_SET(GE_renderer_string, V_strdup((const char*)glGetString(GL_RENDERER)));
      ATOMIC_SET(GE_version_string, V_strdup((const char*)glGetString(GL_VERSION)));
      printf("vendor: %s, renderer: %s, version: %s \n",(const char*)ATOMIC_GET(GE_vendor_string),(const char*)ATOMIC_GET(GE_renderer_string),(const char*)ATOMIC_GET(GE_version_string));
      
      ATOMIC_SET(GE_opengl_version_flags, QGLFormat::openGLVersionFlags());
      //abort();
    }


    ATOMIC_SET(g_has_updated_at_least_once, true);

#if THREADED_OPENGL
#if USE_QT5
    if (ATOMIC_GET(g_qtgui_has_started_step2)==false || ATOMIC_GET(_main_window_is_exposed)==false){
      if (handle_current)
        Q_QL_Widget::doneCurrent();
      //printf("hepp1\n");
      lock.wait_and_pause_lock(200);
      return;
    }
#endif
#endif

    double overridden_vblank_value = ATOMIC_DOUBLE_GET(override_vblank_value);
    
#if USE_QT5
    time_estimator.set_vblank(overridden_vblank_value);    
#else
    if (has_overridden_vblank_value==false && overridden_vblank_value > 0.0) {

      time_estimator.set_vblank(overridden_vblank_value);
      ATOMIC_SET(is_training_vblank_estimator, false);
      has_overridden_vblank_value = true;

    } else {

      if(ATOMIC_GET(is_training_vblank_estimator))
        ATOMIC_SET(is_training_vblank_estimator, time_estimator.train());

    }
#endif

    // This is the only place the opengl thread waits. After swap()/msleep() returns, and updateEvent() is finished, updateEvent is called again immediately.

    {
#if !USE_QT5
      if (ATOMIC_GET(is_training_vblank_estimator)==true) {

        //printf("hepp3\n");
        
        swap();

      } else
#endif
        {

          //printf("hepp4\n");
          
          bool must_swap;
          
          {
            
#if USE_SWAP_MUTEX            
            //radium::ScopedMutex swap_mutex_lock(swap_mutex);
#endif
            radium::ScopedMutex lock(draw_mutex);

            //printf("hepp5\n");
            
            if (canDraw()) {
              must_swap = draw();
            } else {
              must_swap = true;
            }
          }

          //printf("Must swap: %d\n", must_swap);

          
          if (must_swap)
            swap();
          
          else if (g_safe_mode || !sleep_when_not_painting) // probably doesn't make any sense setting sleep_when_not_painting to false. Besides, setting it to false may cause 100% CPU usage (intel gfx) or very long calls to GL_lock() (nvidia gfx).
            swap();

          else {
            if (old_t2_datas.size() >= T3_TO_T2_QUEUE_SIZE){
              // Something is very wrong, and it's risky to free here [ref 1], but we can't let this data grow forever.
              // It might be safe to call this function at any time though when the 'keep_last_element' argument is true. Not sure.
              swap(); // Swap too. Maybe it's safer to free this data after swapping even though it hasn't been rendered.
              send_back_old_t2_data(true);
            } else
              msleep(time_estimator.get_vblank());  // Don't have to call swap() here. We sleep instead since swap() in some GFX drivers returns immediately if calling swap without rendering.
          }
          
          //if (g_safe_mode)
          //  GL_unlock();
        }
      
    }

    ATOMIC_SET(g_has_updated_at_least_once, true);

#if THREADED_OPENGL
    if (handle_current)
      Q_QL_Widget::doneCurrent();
#endif
  }

  // Main thread
  void set_vblank(double value){
    ATOMIC_DOUBLE_SET(override_vblank_value, value);
  }

  void resizeEvent(QResizeEvent *qresizeevent) override {
#if THREADED_OPENGL
    if (!this->GL_has_initialized()){
      vlQt5::Qt5ThreadedWidget::Q_QL_Widget::resizeEvent(qresizeevent); // Trigger call to QOpenGLWidget::initializeGL().
      return;
    }
#endif
    
    g_last_resize_time = TIME_get_ms();
      
    radium::ScopedResizeEventTracker resize_event_tracker;

    show_cover();
    
    ATOMIC_SET(_dont_swap_right_now, true);

#if USE_LAYOUT_COVER
    show_layout_cover();
#endif    

    //printf("   Resize. g_editor->window: %p. gl_widget: %p. editor_layout_widget: %p\n", g_editor->window, g_editor->gl_widget, g_editor->editor_layout_widget);
    if (g_editor->window != NULL)
      calculateNewWindowWidthAndHeight(g_editor->window);
    
    GE_set_height(qresizeevent->size().height());

#if USE_QT5
#  if THREADED_OPENGL
    vlQt5::Qt5ThreadedWidget::resizeEvent(qresizeevent);
#  else
    vlQt5::Qt5Widget::resizeEvent(qresizeevent);
#  endif
#else
    vlQt4::Qt4ThreadedWidget::resizeEvent(qresizeevent);
#endif

    if (Undo_num_undos()==0 && !CanRedo() && isIllegalFilepath(dc.filename)){

      // Schedule it to run a little bit later just to be safe. minimizeBlockTracks is doing a lot so it's hard to keep track at all times of whether it does Qt operations or not.
      QTimer::singleShot(1, []{
          radium::ScopedIgnoreUndo ignore;
          //MinimizeBlock_CurrPos(window, wblock);
          minimizeBlockTracks(-1, -1, false); // maximize track widths.
        });
    }

  }

  
  /** Event generated when the bound OpenGLContext is resized. */
  // OpenGL thread
  void resizeEvent(int w, int h) override {
    //printf("|||||||||||||||||||||||--------------------------resisizing %d %d\n",w,h);
#if THREADED_OPENGL
    is_patched();
#endif
    
#if USE_COVER
    if(0){
      radium::ScopedMutex lock(_cover_mutex);
      if (_cover != NULL)
        _cover->maybe_grab_frame_buffer(this);
    }
#endif
    
    if (w<32)
      w = 32;
    if (h<32)
      h = 32;
    
    current_width = w;
    current_height = h;
    
    initEvent();

    // _dont_swap_right_now_downcount and _dont_swap_right_now prevent crash on OSX when resizing. At least I haven't seen crash after adding these two.
    //
    // Note: The crash didn't disappear when running single-threadedly.
    //
    // Doesn't seem necessary on other platforms to avoid crashes, but it seems to lower flickering when resizing.
    //
    // For each time this variable is decremented, we also wait 15ms, so spent_time(10) is effectively between 15ms*10 and 30ms*10 = 150-300ms. (on OSX the value is much much closer to 150ms than 300ms)

#ifdef FOR_MACOSX
    _dont_swap_right_now_downcount = 10;
#else
    _dont_swap_right_now_downcount = 10;
#endif

      ATOMIC_SET(_dont_swap_right_now, false);

      GFX_ScheduleEditorRedraw();

      ATOMIC_SET(g_has_resized_at_least_once, true);
  }
  
  // The rest of the methods in this class are virtual methods required by the vl::UIEventListener class. Not used.

  /** Event generated whenever setEnabled() is called. */
  void enableEvent(bool enabled) override {
    printf("enableEvent %d\n",(int)enabled);
  }

  /** Event generated whenever a listener is bound to an OpenGLContext context. */
  void addedListenerEvent(vl::OpenGLContext*) override {
    printf("addedListenerEvent\n");
  }

  /** Event generated whenever a listener is unbound from an OpenGLContext context. */
  void removedListenerEvent(vl::OpenGLContext*) override {
    printf("removedListenerEvent\n");
  }
  
  /** Event generated when the mouse moves. */
  void mouseMoveEvent(int x, int y) override {
    R_ASSERT_NON_RELEASE(false);
    printf("mouseMove %d %d\n",x,y);
    //_rendering->sceneManagers()->clear();
    //create_block();
    //initEvent();
  }
  
  /** Event generated when one of the mouse buttons is released. */
  void mouseUpEvent(vl::EMouseButton button, int x, int y) override {
    R_ASSERT_NON_RELEASE(false);
    printf("mouseMove %d %d\n",x,y);
    //_rendering->sceneManagers()->clear();
    //create_block(_rendering->camera()->viewport()->width(), _rendering->camera()->viewport()->height());
    //initEvent();
  }
  
  /** Event generated when one of the mouse buttons is pressed. */
  void mouseDownEvent(vl::EMouseButton button, int x, int y) override {
    R_ASSERT_NON_RELEASE(false);
    printf("mouseMove %d %d\n",x,y);
    //_rendering->sceneManagers()->clear();
    //create_block();
    //initEvent(); 
  }
  
  /** Event generated when the mouse wheel rotated. */
  void mouseWheelEvent(int n) override {
    R_ASSERT_NON_RELEASE(false);
    printf("mouseWheel %d\n",n);
  }

  // Necessary to avoid error with clang++.
  void keyPressEvent(QKeyEvent *event) override {
#if USE_QT5
#  if THREADED_OPENGL
    vlQt5::Qt5ThreadedWidget::keyPressEvent(event);
#  endif
#else
    vlQt4::Qt4ThreadedWidget::keyPressEvent(event);
#endif
  }

  /** Event generated when a key is pressed. */
  void keyPressEvent(unsigned short unicode_ch, vl::EKey key) override {
    printf("key pressed\n");

    //_rendering->sceneManagers()->clear();
    //create_block();
    //initEvent();
  }

  // Necessary to avoid error with clang++.
  void keyReleaseEvent(QKeyEvent *event) override {
#if USE_QT5
#  if THREADED_OPENGL
    vlQt5::Qt5ThreadedWidget::keyReleaseEvent(event);
#  endif
#else
    vlQt4::Qt4ThreadedWidget::keyReleaseEvent(event);
#endif
  }


  /** Event generated when a key is released. */
  void keyReleaseEvent(unsigned short unicode_ch, vl::EKey key) override {
  }
  
  /** Event generated when one or more files are dropped on the bound OpenGLContext's area. */
  void fileDroppedEvent(const std::vector<vl::String>& files) override {
  }
  
  /** Event generated when the bound OpenGLContext is shown or hidden. */
  void visibilityEvent(bool visible) override {
    printf("visibilityEvent %d\n",(int)visible);
  }
};


#if THREADED_OPENGL
static vl::ref<MyQtThreadedWidget> widget;
#else
static MyQtThreadedWidget *widget;
#endif

/*
QSurfaceFormat GL_get_qsurface_format(void){
  return QGLFormat::toSurfaceFormat(widget->format());
}
*/

#if !USE_QT5
static bool use_estimated_vblank_questionmark(){
  return SETTINGS_read_bool("use_estimated_vblank", false);
}

static void store_use_estimated_vblank(bool value){
  return SETTINGS_write_bool("use_estimated_vblank", value);
}

static bool have_earlier_estimated_value(){
return SETTINGS_read_double("vblank", -1.0) > 0.0;
}
#endif


static bool g_has_started_t2_thread = false;

static bool maybe_start_t2_thread(void){
  if (g_has_started_t2_thread)
    return true;
  
  QWindow *qwindow = widget->windowHandle();  
  if (qwindow!=NULL){

    QScreen *qscreen = qwindow->screen();
    if (qscreen!=NULL) {
      
      radium::ScopedMutex lock(make_current_mutex);

      auto *glcontext = widget->context();
      if (glcontext==NULL)
        return false;

      /*
      if (glcontext->initialized()==false) // QGLContext::initialized() is a protected method. I guess it's no point using it like this then.
        return false;
      */
      
      if (glcontext->isValid()==false)
        return false;

#if USE_QOPENGL
      QOpenGLContext *openglcontext = glcontext;
#else
      QOpenGLContext *openglcontext = glcontext->contextHandle();
      if (openglcontext->isValid()==false)
        return false;
#endif
      
      T1_start_t2_thread(openglcontext);
      g_has_started_t2_thread = true;
      return true;
    }
  }

  return false;
}

static double get_refresh_rate(void){
  
  QWindow *qwindow = widget->windowHandle();
  if (qwindow!=NULL){

    double ratio = qwindow->devicePixelRatio();
    //ratio = 0.5;

    if (fabs(ratio-1.0) > 0.01){

      /*
      static bool has_shown_warning = false;

      if (has_shown_warning==false){
        RT_message("Note: High DPI display detected. Radium has not supported high DPI display very long. If you notice strange graphics, please report it.");
        has_shown_warning=true;
      }
      */
      
      g_opengl_scale_ratio = ratio;
      
      // Make sure editor font is scaled.
      if(root!=NULL && root->song!=NULL && root->song->tracker_windows!=NULL){
        setFontValues(root->song->tracker_windows);
      }
      
    }
    
    QScreen *qscreen = qwindow->screen();
    if (qscreen!=NULL) {

      maybe_start_t2_thread();

      return qscreen->refreshRate();
    }

  }

  return -1;
}

static DEFINE_ATOMIC(bool, g_has_found_refresh_rate) = false;
static DEFINE_ATOMIC(double, g_vblank) = 1000 / 60.0;
double GL_get_vblank(void){
  if (ATOMIC_GET(g_has_found_refresh_rate)==false)
    return -1;
  else
    return ATOMIC_DOUBLE_GET(g_vblank);
}

int GL_maybe_notify_that_main_window_is_exposed(int interval){
  static int ret = 0;

#if USE_QT5
  
  if (ret==0){
    QWindow *window = widget->windowHandle();
    if (window != NULL){
      if (window->isExposed()) {
        ATOMIC_SET(widget->_main_window_is_exposed, true);
        ret = 1;
      }
    }
  }else{
    
    ret = 2;
    
    static int downcounter = 0;
    if (downcounter==0) {
#if !defined(RELEASE)
      printf("Checking refresh rate\n");
#endif
      double refresh_rate = get_refresh_rate();
      if (refresh_rate >= 0.5) {
        widget->set_vblank(1000.0 / refresh_rate);
        ATOMIC_DOUBLE_SET(g_vblank, 1000.0 / refresh_rate);
        ATOMIC_SET(g_has_found_refresh_rate, true);
        downcounter = 200 * 1000 / interval; // Check refresh rate every 200 seconds.
      }else{
        printf("Warning: Unable to find screen refresh rate\n");
        downcounter = 500 / interval; // Check again in 0.5 seconds
      }
    }else
      downcounter--;
  }

#else
  
  if (ret==0){
    ATOMIC_SET(widget->_main_window_is_exposed, true);
    ret = 1;
  } else
    ret = 2;
  
#endif
  
  return ret;
}
            
void GL_pause_gl_thread_a_short_while(void){

  if (!USE_GL_LOCK)
    return;
  
  if (GL_get_pause_rendering_on_off()==false)
    return;
  
  if (g_gl_widget_started == false) // deadlock without this check.
    return;

  if (ATOMIC_GET(g_is_currently_pausing))
    return;
    
  R_ASSERT_RETURN_IF_FALSE(g_gl_lock_visits>=1);
  if (g_gl_lock_visits>=2) // If this happens we are probably called from inside a widget/dialog->exec() call. Better not wake up the gl thread.
    return;

  ATOMIC_SET(g_is_currently_pausing, true);
  
  g_order_pause_gl_thread.signal();

  if (g_ack_pause_gl_thread.wait(&mutex, 4000)==false){  // Have a timeout in case the opengl thread is stuck
#ifdef RELEASE
    printf("warning: g_ack_pause_gl_thread timed out\n");
#else
    GFX_Message(NULL, "Debug mode warning: g_ack_pause_gl_thread timed out\n");
#endif
  }
}


void GL_EnsureMakeCurrentIsCalled(void){
  //printf("  Ensure make current\n");
  
  if (!USE_GL_LOCK)
    return;

  R_ASSERT_RETURN_IF_FALSE(g_gl_lock_visits>=1); 
  if (g_gl_lock_visits>=2) // If this happens we are probably called from inside a widget/dialog->exec() call. Better not wake up the gl thread.
    return;

  g_order_make_current.signal();

  if (g_ack_make_current.wait(&mutex, 4000)==false){  // Have a timeout in case the opengl thread is stuck
#ifdef RELEASE
    printf("warning: g_ack_make_current timed out\n");
#else
    GFX_Message(NULL, "warning: g_ack_make_current timed out\n");
#endif
  }
}

#if !USE_QT5
double GL_get_estimated_vblank(){
  return 1000.0 / SETTINGS_read_double("vblank", 60.0);
}

static void store_estimated_value(double period){
  printf("***************** Storing %f\n",1000.0 / period);
  SETTINGS_write_double("vblank", 1000.0 / period);
}

void GL_erase_estimated_vblank(void){
  SETTINGS_write_double("vblank", -1.0);
  store_use_estimated_vblank(false);
  GFX_Message2(NULL, true, "Stored vblank value erased. Restart Radium to estimate a new vblank value.");
}

#endif

void GL_set_vsync(bool onoff){
  SETTINGS_write_bool("vsync", onoff);
}

bool GL_get_vsync(void){
  return SETTINGS_read_bool("vsync", true);
}

void GL_set_multisample(int size){
  SETTINGS_write_int("multisample", size);
}

int GL_get_multisample(void){
  return R_BOUNDARIES(1, SETTINGS_read_int32("multisample", 4), 32);
}

void GL_set_safe_mode(bool onoff){
  printf("setting safe mode to %d\n",onoff);
  SETTINGS_write_bool("safe_mode", onoff);
}


static bool g_high_render_thread_priority = true;
bool GL_get_high_render_thread_priority(void){
  static bool s_has_inited = false;
  if (s_has_inited==false){
    g_high_render_thread_priority = SETTINGS_read_bool("high_render_thread_priority", g_high_render_thread_priority);
    s_has_inited = true;
  }
  
  return g_high_render_thread_priority;
}

void GL_set_high_render_thread_priority(bool onoff){
  printf("setting safe mode to %d\n",onoff);
  SETTINGS_write_bool("high_render_thread_priority", onoff);
  g_high_render_thread_priority = onoff;
  if (g_render_thread != NULL)
    g_render_thread->setPriority(onoff ? QThread::HighestPriority : QThread::NormalPriority);
}


bool GL_get_safe_mode(void){
  return SETTINGS_read_bool("safe_mode", false);
}

static bool g_pause_rendering_on_off = false;

static void init_g_pause_rendering_on_off(void){
  g_pause_rendering_on_off = SETTINGS_read_bool("pause_rendering", false);
}

void GL_set_pause_rendering_on_off(bool onoff){
  SETTINGS_write_bool("pause_rendering", onoff);
  g_pause_rendering_on_off = onoff;
}

bool GL_get_pause_rendering_on_off(void){
  return g_pause_rendering_on_off;
}

#if defined(FOR_MACOSX)
extern "C" void cocoa_set_best_resolution(void *view);
#endif

static bool is_opengl_certainly_too_old_questionmark(void){
  if (! ((QGLFormat::openGLVersionFlags()&QGLFormat::OpenGL_Version_2_0) == QGLFormat::OpenGL_Version_2_0))
    return true;
  else
    return false;
}

static bool is_opengl_version_recent_enough_questionmark(void){
  if ((QGLFormat::openGLVersionFlags()&QGLFormat::OpenGL_Version_3_0) == QGLFormat::OpenGL_Version_3_0)
    return true;
  else
    return false;
}

static void init_widget2(void){
#if THREADED_OPENGL
  R_ASSERT(widget->GL_has_initialized());
#endif
  
#if !USE_QT5
  widget->set_vblank(GL_get_estimated_vblank());
#endif

#if THREADED_OPENGL
  while(ATOMIC_GET(GE_vendor_string)==NULL || ATOMIC_GET(GE_renderer_string)==NULL || ATOMIC_GET(GE_version_string)==NULL)
    msleep(5);
#endif
  
  {
    QString s_vendor((const char*)ATOMIC_GET(GE_vendor_string));
    QString s_renderer((const char*)ATOMIC_GET(GE_renderer_string));
    QString s_version((const char*)ATOMIC_GET(GE_version_string));
    qDebug() << "___ GE_version_string: " << s_version;
    printf("vendor: %s, renderer: %s, version: %s \n",(const char*)ATOMIC_GET(GE_vendor_string),(const char*)ATOMIC_GET(GE_renderer_string),(const char*)ATOMIC_GET(GE_version_string));
    //getchar();

    
        

    bool show_mesa_warning = true;

#if 0  // Seems like old libxcb library was the cause of the crashes, not the intel driver
#ifdef FOR_LINUX
    if (!s_version.contains("nvidia", Qt::CaseInsensitive) && SETTINGS_read_bool("show_not_nvidia_warning", true)) {
      GL_set_pause_rendering_on_off(true);
        
      GFX_Message(NULL,
                  "It seems like you are not using an NVIDIA card or the nvidia gfx driver."
                  "<p>"
                  "In order to avoid seemingly random crashes, the option \"Briefly pause graphics rendering when opening windows\" has been enabled "
                  "under the \"Windows\" tab in the \"Preferences\" window."
                  "<p>"
                  "In case you are using a new GFX driver, you might want to uncheck this option."
                  "<p>"
                  "You will not see this warning again."
                  );
      
      SETTINGS_write_bool("show_not_nvidia_warning", false);
    }
#endif
#endif

#ifdef FOR_LINUX
    if (s_vendor.contains("ATI")) {
      if (SETTINGS_read_bool("show_catalyst_gfx_message_during_startup", true)) {
        vector_t v = {};
        VECTOR_push_back(&v,"Ok");
        int dont_show = VECTOR_push_back(&v,"Don't show this message again");
        
        int result = GFX_Message(&v,
                                 "AMD Catalyst OpenGL driver on Linux detected."
                                 "<p>"
                                 "For best performance, vsync should be turned off. You do this by going to the \"Edit\" menu and select \"Preferences\"."
                                 "<p>"
                                 "In addition, \"Tear Free Desktop\" should be turned on in the catalyst configuration program (run the \"amdcccle\" program)."
                                 "<p>"
                                 "If you notice choppy graphics, it might help to overclock the graphics card: <A href=\"http://www.overclock.net/t/517861/how-to-overclocking-ati-cards-in-linux\">Instructions for overclocking can be found here</A>. If Radium crashes when you show or hide windows, it might help to upgrade driver to the latest version, or (better) use the Gallium AMD OpenGL driver instead, or (even better) get an Nvidia card."
                                 );
        if (result==dont_show)
          SETTINGS_write_bool("show_catalyst_gfx_message_during_startup", false);

        GL_set_pause_rendering_on_off(true);

        //g_should_do_modal_windows = true;
        g_should_do_modal_windows = false;
        
      } else {

#if 0  // Seems like old libxcb library was the cause of the crashes, not the intel driver
        if (SETTINGS_read_bool("show_catalyst_gfx_message3_during_startup", true)) {

          GFX_Message(NULL,
                      "Catalyst Linux driver detected. Turning on the \"Briefly pause rendering when opening windows\" option."
                      );
          
          GL_set_pause_rendering_on_off(true);
          
          SETTINGS_write_bool("show_catalyst_gfx_message3_during_startup", true);
        }
#endif
        
      }
    }

#endif

#ifdef FOR_LINUX

    if (s_renderer.contains("Gallium") && s_renderer.contains("AMD")) {
      if (SETTINGS_read_bool("show_gallium_gfx_message_during_startup", true)) {
        vector_t v = {};
        VECTOR_push_back(&v,"Ok");
        VECTOR_push_back(&v,"Don't show this message again");
        
        int result = GFX_Message(&v,
                                 "Gallium AMD OpenGL driver detected."
                                 "<p>"
                                 "For best performance with this driver, vsync should be turned off. You do this by going to the \"Edit\" menu and select \"Preferences\"."
                                 "<p>"
                                 "If you notice choppy graphics, it might help to install the binary driver instead. However for newer versions of MESA, the performance of this driver seems just as good. It could be worth trying though, but the binary driver might be less stable.</A>."
                                 "<p>"
                                 "However, although the Gallium driver seems less unstable than the Catalyst driver, Radium still crashes sometimes using this driver (tested with the Gallium 0.4). The best option on Linux is to get an Nvidia card and use the closed-source binary driver. Newest Intel GFX drivers also seem stable."
                                 );
        if (result==1)
          SETTINGS_write_bool("show_gallium_gfx_message_during_startup", false);

        GL_set_pause_rendering_on_off(true);

      } else {

#if 0  // Seems like old libxcb library was the cause of the crashes, not the intel driver
        
        if (SETTINGS_read_bool("show_gallium_gfx_message3_during_startup", true)) {

          GFX_Message(NULL,
                      "Gallium Linux driver detected. Turning on the \"Briefly pause rendering when opening windows\" option."
                      );
          
          GL_set_pause_rendering_on_off(true);
          
          SETTINGS_write_bool("show_gallium_gfx_message3_during_startup", true);
        }

#endif
        
      }
      
      show_mesa_warning = false;
    }

#endif
    

#ifdef FOR_LINUX
    if (s_vendor.contains("nouveau", Qt::CaseInsensitive)) {
      GFX_Message(NULL,
                  "Warning!"
                  "<p>"
                  "Nouveau OpenGL driver detected."
                  "<p>"
                  "There have been many reports of the Nouveau driver crashing when running Radium.<br>"
                  "<p>"
                  "Please don't report any more crashes caused by the Nouveau driver.<br>"
                  "<p>"
                  "<b>The Nouvea driver is likely to freeze your computer!</b><br>"
                  "You should quit the program immediately."
                  "<p>"
                  "The Nouveau driver also performs worse than the Nvidia driver."
                  "<p>"
                  "The Nvidia driver can be installed to get faster and smoother graphics, and to avoid crashing and freezing.<br>"
                  "The Nvidia driver can be downloaded here: <a href=\"https://www.nvidia.com/object/unix.html\">https://www.nvidia.com/object/unix.html</a>"
                  );
      show_mesa_warning = false;
    }
#endif

#ifdef FOR_LINUX
    if (s_vendor.contains("Intel")) {      
      if (SETTINGS_read_bool("show_intel_gfx_message2_during_startup", true)) {
        vector_t v = {};
        VECTOR_push_back(&v,"Ok");
        VECTOR_push_back(&v,"Don't show this message again");

        int result = GFX_Message(&v,
                                 "<strong>Intel OpenGL driver detected.</strong>"
                                 "<p>"
                                 //"<ol>"
                                 "For best performance, the Intel Xorg driver should be configured like this:"
                                 "<p>"
                                 "<pre>"
                                 "Section \"Device\"\n"
                                 "\n"
                                 "   Identifier  \"Intel Graphics\"\n"
                                 "   Driver      \"intel\"\n"
                                 "\n"
                                 "   Option \"AccelMethod\" \"sna\"\n"
                                 "   Option \"TearFree\" \"true\"\n"
                                 "\n"
                                 "EndSection\n"
                                 "</pre>"
                                 "<p>"
                                 "Your driver is likely to already be configured like this. But in "
                                 "case you see tearing or the scrolling is not silky smooth, you might want to check "
                                 "your X configuration. You might also want to download the latest version "
                                 "of the driver, which can be found here: <a href=\"https://01.org/linuxgraphics/\">https://01.org/linuxgraphics/</a>"
#if 0
                                 "<p>"
                                 "<p>"
                                 "<li>In addition, vsync in Radium should be turned off. You do this by going to the \"Edit\" menu and select \"Preferences\"."
#endif
                                 "</ol>"
                                 );

        if (result==1)
          SETTINGS_write_bool("show_intel_gfx_message2_during_startup", false);

        GL_set_pause_rendering_on_off(true);
        
      } else if (SETTINGS_read_bool("show_intel_gfx_message3_during_startup", true)) {

#if 0  // Seems like old libxcb library was the cause of the crashes, not the intel driver

        GFX_Message(NULL,
                      "Intel Linux driver detected. Turning on the \"Briefly pause rendering when opening windows\" option."
                    );
        
        GL_set_pause_rendering_on_off(true);
        
        SETTINGS_write_bool("show_intel_gfx_message3_during_startup", true);

#endif
      }

#if 0  // Seems like old libxcb library was the cause of the crashes, not the intel driver

      //if (SETTINGS_read_bool("show_intel_gfx_message4_during_startup", true)) {
      if (s_version.contains("Mesa 10.")) {
          GFX_Message(NULL,
                      "Old Intel Linux driver detected. Upgrading the GFX driver might prevent Radium from crashing."
                      );
      }
      //SETTINGS_write_bool("show_intel_gfx_message4_during_startup", true);
      //}
      
      //g_should_do_modal_windows = true;
      g_should_do_modal_windows = false;
#endif

      show_mesa_warning = false;
    }

#endif // FOR_LINUX


    if (s_version.contains("mesa", Qt::CaseInsensitive) && !s_renderer.contains("AMD") && show_mesa_warning==true)
      GFX_Message(NULL,
                  "Warning!\n"
                  "MESA OpenGL driver detected.\n"
                  "\n"
                  "MESA OpenGL driver renders graphics in software, which is likely to be significantly slower than to render in hardware.\n"
                  "\n"
                  "In addition, the graphics tends to not look as good."
                  );
  }
  
#if THREADED_OPENGL
  while(ATOMIC_GET(g_has_updated_at_least_once)==false)
    msleep(5);

  msleep(200);  // Possible fix for font garbage in windows.
#endif
  
  g_gl_widget_started = true;
}

static void maybe_init_widget3(void){
  static int s_num_tries = 0;

  s_num_tries++;

  if (ATOMIC_GET(g_has_resized_at_least_once)==false) {
    if (s_num_tries > 100){ // 10s
      GFX_addMessage("Unable to initialize OpenGL.");
    } else {
      QTimer::singleShot(100, maybe_init_widget3);
    }
    return;
  }

  msleep(200); // Possible fix for font garbage in windows.
  
  if (maybe_start_t2_thread()==false){
    if (s_num_tries > 100){ // 10s
      GFX_addMessage("Unable to initialize OpenGL.");
    } else {
      QTimer::singleShot(100, maybe_init_widget3);
    }
  }
}

static void maybe_init_widget(void){
#if THREADED_OPENGL
  if (widget->GL_has_initialized()==false){
    QTimer::singleShot(100, maybe_init_widget);
    return;
  }
  widget->start();
  //GFX_ShowProgressMessage("Finished initializing OpenGL Context. Now setting up the OpenGL Widget", true);
#else
  //widget->initializeGL();
#endif
  
  init_widget2();

  // Make sure editor has correct size after startup.
  {
    QResizeEvent qresizeevent(widget->Q_QL_Widget::size(), widget->Q_QL_Widget::size());
    widget->resizeEvent(&qresizeevent);
  }

#if !THREADED_OPENGL
  ATOMIC_SET(g_has_resized_at_least_once, true);
#endif
    
  maybe_init_widget3();
}

static void setup_widget(QWidget *parent){
  vl::VisualizationLibrary::init();

  vl::OpenGLContextFormat vlFormat;
  vlFormat.setDoubleBuffer(true);
  //vlFormat.setDoubleBuffer(false);
  vlFormat.setRGBABits( 8,8,8,0 );
  vlFormat.setDepthBufferBits(24);
  vlFormat.setFullscreen(false);
  vlFormat.setMultisampleSamples(GL_get_multisample()); // multisampling 32 seems to make text more blurry. 16 sometimes makes program crawl in full screen (not 32 though).
  //vlFormat.setMultisampleSamples(8); // multisampling 32 seems to make text more blurry. 16 sometimes makes program crawl in full screen (not 32 though).
  //vlFormat.setMultisampleSamples(32); // multisampling 32 seems to make text more blurry. 16 sometimes makes program crawl in full screen (not 32 though).
  vlFormat.setMultisample(GL_get_multisample()>1);
  //vlFormat.setMultisample(false);
  vlFormat.setVSync(GL_get_vsync());
  //vlFormat.setVSync(false);

  widget = new MyQtThreadedWidget(vlFormat, parent);

  widget->resize(1000,1000);
  widget->show();
  
#if THREADED_OPENGL
  widget->setAutomaticDelete(false);  // dont want auto-desctruction at program exit.
#endif
  
  //GFX_ShowProgressMessage("Waiting for OpenGL Context to initialize", true);

  //QTimer::singleShot(10000, maybe_init_widget);
  maybe_init_widget();
}

static bool g_compatibility_ok = false;

bool GL_check_compatibility(void){

  // Load setting values from disk
  {
    doHighCpuOpenGlProtection();
    doLockJuceWhenSwappingOpenGL();
    g_safe_mode = GL_get_safe_mode();
    init_g_pause_rendering_on_off();
  }
  
    
  if (QGLFormat::hasOpenGL()==false) {
    GFX_Message(NULL,"OpenGL not found");
    //exit(-1);
    return false;
  }

#if !defined(RADIUM_USES_TSAN)
  if (CHECKOPENGL_checkit()==true)
    return false;
#endif

  if (GL_get_vsync()==false){
    if (SETTINGS_read_bool("show_vsync_warning_during_startup", true)) {
      vector_t v = {};
      VECTOR_push_back(&v,"Ok");
      int vsync_hide = VECTOR_push_back(&v,"Don't show this message again");
      
      int vsyncret = GFX_Message(&v,
                                 "Warning: VSync is disabled. You probably don't want to do that. You can enable vsync under"
                                 "<p>"
                                 "Edit -> Preferences -> OpenGL -> Vertical Blank -> VSync"
                                 "<p>"
                                 "Disabling vsync will normally cripple graphical performance.<br>"
                                 "You should only turn off vsync if your gfx driver is misbehaving."
                                 );
      if (vsyncret==vsync_hide)
        SETTINGS_write_bool("show_vsync_warning_during_startup", false);
    }
  }
 
  if (is_opengl_certainly_too_old_questionmark()){
    GFX_Message(NULL,
                "Your version of OpenGL is too old. Radium can not run.\n"
                "\n"
                "To solve this problem, you might want to try updating your graphics card driver."
                );
    //exit(-1);
    return false;
  }
 
  if (!is_opengl_version_recent_enough_questionmark()){
    vector_t v = {};
    VECTOR_push_back(&v,"Try to run anywyay"); // (but please don't send a bug report if Radium crashes)");
    int exit_=VECTOR_push_back(&v,"Exit");

    int ret = GFX_Message(&v,
                          "Your version of OpenGL is too old.\n"
                          "\n"
                          "To solve this problem, you might want to try updating your graphics card driver.\n"
                          "\n"
                          "You might also have to disable \"Draw in separate process\" under Edit -> Preferences -> OpenGL if you choose to run anyway."
                          );
    if (ret==exit_){
      //exit(-1);
      return false;
    }

    msleep(10);
    //QThread::currentThread()->wait(1000*10);
  }


  g_compatibility_ok = true;
  
  CRASHREPORTER_do_report();
    
  return true;
}

QWidget *GL_create_widget(QWidget *parent){

#if defined(FOR_MACOSX)
  // doesn't work.
  //cocoa_set_best_resolution(NULL);//(void*)widget->winId());
#endif

  R_ASSERT_RETURN_IF_FALSE2(g_compatibility_ok==true, NULL);

  GL_get_high_render_thread_priority();
  GL_get_high_draw_thread_priority();
  
  setup_widget(parent);



#if THREADED_OPENGL
  return widget.get();
#else
  return widget;
#endif
}


void GL_stop_widget(QWidget *widget){
#if THREADED_OPENGL
  MyQtThreadedWidget *mywidget = static_cast<MyQtThreadedWidget*>(widget);
#endif
  
  T1_stop_t2();

#if THREADED_OPENGL
  mywidget->stop();
#endif
  
  //delete mywidget;
}

#if !USE_QT5
static void show_message_box(QMessageBox *box){
  //box->setWindowFlags(Qt::WindowStaysOnTopHint);//Qt::WindowStaysOnTopHint|Qt::SplashScreen|Qt::Window | Qt::FramelessWindowHint|Qt::Popup);
  box->setWindowFlags(box->windowFlags() | DEFAULT_WINDOW_FLAGS); //Qt::Popup);
  
    
  box->setText("Please wait, estimating vblank refresh rate. This takes 3 - 10 seconds");
  box->setInformativeText("!!! Don't move the mouse or press any key !!!");

  if(have_earlier_estimated_value()){

    QString message = QString("Don't estimate again. Use last estimated value instead (")+QString::number(1000.0/GL_get_estimated_vblank())+" Hz).";
#if 1
    box->addButton(message,QMessageBox::ApplyRole);
#else
    QAbstractButton *msgBox_useStoredValue = (QAbstractButton*)box->addButton(message,QMessageBox::ApplyRole);
    printf((char*)msgBox_useStoredValue);
#endif

    //safeShow(box);
    box->show();

  } else {

    //safeShow(box);//->show();
    box->show();

    box->button(QMessageBox::Ok)->hide();

  }
  
  qApp->processEvents();
}
#endif

void GL_maybe_estimate_vblank(QWidget *qwidget){
#if !USE_QT5
  static bool been_here = false;
  
  MyQtThreadedWidget *widget = static_cast<MyQtThreadedWidget*>(qwidget);

  if (been_here==true)
    return;
  else
    been_here = true;
  
  if (use_estimated_vblank_questionmark() == true) {
    widget->set_vblank(GL_get_estimated_vblank());
    return;
  }
  

  ScopedQPointer<MyQMessageBox> box(MyQMessageBox::create(false));

  bool have_earlier = have_earlier_estimated_value();

  show_message_box(&box);

  ATOMIC_SET(widget->is_training_vblank_estimator, true);
      
  while(ATOMIC_GET(widget->is_training_vblank_estimator)==true) {
    if(box->clickedButton()!=NULL){
      widget->set_vblank(GL_get_estimated_vblank());
      store_use_estimated_vblank(true);
      break;
    }
    if (have_earlier)
      qApp->processEvents();
    else
      qApp->processEvents(QEventLoop::ExcludeUserInputEvents);
    
    qApp->flush();

    if (box==NULL)
      break;
    
    msleep(5);
  }
  
  if (box!=NULL && box->clickedButton()==NULL)
    store_estimated_value(time_estimator.get_vblank());
  

  printf("\n\n\n Closing box\n\n\n");
  if(box!=NULL)
    box->close();
#endif
}

