#include <stdio.h>

#include <vlVG/VectorGraphics.hpp>
#include <vlGraphics/Rendering.hpp>
#include <vlQt4/Qt4ThreadedWidget.hpp>
#include <vlGraphics/SceneManagerActorTree.hpp>
#include <vlVG/SceneManagerVectorGraphics.hpp>

#include <QWidget>
#include <QGLWidget>
#include <QMutex>
#include <QTextEdit>

#include "../common/nsmtracker.h"

#define GE_DRAW_VL
#include "GfxElements.h"


#include "Widget_proc.h"



#include <sys/types.h>
#include <unistd.h>
#include <errno.h>

static int set_pthread_priority(pthread_t pthread,int policy,int priority,const char *message,const char *name){
  struct sched_param par={0};
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


static float das_pos = 1000.0f; //_rendering->camera()->viewport()->height();

static QMutex mutex;

void GL_lock(void){
  mutex.lock();
}

void GL_unlock(void){
  mutex.unlock();
}

class MyQt4ThreadedWidget : public vlQt4::Qt4ThreadedWidget, public vl::UIEventListener {

public:
  
  vl::ref<vl::Rendering> _rendering;
  vl::ref<vl::Transform> _scroll_transform;
  vl::ref<vl::Transform> _linenumbers_transform;
  vl::ref<vl::Transform> _scrollbar_transform;
  vl::ref<vl::SceneManagerActorTree> _sceneManager;

  vl::ref<vl::VectorGraphics> vg;
  vl::ref<vl::SceneManagerVectorGraphics> vgscene;

  MyQt4ThreadedWidget(vl::OpenGLContextFormat vlFormat, QWidget *parent=0)
    : Qt4ThreadedWidget(vlFormat, parent)
  {
    setMouseTracking(true);
  }

  ~MyQt4ThreadedWidget(){
    fprintf(stderr,"Exit myqt4threaderwidget\n");
  }

  virtual void init_vl(vl::OpenGLContext *glContext) {

    printf("init_vl\n");

    _rendering = new vl::Rendering;
    _rendering->renderer()->setFramebuffer(glContext->framebuffer() );
    //_rendering->camera()->viewport()->setClearColor( vl::green );

    _scroll_transform = new vl::Transform;
    _rendering->transform()->addChild(_scroll_transform.get());

    _linenumbers_transform = new vl::Transform;
    _rendering->transform()->addChild(_linenumbers_transform.get());

    _scrollbar_transform = new vl::Transform;
    _rendering->transform()->addChild(_scrollbar_transform.get());


    // installs a SceneManagerActorTree as the default scene manager
    
    _sceneManager = new vl::SceneManagerActorTree;
    _rendering->sceneManagers()->push_back(_sceneManager.get());

#if 0
    vl::mat4 mat = vl::mat4::getTranslation(0,0,0);
    mat = vl::mat4::getTranslation(0,0,0) * mat;
    _rendering->camera()->setViewMatrix( mat );
#endif

    glContext->initGLContext();
    glContext->addEventListener(this);

    if(0)set_realtime(SCHED_FIFO,1);
  }

    /** Event generated when the bound OpenGLContext bocomes initialized or when the event listener is bound to an initialized OpenGLContext. */
  virtual void initEvent() {
    printf("initEvent\n");
    
    _rendering->sceneManagers()->clear();
    
    vg = new vl::VectorGraphics;
    
    vgscene = new vl::SceneManagerVectorGraphics;
    vgscene->vectorGraphicObjects()->push_back(vg.get());
    _rendering->sceneManagers()->push_back(vgscene.get());
  }

private:
  void maybeRedraw(){
    if (vg.get()==NULL)
      initEvent();

    if(GE_new_read_contexts()==true) {
      vg->clear();
      GE_draw_vl(_rendering->camera()->viewport(), vg, _scroll_transform, _linenumbers_transform, _scrollbar_transform);
    }
  }
public:

  virtual void mousePressEvent( QMouseEvent *qmouseevent){
    printf("Pressing das button\n");
  }

  virtual void mouseMoveEvent( QMouseEvent *qmouseevent){
    //tevent.ID=TR_MOUSEMOVE;
    //tevent.x=qmouseevent->x();//-XOFFSET;
    //tevent.y=qmouseevent->y();//-YOFFSET;
    //EventReciever(&tevent,this->window);
    fprintf(stderr, "mouse %d / %d\n", (int)qmouseevent->x(), (int)qmouseevent->y());

  }

  /** Event generated right before the bound OpenGLContext is destroyed. */
  virtual void destroyEvent() {
    fprintf(stderr,"destroyEvent\n");
  }

  /** Event generated when the bound OpenGLContext does not have any other message to process 
      and OpenGLContext::continuousUpdate() is set to \p true or somebody calls OpenGLContext::update(). */
  virtual void updateEvent() {
    //printf("updateEvent\n");

    GL_lock();

    static float pos = -123412;

    maybeRedraw();

    if(true || pos != das_pos) {

      // scroll
      {
        vl::mat4 mat = vl::mat4::getRotation(0.0f, 0, 0, 1);
        //mat.translate(200*cos(vl::Time::currentTime()*vl::fPi*2.0f/17.0f),0,0);
        //mat.translate(-(das_pos-1000)/16.0f,das_pos,0);
        mat.translate(0,das_pos,0);
        //mat.scale(das_pos/16.0f,0,0);
        _scroll_transform->setLocalAndWorldMatrix(mat);
      }

      int extra = root->song->tracker_windows->wblock->curr_realline - root->song->tracker_windows->wblock->top_realline;
      //printf("%d %d (%d)\n",root->song->tracker_windows->wblock->curr_realline, root->song->tracker_windows->wblock->top_realline, extra);
      das_pos = (extra + root->song->tracker_windows->wblock->curr_realline) * root->song->tracker_windows->fontheight;

      // linenumbers
      {
        vl::mat4 mat = vl::mat4::getRotation(0.0f, 0, 0, 1);
        //mat.translate(200*cos(vl::Time::currentTime()*vl::fPi*2.0f/17.0f),0,0);
        mat.translate(0,das_pos,0);
        _linenumbers_transform->setLocalAndWorldMatrix(mat);
      }
      

#if 0
      das_pos += 2.03;
      if(das_pos>64*20)
        das_pos=_rendering->camera()->viewport()->height();
#endif

      //_vg->scale(pos / 10.0,pos / 10.0);
      
      _rendering->render();

      pos = das_pos;

      // scrollbar
      {
        vl::mat4 mat = vl::mat4::getRotation(0.0f, 0, 0, 1);
        //mat.translate(200*cos(vl::Time::currentTime()*vl::fPi*2.0f/17.0f),0,0);
        mat.translate(0,
                      scale(das_pos,
                            0,512*20 + 1200,
                            _rendering->camera()->viewport()->height(),100
                            ),
                      0);

        _scrollbar_transform->setLocalAndWorldMatrix(mat);
      }

    }


    // show rendering
    if ( openglContext()->hasDoubleBuffer() )
      openglContext()->swapBuffers();

    GL_unlock();
  }
    
  /** Event generated whenever setEnabled() is called. */
  virtual void enableEvent(bool enabled){
    printf("enableEvent %d\n",(int)enabled);
  }

  /** Event generated whenever a listener is bound to an OpenGLContext context. */
  virtual void addedListenerEvent(vl::OpenGLContext*) {
    printf("addedListenerEvent\n");
  }

  /** Event generated whenever a listener is unbound from an OpenGLContext context. */
  virtual void removedListenerEvent(vl::OpenGLContext*) {
    printf("removedListenerEvent\n");
  }
  
  /** Event generated when the mouse moves. */
  virtual void mouseMoveEvent(int x, int y) {
    printf("mouseMove %d %d\n",x,y);
    das_pos -= 5;//2.03;
    //_rendering->sceneManagers()->clear();
    //create_block();
    //initEvent();
  }
  
  /** Event generated when one of the mouse buttons is released. */
  virtual void mouseUpEvent(vl::EMouseButton button, int x, int y) {
    printf("mouseMove %d %d\n",x,y);
    //_rendering->sceneManagers()->clear();
    //create_block(_rendering->camera()->viewport()->width(), _rendering->camera()->viewport()->height());
    //initEvent();
  }
  
  /** Event generated when one of the mouse buttons is pressed. */
  virtual void mouseDownEvent(vl::EMouseButton button, int x, int y) {
    printf("mouseMove %d %d\n",x,y);
    das_pos -= 5;//2.03;
    //_rendering->sceneManagers()->clear();
    //create_block();
    //initEvent(); 
  }
  
  /** Event generated when the mouse wheel rotated. */
  virtual void mouseWheelEvent(int n) {
  }
  
  /** Event generated when a key is pressed. */
  virtual void keyPressEvent(unsigned short unicode_ch, vl::EKey key) {
    printf("key pressed\n");

    das_pos += 20;//2.03;

    //_rendering->sceneManagers()->clear();
    //create_block();
    //initEvent();
  }
  
  /** Event generated when a key is released. */
  virtual void keyReleaseEvent(unsigned short unicode_ch, vl::EKey key) {
  }
  
  /** Event generated when the bound OpenGLContext is resized. */
  virtual void resizeEvent(int w, int h) {
    _rendering->sceneManagers()->clear();

    _rendering->camera()->viewport()->setWidth(w);
    _rendering->camera()->viewport()->setHeight(h);
    _rendering->camera()->setProjectionOrtho(-0.5f);

    GE_set_height(h);
    //create_block(_rendering->camera()->viewport()->width(), _rendering->camera()->viewport()->height());

    initEvent();

    updateEvent();
  }
  
  /** Event generated when one or more files are dropped on the bound OpenGLContext's area. */
  virtual void fileDroppedEvent(const std::vector<vl::String>& files) {
  }
  
  /** Event generated when the bound OpenGLContext is shown or hidden. */
  virtual void visibilityEvent(bool visible) {
    printf("visibilityEvent %d\n",(int)visible);
  }
};


static vl::ref<MyQt4ThreadedWidget> widget;

QWidget *GL_create_widget(void){
  vl::VisualizationLibrary::init();

  vl::OpenGLContextFormat vlFormat;
  vlFormat.setDoubleBuffer(true);
  vlFormat.setRGBABits( 8,8,8,8 );
  vlFormat.setDepthBufferBits(24);
  vlFormat.setFullscreen(false);
  vlFormat.setMultisampleSamples(4); // multisampling 32 seems to make text more blurry. 16 sometimes makes program crawl in full screen (not 32 though).
  //vlFormat.setMultisampleSamples(8); // multisampling 32 seems to make text more blurry. 16 sometimes makes program crawl in full screen (not 32 though).
  vlFormat.setMultisample(true);
  //vlFormat.setMultisample(false);
  vlFormat.setVSync(true);
  
  widget = new MyQt4ThreadedWidget(vlFormat, NULL);
  widget->resize(1000,1000);
  widget->show();

  widget->incReference();  // dont want auto-desctruction at program exit.

  return widget.get();
}


void GL_stop_widget(QWidget *widget){
  MyQt4ThreadedWidget *mywidget = static_cast<MyQt4ThreadedWidget*>(widget);

  mywidget->stop();
  //delete mywidget;
}
