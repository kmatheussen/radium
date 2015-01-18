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
#include <QMessageBox>
#include <QApplication>
#include <QAbstractButton>
#include <QMainWindow>
#include <QGLFormat>

#include "../common/nsmtracker.h"
#include "../common/playerclass.h"
#include "../common/list_proc.h"
#include "../common/realline_calc_proc.h"
#include "../common/time_proc.h"
#include "../common/settings_proc.h"

#define GE_DRAW_VL
#include "GfxElements.h"
#include "Timing.hpp"
#include "Render_proc.h"

#include "Widget_proc.h"

#include <sys/types.h>
#include <unistd.h>
#include <errno.h>

#if FOR_LINUX
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
#endif


static QMutex mutex(QMutex::Recursive);

void GL_lock(void){
  mutex.lock();
}

void GL_unlock(void){
  mutex.unlock();
}


static volatile char *driver_version_string=NULL;
      
static volatile int g_curr_realline;

// TS (called from both main thread and opengl thread)
void GE_set_curr_realline(int curr_realline){
  g_curr_realline = curr_realline;
}

// OpenGL thread
static float GE_scroll_pos(SharedVariables *sv, double realline){
  double extra = sv->top_realline - sv->curr_realline;
  return
    (   (realline+extra) * sv->fontheight  );
}



extern PlayerClass *pc;
extern struct Root *root;

extern int scrolls_per_second;
extern int default_scrolls_per_second;

TimeEstimator time_estimator;


// OpenGL thread
static double get_realline_stime(SharedVariables *sv, int realline){  
  if(realline==sv->num_reallines)
    return sv->block_duration;
  else
    return Place2STime_from_times(sv->times, &sv->realline_places[realline]);
}


// OpenGL thread
static double find_current_realline_while_playing(SharedVariables *sv){

  double time_in_ms = (double)(pc->start_time - pc->seqtime) * 1000.0 / (double)pc->pfreq;
  double stime      = time_estimator.get(time_in_ms, sv->reltempo) * (double)pc->pfreq / 1000.0;

  double prev_line_stime = 0.0;

  static int i_realline = 0; // Note that this one is static. The reason is that we are usually placed on the same realline as last time, so we remember last position and start searching from there/

  // Since i_realline is static, we need to first ensure that the current value has a valid valid value we can start searching from.
  {
    if(i_realline>sv->num_reallines) // First check that i_realline is within the range of the block.
      i_realline = 0;
    
    else {
      if(i_realline>0) // Common situation. We are usually on the same line as the last visit, but we need to go one step back to reload prev_line_stime. (we cant store prev_line_stime, because it could have been calculated from a different block)
        i_realline--;

      if(stime < get_realline_stime(sv, i_realline)) // Behind the time of the last visit. Start searching from 0 again.
        i_realline = 0;
    }
  }

  for(; i_realline<=sv->num_reallines; i_realline++){
    double curr_line_stime = get_realline_stime(sv, i_realline);
    if (stime <= curr_line_stime)
      return scale_double(stime,
                          prev_line_stime, curr_line_stime,
                          i_realline-1, i_realline
                          );
    prev_line_stime = curr_line_stime;
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
    ScrollEditorToRealLine(window,wblock,till_curr_realline);
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


volatile float scroll_pos = 0.0f;


// Main thread
static QMouseEvent translate_qmouseevent(const QMouseEvent *qmouseevent){
  const QPoint p = qmouseevent->pos();

  return QMouseEvent(qmouseevent->type(),
                     QPoint(p.x(), p.y() + root->song->tracker_windows->wblock->t.y1),
                     qmouseevent->globalPos(),
                     qmouseevent->button(),
                     qmouseevent->buttons()
                     );
}

void freeit(void);


class MyQt4ThreadedWidget : public vlQt4::Qt4ThreadedWidget, public vl::UIEventListener {

public:

  int current_width;
  int current_height;
  
  int new_width;
  int new_height;
    
  vl::ref<vl::Rendering> _rendering;
  vl::ref<vl::Transform> _scroll_transform;
  vl::ref<vl::Transform> _linenumbers_transform;
  vl::ref<vl::Transform> _scrollbar_transform;
  vl::ref<vl::SceneManagerActorTree> _sceneManager;

  vl::ref<vl::VectorGraphics> vg;
  vl::ref<vl::SceneManagerVectorGraphics> vgscene;

  PaintingData *painting_data;

  volatile bool is_training_vblank_estimator;
  volatile double override_vblank_value;
  bool has_overridden_vblank_value;

  float last_pos;

  bool sleep_when_not_painting;

  // Main thread
  MyQt4ThreadedWidget(vl::OpenGLContextFormat vlFormat, QWidget *parent=0)
    : Qt4ThreadedWidget(vlFormat, parent)
    , current_width(-1)
    , current_height(-1)
    , new_width(500)
    , new_height(500)
    , painting_data(NULL)
    , is_training_vblank_estimator(true)
    , override_vblank_value(-1.0)
    , has_overridden_vblank_value(false)
    , last_pos(-1.0f)
    , sleep_when_not_painting(true) //SETTINGS_read_bool("opengl_sleep_when_not_painting", false))
  {
    setMouseTracking(true);
    //setAttribute(Qt::WA_PaintOnScreen);
  }

  // Main thread
  ~MyQt4ThreadedWidget(){
    fprintf(stderr,"Exit myqt4threaderwidget\n");
  }

  // OpenGL thread
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

    //printf("FLAGS: %d\n",QGLFormat::openGLVersionFlags());
    //gets(NULL);
  
#if FOR_LINUX
    if(0)set_realtime(SCHED_FIFO,1);
#endif
  }

  /** Event generated when the bound OpenGLContext bocomes initialized or when the event listener is bound to an initialized OpenGLContext. */
  // OpenGL thread
  virtual void initEvent() {
    printf("initEvent\n");
    
    _rendering->sceneManagers()->clear();
    
    vg = new vl::VectorGraphics;
    
    vgscene = new vl::SceneManagerVectorGraphics;
    vgscene->vectorGraphicObjects()->push_back(vg.get());
    _rendering->sceneManagers()->push_back(vgscene.get());
  }

private:
  // OpenGL thread
  bool canDraw(){
    if (vg.get()==NULL)
      return false;
    else
      return true;
  }
public:

  // Main thread
  virtual void mouseReleaseEvent( QMouseEvent *qmouseevent){
    QMouseEvent event = translate_qmouseevent(qmouseevent);
    get_editorwidget()->mouseReleaseEvent(&event);
    //GL_create(get_window(), get_window()->wblock);
  }

  // Main thread
  virtual void mousePressEvent( QMouseEvent *qmouseevent){
    QMouseEvent event = translate_qmouseevent(qmouseevent);
    get_editorwidget()->mousePressEvent(&event);
    //GL_create(get_window(), get_window()->wblock);
  }

  // Main thread
  virtual void mouseMoveEvent( QMouseEvent *qmouseevent){
    QMouseEvent event = translate_qmouseevent(qmouseevent);
    get_editorwidget()->mouseMoveEvent(&event);
    //GL_create(get_window(), get_window()->wblock);
  }

  virtual void wheelEvent(QWheelEvent *qwheelevent){
    //QMouseEvent event = translate_qmouseevent(qmouseevent);
    get_editorwidget()->wheelEvent(qwheelevent);
    //GL_create(get_window(), get_window()->wblock);
  }
    
      
  /** Event generated right before the bound OpenGLContext is destroyed. */
  virtual void destroyEvent() {
    fprintf(stderr,"destroyEvent\n");
  }

  virtual void paintEvent( QPaintEvent *e ){
    fprintf(stderr,"GLWindow paintEvent\n");
    GL_create(get_window(), get_window()->wblock);
  }

private:

  // OpenGL thread
  double find_till_realline(SharedVariables *sv){
    if(pc->isplaying && pc->playertask_has_been_called) // When pc->playertask_has_been_called is true, we can be sure that the timing values are valid.
      return find_current_realline_while_playing(sv);
    else
      return g_curr_realline;
  }

  // OpenGL thread
  bool draw(){
    bool needs_repaint;

    painting_data = GE_get_painting_data(painting_data, &needs_repaint);
    //printf("needs_repaint: %d, painting_data: %p\n",(int)needs_repaint,painting_data);

    if (painting_data==NULL){
      return false;
    }

    SharedVariables *sv = GE_get_shared_variables(painting_data);

    if (needs_repaint) {

      if (current_height != new_height || current_width != new_width){
        
        //_rendering->sceneManagers()->clear();
        _rendering->camera()->viewport()->setWidth(new_width);
        _rendering->camera()->viewport()->setHeight(new_height);
        _rendering->camera()->setProjectionOrtho(-0.5f);
   
        GE_set_height(new_height);
        //create_block(_rendering->camera()->viewport()->width(), _rendering->camera()->viewport()->height());
        
        //initEvent();
        
        current_height = new_height;
        current_width = new_width;

      } else
        vg->clear();

      GE_set_curr_realline(sv->curr_realline);

      GE_draw_vl(painting_data, _rendering->camera()->viewport(), vg, _scroll_transform, _linenumbers_transform, _scrollbar_transform);
    }
    
    if(pc->isplaying && sv->block!=pc->block) // sanity check
      return false;

    double till_realline = find_till_realline(sv);
    float pos = GE_scroll_pos(sv, till_realline);

    if(pc->isplaying && sv->block!=pc->block) // Do the sanity check once more. pc->block might have changed value during computation of pos.
      return false;

    if (needs_repaint==false && pos==last_pos)
      return false;

    //printf("scrolling\n");

    scroll_pos = pos;
    
    // scroll
    {
      vl::mat4 mat = vl::mat4::getRotation(0.0f, 0, 0, 1);
      mat.translate(0,pos,0);
      _scroll_transform->setLocalAndWorldMatrix(mat);
    }
    
    // linenumbers
    {
      vl::mat4 mat = vl::mat4::getRotation(0.0f, 0, 0, 1);
      mat.translate(0,pos,0);
      _linenumbers_transform->setLocalAndWorldMatrix(mat);
    }
    
    // scrollbar
    {
      vl::mat4 mat = vl::mat4::getRotation(0.0f, 0, 0, 1);
      float scrollpos = scale(till_realline,
                              0, sv->num_reallines - (pc->isplaying?0:1),
                              -2, -(sv->scrollbar_height - sv->scrollbar_scroller_height - 1)
                              );
      //printf("bar_length: %f, till_realline: %f. scrollpos: %f, pos: %f, max: %d\n",bar_length,till_realline, scrollpos, pos, window->leftslider.x2);
      mat.translate(0,scrollpos,0);
      _scrollbar_transform->setLocalAndWorldMatrix(mat);
    }
    
    GE_update_triangle_gradient_shaders(painting_data, pos);
    
    _rendering->render();
    
    last_pos = pos;
    
    return true;
  }

  // OpenGL thread
  void swap(){
    GL_lock();
    {
      // show rendering
      if ( openglContext()->hasDoubleBuffer() )
        openglContext()->swapBuffers();
    }
    GL_unlock();
  }

public:

  /** Event generated when the bound OpenGLContext does not have any other message to process 
      and OpenGLContext::continuousUpdate() is set to \p true or somebody calls OpenGLContext::update(). */
  // OpenGL thread
  virtual void updateEvent() {
    //printf("updateEvent\n");

    if (driver_version_string==NULL)
      driver_version_string = strdup((const char*)glGetString(GL_VERSION));
    //printf("%s\n",(const char*)driver_version_string);
    
    if (has_overridden_vblank_value==false && override_vblank_value > 0.0) {

      time_estimator.set_vblank(override_vblank_value);
      is_training_vblank_estimator = false;
      has_overridden_vblank_value = true;

    } else {

      if(is_training_vblank_estimator)
        is_training_vblank_estimator = time_estimator.train();

    }


    //printf("        vblank: %f\n",(float)time_estimator.get_vblank());


    // This is the only place the opengl thread waits. When swap()/usleep() returns, updateEvent is called again immediately.
  
    if (is_training_vblank_estimator==true)
      swap();

    else if (!canDraw())
      swap(); // initializing.

    else if (draw()==true)
      swap();

    else if (!sleep_when_not_painting) // probably doesn't make any sense setting sleep_when_not_painting to false. Besides, setting it to false may cause 100% CPU usage (intel gfx) or very long calls to GL_lock() (nvidia gfx).
      swap();
    
    else
      usleep(1000 * time_estimator.get_vblank());
  }

  // Main thread
  void set_vblank(double value){
    override_vblank_value = value;
  }

  /** Event generated when the bound OpenGLContext is resized. */
  // OpenGL thread
  virtual void resizeEvent(int w, int h) {
    printf("resisizing %d %d\n",w,h);

    new_width = w;
    new_height = h;


#if 0
    _rendering->sceneManagers()->clear();

    _rendering->camera()->viewport()->setWidth(w);
    _rendering->camera()->viewport()->setHeight(h);
    _rendering->camera()->setProjectionOrtho(-0.5f);

    GE_set_height(h);
    //create_block(_rendering->camera()->viewport()->width(), _rendering->camera()->viewport()->height());

    initEvent();
#endif
    //updateEvent();
  }
  // The rest of the methods in this class are virtual methods required by the vl::UIEventListener class. Not used.

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
    //_rendering->sceneManagers()->clear();
    //create_block();
    //initEvent(); 
  }
  
  /** Event generated when the mouse wheel rotated. */
  virtual void mouseWheelEvent(int n) {
    printf("mouseWheel %d\n",n);
  }
  
  /** Event generated when a key is pressed. */
  virtual void keyPressEvent(unsigned short unicode_ch, vl::EKey key) {
    printf("key pressed\n");

    //_rendering->sceneManagers()->clear();
    //create_block();
    //initEvent();
  }
  
  /** Event generated when a key is released. */
  virtual void keyReleaseEvent(unsigned short unicode_ch, vl::EKey key) {
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

static bool do_estimate_questionmark(){
  return SETTINGS_read_bool("use_estimated_vblank", false);
}

static void store_use_estimated_vblank(bool value){
  return SETTINGS_write_bool("use_estimated_vblank", value);
}

static bool have_earlier_estimated_value(){
  return SETTINGS_read_double("vblank", -1.0) > 0.0;
}

static double get_earlier_estimated(){
  return 1000.0 / SETTINGS_read_double("vblank", 60.0);
}

static void store_estimated_value(double period){
  printf("***************** Storing %f\n",1000.0 / period);
  SETTINGS_write_double("vblank", 1000.0 / period);
}

void GL_erase_estimated_vblank(void){
  SETTINGS_write_double("vblank", -1.0);
  store_use_estimated_vblank(false);
  GFX_Message(NULL, "Stored vblank value erased. Restart Radium to estimate a new vblank value.");
}


static void show_message_box(QMessageBox *box){
  box->setText("Please wait, estimating vblank refresh rate. This takes 3 - 10 seconds");
  box->setInformativeText("!!! Don't move the mouse or press any key !!!");

  if(have_earlier_estimated_value()){

    QString message = QString("Don't estimate again. Use last estimated value instead (")+QString::number(1000.0/get_earlier_estimated())+" Hz).";
    QAbstractButton *msgBox_useStoredValue = (QAbstractButton*)box->addButton(message,QMessageBox::ApplyRole);
    if(0)printf((char*)msgBox_useStoredValue);
    box->show();

  } else {

    box->show();
    box->button(QMessageBox::Ok)->hide();

  }

  qApp->processEvents();
}

static void setup_widget(QWidget *parent){
  vl::VisualizationLibrary::init();

  vl::OpenGLContextFormat vlFormat;
  vlFormat.setDoubleBuffer(true);
  vlFormat.setRGBABits( 8,8,8,8 );
  vlFormat.setDepthBufferBits(24);
  vlFormat.setFullscreen(false);
  vlFormat.setMultisampleSamples(4); // multisampling 32 seems to make text more blurry. 16 sometimes makes program crawl in full screen (not 32 though).
  //vlFormat.setMultisampleSamples(8); // multisampling 32 seems to make text more blurry. 16 sometimes makes program crawl in full screen (not 32 though).
  //vlFormat.setMultisampleSamples(32); // multisampling 32 seems to make text more blurry. 16 sometimes makes program crawl in full screen (not 32 though).
  vlFormat.setMultisample(true);
  //vlFormat.setMultisample(false);
  vlFormat.setVSync(true);
  
  widget = new MyQt4ThreadedWidget(vlFormat, parent);
  widget->resize(1000,1000);
  widget->show();

  widget->setAutomaticDelete(false);  // dont want auto-desctruction at program exit.
}

QWidget *GL_create_widget(QWidget *parent){

  if (QGLFormat::hasOpenGL()==false) {
    GFX_Message(NULL,"OpenGL not found");
    return NULL;
  }

  if (do_estimate_questionmark() == true) {

    setup_widget(parent);
    widget->set_vblank(get_earlier_estimated());

  } else {

    QMessageBox box;

    bool have_earlier = have_earlier_estimated_value();

    setup_widget(parent);
    show_message_box(&box);
    
    while(widget->is_training_vblank_estimator==true) {
      if(box.clickedButton()!=NULL){
        widget->set_vblank(get_earlier_estimated());
        store_use_estimated_vblank(true);
        break;
      }
      if (have_earlier)
        qApp->processEvents();
      else
        qApp->processEvents(QEventLoop::ExcludeUserInputEvents);
      
      qApp->flush();
      
      usleep(5*1000);
    }

    if (box.clickedButton()==NULL)
      store_estimated_value(time_estimator.get_vblank());

    box.close();
  }

  while(driver_version_string==NULL)
    usleep(5*1000);
  
  {
    QString s((const char*)driver_version_string);
    if (s.contains("mesa", Qt::CaseInsensitive))
      GFX_Message(NULL,
                  "Warning!\n"
                  "MESA OpenGL driver detected.\n"
                  "\n"
                  "MESA OpenGL driver renders graphics in software, which is likely to be significantly slower than to render in hardware.\n"
                  "\n"
                  "In addition, the graphics tends to not look good. " // lacking anti-aliasing
                  );
  }
  
  return widget.get();
}


void GL_stop_widget(QWidget *widget){
  MyQt4ThreadedWidget *mywidget = static_cast<MyQt4ThreadedWidget*>(widget);

  mywidget->stop();
  //delete mywidget;
}
