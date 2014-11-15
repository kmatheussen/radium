#include <sys/types.h>
#include <unistd.h>
#include <errno.h>

#include <vlCore/DiskDirectory.hpp>
#include <vlQt4/Qt4ThreadedWidget.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>
#include <vlCore/Time.hpp>
#include <vlVG/VectorGraphics.hpp>
#include <vlGraphics/Rendering.hpp>
#include <vlGraphics/ImagePBO.hpp>
#include <vlGraphics/DrawPixels.hpp>
#include <vlGraphics/OcclusionCullRenderer.hpp>
#include <vlGraphics/SceneManagerActorTree.hpp>
#include <vlVG/SceneManagerVectorGraphics.hpp>

#include <QWidget>
#include <QGLWidget>
#include <QApplication>
#include <QPainter>
#include <QTime>
#include <QMouseEvent>

// http://qt-project.org/wiki/Using_3D_engines_with_Qt

#include "../common/nsmtracker.h"
#include "../common/memory_proc.h"
#include "../common/control_proc.h"
#include "../common/notestext.c"


void EndProgram(void){
  printf("ENDPROGRAM called\n");
}

void RError(const char *fmt,...){
  char message[1000];
  va_list argp;
  
  va_start(argp,fmt);
  /*	vfprintf(stderr,fmt,argp); */
  vsprintf(message,fmt,argp);
  va_end(argp);

  fprintf(stderr,"error: %s\n",message);
}

#define GE_DRAW_VL
#include "GfxElements.h"



/*
static float gl_y(float y){
  return y;
}
*/

//#include "tests.hpp"

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


static void create_block(int width, int height);

static float das_pos = 1000.0f; //_rendering->camera()->viewport()->height();


struct MainWidget : public QWidget {
  virtual void paintEvent ( QPaintEvent * event ){
    QPainter p(this);
    p.fillRect(0,0,500,100,QColor(50,255,10));
  }
};

struct PaintingData{
  int hepp;
};

class MyQt4ThreadedWidget : public vlQt4::Qt4ThreadedWidget, public vl::UIEventListener {

public:
  
  vl::ref<vl::Rendering> _rendering;
  vl::ref<vl::Transform> _scroll_transform;
  vl::ref<vl::Transform> _linenumbers_transform;
  vl::ref<vl::Transform> _scrollbar_transform;
  vl::ref<vl::SceneManagerActorTree> _sceneManager;

  MyQt4ThreadedWidget(vl::OpenGLContextFormat vlFormat, QWidget *parent=0)
    : Qt4ThreadedWidget(vlFormat, parent)
  {
  }

  ~MyQt4ThreadedWidget(){
    fprintf(stderr,"Exit myqt4threaderwidget\n");
  }

  virtual void init_vl(vl::OpenGLContext *glContext) {

    printf("init_vl\n");

    _rendering = new vl::Rendering;
    _rendering->renderer()->setFramebuffer(glContext->framebuffer() );
    _rendering->camera()->viewport()->setClearColor( vl::white );

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

    set_realtime(SCHED_FIFO,1);
  }

    /** Event generated when the bound OpenGLContext bocomes initialized or when the event listener is bound to an initialized OpenGLContext. */
  virtual void initEvent() {
    printf("initEvent\n");
    vl::ref<vl::VectorGraphics> vg = new vl::VectorGraphics;

    vl::ref<vl::SceneManagerVectorGraphics> vgscene = new vl::SceneManagerVectorGraphics;
    vgscene->vectorGraphicObjects()->push_back(vg.get());
    _rendering->sceneManagers()->push_back(vgscene.get());

    //GE_draw_vl(NULL, _scroll_transform, _linenumbers_transform, _scrollbar_transform); // fix
  }

  /** Event generated right before the bound OpenGLContext is destroyed. */
  virtual void destroyEvent() {
    fprintf(stderr,"destroyEvent\n");
  }

  /** Event generated when the bound OpenGLContext does not have any other message to process 
      and OpenGLContext::continuousUpdate() is set to \p true or somebody calls OpenGLContext::update(). */
  virtual void updateEvent() {
    //printf("updateEvent\n");

    static float pos = -123412;

    if(true || pos != das_pos) {

      // scroll
      {
        vl::mat4 mat = vl::mat4::getRotation(0.0f, 0, 0, 1);
        //mat.translate(200*cos(vl::Time::currentTime()*vl::fPi*2.0f/17.0f),0,0);
        mat.translate(-(das_pos-1000)/16.0f,das_pos,0);
        //mat.scale(das_pos/16.0f,0,0);
        _scroll_transform->setLocalAndWorldMatrix(mat);
      }
      
      // linenumbers
      {
        vl::mat4 mat = vl::mat4::getRotation(0.0f, 0, 0, 1);
        //mat.translate(200*cos(vl::Time::currentTime()*vl::fPi*2.0f/17.0f),0,0);
        mat.translate(0,das_pos,0);
        _linenumbers_transform->setLocalAndWorldMatrix(mat);
      }
      

#if 1
      das_pos += 2.03;
      if(das_pos>512*20 + 1200)
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
    _rendering->sceneManagers()->clear();
    create_block(_rendering->camera()->viewport()->width(), _rendering->camera()->viewport()->height());
    initEvent();
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
    create_block(_rendering->camera()->viewport()->width(), _rendering->camera()->viewport()->height());

    initEvent();
    _rendering->render();
  }
  
  /** Event generated when one or more files are dropped on the bound OpenGLContext's area. */
  virtual void fileDroppedEvent(const std::vector<vl::String>& files) {
  }
  
  /** Event generated when the bound OpenGLContext is shown or hidden. */
  virtual void visibilityEvent(bool visible) {
    printf("visibilityEvent %d\n",(int)visible);
  }
};



static float rnd(float min, float max){
  return scale(rand(), 0, RAND_MAX, min, max);
}


static void create_block(int width, int height){
  //GE_clear();

  int num_tracks = 12;
  int num_lines = 512;
  float max_note_length = 4; // anywhere between 0.1 and 100 lines
  float num_notes_to_next_line = 2; // anywhere between 0 and 5 lines.

  float track_width = 1800 / num_tracks;//150;
  float notetrack_width = 3*12+5;
  float line_height = 20;

  GE_Context *linenumberscolor = GE_rgba_color_z(255,255,255,150, Z_LINENUMBERS);
  GE_Context *line_border_color = GE_rgba_color_z(150,150,150,50, Z_BACKGROUND + 1);

  GE_Context *waveform = GE_rgba_color_z(145,105,135,250,Z_ABOVE(50));
  GE_Context *black = GE_rgb_color(0,0,0);

  //GE_Context note_color = GE_rgb_color(20,200,100);
  //GE_Context *velocity_color = GE_rgb_color(50,200,200);
  GE_Context *velocity_border_color = GE_rgb_color(50,50,150);
  GE_Context *track_border_color = GE_rgb_color(20,20,20);
  GE_Context *background_color = GE_gradient_z(GE_rgb(70,100,30), GE_rgba(0,100,200,150), Z_BACKGROUND);
  GE_Context *background_linecolor = GE_rgb_color_z(70,100,30, Z_LINENUMBERS - 1);//, GE_rgb(100,200,70));

  int track1_x1 = 5*12;

  int track,line;

  GE_filledBox(background_linecolor, -1,        0,    track1_x1, num_lines*line_height);
  GE_filledBox(background_color    , track1_x1, 0,    num_tracks*track_width, num_lines*line_height);

  // line numbers and background
  for(line=0;line<num_lines;line++) {
    float y = line*line_height;
    //float y2 = y + line_height;
    float x = 0;
    float x2 = num_tracks * track_width;
    //GE_add(GE_filledBox(GE_point(x+1.0f,y),GE_point(x2,y2)),
    //       background_color);
    GE_line(line_border_color, x, y, x2, y, 2.5f);

    GE_text(linenumberscolor, talloc_format("%.4d",line+1), 10, y);
  }

  for(track=0;track<num_tracks;track++){
    float x = track1_x1 + track*track_width;
    float x2 = x + track_width;
    GE_line(track_border_color,
            x, 0,
            x, num_lines*line_height,
            2.0f);

    int line = 0;
    float line_end = 0;
    do{
      line_end = line + rnd(0.1,max_note_length);
      if(line_end > num_lines)
        line_end = num_lines;

      int notenum = rnd(0,130);
      const char *notename = NotesTexts3[notenum];

      GE_text(black,
              notename,
              x,line*line_height);

      int num_points = 6;
      APoint *polygon = (APoint*)talloc_atomic(num_points*sizeof(APoint));

      polygon[0].x = x+notetrack_width;
      polygon[0].y = line*line_height;

      polygon[1].x = rnd(x+notetrack_width,x2);
      polygon[1].y = line*line_height;

      polygon[2].x = x+notetrack_width;
      polygon[2].y = (line+line_end)*line_height/2;

      polygon[3].x = polygon[1].x-5;
      polygon[3].y = (line+line_end)*line_height/2;

      polygon[4].x = x+notetrack_width;
      polygon[4].y = line_end*line_height;

      polygon[5].x = rnd(x+notetrack_width,x2);
      polygon[5].y = line_end*line_height;

      GE_Context *col = GE_rgba_color(50,200,200,scale(notenum,0,130,50,255));
      GE_trianglestrip(col, num_points, polygon);

      polygon[2] = polygon[3];
      polygon[3] = polygon[5];

      GE_polyline(velocity_border_color, num_points-1, polygon, 2.5);

      {
        num_points = 8 + (line_end-line)*line_height / 2;
        APoint *triangles = (APoint*)talloc_atomic(num_points*sizeof(APoint));

        int p=0;

        float middle = (x+notetrack_width + x2)/2.0f;
        float i = line*line_height;
        while(i < line_end*line_height){
          triangles[p].x = rnd(x+notetrack_width, middle);
          triangles[p].y = i;
          p++;
          if(p==num_points)
            break;

          triangles[p].x = rnd(middle, x2);
          triangles[p].y = i;
          //printf("p1: %d, x: %f, y: %f\n",p,triangles[p].x,triangles[p].y);
          p++;
          if(p==num_points)
            break;

          i+= 4.0;
        }

        //GE_polyline(white, p, triangles,1.0);
        GE_trianglestrip(waveform, p, triangles);
      }

      /*
      GE_add(GE_line(GE_point(rnd(x+notetrack_width,x2), line*line_height),
                     GE_point(rnd(x+notetrack_width,x2), line_end*line_height),
                     2.5),
             velocity_color);
      */
      line = line_end + rnd(1,num_notes_to_next_line);
      if(line>=num_lines)
        break;

    }while(true);
  }

  // scrollbar
  GE_Context *c = GE_rgba_color_z(0, 150, 180, 155, Z_SCROLLBAR);
  GE_filledBox(c, 0, 0, 20, 100);

  // cursor
  c = GE_rgba_color_z(0,150,100,100,Z_STATIC);
  GE_filledBox(c, 0, height / 2 - 20, width, height / 2);
}


int main(int argc, char **argv){
  QCoreApplication::setAttribute(Qt::AA_X11InitThreads);


  QApplication *qapplication=new QApplication(argc,argv);
  //new QApplication(argc,argv);

  vl::VisualizationLibrary::init();

  //int e = 5;
  //GE p = GE_point(2.3,4.5);
  //int f = 10;

  int height = 1000;

#if 0
  GE_add(GE_line(GE_point(2.3, 4.5),
                 GE_point(5.6, 7.8),
                 1.2f),
         GE_color(0));

  /*
    GE_line(GE_color(0),
            2.3, 4.5,
            5.6, 7.8,
            1.2f);
  */

  //Ge_add(NULL, GE_line(2.3, 4.5, 5.6, 7.8));
  GE_add(GE_text("hello", GE_point(2,4), 50, 0), GE_color(1));

  //GE_text(GE_color(1), "hello", 2, 4, 0);
#endif

  //printf("x/y: %f/%f %d/%d\n",p.point.x,p.point.y,e,f);


  /* setup the OpenGL context format */
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

  {
    MainWidget wadget;

    wadget.resize(2000,1900);
    wadget.show();

    vl::ref<MyQt4ThreadedWidget> widget = new MyQt4ThreadedWidget(vlFormat, &wadget);
    widget->resize(1000,height);

    widget->move(100,200);
    widget->show();

    qapplication->exec();
    //main_loop(widget);

    widget->stop();
  }


  return 0;
}

/*
  GE_AddLine(GE_color(5), GE_point(2,3), GE_point(5,6))
 */

