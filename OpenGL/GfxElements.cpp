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

#include <stdint.h>

#include <unistd.h>

#include <map>
#include <vector>

#include <QMap>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-equal"
#include <vlCore/VisualizationLibrary.hpp>
#include <vlGraphics/OpenGLContext.hpp>
#include <vlVG/VectorGraphics.hpp>
#include <vlGraphics/Rendering.hpp>
#include <vlVG/SceneManagerVectorGraphics.hpp>
#include <vlGraphics/Effect.hpp>
#include <vlGraphics/GLSL.hpp>
#pragma GCC diagnostic pop

#define USE_FREETYPE 0
#define RADIUM_DRAW_FONTS_DIRECTLY 0
#define INCLUDE_SHADERS 1

extern double g_opengl_scale_ratio;

struct PaintingData;
static void setActorEnableMask(vl::Actor *actor, const PaintingData *painting_data);
#include "TextBitmaps.hpp"

#include "../common/nsmtracker.h"
#include "../common/OS_settings_proc.h"
#include "../common/Mutex.hpp"
#include "../common/QueueStack.hpp"

#include "../Qt/Qt_colors_proc.h"

#define OPENGL_GFXELEMENTS_CPP
#define GE_DRAW_VL
#include "GfxElements.h"

#include "T2.hpp"


#define DEBUG_PRINT 0

#if defined(RELEASE) && DEBUG_PRINT==1
#error "oops"
#endif


#define USE_TRIANGLE_STRIPS 0
#define NUM_PREDEFINED_COLORS 16


double g_opengl_scale_ratio = 1.0;

static float g_height = 512;

// Called from vl::Widget::resizeEvent
void GE_set_height(int height){
  if (height<=0)
    height = 1; // avoid various situations.

  g_height = height;
}


int GE_get_height(void){
  //return root->song->tracker_windows->wblock->t.y2 - root->song->tracker_windows->wblock->t.y1;
  return g_height;
}


/*
int GE_get_tracker_height(){
  return root->song->tracker_windows->wblock->t.y2 - root->song->tracker_windows->wblock->t.y1;
}

extern struct Root *root;
*/

GE_Rgb GE_get_rgb(enum ColorNums colornum, bool is_instrument){
  QColor c = get_qcolor(colornum);
  //GE_Rgb ret = {50,60,20,255};
  
  if(is_instrument)
    apply_instrument_in_editor_colorization(c);
  
  GE_Rgb ret = {(unsigned char)c.red(), (unsigned char)c.green(), (unsigned char)c.blue(), (unsigned char)c.alpha()};
  return ret;
}

GE_Rgb GE_get_custom_rgb(int custom_colornum){
  const QColor c = get_custom_qcolor(custom_colornum);
  GE_Rgb ret = {(unsigned char)c.red(), (unsigned char)c.green(), (unsigned char)c.blue(), (unsigned char)c.alpha()};
  return ret;
}

#if INCLUDE_SHADERS
static vl::GLSLFragmentShader *get_gradient_fragment_shader(GradientType::Type type){
  static vl::ref<vl::GLSLFragmentShader> gradient_velocity_shader = NULL;
  static vl::ref<vl::GLSLFragmentShader> gradient_horizontal_shader = NULL;

  if(gradient_velocity_shader.get()==NULL) {
    vl::String path1 =
      vl::String(OS_get_program_path2().id)
      .append(vl::String(OS_get_directory_separator()))
      .append(vl::String("glsl"))
      .append(vl::String(OS_get_directory_separator()))
      ;

    vl::String path2 = path1;
    
    gradient_velocity_shader = new vl::GLSLFragmentShader(path1.append("gradient.fs"));
    gradient_horizontal_shader = new vl::GLSLFragmentShader(path2.append("horizontal_gradient.fs"));
  }

  if (type==GradientType::VELOCITY)
    return gradient_velocity_shader.get();
  else if (type==GradientType::HORIZONTAL)
    return gradient_horizontal_shader.get();
  else
    RWarning("Unknown gradient type %d", type);
   
  return NULL;
}
#endif

#if 0
static vl::GLSLVertexShader *get_gradient_vertex_shader(void){
  static vl::ref<vl::GLSLVertexShader> gradient_shader = NULL;

  if(gradient_shader.get()==NULL) {
    vl::String path =
      vl::String(OS_get_program_path2())
      .append(vl::String(OS_get_directory_separator()))
      .append(vl::String("glsl"))
      .append(vl::String(OS_get_directory_separator()))
      .append(vl::String("gradient.vs"))
      ;

    gradient_shader = new vl::GLSLVertexShader(path);
  }
  
  return gradient_shader.get();
}
#endif


struct GradientTriangles : public vl::Effect {
  GradientTriangles *next = NULL;
  
  std::vector<vl::dvec2> triangles;

  GradientType::Type type;
  
  float y, height;
  float x, width;
  
  vl::fvec4 color1;
  vl::fvec4 color2;
   
private:
  
  vl::ref<vl::GLSLProgram> glsl; // seems like reference must be stored here to avoid memory problems.
  bool glsl_is_valid = true;

  vl::Uniform *uniform_y;
  vl::Uniform *uniform_height;
  vl::Uniform *uniform_color1;
  vl::Uniform *uniform_color2;
  vl::Uniform *uniform_x;
  vl::Uniform *uniform_width;

public:
  
  MyIMutex ref_mutex;
  
  GradientTriangles(GradientType::Type type)
    : type(type)
  {
    setRefCountMutex(&ref_mutex);
    setAutomaticDelete(false);

    init_shader();
  }

  ~GradientTriangles(){
    RError("~GradientTriangles was called. That should not happen. Expect crash.");
    //abort();
  }

private:

  void init_shader(void){
#if !INCLUDE_SHADERS
    glsl_is_valid = false;
#else
    vl::Shader* shader = this->shader();
    shader->enable(vl::EN_BLEND);
    
    // These two lines probably takes a lot of time.
    glsl = shader->gocGLSLProgram();    
    if (glsl->attachShader(get_gradient_fragment_shader(type)) == false){
      GFX_Message2(NULL, true, "Unable to create OpenGL Shader. It might help to disable \"Draw in separate process\".");
      glsl_is_valid = false;
      return;
    }
    
    // And this one.
    glsl->linkProgram();
    
    if (glsl->linked()==false){
      GFX_Message2(NULL, true, "Linking OpenGL Shader failed. It might help to disable \"Draw in separate process\".");
      glsl_is_valid = false;
      return;
    }
    
    if (type==GradientType::VELOCITY) {
      uniform_y = glsl->gocUniform("y");
      uniform_height = glsl->gocUniform("height");
    }
    
    uniform_color1 = glsl->gocUniform("color1");
    uniform_color2 = glsl->gocUniform("color2");
    
    uniform_x = glsl->gocUniform("x");
    uniform_width = glsl->gocUniform("width");
#endif
  }
  
public:
  
  vl::Actor *render(vl::VectorGraphics *vg) {
      
    vl::Actor *actor = vg->fillTriangles(triangles);

    if (glsl_is_valid==true) {
    
      actor->setEffect(this);

      uniform_color1->setUniform(color1);
      uniform_color2->setUniform(color2);
      if (type==GradientType::VELOCITY)
        uniform_height->setUniformF(height*g_opengl_scale_ratio);
      uniform_x->setUniformF(x*g_opengl_scale_ratio);
      uniform_width->setUniformF(width);
      
      if (type==GradientType::VELOCITY)
        set_y_offset(0.0f);
    }

    return actor;
  }

  // called from main thread
  void clean(){
    triangles.clear();
  }

  // OpenGL thread, except when initializing. During initialization, it's T3 thread.
  void set_y_offset(float y_offset){
    if (glsl_is_valid && type==GradientType::VELOCITY)
      uniform_y->setUniformF((y + y_offset)*g_opengl_scale_ratio);
  }
};

#if DEBUG_PRINT
static int gradlist_length(GradientTriangles *gradients){
  int ret = 0;
  while(gradients!=NULL){
    ret++;
    gradients = gradients->next;
  }
  return ret;
}
#endif

static void TS_push_free_gradient_triangle(GradientType::Type type, GradientTriangles* gradient);

#define GQUEUE_MIN_SIZE 128 // If we have less gradients than this, we panic and allocate immediately.
#define GQUEUE_MAX_SIZE 256 // If we have less gradients than this, we allocate new ones when it's a good time to do so.

#define GQUEUE_TO_TYPE(I) ((I)==0 ? GradientType::HORIZONTAL : GradientType::VELOCITY)
#define TYPE_TO_GQUEUE(T) (T==GradientType::HORIZONTAL ? 0 : 1)

// Having two collections is not necessary, but I figured the code would be simpler this way (not so sure anymore).
// Buffer1 (GradientTrianglesCollection1) holds gradients which are currently used and
// gradients which have been used earlier, while Buffer2 (GradientTrianglesCollection2) holds fresh, i.e. never used, gradients.
// Of course, the "earlier used" and the "never used" buffers could be merged, and the code would probably be cleaner if we did.
//
// Why so complicated? Because there's some indication (virtualbox opengl driver) that drivers sometimes doesn't work
// if we allocate shaders on a different thread than the main OpenGL thread. Earlier we allocated in the T2 thread,
// which has it's own context, so it should be fine, but it didn't work with the virtualbox opengl driver. It's not very
// important what the virtualbox opengl driver does though, but it might be an indication that it's better to do it this way.
// Maybe it will fix the crashes that happens when enabling "draw in separate process" on OSX.
struct GradientTrianglesCollection2 {

  GradientType::Type _type;

  radium::Queue<GradientTriangles*, GQUEUE_MAX_SIZE> _queue; // t3 -> t2

  DEFINE_ATOMIC(int, _num_gradients_to_push_to_1) = 0;

#if !defined(RELEASE)
  int _num_created_total = 0;
  int _num_requested_total = 0;
#endif

  
  GradientTrianglesCollection2(GradientType::Type type)
    : _type(type)
  {}

  DEFINE_ATOMIC(int, _visitnum) = 0;
  
  bool T3_create_gradienttriangles_if_needed(bool force_creating_all, bool has_alloced_this_time, bool spent_some_time_last_call, bool got_new_t2_data){

    bool has_alloced = false;
    
    int size = _queue.size();

    if (size > GQUEUE_MIN_SIZE && !force_creating_all)
      if (has_alloced_this_time || spent_some_time_last_call || got_new_t2_data || is_playing())  // Preferably, we only create at most one shader each second frame while not playing. We don't want to allocate when we get new t2 data either, because those frames takes much longer time to render.
        return has_alloced;
      
    int num_to_push_to_1 = ATOMIC_GET(_num_gradients_to_push_to_1);
    int num_pushed_to_1 = 0;
      
    while (size < GQUEUE_MAX_SIZE || num_to_push_to_1>num_pushed_to_1){
#if DEBUG_PRINT
      printf("T3: Creating new GradientTriangles instance. Queue #%d. queue size: %d. NULL: %d, Total: %d\n", TYPE_TO_GQUEUE(_type), size, num_to_push_to_1, ++_num_created_total);
#endif
      auto *gradient = new GradientTriangles(_type);
      has_alloced = true;

      if (size==GQUEUE_MAX_SIZE){
        TS_push_free_gradient_triangle(_type, gradient);
        num_pushed_to_1++;
      }else{
        _queue.put(gradient);
        size++;
      }
      
      if (size > GQUEUE_MIN_SIZE && !force_creating_all)
        break;
    }

    if (num_pushed_to_1 > 0){
      int newnum = ATOMIC_ADD_RETURN_NEW(_num_gradients_to_push_to_1, -num_pushed_to_1);
      if (newnum < 0)
        ATOMIC_SET(_num_gradients_to_push_to_1, 0);
    }

    ATOMIC_ADD(_visitnum, 1);
    
    return has_alloced;
  }

  void T1_wait_until_all_gradients_are_created(void){
    int visitnum = ATOMIC_GET(_visitnum);

    if (visitnum==0) // Program startup. Deadlock without this check.
      return;

#if THREADED_OPENGL
    do{
      msleep(20);
    }while(ATOMIC_GET(_visitnum) < visitnum+2 || _queue.size() < GQUEUE_MAX_SIZE || ATOMIC_GET(_num_gradients_to_push_to_1) > 0);
#endif
  }
  
  void T1_prepare_for_new_rendering(void){
    ATOMIC_SET(_num_gradients_to_push_to_1, 0); // If not, this number can grow very large.
  }
  
  GradientTriangles *T1_get_gradienttriangles(void){
    
    int size = _queue.size();


#if DEBUG_PRINT
    printf("T1: Requesting new gradient triangles. Queue #%d. queue size: %d. Total: %d. \n", TYPE_TO_GQUEUE(_type), size, ++_num_requested_total);
#endif
    
    if (size > 0)
      return _queue.get();
    else{
      ATOMIC_ADD(_num_gradients_to_push_to_1, 1);
      return NULL;
    }
  }

};

  
static GradientTrianglesCollection2 horizontalGradientTriangles2(GradientType::HORIZONTAL);
static GradientTrianglesCollection2 velocityGradientTriangles2(GradientType::VELOCITY);

static GradientTrianglesCollection2 &get_collection2(GradientType::Type type){
  if (type==GradientType::HORIZONTAL)
    return horizontalGradientTriangles2;
  else
    return velocityGradientTriangles2;
}

void T3_create_gradienttriangles_if_needed(bool got_new_t2_data){
  static bool spent_some_time_last_call = false;
  static bool has_inited = false;
  
  GradientTrianglesCollection2 *queue1, *queue2;
  
  if (horizontalGradientTriangles2._queue.size() < velocityGradientTriangles2._queue.size()){
    queue1 = &horizontalGradientTriangles2;
    queue2 = &velocityGradientTriangles2;
  } else {
    queue1 = &velocityGradientTriangles2;
    queue2 = &horizontalGradientTriangles2;
  }

  bool force_creating_all = has_inited==false || ATOMIC_GET(g_is_creating_all_GL_blocks);
    
  bool has_alloced_this_time1 = queue1->T3_create_gradienttriangles_if_needed(force_creating_all, false, spent_some_time_last_call, got_new_t2_data);
  bool has_alloced_this_time2 = queue2->T3_create_gradienttriangles_if_needed(force_creating_all, has_alloced_this_time1, spent_some_time_last_call, got_new_t2_data);

  spent_some_time_last_call = has_alloced_this_time1 || has_alloced_this_time2 || got_new_t2_data;
  
  has_inited = true;
}

static void T1_wait_until_all_gradients_are_created(void){
  horizontalGradientTriangles2.T1_wait_until_all_gradients_are_created();
  velocityGradientTriangles2.T1_wait_until_all_gradients_are_created();
}
  
static void T1_prepare_gradients2_for_new_rendering(void){
  horizontalGradientTriangles2.T1_prepare_for_new_rendering();
  velocityGradientTriangles2.T1_prepare_for_new_rendering();
}

static GradientTriangles *T1_get_gradienttriangles2(GradientType::Type type){
  return get_collection2(type).T1_get_gradienttriangles();
}

  


  

struct GradientTrianglesCollection {

  GradientType::Type type;

  radium::Mutex lock;
  
  // These two are only accessed by the main thread
  GradientTriangles *used_gradient_triangles; // Only used by T3.
  
  GradientTriangles *free_gradient_triangles; // Used by both T1 and T3. Must be protected by lock

  GradientTrianglesCollection(GradientType::Type type)
    : type(type)
    , used_gradient_triangles(NULL)
    , free_gradient_triangles(NULL)
  {}

#if DEBUG_PRINT
  int freesize(void){
    radium::ScopedMutex daslock(lock);
    return gradlist_length(free_gradient_triangles);
  }
#endif
  
  GradientTriangles *TS_get_free_gradient_triangle(void){
    radium::ScopedMutex daslock(lock);

    GradientTriangles *gradient = free_gradient_triangles;

    if (gradient == NULL) {
      
      return NULL;
      
    } else {
    
      free_gradient_triangles = gradient->next;

      return gradient;
    }
  }
  
  void TS_push_free_gradient_triangle(GradientTriangles *gradient){
    radium::ScopedMutex daslock(lock);

    R_ASSERT_NON_RELEASE(gradient->next==NULL);
    
    gradient->next = free_gradient_triangles;
    free_gradient_triangles = gradient;    
  }
  
  void TS_push_several_free_gradient_triangle(GradientTriangles *gradients, GradientTriangles *last_element){
    if (gradients==NULL){
      
      R_ASSERT(last_element==NULL);

    } else {      
      radium::ScopedMutex daslock(lock);
      
      last_element->next = free_gradient_triangles;
      free_gradient_triangles = gradients;
    }
  }
  
  // main thread
  void T1_collect_gradient_triangles_garbage(void){
    GradientTriangles *new_used = NULL;
    GradientTriangles *new_free = NULL;
    GradientTriangles *last_new_free = NULL;
    
    //R_ASSERT(free_gradient_triangles == NULL); // T3 could have pushed a free gradient since last check.

#if DEBUG_PRINT
    int bef = gradlist_length(used_gradient_triangles);
#endif
    
    GradientTriangles *gradient = used_gradient_triangles;

    int numa=0,numb=0;
    
    while(gradient!=NULL){
      GradientTriangles *next = gradient->next;
      
      gradient->ref_mutex.lock();
      bool is_free = gradient->referenceCount()==0;
      gradient->ref_mutex.unlock();
      
      if(is_free) {
        gradient->next = new_free;
        if (new_free==NULL)
          last_new_free = gradient;
        new_free = gradient;
        numa++;
      } else {
        gradient->next = new_used;
        new_used = gradient;
      }

      numb++;
      
      gradient = next;
    }

    /*
      // We clean it when allocating instead. Might cause less cpu spikes.
    gradient = new_free;
    while(gradient != NULL){
      gradient->clean();
      gradient = gradient->next;
    }
    */

#if DEBUG_PRINT
    int aft = gradlist_length(new_used);
    int fbef = freesize();
#endif
    
    used_gradient_triangles = new_used;
    TS_push_several_free_gradient_triangle(new_free, last_new_free);

#if DEBUG_PRINT
    printf("    Collecting gradient garbage finished #%d. %d / %d. Len: %d / %d. Free: %d/%d\n",TYPE_TO_GQUEUE(type), numa,numb,bef,aft,fbef,freesize());
#endif
  }


  // main thread
  GradientTriangles *T1_get_gradient_triangles(void){
    GradientTriangles *gradient = TS_get_free_gradient_triangle(); // Reuse old.
    if (gradient!=NULL)
      gradient->clean();
    else
      gradient = ::T1_get_gradienttriangles2(type); // Get a new.
    
    if (gradient==NULL)
      return NULL;
    
    // push used
    gradient->next = used_gradient_triangles;
    used_gradient_triangles = gradient;
    
    //printf("Size of #%d: %d. Freesize: %d\n", TYPE_TO_GQUEUE(type), gradlist_length(used_gradient_triangles), freesize());
      
    return gradient;
  }

};
   
static GradientTrianglesCollection horizontalGradientTriangles(GradientType::HORIZONTAL);
static GradientTrianglesCollection velocityGradientTriangles(GradientType::VELOCITY);

static GradientTrianglesCollection &get_collection(GradientType::Type type){
  if (type==GradientType::HORIZONTAL)
    return horizontalGradientTriangles;
  else
    return velocityGradientTriangles;
}

static GradientTriangles *T1_get_gradient_triangles(GradientType::Type type){
  return get_collection(type).T1_get_gradient_triangles();
}

static void TS_push_free_gradient_triangle(GradientType::Type type, GradientTriangles* gradient){
  get_collection(type).TS_push_free_gradient_triangle(gradient);
}

// Called from GE_start_writing();
static void T1_collect_gradients1_garbage(void){
  horizontalGradientTriangles.T1_collect_gradient_triangles_garbage();
  velocityGradientTriangles.T1_collect_gradient_triangles_garbage();
}


struct _GE_Context : public vl::Object{
  //std::map< int, std::vector<vl::dvec2> > lines; // lines, boxes and polylines
  std::vector< vl::dvec2 > boxes; // filled boxes

#if USE_TRIANGLE_STRIPS
  std::vector< std::vector<vl::dvec2> > trianglestrips; // Used instead of polygons for displaying waveform data. Trianglestrips are directly supported by opengl, so it's faster plus that conversions from polygons to triangles are unnecessary.
#else
  std::vector<vl::dvec2> triangles; // filled triangles. Used instead of trianglestrips. Just paint all triangles one by one. GPU usage seems approxiately the same as tringle strips, but CPU usage is significantly lower.
#endif

  TextBitmaps textbitmaps;
  TextBitmaps textbitmaps_halfsize;

  union Color{
    struct{
      GE_Rgb c;
      GE_Rgb c_gradient;
    };
    uint64_t key;
  } color;

  GE_Conf _conf;
  int _slice;
  mutable vl::ref<vl::Scissor> _scissor;

  std::vector< vl::ref<GradientTriangles> > gradient_triangles;
  
  //private:
  // vl::ref<vl::Image> gradient;
public:
  //bool is_gradient;
  /*
  vl::ref<vl::Image> get_gradient(){
    if(gradient.get()==NULL)
      gradient = vl::makeColorSpectrum(128,
                                       get_vec4(color.c),
                                       get_vec4(color.c_gradient)
                                       );
    return gradient;
  }
  */

  _GE_Context(const Color &_color, const GE_Conf &conf, int slice)
    : textbitmaps(false)
    , textbitmaps_halfsize(true)
      //, has_scissor(false)
    , color(_color)
    , _conf(conf)
    , _slice(slice)
      //, gradient(NULL)
      //, is_gradient(false)
  {
    //static int num = 0; printf("num2: %d\n", num++);
    R_ASSERT(sizeof(Color)==sizeof(uint64_t));
  }

  vl::Scissor *get_scissor(const PaintingData *painting_data) const {
    if (_conf.use_scissors==NO_SCISSORS)
      return NULL;
    
    if (_scissor.get()==NULL){
      //static int num = 0; printf("num: %d\n", num++);
      const SharedVariables *shared_variables = GE_get_shared_variables(painting_data);
      _scissor = new vl::Scissor(shared_variables->wtracks_scissor_x1 * g_opengl_scale_ratio,
                                 0,
                                 (shared_variables->wtracks_scissor_x2 - shared_variables->wtracks_scissor_x1) * g_opengl_scale_ratio,
                                 g_height*g_opengl_scale_ratio
                                 );
    }

    return _scissor.get();
  }

#if 0
  ~_GE_Context(){
    printf("Deleting context with z %d\n",_conf.z);
  }
#endif

  static float y(float y){
    float height = safe_volatile_float_read(&g_height);
    float ret = scale(y,
                      0,height,
                      height,0
                      );

    //if (g_opengl_scale_ratio > 1.0)
    //  ret -= height/g_opengl_scale_ratio;
    //else if (g_opengl_scale_ratio < 1.0)
    //  ret += height/2.0;

    return ret;
  }

  vl::Transform *get_transform(T2_data *t2_data, bool &is_scroll_transform) const {
    is_scroll_transform = false;
    if (Z_IS_STATIC_X(_conf.z)){
      is_scroll_transform = true;
      return t2_data->scroll_transform.get();
    }else if (_conf.z == Z_PLAYCURSOR)
      return t2_data->playcursor_transform.get();
    else if (_conf.z <= Z_MAX_SCROLLTRANSFORM){
      is_scroll_transform = true;
      return t2_data->scroll_transform.get();
    }else if (_conf.z < Z_MIN_STATIC)
      return t2_data->scrollbar_transform.get();
    else
      return NULL;
  }

};

void GE_set_z(GE_Context *c, int new_z) {
  c->_conf.z = new_z;
}

int GE_get_z(const GE_Context *c){
  return c->_conf.z;
}

GE_Rgb GE_get_rgb(const GE_Context *c){
  return c->color.c;
}

/* Drawing */

static void setActorEnableMask(vl::Actor *actor, const PaintingData *painting_data){
  int height = safe_volatile_float_read(&g_height);// * g_opengl_scale_ratio;

  int y1 = scale(actor->boundingBox().maxCorner().y(),
                 height, 0,
                 0, height);
  int y2 = scale(actor->boundingBox().minCorner().y(),
                 height, 0,
                 0, height
                 );

  //printf("  y1: %d, y2: %d. mask: %x\n",y1,y2,getMask(y1,y2));
  actor->setEnableMask(getMask(y1, y2, GE_get_slice_size(painting_data)));
}

static void setScrollTransform(const GE_Context *c, vl::Actor *actor, T2_data *t2_data){

  vl::Scissor *scissor = c->get_scissor(t2_data->painting_data);
  if (scissor != NULL)
    actor->setScissor(scissor);
  
  actor->computeBounds();

  bool set_mask;
  vl::Transform *transform = c->get_transform(t2_data, set_mask);

  if (set_mask)
    setActorEnableMask(actor, t2_data->painting_data);

  actor->setTransform(transform);
}


/*
static int get_key_from_pen_width(float pen_width){
  int ret = pen_width * 10;
  if (ret==0)
    ret = 1;
  return ret;
}
static float get_pen_width_from_key(int key){
  return (float)key/10.0;
}
*/

typedef QMap<int,                        // <- z
             QHash<int,                  // <- block slice (from calculated from y)
                   QHash<enum UseScissors,            // <- whether to use scissor. 0: no scissor, 1: scissor (tried to use array instead of hash, but the code became too complicated)
                         QHash<uint64_t, // <- color
                               vl::ref<GE_Context>
                               >
                         >
                   >
             > Contexts;


// Contains all data necessary to paint the editor.
// Created in the main thread, and then transfered to the OpenGL thread.
struct PaintingData{
  Contexts contexts;
  //std::vector< vl::ref<GradientTriangles> > gradient_triangles;
  SharedVariables shared_variables;

  int slice_size;
  
  PaintingData(int full_height, bool block_is_visible)
    : shared_variables(block_is_visible)
  {
    slice_size = full_height / 32;
    if (slice_size < MIN_SLICE_SIZE)
      slice_size = MIN_SLICE_SIZE;
  }
};

void GE_delete_painting_data(PaintingData *painting_data){
  delete painting_data;
}



// This variable is only accessed by the main thread while building up a new PaintingData.
// It is not necessary for this variable to be global, and the code is more confusing because of that.
// However, by letting it be global, we don't have to send it around everywhere.
//
// In short: It can only be used by the main thread while while GL_create is called.
static PaintingData *g_painting_data = NULL;

// Called from the OpenGL thread
const SharedVariables *GE_get_shared_variables(const PaintingData *painting_data){
  return &painting_data->shared_variables;
}

int GE_get_slice_size(const PaintingData *painting_data){
  return painting_data->slice_size;
}

// Called from the main thread
void GE_start_writing(int full_height, bool block_is_visible){
  R_ASSERT(g_painting_data==NULL);

  T1_ensure_t2_is_initialized();
  
  g_painting_data = new PaintingData(full_height, block_is_visible);
  GE_fill_in_shared_variables(&g_painting_data->shared_variables);

  T1_prepare_gradients2_for_new_rendering();
  T1_collect_gradients1_garbage();
}

// Called from the main thread
void GE_end_writing(GE_Rgb new_background_color){
  T1_send_data_to_t2(g_painting_data, new_background_color);
    
  g_painting_data = NULL;
}

// Called from the main thread. Only used when loading song to ensure all gradients are created before starting to play.
void GE_wait_until_block_is_rendered(void){
  T1_wait_until_t2_got_t1_data();
  // T1_wait_until_t3_got_t2_data(); // A bit inconvenient to make that function since the t2_to_t3 queue is not threadsafe. Instead we let T1_wait_until_all_gradients_are_created wait at least two periods.
  T1_wait_until_all_gradients_are_created();
}



/*****************************************/
/* Drawing.  Called from OpenGL thread. */
/****************************************/


static void setColorBegin(vl::VectorGraphics *vg, const GE_Context *c){
#if 0
  if(false && c->is_gradient){
    vg->setImage(c->get_gradient().get());
    vg->setColor(vl::white);
  } else
#endif
    vg->setColor(get_vec4(c->color.c));
}

static void setColorEnd(vl::VectorGraphics *vg, const GE_Context *c){
#if 0
  if(c->is_gradient)
    vg->setImage(NULL);
#endif
}


// This function can probably be avoided somehow. The absolute y position of the vertexes should be available for the shader GLSL code, but I haven't
// figured out how to get it yet.
//
// OpenGL Thread
void GE_update_triangle_gradient_shaders(PaintingData *painting_data, float y_offset){
  for (const auto &hepp : painting_data->contexts) {
    
    for (const auto &contexts : hepp) {
      
      for (const auto &c2 : contexts) {

        for (const auto &c : c2) {
          
          for (vl::ref<GradientTriangles> gradient_triangles : c->gradient_triangles)
            gradient_triangles->set_y_offset(y_offset);
          
        }
      }
    }
  }
}

// T2 thread.
void GE_draw_vl(T2_data *t2_data){
  //GL_draw_lock();

  PaintingData *painting_data = t2_data->painting_data;
  vl::VectorGraphics *vg = t2_data->vg.get();

  vg->startDrawing(); {

#if RADIUM_DRAW_FONTS_DIRECTLY
    vg->setFont("font/Cousine-Bold.ttf", 10, false);
#endif

    vg->setLineSmoothing(true);
    vg->setPolygonSmoothing(true);
    //vg->setPointSmoothing(true); /* default value */
    //vg->setPointSmoothing(root->editonoff); /* default value */
    vg->setPointSmoothing(false); // images are drawn using drawPoint.
    //vg->setTextureMode(vl::TextureMode_Repeat 	); // Note: MAY FIX gradient triangle non-overlaps.

    /*
    vl::EBlendFactor src_rgb;
    vl::EBlendFactor dst_rgb;
    vl::EBlendFactor src_alpha;
    vl::EBlendFactor dst_alpha;
    vg->getBlendFunc(src_rgb,dst_rgb,src_alpha,dst_alpha);
    
    vg->setBlendFunc(src_rgb, dst_rgb, vl::BF_ONE_MINUS_SRC_ALPHA, vl::BF_ONE_MINUS_DST_ALPHA);
    */
    
      for (const auto &hepp : painting_data->contexts) {
        
        for (const auto &contexts : hepp) {
          
          for (const auto &c2 : contexts) {
        
            // 1. Filled boxes
            for(const auto &c : c2){
              
              if(c->boxes.size() > 0) {
                setColorBegin(vg, c.get());
                
                setScrollTransform(c.get(), vg->fillQuads(c->boxes), t2_data);
                
                setColorEnd(vg, c.get());
              }
            }
          
            // 2. triangle strips
            for(const auto &c : c2){
              
#if USE_TRIANGLE_STRIPS
              if(c->trianglestrips.size() > 0) {
                setColorBegin(vg, c.get());
                setScrollTransform(c.get(), vg->fillTriangleStrips(c->trianglestrips), t2_data);
                //vg->fillPolygons(c->trianglestrips);
                
                setColorEnd(vg, c.get());
              }
              // note: missing gradient triangles for USE_TRIANGLE_STRIPS.
#else
              for (vl::ref<GradientTriangles> gradient_triangles : c->gradient_triangles)
                setScrollTransform(c.get(), gradient_triangles->render(vg), t2_data);
              
              if(c->triangles.size() > 0) {
                setColorBegin(vg, c.get());
                
                setScrollTransform(c.get(), vg->fillTriangles(c->triangles), t2_data);
                //printf("triangles size: %d\n",(int)c->triangles.size());
                
                setColorEnd(vg, c.get());
              }
            }
            
#endif

      
          // 3. Polylines
          // 4. Boxes
          // 5. Lines
          /*
            for(auto iterator = contexts.begin(); iterator != contexts.end(); ++iterator) {
        
            const GE_Context *c = iterator.value().get();
        
            bool has_set_color = false;
        
            for(auto iterator = c->lines.begin(); iterator != c->lines.end(); ++iterator) {
            if(has_set_color==false)
            setColorBegin(vg, c);
            has_set_color=true;
          
            vg->setLineWidth(get_pen_width_from_key(iterator->first));
            setScrollTransform(c, vg->drawLines(iterator->second), t2_data);
            //if(c->triangles.size()>0)
            //  setScrollTransform(c, vg->drawLines(c->triangles), scroll_transform, static_x_transform, scrollbar_transform);
            }
        
            if(has_set_color==true)
            setColorEnd(vg, c);
            }
          */
        
          // 6. Text
            for(const auto &c : c2) {
        
              if(c->textbitmaps.points.size() != 0 || c->textbitmaps_halfsize.points.size() != 0) {
                
                setColorBegin(vg, c.get());
                
                bool set_mask;
                vl::Transform *transform = c->get_transform(t2_data, set_mask);
                
                if(c->textbitmaps.points.size() > 0)
                  c->textbitmaps.drawAllCharBoxes(vg, transform, set_mask, painting_data, c->get_scissor(painting_data));
                
                if(c->textbitmaps_halfsize.points.size() > 0)
                  c->textbitmaps_halfsize.drawAllCharBoxes(vg, transform, set_mask, painting_data, c->get_scissor(painting_data));
                
                setColorEnd(vg, c.get());
              }
            }
          }


          //printf("************ z: %d, NUM contexts: %d\n",z, (int)g_contexts.size());

      
        }
      }
      
  }vg->endDrawing();
  //GL_draw_unlock();           
}






/*************************************************/
/* Creating painting_data.  Called from main thread. */
/***********************************************/

static int get_slice_from_y(const int y){
  return y/g_painting_data->slice_size;
}

static GE_Context *get_context(const GE_Context::Color &color, const GE_Conf &conf){
  int slice = get_slice_from_y(conf.y);

  if(g_painting_data->contexts[conf.z][slice][conf.use_scissors].contains(color.key))
    return g_painting_data->contexts[conf.z][slice][conf.use_scissors][color.key].get();

  GE_Context *c = new GE_Context(color, conf, slice);

  g_painting_data->contexts[conf.z][slice][conf.use_scissors][color.key] = c;
  return c;
}

GE_Context *GE_y(GE_Context *c, int y){
  const int slice = get_slice_from_y(y);

  if (slice==c->_slice)
    return c;
  
  c->_conf.y = y;

  return get_context(c->color, c->_conf);
}


GE_Context *GE_z(const GE_Rgb rgb, const GE_Conf &conf){
  GE_Context::Color color;

  color.key = 0;
  color.c = rgb;

  return get_context(color, conf);
}

static GE_Rgb rgb_from_qcolor(const QColor &color){
  GE_Rgb rgb = {(unsigned char)color.red(), (unsigned char)color.green(), (unsigned char)color.blue(), (unsigned char)color.alpha()};
  return rgb;
}

GE_Context *GE_color_z(const QColor &color, const GE_Conf &conf){
  return GE_z(rgb_from_qcolor(color), conf);
}

GE_Context *GE_color_z(enum ColorNums colornum, const GE_Conf &conf){
  //const QColor c = get_qcolor(window, colornum);
  return GE_z(GE_get_rgb(colornum), conf);
}

GE_Context *GE_color_alpha_z(enum ColorNums colornum, float alpha, const GE_Conf &conf){
  GE_Rgb rgb = GE_get_rgb(colornum);
  rgb.a = alpha * 255;
  return GE_z(rgb, conf);
}

GE_Context *GE_textcolor_z(enum ColorNums colornum, const GE_Conf &conf){
  GE_Rgb rgb = GE_get_rgb(colornum);
  rgb.a=230;
  return GE_z(rgb, conf);
}

GE_Context *GE_rgba_color_z(unsigned char r, unsigned char g, unsigned char b, unsigned char a, const GE_Conf &conf){
#if 1
  // Reduce number of contexts. May also reduce cpu usage significantly.
  r |= 15;
  g |= 15;
  b |= 15;
  a |= 15;
#endif

  GE_Rgb rgb = {r,g,b,a};

  return GE_z(rgb, conf);
}

GE_Context *GE_rgb_color_z(unsigned char r, unsigned char g, unsigned char b, const GE_Conf &conf){
  return GE_rgba_color_z(r,g,b,255, conf);
}

GE_Rgb GE_mix(const GE_Rgb c1, const GE_Rgb c2, float how_much){
  GE_Rgb rgb;

  float a1 = how_much / 1000.0f;
  float a2 = 1.0f-a1;

  if(c1.r==0 && c1.g==0 && c1.b==0){ // some of the black lines doesn't look look very good.
    rgb.r = 74*a1 + c2.r*a2;
    rgb.g = 74*a1 + c2.g*a2;
    rgb.b = 74*a1 + c2.b*a2;
    rgb.a = c1.a*a1 + c2.a*a2;
  }else{
    rgb.r = (float)c1.r*a1 + (float)c2.r*a2;
    rgb.g = (float)c1.g*a1 + (float)c2.g*a2;
    rgb.b = (float)c1.b*a1 + (float)c2.b*a2;
    rgb.a = (float)c1.a*a1 + (float)c2.a*a2;
    //printf("r: %d, g: %d, b: %d, a: %d. a1: %f, a2: %f\n",rgb.r,rgb.g,rgb.b,rgb.a,a1,a2);
 }

  return rgb;
}

GE_Context *GE_mix_color_z(const GE_Rgb c1, const GE_Rgb c2, float how_much, const GE_Conf &conf){
  return GE_z(GE_mix(c1, c2, how_much), conf);
}

GE_Context *GE_gradient_z(const GE_Rgb c1, const GE_Rgb c2, const GE_Conf &conf){
  GE_Context::Color color;

  color.c=c1;
  color.c_gradient=c2;

  GE_Context *c = get_context(color, conf);

  //c->is_gradient = true;

  return c;
}

GE_Context *GE_gradient_z(const QColor &c1, const QColor &c2, const GE_Conf &conf){
  return GE_gradient_z(rgb_from_qcolor(c1), rgb_from_qcolor(c2), conf);
}



/************************************************************/
/* Scheduling drawing operations. Called from main thread. */
/**********************************************************/

void GE_set_font(const QFont &font){
  GE_set_new_font(font);
}


#if 0

void GE_line(GE_Context *c, float x1, float y1, float x2, float y2, float pen_width){

  if (c->is_gradient || y1!=y2){
    int key = get_key_from_pen_width(pen_width);
    c->lines[key].push_back(vl::dvec2(x1,c->y(y1+0.1f)));
    c->lines[key].push_back(vl::dvec2(x2,c->y(y2-0.1f)));
  }else{
    float half = pen_width/2.0f;
    c->boxes.push_back(vl::dvec2(x1,c->y(y1-half)));
    c->boxes.push_back(vl::dvec2(x1,c->y(y2+half)));
    c->boxes.push_back(vl::dvec2(x2,c->y(y2+half)));
    c->boxes.push_back(vl::dvec2(x2,c->y(y1-half)));
  }
}

#else

static float scissor_x,scissor_x2;
static bool has_x_scissor=false;

//static float scissor_y,scissor_y2;
//static bool has_y_scissor=false;

/*  
void GE_set_scissor(float x, float y, float x2, float y2) {
  scissor_x = x;
  scissor_y = y;
  scissor_x2 = x2;
  scissor_y2 = y2;
  has_scissor=true;
}
*/

void GE_set_x_scissor(float x, float x2) {
  scissor_x = x;
  scissor_x2 = x2;
  has_x_scissor=true;
}

void GE_unset_x_scissor(void){
  has_x_scissor=false;
}

static void GE_line_lowlevel(GE_Context *c, std::vector<vl::dvec2> &triangles, float x1, float y1, float x2, float y2, float pen_width){
  // Code below mostly copied from http://www.softswit.ch/wiki/index.php?title=Draw_line_with_triangles

  float dx = x2-x1;
  float dy = y2-y1;
 
  float length = sqrtf( dx*dx + dy*dy );   
 
  // perp
  float perp_x = -dy;
  float perp_y = dx;
  if (!equal_floats(length, 0.0f)){
    // Normalize the perp
    perp_x /= length;
    perp_y /= length;
  }
 
  float h = pen_width;//.125*length;
  
  // since perp defines how wide our quad is, scale it
  perp_x *= h;
  perp_y *= h;
 
  float v1x = x1 + perp_x*.5;
  float v1y = y1 + perp_y*.5;
  
  float v2x = x2 + perp_x*.5;
  float v2y = y2 + perp_y*.5;
 
  float v3x = x2 - perp_x*.5;
  float v3y = y2 - perp_y*.5;
 
  float v4x = x1 - perp_x*.5;
  float v4y = y1 - perp_y*.5;

  triangles.push_back(vl::dvec2(v1x, c->y(v1y)));
  triangles.push_back(vl::dvec2(v2x, c->y(v2y)));
  triangles.push_back(vl::dvec2(v3x, c->y(v3y)));

  triangles.push_back(vl::dvec2(v1x, c->y(v1y)));
  triangles.push_back(vl::dvec2(v3x, c->y(v3y)));
  triangles.push_back(vl::dvec2(v4x, c->y(v4y)));
}


#define SWAPFLOAT(a,b) \
  do{                      \
    float c = a;           \
    a = b;                 \
    b = c;                 \
  } while(0)

void GE_line(GE_Context *c, float x1, float y1, float x2, float y2, float pen_width){

  if (has_x_scissor){

    if (x1 <= scissor_x && x2 <= scissor_x)
      return;

    if (x1 >= scissor_x2 && x2 >= scissor_x2)
      return;

    if (x2 < x1) {
      SWAPFLOAT(y1,y2);
      SWAPFLOAT(x1,x2);
    }

    if (x1 < scissor_x) {

      if (!equal_floats(y1, y2))
        y1 = scale(scissor_x, x1, x2, y1, y2);
      
      x1 = scissor_x;

    } 

    if (x2 > scissor_x2) {

      if (!equal_floats(y1, y2))
        y2 = scale(scissor_x2, x1, x2, y1, y2);

      x2 = scissor_x2;

    }

    if (equal_floats(x1, x2) && equal_floats(y1, y2))
      return;
  }
  
#if 0
  if (c->is_gradient) {
    int key = get_key_from_pen_width(pen_width);
    c->lines[key].push_back(vl::dvec2(x1,c->y(y1+0.1f)));
    c->lines[key].push_back(vl::dvec2(x2,c->y(y2-0.1f)));
    return;
  }
#endif


  GE_line_lowlevel(c, c->triangles, x1, y1, x2, y2, pen_width);
}
#endif

void GE_text(GE_Context *c, const char *text, int x, int y){
  c->textbitmaps.addCharBoxes(text, x, c->y(y+1));
}

void GE_text2(GE_Context *c, QString text, int x, int y){
  c->textbitmaps.addCharBoxes(text, x, c->y(y+1));
}

void GE_text_halfsize(GE_Context *c, const char *text, int x, int y){
  c->textbitmaps_halfsize.addCharBoxes(text, x, c->y(y+1));
}

void GE_text_halfsize2(GE_Context *c, QString text, int x, int y){
  c->textbitmaps_halfsize.addCharBoxes(text, x, c->y(y+1));
}

#if 0
void GE_box(GE_Context *c, float x1, float y1, float x2, float y2, float pen_width){
  int key = get_key_from_pen_width(pen_width);
  y1=c->y(y1);
  y2=c->y(y2);

  c->lines[key].push_back(vl::dvec2(x1, y1));
  c->lines[key].push_back(vl::dvec2(x2, y1));

  c->lines[key].push_back(vl::dvec2(x2, y1));
  c->lines[key].push_back(vl::dvec2(x2, y2));

  c->lines[key].push_back(vl::dvec2(x2, y2));
  c->lines[key].push_back(vl::dvec2(x1, y2));

  c->lines[key].push_back(vl::dvec2(x1, y2));
  c->lines[key].push_back(vl::dvec2(x1, y1));
}
#else
void GE_box(GE_Context *c, float x1, float y1, float x2, float y2, float pen_width){
  GE_line(c, x1, y1, x2, y1, pen_width);
  GE_line(c, x2, y1, x2, y2, pen_width);
  GE_line(c, x2, y2, x1, y2, pen_width);
  GE_line(c, x1, y2, x1, y1, pen_width);
}
#endif

void GE_filledBox(GE_Context *c, float x1, float y1, float x2, float y2){
  c->boxes.push_back(vl::dvec2(x1,c->y(y1)));
  c->boxes.push_back(vl::dvec2(x1,c->y(y2)));
  c->boxes.push_back(vl::dvec2(x2,c->y(y2)));
  c->boxes.push_back(vl::dvec2(x2,c->y(y1)));
}

/*
void GE_polyline(GE_Context *c, int num_points, const APoint *points, float pen_width){
  if(num_points>0) {
    int key = get_key_from_pen_width(pen_width);
    c->lines[key].push_back(vl::dvec2(points[0].x, -points[0].y));
    for(int i=1;i<num_points;i++){
      float x = points[i].x;
      float y = c->y(points[i].y);
      c->lines[key].push_back(vl::dvec2(x,y));
      c->lines[key].push_back(vl::dvec2(x,y));
    }
    c->lines[key].push_back(vl::dvec2(points[0].x, c->y(points[0].y)));
  }
}
*/

void GE_trianglestrip(GE_Context *c, int num_points, const APoint *points){
  if(num_points>0){
#if USE_TRIANGLE_STRIPS
    std::vector<vl::dvec2> trianglestrip;
    for(int i=0;i<num_points;i++)
      trianglestrip.push_back(vl::dvec2(points[i].x, c->y(points[i].y)));
    c->trianglestrips.push_back(trianglestrip);
#else
    for(int i=0; i<num_points-2; i++){
      c->triangles.push_back(vl::dvec2(points[i].x, c->y(points[i].y)));
      c->triangles.push_back(vl::dvec2(points[i+1].x, c->y(points[i+1].y)));
      c->triangles.push_back(vl::dvec2(points[i+2].x, c->y(points[i+2].y)));
    }
#endif
  }
}

#if USE_TRIANGLE_STRIPS

static std::vector<vl::dvec2> trianglestrip;
void GE_trianglestrip_start(){
  trianglestrip.clear();
}
void GE_trianglestrip_add(GE_Context *c, float x, float y){
  trianglestrip.push_back(vl::dvec2(x,y));
}
void GE_trianglestrip_end(GE_Context *c){
  c->trianglestrips.push_back(trianglestrip);
}

#else //  USE_TRIANGLE_STRIPS

static int num_trianglestrips;

void GE_trianglestrip_start(void){
  //R_ASSERT(num_trianglestrips==0);
  num_trianglestrips = 0;
}

void GE_trianglestrip_add(GE_Context *new_c, float x, float y){
  static float y2,y1;
  static float x2,x1;

  static GE_Context *c = new_c;
    
  num_trianglestrips++;

  if(num_trianglestrips>=3){
    c->triangles.push_back(vl::dvec2(x, c->y(y)));
    c->triangles.push_back(vl::dvec2(x1, c->y(y1)));
    c->triangles.push_back(vl::dvec2(x2, c->y(y2)));
  }

  c = new_c;
  
  y2 = y1;  y1 = y;
  x2 = x1;  x1 = x;
}

/*
void GE_trianglestrip_add_line(GE_Context *c, float x1, float y1, float x2, float y2, float pen_width){
  GE_line_lowlevel(c, c->triangles, x1, y1, x2, y2, pen_width);
}
*/

void GE_trianglestrip_end(GE_Context *c){
}




static int num_gradient_triangles;
static vl::ref<GradientTriangles> current_gradient_rectangle;
static float triangles_min_y;
static float triangles_max_y;
static GradientType::Type current_gradient_type;

void GE_gradient_triangle_start(GradientType::Type type){
  current_gradient_rectangle = T1_get_gradient_triangles(type);
  current_gradient_type = type;
  num_gradient_triangles = 0;
}

void GE_gradient_triangle_add(GE_Context *c, float x, float y){
  static float y2,y1;
  static float x2,x1;

  if (current_gradient_rectangle.get()==NULL){ // Happens when we are too buzy.
    current_gradient_rectangle = T1_get_gradient_triangles(current_gradient_type); // try again.
    if (current_gradient_rectangle.get()==NULL){

      // We could have slept for the duration of a frame here, and tried again, but that would probably have been worse than using a non-gradient triangle.
      
      GE_trianglestrip_start(); // fallback to non-gradient triangles
      GE_trianglestrip_add(c, x, y);
      return;
    }
  }
  
  if(num_gradient_triangles==0){
    triangles_min_y = triangles_max_y = y;
  }else{
    if (y<triangles_min_y)
      triangles_min_y = y;
    
    if (y>triangles_max_y)
      triangles_max_y = y;
  }
  
  num_gradient_triangles++;
  
  if(num_gradient_triangles>=3){
    current_gradient_rectangle->triangles.push_back(vl::dvec2(x, c->y(y)));
    current_gradient_rectangle->triangles.push_back(vl::dvec2(x1, c->y(y1)));
    current_gradient_rectangle->triangles.push_back(vl::dvec2(x2, c->y(y2)));
  }
  
  y2 = y1;  y1 = y;
  x2 = x1;  x1 = x;
}

void GE_gradient_triangle_end(GE_Context *c, float x1, float x2){
  if (current_gradient_rectangle.get()==NULL){
    GE_trianglestrip_end(c);
    return;
  }
  
  //printf("min_y: %f, max_y: %f. height: %f\n",triangles_min_y, triangles_max_y, triangles_max_y-triangles_min_y);
  current_gradient_rectangle->y = c->y(triangles_max_y);
  current_gradient_rectangle->height = triangles_max_y-triangles_min_y;

  current_gradient_rectangle->x = x1;
  current_gradient_rectangle->width = x2-x1;

  if (current_gradient_rectangle->type==GradientType::VELOCITY) {
    current_gradient_rectangle->color1 = get_vec4(c->color.c_gradient);
    current_gradient_rectangle->color2 = get_vec4(c->color.c);
  } else {
    current_gradient_rectangle->color1 = get_vec4(c->color.c);
    current_gradient_rectangle->color2 = get_vec4(c->color.c_gradient);  
  }

  c->gradient_triangles.push_back(current_gradient_rectangle);

  current_gradient_rectangle = NULL;
}


#endif //  !USE_TRIANGLE_STRIPS
