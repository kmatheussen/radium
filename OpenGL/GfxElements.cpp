#include <assert.h>
#include <stdint.h>

#include <map>
#include <vector>

#include <QMap>
#include <QMutex>
#include <QMutexLocker>

#include <vlVG/VectorGraphics.hpp>
#include <vlGraphics/Rendering.hpp>
#include <vlVG/SceneManagerVectorGraphics.hpp>
#include <vlGraphics/Effect.hpp>
#include <vlGraphics/GLSL.hpp>

#include "TextBitmaps.hpp"

#include "../common/nsmtracker.h"
#include "../common/OS_settings_proc.h"

#include "../Qt/Qt_colors_proc.h"

#define OPENGL_GFXELEMENTS_CPP
#include "GfxElements.h"


#define USE_TRIANGLE_STRIPS 0
#define NUM_PREDEFINED_COLORS 16




static volatile float g_height = 512;

void GE_set_height(int height){
  g_height = height;
}

int GE_get_height(void){
  //return root->song->tracker_windows->wblock->t.y2 - root->song->tracker_windows->wblock->t.y1;
  return g_height;
}

int GE_get_tracker_height(){
  return root->song->tracker_windows->wblock->t.y2 - root->song->tracker_windows->wblock->t.y1;
}

extern struct Root *root;

GE_Rgb GE_get_rgb(int colornum){
  const QColor c = get_qcolor(root->song->tracker_windows, colornum);
  //GE_Rgb ret = {50,60,20,255};
  GE_Rgb ret = {(unsigned char)c.red(), (unsigned char)c.green(), (unsigned char)c.blue(), (unsigned char)c.alpha()};
  return ret;
}


static vl::vec4 get_vec4(GE_Rgb rgb){
  return vl::vec4(rgb.r/255.0f, rgb.g/255.0f, rgb.b/255.0f, rgb.a/255.0f);
}


static vl::GLSLFragmentShader *get_gradient_fragment_shader(GradientType::Type type){
  static vl::ref<vl::GLSLFragmentShader> gradient_velocity_shader = NULL;
  static vl::ref<vl::GLSLFragmentShader> gradient_horizontal_shader = NULL;

  if(gradient_velocity_shader.get()==NULL) {
    vl::String path1 =
      vl::String(OS_get_program_path2())
      .append(vl::String(OS_get_directory_separator()))
      .append(vl::String("glsl"))
      .append(vl::String(OS_get_directory_separator()))
      ;

    vl::String path2 = path1;
    
    gradient_velocity_shader = new vl::GLSLFragmentShader(path1.append("gradient.fs"));
    gradient_horizontal_shader = new vl::GLSLFragmentShader(path2.append("horizontal_gradient.fs"));

    R_ASSERT(gradient_velocity_shader.get()!=NULL);
    R_ASSERT(gradient_horizontal_shader.get()!=NULL);
  }

  if (type==GradientType::VELOCITY)
    return gradient_velocity_shader.get();
  else if (type==GradientType::HORIZONTAL)
    return gradient_horizontal_shader.get();
  else
    RError("Unknown gradient type %d", type);
   
  return NULL;
}

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
  GradientTriangles *next;
  
  std::vector<vl::dvec2> triangles;

  GradientType::Type type;
  
  float y, height;
  float x, width;
  
  vl::fvec4 color1;
  vl::fvec4 color2;
   
  vl::ref<vl::GLSLProgram> glsl; // seems like reference must be stored here to avoid memory problems.
  vl::Uniform *uniform_y;

  GradientTriangles(GradientType::Type type)
    : next(NULL)
    , type(type)
  {
    setAutomaticDelete(false);
  }

  ~GradientTriangles(){
    RError("~GradientTriangles was called. That should not happen. Expect crash.");
    //abort();
  }
  
  vl::Actor *render(vl::VectorGraphics *vg){

    vl::Actor *actor = vg->fillTriangles(triangles);

    actor->setEffect(this);

    if (glsl.get()==NULL) {
      vl::Shader* shader = this->shader();
      shader->enable(vl::EN_BLEND);
        
      glsl = shader->gocGLSLProgram();    
      //glsl->attachShader(get_gradient_vertex_shader()); 
      glsl->attachShader(get_gradient_fragment_shader(type)); 

      if (type==GradientType::VELOCITY)
        uniform_y = glsl->gocUniform("y");
    }


    glsl->gocUniform("color1")->setUniform(color1);
    glsl->gocUniform("color2")->setUniform(color2);
    if (type==GradientType::VELOCITY)
      glsl->gocUniform("height")->setUniformF(height);
    glsl->gocUniform("x")->setUniformF(x);
    glsl->gocUniform("width")->setUniformF(width);

    if (type==GradientType::VELOCITY)
      set_y_offset(0.0f);
    
    return actor;
  }

  // called from main thread
  void clean(){
    triangles.clear();
  }
  
  void set_y_offset(float y_offset){
    if (type==GradientType::VELOCITY)
      uniform_y->setUniformF(y + y_offset);
  }
};


struct GradientTrianglesCollection {

  GradientType::Type type;

  // These two are only accessed by the main thread
  GradientTriangles *used_gradient_triangles;
  GradientTriangles *free_gradient_triangles;

  GradientTrianglesCollection(GradientType::Type type)
    : type(type)
    , used_gradient_triangles(NULL)
    , free_gradient_triangles(NULL)
  {}
  
  // main thread
  void collect_gradient_triangles_garbage(void){
    GradientTriangles *new_used = NULL;
    GradientTriangles *new_free = NULL;
    
    R_ASSERT(free_gradient_triangles == NULL);
    
    GradientTriangles *gradient = used_gradient_triangles;
    
    while(gradient!=NULL){
      GradientTriangles *next = gradient->next;
      
      if(gradient->referenceCount()==0) {
        gradient->clean();
        gradient->next = new_free;
        new_free = gradient;
      } else {
        gradient->next = new_used;
        new_used = gradient;
      }
      
      gradient = next;
    }
    
    used_gradient_triangles = new_used;
    free_gradient_triangles = new_free;
  }


  // main thread
  void add_gradient_triangles(void){
    for(int i=0;i<15;i++){
      GradientTriangles *gradient = new GradientTriangles(type);
      gradient->next = free_gradient_triangles;
      free_gradient_triangles = gradient;
    }
  }


  // main thread
  GradientTriangles *get_gradient_triangles(void){  
    if (free_gradient_triangles==NULL)
      collect_gradient_triangles_garbage();
    
    if (free_gradient_triangles==NULL) {
      printf("1111. Allocating new gradient triangles\n");
      add_gradient_triangles();
    } else {
      //printf("2222. Using recycled gradient triangles\n");
    }
    
    // pop free
    GradientTriangles *gradient = free_gradient_triangles;
    free_gradient_triangles = gradient->next;
    
    // push used
    gradient->next = used_gradient_triangles;
    used_gradient_triangles = gradient;
    
#if 0
    if(free_gradient_triangles!=NULL)
      assert(free_gradient_triangles != used_gradient_triangles);
    
    if (free_gradient_triangles!=NULL)
      assert(free_gradient_triangles->next != free_gradient_triangles);
    
    assert(used_gradient_triangles->next != used_gradient_triangles);
    
    if(free_gradient_triangles!=NULL)
      assert(free_gradient_triangles != used_gradient_triangles);
#endif
  
    return gradient;
  }

};
   
static GradientTrianglesCollection horizontalGradientTriangles(GradientType::HORIZONTAL);
static GradientTrianglesCollection velocityGradientTriangles(GradientType::VELOCITY);

static GradientTriangles *get_gradient_triangles(GradientType::Type type){
  if (type==GradientType::HORIZONTAL)
    return horizontalGradientTriangles.get_gradient_triangles();
  else if (type==GradientType::VELOCITY)
    return velocityGradientTriangles.get_gradient_triangles();

  RError("Unknown gradient type %d",type);
  return NULL;
}
    

struct _GE_Context : public vl::Object{
  std::map< int, std::vector<vl::dvec2> > lines; // lines, boxes and polylines
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

  int _z;

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

  _GE_Context(const Color _color, int z)
    : textbitmaps(false)
    , textbitmaps_halfsize(true)
      //, has_scissor(false)
    , color(_color)
    , _z(z)
      //, gradient(NULL)
      //, is_gradient(false)
  {
    R_ASSERT(sizeof(Color)==sizeof(uint64_t));
  }

#if 0
  ~_GE_Context(){
    printf("Deleting context with z %d\n",_z);
  }
#endif

  static float y(float y){
    return scale(y,0,g_height,
                 g_height,0);
  }

  vl::Transform *get_transform(vl::ref<vl::Transform> scroll_transform, vl::ref<vl::Transform> static_x_transform, vl::ref<vl::Transform> scrollbar_transform){
    if (Z_IS_STATIC_X(_z))
      return static_x_transform.get();
    else if (_z <= Z_MAX_SCROLLTRANSFORM)
      return scroll_transform.get();
    else if (_z < Z_MIN_STATIC)
      return scrollbar_transform.get();
    else
      return NULL;
  }

};

    
void GE_set_z(GE_Context *c, int new_z) {
  c->_z = new_z;
}

int GE_get_z(GE_Context *c){
  return c->_z;
}

/* Drawing */

static void setScrollTransform(vl::ref<GE_Context> c, vl::Actor *actor, vl::ref<vl::Transform> scroll_transform, vl::ref<vl::Transform> static_x_transform, vl::ref<vl::Transform> scrollbar_transform){
  vl::Transform *transform = c->get_transform(scroll_transform, static_x_transform, scrollbar_transform);
  if (transform != NULL)
    actor->setTransform(transform);
}


static int get_key_from_pen_width(float pen_width){
  int ret = pen_width * 10;
  if (ret==0)
    ret = 1;
  return ret;
}

static float get_pen_width_from_key(int key){
  return (float)key/10.0;
}


static QMutex mutex;

static GE_Rgb background_color;

typedef QMap<int, std::map<uint64_t, vl::ref<GE_Context> > > Contexts;
//                         ^             ^
//                         |             |
//                         z             color


// Contains all data necessary to paint the editor.
// Created in the main thread, and then transfered to the OpenGL thread.
struct PaintingData{
  Contexts contexts;
  std::vector< vl::ref<GradientTriangles> > gradient_triangles;
  SharedVariables shared_variables;
};




// Accessed by both the main thread and the OpenGL thread. Access to this variable is protected by the mutex.
static PaintingData *g_last_written_painting_data = NULL;


// This variable is only accessed by the main thread while building up a new PaintingData.
// It is not necessary for this variable to be global, and the code is more confusing because of that.
// However, by letting it be global, we don't have to send it around everywhere.
static PaintingData *g_painting_data = NULL;

// Called from the OpenGL thread
SharedVariables *GE_get_shared_variables(PaintingData *painting_data){
  return &painting_data->shared_variables;
}

// Called from the OpenGL thread
PaintingData *GE_get_painting_data(PaintingData *current_painting_data, bool *needs_repaint){
  PaintingData *ret;

  {
    QMutexLocker locker(&mutex);
  
    if(g_last_written_painting_data==NULL) {
      *needs_repaint = false;
      return current_painting_data;
    } else {
      *needs_repaint = true;
      ret = g_last_written_painting_data;
      g_last_written_painting_data = NULL;
    }
  }

  if(current_painting_data != NULL)
    delete current_painting_data;
  
  return ret;
}


// Called from the main thread
void GE_start_writing(void){
  g_painting_data = new PaintingData();
  GE_fill_in_shared_variables(&g_painting_data->shared_variables);
}

// Called from the main thread
void GE_end_writing(GE_Rgb new_background_color){
  QMutexLocker locker(&mutex);

  if (g_last_written_painting_data != NULL) // Unnecessary, but the code is clearer this way. It shows that the variable might be NULL.
    delete g_last_written_painting_data;

  g_last_written_painting_data = g_painting_data;

  background_color = new_background_color;
}



/*****************************************/
/* Drawing.  Called from OpenGL thread. */
/****************************************/


static void setColorBegin(vl::ref<vl::VectorGraphics> vg, vl::ref<GE_Context> c){
#if 0
  if(false && c->is_gradient){
    vg->setImage(c->get_gradient().get());
    vg->setColor(vl::white);
  } else
#endif
    vg->setColor(get_vec4(c->color.c));
}

static void setColorEnd(vl::ref<vl::VectorGraphics> vg, vl::ref<GE_Context> c){
#if 0
  if(c->is_gradient)
    vg->setImage(NULL);
#endif
}


// This function can probably be avoided somehow. The absolute y position of the vertexes should be available for the shader GLSL code, but I haven't
// figured out how to get it yet.
void GE_update_triangle_gradient_shaders(PaintingData *painting_data, float y_offset){
  for (Contexts::Iterator it = painting_data->contexts.begin(); it != painting_data->contexts.end(); ++it) {
    std::map<uint64_t, vl::ref<GE_Context> > contexts = it.value();
      
    for(std::map<uint64_t, vl::ref<GE_Context> >::iterator iterator = contexts.begin(); iterator != contexts.end(); ++iterator) {      
      vl::ref<GE_Context> c = iterator->second;
      
      for (std::vector< vl::ref<GradientTriangles> >::iterator it = c->gradient_triangles.begin(); it != c->gradient_triangles.end(); ++it) {
        vl::ref<GradientTriangles> gradient_triangles = *it;
        gradient_triangles->set_y_offset(y_offset);
      }
    }
  }
}


void GE_draw_vl(PaintingData *painting_data, vl::Viewport *viewport, vl::ref<vl::VectorGraphics> vg, vl::ref<vl::Transform> scroll_transform, vl::ref<vl::Transform> static_x_transform, vl::ref<vl::Transform> scrollbar_transform){

  GE_Rgb new_background_color;

  {
    QMutexLocker locker(&mutex);
    new_background_color = background_color;
  }

  viewport->setClearColor(get_vec4(new_background_color));

  vg->startDrawing(); {

#if defined(RADIUM_DRAW_FONTS_DIRECTLY)
    vg->setFont("font/Cousine-Bold.ttf", 10, false);
#endif

    vg->setLineSmoothing(true);
    vg->setPolygonSmoothing(true);
    //vg->setPointSmoothing(true); /* default value */
    //vg->setPointSmoothing(root->editonoff); /* default value */
    vg->setPointSmoothing(false); // images are drawn using drawPoint.
    //vg->setTextureMode(vl::TextureMode_Repeat 	); // Note: MAY FIX gradient triangle non-overlaps.

    for (Contexts::Iterator it = painting_data->contexts.begin(); it != painting_data->contexts.end(); ++it) {

      //int z = it.key();
      std::map<uint64_t, vl::ref<GE_Context> > contexts = it.value();
      

      // 1. Filled boxes
      for(std::map<uint64_t, vl::ref<GE_Context> >::iterator iterator = contexts.begin(); iterator != contexts.end(); ++iterator) {
        
        vl::ref<GE_Context> c = iterator->second;
        
        if(c->boxes.size() > 0) {
          setColorBegin(vg, c);
          
          setScrollTransform(c, vg->fillQuads(c->boxes), scroll_transform, static_x_transform, scrollbar_transform);
          
          setColorEnd(vg, c);
        }
      }
      
      // 2. Text
      for(std::map<uint64_t, vl::ref<GE_Context> >::iterator iterator = contexts.begin(); iterator != contexts.end(); ++iterator) {
        
        vl::ref<GE_Context> c = iterator->second;

        if(c->textbitmaps.points.size() != 0 || c->textbitmaps_halfsize.points.size() != 0) {
           
          setColorBegin(vg, c);

          if(c->textbitmaps.points.size() > 0)
            c->textbitmaps.drawAllCharBoxes(vg.get(), c->get_transform(scroll_transform, static_x_transform, scrollbar_transform));
        
          if(c->textbitmaps_halfsize.points.size() > 0)
            c->textbitmaps_halfsize.drawAllCharBoxes(vg.get(), c->get_transform(scroll_transform, static_x_transform, scrollbar_transform));

          setColorEnd(vg, c);
        }
      }

      // 2. triangle strips
      for(std::map<uint64_t, vl::ref<GE_Context> >::iterator iterator = contexts.begin(); iterator != contexts.end(); ++iterator) {

        vl::ref<GE_Context> c = iterator->second;
        
#if USE_TRIANGLE_STRIPS
        if(c->trianglestrips.size() > 0) {
          setColorBegin(vg, c);
          setScrollTransform(c, vg->fillTriangleStrips(c->trianglestrips), scroll_transform, static_x_transform, scrollbar_transform);
          //vg->fillPolygons(c->trianglestrips);
          
          setColorEnd(vg, c);
        }
        // note: missing gradient triangles for USE_TRIANGLE_STRIPS.
#else
        for (std::vector< vl::ref<GradientTriangles> >::iterator it = c->gradient_triangles.begin(); it != c->gradient_triangles.end(); ++it) {
          vl::ref<GradientTriangles> gradient_triangles = *it;
          setScrollTransform(c, gradient_triangles->render(vg.get()), scroll_transform, static_x_transform, scrollbar_transform);
        }

        if(c->triangles.size() > 0) {
          setColorBegin(vg, c);
          
          setScrollTransform(c, vg->fillTriangles(c->triangles), scroll_transform, static_x_transform, scrollbar_transform);
          //printf("triangles size: %d\n",(int)c->triangles.size());
          
          setColorEnd(vg, c);
        }

#endif
      }

      
      // 3. Polylines
      // 4. Boxes
      // 5. Lines
      for(std::map<uint64_t, vl::ref<GE_Context> >::iterator iterator = contexts.begin(); iterator != contexts.end(); ++iterator) {
        
        vl::ref<GE_Context> c = iterator->second;
        
        bool has_set_color = false;
        
        for(std::map< int, std::vector<vl::dvec2> >::iterator iterator = c->lines.begin(); iterator != c->lines.end(); ++iterator) {
          if(has_set_color==false)
            setColorBegin(vg, c);
          has_set_color=true;
          
          vg->setLineWidth(get_pen_width_from_key(iterator->first));
          setScrollTransform(c, vg->drawLines(iterator->second), scroll_transform, static_x_transform, scrollbar_transform);
          //if(c->triangles.size()>0)
          //  setScrollTransform(c, vg->drawLines(c->triangles), scroll_transform, static_x_transform, scrollbar_transform);
        }
        
        if(has_set_color==true)
          setColorEnd(vg, c);
      }

      //printf("************ z: %d, NUM contexts: %d\n",z, (int)g_contexts.size());

    }

    
  }vg->endDrawing();
}






/*************************************************/
/* Creating painting_data.  Called from main thread. */
/***********************************************/

static GE_Context *get_context(const GE_Context::Color color, int z){
  if(g_painting_data->contexts[z].count(color.key)>0)
    return g_painting_data->contexts[z][color.key].get();

  GE_Context *c = new GE_Context(color, z);

  g_painting_data->contexts[z][color.key] = c;
  return c;
}

GE_Context *GE_z(const GE_Rgb rgb, int z){
  GE_Context::Color color;

  color.key = 0;
  color.c = rgb;

  return get_context(color, z);
}

static GE_Rgb rgb_from_qcolor(const QColor &color){
  GE_Rgb rgb = {(unsigned char)color.red(), (unsigned char)color.green(), (unsigned char)color.blue(), (unsigned char)color.alpha()};
  return rgb;
}

GE_Context *GE_color_z(const QColor &color, int z){
  return GE_z(rgb_from_qcolor(color), z);
}

GE_Context *GE_color_z(int colornum, int z){
  //const QColor c = get_qcolor(window, colornum);
  return GE_z(GE_get_rgb(colornum), z);
}

GE_Context *GE_color_alpha_z(int colornum, float alpha, int z){
  GE_Rgb rgb = GE_get_rgb(colornum);
  rgb.a = alpha * 255;
  return GE_z(rgb, z);
}

GE_Context *GE_textcolor_z(int colornum, int z){
  GE_Rgb rgb = GE_get_rgb(colornum);
  rgb.a=230;
  return GE_z(rgb, z);
}

GE_Context *GE_rgba_color_z(unsigned char r, unsigned char g, unsigned char b, unsigned char a, int z){
#if 1
  // Reduce number of contexts. May also reduce cpu usage significantly.
  r |= 15;
  g |= 15;
  b |= 15;
  a |= 15;
#endif

  GE_Rgb rgb = {r,g,b,a};

  return GE_z(rgb, z);
}

GE_Context *GE_rgb_color_z(unsigned char r, unsigned char g, unsigned char b, int z){
  return GE_rgba_color_z(r,g,b,255, z);
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

GE_Context *GE_mix_color_z(const GE_Rgb c1, const GE_Rgb c2, float how_much, int z){
  return GE_z(GE_mix(c1, c2, how_much), z);
}

GE_Context *GE_gradient_z(const GE_Rgb c1, const GE_Rgb c2, int z){
  GE_Context::Color color;

  color.c=c1;
  color.c_gradient=c2;

  GE_Context *c = get_context(color, z);

  //c->is_gradient = true;

  return c;
}

GE_Context *GE_gradient_z(const QColor &c1, const QColor &c2, int z){
  return GE_gradient_z(rgb_from_qcolor(c1), rgb_from_qcolor(c2), z);
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

      if (y1 != y2)
        y1 = scale(scissor_x, x1, x2, y1, y2);
      
      x1 = scissor_x;

    } 

    if (x2 > scissor_x2) {

      if (y1 != y2)
        y2 = scale(scissor_x2, x1, x2, y1, y2);

      x2 = scissor_x2;

    }

    if (x1==x2 && y1==y2)
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


  // Code below mostly copied from http://www.softswit.ch/wiki/index.php?title=Draw_line_with_triangles

  float dx = x2-x1;
  float dy = y2-y1;
 
  float length = sqrt( dx*dx + dy*dy );   
 
  // perp
  float perp_x = -dy;
  float perp_y = dx;
  if ( length ){
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

  c->triangles.push_back(vl::dvec2(v1x, c->y(v1y)));
  c->triangles.push_back(vl::dvec2(v2x, c->y(v2y)));
  c->triangles.push_back(vl::dvec2(v3x, c->y(v3y)));

  c->triangles.push_back(vl::dvec2(v1x, c->y(v1y)));
  c->triangles.push_back(vl::dvec2(v3x, c->y(v3y)));
  c->triangles.push_back(vl::dvec2(v4x, c->y(v4y)));
}
#endif

void GE_text(GE_Context *c, const char *text, int x, int y){
  c->textbitmaps.addCharBoxes(text, x, c->y(y+1));
}

void GE_text_halfsize(GE_Context *c, const char *text, int x, int y){
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

void GE_trianglestrip_start(){
  num_trianglestrips = 0;
}
void GE_trianglestrip_add(GE_Context *c, float x, float y){
  static float y2,y1;
  static float x2,x1;

  num_trianglestrips++;
  
  if(num_trianglestrips>=3){
    c->triangles.push_back(vl::dvec2(x, c->y(y)));
    c->triangles.push_back(vl::dvec2(x1, c->y(y1)));
    c->triangles.push_back(vl::dvec2(x2, c->y(y2)));
  }
  
  y2 = y1;  y1 = y;
  x2 = x1;  x1 = x;
}
void GE_trianglestrip_end(GE_Context *c){
}




static int num_gradient_triangles;
static vl::ref<GradientTriangles> current_gradient_rectangle;
static float triangles_min_y;
static float triangles_max_y;

void GE_gradient_triangle_start(GradientType::Type type){
  current_gradient_rectangle = get_gradient_triangles(type);
  num_gradient_triangles = 0;
}

void GE_gradient_triangle_add(GE_Context *c, float x, float y){
  static float y2,y1;
  static float x2,x1;

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
