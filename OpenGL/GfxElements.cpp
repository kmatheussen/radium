
#include <stdint.h>

#include <map>
#include <vector>

#include <QMap>
#include <QMutex>
#include <QMutexLocker>

#include <vlVG/VectorGraphics.hpp>
#include <vlGraphics/Rendering.hpp>
#include <vlVG/SceneManagerVectorGraphics.hpp>

#include "TextBitmaps.hpp"

#include "../common/nsmtracker.h"

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


struct _GE_Context : public vl::Object{
  std::map< int, std::vector<vl::dvec2> > lines; // lines, boxes and polylines
  std::vector< vl::dvec2 > boxes; // filled boxes

#if USE_TRIANGLE_STRIPS
  std::vector< std::vector<vl::dvec2> > trianglestrips; // Used instead of polygons for displaying waveform data. Trianglestrips are directly supported by opengl, so it's faster plus that conversions from polygons to triangles are unnecessary.
#else
  std::vector<vl::dvec2> triangles; // filled triangles. Used instead of trianglestrips. Just paint all triangles one by one. GPU usage seems approxiately the same as tringle strips, but CPU usage is significantly lower.
#endif

  TextBitmaps textbitmaps;

  union Color{
    struct{
      GE_Rgb c;
      GE_Rgb c_gradient;
    };
    uint64_t key;
  } color;

  int _z;

  vl::ref<vl::Image> gradient;

  _GE_Context(const Color _color, int z)
    : color(_color)
    , _z(z)
    , gradient(NULL)
  {
    assert(sizeof(Color)==sizeof(uint64_t));
  }

#if 0
  ~_GE_Context(){
    printf("Deleting context with z %d\n",_z);
  }
#endif

  float y(float y){
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
  if(c->gradient.get() != NULL) {
    vg->setImage(c->gradient.get());
    vg->setColor(vl::white);
  } else
    vg->setColor(vl::fvec4(c->color.c.r/255.0f, c->color.c.g/255.0f, c->color.c.b/255.0f, c->color.c.a/255.0f));
}

static void setColorEnd(vl::ref<vl::VectorGraphics> vg, vl::ref<GE_Context> c){
  if(c->gradient.get() != NULL)
    vg->setImage(NULL);
}


void GE_draw_vl(PaintingData *painting_data, vl::Viewport *viewport, vl::ref<vl::VectorGraphics> vg, vl::ref<vl::Transform> scroll_transform, vl::ref<vl::Transform> static_x_transform, vl::ref<vl::Transform> scrollbar_transform){
  vg->setLineSmoothing(true);
  vg->setPolygonSmoothing(true);
  //vg->setPointSmoothing(true); /* default value */
  vg->setPointSmoothing(false); // images are drawn using drawPoint.
  //vg->setTextureMode(vl::TextureMode_Repeat 	);

  GE_Rgb new_background_color;

  {
    QMutexLocker locker(&mutex);
    new_background_color = background_color;
  }

  viewport->setClearColor(vl::fvec4(new_background_color.r/255.0f, new_background_color.g/255.0f, new_background_color.b/255.0f, new_background_color.a/255.0f));

  vg->startDrawing(); {

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
        
        if(c->textbitmaps.points.size()>0){
          setColorBegin(vg, c);
          c->textbitmaps.drawAllCharBoxes(vg.get(), c->get_transform(scroll_transform, static_x_transform, scrollbar_transform));
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
#else
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
  rgb.a=200;
  return GE_z(rgb, z);
}

GE_Context *GE_rgba_color_z(unsigned char r, unsigned char g, unsigned char b, unsigned char a, int z){
#if 0
  // Reduce number of contexts. May also reduce cpu usage significantly.
  r |= 7;
  g |= 7;
  b |= 7;
  a |= 7;
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

  c->gradient = vl::makeColorSpectrum(128,
                                      vl::fvec4(c->color.c.r/255.0f, c->color.c.g/255.0f, c->color.c.b/255.0f, c->color.c.a/255.0f),
                                      vl::fvec4(c->color.c_gradient.r/255.0f, c->color.c_gradient.g/255.0f, c->color.c_gradient.b/255.0f, c->color.c_gradient.a/255.0f)
                                      );

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

void GE_line(GE_Context *c, float x1, float y1, float x2, float y2, float pen_width){
  int key = get_key_from_pen_width(pen_width);

  c->lines[key].push_back(vl::dvec2(x1,c->y(y1)));
  c->lines[key].push_back(vl::dvec2(x2,c->y(y2)));
}

void GE_text(GE_Context *c, const char *text, float x, float y){
  c->textbitmaps.addCharBoxes(text, x, c->y(y));
}

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
void GE_trianglestrip_add(float x, float y){
  trianglestrip.push_back(vl::dvec2(x,y));
}
void GE_trianglestrip_end(GE_Context *c){
  c->trianglestrips.push_back(trianglestrip);
}

#else //  USE_TRIANGLE_STRIPS

static float num_trianglestrips;

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

#endif //  !USE_TRIANGLE_STRIPS
