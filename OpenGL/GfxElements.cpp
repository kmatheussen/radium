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

double g_opengl_scale_ratio = 1.0;

#include "../common/nsmtracker.h"
#include "../common/OS_settings_proc.h"
#include "../common/Mutex.hpp"
#include "../common/QueueStack.hpp"

//#include "../Qt/Qt_colors_proc.h"

#define OPENGL_GFXELEMENTS_CPP
#define GE_DRAW_VL
#include "GfxElements.h"

#include "TextBitmaps.hpp"

#include "T2.hpp"

#include "Context.hpp"


#define DEBUG_PRINT 0

#if defined(RELEASE) && DEBUG_PRINT==1
#error "oops"
#endif


#define NUM_PREDEFINED_COLORS 16


static float g_height = 512; // Only access from main thread

static float g_height_t2_thread = 512; // Only access from t2 thread

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

r::Context *g_context = new r::Context();

struct _GE_Context
{
  union Color
  {
    struct
	{
		GE_Rgb c;
		GE_Rgb c_gradient;
    };
	  uint64_t key;
  } color;

	GE_Conf _conf;
	int _slice;
  
public:
	
	//std::vector<r::fvec2> _triangles;
	
	_GE_Context(const Color &_color, const GE_Conf &conf, int slice)
		: color(_color)
		, _conf(conf)
		, _slice(slice)
	{
		R_ASSERT(sizeof(Color)==sizeof(uint64_t));
	}

	void add_triangle(const r::fvec2 &p1, const r::fvec2 &p2, const r::fvec2 &p3, GradientType::Type gradient_type = GradientType::Type::NOTYPE)
	{
		if (g_rhi != NULL)
		{
			switch(gradient_type)
			{
				case GradientType::Type::NOTYPE:
					g_context->add_triangle(p1, p2, p3, color.c);
					break;
				case GradientType::Type::HORIZONTAL:
					g_context->add_triangle(p1, p2, p3, color.c, color.c_gradient);
					break;
				case GradientType::Type::VELOCITY:
					g_context->add_triangle(p1, p2, p3, GE_rgb(200,80,80), /*color.c_gradient, */ color.c);
					break;
			}
		}
	}

	static float y(float y)
	{
#if 0 // TODO: Find out if we need to invert y or not.
		float height = g_height;
		float ret = scale(y,
						  0, height,
						  height, 0);
		return ret;
#else
		return y;
#endif
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

/*
typedef QMap<int,                        // <- z
             QHash<int,                  // <- block slice (from calculated from y)
                   QHash<enum UseScissors,            // <- whether to use scissor. 0: no scissor, 1: scissor (tried to use array instead of hash, but the code became too complicated)
                         QHash<uint64_t, // <- color
                               vl::ref<GE_Context>
                               >
                         >
                   >
             > Contexts;
*/

struct Contexts
{
	int something;
};

namespace r
{
// Contains all data necessary to paint the editor.
// Created in the main thread, and then transfered to the OpenGL thread.
struct PaintingData
{
	//Contexts contexts;
	std::vector<GE_Context*> _contexts;
	
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

	~PaintingData()
	{
		for(GE_Context* context : _contexts)
			delete context;
	}
};
} // namespace r

void GE_delete_painting_data(r::PaintingData *painting_data)
{
  delete painting_data;
}



// This variable is only accessed by the main thread while building up a new r::PaintingData.
// It is not necessary for this variable to be global, and the code is more confusing because of that.
// However, by letting it be global, we don't have to send it around everywhere.
//
// In short: It can only be used by the main thread while while GL_create is called.
static r::PaintingData *g_painting_data = NULL;

// Called from the OpenGL thread
const SharedVariables *GE_get_shared_variables(const r::PaintingData *painting_data){
  return &painting_data->shared_variables;
}

int GE_get_slice_size(const r::PaintingData *painting_data){
  return painting_data->slice_size;
}

// Called from the main thread
void GE_start_writing(int full_height, bool block_is_visible){
  R_ASSERT(g_painting_data==NULL);

  T1_ensure_t2_is_initialized();
  
  g_painting_data = new r::PaintingData(full_height, block_is_visible);
  GE_fill_in_shared_variables(&g_painting_data->shared_variables, g_height);

  g_context->clear();
  
  //T1_prepare_gradients2_for_new_rendering();
  //T1_collect_gradients1_garbage();
}

// Called from the main thread
void GE_end_writing(GE_Rgb new_background_color){

	if (g_rhi != NULL)
	{
		g_context->call_me_when_finished_painting(g_rhi);
	}
	
	T1_send_data_to_t2(g_painting_data, new_background_color);
    
	g_painting_data = NULL;
}

// Called from the main thread. Only used when loading song to ensure all gradients are created before starting to play.
void GE_wait_until_block_is_rendered(void){
  T1_wait_until_t2_got_t1_data();
  // T1_wait_until_t3_got_t2_data(); // A bit inconvenient to make that function since the t2_to_t3 queue is not threadsafe. Instead we let T1_wait_until_all_gradients_are_created wait at least two periods.
  //T1_wait_until_all_gradients_are_created();
}



/*****************************************/
/* Drawing.  Called from OpenGL thread. */
/****************************************/


// This function can probably be avoided somehow. The absolute y position of the vertexes should be available for the shader GLSL code, but I haven't
// figured out how to get it yet.
//
// OpenGL Thread
void GE_update_triangle_gradient_shaders(r::PaintingData *painting_data, float y_offset)
{
}

// T2 thread.
void GE_draw_vl(T2_data *t2_data){
  //GL_draw_lock();

  r::PaintingData *painting_data = t2_data->painting_data;

  g_height_t2_thread = painting_data->shared_variables.opengl_widget_height;
	  
}






/*************************************************/
/* Creating painting_data.  Called from main thread. */
/***********************************************/

static int get_slice_from_y(const int y){
  return y/g_painting_data->slice_size;
}

static GE_Context *get_context(const GE_Context::Color &color, const GE_Conf &conf){
  int slice = get_slice_from_y(conf.y);

  //if(g_painting_data->contexts[conf.z][slice][conf.use_scissors].contains(color.key))
  // return g_painting_data->contexts[conf.z][slice][conf.use_scissors][color.key].get();

  GE_Context *c = new GE_Context(color, conf, slice);

  //g_painting_data->contexts[conf.z][slice][conf.use_scissors][color.key] = c;
  g_painting_data->_contexts.push_back(c);
  
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

static void GE_line_lowlevel(GE_Context *c, float x1, float y1, float x2, float y2, float pen_width){
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
  perp_x *= h * 0.5f;
  perp_y *= h * 0.5f;
 
  float v1x = x1 + perp_x;
  float v1y = y1 + perp_y;
  
  float v2x = x2 + perp_x;
  float v2y = y2 + perp_y;
 
  float v3x = x2 - perp_x;
  float v3y = y2 - perp_y;
 
  float v4x = x1 - perp_x;
  float v4y = y1 - perp_y;

  c->add_triangle({v1x, c->y(v1y)},
				  {v2x, c->y(v2y)},
				  {v3x, c->y(v3y)});
/*
  triangles.push_back(r::fvec2(v1x, c->y(v1y)));
  triangles.push_back(r::fvec2(v2x, c->y(v2y)));
  triangles.push_back(r::fvec2(v3x, c->y(v3y)));
*/
  c->add_triangle({v1x, c->y(v1y)},
				  {v3x, c->y(v3y)},
				  {v4x, c->y(v4y)});

  /*
  triangles.push_back(r::fvec2(v1x, c->y(v1y)));
  triangles.push_back(r::fvec2(v3x, c->y(v3y)));
  triangles.push_back(r::fvec2(v4x, c->y(v4y)));
  */
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


  GE_line_lowlevel(c, x1, y1, x2, y2, pen_width);
}

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

void GE_box(GE_Context *c, float x1, float y1, float x2, float y2, float pen_width){
  GE_line(c, x1, y1, x2, y1, pen_width);
  GE_line(c, x2, y1, x2, y2, pen_width);
  GE_line(c, x2, y2, x1, y2, pen_width);
  GE_line(c, x1, y2, x1, y1, pen_width);
}

void GE_filledBox(GE_Context *c, float x1, float y1, float x2, float y2){

	c->add_triangle({x1, c->y(y1)},
					{x2, c->y(y1)},
					{x1, c->y(y2)});
	
	c->add_triangle({x1, c->y(y2)},
					{x2, c->y(y1)},
					{x2, c->y(y2)});
	
	/*
	  c->boxes.push_back(vl::dvec2(x1,c->y(y1)));
	  c->boxes.push_back(vl::dvec2(x1,c->y(y2)));
	  c->boxes.push_back(vl::dvec2(x2,c->y(y2)));
	  c->boxes.push_back(vl::dvec2(x2,c->y(y1)));
	*/
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
    for(int i=0; i<num_points-2; i++)
	{
		c->add_triangle({points[i].x, c->y(points[i].y)},
						{points[i+1].x, c->y(points[i+1].y)},
						{points[i+2].x, c->y(points[i+2].y)});

		/*
		c->_triangles.push_back(r::fvec2(points[i].x, c->y(points[i].y)));
		c->_triangles.push_back(r::fvec2(points[i+1].x, c->y(points[i+1].y)));
		c->_triangles.push_back(r::fvec2(points[i+2].x, c->y(points[i+2].y)));
		*/
    }
  }
}

static int num_trianglestrips;

void GE_trianglestrip_start(void){
  //R_ASSERT(num_trianglestrips==0);
  num_trianglestrips = 0;
}

void GE_trianglestrip_add(GE_Context *c, float x, float y){
  static float y2,y1;
  static float x2,x1;

  num_trianglestrips++;

  if(num_trianglestrips>=3)
  {
	  c->add_triangle({x, c->y(y)},
					  {x1, c->y(y1)},
					  {x2, c->y(y2)});
  }

  y2 = y1;  y1 = y;
  x2 = x1;  x1 = x;
}

/*
void GE_trianglestrip_add_line(GE_Context *c, float x1, float y1, float x2, float y2, float pen_width){
  GE_line_lowlevel(c, c->_triangles, x1, y1, x2, y2, pen_width);
}
*/

void GE_trianglestrip_end(GE_Context *c){
}



static int num_gradient_triangles;
static QVector<r::fvec2> current_gradient_rectangle;
static float triangles_min_y;
static float triangles_max_y;
static GradientType::Type current_gradient_type;

void GE_gradient_triangle_start(GradientType::Type type)
{
	current_gradient_rectangle.clear();
	current_gradient_type = type;
	num_gradient_triangles = 0;
}

void GE_gradient_triangle_add(GE_Context *c, float x, float y){
	static float y2,y1;
	static float x2,x1;
	
	if(num_gradient_triangles==0)
	{
		triangles_min_y = triangles_max_y = y;
	}
	else
	{
		if (y<triangles_min_y)
			triangles_min_y = y;
		
		if (y>triangles_max_y)
			triangles_max_y = y;
	}
	
	num_gradient_triangles++;
	
	if(num_gradient_triangles>=3)
	{
		c->add_triangle(r::fvec2(x, c->y(y)),
						r::fvec2(x1, c->y(y1)),
						r::fvec2(x2, c->y(y2)),
						current_gradient_type);
	}
	
	y2 = y1;  y1 = y;
	x2 = x1;  x1 = x;
}

void GE_gradient_triangle_end(GE_Context *c, float x1, float x2){
	// TODO: Fix correct gradiation. It's supposed to go from x1->x2. Now, it' just hacked and quite random.
}
