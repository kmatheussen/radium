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


#if defined(__GNUC__) && !defined(__clang__)
#  include "../Qt/Qt_precompiled.hpp"
#endif

#include <stdint.h>

#include <unistd.h>

#include <map>
#include <vector>

#include <QMap>
#include <QVarLengthArray> // Must be included before nsmtracker.h to avoid compilation error.

double g_opengl_scale_ratio = 1.0;

#include "../common/nsmtracker.h"
#include "../common/OS_settings_proc.h"
#include "../common/Mutex.hpp"
#include "../common/QueueStack.hpp"

//#include "../Qt/Qt_colors_proc.h"

#define OPENGL_GFXELEMENTS_CPP
#define GE_DRAW_VL
#include "GfxElements.h"

//#include "TextBitmaps.hpp"

//#include "T2.hpp"

#include "Vertices.hpp"


#define DEBUG_PRINT 0

#if defined(RELEASE) && DEBUG_PRINT==1
#error "oops"
#endif


static float g_height = 512; // Only access from main thread


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

void GE_set_z(GE_Context &c, int new_z)
{
  c._conf.z = new_z;
}

int GE_get_z(const GE_Context &c){
  return c._conf.z;
}

GE_Rgb GE_get_rgb(const GE_Context &c){
  return c.color.c;
}

// This variable is only accessed by the main thread while building up a new r::PaintingData.
// It is not necessary for this variable to be global, and the code is more confusing because of that.
// However, by letting it be global, we don't have to send it around everywhere.
//
// In short: It can only be used by the main thread while while GL_create is called.
static r::PaintingData *g_painting_data = NULL;

int g_main_thread_slice_size = 64;

// Called from the main thread
bool GE_start_writing(int full_height, bool block_is_visible)
{
  R_ASSERT(g_painting_data==NULL);

  if (!GL_call_me_before_starting_to_generate_vertices1())
	  return false;

  {
	  g_painting_data = new r::PaintingData(full_height, block_is_visible);
	  
	  g_main_thread_slice_size = g_painting_data->slice_size;
	  
	  GE_fill_in_shared_variables(&g_painting_data->shared_variables, g_height);
  }
  
  GL_call_me_before_starting_to_generate_vertices2();

  return true;
}


void GE_end_writing(GE_Rgb new_background_color)
{
	GL_set_new_painting_data(g_painting_data, new_background_color);
		
	g_painting_data = NULL;
}

// Called from the main thread. Only used when loading song to ensure all gradients are created before starting to play.
void GE_wait_until_block_is_rendered(void){
	//T1_wait_until_t2_got_t1_data();
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
//void GE_update_triangle_gradient_shaders(r::PaintingData *painting_data, float y_offset)
//{
//}






/*************************************************/
/* Creating painting_data.  Called from main thread. */
/***********************************************/

static GE_Context get_context(const GE_Context::Color &color, const GE_Conf &conf){
  //if(g_painting_data->contexts[conf.z][slice][conf.use_scissors].contains(color.key))
  // return g_painting_data->contexts[conf.z][slice][conf.use_scissors][color.key].get();

  GE_Context c(color, conf);

  //g_painting_data->contexts[conf.z][slice][conf.use_scissors][color.key] = c;
  //g_painting_data->_contexts.push_back(c);
  
  return c;
}

GE_Context GE_z(const GE_Rgb rgb, const GE_Conf &conf){
  GE_Context::Color color;

  //color.key = 0;
  color.c = rgb;

  return get_context(color, conf);
}

static GE_Rgb rgb_from_qcolor(const QColor &color){
  GE_Rgb rgb = {(unsigned char)color.red(), (unsigned char)color.green(), (unsigned char)color.blue(), (unsigned char)color.alpha()};
  return rgb;
}

GE_Context GE_color_z(const QColor &color, const GE_Conf &conf){
  return GE_z(rgb_from_qcolor(color), conf);
}

GE_Context GE_color_z(enum ColorNums colornum, const GE_Conf &conf){
  //const QColor c = get_qcolor(window, colornum);
  return GE_z(GE_get_rgb(colornum), conf);
}

GE_Context GE_color_alpha_z(enum ColorNums colornum, float alpha, const GE_Conf &conf){
  GE_Rgb rgb = GE_get_rgb(colornum);
  rgb.a = alpha * 255;
  return GE_z(rgb, conf);
}

GE_Context GE_textcolor_z(enum ColorNums colornum, const GE_Conf &conf){
  GE_Rgb rgb = GE_get_rgb(colornum);
  rgb.a=230;
  return GE_z(rgb, conf);
}

GE_Context GE_rgba_color_z(unsigned char r, unsigned char g, unsigned char b, unsigned char a, const GE_Conf &conf){
#if 0
  // Reduce number of contexts. May also reduce cpu usage significantly.
  r |= 15;
  g |= 15;
  b |= 15;
  a |= 15;
#endif

  GE_Rgb rgb = {r,g,b,a};

  return GE_z(rgb, conf);
}

GE_Context GE_rgb_color_z(unsigned char r, unsigned char g, unsigned char b, const GE_Conf &conf){
  return GE_rgba_color_z(r,g,b,255, conf);
}

GE_Rgb GE_mix(const GE_Rgb c1, const GE_Rgb c2, float how_much)
{
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

GE_Context GE_mix_color_z(const GE_Rgb c1, const GE_Rgb c2, float how_much, const GE_Conf &conf){
  return GE_z(GE_mix(c1, c2, how_much), conf);
}

GE_Context GE_gradient_z(const GE_Rgb c1, const GE_Rgb c2, const GE_Conf &conf){
  GE_Context::Color color;

  color.c=c1;
  color.c_gradient=c2;

  GE_Context c = get_context(color, conf);

  //c.is_gradient = true;

  return c;
}

GE_Context GE_gradient_z(const QColor &c1, const QColor &c2, const GE_Conf &conf){
  return GE_gradient_z(rgb_from_qcolor(c1), rgb_from_qcolor(c2), conf);
}



/************************************************************/
/* Scheduling drawing operations. Called from main thread. */
/**********************************************************/

#if 0
void GE_set_font(const QFont &font){
  GE_set_new_font(font);
}
#endif

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

static void GE_line_lowlevel(const GE_Context &c, float x1, float y1, float x2, float y2, float pen_width)
{
	// Nothing
	if (equal_floats(x1, x2) && equal_floats(y1, y2))
		return;


	const float h = pen_width * 0.5f;

	
	// Horizontal line
	//
	if (equal_floats(y1, y2))
	{
		c.add_triangle(r::Triangle({x1, y1 + h},
								   {x2, y2 + h},
								   {x2, y2 - h}));
		
		c.add_triangle(r::Triangle({x1, y1 + h},
								   {x2, y2 - h},
								   {x1, y1 - h}));
		return;
	}


	// Vertical line
	//
	if (equal_floats(x1, x2))
	{
		c.add_triangle(r::Triangle({x1 + h, y1},
								   {x2 + h, y2},
								   {x2 - h, y2}));
		
		c.add_triangle(r::Triangle({x1 + h, y1},
								   {x2 - h, y2},
								   {x1 - h, y1}));
		
		return;
	}


	// Code below mostly copied from http://www.softswit.ch/wiki/index.php?title=Draw_line_with_triangles
	// 

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
 
	// since perp defines how wide our quad is, scale it
	perp_x *= h;
	perp_y *= h;

	c.add_triangle(r::Triangle({x1 + perp_x, y1 + perp_y},
	                           {x2 + perp_x, y2 + perp_y},
	                           {x2 - perp_x, y2 - perp_y}));
	
	c.add_triangle(r::Triangle({x1 + perp_x, y1 + perp_y},
	                           {x2 - perp_x, y2 - perp_y},
	                           {x1 - perp_x, y1 - perp_y}));
}


#define SWAPFLOAT(a,b) \
  do{                      \
    float c = a;           \
    a = b;                 \
    b = c;                 \
  } while(0)

void GE_line(const GE_Context &c, float x1, float y1, float x2, float y2, float pen_width){

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
  if (c.is_gradient) {
    int key = get_key_from_pen_width(pen_width);
    c.lines[key].push_back(vl::dvec2(x1,y1+0.1f));
    c.lines[key].push_back(vl::dvec2(x2,y2-0.1f));
    return;
  }
#endif


  GE_line_lowlevel(c, x1, y1, x2, y2, pen_width);
}

//extern void gakk_GE_text(const char *text, int x, int y, float r, float g, float b, float a);

void GE_text(const GE_Context &c, const char *text, int x, int y){
	//c.textbitmaps.addCharBoxes(text, x, y+1);

	c.add_text(text, x, y);
		
#if 0
	const GE_Rgb &rgb = c.color.c;
	gakk_GE_text(text, x, y,
				 rgb.r / 256.0,
				 rgb.g / 256.0,
				 rgb.b / 256.0,
				 rgb.a / 256.0
		);
#endif
}

void GE_text2(const GE_Context &c, QString text, int x, int y){
	c.add_text(text, x, y);
	//c.textbitmaps.addCharBoxes(text, x, y+1);
}

void GE_text_halfsize(const GE_Context &c, const char *text, int x, int y){
	//c.textbitmaps_halfsize.addCharBoxes(text, x, y+1);
	c.add_text_halfsize(text, x, y);
}

void GE_text_halfsize2(const GE_Context &c, QString text, int x, int y){
	//c.textbitmaps_halfsize.addCharBoxes(text, x, y+1);
	c.add_text_halfsize(text, x, y);
}

void GE_box(const GE_Context &c, float x1, float y1, float x2, float y2, float pen_width){
	GE_line(c, x1, y1, x2, y1, pen_width);
	GE_line(c, x2, y1, x2, y2, pen_width);
	GE_line(c, x2, y2, x1, y2, pen_width);
	GE_line(c, x1, y2, x1, y1, pen_width);
}

void GE_filledBox(const GE_Context &c, float x1, float y1, float x2, float y2){

	c.add_triangle(r::Triangle({x1, y1},
							   {x2, y1},
							   {x1, y2}));
	
	c.add_triangle(r::Triangle({x1, y2},
							   {x2, y1},
							   {x2, y2}));
	
	/*
	  c.boxes.push_back(vl::dvec2(x1,y1));
	  c.boxes.push_back(vl::dvec2(x1,y2));
	  c.boxes.push_back(vl::dvec2(x2,y2));
	  c.boxes.push_back(vl::dvec2(x2,y1));
	*/
}

/*
void GE_polyline(GE_Context &c, int num_points, const APoint *points, float pen_width){
  if(num_points>0) {
    int key = get_key_from_pen_width(pen_width);
    c.lines[key].push_back(vl::dvec2(points[0].x, -points[0].y));
    for(int i=1;i<num_points;i++){
      float x = points[i].x;
      float y = points[i].y;
      c.lines[key].push_back(vl::dvec2(x,y));
      c.lines[key].push_back(vl::dvec2(x,y));
    }
    c.lines[key].push_back(vl::dvec2(points[0].x, points[0].y));
  }
}
*/

void GE_trianglestrip(const GE_Context &c, int num_points, const APoint *points){
  if(num_points>0){
    for(int i=0; i<num_points-2; i++)
	{
		c.add_triangle(r::Triangle({points[i].x, points[i].y},
								   {points[i+1].x, points[i+1].y},
								   {points[i+2].x, points[i+2].y}));

		/*
		c._triangles.push_back(r::fvec2(points[i].x, points[i].y));
		c._triangles.push_back(r::fvec2(points[i+1].x, points[i+1].y));
		c._triangles.push_back(r::fvec2(points[i+2].x, points[i+2].y));
		*/
    }
  }
}

static int num_trianglestrips;

void GE_trianglestrip_start(void){
  //R_ASSERT(num_trianglestrips==0);
  num_trianglestrips = 0;
}

void GE_trianglestrip_add(GE_Context &c, float x, float y){
  static float y2,y1;
  static float x2,x1;

  num_trianglestrips++;

  if(num_trianglestrips>=3)
  {
	  c.add_triangle(r::Triangle({x, y},
								 {x1, y1},
								 {x2, y2}));
  }

  y2 = y1;  y1 = y;
  x2 = x1;  x1 = x;
}

/*
void GE_trianglestrip_add_line(GE_Context &c, float x1, float y1, float x2, float y2, float pen_width){
  GE_line_lowlevel(c, c._triangles, x1, y1, x2, y2, pen_width);
}
*/

void GE_trianglestrip_end(GE_Context &c){
}



static int num_gradient_triangles;
static QVector<r::fvec2> current_gradient_rectangle;
static float triangles_min_y;
static float triangles_max_y;
static r2::GradientType::Type current_gradient_type;

void GE_gradient_triangle_start(r2::GradientType::Type type)
{
	current_gradient_rectangle.clear();
	current_gradient_type = type;
	num_gradient_triangles = 0;
}

void GE_gradient_triangle_add(GE_Context &c, float x, float y){
	static float y2,y1;
	static float x2,x1;

	// TODO: Fix this, probably not correct. Look at original code.
	
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
		c.add_triangle(r::Triangle(r::fvec2(x, y),
								   r::fvec2(x1, y1),
								   r::fvec2(x2, y2)),
					   current_gradient_type);
	}
	
	y2 = y1;  y1 = y;
	x2 = x1;  x1 = x;
}

void GE_gradient_triangle_end(GE_Context &c, float x1, float x2){
#if 0
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
#endif
}
