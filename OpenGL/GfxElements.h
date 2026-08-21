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


#ifndef OPENGL_GFXELEMENTS_H
#define OPENGL_GFXELEMENTS_H


#ifdef __cplusplus
#include <QColor>
#include <QFont>
#include <QString>
#endif

#include "../api/api_proc.h"
#include "../Qt/Qt_colors_proc.h"

#include "SharedVariables.hpp"

#define MAX_NUM_SLICES 32
#define MIN_SLICE_SIZE 64 //1024

//extern double g_opengl_scale_ratio;

namespace r
{
struct fvec2
{
	float a,b;
	
	fvec2(const float a_, const float b_)
		: a(a_)
		, b(b_)
	{
	}

	fvec2()
	{
	}
};
struct fvec3
{
	float a,b,c;
};
struct fvec4
{
	float a,b,c,d;
};
} // namespace r

enum UseScissors{
  NO_SCISSORS = 0,
  USE_SCISSORS = 1
};


struct GE_Conf{
  int z;
  enum UseScissors use_scissors;

  GE_Conf(int z, enum UseScissors use_scissors = USE_SCISSORS)
    : z(z)
    , use_scissors(use_scissors)
  {
  }
};


typedef struct{
  unsigned char r,g,b,a;
} GE_Rgb;


GE_Rgb GE_get_rgb(enum ColorNums colornum, bool is_instrument = false);
GE_Rgb GE_get_custom_rgb(int custom_colornum);
static inline GE_Rgb GE_get_rgb(unsigned int color, bool is_instrument = false){
  QColor c(color);

  if(is_instrument)
    apply_instrument_in_editor_colorization(c);

  GE_Rgb ret = {(unsigned char)c.red(), (unsigned char)c.green(), (unsigned char)c.blue(), (unsigned char)c.alpha()};
  return ret;
}

static inline GE_Rgb GE_rgb(unsigned char r, unsigned char g, unsigned char b){
  GE_Rgb ret = {r,g,b,255};
  return ret;
}
#define Black_rgb() GE_rgb(0,0,0)
#define White_rgb() GE_rgb(255,255,255)

static inline GE_Rgb GE_rgba(unsigned char r, unsigned char g, unsigned char b, unsigned char a){
  GE_Rgb ret = {r,g,b,a};
  return ret;
}

GE_Rgb GE_mix(const GE_Rgb c1, const GE_Rgb c2, float how_much);
static inline GE_Rgb GE_alpha(const GE_Rgb c, float alpha){
  GE_Rgb ret = c;
  ret.a = alpha*255.0f;  
  return ret;
}

#if 0 //def Object_INCLUDE_ONCE
static inline vl::vec4 get_vec4(GE_Rgb rgb){
  return vl::vec4(rgb.r/255.0f, rgb.g/255.0f, rgb.b/255.0f, rgb.a/255.0f);
}
#endif

//typedef struct _GE_Context GE_Context;

struct GE_Context;

namespace r2
{
struct GradientType {
  enum Type {
    NOTYPE,
    HORIZONTAL,
    VELOCITY
  };
};
}

namespace r
{
struct Triangle
{
	r::fvec2 _v0, _v1, _v2;

	Triangle(const r::fvec2 &p1, const r::fvec2 &p2, const r::fvec2 &p3)
		: _v0(p1)
		, _v1(p2)
		, _v2(p3)
	{
#if 0
		const r::fvec2 *vs[3] = {&p1, &p2, &p3};

		// Sort by y (ascending): _v0 = topmost, _v1 = middle, _v2 = bottommost
		if (vs[1]->b < vs[0]->b) std::swap(vs[0], vs[1]);
		if (vs[2]->b < vs[0]->b) std::swap(vs[0], vs[2]);
		if (vs[2]->b < vs[1]->b) std::swap(vs[1], vs[2]);

		_v0 = *vs[0];
		_v1 = *vs[1];
		_v2 = *vs[2];
#endif
	}

	float get_y1(void) const
	{
		return R_MIN(_v0.b, R_MIN(_v1.b, _v2.b));
	}
	
	//float get_y2(void) const { return _v1.b; }
	
	float get_y3(void) const
	{
		return R_MAX(_v0.b, R_MAX(_v1.b, _v2.b));
	}
};
} // namespace r


extern int g_main_thread_slice_size;


struct GE_Context
{
	union Color
	{
		struct
		{
			GE_Rgb c;
			GE_Rgb c_gradient;
		};
		//uint64_t key;
	} color;
	
	GE_Conf _conf;
	bool _is_null = false;

public:

	GE_Context(const Color &_color, const GE_Conf &conf)
		: color(_color)
		, _conf(conf)
	{
		//printf("ctor: this=%p\n", this);
		R_ASSERT(sizeof(Color)==sizeof(uint64_t));
	}

	void add_triangle(const r::Triangle &triangle, r2::GradientType::Type gradient_type = r2::GradientType::Type::NOTYPE) const;
	
	void add_text(const QString &text, int x, int y) const;
	
	void add_text_halfsize(const QString &text, int x, int y) const;

	bool isNull(void) const
	{
		return _is_null;
	}
};


static GE_Context g_c_NULL = [](){
	GE_Context c({}, GE_Conf(0));
	c._is_null = true;
	return c;
}();


#ifdef __cplusplus
namespace r
{
// Contains all data necessary to paint the editor.
// Created on the main thread, and then transfered to the qhri thread.
struct PaintingData
{
	//Contexts contexts;
	//std::vector<GE_Context*> _contexts;
	
	//std::vector< vl::ref<GradientTriangles> > gradient_triangles;
	SharedVariables shared_variables;
	
	int slice_size;
	
	PaintingData(int full_height, bool block_is_visible)
		: shared_variables(block_is_visible)
	{
		slice_size = full_height / MAX_NUM_SLICES;
		
		if (slice_size < MIN_SLICE_SIZE)
			slice_size = MIN_SLICE_SIZE;
	}

	~PaintingData()
	{
		//	for(GE_Context* context : _contexts)
		//	delete context;
	}
};
} // namespace r

extern void GL_set_new_painting_data(r::PaintingData *painting_data, GE_Rgb new_background_color); // Implemented in Widget.cpp
#endif


// OpenGL draws from the bottom and up, so we need to know the height in order for the scheduling calls to accept "normal" y values.
// All drawing operations must be submitted again after changing the height.
void GE_set_height(int height);
int GE_get_height(void);


#define Z_ABOVE(z) ((z)+2)
#define Z_BELOW(z) ((z)-2)
#define Z_STATIC_X 1
#define Z_IS_STATIC_X(z) ((z)&1)


enum
{
  Z_BACKGROUND = -10,
  Z_ZERO = 0,

  Z_LINENUMBERS = 50,

  Z_MAX_SCROLLTRANSFORM = 100,  // All contexts with a z level of at 100 or less belongs to the scrolling and linenumber transform. (note names, etc.)

  Z_SCROLLBAR = 200, // All contexts with a z level higher than 100 and less than 200, belongs to the scrollbar transform.

  Z_MIN_STATIC = 300,  // All contexts with a z level of at least 300, and below Z_MAX_STATIC are static contexts. (I.e. not scrolling or scrollbar. Used to paint cursor, etc.)
  Z_STATIC = 400,
  Z_MAX_STATIC = 500,
  
  Z_PLAYCURSOR = 600, // The play cursor is drawn on top of everything else.
};

GE_Context GE_set_static_x(const GE_Context &c);

void GE_set_z(GE_Context &c, int new_z); // 'c' should not be used before calling this function.
int GE_get_z(const GE_Context &c);
GE_Rgb GE_get_rgb(const GE_Context &c);


bool GE_start_writing(int full_height, bool block_is_visible); // 'full_height' is not used if block_is_visible is false. Returns false if gpu widget isn't ready yet.
void GE_end_writing(GE_Rgb new_background_color);
void GE_wait_until_block_is_rendered(void);

GE_Context GE_z(const GE_Rgb rgb, const GE_Conf &conf);
GE_Context GE_color_z(enum ColorNums colornum, const GE_Conf &conf);
GE_Context GE_textcolor_z(enum ColorNums colornum, const GE_Conf &conf);
GE_Context GE_rgb_color_z(unsigned char r, unsigned char g, unsigned char b, const GE_Conf &conf);
GE_Context GE_rgba_color_z(unsigned char r, unsigned char g, unsigned char b, unsigned char a, const GE_Conf &conf);
GE_Context GE_mix_color_z(const GE_Rgb c1, const GE_Rgb c2, float how_much, const GE_Conf &conf);
GE_Context GE_gradient_z(const GE_Rgb c1, const GE_Rgb c2, const GE_Conf &conf);

#ifdef __cplusplus
GE_Context GE_color_z(const QColor &color, const GE_Conf &conf);
GE_Context GE_gradient_z(const QColor &c1, const QColor &c2, const GE_Conf &conf);
static inline GE_Context GE_mix_alpha_z(const GE_Rgb c1, const GE_Rgb c2, float how_much, float alpha, const GE_Conf &conf){
  return GE_z(GE_alpha(GE_mix(c1, c2, how_much), alpha), conf);
}

static inline QColor GE_qcolor(enum ColorNums colornum){
  return get_qcolor(colornum);
}
#endif

GE_Context GE_color_alpha_z(enum ColorNums colornum, float alpha, const GE_Conf &conf);
static inline GE_Context GE_color_alpha(enum ColorNums colornum, float alpha, enum UseScissors use_scissors = USE_SCISSORS) {
  return GE_color_alpha_z(colornum, alpha, GE_Conf(Z_ZERO, use_scissors));
}

static inline GE_Context GE_rgb_color(unsigned char r, unsigned char g, unsigned char b, enum UseScissors use_scissors = USE_SCISSORS) {
  return GE_rgb_color_z(r,g,b, GE_Conf(Z_ZERO, use_scissors));
}

#define Black_color(use_scissors) GE_rgb_color(0,0,0,use_scissors)
#define White_color(use_scissors) GE_rgb_color(255,255,255,use_scissors)
#define Green_color(use_scissors) GE_rgb_color(0,255,0,use_scissors)

#ifdef __cplusplus
//void GE_set_font(QFont font);
void GE_set_font(const QFont &font);
#endif

void GE_set_x_scissor(float x, float x2);
void GE_unset_x_scissor(void);
  
void GE_line(const GE_Context &c, float x1, float y1, float x2, float y2, float pen_width);
void GE_text(const GE_Context &c, const char *text, int x, int y);
#ifdef __cplusplus
void GE_text2(const GE_Context &c, QString text, int x, int y);
#endif
void GE_text_halfsize(const GE_Context &c, const char *text, int x, int y);
#ifdef __cplusplus
void GE_text_halfsize2(const GE_Context &c, QString text, int x, int y);
#endif
void GE_box(const GE_Context &c, float x1, float y1, float x2, float y2, float pen_width);

static inline void GE_box_without_left(const GE_Context &c, float x1, float y1, float x2, float y2, float pen_width){
  GE_line(c, x1, y1, x2, y1, pen_width);
  GE_line(c, x2, y1, x2, y2, pen_width);
  GE_line(c, x2, y2, x1, y2, pen_width);
  //GE_line(c, x1, y2, x1, y1, pen_width);
}

static inline void GE_box_without_right(const GE_Context &c, float x1, float y1, float x2, float y2, float pen_width){
  GE_line(c, x1, y1, x2, y1, pen_width);
  //GE_line(c, x2, y1, x2, y2, pen_width);
  GE_line(c, x2, y2, x1, y2, pen_width);
  GE_line(c, x1, y2, x1, y1, pen_width);
}

void GE_filledBox(const GE_Context &c, float x1, float y1, float x2, float y2);
void GE_polyline(const GE_Context &c, int num_points, const APoint *points, float pen_width);
//void GE_polygon(GE_Context &c, int num_points, const APoint *points);
void GE_trianglestrip(const GE_Context &c, int num_points, const APoint *points);

void GE_trianglestrip_start(void);
void GE_trianglestrip_add(GE_Context &c, float x, float y);
void GE_trianglestrip_end(GE_Context &c);

#if __cplusplus
namespace{
  class GE_ScopedTrianglestrip{

	  GE_Context *_c = NULL;

  public:
    
    GE_ScopedTrianglestrip()
    {
		GE_trianglestrip_start();
    }

    ~GE_ScopedTrianglestrip(){
		if (_c)
			GE_trianglestrip_end(*_c);
    }

  public:

    /*
    void add_line(GE_Context &c, float x1, float y1, float x2, float y2, float pen_width){
      change_context(c);

      GE_trianglestrip_add_line(c, x1, y1, x2, y2, pen_width);
    }
    */
    
    void add(GE_Context &c, float x, float y){
		_c = &c;
		GE_trianglestrip_add(c, x, y);
    }
  };
}
#endif

void T3_create_gradienttriangles_if_needed(bool got_new_t2_data);

void GE_gradient_triangle_start(r2::GradientType::Type type);
void GE_gradient_triangle_add(GE_Context &c, float x, float y);
void GE_gradient_triangle_end(GE_Context &c, float x1, float x2);

#endif
