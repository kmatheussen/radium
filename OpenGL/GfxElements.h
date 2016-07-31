#ifndef OPENGL_GFXELEMENTS_H
#define OPENGL_GFXELEMENTS_H


#ifdef __cplusplus
#include <QColor>
#include <QFont>
#endif

#include "SharedVariables.hpp"


typedef struct{
  unsigned char r,g,b,a;
} GE_Rgb;


GE_Rgb GE_get_rgb(enum ColorNums colornum);
GE_Rgb GE_get_custom_rgb(int custom_colornum);

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

typedef struct _GE_Context GE_Context;

#ifdef __cplusplus
struct PaintingData;
#endif

// OpenGL draws from the bottom and up, so we need to know the height in order for the scheduling calls to accept "normal" y values.
// All drawing operations must be submitted again after changing the height.
void GE_set_height(int height);
int GE_get_height(void);

#define SLICE_SIZE 512
#define NOMASK_Y (-SLICE_SIZE*20)

static inline uint32_t getMask(int a_y1, int a_y2){
  uint32_t mask = 0;

  a_y1--; // floating point rounding fix
  a_y2++; // floating point rounding fix
  
  int b_y1 = 0;
  int b_y2 = SLICE_SIZE;
  int i=0;
  for(i=0 ; i<32 ; i++, b_y1+=SLICE_SIZE, b_y2+=SLICE_SIZE){

    if (i < 31) { // The last bit includes everyting below

      if (b_y2 < a_y1)
        continue;

      if (b_y1 > a_y2)
        break;
    }

    mask |= ( 1<<i );
  }

  return mask;
}

#if defined(GE_DRAW_VL)
void GE_update_triangle_gradient_shaders(PaintingData *painting_data, float y_offset);
void GE_draw_vl(PaintingData *das_painting_data, vl::Viewport *viewport, vl::VectorGraphics *vg, vl::ref<vl::Transform> scroll_transform, vl::ref<vl::Transform> linenumbers_transform, vl::ref<vl::Transform> scrollbar_transform, vl::ref<vl::Transform> playcursor_transform);
#endif

#define Z_ABOVE(z) ((z)+2)
#define Z_BELOW(z) ((z)-2)
#define Z_STATIC_X 1
#define Z_IS_STATIC_X(z) ((z)&1)

enum{
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

GE_Context *GE_set_static_x(GE_Context *c);

void GE_set_z(GE_Context *c, int new_z); // 'c' should not be used before calling this function.
int GE_get_z(GE_Context *c);
GE_Rgb GE_get_rgb(GE_Context *c);

#ifdef __cplusplus
SharedVariables *GE_get_shared_variables(PaintingData *painting_data);
PaintingData *GE_get_painting_data(PaintingData *current_painting_data, bool *needs_repaint);  // returns NULL if nothing was written since last call to the function.
#endif

void GE_start_writing(void);
void GE_end_writing(GE_Rgb new_background_color);
void GE_wait_until_block_is_rendered(void);

GE_Context *GE_z(const GE_Rgb rgb, int z, int y);
static inline GE_Context *GE(const GE_Rgb rgb, int y){
  return GE_z(rgb, Z_ZERO, y);
}
GE_Context *GE_y(GE_Context *c, int y);
GE_Context *GE_color_z(enum ColorNums colornum, int z, int y);
GE_Context *GE_textcolor_z(enum ColorNums colornum, int z, int y);
GE_Context *GE_rgb_color_z(unsigned char r, unsigned char g, unsigned char b, int z, int y);
GE_Context *GE_rgba_color_z(unsigned char r, unsigned char g, unsigned char b, unsigned char a, int z, int y);
GE_Context *GE_mix_color_z(const GE_Rgb c1, const GE_Rgb c2, float how_much, int z, int y);
GE_Context *GE_gradient_z(const GE_Rgb c1, const GE_Rgb c2, int z, int y);

#ifdef __cplusplus
GE_Context *GE_color_z(const QColor &color, int z, int y);
static inline GE_Context *GE_color(const QColor &color, int y) {
  return GE_color_z(color, Z_ZERO, y);
}
GE_Context *GE_gradient_z(const QColor &c1, const QColor &c2, int z, int y);
static inline GE_Context *GE_mix_alpha(const GE_Rgb c1, const GE_Rgb c2, float how_much, float alpha, int y){
  return GE(GE_alpha(GE_mix(c1, c2, how_much), alpha), y);
}
static inline GE_Context *GE_mix_alpha_z(const GE_Rgb c1, const GE_Rgb c2, float how_much, float alpha, int z, int y){
  return GE_z(GE_alpha(GE_mix(c1, c2, how_much), alpha), z, y);
}

#ifndef EDITOR_WIDGET_H
#include "../Qt/EditorWidget.h"
extern struct Root *root;
static inline QColor GE_qcolor(enum ColorNums colornum){
  return get_qcolor(colornum);
}
#endif

#endif

static inline GE_Context *GE_color(enum ColorNums colornum, int y) {
  return GE_color_z(colornum, Z_ZERO, y);
}

GE_Context *GE_color_alpha_z(enum ColorNums colornum, float alpha, int z, int y);
static inline GE_Context *GE_color_alpha(enum ColorNums colornum, float alpha, int y) {
  return GE_color_alpha_z(colornum, alpha, Z_ZERO, y);
}

static inline GE_Context *GE_textcolor(enum ColorNums colornum, int y) {
  return GE_textcolor_z(colornum, Z_ZERO, y);
}
static inline GE_Context *GE_rgb_color(unsigned char r, unsigned char g, unsigned char b, int y) {
  return GE_rgb_color_z(r,g,b, Z_ZERO, y);
}

#define Black_color(y) GE_rgb_color(0,0,0,y)
#define White_color(y) GE_rgb_color(0,0,0,y)


static inline GE_Context *GE_rgba_color(unsigned char r, unsigned char g, unsigned char b, unsigned char a, int y) {
  return GE_rgba_color_z(r,g,b,a, Z_ZERO,y);
}
static inline GE_Context *GE_mix_color(const GE_Rgb c1, const GE_Rgb c2, float how_much, int y) {
  return GE_mix_color_z(c1,c2,how_much, Z_ZERO,y);
}
static inline GE_Context *GE_gradient(const GE_Rgb c1, const GE_Rgb c2, int y) {
  return GE_gradient_z(c1,c2, Z_ZERO,y);
}

#ifdef __cplusplus
//void GE_set_font(QFont font);
void GE_set_font(const QFont &font);
#endif

void GE_set_x_scissor(float x, float x2);
void GE_unset_x_scissor(void);
  
void GE_line(GE_Context *c, float x1, float y1, float x2, float y2, float pen_width);
void GE_text(GE_Context *c, const char *text, int x, int y);
void GE_text_halfsize(GE_Context *c, const char *text, int x, int y);
void GE_box(GE_Context *c, float x1, float y1, float x2, float y2, float pen_width);
void GE_filledBox(GE_Context *c, float x1, float y1, float x2, float y2);
void GE_polyline(GE_Context *c, int num_points, const APoint *points, float pen_width);
//void GE_polygon(GE_Context *c, int num_points, const APoint *points);
void GE_trianglestrip(GE_Context *c, int num_points, const APoint *points);

void GE_trianglestrip_start();
void GE_trianglestrip_add(GE_Context *c, float x, float y);
void GE_trianglestrip_end(GE_Context *c);

struct GradientType {
  enum Type {
    NOTYPE,
    HORIZONTAL,
    VELOCITY
  };
};
  
void GE_gradient_triangle_start(GradientType::Type type);
void GE_gradient_triangle_add(GE_Context *c, float x, float y);
void GE_gradient_triangle_end(GE_Context *c, float x1, float x2);

#endif
