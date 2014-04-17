#ifndef OPENGL_GFXELEMENTS_H
#define OPENGL_GFXELEMENTS_H


#include <assert.h>

#ifdef __cplusplus
#include <QColor>
#include <QFont>
#endif

#include "SharedVariables.hpp"


typedef struct{
  unsigned char r,g,b,a;
} GE_Rgb;


GE_Rgb GE_get_rgb(int colornum);

static inline GE_Rgb GE_rgb(unsigned char r, unsigned char g, unsigned char b){
  GE_Rgb ret = {r,g,b,255};
  return ret;
}
static inline GE_Rgb GE_rgba(unsigned char r, unsigned char g, unsigned char b, unsigned char a){
  GE_Rgb ret = {r,g,b,a};
  return ret;
}

GE_Rgb GE_mix(const GE_Rgb c1, const GE_Rgb c2, float how_much);
static inline GE_Rgb GE_alpha(const GE_Rgb c, float alpha){
  GE_Rgb ret = c;
  ret.a = alpha*255;
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

#if defined(GE_DRAW_VL)
void GE_draw_vl(PaintingData *das_painting_data, vl::Viewport *viewport, vl::ref<vl::VectorGraphics> vg, vl::ref<vl::Transform> scroll_transform, vl::ref<vl::Transform> linenumbers_transform, vl::ref<vl::Transform> scrollbar_transform);
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

  Z_MIN_STATIC = 300,  // All contexts with a z level of at least 100 are static contexts. (I.e. not scrolling or scrollbar. Used to paint cursor, etc.)
  Z_STATIC = 400
};

GE_Context *GE_set_static_x(GE_Context *c);

#ifdef __cplusplus
SharedVariables *GE_get_shared_variables(PaintingData *painting_data);
PaintingData *GE_get_painting_data(PaintingData *current_painting_data, bool *needs_repaint);  // returns NULL if nothing was written since last call to the function.
#endif

void GE_start_writing(void);
void GE_end_writing(GE_Rgb new_background_color);

GE_Context *GE_z(const GE_Rgb rgb, int z);
static inline GE_Context *GE(const GE_Rgb rgb){
  return GE_z(rgb, Z_ZERO);
}
GE_Context *GE_color_z(int colornum, int z);
GE_Context *GE_textcolor_z(int colornum, int z);
GE_Context *GE_rgb_color_z(unsigned char r, unsigned char g, unsigned char b, int z);
GE_Context *GE_rgba_color_z(unsigned char r, unsigned char g, unsigned char b, unsigned char a, int z);
GE_Context *GE_mix_color_z(const GE_Rgb c1, const GE_Rgb c2, float how_much, int z);
GE_Context *GE_gradient_z(const GE_Rgb c1, const GE_Rgb c2, int z);


#ifdef __cplusplus
GE_Context *GE_color_z(const QColor &color, int z);
static inline GE_Context *GE_color(const QColor &color) {
  return GE_color_z(color, Z_ZERO);
}
GE_Context *GE_gradient_z(const QColor &c1, const QColor &c2, int z);
static inline GE_Context *GE_mix_alpha(const GE_Rgb c1, const GE_Rgb c2, float how_much, float alpha){
  return GE(GE_alpha(GE_mix(c1, c2, how_much), alpha));
}

#include "../Qt/EditorWidget.h"
extern struct Root *root;
static inline QColor GE_qcolor(int colornum){
  EditorWidget *editor=(EditorWidget *)root->song->tracker_windows->os_visual.widget;
  return editor->colors[colornum];
}
#endif

static inline GE_Context *GE_color(int colornum) {
  return GE_color_z(colornum, Z_ZERO);
}

GE_Context *GE_color_alpha_z(int colornum, float alpha, int z);
static inline GE_Context *GE_color_alpha(int colornum, float alpha) {
  return GE_color_alpha_z(colornum, alpha, Z_ZERO);
}

static inline GE_Context *GE_textcolor(int colornum) {
  return GE_textcolor_z(colornum, Z_ZERO);
}
static inline GE_Context *GE_rgb_color(unsigned char r, unsigned char g, unsigned char b) {
  return GE_rgb_color_z(r,g,b, Z_ZERO);
}
static inline GE_Context *GE_rgba_color(unsigned char r, unsigned char g, unsigned char b, unsigned char a) {
  return GE_rgba_color_z(r,g,b,a, Z_ZERO);
}
static inline GE_Context *GE_mix_color(const GE_Rgb c1, const GE_Rgb c2, float how_much) {
  return GE_mix_color_z(c1,c2,how_much, Z_ZERO);
}
static inline GE_Context *GE_gradient(const GE_Rgb c1, const GE_Rgb c2) {
  return GE_gradient_z(c1,c2, Z_ZERO);
}

#ifdef __cplusplus
//void GE_set_font(QFont font);
void GE_set_font(const QFont &font);
#endif

void GE_line(GE_Context *c, float x1, float y1, float x2, float y2, float pen_width);
void GE_text(GE_Context *c, const char *text, float x, float y);
void GE_box(GE_Context *c, float x1, float y1, float x2, float y2, float pen_width);
void GE_filledBox(GE_Context *c, float x1, float y1, float x2, float y2);
void GE_polyline(GE_Context *c, int num_points, const APoint *points, float pen_width);
//void GE_polygon(GE_Context *c, int num_points, const APoint *points);
void GE_trianglestrip(GE_Context *c, int num_points, const APoint *points);
void GE_trianglestrip_start();
void GE_trianglestrip_add(GE_Context *c, float x, float y);
void GE_trianglestrip_end(GE_Context *c);


#endif
