#include <assert.h>

#include <QColor>



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

typedef struct _GE_Context GE_Context;

// OpenGL draws from the bottom and up, so we need to know the height in order for the scheduling calls to accept "normal" y values.
// All drawing operations must be submitted again after changing the height.
void GE_set_height(int height);
int GE_get_height(void);

#ifdef GE_DRAW_VL
void GE_draw_vl(vl::ref<vl::VectorGraphics> vg, vl::ref<vl::Transform> scroll_transform, vl::ref<vl::Transform> linenumbers_transform, vl::ref<vl::Transform> scrollbar_transform);
#endif

enum{
  Z_BACKGROUND = -10,
  Z_ZERO = 0,
  Z_ABOVE = 10,

  Z_LINENUMBERS = 50,

  Z_MAX_SCROLLTRANSFORM = 100,  // All contexts with a z level of at 100 or less belongs to the scrolling and linenumber transform. (note names, etc.)

  Z_SCROLLBAR = 200, // All contexts with a z level higher than 100 and less than 200, belongs to the scrollbar transform.

  Z_MIN_STATIC = 300,  // All contexts with a z level of at least 100 are static contexts. (I.e. not scrolling or scrollbar. Used to paint cursor, etc.)
  Z_STATIC = 400
};

GE_Context *GE_linenumber(GE_Context *c);
static inline GE_Context *GE_static_x(GE_Context *c){
  return GE_linenumber(c);
}

void GE_clear(void);

GE_Context *GE_color_z(int colornum, int z);
GE_Context *GE_rgb_color_z(unsigned char r, unsigned char g, unsigned char b, int z);
GE_Context *GE_rgba_color_z(unsigned char r, unsigned char g, unsigned char b, unsigned char a, int z);
GE_Context *GE_mix_color_z(const GE_Rgb c1, const GE_Rgb c2, float how_much, int z);
GE_Context *GE_gradient_z(const GE_Rgb c1, const GE_Rgb c2, int z);

static inline GE_Context *GE_color(int colornum) {
  return GE_color_z(colornum, Z_ZERO);
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


void GE_line(GE_Context *c, float x1, float y1, float x2, float y2, float pen_width);
void GE_text(GE_Context *c, const char *text, float x, float y);
void GE_box(GE_Context *c, float x1, float y1, float x2, float y2, float pen_width);
void GE_filledBox(GE_Context *c, float x1, float y1, float x2, float y2);
void GE_polyline(GE_Context *c, int num_points, const APoint *points, float pen_width);
//void GE_polygon(GE_Context *c, int num_points, const APoint *points);
void GE_trianglestrip(GE_Context *c, int num_points, const APoint *points);
