/* Copyright 2016 Kjetil S. Matheussen

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

#define __STDC_FORMAT_MACROS 1
#include <inttypes.h>

#include <math.h>

#include <QColor>
#include <QRawFont>
#include <QApplication>
#include <QMimeData>


#define INCLUDE_SNDFILE_OPEN_FUNCTIONS 1
#include "../common/nsmtracker.h"

#include "Qt_MyQCheckBox.h"
#include "Qt_MyQSlider.h"
#include "Qt_Bs_edit_proc.h"
#include "Qt_Fonts_proc.h"

#include "../embedded_scheme/scheme_proc.h"
#include "../common/seqtrack_automation_proc.h"
#include "../common/song_tempo_automation_proc.h"
#include "../common/seqblock_automation_proc.h"
#include "../common/sequencer_timing_proc.h"
#include "../common/tracks_proc.h"
#include "../common/player_proc.h"
#include "../common/OS_Bs_edit_proc.h"

#include "../api/api_proc.h"
#include "../api/api_gui_proc.h"
#include "../api/api_common_proc.h"

#include "../audio/Mixer_proc.h"

#include "../audio/Peaks.hpp"
#include "../audio/Envelope.hpp"
#include "../audio/SampleReader_proc.h"

#include "../embedded_scheme/s7extra_proc.h"

#include "../common/seqtrack_proc.h"

#include "Qt_sequencer_proc.h"


// May (probably) experience problems (crash or garbage gfx) if zooming in too much and qreal is just 32 bits.
static_assert (sizeof(qreal) >= 8, "qreal should be at least 64 bits");


#define DO_DEBUG 0  // Tip: uncommenting this line can be very useful during development.

#if defined(RELEASE)
#  define D(n)
#elif DO_DEBUG
#  define D(n) n
#else
#  define D(n)
#endif


#define USE_BLURRED 0

#if USE_BLURRED
// copied from https://stackoverflow.com/questions/3903223/qt4-how-to-blur-qpixmap-image
static QImage blurred(QImage& image, const QRect& rect, int radius, bool alphaOnly = false)
{
    int tab[] = { 14, 10, 8, 6, 5, 5, 4, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2 };
    int alpha = (radius < 1)  ? 16 : (radius > 17) ? 1 : tab[radius-1];

    QImage &result = image;//image.convertToFormat(QImage::Format_ARGB32_Premultiplied);
    int r1 = rect.top();
    int r2 = rect.bottom();
    int c1 = rect.left();
    int c2 = rect.right();

    int bpl = result.bytesPerLine();
    int rgba[4];
    unsigned char* p;

    int i1 = 0;
    int i2 = 3;

    if (alphaOnly)
        i1 = i2 = (QSysInfo::ByteOrder == QSysInfo::BigEndian ? 0 : 3);

    for (int col = c1; col <= c2; col++) {
        p = result.scanLine(r1) + col * 4;
        for (int i = i1; i <= i2; i++)
            rgba[i] = p[i] << 4;

        p += bpl;
        for (int j = r1; j < r2; j++, p += bpl)
            for (int i = i1; i <= i2; i++)
                p[i] = (rgba[i] += ((p[i] << 4) - rgba[i]) * alpha / 16) >> 4;
    }

    for (int row = r1; row <= r2; row++) {
        p = result.scanLine(row) + c1 * 4;
        for (int i = i1; i <= i2; i++)
            rgba[i] = p[i] << 4;

        p += 4;
        for (int j = c1; j < c2; j++, p += 4)
            for (int i = i1; i <= i2; i++)
                p[i] = (rgba[i] += ((p[i] << 4) - rgba[i]) * alpha / 16) >> 4;
    }

    for (int col = c1; col <= c2; col++) {
        p = result.scanLine(r2) + col * 4;
        for (int i = i1; i <= i2; i++)
            rgba[i] = p[i] << 4;

        p -= bpl;
        for (int j = r1; j < r2; j++, p -= bpl)
            for (int i = i1; i <= i2; i++)
                p[i] = (rgba[i] += ((p[i] << 4) - rgba[i]) * alpha / 16) >> 4;
    }

    for (int row = r1; row <= r2; row++) {
        p = result.scanLine(row) + c2 * 4;
        for (int i = i1; i <= i2; i++)
            rgba[i] = p[i] << 4;

        p -= 4;
        for (int j = c1; j < c2; j++, p -= 4)
            for (int i = i1; i <= i2; i++)
                p[i] = (rgba[i] += ((p[i] << 4) - rgba[i]) * alpha / 16) >> 4;
    }

    return result;
}
#endif


static DEFINE_ATOMIC(uint32_t, g_needs_update) = 0;

static bool smooth_scrolling(void){
  return smoothSequencerScrollingEnabled();
}

static bool g_draw_colored_seqblock_tracks = true;

int64_t g_curr_seqblock_id_under_mouse = -1;


/* Custom font drawing code */

namespace{
  struct GlyphpathAndWidth{
    QPainterPath path;
    float width;
    GlyphpathAndWidth(const QPainterPath _path, float _width)
      : path(_path)
      , width(_width)
    {}
    GlyphpathAndWidth()
      : width(0.0f)
    {}
  };
}

static const GlyphpathAndWidth &getGlyphpathAndWidth(const QFont &font, const QChar c){
  static QHash<const QChar,GlyphpathAndWidth> glyphpathCache;

  static QFont cacheFont = font;
  static QRawFont rawFont = QRawFont::fromFont(font);
  static QFontMetricsF fn(font);
  
  if (font != cacheFont){
    glyphpathCache.clear();
    cacheFont = font;
    rawFont = QRawFont::fromFont(font);
    fn = QFontMetrics(font);
  }

  if (!glyphpathCache.contains(c)){
    const QVector<quint32> indexes = rawFont.glyphIndexesForString(c);    
    glyphpathCache[c] = GlyphpathAndWidth(rawFont.pathForGlyph(indexes[0]), fn.width(c));
  }
  
  return glyphpathCache[c];
}

static bool g_is_drawing_sequencer_time = false;
static bool g_is_drawing_sequencer_gfx_block = false;

bool myDrawText(QPainter *painter, QRectF rect, QString text, int flags, bool wrap_lines, int rotate, bool scale_font_size, bool cut_text_to_fit){

  QFont font;

  rect.adjust(0,-1,0,0); // strange.
  
  if (scale_font_size) {
    
    if (cut_text_to_fit){
      
      if ( (rotate > 45 && rotate < 90+45) || (rotate > 180+45 && rotate < 270+45))
        font = GFX_getFittingFont(text, flags, 4096, rect.width());
      else
        font = GFX_getFittingFont(text, flags, 4096, rect.height());
      
    } else {
      
      if ( (rotate > 45 && rotate < 90+45) || (rotate > 180+45 && rotate < 270+45))
        font = GFX_getFittingFont(text, flags, rect.height(), rect.width());
      else
        font = GFX_getFittingFont(text, flags, rect.width(), rect.height());
      
    }
    
    painter->setFont(font);
  }

  QString org_text = text;
  QString draw_text = text;
  
  if (cut_text_to_fit) {
    
    if ( (rotate > 45 && rotate < 90+45) || (rotate > 180+45 && rotate < 270+45))
      draw_text = GFX_getFittingText(font, text, flags, wrap_lines, rect.height(), rect.width()); // vertical
    else
      draw_text = GFX_getFittingText(font, text, flags, wrap_lines, rect.width(), rect.height()); // horizontal
    
  }

  if (rotate != 0) {

    painter->save();
    
    painter->rotate(rotate);
    
    // Not good enough. Need some trigonometry here to get it correct for all rotate values, but we have only used 90 and 270 degrees in the code so far.
    if (rotate==270){
      painter->translate(-rect.y()-rect.height(), rect.x());
    } else if (rotate==90){
      painter->translate(rect.y(), -rect.x()-rect.width());
    }else{
      handleError("Only rotate values of 90 and 270 are supported. (It's probably not hard to fix this though.)");
    }
    
    rect = QRect(0,0,rect.height(),rect.width());
  }

  
  if (g_is_drawing_sequencer_gfx_block || (g_is_drawing_sequencer_time && is_playing() && pc->playtype==PLAYSONG && smooth_scrolling())){

    R_ASSERT_NON_RELEASE(g_is_drawing_sequencer_time);
    
    // Paint glyphs manually. QPainter::drawText doesn't have floating point precision.
    int len=text.length();

    double x = rect.x();
    double y = rect.y();

    //const QFont &font = painter->font();

    QFontMetrics fm(font);      
    y += fm.ascent();
    
    if (flags & Qt::AlignVCenter){
      y += (rect.height()-fm.height()) / 2.0;
    }
    
    QBrush brush = painter->pen().brush();

    bool was_clipping = painter->hasClipping();
    const QRegion &old_clipregion = painter->clipRegion();

    if (was_clipping)
      painter->setClipRegion(old_clipregion.intersected(rect.toRect()));
    else
      painter->setClipRect(rect.toRect());
    
    if(was_clipping==false)
      painter->setClipping(true);
        
    for(int i=0; i<len; i++){
      const GlyphpathAndWidth &g = getGlyphpathAndWidth(font, text.at(i));
      painter->save();
      painter->translate(x, y);
      painter->fillPath(g.path,brush);
      painter->restore();
      
      x += g.width;
    }

    if (was_clipping)
      painter->setClipRegion(old_clipregion);
    else
      painter->setClipping(false);
    
  } else {

    QTextOption option((Qt::Alignment)flags);
    option.setWrapMode(QTextOption::NoWrap);
    painter->drawText(rect, text, option);
    
  }

  if (rotate != 0)
    painter->restore();
  
  if(scale_font_size)
    painter->setFont(QFont());

  return org_text==draw_text;
}

static void myFilledPolygon(QPainter &p, QPointF *points, int num_points, const QColor &color){
  QPen pen = p.pen();
  p.setPen(Qt::NoPen);
  p.setBrush(color);
  p.drawPolygon(points, num_points);
  p.setBrush(Qt::NoBrush);
  p.setPen(pen);
}

static void my_update_sequencer_widget(const QRect &rect);

/*
static void g_position_widgets(void);
*/

static QPoint skewedPoint(QWidget *widget, const QPoint &p, int dx, int dy){
  QPoint p2 = widget->mapToGlobal(p);
  return QPoint(p2.x()+dx, p2.y()+dy);
}

static void paintMarkerLines(QPainter &p, int64_t start_time, int64_t end_time, double x1, double y1, double x2, double y2){
  QColor color = get_qcolor(SEQUENCER_MARKER_COLOR_NUM);
  color.setAlphaF(0.5);
  QPen blue_pen(color);
  blue_pen.setWidthF((double)root->song->tracker_windows->systemfontheight / 5.17);
  
  p.setPen(blue_pen);
  
  const dynvec_t *markers = SEQUENCER_MARKER_get_state().array;
  for(const dyn_t &marker : *markers){
    hash_t *hash = marker.hash;
    double time = HASH_get_number(hash, ":time");
    //QString name = HASH_get_qstring(hash, ":name");
    
    double x = scale_double(time, start_time, end_time, x1, x2);
    
    QLineF line(x, y1+2, x, y2);
    p.drawLine(line);
  }
}


/*
static int get_seqtracks_total_height(void){
  const int num_seqtracks = root->song->seqtracks.num_elements;
  
  if(num_seqtracks==0)
    return 0;

  double seqtrack0_y1 = getSeqtrackFromNum(0)->y1;
  double seqtrackN_y2 = getSeqtrackFromNum(num_seqtracks-1)->y2;

  return seqtrackN_y2 - seqtrack0_y1;
}
*/

static double get_seqtrack_border_width(void){
  return floor(root->song->tracker_windows->systemfontheight / 2.5);
}

static int get_block_header_height(void) {
  return root->song->tracker_windows->systemfontheight*1.3;
}

static double get_seqblock_xsplit1(double seqblock_x1, double seqblock_x2){
  
  return seqblock_x1 + R_MIN((double)root->song->tracker_windows->systemfontheight*10, (seqblock_x2-seqblock_x1) / 4);
}

static double get_seqblock_xsplit2(double seqblock_x1, double seqblock_x2){
  return seqblock_x2 - R_MIN((double)root->song->tracker_windows->systemfontheight*10, (seqblock_x2-seqblock_x1) / 4);
}

static double get_seqblock_ysplit1(double seqblock_y1, double seqblock_y2){
  double y1 = seqblock_y1;
  return y1 + (seqblock_y2-y1) / 4;
}

static double get_seqblock_ysplit2(double seqblock_y1, double seqblock_y2){
  double y1 = seqblock_y1;
  return y1 + (seqblock_y2-y1) / 2;
}

static double get_seqblock_ysplit3(double seqblock_y1, double seqblock_y2){
  double y1 = seqblock_y1;
  return seqblock_y2 - (seqblock_y2-y1) / 4;
}

static void paintCurrBorder(QPainter &p, const QRectF &rect, const QColor &color, bool paint_float = false, int round_x = 3, int round_y = 3) {
  float b1 = 1.0;
  float b2 = 1.0;
  float b = floor(get_seqtrack_border_width());

  // fill
  {
    QPen pen(color);
    pen.setWidthF(b);
    p.setPen(pen);


    QRectF rect2 = rect.toRect();
    rect2.adjust(b/2.0, 0, 0, 0);
    
    if (paint_float){

      QRectF rect3(rect.x()+b/2, rect2.y(), rect.width()-b/2, rect2.height());
      p.drawRoundedRect(rect3,round_x,round_y);//rect, round_x, round_y);
      
    } else {
      
      p.drawRoundedRect(rect2,round_x,round_y);//rect, round_x, round_y);
      
    }
  }

  //p.setRenderHints(QPainter::Antialiasing,false);
  // outer border
  {
    QColor color("black");
    float width = b1;
      
    QPen pen(color);
    pen.setWidthF(width);
    p.setPen(pen);
      
    QRectF rect3 = rect.toRect();
    rect3.adjust(0,-b/2,b/2,+b/2);
    
    QRectF rect2;
    
    if (paint_float)
      rect2 = QRectF(rect.x(), rect3.y(), rect.width()+b/2, rect3.height());
    else
      rect2 = rect3;
    
    //printf("Outer border y2: %f\n", rect2.y()+rect2.height());
    p.drawRoundedRect(rect2,round_x,round_y);//rect, round_x, round_y);
  }
    
  // inner border
  {
    QColor color("#222222");
    float width = b2;
      
    QPen pen(color);
    pen.setWidthF(width);
    p.setPen(pen);
      
    QRectF rect3 = rect.toRect();
    rect3.adjust(b,b/2,-b/2,-b/2);
    
    QRectF rect2;
    
    if (paint_float)
      rect2 = QRectF(rect.x() + b, rect3.y(), rect.width() - b - b/2, rect3.height());
    else
      rect2 = rect3;
    
    p.drawRoundedRect(rect2,round_x,round_y);//rect, round_x, round_y);
  }
  //p.setRenderHints(QPainter::Antialiasing,true);
}

static bool in_editor_window(QWidget *widget){
  return g_editor->isVisible() && widget->window()==g_editor->QWidget::window();
}

QPoint mapFromEditor(QWidget *widget, QPoint point){
  QPoint global = in_editor_window(widget)
    ? g_editor->mapToGlobal(point)
    : QPoint(point.x()-10000, point.y()-10000);

  //printf("    G: %d / %d. Mapped: %d / %d\n", global.x(), global.y(), widget->mapFromGlobal(global).x(), widget->mapFromGlobal(global).y());
  return widget->mapFromGlobal(global);
}

QPoint mapToEditor(QWidget *widget, QPoint point){
  if (!in_editor_window(widget))
    return skewedPoint(widget, point, 10000, 10000);
  
  //return widget->mapTo(g_editor, point); (g_editor must be a parent, for some reason)
  QPoint global = widget->mapToGlobal(point);
  return g_editor->mapFromGlobal(global);
}

static double mapToEditorX(QWidget *widget, double x){
  //auto global = widget->mapToGlobal(QPoint(x, 0));
  //return g_editor->mapFromGlobal(global).x();
  return mapToEditor(widget, QPoint(x, 0)).x();
}

static double mapToEditorY(QWidget *widget, double y){
  //auto global = widget->mapToGlobal(QPoint(0, y));
  //return g_editor->mapFromGlobal(global).y();
  return mapToEditor(widget, QPoint(0, y)).y();
}

static double mapToEditorX1(QWidget *widget){
  //auto global = widget->mapToGlobal(QPoint(x, 0));
  //return g_editor->mapFromGlobal(global).x();
  return mapToEditor(widget, QPoint(0, 0)).x();
}

static double mapToEditorY1(QWidget *widget){
  //auto global = widget->mapToGlobal(QPoint(0, y));
  //return g_editor->mapFromGlobal(global).y();
  return mapToEditor(widget, QPoint(0, 0)).y();
}

static double mapToEditorX2(QWidget *widget){
  //auto global = widget->mapToGlobal(QPoint(x, 0));
  //return g_editor->mapFromGlobal(global).x();
  return mapToEditor(widget, QPoint(0, 0)).x() + widget->width();
}

static double mapToEditorY2(QWidget *widget){
  //auto global = widget->mapToGlobal(QPoint(0, y));
  //return g_editor->mapFromGlobal(global).y();
  return mapToEditor(widget, QPoint(0, 0)).y() + widget->height();
}

/*
static double getBlockAbsDuration(const struct Blocks *block){
  return getBlockSTimeLength(block) * ATOMIC_DOUBLE_GET(block->reltempo);
}
*/

static QColor get_seqtrack_background_color(const SeqTrack *seqtrack){
  QColor color = get_qcolor(SEQUENCER_BACKGROUND_COLOR_NUM);
  if (seqtrack->patch!=NULL)
    return mix_colors(color, QColor(seqtrack->patch->color), 0.6);
  else
    return color;
}

static QColor get_block_color(const struct Blocks *block){
  //return mix_colors(QColor(block->color), get_qcolor(SEQUENCER_BLOCK_BACKGROUND_COLOR_NUM), 0.32f);
  return QColor(block->color);
}

static QColor get_sample_color(const SeqTrack *seqtrack, const SeqBlock *seqblock){
  if (seqtrack->patch!=NULL && seqtrack->patch->patchdata!=NULL){
    SoundPlugin *plugin = (SoundPlugin*) seqtrack->patch->patchdata;
    //return QColor(SEQTRACKPLUGIN_get_sample_color(plugin, seqblock->sample_id));
    return QColor(SEQTRACKPLUGIN_get_sample_color(plugin, seqblock->sample_id));
  } else {
#if !defined(RELEASE)
    printf("Qt_seqtrack_widget_callbacks.h: Warning: Could not find patch or patchdata for sample\n");
#endif
    return get_qcolor(SEQUENCER_BLOCK_AUDIO_FILE_BACKGROUND_COLOR_NUM); //QColor("white");
  }
}

static QColor get_seqblock_color(const SeqTrack *seqtrack, const SeqBlock *seqblock){
  if (seqblock->block==NULL)
    return get_sample_color(seqtrack, seqblock);
  else
    return get_block_color(seqblock->block);
}

class MouseTrackerQWidget : public QWidget, public radium::MouseCycleFix {
public:

  bool _is_sequencer_widget;

  MouseTrackerQWidget(QWidget *parent, bool is_sequencer_widget = false)
    : QWidget(parent)
    , _is_sequencer_widget(is_sequencer_widget)
  {
    setMouseTracking(true);
  }
  
  int _currentButton = 0;

  int getMouseButtonEventID( QMouseEvent *qmouseevent){
    if(qmouseevent->button()==Qt::LeftButton)
      return TR_LEFTMOUSEDOWN;
    else if(qmouseevent->button()==Qt::RightButton)
      return TR_RIGHTMOUSEDOWN;
    else if(qmouseevent->button()==Qt::MiddleButton)
      return TR_MIDDLEMOUSEDOWN;
    else
      return 0;
  }

  void	fix_mousePressEvent(QMouseEvent *event) override{
    event->accept();
    
    if (_is_sequencer_widget)
      if (API_run_mouse_press_event_for_custom_widget(SEQUENCER_getWidget(), event))
        return;

    _currentButton = getMouseButtonEventID(event);
    QPoint point = mapToEditor(this, event->pos());
    SCHEME_mousepress(_currentButton, point.x(), point.y());
    //printf("  Press. x: %d, y: %d. This: %p\n", point.x(), point.y(), this);
  }
  void	fix_mouseMoveEvent(QMouseEvent *event) override{
    //printf("sequencer. mouseMove: %d,%d\n", event->x(), event->y());

    event->accept();
    
    if (_is_sequencer_widget)
      if (API_run_mouse_move_event_for_custom_widget(SEQUENCER_getWidget(), event))
        return;

    QPoint point = mapToEditor(this, event->pos());
    SCHEME_mousemove(_currentButton, point.x(), point.y());
    //printf("    move. x: %d, y: %d. This: %p\n", point.x(), point.y(), this);
  }
  void fix_mouseReleaseEvent(radium::MouseCycleEvent &event) override{    
    event.accept();
    
    if (_is_sequencer_widget)
      if (API_run_mouse_release_event_for_custom_widget(SEQUENCER_getWidget(), event))
        return;

    QPoint point = mapToEditor(this, event.pos());
    SCHEME_mouserelease(_currentButton, point.x(), point.y());
    _currentButton = 0;
    //printf("  Release. x: %d, y: %d. This: %p\n", point.x(), point.y(), this);
  }

  MOUSE_CYCLE_CALLBACKS_FOR_QT;
  
  void leaveEvent(QEvent *event) override{
    API_run_mouse_leave_event_for_custom_widget(SEQUENCER_getWidget(), event);
  }
};

static double get_visible_song_length(void){
  return SONG_get_length() + SEQUENCER_EXTRA_SONG_LENGTH;
}

static void handle_wheel_event(QWidget *widget, QWheelEvent *e, int x1, int x2, double start_play_time, double end_play_time) {

  double pos = R_MAX(0, scale_double(e->x(), x1, x2, start_play_time, end_play_time));
  //printf("pos: %f, _start/end: %f / %f. x: %d\n", (double)pos/48000.0, (double)start_play_time / 48000.0, (double)end_play_time / 48000.0, e->x());


  if (  (e->modifiers() & Qt::ControlModifier) || (HorizontalModifierPressed(e->modifiers()))) {

    double nav_start_time = SEQUENCER_get_visible_start_time();
    double nav_end_time = SEQUENCER_get_visible_end_time();
    double visible_song_length = get_visible_song_length() * MIXER_get_sample_rate();
    double new_start,new_end;
    
    double range = nav_end_time - nav_start_time;
    double middle = nav_start_time + range/2.0;
    double how_much = range / 10.0;
     
    if (e->modifiers() & Qt::ControlModifier) {

      // CTRL. Zoom in / out
      
      if (e->delta() > 0) {

        // Zoom in
        
        new_start = nav_start_time + how_much;
        new_end = nav_end_time - how_much;
        
      } else {

        // Zoom out
        
        new_start = nav_start_time - how_much;
        new_end = nav_end_time + how_much;
        
      }

      if (fabs(new_end-new_start) < 400 || new_end<=new_start) {
        
        // Zooming in too much.
        
        new_start = middle - 200;
        new_end = middle + 200;
      }
      
      {
        // Make sure time under mouse pointer is still the same.
        double new_pos = scale_double(e->x(), x1, x2, new_start, new_end);
        double delta = new_pos - pos;
        new_start -= delta;
        new_end -= delta;
      }
      
    } else {

      // Scroll horizontally (meta/osx:alt). Scroll left /right
      
      if (e->delta() > 0) {
        
        new_start = nav_start_time + how_much;
        new_end = nav_end_time + how_much;
        
      } else {
        
        new_start = nav_start_time - how_much;
        new_end = nav_end_time - how_much;
        
      }
      
    }

    if (new_start < 0){
      new_end -= new_start;
      new_start = 0;
    }
    
    if (new_end > visible_song_length){
      double over = new_end - visible_song_length;
      new_end = visible_song_length;
      new_start -= over;
    }
      
    SEQUENCER_set_visible_start_and_end_time(new_start, new_end);

  } else {

    int vu_width = root->song->tracker_windows->systemfontheight;
    if ((e->modifiers() & Qt::MetaModifier) || mapToEditorX(widget, e->x()) < SEQTRACK_get_x1(0)-vu_width) {
        
      int lowest_reasonable_topmost_seqtracknum = root->song->seqtracks.num_elements-1;
      int seqtracknum = root->song->topmost_visible_seqtrack;
      
      if (e->delta() > 0){
        seqtracknum--; 
      } else {
        seqtracknum++;
        lowest_reasonable_topmost_seqtracknum = SEQUENCER_get_lowest_reasonable_topmost_seqtracknum();
        //printf("seqtracknum: %d. lowest: %d. num: %d. \n", seqtracknum, lowest_reasonable_topmost_seqtracknum, root->song->seqtracks.num_elements);
      }
      
      if (seqtracknum >= 0 && seqtracknum <= lowest_reasonable_topmost_seqtracknum)
        setTopmostVisibleSeqtrack(seqtracknum);

    } else {
      
      if (e->delta() > 0)
        PlaySong(pos);
      else {
        if (is_playing_song())
          PlayStop();
        setSongPos(pos);
      }
      
    }
  }
}


static QColor half_alpha(QColor c, Seqblock_Type type) {
  if (type==Seqblock_Type::GFX_GFX)
      c.setAlpha(c.alpha() / 4);      
  return c;
}

static QColor get_block_qcolor(enum ColorNums colornum, Seqblock_Type type) {
  QColor c = get_qcolor(colornum);
  return half_alpha(c, type);
}


class Seqblocks_widget {
public:

  const double _start_time;
  const double _end_time;

  double t_x1,t_y1,t_x2,t_y2,t_width,t_height;
  QRectF _rect;

  Seqblocks_widget(const double start_time, const double end_time, double x1, double y1, double x2, double y2)
    : _start_time(start_time)
    , _end_time(end_time)
    , t_x1 (x1)
    , t_y1 (y1)
    , t_x2 (x2)
    , t_y2 (y2)
    , t_width (x2-x1)    
    , t_height (y2-y1)
    , _rect(QRectF(x1, y1, x2-x1, y2-y1))
  {
  }

  bool seqblock_is_visible(const struct SeqBlock *seqblock) const {
    if (seqblock->t.time >= _end_time)
      return false;
    else if (seqblock->t.time2 < _start_time)
      return false;
    else
      return true;
  }
  
  double get_seqblock_noninterior_x1(const struct SeqBlock *seqblock) const {
    const double interior_start_length = seqblock->t.interior_start * seqblock->t.stretch  * seqblock->t.speed;
    const double noninterior_start = seqblock->t.time - interior_start_length;
    return scale_double(noninterior_start,
                        _start_time, _end_time,
                        t_x1, t_x2);      
  }
  
  double get_seqblock_x1(const struct SeqBlock *seqblock, bool for_update = false) const {
    if (for_update && seqblock->id==g_curr_seqblock_id_under_mouse)
      return get_seqblock_noninterior_x1(seqblock);
    else
      return scale_double(seqblock->t.time, _start_time, _end_time, t_x1, t_x2);
  }

  double get_seqblock_x1(const struct SeqTrack *seqtrack, int seqblocknum) const {
    R_ASSERT_RETURN_IF_FALSE2(seqblocknum>=0, 0);

    // This can happen while the sequencer is updated. (shouldn't happen anymore)
    if (seqblocknum >= gfx_seqblocks(seqtrack)->num_elements){
      R_ASSERT_NON_RELEASE(false);
      return 0;
    }
    
    return get_seqblock_x1((struct SeqBlock*)gfx_seqblocks(seqtrack)->elements[seqblocknum]);
  }

  double get_seqblock_x1(int seqtracknum, int seqblocknum) const {
    R_ASSERT_RETURN_IF_FALSE2(seqtracknum>=0, 0);
    R_ASSERT_RETURN_IF_FALSE2(seqtracknum<root->song->seqtracks.num_elements, 0);

    return get_seqblock_x1((struct SeqTrack*)root->song->seqtracks.elements[seqtracknum], seqblocknum);
  }

  double get_seqblock_noninterior_x2(const struct SeqBlock *seqblock) const {
    const double interior_end_length = (seqblock->t.default_duration - seqblock->t.interior_end) * seqblock->t.stretch * seqblock->t.speed;
    const double noninterior_end = seqblock->t.time2 + interior_end_length;

    return scale_double(noninterior_end,
                        _start_time, _end_time,
                        t_x1, t_x2);

  }
  
  double get_seqblock_x2(const struct SeqBlock *seqblock, bool for_update = false) const {
    if (for_update && seqblock->id==g_curr_seqblock_id_under_mouse)
      return get_seqblock_noninterior_x2(seqblock);
    else
      return scale_double(seqblock->t.time2, _start_time, _end_time, t_x1, t_x2);
  }
 
  double get_seqblock_x2(const struct SeqTrack *seqtrack, int seqblocknum) const {
    R_ASSERT_RETURN_IF_FALSE2(seqblocknum>=0, 0);

    // This can happen while the sequencer is updated. (shouldn't happen anymore)
    if (seqblocknum >= gfx_seqblocks(seqtrack)->num_elements){
      R_ASSERT(false);
      return 100;
    }

    return get_seqblock_x2((struct SeqBlock*)gfx_seqblocks(seqtrack)->elements[seqblocknum]);
  }

  double get_seqblock_x2(int seqtracknum, int seqblocknum) const {
    R_ASSERT_RETURN_IF_FALSE2(seqtracknum>=0, 0);
    R_ASSERT_RETURN_IF_FALSE2(seqtracknum<root->song->seqtracks.num_elements, 0);

    return get_seqblock_x2((struct SeqTrack*)root->song->seqtracks.elements[seqtracknum], seqblocknum);
  }

  QRectF get_seqblock_rect(const struct SeqBlock *seqblock, bool for_update = false) const {
    double x1 = get_seqblock_x1(seqblock, for_update);
    double x2 = get_seqblock_x2(seqblock, for_update);
    
    return QRectF(x1,t_y1+1,x2-x1,t_height-2);
  }

  QRectF get_seqblock_update_rect(const struct SeqBlock *seqblock, bool force_current) const {
    if (force_current || is_current_seqblock(seqblock))
      return get_current_seqblock_rect(seqblock, true);
    else
      return get_seqblock_rect(seqblock, true);
  }
  
  void update_seqblock(const struct SeqBlock *seqblock, bool force_current = false, int64_t start_time = -1, int64_t end_time = -1) const {
    if (seqblock_is_visible(seqblock)){
      QRectF rect = get_seqblock_update_rect(seqblock, force_current);

#if !defined(RELEASE)
      if(end_time >= 0 && start_time>=0 && end_time < start_time)
        abort();
#endif
      
      if(start_time >= 0){
        
        double x1 = R_BOUNDARIES(t_x1,
                                 scale_double(start_time,
                                              _start_time, _end_time,
                                              t_x1, t_x2)
                                 - get_min_node_size(),
                                 t_x2);

        rect.setX(x1);

      }

      if (end_time >= 0){
        double x2 = R_BOUNDARIES(t_x1,
                                 scale_double(end_time,
                                              _start_time, _end_time,
                                              t_x1, t_x2)
                                 + get_min_node_size(),
                                 t_x2);
        rect.setWidth(x2 - rect.x());                                 
      }

      if (seqblock->id==g_curr_seqblock_id_under_mouse){ // && !is_current_seqblock(seqblock)){
        float min_node_size = get_min_node_size();
        rect.adjust(-min_node_size,0,min_node_size,min_node_size); // Seqblock nodes can be painted outside the seqblock rectangle.
      }

      my_update_sequencer_widget(rect.toAlignedRect());
    }
  }
  
  void set_seqblocks_is_selected(const struct SeqTrack *seqtrack, const QRect &selection_rect){
    VECTOR_FOR_EACH(struct SeqBlock *, seqblock, gfx_seqblocks(seqtrack)){

      bool new_val = false;
      
      if (seqblock_is_visible(seqblock)){
        QRectF rect = get_seqblock_rect(seqblock);
        new_val = rect.intersects(selection_rect);
      }

      if (new_val != seqblock->is_selected) {
        update_seqblock(seqblock);
        seqblock->is_selected = new_val;
      }
      
    }END_VECTOR_FOR_EACH;
     
  }

  void paintEditorTrack(QPainter &p, double x1, double y1, double x2, double y2, const struct SeqBlock *seqblock, const struct Blocks *block, const struct Tracks *track, int64_t blocklen, bool is_multiselected) const {
    QColor color1 = get_qcolor(SEQUENCER_NOTE_COLOR_NUM);
    QColor color2 = get_qcolor(SEQUENCER_NOTE_START_COLOR_NUM);
    
#define SHOW_BARS 0

#if SHOW_BARS
    p.setBrush(QBrush(color));
#else
    const double bar_height = (double)root->song->tracker_windows->systemfontheight / 6.5;
    const double bar_header_length = (double)root->song->tracker_windows->systemfontheight / 4.7;
    
    QPen pen1(color1);
    pen1.setWidthF(bar_height);
    pen1.setCapStyle(Qt::FlatCap);

    QPen pen2(pen1);
    pen2.setColor(color2);

    {
      QColor color;

      if (is_multiselected)
        color = get_block_qcolor(SEQUENCER_BLOCK_MULTISELECT_BACKGROUND_COLOR_NUM, Seqblock_Type::GFX_GFX);

      else if (!g_draw_colored_seqblock_tracks)
        color = get_block_qcolor(SEQUENCER_BLOCK_BACKGROUND_COLOR_NUM, Seqblock_Type::REGULAR);
      
      else if (track->patch!=NULL)
        color = QColor(track->patch->color);

      else
        goto no_track_background;
      
      QRectF rect(x1,y1,x2-x1,y2-y1);

      myFillRect(p, rect, color);
    }
    
  no_track_background:
    
#endif

    float track_pitch_min;
    float track_pitch_max;
    TRACK_get_min_and_max_pitches(track, &track_pitch_min, &track_pitch_max);
    
    struct Notes *note = track->gfx_notes!=NULL ? track->gfx_notes : track->notes;
    while(note != NULL){

      struct Pitches last_pitch;
      last_pitch.l.p = note->end;
      last_pitch.note = note->pitch_end;
      
      struct Pitches first_pitch;
      first_pitch.l.p = note->l.p;
      first_pitch.l.next = note->pitches==NULL ? NULL : &note->pitches->l;
      first_pitch.note = note->note;
      first_pitch.logtype = note->pitch_first_logtype;
      
      struct Pitches *pitch = &first_pitch;

      const double init_last_y = -10000;
      double last_y = init_last_y;
      
      while(true){

        struct Pitches *next_pitch = NextPitch(pitch);
        if(next_pitch==NULL)
          next_pitch = &last_pitch;
        
        int64_t start = Place2STime(block, &pitch->l.p);
        int64_t end = Place2STime(block, &next_pitch->l.p);
        
        double n_x1 = scale_double(start, 0, blocklen, x1, x2);
        double n_x2 = scale_double(end, 0, blocklen, x1, x2);
        
        p.setPen(pen1);
        
        double n_y1 = track_pitch_max==track_pitch_min ? (y1+y2)/2.0 : scale_double(pitch->note+0.5, track_pitch_max, track_pitch_min, y1, y2);
        double n_y2;

        if(track_pitch_max==track_pitch_min)
          n_y2 = (y1+y2)/2.0;
        else if (next_pitch==&last_pitch && next_pitch->note==0)
          n_y2 = scale_double(note->note+0.5, track_pitch_max, track_pitch_min, y1, y2);
        else if (pitch->logtype==LOGTYPE_HOLD)
          n_y2 = n_y1;
        else
          n_y2 = scale_double(next_pitch->note+0.5, track_pitch_max, track_pitch_min, y1, y2);
                 

        if (last_y == init_last_y){
          
          double x2 = R_MIN(n_x2, n_x1+bar_header_length);
          
          QLineF line(n_x1,n_y1,x2,n_y1);
          p.setPen(pen2);
          p.drawLine(line);
          
          if (n_x2 > x2){
            QLineF line(x2,n_y1,n_x2,n_y2);
            p.setPen(pen1);
            p.drawLine(line);
          }
          
        } else {
          
          p.setPen(pen1);
          
          if (fabs(last_y-n_y1) > 0.5){
            QLineF line(n_x1,last_y, n_x1, n_y1);
            p.drawLine(line);
          }
          
          QLineF line(n_x1,n_y1,n_x2,n_y2);
          p.drawLine(line);
        }

        last_y = n_y2;
        
        if (next_pitch==&last_pitch)
          break;
        else
          pitch = next_pitch;
      }
      
#undef SHOW_BARS
      
      note = NextNote(note);
    }

    if (track->l.num < MAX_DISABLED_SEQBLOCK_TRACKS){
      if (seqblock->track_is_disabled[track->l.num]){
        QPen pen1(QColor(250,250,250));
        pen1.setWidthF((double)root->song->tracker_windows->systemfontheight / 6.52);
        pen1.setCapStyle(Qt::FlatCap);
        p.setPen(pen1);
        
        QLineF line1(x1, y1, x2, y2);
        QLineF line2(x1, y2, x2, y1);
        p.drawLine(line1);
        p.drawLine(line2);
      }
    }
      
  }
  
  void drawWaveform(QPainter &p, const QRegion &update_region, const SoundPlugin *plugin, radium::Peaks **peaks, const struct SeqBlock *seqblock, Seqblock_Type type, const QColor &color, int64_t time1, int64_t time2, double x1, double x2, double w_y1, double w_y2) const {

    if (false==workingQRegionIntersects(update_region, QRectF(x1, w_y1, x2-x1, w_y2-w_y1))){
      //printf("   drawWaveform: No intersection\n");
      return;
    }
      
    QRect update_rect = update_region.boundingRect();

    double u_x1 = R_MAX(t_x1, update_rect.x());
    double u_x2 = R_MIN(t_x2, update_rect.x() + update_rect.width());

    /*
    if(u_x1 != t_x1)
      printf("   drawWaveform:    Increased u_x1 from %f to %f\n", t_x1, u_x1);

    if(u_x2 != t_x2)
      printf("   drawWaveform:    Decreased u_x2 from %f to %f\n", t_x2, u_x2);
    */
    
    if (x1 >= u_x2)
      return;

    if (x2 < u_x1)
      return;

    if (time1 >= time2){
      R_ASSERT_NON_RELEASE(time1-2 <= time2);
      return;
    }

    if (x1 < u_x1) { // if seqblock starts before visible area
      time1 = R_SCALE(u_x1,
                      x1, x2,
                      time1, time2);
      x1 = u_x1;
    }
    
    if (x2 > t_x2){ // if seqblock ends after visible area
      time2 = R_SCALE(u_x2,
                      x1, x2,
                      time1, time2);
      x2 = u_x2;
    }

    R_ASSERT_NON_RELEASE(time2 >= time1);
        
    if (time2 <= time1)
      return;    

    int64_t p_time2 = get_stretch_automation_sample_pos(seqblock, type==Seqblock_Type::RECORDING ? time2 : R_MIN(seqblock->t.num_samples, time2));

    const double pixels_per_peak = R_MAX(2.7, (double)root->song->tracker_windows->systemfontheight / 6.5);
    double width = x2-x1;

    int num_ch = SEQTRACKPLUGIN_get_num_channels(plugin, seqblock->sample_id);

    int num_points = R_MAX(4, width / pixels_per_peak);
    if ((num_points % 2) != 0)
      num_points++;

    const float sample_gain = seqblock->gain;
    
    for(int ch=0;ch<num_ch;ch++){

      double y1 = scale_double(ch, 0, num_ch, w_y1, w_y2);
      double y2 = scale_double(ch+1, 0, num_ch, w_y1, w_y2);

      QPointF points[num_points*2];

      int64_t p_last_used_end_time = get_stretch_automation_sample_pos(seqblock, time1);

      const float m = 0.3f; // half minimum waveform height (to avoid blank gfx for silent audio)
          
      bool has_data = true;
      double min_y = 0.0;
      double max_y = 0.0;

      if(true)
      for(int i=1;i<=num_points;i++){

        if (has_data){
              
          int64_t p_start_time = p_last_used_end_time;

          int64_t end_time1 = scale_int64(i,
                                          0, num_points,
                                          time1, time2);
          int64_t p_end_time2 = get_stretch_automation_sample_pos(seqblock, end_time1);
          int64_t p_end_time = R_BOUNDARIES(p_start_time,
                                            p_end_time2,
                                            p_time2);

          //printf("%d: %f -> %f (1: %d. 2: %d. time1: %d, time2: %d. p_time2: %d)\n", i, (double)p_start_time / pc->pfreq, (double)p_end_time / pc->pfreq, (int)end_time1, (int)p_end_time2, (int)time1, (int)time2, (int)p_time2);

          R_ASSERT_NON_RELEASE(p_end_time >= p_start_time);

          if (p_end_time > p_start_time) {
                
            const radium::Peak peak = peaks[ch]->get(p_start_time, p_end_time);

            double min,max;
                
            if (peak.has_data()==true) {
              min = peak.get_min() * sample_gain;
              max = peak.get_max() * sample_gain;
            } else {
              min = 0.0f;
              max = 0.0f;
              has_data = false; // I.e. waveform is not finished loading. (SEQUENCER_update() is called often while loading, and also when finished loading).
            }

            min_y = scale_double(min, -1, 1, y1+m, y2   ) - m;
            max_y = scale_double(max, -1, 1, y1,   y2-m ) + m;

            //if (i > 160)
            //  printf("   %d: min: %f, y1: %f, y2: %f\n", i, min, y1, y2);
            p_last_used_end_time = p_end_time;
          }
              
        }

        double x = scale_double(i,
                                1,num_points,
                                x1, x2);
            
        points[i-1].setX(x);
        points[i-1].setY(min_y);

        points[num_points*2-i].setX(x);
        points[num_points*2-i].setY(max_y);

        //if (i > 160){
        //  printf("%d: %f, %f -> %f, %f. num_points: %d, x1,x2: %f, %f\n", i, x, min_y, x, max_y, num_points, x1, x2);
        //}
      }

      myFilledPolygon(p, points, num_points*2, color);
    }


    //printf("  DRAW: %f -> %f\n", (double)time1/pc->pfreq, (double)time2/pc->pfreq);
  }

  
  void paintSampleGraphics(QPainter &p, const QRegion &update_region, const QRectF &rect, const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock, Seqblock_Type type) const {
    const int header_height = get_block_header_height();

    QColor waveform_color = get_block_qcolor(SEQUENCER_WAVEFORM_COLOR_NUM, type);
    QColor background_color = get_sample_color(seqtrack, seqblock).lighter(200);
    if (type==Seqblock_Type::GFX_GFX)
      background_color = half_alpha(mix_colors(background_color, get_qcolor(SEQUENCER_BLOCK_MULTISELECT_BACKGROUND_COLOR_NUM), 0.5), type);
    else
      background_color = mix_colors(background_color, get_seqtrack_background_color(seqtrack), 0.25).lighter(150);

    background_color.setAlpha(180);
    
    myFillRect(p, rect.adjusted(0,header_height,0,0), background_color);

    const SoundPlugin *plugin = (SoundPlugin*) seqtrack->patch->patchdata;
    //    R_ASSERT(plugin!=NULL); // Commented out. Plugin can be NULL during loading.

    if (plugin != NULL){

      // Solution: Paint 3 waveforms:
      // 1. Interior start (in a lot more transparent color)
      // 2. The thing
      // 3. Interior end (in a lot more transparent color)
      
      radium::Peaks **peaks = SEQTRACKPLUGIN_get_peaks(plugin, seqblock->sample_id);
      if (peaks != NULL){

        const double resample_ratio = SEQTRACKPLUGIN_get_resampler_ratio(plugin, seqblock->sample_id);

        const double x1 = rect.x();
        const double x2 = rect.x() + rect.width();
        const int64_t time1 = seqblock->t.interior_start / resample_ratio;
        const int64_t time2 = seqblock->t.interior_end / resample_ratio;

        const double y1 = rect.y() + header_height;
        const double y2 = rect.y() + rect.height();

        if (seqblock->id==g_curr_seqblock_id_under_mouse){

          QColor interior_waveform_color = waveform_color;
          interior_waveform_color.setAlpha(70);

          if (seqblock->t.interior_start>0) {
            double i_x1 = get_seqblock_noninterior_x1(seqblock);
            double i_time1 = 0;

            /*
            // This optimization is already done in drawWaveform.
            if (i_x1 < t_x1) {
              i_time1 = scale_double(t_x1,
                                     i_x1, x1,
                                     i_time1, time1);
              printf("Interior start: Scaling down to %f. i_x1: %f, t_x1: %f\n", i_time1, i_x1, t_x1);
              i_x1 = t_x1;
            }
            */
            
            drawWaveform(p, update_region,
                         plugin, peaks, seqblock, type, interior_waveform_color,
                         i_time1, time1,
                         i_x1, x1,
                         y1, y2);
          }
          
          if (seqblock->t.interior_end!=seqblock->t.default_duration) {

            double i_x2 = get_seqblock_noninterior_x2(seqblock);
            double i_time2 = seqblock->t.default_duration / resample_ratio;

            /*
            // This optimization is already done in drawWaveform.
            if (i_x2 > t_x2) {
              i_time2 = scale_double(t_x2,
                                     x2, i_x2,
                                     time2, i_time2);
              printf("Interior end: Scaling down to %f. i_x2: %f, t_x2: %f\n", i_time2, i_x2, t_x2);
              i_x2 = t_x2;
            }
            */
            
            drawWaveform(p, update_region,
                         plugin, peaks, seqblock, type, interior_waveform_color,
                         time2, i_time2,
                         x2, i_x2,
                         y1, y2);

          }
          
        }

        drawWaveform(p, update_region,
                     plugin, peaks, seqblock, type, waveform_color,
                     time1, time2,
                     x1, x2,
                     y1, y2);
     
      }
    }
  }

  void paintBlockGraphics(QPainter &p, const QRectF &rect, const struct SeqBlock *seqblock, Seqblock_Type type) const {
    R_ASSERT(seqblock->block != NULL);
    
    const struct Blocks *block = seqblock->block;

    const int header_height = get_block_header_height();
    
    QColor track_border_color  = get_block_qcolor(SEQUENCER_TRACK_BORDER2_COLOR_NUM, type);

    QPen track_border_pen(track_border_color);

    track_border_pen.setWidthF((double)root->song->tracker_windows->systemfontheight / 11.54);

    qreal x1,y1,x2,y2;
    rect.getCoords(&x1, &y1, &x2, &y2);

    int64_t blocklen = getBlockSTimeLength(block);

    // Tracks
    {
      int num_tracks = block->num_tracks;
      
      struct Tracks *track = block->tracks;
      while(track != NULL){
        
        double t_y1 = scale_double(track->l.num,0,num_tracks,y1+header_height,y2);
        double t_y2 = scale_double(track->l.num+1,0,num_tracks,y1+header_height,y2);

        // Draw track border
        if (track->l.num > 0){
          p.setPen(track_border_pen);
          p.drawLine(QLineF(x1,t_y1,x2,t_y1));
        }
        
        // Draw track
        paintEditorTrack(p, x1, t_y1, x2, t_y2, seqblock, block, track, blocklen, type==Seqblock_Type::GFX_GFX);
        
        track = NextTrack(track);
      }
    }

    // Bar and beats
    if(1){
      const struct Beats *beat = block->beats;
      QColor bar_color = get_block_qcolor(SEQUENCER_BLOCK_BAR_COLOR_NUM, type);
      QColor beat_color = get_block_qcolor(SEQUENCER_BLOCK_BEAT_COLOR_NUM, type);
      QPen bar_pen(bar_color);
      QPen beat_pen(beat_color);

      bar_pen.setWidthF((double)root->song->tracker_windows->systemfontheight / 11.54);
      beat_pen.setWidthF((double)root->song->tracker_windows->systemfontheight / 11.54);

      if (beat!=NULL)
        beat = NextBeat(beat);
      
      while(beat != NULL){
        
        double b_y1 = y1+header_height;
        double b_y2 = y2;

        double pos = Place2STime(block, &beat->l.p);
        
        double b_x = scale_double(pos, 0, blocklen, x1, x2);

        if (beat->beat_num==1)
          p.setPen(bar_pen);
        else
          p.setPen(beat_pen);

        QLineF line(b_x, b_y1, b_x, b_y2);
        p.drawLine(line);
        
        beat = NextBeat(beat);
      }
    }
  }

  void paintSeqblockText(QPainter &p, const QRectF &rect, const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock, Seqblock_Type type, bool scale_text) const {

    QColor text_color = get_block_qcolor(SEQUENCER_TEXT_COLOR_NUM, type);

    // background
    QColor header_color = get_seqblock_color(seqtrack, seqblock);//.lighter(150);
    header_color.setAlpha(128);
    myFillRect(p, rect, header_color); //half_alpha(header_color, type));

    // name
    p.setPen(text_color);
    myDrawText(&p, rect.adjusted(2,0,-4,0), get_seqblock_name(seqtrack, seqblock),
               Qt::AlignLeft | Qt::AlignVCenter,
               0, // rotate
               false, // wrap
               scale_text, // scale
               true); // cut text to fit
  }
  
  void paintSeqblockHeader(QPainter &p, const QRectF &rect, const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock, Seqblock_Type type) const {
    const int header_height = get_block_header_height();

    QColor header_border_color = get_block_qcolor(SEQUENCER_TRACK_BORDER1_COLOR_NUM, type);
    QColor border_color = get_block_qcolor(SEQUENCER_BLOCK_BORDER_COLOR_NUM, type);

    QPen header_border_pen(header_border_color);
    header_border_pen.setWidthF((double)root->song->tracker_windows->systemfontheight / 6.52);

    qreal x1,y1,x2,y2;
    rect.getCoords(&x1, &y1, &x2, &y2);

    // Draw track header
    {
      QRectF rect1(x1, y1, x2-x1, header_height);
      paintSeqblockText(p, rect1, seqtrack, seqblock, type, false);
    }

    // horizontal line between header and body
    {
      p.setPen(header_border_pen);
      p.drawLine(QLineF(x1,y1+header_height,x2,y1+header_height));
    }
    
    // Seqblock border
    {
      bool is_current_block = seqblock->block!=NULL && seqblock->block == root->song->tracker_windows->wblock->block;

      if (is_current_block){
        QColor c = get_block_qcolor(CURSOR_EDIT_ON_COLOR_NUM, type);
        c.setAlpha(150);      
        p.setPen(QPen(c, 4));
      } else {
        p.setPen(border_color);
      }

      p.drawRoundedRect(rect,1,1);
    }

  }
  
  void draw_seqblock_automations(const radium::AutomationPainter **automation_painters, unsigned int what_to_paint, QPainter *painter, const QRectF &rect, const struct SeqBlock *seqblock){
    int num_automations = SEQBLOCK_num_automations(seqblock);

    for(int i=0 ; i<num_automations;i++){

      struct SeqblockAutomation *automation = seqblock->automations[i];

      if (RT_seqblock_automation_is_enabled(automation)){
        qreal x1,y1,x2,y2;
        rect.getCoords(&x1, &y1, &x2, &y2);

        double noninterior_start = get_seqblock_noninterior_start(seqblock);
        double noninterior_end = get_seqblock_noninterior_end(seqblock);

        qreal ni_x1 = scale_double(noninterior_start,
                                   _start_time, _end_time,
                                   t_x1, t_x2);
        qreal ni_x2 = scale_double(noninterior_end,
                                   _start_time, _end_time,
                                   t_x1, t_x2);
      
        bool draw_all = seqblock->t.interior_start==0 && seqblock->t.interior_end==seqblock->t.default_duration;
        bool is_current = seqblock->id==g_curr_seqblock_id_under_mouse;
        //printf("draw_all: %d. is_current: %d. x1: %f, x1: %f, x2: %f, x2: %f\n", draw_all, is_current, x1, ni_x1, x2, ni_x2);

        const QRegion org_clip = painter->clipRegion();

        bool has_custom_clip = false;

        if(automation_painters[i]==NULL)
          automation_painters[i] = SEQBLOCK_AUTOMATION_get_painter(automation, ni_x1, y1, ni_x2, y2, true, x1, x2);

        const auto *automation_painter = automation_painters[i];
        
        // 1. (before start_interior and after end_interior)
        if(is_current && !draw_all){

          qreal clip_x1 = R_MAX(t_x1, ni_x1);
          
          QRegion clip(clip_x1, y1, x1-clip_x1, rect.height());

          clip_x1 = R_MAX(t_x1, x2);
          clip = clip.united(QRect(clip_x1, y1, ni_x2-clip_x1, rect.height()));
          
          painter->setClipRegion(clip);
          has_custom_clip = true;

          {
            painter->setOpacity(0.05);
            
            automation_painter->paint(painter, what_to_paint);
            
            painter->setOpacity(1.0);
          }
        }

        // 2. (rect)
        if(1){
          if(!draw_all){
            
            qreal clip_x1 = R_MAX(t_x1, rect.x());
            qreal clip_x2 = rect.x() + rect.width();
            
            QRectF cliprect(clip_x1, rect.y(), clip_x2-clip_x1, rect.height());
            painter->setClipRect(cliprect);
            has_custom_clip = true;
          }

          automation_painter->paint(painter, what_to_paint);
        }

        if(has_custom_clip)
          painter->setClipRegion(org_clip);
      }      
    }
  }

#if 0
    // Now drawn in seqblock-painter.scm
  void draw_interface(QPainter *painter, const struct SeqBlock *seqblock, const QRectF &_blury_areaF){
    qreal x1,y1,x2,y2;
    _blury_areaF.getCoords(&x1, &y1, &x2, &y2);

    double border = 2.1;
    
    double xsplit1 = get_seqblock_xsplit1(x1, x2);
    double xsplit2 = get_seqblock_xsplit2(x1, x2);

    double ysplit1 = get_seqblock_ysplit1(y1, y2);
    double ysplit2 = get_seqblock_ysplit2(y1, y2);
    double ysplit3 = get_seqblock_ysplit3(y1, y2);

    double width = (double)root->song->tracker_windows->systemfontheight / 4;
    double sel_width = width*2;

    QColor color = get_qcolor(SEQUENCER_BLOCK_INTERFACE_COLOR_NUM);
    QColor fill_color(color);
    fill_color.setAlpha(80);

    QPen pen(color);
    pen.setWidthF(width);

    QPen sel_pen(color);
    sel_pen.setWidthF(sel_width);

    /* Vertical lines (Ys) */
    //////////////////////////
    
    // fade
    {
      if (seqblock->selected_box==SB_FADE_LEFT) painter->setPen(sel_pen);  else  painter->setPen(pen);
      QLineF line1a(xsplit1, y1 + border,
                    xsplit1, ysplit1);
      painter->drawLine(line1a);

      if (seqblock->selected_box==SB_FADE_RIGHT) painter->setPen(sel_pen);  else  painter->setPen(pen);
      QLineF line2a(xsplit2,y1 + border,xsplit2,ysplit1);
      painter->drawLine(line2a);

    }


    if (seqblock->block==NULL){

      // interior
      {
        if (seqblock->selected_box==SB_INTERIOR_LEFT) painter->setPen(sel_pen);  else  painter->setPen(pen);
        QLineF line1b(xsplit1,ysplit1,
                      xsplit1,ysplit2);
        painter->drawLine(line1b);
        
        // xsplit1 vertical line 2, right
        if (seqblock->selected_box==SB_INTERIOR_RIGHT) painter->setPen(sel_pen);  else  painter->setPen(pen);
        QLineF line2b(xsplit2,ysplit1,
                    xsplit2,ysplit2);
        painter->drawLine(line2b);
      }
      
      // speed
      {
        if (seqblock->selected_box==SB_SPEED_LEFT) painter->setPen(sel_pen);  else  painter->setPen(pen);
        QLineF line1b(xsplit1,ysplit2,
                      xsplit1,ysplit3);
        painter->drawLine(line1b);
        
        // xsplit1 vertical line 2, right
        if (seqblock->selected_box==SB_SPEED_RIGHT) painter->setPen(sel_pen);  else  painter->setPen(pen);
        QLineF line2b(xsplit2,ysplit2,
                      xsplit2,ysplit3);
        painter->drawLine(line2b);
      }
    }

    // stretch
    {
      if (seqblock->selected_box==SB_STRETCH_LEFT) painter->setPen(sel_pen);  else  painter->setPen(pen);
      QLineF line1c(xsplit1,ysplit3,xsplit1,y2-border);
      painter->drawLine(line1c);
      
      if (seqblock->selected_box==SB_STRETCH_RIGHT) painter->setPen(sel_pen);  else  painter->setPen(pen);
      QLineF line2c(xsplit2,ysplit3,xsplit2,y2-border);
      painter->drawLine(line2c);
    }

    
    /* Horizontal lines (Xs) */
    //////////////////////////

    // ysplit1 (fade | interior)
    {
      // left
      if (seqblock->selected_box==SB_FADE_LEFT || seqblock->selected_box==SB_INTERIOR_LEFT) painter->setPen(sel_pen);  else  painter->setPen(pen);
      QLineF line3a(x1+border, ysplit1, xsplit1-border, ysplit1);
      painter->drawLine(line3a);
      
      // right
      if (seqblock->selected_box==SB_FADE_RIGHT || seqblock->selected_box==SB_INTERIOR_RIGHT) painter->setPen(sel_pen);  else  painter->setPen(pen);
      QLineF line3b(xsplit2+border, ysplit1, x2-border, ysplit1);
      painter->drawLine(line3b);
    }
    
    // ysplit2 (interior | speed)
    if(seqblock->block==NULL){
      // left
      if (seqblock->selected_box==SB_INTERIOR_LEFT || seqblock->selected_box==SB_SPEED_LEFT) painter->setPen(sel_pen);  else  painter->setPen(pen);
      QLineF line4a(x1+border, ysplit2, xsplit1-border, ysplit2);
      painter->drawLine(line4a);
    
      // right
      if (seqblock->selected_box==SB_INTERIOR_RIGHT || seqblock->selected_box==SB_SPEED_RIGHT) painter->setPen(sel_pen);  else  painter->setPen(pen);
      QLineF line4b(xsplit2+border, ysplit2, x2-border, ysplit2);
      painter->drawLine(line4b);
    }
    
    // ysplit3 (speed | stretch)
    {
      // left
      if (seqblock->selected_box==SB_SPEED_LEFT || seqblock->selected_box==SB_STRETCH_LEFT) painter->setPen(sel_pen);  else  painter->setPen(pen);
      QLineF line4a(x1+border, ysplit3, xsplit1-border, ysplit3);
      painter->drawLine(line4a);

      // right
      if (seqblock->selected_box==SB_STRETCH_RIGHT || seqblock->selected_box==SB_STRETCH_RIGHT) painter->setPen(sel_pen);  else  painter->setPen(pen);
      QLineF line4b(xsplit2+border, ysplit3, x2-border, ysplit3);
      painter->drawLine(line4b);
    }

    
    // fill selected box
    //
    {
      border *= 0.5;
      
      if (false) {

        // fade
        /////////
      } else if (seqblock->selected_box==SB_FADE_LEFT){
        myFillRect(*painter,
                   QRectF(QPointF(x1+border, y1+border),
                          QPointF(xsplit1, ysplit1)),
                   fill_color);
      } else if (seqblock->selected_box==SB_FADE_RIGHT){
        myFillRect(*painter,
                   QRectF(QPointF(xsplit2, y1+border),
                          QPointF(x2-border, ysplit1)),
                   fill_color);

        // interior
        ///////////
      } else if (seqblock->selected_box==SB_INTERIOR_LEFT){
        myFillRect(*painter,
                   QRectF(QPointF(x1+border, ysplit1),
                          QPointF(xsplit1, ysplit2)),
                   fill_color);
      } else if (seqblock->selected_box==SB_INTERIOR_RIGHT){
        myFillRect(*painter,
                   QRectF(QPointF(xsplit2, ysplit1),
                          QPointF(x2-border, ysplit2)),
                   fill_color);

        // speed
        ///////////
      } else if (seqblock->selected_box==SB_SPEED_LEFT){
        myFillRect(*painter,
                   QRectF(QPointF(x1+border, ysplit2),
                          QPointF(xsplit1, ysplit3)),
                   fill_color);
      } else if (seqblock->selected_box==SB_SPEED_RIGHT){
        myFillRect(*painter,
                   QRectF(QPointF(xsplit2, ysplit2),
                          QPointF(x2-border, ysplit3)),
                   fill_color);
      
        // stretch
        ///////////
      } else if (seqblock->selected_box==SB_STRETCH_LEFT){
        myFillRect(*painter,
                   QRectF(QPointF(x1+border, ysplit3),
                          QPointF(xsplit1, y2-border)),
                   fill_color);
      } else if (seqblock->selected_box==SB_STRETCH_RIGHT){
        myFillRect(*painter,
                   QRectF(QPointF(xsplit2, ysplit3),
                          QPointF(x2-border, y2-border)),
                   fill_color);
      }
    }
  }
#endif

  void paintSelected(QPainter &p, const QRectF &rect, const struct SeqBlock *seqblock, Seqblock_Type type) const {
    //printf("Seqblock: %p, %d\n", seqblock, seqblock->is_selected);
    if (seqblock->is_selected){
      QColor grayout_color = get_block_qcolor(SEQUENCER_BLOCK_SELECTED_COLOR_NUM, type);
      
      p.setPen(Qt::NoPen);
      p.setBrush(grayout_color);
      
      p.drawRect(rect);
      
      p.setBrush(Qt::NoBrush);
    }
  }
      
  void draw_fades(QPainter &p, const QRectF &rect, const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock) const {
    QColor color = get_seqtrack_background_color(seqtrack); //get_qcolor(SEQUENCER_BACKGROUND_COLOR_NUM); //mix_colors(QColor(50,50,50,200), get_qcolor(SEQUENCER_BACKGROUND_COLOR_NUM), 0.52f);
    color.setAlpha(180);
    //QColor color(50,50,50,200);

    QColor border_color(150,150,160);

    QPen pen(border_color);
    pen.setWidth((double)root->song->tracker_windows->systemfontheight / 8);

    QPen sel_pen(border_color);
    sel_pen.setWidthF((double)root->song->tracker_windows->systemfontheight / 3);

    if (seqblock->fadein > 0){
      double fade_x = scale_double(seqblock->fadein, 0, 1, rect.left(), rect.right());
#if 0
      const int num_points = 3;
      QPointF points[num_points] = {
        QPointF(rect.left(),
                rect.top()),
        QPointF(fade_x,
                rect.top()),
        QPointF(rect.left(),
                rect.bottom())
      };
      myFilledPolygon(p, points, num_points, color);
      if (seqblock->selected_box==SB_FADE_LEFT) p.setPen(sel_pen);  else  p.setPen(pen);
      p.drawLine(points[1], points[2]);
#else
      float *xs, *ys;
      
      int num_points = seqblock->fade_in_envelope->getPoints2(&xs, &ys);
      
      QPointF points[num_points+2];
      
      for(int i=0;i<num_points;i++){
        points[i].setX(scale(xs[i], 0, 1, rect.left(), fade_x));
        points[i].setY(scale(ys[i], 0, 1, rect.bottom(), rect.top()));
      }
      
      points[num_points].setX(fade_x);
      points[num_points].setY(rect.top());
      
      points[num_points+1].setX(rect.left());
      points[num_points+1].setY(rect.top());
      
      myFilledPolygon(p, points, num_points+2, color);
      if (seqblock->selected_box==SB_FADE_LEFT) p.setPen(sel_pen);  else  p.setPen(pen);
      p.drawPolyline(points, num_points);
      //p.drawLine(points[1], points[2]);
#endif
    }

      
    if (seqblock->fadeout > 0){
      double fade_x = scale_double(seqblock->fadeout, 0, 1, rect.right(), rect.left());
#if 0
      QPointF points[3] = {
        QPointF(rect.right(),
                rect.top()),
        QPointF(fade_x,
                rect.top()),
        QPointF(rect.right(),
                rect.bottom())
      };
      myFilledPolygon(p, points, 3, color);
      if (seqblock->selected_box==SB_FADE_RIGHT) p.setPen(sel_pen);  else  p.setPen(pen);
      p.drawLine(points[1], points[2]);
#else
      float *xs, *ys;
      
      int num_points = seqblock->fade_out_envelope->getPoints2(&xs, &ys);
      
      QPointF points[num_points+2];
      
      for(int i=0;i<num_points;i++){
        points[i].setX(scale(xs[i], 0, 1, fade_x, rect.right()));
        points[i].setY(scale(ys[i], 0, 1, rect.bottom(), rect.top()));
      }
      
      points[num_points].setX(rect.right());
      points[num_points].setY(rect.top());
      
      points[num_points+1].setX(fade_x);
      points[num_points+1].setY(rect.top());
      
      myFilledPolygon(p, points, num_points+2, color);
      if (seqblock->selected_box==SB_FADE_RIGHT) p.setPen(sel_pen);  else  p.setPen(pen);
      p.drawPolyline(points, num_points);
      //p.drawLine(points[1], points[2]);
#endif
    }
  }

  void paintSeqBlockElements(QPainter &p, const QRegion &update_region, const QRectF &rect, const QRectF &rect_without_header, const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock, Seqblock_Type type) const {
    if (seqblock->block==NULL)
      paintSampleGraphics(p, update_region, rect, seqtrack, seqblock, type);
    else
      paintBlockGraphics(p, rect, seqblock, type);
    
    paintSelected(p, rect, seqblock, type);
    
    paintSeqblockHeader(p, rect, seqtrack, seqblock, type);

    draw_fades(p, rect_without_header, seqtrack, seqblock);
  }

  QRectF get_current_seqblock_rect(const struct SeqBlock *seqblock, bool for_update) const {
    double x1 = get_seqblock_x1(seqblock, for_update);
    double x2 = get_seqblock_x2(seqblock, for_update);
    float b = get_seqtrack_border_width();

    if (for_update)
      return QRectF(x1 - b-1,        t_y1 - b,
                    x2 - x1 + 2*b+2, t_y2 - t_y1 + b*2);
    else
      return QRectF(x1 - b,              t_y1 - b/2.0,
                    x2 - x1 + b + b/2.0, t_y2 - t_y1 + b);
  }
  
  void paint_current_seqblock_border(QPainter &p, const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock, bool paint_float) const {
    //printf("Seqblock: ");
    if(get_seqtracknum(seqtrack) >= root->song->topmost_visible_seqtrack)
      paintCurrBorder(p, get_current_seqblock_rect(seqblock, false), QColor(0,0x60,0x90,0xb0), paint_float, 0, 0);
  }
  
  bool paintSeqBlock(QPainter &p, const QRegion &update_region, const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock, int seqtracknum, int seqblocknum, Seqblock_Type type){
    //QPoint mousep = _sequencer_widget->mapFromGlobal(QCursor::pos());

    radium::ScopedBoolean scoped_boolean_is_drawing_sequencer_time(g_is_drawing_sequencer_gfx_block, type==Seqblock_Type::GFX_GFX || type==Seqblock_Type::GFX || (type==Seqblock_Type::REGULAR && seqtrack->gfx_seqblocks != NULL));

    QRectF rect;

    // First check if we need to paint it.
    {
      if (seqblock_is_visible(seqblock)==false)
        return false;

      rect = get_seqblock_rect(seqblock);
      
      if (false==workingQRegionIntersects(update_region, rect))
        return false;
    }

    int header_height = get_block_header_height();

    if(rect.height() <= header_height){
      paintSeqblockText(p, rect, seqtrack, seqblock, type, true);
      return true;
    }
    
    const QRectF rect_without_header = rect.adjusted(0, header_height, 0, 0);

    if(seqblock->id != g_curr_seqblock_id_under_mouse){ //!rect.contains(mousep)){ // FIX. Must be controlled from bin/scheme/mouse.scm.

      paintSeqBlockElements(p, update_region, rect, rect_without_header, seqtrack, seqblock, type);

    } else {

#if USE_BLURRED
      
      int rwidth = ceil(rect.width());
      int rheight = ceil(rect.height());

      if(rwidth<=0 || rheight<=0){
        R_ASSERT_NON_RELEASE(false);
        return;
      }

      QImage image(rwidth, rheight, QImage::Format_ARGB32);
      // todo: must fill background image of image before painting.
      
      QRectF rect2(0, 0, rwidth, rheight);
      QRect blur_rect = rect_without_header.toRect();

      {
        QPainter ptr(&image);
        ptr.setRenderHints(QPainter::Antialiasing,true);
        paintBlockGraphics(ptr, rect2, seqtrack, seqblock, type);
        ptr.setRenderHints(QPainter::Antialiasing,false);
      }

#if 1
      blurred(image, blur_rect, 3);
      //QImage image2 = blurred(image, rect2.toRect(), 3);
#else
      QImage &image2 = image;
#endif
      
      p.drawImage((int)rect.x(), (int)rect.y(), image);
      
#else

      paintSeqBlockElements(p, update_region, rect, rect_without_header, seqtrack, seqblock, type);
      
#endif
      
      //draw_interface(&p, seqblock, rect_without_header);
    }

    const radium::AutomationPainter *automation_painters[NUM_SATS] = {};
    
    draw_seqblock_automations(automation_painters, radium::AutomationPainter::What::FILL, &p, rect_without_header, seqblock);
    
    // We paint some stuff in scheme as well (seqpaint.scm)
    if (type==Seqblock_Type::REGULAR || type==Seqblock_Type::GFX){
      R_ASSERT(seqblocknum>=0);
      int seqblockid = seqblock->id;

      API_run_custom_gui_paint_function(SEQUENCER_getWidget(),
                                        &p, &update_region,
                                        [seqtracknum,seqblocknum,seqblockid](){
                                          static func_t *s_seqpaint_func = NULL;
                                          if (s_seqpaint_func==NULL || !releaseMode()){
                                            //printf("new seqpaintfunc\n");
                                            s_seqpaint_func = s7extra_get_func_from_funcname("FROM_C-paint-seqblock-stuff");
                                          }
                                          R_ASSERT_NON_RELEASE(getGfxSeqblockFromId(seqblockid) != NULL);
                                          S7CALL(void_int_int_int, s_seqpaint_func, seqtracknum, seqblocknum, seqblockid);
                                        });
    }else
      R_ASSERT(seqblocknum==-1);

    draw_seqblock_automations(automation_painters, radium::AutomationPainter::What::LINES|radium::AutomationPainter::What::NODES, &p, rect_without_header, seqblock);

    /*
    bool is_current_seqblock = true;
    if(is_current_seqblock){
      paint_current_seqblock_border(p, rect, seqtrack, seqblock);
    }
    */
    
    return true;
  }
  
  void paint_automation(const QRegion &update_region, QPainter &p, const struct SeqTrack *seqtrack) {
    SEQTRACK_AUTOMATION_paint(&p, seqtrack, t_x1, t_y1, t_x2, t_y2, _start_time, _end_time);
  }

  void paint(const QRegion &update_region, QPainter &p, const struct SeqTrack *seqtrack, int seqtracknum) {
    RETURN_IF_DATA_IS_INACCESSIBLE();
    
    //printf("  PAINTING %d %d -> %d %d\n",t_x1,t_y1,t_x2,t_y2);

    /*
    QColor background_color
      = seqtrack==(const struct SeqTrack*)root->song->seqtracks.elements[ATOMIC_GET(root->song->curr_seqtracknum)]
      ? get_seqtrack_background_color(seqtrack).lighter(110)
      : get_seqtrack_background_color(seqtrack).darker(110);


    myFillRect(p, _rect, background_color);
    */
    
    for(int i=0;i<3;i++){
      Seqblock_Type type
        = i==0 ? Seqblock_Type::REGULAR
        : i==1 ? Seqblock_Type::GFX_GFX
        : Seqblock_Type::RECORDING;

      QVector<struct SeqBlock*> seqblocks
        = type==Seqblock_Type::REGULAR ? SEQTRACK_get_seqblocks_in_z_order(seqtrack, false)
        : type==Seqblock_Type::GFX_GFX ? SEQTRACK_get_seqblocks_in_z_order(seqtrack, true)
        : VECTOR_get_qvector<struct SeqBlock*>(&seqtrack->recording_seqblocks);

      //printf("  seqblocks size: %d. (%d %d)\n", seqblocks.size(), _seqtrack->seqblocks.num_elements, _seqtrack->gfx_seqblocks==NULL ? -1 : _seqtrack->gfx_seqblocks->num_elements);
      
      for(int i=seqblocks.size()-1 ; i>=0 ; i--){
        struct SeqBlock *seqblock = seqblocks.at(i);
        
        int seqblocknum;
        if (type==Seqblock_Type::REGULAR || type==Seqblock_Type::GFX)
          seqblocknum = get_gfxseqblocknum(seqtrack, seqblock);
        else
          seqblocknum = -1;
        
        paintSeqBlock(p, update_region, seqtrack, seqblocks.at(i), seqtracknum, seqblocknum, type);
      }
    }
  }

#define UPDATE_EVERY_5_SECONDS 0

#if UPDATE_EVERY_5_SECONDS
  QTime _time;
#endif
  int _last_num_seqblocks = 0;
  
  void call_very_often(const struct SeqTrack *seqtrack){
    
    if (_last_num_seqblocks != gfx_seqblocks(seqtrack)->num_elements) {
      SEQUENCER_update(SEQUPDATE_TIME);
      _last_num_seqblocks = gfx_seqblocks(seqtrack)->num_elements;
    }

#if UPDATE_EVERY_5_SECONDS
    {
      if (_time.elapsed() > 5000) { // Update at least every five seconds.
        _sequencer_widget->update();
        _time.restart();
      }
    }
#endif
  }
};



namespace{

class Seqtracks_widget {
public:

  const double &_start_time;
  const double &_end_time;
  
  Seqtracks_widget(const double &start_time, const double &end_time)
    :_start_time(start_time)
    ,_end_time(end_time)
  {
  }

  double t_x1=0,t_y1=0,t_x2=50,t_y2=50;
  double t_width=0,t_height=0;

  QRectF t_rect;
  
  void position_widgets(double x1, double y1, double x2, double y2){
    t_x1 = x1;
    t_y1 = y1;
    t_x2 = x2;
    t_y2 = y2;
    t_width = t_x2-t_x1;
    t_height = t_y2-t_y1;
    t_rect = QRectF(t_x1, t_y1, t_x2-t_x1, t_y2-t_y1);
  }  

  void set_seqblocks_is_selected(const QRect &selection_rect){
    VECTOR_FOR_EACH(struct SeqTrack *, seqtrack, &root->song->seqtracks){
      Seqblocks_widget seqblocks_widget = get_seqblocks_widget(iterator666, false);
      seqblocks_widget.set_seqblocks_is_selected(seqtrack, selection_rect);
    }END_VECTOR_FOR_EACH;
  }

  void paint_automation(const QRegion &update_region, QPainter &p){

    float y_min = get_seqtracks_y1();
    float y_max = get_seqtracks_y2();
        

    VECTOR_FOR_EACH(struct SeqTrack *, seqtrack, &root->song->seqtracks){
      Seqblocks_widget seqblocks_widget = get_seqblocks_widget(iterator666, true);

      if (seqblocks_widget.t_y2 > y_min){
          
        if (seqblocks_widget.t_y1 >= y_max)
          return;
        
        seqblocks_widget.paint_automation(update_region, p, seqtrack);
      }
    }END_VECTOR_FOR_EACH;
  }

  void paint_curr_seqblock_border(const QRegion &update_region, QPainter &p){
    if(workingQRegionIntersects(update_region, t_rect)){
      
      VECTOR_FOR_EACH(struct SeqTrack *, seqtrack, &root->song->seqtracks){
        int seqtracknum = iterator666;
        
        VECTOR_FOR_EACH(struct SeqBlock *, seqblock, gfx_seqblocks(seqtrack)){

          if (is_current_seqblock(seqblock)){
            
            bool paint_float = seqtrack->gfx_seqblocks != NULL;

            if(false==paint_float)
              p.setRenderHints(QPainter::Antialiasing,false);

            Seqblocks_widget seqblocks_widget = get_seqblocks_widget(seqtracknum, true);
            seqblocks_widget.paint_current_seqblock_border(p, seqtrack, seqblock, paint_float);

            if(false==paint_float)
              p.setRenderHints(QPainter::Antialiasing,true);

            return;
          }
          
        }END_VECTOR_FOR_EACH;
      }END_VECTOR_FOR_EACH;
      
    }
  }
  
  void paint(const QRegion &update_region, QPainter &p) {
    if(workingQRegionIntersects(update_region, t_rect)){

      float y_min = get_seqtracks_y1();
      float y_max = get_seqtracks_y2();
        
      VECTOR_FOR_EACH(struct SeqTrack *, seqtrack, &root->song->seqtracks){
        Seqblocks_widget seqblocks_widget = get_seqblocks_widget(iterator666, true);
        
        if (seqblocks_widget.t_y2 > y_min){
          
          if (seqblocks_widget.t_y1 >= y_max)
            return;
          
          seqblocks_widget.paint(update_region, p, seqtrack, iterator666);
        }
      }END_VECTOR_FOR_EACH;
      
    }
  }

  void get_heights(double heights[], const bool get_min) const {
    const double font_height = 4 + root->song->tracker_windows->systemfontheight; // If changing this one, change get-fontheight in mixer-strips.scm too.
    const double border_width =  get_seqtrack_border_width() / 2.0;
    
    VECTOR_FOR_EACH(struct SeqTrack *, seqtrack, &root->song->seqtracks){

      switch(get_min ? seqtrack->min_height_type : seqtrack->max_height_type){
      case SHT_CUSTOM:
        heights[iterator666] = 2*border_width + root->song->tracker_windows->systemfontheight * (get_min ? seqtrack->custom_min_height : seqtrack->custom_max_height);
        break;
      case SHT_1ROW:
        heights[iterator666] = 2*border_width + 2.0*font_height;
        break;
      case SHT_2ROWS:
        heights[iterator666] = 2*border_width + 4.0*font_height; // If changing 2.6, also change 2.5 in seqtrack-headers.scm
        break;
      case SHT_3ROWS:
        heights[iterator666] = 2*border_width + 8.0*font_height; // If changing 4.0, also change 3.5 in seqtrack-headers.scm
        break;
      case SHT_UNLIMITED:
        heights[iterator666] = get_min ? (2*border_width + 0.5*font_height) : -1;
        break;
      default:
        R_ASSERT(false);
        break;
      }

    }END_VECTOR_FOR_EACH;
  }
  
  void calculate_heights(double heights[], const int num_seqtracks, double available_space) const {
    double max_heights[num_seqtracks];

    get_heights(heights, true); // first set heigths to min size.
    get_heights(max_heights, false);
    
    double min_height = 0;
    for(int i=0;i<num_seqtracks;i++)
      min_height += heights[i];
    
    double space_left = available_space - min_height;

    if (space_left <= 0)
      return;

    
    int num_seqtracks_to_increase = 0;
    bool has_increased[num_seqtracks];
    
    for(int i=0;i<num_seqtracks;i++)
      if (max_heights[i]==-1 || max_heights[i] > heights[i]){
        num_seqtracks_to_increase++;
        has_increased[i] = false;
      } else {
        has_increased[i] = true;
      }

    if(num_seqtracks_to_increase==0)
      return;
            
    double average_inc_height;
    
    // 1. Increase to max those that can be safely increased to max.
    {
    again:

      average_inc_height = space_left / (double)num_seqtracks_to_increase;
      
      for(int i=0;i<num_seqtracks;i++){
        
        if (max_heights[i] != -1){
          
          if (has_increased[i]==false){

            double dx = max_heights[i] - heights[i];
            
            if (dx <= average_inc_height){
              
              heights[i] = max_heights[i];
              
              has_increased[i] = true;
              space_left -= dx;
              num_seqtracks_to_increase--;

              if (num_seqtracks_to_increase==0)
                return;

              if(space_left <= 0){
                R_ASSERT_NON_RELEASE(false);
                return;
              }

              if (dx < average_inc_height)
                goto again; // average_inc_height can be increased now, so we need to check everyone again.
              
            }
          }
        }
      }
    }


    R_ASSERT_NON_RELEASE(num_seqtracks_to_increase > 0);
    R_ASSERT_NON_RELEASE(average_inc_height > 0);
    
    // 3. Then increase the remaining.
    //
    for(int i=0;i<num_seqtracks;i++){
      if (has_increased[i]==false)
        heights[i] += average_inc_height;
    }
  }

  double get_y1_from_heights(const double heights[], int seqtracknum) const {
    double height = 0;
    for(int i=0;i<seqtracknum;i++){
      height += heights[i];
    }
    return height;
  }

  double get_seqtracks_y1(void) const {
    const double border_width =  get_seqtrack_border_width() / 2.0;
    return t_y1+border_width;
  }
  
  double get_seqtracks_y2(void) const {
    const double border_width =  get_seqtrack_border_width() / 2.0;
    return t_y2-border_width;
  }
  
  void calculate_seqtrack_coordinates(void) const {
    const int num_seqtracks = root->song->seqtracks.num_elements;
    //double seqtrack_height = t_height / num_seqtracks;

    // && (abs(ATOMIC_GET(root->song->curr_seqtracknum)-seqtracknum)<=1))

    double seqtracks_y1 = get_seqtracks_y1();
    double seqtracks_y2 = get_seqtracks_y2();

    double heights[num_seqtracks];
    calculate_heights(heights, num_seqtracks, seqtracks_y2 - seqtracks_y1);

    // Above topmost visible seqtrack
    {
      double prev_y1 = seqtracks_y1;

      for(int seqtracknum=root->song->topmost_visible_seqtrack-1;seqtracknum>=0;seqtracknum--){
        struct SeqTrack *seqtrack=(struct SeqTrack *)root->song->seqtracks.elements[seqtracknum];
        seqtrack->y2 = prev_y1;
        seqtrack->y1 = seqtrack->y2 - heights[seqtracknum];
        prev_y1 = seqtrack->y1;
      }
    }

    // Below topmost visible seqtrack
    {
      double next_y1 = seqtracks_y1;
    
      for(int seqtracknum=root->song->topmost_visible_seqtrack;seqtracknum<num_seqtracks;seqtracknum++){
        struct SeqTrack *seqtrack=(struct SeqTrack *)root->song->seqtracks.elements[seqtracknum];
        seqtrack->y1 = next_y1;
        seqtrack->y2 = seqtrack->y1 + heights[seqtracknum];
        next_y1 = seqtrack->y2;
      }
    }
  }

  int get_lowest_reasonable_topmost_seqtracknum(void){
    const int num_seqtracks = root->song->seqtracks.num_elements;
    if (num_seqtracks==0)
      return 0;
    
    double heights[num_seqtracks];
    
    double seqtracks_y1 = get_seqtracks_y1();
    double seqtracks_y2 = get_seqtracks_y2();
    double available_space = seqtracks_y2 - seqtracks_y1;
      
    calculate_heights(heights, num_seqtracks, available_space);

    available_space += get_seqtrack_border_width()/2.0;
    
    double height = 0;
    
    for(int seqtracknum=num_seqtracks-1;seqtracknum>=0;seqtracknum--){
      height += heights[seqtracknum];
      //printf("%d: height %f. sum: %f. available_space: %f\n",seqtracknum,height,heights[seqtracknum], available_space);
      if(height > available_space)
        return R_MIN(num_seqtracks-1, seqtracknum+1);
    }
    
    return 0;
  }

  bool last_seqtrack_is_visible(void){
    const int num_seqtracks = root->song->seqtracks.num_elements;
    if (num_seqtracks==0)
      return false;
    
    double seqtracks_y2 = get_seqtracks_y2();
    
    const struct SeqTrack *last_seqtrack = (struct SeqTrack*)root->song->seqtracks.elements[num_seqtracks-1];

    double gap = (seqtracks_y2+get_seqtrack_border_width()/2.0) - last_seqtrack->y2;
    
    return gap >= 0;
  }
  
  Seqblocks_widget get_seqblocks_widget(int seqtracknum, bool include_border) {
    double border_width =  get_seqtrack_border_width() / 2.0;
    const double border = include_border ? border_width : 0;

#if 0
    const int num_seqtracks = root->song->seqtracks.num_elements;
    //double seqtrack_height = t_height / num_seqtracks;

    // && (abs(ATOMIC_GET(root->song->curr_seqtracknum)-seqtracknum)<=1))

    double heights[num_seqtracks];
    calculate_heights(heights, num_seqtracks, (t_y2-border_width) - (t_y1+border_width));

    const double y1 = t_y1+border_width + get_y1_from_heights(heights, seqtracknum) + border;
    const double y2 = y1 + heights[seqtracknum] - border*2;

    /*
    const double y1 = scale_double(seqtracknum, 0, num_seqtracks, t_y1+border_width, t_y2-border_width) + border;
    const double y2 = scale_double(seqtracknum+1, 0, num_seqtracks, t_y1+border_width, t_y2-border_width/2.0) - border;
    */

#else

    if(seqtracknum<0){
      R_ASSERT_NON_RELEASE(false);
      seqtracknum = 0;
    }
    
    if(seqtracknum>=root->song->seqtracks.num_elements){
      R_ASSERT_NON_RELEASE(false);
      seqtracknum = root->song->seqtracks.num_elements-1;
    }
    
    double y1 = 0;
    double y2 = 10;

    if(seqtracknum >= 0){

      const struct SeqTrack *seqtrack = (const struct SeqTrack *)root->song->seqtracks.elements[seqtracknum];
      
      R_ASSERT_NON_RELEASE(seqtrack->y2 > seqtrack->y1);
    
      y1 = seqtrack->y1 + border;
      y2 = seqtrack->y2 - border;

    }
    
#endif
    
    Seqblocks_widget seqblocks_widget(_start_time, _end_time, t_x1, y1, t_x2, y2);

    return seqblocks_widget;
  }

};


struct SongTempoAutomation_widget {
  bool is_visible = false;
   
  const double &_start_time;
  const double &_end_time;
   
  double t_x1,t_y1,t_x2,t_y2,width,height;
  QRectF t_rect;
   
  SongTempoAutomation_widget(const double &start_time, const double &end_time)
    : _start_time(start_time)
    , _end_time(end_time)
  {
    position_widgets(0,0,100,100);
  }

  void position_widgets(double x1, double y1, double x2, double y2){
    t_x1 = x1;
    t_y1 = y1;
    t_x2 = x2;
    t_y2 = y2;
    height = t_y2-t_y1;
    width = t_x2-t_x1;
    
    t_rect = QRectF(t_x1, t_y1, width, height);
  }

  void paint(const QRegion &update_region, QPainter &p){
    
    if(workingQRegionIntersects(update_region, t_rect)){
  
      myFillRect(p, t_rect.adjusted(1,1,-2,-1), get_qcolor(SEQUENCER_BACKGROUND_COLOR_NUM));
    
      //printf("height: %d\n",height());
      TEMPOAUTOMATION_paint(&p, t_x1, t_y1, t_x2, t_y2, _start_time, _end_time);
      
    }
  }

};

// Return true if continuing.
static inline bool iterate_beats_between_seqblocks(const struct SeqTrack *seqtrack,
                                                   int &barnum,
                                                   int64_t start_seqtime, int64_t end_seqtime,
                                                   GridType what_to_find,
                                                   int64_t end_blockseqtime, int64_t next_blockstarttime,
                                                   int64_t last_barseqtime, int64_t last_beatseqtime,
                                                   std::function<bool(int64_t,int,int,int)> callback
                                                   )
{
    int64_t bar_seqlength = end_blockseqtime - last_barseqtime;
    int64_t beat_seqlength = end_blockseqtime - last_beatseqtime;

    //printf("last_x: %d, width: %d. bar_length: %d. next: %f, end_seqtime: %f\n", (int)last_x, width(),(int)bar_length,((double)end_blockseqtime + bar_length)/44100.0, (double)end_seqtime/44100.0);


    if (bar_seqlength <= 0){
      R_ASSERT_NON_RELEASE(false);
      return false;
    }

    if (beat_seqlength <= 0){
      fprintf(stderr,"BEAT_seqlength: %d. end_blockseqtime: %d. last_beatseqtime: %d\n", (int)beat_seqlength, (int)end_blockseqtime, (int)last_beatseqtime);
      //printf("\n\n");
      R_ASSERT_NON_RELEASE(false);
      return false;
    }

    
    int beatnum = 1;
    int64_t bar_seqtime = end_blockseqtime;
    

    for(;;){

      if (next_blockstarttime != -1 && R_ABS(bar_seqtime-next_blockstarttime) < 20)
        return true;

      if (bar_seqtime >= end_seqtime)
        return false;

      barnum++;
      beatnum = 1;
      callback(bar_seqtime, barnum, beatnum, -1);

      const int64_t next_bartime = bar_seqtime + bar_seqlength;

      if (what_to_find==GridType::BEAT_GRID) {

        beatnum++;
        
        for(int64_t beat_seqtime = bar_seqtime+beat_seqlength ; beat_seqtime < next_bartime ; beat_seqtime += beat_seqlength){

          if (next_blockstarttime != -1 && R_ABS(beat_seqtime-next_blockstarttime) < 20)
            return true;

          if (beat_seqtime >= end_seqtime)
            return false;

          if (callback(beat_seqtime, barnum, beatnum, -1)==false)
            return false;

          beatnum++;
        }
      }

      //printf("  after. abstime: %f\n",(double)abstime/44100.0);

      bar_seqtime = next_bartime;
    }

    return true;
}

// Next (or current) seqblock when ignoring audio seqblocks.
static int get_next_block_seqblocknum(const struct SeqTrack *seqtrack, int seqtracknum){
  //printf("Seqtracknum: %d (%d)\n", seqtracknum, seqtrack->seqblocks.num_elements);

  if(seqtracknum >= (seqtrack->seqblocks.num_elements)){
    R_ASSERT(seqtracknum==(seqtrack->seqblocks.num_elements));
    return -1;
  }

  const struct SeqBlock *seqblock = (const struct SeqBlock *)seqtrack->seqblocks.elements[seqtracknum];

  if (seqblock->block==NULL)
    return get_next_block_seqblocknum(seqtrack, seqtracknum+1);
  else
    return seqtracknum;
}

} // and anon. namespace


void SEQUENCER_iterate_time_seqblocks(int64_t start_seqtime, int64_t end_seqtime, bool include_previous_and_next_seqblock,
                                      std::function<radium::IterateSeqblocksCallbackReturn(const struct SeqTrack*, const struct SeqBlock *,const struct Blocks*,const struct SeqBlock *)> callback
                                             )
{
  R_ASSERT_NON_RELEASE(end_seqtime >= 0);

  const struct SeqTrack *seqtrack = (struct SeqTrack*)root->song->seqtracks.elements[0];
  if (seqtrack->seqblocks.num_elements==0)
    return;
        
  int next_seqblocknum = get_next_block_seqblocknum(seqtrack, 0);

  int num_seqblocks_after = 0;

  while(next_seqblocknum != -1){
    int seqblocknum = next_seqblocknum;
    next_seqblocknum = get_next_block_seqblocknum(seqtrack, seqblocknum + 1);

    const struct SeqBlock *seqblock = (struct SeqBlock *)seqtrack->seqblocks.elements[seqblocknum];
    const struct SeqBlock *next_seqblock = next_seqblocknum==-1 ? NULL : (struct SeqBlock *)seqtrack->seqblocks.elements[next_seqblocknum];

    int64_t next_blockstarttime = -1;
          
    if (next_seqblock!=NULL) {

      next_blockstarttime = next_seqblock->t.time;
      
      if (start_seqtime >= next_blockstarttime){
        //printf("   1. SEQUENCER_iterate_time. next_blockstarttime <= start_seqtime: %f >= %f\n", (double)next_blockstarttime/pc->pfreq, (double)start_seqtime/pc->pfreq);
        continue;
      }
    }

    int64_t start_blockseqtime = seqblock->t.time;
    //int64_t end_blockseqtime = seqblock->t.time2;

    if (start_blockseqtime >= end_seqtime){
      //R_ASSERT_NON_RELEASE(false);
#if !defined(RELEASE)
      printf("   2. SEQUENCER_iterate_time. start_blockseqtime >= end_seqtime: %f >= %f\n", (double)start_blockseqtime/pc->pfreq, (double)end_seqtime/pc->pfreq);
#endif

      if (include_previous_and_next_seqblock==false || num_seqblocks_after==1)
        return;

      num_seqblocks_after++;
    }
    
    const struct Blocks *block = seqblock->block;

    if (callback(seqtrack, seqblock, block, next_seqblock)==radium::IterateSeqblocksCallbackReturn::ISCR_BREAK)
      return;
  }
}

// If callback returns false, we stop iterating.
void SEQUENCER_iterate_time(int64_t start_seqtime, int64_t end_seqtime, GridType what_to_find, std::function<bool(int64_t,int,int,int)> callback){

  if(root->song->use_sequencer_tempos_and_signatures){
    SEQUENCER_iterate_sequencer_time(start_seqtime, end_seqtime, what_to_find, callback);
    return;
  }

  int barnum = 0;
  //printf("Start_seqtime: %f\n", (float)start_seqtime/pc->pfreq);

  SEQUENCER_iterate_time_seqblocks
    (start_seqtime,end_seqtime,false,
     [&](const struct SeqTrack *seqtrack,const struct SeqBlock *seqblock, const struct Blocks *block, const struct SeqBlock *next_seqblock){

    int64_t start_blockseqtime = seqblock->t.time;
    int64_t end_blockseqtime = seqblock->t.time2;

    int64_t next_blockstarttime = next_seqblock==NULL ? -1 : next_seqblock->t.time;
      
    if (what_to_find==GridType::BEAT_GRID || what_to_find==GridType::BAR_GRID){
      
      const struct Beats *beat = block->beats;
          
      int64_t last_barseqtime = -1;
      int64_t last_beatseqtime = -1;
      //Ratio *last_signature = NULL;
      
      while(beat!=NULL){
        //last_signature = &beat->valid_signature;

        int64_t blocktime = Place2STime(block, &beat->l.p);
        R_ASSERT_NON_RELEASE(blocktime>=0);
        
        int64_t seqtime = start_blockseqtime + blocktime_to_seqtime(seqblock, blocktime);
        //printf("  Beat seqtime: %d. %d/%d\n", (int)seqtime, beat->beat_num, beat->bar_num);
        last_beatseqtime = seqtime;

        bool is_bar = beat->beat_num==1;
        
        if (is_bar){
          last_barseqtime = seqtime;          
          barnum++;
        }
        
        if (what_to_find==GridType::BEAT_GRID || is_bar){
          if (callback(seqtime, barnum, beat->beat_num, 0)==false)
            return radium::IterateSeqblocksCallbackReturn::ISCR_BREAK;
        }
        
        beat = NextBeat(beat);
      }

      //printf("   3. SEQUENCER_iterate_time. start_seqtime: %f. end_seqtime: %f.\n", (double)start_seqtime/pc->pfreq, (double)end_seqtime/pc->pfreq);
      
      if (iterate_beats_between_seqblocks(seqtrack,
                                          barnum,
                                          start_seqtime, end_seqtime,
                                          what_to_find,
                                          end_blockseqtime, next_blockstarttime,
                                          last_barseqtime, last_beatseqtime,
                                          callback)
          == false)
        return radium::IterateSeqblocksCallbackReturn::ISCR_BREAK;
     
    } else {

      R_ASSERT(what_to_find==GridType::LINE_GRID);

      int64_t seqtime = 0;
      int line = 0;

      while(line < block->num_lines){
        Place place = {line,0,1};
        seqtime = start_blockseqtime + blocktime_to_seqtime(seqblock, Place2STime(block, &place));
        if (callback(seqtime, -1, -1, line)==false)
          return radium::IterateSeqblocksCallbackReturn::ISCR_BREAK;
        line++;
      }

      R_ASSERT(line==block->num_lines);
      
      int64_t linetime = end_blockseqtime - seqtime;

      if(linetime<=0){ // Playing veery fast.
        //R_ASSERT_NON_RELEASE(false);
        //R_ASSERT(linetime==0);
        return radium::IterateSeqblocksCallbackReturn::ISCR_BREAK;
      }
      
#if 0 //!defined(RELEASE)
      {
        Place place1 = {line-1,0,1}; // last line
        Place place2 = {line,0,1}; // last line + 1
        int64_t t1 = Place2STime(block, &place1);
        int64_t t2 = Place2STime(block, &place2);
        int64_t u1 = blocktime_to_seqtime(seqblock, t1);
        int64_t u2 = blocktime_to_seqtime(seqblock, t2);
        int64_t linetime2 = u2-u1;
        R_ASSERT(llabs(linetime2 - linetime) < 10);
      }
#endif
      
      seqtime += linetime;

      //printf("Seq: %f. Next: %f\n", (float)seqtime/pc->pfreq, (float)next_blockstarttime/pc->pfreq);
      
      while(next_blockstarttime < 0 || seqtime < next_blockstarttime){
        if (callback(seqtime, -1, -1, line)==false)
          return radium::IterateSeqblocksCallbackReturn::ISCR_BREAK;
        line++;
        seqtime += linetime;
      }
      
    }

    return radium::IterateSeqblocksCallbackReturn::ISCR_CONTINUE;
    });
}


namespace{
  
struct Timeline_widget { //: public MouseTrackerQWidget {
  const double &_start_time;
  const double &_end_time;
  const bool _show_timeline;

  Timeline_widget(QWidget *parent, const double &start_time, const double &end_time, bool show_timeline)
  //:MouseTrackerQWidget(parent)
    : _start_time(start_time)
    , _end_time(end_time)
    , _show_timeline(show_timeline)
  {    
  }

  /*
    // g_sequencer_widget handle wheel events too. Don't need (or want) to start playing twice.
  void wheelEvent(QWheelEvent *e) override {
    handle_wheel_event(e, 0, width(), _start_time, _end_time);
  }
  */

  double t_x1=0,t_y1=0,t_x2=50,t_y2=50;
  double t_width=0,t_height=0;

  QRectF t_rect;
  
  void position_widgets(double x1, double y1, double x2, double y2){
    t_x1 = x1;
    t_y1 = y1;
    t_x2 = x2;
    t_y2 = y2;
    t_width = t_x2-t_x1;
    t_height = t_y2-t_y1;
    t_rect = QRectF(t_x1, t_y1, t_x2-t_x1, t_y2-t_y1);
  }  

  void draw_filled_triangle(QPainter &p, double x1, double y1, double x2, double y2, double x3, double y3){
    const QPointF points[3] = {
      QPointF(x1, y1),
      QPointF(x2, y2),
      QPointF(x3, y3)
    };

    p.drawPolygon(points, 3);
  }
  
  QString seconds_to_timestring(double time){
    return radium::get_time_string(time, false);
  }

  void paint(const QRegion &update_region, QPainter &p) {

    const double y1 = t_y1 + 4;
    const double y2 = t_y2 - 4; //height() - 4;
    const double t1 = (y2-y1) / 2.0;

    //p.setRenderHints(QPainter::Antialiasing,true);

    QColor border_color = get_qcolor(SEQUENCER_BORDER_COLOR_NUM);
    //QColor text_color = get_qcolor(MIXER_TEXT_COLOR_NUM);

    QRect rect(t_x1+1,t_y1+1,t_width-1,t_height-1);
    p.fillRect(rect, get_qcolor(SEQUENCER_TIMELINE_BACKGROUND_COLOR_NUM));
    
    //p.setPen(text_color);
    //p.drawText(4,2,width()-6,height()-4, Qt::AlignLeft, "timeline");
    
    p.setPen(border_color);
    p.drawRect(rect);

    if(doPaintVerticalMarkersInSequencer())
      paintMarkerLines(p, _start_time, _end_time, t_x1, y1, t_x2, y2);

    // This code is copied from hurtigmixer. (translated from scheme)

    QColor timeline_arrow_color = get_qcolor(SEQUENCER_TIMELINE_ARROW_COLOR_NUM);
    QColor timeline_arrow_color_alpha = timeline_arrow_color;
    timeline_arrow_color_alpha.setAlpha(20);

    QLinearGradient gradient(0,0,10,15);
    gradient.setColorAt(0, timeline_arrow_color.lighter(125));
    gradient.setColorAt(1, timeline_arrow_color.darker(125));

    QLinearGradient gradient_alpha(0,0,10,15);
    gradient_alpha.setColorAt(0, timeline_arrow_color_alpha.lighter(125));
    gradient_alpha.setColorAt(1, timeline_arrow_color_alpha.darker(125));

    p.setBrush(gradient);//timeline_arrow_color);

    QColor text_color = get_qcolor(SEQUENCER_TEXT_COLOR_NUM);
    QColor text_color_alpha = text_color;
    text_color_alpha.setAlpha(20);

    p.setPen(text_color);
        
    const QFontMetrics fn = QFontMetrics(QApplication::font());
    double min_pixels_between_text;
    if (false==_show_timeline)
      min_pixels_between_text = fn.width("125") + 10;
    else
      min_pixels_between_text = fn.width("00:00:00") + t1*2 + 10;
    
    //double  = 40; //width() / 4;

    const bool show_punching = SEQUENCER_is_punching();
    const bool show_looping = !show_punching;
    
    const double looppunch_start_x = scale_double(show_looping ? SEQUENCER_get_loop_start() : SEQUENCER_get_punch_start(), _start_time, _end_time, t_x1, t_x2);
    const double looppunch_end_x = scale_double(show_looping ? SEQUENCER_get_loop_end(): SEQUENCER_get_punch_end(), _start_time, _end_time, t_x1, t_x2);

    const double start_time = _start_time / MIXER_get_sample_rate();
    const double end_time = _end_time / MIXER_get_sample_rate();

    //if (end_time >= start_time)
    //  return;
    R_ASSERT_RETURN_IF_FALSE(end_time > start_time);

    int inc_time = R_MAX(1, ceil(scale_double(min_pixels_between_text, 0, t_width, 0, end_time-start_time)));

    //bool did_draw_alpha = false;

    if (false==_show_timeline){ //showBarsInTimeline()){

      QColor bar_color(text_color);
      bar_color.setAlpha(50);
      
      //double x1 = _seqtracks_widget.t_x1;
      //double x2 = _seqtracks_widget.t_x2;
      //const double width_ = width(); //x2-x1;
      double last_x = -10000;
      //int barnum = 1;

      //p.setPen("red");
      
      //int64_t start_seqtime = _start_time;
      int64_t end_seqtime = _end_time;

      auto callback = [&last_x, &p, &bar_color, &text_color, this, min_pixels_between_text]
        (int64_t seqtime, int barnum, int beatnum, int linenum)
        {
                                               
          double x = scale_double(seqtime, _start_time, _end_time, t_x1, t_x2);
          if (x >= t_x2)
            return false;
          
          
          {
            p.setPen(bar_color);
            
            double y1 = t_y1 + 2;
            double y2 = t_y1 + t_height/2.0;
            if(beatnum==1)
              y2=t_y2 - 2;
            QLineF line(x, y1, x, y2);
            p.drawLine(line);
            if (beatnum!=1)
              return true;
          }
          
          //printf("%d: %d/%d.\n", (int)seqtime, barnum, beatnum);
          
          if (x > last_x + min_pixels_between_text) {

            if (x >= 0){
              p.setPen(text_color);
              
              QRectF rect(x + 2, t_y1 + 2, t_x2-(x+2), t_y2 - (t_y1+2));
              myDrawText(&p, rect, QString::number(barnum), Qt::AlignVCenter);
            }

            last_x = x;
          }
          
          return true;
        };
    
      //SEQUENCER_iterate_time(start_seqtime, end_seqtime, GridType::BEAT_GRID, callback);
      SEQUENCER_iterate_time(0, end_seqtime, GridType::BEAT_GRID, callback);
                             
      
    } else {

      // Ensure inc_time is aligned in seconds, 5 seconds, or 30 seconds.
      {
        if (inc_time%2 != 0)
          inc_time++;
        
        if (inc_time%5 != 0)
          inc_time += 5-(inc_time%5);
        
        if ((end_time-start_time) > 110)
          if (inc_time%30 != 0)
            inc_time += 30-(inc_time%30);
        
        // Again? (might be a copy and paste error)
        if ((end_time-start_time) > 110)
          if (inc_time%30 != 0)
            inc_time += 30-(inc_time%30);
      }
      //inc_time = 1;

      double start_x = t_x1 - fn.width("00:00:00") - t1*2 - 4;

      int64_t time = inc_time * int((double)start_time/(double)inc_time);
    
      for(;;){
        const double x = scale_double(time, start_time, end_time, t_x1, t_x2);
        if (x >= t_x2)
          break;

        bool draw_alpha = false;

        if (looppunch_start_x >= x-t1 && looppunch_start_x <= x+t1+min_pixels_between_text)
          draw_alpha = true;
        if (looppunch_end_x >= x-t1 && looppunch_end_x <= x+t1+min_pixels_between_text)
          draw_alpha = true;

        if (draw_alpha==true){
          p.setPen(text_color_alpha);
          //p.setBrush(timeline_arrow_color_alpha);
          gradient_alpha.setStart(x,y1);
          gradient_alpha.setFinalStop(x,y2);          
          p.setBrush(gradient_alpha);
        } else {
          p.setPen(text_color);
          gradient.setStart(x,y1);
          gradient.setFinalStop(x,y2);          
          p.setBrush(gradient);
          //p.setBrush(timeline_arrow_color);
        }

        //did_draw_alpha = draw_alpha;

        //printf("%s: start_x: %f. t_x1: %f. x: %f\n",seconds_to_timestring(time).toUtf8().constData(),start_x,t_x1,x);
        if (x > start_x) {
        
          draw_filled_triangle(p, x-t1, y1, x+t1, y1, x, y2);
          p.setBrush(Qt::NoBrush);
        
          QRectF rect(x + t1 + 4, t_y1+2, t_x2 - (x+t1+4), t_y2-(t_y1+2));
          myDrawText(&p, rect, seconds_to_timestring(time), Qt::AlignVCenter);
        }

        time += inc_time;
      }
    }
    
    p.setPen(Qt::black);
    if (show_looping)
      p.setBrush(QColor(0,0,255,150));
    else
      p.setBrush(QColor(255,0,0,150));

    // loop start
    {
      draw_filled_triangle(p, looppunch_start_x-t1*2, y1, looppunch_start_x, y1, looppunch_start_x, y2);
    }

    // loop end
    {
      draw_filled_triangle(p, looppunch_end_x, y1, looppunch_end_x+t1*2, y1, looppunch_end_x, y2);
    }

    p.setBrush(Qt::NoBrush);
  }

};

struct CursorPainter {
  const double cursor_width = 2.7;
  double _last_painted_cursor_x = 0.0f;

  bool _was_playing_smooth_song = false;

  double t_x1,t_y1,t_x2,t_y2,t_width,t_height;
  QRectF t_rect;

  double &_start_time;
  double &_end_time;

  CursorPainter(double &start_time, double &end_time)
    : _start_time(start_time)
    , _end_time(end_time)
  {}
  
  void position_widgets(double x1, double y1, double x2, double y2){
    t_x1 = x1;
    t_y1 = y1;
    t_x2 = x2;
    t_y2 = y2;
    t_width = t_x2-t_x1;
    t_height = t_y2-t_y1;
    t_rect = QRectF(t_x1, t_y1, t_x2-t_x1, t_y2-t_y1);
  }  

  double get_curr_cursor_x(int frames_to_add) const {
    if (is_playing() && pc->playtype==PLAYSONG && smooth_scrolling())
      return t_x2 / 2.0;
    else
      return scale_double(ATOMIC_DOUBLE_GET(pc->song_abstime)+frames_to_add, _start_time, _end_time, t_x1, t_x2);
  }


  void update_cursor(QWidget *parent){
    if (is_playing() && pc->playtype==PLAYSONG) {

      double song_abstime = ATOMIC_DOUBLE_GET(pc->song_abstime);
      
      if (smooth_scrolling()){

        _was_playing_smooth_song = true;
        
        double middle = (_start_time+_end_time) / 2.0;

        if (song_abstime != middle){
          double diff = song_abstime - middle;
          _start_time += diff;
          _end_time += diff;
          parent->update();
          }
        
        return;
        
      } else {

        double x = get_curr_cursor_x(1 + MIXER_get_sample_rate() * 60.0 / 1000.0);
        
        double x_min = R_MIN(x-cursor_width/2.0, _last_painted_cursor_x-cursor_width/2.0) - 2;
        double x_max = R_MAX(x+cursor_width/2.0, _last_painted_cursor_x+cursor_width/2.0) + 2;
        
        //printf("x_min -> x_max: %f -> %f\n",x_min,x_max);
        double y1 = t_y1; //_songtempoautomation_widget.t_y1;
        double y2 = t_y2; //height(); //t_y2; //get_cursor_y2() + 1;
        
        parent->update(x_min, y1, 1+x_max-x_min, y2-y1);

        if (autoscrollSequencerToMakePlaycursorVisible()) {
          
          if (song_abstime < _start_time) {
            
            int64_t diff = _start_time - song_abstime;
            _start_time -= diff;
            _end_time -= diff;
            parent->update();
            
          } else if (song_abstime > _end_time){
            
            double onefourth = (_start_time+_end_time) / 4.0;
            
            double diff = song_abstime - onefourth;
            _start_time += diff;
            _end_time += diff;
            parent->update();
          }

        }

      }
    }
    
    if (_was_playing_smooth_song==true){
      if (_start_time < 0){
        R_ASSERT_NON_RELEASE(false);
        /*
        _end_time -= _start_time;
        _start_time = 0;
        legalize_start_end_times();
        */
      }
      parent->update();
      _was_playing_smooth_song = false;
    }    
  }
  
  void paintCursor2(QPainter &p, QColor color, double x){
    if(x >= t_x1 && x < t_x2) {

      QPen pen(color);
      pen.setWidthF(cursor_width);
      
      QLineF line(x, t_y1, x, t_y2);
      
      //printf("   line: %f, %f -> %f, %f\n", _last_painted_cursor_x, y1, _last_painted_cursor_x, y2);
      p.setPen(pen);
      p.drawLine(line);
      
    }
  }
  
  void paintCursor(const QRegion &update_region, QPainter &p){

    // Start-pos cursor (blue)
    {
      double x = scale_double(pc->last_song_starttime, _start_time, _end_time, t_x1, t_x2);
      paintCursor2(p, Qt::blue, x);
    }

    // red cursor
    {
      _last_painted_cursor_x = get_curr_cursor_x(0);
      paintCursor2(p, get_qcolor(SEQUENCER_CURSOR_COLOR_NUM), _last_painted_cursor_x);
    }
    
  }
  
};

struct Seqtracks_navigator_widget : public MouseTrackerQWidget {
  double _cursor_start_time = 0;
  double _cursor_end_time = 100;
  double &_start_time;
  double &_end_time;
  Seqtracks_widget &_seqtracks_widget;

  CursorPainter _cursor_painter;
  
  Seqtracks_navigator_widget(QWidget *parent, double &start_time, double &end_time, Seqtracks_widget &seqtracks_widget)
    : MouseTrackerQWidget(parent)
    , _start_time(start_time)
    , _end_time(end_time)
    , _seqtracks_widget(seqtracks_widget)
    , _cursor_painter(_cursor_start_time, _cursor_end_time)
  {
  }

  void position_widgets(double x1, double y1, double x2, double y2){
    setGeometry(x1, y1,
                x2-x1, y2-y1);
    
    _cursor_painter.position_widgets(0, 0,
                                     x2-x1, y2-y1);
  }

  void update_cursor(void){
    _cursor_end_time = get_visible_song_length()*MIXER_get_sample_rate();
    _cursor_painter.update_cursor(this);
  }
  
  void wheelEvent(QWheelEvent *e) override {
    handle_wheel_event(this, e, 0, width(), 0, _cursor_end_time);
    e->accept();
  }

public:
  
  double get_x1(void) const {
    return scale_double(_start_time, 0, _cursor_end_time, 0, width());
  }
  double get_x2(void) const {
    return scale_double(_end_time, 0, _cursor_end_time, 0, width());
  }

  void paintEvent ( QPaintEvent * ev ) override {
    TRACK_PAINT();

    RETURN_IF_DATA_IS_INACCESSIBLE();
    
    QPainter p(this);

    p.setRenderHints(QPainter::Antialiasing,true);

    QColor border_color = get_qcolor(SEQUENCER_BORDER_COLOR_NUM);      

    // Background
    //
    QRect rect1(0,0,width(),height());
    p.fillRect(rect1, get_qcolor(SEQUENCER_BACKGROUND_COLOR_NUM));


    int num_seqtracks = root->song->seqtracks.num_elements;
    if (num_seqtracks==0)
      return;

    double seqtrack0_y1 = getSeqtrackFromNum(0)->y1;
    double seqtrackN_y2 = getSeqtrackFromNum(num_seqtracks-1)->y2;
      
    // Seqblocks
    {

      //QColor block_color = QColor(140,140,140,180);
      QColor text_color = get_qcolor(SEQUENCER_TEXT_COLOR_NUM);
      //text_color.setAlpha(128);

      VECTOR_FOR_EACH(const struct SeqTrack *, seqtrack, &root->song->seqtracks){
        //int seqtracknum = iterator666;

        /*
        double y1 = scale_double(seqtracknum,   0, num_seqtracks, 3, height()-3);
        double y2 = scale_double(seqtracknum+1, 0, num_seqtracks, 3, height()-3);
        */
        
        double y1 = scale_double(seqtrack->y1, seqtrack0_y1, seqtrackN_y2, 3, height()-3);
        double y2 = scale_double(seqtrack->y2, seqtrack0_y1, seqtrackN_y2, 3, height()-3);

        /*
        SEQTRACK_update_all_seqblock_start_and_end_times(seqtrack);
        //double start_time = _start_time / MIXER_get_sample_rate();
        //double end_time = _end_time / MIXER_get_sample_rate();
        */

        for(int i=0;i<3;i++){
          Seqblock_Type type
            = i==0 ? Seqblock_Type::REGULAR
            : i==1 ? Seqblock_Type::GFX_GFX
            : Seqblock_Type::RECORDING;

          const vector_t *seqblocks
            = type==Seqblock_Type::REGULAR ? gfx_seqblocks(seqtrack)
            : type==Seqblock_Type::GFX_GFX ? &seqtrack->gfx_gfx_seqblocks
            : &seqtrack->recording_seqblocks;

          VECTOR_FOR_EACH(const struct SeqBlock *, seqblock, seqblocks){

            QColor seqblock_color = get_seqblock_color(seqtrack, seqblock); //.lighter(150);
            
            seqblock_color.setAlpha(128);
          
            //printf("\n\n\n Start/end: %f / %f. Seqtrack/seqblock %p / %p\n\n", seqblock->start_time, seqblock->end_time, seqtrack, seqblock);
            
            double x1 = scale_double(seqblock->t.time, 0, _cursor_end_time, 0, width()); //seqtrack_widget->_seqblocks_widget->get_seqblock_x1(seqblock, start_time, end_time);
            double x2 = scale_double(seqblock->t.time2, 0, _cursor_end_time, 0, width()); //seqtrack_widget->_seqblocks_widget->get_seqblock_x2(seqblock, start_time, end_time);
            
            QRectF rect(x1,y1+1,x2-x1,y2-y1-2);
            myFillRect(p, rect, seqblock_color);

            {
              if(rect.height() > 5){
                p.setPen(text_color);
                
                myDrawText(&p, rect.adjusted(2,1,-1,-1), get_seqblock_name(seqtrack, seqblock), Qt::AlignLeft | Qt::AlignVCenter,
                           false, //wrap
                           0, //rotate
                           true, //scale
                           false // cut
                           );
              }
            }

            if (type==Seqblock_Type::RECORDING)
              p.setPen(QColor("red"));
            else if (type==Seqblock_Type::GFX_GFX)
              p.setPen(get_qcolor(SEQUENCER_BLOCK_MULTISELECT_BACKGROUND_COLOR_NUM).lighter(150));
            else if(rect.height() > 2)
              p.setPen(border_color);
            else
              p.setPen(seqblock_color);
            
            p.drawRect(rect);
            
          }END_VECTOR_FOR_EACH;

        }
        
      }END_VECTOR_FOR_EACH;
      
    }

    // Navigator
    //
    {
      double seqtracks_y1 = _seqtracks_widget.get_seqtracks_y1();
      double seqtracks_y2 = _seqtracks_widget.get_seqtracks_y2();

      //int total_seqtracks_height = get_seqtracks_total_height();
      //int visible_seqtracks_height = seqtracks_y2 - seqtracks_y1;

      double x1 = get_x1();
      double x2 = get_x2();

      double y1 = scale_double(seqtracks_y1,
                               seqtrack0_y1, seqtrackN_y2,
                               0, height()
                               );
      
      double y2 = scale_double(seqtracks_y2,
                               seqtrack0_y1, seqtrackN_y2,
                               0, height()
                               );
      
      QRectF rectX1(0,  1, x1,         height()-2);
      QRectF rectX2(x2, 1, width()-x2, height()-2);

      QRectF rectY1(x1, 1,                x2-x1,      (y1+1)-1);
      QRectF rect2 (x1, y1+1,             x2-x1,      y2-y1-1);
      QRectF rectY2(x1, y1+1+(y2-y1-1),   x2-x1,      height()-(y1+1+(y2-y1-1))-1); // Really messy when third and fourth arguments for QRect are width and height instead of x2 and y2.
      
      
      {
        QColor grayout_color = get_qcolor(SEQUENCER_NAVIGATOR_GRAYOUT_COLOR_NUM);
      
        p.fillRect(rectX1, grayout_color);
        p.fillRect(rectX2, grayout_color);
        
        p.fillRect(rectY1, grayout_color);
        p.fillRect(rectY2, grayout_color);
      }
      
      p.setPen(border_color);
      p.drawRect(rect2);
      
      double handle1_x = x1+SEQNAV_SIZE_HANDLE_WIDTH;
      double handle2_x = x2-SEQNAV_SIZE_HANDLE_WIDTH;
      //p.drawLine(handle1_x, 0, handle1, height());
      //p.drawLine(handle2_x, 0, handle1, height());
      
      QRectF handle1_rect(x1,        0, handle1_x-x1, height());
      QRectF handle2_rect(handle2_x, 0, x2-handle2_x, height());
      
      p.setBrush(get_qcolor(SEQUENCER_NAVIGATOR_HANDLER_COLOR_NUM));
      
      p.drawRect(handle1_rect);
      p.drawRect(handle2_rect);
    }


    // Markers
    //
    {
      QColor blue_color = get_qcolor(SEQUENCER_MARKER_COLOR_NUM);
      QPen blue_pen(blue_color);
      blue_pen.setWidthF((double)root->song->tracker_windows->systemfontheight / 5.17);

      blue_color.setAlphaF(0.5);
      QPen blue_round_pen(blue_color);
      blue_pen.setWidthF((double)root->song->tracker_windows->systemfontheight / 5.17);

      QPen text_pen(QColor("#b0eeeeee"));

#if 1
      QColor filled_color(blue_color); //"#80222222");
      filled_color.setAlphaF(0.2);
#else
      QColor filled_color("#80222222");
#endif

      float text_height = root->song->tracker_windows->systemfontheight;

      const dynvec_t *markers = SEQUENCER_MARKER_get_state().array;
      int i = 0;
      for(const dyn_t &marker : *markers){
        i++;
        hash_t *hash = marker.hash;
        double time = HASH_get_number(hash, ":time");
        QString name = QString::number(i) + ": "+HASH_get_qstring(hash, ":name");

        double x = scale_double(time, 0, _cursor_end_time, 0, width());
        double y1 = 0;
        double y2 = height();

        //double y = (y1+y2)/2.0;
        //float b = 1.5;
        //float text_x = x + 2;

        {
          float text_width = 1.2 * GFX_get_text_width(root->song->tracker_windows, name.toUtf8().constData());
          double text_y1 = R_MAX(0, height() - text_height - 2);
          double text_y2 = height() - 2;
          QRectF text_rect(x, text_y1, text_width, text_y2-text_y1);
          myFillRoundedRect(p, text_rect, filled_color, 5);
          
          p.setPen(text_pen);
          myDrawText(&p, text_rect, name,
                     Qt::AlignHCenter | Qt::AlignVCenter,
                     0, // rotate
                     false, // wrap
                     false, // scale
                     false); // cut text to fit

          p.setPen(blue_round_pen);
          p.drawRoundedRect(text_rect, 5, 5);
        }

        {
          p.setPen(blue_pen);
          QLineF line(x, y1, x, y2);
          p.drawLine(line);
        }

      }
    }


    _cursor_painter.paintCursor(ev->region(), p);

    
    D({
        QColor color(GFX_MakeRandomColor());
        color.setAlpha(120);
        p.fillRect(ev->rect(), color);
        
        /*
          p.setPen(QColor("black"));
          p.drawRect(ev->rect());
          p.drawText(ev->rect(), QString::number(num_calls));
        */
      });

  }

};

static inline int getSeqtrackNumFromY(float y){
  for(int seqtracknum=0;seqtracknum<root->song->seqtracks.num_elements;seqtracknum++){
    //printf("y: %f. %d: %f\n", y, seqtracknum, getSeqtrackY1(seqtracknum));
    if (y <= getSeqtrackY2(seqtracknum))
      return seqtracknum;
  }
  
  return root->song->seqtracks.num_elements-1;
}

struct Sequencer_widget : public MouseTrackerQWidget {

  int _old_width = 600;
  double _start_time = 0;
  double _end_time = 600;
  double _samples_per_pixel;

  enum GridType _grid_type = NO_GRID;

  SongTempoAutomation_widget _songtempoautomation_widget;
  Timeline_widget _timeline_widget;
  Timeline_widget _bars_and_beats_widget;
  Seqtracks_widget _seqtracks_widget;
  Seqtracks_navigator_widget _navigator_widget;
  //MyQSlider _main_reltempo;

  int _timing_markers_y1;
  int _timing_markers_y2;

  bool _has_selection_rectangle = false;
  QRect _selection_rectangle;

  CursorPainter _cursor_painter;

  bool _left_part_is_empty = false;

  Sequencer_widget(QWidget *parent)
    : MouseTrackerQWidget(parent, true)
    , _end_time(get_visible_song_length()*MIXER_get_sample_rate())
    , _samples_per_pixel((_end_time-_start_time) / width())
    , _songtempoautomation_widget(_start_time, _end_time)
    , _timeline_widget(this, _start_time, _end_time, true)
    , _bars_and_beats_widget(this, _start_time, _end_time, false)
    , _seqtracks_widget(_start_time, _end_time)
    , _navigator_widget(this, _start_time, _end_time, _seqtracks_widget)
    , _cursor_painter(_start_time, _end_time)
      //, _main_reltempo(this)
  {
    setAcceptDrops(true);
    
    //_timeline_widget.show();
    _navigator_widget.show();

    setSizePolicy(QSizePolicy::Expanding,QSizePolicy::MinimumExpanding);

    set_widget_takes_care_of_painting_everything(this);

    int minimum_height = (double)root->song->tracker_windows->systemfontheight*1.3 * 2;
    setMinimumHeight(minimum_height);
    //setMaximumHeight(height);
  }

  void legalize_start_end_times(void){
    if (_start_time < 0)
      _start_time = 0;
    if (_end_time < _start_time+10)
      _end_time = _start_time+10;
  }

#if 0
  /*
    Need custom update() system. We don't know where the cursor will be placed until the paintEvent() function is called.
    Calculating the cursor position in the timer callback is suboptimal and might (not unlikely) cause unnecessary visible jumpiness.
    Currently we are trying to predict where the cursor position will be when the paint event method is called, and when
    we predict wrong, we get graphical garbage.

    The plan is to only call update() (i.e. update with no arguments), and let paintEvent look like this:

    void paintEvent (QPaintEvent *ev) override {
      QRectF new_cursor_rect = get_cursor_rect();
      if (_last_painted_cursor_rect != new_cursor_rect) {
        _update_region += _last_painted_cursor_rect.toAlignedRect()
        _update_region += new_cursor_rect.toAlignedRect();
      }

      setClipRegion(_update_region);

      ...

      paintCursor(new_cursor_rect);
      _last_painted_cursor_rect = new_cursor_rect;
      _update_region.clear();
    }
   */

  QRectF _last_painted_cursor_rect;
  QRegion _update_region;
  
  void my_update(const QRegion &reg){
    _update_region += reg;
    update();
  }
    
  void my_update(const QRect &rect){
    _update_region += rect;
    update();
  }
    
  void my_update(const QRectF &rect){
    my_update(rect.toAlignedRect());
  }
    
  void my_update(qreal x1, qreal y1, qreal x2, qreal y2){
    my_update(QRectF(x1, y1, x2-x1, y2-y1));
  }
#endif

  void update_cursor(void){
    if (_start_time < 0){
      _end_time -= _start_time;
      _start_time = 0;
      legalize_start_end_times();
    }

    _cursor_painter.update_cursor(this);
    _navigator_widget.update_cursor();
  }
  
  bool _called_from_my_update = false;
  
  void my_update(const QRegion &region){
    radium::ScopedBoolean s(_called_from_my_update);
    
    update(region);
    update_cursor();
  }
  
  void my_update(const QRect &rect){
    radium::ScopedBoolean s(_called_from_my_update);
    
    update(rect);
    update_cursor();
  }
  
  void my_update(const QRectF &rect){
    my_update(rect.toAlignedRect());
  }
    
  void my_update(qreal x1, qreal y1, qreal x2, qreal y2){
    my_update(QRectF(x1, y1, x2-x1, y2-y1));
  }

  void my_update_all(void){
    radium::ScopedBoolean s(_called_from_my_update);
    
    D(printf("SEQ: my_update_all called\n"));
    int64_t visible_song_length = MIXER_get_sample_rate() * get_visible_song_length();
    if (_end_time > visible_song_length) {
      _end_time = visible_song_length;
      if (_start_time >= _end_time)
        _start_time = 0;
      legalize_start_end_times();
    }
    
    //_timeline_widget.update();
    _navigator_widget.update();
    update();
  }

  /*
  void set_end_time(void){
    _end_time = _start_time + width() * _samples_per_pixel;
    printf("End_time: %d\n",(int)_end_time);
  }
  */

  void dragEnterEvent(QDragEnterEvent *e) override {
    printf("               GOT Drag\n");
    e->acceptProposedAction();
  }
  

  void dropEvent(QDropEvent *event) override {
    printf("               GOT DOP. Text: -%s-\n", event->mimeData()->text().toUtf8().constData());

    QPoint point = mapToEditor(this, event->pos());
    float x = point.x();
    float y = point.y();
    
    QByteArrayList hotSpotPos = event->mimeData()->data(QStringLiteral("application/x-hotspot")).split(' ');
    if (hotSpotPos.size() == 2) {
      x -= hotSpotPos.first().toInt();
      y -= hotSpotPos.last().toInt();
    }

    if (x >= getSequencerX2())
      return;
    
    int seqtracknum = getSeqtrackNumFromY(y);
    //struct SeqTrack *seqtrack = (struct SeqTrack*)root->song->seqtracks.elements[seqtracknum];
    int64_t pos = round(scale_double(x,
                                     getSequencerX1(), getSequencerX2(),
                                     getSequencerVisibleStartTime(), getSequencerVisibleEndTime()
                                     )
                        );
    pos = getSeqGriddedTime(pos, getSeqBlockGridType());
    pos = R_MAX(0, pos);

    if (event->mimeData()->hasUrls()) {
      foreach (QUrl url, event->mimeData()->urls())
        {
          
          printf("File: -%s-. Y: %f\n", url.toLocalFile().toUtf8().constData(), y);
          
          const char *w_filename = STRING_get_chars(STRING_toBase64(STRING_create(url.toLocalFile())));
          if (seqtrackForAudiofiles(seqtracknum)) {
            createSampleSeqblock(seqtracknum,
                                 w_filename,
                                 pos,
                                 -1
                                 );
          } else {
            static bool has_shown_warning = false;
            if (has_shown_warning==false){
              GFX_addMessage("Seqtrack %d is not for audiofiles. (this message will only be shown once)", seqtracknum);
              has_shown_warning = true;
            }
            //loadBlock(w_filename);
          }
        }
    } else if (event->mimeData()->hasText()) {

      QString text = event->mimeData()->text();

      if(text.startsWith("block:")){
        if (!seqtrackForAudiofiles(seqtracknum)) {

          int blocknum = text.mid(6).toInt();
          if (blocknum < 0 || blocknum >= getNumBlocks()){
            GFX_addMessage("There is no block %d", seqtracknum);
          } else {
            createSeqblock(seqtracknum, blocknum, pos, -1);
          }

        } else {

          static bool has_shown_warning = false;
          if (has_shown_warning==false){
            GFX_addMessage("Seqtrack %d is not for editor blocks. (this message will only be shown once)", seqtracknum);
            has_shown_warning = true;
          }

        }
      }
    }
  }


  void wheelEvent(QWheelEvent *e) override {
    if (API_run_mouse_wheel_event_for_custom_widget(this, e))
      return;
    else
      handle_wheel_event(this, e, _seqtracks_widget.t_x1, _seqtracks_widget.t_x2, _start_time, _end_time);
  }

  void resizeEvent( QResizeEvent *qresizeevent) override {
    radium::ScopedResizeEventTracker resize_event_tracker;
    
    RETURN_IF_DATA_IS_INACCESSIBLE();
    
    int num_seqtracks = root->song->seqtracks.num_elements;

    if (num_seqtracks==0)
      return;

    //printf("    RESIZEEvent. Height: %d\n", height());

    // Hack to workaround screwed up layout, probably caused by a Qt bug. For some reason resizeEvent() is called twice when shown, first with minimumHeight, and then with last height.
    if (height() <= minimumHeight())
      return;
    
    bool last_seqtrack_was_visible = SEQUENCER_last_seqtrack_is_visible();
    
    //int lowest_topmost_seqtracknum = SEQUENCER_get_lowest_seqtracknum_after_resizing(dy);

    //  set_end_time();
    // _samples_per_pixel = (_end_time-_start_time) / width();
    position_widgets();
    
    API_run_resize_event_for_custom_widget(this, qresizeevent);

    if (last_seqtrack_was_visible){
      int lowest_reasonable_topmost_seqtracknum = SEQUENCER_get_lowest_reasonable_topmost_seqtracknum();
      setTopmostVisibleSeqtrack(lowest_reasonable_topmost_seqtracknum);
    }
  }

  int get_sequencer_left_part_width(void) const {
    return 1.5 * GFX_get_text_width(root->song->tracker_windows, "S Seqtrack 0.... H+R+M+S|");
  }

  int get_sequencer_right_part_width(int systemfontheight) const {
    int empty_width = systemfontheight * getTabBarHeight();
    int maybe = _left_part_is_empty ? empty_width : 1.5 * GFX_get_text_width(root->song->tracker_windows, "S Seqtrack 0.... H+R+M+S|");
    //if (maybe > width())
    //  maybe = empty_width;
    return maybe;
  }

  void position_widgets(void){
    //R_ASSERT_RETURN_IF_FALSE(_seqtracks_widget._seqtrack_widgets.size() > 0);

    //printf("   ***** Posisiotioing sequencer widgets ********\n");
    
#if 0
    const QWidget *mute_button = _seqtracks_widget._seqtrack_widgets.at(0)->mute_button;
    const QPoint p = mute_button->mapTo(this, mute_button->pos());
#endif

    QFontMetrics fm(QApplication::font());
    double systemfontheight = fm.height();

    const int right_part_width = get_sequencer_right_part_width(systemfontheight);

    const int x1 = get_sequencer_left_part_width(); //p.x() + mute_button->width();
    const int x1_width = R_MAX(systemfontheight*1.5, width() - x1 - right_part_width);

    const int min_bottom_height = systemfontheight*2;
    const int max_bottom_height = systemfontheight*6;//height() / 3;
    if(max_bottom_height <= min_bottom_height)
      return;
    
    const int bottom_height = R_BOUNDARIES(min_bottom_height, root->song->seqtracks.num_elements * (systemfontheight/2), max_bottom_height);

    const int timeline_widget_height = systemfontheight*1.3 + 2;
 
    int y1 = 0;
    

    // song tempo automation
    //
    {
      const int songtempoautomation_widget_height = systemfontheight*1.3*3.5;

      int y2 = y1 + songtempoautomation_widget_height;

      _songtempoautomation_widget.position_widgets(x1, y1,
                                                   width(), y2);

      if (_songtempoautomation_widget.is_visible)
        y1 = y2;
    }
    
    

    // timeline
    //
    {
      _timeline_widget.position_widgets(x1, y1,
                                        x1 + x1_width, y1+timeline_widget_height);

      if (showTimeSequencerLane())      
        y1 += timeline_widget_height;
    }

    // bars and beats
    {
      _bars_and_beats_widget.position_widgets(x1, y1,
                                              x1 + x1_width, y1+timeline_widget_height);
      if (showBarsAndBeatsSequencerLane())
        y1 += timeline_widget_height;
    }

    // timing + markers
    {
      int num_lanes = 0;
      
      if (showTemposSequencerLane())
        num_lanes++;
      
      if (showSignaturesSequencerLane())
        num_lanes++;
      
      if (showMarkersSequencerLane())
        num_lanes++;

      if(num_lanes > 0){
        int timing_markers_height = num_lanes*systemfontheight*1.3 + 2;
        
        _timing_markers_y1 = y1;
        _timing_markers_y2 = y1 + timing_markers_height;
        
        y1 = _timing_markers_y2;
      }
    }


    // sequencer tracks
    //
    {
      int y2 = height() - bottom_height;
      /*
      _seqtracks_widget.setGeometry(0, y1,
                                    width()-1, y2 - y1);
      */
      
      _seqtracks_widget.position_widgets(x1 + 1, y1,
                                         x1 + x1_width, y2);

      y1 = y2;
    }

    // cursor
    _cursor_painter.position_widgets(_seqtracks_widget.t_x1, 0, //_songtempoautomation_widget.t_y1,
                                     x1 + x1_width, _seqtracks_widget.t_y2 + get_seqtrack_border_width());

    // navigator
    //
    y1 += get_seqtrack_border_width()/2.0;
    _navigator_widget.position_widgets(x1, y1,
                                       x1 + x1_width, height());

    /*
    _main_reltempo.setGeometry(0, y1,
                               x1, bottom_height);
    */

    _seqtracks_widget.calculate_seqtrack_coordinates();
        
    S7CALL2(void_void, "FROM_C-reconfigure-sequencer-left-part");
    S7CALL2(void_void, "FROM_C-reconfigure-sequencer-right-part");
    S7CALL2(void_int_int_int_int, "FROM_C-reconfigure-sequencer-timing-part", x1, _timing_markers_y1, x1 + x1_width, _timing_markers_y2);

    my_update_all();
  }

  
  int _last_num_seqtracks = 0;
  double _last_visible_song_length = 0;

  bool _song_tempo_automation_was_visible = false;

  void call_very_often(void){

    RETURN_IF_DATA_IS_INACCESSIBLE();
      
    if (_song_tempo_automation_was_visible != _songtempoautomation_widget.is_visible){
      _song_tempo_automation_was_visible = _songtempoautomation_widget.is_visible;
      position_widgets();
    }

    if (ATOMIC_GET_RELAXED(g_needs_update) != 0)
      SEQUENCER_update(0);
    
    // Check if the number of seqtracks have changed
    //
    if (is_called_every_ms(50)){
      bool do_update = root->song->seqtracks.num_elements != _last_num_seqtracks;
      
      if (!do_update) {
        if (is_called_every_ms(1000)) // This is only an insurance. SEQUENCER_update is supposed to be called manually when needed.
          if (_last_visible_song_length != get_visible_song_length()) {
            do_update = true;
            _last_visible_song_length = get_visible_song_length();
          }
      }  
      
      if (do_update){
        position_widgets();
        _last_num_seqtracks = root->song->seqtracks.num_elements;
      }
    }

    // Update cursor
    //
    if (is_called_every_ms(15)){  // call each 15 ms. (i.e. more often than vsync)
      update_cursor();
    }
  }

  void set_seqblocks_is_selected(void){
    _seqtracks_widget.set_seqblocks_is_selected(_selection_rectangle);
  }


  bool _sequencer_was_full_before_hidden = false;
  
  void hideEvent(QHideEvent * event) override {
    _sequencer_was_full_before_hidden = !upperPartOfMainWindowIsVisible();
    showUpperPartOfMainWindow();
  }
  
  void showEvent(QShowEvent * event) override {
    if (_sequencer_was_full_before_hidden)
      hideUpperPartOfMainWindow();
  }
  
#if 0
  void paintGrid_old(const QRegion &update_region, QPainter &p, enum GridType grid_type) const {
    if (grid_type==NO_GRID)
      return;
    
    double x1 = _seqtracks_widget.t_x1;
    double x2 = _seqtracks_widget.t_x2;
    double width = x2-x1;
    
    double y1 = _songtempoautomation_widget.t_y1;
    double y2 = _seqtracks_widget.t_y2;

    {      
      int inc = (_end_time-_start_time) / width;
      
      if (inc > 0) {
        
        QPen pen(get_qcolor(SEQUENCER_GRID_COLOR_NUM));
        p.setPen(pen);
        
        int64_t last_bar = -50000;
        int64_t abstime = _start_time;

        // This code seems very inefficient...
        
        while(abstime < _end_time){
          int64_t maybe = SEQUENCER_find_closest_grid_start(abstime, grid_type);
          if (maybe > last_bar){
            double x = scale_double(maybe, _start_time, _end_time, x1, x2);
            //printf("x: %f, abstime: %f\n",x,(double)maybe/44100.0);
            QLineF line(x, y1+2, x, y2-2);
            p.drawLine(line);
            last_bar = maybe;
            abstime = maybe;
          }
          abstime += inc;
        }
      }
    }
  }
#endif
  
  void paintGrid(const QRegion &update_region, QPainter &p, enum GridType grid_type) const {
    if (grid_type==NO_GRID)
      grid_type=BEAT_GRID;
     //      return;


    double x1 = _seqtracks_widget.t_x1;
    double x2 = _seqtracks_widget.t_x2;
    //double width = x2-x1;
    
    double y1 = _timing_markers_y1; //_seqtracks_widget.t_y1;
    double y2 = _seqtracks_widget.t_y2;
    
    static func_t *s_paint_sequencer_grid_func = NULL;
    if (s_paint_sequencer_grid_func==NULL || !releaseMode()){
      s_paint_sequencer_grid_func = s7extra_get_func_from_funcname("FROM_C-paint-sequencer-grid");
    }
    
    API_gui_set_curr_painter(SEQUENCER_getWidget(), &p);
    S7CALL(void_int_float_float_float_float, s_paint_sequencer_grid_func, gui_getSequencerGui(), x1,y1,x2,y2);
    API_gui_set_curr_painter(SEQUENCER_getWidget(), NULL);

    return;

    QColor beat_color(get_qcolor(SEQUENCER_GRID_COLOR_NUM));
    QColor bar_color(get_qcolor(SEQUENCER_GRID_COLOR_NUM));

    if (grid_type==BEAT_GRID)
      beat_color = beat_color.lighter();
      
    QPen beat_pen(beat_color);
    QPen bar_pen(bar_color);

    if (grid_type==BEAT_GRID){
      float alpha = beat_color.alphaF();
      beat_color.setAlphaF(alpha/2);
      
      beat_pen.setWidthF(root->song->tracker_windows->systemfontheight / 25.0);
      bar_pen.setWidthF(root->song->tracker_windows->systemfontheight / 15.0);
    }
  
    p.setPen(beat_pen);
    
    double last_painted_x = x1-100;

    int min_width1 = root->song->tracker_windows->systemfontheight / 2;
    int min_width2 = root->song->tracker_windows->systemfontheight*3;
    
    auto callback = [&](int64_t seqtime, int barnum, int beatnum, int linenum)
      {
        if(seqtime >= _end_time)
          return false;
        
        double x = scale_double(seqtime, _start_time, _end_time, x1, x2);
        double diff = x-last_painted_x;
        
        if (diff >= 1){
          //printf("x: %f, abstime: %f\n",x,(double)maybe/44100.0);
          QLineF line(x, y1+2, x, y2-2);

          bool is_bar = beatnum==1;

          bool not_so_important = grid_type==BEAT_GRID && !is_bar;
          
          if (not_so_important && diff < min_width1)
            return true;
          
          QPen pen = is_bar ? bar_pen : beat_pen;
          
          if (diff < min_width2)
            pen.setWidthF(scale(diff,
                                0, min_width2,
                                0.3, pen.widthF()
                                )
                          );
          
          p.setPen(pen);
          p.drawLine(line);

#if 0
          if(is_bar){
            p.setPen("white");
            p.drawLine(line);

            QLineF line2(x-0.5, y1+2, x-0.5, y2-2);
            p.setPen("black");
            p.drawLine(line2);
          }else{
            p.setPen(pen);
            p.drawLine(line);
          }
#else
          p.setPen(pen);
          p.drawLine(line);
#endif

          last_painted_x = x;
        }        
        
        return true;
      };
    
    SEQUENCER_iterate_time(_start_time, _end_time, grid_type, callback);

  }

  void paintMarkerGrid(QPainter &p){
    if(doPaintVerticalMarkersInSequencer()){
      double x1 = _seqtracks_widget.t_x1;
      double x2 = _seqtracks_widget.t_x2;
      //double width = x2-x1;
      
      double y1 = _seqtracks_widget.t_y1;
      double y2 = _seqtracks_widget.t_y2;
      
      paintMarkerLines(p, _start_time, _end_time, x1,y1,x2,y2);
    }
  }

  void paintSeqPunchOrLoop(const QRegion &update_region, bool is_looping, QPainter &p) const {
    double y1 = _songtempoautomation_widget.t_y1;
    double y2 = _seqtracks_widget.t_y2;

    int64_t start = is_looping ? SEQUENCER_get_loop_start() : SEQUENCER_get_punch_start();
    int64_t end = is_looping ? SEQUENCER_get_loop_end() : SEQUENCER_get_punch_end();
    
    double x_start = scale_double(start, _start_time, _end_time, _seqtracks_widget.t_x1, _seqtracks_widget.t_x2);
    double x_end = scale_double(end, _start_time, _end_time, _seqtracks_widget.t_x1, _seqtracks_widget.t_x2);
    
    QRectF rect1(_seqtracks_widget.t_x1, y1, x_start - _seqtracks_widget.t_x1,  y2);
    QRectF rect2(x_end, y1, _seqtracks_widget.t_x2 - x_end,  y2);

    bool paintrect1 = x_start > _seqtracks_widget.t_x1 && workingQRegionIntersects(update_region, rect1);
    bool paintrect2 = x_end < _seqtracks_widget.t_x2 && workingQRegionIntersects(update_region, rect2);
    
    if (!paintrect1 && !paintrect2)
      return;

    QColor mix = is_looping ? QColor("blue") : QColor("red");
    QColor gray = get_qcolor(SEQUENCER_NAVIGATOR_GRAYOUT_COLOR_NUM);
    QColor grayout_color = mix_colors(mix, gray, 0.1); //.darker(200);
    grayout_color.setAlpha(gray.alpha());
    
    p.setPen(Qt::NoPen);
    p.setBrush(grayout_color);

    if (paintrect1){
      p.drawRect(rect1);
    }
    
    if (paintrect2){
      p.drawRect(rect2);
    }
  }
      
  void paintSelectionRectangle(const QRegion &update_region, QPainter &p) const {
    QColor grayout_color = QColor(220,220,220,0x40); //get_qcolor(SEQUENCER_NAVIGATOR_GRAYOUT_COLOR_NUM);

    QPen pen(Qt::black);
    p.setPen(pen);
    p.setBrush(grayout_color);

    p.drawRect(_selection_rectangle);
  }

  void paintSeqtrackBorders(QPainter &p) {
    int num_elements = root->song->seqtracks.num_elements;
    for(int seqtracknum=0 ; seqtracknum<=num_elements ; seqtracknum++){
      bool is_last = seqtracknum==num_elements;
      const Seqblocks_widget w = get_seqblocks_widget(R_MIN(num_elements-1,seqtracknum), false);
      double y = floor(is_last ? w.t_y2 : w.t_y1) - 1;

      if (y >= _seqtracks_widget.t_y2)
        break;
      
      if (y > _seqtracks_widget.t_y1) {
      
        float x1 = SEQUENCER_get_left_part_x1();//w.t_x1; //get_seqtrack_border_width()+3;//w.t_x1; //;
        float x2 = w.t_x2; //width();
        
        {
          QColor color("#000000");
          QPen pen(color);
          pen.setWidthF(1);
          p.setPen(pen);
          p.drawLine(x1, y, x2, y);
        }
        {
          QPen pen(QColor("#777777"));
          pen.setWidthF(1);
          p.setPen(pen);
          p.drawLine(x1, y+1, x2, y+1);
        }
      }
    }
  }
  
  void paintCurrentSeqtrackBorder(QPainter &p) {
    if (root->song->seqtracks.num_elements <= 1)
      return;

    int curr_seqtracknum = ATOMIC_GET(root->song->curr_seqtracknum);
    if (curr_seqtracknum < 0 || curr_seqtracknum >= root->song->seqtracks.num_elements)
      return;

    if(curr_seqtracknum < root->song->topmost_visible_seqtrack)
      return;
          
    //const SeqTrack *seqtrack = (const struct SeqTrack*)root->song->seqtracks.elements[curr_seqtracknum];

    {
      const Seqblocks_widget w = get_seqblocks_widget(curr_seqtracknum, false);

      //QRectF rect(b/2.0, w.t_y1-b/2.0, this->width()-b, w.t_y2-w.t_y1+b);
      QRectF rect(0, w.t_y1, w.t_x2+20, w.t_y2-w.t_y1);

      //printf("Seqtrack: ");
      paintCurrBorder(p, rect, get_qcolor(MIXERSTRIPS_CURRENT_INSTRUMENT_BORDER_COLOR_NUM));
    }
  }

  void paintEvent(QPaintEvent *ev) override {
    R_ASSERT(_called_from_my_update==false); // Neven know what Qt might do. Also think I've seen Qt do this before, and if Qt calls paintEvent directly from update(), we can get graphical garbage.
    
    if (workingQRegionContains(childrenRegion(), ev->region())){ // We get a paint event with the area under the navigator widget when _navigator_widget->update() is called.
      //printf("skipped painting area below children widgets\n");
      return;
    }
    
    D(static int num_calls = 0;
      printf("   SEQ paintEvent called %d, %d -> %d, %d (%d)\n", ev->rect().x(), ev->rect().y(), ev->rect().x()+ev->rect().width(), ev->rect().y()+ev->rect().height(), num_calls++);
      //printf("              navigator: %d, %d -> %d, %d\n", _navigator_widget.x(), _navigator_widget.y(), _navigator_widget.x()+_navigator_widget.rect().width(), _navigator_widget.y()+_navigator_widget.rect().height());
      );

    
    RETURN_IF_DATA_IS_INACCESSIBLE();

    bool seqtracks_are_painted =
      ev->rect().right() >= _seqtracks_widget.t_x1 && 
      ev->rect().left() < _seqtracks_widget.t_x2;

    bool right_part_is_painted = ev->rect().right() >= _seqtracks_widget.t_x2;

#define ERASE_FIRST 1
    
    // Erase background
    //
#if ERASE_FIRST
    if(seqtracks_are_painted || right_part_is_painted){ // This is strange, but if we don't do this here, some graphical artifacts are shown in the left part of the headers.
      TRACK_PAINT();
      
      QPainter p(this);
      for(const QRect &rect : ev->region())
        p.eraseRect(rect);
    }
#endif

    float seqtracks_y_max = _seqtracks_widget.get_seqtracks_y2() + get_seqtrack_border_width();

    // Paint seqtrack headers (left part)
    //
    const QRegion reg(_seqtracks_widget.t_x1, _seqtracks_widget.t_y1,
                      _seqtracks_widget.t_width, _seqtracks_widget.t_height);
    
    API_run_paint_event_for_custom_widget(this,
                                          ev,
                                          reg
                                          );
 
    // Paint seqblocks and seqtrack backround
    //
    if(seqtracks_are_painted){
      // Need to put TRACK_PAINT in a different scope than the call to API_run_paint_event_for_custom_widget.
      TRACK_PAINT();

      radium::ScopedBoolean scoped_boolean_is_drawing_sequencer_time(g_is_drawing_sequencer_time);
      
      //printf("Painting seq\n");

      QPainter p(this);

#if !ERASE_FIRST
      QRect erase_rect(rect());
      erase_rect.setLeft(_seqtracks_widget.t_x1);
      erase_rect.setRight(width() - _seqtracks_widget.t_x1);
      p.eraseRect(erase_rect); // We don't paint everything.
#endif

#undef ERASE_FIRST
      
      p.setRenderHints(QPainter::Antialiasing,true);    
      
      p.setClipRect(QRectF(_seqtracks_widget.t_x1, 0, _seqtracks_widget.t_width, seqtracks_y_max));
      p.setClipping(true);

      paintGrid(ev->region(), p, _grid_type);
      
      {

        _seqtracks_widget.paint(ev->region(), p);

      }

      if (showTimeSequencerLane())
        _timeline_widget.paint(ev->region(), p);
      
      if (showBarsAndBeatsSequencerLane())
        _bars_and_beats_widget.paint(ev->region(), p);

      if (_songtempoautomation_widget.is_visible)
        _songtempoautomation_widget.paint(ev->region(), p);
      
      if (SEQUENCER_is_looping())
        paintSeqPunchOrLoop(ev->region(), true, p);    
      
      if (SEQUENCER_is_punching())
        paintSeqPunchOrLoop(ev->region(), false, p);
      
      if (_has_selection_rectangle)
        paintSelectionRectangle(ev->region(), p);

    }

    // Paint seqtrack borders, seqtrack automation, and cursor
    //
    {
      TRACK_PAINT();

      QPainter p(this);

      {
        p.setClipRect(QRectF(0, 0, _seqtracks_widget.t_x2, seqtracks_y_max));
        p.setClipping(true);
      
        paintSeqtrackBorders(p);
        paintCurrentSeqtrackBorder(p);

        p.setClipping(false);
      }
      
      if (seqtracks_are_painted) {

        p.setRenderHints(QPainter::Antialiasing,true);

        paintMarkerGrid(p);

        p.setClipRect(QRectF(_seqtracks_widget.t_x1, _seqtracks_widget.t_y1, _seqtracks_widget.t_width, seqtracks_y_max - _seqtracks_widget.t_y1));
        p.setClipping(true);

        _seqtracks_widget.paint_curr_seqblock_border(ev->region(), p);

        _seqtracks_widget.paint_automation(ev->region(), p);    

        p.setClipping(false);
        _cursor_painter.paintCursor(ev->region(), p);
      }

      // Debugging update.
      D({
          QColor color(GFX_MakeRandomColor());
          color.setAlpha(120);
          for(const QRect &rect : ev->region())
            p.fillRect(rect, color);

          /*
          p.setPen(QColor("black"));
          p.drawRect(ev->rect());
          p.drawText(ev->rect(), QString::number(num_calls));
          */
        });
    }
  }


  Seqblocks_widget get_seqblocks_widget(int seqtracknum, bool include_border){
    return _seqtracks_widget.get_seqblocks_widget(seqtracknum, include_border);
  }

};


}

static Sequencer_widget *g_sequencer_widget = NULL;

/*
static void g_position_widgets(void){
  if (g_sequencer_widget != NULL)
    g_sequencer_widget->position_widgets();
}
*/

QWidget *SEQUENCER_getWidget_r0(void){
  return g_sequencer_widget;
}

QWidget *SEQUENCER_getWidget(void){
  R_ASSERT(g_sequencer_widget != NULL);
  return g_sequencer_widget;
}

// sequencer

float SEQUENCER_get_x1(void){
  return mapToEditorX(g_sequencer_widget, g_sequencer_widget->_seqtracks_widget.t_x1);
}

float SEQUENCER_get_x2(void){
  return mapToEditorX(g_sequencer_widget, g_sequencer_widget->_seqtracks_widget.t_x2);
}

float SEQUENCER_get_y1(void){
  return mapToEditorY1(g_sequencer_widget);
}

float SEQUENCER_get_y2(void){
  return mapToEditorY2(g_sequencer_widget);
}

float SEQTRACKS_get_x1(void){
  return mapToEditorX(g_sequencer_widget, g_sequencer_widget->_seqtracks_widget.t_x1);
}

float SEQTRACKS_get_x2(void){
  return mapToEditorX(g_sequencer_widget, g_sequencer_widget->_seqtracks_widget.t_x2);
}

float SEQTRACKS_get_y1(void){
  return mapToEditorY(g_sequencer_widget, g_sequencer_widget->_seqtracks_widget.t_y1);
}

float SEQTRACKS_get_y2(void){
  return mapToEditorY(g_sequencer_widget, g_sequencer_widget->_seqtracks_widget.t_y2);
}

float SEQUENCER_get_left_part_x1(void){
  return get_seqtrack_border_width();
}

float SEQUENCER_get_left_part_x2(void){
  return g_sequencer_widget->_seqtracks_widget.t_x1 - 1;
}

float SEQUENCER_get_left_part_y1(void){
  return 0;
}

float SEQUENCER_get_left_part_y2(void){
  return g_sequencer_widget->height();
}

bool SEQUENCER_right_part_is_empty(void){
  return g_sequencer_widget->_left_part_is_empty;
}

void SEQUENCER_set_right_part_is_empty(bool is_empty){
  if (is_empty != g_sequencer_widget->_left_part_is_empty){
    g_sequencer_widget->_left_part_is_empty = is_empty;
    g_sequencer_widget->position_widgets();
  }
}

float SEQUENCER_get_right_part_x1(void){
  return g_sequencer_widget->_seqtracks_widget.t_x2;
}

float SEQUENCER_get_right_part_x2(void){
  return g_sequencer_widget->width();
}

float SEQUENCER_get_right_part_y1(void){
  return 0;
}

float SEQUENCER_get_right_part_y2(void){
  return g_sequencer_widget->height();
}

void SEQUENCER_WIDGET_initialize(QWidget *main_window){
  R_ASSERT(g_sequencer_widget==NULL);
  g_sequencer_widget = new Sequencer_widget(main_window);
}

void SEQUENCER_WIDGET_call_very_often(void){
  if (g_sequencer_widget != NULL)
    g_sequencer_widget->call_very_often();
}

QWidget *SEQUENCER_WIDGET_get_widget(void){
  return g_sequencer_widget;
}


static int update_enabled_counter = 0;

void SEQUENCER_disable_gfx_updates(void){
  update_enabled_counter++;

  // Commented out since it didn't make a difference, plus that the setUpdatesEnabled() function takes a while to execute
  /*
  if (update_enabled_counter==1 && g_sequencer_widget!=NULL)
    g_sequencer_widget->setUpdatesEnabled(false);
  */
}
void SEQUENCER_enable_gfx_updates(void){
  update_enabled_counter--;

  // Commented out since it didn't make a difference, plus that the setUpdatesEnabled() function takes a while to execute
  /*
  if (update_enabled_counter==0 && g_sequencer_widget!=NULL)
    g_sequencer_widget->setUpdatesEnabled(true);
  */
}

double SEQUENCER_get_visible_start_time(void){
  return g_sequencer_widget->_start_time;
}

double SEQUENCER_get_visible_end_time(void){
  return g_sequencer_widget->_end_time;
}

void SEQUENCER_set_visible_start_and_end_time(int64_t start_time, int64_t end_time){
  R_ASSERT_RETURN_IF_FALSE(end_time > start_time);
  
  g_sequencer_widget->_start_time = R_MAX(start_time, 0);
  g_sequencer_widget->_end_time = R_MIN(end_time, get_visible_song_length() * MIXER_get_sample_rate());

  g_sequencer_widget->legalize_start_end_times();
          
  g_sequencer_widget->my_update_all();
}

void SEQUENCER_set_visible_start_time(int64_t val){
  R_ASSERT_RETURN_IF_FALSE(val < g_sequencer_widget->_end_time);

  g_sequencer_widget->_start_time = R_MAX(val, 0);

  g_sequencer_widget->legalize_start_end_times();

  SEQUENCER_update(SEQUPDATE_TIME|SEQUPDATE_TIMELINE|SEQUPDATE_NAVIGATOR|SEQUPDATE_TIMING);
  //g_sequencer_widget->my_update_all();
}

void SEQUENCER_set_visible_end_time(int64_t val){
  R_ASSERT_RETURN_IF_FALSE(val > g_sequencer_widget->_start_time);
  
  g_sequencer_widget->_end_time = R_MIN(val, get_visible_song_length() * MIXER_get_sample_rate());

  g_sequencer_widget->legalize_start_end_times();
    
  SEQUENCER_update(SEQUPDATE_TIME|SEQUPDATE_TIMELINE|SEQUPDATE_NAVIGATOR|SEQUPDATE_TIMING);
  //g_sequencer_widget->my_update_all();
}

void SEQUENCER_set_grid_type(enum GridType grid_type){
  if(g_sequencer_widget->_grid_type != grid_type){
    g_sequencer_widget->_grid_type = grid_type;
    SEQUENCER_update(SEQUPDATE_TIME);
  }
}

enum GridType SEQUENCER_get_grid_type(void){
  return g_sequencer_widget->_grid_type;
}

void SEQUENCER_set_selection_rectangle(float x1, float y1, float x2, float y2){
  bool was_selected = g_sequencer_widget->_has_selection_rectangle;
    
  g_sequencer_widget->_has_selection_rectangle = true;
  QPoint p1 = mapFromEditor(g_sequencer_widget, QPoint(x1, y1));
  QPoint p2 = mapFromEditor(g_sequencer_widget, QPoint(x2, y2));

  //printf("    P1 x/y: %d / %d.\n", p1.x(), p1.y());
  
  // legalize values
  //
  int &_x1 = p1.rx();
  int &_y1 = p1.ry();
  int &_x2 = p2.rx();
  int &_y2 = p2.ry();

  if (_x1 < g_sequencer_widget->_seqtracks_widget.t_x1)
    _x1 = g_sequencer_widget->_seqtracks_widget.t_x1;
  if (_x2 < _x1)
    _x2 = _x1;

  if (_x2 > g_sequencer_widget->_seqtracks_widget.t_x2)
    _x2 = g_sequencer_widget->_seqtracks_widget.t_x2;
  if (_x1 > _x2)
    _x1 = _x2;
  
  if (_y1 < g_sequencer_widget->_seqtracks_widget.t_y1)
    _y1 = g_sequencer_widget->_seqtracks_widget.t_y1;
  if (_y2 < _y1)
    _y2 = _y1;

  if (_y2 > g_sequencer_widget->_seqtracks_widget.t_y2)
    _y2 = g_sequencer_widget->_seqtracks_widget.t_y2;
  if (_y1 > _y2)
    _y1 = _y2;
  
  const QRect new_rect(p1, p2);
  const QRect new_draw_rect = new_rect.adjusted(-1,-1,1,1);

  if (was_selected==false){
    
    g_sequencer_widget->my_update(new_rect);

  } else {
    
    const QRect &old_draw_rect = g_sequencer_widget->_selection_rectangle.adjusted(-1,-1,1,1);

    QRegion region(new_draw_rect);
    region = region.xored(QRegion(old_draw_rect));

    QRegion new_border(new_draw_rect);
    new_border -= new_rect.adjusted(2,2,-2,-2);

    QRegion old_border(old_draw_rect);
    old_border -= old_draw_rect.adjusted(2,2,-2,-2);

    region += new_border;
    region += old_border;

    /*
    int i = 0;
    for(const auto &rect : region){
      printf("%d: %d, %d -> %d, %d (w: %d, h: %d)\n", i++, rect.x(), rect.y(), rect.x()+rect.width(), rect.y()+rect.height(), rect.width(), rect.height());
    }
    printf("\n");
    */
    
    g_sequencer_widget->my_update(region);
    
  }
  
  /*
  int seqtrack1 = R_MIN(getSeqtrackNumFromY(mapToEditorY(g_sequencer_widget, old_rect.y())), mapToEditorY(g_sequencer_widget, getSeqtrackNumFromY(new_rect.y())));
  int seqtrack2 = R_MAX(getSeqtrackNumFromY(mapToEditorY(g_sequencer_widget, old_rect.y()+old_rect.height())), getSeqtrackNumFromY(mapToEditorY(g_sequencer_widget, new_rect.y()+new_rect.height())));

  for(int seqtracknum=seqtrack1 ; seqtracknum < seqtrack2+1 ; seqtracknum++)
    SEQTRACK_update((struct SeqTrack*)root->song->seqtracks.elements[seqtracknum]);
  */
  
  g_sequencer_widget->_selection_rectangle = new_rect;
  
  g_sequencer_widget->set_seqblocks_is_selected();
}

void SEQUENCER_unset_selection_rectangle(void){
  if(g_sequencer_widget->_has_selection_rectangle==false)
    return;
  
  g_sequencer_widget->_has_selection_rectangle = false;

  const QRect &old_rect = g_sequencer_widget->_selection_rectangle;

  // Not sure why this is necessary. Forgot to comment.
  {
    int seqtrack1 = getSeqtrackNumFromY(mapToEditorY(g_sequencer_widget, old_rect.y()));
    int seqtrack2 = getSeqtrackNumFromY(mapToEditorY(g_sequencer_widget, old_rect.y()+old_rect.height()));
    
    for(int seqtracknum=seqtrack1 ; seqtracknum < seqtrack2+1 ; seqtracknum++)
      SEQTRACK_update((struct SeqTrack*)root->song->seqtracks.elements[seqtracknum]);
  }
  
  // This one is needed (after the above updates of seqtracks) to update seqtrack borders.
  g_sequencer_widget->update(old_rect);
}


// sequencer navigator

float SEQNAV_get_x1(void){
  return mapToEditorX1(&g_sequencer_widget->_navigator_widget);
}

float SEQNAV_get_x2(void){
  return mapToEditorX2(&g_sequencer_widget->_navigator_widget);
}

float SEQNAV_get_y1(void){
  return mapToEditorY1(&g_sequencer_widget->_navigator_widget);
}

float SEQNAV_get_y2(void){
  return mapToEditorY2(&g_sequencer_widget->_navigator_widget);
}

float SEQNAV_get_left_handle_x(void){
  return SEQNAV_get_x1() + g_sequencer_widget->_navigator_widget.get_x1();
}

float SEQNAV_get_right_handle_x(void){
  return SEQNAV_get_x1() + g_sequencer_widget->_navigator_widget.get_x2();
}

/*
void SEQNAV_update(void){
  g_sequencer_widget->_navigator_widget.update();
}
*/


// sequencer looping

float SEQTIMELINE_get_x1(void){
  return mapToEditorX(g_sequencer_widget, g_sequencer_widget->_timeline_widget.t_x1);
}

float SEQTIMELINE_get_x2(void){
  return mapToEditorX(g_sequencer_widget, g_sequencer_widget->_timeline_widget.t_x2);
}

float SEQTIMELINE_get_y1(void){
  return mapToEditorY(g_sequencer_widget, g_sequencer_widget->_timeline_widget.t_y1);
}

float SEQTIMELINE_get_y2(void){
  if(showBarsAndBeatsSequencerLane())
    return mapToEditorY(g_sequencer_widget, g_sequencer_widget->_bars_and_beats_widget.t_y2);
  else
    return mapToEditorY(g_sequencer_widget, g_sequencer_widget->_timeline_widget.t_y2);
}




// seqtempo automation

float SEQTEMPO_get_x1(void){
  return mapToEditorX(g_sequencer_widget, g_sequencer_widget->_songtempoautomation_widget.t_x1);
}

float SEQTEMPO_get_x2(void){
  return mapToEditorX(g_sequencer_widget, g_sequencer_widget->_songtempoautomation_widget.t_x2);
}

float SEQTEMPO_get_y1(void){
  return mapToEditorY(g_sequencer_widget, g_sequencer_widget->_songtempoautomation_widget.t_y1);
}

float SEQTEMPO_get_y2(void){
  return mapToEditorY(g_sequencer_widget, g_sequencer_widget->_songtempoautomation_widget.t_y2);
}

void SEQTEMPO_set_visible(bool visible){
  if (g_sequencer_widget != NULL){
    g_sequencer_widget->_songtempoautomation_widget.is_visible = visible;
    g_sequencer_widget->position_widgets();
  }
}

bool SEQTEMPO_is_visible(void){
  return g_sequencer_widget->_songtempoautomation_widget.is_visible;
}


// seqblocks

float SEQBLOCK_get_x1(int seqblocknum, int seqtracknum){
  const Seqblocks_widget w = g_sequencer_widget->get_seqblocks_widget(seqtracknum, true);

  return mapToEditorX(g_sequencer_widget, 0) + w.get_seqblock_x1(seqtracknum, seqblocknum);
}

/*
float SEQBLOCK_get_x1(const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock){

  auto *w0 = g_sequencer_widget->get_seqtrack_widget(seqtrack);
  if (w0==NULL)
    return 0.0;
  
  const auto &w = w0->_seqblocks_widget;

  return mapToEditorX(g_sequencer_widget, 0) + w.get_seqblock_x1(seqblock);
}
*/

float SEQBLOCK_get_x2(int seqblocknum, int seqtracknum){
  const Seqblocks_widget w = g_sequencer_widget->get_seqblocks_widget(seqtracknum, true);
  /*
  auto *w0 = g_sequencer_widget->get_seqtrack_widget(seqtracknum);
  if (w0==NULL)
    return 0.0;
  
  const auto &w = w0->_seqblocks_widget;
  */

  return mapToEditorX(g_sequencer_widget, 0) + w.get_seqblock_x2(seqtracknum, seqblocknum);
}

/*
float SEQBLOCK_get_x2(const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock){
  auto *w0 = g_sequencer_widget->get_seqtrack_widget(seqtrack);
  if (w0==NULL)
    return 0.0;
  
  const auto &w = w0->_seqblocks_widget;

  return mapToEditorX(g_sequencer_widget, 0) + w.get_seqblock_x2(seqblock);
}
*/

float SEQBLOCK_get_y1(int seqblocknum, int seqtracknum){
  const Seqblocks_widget w = g_sequencer_widget->get_seqblocks_widget(seqtracknum, true);
  /*
  auto *w0 = g_sequencer_widget->get_seqtrack_widget(seqtracknum);
  if (w0==NULL)
    return 0.0;
  
  const auto &w = w0->_seqblocks_widget;
  */

  return mapToEditorY(g_sequencer_widget, w.t_y1);
}

float SEQBLOCK_get_header_height(void){
  return get_block_header_height();
}

float SEQBLOCK_get_y2(int seqblocknum, int seqtracknum){
  const Seqblocks_widget w = g_sequencer_widget->get_seqblocks_widget(seqtracknum, true);
  /*
  auto *w0 = g_sequencer_widget->get_seqtrack_widget(seqtracknum);
  if (w0==NULL)
    return 0.0;
  
  const auto &w = w0->_seqblocks_widget;
  */
  return mapToEditorY(g_sequencer_widget, w.t_y2);
}


#define SEQBLOCK_handles(Name, Yfunc1, Yfunc2)                        \
                                                                        \
  float SEQBLOCK_get_left_##Name##_x1(int seqblocknum, int seqtracknum){ \
    return SEQBLOCK_get_x1(seqblocknum, seqtracknum);                   \
  }                                                                     \
                                                                        \
  float SEQBLOCK_get_left_##Name##_y1(int seqblocknum, int seqtracknum){ \
    return Yfunc1(seqblocknum, seqtracknum);\
  }                                                                     \
                                                                        \
  float SEQBLOCK_get_left_##Name##_x2(int seqblocknum, int seqtracknum){ \
    return get_seqblock_xsplit1(SEQBLOCK_get_x1(seqblocknum, seqtracknum), \
                                SEQBLOCK_get_x2(seqblocknum, seqtracknum) \
                                );                                      \
  }                                                                     \
                                                                        \
  float SEQBLOCK_get_left_##Name##_y2(int seqblocknum, int seqtracknum){ \
    return Yfunc2(seqblocknum, seqtracknum);                   \
  }                                                                     \
                                                                        \
                                                                        \
                                                                        \
  float SEQBLOCK_get_right_##Name##_x1(int seqblocknum, int seqtracknum){ \
    return get_seqblock_xsplit2(SEQBLOCK_get_x1(seqblocknum, seqtracknum), \
                                SEQBLOCK_get_x2(seqblocknum, seqtracknum) \
                                );                                      \
  }                                                                     \
                                                                        \
  float SEQBLOCK_get_right_##Name##_y1(int seqblocknum, int seqtracknum){ \
    return SEQBLOCK_get_left_##Name##_y1(seqblocknum, seqtracknum);      \
  }                                                                     \
                                                                        \
  float SEQBLOCK_get_right_##Name##_x2(int seqblocknum, int seqtracknum){ \
    return SEQBLOCK_get_x2(seqblocknum, seqtracknum);                   \
  }                                                                     \
                                                                        \
  float SEQBLOCK_get_right_##Name##_y2(int seqblocknum, int seqtracknum){ \
    return SEQBLOCK_get_left_##Name##_y2(seqblocknum, seqtracknum);      \
  }


static float yfunc_ysplit0(int seqblocknum, int seqtracknum){
  return SEQBLOCK_get_y1(seqblocknum, seqtracknum) + get_block_header_height();
}

static float yfunc_ysplit1(int seqblocknum, int seqtracknum){
  return get_seqblock_ysplit1(SEQBLOCK_get_y1(seqblocknum, seqtracknum) + get_block_header_height(),
                              SEQBLOCK_get_y2(seqblocknum, seqtracknum)
                              );
}

static float yfunc_ysplit2(int seqblocknum, int seqtracknum){
  return get_seqblock_ysplit2(SEQBLOCK_get_y1(seqblocknum, seqtracknum) + get_block_header_height(),
                              SEQBLOCK_get_y2(seqblocknum, seqtracknum)
                              );
}

static float yfunc_ysplit3(int seqblocknum, int seqtracknum){
  return get_seqblock_ysplit3(SEQBLOCK_get_y1(seqblocknum, seqtracknum) + get_block_header_height(),
                              SEQBLOCK_get_y2(seqblocknum, seqtracknum)
                              );
}

SEQBLOCK_handles(fade, yfunc_ysplit0, yfunc_ysplit1);
SEQBLOCK_handles(interior, yfunc_ysplit1, yfunc_ysplit2);
SEQBLOCK_handles(speed, yfunc_ysplit2, yfunc_ysplit3);
SEQBLOCK_handles(stretch, yfunc_ysplit3, SEQBLOCK_get_y2);




// seqtracks

float SEQTRACK_get_x1(int seqtracknum){
  return mapToEditorX(g_sequencer_widget, g_sequencer_widget->_seqtracks_widget.t_x1);
}

float SEQTRACK_get_x2(int seqtracknum){
  return mapToEditorX(g_sequencer_widget, g_sequencer_widget->_seqtracks_widget.t_x2);
}

float SEQTRACK_get_y1(int seqtracknum){
  const Seqblocks_widget w = g_sequencer_widget->get_seqblocks_widget(seqtracknum, true);

  return mapToEditorY(g_sequencer_widget, w.t_y1);
}

float SEQTRACK_get_y2(int seqtracknum){
  const Seqblocks_widget w = g_sequencer_widget->get_seqblocks_widget(seqtracknum, true);

  return mapToEditorY(g_sequencer_widget, w.t_y2);
}

int SEQUENCER_get_lowest_reasonable_topmost_seqtracknum(void){
  return g_sequencer_widget->_seqtracks_widget.get_lowest_reasonable_topmost_seqtracknum();
}
bool SEQUENCER_last_seqtrack_is_visible(void){
  return g_sequencer_widget->_seqtracks_widget.last_seqtrack_is_visible();
}

static void update(const struct SeqTrack *seqtrack, bool also_update_borders, bool also_update_nodes, int64_t start_time, int64_t end_time){
  if (g_sequencer_widget == NULL)
    return;

  int seqtracknum = get_seqtracknum(seqtrack);
  if(seqtracknum==-1)
    return;
  
  const Seqblocks_widget w = g_sequencer_widget->get_seqblocks_widget(seqtracknum, true);

  qreal x1 = start_time==-1
    ? w.t_x1
    : scale_double(start_time, g_sequencer_widget->_start_time, g_sequencer_widget->_end_time, w.t_x1, w.t_x2) - 5; // subtracting 5 to show "Recording". TODO: Found out why it's necessary.
  
  qreal x2 = end_time==-1
    ? w.t_x2
    : scale_double(end_time, g_sequencer_widget->_start_time, g_sequencer_widget->_end_time, w.t_x1, w.t_x2);
  
#if 0
  printf("SEQTRACK_update called %d\n", get_seqtracknum(seqtrack));
#endif

  // Hmm, it might be necessary to update borders as well when updating nodes, and vice versa.
  
  if (also_update_nodes){
    float min_node_size = get_min_node_size();
    g_sequencer_widget->my_update(QRect(floor(x1-min_node_size-1), floor(w.t_y1-min_node_size-1),
                                        ceil(x2-x1+min_node_size*2+2), ceil(w.t_height+min_node_size*2+2)));
  }else if (also_update_borders)
    g_sequencer_widget->my_update(QRect(floor(x1), floor(w.t_y1-get_seqtrack_border_width()-1),
                                        ceil(x2-x1), ceil(w.t_height+get_seqtrack_border_width()*2+2)));
  else
    g_sequencer_widget->my_update(QRect(floor(x1), floor(w.t_y1),
                                        ceil(x2-x1), ceil(w.t_height)));

}

void SEQTRACK_update_with_nodes(const struct SeqTrack *seqtrack, int64_t start_time, int64_t end_time){
  update(seqtrack, true, true, start_time, end_time);
}

void SEQTRACK_update_with_borders(const struct SeqTrack *seqtrack, int64_t start_time, int64_t end_time){
  update(seqtrack, true, false, start_time, end_time);
}

void SEQTRACK_update(const struct SeqTrack *seqtrack, int64_t start_time, int64_t end_time){
  update(seqtrack, false, false, start_time, end_time);
}

void SEQTRACK_update_with_borders(const struct SeqTrack *seqtrack){
  update(seqtrack, true, false, -1, -1);
}

void SEQTRACK_update(const struct SeqTrack *seqtrack){
  update(seqtrack, false, false, -1, -1);
}


static void my_update_sequencer_widget(const QRect &rect){
  g_sequencer_widget->my_update(rect);
}


void SEQBLOCK_update(const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock){
  int seqtracknum = get_seqtracknum(seqtrack);

  if (seqtracknum < 0)
    return;
  
  const Seqblocks_widget w = g_sequencer_widget->get_seqblocks_widget(seqtracknum, seqblock->id==g_curr_seqblock_id);
  w.update_seqblock(seqblock);
}

void SEQBLOCK_update2(const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock, int64_t start_time, int64_t end_time){
  int seqtracknum = get_seqtracknum(seqtrack);
  
  const Seqblocks_widget w = g_sequencer_widget->get_seqblocks_widget(seqtracknum, seqblock->id==g_curr_seqblock_id);
  w.update_seqblock(seqblock, false, start_time, end_time);
}

void SEQBLOCK_update_with_borders(const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock){
  //SEQTRACK_update_with_borders(seqtrack);
      
  int seqtracknum = get_seqtracknum(seqtrack);
  
  const Seqblocks_widget w = g_sequencer_widget->get_seqblocks_widget(seqtracknum, true);
  w.update_seqblock(seqblock, true);  
}

// Note: Might be called from a different thread than the main thread. (DiskPeak thread calls this function)
void SEQUENCER_update(uint32_t what){
  D({
  static int i=0;
  printf("%d: SEQUENCER_update called Main: %d. Has lock: %d. Time: %d. Header: %d. Trackorder: %d. Playlist: %d. Navigator: %d\n%s\n\n",
         i++,
         THREADING_is_main_thread(),PLAYER_current_thread_has_lock(),
         what&SEQUPDATE_TIME, what&SEQUPDATE_HEADERS, what&SEQUPDATE_TRACKORDER, what&SEQUPDATE_PLAYLIST, what&SEQUPDATE_NAVIGATOR,
         (ATOMIC_GET(root->editonoff) || !(what&SEQUPDATE_TIME) )? "" : JUCE_get_backtrace()
         );
});

  if (THREADING_is_main_thread()==false || g_sequencer_widget==NULL || PLAYER_current_thread_has_lock() || g_qt_is_painting){
    R_ASSERT_NON_RELEASE( (what & SEQUPDATE_HEADERS)==false);
    R_ASSERT_NON_RELEASE( (what & SEQUPDATE_TRACKORDER)==false);
    ATOMIC_OR_RETURN_OLD_RELAXED(g_needs_update, what);
    return;
  }

  what = what | ATOMIC_SET_RETURN_OLD_RELAXED(g_needs_update, 0);

  R_ASSERT_NON_RELEASE(what != 0);
  
  //g_sequencer_widget->position_widgets();
  //printf("SEQUENCER_update called\n%s\n",JUCE_get_backtrace());

  if (what & SEQUPDATE_TIMING){
    what = what | SEQUPDATE_TIME;
  }
  
  if (what & SEQUPDATE_TRACKCOORDINATES){
    //g_sequencer_widget->_seqtracks_widget.calculate_seqtrack_coordinates(); // g_sequencer_widget->position_widgets calls calculate_seqtrack_coordinates()
    what = what | SEQUPDATE_TIME | SEQUPDATE_HEADERS | SEQUPDATE_TRACKORDER | SEQUPDATE_PLAYLIST | SEQUPDATE_BLOCKLIST | SEQUPDATE_NAVIGATOR;
  }
  
  if (what & SEQUPDATE_TRACKORDER) {
    
    g_sequencer_widget->position_widgets();
    
  } else {
    
    if (what & SEQUPDATE_HEADERS)
      g_sequencer_widget->update(0, 0,
                                 ceil(g_sequencer_widget->_seqtracks_widget.t_x1), g_sequencer_widget->height()
                                 );

    if (what & SEQUPDATE_SONGTEMPO){
      if (g_sequencer_widget->_songtempoautomation_widget.is_visible){
        QRectF rect = g_sequencer_widget->_songtempoautomation_widget.t_rect;
        float min_node_size = get_min_node_size();
        rect.adjust(-min_node_size,-min_node_size,min_node_size,min_node_size); // songtempo automation nodes can be painted outside the songtemp area.
        
        g_sequencer_widget->my_update(rect.toAlignedRect());
      }
    }
    
    if (what & SEQUPDATE_TIMELINE){
      g_sequencer_widget->my_update(g_sequencer_widget->_timeline_widget.t_rect.toAlignedRect());
      g_sequencer_widget->my_update(g_sequencer_widget->_bars_and_beats_widget.t_rect.toAlignedRect());
    }
    
    if (what & SEQUPDATE_TIME){
      //printf("SEQUENCER_update called\n%s\n",JUCE_get_backtrace());
      g_sequencer_widget->my_update(QRect(floor(g_sequencer_widget->_seqtracks_widget.t_x1), 0,
                                          ceil(g_sequencer_widget->_seqtracks_widget.t_width), g_sequencer_widget->height())
                                    );
    }

    if ((what & SEQUPDATE_NAVIGATOR) || (what & SEQUPDATE_TIME)){
      g_sequencer_widget->_navigator_widget.update();
    }
  }

  if (what & SEQUPDATE_RIGHT_PART)
    g_sequencer_widget->update(SEQUENCER_get_right_part_x1(), SEQUENCER_get_right_part_y1(),
                               SEQUENCER_get_right_part_x2()-SEQUENCER_get_right_part_x1(), SEQUENCER_get_right_part_y2()-SEQUENCER_get_right_part_y1());
  
  if (what & SEQUPDATE_PLAYLIST)
    BS_UpdatePlayList();
  
  if (what & SEQUPDATE_BLOCKLIST)
    BS_UpdateBlockList();
}

// Only called from the main thread.
/*
void RT_SEQUENCER_update_sequencer_and_playlist(void){
  R_ASSERT_RETURN_IF_FALSE(THREADING_is_main_thread());

  if (PLAYER_current_thread_has_lock()) {
    
    g_needs_update = true;
    
  } else {

    D(printf("RT_SEQUENDER_update_and_playlist called\n"));
    SEQUENCER_update();
    BS_UpdatePlayList();
    
    g_needs_update=false;
    
  }
}
*/

static bool g_sequencer_hidden_because_instrument_widget_is_large = false;

/*
bool GFX_SequencerIsVisible(void){
  init_sequencer_visible();
  return g_sequencer_visible;
}
*/

void GFX_ShowSequencer(void){
  //set_widget_height(30);
  if (g_sequencer_hidden_because_instrument_widget_is_large == false){
    API_showSequencerGui();
  }

  set_editor_focus();
}

void GFX_HideSequencer(void){
  API_hideSequencerGui();
  //set_widget_height(0);

  set_editor_focus();
}

void SEQUENCER_hide_because_instrument_widget_is_large(void){
  if (sequencerInWindow())
    return;
  
  g_sequencer_widget->hide();
  g_sequencer_hidden_because_instrument_widget_is_large = true;
}

void SEQUENCER_show_because_instrument_widget_is_large(void){
  if (sequencerInWindow())
    return;
  
  if (GFX_SequencerIsVisible()){
    GL_lock(); {
      g_sequencer_widget->show();
    }GL_unlock();
  }

  g_sequencer_hidden_because_instrument_widget_is_large = false; 
}

bool SEQUENCER_has_mouse_pointer(void){
  if (g_sequencer_widget->isVisible()==false)
    return false;

  QPoint p = g_sequencer_widget->mapFromGlobal(QCursor::pos());
  if (true
      && p.x() >= 0
      && p.x() <  g_sequencer_widget->width()
      && p.y() >= 0
      && p.y() <  g_sequencer_widget->height()
      )
    return true;
  else
    return false;
}
