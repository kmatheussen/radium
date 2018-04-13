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

#include <math.h>

#include <QColor>
#include <QRawFont>
#include <QApplication>

#include "Qt_MyQCheckBox.h"
#include "Qt_MyQSlider.h"

#include "Qt_seqtrack_widget.h"

#include "../embedded_scheme/scheme_proc.h"
#include "../common/seqtrack_automation_proc.h"
#include "../common/song_tempo_automation_proc.h"
#include "../common/seqblock_envelope_proc.h"
#include "../common/tracks_proc.h"

#include "../audio/Mixer_proc.h"

#include "../audio/Peaks.hpp"
//#include "../audio/Envelope.hpp"

#include "../common/seqtrack_proc.h"


// May (probably) experience problems (crash or garbage gfx) if zooming in too much and qreal is just 32 bits.
static_assert (sizeof(qreal) >= 8, "qreal should be at least 64 bits");



#define D(n)
//#define D(n) n

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


static bool g_need_update = false;

static bool smooth_scrolling(void){
  return smoothSequencerScrollingEnabled();
}

static bool g_draw_colored_seqblock_tracks = true;

SeqBlock *g_curr_seqblock = NULL;


/* Custom font drawing code */

namespace{
  struct GlyphpathAndWidth{
    QPainterPath path;
    float width;
    QFontMetricsF fn;
    GlyphpathAndWidth(QPainterPath _path, float _width, const QFontMetricsF &_fn)
      : path(_path)
      , width(_width)
      , fn(_fn)
    {}
    GlyphpathAndWidth()
      : width(0.0f)
      , fn(QFontMetricsF(QApplication::font()))
    {}
  };
}

static GlyphpathAndWidth getGlyphpathAndWidth(const QFont &font, const QChar c){
  static QFont cacheFont = font;
  static QRawFont rawFont = QRawFont::fromFont(font);
  static QHash<QChar,GlyphpathAndWidth> glyphpathCache;
  static QFontMetricsF fn(font);

  if (font != cacheFont){
    glyphpathCache.clear();
    cacheFont = font;
    rawFont = QRawFont::fromFont(font);
    fn = QFontMetrics(font);
  }

  if (glyphpathCache.contains(c))
    return glyphpathCache[c];


  QVector<quint32> indexes = rawFont.glyphIndexesForString(c);

  GlyphpathAndWidth g(rawFont.pathForGlyph(indexes[0]), fn.width(c), fn);
  
  glyphpathCache[c] = g;

  return g;
}

static void myDrawText(QPainter &painter, QRectF rect, QString text, QTextOption option = QTextOption(Qt::AlignLeft | Qt::AlignTop)){
  if (is_playing() && pc->playtype==PLAYSONG && smooth_scrolling()){

    // Paint glyphs manually. QPainter::drawText doesn't have floating point precision.

    int len=text.length();

    double x = rect.x();
    double y = -1;
    
    QBrush brush = painter.pen().brush();
    
    for(int i=0; i<len; i++){
      GlyphpathAndWidth g = getGlyphpathAndWidth(painter.font(), text.at(i));
      if (y < 0)
        y = rect.y()+g.fn.ascent();//+g.fn.descent();//height();//2+rect.height()/2;      
      painter.save();
      painter.translate(x, y);
      painter.fillPath(g.path,brush);
      painter.restore();
      
      x += g.width;
    }
  } else {
    painter.drawText(rect, text, option);
  } 
}

static void myFillRect(QPainter &p, QRectF rect, const QColor &color){
  QPen pen = p.pen();
  p.setPen(Qt::NoPen);
  p.setBrush(color);
  p.drawRect(rect);
  p.setBrush(Qt::NoBrush);
  p.setPen(pen);
}

static void myFilledPolygon(QPainter &p, QPointF *points, int num_points, const QColor &color){
  QPen pen = p.pen();
  p.setPen(Qt::NoPen);
  p.setBrush(color);
  p.drawPolygon(points, num_points);
  p.setBrush(Qt::NoBrush);
  p.setPen(pen);
}

static void g_position_widgets(void);

static QPoint skewedPoint(QWidget *widget, const QPoint &p, int dx, int dy){
  QPoint p2 = widget->mapToGlobal(p);
  return QPoint(p2.x()+dx, p2.y()+dy);
}


static int get_block_header_height(void) {
  return root->song->tracker_windows->systemfontheight*1.3;
}

static double get_seqblock_xsplit1(double seqblock_x1, double seqblock_x2){
  return seqblock_x1 + (seqblock_x2-seqblock_x1) / 4;
}

static double get_seqblock_xsplit2(double seqblock_x1, double seqblock_x2){
  return seqblock_x2 - (seqblock_x2-seqblock_x1) / 4;
}

static double get_seqblock_ysplit1(double seqblock_y1, double seqblock_y2){
  double y1 = seqblock_y1;
  return y1 + (seqblock_y2-y1) / 3;
}

static double get_seqblock_ysplit2(double seqblock_y1, double seqblock_y2){
  double y1 = seqblock_y1;
  return seqblock_y2 - (seqblock_y2-y1) / 3;
}

static QPoint mapFromEditor(QWidget *widget, QPoint point){
  QPoint global = g_editor->isVisible()
    ? g_editor->mapToGlobal(point)
    : QPoint(point.x()-10000, point.y()-10000);

  //printf("    G: %d / %d. Mapped: %d / %d\n", global.x(), global.y(), widget->mapFromGlobal(global).x(), widget->mapFromGlobal(global).y());
  return widget->mapFromGlobal(global);
}

static QPoint mapToEditor(QWidget *widget, QPoint point){
  if (!g_editor->isVisible())
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

static QColor get_block_color(const struct Blocks *block){
  //return mix_colors(QColor(block->color), get_qcolor(SEQUENCER_BLOCK_BACKGROUND_COLOR_NUM), 0.32f);
  return QColor(block->color);
}

static QColor get_sample_color(const SeqTrack *seqtrack, const SeqBlock *seqblock){
  if (seqtrack->patch!=NULL && seqtrack->patch->patchdata!=NULL){
    SoundPlugin *plugin = (SoundPlugin*) seqtrack->patch->patchdata;
    //return QColor(SEQTRACKPLUGIN_get_sample_color(plugin, seqblock->sample_id));
    return QColor(SAMPLEREADER_get_sample_color(SEQTRACKPLUGIN_get_sample_name(plugin, seqblock->sample_id, true)));
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

class MouseTrackerQWidget : public QWidget {
public:
  
  MouseTrackerQWidget(QWidget *parent)
    : QWidget(parent)
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

  void	mousePressEvent( QMouseEvent *event) override{
    event->accept();
    _currentButton = getMouseButtonEventID(event);
    QPoint point = mapToEditor(this, event->pos());
    SCHEME_mousepress(_currentButton, point.x(), point.y());
    //printf("  Press. x: %d, y: %d. This: %p\n", point.x(), point.y(), this);
  }
  void	mouseReleaseEvent( QMouseEvent *event) override{
    event->accept();
    QPoint point = mapToEditor(this, event->pos());
    SCHEME_mouserelease(_currentButton, point.x(), point.y());
    _currentButton = 0;
    //printf("  Release. x: %d, y: %d. This: %p\n", point.x(), point.y(), this);
  }
  void	mouseMoveEvent( QMouseEvent *event) override{
    event->accept();
    QPoint point = mapToEditor(this, event->pos());
    SCHEME_mousemove(_currentButton, point.x(), point.y());
    //printf("    move. x: %d, y: %d. This: %p\n", point.x(), point.y(), this);
  }

};

static double get_visible_song_length(void){
  return SONG_get_length() + SEQUENCER_EXTRA_SONG_LENGTH;
}

static void handle_wheel_event(QWheelEvent *e, int x1, int x2, double start_play_time, double end_play_time) {
      
  if (  (e->modifiers() & Qt::ControlModifier) || (e->modifiers() & Qt::ShiftModifier)) {

    double nav_start_time = SEQUENCER_get_visible_start_time();
    double nav_end_time = SEQUENCER_get_visible_end_time();
    double visible_song_length = get_visible_song_length() * MIXER_get_sample_rate();
    double new_start,new_end;
    
    double range = nav_end_time - nav_start_time;
    double middle = nav_start_time + range/2.0;
    double how_much = range / 10.0;
     
    if (e->modifiers() & Qt::ControlModifier) {
      
      if (e->delta() > 0) {
        
        new_start = nav_start_time + how_much;
        new_end = nav_end_time - how_much;
        
      } else {
        
        new_start = nav_start_time - how_much;
        new_end = nav_end_time + how_much;
        
      }
      
      if (fabs(new_end-new_start) < 400 || new_end<=new_start) {
        new_start = middle - 200;
        new_end = middle + 200;
      }
      
    } else {

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

    double pos = R_MAX(0, scale_double(e->x(), x1, x2, start_play_time, end_play_time));
    if (e->delta() > 0)
      PlaySong(pos);
    else {
      PlayStop();
      ATOMIC_DOUBLE_SET(pc->song_abstime, pos);
      SEQUENCER_update();
      if (useJackTransport())
        MIXER_TRANSPORT_set_pos(pos);
    }
    
  }
}


class Seqblocks_widget {
public:

  //QVector<Seqblock_widget*> _seqblock_widgets; // deleted automatically when 'this' is deleted.
  
  QWidget *_sequencer_widget;
  
  SeqTrack *_seqtrack;
  
  const double &_start_time;
  const double &_end_time;
  QRectF _rect;

  double t_x1,t_y1,t_x2,t_y2,width,height;
  
  Seqblocks_widget(QWidget *_sequencer_widget, SeqTrack *seqtrack, const double &start_time, const double &end_time)
    : _sequencer_widget(_sequencer_widget)
    , _seqtrack(add_gc_root(seqtrack))
    , _start_time(start_time)
    , _end_time(end_time)
  {
    position_widgets(0,0,100,100);
    
    //create_seqblock_widgets();
  }
  
  ~Seqblocks_widget(){
    remove_gc_root(_seqtrack);
    printf("Seqblocks widget deleted\n");
    //getchar();
  }

  void position_widgets(double x1, double y1, double x2, double y2){
    t_x1 = x1;
    t_y1 = y1;
    t_x2 = x2;
    t_y2 = y2;
    height = t_y2-t_y1;
    width = t_x2-t_x1;
    
    _rect = QRectF(t_x1, t_y1, width, height);
  }
  
  /*
  void wheelEvent(QWheelEvent *e) override {
    handle_wheel_event(e, 0, width(), _start_time, _end_time);
  }
  */

  /*
  double get_seqblock_x1(const struct SeqBlock *seqblock, double start_time, double end_time) const {
    return scale_double(get_seqblock_time1_s(seqblock), start_time, end_time, t_x1, t_x2);
  }
  
  double get_seqblock_x2(const struct SeqBlock *seqblock, double start_time, double end_time) const {
    return scale_double(get_seqblock_time1_s(seqblock), start_time, end_time, t_x1, t_x2);
  }
  */
  
  double get_seqblock_x1(const struct SeqBlock *seqblock) const {
    /*
    SEQTRACK_update_all_seqblock_start_and_end_times(_seqtrack);
    double start_time = _start_time / MIXER_get_sample_rate();
    double end_time = _end_time / MIXER_get_sample_rate();
    return get_seqblock_x1(seqblock, start_time, end_time);
    */
    return scale_double(seqblock->t.time, _start_time, _end_time, t_x1, t_x2);
  }

  double get_seqblock_x1(int seqblocknum) const {
    R_ASSERT_RETURN_IF_FALSE2(seqblocknum>=0, 0);

    // This can happen while the sequencer is updated.
    if (seqblocknum >= gfx_seqblocks(_seqtrack)->num_elements)
      return 0;
    
    return get_seqblock_x1((struct SeqBlock*)gfx_seqblocks(_seqtrack)->elements[seqblocknum]);
  }

  double get_seqblock_x2(const struct SeqBlock *seqblock) const {
    /*
    SEQTRACK_update_all_seqblock_start_and_end_times(_seqtrack);
    double start_time = _start_time / MIXER_get_sample_rate();
    double end_time = _end_time / MIXER_get_sample_rate();
    return get_seqblock_x2(seqblock, start_time, end_time);
    */
    return scale_double(seqblock->t.time2, _start_time, _end_time, t_x1, t_x2);
  }

  double get_seqblock_x2(int seqblocknum) const {
    R_ASSERT_RETURN_IF_FALSE2(seqblocknum>=0, 0);

    // This can happen while the sequencer is updated.
    if (seqblocknum >= gfx_seqblocks(_seqtrack)->num_elements)
      return 100;

    return get_seqblock_x2((struct SeqBlock*)gfx_seqblocks(_seqtrack)->elements[seqblocknum]);
  }

  void set_seqblocks_is_selected(const QRect &selection_rect){
    /*
    double sample_rate = MIXER_get_sample_rate();
    //double song_length = get_visible_song_length()*sample_rate;
  
    SEQTRACK_update_all_seqblock_start_and_end_times(_seqtrack);

    double start_time = _start_time / sample_rate;
    double end_time = _end_time / sample_rate;
    */
    
    VECTOR_FOR_EACH(struct SeqBlock *, seqblock, gfx_seqblocks(_seqtrack)){

      if (seqblock->t.time < _end_time && seqblock->t.time2 >= _start_time) {
        double x1 = get_seqblock_x1(seqblock);
        double x2 = get_seqblock_x2(seqblock);

        QRect rect(x1,t_y1+1,x2-x1,height-2);
        
        seqblock->is_selected = rect.intersects(selection_rect);
      } else {
        seqblock->is_selected = false;
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
    const double bar_height = 2.3;
    const double bar_header_length = 3.2;
    
    QPen pen1(color1);
    pen1.setWidthF(bar_height);
    pen1.setCapStyle(Qt::FlatCap);

    QPen pen2(pen1);
    pen2.setColor(color2);

    {
      QColor color;

      if (is_multiselected)
        color = get_block_qcolor(SEQUENCER_BLOCK_MULTISELECT_BACKGROUND_COLOR_NUM, true);

      else if (!g_draw_colored_seqblock_tracks)
        color = get_block_qcolor(SEQUENCER_BLOCK_BACKGROUND_COLOR_NUM, false);
      
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
    
    struct Notes *note = track->notes;
    while(note != NULL){

      struct Pitches last_pitch;
      last_pitch.l.p = note->end;
      last_pitch.note = note->pitch_end;
      
      struct Pitches first_pitch;
      first_pitch.l.p = note->l.p;
      first_pitch.l.next = &note->pitches->l;
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
        pen1.setWidthF(2.3);
        pen1.setCapStyle(Qt::FlatCap);
        p.setPen(pen1);
        
        QLineF line1(x1, y1, x2, y2);
        QLineF line2(x1, y2, x2, y1);
        p.drawLine(line1);
        p.drawLine(line2);
      }
    }
      
  }
  
  QColor half_alpha(QColor c, bool is_gfx_gfx) const {
    if (is_gfx_gfx)
      c.setAlpha(c.alpha() / 4);      
    return c;
  }

  QColor get_block_qcolor(enum ColorNums colornum, bool is_gfx_gfx) const {
    QColor c = get_qcolor(colornum);
    return half_alpha(c, is_gfx_gfx);
  }

  void drawWaveform(QPainter &p, const SoundPlugin *plugin, const radium::DiskPeaks *disk_peaks, const struct SeqBlock *seqblock, bool is_gfx_gfx, const QColor &color, int64_t time1, int64_t time2, double x1, double x2, double w_y1, double w_y2){

    if (x1 >= t_x2)
      return;

    if (x2 < t_x1)
      return;

    if (time1 >= time2){
      R_ASSERT_NON_RELEASE(time1-2 <= time2);
      return;
    }

    if (x1 < t_x1) { // if seqblock starts before visible area
      time1 = R_SCALE(t_x1,
                      x1, x2,
                      time1, time2);
      x1 = t_x1;
    }
          
    if (x2 > t_x2){ // if seqblock ends after visible area
      time2 = R_SCALE(t_x2,
                      x1, x2,
                      time1, time2);
      x2 = t_x2;
    }

    R_ASSERT_NON_RELEASE(time2 >= time1);
        
    if (time2 <= time1)
      return;

    const double pixels_per_peak = 1.3;
    double width = x2-x1;

    int num_ch = SEQTRACKPLUGIN_get_num_channels(plugin, seqblock->sample_id);

    int num_points = R_MAX(4, width / pixels_per_peak);
    if ((num_points % 2) != 0)
      num_points++;

    for(int ch=0;ch<num_ch;ch++){

      double y1 = scale_double(ch, 0, num_ch, w_y1, w_y2);
      double y2 = scale_double(ch+1, 0, num_ch, w_y1, w_y2);

      QPointF points[num_points*2];

      int64_t last_used_end_time = time1;

      const float m = 0.3f; // half minimum waveform height (to avoid blank gfx for silent audio)
          
      bool has_data = true;
      double min_y = 0.0;
      double max_y = 0.0;

      for(int i=1;i<=num_points;i++){

        if (has_data){
              
          int64_t start_time = last_used_end_time;
          int64_t end_time = R_BOUNDARIES(start_time,
                                          scale_int64(i,
                                                      0, num_points,
                                                      time1, time2),
                                          time2);
              
          R_ASSERT_NON_RELEASE(end_time >= start_time);

          if (end_time > start_time) {
                
            const radium::Peak peak = disk_peaks->_peaks[ch]->get(start_time, end_time);

            double min,max;
                
            if (peak.has_data()==true) {
              min = peak.get_min();
              max = peak.get_max();
            } else {
              min = 0.0f;
              max = 0.0f;
              has_data = false; // I.e. waveform is not finished loading. (SEQUENCER_update() is called often while loading, and also when finished loading).
            }

            min_y = scale_double(min, -1, 1, y1+m, y2   ) - m;
            max_y = scale_double(max, -1, 1, y1,   y2-m ) + m;

            last_used_end_time = end_time;
          }
              
        }

        double x = scale_double(i,
                                1,num_points,
                                x1, x2);
            
        points[i-1].setX(x);
        points[i-1].setY(min_y);

        points[num_points*2-i].setX(x);
        points[num_points*2-i].setY(max_y);
      }

      myFilledPolygon(p, points, num_points*2, color);
    }
  }
  
  void paintSampleGraphics(QPainter &p, const QRectF &rect, const struct SeqBlock *seqblock, bool is_gfx_gfx){
    const int header_height = get_block_header_height();

    QColor waveform_color = get_block_qcolor(SEQUENCER_WAVEFORM_COLOR_NUM, is_gfx_gfx);
    QColor background_color = get_sample_color(_seqtrack, seqblock).lighter(250);
    background_color.setAlpha(128);
    if (is_gfx_gfx)
      background_color = half_alpha(mix_colors(background_color, get_qcolor(SEQUENCER_BLOCK_MULTISELECT_BACKGROUND_COLOR_NUM), 0.5), true);
    
    const SoundPlugin *plugin = (SoundPlugin*) _seqtrack->patch->patchdata;
    //    R_ASSERT(plugin!=NULL); // Commented out. Plugin can be NULL during loading.

    if (plugin != NULL){

      myFillRect(p, rect, background_color);

      // Solution: Paint 3 waveforms:
      // 1. Interior start (in a lot more transparent color)
      // 2. The thing
      // 3. Interior end (in a lot more transparent color)
      
      const radium::DiskPeaks *disk_peaks = SEQTRACKPLUGIN_get_peaks(plugin, seqblock->sample_id);
      if (disk_peaks != NULL){

        const double resample_ratio = SEQTRACKPLUGIN_get_resampler_ratio(plugin, seqblock->sample_id);
          
        const double x1 = rect.x();
        const double x2 = rect.x() + rect.width();
        const int64_t time1 = seqblock->t.interior_start / resample_ratio;
        const int64_t time2 = seqblock->t.interior_end / resample_ratio;

        const double y1 = rect.y() + header_height;
        const double y2 = rect.y() + rect.height();

        if (seqblock==g_curr_seqblock){

          QColor interior_waveform_color = waveform_color;
          interior_waveform_color.setAlpha(70);
          
          const double interior_start_length = seqblock->t.interior_start * seqblock->t.stretch;
          const double noninterior_start = seqblock->t.time - interior_start_length;

          const double interior_end_length = (seqblock->t.default_duration - seqblock->t.interior_end) * seqblock->t.stretch;
          const double noninterior_end = seqblock->t.time2 + interior_end_length;

          const double i1_x1 = scale_double(noninterior_start,
                                            _start_time, _end_time,
                                            t_x1, t_x2);
          
          drawWaveform(p, plugin, disk_peaks, seqblock, is_gfx_gfx, interior_waveform_color,
                       0, time1,
                       i1_x1, rect.x(),
                       y1, y2);
          
          const double i2_x2 = scale_double(noninterior_end,
                                            _start_time, _end_time,
                                            t_x1, t_x2);

          drawWaveform(p, plugin, disk_peaks, seqblock, is_gfx_gfx, interior_waveform_color,
                       time2, seqblock->t.default_duration / resample_ratio,
                       x2, i2_x2,
                       y1, y2);
        }

        drawWaveform(p, plugin, disk_peaks, seqblock, is_gfx_gfx, waveform_color,
                     time1, time2,
                     x1, x2,
                     y1, y2);
     
      }
    }
  }

  void paintBlockGraphics(QPainter &p, const QRectF &rect, const struct SeqBlock *seqblock, bool is_gfx_gfx){
    R_ASSERT(seqblock->block != NULL);
    
    const struct Blocks *block = seqblock->block;

    const int header_height = get_block_header_height();
    
    QColor track_border_color  = get_block_qcolor(SEQUENCER_TRACK_BORDER2_COLOR_NUM, is_gfx_gfx);

    QPen track_border_pen(track_border_color);

    track_border_pen.setWidthF(1.3);

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
        paintEditorTrack(p, x1, t_y1, x2, t_y2, seqblock, block, track, blocklen, is_gfx_gfx);
        
        track = NextTrack(track);
      }
    }

    // Bar and beats
    if(1){
      const struct Beats *beat = block->beats;
      QColor bar_color = get_block_qcolor(SEQUENCER_BLOCK_BAR_COLOR_NUM, is_gfx_gfx);
      QColor beat_color = get_block_qcolor(SEQUENCER_BLOCK_BEAT_COLOR_NUM, is_gfx_gfx);
      QPen bar_pen(bar_color);
      QPen beat_pen(beat_color);

      bar_pen.setWidthF(1.3);
      beat_pen.setWidthF(1.3);

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

  void paintSeqblockHeader(QPainter &p, const QRectF &rect, const struct SeqBlock *seqblock, bool is_gfx_gfx){
    const int header_height = get_block_header_height();

    QColor text_color = get_block_qcolor(SEQUENCER_TEXT_COLOR_NUM, is_gfx_gfx);
    QColor header_border_color = get_block_qcolor(SEQUENCER_TRACK_BORDER1_COLOR_NUM, is_gfx_gfx);
    QColor border_color = get_block_qcolor(SEQUENCER_BLOCK_BORDER_COLOR_NUM, is_gfx_gfx);

    QPen header_border_pen(header_border_color);
    header_border_pen.setWidthF(2.3);

    qreal x1,y1,x2,y2;
    rect.getCoords(&x1, &y1, &x2, &y2);

    // Draw track header
    {

      // background
      QRectF rect1(x1, y1, x2-x1, header_height);
      QColor header_color = get_seqblock_color(_seqtrack, seqblock).lighter(150);
      header_color.setAlpha(128);
      myFillRect(p, rect1, half_alpha(header_color, is_gfx_gfx));

      // horizontal line
      p.setPen(header_border_pen);
      p.drawLine(QLineF(x1,y1+header_height,x2,y1+header_height));

      // seqblock name
      p.setPen(text_color);
      myDrawText(p, rect.adjusted(2,1,-4,-(rect.height()-header_height)), get_seqblock_name(_seqtrack, seqblock));
    }

    // Seqblock border
    {
      bool is_current_block = seqblock->block!=NULL && seqblock->block == root->song->tracker_windows->wblock->block;

      if (is_current_block){
        QColor c = get_block_qcolor(CURSOR_EDIT_ON_COLOR_NUM, is_gfx_gfx);
        c.setAlpha(150);      
        p.setPen(QPen(c, 4));
      } else {
        p.setPen(border_color);
      }

      p.drawRoundedRect(rect,1,1);
    }

  }
  
  void draw_volume_envelope(QPainter *painter, const QRectF &rect, const struct SeqBlock *seqblock){
    if (seqblock->envelope_enabled){

      qreal x1,y1,x2,y2;
      rect.getCoords(&x1, &y1, &x2, &y2);

      double noninterior_start = get_seqblock_noninterior_start(seqblock);
      double noninterior_end = get_seqblock_noninterior_end(_seqtrack, seqblock);

      qreal ni_x1 = scale_double(noninterior_start,
                                 _start_time, _end_time,
                                 t_x1, t_x2);
      qreal ni_x2 = scale_double(noninterior_end,
                                 _start_time, _end_time,
                                 t_x1, t_x2);
      
      bool draw_all = seqblock->t.interior_start==0 && seqblock->t.interior_end==seqblock->t.default_duration;
      bool is_current = seqblock==g_curr_seqblock;
      //printf("draw_all: %d. is_current: %d. x1: %f, x1: %f, x2: %f, x2: %f\n", draw_all, is_current, x1, ni_x1, x2, ni_x2);

      // 1. (before start_interior and after end_interior)
      if(is_current && !draw_all){
        QRegion clip(ni_x1, y1, x1-ni_x1, rect.height());
        clip = clip.united(QRect(x2, y1, ni_x2-x2, rect.height()));
        painter->setClipRegion(clip);
        painter->setClipping(true);

        painter->setOpacity(0.05);
        SEQBLOCK_ENVELOPE_paint(painter, seqblock, ni_x1, y1, ni_x2, y2, true, x1, x2);
        painter->setOpacity(1.0);

        painter->setClipping(false);
      }

      // 2. (rect)
      {
        if(!draw_all){
          painter->setClipRect(rect);
          painter->setClipping(true);
        }

        SEQBLOCK_ENVELOPE_paint(painter, seqblock, ni_x1, y1, ni_x2, y2, is_current, x1, x2);

        if(!draw_all)
          painter->setClipping(false);
      }
    }
  }

  void draw_interface(QPainter *painter, const struct SeqBlock *seqblock, const QRectF &_blury_areaF){
    qreal x1,y1,x2,y2;
    _blury_areaF.getCoords(&x1, &y1, &x2, &y2);

    double border = 2.1;
    
    double xsplit1 = get_seqblock_xsplit1(x1, x2);
    double xsplit2 = get_seqblock_xsplit2(x1, x2);

    double ysplit1 = get_seqblock_ysplit1(y1, y2);
    double ysplit2 = get_seqblock_ysplit2(y1, y2);

    double width = root->song->tracker_windows->systemfontheight / 4;
    double sel_width = width*2;

    QColor color = get_qcolor(SEQUENCER_BLOCK_INTERFACE_COLOR_NUM);
    QColor fill_color(color);
    fill_color.setAlpha(80);

    QPen pen(color);
    pen.setWidthF(width);

    QPen sel_pen(color);
    sel_pen.setWidthF(sel_width);

    /* Vertical lines */
    
    // xsplit1 vertical line 1, left
    if (seqblock->block==NULL){
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

    // xsplit1 vertical line 0, left
    if (seqblock->selected_box==SB_FADE_LEFT) painter->setPen(sel_pen);  else  painter->setPen(pen);
    QLineF line1a(xsplit1, y1 + border,
                  xsplit1, ysplit1);
    painter->drawLine(line1a);

    // xsplit1 vertical line 2, left
    if (seqblock->selected_box==SB_STRETCH_LEFT) painter->setPen(sel_pen);  else  painter->setPen(pen);
    QLineF line1c(xsplit1,ysplit2,xsplit1,y2-border);
    painter->drawLine(line1c);

    // xsplit2 vertical line 0, right
    if (seqblock->selected_box==SB_FADE_RIGHT) painter->setPen(sel_pen);  else  painter->setPen(pen);
    QLineF line2a(xsplit2,y1 + border,xsplit2,ysplit1);
    painter->drawLine(line2a);

    // xsplit2 vertical line 2, right
    if (seqblock->selected_box==SB_STRETCH_RIGHT) painter->setPen(sel_pen);  else  painter->setPen(pen);
    QLineF line2c(xsplit2,ysplit2,xsplit2,y2-border);
    painter->drawLine(line2c);


    /* Horizontal lines */
    
    // ysplit1 horizontal line, left
    if (seqblock->selected_box==SB_FADE_LEFT || seqblock->selected_box==SB_INTERIOR_LEFT) painter->setPen(sel_pen);  else  painter->setPen(pen);
    QLineF line3a(x1+border, ysplit1, xsplit1-border, ysplit1);
    painter->drawLine(line3a);
    
    // ysplit1 horizontal line, right
    if (seqblock->selected_box==SB_FADE_RIGHT || seqblock->selected_box==SB_INTERIOR_RIGHT) painter->setPen(sel_pen);  else  painter->setPen(pen);
    QLineF line3b(xsplit2+border, ysplit1, x2-border, ysplit1);
    painter->drawLine(line3b);

    // ysplit2 horizontal line
    if (seqblock->selected_box==SB_STRETCH_LEFT || seqblock->selected_box==SB_INTERIOR_LEFT) painter->setPen(sel_pen);  else  painter->setPen(pen);
    QLineF line4a(x1+border, ysplit2, xsplit1-border, ysplit2);
    painter->drawLine(line4a);
    
    // ysplit2 horizontal line
    if (seqblock->selected_box==SB_STRETCH_RIGHT || seqblock->selected_box==SB_INTERIOR_RIGHT) painter->setPen(sel_pen);  else  painter->setPen(pen);
    QLineF line4b(xsplit2+border, ysplit2, x2-border, ysplit2);
    painter->drawLine(line4b);
    
    //myDrawText(*painter, _blury_areaF, "hello");

    if (seqblock->selected_box==SB_STRETCH_LEFT){
      myFillRect(*painter,
                 QRectF(QPointF(x1+border, ysplit2),
                        QPointF(xsplit1, y2-border)),
                 fill_color);
    } else if (seqblock->selected_box==SB_STRETCH_RIGHT){
      myFillRect(*painter,
                 QRectF(QPointF(xsplit2, ysplit2),
                        QPointF(x2-border, y2-border)),
                 fill_color);
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
    }
  }

  void paintSelected(QPainter &p, const QRectF &rect, const struct SeqBlock *seqblock, bool is_gfx_gfx){
    //printf("Seqblock: %p, %d\n", seqblock, seqblock->is_selected);
    if (seqblock->is_selected){
      QColor grayout_color = get_block_qcolor(SEQUENCER_BLOCK_SELECTED_COLOR_NUM, is_gfx_gfx);
      
      p.setPen(Qt::NoPen);
      p.setBrush(grayout_color);
      
      p.drawRect(rect);
      
      p.setBrush(Qt::NoBrush);
    }
  }
      
  void draw_fades(QPainter &p, const QRectF &rect, const struct SeqBlock *seqblock){
    QColor color = get_qcolor(SEQUENCER_BACKGROUND_COLOR_NUM); //mix_colors(QColor(50,50,50,200), get_qcolor(SEQUENCER_BACKGROUND_COLOR_NUM), 0.52f);
    color.setAlpha(180);
    //QColor color(50,50,50,200);

    QColor border_color(150,150,160);

    QPen pen(border_color);
    pen.setWidth(root->song->tracker_windows->systemfontheight / 8);

    QPen sel_pen(border_color);
    sel_pen.setWidthF(root->song->tracker_windows->systemfontheight / 3);

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

  void paintSeqBlockElements(QPainter &p, const QRectF &rect, const QRectF &rect_without_header, const struct SeqBlock *seqblock, bool is_gfx_gfx){
    if (seqblock->block==NULL)
      paintSampleGraphics(p, rect, seqblock, is_gfx_gfx);
    else
      paintBlockGraphics(p, rect, seqblock, is_gfx_gfx);
    
    paintSelected(p, rect, seqblock, is_gfx_gfx);
    
    paintSeqblockHeader(p, rect, seqblock, is_gfx_gfx);

    draw_fades(p, rect_without_header, seqblock);
  }
  
  bool paintSeqBlock(QPainter &p, const QRectF &update_rect, const struct SeqBlock *seqblock, bool is_gfx_gfx){
    //QPoint mousep = _sequencer_widget->mapFromGlobal(QCursor::pos());

    QRectF rect;

    // First check if we need to paint it.
    {
      /*
      double sample_rate = MIXER_get_sample_rate();
      
      double start_time = _start_time / sample_rate;
      double end_time = _end_time / sample_rate;
      */
      
      if (seqblock->t.time >= _end_time)
        return false;
      if (seqblock->t.time2 < _start_time)
        return false;
      
      double x1 = get_seqblock_x1(seqblock);
      double x2 = get_seqblock_x2(seqblock);
      //if (i==1)
      //  printf("   %d: %f, %f. %f %f\n", iterator666, x1, x2, seqblock->start_time / 44100.0, seqblock->end_time / 44100.0);

      rect = QRectF(x1,t_y1+1,x2-x1,height-2);

      //printf("%p: Start: %f, End: %f. X1: %f, X2: %f\n", seqblock, start_time, end_time, x1,x2);
      
#if 0
      // Can not use rect.toAlignedRect() because it returns a QRect, which uses 'int', which is a lot smaller than 'double' which QRectF uses.
      // Why was toAlignedRect() used in the first place?
      if (false==update_rect.intersects(rect.toAlignedRect()))
        return false;
#else
      if (false==update_rect.intersects(rect))
        return false;
#endif
    }

    int header_height = get_block_header_height();
    const QRectF rect_without_header = rect.adjusted(0, header_height, 0, 0);

    if(seqblock != g_curr_seqblock){ //!rect.contains(mousep)){ // FIX. Must be controlled from bin/scheme/mouse.scm.

      paintSeqBlockElements(p, rect, rect_without_header, seqblock, is_gfx_gfx);

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
        paintBlockGraphics(ptr, rect2, seqblock, is_gfx_gfx);
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

      paintSeqBlockElements(p, rect, rect_without_header, seqblock, is_gfx_gfx);
      
#endif
      
      draw_interface(&p, seqblock, rect_without_header);
    }

    draw_volume_envelope(&p, rect_without_header, seqblock);

    return true;
  }
  
  void paint(const QRect &update_rect, QPainter &p) { // QPaintEvent * ev ) override {
    RETURN_IF_DATA_IS_INACCESSIBLE();
    
    //printf("  PAINTING %d %d -> %d %d\n",t_x1,t_y1,t_x2,t_y2);

    myFillRect(p, _rect.adjusted(1,1,-2,-1), get_qcolor(SEQUENCER_BACKGROUND_COLOR_NUM));
      
    //double song_length = get_visible_song_length()*sample_rate;
  
    //SEQTRACK_update_all_seqblock_start_and_end_times(_seqtrack);

    for(int i=0;i<2;i++){
      bool is_gfx_gfx = i==1;

      QVector<struct SeqBlock*> seqblocks = SEQTRACK_get_seqblocks_in_z_order(_seqtrack, is_gfx_gfx);
      //printf("  seqblocks size: %d. (%d %d)\n", seqblocks.size(), _seqtrack->seqblocks.num_elements, _seqtrack->gfx_seqblocks==NULL ? -1 : _seqtrack->gfx_seqblocks->num_elements);
      
      for(int i=seqblocks.size()-1 ; i>=0 ; i--)
        paintSeqBlock(p, update_rect, seqblocks.at(i), is_gfx_gfx);
    }

    // Automation
    SEQTRACK_AUTOMATION_paint(&p, _seqtrack, t_x1, t_y1, t_x2, t_y2, _start_time, _end_time);


    // Current track border and non-current track shadowing
    {
      if (_seqtrack==(struct SeqTrack*)root->song->seqtracks.elements[ATOMIC_GET(root->song->curr_seqtracknum)]){
        if (root->song->seqtracks.num_elements > 1){
          QPen pen(get_qcolor(SEQUENCER_CURRTRACK_BORDER_COLOR_NUM));
          //pen.setWidthF(1);
          p.setPen(pen);
          p.drawLine(t_x1, t_y1, t_x2, t_y1);
          p.drawLine(t_x1, t_y2, t_x2, t_y2);
        }
      }else{
        QColor color(0,0,0,35);
        //QColor color = get_qcolor(SEQUENCER_CURRTRACK_BORDER_COLOR_NUM);
        myFillRect(p, QRectF(t_x1,t_y1,width,height), color);
      }
    }
    
  }


#define UPDATE_EVERY_5_SECONDS 0

#if UPDATE_EVERY_5_SECONDS
  QTime _time;
#endif
  int _last_num_seqblocks = 0;
  
  void call_very_often(void){
    
    if (_last_num_seqblocks != gfx_seqblocks(_seqtrack)->num_elements) {
      SEQUENCER_update();
      _last_num_seqblocks = gfx_seqblocks(_seqtrack)->num_elements;
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
    
#if 0
  Seqblock_widget *get_seqblock_widget(int seqblocknum){
    if (seqblocknum >= _seqblock_widgets.size())
      create_seqblock_widgets();
  
    R_ASSERT_RETURN_IF_FALSE2(seqblocknum < _seqblock_widgets.size(), NULL);

    return _seqblock_widgets.at(seqblocknum);
  }
#endif
  
};




class Seqtrack_widget : public QWidget, public Ui::Seqtrack_widget {
  Q_OBJECT

 public:

  Seqblocks_widget _seqblocks_widget;
  SeqTrack *_seqtrack;

  Seqtrack_widget(QWidget *parent, SeqTrack *seqtrack, const double &start_time, const double &end_time)
    : QWidget(parent)
    , _seqblocks_widget(parent, seqtrack, start_time, end_time)
    , _seqtrack(seqtrack)
  {    
    setupUi(this);

    // Remove widget we don't use yet
#if 1
    header_widget->hide();
#endif
    
    //main_layout->addWidget(_seqblocks_widget);

    add_gc_root(_seqtrack);
  }

  ~Seqtrack_widget(){
    remove_gc_root(_seqtrack);
    printf("Seqtrack widget deleted\n");
  }



  double t_x1,t_y1,t_x2,t_y2,t_width,t_height;
  
  void position_widgets(double x1, double y1, double x2, double y2){
    t_x1 = x1;
    t_y1 = y1;
    t_x2 = x2;
    t_y2 = y2;
    t_height = t_y2-t_y1;
    t_width = t_x2-t_x1;
    
    double x1_b = x1;// + width();
    _seqblocks_widget.position_widgets(x1_b, y1, x2, y2);
  }

  void set_seqblocks_is_selected(const QRect &selection_rect){
    _seqblocks_widget.set_seqblocks_is_selected(selection_rect);
  }

  void paint(const QRect &update_rect, QPainter &p){
    _seqblocks_widget.paint(update_rect, p);
  }
  
  void my_update(void){
    //_seqblocks_widget.update();
  }

  void call_very_often(void){
    _seqblocks_widget.call_very_often();
  }

public slots:

};


namespace{

class Seqtracks_widget : public radium::VerticalScroll {
public:

  QWidget *_sequencer_widget;
  
  QVector<Seqtrack_widget*> _seqtrack_widgets;

  const double &_start_time;
  const double &_end_time;
  
  Seqtracks_widget(QWidget *sequencer_widget, const double &start_time, const double &end_time)
    : radium::VerticalScroll(sequencer_widget)
    , _sequencer_widget(sequencer_widget)
    ,_start_time(start_time)
    ,_end_time(end_time)
  {
    update_seqtracks();
    setContentsMargins(0,0,0,0);
    layout->setContentsMargins(0,0,0,0);
    layout->setSpacing(0);

    setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
  }


  double t_x1=0,t_y1=0,t_x2=50,t_y2=50;
  double width=0,height=0;
  
  void position_widgets(double x1, double y1, double x2, double y2){
    t_x1 = x1;
    t_y1 = y1;
    t_x2 = x2;
    t_y2 = y2;
    width = t_x2-t_x1;
    height = t_y2-t_y1;

#if 0
    int num_seqtracks = _seqtrack_widgets.size();
    
    if (num_seqtracks > 5) {      
      //setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOn);
      for(auto *seqtrack_widget : _seqtrack_widgets){
        seqtrack_widget->setMinimumHeight(height / 5);
        seqtrack_widget->setMaximumHeight(height / 5);
      }
    }else {
      //setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
      for(auto *seqtrack_widget : _seqtrack_widgets){
        seqtrack_widget->setMinimumHeight(height / 5);
        seqtrack_widget->setMaximumHeight(5000000);
      }
    }
#endif
    
    for(auto *seqtrack_widget : _seqtrack_widgets){
      double y1_b = y1+seqtrack_widget->y();
      double y2_b = y1_b + seqtrack_widget->height();
      seqtrack_widget->position_widgets(x1, y1_b, x2, y2_b);
    }

  }  

  void set_seqblocks_is_selected(const QRect &selection_rect){
    for(auto *seqtrack_widget : _seqtrack_widgets)
      seqtrack_widget->set_seqblocks_is_selected(selection_rect);
  }

  void paint(const QRect &update_rect, QPainter &p){
    position_widgets(t_x1, t_y1, t_x2, t_y2);

    // seqtracks
    //
    for(auto *seqtrack_widget : _seqtrack_widgets)
      seqtrack_widget->paint(update_rect, p);
  }

  void my_update(void){
    for(auto *seqtrack_widget : _seqtrack_widgets)
      seqtrack_widget->my_update();
    
    update();
  }

  void update_seqtracks(void){
    SEQUENCER_ScopedGfxDisable gfx_disable;
    
    //printf("  Updating seqtracks\n");
    
    for(auto *seqtrack_widget : _seqtrack_widgets)
      delete seqtrack_widget;
      
    _seqtrack_widgets.clear();
    
    VECTOR_FOR_EACH(struct SeqTrack *, seqtrack, &root->song->seqtracks){
      Seqtrack_widget *seqtrack_widget = new Seqtrack_widget(_sequencer_widget, seqtrack, _start_time, _end_time);
      _seqtrack_widgets.push_back(seqtrack_widget);
      addWidget(seqtrack_widget);
    }END_VECTOR_FOR_EACH;

    SEQUENCER_update(); // If we only call update(), the navigator won't update.
    //position_widgets(t_x1, t_y1, t_x2, t_y2);
  }

  
  void call_very_often(void){
    SEQUENCER_ScopedGfxDisable gfx_disable;
    
    if (_seqtrack_widgets.size() != root->song->seqtracks.num_elements)
      update_seqtracks();

    int i = 0;
    for(auto *seqtrack : _seqtrack_widgets){
      if (root->song->seqtracks.elements[i] != seqtrack->_seqtrack){
        update_seqtracks();
        call_very_often();
        return;
      }
      i++;

      seqtrack->call_very_often();
    }
  }

  Seqtrack_widget *get_seqtrack_widget(const struct SeqTrack *seqtrack){
    for(auto *seqtrack_widget : _seqtrack_widgets)
      if (seqtrack_widget->_seqtrack==seqtrack)
        return seqtrack_widget;
    
    //R_ASSERT(false);    
    return NULL; // Think it may legitimately happen if gui hasn't been updated yet.
  }
  
  Seqtrack_widget *get_seqtrack_widget(int seqtracknum){
    if (seqtracknum >= _seqtrack_widgets.size())
      update_seqtracks();

    if (seqtracknum >= _seqtrack_widgets.size())
      return NULL;
    
    R_ASSERT_RETURN_IF_FALSE2(seqtracknum<_seqtrack_widgets.size(), NULL);

    //printf("%d: y: %d\n",seqtracknum,_seqtrack_widgets.at(seqtracknum)->pos().y());
    
    return _seqtrack_widgets.at(seqtracknum);
  }

};


struct SongTempoAutomation_widget { //: public MouseTrackerQWidget {
  bool is_visible = false;
   
  const double &_start_time;
  const double &_end_time;
   
  double t_x1,t_y1,t_x2,t_y2,width,height;
  QRectF _rect;
   
  SongTempoAutomation_widget(QWidget *parent, const double &start_time, const double &end_time)
  //: MouseTrackerQWidget(parent)
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
    
    _rect = QRectF(t_x1, t_y1, width, height);
  }

  void paint(const QRect &update_rect, QPainter &p){

    myFillRect(p, _rect.adjusted(1,1,-2,-1), get_qcolor(SEQUENCER_BACKGROUND_COLOR_NUM));
    
    //printf("height: %d\n",height());
    TEMPOAUTOMATION_paint(&p, t_x1, t_y1, t_x2, t_y2, _start_time, _end_time);
  }

};

#if 1

#include <functional>

namespace{
  enum class WhatToFind{
    NOTHING,
    LINE,
    BEAT,
    BAR
  };
}

// Return true if continuing.
static inline bool iterate_beats_between_seqblocks(const struct SeqTrack *seqtrack,
                                                   int &barnum,
                                                   int64_t start_seqtime, int64_t end_seqtime,
                                                   WhatToFind what_to_find,
                                                   int64_t end_blockseqtime, int64_t next_blockstarttime,
                                                   int64_t last_barseqtime, int64_t last_beatseqtime,
                                                   std::function<bool(int64_t,int,int)> callback
                                                   )
{
    int64_t bar_seqlength = end_blockseqtime - last_barseqtime;
    int64_t beat_seqlength = end_blockseqtime - last_beatseqtime;

    //printf("last_x: %d, width: %d. bar_length: %d. next: %f, end_seqtime: %f\n", (int)last_x, width(),(int)bar_length,((double)end_blockseqtime + bar_length)/44100.0, (double)end_seqtime/44100.0);

    int beatnum = 1;
    int64_t bar_seqtime = end_blockseqtime;

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

    

    for(;;){

      if (next_blockstarttime != -1 && R_ABS(bar_seqtime-next_blockstarttime) < 20)
        return true;

      if (bar_seqtime >= end_seqtime)
        return false;

      barnum++;
      beatnum = 1;
      callback(bar_seqtime, barnum, 0);


      if (what_to_find==WhatToFind::BEAT) {

        int64_t next_bartime = bar_seqtime + bar_seqlength;

        for(int64_t beat_seqtime = bar_seqtime ; beat_seqtime < next_bartime ; beat_seqtime += beat_seqlength){

          if (next_blockstarttime != -1 && R_ABS(beat_seqtime-next_blockstarttime) < 20)
            return true;

          if (beat_seqtime >= end_seqtime)
            return false;

          if (callback(beat_seqtime, barnum, beatnum)==false)
            return false;

          beatnum++;
        }
      }

      //printf("  after. abstime: %f\n",(double)abstime/44100.0);

      bar_seqtime += bar_seqlength;
    }

    return true;
}

// Next (or current) seqblock when ignoring audio seqblocks.
int get_next_block_seqblocknum(const struct SeqTrack *seqtrack, int seqtracknum){
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

// If callback returns false, we stop iterating.
static inline void SEQUENCER_iterate_time(int64_t start_seqtime, int64_t end_seqtime, WhatToFind what_to_find, std::function<bool(int64_t,int,int)> callback){
  int barnum = 0;
    
  const struct SeqTrack *seqtrack = (struct SeqTrack*)root->song->seqtracks.elements[0];
  if (seqtrack->seqblocks.num_elements==0)
    return;
        
  int next_seqblocknum = get_next_block_seqblocknum(seqtrack, 0);

  while(next_seqblocknum != -1){
    int seqblocknum = next_seqblocknum;
    next_seqblocknum = get_next_block_seqblocknum(seqtrack, seqblocknum + 1);

    const struct SeqBlock *seqblock = (struct SeqBlock *)seqtrack->seqblocks.elements[seqblocknum];
    const struct SeqBlock *next_seqblock = next_seqblocknum==-1 ? NULL : (struct SeqBlock *)seqtrack->seqblocks.elements[next_seqblocknum];

    int64_t start_blockseqtime = seqblock->t.time;
    int64_t end_blockseqtime = seqblock->t.time2;

    int64_t next_blockstarttime = next_seqblock==NULL ? -1 : next_seqblock->t.time;
          
    if (next_seqblock!=NULL) {
      if (next_blockstarttime <= start_seqtime)
        continue;
    } else {
      if (end_blockseqtime <= start_seqtime)
        continue;
    }

    if (start_blockseqtime >= end_seqtime)
      return;

    const struct Blocks *block = seqblock->block;
    const struct Beats *beat = block->beats;
          
    int64_t last_barseqtime = -1;
    int64_t last_beatseqtime = -1;
    //Ratio *last_signature = NULL;

    while(beat!=NULL){
      //last_signature = &beat->valid_signature;

      int64_t seqtime = start_blockseqtime + blocktime_to_seqtime(seqblock, Place2STime(block, &beat->l.p));
      //printf("  Beat seqtime: %d. %d/%d\n", (int)seqtime, beat->beat_num, beat->bar_num);
      last_beatseqtime = seqtime;

      if (beat->beat_num==1){
        last_barseqtime = seqtime;
        
        barnum++;
      }

      if (what_to_find==WhatToFind::BEAT){
        if (callback(seqtime, barnum, beat->beat_num)==false)
          return;
      }
      
      beat = NextBeat(beat);
    }

    if (iterate_beats_between_seqblocks(seqtrack,
                                        barnum,
                                        start_seqtime, end_seqtime,
                                        what_to_find,
                                        end_blockseqtime, next_blockstarttime,
                                        last_barseqtime, last_beatseqtime,
                                        callback)
        == false)
      return;
  }
}
#endif

struct Timeline_widget : public MouseTrackerQWidget {
  const double &_start_time;
  const double &_end_time;
  
  Timeline_widget(QWidget *parent, const double &start_time, const double &end_time)
    :MouseTrackerQWidget(parent)
    ,_start_time(start_time)
    ,_end_time(end_time)
  {    
  }

  void wheelEvent(QWheelEvent *e) override {
    handle_wheel_event(e, 0, width(), _start_time, _end_time);
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

  void paintEvent ( QPaintEvent * ev ) override {
    TRACK_PAINT();

    RETURN_IF_DATA_IS_INACCESSIBLE();
    
    QPainter p(this);

    const double y1 = 4;
    const double y2 = height() - 4;
    const double t1 = (y2-y1) / 2.0;

    p.setRenderHints(QPainter::Antialiasing,true);

    QColor border_color = get_qcolor(SEQUENCER_BORDER_COLOR_NUM);
    //QColor text_color = get_qcolor(MIXER_TEXT_COLOR_NUM);

    QRect rect(1,1,width()-1,height()-2);
    p.fillRect(rect, get_qcolor(SEQUENCER_TIMELINE_BACKGROUND_COLOR_NUM));
    
    //p.setPen(text_color);
    //p.drawText(4,2,width()-6,height()-4, Qt::AlignLeft, "timeline");
    
    p.setPen(border_color);
    p.drawRect(rect);

    // This code is copied from hurtigmixer. (translated from scheme)

    QColor timeline_arrow_color = get_qcolor(SEQUENCER_TIMELINE_ARROW_COLOR_NUM);
    QColor timeline_arrow_color_alpha = timeline_arrow_color;
    timeline_arrow_color_alpha.setAlpha(20);

    p.setBrush(timeline_arrow_color);

    QColor text_color = get_qcolor(SEQUENCER_TEXT_COLOR_NUM);
    QColor text_color_alpha = text_color;
    text_color_alpha.setAlpha(20);

    p.setPen(text_color);
        
    const QFontMetrics fn = QFontMetrics(QApplication::font());
    double min_pixels_between_text;
    if (showBarsInTimeline())
      min_pixels_between_text = fn.width("125") + 10;
    else
      min_pixels_between_text = fn.width("00:00:00") + t1*2 + 10;
    
      //double  = 40; //width() / 4;

    const double loop_start_x = scale_double(SEQUENCER_get_loop_start(), _start_time, _end_time, 0, width());
    const double loop_end_x = scale_double(SEQUENCER_get_loop_end(), _start_time, _end_time, 0, width());

    const double start_time = _start_time / MIXER_get_sample_rate();
    const double end_time = _end_time / MIXER_get_sample_rate();

    //if (end_time >= start_time)
    //  return;
    R_ASSERT_RETURN_IF_FALSE(end_time > start_time);

    int inc_time = R_MAX(1, ceil(scale_double(min_pixels_between_text, 0, width(), 0, end_time-start_time)));

    bool did_draw_alpha = false;

    if (showBarsInTimeline()){

      QColor bar_color(text_color);
      bar_color.setAlpha(50);
      
      //double x1 = _seqtracks_widget.t_x1;
      //double x2 = _seqtracks_widget.t_x2;
      const double width_ = width(); //x2-x1;
      double last_x = -10000;
      //int barnum = 1;

      //p.setPen("red");
      
      const struct SeqTrack *seqtrack = (struct SeqTrack*)root->song->seqtracks.elements[0];
      int64_t start_seqtime = _start_time;
      int64_t end_seqtime = _end_time;

      auto callback = [&last_x, &p, &bar_color, &text_color, width_, this, seqtrack, min_pixels_between_text]
        (int64_t seqtime, int barnum, int beatnum)
        {
                                               
          double x = scale_double(seqtime, _start_time, _end_time, 0, width_);
          if (x >= width_)
            return false;
          
          
          {
            p.setPen(bar_color);
            
            double y1 = 2;
            double y2 = height()/2;
            if(beatnum==1)
              y2=height() - 2;
            QLineF line(x, y1, x, y2);
            p.drawLine(line);
            if (beatnum!=1)
              return true;
          }
          
          //printf("%d: %d/%d.\n", (int)seqtime, barnum, beatnum);
          
          if (x >= 0 && x > last_x + min_pixels_between_text) {
            p.setPen(text_color);
            
            QRectF rect(x + 2, 2, width_, height());                 
            myDrawText(p, rect, QString::number(barnum));             
            
            last_x = x;
          }
          
          return true;
        };
    
      SEQUENCER_iterate_time(start_seqtime, end_seqtime, WhatToFind::BEAT, callback);
                             
      
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
      
      // Another time? (might be a copy and paste error)
      if ((end_time-start_time) > 110)
        if (inc_time%30 != 0)
          inc_time += 30-(inc_time%30);
      }
      //inc_time = 1;

      int64_t time = inc_time * int((double)start_time/(double)inc_time);
    
      for(;;){
        const double x = scale_double(time, start_time, end_time, 0, width());
        if (x >= width())
          break;

        bool draw_alpha = false;

        if (loop_start_x >= x-t1 && loop_start_x <= x+t1+min_pixels_between_text)
          draw_alpha = true;
        if (loop_end_x >= x-t1 && loop_end_x <= x+t1+min_pixels_between_text)
          draw_alpha = true;

        if (draw_alpha==true && did_draw_alpha==false){
          p.setPen(text_color_alpha);
          p.setBrush(timeline_arrow_color_alpha);
        }

        if (draw_alpha==false && did_draw_alpha==true){
          p.setPen(text_color);
          p.setBrush(timeline_arrow_color);
        }

        did_draw_alpha = draw_alpha;
      
        if (x > 20) {
        
          draw_filled_triangle(p, x-t1, y1, x+t1, y1, x, y2);
        
          QRectF rect(x + t1 + 4, 2, width(), height());
          myDrawText(p, rect, seconds_to_timestring(time));
        }

        time += inc_time;
      }
    }
    
    p.setPen(Qt::black);
    p.setBrush(QColor(0,0,255,150));

    // loop start
    {
      draw_filled_triangle(p, loop_start_x-t1*2, y1, loop_start_x, y1, loop_start_x, y2);
    }

    // loop end
    {
      draw_filled_triangle(p, loop_end_x, y1, loop_end_x+t1*2, y1, loop_end_x, y2);
    }
  }

};

struct Seqtracks_navigator_widget : public MouseTrackerQWidget {
  const double &_start_time;
  const double &_end_time;
  Seqtracks_widget &_seqtracks_widget;
  
  Seqtracks_navigator_widget(QWidget *parent, const double &start_time, const double &end_time, Seqtracks_widget &seqtracks_widget)
    : MouseTrackerQWidget(parent)
    , _start_time(start_time)
    , _end_time(end_time)
    , _seqtracks_widget(seqtracks_widget)
  {
  }

  void wheelEvent(QWheelEvent *e) override {
    handle_wheel_event(e, 0, width(), 0, get_visible_song_length()*MIXER_get_sample_rate());
  }

private:
  double get_x1(double total){
    return scale_double(_start_time, 0, total, 0, width());
  }
  double get_x2(double total){
    return scale_double(_end_time, 0, total, 0, width());
  }
public:

  double get_x1(void){
    return get_x1(get_visible_song_length()*MIXER_get_sample_rate());
  }
  double get_x2(void){
    return get_x2(get_visible_song_length()*MIXER_get_sample_rate());
  }
  
  void paintEvent ( QPaintEvent * ev ) override {
    TRACK_PAINT();

    RETURN_IF_DATA_IS_INACCESSIBLE();
    
    QPainter p(this);

    double total_seconds = get_visible_song_length();
    double total = total_seconds*MIXER_get_sample_rate();
    
    p.setRenderHints(QPainter::Antialiasing,true);

    QColor border_color = get_qcolor(SEQUENCER_BORDER_COLOR_NUM);      
    

    // Background
    //
    QRect rect1(1,1,width()-1,height()-2);
    p.fillRect(rect1, get_qcolor(SEQUENCER_BACKGROUND_COLOR_NUM));

    
    // Seqblocks
    {

      //QColor block_color = QColor(140,140,140,180);
      QColor text_color = get_qcolor(SEQUENCER_TEXT_COLOR_NUM);
      
      int num_seqtracks = root->song->seqtracks.num_elements;
      int seqtracknum = 0;
      for(Seqtrack_widget *seqtrack_widget : _seqtracks_widget._seqtrack_widgets) {
        struct SeqTrack *seqtrack = seqtrack_widget->_seqtrack;
        
        double y1 = scale_double(seqtracknum,   0, num_seqtracks, 3, height()-3);
        double y2 = scale_double(seqtracknum+1, 0, num_seqtracks, 3, height()-3);

        /*
        SEQTRACK_update_all_seqblock_start_and_end_times(seqtrack);
        //double start_time = _start_time / MIXER_get_sample_rate();
        //double end_time = _end_time / MIXER_get_sample_rate();
        */
        
        VECTOR_FOR_EACH(struct SeqBlock *, seqblock, gfx_seqblocks(seqtrack)){

          QColor seqblock_color = get_seqblock_color(seqtrack, seqblock).lighter(150);
          seqblock_color.setAlpha(128);
          
          //printf("\n\n\n Start/end: %f / %f. Seqtrack/seqblock %p / %p\n\n", seqblock->start_time, seqblock->end_time, seqtrack, seqblock);
          
          double x1 = scale_double(seqblock->t.time, 0, total, 0, width()); //seqtrack_widget->_seqblocks_widget->get_seqblock_x1(seqblock, start_time, end_time);
          double x2 = scale_double(seqblock->t.time2, 0, total, 0, width()); //seqtrack_widget->_seqblocks_widget->get_seqblock_x2(seqblock, start_time, end_time);
          
          QRectF rect(x1,y1+1,x2-x1,y2-y1-2);
          myFillRect(p, rect, seqblock_color);

          if(rect.height() > root->song->tracker_windows->systemfontheight*1.3){
            p.setPen(text_color);
            //myDrawText(p, rect.adjusted(2,1,-1,-1), QString::number(block->l.num) + ": " + block->name);
            p.drawText(rect.adjusted(2,1,-1,-1), get_seqblock_name(seqtrack, seqblock), QTextOption(Qt::AlignLeft | Qt::AlignTop));
          }
          
          p.setPen(border_color);
          p.drawRect(rect);
          
        }END_VECTOR_FOR_EACH;
        
        seqtracknum++;
      }
    }


    // Navigator
    //
    {
      double x1 = get_x1(total);
      double x2 = get_x2(total);

      QRectF rectA(0,  1, x1,         height()-2);
      QRectF rectB(x2, 1, width()-x2, height()-2);      
      QRectF rect2(x1, 1, x2-x1,      height()-2);

      {
        QColor grayout_color = get_qcolor(SEQUENCER_NAVIGATOR_GRAYOUT_COLOR_NUM);
      
        p.fillRect(rectA, grayout_color);
        p.fillRect(rectB, grayout_color);
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
    
  }

};

struct Sequencer_widget : public MouseTrackerQWidget {

  int _old_width = 600;
  double _start_time = 0; // abstime
  double _end_time = 600; // abstime
  double _samples_per_pixel;

  enum GridType _grid_type = NO_GRID;

  SongTempoAutomation_widget _songtempoautomation_widget;
  Timeline_widget _timeline_widget;
  Seqtracks_widget _seqtracks_widget;
  Seqtracks_navigator_widget _navigator_widget;
  //MyQSlider _main_reltempo;

  bool _has_selection_rectangle = false;
  QRect _selection_rectangle;

  Sequencer_widget(QWidget *parent)
    : MouseTrackerQWidget(parent)
    , _end_time(get_visible_song_length()*MIXER_get_sample_rate())
    , _samples_per_pixel((_end_time-_start_time) / width())
    , _songtempoautomation_widget(this, _start_time, _end_time)
    , _timeline_widget(this, _start_time, _end_time)
    , _seqtracks_widget(this, _start_time, _end_time)
    , _navigator_widget(this, _start_time, _end_time, _seqtracks_widget)
      //, _main_reltempo(this)
  {
    setAcceptDrops(true);
    
    _timeline_widget.show();
    _seqtracks_widget.show();
    _navigator_widget.show();
    /*
    _main_reltempo.show();
    */

    setSizePolicy(QSizePolicy::Minimum,QSizePolicy::MinimumExpanding);
    

    int height = root->song->tracker_windows->systemfontheight*1.3 * 13;
    setMinimumHeight(height);
    //setMaximumHeight(height);
  }

  void legalize_start_end_times(void){
    if (_start_time < 0)
      _start_time = 0;
    if (_end_time < _start_time+10)
      _end_time = _start_time+10;
  }
  
  void my_update(void){
    D(printf("SEQ: my_update called\n"));
    int64_t visible_song_length = MIXER_get_sample_rate() * get_visible_song_length();
    if (_end_time > visible_song_length) {
      _end_time = visible_song_length;
      if (_start_time >= _end_time)
        _start_time = 0;
      legalize_start_end_times();
    }
    
    _timeline_widget.update();
    _seqtracks_widget.my_update();
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
  

  int getSeqtrackNumFromY(float y){
    for(int seqtracknum=0;seqtracknum<root->song->seqtracks.num_elements;seqtracknum++){
      //printf("y: %f. %d: %f\n", y, seqtracknum, getSeqtrackY1(seqtracknum));
      if (y < getSeqtrackY2(seqtracknum))
        return seqtracknum;
    }

    return root->song->seqtracks.num_elements-1;
  }

  void dropEvent(QDropEvent *event) override {
    printf("               GOT DOP\n");
    if (event->mimeData()->hasUrls())
      {
        foreach (QUrl url, event->mimeData()->urls())
          {
            QPoint point = mapToEditor(this, event->pos());
            
            float x = point.x();
            float y = point.y();
            
            printf("File: -%s-. Y: %f\n", url.toLocalFile().toUtf8().constData(), y);
            int seqtracknum = getSeqtrackNumFromY(y);
            //struct SeqTrack *seqtrack = (struct SeqTrack*)root->song->seqtracks.elements[seqtracknum];
            int64_t pos = round(scale_double(x,
                                             getSequencerX1(), getSequencerX2(),
                                             getSequencerVisibleStartTime(), getSequencerVisibleEndTime()
                                             )
                                );
            createSampleSeqblock(seqtracknum,
                                 STRING_get_chars(STRING_toBase64(STRING_create(url.toLocalFile()))),
                                 getSeqGriddedTime(pos, seqtracknum, getSeqBlockGridType()),
                                 -1
                                 );
          }
      }
  }


  void wheelEvent(QWheelEvent *e) override {
    handle_wheel_event(e, 0, width(), _start_time, _end_time);
  }

  void resizeEvent( QResizeEvent *qresizeevent) override {
    RETURN_IF_DATA_IS_INACCESSIBLE();
    
    //  set_end_time();
    // _samples_per_pixel = (_end_time-_start_time) / width();
    position_widgets();
  }
  
  void position_widgets(void){
    R_ASSERT_RETURN_IF_FALSE(_seqtracks_widget._seqtrack_widgets.size() > 0);

    //printf("   ***** Posisiotioing sequencer widgets ********\n");
    
#if 0
    const QWidget *mute_button = _seqtracks_widget._seqtrack_widgets.at(0)->mute_button;
    const QPoint p = mute_button->mapTo(this, mute_button->pos());
#endif

    const int x1 = 0; //p.x() + mute_button->width();
    const int x1_width = width() - x1;

    QFontMetrics fm(QApplication::font());
    double systemfontheight = fm.height();

    const int bottom_height = R_BOUNDARIES(30, root->song->seqtracks.num_elements * (systemfontheight/2), height()/3);

    const int timeline_widget_height = systemfontheight*1.3 + 2;
 
    int y1 = 0;
    

    // timeline
    //
    _timeline_widget.setGeometry(x1, y1,
                                 x1_width, timeline_widget_height);


    y1 += timeline_widget_height;

    
    // song tempo automation
    //
    {
      const int songtempoautomation_widget_height = systemfontheight*1.3*3.5;

      int y2 = y1 + songtempoautomation_widget_height;

      _songtempoautomation_widget.position_widgets(1, y1,
                                                   width()-1, y2);

      if (_songtempoautomation_widget.is_visible)
        y1 = y2;
    }
    
    
    // sequencer tracks
    //
    {
      int y2 = height() - bottom_height;
      _seqtracks_widget.setGeometry(0, y1,
                                    1, y2 - y1);
      
      _seqtracks_widget.position_widgets(1,y1,
                                         width()-1, y2);

      y1 = y2;
    }


    // navigator
    //
    _navigator_widget.setGeometry(x1, y1,
                                  x1_width, bottom_height);

    /*
    _main_reltempo.setGeometry(0, y1,
                               x1, bottom_height);
    */

  }

  
  int _last_num_seqtracks = 0;
  double _last_visible_song_length = 0;
  
  const double cursor_width = 2.7;
  double _last_painted_cursor_x = 0.0f;
  
  double get_curr_cursor_x(int frames_to_add) const {
    if (is_playing() && pc->playtype==PLAYSONG && smooth_scrolling())
      return (_seqtracks_widget.t_x1 + _seqtracks_widget.t_x2) / 2.0;
    else
      return scale_double(ATOMIC_DOUBLE_GET(pc->song_abstime)+frames_to_add, _start_time, _end_time, _seqtracks_widget.t_x1, _seqtracks_widget.t_x2);
  }

  bool _song_tempo_automation_was_visible = false;
  bool _was_playing_smooth_song = false;

  void call_very_often(void){

    RETURN_IF_DATA_IS_INACCESSIBLE();
      
    if (_song_tempo_automation_was_visible != _songtempoautomation_widget.is_visible){
      _song_tempo_automation_was_visible = _songtempoautomation_widget.is_visible;
      position_widgets();
    }

    if (is_called_every_ms(15)){  // call each 15 ms. (i.e. more often than vsync)
      _seqtracks_widget.call_very_often();    
      if (g_need_update){
        my_update();
        BS_UpdatePlayList();
        g_need_update=false;
      }
    }

    // Check if the number of seqtracks have changed
    //
    if (is_called_every_ms(50)){
      bool do_update = _seqtracks_widget._seqtrack_widgets.size() != _last_num_seqtracks;
      
      if (!do_update) {
        if (is_called_every_ms(1000)) // This is only an insurance. SEQUENCER_update is supposed to be called manually when needed.
          if (_last_visible_song_length != get_visible_song_length()) {
            do_update = true;
            _last_visible_song_length = get_visible_song_length();
          }
      }  
      
      if (do_update){
        position_widgets();
        my_update();
        _last_num_seqtracks = _seqtracks_widget._seqtrack_widgets.size();
      }
    }

    // Update cursor
    //
    if (is_called_every_ms(15)){  // call each 15 ms. (i.e. more often than vsync)
      if (is_playing() && pc->playtype==PLAYSONG) {

        double song_abstime = ATOMIC_DOUBLE_GET(pc->song_abstime);
        double middle = (_start_time+_end_time) / 2.0;
        
        if (!smooth_scrolling()){
          double x = get_curr_cursor_x(1 + MIXER_get_sample_rate() * 60.0 / 1000.0);
          
          double x_min = R_MIN(x-cursor_width/2.0, _last_painted_cursor_x-cursor_width/2.0) - 2;
          double x_max = R_MAX(x+cursor_width/2.0, _last_painted_cursor_x+cursor_width/2.0) + 2;
          
          //printf("x_min -> x_max: %f -> %f\n",x_min,x_max);
          double y1 = _songtempoautomation_widget.t_y1;
          double y2 = _seqtracks_widget.t_y2;
          
          update(x_min, y1, 1+x_max-x_min, y2-y1);
          
          if (song_abstime < _start_time) {
            int64_t diff = _start_time - song_abstime;
            _start_time -= diff;
            _end_time -= diff;
            update();
          } else if (song_abstime > _end_time){
            double diff = song_abstime - middle;
            _start_time += diff;
            _end_time += diff;
            update();
          }

        } else {
          
          // Smooth scrolling

          _was_playing_smooth_song = true;

          if (song_abstime != middle){
            double diff = song_abstime - middle;
            _start_time += diff;
            _end_time += diff;
            update();
          }

          return;

        }
      }

      if (_was_playing_smooth_song==true){
        if (_start_time < 0){
          _end_time -= _start_time;
          _start_time = 0;
          legalize_start_end_times();
        }
        update();
        _was_playing_smooth_song = false;
      }
    }
  }

  void set_seqblocks_is_selected(void){
    _seqtracks_widget.set_seqblocks_is_selected(_selection_rectangle);
  }


  void paintGrid(const QRect &update_rect, QPainter &p, enum GridType grid_type) const {
    double x1 = _seqtracks_widget.t_x1;
    double x2 = _seqtracks_widget.t_x2;
    double width = x2-x1;
    
    double y1 = _songtempoautomation_widget.t_y1;
    double y2 = _seqtracks_widget.t_y2;

    if (grid_type==BAR_GRID) {
      
      int inc = (_end_time-_start_time) / width;
      
      if (inc > 0) {
        
        QPen pen(get_qcolor(SEQUENCER_GRID_COLOR_NUM));
        p.setPen(pen);
        
        int64_t last_bar = -50000;
        int64_t abstime = _start_time;

        // This code seems very inefficient...
        
        while(abstime < _end_time){
          int64_t maybe = SEQUENCER_find_closest_bar_start(0, abstime);
          if (maybe > last_bar){
            double x = scale_double(maybe, _start_time, _end_time, 0, width);
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
      
  void paintCursor(const QRect &update_rect, QPainter &p){
    double y1 = _songtempoautomation_widget.t_y1;
    double y2 = _seqtracks_widget.t_y2;
    
    QPen pen(get_qcolor(SEQUENCER_CURSOR_COLOR_NUM));
    pen.setWidthF(cursor_width);
    
    _last_painted_cursor_x = get_curr_cursor_x(0);
    
    QLineF line(_last_painted_cursor_x, y1+2, _last_painted_cursor_x, y2-4);
    
    p.setPen(pen);
    p.drawLine(line);
  }
      
  void paintSeqloop(const QRect &update_rect, QPainter &p) const {
    double y1 = _songtempoautomation_widget.t_y1;
    double y2 = _seqtracks_widget.t_y2;

    double x_start = scale_double(SEQUENCER_get_loop_start(), _start_time, _end_time, 0, width());
    double x_end = scale_double(SEQUENCER_get_loop_end(), _start_time, _end_time, 0, width());

    QColor grayout_color = get_qcolor(SEQUENCER_NAVIGATOR_GRAYOUT_COLOR_NUM);

    p.setPen(Qt::NoPen);
    p.setBrush(grayout_color);

    if (x_start > 0){
      QRectF rect(0, y1, x_start,  y2);
      p.drawRect(rect);
    }

    if (x_end < width()){
      QRectF rect(x_end, y1, width(),  y2);
      p.drawRect(rect);
    }
  }
      
  void paintSelectionRectangle(const QRect &update_rect, QPainter &p) const {
    QColor grayout_color = QColor(220,220,220,0x40); //get_qcolor(SEQUENCER_NAVIGATOR_GRAYOUT_COLOR_NUM);

    p.setPen(Qt::black);
    p.setBrush(grayout_color);

    p.drawRect(_selection_rectangle);
  }

  void paintEvent (QPaintEvent *ev) override {
    D(static int num_calls = 0;
      printf("   SEQ paintEvent called %d, %d -> %d, %d (%d)\n", ev->rect().x(), ev->rect().y(), ev->rect().width(), ev->rect().height(),num_calls++)
      );
    TRACK_PAINT();

    RETURN_IF_DATA_IS_INACCESSIBLE();

    QPainter p(this);

    p.eraseRect(rect()); // We don't paint everything.
    
    p.setRenderHints(QPainter::Antialiasing,true);    

    _seqtracks_widget.paint(ev->rect(), p);

    if (_songtempoautomation_widget.is_visible)
      _songtempoautomation_widget.paint(ev->rect(), p);

    paintGrid(ev->rect(), p, _grid_type);
    paintCursor(ev->rect(), p);
    if (SEQUENCER_is_looping())
      paintSeqloop(ev->rect(), p);    

    if (_has_selection_rectangle)
      paintSelectionRectangle(ev->rect(), p);
  }

  
  Seqtrack_widget *get_seqtrack_widget(const struct SeqTrack *seqtrack){
    return _seqtracks_widget.get_seqtrack_widget(seqtrack);
  }
  
  Seqtrack_widget *get_seqtrack_widget(int seqtracknum){
    return _seqtracks_widget.get_seqtrack_widget(seqtracknum);
  }

};


}

static Sequencer_widget *g_sequencer_widget = NULL;

static void g_position_widgets(void){
  if (g_sequencer_widget != NULL)
    g_sequencer_widget->position_widgets();
}

QWidget *SEQUENCER_getWidget(void){
  R_ASSERT(g_sequencer_widget != NULL);
  return g_sequencer_widget;
}


// sequencer

float SEQUENCER_get_x1(void){
  return mapToEditorX1(g_sequencer_widget);
}

float SEQUENCER_get_x2(void){
  return mapToEditorX2(g_sequencer_widget);
}

float SEQUENCER_get_y1(void){
  return mapToEditorY1(g_sequencer_widget);
}

float SEQUENCER_get_y2(void){
  return mapToEditorY2(g_sequencer_widget);
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
          
  g_sequencer_widget->my_update();
}

void SEQUENCER_set_visible_start_time(int64_t val){
  R_ASSERT_RETURN_IF_FALSE(val < g_sequencer_widget->_end_time);

  g_sequencer_widget->_start_time = R_MAX(val, 0);

  g_sequencer_widget->legalize_start_end_times();

  g_sequencer_widget->my_update();
}

void SEQUENCER_set_visible_end_time(int64_t val){
  R_ASSERT_RETURN_IF_FALSE(val > g_sequencer_widget->_start_time);
  
  g_sequencer_widget->_end_time = R_MIN(val, get_visible_song_length() * MIXER_get_sample_rate());

  g_sequencer_widget->legalize_start_end_times();
    
  g_sequencer_widget->my_update();
}

void SEQUENCER_set_grid_type(enum GridType grid_type){
  g_sequencer_widget->_grid_type = grid_type;
}

void SEQUENCER_set_selection_rectangle(float x1, float y1, float x2, float y2){
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


  g_sequencer_widget->_selection_rectangle = QRect(p1, p2);

  g_sequencer_widget->set_seqblocks_is_selected();

  g_sequencer_widget->update();
}

void SEQUENCER_unset_selection_rectangle(void){
  g_sequencer_widget->_has_selection_rectangle = false;

  g_sequencer_widget->update();
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

void SEQNAV_update(void){
  g_sequencer_widget->_navigator_widget.update();
}



// sequencer looping

float SEQTIMELINE_get_x1(void){
  return mapToEditorX1(&g_sequencer_widget->_timeline_widget);
}

float SEQTIMELINE_get_x2(void){
  return mapToEditorX2(&g_sequencer_widget->_timeline_widget);
}

float SEQTIMELINE_get_y1(void){
  return mapToEditorY1(&g_sequencer_widget->_timeline_widget);
}

float SEQTIMELINE_get_y2(void){
  return mapToEditorY2(&g_sequencer_widget->_timeline_widget);
}




// seqtempo automation

float SEQTEMPO_get_x1(void){
  return mapToEditorX(g_sequencer_widget, 0);
  //return mapToEditorX1(&g_sequencer_widget->_songtempoautomation_widget);
}

float SEQTEMPO_get_x2(void){
  return mapToEditorX(g_sequencer_widget, g_sequencer_widget->width());
  //return mapToEditorX2(&g_sequencer_widget->_songtempoautomation_widget);
}

float SEQTEMPO_get_y1(void){
  return mapToEditorY1(g_sequencer_widget) + g_sequencer_widget->_songtempoautomation_widget.t_y1;
}

float SEQTEMPO_get_y2(void){
  return mapToEditorY1(g_sequencer_widget) + g_sequencer_widget->_songtempoautomation_widget.t_y2;
}

void SEQTEMPO_set_visible(bool visible){
  if (g_sequencer_widget != NULL){
    g_sequencer_widget->_songtempoautomation_widget.is_visible = visible;
    g_sequencer_widget->position_widgets();
    SEQUENCER_update();
  }
}

bool SEQTEMPO_is_visible(void){
  return g_sequencer_widget->_songtempoautomation_widget.is_visible;
}


// seqblocks

float SEQBLOCK_get_x1(int seqblocknum, int seqtracknum){
  auto *w0 = g_sequencer_widget->get_seqtrack_widget(seqtracknum);
  if (w0==NULL)
    return 0.0;
  
  const auto &w = w0->_seqblocks_widget;

  return mapToEditorX(g_sequencer_widget, 0) + w.get_seqblock_x1(seqblocknum);
}

float SEQBLOCK_get_x1(const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock){
  auto *w0 = g_sequencer_widget->get_seqtrack_widget(seqtrack);
  if (w0==NULL)
    return 0.0;
  
  const auto &w = w0->_seqblocks_widget;

  return mapToEditorX(g_sequencer_widget, 0) + w.get_seqblock_x1(seqblock);
}

float SEQBLOCK_get_x2(int seqblocknum, int seqtracknum){
  auto *w0 = g_sequencer_widget->get_seqtrack_widget(seqtracknum);
  if (w0==NULL)
    return 0.0;
  
  const auto &w = w0->_seqblocks_widget;

  return mapToEditorX(g_sequencer_widget, 0) + w.get_seqblock_x2(seqblocknum);
}

float SEQBLOCK_get_x2(const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock){
  auto *w0 = g_sequencer_widget->get_seqtrack_widget(seqtrack);
  if (w0==NULL)
    return 0.0;
  
  const auto &w = w0->_seqblocks_widget;

  return mapToEditorX(g_sequencer_widget, 0) + w.get_seqblock_x2(seqblock);
}

float SEQBLOCK_get_y1(int seqblocknum, int seqtracknum){
  auto *w0 = g_sequencer_widget->get_seqtrack_widget(seqtracknum);
  if (w0==NULL)
    return 0.0;
  
  const auto &w = w0->_seqblocks_widget;

  return mapToEditorY(g_sequencer_widget, w.t_y1);
}

float SEQBLOCK_get_header_height(void){
  return get_block_header_height();
}

float SEQBLOCK_get_y2(int seqblocknum, int seqtracknum){
  auto *w0 = g_sequencer_widget->get_seqtrack_widget(seqtracknum);
  if (w0==NULL)
    return 0.0;
  
  const auto &w = w0->_seqblocks_widget;

  return mapToEditorY(g_sequencer_widget, w.t_y2);
}

// seqblock left stretch

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

SEQBLOCK_handles(fade, yfunc_ysplit0, yfunc_ysplit1);
SEQBLOCK_handles(interior, yfunc_ysplit1, yfunc_ysplit2);
SEQBLOCK_handles(stretch, yfunc_ysplit2, SEQBLOCK_get_y2);




// seqtracks

float SEQTRACK_get_x1(int seqtracknum){
  //auto *w = g_sequencer_widget->get_seqtrack_widget(seqtracknum);
  //if (w==NULL)
  //  return 0.0;
    
  return mapToEditorX(g_sequencer_widget, 0);
}

float SEQTRACK_get_x2(int seqtracknum){
  //auto *w = g_sequencer_widget->get_seqtrack_widget(seqtracknum);
  //if (w==NULL)
  //  return 0.0;
    
  return mapToEditorX(g_sequencer_widget, g_sequencer_widget->width());
}

float SEQTRACK_get_y1(int seqtracknum){
  Seqtrack_widget *w = g_sequencer_widget->get_seqtrack_widget(seqtracknum);
  if (w==NULL)
    return 0.0;
    
  return mapToEditorY(w, 0);
}

float SEQTRACK_get_y2(int seqtracknum){
  auto *w = g_sequencer_widget->get_seqtrack_widget(seqtracknum);
  if (w==NULL)
    return 0.0;
    
  return mapToEditorY(w, w->height());
}

void SEQTRACK_update(struct SeqTrack *seqtrack){
  if (g_sequencer_widget == NULL)
    return;

  //g_sequencer_widget->update();
  //return;
  
  Seqtrack_widget *w = g_sequencer_widget->get_seqtrack_widget(seqtrack);
  if (w==NULL)
    return;

  D(printf("SEQTRACK_update called\n"));

  g_sequencer_widget->update(0, w->t_y1,
                             g_sequencer_widget->width(), w->t_height);
}

// Note: Might be called from a different thread than the main thread. (DiskPeak thread calls this function)
void SEQUENCER_update(void){
  if (g_sequencer_widget != NULL){
    //g_sequencer_widget->position_widgets();
    //printf("SEQUENCER_update called\n%s\n",JUCE_get_backtrace());
    D(printf("SEQUENCER_update called\n%s\n",""));
    g_sequencer_widget->update();
  }
}

// Only called from the main thread.
void RT_SEQUENCER_update_sequencer_and_playlist(void){
  R_ASSERT_RETURN_IF_FALSE(THREADING_is_main_thread());

  if (PLAYER_current_thread_has_lock()) {
    
    g_need_update = true;
    
  } else {

    D(printf("RT_SEQUENDER_update_and_playlist called\n"));
    SEQUENCER_update();
    BS_UpdatePlayList();
    
    g_need_update=false;
    
  }
}

static bool g_sequencer_visible = true;
static bool g_sequencer_hidden_because_instrument_widget_is_large = false;

static void init_sequencer_visible(void){
  static bool g_sequencer_visible_inited = false;
  if (g_sequencer_visible_inited==false){
    g_sequencer_visible = g_sequencer_widget->isVisible();
    g_sequencer_visible_inited=true;
  }
}

/*
bool GFX_SequencerIsVisible(void){
  init_sequencer_visible();
  return g_sequencer_visible;
}
*/

void GFX_ShowSequencer(void){
  init_sequencer_visible();
  
  //set_widget_height(30);
  if (g_sequencer_hidden_because_instrument_widget_is_large == false){
    API_showSequencerGui();
    g_sequencer_visible = true;
  }

  set_editor_focus();
}

void GFX_HideSequencer(void){
  init_sequencer_visible();

  API_hideSequencerGui();
  g_sequencer_visible = false;
  //set_widget_height(0);

  set_editor_focus();
}

void SEQUENCER_hide_because_instrument_widget_is_large(void){
  init_sequencer_visible();

  g_sequencer_widget->hide();
  g_sequencer_hidden_because_instrument_widget_is_large = true;
}

void SEQUENCER_show_because_instrument_widget_is_large(void){
  init_sequencer_visible();
  
  if (g_sequencer_visible == true){
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
