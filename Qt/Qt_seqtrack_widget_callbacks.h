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

#include "Qt_MyQCheckBox.h"
#include "Qt_MyQSlider.h"

#include "Qt_seqtrack_widget.h"

#include "../embedded_scheme/scheme_proc.h"
#include "../common/undo.h"
#include "../common/song_tempo_automation_proc.h"
#include "../common/seqtrack_proc.h"



#define ENABLE_TEMPO_AUTOMATION 1

static bool g_need_update = false;

static void g_position_widgets(void);


static QPoint mapToEditor(QWidget *widget, QPoint point){
  //return widget->mapTo(g_editor, point); (g_editor must be a parent, for some reason)
  QPoint global = widget->mapToGlobal(point);
  return g_editor->mapFromGlobal(global);
}

static float mapToEditorX(QWidget *widget, float x){
  //auto global = widget->mapToGlobal(QPoint(x, 0));
  //return g_editor->mapFromGlobal(global).x();
  return mapToEditor(widget, QPoint(x, 0)).x();
}

static float mapToEditorY(QWidget *widget, float y){
  //auto global = widget->mapToGlobal(QPoint(0, y));
  //return g_editor->mapFromGlobal(global).y();
  return mapToEditor(widget, QPoint(0, y)).y();
}

static float mapToEditorX1(QWidget *widget){
  //auto global = widget->mapToGlobal(QPoint(x, 0));
  //return g_editor->mapFromGlobal(global).x();
  return mapToEditor(widget, QPoint(0, 0)).x();
}

static float mapToEditorY1(QWidget *widget){
  //auto global = widget->mapToGlobal(QPoint(0, y));
  //return g_editor->mapFromGlobal(global).y();
  return mapToEditor(widget, QPoint(0, 0)).y();
}

static float mapToEditorX2(QWidget *widget){
  //auto global = widget->mapToGlobal(QPoint(x, 0));
  //return g_editor->mapFromGlobal(global).x();
  return mapToEditor(widget, QPoint(0, 0)).x() + widget->width();
}

static float mapToEditorY2(QWidget *widget){
  //auto global = widget->mapToGlobal(QPoint(0, y));
  //return g_editor->mapFromGlobal(global).y();
  return mapToEditor(widget, QPoint(0, 0)).y() + widget->height();
}

static double getBlockAbsDuration(const struct Blocks *block){
  return getBlockSTimeLength(block) * ATOMIC_DOUBLE_GET(block->reltempo);
}

static QColor get_block_color(const struct Blocks *block){
  //return mix_colors(QColor(block->color), get_qcolor(SEQUENCER_BLOCK_BACKGROUND_COLOR_NUM), 0.32f);
  return QColor(block->color);
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


static void handle_wheel_event(QWheelEvent *e, int x1, int x2, double start_play_time, double end_play_time) {
      
  if (  (e->modifiers() & Qt::ControlModifier) || (e->modifiers() & Qt::ShiftModifier)) {

    double nav_start_time = SEQUENCER_get_visible_start_time();
    double nav_end_time = SEQUENCER_get_visible_end_time();
    double visible_song_length = SONG_get_gfx_length() * MIXER_get_sample_rate();
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

    int64_t pos = scale(e->x(), x1, x2, start_play_time, end_play_time);
    if (e->delta() > 0)
      PlaySong(pos);
    else {
      PlayStop();
      ATOMIC_SET(pc->song_abstime, pos);
      SEQUENCER_update();
    }
  }
}


class Seqblocks_widget { //: public MouseTrackerQWidget {
public:

  //QVector<Seqblock_widget*> _seqblock_widgets; // deleted automatically when 'this' is deleted.

  QWidget *_sequencer_widget;
  
  SeqTrack *_seqtrack;

  const int64_t &_start_time;
  const int64_t &_end_time;
  QTime _time;
  QRect _rect;

  int t_x1,t_y1,t_x2,t_y2,width,height;
    
  Seqblocks_widget(QWidget *_sequencer_widget, SeqTrack *seqtrack, const int64_t &start_time, const int64_t &end_time)
    : _sequencer_widget(_sequencer_widget)
    , _seqtrack(add_gc_root(seqtrack))
    , _start_time(start_time)
    , _end_time(end_time)
  {
    position_widgets(0,0,100,100);
    
    //create_seqblock_widgets();
    _time.start();
  }

  ~Seqblocks_widget(){
    remove_gc_root(_seqtrack);

    printf("Seqblocks widget deleted\n");
  }

  void position_widgets(int x1, int y1, int x2, int y2){
    t_x1 = x1;
    t_y1 = y1;
    t_x2 = x2;
    t_y2 = y2;
    height = t_y2-t_y1;
    width = t_x2-t_x1;
    
    _rect = QRect(t_x1, t_y1, width, height);
  }
  
  /*
  void wheelEvent(QWheelEvent *e) override {
    handle_wheel_event(e, 0, width(), _start_time, _end_time);
  }
  */
  
  float get_seqblock_x1(struct SeqBlock *seqblock, double start_time, double end_time){
    return scale(seqblock->start_time, start_time, end_time, t_x1, t_x2);
  }
  
  float get_seqblock_x2(struct SeqBlock *seqblock, double start_time, double end_time){
    return scale(seqblock->end_time, start_time, end_time, t_x1, t_x2);
  }

  float get_seqblock_x1(int seqblocknum){
    R_ASSERT_RETURN_IF_FALSE2(seqblocknum>=0, 0);

    // This can happen while the sequencer is updated.
    if (seqblocknum >= _seqtrack->seqblocks.num_elements)
      return 0;
    
    SEQTRACK_update_all_seqblock_gfx_start_and_end_times(_seqtrack);
    double start_time = _start_time / MIXER_get_sample_rate();
    double end_time = _end_time / MIXER_get_sample_rate();
    return get_seqblock_x1((struct SeqBlock*)_seqtrack->seqblocks.elements[seqblocknum], start_time, end_time);
  }

  float get_seqblock_x2(int seqblocknum){
    R_ASSERT_RETURN_IF_FALSE2(seqblocknum>=0, 0);

    // This can happen while the sequencer is updated.
    if (seqblocknum >= _seqtrack->seqblocks.num_elements)
      return 100;

    SEQTRACK_update_all_seqblock_gfx_start_and_end_times(_seqtrack);
    double start_time = _start_time / MIXER_get_sample_rate();
    double end_time = _end_time / MIXER_get_sample_rate();
    return get_seqblock_x2((struct SeqBlock*)_seqtrack->seqblocks.elements[seqblocknum], start_time, end_time);
  }

  void paintTrack(QPainter &p, float x1, float y1, float x2, float y2, const struct Blocks *block, const struct Tracks *track, int64_t blocklen) const {
    QColor color1 = get_qcolor(SEQUENCER_NOTE_COLOR_NUM);
    QColor color2 = get_qcolor(SEQUENCER_NOTE_START_COLOR_NUM);
    
#define SHOW_BARS 0

#if SHOW_BARS
    p.setBrush(QBrush(color));
#else
    const float bar_height = 2.3;
    const float bar_header_length = 3.2;
    
    QPen pen1(color1);
    pen1.setWidthF(bar_height);
    pen1.setCapStyle(Qt::FlatCap);

    QPen pen2(pen1);
    pen2.setColor(color2);
    
#endif
    
    struct Notes *note = track->notes;
    while(note != NULL){

      int64_t start = Place2STime(block, &note->l.p);
      int64_t end = Place2STime(block, &note->end);

      float n_x1 = scale(start, 0, blocklen, x1, x2);
      float n_x2 = scale(end, 0, blocklen, x1, x2);

      p.setPen(pen1);

#if SHOW_BARS
      float n_y1 = scale(note->note, 127, 0, y1, y2);
      float n_y2 = y2;
      
      QRectF rect(n_x1, n_y1, n_x2-n_x1, n_y2-n_y1);
      p.drawRect(rect);
      
#else
      float n_y = scale(note->note+0.5, 127, 0, y1, y2);

      {
        float x2 = R_MIN(n_x2, n_x1+bar_header_length);
        
        QLineF line(n_x1,n_y,x2,n_y);
        p.setPen(pen2);
        p.drawLine(line);

        if (n_x2 > x2){
          QLineF line(x2,n_y,n_x2,n_y);
          p.setPen(pen1);
          p.drawLine(line);
        }
      }
#endif
#undef SHOW_BARS
      
      note = NextNote(note);
    }
  }
  
  void paintBlock(QPainter &p, const QRectF &rect, const struct Blocks *block){

    const int header_height = root->song->tracker_windows->fontheight;
    
    QColor text_color = get_qcolor(SEQUENCER_TEXT_COLOR_NUM);
    QColor border_color = get_qcolor(SEQUENCER_BLOCK_BORDER_COLOR_NUM);

    QColor header_border_color = get_qcolor(SEQUENCER_TRACK_BORDER1_COLOR_NUM);
    QColor track_border_color  = get_qcolor(SEQUENCER_TRACK_BORDER2_COLOR_NUM);

    QPen header_border_pen(header_border_color);
    QPen track_border_pen(track_border_color);

    header_border_pen.setWidthF(2.3);
    track_border_pen.setWidthF(1.3);

    bool is_current_block = block == root->song->tracker_windows->wblock->block;

    qreal x1,y1,x2,y2;
    rect.getCoords(&x1, &y1, &x2, &y2);

    if (true || is_current_block) {
      QRectF rect1(x1, y1, x2-x1, header_height);
      QRectF rect2(x1, y1+header_height, x2-x1, y2-(y1+header_height));
      p.fillRect(rect1, get_block_color(block));
      p.fillRect(rect2, get_qcolor(SEQUENCER_BLOCK_BACKGROUND_COLOR_NUM));
    } else {
      p.fillRect(rect, get_qcolor(SEQUENCER_BLOCK_BACKGROUND_COLOR_NUM));
    }

    //if (x1 > -5000) { // avoid integer overflow error.
    p.setPen(text_color);
    //p.drawText(x1+4,2,x2-x1-6,height()-4, Qt::AlignLeft, QString::number(seqblock->block->l.num) + ": " + seqblock->block->name);
    p.drawText(rect.adjusted(2,1,-4,-(rect.height()-header_height)), QString::number(block->l.num) + ": " + block->name, QTextOption(Qt::AlignLeft | Qt::AlignTop));
    //}

    if (is_current_block){
      QColor c = get_qcolor(CURSOR_EDIT_ON_COLOR_NUM);
      c.setAlpha(150);      
      p.setPen(QPen(c, 4));
    } else {
      p.setPen(border_color);
    }

    p.drawRoundedRect(rect,1,1);

    int64_t blocklen = getBlockSTimeLength(block);

    // Tracks
    {
      int num_tracks = block->num_tracks;
      
      struct Tracks *track = block->tracks;
      while(track != NULL){
        
        float t_y1 = scale(track->l.num,0,num_tracks,y1+header_height,y2);
        float t_y2 = scale(track->l.num+1,0,num_tracks,y1+header_height,y2);
        
        // Draw track border
        {
          p.setPen(track->l.num==0 ? header_border_pen : track_border_pen);        
          p.drawLine(QLineF(x1,t_y1,x2,t_y1));
        }
        
        // Draw track
        paintTrack(p, x1, t_y1, x2, t_y2, block, track, blocklen);
        
        track = NextTrack(track);
      }
    }

    // Bar and beats
    {
      struct Beats *beat = block->beats;
      QColor bar_color = get_qcolor(SEQUENCER_BLOCK_BAR_COLOR_NUM);
      QColor beat_color = get_qcolor(SEQUENCER_BLOCK_BEAT_COLOR_NUM);
      QPen bar_pen(bar_color);
      QPen beat_pen(beat_color);

      //bar_pen.setWidth(1.0);
      //beat_pen.setWidth(1.0);
      
      while(beat != NULL){
        float b_y1 = y1+header_height;
        float b_y2 = y2;

        float pos = Place2STime(block, &beat->l.p);
        
        float b_x = scale(pos, 0, blocklen, x1, x2);

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
  
  void paint(const QRect update_rect, QPainter &p){ // QPaintEvent * ev ) override {

    //printf("  PAINTING %d %d -> %d %d\n",t_x1,t_y1,t_x2,t_y2);

    p.fillRect(_rect.adjusted(1,1,-2,-1), get_qcolor(SEQUENCER_BACKGROUND_COLOR_NUM));
      
    double sample_rate = MIXER_get_sample_rate();
    //double song_length = get_visible_song_length()*sample_rate;
  
    SEQTRACK_update_all_seqblock_gfx_start_and_end_times(_seqtrack);

    double start_time = _start_time / sample_rate;
    double end_time = _end_time / sample_rate;

    VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &_seqtrack->seqblocks){

      //printf("      PAINTING BLOCK %f / %f, seqtrack/seqblock: %p / %p\n", seqblock->start_time, seqblock->end_time, _seqtrack, seqblock);

      if (seqblock->start_time < end_time && seqblock->end_time >= start_time) {
        
        float x1 = get_seqblock_x1(seqblock, start_time, end_time);
        float x2 = get_seqblock_x2(seqblock, start_time, end_time);

        QRectF rect(x1,t_y1+1,x2-x1,height-2);

        if (update_rect.intersects(rect.toAlignedRect()))
          paintBlock(p, rect, seqblock->block);
        
      }
      
    }END_VECTOR_FOR_EACH;


    // Current track border and non-current track shadowing
    {
      if (_seqtrack==(struct SeqTrack*)root->song->seqtracks.elements[root->song->curr_seqtracknum]){
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
        p.fillRect(t_x1,t_y1,width,height, color);
      }
    }

    
  }

  int _last_num_seqblocks = 0;
  int _last_num_undos = -1;
  
  void call_very_often(void){
    if (_last_num_seqblocks != _seqtrack->seqblocks.num_elements) {
      SEQUENCER_update();
      _last_num_seqblocks = _seqtrack->seqblocks.num_elements;
    }

    {
      int num_undos = Undo_num_undos();
      bool unequal_undos = _last_num_undos != num_undos;
      
      if (unequal_undos || _time.elapsed() > 5000) { // Update at least every five seconds.
        if (unequal_undos) {
          _last_num_undos = num_undos;

          double sample_rate = MIXER_get_sample_rate();
          double start_time = _start_time / sample_rate;
          double end_time = _end_time / sample_rate;

          // Not sure if this makes a difference (rather than just calling update()).
          VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &_seqtrack->seqblocks){
            float x1 = get_seqblock_x1(seqblock, start_time, end_time);
            float x2 = get_seqblock_x2(seqblock, start_time, end_time);
            _sequencer_widget->update(x1-1, t_y1, x2-x1+1, height);
          }END_VECTOR_FOR_EACH;
          
        } else {
          _sequencer_widget->update();
        }
        _time.restart();
      }
    }
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

  Seqblocks_widget *_seqblocks_widget; // deleted automatically when 'this' is deleted.
  SeqTrack *_seqtrack;

  Seqtrack_widget(QWidget *parent, SeqTrack *seqtrack, const int64_t &start_time, const int64_t &end_time)
    : QWidget(parent)
    , _seqblocks_widget(new Seqblocks_widget(parent, seqtrack, start_time, end_time))
    , _seqtrack(add_gc_root(seqtrack))
  {
    setupUi(this);

    // Remove widget we don't use yet
#if 1
    header_widget->hide();
#endif
    

    //main_layout->addWidget(_seqblocks_widget);
  }

  ~Seqtrack_widget(){
    remove_gc_root(_seqtrack);
    printf("Seqtrack widget deleted\n");
  }



  int t_x1,t_y1,t_x2,t_y2,t_width,t_height;
  
  void position_widgets(int x1, int y1, int x2, int y2){
    t_x1 = x1;
    t_y1 = y1;
    t_x2 = x2;
    t_y2 = y2;
    t_height = t_y2-t_y1;
    t_width = t_x2-t_x1;
    
    int x1_b = x1;// + width();
    _seqblocks_widget->position_widgets(x1_b, y1, x2, y2);
  }

  void paint(const QRect update_rect, QPainter &p){
    _seqblocks_widget->paint(update_rect, p);
  }
  
  void my_update(void){
    //_seqblocks_widget->update();
  }

  void call_very_often(void){
    _seqblocks_widget->call_very_often();
  }

public slots:

};


namespace{

class Seqtracks_widget : public radium::VerticalScroll {
public:

  QWidget *_sequencer_widget;
  
  QVector<Seqtrack_widget*> _seqtrack_widgets;

  const int64_t &_start_time;
  const int64_t &_end_time;
  
  Seqtracks_widget(QWidget *sequencer_widget, const int64_t &start_time, const int64_t &end_time)
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


  const float cursor_width = 2.7;
  float _last_painted_cursor_x = 0.0f;
  
  float get_curr_cursor_x(int frames_to_add){
    return scale_double(ATOMIC_GET(pc->song_abstime)+frames_to_add, _start_time, _end_time, t_x1, t_x2);
  }

  int t_x1=0,t_y1=0,t_x2=50,t_y2=50;
  int width=0,height=0;
  
  void position_widgets(int x1, int y1, int x2, int y2){
    t_x1 = x1;
    t_y1 = y1;
    t_x2 = x2;
    t_y2 = y2;
    width = t_x2-t_x1;
    height = t_y2-t_y1;

    for(auto *seqtrack_widget : _seqtrack_widgets){
      int y1_b = y1+seqtrack_widget->y();
      int y2_b = y1_b + seqtrack_widget->height();
      seqtrack_widget->position_widgets(x1, y1_b, x2, y2_b);
    }

  }  

  void paint(const QRect update_rect, QPainter &p, enum GridType grid_type){
    position_widgets(t_x1, t_y1, t_x2, t_y2);
    
    for(auto *seqtrack_widget : _seqtrack_widgets)
      seqtrack_widget->paint(update_rect, p);
    
    if (grid_type==BAR_GRID) {
      QPen pen(get_qcolor(SEQUENCER_GRID_COLOR_NUM));
      p.setPen(pen);
      int64_t last_bar = -50000;
      int64_t abstime = _start_time;
      int inc = (_end_time-_start_time) / width;
      while(abstime < _end_time){
        int64_t maybe = SEQUENCER_find_closest_bar_start(0, abstime);
        if (maybe > last_bar){
          float x = scale(maybe, _start_time, _end_time, 0, width);
          //printf("x: %f, abstime: %f\n",x,(float)maybe/44100.0);
          QLineF line(x, t_y1+2, x, t_y2-4);
          p.drawLine(line);
          last_bar = maybe;
          abstime = maybe;
        }
        abstime += inc;
      }
    }
    
    // Cursor
    {
      QPen pen(get_qcolor(SEQUENCER_CURSOR_COLOR_NUM));
      pen.setWidthF(cursor_width);

      _last_painted_cursor_x = get_curr_cursor_x(0);
      
      QLineF line(_last_painted_cursor_x, t_y1+2, _last_painted_cursor_x, t_y2-4);
      
      p.setPen(pen);
      p.drawLine(line);
    }

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

    if (is_playing() && pc->playtype==PLAYSONG) {
      float x = get_curr_cursor_x(1 + MIXER_get_sample_rate() * 60.0 / 1000.0);

      float x_min = R_MIN(x-cursor_width/2.0, _last_painted_cursor_x-cursor_width/2.0) - 2;
      float x_max = R_MAX(x+cursor_width/2.0, _last_painted_cursor_x+cursor_width/2.0) + 2;

      //printf("x_min -> x_max: %f -> %f\n",x_min,x_max);
      _sequencer_widget->update(x_min, t_y1, 1+x_max-x_min, height);
    }
  }

  Seqtrack_widget *get_seqtrack_widget(struct SeqTrack *seqtrack){
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


struct SongTempoAutomation_widget : public MouseTrackerQWidget {
  const int64_t &_start_time;
  const int64_t &_end_time;
  
  SongTempoAutomation_widget(QWidget *parent, const int64_t &start_time, const int64_t &end_time)
    : MouseTrackerQWidget(parent)
    , _start_time(start_time)
    , _end_time(end_time)
  {    
  }

  void paintEvent ( QPaintEvent * ev ) override {
    QPainter p(this);

    p.setRenderHints(QPainter::Antialiasing,true);

    //printf("height: %d\n",height());
    TEMPOAUTOMATION_paint(&p, 0, 0, width(), height(), _start_time, _end_time);
  }

};

struct Timeline_widget : public MouseTrackerQWidget {
  const int64_t &_start_time;
  const int64_t &_end_time;
  
  Timeline_widget(QWidget *parent, const int64_t &start_time, const int64_t &end_time)
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
    QPainter p(this);

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

    p.setBrush(get_qcolor(SEQUENCER_TIMELINE_ARROW_COLOR_NUM));
    
    double min_pixels_between_text = 40; //width() / 4;

    double start_time = _start_time / MIXER_get_sample_rate();
    double end_time = _end_time / MIXER_get_sample_rate();

    R_ASSERT_RETURN_IF_FALSE(end_time > start_time);
    
    int inc_time = R_MAX(1, ceil(scale(min_pixels_between_text, 0, width(), 0, end_time-start_time)));

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
    
    const int t1 = 4;
    
    int64_t time = inc_time * int((double)start_time/(double)inc_time);
    
    for(;;){
      const double x = scale_double(time, start_time, end_time, 0, width());
      if (x >= width())
        break;

      if (x > 20) {
        const double y1 = 4;
        const double y2 = height() - 4;
                
        draw_filled_triangle(p, x-t1, y1, x+t1, y1, x, y2);
        
        QRect rect(x + t1 + 4, 2, width(), height());
        p.drawText(rect, seconds_to_timestring(time), QTextOption(Qt::AlignLeft | Qt::AlignTop));
      }
      
      time += inc_time;
    }
    
  }

};

struct Seqtracks_navigator_widget : public MouseTrackerQWidget {
  const int64_t &_start_time;
  const int64_t &_end_time;
  Seqtracks_widget &_seqtracks_widget;
  
  Seqtracks_navigator_widget(QWidget *parent, const int64_t &start_time, const int64_t &end_time, Seqtracks_widget &seqtracks_widget)
    : MouseTrackerQWidget(parent)
    , _start_time(start_time)
    , _end_time(end_time)
    , _seqtracks_widget(seqtracks_widget)
  {
  }

  void wheelEvent(QWheelEvent *e) override {
    handle_wheel_event(e, 0, width(), 0, SONG_get_gfx_length()*MIXER_get_sample_rate());
  }

private:
  float get_x1(double total){
    return scale(_start_time, 0, total, 0, width());
  }
  float get_x2(double total){
    return scale(_end_time, 0, total, 0, width());
  }
public:

  float get_x1(void){
    return get_x1(SONG_get_gfx_length()*MIXER_get_sample_rate());
  }
  float get_x2(void){
    return get_x2(SONG_get_gfx_length()*MIXER_get_sample_rate());
  }
  
  void paintEvent ( QPaintEvent * ev ) override {
    QPainter p(this);

    double total_seconds = SONG_get_gfx_length();
    double total = total_seconds*MIXER_get_sample_rate();
    
    p.setRenderHints(QPainter::Antialiasing,true);

    QColor border_color = get_qcolor(SEQUENCER_BORDER_COLOR_NUM);      
    

    // Background
    //
    QRect rect1(1,1,width()-1,height()-2);
    p.fillRect(rect1, get_qcolor(SEQUENCER_BACKGROUND_COLOR_NUM));

    
    // Blocks
    {

      //QColor block_color = QColor(140,140,140,180);
      QColor text_color = get_qcolor(SEQUENCER_TEXT_COLOR_NUM);
      
      int num_seqtracks = root->song->seqtracks.num_elements;
      int seqtracknum = 0;
      for(Seqtrack_widget *seqtrack_widget : _seqtracks_widget._seqtrack_widgets) {
        struct SeqTrack *seqtrack = seqtrack_widget->_seqtrack;
        
        float y1 = scale(seqtracknum,   0, num_seqtracks, 3, height()-3);
        float y2 = scale(seqtracknum+1, 0, num_seqtracks, 3, height()-3);
        
        SEQTRACK_update_all_seqblock_gfx_start_and_end_times(seqtrack);
        //double start_time = _start_time / MIXER_get_sample_rate();
        //double end_time = _end_time / MIXER_get_sample_rate();

        VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){

          const struct Blocks *block = seqblock->block;
          
          QColor block_color = get_block_color(block);//->color);// = get_qcolor(SEQUENCER_BLOCK_BACKGROUND_COLOR_NUM);

          //printf("\n\n\n Start/end: %f / %f. Seqtrack/seqblock %p / %p\n\n", seqblock->start_time, seqblock->end_time, seqtrack, seqblock);
            
          float x1 = scale(seqblock->start_time, 0, total_seconds, 0, width()); //seqtrack_widget->_seqblocks_widget->get_seqblock_x1(seqblock, start_time, end_time);
          float x2 = scale(seqblock->end_time, 0, total_seconds, 0, width()); //seqtrack_widget->_seqblocks_widget->get_seqblock_x2(seqblock, start_time, end_time);
          
          QRectF rect(x1,y1+1,x2-x1,y2-y1-2);
          p.fillRect(rect, block_color);

          if(rect.height() > root->song->tracker_windows->fontheight){
            p.setPen(text_color);
            p.drawText(rect.adjusted(2,1,-1,-1), QString::number(block->l.num) + ": " + block->name, QTextOption(Qt::AlignLeft | Qt::AlignTop));
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
      float x1 = get_x1(total);
      float x2 = get_x2(total);

      QRectF rectA(0,  1, x1,         height()-2);
      QRectF rectB(x2, 1, width()-x2, height()-2);      
      QRectF rect2(x1, 1, x2-x1,      height()-2);

      {
        QColor grayout_color = get_qcolor(SEQUENCER_NAVIGATOR_GRAYOUT_COLOR);
      
        p.fillRect(rectA, grayout_color);
        p.fillRect(rectB, grayout_color);
      }
      
      p.setPen(border_color);
      p.drawRect(rect2);
      
      float handle1_x = x1+SEQNAV_SIZE_HANDLE_WIDTH;
      float handle2_x = x2-SEQNAV_SIZE_HANDLE_WIDTH;
      //p.drawLine(handle1_x, 0, handle1, height());
      //p.drawLine(handle2_x, 0, handle1, height());
      
      QRectF handle1_rect(x1,        0, handle1_x-x1, height());
      QRectF handle2_rect(handle2_x, 0, x2-handle2_x, height());
      
      p.setBrush(get_qcolor(SEQUENCER_NAVIGATOR_HANDLER_COLOR));
      
      p.drawRect(handle1_rect);
      p.drawRect(handle2_rect);
    }
    
  }

};

struct Sequencer_widget : public MouseTrackerQWidget {

  int _old_width = 600;
  int64_t _start_time = 0;
  int64_t _end_time = 600;
  double _samples_per_pixel;

  enum GridType _grid_type;

  const int bottom_height = 30;
#if ENABLE_TEMPO_AUTOMATION
  SongTempoAutomation_widget _songtempoautomation_widget;
#endif
  Timeline_widget _timeline_widget;
  Seqtracks_widget _seqtracks_widget;
  Seqtracks_navigator_widget _navigator_widget;
  //MyQSlider _main_reltempo;

  Sequencer_widget(QWidget *parent)
    : MouseTrackerQWidget(parent)
    , _end_time(SONG_get_gfx_length()*MIXER_get_sample_rate())
    , _samples_per_pixel((_end_time-_start_time) / width())
#if ENABLE_TEMPO_AUTOMATION
    , _songtempoautomation_widget(this, _start_time, _end_time)
#endif
    , _timeline_widget(this, _start_time, _end_time)
    , _seqtracks_widget(this, _start_time, _end_time)
    , _navigator_widget(this, _start_time, _end_time, _seqtracks_widget)
      //, _main_reltempo(this)
  {
#if ENABLE_TEMPO_AUTOMATION
    _songtempoautomation_widget.show();
#endif
    _timeline_widget.show();
    _seqtracks_widget.show();
    _navigator_widget.show();
    /*
    _main_reltempo.show();
    */
    
    setMinimumHeight(200);
  }

  void my_update(void){
    int64_t visible_song_length = MIXER_get_sample_rate() * SONG_get_gfx_length();
    if (_end_time > visible_song_length) {
      _end_time = visible_song_length;
      if (_start_time >= _end_time)
        _start_time = 0;
      if (_start_time >= _end_time)
        _end_time = 10;
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

  void wheelEvent(QWheelEvent *e) override {
    handle_wheel_event(e, 0, width(), _start_time, _end_time);
  }

  void resizeEvent( QResizeEvent *qresizeevent) override {
    //  set_end_time();
    // _samples_per_pixel = (_end_time-_start_time) / width();
    position_widgets();
  }
  
  void position_widgets(void){
    R_ASSERT_RETURN_IF_FALSE(_seqtracks_widget._seqtrack_widgets.size() > 0);

    printf("   ***** Posisiotioing sequencer widgets ********\n");
    
#if 0
    const QWidget *mute_button = _seqtracks_widget._seqtrack_widgets.at(0)->mute_button;
    const QPoint p = mute_button->mapTo(this, mute_button->pos());
#endif
    
    const int x1 = 0; //p.x() + mute_button->width();
    const int x1_width = width() - x1;

    const int timeline_widget_height = root->song->tracker_windows->fontheight + 2;

    int y1 = 0;

    
#if ENABLE_TEMPO_AUTOMATION
    const int songtempoautomation_widget_height = root->song->tracker_windows->fontheight*3;

    // tempo automation
    _songtempoautomation_widget.setGeometry(x1, y1,
                                            x1_width, songtempoautomation_widget_height);
    
    
    y1 += songtempoautomation_widget_height;
#endif
    

    // timeline
    _timeline_widget.setGeometry(x1, y1,
                                 x1_width, timeline_widget_height);


    y1 += timeline_widget_height;


    // sequencer tracks
        
    int y2 = height() - bottom_height;
    _seqtracks_widget.setGeometry(0, y1,
                                  1, y2 - y1);

    _seqtracks_widget.position_widgets(1,y1,
                                       width()-1, y2);


    y1 = y2;


    // navigator
    
    _navigator_widget.setGeometry(x1, y1,
                                  x1_width, bottom_height);

    /*
    _main_reltempo.setGeometry(0, y1,
                               x1, bottom_height);
    */

  }

  
  int _last_num_seqtracks = 0;
  double _last_visible_song_length = 0;
  
  void call_very_often(void){
    if (is_called_every_ms(15)){  // call each 15 ms. (i.e. more often than vsync)
      _seqtracks_widget.call_very_often();    
    
      if (g_need_update){
        my_update();
        BS_UpdatePlayList();
        g_need_update=false;
      }
    }

    if (is_called_every_ms(50)){
      bool do_update = _seqtracks_widget._seqtrack_widgets.size() != _last_num_seqtracks;
      
      if (!do_update) {
        if (is_called_every_ms(1000)) // This is only an insurance. SEQUENCER_update is supposed to be called manually when needed.
          if (_last_visible_song_length != SONG_get_gfx_length()) {
            do_update = true;
            _last_visible_song_length = SONG_get_gfx_length();
          }
      }  
      
      if (do_update){
        //position_widgets();
        my_update();
        _last_num_seqtracks = _seqtracks_widget._seqtrack_widgets.size();
      }
    }
      

  }

  void paintEvent (QPaintEvent *ev) override {
    QPainter p(this);

    p.setRenderHints(QPainter::Antialiasing,true);    

    _seqtracks_widget.paint(ev->rect(), p, _grid_type);
  }

  
  Seqtrack_widget *get_seqtrack_widget(struct SeqTrack *seqtrack){
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
  if (update_enabled_counter==1 && g_sequencer_widget!=NULL)
    g_sequencer_widget->setUpdatesEnabled(false);
}
void SEQUENCER_enable_gfx_updates(void){
  update_enabled_counter--;
  if (update_enabled_counter==0 && g_sequencer_widget!=NULL)
    g_sequencer_widget->setUpdatesEnabled(true);
}

int64_t SEQUENCER_get_visible_start_time(void){
  return g_sequencer_widget->_start_time;
}

int64_t SEQUENCER_get_visible_end_time(void){
  return g_sequencer_widget->_end_time;
}

void SEQUENCER_set_visible_start_and_end_time(int64_t start_time, int64_t end_time){
  R_ASSERT_RETURN_IF_FALSE(end_time > start_time);
  
  g_sequencer_widget->_start_time = R_MAX(start_time, 0);
  g_sequencer_widget->_end_time = R_MIN(end_time, SONG_get_gfx_length() * MIXER_get_sample_rate());

  if (g_sequencer_widget->_start_time >= g_sequencer_widget->_end_time - 10)
    g_sequencer_widget->_start_time = R_MAX(g_sequencer_widget->_end_time - 10, 0);
    
  g_sequencer_widget->my_update();
}

void SEQUENCER_set_visible_start_time(int64_t val){
  R_ASSERT_RETURN_IF_FALSE(val < g_sequencer_widget->_end_time);

  g_sequencer_widget->_start_time = R_MAX(val, 0);
  g_sequencer_widget->my_update();
}

void SEQUENCER_set_visible_end_time(int64_t val){
  R_ASSERT_RETURN_IF_FALSE(val > g_sequencer_widget->_start_time);
  
  g_sequencer_widget->_end_time = R_MIN(val, SONG_get_gfx_length() * MIXER_get_sample_rate());
  g_sequencer_widget->my_update();
}

void SEQUENCER_set_grid_type(enum GridType grid_type){
  g_sequencer_widget->_grid_type = grid_type;
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



// seqtempo automation

float SEQTEMPO_get_x1(void){
  return mapToEditorX1(&g_sequencer_widget->_songtempoautomation_widget);
}

float SEQTEMPO_get_x2(void){
  return mapToEditorX2(&g_sequencer_widget->_songtempoautomation_widget);
}

float SEQTEMPO_get_y1(void){
  return mapToEditorY1(&g_sequencer_widget->_songtempoautomation_widget);
}

float SEQTEMPO_get_y2(void){
  return mapToEditorY2(&g_sequencer_widget->_songtempoautomation_widget);
}

bool SEQTEMPO_is_visible(void){
  return true;
}


// seqblocks

float SEQBLOCK_get_x1(int seqblocknum, int seqtracknum){
  auto *w0 = g_sequencer_widget->get_seqtrack_widget(seqtracknum);
  if (w0==NULL)
    return 0.0;
  
  auto *w = w0->_seqblocks_widget;

  return mapToEditorX(g_sequencer_widget, 0) + w->get_seqblock_x1(seqblocknum);
}

float SEQBLOCK_get_x2(int seqblocknum, int seqtracknum){
  auto *w0 = g_sequencer_widget->get_seqtrack_widget(seqtracknum);
  if (w0==NULL)
    return 0.0;
  
  auto *w = w0->_seqblocks_widget;
  if (w==NULL)
    return 0.0;

  return mapToEditorX(g_sequencer_widget, 0) + w->get_seqblock_x2(seqblocknum);
}

float SEQBLOCK_get_y1(int seqblocknum, int seqtracknum){
  auto *w0 = g_sequencer_widget->get_seqtrack_widget(seqtracknum);
  if (w0==NULL)
    return 0.0;
  
  auto *w = w0->_seqblocks_widget;
  if (w==NULL)
    return 0.0;

  return mapToEditorY(g_sequencer_widget, w->t_y1);
}

float SEQBLOCK_get_y2(int seqblocknum, int seqtracknum){
  auto *w0 = g_sequencer_widget->get_seqtrack_widget(seqtracknum);
  if (w0==NULL)
    return 0.0;
  
  auto *w = w0->_seqblocks_widget;
  if (w==NULL)
    return 0.0;

  return mapToEditorY(g_sequencer_widget, w->t_y2);
}



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
  
  g_sequencer_widget->update(0, w->t_y1,
                             g_sequencer_widget->width(), w->t_height);
}

void SEQUENCER_update(void){
  if (g_sequencer_widget != NULL)
    g_sequencer_widget->update();
}

// Only called from the main thread.
void RT_SEQUENCER_update_sequencer_and_playlist(void){
  R_ASSERT_RETURN_IF_FALSE(THREADING_is_main_thread());

  if (PLAYER_current_thread_has_lock()) {
    
    g_need_update = true;
    
  } else {

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

bool GFX_SequencerIsVisible(void){
  init_sequencer_visible();
  return g_sequencer_visible;
}

void GFX_ShowSequencer(void){
  init_sequencer_visible();
  
  //set_widget_height(30);
  if (g_sequencer_hidden_because_instrument_widget_is_large == false){
    GL_lock(); {
      g_sequencer_widget->show();
      g_sequencer_visible = true;
    }GL_unlock();
  }

  set_editor_focus();
}

void GFX_HideSequencer(void){
  init_sequencer_visible();
  
  g_sequencer_widget->hide();
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

