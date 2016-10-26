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

#include "Qt_MyQCheckBox.h"
#include "Qt_MyQSlider.h"

#include "Qt_seqtrack_widget.h"

#include "../embedded_scheme/scheme_proc.h"
#include "../common/seqtrack_proc.h"


static bool g_need_update = false;

static double get_visible_song_length(void){
  return SONG_get_length() + SEQUENCER_EXTRA_SONG_LENGTH;
}
  


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

static double getBlockAbsDuration(struct Blocks *block){
  return getBlockSTimeLength(block) * block->reltempo;
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
    _currentButton = getMouseButtonEventID(event);
    QPoint point = mapToEditor(this, event->pos());
    SCHEME_mousepress(_currentButton, point.x(), point.y());
  }
  void	mouseReleaseEvent( QMouseEvent *event) override{
    QPoint point = mapToEditor(this, event->pos());
    SCHEME_mouserelease(_currentButton, point.x(), point.y());
    _currentButton = 0;
  }
  void	mouseMoveEvent( QMouseEvent *event) override{
    QPoint point = mapToEditor(this, event->pos());
    SCHEME_mousemove(_currentButton, point.x(), point.y());
  }

};



class Seqblocks_widget : public MouseTrackerQWidget {
public:

  //QVector<Seqblock_widget*> _seqblock_widgets; // deleted automatically when 'this' is deleted.
  
  SeqTrack *_seqtrack;

  const int64_t &_start_time;
  const int64_t &_end_time;
  QTime _time;
  
  Seqblocks_widget(QWidget *parent, SeqTrack *seqtrack, const int64_t &start_time, const int64_t &end_time)
    : MouseTrackerQWidget(parent)
    , _seqtrack(add_gc_root(seqtrack))
    , _start_time(start_time)
    , _end_time(end_time)
  {
    //create_seqblock_widgets();
    _time.start();
  }

  ~Seqblocks_widget(){
    remove_gc_root(_seqtrack);

    printf("Seqblocks widget deleted\n");
  }

  float get_seqblock_x1(struct SeqBlock *seqblock, double start_time, double end_time){
    return scale(seqblock->start_time, start_time, end_time, 0, width());
  }
  
  float get_seqblock_x2(struct SeqBlock *seqblock, double start_time, double end_time){
    return scale(seqblock->end_time, start_time, end_time, 0, width());
  }

  float get_seqblock_x1(int seqblocknum){
    R_ASSERT_RETURN_IF_FALSE2(seqblocknum>=0, 0);
    R_ASSERT_RETURN_IF_FALSE2(seqblocknum<_seqtrack->seqblocks.num_elements, 0);
    
    SEQTRACK_update_all_seqblock_start_and_end_times(_seqtrack);
    double start_time = _start_time / MIXER_get_sample_rate();
    double end_time = _end_time / MIXER_get_sample_rate();
    return get_seqblock_x1((struct SeqBlock*)_seqtrack->seqblocks.elements[seqblocknum], start_time, end_time);
  }

  float get_seqblock_x2(int seqblocknum){
    R_ASSERT_RETURN_IF_FALSE2(seqblocknum>=0, 0);
    R_ASSERT_RETURN_IF_FALSE2(seqblocknum<_seqtrack->seqblocks.num_elements, 0);

    SEQTRACK_update_all_seqblock_start_and_end_times(_seqtrack);
    double start_time = _start_time / MIXER_get_sample_rate();
    double end_time = _end_time / MIXER_get_sample_rate();
    return get_seqblock_x2((struct SeqBlock*)_seqtrack->seqblocks.elements[seqblocknum], start_time, end_time);
  }

  const float cursor_width = 2.7;
  float _last_painted_cursor_x = 0.0f;
  
  float get_curr_cursor_x(int frames_to_add){
    return scale_double(ATOMIC_GET(pc->song_abstime)+frames_to_add, _start_time, _end_time, 0, width());
  }

  
  void paintTrack(QPainter &p, float x1, float y1, float x2, float y2, const struct Blocks *block, const struct Tracks *track, int64_t blocklen) const {
    QColor color(1,1,200,150);
    
#define SHOW_BARS 0

#if SHOW_BARS
    p.setBrush(QBrush(color));
#else
    const float bar_height = 2.3;
    
    QPen pen(color);
    pen.setWidthF(bar_height);
    pen.setCapStyle(Qt::FlatCap);
    p.setPen(pen);
#endif
    
    struct Notes *note = track->notes;
    while(note != NULL){

      int64_t start = Place2STime(block, &note->l.p);
      int64_t end = Place2STime(block, &note->end);

      float n_x1 = scale(start, 0, blocklen, x1, x2);
      float n_x2 = scale(end, 0, blocklen, x1, x2);

#if SHOW_BARS
      float n_y1 = scale(note->note, 127, 0, y1, y2);
      float n_y2 = y2;
      QRectF rect(n_x1, n_y1, n_x2-n_x1, n_y2-n_y1);
      p.drawRect(rect);
      
#else
      float n_y = scale(note->note+0.5, 127, 0, y1, y2);
                  
      QLineF line(n_x1,n_y,n_x2,n_y);
      
      p.drawLine(line);
#endif

#undef SHOW_BARS
      
      note = NextNote(note);
    }
  }
  
  void paintBlock(QPainter &p, const QRectF &rect, const struct Blocks *block){
    qreal x1,y1,x2,y2;
    rect.getCoords(&x1, &y1, &x2, &y2);
      
    p.fillRect(rect, QColor(140,140,140));

    const int header_height = root->song->tracker_windows->fontheight + 2;
    
    QColor text_color = get_qcolor(MIXER_TEXT_COLOR_NUM);
    QColor border_color(50,20,35);// = get_qcolor(MIXER_BORDER_COLOR_NUM);

    QColor header_border_color = QColor(20,20,20);
    QColor track_border_color = QColor(20,20,20,128);

    QPen header_border_pen(header_border_color);
    QPen track_border_pen(track_border_color);

    header_border_pen.setWidthF(2.3);
    track_border_pen.setWidthF(1.3);

    //if (x1 > -5000) { // avoid integer overflow error.
    p.setPen(text_color);
    //p.drawText(x1+4,2,x2-x1-6,height()-4, Qt::AlignLeft, QString::number(seqblock->block->l.num) + ": " + seqblock->block->name);
    p.drawText(rect.adjusted(2,1,-4,-(rect.height()-header_height)), QString::number(block->l.num) + ": " + block->name, QTextOption(Qt::AlignLeft | Qt::AlignTop));
    //}
    
    p.setPen(border_color);
    p.drawRect(rect);          

    int64_t blocklen = getBlockSTimeLength(block);
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
  
  void paintEvent ( QPaintEvent * ev ) override {
    
    QPainter p(this);

    p.setRenderHints(QPainter::Antialiasing,true);
        
    //printf("PAINTING seqblocks. gakk\n");

    p.fillRect(1,1,width()-2,height()-1, QColor(50,50,50));

    double sample_rate = MIXER_get_sample_rate();
    //double song_length = get_visible_song_length()*sample_rate;
  
    SEQTRACK_update_all_seqblock_start_and_end_times(_seqtrack);

    double start_time = _start_time / sample_rate;
    double end_time = _end_time / sample_rate;

    VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &_seqtrack->seqblocks){

      if (seqblock->start_time < end_time && seqblock->end_time >= start_time) {
        
        float x1 = get_seqblock_x1(seqblock, start_time, end_time);
        float x2 = get_seqblock_x2(seqblock, start_time, end_time);

        QRectF rect(x1,1,x2-x1,height()-2);

        if (ev->rect().intersects(rect.toAlignedRect()))
          paintBlock(p, rect, seqblock->block);
        
      }
      
    }END_VECTOR_FOR_EACH;


    
    // Cursor
    {
      QPen pen(QColor(200,20,20));
      pen.setWidthF(cursor_width);

      _last_painted_cursor_x = get_curr_cursor_x(0);
      
      QLineF line(_last_painted_cursor_x, 0, _last_painted_cursor_x, height());
      
      p.setPen(pen);
      p.drawLine(line);
    }
  }

  int _last_num_seqblocks = 0;
  
  void call_very_often(void){
    if (_last_num_seqblocks != _seqtrack->seqblocks.num_elements) {
      update();
      _last_num_seqblocks = _seqtrack->seqblocks.num_elements;
    }

    {
      if (_time.elapsed() > 1000) { // Update every second.
        update();
        _time.restart();
      }
    }
    
    if (is_playing() && pc->playtype==PLAYSONG) {
      float x = get_curr_cursor_x(1 + MIXER_get_sample_rate() * 60.0 / 1000.0);

      float x_min = R_MIN(x-cursor_width/2.0, _last_painted_cursor_x-cursor_width/2.0) - 1;
      float x_max = R_MAX(x+cursor_width/2.0, _last_painted_cursor_x+cursor_width/2.0) + 2;

      //printf("x_min -> x_max: %f -> %f\n",x_min,x_max);
      update(x_min, 0, 1+x_max-x_min, height());
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
    , _seqblocks_widget(new Seqblocks_widget(this, seqtrack, start_time, end_time))
    , _seqtrack(add_gc_root(seqtrack))
  {
    setupUi(this);

    main_layout->addWidget(_seqblocks_widget);
  }

  ~Seqtrack_widget(){
    remove_gc_root(_seqtrack);
    printf("Seqtrack widget deleted\n");
  }

  void updateWidgets(void){
  }

  void call_very_often(void){
    _seqblocks_widget->call_very_often();
  }

public slots:

};


namespace{

class Seqtracks_widget : public radium::VerticalScroll {
public:
  
  QVector<Seqtrack_widget*> _seqtrack_widgets;

  const int64_t &_start_time;
  const int64_t &_end_time;
  
  Seqtracks_widget(QWidget *parent, const int64_t &start_time, const int64_t &end_time)
    :radium::VerticalScroll(parent)
    ,_start_time(start_time)
    ,_end_time(end_time)
  {
    update_seqtracks();
    setContentsMargins(0,0,0,0);
    layout->setContentsMargins(0,0,0,0);
    layout->setSpacing(0);
  }

  void my_update(void){
    for(auto *seqtrack_widget : _seqtrack_widgets)
      seqtrack_widget->update();
    
    update();
  }
  
  void update_seqtracks(void){
    printf("  Updating seqtracks\n");
    
    for(auto *seqtrack_widget : _seqtrack_widgets)
      delete seqtrack_widget;
      
    _seqtrack_widgets.clear();
    
    VECTOR_FOR_EACH(struct SeqTrack *, seqtrack, &root->song->seqtracks){
      Seqtrack_widget *seqtrack_widget = new Seqtrack_widget(this, seqtrack, _start_time, _end_time);
      _seqtrack_widgets.push_back(seqtrack_widget);
      addWidget(seqtrack_widget);
    }END_VECTOR_FOR_EACH;
  }

  void call_very_often(void){
    
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

  Seqtrack_widget *get_seqtrack_widget(struct SeqTrack *seqtrack){
    for(auto *seqtrack_widget : _seqtrack_widgets)
      if (seqtrack_widget->_seqtrack==seqtrack)
        return seqtrack_widget;
    
    R_ASSERT(false);    
    return NULL;
  }
  
  Seqtrack_widget *get_seqtrack_widget(int seqtracknum){
    if (seqtracknum >= _seqtrack_widgets.size())
      update_seqtracks();
  
    R_ASSERT_RETURN_IF_FALSE2(seqtracknum < _seqtrack_widgets.size(), NULL);

    return _seqtrack_widgets.at(seqtracknum);
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
                   
    QColor border_color = get_qcolor(MIXER_BORDER_COLOR_NUM);
    //QColor text_color = get_qcolor(MIXER_TEXT_COLOR_NUM);

    QRect rect(1,1,width()-1,height()-2);
    p.fillRect(rect, QColor(140,140,140));
    
    //p.setPen(text_color);
    //p.drawText(4,2,width()-6,height()-4, Qt::AlignLeft, "timeline");
    
    p.setPen(border_color);
    p.drawRect(rect);

    // This code is copied from hurtigmixer. (translated from scheme)

    p.setBrush(QColor(200,50,50));
    
    double min_pixels_between_text = width() / 4;

    double start_time = _start_time / MIXER_get_sample_rate();
    double end_time = _end_time / MIXER_get_sample_rate();

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

    const int t1 = 4;
    const int t2 = 8;
    
    int64_t time = inc_time * int((double)start_time/(double)inc_time);
    
    for(;;){
      const double x = scale(time, start_time, end_time, 0, width());
      if (x >= width())
        break;

      if (x > 20) {
        const double y1 = 5;
        const double y2 = height() - t2 - 4;
        p.drawText(x-12, y1+10, seconds_to_timestring(time));
        draw_filled_triangle(p, x-t1, y2, x+t1, y2, x, y2+t2);
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
  
private:
  float get_x1(double total){
    return scale(_start_time, 0, total, 0, width());
  }
  float get_x2(double total){
    return scale(_end_time, 0, total, 0, width());
  }
public:

  float get_x1(void){
    return get_x1(get_visible_song_length()*MIXER_get_sample_rate());
  }
  float get_x2(void){
    return get_x2(get_visible_song_length()*MIXER_get_sample_rate());
  }
  
  void paintEvent ( QPaintEvent * ev ) override {
    QPainter p(this);

    double total_seconds = get_visible_song_length();
    double total = total_seconds*MIXER_get_sample_rate();
    
    p.setRenderHints(QPainter::Antialiasing,true);

    QColor border_color = get_qcolor(MIXER_BORDER_COLOR_NUM);      
    

    // Background
    //
    QRect rect1(1,1,width()-1,height()-2);
    p.fillRect(rect1, QColor(50,50,50));

    //QColor text_color = get_qcolor(MIXER_TEXT_COLOR_NUM);
    //p.setPen(text_color);
    //p.drawText(4,2,width()-6,height()-4, Qt::AlignLeft, "seqtracks navigator");

        
    //printf("   start: %f -> %f (%d)\n", _start_time/48000.0, _end_time/48000.0, width());

    //printf("\n\n\n Navigator update. end: %f\n",_end_time/48000.0);
    
    // Blocks
    {

      QColor block_color = QColor(140,140,140,180);
      QColor text_color = get_qcolor(MIXER_TEXT_COLOR_NUM);
      
      int num_seqtracks = root->song->seqtracks.num_elements;
      int seqtracknum = 0;
      for(Seqtrack_widget *seqtrack_widget : _seqtracks_widget._seqtrack_widgets) {
        struct SeqTrack *seqtrack = seqtrack_widget->_seqtrack;
        
        float y1 = scale(seqtracknum, 0, num_seqtracks, 5, height()-10);
        float y2 = y1 + (float)(height()-10) / (float)num_seqtracks;
        
        SEQTRACK_update_all_seqblock_start_and_end_times(seqtrack);
        //double start_time = _start_time / MIXER_get_sample_rate();
        //double end_time = _end_time / MIXER_get_sample_rate();

        VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
          
          //printf("\n\n\n Start/end: %f / %f. Seqtrack/seqblock %p / %p\n\n", seqblock->start_time, seqblock->end_time, seqtrack, seqblock);
            
          float x1 = scale(seqblock->start_time, 0, total_seconds, 0, width()); //seqtrack_widget->_seqblocks_widget->get_seqblock_x1(seqblock, start_time, end_time);
          float x2 = scale(seqblock->end_time, 0, total_seconds, 0, width()); //seqtrack_widget->_seqblocks_widget->get_seqblock_x2(seqblock, start_time, end_time);
          
          QRectF rect(x1,y1+1,x2-x1,y2-y1-2);
          p.fillRect(rect, block_color);
          
          p.setPen(text_color);
          p.drawText(rect.adjusted(2,1,-1,-1), QString::number(seqblock->block->l.num) + ": " + seqblock->block->name, QTextOption(Qt::AlignLeft | Qt::AlignTop));
          
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

      QRectF rectA(0, 1, x1, height()-2);
      QRectF rectB(x2, 1, width()-x2, height()-2);      
      QRectF rect2(x1,1,x2-x1,height()-2);
      
      p.fillRect(rectA, QColor(140,140,140,100));
      p.fillRect(rectB, QColor(140,140,140,100));

      p.setPen(border_color);
      p.drawRect(rect2);
      
      float handle1_x = x1+SEQNAV_SIZE_HANDLE_WIDTH;
      float handle2_x = x2-SEQNAV_SIZE_HANDLE_WIDTH;
      //p.drawLine(handle1_x, 0, handle1, height());
      //p.drawLine(handle2_x, 0, handle1, height());
      
      QRectF handle1_rect(x1, 0, handle1_x-x1, height());
      QRectF handle2_rect(handle2_x, 0, x2-handle2_x, height());
      
      p.setBrush(QColor(20,20,20,90));
      
      p.drawRect(handle1_rect);
      p.drawRect(handle2_rect);
    }
    
  }

};

struct Sequencer_widget : public QWidget {

  int _old_width = 600;
  int64_t _start_time = 0;
  int64_t _end_time;
  double _samples_per_pixel;
  
  const int timeline_widget_height = 30;
  const int bottom_height = 30;
  
  Timeline_widget _timeline_widget;
  Seqtracks_widget _seqtracks_widget;
  Seqtracks_navigator_widget _navigator_widget;
  MyQSlider _main_reltempo;
  
  Sequencer_widget(QWidget *parent)
    : QWidget(parent)
    , _end_time(get_visible_song_length()*MIXER_get_sample_rate())
    , _samples_per_pixel((_end_time-_start_time) / width())
    , _timeline_widget(this, _start_time, _end_time)
    , _seqtracks_widget(this, _start_time, _end_time)
    , _navigator_widget(this, _start_time, _end_time, _seqtracks_widget)
    , _main_reltempo(this)
  {
      
    _timeline_widget.show();
    _seqtracks_widget.show();
    _navigator_widget.show();
    _main_reltempo.show();

    setMinimumHeight(200);
  }

  void my_update(void){
    int64_t song_length = MIXER_get_sample_rate() * get_visible_song_length();
    if (_end_time > song_length)
      _end_time = song_length;
    
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
  
  void resizeEvent( QResizeEvent *qresizeevent) override {
    //  set_end_time();
    // _samples_per_pixel = (_end_time-_start_time) / width();
    position_widgets();
  }
  
  void position_widgets(void){
    R_ASSERT_RETURN_IF_FALSE(_seqtracks_widget._seqtrack_widgets.size() > 0);

    const QWidget *mute_button = _seqtracks_widget._seqtrack_widgets.at(0)->mute_button;
    const QPoint p = mute_button->mapTo(this, mute_button->pos());
    
    const int x1 = p.x() + mute_button->width();
    const int x1_width = width() - x1;

    const int y1 = timeline_widget_height;
    const int y2 = height() - bottom_height;
    
    _timeline_widget.setGeometry(x1, 0,
                                 x1_width, timeline_widget_height);
    
    _seqtracks_widget.setGeometry(0, timeline_widget_height,
                                  width(), y2 - y1);

    _navigator_widget.setGeometry(x1, y2,
                                  x1_width, bottom_height);

    _main_reltempo.setGeometry(0, y2,
                               x1, bottom_height);
  }

  
  int _last_num_seqtracks = 0;
  void call_very_often(void){
    if (g_need_update){
      my_update();
      BS_UpdatePlayList();
      g_need_update=false;
    }
    
    _seqtracks_widget.call_very_often();

    if (_seqtracks_widget._seqtrack_widgets.size() != _last_num_seqtracks){
      position_widgets();
      my_update();
      _last_num_seqtracks = _seqtracks_widget._seqtrack_widgets.size();
    }    

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

int64_t SEQUENCER_get_visible_start_time(void){
  return g_sequencer_widget->_start_time;
}

int64_t SEQUENCER_get_visible_end_time(void){
  return g_sequencer_widget->_end_time;
}

void SEQUENCER_set_visible_start_time(int64_t val){
  R_ASSERT_RETURN_IF_FALSE(val < g_sequencer_widget->_end_time);
  
  g_sequencer_widget->_start_time = val;
  g_sequencer_widget->my_update();
}

void SEQUENCER_set_visible_end_time(int64_t val){
  R_ASSERT_RETURN_IF_FALSE(val > g_sequencer_widget->_start_time);
  
  g_sequencer_widget->_end_time = val;
  g_sequencer_widget->my_update();
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


// seqblocks

float SEQBLOCK_get_x1(int seqblocknum, int seqtracknum){
  auto *w0 = g_sequencer_widget->get_seqtrack_widget(seqtracknum);
  if (w0==NULL)
    return 0.0;
  
  auto *w = w0->_seqblocks_widget;

  return mapToEditorX(w, 0) + w->get_seqblock_x1(seqblocknum);
}

float SEQBLOCK_get_x2(int seqblocknum, int seqtracknum){
  auto *w0 = g_sequencer_widget->get_seqtrack_widget(seqtracknum);
  if (w0==NULL)
    return 0.0;
  
  auto *w = w0->_seqblocks_widget;
  if (w==NULL)
    return 0.0;

  return mapToEditorX(w, 0) + w->get_seqblock_x2(seqblocknum);
}

float SEQBLOCK_get_y1(int seqblocknum, int seqtracknum){
  auto *w0 = g_sequencer_widget->get_seqtrack_widget(seqtracknum);
  if (w0==NULL)
    return 0.0;
  
  auto *w = w0->_seqblocks_widget;
  if (w==NULL)
    return 0.0;

  return mapToEditorY(w, 0);
}

float SEQBLOCK_get_y2(int seqblocknum, int seqtracknum){
  auto *w0 = g_sequencer_widget->get_seqtrack_widget(seqtracknum);
  if (w0==NULL)
    return 0.0;
  
  auto *w = w0->_seqblocks_widget;
  if (w==NULL)
    return 0.0;

  return mapToEditorY(w, w->height());
}



// seqtracks

float SEQTRACK_get_x1(int seqtracknum){
  auto *w = g_sequencer_widget->get_seqtrack_widget(seqtracknum);
  if (w==NULL)
    return 0.0;
    
  return mapToEditorX(w, w->x());
}

float SEQTRACK_get_x2(int seqtracknum){
  auto *w = g_sequencer_widget->get_seqtrack_widget(seqtracknum);
  if (w==NULL)
    return 0.0;
    
  return mapToEditorX(w, w->x()+w->width());
}

float SEQTRACK_get_y1(int seqtracknum){
  auto *w = g_sequencer_widget->get_seqtrack_widget(seqtracknum);
  if (w==NULL)
    return 0.0;
    
  return mapToEditorY(w, w->y());
}

float SEQTRACK_get_y2(int seqtracknum){
  auto *w = g_sequencer_widget->get_seqtrack_widget(seqtracknum);
  if (w==NULL)
    return 0.0;
    
  return mapToEditorY(w, w->y()+w->height());
}

void SEQTRACK_update(struct SeqTrack *seqtrack){
  auto *w = g_sequencer_widget->get_seqtrack_widget(seqtrack);
  if (w!=NULL)
    w->update();
}

void SEQUENCER_update(void){
  if (g_sequencer_widget != NULL)
    g_sequencer_widget->my_update();
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
