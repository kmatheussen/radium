
/* Copyright 2018 Kjetil S. Matheussen

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


/*
  Lots of code in this file is taken from Ardour (and lightly modified): http://www.ardour.org
  Copyright (C) 2000-2006 Paul Davis.
 */

#ifndef _RADIUM_AUDIO_ENVELOPE_HPP
#define _RADIUM_AUDIO_ENVELOPE_HPP



#include <QVector>
#include <QIcon>
#include <QStringList>
#include <QPen>
#include <QPainter>

#include "SoundPlugin.h"


namespace radium{


// From Ardour.
#define GAIN_COEFF_ZERO 0.f
#define GAIN_COEFF_SMALL 0.0000001f  //-140dB
#define GAIN_COEFF_UNITY 1.f


class Envelope{

public:

  struct Point{
    float x,y;
    
    Point(float x, float y)
      : x(x)
      , y(y)
    {
    }

    // required by QVector.
    Point(){
      abort();
    }
  };

private:
  
  int _num_points = 0;
  float *_xs=NULL,*_ys=NULL;
  
  int _last_i = 1;
  
public:

  FadeShape _shape;
  bool _is_fade_in;

  Envelope(const Envelope &env)
    : Envelope(env._num_points, env._xs, env._ys)
  {
    _shape = env._shape;
    _is_fade_in = env._is_fade_in;
  }

  
  Envelope(int num_points, const float *xs, const float *ys)
    : _num_points(num_points)
    , _xs((float*)memcpy(malloc(num_points*sizeof(float)), xs, num_points*sizeof(float)))
    , _ys((float*)memcpy(malloc(num_points*sizeof(float)), ys, num_points*sizeof(float)))
    , _shape(FADE_CUSTOM)
    , _is_fade_in(true)
  {
    R_ASSERT(num_points >= 2);
  }

  Envelope(const QVector<Point> &points)
    : _shape(FADE_CUSTOM)
    , _is_fade_in(true)
  {
    setPoints(points);
  }

  int getPoints2(float **xs, float **ys) const {
    *xs = _xs;
    *ys = _ys;
    return _num_points;
  }
  
  QVector<Point> getPoints(void) const {
    QVector<Point> ret;
    for(int i=0;i<_num_points;i++)
      ret.push_back(Point(_xs[i], _ys[i]));
    return ret;
  }

  void setPoints(const QVector<Point> &points){
    R_ASSERT(points.size() >= 2);
    
    free(_xs);
    free(_ys);
    _num_points = points.size();
    _xs = (float*)malloc(_num_points*sizeof(float));
    _ys = (float*)malloc(_num_points*sizeof(float));
    for(int i=0; i < _num_points ; i++){
      _xs[i] = points.at(i).x;
      _ys[i] = points.at(i).y;
    }
  }

  // from ardour (code based on)
  QVector<Point> generate_db_fade_out (double len, int num_steps, float dB_drop) const {
    QVector<Point> env;
    env.push_back(Point(0, 1));
    
    //generate a fade-out curve by successively applying a gain drop
    float fade_speed = db2gain(dB_drop / (float) num_steps);
    float coeff = GAIN_COEFF_UNITY;
    for (int i = 1; i < (num_steps-1); i++) {
      coeff *= fade_speed;
      env.push_back(Point(len*(double)i/(double)num_steps, coeff));
    }
    
    env.push_back(Point(len, GAIN_COEFF_SMALL));
    //fprintf(stderr, "Size: %d\n", env.size());
    return env;
  }

  // from ardour (code based on)
  QVector<Point> get_reversed_curve (const QVector<Point> &env) const {
    QVector<Point> ret;

    int length = env.size();
    float last_x = env.at(length-1).x;

    for(int i=length-1 ; i >= 0 ; i--){
      //fprintf(stderr,"xi: %d. yi: %d. Size: %d\n", i, env.size()-i-1, env.size());
      ret.push_back(Point(last_x - env.at(i).x, env.at(i).y));
    }

    return ret;
  }

  // from ardour (code based on)
  QVector<Point> generate_inverse_power_curve (const QVector<Point> &src) const {
    QVector<Point> ret;
    // calc inverse curve using sum of squares
    for (const Point &point : src){
      float value = point.y;
      value = 1 - powf(value,2);
      value = sqrtf(value);
      ret.push_back(Point(point.x, value));
    }
    return ret;
  }

  // from ardour (code based on)
  QVector<Point> merge_curves (const QVector<Point> &curve1, const QVector<Point> &curve2) const {
    QVector<Point> dst;

    int size = curve1.size();

    //curve lengths must match for now
    if (size != curve2.size()) {
      R_ASSERT_NON_RELEASE(false);
      return dst;
    }

    for(int i = 0 ; i < size ; i++){
      const Point &point1 = curve1.at(i);
      const Point &point2 = curve2.at(i);

      float v1 = gain2db(point1.y);
      float v2 = gain2db(point2.y);
      
      double interp = v1 * ( 1.0-( (double)i / (double)size) );
      interp += v2 * ( (double)i / (double)size );
      
      interp = db2gain(interp);
      dst.push_back(Point(point1.x, interp));
    }

    return dst;
  }
  
  // from ardour (code based on)
  Envelope(FadeShape shape, double length, bool want_fade_in)
    : _shape(shape)
    , _is_fade_in(want_fade_in)
  {
    QVector<Point> env;
    bool env_is_fade_in;

    const int num_steps = 32;

    // Generate fade in.
    //
    switch (shape) {
      case FADE_CUSTOM:
        abort();
        break;

      case FADE_LINEAR:
        env.push_back(Point(0.0, GAIN_COEFF_UNITY));
        env.push_back(Point(length, GAIN_COEFF_SMALL));
        env_is_fade_in = false;
        break;
        
      case FADE_FAST:
        env = generate_db_fade_out (length, num_steps, MIN_DB);
        //fprintf(stderr, "Size3: %d\n", env.size());
        env_is_fade_in = false;
        break;
        
      case FADE_SLOW:
        {
          auto c1 = generate_db_fade_out (length, num_steps, -1);  // start off with a slow fade
          auto c2 = generate_db_fade_out (length, num_steps, -80); // end with a fast fade
          env = merge_curves(c1, c2);
          env_is_fade_in = false;
        }
        break;
        
      case FADE_CONSTANT_POWER:
        env.push_back(Point(0.0, GAIN_COEFF_SMALL));
        for (int i = 1; i < num_steps; ++i) {
          const float dist = i / (num_steps + 1.f);
          env.push_back(Point(length * dist, sin (dist * M_PI / 2.0)));
        }
        env.push_back(Point(length, GAIN_COEFF_UNITY));
        env_is_fade_in = true;
        break;
        
      case FADE_SYMMETRIC:
        {
          //start with a nearly linear cuve
          env.push_back(Point(0, 1));
          env.push_back(Point(0.5 * length, 0.6));

          //now generate a fade-out curve by successively applying a gain drop
          const double breakpoint = 0.7;  //linear for first 70%

          for (int i = 2; i < 9; ++i) {
            const float coeff = (1.f - breakpoint) * powf (0.5, i);
            env.push_back(Point(length * (breakpoint + ((GAIN_COEFF_UNITY - breakpoint) * (double)i / (double)9)), coeff));
          }

          env.push_back(Point(length, GAIN_COEFF_SMALL));
          env_is_fade_in = false;
        }
        break;
    }

    //fprintf(stderr, "Size2: %d\n", env.size());

    if (want_fade_in != env_is_fade_in)
      env = get_reversed_curve(env);

    setPoints(env);
  }
  
  ~Envelope(){
    free(_xs);
    free(_ys);
  }

  void myFilledPolygon(QPainter &p, QPointF *points, int num_points, const QColor &color) const {
    QPen pen = p.pen();
    p.setPen(Qt::NoPen);
    p.setBrush(color);
    p.drawPolygon(points, num_points);
    p.setBrush(Qt::NoBrush);
    p.setPen(pen);
  }

  QIcon get_icon(int width, int height, QColor foreground, QColor background, QColor pen_color) const {

    QPixmap pixmap(width, height);
    pixmap.fill(background);

    {
      QPainter p(&pixmap);
      p.setRenderHints(QPainter::Antialiasing,true);
      
      float *xs, *ys;
      int num_points = getPoints2(&xs, &ys);
      
      QPointF points[num_points+2];
      
      for(int i=0;i<num_points;i++){
        points[i].setX(scale(xs[i], 0, 1, 0, width));
        points[i].setY(scale(ys[i], 0, 1, height, 0));
      }

      if (_is_fade_in) {

        points[num_points].setX(width);
        points[num_points].setY(0);
        
        points[num_points+1].setX(0);
        points[num_points+1].setY(0);

      } else {
        points[num_points].setX(width);
        points[num_points].setY(0);
        
        points[num_points+1].setX(0);
        points[num_points+1].setY(0);
      }

      myFilledPolygon(p, points, num_points+2, foreground);
      QPen sel_pen(pen_color);
      sel_pen.setWidthF(get_system_fontheight() / 3);
      p.setPen(sel_pen);
      p.drawPolyline(points, num_points);
    }

    return QIcon(pixmap);
  }

  QString get_icon_filename(void) const {
    R_ASSERT(_shape != FADE_CUSTOM);
    return QString("<<<<<<<<<<envelope_icon^") + fade_shape_to_string(_shape) + "^" + (_is_fade_in ? "fadein" : "fadeout");
  }

  float get_y(float x) {
    R_ASSERT_RETURN_IF_FALSE2(_num_points > 0, 0.0);

    int i = _last_i;
    
    if (x < _xs[i])
      i = 1;

    for( ; i < _num_points ; i++)
      if (x <= _xs[i])
        break;

    _last_i = i;

    return scale(x, _xs[i-1], _xs[i], _ys[i-1], _ys[i]);
  }
    
};
  
}

static inline QIcon ENVELOPE_get_icon(QString text, int width, int height, QColor foreground, QColor background, QColor pen_color){
  QStringList args = text.split("^");
  R_ASSERT_RETURN_IF_FALSE2(args.size()==3, QIcon());

  radium::Envelope env(string_to_fade_shape(args[1].toUtf8().constData()), 1.0, args[2]=="fadein");

  return env.get_icon(width, height, foreground, background, pen_color);
}

#if 0 // Very inefficient since Envelope::get_y is not a const method. ('_last_i' will be set back to 1 very often)

// Length = 1.0
extern radium::Envelope g_linear_fade_in;
extern radium::Envelope g_fast_fade_in;
extern radium::Envelope g_slow_fade_in;
extern radium::Envelope g_constant_power_fade_in;
extern radium::Envelope g_symmetric_fade_in;

// Length = 1.0
extern radium::Envelope g_linear_fade_out;
extern radium::Envelope g_fast_fade_out;
extern radium::Envelope g_slow_fade_out;
extern radium::Envelope g_constant_power_fade_out;
extern radium::Envelope g_symmetric_fade_out;

#endif

#endif
