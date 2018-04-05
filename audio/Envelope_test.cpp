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


// g++ `pkg-config --cflags --libs Qt5Gui Qt5Core` -Wall -Wextra -Werror -g Envelope_test.cpp -fPIC && ./a.out
//
// This script must be in path: https://github.com/kmatheussen/various_scripts/blob/master/plot


#include <stdlib.h>
#include <math.h>

#include <QString>

// Used by the volume sliders and peak meters.
#define MIN_DB -40           // "gain value" = 0.0. "scaled effect value" = 0.0.
#define MIN_DB_THRESHOLD -35 // "gain_value" = 0.01778279410038923. Between MIN_DB and MIN_DB_THRESHOLD, we do linear gain<->db conversion. "scaled effect value" = 0.06666666666666667.
#define MAX_DB 35            // "gain value" = 56.23413251903491. "scaled effect value" = 1.0.
  
#define MAX_DB_GAIN 56.23413251903491 // = powf(10, MAX_DB/20.0f);
#define THRESHOLD_GAIN 0.01778279410038923 // = powf(10, MIN_DB_THRESHOLD / 20.0f);

static inline float scale(float x, float x1, float x2, float y1, float y2){
  if(x2==x1)
    abort();
  return y1 + ( ((x-x1)*(y2-y1))
                /
                (x2-x1)
                );
}
  
static inline float gain2db(float gain){
  if (gain <= THRESHOLD_GAIN) {

    if(gain<=0.0f)
      return MIN_DB;

    // We need to do linear conversion below MIN_DB_THRESHOLD here in order to convert back and forth between gain2db and db2gain correctly. (that's probably the only reason)
    
    return scale(gain, 0, THRESHOLD_GAIN, MIN_DB, MIN_DB_THRESHOLD);
    
  } else {
    
    return 20*log10(gain);
    
  }
}

static inline float db2gain(float db){
  if (db <= MIN_DB_THRESHOLD){

    if (db <= MIN_DB)
      return 0.0f;
    
    // do linear scale down to zero when db is less than -35 (if not, we won't get 0 gain)
    
    return scale(db, MIN_DB, MIN_DB_THRESHOLD, 0, THRESHOLD_GAIN);
    
  }else{
    
    if (db > MAX_DB)
      db = MAX_DB;
    
    return powf(10, db / 20.0f);
  }
}

#define R_ASSERT_NON_RELEASE(maybe) do{if(!(maybe))abort();}while(0)
#define R_ASSERT_RETURN_IF_FALSE2(maybe,ret) R_ASSERT_NON_RELEASE(maybe)


#include "Envelope.hpp"

namespace radium{

void display(const Envelope &env, const char *name, bool is_fade_in){
  fprintf(stderr, "\n\n\n                ======== %s (%s) =========\n", name, is_fade_in ? "fade in" : "fade out");

  QString command = "plot "; // https://github.com/kmatheussen/various_scripts/blob/master/plot

  const QVector<Envelope::Point> points = env.getPoints();

  for(const Envelope::Point &point : points){
    command += QString::number(point.x) + " " + QString::number(point.y) + " ";
  }

  system(command.toUtf8().constData());

  getchar();
}

void testall(double length, bool is_fade_in){
  Envelope linear = Envelope(Envelope::Shape::FadeLinear, length, is_fade_in);
  Envelope fast = Envelope(Envelope::Shape::FadeFast, length, is_fade_in);
  Envelope slow = Envelope(Envelope::Shape::FadeSlow, length, is_fade_in);
  Envelope constant_power = Envelope(Envelope::Shape::FadeConstantPower, length, is_fade_in);
  Envelope symmetric = Envelope(Envelope::Shape::FadeSymmetric, length, is_fade_in);

  display(linear, "Linear", is_fade_in);
  display(fast, "Fast", is_fade_in);
  display(slow, "Slow", is_fade_in);
  display(constant_power, "Constant Power", is_fade_in);
  display(symmetric, "Symmetric", is_fade_in);
}

void testit(void){
  testall(1.0, true);
  testall(1.0, false);
}

}


int main(){
  radium::testit();
  while(true){
    getchar();
    printf("  PRESS CTRC-C!\n");
  }
  return 0;
}

