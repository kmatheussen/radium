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

#ifndef _RADIUM_COMMON_RANDOM_HPP
#define _RADIUM_COMMON_RANDOM_HPP


/*

Doesn't seem like QRandomGenerator is realtime safe. From the source code:

"""
  SystemAndGlobalGenerators::PRNGLocker lock(this);
"""

#if QT_VERSION_MAJOR == 5 && QT_VERSION_MINOR == 9 // Qt 5.9 doesn't have QRandomGenerator
struct QRandomGenerator{
  double bounded(double max){
    auto val = qrand();
    return scale_double(val, 0, RAND_MAX, 0, max);
  }
};
#else
#include <QRandomGenerator>
#endif
*/

#define D2(a)


namespace radium{

struct Random {
  double _mi = 0;
  double _ma = 1;
  double _middle = 0.5;

  double _min_skew = 0;
  double _max_skew = 1;

  double _skew; // i.e. accumulated skew since last call to reset.

  void reset(void){
    _skew = 0;
  }

  // Example: If max_skew = 1.0, mi=0, ma=20, and the accumulated skew (_skew) goes below 9, or above 11,
  // 'get_next' will adjust the boundaries so that _skew starts increasing slightly more than decreasing until _skew is between the max_skew boundaries again.
  //
  void set_boundaries(const double mi, const double ma, const double max_skew = 0){
    R_ASSERT_NON_RELEASE(ma > mi);
    
    _mi = mi;
    _ma = ma;
    _middle = (mi+ma)/2.0;
    
    _min_skew = _middle - max_skew;
    _max_skew = _middle + max_skew;
  }

  
private:

  uint32_t _randx = 1;

#define INVERSE_MAX_RAND2 0.000030517579
  
  double next_random(void){
    
    // This code is copied from sndlib made by Bill Schottstaedt (free license)
    // Comment from sndlib: "rand taken from the ANSI C standard (essentially the same as the Cmix form used earlier)"
    
    _randx = _randx * 1103515245 + 12345;
    
    double a = ((double)((uint32_t)(_randx >> 16) & 32767)) * INVERSE_MAX_RAND2;

    R_ASSERT_NON_RELEASE(a>=0);
    R_ASSERT_NON_RELEASE(a<=1);
    
    return a;
  }
  
public:
  
  double get_next(double mi, double ma) {
    return mi + next_random() * (ma-mi);
  }
  
  double get_next(void) {
    return get_next(_mi, _ma);
  }
  
  double get_next_adjusted(void) {
    
    double mi = _mi;
    double ma = _ma;

    if(true) { //_num_calls > _num_calls_before_starting_to_adjust) {

      if (_skew < 0) {
        if (_middle+_skew < _min_skew){
          mi = scale_double(1, 0, 16, _mi, _ma);
          D2(printf("   Adjusting MIN: %f -> %f\n", _mi, mi););
        }
      } else {
        if (_middle+_skew > _max_skew){
          ma = scale_double(1, 0, 16, _ma, _mi);
          D2(printf("   Adjusting MAX: %f -> %f\n", _ma, ma););
        }
      }
      
    }

    double ret = get_next(mi, ma);

    _skew += (ret-_middle);

    D2(printf("_skew: %f  (now: %f. middle: %f. min_skew: %f. max_skew: %f)\n", _skew, (ret-_middle), _middle, _min_skew, _max_skew););
    
    return ret;
  }
};
}


#endif
