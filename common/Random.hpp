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


  /***************************************************************************************************
   *                                                                                                 *
   * 'next_random' is from the s7 source code written by Bill Schottstaedt.                          *
   * The only changes I've done is 'random_seed(r); -> '_seed', and 'random_curry(r)' -> '_curry'.   *
   *                                                                                                 *
   ***************************************************************************************************/
  
  
  uint64_t _seed = 1;
  uint64_t _carry = 1675393560;                          /* should this be dependent on the seed? */
  
  double next_random(void)
  {
    /* The multiply-with-carry generator for 32-bit integers:
     *        x(n)=a*x(n-1) + carry mod 2^32
     * Choose multiplier a from this list:
     *   1791398085 1929682203 1683268614 1965537969 1675393560
     *   1967773755 1517746329 1447497129 1655692410 1606218150
     *   2051013963 1075433238 1557985959 1781943330 1893513180
     *   1631296680 2131995753 2083801278 1873196400 1554115554
     * ( or any 'a' for which both a*2^32-1 and a*2^31-1 are prime)
     */
    double result;
    uint64_t temp;
#define RAN_MULT 2131995753UL
    
    temp = _seed * RAN_MULT + _carry;
    _seed = (temp & 0xffffffffUL);
    _carry = (temp >> 32);
    result = (double)((uint32_t)(_seed)) / 4294967295.5;
    /* divisor was 2^32-1 = 4294967295.0, but somehow this can round up once in a billion tries?
     *   do we want the double just less than 2^32?
     */
    
    /* (let ((mx 0) (mn 1000)) (do ((i 0 (+ i 1))) ((= i 10000)) (let ((val (random 123))) (set! mx (max mx val)) (set! mn (min mn val)))) (list mn mx)) */
    return(result);
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
