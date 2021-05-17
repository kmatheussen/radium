#ifndef _RADIUM_COMMON_RATIO_FUNCS_H
#define _RADIUM_COMMON_RATIO_FUNCS_H

#if !defined(__STDC_FORMAT_MACROS)
#define __STDC_FORMAT_MACROS 1
#endif

#include <inttypes.h>

#ifdef TEST_MAIN

# include <stdio.h>

# define R_ASSERT_NON_RELEASE(a) //if(!(a))abort()
# define R_ASSERT(a) if(!(a))abort()
# include <algorithm>
# include "ratio_type.h"

#else

//# include "placement_proc.h"

#endif

#include <math.h>

#if __MINGW32__
#  if __MINGW64__
#    define USE_RATIO128 1
#  else
#    define USE_RATIO128 0
#  endif
#else
#  define USE_RATIO128 1
#endif

#if USE_RATIO128
typedef struct {
  __int128_t num;
  __int128_t den;
} Ratio128;


static inline Ratio128 make_ratio128(Ratio ratio){
  Ratio128 ratio128 = {ratio.num, ratio.den};
  return ratio128;
}
#endif


static inline Ratio make_ratio_from_double_internal(double val, const int num_bits){

#ifndef TEST_MAIN
  //R_ASSERT_NON_RELEASE(false);
#endif

  int64_t den = (2LL << num_bits) / (1 + (int64_t)val);
  int64_t num = round((double)den * val);
  //printf("num: %d\n",(int)num);

  if (den == 0){
    den = 1;
#if !defined(RELEASE)
    abort();
#endif
  } else if (den < 0){
    num = -num;
    den = -den;
  }

  return make_ratio(num, den);
}

#ifdef __cplusplus
static inline Ratio make_ratio_from_double(double val, const int num_bits = 60){
  return make_ratio_from_double_internal(val, num_bits);
}
#else
static inline Ratio make_ratio_from_double(double val){
  return make_ratio_from_double_internal(val, 60);
}
#endif
  
static inline double make_double_from_ratio(Ratio r){
  return (double)r.num / (double)r.den;
}

#include "overflow_funcs.h"

static inline Ratio RATIO_mul(const Ratio r1, const Ratio r2){
  R_ASSERT_NON_RELEASE(r1.den > 0);
  R_ASSERT_NON_RELEASE(r2.den > 0);

  int64_t a,b;
  if (ov_mul(r1.num, r2.num, &a)){
    R_ASSERT_NON_RELEASE(false);
    return make_ratio_from_double(make_double_from_ratio(r1) * make_double_from_ratio(r2));
  }
  
  if (ov_mul(r1.den, r2.den, &b)){
    R_ASSERT_NON_RELEASE(false);
    return make_ratio_from_double(make_double_from_ratio(r1) * make_double_from_ratio(r2));
  }
  
  return make_ratio(a, b);
}

static inline Ratio RATIO_div(const Ratio r1, const Ratio r2){
  R_ASSERT_NON_RELEASE(r1.den > 0);
  R_ASSERT_NON_RELEASE(r2.den > 0);

  if (r2.num==0){
    R_ASSERT(false);
    return r1;
  }
  
  Ratio r2_2 = {r2.den, r2.num};
  return RATIO_mul(r1, r2_2);
}

static inline Ratio RATIO_add(const Ratio r1, const Ratio r2){
  R_ASSERT_NON_RELEASE(r1.den > 0);
  R_ASSERT_NON_RELEASE(r2.den > 0);

  if (r1.den==r2.den){
    int64_t a;
    if (ov_add(r1.num, r2.num, &a))
      goto overflow_fallback;
    
    return make_ratio(a, r1.den);
  }
  
  int64_t a1,a2,a,b;

  if (ov_mul(r1.num, r2.den, &a1))
    goto overflow_fallback;

  if (ov_mul(r2.num, r1.den, &a2))
    goto overflow_fallback;

  if (ov_add(a1, a2, &a))
    goto overflow_fallback;
  
  if (ov_mul(r1.den, r2.den, &b))
    goto overflow_fallback;

  return make_ratio(a, b);

 overflow_fallback:
  R_ASSERT_NON_RELEASE(false);
  return make_ratio_from_double(make_double_from_ratio(r1) + make_double_from_ratio(r2));
}

static inline Ratio RATIO_sub(const Ratio r1, const Ratio r2){
  Ratio r3 = {-r2.num, r2.den};
  return RATIO_add(r1, r3);
}




static inline bool RATIO_greater_than(const Ratio r1, const Ratio r2){
  //return RATIO_sub(r1, r2).den > 0;
  R_ASSERT_NON_RELEASE(r1.den > 0);
  R_ASSERT_NON_RELEASE(r2.den > 0);

  if (r1.den == r2.den) {

    // For instance 5/1 > 5/1
    
    return r1.num > r2.num;

  } else if (r1.den < r2.den) {

    // For instance 5/1 > 5/2
    
    if (r1.num >= r2.num)
      return true;

       
  } else {

    R_ASSERT_NON_RELEASE(r1.den > r2.den);

    // For instance 5/2 > 5/1
    
    if (r2.num >= r1.num)
      return false;
    
  }

  // For instance 1/2 > 2/3
  
  int64_t a,b;
  
  if (ov_mul(r1.num, r2.den, &a))
    goto overflow_fallback;
  
  if (ov_mul(r2.num, r1.den, &b))
    goto overflow_fallback;

  return a > b;

  
 overflow_fallback:

  R_ASSERT_NON_RELEASE(false);

#if USE_RATIO128
  Ratio128 ra = make_ratio128(r1);
  Ratio128 rb = make_ratio128(r2);
  return (ra.num * rb.den) > (rb.num * ra.den);
#else
  return make_double_from_ratio(r1) > make_double_from_ratio(r2);
#endif
}

static inline bool RATIO_less_than(const Ratio r1, const Ratio r2){
  if (RATIO_equal(r1, r2))
    return false;

  return !RATIO_greater_than(r1, r2);
}

static inline bool RATIO_greater_or_equal_than(const Ratio r1, const Ratio r2){
  return RATIO_equal(r1, r2) || RATIO_greater_than(r1, r2);
}

static inline bool RATIO_less_or_equal_than(const Ratio r1, const Ratio r2){
  return RATIO_equal(r1, r2) || RATIO_less_than(r1, r2);
}

static inline bool RATIO_is_zero(const Ratio r){
  R_ASSERT_NON_RELEASE(r.den > 0);
  return r.num == 0;
}

static inline bool RATIO_less_than_zero(const Ratio r){
  R_ASSERT_NON_RELEASE(r.den > 0);
  return r.num < 0;
}

static inline bool RATIO_greater_than_zero(const Ratio r){
  R_ASSERT_NON_RELEASE(r.den > 0);
  return r.num > 0;
}

static inline Ratio RATIO_floor(const Ratio r){
  R_ASSERT_NON_RELEASE(r.den > 0);
  if (r.den==1)
    return r;
  else
    return make_ratio(r.num / r.den, 1);
}

static inline Ratio RATIO_ceil(const Ratio r){
  R_ASSERT_NON_RELEASE(r.den > 0);
  if (r.den==1)
    return r;
  else
    return make_ratio(1 + (r.num / r.den), 1);
}


#ifdef __cplusplus                
static inline Ratio operator+(const Ratio &r1, const Ratio &r2){
  return RATIO_add(r1, r2);
}
static inline Ratio operator-(const Ratio &r1, const Ratio &r2){
  return RATIO_sub(r1, r2);
}
static inline Ratio operator*(const Ratio &r1, const Ratio &r2){
  return RATIO_mul(r1, r2);
}
static inline Ratio operator/(const Ratio &r1, const Ratio &r2){
  return RATIO_div(r1, r2);
}

static inline Ratio operator+(const Ratio &r1, int64_t i2){
  return RATIO_add(r1, make_ratio(i2, 1));
}
static inline Ratio operator-(const Ratio &r1, int64_t i2){
  return RATIO_sub(r1, make_ratio(i2, 1));
}
static inline Ratio operator*(const Ratio &r1, int64_t i2){
  return RATIO_mul(r1, make_ratio(i2, 1));
}
static inline Ratio operator/(const Ratio &r1, int64_t i2){
  return RATIO_div(r1, make_ratio(i2, 1));
}

static inline Ratio operator-(const Ratio &r){
  Ratio ret = r;
  ret.num = -ret.num;
  return ret;              
}

static inline bool operator==(const Ratio &r1, const Ratio &r2){
  return RATIO_equal(r1, r2);
}
static inline bool operator!=(const Ratio &r1, const Ratio &r2){
  return !RATIO_equal(r1, r2);
}

static inline bool operator>(const Ratio &r1, const Ratio &r2){
  return RATIO_greater_than(r1, r2);
}
static inline bool operator>=(const Ratio &r1, const Ratio &r2){
  return RATIO_greater_or_equal_than(r1, r2);
}
static inline bool operator<(const Ratio &r1, const Ratio &r2){
  return RATIO_less_than(r1, r2);
}
static inline bool operator<=(const Ratio &r1, const Ratio &r2){
  return RATIO_less_or_equal_than(r1, r2);
}

static inline bool operator==(const Ratio &r1, int64_t i2){
  return RATIO_equal(r1, make_ratio(i2,1));
}
static inline bool operator!=(const Ratio &r1, int64_t i2){
  return !RATIO_equal(r1, make_ratio(i2,1));
}

static inline bool operator>(const Ratio &r1, int64_t i2){
  return RATIO_greater_than(r1, make_ratio(i2,1));
}
static inline bool operator>=(const Ratio &r1, int64_t i2){
  return RATIO_greater_or_equal_than(r1, make_ratio(i2,1));
}
static inline bool operator<(const Ratio &r1, int64_t i2){
  return RATIO_less_than(r1, make_ratio(i2,1));
}
static inline bool operator<=(const Ratio &r1, int64_t i2){
  return RATIO_less_or_equal_than(r1, make_ratio(i2,1));
}

static inline Ratio& operator+=(Ratio& r1, const Ratio &r2){
  r1 = r1 + r2;
  return r1;
}

static inline Ratio& operator-=(Ratio& r1, const Ratio &r2){
  r1 = r1 - r2;
  return r1;
}

static inline Ratio& operator*=(Ratio& r1, const Ratio &r2){
  r1 = r1 * r2;
  return r1;
}

static inline Ratio& operator/=(Ratio& r1, const Ratio &r2){
  r1 = r1 / r2;
  return r1;
}

static inline Ratio& operator+=(Ratio& r1, int i2){
  r1 = r1 + make_ratio(i2, 1);
  return r1;
}

static inline Ratio& operator-=(Ratio& r1, int i2){
  r1 = r1 - make_ratio(i2, 1);
  return r1;
}

static inline Ratio& operator*=(Ratio& r1, int i2){
  r1 = r1 * make_ratio(i2, 1);
  return r1;
}

static inline Ratio& operator/=(Ratio& r1, int i2){
  r1 = r1 / make_ratio(i2, 1);
  return r1;
}

static inline Ratio scale_ratio(const Ratio &x, const Ratio &x1, const Ratio &x2, const Ratio &y1, const Ratio &y2){
  const Ratio diff = x2-x1;

#ifdef TEST_MAIN
  if (diff.num<=0)
    abort();
#else
  R_ASSERT_RETURN_IF_FALSE2(diff.num>0, y1);
#endif
  
  return y1 + ( ((x-x1)*(y2-y1))
                /
                diff
                );
}


#endif

#ifdef __cplusplus
namespace r{
  struct RatioPeriod{
    Ratio _start, _end;
    RatioPeriod(const Ratio &start, const Ratio &end)
      : _start(start)
      , _end(end)
    {
      R_ASSERT_NON_RELEASE(_end >= _start);
    }
    RatioPeriod(){
    }
  };
}
#endif


#undef USE_RATIO128

#ifndef TEST_MAIN
#ifndef TEST_TIMEDATA_MAIN
static inline char *ratio_to_string(const Ratio ratio){
  if(ratio.den==1)
    return talloc_format("%" PRId64 "", ratio.num);
  else
    return talloc_format("%" PRId64 "/%" PRId64 "", ratio.num, ratio.den);
}
#endif
#endif

#endif


#ifdef TEST_MAIN                

/*
ln -sf ratio_funcs.h test_ratio.cpp
g++ test_ratio.cpp -DTEST_MAIN -Wall -Werror -g -o a.out && gdb ./a.out
*/

static void comp(double a,double b){
  //printf("%f %f\n", fabs(a-b), std::max(a,b) / 10000000.0);
  if (fabs(a-b) > fabs(std::min(a,b) / 10000000000000.0))
    abort();
}

static void comp(Ratio a, Ratio b){
  if (a != b)
    abort();
}

static void comp(double a, Ratio b){
  comp(a, make_double_from_ratio(b));
}

int main(){
  double org1 = 34444223998701.213452909233423423434523452345;
  double org2 = -8456.4545245624561;
  Ratio r1 = make_ratio_from_double(org1);
  Ratio r2 = make_ratio_from_double(org2);
  double org3 = make_double_from_ratio(r1);
  double org4 = make_double_from_ratio(r2);

  comp(org1, org3);
  comp(org1, r1);
  comp(org2, org4);
  comp(org2, r2);
  comp(org1+org2, r1+r2);
  comp(org1-org2, r1-r2);
  comp(org1*org2, r1*r2);
  comp(org1/org2, r1/r2);

  comp(r1+r2-r2, r1);

  printf("org: %f. org3: %f. Diff: %f. r: %" PRId64 "/%" PRId64 "\n",org1,org3,fabs(org1-org3),r1.num,r1.den);

  Ratio r3 = make_ratio(2,3);
  Ratio r4 = make_ratio(4,5);
  Ratio r5 = r3;

  // test +=
  {
    r5 += r4;
    if (r5 != make_ratio(2,3) + r4)
      abort();
  }

  // test >, >=, ==, !=, <, and <= when r3.den != r4.den
  {
    if (r3 > r4)
      abort();
    if (r3 >= r4)
      abort();
    if (r3 == r4)
      abort();
    if (! (r3 != r4))
      abort();
    if (r4 < r3)
      abort();
    if (r4 <= r3)
      abort();
  }

  r3 = make_ratio(3,5);
  r4 = make_ratio(4,5);
  
  // test >, >=, ==, !=, <, and <= when r3.den == r4.den
  {
    if (r3 > r4)
      abort();
    if (r3 >= r4)
      abort();
    if (r3 == r4)
      abort();
    if (! (r3 != r4))
      abort();
    if (r4 < r3)
      abort();
    if (r4 <= r3)
      abort();
  }

  r3 = make_ratio(3,7);
  r4 = make_ratio(3,5);
  
  // test >, >=, ==, !=, <, and <= when r3.num == r4.num
  {
    if (r3 > r4)
      abort();
    if (r3 >= r4)
      abort();
    if (r3 == r4)
      abort();
    if (! (r3 != r4))
      abort();
    if (r4 < r3)
      abort();
    if (r4 <= r3)
      abort();
  }

  r3 = make_ratio(3,7);
  r4 = make_ratio(3,7);
  
  // test >, >=, ==, !=, <, and <= when r3.num == r4.num and r3.den == r3.den
  {
    if (r3 > r4)
      abort();
    if (! (r3 >= r4))
      abort();
    if (! (r3 == r4))
      abort();
    if (r3 != r4)
      abort();
    if (r4 < r3)
      abort();
    if (! (r4 <= r3))
      abort();
  }

  printf("Success\n");
  
  return 0;
}

#endif
