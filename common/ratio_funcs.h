#ifndef _RADIUM_COMMON_RATIO_FUNCS_H
#define _RADIUM_COMMON_RATIO_FUNCS_H

#include <math.h>

#include "placement_proc.h"

#if !__MINGW32__
typedef struct {
  __int128_t num;
  __int128_t den;
} Ratio128;


static inline Ratio128 make_ratio128(Ratio ratio){
  Ratio128 ratio128 = {ratio.num, ratio.den};
  return ratio128;
}
#endif

static inline Ratio make_ratio_from_double(double val){

  R_ASSERT_NON_RELEASE(false);
  
  int64_t den = (2L << 60L) / (1 + (int64_t)val);
  int64_t num = round((double)den * val);

  if (den == 0){
    den = 1;
#if !defined(RELEASE)
    abort();
#endif
  } else if (den < 0){
    num = -num;
    den = -den;
  }

  return make_ratio(den, num);
}

static inline double make_double_from_ratio(Ratio r){
  return (double)r.num / (double)r.den;
}

// the ov_ macros are copied from the s7 source (macros are slightly renamed)
#if (defined(__clang__) && ((__clang_major__ > 3) || (__clang_major__ == 3 && __clang_minor__ >= 4)))
#ifdef __cplusplus
  static_assert(sizeof(void*) == 8, "sizeof(void*) is not 8");
#endif
  #define ov_sub(A, B, C)       __builtin_ssubll_overflow((long long)A, (long long)B, (long long *)C)
  #define ov_add(A, B, C)       __builtin_saddll_overflow((long long)A, (long long)B, (long long *)C)
  #define ov_mul(A, B, C)       __builtin_smulll_overflow((long long)A, (long long)B, (long long *)C)
#elif (defined(__GNUC__) && __GNUC__ >= 5)
  #define ov_sub(A, B, C)       __builtin_sub_overflow(A, B, C)
  #define ov_add(A, B, C)       __builtin_add_overflow(A, B, C)
  #define ov_mul(A, B, C)       __builtin_mul_overflow(A, B, C)
#endif


static inline Ratio make_ratio_from_place(const Place p){
  R_ASSERT_NON_RELEASE(p.dividor > 0);

  int64_t num;
  
  if (ov_mul(p.line, p.dividor, &num)){
    R_ASSERT_NON_RELEASE(false); // should be impossible
  }
  
  if (ov_add(num, p.counter, &num)){
    R_ASSERT_NON_RELEASE(false); // should be impossible
  }
  
  return make_ratio(num, p.dividor);
}

#ifndef CUSTOM_R_ASSERT
#ifdef COMMON_PLACEMENT_PROC_H
static inline Place make_place_from_ratio(const Ratio ratio){
  R_ASSERT_NON_RELEASE(ratio.den > 0);
  Place place;
  
  place.line = ratio.num / ratio.den;

  if (place.line < 0){
#if !defined(RELEASE)
    abort();
#endif
    place.line = 0;
    place.counter = 0;
    place.dividor = 1;
    return place;
  }
  
  int64_t num;
  if (ov_mul(place.line, ratio.den, &num))
    goto overflow_handler;
  
  if (ov_sub(ratio.num, num, &num))
    goto overflow_handler;

  {
    Ratio r2 = RATIO_minimize(make_ratio(num, ratio.den));
    
    if (r2.den <= MAX_UINT32) {
      
      place.counter = r2.num;
      place.dividor = r2.den;
      
    } else {
      
      place.counter = scale_int64(MAX_UINT32,
                                  0, r2.den,
                                  0, r2.num);
      
      place.dividor = MAX_UINT32;    
    }

    return place;
  }
  
 overflow_handler:
  {
#if !defined(RELEASE)
    abort();
#endif
    double r = (double)ratio.num / (double)ratio.den;
    return p_FromDouble(r);
  }
}
#endif
#endif

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

  if (r1.den == r2.den)
    return r1.num > r2.num;

  
  int64_t a,b;
  
  if (ov_mul(r1.num, r2.den, &a))
    goto overflow_fallback;
  
  if (ov_mul(r2.num, r1.den, &b))
    goto overflow_fallback;

  return a > b;

  
 overflow_fallback:

  R_ASSERT_NON_RELEASE(false);

#if !__MINGW32__
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

  return !RATIO_greater_than(r2, r1);
}

static inline bool RATIO_greater_or_equal_than(const Ratio r1, const Ratio r2){
  return RATIO_equal(r1, r2) || RATIO_greater_than(r1, r2);
}

static inline bool RATIO_less_or_equal_than(const Ratio r1, const Ratio r2){
  return !RATIO_greater_than(r2, r1);
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

static inline Ratio operator-(const Ratio &r){
  Ratio ret = r;
  ret.num = -ret.num;
  return ret;              
}

static inline bool operator==(const Ratio &r1, const Ratio &r2){
  return RATIO_equal(r1, r2);
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

#endif
                


#endif
