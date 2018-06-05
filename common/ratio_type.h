#ifndef _RADIUM_COMMON_RATIO_TYPE_H
#define _RADIUM_COMMON_RATIO_TYPE_H

typedef struct {
  int64_t numerator;
  int64_t denominator;
} Ratio;

static inline Ratio make_ratio(int64_t numerator, int64_t denominator) {
#if !defined(RELEASE)
#if defined(R_ASSERT)
  if (numerator!=0 && denominator!=0){
    R_ASSERT(numerator>=0);
    R_ASSERT(denominator>0);
  }
#endif
#endif

  Ratio ratio = {numerator, denominator};
  return ratio;
}

// Function copied from s7 scheme. (c_gcd)
static inline int64_t ratio_gcd(int64_t u, int64_t v){
#if defined(R_ASSERT_RETURN_IF_FALSE2)
  R_ASSERT_RETURN_IF_FALSE2(v>0, 1);
#endif
  
  int64_t a, b;
  a = u;
  b = v;
  while (b != 0)
    {
      int64_t temp;
      temp = a % b;
      a = b;
      b = temp;
    }
  return(a);
}

static inline Ratio RATIO_minimize(const Ratio a){
  int64_t gcd_a = ratio_gcd(a.numerator, a.denominator);
  return make_ratio(a.numerator/gcd_a, a.denominator/gcd_a);
}

/*
// Using RATIO_equal below instead. We probably don't need to worry about integer overflows.
static inline bool ratio_equal(const Ratio a, const Ratio b){
  if (a.numerator==b.numerator && a.denominator==b.denominator)
    return true;

  Ratio a2 = ratio_minimize(a);
  Ratio b2 = ratio_minimize(b);

  return a2.numerator==b2.numerator && a2.denominator==b2.denominator;
}
*/

static inline bool RATIO_equal(const Ratio r1, const Ratio r2){
  return r1.numerator*r2.denominator == r2.numerator*r1.denominator;
}

static inline bool RATIO_literary_equal(const Ratio r1, const Ratio r2){
  return r1.numerator==r2.numerator && r1.denominator==r2.denominator;
}

static inline Ratio RATIO_multiply(const Ratio r1, const Ratio r2){
  return make_ratio(r1.numerator*r2.numerator, r1.denominator*r2.denominator);
}

static inline Ratio RATIO_divide(const Ratio r1, const Ratio r2){
  return make_ratio(r1.numerator*r2.denominator, r1.denominator*r2.numerator);
}

#ifdef __cplusplus
extern "C"{
#endif
  
extern wchar_t *RATIO_as_string(const Ratio ratio);
extern Ratio RATIO_from_string(const wchar_t *wstring);

#ifdef __cplusplus
}
#endif

#if defined(USE_QT4) && defined(QSTRING_H)
extern QString RATIO_as_qstring(const Ratio ratio);
#endif
  

  
#endif
