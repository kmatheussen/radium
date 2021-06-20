#ifndef _RADIUM_COMMON_RATIO_TYPE_H
#define _RADIUM_COMMON_RATIO_TYPE_H

typedef struct {
  int64_t num;
  int64_t den;
} Ratio;

#ifdef __cplusplus
namespace r{
#if 0
  struct Ratio{
    ::Ratio r;
    Ratio(::Ratio r)
    : r(r)
    {}
    Ratio(int64_t num, int64_t den){
      r = {num, den};
    }
  };
#endif
}
#endif

// The difference between Ratio and StaticRatio is that numerator and denominoator of StaticRatio are not divided by gcd. StaticRatio is used in quantization, time signatures, bar/beat, and maybe other things.
// Denominoator of StaticRatio can be 0.
// StaticRatio can not be used in the arithmetic operations since those assume num and dem are always divided by gcd, plus that a StaticRatio doesn't have to be a legal number (i.e. the denominator of StaticRatio can be 0).
// To be used in arithmetic operations, StaticRatios must first be converted to Ratio by using the make_ratio_from_static_ratio function.
typedef struct {
  int numerator;
  int denominator;
} StaticRatio;


#if !defined(R_ASSERT_NON_RELEASE)
#  define CUSTOM_R_ASSERT 1
#  define R_ASSERT_NON_RELEASE(a)
#endif


// Function copied from s7 scheme. (c_gcd)
static inline int64_t ratio_gcd(int64_t u, int64_t v){
#if defined(R_ASSERT_RETURN_IF_FALSE2)
  R_ASSERT_RETURN_IF_FALSE2(v>0, 1);
#endif
  
  int64_t a = u;
  int64_t b = v;
  
  if (a < 0)
    a = -a;
  if (b < 0)
    b = -b;
  
  while (b != 0)
    {
      //printf("a: %" PRId64 ". b: %" PRId64 "\n",a, b);
      int64_t temp;
      temp = a % b;
      a = b;
      b = temp;
    }

  return a;
}

static inline Ratio RATIO_minimize(const Ratio a){
  R_ASSERT_NON_RELEASE(a.den > 0L);
  int64_t gcd_a = ratio_gcd(a.num, a.den);
  if (gcd_a==1)
    return a;
  
  Ratio ratio = {a.num/gcd_a, a.den/gcd_a};
  
  R_ASSERT_NON_RELEASE(ratio.den > 0L);
  
  return ratio;
}

static inline Ratio make_ratio(int64_t num, int64_t den) {
#if !defined(RELEASE)
#if defined(R_ASSERT)
  /*
  if (num!=0 && den!=0){
    R_ASSERT(num>=0);
    R_ASSERT(den>0);
  }
  */
  R_ASSERT(den > 0L);
#endif
#endif
  
  Ratio ratio = {num, den};
  return RATIO_minimize(ratio);
}


static inline StaticRatio make_empty_static_ratio(void) {
  StaticRatio ratio = {};
  return ratio;
}
  
static inline StaticRatio make_static_ratio(int num, int den) {
#if !defined(RELEASE)
#if defined(R_ASSERT)
  /*
  if (num!=0 && den!=0){
    R_ASSERT(num>=0);
    R_ASSERT(den>0);
  }
  */
  R_ASSERT(den > 0L);
#endif
#endif

  StaticRatio ratio = {num, den};
  return ratio;
}

static inline StaticRatio make_static_ratio_from_ratio(const Ratio ratio){
  return make_static_ratio(ratio.num, ratio.den);
}
                             
static inline Ratio make_ratio_from_static_ratio(const StaticRatio ratio){
  return make_ratio(ratio.numerator, ratio.denominator);
}

// intratio is a ratio stored in a single integer. Only works if either numerator or denominator is 1.
static inline int RATIO_get_intratio(const Ratio ratio){
  if(ratio.num==1)
    return -ratio.den;
  else
    return ratio.num;
}

static inline Ratio RATIO_create_from_intratio(int intratio){
  if(intratio < 0)
    return make_ratio(1, -intratio);
  else
    return make_ratio(intratio, 1);
}

/*
// Using RATIO_equal below instead. We probably don't need to worry about integer overflows.
static inline bool ratio_equal(const Ratio a, const Ratio b){
  if (a.num==b.num && a.den==b.den)
    return true;

  Ratio a2 = ratio_minimize(a);
  Ratio b2 = ratio_minimize(b);

  return a2.num==b2.num && a2.den==b2.den;
}
*/


static inline bool STATIC_RATIO_literary_equal(const StaticRatio r1, const StaticRatio r2){
  return r1.numerator==r2.numerator && r1.denominator==r2.denominator;
}


static inline bool RATIO_literary_equal(const Ratio r1, const Ratio r2){
  R_ASSERT_NON_RELEASE(r1.den > 0L);
  R_ASSERT_NON_RELEASE(r2.den > 0L);
  return r1.num==r2.num && r1.den==r2.den;
}


static inline bool RATIO_equal(const Ratio r1, const Ratio r2){
  return RATIO_literary_equal(r1, r2); // since we run gcd every time a Ratio is created.
  /*
  R_ASSERT_NON_RELEASE(r1.den > 0L);
  R_ASSERT_NON_RELEASE(r2.den > 0L);

  int64_t a,b;
  if (ov_mul(r1.num, r2.den, &a)==false)
    goto overflow_handler;

  if (ov_mul(r2.num, r1.den, &a)==false)
    goto overflow_handler;
      
  return a == b;
  */
}



#ifdef __cplusplus
extern "C"{
#endif
  
extern const wchar_t *STATIC_RATIO_as_string(const StaticRatio ratio);
extern StaticRatio STATIC_RATIO_from_string(const wchar_t *wstring);

#ifdef __cplusplus
}
#endif

#if defined(USE_QT4) && defined(QSTRING_H)
extern QString STATIC_RATIO_as_qstring(const StaticRatio ratio);
#endif
  


#if CUSTOM_R_ASSERT
#  undef R_ASSERT_NON_RELEASE
#endif



#endif

