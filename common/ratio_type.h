#ifndef _RADIUM_COMMON_RATIO_TYPE_H
#define _RADIUM_COMMON_RATIO_TYPE_H

typedef struct {
  int64_t numerator;
  int64_t denominator;
} Ratio;

static inline bool RATIO_equal(Ratio r1, Ratio r2){
  return r1.numerator*r2.denominator == r2.numerator*r1.denominator;
}

static inline bool RATIO_literary_equal(Ratio r1, Ratio r2){
  return r1.numerator==r2.numerator && r1.denominator==r2.denominator;
}

#ifdef __cplusplus
extern "C"{
#endif
  
extern wchar_t *RATIO_as_string(Ratio ratio);
extern Ratio RATIO_from_string(const wchar_t *wstring);

#ifdef __cplusplus
}
#endif

#if defined(USE_QT4) && defined(QSTRING_H)
extern QString RATIO_as_qstring(Ratio ratio);
#endif
  

  
#endif
