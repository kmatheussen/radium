#pragma once


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
#else
#error "missing definitions for sub_overflow/add_overflow/mul_overflow"
#endif

