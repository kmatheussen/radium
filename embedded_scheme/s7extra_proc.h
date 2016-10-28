#ifdef __cplusplus
extern "C" {
#endif

  #ifdef S7_VERSION
  bool s7extra_is_place(s7_pointer place);
  Place s7extra_place(s7_scheme *s7, s7_pointer place);
  func_t *s7extra_func(s7_scheme *s7, s7_pointer func);  
  s7_pointer s7extra_make_place(s7_scheme *radiums7_sc, Place place);
  #endif

  void s7extra_callFunc_void_int_bool(func_t *func, int64_t arg1, bool arg2);
  void s7extra_callFunc2_void_int_bool(const char *funcname, int64_t arg1, bool arg2);
  
  void s7extra_callFunc_void_int(func_t *func, int64_t arg1);
  void s7extra_callFunc2_void_int(const char *funcname, int64_t arg1);

  void s7extra_callFunc_void_charpointer(func_t *func, int64_t arg1, const char* arg2);
  void s7extra_callFunc2_void_int_charpointer(const char *funcname, int64_t arg1, const char* arg2);

#ifdef __cplusplus
}
#endif

