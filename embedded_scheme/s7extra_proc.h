#ifdef __cplusplus
extern "C" {
#endif

  #ifdef S7_VERSION
  bool s7extra_is_place(s7_pointer place);
  Place s7extra_place(s7_scheme *s7, s7_pointer place);
  func_t *s7extra_func(s7_scheme *s7, s7_pointer func);  
  s7_pointer s7extra_make_place(s7_scheme *radiums7_sc, Place place);
  #endif

  void s7extra_callFunc_void_int_bool(func_t *func, int arg1, bool arg2);
  
#ifdef __cplusplus
}
#endif

