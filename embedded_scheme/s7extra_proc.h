#ifdef __cplusplus
extern "C" {
#endif
  
  bool s7extra_is_place(s7_pointer place);
  Place s7extra_place(s7_scheme *s7, s7_pointer place);
  s7_pointer s7extra_make_place(s7_scheme *radiums7_sc, Place place);

#ifdef __cplusplus
}
#endif

