#ifdef __cplusplus
extern "C" {
#endif


  #ifdef S7_VERSION
  void init_radium_s7(s7_scheme *s7);
  
  bool s7extra_is_place(s7_pointer place);
  Place s7extra_place(s7_scheme *s7, s7_pointer place);
  s7_pointer s7extra_make_place(s7_scheme *radiums7_sc, Place place);

  bool s7extra_is_dyn(s7_pointer dyn);
  dyn_t s7extra_dyn(s7_scheme *s7, s7_pointer s);
  s7_pointer s7extra_make_dyn(s7_scheme *radiums7_sc, const dyn_t dyn);

  func_t *s7extra_func(s7_scheme *s7, s7_pointer func);

  int64_t s7extra_get_integer(s7_scheme *s7, s7_pointer s, const char **error);
  float s7extra_get_float(s7_scheme *s7, s7_pointer s, const char **error);
  double s7extra_get_double(s7_scheme *s7, s7_pointer s, const char **error);
  const char *s7extra_get_string(s7_scheme *s7, s7_pointer s, const char **error);
  bool s7extra_get_boolean(s7_scheme *s7, s7_pointer s, const char **error);
  Place s7extra_get_place(s7_scheme *s7, s7_pointer s, const char **error);
  func_t *s7extra_get_func(s7_scheme *s7, s7_pointer func, const char **error);
  dyn_t s7extra_get_dyn(s7_scheme *s7, s7_pointer s, const char **error);

  #endif

  extern int g_scheme_nested_level; // If this number > 0, then some scheme code is currently running. (i.e. we are inside s7_eval)
  extern bool g_scheme_failed;
  extern bool g_is_going_to_call_throwExceptionIfError; // If true, it means that 'handleError' doesn't have to display backtrace directly since throwExceptionIfError is soon going to be called and throw exception at a safe moment if necessary. It's better to throw exception since we stop running scheme code, and there's also (currently) less noise in the backtrace when throwing.

#define S7CALL_NO_HISTORY(Type,Func,...)                                \
  do{                                                                   \
    s7extra_disable_history();                                          \
    s7extra_callFunc_ ## Type (Func,##__VA_ARGS__);                     \
    s7extra_enable_history();                                           \
  }while(0)

#define S7CALL2_NO_HISTORY(Type,Func,...)                                \
  do{                                                                   \
    s7extra_disable_history();                                          \
    s7extra_callFunc2_ ## Type (Func,##__VA_ARGS__);                     \
    s7extra_enable_history();                                           \
  }while(0)

#define S7CALL(Type,Func,...)                                           \
  (s7extra_add_history(__func__, CR_FORMATEVENT("========== s7call_" # Type, "\n\n")), \
   s7extra_callFunc_ ## Type (Func,##__VA_ARGS__))
  
#define S7CALL2(Type,Funcname,...)                                      \
  (s7extra_add_history(__func__, CR_FORMATEVENT("========== s7call_" # Type, "\n\n")), \
   s7extra_callFunc2_ ## Type (Funcname,##__VA_ARGS__))
    
  void s7extra_add_history(const char *funcname, const char *info);

  void s7extra_callFunc_void_void(const func_t *func);
  void s7extra_callFunc2_void_void(const char *funcname);

  double s7extra_callFunc_double_void(const func_t *func);
  double s7extra_callFunc2_double_void(const char *funcname);

  bool s7extra_callFunc_bool_void(const func_t *func);
  bool s7extra_callFunc2_bool_void(const char *funcname);

  dyn_t s7extra_callFunc_dyn_void(const func_t *func);
  dyn_t s7extra_callFunc2_dyn_void(const char *funcname);

  dyn_t s7extra_callFunc_dyn_charpointer(const func_t *func, const char *arg1);
  dyn_t s7extra_callFunc2_dyn_charpointer(const char *funcname, const char *arg1);

  dyn_t s7extra_callFunc_dyn_int(const func_t *func, int64_t arg1);
  dyn_t s7extra_callFunc2_dyn_int(const char *funcname, int64_t arg1);

  dyn_t s7extra_callFunc_dyn_int_int_int(const func_t *func, int64_t arg1, int64_t arg2, int64_t arg3);
  dyn_t s7extra_callFunc2_dyn_int_int_int(const char *funcname, int64_t arg1, int64_t arg2, int64_t arg3);

  dyn_t s7extra_callFunc_dyn_int_int_int_dyn_dyn_dyn(const func_t *func, int64_t arg1, int64_t arg2, int64_t arg3, const dyn_t arg4, const dyn_t arg5, const dyn_t arg6);
  dyn_t s7extra_callFunc2_dyn_int_int_int_dyn_dyn_dyn(const char *funcname, int64_t arg1, int64_t arg2, int64_t arg3, const dyn_t arg4, const dyn_t arg5, const dyn_t arg6);

  dyn_t s7extra_callFunc_dyn_int_int_int_dyn_dyn_dyn_dyn_dyn(const func_t *func, int64_t arg1, int64_t arg2, int64_t arg3, const dyn_t arg4, const dyn_t arg5, const dyn_t arg6, const dyn_t arg7, const dyn_t arg8);
  dyn_t s7extra_callFunc2_dyn_int_int_int_dyn_dyn_dyn_dyn_dyn(const char *funcname, int64_t arg1, int64_t arg2, int64_t arg3, const dyn_t arg4, const dyn_t arg5, const dyn_t arg6, const dyn_t arg7, const dyn_t arg8);

  dyn_t s7extra_callFunc_dyn_dyn_dyn_dyn_int(const func_t *func, const dyn_t arg1, const dyn_t arg2, const dyn_t arg3, int64_t arg4);
  dyn_t s7extra_callFunc2_dyn_dyn_dyn_dyn_int(const char *funcname, const dyn_t arg1, const dyn_t arg2, const dyn_t arg3, int64_t arg4);

  void s7extra_callFunc_void_int_charpointer_dyn(const func_t *func, int64_t arg1, const char* arg2, const dyn_t arg3);
  void s7extra_callFunc2_void_int_charpointer_dyn(const char *funcname, int64_t arg1, const char* arg2, const dyn_t arg3);

  void s7extra_callFunc_void_int_charpointer_int(const func_t *func, int64_t arg1, const char* arg2, int64_t arg3);
  void s7extra_callFunc2_void_int_charpointer_int(const char *funcname, int64_t arg1, const char* arg2, int64_t arg3);

  void s7extra_callFunc_void_int_bool(const func_t *func, int64_t arg1, bool arg2);
  void s7extra_callFunc2_void_int_bool(const char *funcname, int64_t arg1, bool arg2);
  
  void s7extra_callFunc_void_int(const func_t *func, int64_t arg1);
  void s7extra_callFunc2_void_int(const char *funcname, int64_t arg1);

  void s7extra_callFunc_void_double(const func_t *func, double arg1);
  void s7extra_callFunc2_void_double(const char *funcname, double arg1);

  void s7extra_callFunc2_void_bool(const char *funcname, bool arg1);
  void s7extra_callFunc_void_bool(const func_t *func, bool arg1);

  void s7extra_callFunc_void_dyn(const func_t *func, const dyn_t arg1);
  void s7extra_callFunc2_void_dyn(const char *funcname, const dyn_t arg1);

  void s7extra_callFunc_void_dyn_dyn(const func_t *func, const dyn_t arg1, const dyn_t arg2);
  void s7extra_callFunc2_void_dyn_dyn(const char *funcname, const dyn_t arg1, const dyn_t arg2);

  void s7extra_callFunc_void_charpointer(const func_t *func, const char* arg1);
  void s7extra_callFunc2_void_charpointer(const char *funcname, const char* arg1);

  void s7extra_callFunc_void_int_charpointer(const func_t *func, int64_t arg1, const char* arg2);
  void s7extra_callFunc2_void_int_charpointer(const char *funcname, int64_t arg1, const char* arg2);

  bool s7extra_callFunc_bool_int(const func_t *func, int64_t arg1);
  bool s7extra_callFunc2_bool_int(const char *funcname, int64_t arg1);

  bool s7extra_callFunc_bool_int_charpointer(const func_t *func, int64_t arg1, const char* arg2);
  bool s7extra_callFunc2_bool_int_charpointer(const char *funcname, int64_t arg1, const char* arg2);

  void s7extra_callFunc_void_int_charpointer_bool_bool(const func_t *func, int64_t arg1, const char* arg2, bool arg3, bool arg4);
  void s7extra_callFunc2_void_int_charpointer_bool_bool(const char *funcname, int64_t arg1, const char* arg2, bool arg3, bool arg4);

  void s7extra_callFunc_void_int_int(const func_t *func, int64_t arg1, int64_t arg2);
  void s7extra_callFunc2_void_int_int(const char *funcname, int64_t arg1, int64_t arg2);

  void s7extra_callFunc_void_int_int_int(const func_t *func, int64_t arg1, int64_t arg2, int64_t arg3);
  void s7extra_callFunc2_void_int_int_int(const char *funcname, int64_t arg1, int64_t arg2, int64_t arg3);

  void s7extra_callFunc_void_int_dyn(const func_t *func, int64_t arg1, const dyn_t arg2);
  void s7extra_callFunc2_void_int_dyn(const char *funcname, int64_t arg1, const dyn_t arg2);

  void s7extra_callFunc_void_int_float_float(const func_t *func, int64_t arg1, float arg2, float arg3);
  void s7extra_callFunc2_void_int_float_float(const char *funcname, int64_t arg1, float arg2, float arg3);

  void s7extra_callFunc_void_int_int_float_float(const func_t *func, int64_t arg1, int64_t arg2, float arg3, float arg4);
  void s7extra_callFunc2_void_int_int_float_float(const char *funcname, int64_t arg1, int64_t arg2, float arg3, float arg4);

  bool s7extra_callFunc_bool_int_int_float_float(const func_t *func, int64_t arg1, int64_t arg2, float arg3, float arg4);
  bool s7extra_callFunc2_bool_int_int_float_float(const char *funcname, int64_t arg1, int64_t arg2, float arg3, float arg4);

  bool s7extra_callFunc_bool_int_float_float(const func_t *func, int64_t arg1, float arg2, float arg3);
  bool s7extra_callFunc2_bool_int_float_float(const char *funcname, int64_t arg1, float arg2, float arg3);

  bool s7extra_callFunc_bool_bool(const func_t *func, bool arg1);
  bool s7extra_callFunc2_bool_bool(const char *funcname, bool arg1);

  int64_t s7extra_callFunc_int_void(const func_t *func);
  int64_t s7extra_callFunc2_int_void(const char *funcname);

  int64_t s7extra_callFunc_int_int(const func_t *func, int64_t arg1);
  int64_t s7extra_callFunc2_int_int(const char *funcname, int64_t arg1);

  int64_t s7extra_callFunc_int_int_int_int(const func_t *func, int64_t arg1, int64_t arg2, int64_t arg3);
  int64_t s7extra_callFunc2_int_int_int_int(const char *funcname, int64_t arg1, int64_t arg2, int64_t arg3);

  int64_t s7extra_callFunc_int_int_int_int_bool(const func_t *func, int64_t arg1, int64_t arg2, int64_t arg3, bool arg4);
  int64_t s7extra_callFunc2_int_int_int_int_bool(const char *funcname, int64_t arg1, int64_t arg2, int64_t arg3, bool arg4);

  int64_t s7extra_callFunc_int_int_dyn(const func_t *func, int64_t arg1, const dyn_t arg2);
  int64_t s7extra_callFunc2_int_int_dyn(const char *funcname, int64_t arg1, const dyn_t arg2);

  int64_t s7extra_callFunc_int_charpointer_charpointer(const func_t *func, const char *arg1, const char *arg2);
  int64_t s7extra_callFunc2_int_charpointer_charpointer(const char *funcname, const char *arg1, const char *arg2);

  int64_t s7extra_callFunc_int_charpointer_charpointer_bool_bool_bool_bool_bool(const func_t *func, const char *arg1, const char *arg2, bool arg3, bool arg4, bool arg5, bool arg6, bool arg7);
  int64_t s7extra_callFunc2_int_charpointer_charpointer_bool_bool_bool_bool_bool(const char *funcname, const char *arg1, const char *arg2, bool arg3, bool arg4, bool arg5, bool arg6, bool arg7);

  const char *s7extra_callFunc_charpointer_dyn(const func_t *func, const dyn_t arg1);
  const char *s7extra_callFunc2_charpointer_dyn(const char *funcname, const dyn_t arg1);

  void s7extra_callFunc_void_charpointer(const func_t *func, const char* arg1);

  func_t *s7extra_get_func_from_funcname_for_storing(const char *funcname); // Must be used when storing the pointer. (gc-protected) Must NOT be used if the function is called again and again. (leaks memory)
  func_t *s7extra_get_func_from_funcname(const char *funcname); // Must NOT be used if the pointer is stored.
  
  void s7extra_protect(void *v);
  void s7extra_unprotect(void *v);

  void s7extra_disable_history(void);  
  void s7extra_enable_history(void);
  
  bool s7extra_is_defined(const char* funcname);
#ifdef __cplusplus
}
#endif

