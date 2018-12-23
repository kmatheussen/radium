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

  bool s7extra_callFunc_bool_bool_float_float(const func_t *func, bool arg1, float arg2, float arg3);
  bool s7extra_callFunc2_bool_bool_float_float(const char *funcname, bool arg1, float arg2, float arg3);

  bool s7extra_callFunc_bool_bool_dyn(const func_t *func, bool arg1, dyn_t arg2);
  bool s7extra_callFunc2_bool_bool_dyn(const char *funcname, bool arg1, dyn_t arg2);

  bool s7extra_callFunc_bool_bool_charpointer_charpointer_dyn(const func_t *func, bool arg1, const char *arg2, const char *arg3, dyn_t arg4);
  bool s7extra_callFunc2_bool_bool_charpointer_charpointer_dyn(const char *funcname, bool arg1, const char *arg2, const char *arg3, dyn_t arg4);

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

  int64_t s7extra_callFunc_int_int_charpointer_charpointer_bool_bool_bool_bool_bool(const func_t *func, int64_t arg0, const char *arg1, const char *arg2, bool arg3, bool arg4, bool arg5, bool arg6, bool arg7);
  int64_t s7extra_callFunc2_int_int_charpointer_charpointer_bool_bool_bool_bool_bool(const char *funcname, int64_t arg0, const char *arg1, const char *arg2, bool arg3, bool arg4, bool arg5, bool arg6, bool arg7);

  const char *s7extra_callFunc_charpointer_dyn(const func_t *func, const dyn_t arg1);
  const char *s7extra_callFunc2_charpointer_dyn(const char *funcname, const dyn_t arg1);

  void s7extra_callFunc_void_charpointer(const func_t *func, const char* arg1);

  func_t *s7extra_get_func_from_funcname_for_storing(const char *funcname); // Must be used when storing the pointer. (gc-protected) Must NOT be used if the function is called again and again. (leaks memory)
  func_t *s7extra_get_func_from_funcname(const char *funcname); // Must NOT be used if the pointer is stored.
  
  int64_t s7extra_protect(void *v) __attribute__((warn_unused_result)); // returns pos
  //void s7extra_unprotect(void *v); // Slower than s7extra_unprotect2, but don't have to remember pos. (dangerous)
  void s7extra_unprotect(void *v, int64_t pos); // Might use this one instead of s7extra_unprotect if unprotecting a lot of objects.

  void s7extra_disable_history(void);  
  void s7extra_enable_history(void);
  
  bool s7extra_is_defined(const char* funcname);
  
#ifdef __cplusplus
}


namespace radium{

  template <typename T> struct ProtectedS7Extra{
    T v;
    int64_t _pos;

#if !defined(RELEASE)
    bool _do_unprotect = true;
#endif

    ProtectedS7Extra(const ProtectedS7Extra &another)
      : ProtectedS7Extra(another.v)
    {
    }
    
    ProtectedS7Extra& operator=(const ProtectedS7Extra&) = delete;

    ProtectedS7Extra(T val)
    {
      protect(val);
    }
    
    ProtectedS7Extra()
      : ProtectedS7Extra(NULL)
    {
    }
    
    ~ProtectedS7Extra(){
      unprotect();
    }

  private:

    void protect(T val){
      v = val;
      
      if (v==NULL)
        _pos = -1;
      else
        _pos = s7extra_protect(v);
    }
    
    void unprotect(void){
#if !defined(RELEASE)
      if(_do_unprotect==true){
#endif
        if (v != NULL){
          R_ASSERT_NON_RELEASE(_pos >= 0);
          s7extra_unprotect(v, _pos);
          v = NULL;
          _pos = -1;
        }else{
          R_ASSERT_NON_RELEASE(_pos == -1);
        }
#if !defined(RELEASE)
      }
#endif
    }
    
  public:
    
    
    void set(T new_val){
      unprotect();
      protect(new_val);
    }
    
    // Or just use ".v".
    T get(void){
      return v;
    }
  };

#if defined(QVECTOR_H) && defined(QSET_H)

  struct ProtectedS7FuncVector{
    
  private:
    QVector<radium::ProtectedS7Extra<func_t*>> _elements;
    
    int64_t _generation = 0;
    bool _changes_allowed = true;

  public:

#if !defined(RELEASE)
    bool _do_unprotect = true;
#endif

    bool _only_unique_elements;
    
    ProtectedS7FuncVector(const ProtectedS7FuncVector&) = delete;
    ProtectedS7FuncVector& operator=(const ProtectedS7FuncVector&) = delete;

    ProtectedS7FuncVector(bool only_unique_elements)
      : _only_unique_elements(only_unique_elements)
    {
    }
    
    /*
    // use safe_for_all instead.
    const func_t* begin() const {
      return _elements.begin();
    }

    // This function can be called in parallell with the other const functions (i.e. the non-mutating ones).
    const func_t* end() const {      
      return _elements.end();
    }
    */
    
    // Very safe function to iterate all elements. The callback is allowed to both add and remove elements.
    // If allow_changes==false, assertion will be thrown if the 'for_all_callback' tries to change the vector.
    void safe_for_all(bool allow_changes, std::function<bool(func_t*)> for_all_callback){
      bool old_allowed = _changes_allowed;
      
      if (old_allowed==true && allow_changes == false)
        _changes_allowed = false;
      
      {
        QSet<const func_t*> called_funcs;
        
        int64_t generation;
        
        do{
          
          generation = _generation;
          
          int i = 0;
          
          while(i < _elements.size()){
            
            func_t *callback = _elements.at(i).v;
            
            if (called_funcs.contains(callback)==false){
              
              called_funcs << callback;
              
              if (for_all_callback(callback)==false)
                goto finished;
            }
            
            i++;
          }
          
        }while(generation != _generation);
        
      }
      
    finished:
      _changes_allowed = old_allowed;
    }
      
    int find_pos(func_t *callback) const {
      int i = 0;
      for(const auto &callback2 : _elements){
        if (callback2.v == callback)
          return i;
        i++;
      }
      
      return -1;
    }

    int removeAll(func_t *callback){
      if (_changes_allowed==false){
        R_ASSERT(false);
        return 0;
      }
      
      int num_removed = 0;

      {
      again:
        int i = 0;
        for(auto &callback2 : _elements){
          if(callback2.v == callback){
            _generation++;
            _elements.removeAt(i);
            num_removed++;
#if defined(RELEASE)
            if (_only_unique_elements)              
              return 1;
#endif
            goto again; // It's not documented how QVector might change order after removing an element, so we just start again from the beginning.
          }
          i++;
        }
      }

#if !defined(RELEASE)
      R_ASSERT(num_removed==0 || num_removed==1);
#endif
      
      return num_removed;
    }

    void clear(void){
      if (_changes_allowed==false){
        R_ASSERT(false);
        return;
      }
      
      _elements.clear();
    }
    
    bool removeOne(func_t *callback){
      if (_changes_allowed==false){
        R_ASSERT(false);
        return false;
      }
      
      int pos = find_pos(callback);
      if (pos == -1)
        return false;

      _generation++;
      _elements.removeAt(pos);
      
      return true;
    }
    
    void removeAt(int pos){
      if (_changes_allowed==false){
        R_ASSERT(false);
        return;
      }
      
      R_ASSERT_RETURN_IF_FALSE(pos >= 0);
      R_ASSERT_RETURN_IF_FALSE(pos < _elements.size());

      _generation++;
      _elements.removeAt(pos);
    }
    
    bool contains(func_t *callback) const{
      return find_pos(callback) >= 0;
    }
    
    bool push_back(func_t *callback){
      if (_changes_allowed==false){
        R_ASSERT(false);
        return false;
      }

      if (_only_unique_elements && contains(callback))
        return false;

      _generation++;
      _elements.push_back(ProtectedS7Extra<func_t*>(callback));
      
#if !defined(RELEASE)
      if (_do_unprotect==false)
        _elements.last()._do_unprotect = false;
#endif

      return true;
    }

  };

#endif // QVECTOR_H
  
}

#endif

