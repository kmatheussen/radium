#!/bin/sh
# -*- scheme -*-
exec guile -e main -s $0 $*
!#

(use-modules (ice-9 format))


(define *radium-s7-types*
  '((instrument int64_t)
    (file int64_t)
    (filepath "const wchar_t*")))



(define (<-> . args)
  (apply string-append (map (lambda (a)
                              (format #f "~A" a))
                            args)))

(define (c-display . args)
  (apply display args)
  (newline))

(define (get-s7-get-value-from-s7_pointer-code c-type var)
  (if (eq? c-type 'int64_t)
      (<-> "s7_integer(" var ")")
      (error (<-> "unknown type " c-type))))

(define (get-s7-types-code type c-type)
  (<-> "extern int g_" type "_type_tag;\n"
       "typedef struct { " c-type " id; } " type "_t;\n"
       (if (eq? type 'filepath)
           (<-> "extern const wchar_t* g_illegal_filepath_string;\n"
                "static inline " type "_t make_" type "(" c-type " id){\n"
                "#if !defined(RELEASE)\n"
                "  if (id==NULL) abort();\n"
                "#endif\n"
                "  " type "_t ret = {.id=(id==NULL ? g_illegal_filepath_string : id)}; return ret;\n"
                "}\n")
           (<-> "static inline " type "_t make_" type "(" c-type " id){" type "_t ret = {.id=id}; return ret;}\n"))
       "#ifdef __cplusplus\n"
       (if (eq? type 'filepath)
           (<->
            "  #if defined(QSTRING_H)\n"
            "    #if defined(RADIUM_COMMON_NSMTRACKER_H)\n"
            "      #ifdef USE_QT4\n"
            "        #include \"../common/OS_string_proc.h\"\n"
            "        static inline filepath_t make_filepath(QString id){filepath_t ret = {.id=STRING_create(id)}; return ret;}\n"
            "      #endif\n"
            "    #endif\n"
            "  #endif\n"
            )
           (<->
            "  static inline bool operator==(const " type "_t &a, const " type "_t &b){ return a.id==b.id; }\n"
            "  static inline unsigned int qHash(" type "_t a){ return a.id; }\n"
            "  static inline bool operator<(const " type "_t &e1, const " type "_t &e2){ return e1.id < e2.id; }\n"))
       "#endif\n"
       ))
          
(define (get-s7-type-c-functions-code type c-type)
  (<->
   "int g_" type "_type_tag;\n"
   "\n"
   "s7_pointer s7extra_make_" type "(s7_scheme *s7, " type "_t val){\n"
   "\n"
   (if (eq? type 'filepath)
       (<->
        "  const wchar_t* o = val.id==NULL ? NULL : wcsdup(val.id);\n")
       (<->
        "  if (sizeof(" c-type ") <= sizeof(void*))\n"
        "    return s7_make_c_object(s7, g_" type "_type_tag, (void*)(val.id));\n"
        "\n"
        "  " c-type " *o = (" c-type "*)malloc(sizeof(" c-type "));\n"
        "  *o = val.id;\n"))
   "\n"
   "  return(s7_make_c_object(s7, g_" type "_type_tag, (void *)o));\n"
   "}\n"
   "\n"
   "static void free_" type "(void *val){\n"
   "  if (val) free(val);\n"
   "}\n"
   "\n"
   "static s7_pointer is_" type "(s7_scheme *s7, s7_pointer args){\n"
   "  return(s7_make_boolean(s7,\n"
   "                         s7_is_c_object(s7_car(args)) &&\n"
   "                         s7_c_object_type(s7_car(args)) == g_" type "_type_tag));\n"
   "}\n"
   "\n"
   "static s7_pointer equal_" type "(s7_scheme *s7, s7_pointer args){\n"
   "  s7_pointer p1 = s7_car(args);\n"
   "  if (!s7_is_c_object(p1))\n"
   "    return s7_f(s7);\n"
   "\n"
   "  s7_pointer p2 = s7_cadr(args);\n"
   "\n"
   "  if (p1 == p2)\n"
   "    return(s7_t(s7));\n"
   "\n"
   "  if (!s7_is_c_object(p2))\n"
   "    return s7_f(s7);\n"
   "\n"
   (if (eq? type 'filepath)
       (<-> "  const wchar_t *d1 = (const wchar_t*)s7_c_object_value(p1);\n"
            "  const wchar_t *d2 = (const wchar_t*)s7_c_object_value(p2);\n"
            "  if (d1==NULL || d2==NULL) return s7_f(s7);\n"
            "  if (!wcscmp(d1,d2)) return s7_t(s7);\n"
            "  return s7_f(s7);\n")
       (<-> "  if (sizeof(" c-type ") <= sizeof(void*))\n"
            "    return s7_make_boolean(s7, s7_c_object_value(p1) == s7_c_object_value(p2));\n"
            "\n"
            "  " c-type " *d1 = (" c-type "*)s7_c_object_value(p1);\n"
            "  " c-type " *d2 = (" c-type "*)s7_c_object_value(p2);\n"
            "\n"
            "  if ((*d1)==(*d2))\n"
            "    return s7_t(s7);\n"
            "  else\n"
            "    return s7_f(s7);\n"))
   "}\n"
   ))

(define (get-s7-type-init-c-code type c-type)
  (<->
   "  g_" type "_type_tag = s7_make_c_type(s7, \"" type "\");\n"
   (if (eq? type 'filepath)
       ""
       (<-> "  if (sizeof(" c-type ") > sizeof(void*))\n"))
   "    s7_c_type_set_free(s7, g_" type "_type_tag, free_" type ");\n"
   "  s7_c_type_set_is_equal(s7, g_" type "_type_tag, equal_" type ");\n"
   "  s7_define_function(s7, \"" type "?\", is_" type ", 1, 0, false, \"(" type "? anything) returns #t if its argument is a/an " type "\");\n"
   ))


(define (gen-s7_types)
  (let ((port (open-output-file "api/s7_types.h")))
    (display "#pragma once\n" port)       
    (for-each (lambda (gakk)
                (display (apply get-s7-types-code gakk) port))
              *radium-s7-types*)
    (close-port port)))

(define (gen-s7_types_code)
  (let ((port (open-output-file "api/s7_types_code.c")))
    (for-each (lambda (gakk)
                (display (apply get-s7-type-c-functions-code gakk) port))
              *radium-s7-types*)
    (close-port port)))

(define (gen-s7_types_code_init)
  (let ((port (open-output-file "api/s7_types_code_init.c")))
    (for-each (lambda (gakk)
                (display (apply get-s7-type-init-c-code gakk) port))
              *radium-s7-types*)
    (close-port port)))


(define (main args)
  (define arg (if (null? (cdr args))
                  'gakk'
                  (string->symbol (cadr args))))
  (cond ((eq? arg 'types)
         (gen-s7_types))
        ((eq? arg 'code)
         (gen-s7_types_code))
        ((eq? arg 'init)
         (gen-s7_types_code_init))
        (else
         (c-display "Error. Unknown args. Ether code or init"))))

