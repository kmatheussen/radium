#!/bin/sh
# -*- scheme -*-
exec guile -e main -s $0 $*
!#

(use-modules (ice-9 format))


(define *radium-s7-types*
  '((instrument int64_t)))



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
  (<-> "#pragma once\n"
       "extern int g_" type "_type_tag;\n"
       "typedef " c-type " " type "_t;\n"))
          
(define (get-s7-type-c-functions-code type c-type)
  (<->
   "int g_" type "_type_tag;\n"
   "\n"
   "s7_pointer s7extra_make_" type "(s7_scheme *s7, " c-type " val){\n"
   "\n"
   "  if (sizeof(" c-type ") <= sizeof(void*))\n"
   "    return s7_make_c_object(s7, g_" type "_type_tag, (void*)val);\n"
   "\n"
   "  " c-type " *o = (" c-type "*)malloc(sizeof(" c-type "));\n"
   "  *o = val;\n"
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
   "  if (sizeof(" c-type ") <= sizeof(void*))\n"
   "    return s7_make_boolean(s7, s7_c_object_value(p1) == s7_c_object_value(p2));\n"
   "\n"
   "  " c-type " *d1 = (" c-type "*)s7_c_object_value(p1);\n"
   "  " c-type " *d2 = (" c-type "*)s7_c_object_value(p2);\n"
   "\n"
   "  if ((*d1)==(*d2))\n"
   "    return s7_t(s7);\n"
   "  else\n"
   "    return s7_f(s7);\n"
   "}\n"
   "\n"
   ))

(define (get-s7-type-init-c-code type c-type)
  (<->
   "  g_" type "_type_tag = s7_make_c_type(s7, \"" type "\");\n"
   "  if (sizeof(" c-type ") > sizeof(void*))\n"
   "    s7_c_type_set_free(s7, g_" type "_type_tag, free_" type ");\n"
   "  s7_c_type_set_is_equal(s7, g_" type "_type_tag, equal_" type ");\n"
   "  s7_define_function(s7, \"" type "?\", is_" type ", 1, 0, false, \"(" type "? anything) returns #t if its argument is a/an " type "\");\n"
   ))


(define (gen-s7_types)
  (let ((port (open-output-file "api/s7_types.h")))
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

