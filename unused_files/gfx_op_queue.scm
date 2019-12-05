#!/bin/sh
# -*- scheme -*-
exec guile -e main -s $0 $*
!#


'(
/* Copyright 2003-2012 Kjetil S. Matheussen

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */
)


(use-modules (ice-9 optargs)
	     (srfi srfi-1)
	     (srfi srfi-13)
	     (ice-9 rdelim)
	     (ice-9 pretty-print))

;; Functions which are called from the queue, but are not OS specific.
(define pre-os-funcs '(GFX_Line GFX_Text))
(define pre-os-includes '())

;;(define pre-os-funcs '())
;;(define pre-os-includes '())


(define protos-in-one-string "

void GFX_C2V_bitBlt(
				    struct Tracker_Windows* window,
				    int from_x1,int from_x2,
				    int to_y
                    );


void GFX_C_DrawCursor(
				      struct Tracker_Windows* window,
				      int x1,int x2,int x3,int x4,int height,
				      int y_pixmap
				      );

void GFX_P2V_bitBlt(
				struct Tracker_Windows* window,
				int from_x,int from_y,
				int to_x,int to_y,
				int width,int height
			);

void GFX_FilledBox(struct Tracker_Windows* tvisual,enum ColorNums color,int x,int y,int x2,int y2,int where);

void GFX_Box(struct Tracker_Windows* tvisual,enum ColorNums color,int x,int y,int x2,int y2, int where);

void GFX_SetClipRect(
                                          struct Tracker_Windows* tvisual,
                                          int x,int y,
                                          int x2,int y2,
                                          int where
                                          );
void GFX_CancelClipRect(struct Tracker_Windows* tvisual,int where);

void GFX_Line(struct Tracker_Windows* tvisual,enum ColorNums color,int x,int y,int x2,int y2,int where);

void GFX_Polygon(
                                    struct Tracker_Windows* tvisual,
                                    enum ColorNums color,
                                    int x1, int y1, int x2, int y2,
                                    int num_points,
                                    APoint* peaks,
                                    int where
                                    );

void GFX_Polyline(
                                     struct Tracker_Windows* tvisual,
                                     enum ColorNums color,
                                     int x1, int y1, int x2, int y2,
                                     int num_points,
                                     APoint* peaks,
                                     int where
                                     );

void GFX_CancelMixColor(struct Tracker_Windows* tvisual);
void GFX_SetMixColor(struct Tracker_Windows* tvisual,enum ColorNums color1,enum ColorNums color2,int mix_factor);
void GFX_SetMixColor2(struct Tracker_Windows* tvisual,enum ColorNums color1,unsigned int color2,int mix_factor);

void GFX_Text(
	struct Tracker_Windows* tvisual,
	enum ColorNums color,
	const char* text,
	int x,
	int y,
	int width,
	int flags,
        int where
	);

void GFX_BitBlt(
	struct Tracker_Windows* tvisual,
	int dx,int dy,
	int x,int y,
	int x2,int y2
	);
")



(define (c-butlast l)
  (reverse (cdr (reverse l))))

(define (last l)
  (car (reverse l)))

(define <-> string-append)

(define (c-display . args)
  (if (null? args)
      (newline)
      (begin
        (display (car args))
        (display " ")
        (apply c-display (cdr args)))))

(define protos (c-butlast (string-split protos-in-one-string #\;)))


;; Code copied from snd-rt
(define (parse-c-proto funcdef cont)
  (let* ((temp (map string-trim-both (string-split funcdef #\()))
	 (retname (string-split (car temp) #\space))
	 (rettype (string-trim-both (apply <-> (map (lambda (x) (<-> x " ")) (c-butlast retname)))))
	 (name (last retname))
	 (args (let* ((temp1 (string-trim-right funcdef))
		      (temp2 (string-trim-both (substring temp1
							  (1+ (string-index temp1 #\())
							  (1- (string-length temp1))))))
		 (if (or (= (string-length temp2) 0)
			 (string=? temp2 "void"))
		     '()
		     (map (lambda (x)
			    (if (string-index x #\()
				(list "void*" (eval-c-get-unique-name))
				(let ((dassplit (map string-trim-both (string-split (string-trim-both x) #\space))))
				  (if (= 1 (length dassplit))
				      (list (car dassplit) (eval-c-get-unique-name))
				      (list (string-trim-both (apply <-> (map (lambda (x)
										(<-> x " "))
									      (c-butlast dassplit))))
					    (string-trim-both (last dassplit)))))))
			  (string-split temp2 #\,)))))
	 )
    (cont rettype name args)))

(define (get-element-slot-name type n)
  (<-> (cond ((string=? type "int") "i")
             ((string=? type "unsigned int") "u")
             ((string=? type "enum ColorNums") "i")
             ((string=? type "bool") "b")
             ((string=? type "char*") "s")
             ((string=? type "const char*") "s")
             ((string=? type "APoint*") "p")
             (else
              (throw (<-> "   UNKNOWN TYPE " type))))
       (number->string (+ n 1))))

(define (create-gfx-func funcdef)
  (parse-c-proto funcdef
                 (lambda (rettype name args)
                   (let ((window-name (cadr (car args))))
                     (display (<-> rettype " QUEUE_" name "("))
                     (for-each (lambda (arg)
                                 (display (car arg))(display " ") (display (cadr arg))
                                 (if (equal? arg (last args))
                                     (c-display "){")
                                     (display ", ")))
                               args)
                     (if #f ;; Separates off-screen painting from on-screen painting. (now they are intertwined) Some cleanups are needed before this can be enabled.
                         (if (member "where" (map cadr args))
                             (begin
                               (display (<-> "if(where==PAINT_BUFFER){OS_" name "("))
                               (for-each (lambda (arg)
                                           (display (cadr arg))
                                           (if (equal? arg (last args))
                                               (display ");")
                                               (display ", ")))
                                         args)
                               (c-display "return;}"))))

                     (c-display (<-> "  if(" window-name "->must_redraw==true) return;"))

                     (c-display (<-> "  queue_element_t *el = get_next_element(" window-name "->op_queue);"))
                     (c-display (<-> "  el->type = ENUM_" name) ";")
                     (for-each (lambda (arg n)
                                 (let ((type (car arg))
                                       (name (cadr arg)))
                                   (if (or (string=? type "char*") (string=? type "const char*"))
                                       (c-display (<-> "  memcpy(el->" (get-element-slot-name type n) ", " name ", R_MIN((int)strlen(" name ")+1,62));"))
                                       (c-display (<-> "  el->" (get-element-slot-name type n)) "=" name ";"))))
                               (cdr args)
                               (iota (1- (length args))))
                     (c-display "}"))
                   (newline))))
                     
(define (create-gfx-funcs)
  (for-each create-gfx-func protos))

(define (get-queue-func-name name)
  (if (memq (string->symbol name) pre-os-funcs)
      (<-> "PREOS_" name)
      (<-> "OS_" name)))

(define (create-play-op-queue-case funcdef)
  (parse-c-proto funcdef
                 (lambda (rettype name args)
                   (display (<-> "case ENUM_" name ": " (get-queue-func-name name) "(window"))
                   (for-each (lambda (arg n)
                               (display (<-> ", el->" (get-element-slot-name (car arg) n))))
                             (cdr args)
                             (iota (1- (length args))))
                   (c-display "); break;"))))

(define (create-play-op-queue-cases)
  (for-each create-play-op-queue-case protos))

(define (create-enums)
  (for-each (lambda (funcdef)
              (parse-c-proto funcdef
                             (lambda (rettype name args)
                               (c-display (<-> "ENUM_" name ",")))))
            protos))

(define (create-gfx-op-queue-code)
  (c-display "// This file is automatically generated from gfx_op_queue.scm")
  (newline)

  (c-display "#ifdef OP_ENUMS")
  (create-enums)
  (c-display "#endif")

  (newline)
  
  (c-display "#ifdef OP_CASES")
  (create-play-op-queue-cases)
  (c-display "#endif")

  (newline)

  (c-display "#ifdef OP_FUNCS")
  (create-gfx-funcs)
  (c-display "#endif"))


(define (create-qprotos)
  (c-display "// This file is automatically generated from gfx_op_queue.scm")
  (newline)
  (c-display "#ifndef VISUAL_OP_QUEUE_PROC_H")
  (c-display "#define VISUAL_OP_QUEUE_PROC_H")
  (c-display "#include \"visual_proc.h\"")
  (newline)

  (for-each (lambda (pre-os-include)
              (c-display (<-> "#include \"" pre-os-include "\"")))
            pre-os-includes)

  (for-each (lambda (funcdef)
              (parse-c-proto funcdef
                             (lambda (rettype name args)
                               (display (<-> "extern LANGSPEC " rettype " OS_" name "("))
                               (for-each (lambda (arg n)
                                           (if (> n 0)
                                               (display ","))
                                           (display (<-> (car arg) " " (cadr arg))))
                                         args
                                         (iota (length args)))
                               (c-display ");"))))

            protos)

  (newline)
  (c-display "#endif"))


(define (main args)
  (define arg (if (null? (cdr args))
                  'gakk'
                  (string->symbol (cadr args))))
  (cond ((eq? arg 'protos)
         (create-qprotos))
        ((eq? arg 'op-queue-code)
         (create-gfx-op-queue-code))
        (else
         (c-display "Error. Unknown args. Ether protos or op-queue-code"))))

