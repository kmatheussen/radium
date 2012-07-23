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

void GFX_P_FilledBox(struct Tracker_Windows* tvisual,int color,int x,int y,int x2,int y2);

void GFX_P_Box(struct Tracker_Windows* tvisual,int color,int x,int y,int x2,int y2);


void GFX_P_Line(struct Tracker_Windows* tvisual,int color,int x,int y,int x2,int y2);
void GFX_P_Point(struct Tracker_Windows* tvisual,int color,int x,int y);

void GFX_P_Text(
	struct Tracker_Windows* tvisual,
	int color,
	char* text,
	int x,
	int y,
	int width,
	int flags
	);

void GFX_Line(struct Tracker_Windows* tvisual,int color,int x,int y,int x2,int y2);
void GFX_Box(struct Tracker_Windows* tvisual,int color,int x,int y,int x2,int y2);
void GFX_FilledBox(struct Tracker_Windows* tvisual,int color,int x,int y,int x2,int y2);

void GFX_Text(
	struct Tracker_Windows* tvisual,
	int color,
	char* text,
	int x,
	int y,
	int width,
	int flags
);

void GFX_P_Scroll(
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
             ((string=? type "bool") "b")
             ((string=? type "char*") "s"))
       (number->string (+ n 1))))

(define (create-gfx-func funcdef)
  (parse-c-proto funcdef
                 (lambda (rettype name args)
                   (let ((window-name (last (car args))))
                     (c-display funcdef)
                     (c-display "{")
                     (c-display (<-> "  queue_element_t *el = get_next_element(" window-name "->op_queue);"))
                     (c-display (<-> "  el->type = ENUM_" name) ";")
                     (for-each (lambda (arg n)
                                 (let ((type (car arg))
                                       (name (cadr arg)))
                                   (if (string=? type "char*")
                                       (c-display (<-> "  memcpy(el->" (get-element-slot-name type n) ", " name ", R_MIN(strlen(" name ")+1,62));"))
                                       (c-display (<-> "  el->" (get-element-slot-name type n)) "=" name ";"))))
                               (cdr args)
                               (iota (1- (length args))))
                     (c-display "}"))
                   (newline))))
                     
(define (create-gfx-funcs)
  (for-each create-gfx-func protos))


(define (create-play-op-queue-case funcdef)
  (parse-c-proto funcdef
                 (lambda (rettype name args)
                   (display (<-> "case ENUM_" name ": OS_" name "(window"))
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
  (newline)

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

