(provide 'common1.scm)


;; redefine 'ow!'
(define (ow!)
  (call-with-output-string
   (lambda (p)
     (let ((ow (owlet))
	   (elist (list (rootlet))))
       
       ;; show current error data
       (format p "\n;error: ~A" (ow 'error-type))
       (when (pair? (ow 'error-data))
             (format p ": ~A" (apply format #f (ow 'error-data))))

       (format p "~%;error-code: ~S~%" (ow 'error-code))
       (if (ow 'error-line)
           (format p "~%;error-file/line: ~S[~A]~%" (ow 'error-file) (ow 'error-line))
           (format p "~%;error-file/line: ; no file/linenum"))

       (define (print-frame num x l f)
         (if (and (integer? (car l))
                  (string? (car f))
                  (not (string=? (car f) "*stdout*")))
             (format p " ~%    ~A. ~S~40T;~A[~A]" num (car x) (car f) (car l))
             (format p " ~%    ~A. ~S; no file/linenum" num (car x))))
         
       ;; show history, if available
       (when (pair? (ow 'error-history)) ; a circular list, starts at error-code, entries stored backwards
	 (let ((history ())
	       (lines ())
	       (files ())
               (num 2)
	       (start (ow 'error-history)))
	   (do ((x (cdr start) (cdr x)))
	       ((eq? x start)
		(format p "~%error-history:~%    1. ~S ; no file/linenum" (car start))
                ;;(print-frame 1 x lines files)
		(do ((x history (cdr x))
		     (l lines (cdr l))
		     (f files (cdr f)))
		    ((null? x))
		  (if (and (integer? (car l))
			   (string? (car f))
			   (not (string=? (car f) "*stdout*")))
		      (format p " ~%    ~A. ~S~40T;~A[~A]" num (car x) (car f) (car l))
		      (format p " ~%    ~A. ~S; no file/linenum" num (car x)))
                  (set! num (1+ num)))
		(format p "~%"))
	     (set! history (cons (car x) history))
	     (set! lines (cons (pair-line-number (car x)) lines))
	     (set! files (cons (pair-filename (car x)) files)))))
       
       ;; show the enclosing contexts
       (let ((old-print-length (*s7* 'print-length)))
	 (set! (*s7* 'print-length) 8)
	 (do ((e (outlet ow) (outlet e))) 
	     ((memq e elist)
	      (set! (*s7* 'print-length) old-print-length))
	   (if (> (length e) 0)
	       (format p "~%~{~A~| ~}~%" e))
	   (set! elist (cons e elist))))))))

(define (assert something)
  (if (not something)
      (throw "assert-failed")))

#||
(define (c-display . rest)
  (for-each (lambda (d)
              (display d)
              (display " "))
            rest)
  (newline))
||#

(define (to-displayable-string a)
  ;;(display "____ a: ")(display a)(newline)
  (cond ((keyword? a)
         (<-> "#:" (to-displayable-string (keyword->symbol a))))
        ((symbol? a)
         (<-> "'" (symbol->string a)))
        ((string? a)
         a)
        ((number? a)
         (number->string a))
        ((equal? #t a)
         "#t")
        ((equal? #f a)
         "#f")
        ((list? a)
         (<-> "(" (apply <-> (map (lambda (b) (<-> (to-displayable-string b) " ")) a)) ")"))
        ((pair? a)
         (<-> "(" (to-displayable-string (car a)) " . " (to-displayable-string (cdr a)) ")"))
        ((vector? a)
         (<-> "[" (apply <-> (map (lambda (b) (<-> (to-displayable-string b) " ")) (vector->list a))) "]"))
        ((procedure? a)
         (if #f
             "something"
             (catch #t
                    (lambda ()
                      (event-to-string a))
                    (lambda args
                      (catch #t
                             (lambda ()
                               (cloned-instrument-to-string a))
                             (lambda args
                               (with-output-to-string
                                 (lambda ()
                                   (display a)))))))))
        
         ;;(<-> "function [ " (to-displayable-string (procedure-source a)) " ]"))))))
         (else
         "#unknown type")))


#||

(define (provoceit)
  (define (delete-note2)
    (ra:undo-notes (pianonote-info :tracknum))
    (ra:delete-pianonote 0
                         (pianonote-info :notenum)
                         (pianonote-info :tracknum))
    #f)
  
  (define (add-pitch2)
    (ra:undo-notes (pianonote-info :tracknum))
    (define Place (get-place-from-y $button $y))
    (define Value (ra:get-note-value (pianonote-info :notenum) (pianonote-info :tracknum)))
    (define Num (ra:create-pitch Value Place (pianonote-info :tracknum)))
    (if (= -1 Num)
        #f
        #f))
  (popup-menu "Delete Note2" delete-note2
              "Add Portamento2" add-pitch2))

;;(provoceit)



'(string-append ""
               (catch #t
                      (lambda ()
                        (cloned-instrument-to-string 'asdf))
                      (lambda args
                        (catch #t
                               (lambda ()
                                 (event-to-string a))
                               (lambda args
                                 "error"))))
               "")
(define level 0)
(define (to-displayable-string2 a)
  (for-each (lambda (level)
             (display "  "))
           (iota level))
  (display level)(display ": ")(display a)(newline)
  (set! level (1+ level))
  (define result (to-displayable-string-intern a))
  (set! level (1- level))
  (for-each (lambda (level)
             (display "  "))
           (iota level))
  (display level)(display "<____ res: ")(display result)(newline)
  result)

||#

(define *empty-symbol* '___empty_symbol) ;; s7 doesn't allow converting empty string to symbol

(define (to-string a)
  (cond ((symbol? a)
         (if (eq? *empty-symbol* a)
             ""
             (symbol->string a)))
        ((string? a)
         a)
        ((number? a)
         (number->string a))
        ((equal? #t a)
         "#t")
        ((equal? #f a)
         "#f")
        ((procedure? a)
         (catch #t
                (lambda ()
                  (event-to-string a))
                (lambda args
                  a)))
        ((keyword? a)
         (<-> "#:" (to-string (keyword->symbol a))))
        (else
         (with-output-to-string
           (lambda ()
             (display a))))))

(define (<-> . args) (apply string-append (map to-string args)))
(define (<_> . args)
  (let ((s (apply <-> args)))
    (if (string=? "" s)
        *empty-symbol*
        (string->symbol s))))

(define (<-displayable-> . args) (apply string-append (map to-displayable-string args)))

#||
(let ((result (<-> "hello: "
                   (with-output-to-string
                     (lambda ()
                       (write (lambda (a b c) 'hello)))))))
  (c-display "result2: -" result "-"))
||#


#||
(<-displayable-> (list 2 3 4))
(<-displayable-> (cons 3 4))
(<-displayable-> :ga)
(<-displayable-> ':ga)
(<-displayable-> 'ga)
(<-displayable-> :'ga)
||#

(define (c-display . args)
  (for-each (lambda (arg)
              (display (to-displayable-string arg))
              (display " "))
            args)
  (newline))

(define *my-gensym-N* 0)

(define (nth n list)
  (list-ref list (- n 1)))

#||
(define (1+ n)
  (+ n 1))

(define (1- n)
  (- n 1))
||#

(define (yppla l c)
  (apply c l))

(define (delete-from das-list element)
  (if (eqv? (car das-list) element)
      (cdr das-list)
      (cons (car das-list)
            (delete-from (cdr das-list) element))))

(define (delete-list-from das-list elements)
  (if (null? elements)
      das-list
      (delete-list-from (delete-from das-list (car elements))
                        (cdr elements))))


(define (last das-list) ;; Wouldn't be surprised if this version is slower than '(car (reverse das-list))' though... (but no, this one is much faster with the test below)
  ;;(c-display "last//// " das-list)
  (let loop ((a (car das-list))
             (b (cdr das-list)))
    (if (null? b)
        a
        (loop (car b)
              (cdr b)))))

#||
(define (last2 das-list)
  (car (reverse das-list)))

(let ((list (make-list 10000000 "hello")))
  (c-display "1")
  (last list)
  (c-display "2")
  (last2 list)
  (c-display "3"))
||#


(define (find-first das-list func)
  (cond ((null? das-list)
         #f)
        ((func (car das-list))
         (car das-list))
        (else
         (find-first (cdr das-list) func))))


(define (find-last das-list func)
  (let loop ((candidate #f)
             (das-list das-list))
    (if (null? das-list)
        candidate
        (loop (or (and (func (car das-list))
                       (car das-list))
                  candidate)
              (cdr das-list)))))

#||
(find-last '(2 5 3 9 8) (lambda (x)
                          (< x 4)))
||#

(define (split-list das-list func)
  (let loop ((before '())
             (after das-list))
    (cond ((null? after)
           (list (reverse before) '()))
          ((func (car after))
           (list (reverse before) after))
          (else
           (loop (cons (car after) before)
                 (cdr after))))))
  
(define (take-while das-list func)
  (cond ((null? das-list)
         '())
        ((func (car das-list))
         (cons (car das-list)
               (take-while (cdr das-list) func)))
        (else
         '())))

(define (remove-while das-list func)
  (cond ((null? das-list)
         '())
        ((func (car das-list))
         (remove-while (cdr das-list) func))
        (else
         das-list)))

