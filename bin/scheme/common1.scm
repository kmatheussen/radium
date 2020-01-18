(provide 'common1.scm)


;; redefine 'ow!'
#||
(set! ow! (lambda ()      
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
      )
||#


 
#||
(define (c-display . rest)
  (for-each (lambda (d)
              (display d)
              (display " "))
            rest)
  (newline))
||#

#||
(define (hash-table-to-string table)
  (<-> "hash:(map values (hash-table* 'b 2 'c 3))
  (values table))

(hash-table-to-string (hash-table* 'b 2 'c 3))
||#

(declare-overridable 'and-let*)

(c-define-macro (*and-let** vars . body)
  (let loop ((vars vars))
    (if (null? vars)
        `(begin ,@body)
        `(let (,(car vars))
           (and ,(car (car vars))
                ,(loop (cdr vars)))))))

#!!
(pp (macroexpand (define2 add9 integer? 50)))

(define2 anint2 integer? 'b)
(define2 anint2 integer? 5)

(set! anint2 30)
(set! anint2 'b)
!!#

(define i-max (let ((+signature+ '(integer? integer? integer?)))
                (lambda (a b)
                  (max a b))))

(define i-min (let ((+signature+ '(integer? integer? integer?)))
                (lambda (a b)
                  (min a b))))

;; Partial application
(define (P-> funcname . args)
  (lambda args2
    (apply funcname (append args args2))))

;; Small one-arg function
(c-define-expansion (*L->* body)
  `(lambda (_)
     ,body))

(define (curry-or . funcs)
  (<declare-variable> _)
  (L-> (let loop ((funcs funcs))
         (if (null? funcs)
             #f
             (or ((car funcs) _)
                 (loop (cdr funcs)))))))

;; Note! This function is called from the error handler.
(define (to-displayable-string a)
  (cond ((keyword? a)
         (string-append "#:" (to-displayable-string (keyword->symbol a))))
        ((symbol? a)
         (string-append "'" (symbol->string a)))
        ((string? a)
         a)
        ((char? a)
         a)
        ((number? a)
         (number->string a))
        ((equal? #t a)
         "#t")
        ((equal? #f a)
         "#f")
        ((pair? a)
         (if (infinite? (length a))
             "(Infinite list)"
             (string-append "(" (let loop ((as a)
                                           (is-first #t))
                                  ;;(display "as: ")(display as) (display ". is-first: ") (display is-first)(display ". pair?: ")(display (pair? as))
                                  ;;(newline)
                                  (cond ((null? as)
                                         "")
                                        ((pair? as)
                                         ;;(display "car: ")(display (car as)) (display ". cdr:") (display (cdr as))(newline)
                                         (string-append (if is-first
                                                            ""
                                                            " ")
                                                        (to-displayable-string (car as))
                                                        (loop (cdr as) #f)))
                                        (else
                                         (string-append " . " (to-displayable-string as)))))
                            ")")))
        ((null? a)
         "()")
        ((vector? a)
         (string-append "[" (apply string-append
                                   (map (lambda (b)
                                          (string-append (to-displayable-string b) " "))
                                        (vector->list a)))
                        "]"))
        ;;((hash-table? a)
        ;; (<-> 
        ((procedure? a)
         (format #f "func:~S" a))
        ((hash-table? a)
         (format #f "hash-table: ~S" a))
        ;;(<-> "{ " (to-displayable-string (map values a)) " }"))
        ;;(<-> "function [ " (to-displayable-string (procedure-source a)) " ]"))))))
        (else
         (format #f "unknown type: ~S" a))))


#||
(to-displayable-string (cons 1 2))
(to-displayable-string (list 1 2 '()))

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

;; Note! This function is called from the error handler.
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
                  (<declare-variable> event-to-string)
                  (event-to-string a))
                (lambda args
                  (format #f "func:~S" a))))
        ((keyword? a)
         (<-> "#:" (to-string (keyword->symbol a))))
        (else
         (object->string a))))

#!!
(c-display "args:" x)
!!#

;; Note! This function is called from the error handler.
(set! <->
      (lambda args
        (apply string-append (map to-string args))))

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

(set! c-display
      (lambda args
        (for-each (lambda (arg)
                    (display (to-displayable-string arg))
                    (display " "))
                  args)
        (newline)))


(define (common1-finished-loading)
  (<declare-variable> with-history-disabled)
  (let ((old-<-> <->))
    (set! <-> (lambda x
                (with-history-disabled
                 (apply old-<-> x)))))
  (let ((old-<_> <_>))
    (set! <_> (lambda x
                (with-history-disabled
                 (apply old-<_> x)))))
  (let ((old-c-display c-display))
    (set! c-display (lambda x
                      (with-history-disabled
                       (apply old-c-display x))))))

(define *my-gensym-N* 0)

(define (identity a)
  a)

(define (nth n list*)
  (list-ref list* (- n 1)))

#||
(define (1+ n)
  (+ n 1))

(define (1- n)
  (- n 1))
||#

(define (yppla l c)
  (apply c l))

(define (maybe-thunk-value m)
  (if (procedure? m)
      (m)
      m))

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
           (list (reverse! before) '()))
          ((func (car after))
           (list (reverse! before) after))
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

;; '(5 5 2 3 9) -> '((5 5) (2) (3) (9))
(define (group-list das-list two-elements-mergable?)
  (let loop ((das-list das-list)
             (ret '()))
    (if (null? das-list)
        ret
        (let ((el (car das-list)))
          (loop (cdr das-list)
                (let loop ((ret ret))
                  ;;(c-display "ret:" ret)
                  (cond ((null? ret)
                         (list (list el)))
                        ((two-elements-mergable? el (caar ret))
                         (cons (append (car ret) (list el))
                               (cdr ret)))
                        (else
                         (cons (car ret)
                               (loop (cdr ret)))))))))))

(***assert*** (group-list '(5 5 2 3 9 5) =)
              '((5 5 5) (2) (3) (9)))

(***assert*** (group-list '((5 a) (5 b) (2 c) (3 d) (2 de) (9 e) (5 f))
                          (lambda (a b)
                            (= (car a) (car b))))
              '(((5 a) (5 b) (5 f))
                ((2 c) (2 de))
                ((3 d))
                ((9 e))))
#!!
(merge-list '(5 5 2 3 9 5) =)
!!#
         
;; a 50 b 90 c 100 -> '((a 50)(b 90)(c 100))
(define (make-assoc-from-flat-list rest)  
  (if (null? rest)
      '()
      (let ((A (car rest))
            (B (cadr rest))
            (Rest (cddr rest)))
        (cons (list A B)
              (make-assoc-from-flat-list Rest)))))

#||
(make-assoc-from-flat-list (list "a" 50 "b" 90 "c" 100))
||#

(define (take l how-many)
  (assert (>= how-many 0))
  (let loop ((l l)
             (how-many how-many))
    (if (= 0 how-many)
        '()
        (cons (car l)
              (loop (cdr l)
                    (1- how-many))))))

(define (sublist l start end)
  (if (> start 0)
      (sublist (cdr l) (1- start) (1- end))
      (take l end)))

(***assert*** (sublist '() 0 0)
              '())

(***assert*** (sublist '(1 2 3) 0 0)
              '())

(***assert*** (sublist '(1 2 3) 0 1)
              '(1))

(***assert*** (sublist '(1 2 3) 0 2)
              '(1 2))

(***assert*** (sublist '(1 2 3) 0 3)
              '(1 2 3))

(***assert*** (sublist '(1 2 3) 1 1)
              '())

(***assert*** (sublist '(1 2 3) 1 2)
              '(2))

(***assert*** (sublist '(1 2 3) 1 3)
              '(2 3))

(***assert*** (sublist '(1 2 3) 2 2)
              '())

(***assert*** (sublist '(1 2 3) 2 3)
              '(3))

(define (delete-maybe el l comp)
  (cond ((null? l)
         '())
        ((comp el (car l))
         (cdr l))
        (else
         (cons (car l)
               (delete-maybe el (cdr l) comp)))))
      
;; string

(define (string-split string* ch)
  (if (string=? string* "")
      '()
      (let ((splitted (split-list (string->list string*)
                                  (lambda (ch2)
                                    (char=? ch ch2)))))
        (cond ((null? (cadr splitted))
               (list string*))
              ((null? (car splitted))
               (string-split (list->string (cdr (cadr splitted))) ch))
              (else
               (cons (list->string (car splitted))
                     (string-split (list->string (cdr (cadr splitted))) ch)))))))

(define (string-take string* pos)
  (substring string* 0 pos))

(***assert*** (string-take "1234" 2)
              "12")

;; Other variants (not implemented): string-take, string-drop-right, string-take-right
(define (string-drop string* pos)
  (substring string* pos))

(***assert*** (string-drop "abcd" 1)
              "bcd")

(define (string-drop-right string* num)
  (string-take string* (- (string-length string*) num)))

(***assert*** (string-drop-right "abcd" 1)
              "abc")


;; string-starts-with? is moved to mylint.scm.

(***assert*** (string-starts-with? "" "") #t)
(***assert*** (string-starts-with? "asdf" "as") #t)
(***assert*** (string-starts-with? "asdf" "") #t)
(***assert*** (string-starts-with? "" "a") #f)
(***assert*** (string-starts-with? "a" "a") #t)
(***assert*** (string-starts-with? "a" "b") #f)
(***assert*** (string-starts-with? "ab" "a") #t)

(define (string-ends-with? string* endswith)
  (define (loop string* startswith)
    (cond ((null? startswith)
           #t)
          ((null? string*)
           #f)
          ((char=? (car string*) (car startswith))
           (loop (cdr string*) (cdr startswith)))
          (else
           #f)))
  (loop (reverse! (string->list string*))
        (reverse! (string->list endswith))))

(***assert*** (string-ends-with? "" "") #t)
(***assert*** (string-ends-with? "asdf" "df") #t)
(***assert*** (string-ends-with? "asdf" "") #t)
(***assert*** (string-ends-with? "" "a") #f)
(***assert*** (string-ends-with? "a" "a") #t)
(***assert*** (string-ends-with? "a" "b") #f)
(***assert*** (string-ends-with? "ab" "b") #t)

;; Returns true if bb is placed inside aa.
(define (string-contains? aa bb)
  (if (or (string=? bb "")
          (string-position bb aa))
      #t
      #f))
#||
  (if (string=? bb "")
      #t
      (begin
        (define b (bb 0))
        (let loop ((aa (string->list aa)))
          (cond ((null? aa)
                 #f)
                ((and (char=? b (car aa))
                      (string-starts-with? (list->string aa) bb))
                 #t)
                (else
                 (loop (cdr aa))))))))
  ||#
  
(***assert*** (string-contains? "" "") #t)
(***assert*** (string-contains? "asdf" "df") #t)
(***assert*** (string-contains? "asdf" "") #t)
(***assert*** (string-contains? "" "a") #f)
(***assert*** (string-contains? "a" "a") #t)
(***assert*** (string-contains? "a" "b") #f)
(***assert*** (string-contains? "ab" "b") #t)
(***assert*** (string-contains? "abcd" "bc") #t)
(***assert*** (string-contains? "abccb" "bcd") #f)
(***assert*** (string-contains? "abbcd" "bcd") #t)

(define (string-case-insensitive-contains? aa bb)
  (string-contains? (string-upcase aa) (string-upcase bb)))

(define (capitalize-first-char-in-stringlist l)
  (cons (char-upcase (car l))
        (cdr l)))

(define (capitalize-first-char-in-string str)
  (list->string (capitalize-first-char-in-stringlist (string->list str))))


(define (string-join strings separator)
  (if (null? strings)
      ""
      (<-> (car strings)
           (let loop ((strings (cdr strings)))
             (if (null? strings)
                 ""
                 (<-> separator
                      (car strings)
                      (loop (cdr strings))))))))

(***assert*** (string-join (list "a" "bb" "ccc") " + ")
              "a + bb + ccc")

(define (is-whitespace? char*)
  (or (char=? char* #\newline)
      (char=? char* #\space)
      (char=? char* #\tab)))

(define (string-strip-right string*)
  (list->string
   (reverse!
    (remove-while (reverse! (string->list string*))
                  is-whitespace?))))

(***assert*** (string-strip-right "   as dfasdf \n\n ")
              "   as dfasdf")

(define (string-strip-left string*)
  ;;(assert (< (string-length string*) 1000))
  (list->string
   (remove-while (string->list string*)
                  is-whitespace?)))

(***assert*** (string-strip-left "   as dfasdf \n\n ")
              "as dfasdf \n\n ")

(define (string-strip string*)
  (string-strip-left (string-strip-right string*)))

(define (string-rightjustify string1 string2pos string2)
  (define len1 (string-length string1))
  (define middle-length (max 1 (- string2pos len1)))
  (<-> string1 (apply <-> (make-list middle-length " ")) string2)
  )

(***assert*** (string-rightjustify "aa" 0 "bb")
              "aa bb")

(***assert*** (string-rightjustify "aa" 1 "bb")
              "aa bb")

(***assert*** (string-rightjustify "aa" 2 "bb")
              "aa bb")

(***assert*** (string-rightjustify "aa" 3 "bb")
              "aa bb")

(***assert*** (string-rightjustify "aa" 4 "bb")
              "aa  bb")

(define (string-replace thestring before-key after-key)
  (let loop ((processed "")
             (remaining thestring))
    (let ((pos (string-position before-key remaining)))
      ;;(c-display "\n" before-key after-key ", processed:" processed ", remaining" remaining ", pos:" pos)
      (<-> processed
           (if (not pos)
               remaining
               (loop (<-> (string-take remaining pos) after-key)
                     (string-drop remaining (+ pos (string-length before-key)))))))))

(***assert*** (string-replace "" "" "")
              "")

(***assert*** (string-replace "aiai" "" "")
              "aiai")

(***assert*** (string-replace "aiai" "a" "b")
              "bibi")

(***assert*** (string-replace "ai999 2ai" "ai" "b")
              "b999 2b")

(define (get-python-ra-funcname funcname)
  (cond ((string=? funcname "ra:show-hide-bpm-track")
         "ra.showHideBPMTrack")
        ((string=? funcname "ra:show-hide-lpb-track")
         "ra.showHideLPBTrack")
        (else
         (let ((parts (string-split (string-drop funcname 3) #\-)))
           (<-> "ra."
                (car parts)
                (apply <->
                       (map capitalize-first-char-in-string
                            (cdr parts))))))))

#!!
(get-python-ra-funcname "ra:w")
!!#

(***assert*** (get-python-ra-funcname "ra:transpose-block")
              "ra.transposeBlock")

(define (get-python-ra-funccall rafuncname args)
  (<-> (get-python-ra-funcname rafuncname)
       "("
       (string-join (map to-displayable-string args) ",")
       ")"))
              
(***assert*** (get-python-ra-funccall "ra:transpose-block" (list 1))
              "ra.transposeBlock(1)")

  
(define (get-all-lines-in-file wfilename)
  (let ((file (<ra> :open-file-for-reading wfilename)))
    (let loop ((ret '()))
      (if (not (<ra> :file-at-end file))
          (loop (cons (<ra> :read-line-from-file file)
                      ret))
          (begin
            (<ra> :close-file file)
            (reverse! ret))))))
