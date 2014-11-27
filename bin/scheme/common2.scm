(provide 'common2.scm)

(define-macro (match args . matchers)
  (define matcher-func (gensym "matcher-func"))
  `(let ()
     (define-match ,matcher-func
       ,@matchers)
     (apply ,matcher-func ,args)))

#||  
(test (match (list 'a 'b)
             a b :> 5
             _ _ :> #f)
      5)
(match (list 'a 'b)
       a b :> 5
       _ :> 9)
||#

(define (keep func list)
  (if (null? list)
      '()
      (if (func (car list))
          (cons (car list)
                (keep func (cdr list)))
          (keep func (cdr list)))))

;;(keep (lambda (x) (= x 1)) (list 1 3 1 5))

(define (remove func list)
  (if (null? list)
      '()
      (if (func (car list))
          (remove func (cdr list))
          (cons (car list)
                (remove func (cdr list))))))

(define-macro (push-back! list el)
  `(set! ,list (append ,list (list ,el))))


(define (scale x x1 x2 y1 y2)
  (+ y1 (/ (* (- x x1)
              (- y2 y1))
           (- x2 x1))))

(define (average . numbers)
  (/ (apply + numbers)
     (length numbers)))

(define (between Min Try-it Max)
  (min Max
       (max Min Try-it)))

;; (round 2.5) -> 2
;; (roundup 2.5) -> 3
(define (roundup A)
  (floor (+ A 0.5)))

(define (two-decimals val)
  (/ (roundup (* val 100))
     100.0))

(define (two-decimal-string number)
  (format #f "~,2F" (* 1.0 number)))


(define (min-notfalse . Args)
  (match (list Args)
         ()          :> #f
         (N)         :> N
         (#f . Rest) :> (apply min-notfalse Rest)
         (N  . Rest) :> (let ((that (apply min-notfalse Rest)))
                          (if that
                              (min N that)
                              N))))

#||
(test (min-notfalse)
      #f)
(test (min-notfalse #f)
      #f)
(test (min-notfalse #f 5)
      5)
(test (min-notfalse 5 #f)
      5)
(test (min-notfalse 8 #f 5)
      5)
||#

(define (max-notfalse . Args)
  (match (list Args)
         ()          :> #f
         (N)         :> N
         (#f . Rest) :> (apply max-notfalse Rest)
         (N  . Rest) :> (let ((that (apply max-notfalse Rest)))
                          (if that
                              (max N that)
                              N))))

#||
(test (max-notfalse)
      #f)
(test (max-notfalse #f)
      #f)
(test (max-notfalse #f 5)
      5)
(test (max-notfalse 5 #f)
      5)
(test (max-notfalse 8 #f 5)
      8)
||#



;; force and delay are missing from s7. Simple implementation below.
(define-macro (delay . body)
  `(vector #f
           #f
           (lambda ()
             ,@body)))

(define (force something)
  (if (not (vector-ref something 0))
      (begin
        (vector-set! something 1 ((vector-ref something 2)))
        (vector-set! something 0 #t)))
  (vector-ref something 1))

#||
(define a (delay
            (c-display "hello")
            50))
(c-display a)
(force a)
||#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; define-struct ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-match keyvalues-to-define-args
  ()                 :> '()
  (Key)              :> (cons (list (keyword->symbol Key) ''must-be-defined)
                              '())
  (Key1 Key2 . Rest) :> (cons (list (keyword->symbol Key1) ''must-be-defined)
                              (keyvalues-to-define-args (cons Key2 Rest)))
                        :where (keyword? Key2)                   
  (Key Value . Rest) :> (cons (list (keyword->symbol Key) Value)
                              (keyvalues-to-define-args Rest)))

#||
(test (keyvalues-to-define-args '(:a 90 :b 50 :c :d 80))
      '((a 90) (b 50) (c 'must-be-defined) (d 80)))
(test (keyvalues-to-define-args '(:a 90 :b 50 :c))
      '((a 90) (b 50) (c 'must-be-defined)))
||#




(define-macro (define-struct name . args)
  (define define-args (keyvalues-to-define-args args))
  (define keys (map car define-args))
  (define must-be-defined (keep (lambda (arg)
                                  (equal? ''must-be-defined (cadr arg)))
                                define-args))
  (define table (gensym "table"))
  `(define* (,(<_> 'make- name) ,@(keyvalues-to-define-args args))
     ,@(map (lambda (must-be-defined)
              `(if (eq? ,(car must-be-defined) 'must-be-defined)
                   (throw ,(<-> "key '" (car must-be-defined) "' not defined when making struct '" name "'"))))
            must-be-defined)
     (let* ((,table (make-hash-table 32 eq?))
            (keys (quote ,keys)))
       ,@(map (lambda (key)
                `(hash-table-set! ,table ,(symbol->keyword key) ,key))
              keys)
       (lambda (key)
         (if (eq? key :dir)
             ,table
             (let ((ret (,table key)))
               (if (and (not ret)
                        (not (memq key keys)))
                   (throw (<-> "key '" key ,(<-> "' not found in struct '" name "'")))
                   ret)))))))

#||
(pretty-print (macroexpand (define-struct teststruct
                             :a 'asdf
                             :b
                             :c #f)))

(pretty-print (macroexpand
               (define-struct test
                 :b 59
                 :c)))

           
(define-struct test
  :b 59
  :c)

(make-test :b 32)

(define t (make-test :c 2))
(t :b)
(t :c)
(t :dir)
(t :bc)

||#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; delafina ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define-match delafina-args-to-define*-args
  ()                 :> '()
  (Var . Rest)       :> (throw (<-> "All parameters for a delafina functions must be keywords. '" Var "' is not a keyword"))
                        :where (not (keyword? Var))
  (Key)              :> (list (keyword->symbol Key))
  (Key1 Key2 . Rest) :> (cons (keyword->symbol Key1)
                              (delafina-args-to-define*-args (cons Key2 Rest)))
                        :where (keyword? Key2)                   
  (Key Value . Rest) :> (cons (list (keyword->symbol Key) Value)
                              (delafina-args-to-define*-args Rest)))

(define-macro (delafina def . body)
  `(define* (,(car def) ,@(delafina-args-to-define*-args (cdr def)))
     ,@body))

#||
(pretty-print (macroexpand (delafina (testfunc :b 30 :c 90)
                             (+ 2 3)
                             (+ 5 6))))


(pretty-print (macroexpand (delafina (testfunc :a :b :c 30 :d 90 :e f g)
                             (+ 2 3)
                             (+ 5 6))))

(test (macroexpand (delafina (testfunc a b :c 30 :d 90) a b))
      '(define* (testfunc a b (c 30) (d 90)) a b))

(define* (aiai (a 5) b (c 20))
  (list a b c))

(aiai)

(define* (aiai2 a b c)
  (list a b c))

(aiai2 2 3 4 5 6)
||#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Box handling ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct box :x :y :x1 :y1 :x2 :y2 :width :height)

(define (make-box2 $x1 $y1 $x2 $y2)
  (make-box :x (average $x1 $x2)
            :y (average $y1 $y2)
            :x1 $x1
            :y1 $y1
            :x2 $x2
            :y2 $y2
            :width (- $x2 $x1)
            :height (- $y2 $y1)))
  
(define-macro (ra:get-box prefix . rest)
  `(make-box2 ( ,(<_> 'ra:get- prefix '-x1) ,@rest)
              ( ,(<_> 'ra:get- prefix '-y1) ,@rest)
              ( ,(<_> 'ra:get- prefix '-x2) ,@rest)
              ( ,(<_> 'ra:get- prefix '-y2) ,@rest)))


(define (box-to-string box)
  (if (not box)
      "<box is #f>"
      (<-> "(box"
           " :x1 "     (box :x1)
           " :y1 "     (box :y1)
           " :x2 "     (box :x2)
           " :y1 "     (box :y2)
           " :width "  (box :width)
           " :height " (box :height)
           ")")))

#||
(pretty-print (macroexpand (define-struct box :x1 :y1 :x2 :y2)))

(list
 ((ra:get-box reltempo-slider) :x1)
 ((ra:get-box reltempo-slider) :y1)
 ((ra:get-box reltempo-slider) :x2)
 ((ra:get-box reltempo-slider) :y2)
 ((ra:get-box reltempo-slider) :width)
 ((ra:get-box reltempo-slider) :height))
||#

(define (inside-box box x y)
  (and (>= x (box :x1))
       (<  x (box :x2))
       (>= y (box :y1))
       (<  y (box :y2))))

#||
||#

(define (inside-box-forgiving Box X Y) ;; Inside a box, inluding half the width of a node.
  (define width/2 (1+ (ra:get-half-of-node-width)))
  (and (>= X (- (Box :x1) width/2))
       (<  X (+ (Box :x2) width/2))
       (>= Y (Box :y1))
       (<  Y (Box :y2))))


;; Replaces all occurences of A with B in List
(define-match deep-list-replace
  A B A          :> B
  _ _ (        ) :> '()
  A B (R . Rest) :> (cons (deep-list-replace A B R)
                          (deep-list-replace A B Rest))
  A B C          :> C)
                           
#||
(test (deep-list-replace 1 2 1)
      2)
(test (deep-list-replace '() 2 '())
      2)
(test (deep-list-replace 1 2 3)
      3)
(test (deep-list-replace 1 2 '(1 1))
      '(2 2))
(test (deep-list-replace 1 2 '(1 1 . 1))
      '(2 2 . 2))
(test (deep-list-replace 1 2 '(1 (1 . 1) 2 (2 . 1) (3 1)))
      '(2 (2 . 2) 2 (2 . 2) (3 2)))
||#

(define-match deep-list-replace-several
  ()            List :> List
  ((A B) . ABs) List :> (deep-list-replace-several ABs
                                                   (deep-list-replace A B List)))

#||
(test (deep-list-replace-several '((1 2)(3 4)) '(1 3))
      '(2 4))
(test (deep-list-replace-several '((a (force a))) '(+ a a a))
      '(+ (force a) (force a) (force a)))
||#


#||
for .emacs:

(font-lock-add-keywords
 'scheme-mode
 '(("(\\(define-lazy\\)\\>\\s-*(?\\(\\sw+\\)?"
    (1 font-lock-keyword-face)
    (2 font-lock-type-face)
    (3 (cond ((match-beginning 1) font-lock-function-name-face)
	     ((match-beginning 2) font-lock-variable-name-face)
	     ((match-beginning 3) font-lock-function-name-face)
	     (t font-lock-type-face))
       nil t))))

(font-lock-add-keywords
 'scheme-mode
 '(("(\\(lazy\\)\\>\\s-*(?\\(\\sw+\\)?"
    (1 font-lock-keyword-face)
    (2 (cond ((match-beginning 1) font-lock-function-name-face)
	     ((match-beginning 2) font-lock-variable-name-face)
	     (t font-lock-type-face))
       nil t))))
||#

(define-macro (lazy . body)
  
  (define-match is-define-lazy
    (define-lazy _ _ ) :> #t
    __________________ :> #f)

  (define-match get-lazy-replacement
    (define-lazy Name _____) :> `(,Name (force ,Name))
    ________________________ :> (throw 'something-went-wrong-in-get-lazy-replacement-in-lazy))
  
  (define-match transform-lazy-code
    Replacements (define-lazy Name Value) :> `(define ,Name (delay ,(deep-list-replace-several Replacements Value)))
    ____________ _________________________ :> #f)
  
  (define lazy-vals (keep is-define-lazy body))
  (define lazy-replacements (map get-lazy-replacement lazy-vals))
  (define lazy-vals-code (map (lambda (lazy-val)
                                (transform-lazy-code lazy-replacements lazy-val))
                              lazy-vals))
  
  (define rest-body (remove is-define-lazy body))
  (define rest-body-code (deep-list-replace-several lazy-replacements rest-body))
  
  `(begin
     ,@lazy-vals-code
     ,@rest-body-code))

#||
(test (lazy
       (define-lazy a 50)
       (define-lazy b 60)
       (+ a b))
      110)

(test (let ((val 0))
        (lazy
         (define-lazy a (begin
                          (set! val (+ val 1))
                          val))
         (+ a a a)))
      3)

(test (lazy
        (define-lazy a 5)
        (define-lazy b a)
        b)
      5)

(pretty-print (macroexpand (lazy
                             (define-lazy a 5)
                             (define-lazy b a)
                             b)))

(macroexpand (lazy
              (define-lazy a (begin
                               (set! val (+ val 1))
                               val))
              (+ a a a)))

(macroexpand (lazy
              (define-lazy a 50)
              (define-lazy b 60)
              (+ a b)))
||#
