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

(define-macro (push-back! list el)
  `(set! ,list (append ,list (list ,el))))


(define (scale x x1 x2 y1 y2)
  (+ y1 (/ (* (- x x1)
              (- y2 y1))
           (- x2 x1))))

(define (average . numbers)
  (/ (apply + numbers)
     (length numbers)))


;; force and delay are missing from s7. Simple implementation below.
(define-macro (delay . body)
  `(vector #f
           #f
           (lambda ()
             ,@body)))

(define (force something)
  (if (not (something 0))
      (begin
        (set! (something 1) ((something 2)))
        (set! (something 0) #t)))
  (something 1))

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

#|
(test (keyvalues-to-define-args '(:a 90 :b 50 :c :d 80))
      '((a 90) (b 50) (c 'must-be-defined) (d 80)))
(test (keyvalues-to-define-args '(:a 90 :b 50 :c))
      '((a 90) (b 50) (c 'must-be-defined)))
|#




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
         (let ((ret (,table key)))
           (if (and (not ret)
                    (not (memq key keys)))
               (throw (<-> "key '" key ,(<-> "' not found in struct '" name "'")))
               ret))))))

#|
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
(t :bc)

|#




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

#|
(pretty-print (macroexpand (delafina (testfunc a b :b 30 :c 90)
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
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Box handling ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct box :x1 :y1 :x2 :y2 :width :height)

(define (make-box2 $x1 $y1 $x2 $y2)
  (make-box :x1 $x1
            :y1 $y1
            :x2 $x2
            :y2 $y2
            :width (- $x2 $x1)
            :height (- $y2 $y1)))
  
(define-macro (ra:get-box prefix)
  `(make-box2 ( ,(<_> 'ra:get- prefix '-x1))
              ( ,(<_> 'ra:get- prefix '-y1))
              ( ,(<_> 'ra:get- prefix '-x2))
              ( ,(<_> 'ra:get- prefix '-y2))))


(define (box-to-string box)
  (<-> "(box"
       " :x1 "     (box :x1)
       " :y1 "     (box :y1)
       " :x2 "     (box :x2)
       " :y1 "     (box :y2)
       " :width "  (box :width)
       " :height " (box :height)
       ")"))

#|
(pretty-print (macroexpand (define-struct box :x1 :y1 :x2 :y2)))

(list
 ((ra:get-box reltempo-slider) :x1)
 ((ra:get-box reltempo-slider) :y1)
 ((ra:get-box reltempo-slider) :x2)
 ((ra:get-box reltempo-slider) :y2)
 ((ra:get-box reltempo-slider) :width)
 ((ra:get-box reltempo-slider) :height))
|#

(define (inside-box box x y)
  (and (>= x (box :x1))
       (<  x (box :x2))
       (>= y (box :y1))
       (<  y (box :y2))))

#|
|#
