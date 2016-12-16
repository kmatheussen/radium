(provide 'common2.scm)

#!!
(define-expansion (match-old args . matchers)
  (define matcher-func (gensym "matcher-func"))
  (eval
   `(let ()
      (define-match ,matcher-func
        ,@matchers)
      (apply ,matcher-func ,args))))
!!#

(define-expansion (match args . matchers)
  `(begin
     (eval (create-matcher-func 'matcher-func-temp-name ',matchers))
     (apply matcher-func-temp-name ,args)))


#!!
(pretty-print (macroexpand   (define-match is-define-lazy
                               (define-lazy _ _ ) :> #t
                               __________________ :> #f)))

(test (match (list 'a 'b)
             a b :> 5
             _ _ :> #f)
      5)
(match (list 'a 'b)
       a b :> 5
       _ :> 9)

(let-ref (rootlet) 'aiai2)
(defined? 'aiai (rootlet))

(procedure-source setaiai!)

(define (setaiai! val)
  (varlet (rootlet) 'aiai val))


!!#

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

(define-expansion (push! list el)
  `(set! ,list (cons ,el ,list)))

(define-expansion (push-back! list el)
  `(set! ,list (append ,list (list ,el))))


(define (scale x x1 x2 y1 y2)
  (+ y1 (/ (* (- x x1)
              (- y2 y1))
           (- x2 x1))))

(define (average . numbers)
  (/ (apply + numbers)
     (length numbers)))

(define (X/Y a b x y)
  (+ a (/ (* x (- b a))
          y)))

(define (onethird a b)
  (X/Y a b 1 3))
(define (twothirds a b)
  (X/Y a b 2 3))
         
(define (twofifths a b)
  (X/Y a b 2 5))
(define (threefifths a b)
  (X/Y a b 3 5))
         
;; fix max, which is buggy in s7 (the bug is most likely fixed now if you read this though)
#||
(define (max a . rest)
  (if (null? rest)
      a
      (let ((b (apply max rest)))
        (if (< a b)
            b
            a))))
||#

(assert (= 0 (max 0 -1/2)))

(define (between Min Try-it Max)
  (cond ((< Try-it Min)
         Min)
        ((> Try-it Max)
         Max)
        (else
         Try-it)))     

;; (round 2.5) -> 2
;; (roundup 2.5) -> 3
(define (roundup A)
  (floor (+ A 0.5)))

(define (two-decimals val)
  (/ (roundup (* val 100))
     100.0))

(define (two-decimal-string number)
  (format #f "~,2F" (* 1.0 number)))

(define (one-decimal-percentage-string number)
  (format #f "~,1F" (* 100.0 number)))
   
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
(define-expansion (delay . body)
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


(define (copy-struct-helper original struct-name keys arguments mapper)

  ;; check that new data is valid
  (let loop ((arguments arguments))
    (if (not (null? arguments))
        (let ((key (car arguments))
              (value (cadr arguments)))
          (if (not (memq (keyword->symbol key) keys))
              (throw (<-displayable-> "key '" key (<-> "' not found in struct '" struct-name "'") ". keys: " (map symbol->keyword keys))))
          (loop (cddr arguments)))))

  (define old-table original)
  (define new-table (make-hash-table 32 mapper))

  ;; copy old
  (for-each (lambda (key)
              (let ((key (symbol->keyword key)))
                (hash-table-set! new-table
                                 key
                                 (old-table key))))
            keys)

  ;; add new data
  (let loop ((arguments arguments))
    (if (not (null? arguments))
        (let ((key (car arguments))
              (value (cadr arguments)))
          (hash-table-set! new-table key value)
          (loop (cddr arguments)))))

  new-table)


(define-expansion (define-struct name . args)
  (define define-args (keyvalues-to-define-args args))
  (define keys (map car define-args))
  (define must-be-defined (keep (lambda (arg)
                                  (equal? ''must-be-defined (cadr arg)))
                                define-args))
  (define table (gensym "table"))
  (define key (gensym "key"))
  (define keysym (gensym "keysym"))
  (define ret (gensym "ret"))
  (define keysvar (gensym "keys"))
  (define keysvar2 (gensym "keys2"))
  (define original (gensym "original"))
  (define arguments (gensym "arguments"))
  (define loop (gensym "loop"))
  (define n (gensym "n"))
  
  `(begin
     
     (define ,(<_> name '-struct-mapper)
       (let ((keytablemapper (make-hash-table (length (quote ,keys)) eq?)))
         (for-each (lambda (key n)
                     (hash-table-set! keytablemapper (symbol->keyword key) n))
                   (quote ,keys)
                   (iota (length (quote ,keys))))
         (lambda (key)
           (or (keytablemapper key)
               (throw (<-displayable-> "key " (keyword->symbol key) ,(<-> " not found in struct '" name "'") ". keys: " (quote ,keys)))))))
     
     (define (,(<_> 'copy- name) ,original . ,arguments)
       (copy-struct-helper ,original (quote ,name) (quote ,keys) ,arguments (cons eq? ,(<_> name '-struct-mapper))))
                     
     (define* (,(<_> 'make- name) ,@(keyvalues-to-define-args args))
       ,@(map (lambda (must-be-defined)
                `(if (eq? ,(car must-be-defined) 'must-be-defined)
                     (throw ,(<-> "key '" (car must-be-defined) "' not defined when making struct '" name "'"))))
              must-be-defined)
       (let* ((,table (make-hash-table 32 (cons eq? ,(<_> name '-struct-mapper))))
              (,keysvar (quote ,keys)))
         ,@(map (lambda (key)
                  `(hash-table-set! ,table ,(symbol->keyword key) ,key))
                keys)
         ,table))))

#||
(define-struct test
  :b 59
  :c)

(define t (make-test :c 2))
(t :b)
(t :c)
(t :dir)

;; error, unknown key:
(copy-test t :unknown-key 2))


(define t2 (copy-test t :b 2))
(t2 :b)

(pretty-print (macroexpand (define-struct teststruct
                             :a 'asdf
                             :b
                             :c #f)))

(pretty-print (macroexpand
               (define-struct test
                 :b 59
                 :c)))

           
(make-test :b 33)

(define t (make-test :c 2))
(t :b)
(t :c)
(t :dir)
(t :bc)
(t :b)

(define t2 (t :copy :b 2))

(define tab (make-hash-table 32 eq?))
(hash-table-set! tab :hello 2)
(hash-table-set! tab :hello 3)
(tab :hello)


||#

;; doesn't "(morally-equal? hash-table1 hash-tabl2)" work? (not always, morally-equal? doesn't call my-equal?).
(define (structs-equal? a b)
  (morally-equal? a b))
#||
  (define alist-a a)
  (define alist-b b)
  (define keys-a (map car alist-a))
  
  (and (= (length keys-a)
          (length alist-b))
       (let loop ((keys-a keys-a))
         (if (null? keys-a)
             #t
             (and (my-equal? (a (car keys-a))
                             (b (car keys-a)))
                  (loop (cdr keys-a)))))))
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

(define-expansion (delafina def . body)
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

(define-expansion (ra:get-box2 prefix . rest)
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
           " :y2 "     (box :y2)
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
  ;;(c-display "Whtas the box:" box)
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
       (>= Y (- (Box :y1) width/2))
       (<  Y (+ (Box :y2) width/2))))


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

(define-match is-define-lazy  
  (define-lazy _ _ ) :> #t
  __________________ :> #f)

(define-match get-lazy-replacement
  (define-lazy Name _____) :> `(,Name (force ,Name))
  ________________________ :> (throw 'something-went-wrong-in-get-lazy-replacement-in-lazy))
  
(define-match transform-lazy-code
  Replacements (define-lazy Name Value) :> `(define ,Name (delay ,(deep-list-replace-several Replacements Value)))
  ____________ _________________________ :> #f)
  

(define-expansion (lazy . body)
  ;;(c-display "EXPSNDFING lazy macro")
  
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

(define-expansion (lazy2 . body)
  (define-match hepp
    _ :> "hepp")

  `(begin
     (c-display ,(hepp #t))
     ,@body))

(define (hepp)
  (lazy2
    ;;(define-lazy a 50)
    60))

(hepp)

(lazy2 5)

(macroexpand (lazy 60))

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


(define-expansion (<ra> command . args)
  `( ,(<_> 'ra: (keyword->symbol command)) ,@args))

                              
(define (my-equal? a b)
  (morally-equal? a b))
#||
  ;;(c-display "my-equal?" a b)
  (cond ((and (pair? a)
              (pair? b))
         (and (my-equal? (car a)
                         (car b))
              (my-equal? (cdr a)
                         (cdr b))))
        ((and (vector? a)
              (vector? b))
         (my-equal? (vector->list a)
                    (vector->list b)))
        ((and (procedure? a)
              (procedure? b))
         (structs-equal? a b))
        (else
         (morally-equal? a b))))
||#


(define (***assert*** a b)
  (define (test -__Arg1 -__Arg2)
    (define (-__Func1)
      (let ((A -__Arg1))
        (if (my-equal? A -__Arg2)
            (begin
              (newline)
              (pretty-print "Correct: ")
              (pretty-print (to-displayable-string A))
              (pretty-print "")
              (newline)
              #t)
            (-__Func2))))
    (define (-__Func2)
      (let ((A -__Arg1))
        (let ((B -__Arg2))
          (begin
            (newline)
            (pretty-print "Wrong. Result: ")
            (pretty-print (to-displayable-string A))
            (pretty-print ". Correct: ")
            (pretty-print (to-displayable-string B))
            (pretty-print "")
            (newline)
            #f))))
    (-__Func1))

  (assert (test a b)))

(define (group-by get-key key-compare elements)
  (define keys '())
  (define hash (make-hash-table 39 key-compare))
  (for-each (lambda (element)
              (let* ((key (get-key element))
                     (old-value (hash-table-ref hash key)))
                (if (not old-value)
                    (push-back! keys key))
                (hash-table-set! hash
                                 key
                                 (cons element
                                       (or old-value '())))))
            elements)
  (map (lambda (key)
         (reverse! (hash-table-ref hash key)))
       keys))

(***assert*** (group-by (lambda (x)
                          x)
                        =
                        '(1 5 2 3 5 1))
              '((1 1)
                (5 5)
                (2)
                (3)))

(define (true-for-all? pred elements)
  (cond ((null? elements)
         #t)
        ((pred (car elements))
         (true-for-all? pred (cdr elements)))
        (else
         #f)))
               

(***assert*** (true-for-all? even? '())
              #t)

(***assert*** (true-for-all? even? '(2 4 6))
              #t)

(***assert*** (true-for-all? even? '(2 4 3))
              #f)


(define (true-for-at-least-one? pred elements)
  (if (null? elements)
      #f
      (or (pred (car elements))
          (true-for-at-least-one? pred (cdr elements)))))

(***assert*** (true-for-at-least-one? even? '())
              #f)

(***assert*** (true-for-at-least-one? even? '(2 4 6))
              #t)

(***assert*** (true-for-at-least-one? even? '(2 4 3))
              #t)

(***assert*** (true-for-at-least-one? even? '(1 9 3))
              #f)

(define (vector-copy vector)
  (define out (make-vector (length vector)))
  (for-each (lambda (n)
              (vector-set! out n (vector n)))
            (iota (length vector)))
  out)

(define (butlast elements)
  (let ((rest (cdr elements)))
    (if (null? rest)
        '()
        (cons (car elements)
              (butlast rest)))))

(***assert*** (butlast '(2)) '())
(***assert*** (butlast '(2 3)) '(2))


(define (second-last elements)
  (cadr (reverse elements)))

;; like list-set! except that it doesn't modify the list
(define (list-replace-element das-list pos new-value)
  (if (= 0 pos)
      (cons new-value
            (cdr das-list))
      (cons (car das-list)
            (list-replace-element (cdr das-list)
                                  (1- pos)
                                  new-value))))

(define (list-remove das-list pos)
  (if (= 0 pos)
      (cdr das-list)
      (cons (car das-list)
            (list-remove (cdr das-list)
                         (1- pos)))))

(***assert*** (list-remove '(0 1 2) 0) '(1 2))
(***assert*** (list-remove '(0 1 2) 1) '(0 2))
(***assert*** (list-remove '(0 1 2) 2) '(0 1))


(define (remove-duplicates-in-sorted-list comparer das-list)
  (if (null? das-list)
      '()
      (let ((a (car das-list)))
        (if (null? (cdr das-list))
            das-list
            (let ((b (cadr das-list)))
              (if (comparer a b)
                  (remove-duplicates-in-sorted-list comparer (cdr das-list))
                  (cons a
                        (remove-duplicates-in-sorted-list comparer (cdr das-list)))))))))
                   

(define (integer-range start-inclusive end-inclusive)
  (map (lambda (i)
         (+ i start-inclusive))
       (iota (1+ (- end-inclusive start-inclusive)))))

(***assert*** (integer-range 0 5)
              '(0 1 2 3 4 5))
(***assert*** (integer-range 5 5)
              '(5))



;;;;;;;;;; popup menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a 50 b 90 c 100 -> '((a 50)(b 90)(c 100))
(define-match make-assoc-from-flat-list
  ()           :> '()
  (A B . Rest) :> (cons (list A B)
                        (make-assoc-from-flat-list Rest)))

#||
(make-assoc-from-flat-list (list "a" 50 "b" 90 "c" 100))
||#

(define (string-starts-with? string startswith)
  (define (loop string startswith)
    (cond ((null? startswith)
           #t)
          ((null? string)
           #f)
          ((char=? (car string) (car startswith))
           (loop (cdr string) (cdr startswith)))
          (else
           #f)))
  (loop (string->list string)
        (string->list startswith)))

(***assert*** (string-starts-with? "asdf" "as") #t)
(***assert*** (string-starts-with? "asdf" "") #t)
(***assert*** (string-starts-with? "" "a") #f)
(***assert*** (string-starts-with? "a" "a") #t)
(***assert*** (string-starts-with? "a" "b") #f)
(***assert*** (string-starts-with? "ab" "a") #t)


(define (parse-popup-menu-options args)
  (if (null? args)
      '()
      (cond ((list? (car args))
             (parse-popup-menu-options (append (car args)
                                               (cdr args))))
            ((not (car args))
             (parse-popup-menu-options (cdr args)))
            (else
             (let ((text (car args))
                   (arg2 (cadr args)))
               (cond ((eq? :check arg2)
                      (let ((check-on (caddr args)))
                        (parse-popup-menu-options (cons (<-> (if check-on "[check on]" "[check off]") text)
                                                        (cdddr args)))))
                     ((eq? :enabled arg2)
                      (let ((enabled (caddr args)))
                        (if enabled
                            (parse-popup-menu-options (cons text
                                                            (cdddr args)))
                            (parse-popup-menu-options (cons (<-> "[disabled]" text)
                                                            (cdddr args))))))
                     ((string-starts-with? text "--")
                      (cons text
                            (cons (lambda _ #t)
                                  (parse-popup-menu-options (cdr args)))))
                     ((procedure? (cadr args))
                      (cons text
                            (cons (cadr args)
                                  (parse-popup-menu-options (cddr args)))))
                     ((list? arg2)
                      (append (list (<-> "[submenu start]" text)
                                    (lambda () #t))
                              (parse-popup-menu-options arg2)
                              (list "[submenu end]"
                                    (lambda () #t))
                              (parse-popup-menu-options (cddr args))))))))))

#||
(parse-popup-menu-options (list "hello1" :enabled #t (lambda ()
                                                       (c-display "hepp1"))
                                "hello2" :enabled #f (lambda ()
                                                       (c-display "hepp2"))                                
                                "hello4" (lambda ()
                                           (c-display "hepp4"))))

(parse-popup-menu-options (list "hello1" :check #t (lambda ()
                                                     (c-display "hepp1"))                                
                                "hello4" (lambda ()
                                           (c-display "hepp4"))))

(parse-popup-menu-options (list "hello1" :check #f (lambda ()
                                                     (c-display "hepp1"))                                
                                "hello4" (lambda ()
                                           (c-display "hepp4"))))

(parse-popup-menu-options (list "hello1" (lambda ()
                                           (c-display "hepp1"))                                
                                "submenu" (list
                                           "hello2" (lambda ()
                                                      (c-display "hepp2"))
                                           "hello3" (lambda ()
                                                      (c-display "hepp3")))
                                "hello4" (lambda ()
                                           (c-display "hepp4"))))
||#

(define (popup-menu . args)
  (define options (parse-popup-menu-options args))
  ;;(c-display "optinos:" options)
  (define relations (make-assoc-from-flat-list options))
  (define strings (list->vector (map car relations)))
  
  (define popup-arg (let loop ((strings (vector->list strings)))
                      ;;(c-display "strings" strings)
                      (if (null? strings)
                          ""
                          (<-> (car strings) " % " (loop (cdr strings))))))

  ;;(c-display "   relations: " relations)
  ;;(for-each c-display relations (iota (length relations)))
  ;;(c-display "strings: " strings)
  ;;(c-display "popup-arg: " popup-arg)

  (define (get-func n)
    ;;(c-display "N: " n)
    ;;(define result-string (vector-ref strings n))
    ;;(cadr (assoc result-string relations))
    (cadr (list-ref relations n))
    )
  
  (define result-num (<ra> :popup-menu2 popup-arg (lambda (n val)
                                                    (define result-string (vector-ref strings n))
                                                    ;;(c-display "n: " n ", val:" val)
                                                    ((get-func n) val))))
  ;;(c-display "     RESULT-NUM" result-num)

  (if (not (= -1 result-num))
      ((get-func result-num)))
  )


#||
(popup-menu "aaa" (lambda ()
                    (c-display "main menu"))
            "bbb" (list "aaa"
                        (lambda ()
                          (c-display "submenu"))))

(popup-menu "hello" :check #t (lambda (ison)
                                (c-display "gakk1" ison))
            "hello2" :enabled #t (lambda ()
                                   (c-display "gakk2"))
            "hello3" :enabled #f (lambda ()
                                   (c-display "gakk3"))
            )
||#
            
#||
(popup-menu "[check on] gakk1 on" (lambda (ison)
                                   (c-display "gakk1 " ison))
            "[check off] gakk2 off" (lambda (ison)
                                     (c-display "gakk2 " ison))
            "hepp1" (lambda ()
                     (c-display "hepp1"))
            "hepp2" (lambda ()
                     (c-display "hepp2"))
            )

(popup-menu "hello" (lambda ()
                      (c-display "hepp"))
            "[submenu start]Gakk gakk-" (lambda () #t)
            "[submenu start]Gakk gakk-" (lambda () #t)
            "hello2" (lambda ()
                       (c-display "hepp2"))
            "[submenu end]" (lambda () #t)
            "[submenu end]" (lambda () #t)
            "hepp" (lambda ()
                     (c-display "hepp3")))
(popup-menu "hello" (lambda ()
                      (c-display "hepp"))
            "Gakk gakk" (list
                         "Gakk gakk2" (list
                                       "hello2" (lambda ()
                                                  (c-display "hepp2"))
                                       "hello3" (lambda ()
                                                  (c-display "hepp3"))))
            "hepp" (lambda ()
                     (c-display "hepp3")))
||#

(define *num-radium-ticks* (<ra> :get-highest-legal-place-denominator))
(define *smallest-radium-tick* (/ 1 *num-radium-ticks*))
(define (-line linenum)
  (- linenum *smallest-radium-tick*))

#||
(define (+line linenum)
  (+ linenum *smallest-radium-tick*))
||#


(define (undo-block block)
  (<ra> :open-undo)
  (let ((ret (catch #t
                    block
                    (lambda args ;; Catch exceptions to ensure (<ra> :cose-undo) will be called
                      (display "args")(display args)(newline)
                      (apply format #t (cadr args))
                      (display (ow!))))))
    (<ra> :close-undo)
    ret))

(define (ignore-undo-block block)
  (<ra> :start-ignoring-undo)
  (let ((ret (catch #t
                    block
                    (lambda args ;; Catch exceptions to ensure (<ra> :cose-undo) will be called
                      (display "args")(display args)(newline)
                      (apply format #t (cadr args))
                      (display (ow!))))))
    (<ra> :stop-ignoring-undo)
    ret))


