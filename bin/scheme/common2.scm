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

(define-macro (push! list el)
  `(set! ,list (cons ,el ,list)))

(define-macro (push-back! list el)
  `(set! ,list (append ,list (list ,el))))


(define (scale x x1 x2 y1 y2)
  (+ y1 (/ (* (- x x1)
              (- y2 y1))
           (- x2 x1))))

(define (average . numbers)
  (/ (apply + numbers)
     (length numbers)))

;; fix max, which is buggy in s7 (the bug is most likely fixed now if you read this though)
(define (max a . rest)
  (if (null? rest)
      a
      (let ((b (apply max rest)))
        (if (< a b)
            b
            a))))

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


(define (copy-struct-helper original struct-name keys arguments)

  ;; check that new data is valid
  (let loop ((arguments arguments))
    (if (not (null? arguments))
        (let ((key (car arguments))
              (value (cadr arguments)))
          (if (not (memq (keyword->symbol key) keys))
              (throw (<-displayable-> "key '" key (<-> "' not found in struct '" struct-name "'") ". keys: " (map symbol->keyword keys))))
          (loop (cddr arguments)))))

  (define old-table (original :dir))
  (define new-table (make-hash-table 32 eq?))

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


(define-macro (define-struct name . args)
  (define define-args (keyvalues-to-define-args args))
  (define keys (map car define-args))
  (define must-be-defined (keep (lambda (arg)
                                  (equal? ''must-be-defined (cadr arg)))
                                define-args))
  (define table (gensym "table"))
  (define key (gensym "key"))
  (define ret (gensym "ret"))
  (define keysvar (gensym "keys"))
  (define original (gensym "original"))
  (define arguments (gensym "arguments"))
  
  `(begin
     (define (,(<_> 'make- name '-internal) ,table)
       (let ((,keysvar (quote ,keys)))
         (lambda (,key)
           (cond ((eq? ,key :dir)
                  ,table)
                 (else
                  (let ((,ret (,table ,key)))
                    (if (and (not ,ret)
                             (not (memq (keyword->symbol ,key) ,keysvar)))
                        (throw (<-displayable-> "key '" ,key ,(<-> "' not found in struct '" name "'") ". keys: " (map symbol->keyword ,keysvar)))
                        ,ret)))))))

     (define (,(<_> 'old-copy- name) ,original new-key new-value)
       (if (not (memq (keyword->symbol new-key) (quote ,keys)))
           (throw (<-displayable-> "key '" new-key ,(<-> "' not found in struct '" name "'") ". keys: " (map symbol->keyword (quote ,keys)))))
       ;;(c-display "copy-" new-key new-value)
       (define ,table (make-hash-table 32 eq?))
       ,@(map (lambda (key)                
                `(hash-table-set! ,table ,(symbol->keyword key) (,original ,(symbol->keyword key))))
              keys)
       (hash-table-set! ,table new-key new-value)
       (,(<_> 'make- name '-internal) ,table))

     (define (,(<_> 'copy- name) ,original . ,arguments)
       (let ((,table (copy-struct-helper ,original (quote ,name) (quote ,keys) ,arguments)))
         (,(<_> 'make- name '-internal) ,table)))
                     
     (define* (,(<_> 'make- name) ,@(keyvalues-to-define-args args))
       ,@(map (lambda (must-be-defined)
                `(if (eq? ,(car must-be-defined) 'must-be-defined)
                     (throw ,(<-> "key '" (car must-be-defined) "' not defined when making struct '" name "'"))))
              must-be-defined)
       (let* ((,table (make-hash-table 32 eq?))
              (,keysvar (quote ,keys)))
         ,@(map (lambda (key)
                  `(hash-table-set! ,table ,(symbol->keyword key) ,key))
                keys)
         (,(<_> 'make- name '-internal) ,table)))))
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

;; doesn't "(morally-equal? hash-table1 hash-tabl2)" work?
(define (structs-equal? a b)
  (define alist-a (hash-table->alist (a :dir)))
  (define alist-b (hash-table->alist (b :dir)))
  (define keys-a (map car alist-a))
  
  (and (= (length keys-a)
          (length alist-b))
       (let loop ((keys-a keys-a))
         (if (null? keys-a)
             #t
             (and (my-equal? (a (car keys-a))
                             (b (car keys-a)))
                  (loop (cdr keys-a)))))))




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


(define-macro (<ra> command . args)
  `( ,(<_> 'ra: (keyword->symbol command)) ,@args))



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

(define (popup-menu . options)
  (define relations (make-assoc-from-flat-list options))
  (define strings (list->vector (map car relations)))
  
  (define popup-arg (let loop ((strings (map car relations)))
                      (c-display "strings" strings)
                      (if (null? strings)
                          ""
                          (<-> (car strings) " % " (loop (cdr strings))))))
    
  (c-display "relations: " relations)
  (c-display "strings: " strings)
  (c-display "popup-arg: " popup-arg)

  (define (get-func n)
    (define result-string (vector-ref strings n))
    (cadr (assoc result-string relations)))
  
  (define result-num (<ra> :popup-menu2 popup-arg (lambda (n val)
                                                    (define result-string (vector-ref strings n))
                                                    (c-display "n: " n ", val:" val)
                                                    ((get-func n) val))))

  (if (not (= -1 result-num))
      ((get-func result-num)))
  )


#||
(popup-menu "[check on] gakk1 on" (lambda (ison)
                                   (c-display "gakk1 " ison))
            "[check off] gakk2 off" (lambda (ison)
                                     (c-display "gakk2 " ison))
            "hepp" (lambda ()
                     (c-display "hepp")))
||#

(define *num-radium-ticks* (<ra> :get-highest-legal-place-denominator))
(define *smallest-radium-tick* (/ 1 *num-radium-ticks*))
(define (-line linenum)
  (- linenum *smallest-radium-tick*))

#||
(define (+line linenum)
  (+ linenum *smallest-radium-tick*))
||#


(define (my-equal? a b)
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
