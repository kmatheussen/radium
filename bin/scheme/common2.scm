(provide 'common2.scm)

#!!
(c-define-expansion (*match-old* args . matchers)
  (define matcher-func (gensym "matcher-func"))
  (eval
   `(let ()
      (define-match ,matcher-func
        ,@matchers)
      (apply ,matcher-func ,args))))
!!#

(c-define-expansion (*match* args . matchers)
  `(begin
     (eval (create-matcher-func 'matcher-func-temp-name ',matchers))
     (<declare-variable> matcher-func-temp-name)
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

(define (time)
  (*s7* 'cpu-time))


(define (safe-scale x x1 x2 y1 y2)
  (let ((div (- x2 x1)))
    (if (= div 0)
        (begin
          (<declare-variable> safe-add-message-window-txt)
          (safe-add-message-window-txt (string-append "Error. Almost divided by zero in safe-scale: (= (- x2 x1) 0) " (number->string x2) " " (number->string x1)))
          0)
        (+ y1 (/ (* (- x x1)
                    (- y2 y1))
                 (- x2 x1))))))

(assert (= 0 (max 0 -1/2)))

(define (random-shuffle seq)
  (define v (to-vector seq))
  (define size (length v))
  (if (< size 2)
      seq
      (begin
        (for-each (lambda (to-pos)
                    (define from-pos (integer-myrand to-pos (1- size)))
                    (define to-val (v to-pos))
                    (define from-val (v from-pos))
                    ;;(c-display "swapping " to-pos from-pos)
                    (set! (v to-pos) from-val)
                    (set! (v from-pos) to-val))
                  (integer-range 0 (- size 2)))
        (if (vector? seq)
            v
            (to-list v)))))

#!!
(random-shuffle '(6 2 3 1))
(random-shuffle '())
(random-shuffle '(4))
(random-shuffle '(5 2))

!!#

;; uses "chance" to determine the chance of swapping two sequential elements
(define (light-shuffle seq chance)
  (let loop ((seq (to-list seq)))
    (if (or (null? seq)
            (null? (cdr seq)))
        seq
        (let ((element1 (car seq))
              (element2 (cadr seq)))
          (if (>= chance (myrand 0 1))
              (if #f
                  (append (list element2 element1)
                          (loop (cddr seq)))
                  (cons element2
                        (loop (cons element1
                                    (cddr seq)))))
              (cons element1
                    (loop (cdr seq))))))))
#!!
(light-shuffle '(1 2 3 4) 0.0)
!!#

;; (round 2.5) -> 2
;; (roundup 2.5) -> 3
(define (roundup A)
  (floor (+ A 0.5)))


(define (unit-ceiling value unit)
  (* unit (ceiling (/ value unit))))

(***assert*** (unit-ceiling 5 5)
              5)
(***assert*** (unit-ceiling 10 10)
              10)
(***assert*** (unit-ceiling 5.2 1)
              6)
(***assert*** (unit-ceiling 5.2 2)
              6)
(***assert*** (unit-ceiling 5.2 3)
              6)
(***assert*** (unit-ceiling 5.2 4)
              8)
(***assert*** (unit-ceiling 5.2 5)
              10)
(***assert*** (unit-ceiling 5.2 6)
              6)

(define (unit-floor value unit)
  (* unit (floor (/ value unit))))


(define (unit-round value unit)
  (* unit (round (/ value unit))))


(define (two-decimals val)
  (/ (roundup (* val 100))
     100.0))

(define (one-decimal-string number)
  (format #f "~,1F" (* 1.0 number)))

(define (two-decimal-string number)
  (format #f "~,2F" (* 1.0 number)))

(define (one-decimal-percentage-string number)
  (format #f "~,1F" (* 100.0 number)))

(define (to-integer A)
  (inexact->exact (floor A)))

(define (to-list A)
  (if (vector? A)
      (vector->list A)
      A))

(define (to-vector A)
  (if (list? A)
      (list->vector A)
      A))

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


(define (get-procedure-name procedure)
  ;;(c-display "doc:" (documentation procedure))
  (let ((doclist (string-split (documentation procedure) #\space)))
    ;;(c-display "DOCLIST: -" doclist "-")
    (if (null? doclist)
        ""
        (if (string=? "" (car doclist))
            ""
            (string-drop (car doclist) 1)))))



;; force and delay are missing from s7. Simple implementation below.
(c-define-expansion (*delay* . body)
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


;; copy-hash
(define (copy-hash hash . rest)
  (define ret (copy hash))
  (let loop ((rest rest))
    (if (null? rest)
        ret
        (let ((key (car rest))
              (value (cadr rest)))
          (hash-table-set! ret key value)
          (loop (cddr rest))))))

(***assert*** (copy-hash (hash-table :a 9 :b 10 :c 'a)
                         :a 8
                         :b 11)
              (hash-table :a 8
                           :b 11
                           :c 'a))

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
  (if (keyword? original)
      (error 'not-a-struct-but-a-keyword (<-> "Copy " struct-name " struct: First argument is not a struct, but a keyword")))

  ;; check that new data is valid
  (let loop ((arguments arguments))
    (if (not (null? arguments))
        (let ((key (car arguments))
              (value (cadr arguments)))
          (if (not (memq (keyword->symbol key) keys))
              (error 'key-not-found-in-struct1 (<-displayable-> "key '" key (<-> "' not found in struct '" struct-name "'") ". keys: " (map symbol->keyword keys))))
          (loop (cddr arguments)))))

  (define new-table (copy original)) ;; No worries. 'new-table' will contain the "(cons eq? ,struct-mapper)" argument similar to 'original'.
  
  ;; add new data
  (let loop ((arguments arguments))
    (if (not (null? arguments))
        (let ((key (car arguments))
              (value (cadr arguments)))
          (hash-table-set! new-table key value)
          (loop (cddr arguments)))))

  new-table)


(c-define-expansion (*define-struct* name . args)
  (define define-args (keyvalues-to-define-args args))
  (define keys (map car define-args))
  (define keys-length (length keys))
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

  (define struct-mapper (<_> name '-struct-mapper))
                             
  `(begin
     
     (define ,struct-mapper
       (let ((keytablemapper (make-hash-table ,keys-length eq?)))
         (for-each (lambda (key n)
                     (hash-table-set! keytablemapper (symbol->keyword key) n))
                   (quote ,keys)
                   (iota ,keys-length))
         (lambda (key)
           (or (keytablemapper key)
               (error 'key-not-found-in-struct2 (<-displayable-> "key " (keyword->symbol key) ,(<-> " not found in struct '" name "'") ". keys: " (quote ,keys)))))))
     
     (define (,(<_> '<copy- name '>) ,original . ,arguments)
       (copy-struct-helper ,original
                           (quote ,name)
                           (quote ,keys)
                           ,arguments))
                     
     (define (,(<_> 'copy- name) ,original . ,arguments)
       (copy-struct-helper ,original
                           (quote ,name)
                           (quote ,keys)
                           ,arguments))
                     
     (define (,(<_> 'make- name '-nokeywords) ,@(map car (keyvalues-to-define-args args)))
       (let* ((,table (make-hash-table ,keys-length (cons eq? ,struct-mapper)))
              (,keysvar (quote ,keys)))
         ,@(map (lambda (key)
                  `(hash-table-set! ,table ,(symbol->keyword key) ,key))
                keys)
         ,table))

     (define* (,(<_> 'make- name) ,@(keyvalues-to-define-args args))
       ,@(map (lambda (must-be-defined)
                `(if (eq? ,(car must-be-defined) 'must-be-defined)
                     (error 'missing-key-when-making-struct ,(<-> "key '" (car must-be-defined) "' not defined when making struct '" name "'"))))
              must-be-defined)
       (let* ((,table (make-hash-table ,keys-length (cons eq? ,struct-mapper)))
              (,keysvar (quote ,keys)))
         ,@(map (lambda (key)
                  `(hash-table-set! ,table ,(symbol->keyword key) ,key))
                keys)
         ,table))))

#||
(define-struct test
  :b 59
  :c)

(make-test-nokeywords 3 4)
(make-test 3 4)

(define t (make-test :c 2))
(t :b)
(t :c)
(t :dir)

(define t2 (copy-test t :b 80))
(t2 :b)
(t2 :c)
(t2 :dir)


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

(hash-table* :a 9 :b 8)
           
(make-test :b 33)

(define t (make-test :c 2))
(t :b)
(t :c)
(t :dir)
(t :bc)
(t :b)

(hash-table-set! t :b 8)

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
  (Var . Rest)       :> (error 'delafina-error (<-> "All parameters for delafina must be keywords. '" Var "' is not a keyword"))
                        :where (not (keyword? Var))
  (Key)              :> (list (keyword->symbol Key))
  (Key1 Key2 . Rest) :> (cons (keyword->symbol Key1)
                              (delafina-args-to-define*-args (cons Key2 Rest)))
                        :where (keyword? Key2)                   
  (Key Value . Rest) :> (cons (list (keyword->symbol Key) Value)
                              (delafina-args-to-define*-args Rest)))

(c-define-expansion (*delafina* def . body)
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
;;;;;;;;;; Try - Catch - Finally ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
#||
"eat-errors" is NOT the same as dynamic-wind. It doesn't rethrow. I.e. the code following a call to "eat-errors" should always run [1].

Also note that the :finally thunk doesn't have an important purpose. It's just syntactic sugar. This:


(eat-errors :try (lambda () 5)
             :finally newline)


...is the same as this:


(let ((ret (eat-errors :try (lambda () 5))))
  (newline)
  ret)



[1] At least in the normal situations, not too sure about what happens if there is creative use of call/cc.
||#


(define-constant *try-finally-rethrown* (gensym "try-finally-rethrown"))


(define (catch-args-are-from-try-finally args)
  (and (not (null? args))
       (symbol? (car args))
       (eq? *try-finally-rethrown* (car args))))

#!!
(+ notanumber1 notanumber2)
(error *try-finally-rethrown*)
(begin
  (error *try-finally-rethrown*)
  (awefoaijweogijareg))
!!#

;;
;;
;;
;; A helper function:

(define (catch-all-errors-and-display-backtrace-automatically thunk)
  (catch #t
         thunk
         (lambda args
           (display "(catch-all-errors-and-display-backtrace-automatically thunk) failed. args:")(newline)
           (display args)(newline)
           (when (not (catch-args-are-from-try-finally args))
             (catch #t
                    safe-display-ow!
                    (lambda args
                      (error 'safe-display-ow!-failed))))
           *eat-errors-failed-return-value*)))

(define (catch-all-errors-failed? ret)
  (eq? ret *eat-errors-failed-return-value*))

;; Then the function we want to use everywhere:

(define-constant *eat-errors-failure-thunk-failed* 'eat-errors-failure-thunk-failed)
(define-constant *eat-errors-false-unless-failure-is-overridden* 'false-unless-failure-is-overridden)

(delafina (eat-errors :try
                       
                       :rethrow #f
                       
                       ;; If overridden, :failure will always return this value. If not overridden, the default :failure implementation will return #f.
                       :failure-return-value *eat-errors-false-unless-failure-is-overridden*
                       
                       :failure (lambda ()
                                  (if (eq? failure-return-value *eat-errors-false-unless-failure-is-overridden*)
                                      #f
                                      failure-return-value))
                       
                       :finally (lambda ()
                                  #f))
  
  (define (return ret maybe-rethrow)
    (finally)
    (if (and rethrow maybe-rethrow)
        (throw *try-finally-rethrown*)
        ret))
  
  (define try-ret (catch-all-errors-and-display-backtrace-automatically try))

  (if (catch-all-errors-failed? try-ret)
      (begin                    
        (define failed-ret (catch-all-errors-and-display-backtrace-automatically failure))
        (cond ((catch-all-errors-failed? failed-ret)
               (return *eat-errors-failure-thunk-failed* #t))
              ((eq? failure-return-value *eat-errors-false-unless-failure-is-overridden*)
               (return failed-ret #t))
              (else
               (return failure-return-value #t))))
      (return try-ret #f)))


(delafina (try-finally :try
                       :failure #f
                       :failure-return-value *eat-errors-false-unless-failure-is-overridden*
                       :finally (lambda ()
                                  #f))
  (if failure
      (eat-errors :try try
                  :rethrow #t
                  :failure failure
                  :finally finally)
      (eat-errors :try try
                  :rethrow #t
                  :finally finally)))



(c-display "\n\n\n\n=======================================================================================================")
(c-display "    START testing try-catch-failure. Lots of backtrace will be printed, but nothing is wrong, hopefully.")
(c-display "=======================================================================================================\n\n\n\n")

;; Test try-finally
(let* ((somethingspecial (gensym "somethingspecial")))
  (***assert*** (let ((should-be-somethingspecial somethingspecial))
                  (eat-errors :try (lambda ()
                                     (try-finally :try (lambda ()
                                                         (<declare-variable> weofij)
                                                         (<declare-variable> oiwgrjoewrgi)
                                                         (+ weofij oiwgrjoewrgi)))
                                     (ra:play-block-from-start)
                                     (set! should-be-somethingspecial 'not-so-special)
                                     (<declare-variable> aoregijoaija)
                                     (<declare-variable> aeorgijaoirgje)
                                     (+ aoregijoaija aeorgijaoirgje))
                              :finally (lambda ()
                                         50))
                  (assert (not (ra:is-playing-block)))
                  should-be-somethingspecial)
                somethingspecial))


;; Test all fine.
(***assert*** (eat-errors :try (lambda ()
                                  (c-display "returning 4")
                                  (+ 1 3))
                           :rethrow #f)
              4)

;; Test all fine with finalizer
(let ((is-finalized #f))
  (***assert*** (eat-errors :try (lambda ()
                                    (c-display "returning 5")
                                    (+ 2 3))
                             :finally (lambda ()
                                        (c-display "finally")
                                        (set! is-finalized #t)))
                5)
  (***assert*** is-finalized #t))

;; Test all fine and catch not called.
(let ((is-finalized #f)
      (is-catched #f))
  (***assert*** (eat-errors :try (lambda ()
                                    (c-display "returning 5")
                                    (***assert*** is-finalized #f)
                                    (+ 2 3))
                             :failure (lambda ()
                                        (c-display "catching")
                                        (***assert*** is-finalized #f)
                                        (set! is-catched #t))
                             :finally (lambda ()
                                        (c-display "finally")
                                        (***assert*** is-catched #f)
                                        (set! is-finalized #t)))
                5)
  (***assert*** is-finalized #t)
  (***assert*** is-catched #f))

(<declare-variable> *undefined-var*)
(<declare-variable> *undefined-var2*)

;; Test failure in 'try'. Returns #f by default.
(let ((is-finalized #f))
  (define result (eat-errors :try (lambda ()
                                     (c-display "returning 5")
                                     (***assert*** is-finalized #f)
                                     (+ *undefined-var* 3))
                              :failure-return-value 281
                              :finally (lambda ()
                                         (c-display "finally")
                                         (set! is-finalized #t))))
  (***assert*** result 281)
  (***assert*** is-finalized #t))


;; Test failure in 'try'. Custom failure function.
(let ((is-finalized #f))
  (define result (eat-errors :try (lambda ()
                                     (c-display "returning 5")
                                     (***assert*** is-finalized #f)
                                     (+ *undefined-var* 3))
                              :finally (lambda ()
                                         (c-display "finally")
                                         (set! is-finalized #t))))
  (***assert*** result #f)
  (***assert*** is-finalized #t))


;; Test failure in 'try'. Custom failure func.
(let ((is-finalized #f)
      (is-catched #f))

  (define result (eat-errors :try (lambda ()
                                     (c-display "returning 5")
                                     (***assert*** is-finalized #f)
                                     (+ *undefined-var* 3))
                              :failure (lambda ()
                                         (c-display "catching")
                                         (***assert*** is-finalized #f)
                                         (set! is-catched #t)
                                         'failed)
                              :finally (lambda ()
                                         (c-display "finally")
                                         (***assert*** is-catched #t)
                                         (set! is-finalized #t))))
  (***assert*** result 'failed)
  (***assert*** is-finalized #t)
  (***assert*** is-catched #t))


;; Test failure in 'try'. Custom failure function and custom failure return value.
(let ((is-finalized #f)
      (is-catched #f))
  (define result (eat-errors :try (lambda ()
                                     (c-display "returning 5")
                                     (***assert*** is-finalized #f)
                                     (+ *undefined-var* 3))
                              :failure-return-value 281
                              :failure (lambda ()
                                         (c-display "catching")
                                         (***assert*** is-finalized #f)
                                         (set! is-catched #t)
                                         'failed)
                              :finally (lambda ()
                                         (c-display "finally")
                                         (***assert*** is-catched #t)
                                         (set! is-finalized #t))))
  (***assert*** result 281)
  (***assert*** is-catched #t)
  (***assert*** is-finalized #t))


;; Test failure in 'failure'. (a lot of backtrace is supposed to be printed now, but the important thing is that the last three asserts are correct.)
(let ((is-finalized #f)
      (is-catched #f))
  (define result (eat-errors :try (lambda ()
                                     (c-display "returning 5")
                                     (***assert*** is-finalized #f)
                                     (+ *undefined-var* 3))
                              :failure (lambda ()
                                         (c-display "catching")
                                         (***assert*** is-finalized #f)
                                         (set! is-catched #t)
                                         (+ *undefined-var2* 4)
                                         'failed)
                              :finally (lambda ()
                                         (c-display "finally")
                                         (***assert*** is-catched #t)
                                         (set! is-finalized #t))))
  (***assert*** result *eat-errors-failure-thunk-failed*)
  (***assert*** is-finalized #t)
  (***assert*** is-catched #t))

;; Test failure in 'finally'. (we test that a failure in 'finally' is not caught)
(let ((is-finalized #f)
      (is-catched #f)
      (finally-failed #f))
  (define result (catch #t
                        (lambda ()
                          (eat-errors :try (lambda ()
                                              (c-display "returning 5")
                                              (***assert*** is-finalized #f)
                                              (+ *undefined-var* 3))
                                       :failure (lambda ()
                                                  (c-display "catching")
                                                  (***assert*** is-finalized #f)
                                                  (set! is-catched #t)
                                                  (+ *undefined-var2* 4)
                                                  'failed)
                                       :finally (lambda ()
                                                  (c-display "finally")
                                                  (***assert*** is-catched #t)
                                                  (set! is-finalized #t)
                                                  (<declare-variable> c)
                                                  (+ c 5))))
                        (lambda args
                          (set! finally-failed #t)
                          'finally-failed2)))
  
  (***assert*** result 'finally-failed2)
  (***assert*** finally-failed #t)
  (***assert*** is-finalized #t)
  (***assert*** is-catched #t))


(c-display "\n\n\n\n=======================================================================================================")
(c-display "    FINISHED testing try-catch-failure. Lots of backtrace was printed, but nothing is wrong, hopefully.")
(c-display "=======================================================================================================\n\n\n\n")



(c-define-expansion (*with-history-disabled* . code)
  `(begin
     (ra:disable-scheme-history)
     (try-finally :try (lambda ()
                         ,@code)
                  :finally ra:enable-scheme-history)))



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
  
(c-define-macro (*ra:get-box* prefix . rest)
  `(make-box2 ( ,(<_> 'ra:get- prefix '-x1) ,@rest)
              ( ,(<_> 'ra:get- prefix '-y1) ,@rest)
              ( ,(<_> 'ra:get- prefix '-x2) ,@rest)
              ( ,(<_> 'ra:get- prefix '-y2) ,@rest)))

(c-define-expansion (*ra:get-box2* prefix . rest)
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
  ________________________ :> (error 'something-went-wrong-in-get-lazy-replacement-in-lazy))
  
(define-match transform-lazy-code
  Replacements (define-lazy Name Value) :> `(define ,Name (delay ,(deep-list-replace-several Replacements Value)))
  ____________ _________________________ :> #f)
  

(c-define-expansion (*lazy* . body)
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

(define true-for-all? every?)               

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

;; Precompute all iota results up to, and including, 1024.
(let* ((org-iota iota)
       (iota-results (list->vector (map (lambda (n)
                                          (org-iota n))
                                        (org-iota 1026)))))
  ;;(c-display "res" iota-results)
  (set! iota
        (lambda (n)
          (if (< n 1025)
              (iota-results n)
              (org-iota n)))))


(define (vector-copy vector)
  (copy vector))

;; cl-car and cl-cdr moved to mylint.scm
;;

(define (cl-cddr a)
  (cl-cdr (cl-cdr a)))

(define (cl-cadr a)
  (cl-car (cl-cdr a)))

(define (cl-caddr a)
  (cl-car (cl-cdr (cl-cdr a))))

(define (cl-cadddr a)
  (cl-car (cl-cdr (cl-cdr (cl-cdr a)))))

(***assert*** (butlast '(2)) '())
(***assert*** (butlast '(2 3)) '(2))

(define (map-butlast elements func)
  (if (null? elements)
      elements
      (append (map func
                   (butlast elements))
              (list (last elements)))))

;;(define (second-last elements)
;;  (cadr (reverse elements)))
(define (second-last alist)
  (if (null? (cddr alist))
      (car alist)
      (second-last (cdr alist))))

(***assert*** (second-last '(1 2))
              1)
(***assert*** (second-last '(1 2 3))
              2)
               

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

;; Comparer must return #t if its two arguments are equal and false if not.
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

(define (remove-duplicates less-than equal das-list)
  (remove-duplicates-in-sorted-list equal
                                    (sort das-list less-than)))

#!!
(remove-duplicates < = '(8 2 2 5 7))
(remove-duplicates-in-sorted-list = '(1 1 8 9 10 10))

(< 1 2)

(procedure-source remove-duplicates3)
(gakk)

(string<? "ab" "bb")
!!#


(define (integer-range start-inclusive end-inclusive)
  (map (lambda (i)
         (+ i start-inclusive))
       (iota (1+ (- end-inclusive start-inclusive)))))

(***assert*** (integer-range 0 5)
              '(0 1 2 3 4 5))
(***assert*** (integer-range 5 5)
              '(5))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; define-class ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (define-class-helper class-name hash-table-name methods)
  (define this-method-names (map (lambda (method)
                                   ;;(c-display "METHOD" method)
                                   (<_> 'this-> (keyword->symbol (car method))))
                                 methods))

  (define this-methods (map (lambda (this-method-name method)
                              (define args (cadr method))
                              (define body (cddr method))
                              (if (pair? args)
                                  `(define (,this-method-name ,@args)
                                     ,@body)
                                  `(define (,this-method-name . ,args)
                                     ,@body)))
                            this-method-names
                            methods))

  `(,@this-methods
    ,@(map (lambda (method-name method)             
             `(hash-table-set! ,hash-table-name ,(car method) ,method-name))
           this-method-names
           methods)
    this))

(***assert*** (define-class-helper 'list-and-set 'methods_of_list-and-set
                '((:contains (key)
                             (hash key))
                  
                  (:list () 
                         alist)
                  
                  (:set ()
                        hash)))
              '((define (this->contains key)
                  (hash key))
                (define (this->list)
                  alist)
                (define (this->set)
                  hash)
                (hash-table-set! methods_of_list-and-set :contains this->contains)
                (hash-table-set! methods_of_list-and-set :list this->list)
                (hash-table-set! methods_of_list-and-set :set this->set)
                this))

(c-define-expansion (*<define-class-with-custom-definer>* definer signature . rest)
  (define class-name (string->symbol (list->string (butlast (cdr (string->list (symbol->string (car signature))))))))
  (define new-class-name (<_> 'new_instance_of_ class-name))
  (define args (cdr signature))

  (define body '())
  (define methods '((:add-method! (name func) (add-method! name func))))

  (define hash-table-name (<_> 'methods_of_ class-name))

  (let loop ((rest rest)
             (has-methods #f))
    (when (not (null? rest))
      (cond ((keyword? (car rest))
             (push-back! methods (list (car rest) (cadr rest) (caddr rest)))
             (loop (cdddr rest) #t))
            (else
             (assert (not has-methods))
             (push-back! body (car rest))
             (loop (cdr rest) #f)))))
  
  (append (cons definer (list (cons new-class-name args)))
          `((define ,hash-table-name (make-hash-table ,(+ 2 (length methods)) eq?))
            (define (this methodname . rest)
              ;;(c-display "   CALLING THIS" methodname ,hash-table-name)
              (let ((func (,hash-table-name methodname)))
                ;;(c-display "   CALLING THIS2" methodname func)
                (if func
                    (apply func rest)
                    (error (<-> "Method \"" methodname ,(<-> "\" not found in class " class-name ". methods: ") (map car ,hash-table-name))))))
            (define (add-method! name func)
              (hash-table-set! ,hash-table-name name func)))
          body
          (define-class-helper class-name hash-table-name methods)))

(c-define-expansion (*define-class* signature . rest)
  (append '(<define-class-with-custom-definer>) '(delafina) (list signature)
          rest))

(c-define-expansion (*<new>* class-name . args)
  `(,(<_> 'new_instance_of_ (keyword->symbol class-name)) ,@args))



#!!
(pretty-print
 (define-class-helper 'list-and-set 'hello
   '((:contains x
                (hash key)))))

(pretty-print
 (macroexpand
  (define-class (<test-class>)
    (define (dosomething a b)
      ;;(c-display a b)
      50
      )
    :dosomething x
    (apply dosomething x))))

(pretty-print
 (macroexpand
  (<define-class-with-custom-definer>
   delafina
   (<test-class>)
   (define (dosomething a b) 
     50)
   :dosomething x
   (apply dosomething x))))


(define-class (<test-class>)
  (define (dosomething a b)
    (+ a b))
  :dosomething x
  (apply dosomething x))

(define testinstance (<new> :test-class))

(testinstance :dosomething 5 9)
(testinstance :add-method! :ai (lambda ()
                                 (c-display "ai called")))
(testinstance :ai)

(begin
  (newline)
  (pretty-print
   (macroexpand
    (define-class (<test-class>)
      )))
  (newline))

(begin
  (newline)
  (pretty-print
   (macroexpand (<define-class-with-custom-definer> delafina (<test-class>)))
   )
  (newline))

(define-class (<test-class>)
  (define (dosomething a b)
    (c-display a b this)
    50
    )
  :dosomething x
  (apply dosomething x))

(define testinstance (<new> :test-class))

(testinstance :dosomething 5 9)

!!#

#||
(pretty-print
 (macroexpand
  (define-class (list-and-set :alist
                              :eq-func eq?)
    
    (define hash (make-hash-table (max 1 (length alist)) eq-func))
    
    (for-each (lambda (element)
                (set! (hash element) #t))
              alist)
    
    ;;
    ((:contains (key)
                (hash key))
     
     (:list () 
            alist)
     
     (:set ()
           hash))
    
    
    )
  )
 )
||#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; container ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class (<container> :elements ;; <-- elements must be either a list, a vector, or a hash table (only the keys are used in the hash table, the values must not be #f (impossible in s7)).
                           :eq-func #f) ;; <-- #if #f, automatically determined, but should be set anyway. If elements is a hash table, eq-func is not used.
  
  (define vector #f)
  (define list #f)
  (define hash #f)
  
  (define num-elements #f)

  (define (clear! new-elements new-vector new-list new-hash new-num-elements)
    (set! elements new-elements)
    (set! vector (if new-vector
                     vector
                     (if (vector? new-elements)
                         new-elements
                         #f)))
    (set! list (if new-list
                   new-list
                   (if (pair? new-elements)
                       new-elements
                       #f)))
    (set! hash (if new-hash
                   new-hash
                   (if (hash-table? new-elements)
                       new-elements
                       #f)))
    
    (set! num-elements (if new-num-elements
                           new-num-elements
                           (cond (vector
                                  (vector-length new-elements))
                                 (hash
                                  (hash-table-entries hash))
                                 (else
                                  #f)))))
                                  

  (clear! elements #f #f #f #f)

  (define (get-eq-func)
    (when (not eq-func)
      (set! eq-func (if (= 0 (get-size))
                        morally-equal?
                        (let ((el0 (elements 0)))
                          (cond ((symbol? el0)
                                 eq?)
                                ((number? el0)
                                 =)
                                ((string? el0)
                                 string=?)
                                ((pair? el0)
                                 equal?)
                                (else
                                 morally-equal?))))))
    eq-func)

  (define (get-vector)
    (when (not vector)
      (if (vector? elements)
          (set! vector elements)
          (set! vector (to-vector (get-list)))))
    vector)

  (define (get-list)
    (when (not list)
      (if (hash-table? elements)
          (set! list (map car elements))
          (set! list (to-list elements))))
    list)

  (define (get-hash)
    (when (not hash)
      ;;(c-display "================ elements:" elements ". num:" (get-size) "====================")
      (set! hash (make-hash-table (max 1 (get-size)) (get-eq-func)))
      (for-each (lambda (element)
                  (set! (hash element) #t))
                elements))
    hash)
  
  (define (get-size)
    (when (not num-elements)
      (if hash
          (set! num-elements (hash-table-entries hash))
          (set! num-elements (vector-length (get-vector)))))
    num-elements)
    
  :contains (key)
  ((get-hash) key)

  :intersection (elements2)
  (keep (lambda (el2)
          (this->contains el2))
        elements2)

  :set-difference (elements2) ;; returns all elements in 'elements2' that are not in 'this'.
  (let ((elements2 (if (pair? elements2)
                       (<new> :container elements2)
                       elements2)))
    (if (hash-table? (elements2 :elements))
        (let ((ret (copy (elements2 :elements))))
          (this->for-each (lambda (el2)
                            (hash-table-set! ret (car el2) #f)))
          (<new> :container ret))
        (<new> :container (remove (lambda (el2)
                                    (if (pair? el2)
                                        (this->contains (car el2))
                                        (this->contains el2)))                                          
                                  (elements2 :elements)))))

  :has-all? (elements2)
  (every? (lambda (el2)
            (this->contains el2))
          elements2)

  :for-each (func)
  (cond (list
         (for-each func list))
        (vector
         (for-each func vector))
        (else
         (for-each (lambda (el)
                     (func (car el)))
                   hash)))
                   
  :num-elements ()
  (get-size)

  :size ()
  (get-size)

  :length ()
  (get-size)

  :vector ()
  (get-vector)

  :list ()
  (get-list)

  :hash ()
  (get-hash)
  
  :elements ()
  elements
  
  :get (pos)
  ((get-vector) pos)

  :add! (element) ;; Very inefficient if primary format is hash-table.
  (let ((elements (cons element (get-list))))
    (clear! elements
            #f
            elements
            #f
            (and num-elements
                 (+ 1 num-elements))))
  
  :add-unique! (element) ;; Very inefficient if primary format is not hash-table.
  (when (not (this->contains element))
    (set! (hash element) #t)
    (clear! hash
            #f
            (and list
                 (cons element list))
            hash
            (and num-elements
                 (+ 1 num-elements))))

  :cons (element)
  (<new> :container (cons element (get-list))))

#||
=>

(delafina (new_instance_of_list-and-set :alist
                                        :eq-func eq?)
  (define hash (make-hash-table (max 1 (length alist)) eq-func))
  
  (for-each (lambda (element)
              (set! (hash element) #t))
            alist)

  (define (this->contains key)
    (hash key))
  (define (this->list)
    alist)
  (define (this->set)
    hash)

  (define methods_of_list-and-set
    (hash-table :contains this->contains
                 :list this->list
                 :set this->set))

  (lambda (methodname . rest)
    (let ((func (methods_of_list-and-set methodname)))
      (if func
          (apply func rest)
          (error (<-> "Method \"" methodname "\" not found in class list-and-set"))))))
||#






(define-constant *num-radium-ticks* (<ra> :get-highest-legal-place-denominator))
(define-constant *smallest-radium-tick* (/ 1 *num-radium-ticks*))
(define (-line linenum)
  (- linenum *smallest-radium-tick*))

#||
(define (+line linenum)
  (+ linenum *smallest-radium-tick*))
||#


(define (undo-block block)
  (<ra> :open-undo)
  (try-finally :try block
               :finally (lambda ()
                          (<ra> :close-undo))))

(define (ignore-undo-block block)
  (<ra> :start-ignoring-undo)
  (try-finally :try block
               :finally (lambda ()
                          (<ra> :stop-ignoring-undo))))

;; ensures that (<ra> :update-notes-in-player) is only called after the outermost block is finished.
(define *currently-in-update-notes-after-block-block* #f)
(define (update-notes-after-block block)
  (if *currently-in-update-notes-after-block-block*
      (block)
      (begin
        (set! *currently-in-update-notes-after-block-block* #t)
        (try-finally :try block
                     :finally (lambda ()
                                (set! *currently-in-update-notes-after-block-block* #f)
                                (<ra> :update-notes-in-player))))))
  

(define (draw-plot xs func)
  (define ys (map func xs))
  (define args "")
  (for-each (lambda (x y)
              (set! args (<-> args " " (* 1.0 x) " " (* 1.0 y) )))
            xs ys)
  (system (<-> "plot" args "&"))
  ;;(read-char)
  (c-display "args" args)
  ys)


#||
(define (fibgakk arg)
  (if (= arg 0)
      (<ra> :testsomething arg)
      (fibgakk (1- arg))))

(define (test-crash)
  (fibgakk 50))
||#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Coroutines (missing yield and so forth, but we don't need it it, at least not yet)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct coroutine
  :func #f
  :is-scheduled #f
  :is-running #f
  :please-stop-me #f)

(define (coroutine-alive? coroutine)
  ;;(c-display "coroutine:" coroutine (coroutine :is-scheduled) (coroutine :is-running))
  (or (coroutine :is-scheduled)
      (coroutine :is-running)))

(define (stop-coroutine! coroutine)
  (cond ((coroutine :is-scheduled)
         (let ((func (coroutine :func)))
           (<ra> :remove-schedule func)
           (set! (coroutine :is-scheduled) #f)))
        ((coroutine :is-running)
         (set! (coroutine :please-stop-me) #t))
        (else
         #f)))
        
(define (run-coroutine coroutine args func)
  
  (define (coroutine-helper)
    (set! (coroutine :is-scheduled) #f)
    (when (not (coroutine :please-stop-me))
      (let loop ((args args))
        (set! (coroutine :is-running) #t)
        (let ((pausetime-and-args (eat-errors :try (lambda ()
                                                     (apply func args))
                                              :failure-return-value #f)))
          (set! (coroutine :is-running) #f)
          (if (and (not (coroutine :please-stop-me))
                   pausetime-and-args)
              (let* ((pausetime (car pausetime-and-args))
                     (next-args (cdr pausetime-and-args)))
                (if (> pausetime 0)
                    (schedule-next! next-args pausetime)
                    (loop next-args)))))))
    #f)
  
  (define (schedule-next! next-args pausetime)
    (set! args next-args)
    (set! (coroutine :is-scheduled) #t)
    (<ra> :schedule pausetime coroutine-helper))

  (assert (not (coroutine-alive? coroutine)))
  (set! (coroutine :please-stop-me) #f)
  (set! (coroutine :func) coroutine-helper) ;; Not currently used for anything.
  (schedule-next! args 0))


(let ()
  (<declare-variable> notdefined)
  (***assert-error*** (+ notdefined) 'unbound-variable))

