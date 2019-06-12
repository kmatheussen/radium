(provide 'mylint.scm)


(define (keep func list)
  (if (null? list)
      '()
      (if (func (car list))
          (cons (car list)
                (keep func (cdr list)))
          (keep func (cdr list)))))

;;(keep (lambda (x) (= x 1)) (list 1 3 1 5))


(define (butlast elements)
  (let ((rest (cdr elements)))
    (if (null? rest)
        '()
        (cons (car elements)
              (butlast rest)))))

(define (last das-list) ;; Wouldn't be surprised if this version is slower than '(car (reverse das-list))' though... (but no, this one is much faster with the test below)
  ;;(c-display "last//// " das-list)
  (let loop ((a (car das-list))
             (b (cdr das-list)))
    (if (null? b)
        a
        (loop (car b)
              (cdr b)))))

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

;; redefined later
(define (<-> . args)
  (apply string-append (map (lambda (something)
                              (format #f "~A" something))
                            args)))

;; redefined later
(define (c-display . args)
  (for-each (lambda (arg)
              (display (format #f "~A" arg))
              (display " "))
            args)
  (newline))


(define *schemecodeparser-global-declarations* (if (defined? '*schemecodeparser-global-declarations*)
                                                   *schemecodeparser-global-declarations*
                                                   '()))

(define (declare-variable name)  
  #<undefined>)

(define-expansion (<declare-variable> name)
  (if (not (symbol? name))
      (error (<-> "<declare-variable>: " name " is not a symbol")))
  `(begin
     (if (equal? (rootlet) (curlet))
         (push! *schemecodeparser-global-declarations* ',name ))
     (declare-variable ',name)))

(define (safe-display-txt-as-displayable-as-possible txt)
  (display (ra:get-text-from-html (format #f "~A" txt)))(newline)
  (<declare-variable> safe-add-message-window-txt)
  (if (defined? 'safe-add-message-window-txt)
      (safe-add-message-window-txt txt)))

(define schemecodeparser-varlist '())

(define (schemecodeparser-get-varlist)
  schemecodeparser-varlist)

(define* (schemecodeparser2 expr
                            (elsefunc #f)
                            (symbolfunc #f)
                            (keywordfunc #f)
                            (atomfunc #f)
                            (nullfunc #f)
                            (pairfunc #f)
                            (use-customsymbolhandler? #f)
                            (customsymbolhandler #f)
                            (blockhandler #f)
                            (symbolhandler #f)
                            (varlist '()))

  (if (not (proper-list? expr))
      
      (begin
        (define message (<-> "==========\"schemecodeparser2 Warning\": Expected proper list for expression " expr "========="))
        (if (ra:release-mode)
            (safe-display-txt-as-displayable-as-possible message)
            (error message))
        expr)
      
      (let parse ((varlist varlist)
                  (expr expr))
        
        ;;(c-display "PARSING:" (pp expr) varlist)
        
        (define (blockhandlerfunc varlist expr) ;; expr is a list of expr. Called for the body of lambda, begin, when, etc.

          (define (get-flattened-expr expr) ;; Remove begin
            (cond ((null? expr)
                   '())
                  ((and (pair? (car expr))
                        (eq? 'begin (car (car expr))))
                   (append (get-flattened-expr (cdr (car expr)))
                           (get-flattened-expr (cdr expr))))
                  (else
                   (cons (car expr)
                         (get-flattened-expr (cdr expr))))))

          (set! expr (get-flattened-expr expr))
                      
          (define defines
            (map (lambda (def)
                   (if (pair? (cadr def))
                       (let ((name (car (cadr def))))
                         (if (memq (car def) '(c-define-macro c-define-expansion))
                             (string->symbol (list->string (cdr (butlast (string->list (symbol->string name))))))
                             name))
                       (cadr def)))
                 (keep (lambda (expr)
                         (and (pair? expr)
                              (memq (car expr) '(define define* define-constant c-define-macro define-macro c-define-expansion define-expansion))))
                       expr)))
          
          (define declared (map (lambda (declaration)
                                  (define declared (cadr declaration))
                                  ;;(c-display "DECL:" declared)
                                  (if (pair? declared)
                                      (cadr declared)
                                      declared))
                                (keep (lambda (expr)
                                        (and (pair? expr)
                                             (memq (car expr) '(declare-variable <declare-variable>))))
                                      expr)))

          ;;(if (pair? declared)
          ;;    (c-display "DECLARED:" declared))
          
          (define new-varlist (append declared defines varlist))
          
          ;;(c-display "blockhandlerfunc" varlist (pp expr) defines)
          (if (not blockhandler)
              (map (lambda (expr)
                     (parse new-varlist expr))
                   expr)
              (begin
                (set! schemecodeparser-varlist new-varlist)
                (blockhandler expr))))
  
        ;; Like append, but varlist1 might not be a valid list (in case of optional arguments)
        (define (append-varlists varlist1 varlist2)
          (append (let append ((varlist varlist1))
                    (cond ((null? varlist) varlist)
                          ((not (pair? varlist)) (list varlist))
                          (else (cons (car varlist)
                                      (append (cdr varlist))))))
                  varlist2))
        
        (set! schemecodeparser-varlist varlist)

        ;;(c-display "scp/expr:" expr)
        
        (cond ((and (symbol? expr)
                    symbolfunc)
               (symbolfunc expr))
              ((and (keyword? expr)
                    keywordfunc)
               (keywordfunc expr))
              ((not (pair? expr)) 
               (if atomfunc
                   (atomfunc expr)
                   expr))
              ((null? expr) 
               (if nullfunc
                   (nullfunc expr)
                   expr))
              ((pair? (car expr))
               ;;(c-display "hepp" (car expr))
               (if pairfunc
                   (pairfunc expr)
                   (blockhandlerfunc varlist expr)))
              ((and use-customsymbolhandler?
                    (use-customsymbolhandler? expr))
               (customsymbolhandler expr))
              ((eq? 'lambda (car expr))
               `(lambda ,(cadr expr)
                  ,@(blockhandlerfunc (append-varlists (cadr expr) varlist) (cddr expr))))
              ((eq? 'lambda* (car expr))
               `(lambda* ,(cadr expr)
                         ,@(blockhandlerfunc (append-varlists (map (lambda (parm)
                                                                     (if (pair? parm)
                                                                         (car parm)
                                                                         parm))
                                                                   (cadr expr))
                                                              varlist)
                                             (cddr expr))))
              ((memq (car expr) '(define define-constant c-define-macro define-macro c-define-expansion define-expansion))
               `(,(car expr) ,(cadr expr)
                 ,@(blockhandlerfunc (append-varlists (cadr expr) varlist) (cddr expr))))
              ((eq? 'define* (car expr))
               `(define* ,(cadr expr)
                  ,@(blockhandlerfunc (append-varlists (map (lambda (parm)
                                                              (if (pair? parm)
                                                                  (car parm)
                                                                  parm))
                                                            (cadr expr))
                                                       varlist)
                                      (cddr expr))))
              ((memq (car expr) '(declare-variable <declare-variable>))
               expr)
              ((eq? 'begin (car expr))
               `(begin
                  ,@(blockhandlerfunc varlist (cdr expr))))
              ((eq? 'when (car expr))
               `(when (parse varlist (cadr expr))
                  ,@(blockhandlerfunc varlist (cddr expr))))
              ((eq? 'do (car expr))
               (let* ((newvars (append (map car (cadr expr)) varlist))
                      (first (map (lambda (a)
                                    (let ((second (parse varlist (cadr a))))
                                      `(,(car a) ,second ,@(blockhandlerfunc newvars (cddr a)))))
                                  (cadr expr))))
                 `(do ,first
                      ,@(blockhandlerfunc newvars (cddr expr)))))
              ;; named let
              ((and (eq? 'let (car expr))
                    (symbol? (cadr expr)))
               (let* ((newvars (append (cons (cadr expr) (map car (caddr expr)))
                                       varlist))
                      (vars (map (lambda (a)
                                   `(,(car a) ,@(blockhandlerfunc varlist (cdr a))))
                                 (caddr expr))))
                 `(let ,(cadr expr) ,vars
                       ,@(blockhandlerfunc newvars (cdddr expr)))))
              ((eq? 'let (car expr))
               (let ((vars (map (lambda (a)			      
                                  `(,(car a) ,@(blockhandlerfunc varlist (cdr a))))
                                (cadr expr))))
                 `(let ,vars
                    ,@(blockhandlerfunc (append (map car (cadr expr))
                                                varlist)
                                        (cddr expr)))))
              ((eq? 'let* (car expr))
               (let* ((newvars varlist)
                      (let*vars (map (lambda (a)
                                       (let ((ret `(,(car a) ,@(blockhandlerfunc newvars (cdr a)))))
                                         (push! newvars (car a))
                                         ret))
                                     (cadr expr))))
                 `(let* ,let*vars
                    ,@(blockhandlerfunc (append (map car let*vars)
                                                newvars)
                                        (cddr expr)))))
              ((memq (car expr) '(letrec letrec*))
               (let* ((newvars (append (map car (cadr expr))
                                       varlist))
                      (vars (map (lambda (a)
                                   `(,(car a) ,@(blockhandlerfunc newvars (cdr a)))) ;; Not entily correct for letrec...
                                 (cadr expr)))) 
                 `(,(car expr) ,vars
                    ,@(blockhandlerfunc newvars (cddr expr)))))
              ((or (eq? 'quote (car expr))
                   (eq? 'QUOTE (car expr)))
               expr)
              ((or (eq? 'quasiquote (car expr))
                   (eq? 'QUASIQUOTE (car expr)))
               (letrec* ((unquotes '())
                         (parser (lambda (expr)
                                   ;;(c-display "expr" expr)
                                   (cond ((and (pair? expr)
                                               (eq? 'unquote (car expr)))
                                          (let ((res (parse varlist (cadr expr))))
                                            (cond ((not (pair? res))
                                                   ;;(c-display "heppsann" (cadr expr) res (list 'unquote res))
                                                   (list 'unquote res))
                                                  (else
                                                   (let ((name (gensym)))
                                                     ;;(c-display "got something:" expr (cadr expr))
                                                     (push! unquotes (list name res))
                                                     (list 'unquote name))))))
                                         ((and (pair? expr)
                                               (eq? 'unquote-splicing (car expr)))
                                          (let ((res (parse varlist (cadr expr))))
                                            (cond ((not (pair? res))
                                                   (list 'unquote-splicing res))
                                                  (else
                                                   (let ((name (gensym)))
                                                     ;;(c-display "got something2:" expr (cadr expr))
                                                     (push! unquotes (list name res))
                                                     (list 'unquote-splicing name))))))
                                         ((pair? expr)
                                          ;;(c-display "yes, pair:" expr)
                                          ;;(c-display "car/cadr" (car expr) (cadr expr))
                                          (map parser expr))
                                         
                                         (else
                                          expr))))
                         (newexpr (map parser expr)))
                 ;;(c-display "unquotes:" unquotes)
                 (if (null? unquotes)
                     newexpr
                     `(let ,(reverse! unquotes)
                        ,newexpr))))
              ((eq? 'cond (car expr))
               `(cond ,@(map (lambda (exprs)
                               (let ((test (parse varlist (car exprs))))
                                 `(,test ,@(blockhandlerfunc varlist (cdr exprs)))))
                             (cdr expr))))
              ((eq? 'case (car expr))
               (let ((first (parse varlist (cadr expr))))
                 `(case ,first
                    ,@(map (lambda (expr)
                             `(,(car expr) ,@(blockhandlerfunc varlist (cdr expr))))
                           (cddr expr)))))
              ((and symbolhandler
                    (eq? (car expr) (car symbolhandler)))
               ((cadr symbolhandler) expr))
              (else
               (define (parseithere)
                 `(,(parse varlist (car expr)) ,@(map (lambda (expr)
                                                        (parse varlist expr))
                                                      (cdr expr))))
               (if elsefunc
                   (let ((ret (elsefunc expr)))
                     (if (eq? ret '_schemecodeparser-elsefunc-rejected-it)
                         (parseithere)
                         ret))
                   (parseithere)))))))

#!
(schemecodeparser '(begin `(+ ,a 3)))
(pp(schemecodeparser '(let ()
		     (define ((a)) 9)
		     (+ 2 3 a)
		     (define b 60)
		     (+ a b))))
!#

(define* (schemecodeparser-find-atom-func (elsefunc #f)
                                          (atomfunc #f)
                                          (nullfunc #f)
                                          (pairfunc #f)
                                          (use-customsymbolhandler? #f)
                                          (customsymbolhandler #f)
                                          (blockhandler #f)
                                          (symbolhandler #f)
                                          (varlist '()))
  atomfunc)

#!!
(schemecodeparser-find-atom-func schemecodeparser)
(schemecodeparser-find-atom-func :elsefunc #t :atomfunc 50)
!!#


(define (schemecodeparser . args)
  (let ((expr (car args)))
    (if (not (pair? expr))
        (let ((atomfunc (apply schemecodeparser-find-atom-func (cdr args))))
          (if atomfunc
              (atomfunc expr)
              expr))
        (apply schemecodeparser2 args))))
  
#!!
(schemecodeparser :a)
(schemecodeparser :a
                  :atomfunc (lambda (atom)
                              (c-display "got atom:" atom)
                              'gotit))
!!#

(define *macro-generation* (if (defined? '*macro-generation*)
                               *macro-generation*
                               0))
(define *all-c-macros* (if (defined? '*all-c-macros*)
                           *all-c-macros*
                           (make-hash-table 100 eq?)))

(define (c-define-macro-expansion-internal def body macrotype)
  (if (not (pair? def))
      (error "c-define-macro: \"" def "\" is not a pair"))
  (define def-macro-name (symbol->string (car def)))
  (if (not (string=? "*" (string (car (string->list def-macro-name)))))
      (error (<-> "c-define-macro: The name \"" def-macro-name "\" does not start with a star. It should look like this: *" def-macro-name ".")))
  (if (not (string=? "*" (string (last (string->list def-macro-name)))))
      (error (<-> "c-define-macro: The name \"" def-macro-name "\" does not end with a star. It should look like this: " def-macro-name "*.")))
  (define macro-name (string->symbol (list->string (cdr (butlast (string->list def-macro-name))))))
  `(begin     
     (set! *macro-generation* (1+ *macro-generation*))
     (hash-table-set! *all-c-macros* ',macro-name (lambda ,(cdr def)
                                                    ,@body))
     (,macrotype ,(cons macro-name (cdr def)) ;; (,macro-name ,@(cdr def)) doesn't work in s7.
       ,@body)))
         
(define-macro (c-define-macro def . body)
  (c-define-macro-expansion-internal def body 'define-macro))

(define-macro (c-define-expansion def . body)
  (c-define-macro-expansion-internal def body 'define-expansion))

#!!
(pp (macroexpand (c-define-expansion (*hepp* a . b)
                   (list + woeijfwe (oijnna) 2 ,a ,b))))

(c-define-expansion (*testexp* a d . b)
  (c-display "expanding testexp")
  `(+ woeijfwe (oijnna) ,a ,d ,@b))

(c-define-expansion (*testexp* a b)
  (c-display "expanding testexp2")
  `(+ woeijfwe (oijnna) 2 ,a ,b)))

(define (test-testexp)
  (let ((woeijfwe 5)
        (oijnna (lambda () 6)))
    (testexp 1 2 9 10)))

(test-testexp)


(define-macro (c-define-macro2 def . body)
  `(define-macro def
     ,@body))

(macroexpand (c-define-macro2 (*testexp* a . b)
                              (list ,a ,@b)))

!!#


(c-define-expansion (*push!* list el)
  `(set! ,list (cons ,el ,list)))


;; Returns expr (the exact same one as the input), unless its transformed
(define (c-macroexpand-1 expr)
  (if (or (not (pair? expr))
	  (null? expr)
	  (not (symbol? (car expr))))
      expr
      (let ((qua (hash-table-ref *all-c-macros* (car expr))))
	(if (not qua)
	    (begin
	      ;;(c-display "Error in expand-a-macro. Macro for " expr " Not found.")
	      expr)
	    (apply qua (cdr expr))))))

#!!
(c-macroexpand-1 '(dosomething 50))
(hash-table-ref *all-c-macros* '<declare-variable>)
!!#

(define (c-macroexpand expr)
  ;;(c-display "a-mac" expr)
  (schemecodeparser expr
		    :elsefunc (lambda (expr)
                                ;;(c-display "    Elsefunc:" expr)
                                (let ((topexpand (c-macroexpand-1 expr)))
                                  ;;(c-display "      expr/topexpand" expr topexpand)
                                  (if (eq? expr topexpand)
                                      `(,(car expr) ,@(map c-macroexpand (cdr expr)))
                                      (c-macroexpand topexpand))))))

#!!

(c-macroexpand
 '(begin
    (<declare-variable> hello)))

(c-macroexpand '(c-define-macro (*testexp* a . b)
                  `(list ,a ,@b)))
(c-define-macro (*testexp* a . b)
  `(list ,a ,@b))

(testexp 2 3 4 5)

(c-macroexpand '(let ((a 50))
                  (+ 2 3 a :e 'd d schemecodeparser2)))

(c-define-macro (*dosomething* b)
  `(+ 50 ,b 60))
(c-define-macro (*dosomething2* a . b)
  `(+ 50 ,a ,@b 60))
(c-define-macro (*dosomething3* . c)
  (let ((d c))
    `(dosomething2 (dosomething 5) ,@d)))

(c-define-macro (lettest)
  `(let ((a 5))
     (dosomething 50)))

(c-macroexpand '(lettest))

(dosomething2 3 4 5)
(c-macroexpand '(dosomething2 3 4 5))

(c-macroexpand '(dosomething 77))

(dosomething3 6 7 8)
(c-macroexpand '(dosomething3 6 7 8))


(c-macroexpand
 '(delafina (try-finally :try
                         :failure
                         :failure-return-value *eat-errors-false-unless-failure-is-overridden*
                         :finally (lambda ()
                                    #f))
    (if failure
        #t)))


(define-macro (qq-expand letlist term)
  (let loop ((term term))
    (c-display "term" term letlist)
    (cond ((not (list? term)) term)
	  ((null? term) term)
	  ((eq? 'unquote (car term))
	   (c-display "hmm" (cadr term) letlist)
	   (if (assq (cadr term) letlist)
	       (cadr (assq (cadr term) letlist))
	       term))
	  (else
	   (map loop term)))))

(let ((a 60))
  (qq-expand ((a 50))
	     (let ((b (+ 6 8)))
	       `(+ ,a ,b 6)))
)


(let ((c '(6 7 8)))
  (let ((d c))
    `(dosomething2 (dosomething 5) (unquote-splicing d))))

=> '(dosomething2 (dosomething 5) 6 7 8)


(let ((a '(dosomething 5))
      (b '(6 7 8)))
  `(+ 50 (unquote a) (unquote (apply-values b)) 60))

=> '(+ 50 (dosomething 5) 6 7 8 60)


!!#

;;(define (find-number-of-args-for-function-call)
  

(define (mylint code)
  (define (warn what)
    (define message (<-> "==========\"READER Warning\": " what "========="))
    (newline)
    (newline)
    (display message)
    (when *is-initializing*
      (if (ra:release-mode)
          (safe-display-txt-as-displayable-as-possible message)
          (begin
            (error message)))
      ))
  
  (define (warn-not-defined what for)
    (warn (<-> "\"" what "\" has not been defined " for)))

  (define (is-defined? symbol)
    ;;(c-display "CALLING defined? for" symbol ". is-symbol?" (symbol? symbol))
    ;;(c-display "GLOBALS:" *schemecodeparser-global-declarations*)
    ;;(c-display "varlist:" (schemecodeparser-get-varlist))
    (or (defined? symbol)
        (memq symbol *schemecodeparser-global-declarations*)
        (memq symbol (schemecodeparser-get-varlist))))

  (define (check-atom atom . extrainfo)
    (if (and (symbol? atom)
             (not (is-defined? atom)))
        (warn-not-defined atom (<-> "when parsing atom" (if (null? extrainfo) "" (car extrainfo)))))
    atom)
  
  (define (check-atoms-in-expr expr)
    (if (not (proper-list? expr))
        (warn (<-> "Expected proper list for " expr))
        (let loop ((args (cdr expr)))
          (when (not (null? args))
            (check-atom (car args) (<-> " in " expr))
            (loop (cdr args))))))
  
  (schemecodeparser (c-macroexpand code)
                    :atomfunc check-atom
                    :elsefunc (lambda (expr)
                                ;;(c-display "ELSE:" expr)
                                (let ((funcname (car expr)))
                                  (cond ((not (symbol? funcname))
                                         (warn (<-> "\"" funcname "\" is not a symbol=========")))
                                        ((not (is-defined? funcname))
                                         (warn-not-defined funcname (<-> "in function call " expr)))
                                        (else
                                         (check-atoms-in-expr expr)))
                                  (if (and (defined? funcname)
                                           (not (memq funcname *schemecodeparser-global-declarations*))
                                           (not (memq funcname (schemecodeparser-get-varlist))))
                                      (let ((func (eval funcname)))
                                        (when (and (procedure? func)
                                                 (not (aritable? func (- (length expr) 1)))
                                                 (not (string-starts-with? (symbol->string funcname) "ra:gui_")) ;; because of the the <gui> macro. TODO: Fix the <gui> macro.
                                                 (or (null? (procedure-source func))
                                                     (not (eq? 'lambda* (car (procedure-source func)))))) ;; TODO: Fix this. define*/lambda*/delafina are not checked now.
                                          ;;(c-display "varlist:")
                                          ;;(for-each c-display (schemecodeparser-get-varlist))
                                          (warn (<-> "Wrong number of arguments for \"" funcname "\" in " expr ". Arity:" (arity func)))))))
                                '_schemecodeparser-elsefunc-rejected-it)))
                                         

#!!

(mylint
 '(define (dosmething a b c)
    (+ (hello!))))

(c-macroexpand 'schemecodeparser)
(mylint 'schemecodeparser)

(mylint '(let ((a 50))
           (aloijwef)
           (+ 2 3 a :e 'd d schemecodeparser)))

(c-define-expansion (*testexp* a b)
  `(+ woeijfwe (oijnna) ,a ,b))

(c-define-expansion (*testexp* a b)
  `(+ woeijfwe (oijnna) 2 gakk ,a ,b))

(mylint '(let ((a 50))
           (aloijwef)
           (testexp a rijoije)
           (+ 2 3 a :e 'd d schemecodeparser)))

!!#

(define (get-expression-from-file filename)
  (c-display "FILENAME:" filename)
  (call-with-input-file filename
    (lambda (f)
      (let loop ((result '()))
        (let ((expr (read f)))                 
          (if (eof-object? expr)
              (reverse result)
              (begin
                (display filename) (display ": ") (display expr)
                (newline)
                (if (and #f
                         (pair? expr)
                         (eq? 'load (car expr)))
                    (loop (cons `(begin
                                   ,(get-expression-from-file (cadr expr)))
                                result))
                    (loop (cons expr result))))))))))

#!!
(define (mylint-file filename)
  (mylint `(begin
             ,@(reverse (get-expression-from-file filename)))))
!!#
(define (mylint-file filename)
  (call-with-input-file filename
    (lambda (f)
      (let loop ()
        (let ((expr (read f)))                 
          (if (eof-object? expr)
              #t
              (begin
                (when *is-initializing*
                  (display filename) (display ": ") (display expr)
                  (newline))
                (mylint expr)
                (loop))))))))

#!!
(load "mylint.scm")
(get-expression-from-file "/home/kjetil/radium/bin/scheme/define-match.scm")
(mylint-file "/home/kjetil/radium/bin/scheme/common1.scm")
!!#


(define (mylint-string string)
  (call-with-input-string
   string
   (lambda (f)
     (let loop ((ret #<undefined>))
       (let ((expr (read f)))                 
         (if (eof-object? expr)
             ret
             (begin
               (display expr)
               (newline)
               (loop (mylint expr)))))))))

(define (mylint-and-eval-string string . envlist)
  (define env (if (null? envlist)
                  (rootlet)
                  (car envlist)))
  (call-with-input-string
   string
   (lambda (f)
     (let loop ((ret #<undefined>))
       (let ((expr (read f)))                 
         (if (eof-object? expr)
             ret
             (begin
               ;;(display expr) (newline)
               (mylint expr)
               (loop (eval expr env)))))))))
  
#!!
(mylint-and-eval-string "(c-display 'hello sdf)")
(lambda (a)
  (+ w bg w  ewr ))
(c-display "a")
!!#
