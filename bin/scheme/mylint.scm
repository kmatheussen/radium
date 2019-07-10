(provide 'mylint.scm)

(define (cl-car a)
  (if (pair? a)
      (car a)
      #f))

(define (cl-cdr a)
  (if (pair? a)
      (cdr a)
      #f))


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

(define (flatten l)
  (cond ((null? l)
         '())
        ((pair? l)
         (append (flatten (car l))
                 (flatten (cdr l))))
        (else
         (list l))))

(define (map-improper-list func elements)
  (let loop ((elements elements))
    ;;(c-display "elements:" (object->string elements))
    (cond ((null? elements)
           '())
          ((pair? elements)
           (cons (func (car elements))
                 (loop (cdr elements))))
          (else
           (func elements)))))
  
(define (for-each-improper-list func elements)
  (let loop ((elements elements))
    ;;(c-display "elements:" (object->string elements))
    (cond ((null? elements)
           '())
          ((pair? elements)
           (func (car elements))
           (loop (cdr elements)))
          (else
           (func elements)))))
  
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
(define <-> (if (defined? '<->)
                <->
                (lambda args
                  ;;(display "ARGS:")(display args)(newline)
                  (apply string-append (map (lambda (something)
                                              (format #f "~A" something))
                                            args)))))

;; redefined later
(define c-display (if (defined? 'c-display)
                      c-display
                      (lambda args
                        (for-each (lambda (arg)
                                    (display (format #f "~A" arg))
                                    (display " "))
                                  args)
                        (newline))))

(define (<_> . args)
  (let ((s (apply <-> args)))
    (if (string=? "" s)
        *empty-symbol*
        (string->symbol s))))




(define *mylint-linenum* #f)
(define *mylint-filename* #f)


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

(define *optional-func-have-varargs* 'optional-func-have-varargs)

(define (<optional-func-func> parameters)
  #f)

(define (<optional-hash-table>)
  #f)

(define (is-optional-hash-table? table)
  (or (eq? #f table)
      (hash-table? table)))

(define (is-optional-func? func)
  (or (eq? #f func)
      (procedure? func)))



(define (safe-display-txt-as-displayable-as-possible txt)
  (display (ra:get-text-from-html (format #f "~A" txt)))(newline)
  (<declare-variable> safe-add-message-window-txt)
  (if (defined? 'safe-add-message-window-txt)
      (safe-add-message-window-txt txt)))

(define* (make-schemecodeparser-conf (errorhandler #f)
                                     (num-overriding-local-variables-to-display 1)
                                     (num-overriding-global-variables-to-display 3)
                                     (assert-variable-errors #t))
  (if (not errorhandler)
      (set! errorhandler (lambda (tag . info)
                           (define message (<-> "==========\"Schemecodeparser error: " info "========="))
                           (c-display message)
                           (error tag message))))
  (define (display-overriding is-local info)
    (define key (if is-local
                    :num-overriding-local-variables-to-display
                    :num-overriding-global-variables-to-display))
    (set! (conf key) (- (conf key) 1))
    (when (>= (conf key) 0)
      (display info)
      (when #f
        (display "   num:")
        (display (conf key))
        (display ". key:")
        (display key)
        (display ". is-local?")(display is-local))
      (if (= (conf key) 0)
          (display "....(suppressing rest)....."))
      (newline)))
  (define conf (hash-table :errorhandler errorhandler
                           :num-overriding-local-variables-to-display num-overriding-local-variables-to-display
                           :num-overriding-global-variables-to-display num-overriding-global-variables-to-display
                           :assert-variable-errors assert-variable-errors
                           :display-overriding display-overriding))
  conf)

                              
         

(define-constant *schemecodeparser-var-var-type* 'schemecodeparser-define-var-type)
(define-constant *schemecodeparser-var-func-type* 'schemecodeparser-define-func-type)
(define-constant *schemecodeparser-var-declared-type* 'schemecodeparser-define-declared-type)
(define-constant *schemecodeparser-var-parameter-type* 'schemecodeparser-define-parameter-type)

(define (schemecodeparser-assert-symbol varname conf)
  ;;(c-display "        VARNAME/SYMBOL?" varname (symbol? varname) ". error-handler:" errorhandler ". " (procedure-source errorhandler))
  (when (not (symbol? varname))
    (conf :errorhandler 'schemecodeparser-varname-not-a-symbol varname)))
  
(define (schemecodeparser-create-func-var varname signature conf)
  ;;(c-display "create-func SIGNATURE:" varname (object->string signature))
  (schemecodeparser-assert-symbol varname conf)
  (list varname *schemecodeparser-var-func-type* signature))

(define (schemecodeparser-create-var-var varname value conf)
  ;;(c-display "create-var-var VALUE:" (object->string value))
  (schemecodeparser-assert-symbol varname conf)  
  (if (and (pair? value)
           (not (null? value))
           (memq (car value) '(lambda lambda* <optional-func-func> <optional-hash-table> make-hash-table hash-table copy-hash)))
      (cond ((eq? (car value) '<optional-hash-table>)
             (schemecodeparser-create-func-var varname (cons 'key 'args) conf))
            ((eq? (car value) '<optional-func-func>)
             (assert (pair? (cadr value)))
             (let ((func (cadr value)))
               (assert (eq? 'lambda* (car func)))
               (schemecodeparser-create-func-var varname (cadr func) conf)))
            ((memq (car value) '(make-hash-table hash-table copy-hash))
             (schemecodeparser-create-func-var varname (cons 'key 'args) conf))
            (else
             (schemecodeparser-create-func-var varname (cadr value) conf)))
      (list varname *schemecodeparser-var-var-type* value)))
         
(define (schemecodeparser-create-declared-var varname conf)
  (schemecodeparser-assert-symbol varname conf)
  (list varname *schemecodeparser-var-declared-type*))

(define (schemecodeparser-create-parameter-var varname conf)
  ;;(c-display "VARNAME:" varname)
  ;;(c-display "ERRORHANDLER:" errorhandler)
  (schemecodeparser-assert-symbol varname conf)
  (list varname *schemecodeparser-var-parameter-type*))

(define (schemecodeparser-get-var-type var)
  (car var))
  
(define (schemecodeparser-get-var-var-value var)
  (assert (eq? (schemecodeparser-get-var-type var) *schemecodeparser-var-var-type*))
  (cadr var))
  
(define (schemecodeparser-get-var-func-signature var)
  (assert (eq? (schemecodeparser-get-var-type var) *schemecodeparser-var-func-type*))
  (cadr var))
  
(define schemecodeparser-varlist '())

(define (schemecodeparser-get-varnames)
  (map car schemecodeparser-varlist))

(define* (schemecodeparser-get-var varname (varlist schemecodeparser-varlist))
  (let ((entry (assq varname varlist)))
    (and entry
         (cdr entry))))

(define* (schemecodeparser-has-varname? varname (varlist schemecodeparser-varlist))
  (assq varname varlist))

(define (schemecodeparser-warn-about-defined-vars new-vars old-vars conf)
  (for-each (lambda (var)
              (define varname (car var))
              (define old-var (schemecodeparser-get-var varname))
              (if old-var
                  (conf :display-overriding
                        #t
                        (<-> "*************** NOTE: schemecodeparser: \"" varname "\" has already been defined locally. "
                             "Old: " old-var
                             ". New: " var
                             *mylint-filename* ": " *mylint-linenum*))
                  (if (and ;;(not (null? old-vars)) ;; Don't need to give warning when redefining a global symbol.
                       (defined? varname (rootlet)))
                      (conf :display-overriding
                            #f
                            (<-> "*************** WARNING! schemecodeparser: \"" varname "\" has already been defined globally. " *mylint-filename* ": " *mylint-linenum*)))))
            new-vars))


(define (schemecodeparser-get-type-for-pair expr varlist)
  ;;(c-display "get-type-for-pair:" expr varlist)
  (define funcname (car expr))
  (cond ((pair? funcname)
         (schemecodeparser-get-type-for-pair funcname varlist))
        ((symbol? funcname)
         (let ((var (schemecodeparser-get-var funcname varlist)))
           ;;(c-display "FUNCNAME:" funcname)
           (cond (var
                  (if (eq? (schemecodeparser-get-var-type var) *schemecodeparser-var-func-type*)
                      (begin
                        ;;(schemecodeparser-get-func-return-type var)
                        #f)
                      #f))
                 ((eq? funcname 'quote)
                  (if (pair? (cadr expr))
                      'list?
                      'symbol?))
                 (else
                  (let ((signature (and (defined? funcname)
                                        (signature (eval funcname (rootlet))))))
                    ;;(c-display "SIGN:" signature)
                    (if (not signature)
                        #f
                        (car signature)))))))
        (else
         (c-display "*************** WARNING! schemecodeparser-get-type-for-pair: Unknown type for:" expr)
         #f)))

(define* (schemecodeparser-get-type expr (varlist schemecodeparser-varlist))
  ;;(c-display "get-type:" expr varlist)
  (cond ((null? expr)
         'null?)
        ((pair? expr)
         (schemecodeparser-get-type-for-pair expr varlist))
        ((symbol? expr)
         (if #t
             #f
             (let ((var (schemecodeparser-get-var expr varlist)))
               (cond (var
                      #f)
                     (cond ((eq? (schemecodeparser-get-var-type var) *schemecodeparser-var-var-type*)
                            (schemecodeparser-get-type (schemecodeparser-get-var-var-value var) varlist))
                           ((eq? (schemecodeparser-get-var-type var) *schemecodeparser-var-func-type*)
                            'procedure?)
                           ((eq? (schemecodeparser-get-var-type var) *schemecodeparser-var-declared-type*)
                            #f)
                           ((eq? (schemecodeparser-get-var-type var) *schemecodeparser-var-parameter-type*)
                            #f)
                           (else
                            (assert #f)))
                     ((defined? expr (rootlet))
                      (schemecodeparser-get-type (eval expr) '()))
                     (else
                      #f)))))
        ((string? expr)
         'string?)
        ((char? expr)
         'char?)
        ((eq? expr #t)
         'boolean?)
        ((eq? expr #f)
         'boolean?)
        ((vector? expr)
         'vector?)
        ((undefined? expr)
         'undefined?)
        ((unspecified? expr)
         'unspecified?)
        ((integer? expr)
         'integer?)
        ((real? expr)
         'real?)
        ((rational? expr)
         'rational?)        
        ((procedure? expr)
         'procedure?)
        ((hash-table? expr)
         'hash-table?)
        (else
         ;;(c-display "schemecodeparser-get-type: Unknown type: " (object->string expr))
         ;;(c-display "varlist:" varlist)
         (error 'schemecodeparser-unknown-type expr))))

#!!
(schemecodeparser-get-type 'a '())
!!#

(***assert*** (schemecodeparser-get-type '(+ 2 3) (list (schemecodeparser-create-func-var '+ '(a b) (make-schemecodeparser-conf))))
              #f)

(***assert*** (schemecodeparser-get-type '(+ 2 3) (list (schemecodeparser-create-func-var '- '(a b) (make-schemecodeparser-conf))))
              'number?)


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
                            (conf (make-schemecodeparser-conf))
                            (varlist '()))

  (define (warn expr . args)
    (define message (<-> "==========\"schemecodeparser2 Warning " *mylint-filename* ": " *mylint-linenum* ": " (apply <-> args) " for expression " expr "========="))
    (if (ra:release-mode)
        (safe-display-txt-as-displayable-as-possible message)
        (error 'schemecodeparser-error message)))

  (if (and (not (proper-list? expr))
           (or (null? expr)
               (not (symbol? (car expr)))))
               ;;;(not (*all-c-macros* (car expr)))
               ;;;(not (eq? '<optional-func2> (car expr)))))
      (begin
        (warn expr "Expected proper list")
        expr)
      
      (let parse ((varlist varlist)
                  (expr expr))

        (when #f
          (c-display "\nPARSING:" (length varlist))
          (if (keyword? expr)
              (display expr)
              (pretty-print expr))              
          (newline)
          ;;(display varlist)
          )
        
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
                   ;;(c-display "DEF:" (object->string def))
                   (if (pair? (cadr def))
                       (let* ((name1 (car (cadr def)))
                              (name (if (memq (car def) '(c-define-macro c-define-expansion))
                                        (string->symbol (list->string (cdr (butlast (string->list (symbol->string name1))))))
                                        name1))
                              (signature (cdr (cadr def))))
                         (schemecodeparser-create-func-var name signature conf))
                       (let ((name (cadr def))
                             (value (caddr def)))
                         (schemecodeparser-create-var-var name value conf))))
                 (keep (lambda (expr)
                         (and (pair? expr)
                              (memq (car expr) '(define define* define-constant c-define-macro define-macro c-define-expansion define-expansion))))
                       expr)))

          ;;(c-display "IM HERE")
          
          (define declared (map (lambda (declaration)
                                  (define declared (cadr declaration))
                                  ;;(c-display "DECL:" declared)
                                  (schemecodeparser-create-declared-var (if (pair? declared)
                                                                            (cadr declared)
                                                                            declared)
                                                                        conf))
                                (keep (lambda (expr)
                                        (and (pair? expr)
                                             (memq (car expr) '(declare-variable <declare-variable>))))
                                      expr)))

          ;;(c-display "blockhandlerfunc expr:" expr)
          ;;(c-display "DEFINES:" defines)
          ;;(if (pair? declared)
          ;;    (c-display "DECLARED:" declared))

          ;;(schemecodeparser-warn-about-defined-vars declared varlist)
          (schemecodeparser-warn-about-defined-vars defines varlist conf)
          
          (define new-varlist (append declared defines varlist))
          
          ;;(c-display "blockhandlerfunc" varlist (pp expr) defines)
          (if (not blockhandler)
              (map (lambda (expr)
                     (parse new-varlist expr))
                   expr)
              (begin
                (set! schemecodeparser-varlist new-varlist)
                (blockhandler expr))))
  
        (define (add-parameters-to-varlist parameters varlist)
          (define (treatpar par)
            (schemecodeparser-create-parameter-var (if (pair? par)
                                                       (car par)
                                                       par)
                                                   conf))
          (define new-vars (let append ((parameters parameters))
                             (cond ((null? parameters)
                                    '())
                                   ((not (pair? parameters))
                                    (list (treatpar parameters)))
                                   (else
                                    (cons (treatpar (car parameters))
                                          (append (cdr parameters)))))))
          ;;(c-display "NEW-VARS:" new-vars)
          (schemecodeparser-warn-about-defined-vars new-vars varlist conf)
          (append new-vars
                  varlist))
        
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
              ((memq (car expr) '(lambda lambda*))
               (if (null? (cddr expr))
                   (warn expr (car expr) ": Empty body"))
               `(,(car expr) ,(cadr expr)
                 ,@(blockhandlerfunc (add-parameters-to-varlist (cadr expr) varlist)
                                     (cddr expr))))
              ((memq (car expr) '(define define* define-constant c-define-macro define-macro c-define-expansion define-expansion))
               (if (null? (cddr expr))
                   (warn expr (car expr) ": Empty body"))
               `(,(car expr) ,(cadr expr)
                 ,@(blockhandlerfunc (if (pair? (cadr expr))
                                         (let* ((varname (car (cadr expr)))
                                                (parameters (cdr (cadr expr)))
                                                (varlist2 (if (or #f (null? varlist)) ;; TODO: Fix. If toplevel, the function itself has not previously been added to "defines" in "blockhandlerfunc".
                                                              (cons (schemecodeparser-create-func-var varname parameters conf)
                                                                    varlist)
                                                              (begin
                                                                (when (and (conf :assert-variable-errors)
                                                                           (not (schemecodeparser-has-varname? varname varlist)))
                                                                  (c-display "VARNAME:" varname "VARLIST:" varlist ". expr:" expr)
                                                                  (assert #f))
                                                                varlist))))
                                           (add-parameters-to-varlist parameters varlist2))
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
               (let* ((newvars (let ((das-new-vars (map (lambda (dovar)
                                                          (schemecodeparser-create-var-var (car dovar) (cadr dovar) conf))
                                                        (cadr expr))))
                                 (schemecodeparser-warn-about-defined-vars das-new-vars varlist conf)
                                 (append das-new-vars varlist)))
                      (first (map (lambda (a)
                                    (let ((second (parse varlist (cadr a))))
                                      `(,(car a) ,second ,@(blockhandlerfunc newvars (cddr a)))))
                                  (cadr expr))))
                 `(do ,first
                      ,@(blockhandlerfunc newvars (cddr expr)))))
              ;; named let
              ((and (eq? 'let (car expr))
                    (symbol? (cadr expr)))
               (let* ((newvars (let ((das-new-vars (cons (schemecodeparser-create-func-var (cadr expr)
                                                                                           (map car (caddr expr))
                                                                                           conf)
                                                         (map (lambda (letvar)
                                                                (schemecodeparser-create-var-var (car letvar) (cadr letvar) conf))
                                                              (caddr expr)))))
                                 (schemecodeparser-warn-about-defined-vars das-new-vars varlist conf)
                                 (append das-new-vars varlist)))
                      (vars (map (lambda (a)
                                   `(,(car a) ,@(blockhandlerfunc varlist (cdr a))))
                                 (caddr expr))))
                 `(let ,(cadr expr) ,vars
                       ,@(blockhandlerfunc newvars (cdddr expr)))))
              ((eq? 'let (car expr))
               (let ((vars (map (lambda (a)			      
                                  `(,(car a) ,@(blockhandlerfunc varlist (cdr a))))
                                (cadr expr))))
                 (define new-vars (map (lambda (letvar)
                                         (schemecodeparser-create-var-var (car letvar) (cadr letvar) conf))
                                       (cadr expr)))
                 ;;(c-display "NEW:" new-vars)
                 ;;(c-display "OLD:" varlist)
                 (schemecodeparser-warn-about-defined-vars new-vars varlist conf)
                 `(let ,vars
                    ,@(blockhandlerfunc (append new-vars varlist)
                                        (cddr expr)))))
              ((eq? 'let* (car expr))
               (let* ((newvars varlist)
                      (let*vars (map (lambda (let*var)
                                       (let* ((ret `(,(car let*var) ,@(blockhandlerfunc newvars (cdr let*var))))
                                              (new-var (schemecodeparser-create-var-var (car let*var) (cadr let*var) conf)))
                                         (schemecodeparser-warn-about-defined-vars (list new-var) newvars conf)
                                         (push! newvars new-var)
                                         ret))
                                     (cadr expr))))
                 `(let* ,let*vars
                    ,@(blockhandlerfunc newvars ;;(append (map car let*vars) newvars)
                                        (cddr expr)))))
              ((memq (car expr) '(letrec letrec*))
               (let* ((newvars (let ((dasnewvars (map (lambda (letrecvar)
                                                        (schemecodeparser-create-var-var (car letrecvar) (cadr letrecvar) conf))
                                                      (cadr expr))))
                                 (schemecodeparser-warn-about-defined-vars dasnewvars varlist conf)
                                 (append dasnewvars
                                         varlist)))
                      (vars (map (lambda (a)
                                   `(,(car a) ,@(blockhandlerfunc newvars (cdr a)))) ;; Not entirely correct for letrec...
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
               (define (parseithere expr)
                 (map (lambda (expr)
                        (parse varlist expr))
                      expr))
;                 `(,(parse varlist (car expr)) ,@(map (lambda (expr)
;                                                        (parse varlist expr))
;                                                      (cdr expr))))
               (if elsefunc
                   (let ((result (elsefunc expr)))
                     (if (or (eq? result expr)
                             (and (pair? result)
                                  (not (null? result))
                                  (eq? (car result) (car expr))))
                         (parseithere result)
                         (parse varlist result)))
                   (parseithere expr)))))))


#!
(schemecodeparser 50)
(schemecodeparser '(begin `(+ ,a 3)))
(schemecodeparser '(begin
                     (let ((list 50))
                       (lambda (list2)
                         20))))

(schemecodeparser '(let ((list 50)) 20))

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
                                          (conf #f)
                                          (varlist '()))
  atomfunc)

#!!
(schemecodeparser-find-atom-func schemecodeparser)
(schemecodeparser-find-atom-func :elsefunc #t :atomfunc 50)
(+ 2 3)
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
(schemecodeparser '((define ())))
(schemecodeparser '((c-define-macro (*a*))))
!!#

(when (and #t (not (ra:release-mode)))
  (***assert-error*** (schemecodeparser '((lambda ())))
                      'schemecodeparser-error)
  (***assert-error*** (schemecodeparser '((lambda* ())))
                      'schemecodeparser-error)
  (***assert-error*** (schemecodeparser '((define (a))))
                      'schemecodeparser-error)
  (***assert-error*** (schemecodeparser '((define* (a))))
                      'schemecodeparser-error)
  (***assert-error*** (schemecodeparser '((define-constant (a))))
                      'schemecodeparser-error)
  (***assert-error*** (schemecodeparser '((c-define-macro (*a*))))
                      'schemecodeparser-error)
  (***assert-error*** (schemecodeparser '((define-macro (a))))
                      'schemecodeparser-error)
  (***assert-error*** (schemecodeparser '((c-define-expansion (*a*))))
                      'schemecodeparser-error)
  (***assert-error*** (schemecodeparser '((define-expansion (a))))
                      'schemecodeparser-error)
  )

(when (and #t (not (ra:release-mode)))
  (***assert-error*** (schemecodeparser (cons 1 2))
                      'schemecodeparser-error))

(when (and #t (not (ra:release-mode)))
  (***assert*** (schemecodeparser (list 1 2))
                (list 1 2)))



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
  `(+ woeijfwe (oijnna) 2 ,a ,b))

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


(c-define-expansion (*define2* name correct-type? value)
  (define s (gensym "s"))
  (define v (gensym "v"))
  (if (ra:release-mode)
      `(define ,name ,value)
      `(begin     
         (define ,name (let ((,v ,value))
                         (if (,correct-type? ,v)
                             ,v
                             (error 'wrong-type (list "For " ',name ': ,v)))))
         (set! (setter ',name)
               (lambda (,s ,v)
                 (if (,correct-type? ,v)
                     ,v
                     (error 'wrong-type (list "For " ',name "New value:" ,v "Prev value:" ,name))))))))

(c-define-expansion (*define2-without-init-check* name correct-type? value)
  (define s (gensym "s"))
  (define v (gensym "v"))
  (if (ra:release-mode)
      `(define ,name ,value)
      `(begin     
         (define ,name ,value)
         (set! (setter ',name)
               (lambda (,s ,v)
                 (if (,correct-type? ,v)
                     ,v
                     ;;(list "For " ',name "New value:" ,v "Prev value:" ,name))))))))
                     (error 'wrong-type (list "For " ',name "New value:" ,v "Prev value:" ,name))))))))
#!!
(let ((list (lambda args
              (for-each c-display args))))
  (define2-without-init-check hello string? "hel")
  (set! hello "gakk")
  hello)

(eval
 (c-macroexpand 
  '(let ()
     (define2-without-init-check hello string? "hel")
     (set! hello 50)
     hello)))

(let ((a 3))
  (define-macro (mac b)
    `(with-let (inlet 'b ,b (funclet mac))
       (+ a b)))       ; definition-time "a", call-time "b"
  (define-macro (mac-1 b)
    `(+ a ,b))         ; call-time "a" and "b"
  (let ((a 32))
    (list (mac 1) 
	  (mac-1 1))))

(let ()
  (varlet (curlet) 'daslistsym list)
  (curlet)
  (daslistsym 2 3 4))

!!#
 
(c-define-expansion (*<optional-func>* parameters)
  `(<optional-func-func> (lambda* ,parameters #f)))
;  (if (and (not (null? parameters))
;           (let ((l (last parameters)))
;             (and (not (pair? l))
;                  (eq? :rest l))))
;      (if (null? (cdr parameters))
;          `(<optional-func-func> (lambda rest #t))
;          `(<optional-func-func> (lambda (,@(butlast parameters) . rest) #t)))
;      `(<optional-func-func> (lambda ,parameters #t))))

(define-expansion (define-optional-hash-table name)
  `(define2-without-init-check ,name is-optional-hash-table? (<optional-hash-table>)))
         
(define-expansion (define-optional-func name args)
  `(define2-without-init-check ,name is-optional-func? (<optional-func> ,args)))
         

#!!
(c-macroexpand '(<optional-func> a 2 :rest))
(c-macroexpand '(<optional-func>))
(c-macroexpand '(<optional-func> a 9 c))
!!#


(c-define-expansion (*push!* list el)
  `(set! ,list (cons ,el ,list)))


;; Returns 'c-macroexpand-1-same-as-input unless its transformed
(define (c-macroexpand-2 expr)
  ;;(c-display "c-macro2:" expr)
  (if (or (not (pair? expr))
	  (null? expr)
	  (not (symbol? (car expr))))
      'c-macroexpand-1-same-as-input
      (let ((qua (hash-table-ref *all-c-macros* (car expr))))
	(if (not qua)
	    (begin
	      ;;(c-display "Error in expand-a-macro. Macro for " expr " Not found.")
	      ;;expr
              'c-macroexpand-1-same-as-input
              )
            (apply qua (let loop ((parameters (cdr expr)))
                         (cond ((null? parameters)
                                '())
                               ((not (pair? parameters))
                                (list :c-macroexpand-rest parameters))
                               (else
                                (cons (car parameters)
                                      (loop (cdr parameters)))))))))))

(define (c-macroexpand-1 expr)
  (let ((topexpand (c-macroexpand-2 expr)))
    ;;(c-display "      expr:" topexpand "\ntoexpand:" topexpand "\n\n")
    (if (and (symbol? topexpand)
             (eq? topexpand 'c-macroexpand-1-same-as-input))
        expr
        (c-macroexpand-1 topexpand))))

#!!
(c-macroexpand-1 '(dosomething 50))
(hash-table-ref *all-c-macros* '<declare-variable>)
!!#

(define* (c-macroexpand expr (errorhandler #f))
  ;;(c-display "   MACROEXP: " expr)
  (schemecodeparser expr
                    :elsefunc c-macroexpand-1
                    :conf (make-schemecodeparser-conf :errorhandler errorhandler
                                                      :num-overriding-local-variables-to-display 0
                                                      :num-overriding-global-variables-to-display 0
                                                      :assert-variable-errors #f)))

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


(define (mylint-lambda*-call full-parameters funccall)
  (call-with-exit
   (lambda (return)

     ;;(c-display "full-parameters1:" full-parameters)
     
     ;; quick-fix for dotted args
     (if (not (proper-list? full-parameters))
         (set! full-parameters (let loop ((parameters full-parameters))
                                 (if (symbol? parameters)
                                     (list parameters)
                                     (cons (car parameters)
                                           (loop (cdr parameters)))))))

     ;;(c-display "full-parameters:" full-parameters)
     
     (define (get-parnames parameters)
       (map (lambda (parameter)
              (if (symbol? parameter)
                  parameter
                  (car parameter)))
            parameters))

     (define funcname (car funccall))
     (define args (cdr funccall))
     (define parameter-names (get-parnames full-parameters))
     
     (define (fail . args)
       (return (<-> (apply <-> args)
                    " in call to \"" funcname "\"\n"
                    ". Call: " (pp funccall) "\n"
                    ". Parameters of " funcname ": " (pp full-parameters) ".")))
     
     (define (assert-all-remaining-parameters-have-default-values parameters has-had-a-default-value)
       (if has-had-a-default-value
           #t
           (let loop ((parameters parameters))
             (define parameter (cl-car parameters))
             ;;(c-display "par/pars2" parameter parameters (symbol? parameter))
             (cond ((null? parameters)
                    #t)
                   ((symbol? parameter)
                    (fail "Missing argument :" parameter))
                   (else
                    (assert (pair? parameter))
                    #t)))))

     (define (has-keyword? keyword)
       (memq (keyword->symbol keyword) parameter-names))

     (define (assert-keyword-in-parameters keyword)
       ;;(c-display "parameter-names:" parameter-names ". " keyword)
       (if (not (has-keyword? keyword))
           (fail "Unknown keyword " keyword)))

     (define (treat-keyword-arg keyword parameters has-had-a-default-value kont)
       (define argname (keyword->symbol keyword))
       (let loop ((parameters parameters)
                  (has-had-a-default-value has-had-a-default-value))
         (define parameter (car parameters))
         ;;(c-display "p/h/p/argname:" parameters has-had-a-default-value parameter argname)
         (if (symbol? parameter)
             (if (and (not has-had-a-default-value)
                      (not (eq? parameter argname)))
                 (fail "Keyword " keyword " is in wrong position. Expected " (symbol->keyword parameter) ". Correct order: " (map symbol->keyword parameter-names) ".")
                 (kont (cdr parameters)
                       has-had-a-default-value))
             (let ((parname (car parameter)))
               (if (eq? parname argname)
                   (kont (cdr parameters)
                         #t)
                   (loop (cdr parameters)
                         #t))))))
              
     (let loop ((args args)
                (parameters full-parameters)
                (used-keywords '())
                (has-had-a-default-value #f)
                (has-had-a-keyword #f)
                (argnum 0))
       (define arg (cl-car args))
       ;;(c-display "args/parms" args parameters)

       (when (keyword? arg)
         (if (memq arg used-keywords)
             (fail "Keyword " arg " set more than once"))         
         (assert-keyword-in-parameters arg)
         (if (not (memq (keyword->symbol arg) (get-parnames parameters)))
             (fail "Keyword " arg " is in wrong position. It should have been defined earlier. Correct order: " (map symbol->keyword parameter-names) ".")))

       (cond ((null? parameters)
              (if (not (null? args))
                  (fail "Too many arguments")
                  ""))
             ((null? args)
              (assert-all-remaining-parameters-have-default-values parameters has-had-a-default-value)
              "")
             ((keyword? arg)
              (treat-keyword-arg arg
                                 parameters has-had-a-default-value
                                 (lambda (parameters has-had-a-default-value)
                                   (if (null? (cdr args))
                                       (fail "Missing value for parameter " arg))
                                   (loop (cddr args)
                                         parameters
                                         (cons arg used-keywords)
                                         has-had-a-default-value
                                         #t
                                         (+ 1 argnum)))))
             (has-had-a-keyword
              (fail "Argument #" argnum " is not a keyword"))
             (else
              (loop (cdr args)
                    (cdr parameters)
                    used-keywords
                    (or has-had-a-default-value
                        (pair? (car parameters)))
                    has-had-a-keyword
                    (+ 1 argnum)))))
                        
     "")))


(define (mylint-lambda*-call2 funcname funccall)
  (define func (eval funcname))
  (define source (procedure-source func))
  (define full-parameters (cadr source))
  (mylint-lambda*-call full-parameters funccall))
  
      
  

(define* (test-define*func d c (a 2) e)
  a)

(define* (test-define*func2 (a #f)
                             b
                             c
                             d
                             (e #f))
  50)

(define* (test-define*func3 instrument-id 
                            strips-config 
                            background-color 
                            min-height 
                            use-single-letters 
                            stack-horizontally
                            (set-fixed-size #t))
  50)


(***assert*** (mylint-lambda*-call2 test-define*func3 '(test-define*func3 "instrument-id"
                                                                         "strips-config"
                                                                         "background-color"
                                                                         "min-height"
                                                                         "use-single-letters"
                                                                         #f
                                                                         ))
              "")

(***assert-custom-comp*** string-starts-with?
                          (mylint-lambda*-call2 test-define*func2 '(test-define*func2 :b 1
                                                                                     :c 2
                                                                                     :e 3
                                                                                     :d 4))
                          "Keyword :d is in wrong position. It should have been defined earlier. Correct order:")

(***assert-custom-comp*** string-starts-with?
                          (mylint-lambda*-call2 test-define*func '(test-define*func 1 2 :a 20 9))
                          "Argument #3 is not a keyword")

(***assert*** (mylint-lambda*-call2 test-define*func '(test-define*func 1 2 :e 50))
              "")

(***assert-custom-comp*** string-starts-with?
                          (mylint-lambda*-call2 test-define*func '(test-define*func 1 2 :e 20 :a 9))
                          "Keyword :a is in wrong position. It should have been defined earlier. Correct order:")
  
(***assert-custom-comp*** string-starts-with?
                          (mylint-lambda*-call2 test-define*func '(test-define*func 1 2 :e 20 :a 9))
                          "Keyword :a is in wrong position. It should have been defined earlier. Correct order:")

(***assert-custom-comp*** string-starts-with?
                          (mylint-lambda*-call2 test-define*func '(test-define*func :a 20 9))
                          "Keyword :a is in wrong position. Expected :d.")

(***assert-custom-comp*** string-starts-with?
                          (mylint-lambda*-call2 test-define*func '(test-define*func 1 :wef 20 9))
                          "Unknown keyword :wef in call to")

(***assert-custom-comp*** string-starts-with?
                          (mylint-lambda*-call2 test-define*func '(test-define*func 1 2 :a 20 :a 9))
                          "Keyword :a set more than once in call to")

(***assert-custom-comp*** string-starts-with?
                          (mylint-lambda*-call2 test-define*func '(test-define*func 1 2 :a 20 :d 9))
                          "Keyword :d is in wrong position. It should have been defined earlier. Correct order:")

(***assert-custom-comp*** string-starts-with?
                          (mylint-lambda*-call2 test-define*func '(test-define*func :f 20))
                          "Unknown keyword :f in call to")

(***assert-custom-comp*** string-starts-with?
                          (mylint-lambda*-call2 test-define*func '(test-define*func 1))
                          "Missing argument :c in call to")

(***assert*** (mylint-lambda*-call2 test-define*func '(test-define*func 1 2))
              "")

(***assert*** (mylint-lambda*-call2 test-define*func '(test-define*func 1 2 :a 20))
              "") ;;Missing argument for :e in call to")

(***assert*** (mylint-lambda*-call2 test-define*func '(test-define*func 2 3 4))
              "")

(***assert-custom-comp*** string-starts-with?
                          (mylint-lambda*-call2 test-define*func '(test-define*func 2 3 4 5 6))
                          "Too many arguments in call to")





(define (mylint-lambda-call signature funccall)
  (call-with-exit
   (lambda (return)
     (define funcname (car funccall))
     (define call (cdr funccall))
     (define num-args (length call))
     (define allows-infinite-args #f)
     (define min-args (let loop ((signature signature)
                                 (n 0))
                        (if (null? signature)
                            n
                            (if (pair? signature)
                                (let ((varname (car signature)))
                                  (if (not (symbol? varname))
                                      (return (<-> "Error in function \"" funcname "\": Parameter #" n " is not a symbol. Signature '" (object->string `(lambda ,signature ...)) "'")))
                                  (loop (cdr signature)
                                        (+ n 1)))
                                (begin
                                  (set! allows-infinite-args #t)
                                  n)))))
     '(c-display (not allows-infinite-args)
                 (not (= num-args min-args))
                 num-args min-args)
     
     (cond ((and (not allows-infinite-args)
                 (not (= num-args min-args)))
            (<-> "Expected " min-args " argument(s) for call to " funcname ", found " num-args " args in " (object->string funccall)))
           ((< num-args min-args)
            (<-> "Expected at least " min-args " argument(s) for call to " funcname ", found " num-args " args in " (object->string funccall)))
           (else
            "")))))
     

(***assert*** (mylint-lambda-call '() '(call))
              "")

(***assert-custom-comp*** string-starts-with?
                          (mylint-lambda-call '(5) '(call))
                          "Error in function \"call\": Parameter #0 is not a symbol. Signature '(lambda (5) ...)'")

(***assert-custom-comp*** string-starts-with?
                          (mylint-lambda-call '(a) '(call))
                          "Expected 1 argument(s) for call to call, found 0 args in (call)")

(***assert-custom-comp*** string-starts-with?
                          (mylint-lambda-call '() '(call 1 2))
                          "Expected 0 argument(s) for call to call, found 2 args in (call 1 2)")

(***assert-custom-comp*** string-starts-with?
                          (mylint-lambda-call '(a) '(call 1 2))
                          "Expected 1 argument(s) for call to call, found 2 args in (call 1 2)")

(***assert*** (mylint-lambda-call '(a . b) '(call 2))
              "")

(***assert-custom-comp*** string-starts-with?
                          (mylint-lambda-call '(a . b) '(call))
                          "Expected at least 1 argument(s) for call to call, found 0 args in (call)")

(***assert*** (mylint-lambda-call '(a . b) '(call 1 2))
              "")

(define (mylint-lambda-call4 funccall var conf)
  ;;(c-display "Linting" funccall ". VAR:" var)
  (define (signature-is-for-lambda* signature)
    (cond ((null? signature)
           #f)
          ((symbol? signature)
           #f)
          ((pair? (car signature))
           #t)
          (else
           (signature-is-for-lambda* (cdr signature)))))           
  (define type (schemecodeparser-get-var-type var))
  (cond ((eq? type *schemecodeparser-var-var-type*)
         (let ((value (schemecodeparser-get-var-var-value var)))
           (if (or (symbol? value)
                   (and (pair? value)
                        (symbol? (car value))))
               (cond ((and (any? keyword? (cdr funccall))
                           (pair? value)
                           (symbol? (car value))
                           (memq (car value) '(+ - * / make-vector list vector make-list)))
                      (<-> "******* Mylint: The function '" (car funccall) "' has the value '" (pp value) "', which looks suspicious. Expression: '" (pp funccall) "'. " *mylint-filename* ": " *mylint-linenum* "."))
                     ((or (symbol? value)
                          (not (string-starts-with? (symbol->string (car value)) "new_instance_of_")))
                      (c-display (<-> "******* Note from mylint: The function '" (car funccall) "' has the value '" (pp value) "', which looks suspicious. Expression: '" (pp funccall) "'. " *mylint-filename* ": " *mylint-linenum* "."))
                      "")
                     (else
                      ""))
               (<-> "The function '" (car funccall) "' is not a function. Value: '" (pp value) "'. Expression: '" (pp funccall) "'."))))
        ((eq? type *schemecodeparser-var-func-type*)
         (let ((signature (schemecodeparser-get-var-func-signature var)))
           (if (signature-is-for-lambda* signature)
               (mylint-lambda*-call signature
                                    funccall)
               (mylint-lambda-call signature
                                   funccall))))
        ((eq? type *schemecodeparser-var-parameter-type*)
         (set! (conf :num-higher-order-notices) (- (conf :num-higher-order-notices) 1))
         (if (>= (conf :num-higher-order-notices) 0)
             (c-display (<-> "******* Note from mylint: Higher order function call detected for expression: '" (pp funccall) "'. " *mylint-filename* ": " *mylint-linenum*
                             (if (= 0 (conf :num-higher-order-notices))
                                 "...(suppressing rest)..."
                                 ""))))
         "")
        (else
         "")))

  
(define* (mylint2 code (force-throw #f) (conf (make-schemecodeparser-conf)))
  ;;(c-display "   MYLINT: " code)
  
  (define (warn tag what)
    (c-display "WARN called. force-throw: " force-throw ". conf:" conf)
    (define message (<-> "========== READER Warning: "
                         (if *mylint-linenum*
                             (<-> " " *mylint-filename* ": " *mylint-linenum*)
                             "")
                         ": " what "========="))
    (newline)
    (newline)
    (display message)
    (newline)
    (when (or *is-initializing*
              force-throw)
      (if (ra:release-mode)
          (safe-display-txt-as-displayable-as-possible message)
          (begin
            (error (<_> 'mylint- tag) message)))
      ))
  
  (define (warn-undefined what for)
    ;;(c-display "varlist:" schemecodeparser-varlist)
    (warn 'undefined (<-> "\"" what "\" has not been defined " for)))

  (define (is-defined? symbol)
    ;;(c-display "CALLING defined? for" symbol ". is-symbol?" (symbol? symbol))
    ;;(c-display "GLOBALS:" *schemecodeparser-global-declarations*)
    ;;(c-display "varlist:" (schemecodeparser-get-varlist))
    (or (defined? symbol (rootlet))
        (memq symbol *schemecodeparser-global-declarations*)
        (schemecodeparser-has-varname? symbol)))

  (define (check-atom atom . extrainfo)
    (if (and (symbol? atom)
             (not (is-defined? atom)))
        (warn-undefined atom (<-> "when parsing atom" (if (null? extrainfo) "" (car extrainfo)))))
    atom)
  
  (define (check-atoms-in-expr expr)
    (if (not (proper-list? expr))
        (warn 'inproper-list (<-> "Expected proper list for " expr))
        (let loop ((args (cdr expr)))
          (when (not (null? args))
            (check-atom (car args) (<-> " in " expr))
            (loop (cdr args))))))

  (define (flatten-types types is-parameter)
    ;;(c-display "TYPES:" types
    ;;           (if (symbol? types)
    ;;               (list types)
    ;;               types))
    (map (lambda (parmtype)
           (if (or (eq? parmtype 'proper-list?)
                   (eq? parmtype 'pair?))
               'list?
               parmtype))
         (flatten (map (lambda (parmtype)
                         ;;(c-display "parmtype:" parmtype)
                         (cond ((eq? parmtype 'number?)
                                '(real? integer? rational?))
                               ((eq? parmtype 'sequence?)
                                (if is-parameter
                                    '(list? null? vector? hash-table?)
                                    '(list? vector? hash-table?)))
                               (is-parameter
                                (cond ((eq? parmtype 'real?)
                                       '(real? integer? rational?))
                                      ((eq? parmtype 'rational?)
                                       '(rational? integer?))
                                      ((eq? parmtype 'place?)
                                       '(rational? integer? symbol?))
                                      ((memq parmtype '(list? proper-list? pair?))
                                       '(list? null?))
                                      (else
                                       parmtype)))
                               ((eq? parmtype 'place?)
                                '(rational? integer?))
                               (else
                                parmtype)))
                       (if (symbol? types)
                           (list types)
                           types)))))
  
  (define (check-arguments-for-global-function func expr)
    (let ((signature (signature func)))
      (if (not signature)
          ""
          (let loop ((arguments (cdr expr))
                     (parameters (cdr signature))
                     (argnum 1))
            (if (null? arguments)
                ""
                (if (or (eq? #t (car parameters))
                        (and (symbol? (car parameters))
                             (memq (car parameters) '(values integer:any?))))
                    (loop (cdr arguments)
                          (cdr parameters)
                          (+ argnum 1))                    
                    (let ((argtypes (schemecodeparser-get-type (car arguments))))
                      ;;(c-display "ARGYPTES:" argtypes "arguments:" (car arguments))
                      (if (or (not argtypes)
                              (eq? #t argtypes)
                              (eq? 'values argtypes))
                          (loop (cdr arguments)
                                (cdr parameters)
                                (+ argnum 1))
                          (let ((parmtypes (flatten-types (car parameters) #t))
                                (argtypes (flatten-types argtypes #f)))
                            (if (any? (lambda (argtype)
                                        (memq argtype parmtypes))
                                      argtypes)
                                (loop (cdr arguments)
                                      (cdr parameters)
                                      (+ argnum 1))
                                (begin
                                  ;;(c-display "SIGNATURE:" signature)
                                  (warn 'wrong-argument-type (<-> "Expected one of " parmtypes " for argument #" argnum ". Found " argtypes ". Expr: " expr))
                                  "")))))))))))

  (if (not (conf :num-higher-order-notices))
      (set! (conf :num-higher-order-notices) 2))
  
  (set! (conf :errorhandler)
        (lambda (tag . info)
          (c-display "ERROR-handler called")
          (warn tag (apply <-> (append (list tag ": ")
                                       info)))))
  
  (schemecodeparser (c-macroexpand code) ;; Not possible to avoid having to macroexpand everything before checking if variables are defined.
                    :atomfunc check-atom
                    :elsefunc (lambda (expr)
                                ;;(c-display "ELSE:" expr)
                                (define funcname (car expr))
                                
                                (if (or (not (symbol? (car expr)))
                                        (keyword? (car expr)))
                                    (warn 'not-a-symbol (<-> "\"" funcname "\" is not a symbol=========")))
                                
                                (if (not (is-defined? funcname))
                                    (warn-undefined funcname (<-> "in function call " expr))
                                    (check-atoms-in-expr expr))
                                (define error-string "")
                                (define local-var (schemecodeparser-get-var funcname))
                                (if local-var
                                    (set! error-string (mylint-lambda-call4 expr local-var conf))
                                    (if (and (defined? funcname (rootlet))
                                             (not (memq funcname *schemecodeparser-global-declarations*))
                                             (not (schemecodeparser-has-varname? funcname)))
                                        (let ((func (eval funcname)))
                                          (when (and (procedure? func)                                                   
                                                     (not (string-starts-with? (symbol->string funcname) "ra:gui_"))) ;; because of the the <gui> macro. TODO: Fix the <gui> macro.
                                            ;;(c-display "varlist:")
                                            (set! error-string (cond ((and (not (null? (procedure-source func)))
                                                                           (eq? 'lambda* (car (procedure-source func))))
                                                                      (mylint-lambda*-call2 func expr))
                                                                     ((not (aritable? func (- (length expr) 1)))
                                                                      (<-> "Wrong number of arguments for \"" funcname "\" in " expr ". Arity:" (arity func)))
                                                                     (else
                                                                      (check-arguments-for-global-function func expr))))))))
                                (if (not (string=? "" error-string))
                                    (warn 'illegal-function-call error-string))
                                expr)
                    :conf conf))

(define (mylint expr . args)
  (if (keyword? expr)
      expr
      (apply mylint2 (cons expr args))))


(***assert*** (mylint '(provide 'a) #t)
              '(provide 'a))

(if (not (ra:release-mode))
    (***assert-error*** (mylint '(provide '(a)) #t)
                        'mylint-wrong-argument-type))

(***assert*** (mylint '(+ 2 3 (ra:get-quantitize)) #t)
              '(+ 2 3 (ra:get-quantitize)))

(if (not (ra:release-mode))
    (***assert-error*** (mylint '(ra:get-quantitize "hello") #t)
                        'mylint-wrong-argument-type))

(***assert*** (mylint '(ra:get-quantitize #t) #t)
              '(ra:get-quantitize #t))

(***assert*** (mylint '(ra:get-quantitize #f) #t)
              '(ra:get-quantitize #f))

(when (not (ra:release-mode))
  (***assert-error*** (mylint '(+ 2 3 '(2 3)) #t)
                      'mylint-wrong-argument-type)
  
  (***assert-error*** (mylint '(+ 2 3 "") #t)
                      'mylint-wrong-argument-type)
  
  (***assert-error*** (mylint '(let ((a 50)) b) #t)
                      'mylint-undefined)
  
  (***assert-error*** (mylint '(lambda (a) b) #t)
                      'mylint-undefined)

  (***assert-error*** (mylint '(lambda* (a) b) #t)
                      'mylint-undefined)
  
  (***assert-error*** (mylint '(define (a) b) #t)
                      'mylint-undefined)

  (***assert-error*** (mylint '(define* (a) b) #t)
                      'mylint-undefined)
  
  (***assert-error*** (mylint '(define* (a c) (a b)) #t)
                      'mylint-undefined)
  
  (***assert-error*** (mylint '(define* (a c) (a (a b))) #t)
                      'mylint-undefined)
  
  (***assert-error*** (mylint '(define* (a) (a 1)) #t)
                      'mylint-illegal-function-call)
  
  (***assert-error*** (mylint '(define* (a) (a (a))) #t)
                      'mylint-illegal-function-call))

(***assert*** (mylint '(define* (a b) (a (a b))) #t)
              '(define* (a b) (a (a b))))

(if (not (ra:release-mode))
    (if (defined? 'delafina)
        (***assert-error*** (mylint '(delafina (a) b) #t)
                            'mylint-undefined)))

(when (not (ra:release-mode))
  (***assert-error*** (mylint '(let () (let ((a 50)) a) a) #t)
                      'mylint-undefined))

(***assert*** (mylint '(let () (let ((a 50)) a) 9) #t)
              '(let () (let ((a 50)) a) 9))

(when (not (ra:release-mode))
  (***assert-error*** (mylint '(gakkgakk) #t)
                      'mylint-undefined)
  
  (***assert-error*** (mylint 'gakkgakk #t)
                      'mylint-undefined)
  
  (***assert-error*** (mylint '(eq?) #t)
                      'mylint-illegal-function-call)
  
  (***assert-error*** (mylint '(eq? 'a 'b 'c) #t)
                      'mylint-illegal-function-call))

(***assert*** (mylint '(eq? 'a 'b) #t)
              '(eq? 'a 'b))

(if (defined? 'delafina)
    (***assert*** (mylint '(delafina (ai :a :b 2) #t))
                  '(define* (ai a (b 2)) #t)))

(when (not (ra:release-mode))
  (***assert-error*** (mylint '(let loop () (loop 2)) #t)
                      'mylint-illegal-function-call)
  
  (***assert-error*** (mylint '(let ((loop 50)) (loop 2)) #t)
                      'mylint-illegal-function-call))

(***assert*** (mylint '(let ((loop eq?)) (loop 'a 'b)) #t)
              '(let ((loop eq?)) (loop 'a 'b)))

;; Currently the linter doesn't check result type of function calls, so it doesn't see that this is an error.
;; The linter should print a note though:
(***assert*** (mylint '(let ((loop (ra:release-mode))) (loop 'a 'b)) #t)
              '(let ((loop (ra:release-mode))) (loop 'a 'b)))

;; Also display a warning for plainer higher order function:
(***assert*** (mylint '(lambda (a) (a 2)))
              '(lambda (a) (a 2)))

(when (not (ra:release-mode))
  (***assert-error*** (mylint '(lambda () (let loop () (loop 2))) #t)
                      'mylint-illegal-function-call)
  
  (***assert-error*** (mylint '(let () (define (a b) b) (a)) #t)
                      'mylint-illegal-function-call))


#!!
(mylint '(define (a 100) (a2)) #t)
!!#

(when (not (ra:release-mode))
  (***assert-error*** (mylint '(define (a 100) (a)) #t)
                      'schemecodeparser-varname-not-a-symbol))

(***assert*** (mylint '(define (a . b) (a)) #t)
              '(define (a . b) (a)))

(***assert*** (mylint '(begin
                         (define (a . b) (a)))
                      #t)
              '(begin
                 (define (a . b) (a))))

(***assert*** (mylint '(lambda (a . b) (a)) #t)
              '(lambda (a . b) (a)))

(***assert*** (mylint '(lambda* (a . b) (a)) #t)
              '(lambda* (a . b) (a)))

(let ((code '(let ()
               (define (hellofunc) #t)
               (define paint-voltext (<optional-func> ()))
               (paint-voltext))))
  (mylint code)
  (***assert*** (mylint code #t)
                code))

(let ((code '(let ()
               (define paint-voltext (<optional-func> ((a #f))))
               (paint-voltext))))
  (***assert*** (mylint code #t)
                code))

(let ((code '(let ()
               (define (hellofunc) #t)
               (define paint-voltext (<optional-func> rest))
               (paint-voltext))))
  (***assert*** (mylint code #t)
                code))

(let ((code '(let ()
               (define (hellofunc) #t)
               (define paint-voltext (<optional-func> rest))
               (paint-voltext 2 3 4 5))))
  (***assert*** (mylint code #t)
                code))

(let ((code '(let ()
               (define (hellofunc) #t)
               (define paint-voltext (<optional-func> (a . rest)))
               (paint-voltext 2 3 4 5))))
  (***assert*** (mylint code #t)
                code))
(when (not (ra:release-mode))
  (let ((code '(let ()
                 (define paint-voltext (<optional-func> (a . rest)))
                 (paint-voltext))))
    (***assert-error*** (mylint code #t)                      
                        'mylint-illegal-function-call))

  (let ((code '(let ()
                 (define paint-voltext (<optional-func> (a b)))
                 (paint-voltext))))
    (***assert-error*** (mylint code #t)                      
                        'mylint-illegal-function-call))
  
  (let ((code '(let ()
                 (define paint-voltext (<optional-func> (a (b #f))))
                 (paint-voltext))))
    (***assert-error*** (mylint code #t)                      
                        'mylint-illegal-function-call)))


(let ((code '(let ()
               (define (hellofunc) #t)
               (define-optional-func paint-voltext (a . rest))
               (paint-voltext 2 3 4 5))))
  (***assert*** (mylint code #t)
                (c-macroexpand code)))

(when (not (ra:release-mode))
  (let ((code '(let ()
                 (define-optional-func  paint-voltext (a . rest))
                 (paint-voltext))))
    (***assert-error*** (mylint code #t)                      
                        'mylint-illegal-function-call))

  (let ((code '(let ()
                 (define-optional-func paint-voltext (a b))
                 (paint-voltext))))
    (***assert-error*** (mylint code #t)                      
                        'mylint-illegal-function-call))
  
  (let ((code '(let ()
                 (define-optional-func paint-voltext (a (b #f)))
                 (paint-voltext))))
    (***assert-error*** (mylint code #t)                      
                        'mylint-illegal-function-call))
  )

(let ((code '(let ()
               (define tablet (make-hash-table 5 eq?))
               (tablet 'hello))))
  (***assert*** (mylint code #t)
                code))

(let ((code '(let ()
               (define tablet (hash-table))
               (tablet 'hello 2 3)))) ;; more than one argument happens (the stored value might be a vector/list/hash table).
  (***assert*** (mylint code #t)
                code))

(when (not (ra:release-mode))
  (let ((code '(let ()
                 (define tablet (make-hash-table 5 eq?))
                 (tablet))))
    (***assert-error*** (mylint code #t)                      
                        'mylint-illegal-function-call))
  
  (let ((code '(lambda ()
                 (let ((a (make-vector 5)))
                   (a :hello)))))
    (***assert-error*** (mylint code #t)
                        'mylint-illegal-function-call))
  )


#!!

(c-define-expansion (*testmacro*)
  `(list 2 3 4))
(c-define-expansion (*testmacro2*)
  `(list 2 3 4))

(mylint '(begin (testmacro2)))
(mylint '(testmacro))
(c-macroexpand-1 '(testmacro))

(catch #t
       (lambda ()
         ;;(schemecodeparser-create-declared-var 50)
         ;;(error 'hello2)
         (error 'hello3)
         (mylint '(define (a 100) (a)) #t)
         ;;(throw 'hello23)
         )
       (lambda (tag . rest)
         (if (eq? tag 'hello2)
             (c-display "hepp")
             (error tag rest))))
!!#

#!!

(cdr (assq 'wef '((b 2) (c 9) (a . 5) (d 100))))
(assq 'wef '((b 2) (c 9) (a . 5) (d 100)))

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
           (+ 2 3 a :e 'd d schemecodeparser))
        #t)

(error 'hello "hello2 hello3")
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
             ,@(reverse (get-expression-from-file filename)))
          #t))
!!#
(define (mylint-file filename)
  (define conf (make-schemecodeparser-conf))
  (set! *mylint-filename* filename)
  (catch #t
         (lambda ()
           (call-with-input-file filename
             (lambda (f)
               (let loop ()
                 (let ((expr (read f)))                 
                   (if (eof-object? expr)
                       #t
                       (begin
                         (set! *mylint-linenum* (port-line-number f))
                         (when *is-initializing*
                           (display filename) (display ": ") (display expr)
                           (newline))
                         ;;(c-display "             MYLINT-FILE" expr)
                         (mylint expr #t conf)
                         (loop))))))))
         (lambda args
           (set! *mylint-linenum* #f)
           (set! *mylint-filename* #f)
           (apply throw args)))
  (set! *mylint-linenum* #f)
  (set! *mylint-filename* #f))

#!!
(signature apply-values)
(signature cdr)
(signature >=)
(signature =)
(signature map)
(signature for-each)
(signature map)
(signature floor)
(signature max)
(signature i-max)
(signature floor)


(load "mylint.scm")
(get-expression-from-file "/home/kjetil/radium/bin/scheme/define-match.scm")
(mylint-file "/home/kjetil/radium/bin/scheme/import_mod.scm")
(mylint-file "/home/kjetil/radium/bin/scheme/api_autotesting.scm")
(begin integer:any?)
  
(signature vector-set!)
(signature list-set!)
(signature list-ref)
(signature length)

(let ((v (vector 1 2 34)))
  (vector-set! v 0 5)
  v)

(begin
  (mylint-file "/home/kjetil/radium/bin/scheme/mylint.scm")
  (mylint-file "/home/kjetil/radium/bin/scheme/mixer-strips.scm")
  (mylint-file "/home/kjetil/radium/bin/scheme/mouse.scm")
  (mylint-file "/home/kjetil/radium/bin/scheme/area.scm")
  (mylint-file "/home/kjetil/radium/bin/scheme/instruments.scm")
  (mylint-file "/home/kjetil/radium/bin/scheme/seqtrack-headers.scm")
  (mylint-file "/home/kjetil/radium/bin/scheme/timing.scm")
  (mylint-file "/home/kjetil/radium/bin/scheme/gui.scm")
  (mylint-file "/home/kjetil/radium/bin/scheme/common2.scm")
  (mylint-file "/home/kjetil/radium/bin/scheme/fxrange.scm")
  (mylint-file "/home/kjetil/radium/bin/scheme/notem.scm")
  (mylint-file "/home/kjetil/radium/bin/scheme/sequencer_upper_part.scm")
  (mylint-file "/home/kjetil/radium/bin/scheme/notes.scm")
  (mylint-file "/home/kjetil/radium/bin/scheme/init.scm")
  (mylint-file "/home/kjetil/radium/bin/scheme/various.scm")
  (mylint-file "/home/kjetil/radium/bin/scheme/seqblock_audio.scm")
  (mylint-file "/home/kjetil/radium/bin/scheme/sequencer.scm")
  (mylint-file "/home/kjetil/radium/bin/scheme/pluginmanager.scm")
  (mylint-file "/home/kjetil/radium/bin/scheme/define-match-bootstrapped.scm")
  (mylint-file "/home/kjetil/radium/bin/scheme/keybindings.scm")
  (mylint-file "/home/kjetil/radium/bin/scheme/common1.scm")
  (mylint-file "/home/kjetil/radium/bin/scheme/randomize-note-durations.scm")
  (mylint-file "/home/kjetil/radium/bin/scheme/popupmenu.scm")
  (mylint-file "/home/kjetil/radium/bin/scheme/main_layout.scm")
  (mylint-file "/home/kjetil/radium/bin/scheme/seqblock_editor.scm")
  (mylint-file "/home/kjetil/radium/bin/scheme/solo.scm")
  (mylint-file "/home/kjetil/radium/bin/scheme/editor_lower_part.scm")
  (mylint-file "/home/kjetil/radium/bin/scheme/quantitize.scm")
  (mylint-file "/home/kjetil/radium/bin/scheme/main_menus.scm")
  (mylint-file "/home/kjetil/radium/bin/scheme/seqblock-paint.scm")
  (mylint-file "/home/kjetil/radium/bin/scheme/semi-primitives.scm")
  (mylint-file "/home/kjetil/radium/bin/scheme/nodes.scm")
  (mylint-file "/home/kjetil/radium/bin/scheme/mouse-primitives.scm")
  )

(mylint-file "/home/kjetil/radium/bin/scheme/.scm")

(pp
 (c-macroexpand
  '(delafina (make-qtarea :width 100 :height 100 :sub-area-creation-callback #f :enable-mouse-callbacks #t)
  (define gui (<gui> :widget width height))  
  (define x1 0)
  (define y1 0)
  (define x2 width)
  (define y2 height)
  (def-area-subclass (<qtarea>)
    (<gui> :add-paint-callback gui
           (lambda (width height)
             (paint-internal 0 0 width height)))

    (when enable-mouse-callbacks
      (<gui> :add-mouse-callback gui
             (lambda (button state x y)
               (mouse-callback-internal button state x y)
               ;;(c-display "has-mouse:" (and (defined? 'has-mouse) (has-mouse)))
               ;;50))
               (has-mouse)))
      (<gui> :add-mouse-wheel-callback gui mouse-wheel-moved-internal!))

    (define-optional-func the-sub-area (key . rest))
    
    (when sub-area-creation-callback
      (define (recreate width* height*)
        (resize! width* height*)
        (define state (and the-sub-area
                           (the-sub-area :get-state)))
        (remove-sub-areas!)
        (set! the-sub-area (sub-area-creation-callback gui width height state))
        (if state
            (the-sub-area :apply-state! state))
        (add-sub-area-plain! the-sub-area))
      
      (<gui> :add-resize-callback gui recreate)
      (recreate width height))
    )

  (define area (<new> :qtarea))

  (area :add-method! :get-gui (lambda ()
                                gui))

  area)
  ))

(mylint
 '(let ()
    (define (a)
      (a))
    (a)))

(pp
 (mylint
  '(delafina (make-qtarea :width 100 :height 100 :sub-area-creation-callback #f :enable-mouse-callbacks #t)
     (def-area-subclass (<qtarea>)
       #t)
     50)))

(pp
 (mylint
  '(let ()
     (define (hepp)
       (def-area-subclass (<qtarea>)
         #t)))))

(pp
 (mylint
  '(define (hepp)
     (def-area-subclass (<qtarea>)
       #t))))

(c-macroexpand 
 '(def-area-subclass (<qtarea>)
   #t))

(mylint '(define (a)
           (define (b)
             50)
           (b)))

(pp (c-macroexpand '(define area (<new> :qtarea))))

(c-define-macro (*macro4*)
  `(macro5))

(c-define-macro (*macro3* b)
  `(macro6 ,b))

(c-define-macro (*macro2* b)
  `(macro3 ,b))

(c-define-macro (*macro1* b)
  `(macro2 ,b))

(c-macroexpand '(begin
                  (macro1 (macro4))))

('define-match 'get-track-num-0 'X '_ '___ 'X1 '__ '__________ #:'> #f #:'where
  ('< 'X 'X1) 'X '_ 'Num 'X1 'X2 '__________ #:'> 'Num #:'where ('and ('>= 'X 'X1)
                                                                      ('< 'X 'X2))
  '_ '_ 'Num '__ '__ 'Num-tracks #:'> #f #:'where ('= ('1+ 'Num) 'Num-tracks)
  'X 'Y 'Num 'X1 'X2 'Num-tracks #:'>
  ('get-track-num-0 'X 'Y ('1+ 'Num)
                    'X2 ('if ('= 'Num ('- 'Num-tracks 2))
                             ('ra:get-track-x2 ('1+ 'Num))
                             ('ra:get-track-x1 ('+ 2 'Num)))
                    'Num-tracks))

('define ('get-track-num-0 '-__Arg1 '-__Arg2 '-__Arg3 '-__Arg4 '-__Arg5 '-__Arg6)
  ('define ('-__Func1) ('let (('X '-__Arg1)) ('let (('X1 '-__Arg4)) ('if ('< 'X 'X1) #f ('-__Func2)))))
  ('define ('-__Func2) ('let (('X '-__Arg1)) ('let (('Num '-__Arg3)) ('let (('X1 '-__Arg4)) ('let (('X2 '-__Arg5)) ('if ('and ('>= 'X 'X1) ('< 'X 'X2)) 'Num ('-__Func3)))))))
  ('define ('-__Func3) ('let (('Num '-__Arg3)) ('let (('Num-tracks '-__Arg6)) ('if ('= ('1+ 'Num) 'Num-tracks) #f ('-__Func4)))))
  ('define ('-__Func4) ('let (('X '-__Arg1)) ('let (('Y '-__Arg2))
                                               ('let (('Num '-__Arg3))
                                                 ('let (('X1 '-__Arg4)) ('let (('X2 '-__Arg5))
                                                                          ('let (('Num-tracks '-__Arg6))
                                                                            ('get-track-num-0 'X 'Y ('1+ 'Num) 'X2 ('if ('= 'Num ('- 'Num-tracks 2))
                                                                                                                        ('ra:get-track-x2 ('1+ 'Num))
                                                                                                                        ('ra:get-track-x1 ('+ 2 'Num)))
                                                                                              'Num-tracks)))))))) ('-__Func1)) 



(mylint '(begin (define s (gensym "s"))))

(mylint '(let ()
           (define (get-vector)
             (get-vector))
           #t))

(mylint '(define (get-vector)
           (get-vector)))

(mylint '(let ()
           (define-class (<hepp>)
             (define (get-vector)
               50)
             #t)))

(pretty-print (c-macroexpand '(let ()
                  (define-class (<hepp>)
                    (define (get-vector)
                      50)
                    #t))))

(mylint '(let ()
           (define paint-voltext (<optional-func> (a b)))
           (paint-voltext)))

(c-macroexpand '(<optional-func> a b . c))

(let ((daslist list))
  `(,daslist 50))

(define (hepp . rest)
  (quasiquote ,rest))

(hepp hepp hepp . hepp)

(quote '(a . b))

(lambda (a b)
  (+ swefoi)
  hepp)

(proper-list? (cons 'a 'b))
(let loop ((parameters (cons 'a 'b)))
  (if (symbol? parameters)
      (list parameters)
      (cons (car parameters)
            (loop (cdr parameters)))))

(let ((def '(define s (gensym "s"))))
  (if (pair? (cadr def))
      (let* ((name1 (car (cadr def)))
             (name (if (memq (car def) '(c-define-macro c-define-expansion))
                       (string->symbol (list->string (cdr (butlast (string->list (symbol->string name1))))))
                       name1))
             (signature (cdr (cadr def))))
        (c-display "name:" name ". sig:" signature)
        ;;(schemecodeparser-create-func-var name signature)
        )
      (let ((name (cadr def))
            (value (caddr def)))
        (c-display "name:" name ". value:" value)
        (schemecodeparser-create-var-var name value errorhandler)
        )))

(let ()
  (define (signature-is-for-lambda* signature)
    (cond ((null? signature)
           #f)
          ((symbol? signature)
           #f)
          ((pair? (car signature))
           #t)
          (else
           (signature-is-for-lambda* (cdr signature)))))
  (signature-is-for-lambda* (cdr '(lambda {original}-9612 . {arguments}-9613))))

  

(pp
 (c-macroexpand
  '(define-struct conf
     :instrument-id
     :is-bus
     :row-num
     :is-enable
     :is-unique)))

(pp
 (c-macroexpand
  '(define-struct event
  :patternnum
  :channel
  :linenum
  :type
  :value
  :value2 0
  :instrumentnum 0
  :tick 0
  :is-pattern-delay-line #f)))

(pp
 (c-macroexpand
  '(delafina (m-e-0 :type type
                    :pattern 0
                 :channel 0
                 :instrumentnum 0
                 :line 0
                 :value 0
                 :value2 0
                 :tick 0
                 :is-pattern-delay-line #f                 
                 )
  ;;(c-display "gakk gakk" pattern channel line type value value2 instrumentnum tick is-pattern-delay-line)
  (make-event-nokeywords pattern
                         channel              
                         line
                         type
                         value
                         value2
                         instrumentnum
                         tick
                         is-pattern-delay-line
                         ))
  ))

!!#


(define (mylint-string string*)
  (define conf (make-schemecodeparser-conf))
  (call-with-input-string
   string*
   (lambda (f)
     (let loop ((ret #<undefined>))
       (let ((expr (read f)))                 
         (if (eof-object? expr)
             ret
             (begin
               (display expr)
               (newline)
               (loop (mylint expr :conf conf)))))))))

(define (mylint-and-eval-string string* . envlist)
  (define conf (make-schemecodeparser-conf))
  (define env (if (null? envlist)
                  (rootlet)
                  (car envlist)))
  (call-with-input-string
   string*
   (lambda (f)
     (let loop ((ret #<undefined>))
       (let ((expr (read f)))                 
         (if (eof-object? expr)
             ret
             (begin
               (<declare-variable> with-history-disabled)
               ;;(display expr) (newline)
               ;;(c-display "             MYLINT-AND_EVAL:" expr)
               (with-history-disabled
                (mylint expr :conf conf)
                )
               (loop (eval expr env)))))))))
  
#!!
(mylint-and-eval-string "(c-display 'hello sdf)")
(lambda (a)
  (+ w bg w  ewr ))
(c-display "a")
!!#


