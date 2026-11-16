(provide 'scheme_listener.scm)

;; A Scheme listener window implemented directly in Radium. The input line and
;; the output are ordinary Radium GUI widgets, so no external terminal, HTTP
;; request, or Python installation is involved.

(define (scheme-listener-read-all-forms text)
  (let ((in (open-input-string text)))
    (let loop ((forms '()))
      (let ((form (read in)))
        (if (eof-object? form)
            (reverse forms)
            (loop (cons form forms)))))))

;; Returns (ok forms) on success, or (error <error-info>) on a read error.
(define (scheme-listener-try-parse text)
  (catch #t
    (lambda ()
      (list 'ok (scheme-listener-read-all-forms text)))
    (lambda error-info
      (list 'error error-info))))

(define (scheme-listener-error->string error-info)
  (if (and (pair? error-info)
           (pair? (cdr error-info))
           (list? (cadr error-info)))
      (catch #t
        (lambda ()
          (apply format #f (cadr error-info)))
        (lambda _
          (to-displayable-string error-info)))
      (to-displayable-string error-info)))

(define (scheme-listener-incomplete-input? error-info)
  (and (pair? error-info)
       (memq (car error-info) '(read-error string-read-error))
       (let ((message (scheme-listener-error->string error-info)))
         (or (string-contains? message "missing close paren")
             (string-contains? message "end of input")
             (string-contains? message "unexpected '#' at end of input")))))

;; Returns (output result error-info).
(define (scheme-listener-eval-forms forms)
  (let ((output '())
        (result #<unspecified>)
        (error-info #f))
    (let ((port (open-output-function
                 (lambda (c)
                   (set! output (cons (if (char? c) c (integer->char c))
                                      output))))))
      (let-temporarily (((current-output-port) port)
                        ((current-error-port) port))
        (catch #t
          (lambda ()
            (for-each (lambda (form)
                        (set! result (eval form (rootlet))))
                      forms))
          (lambda args
            (set! error-info args)))))
    (list (list->string (reverse output))
          result
          error-info)))

(define (scheme-listener-common-prefix a b)
  (let ((limit (min (string-length a) (string-length b))))
    (let loop ((i 0))
      (if (or (= i limit)
              (not (char=? (string-ref a i) (string-ref b i))))
          (substring a 0 i)
          (loop (1+ i))))))

(define (scheme-listener-common-prefix-of-list strings)
  (if (null? strings)
      ""
      (let loop ((prefix (car strings))
                 (rest (cdr strings)))
        (if (null? rest)
            prefix
            (loop (scheme-listener-common-prefix prefix (car rest))
                  (cdr rest))))))

(define (scheme-listener-completion-token-start text)
  (let loop ((i (1- (string-length text))))
    (if (< i 0)
        0
        (let ((c (string-ref text i)))
          (if (or (char-whitespace? c)
                  (memv c (list #\( #\) #\[ #\] #\{ #\}
                                #\" #\' #\; #\, #\`)))
              (1+ i)
              (loop (1- i)))))))

(define (scheme-listener-substring-search? prefix)
  (and (> (string-length prefix) 1)
       (char=? (string-ref prefix 0) #\*)))

(define (scheme-listener-completions prefix)
  (let ((substring-search? (scheme-listener-substring-search? prefix))
        (search-string (if (scheme-listener-substring-search? prefix)
                           (substring prefix 1 (string-length prefix))
                           prefix))
        (symbols (symbol-table)))
    (let loop ((i 0)
               (result '()))
      (if (>= i (length symbols))
          (sort result string<?)
          (let* ((sym (vector-ref symbols i))
                 (name (and (symbol? sym)
                            (symbol->string sym))))
            (if (and name
                     (if substring-search?
                         (string-contains? name search-string)
                         (string-starts-with? name search-string))
                     (defined? sym (rootlet)))
                (loop (1+ i) (cons name result))
                (loop (1+ i) result)))))))


(define (open-scheme-listener)
  (let* ((output-gui (<gui> :text-edit "" #t))
         (prompt-label (<gui> :text "radium> " "green"))
         (input-gui (<gui> :line ""))
         (input-layout (<gui> :horizontal-layout prompt-label input-gui))
         (gui (<gui> :vertical-layout output-gui input-layout))
         (pending-input "")
         (in-continuation #f)
         (history '())
         (history-index -1)
         (history-draft "")
         (mouse-inside? #f)
         (in-completion-popup? #f))

    (define (append-output text)
      (<gui> :append-value output-gui (<ra> :get-html-from-text text)))

    (define (set-input-text text)
      (<gui> :set-value input-gui text))

    (define (show-completion-popup . args)
      (set! in-completion-popup? #t)
      (<ra> :add-popup-menu-closed-callback
            (lambda ()
              (set! in-completion-popup? #f)
              ;; Focus asynchronously so the Escape/Return key that closed
              ;; the popup is not also delivered to the line edit.
              (<ra> :schedule 0
                    (lambda ()
                      (when (and mouse-inside?
                                 (<gui> :is-open gui))
                        (focus-input))
                      #f))
              #f))
      (apply popup-menu args))

    (define (complete-input)
      (define text (<gui> :get-value input-gui))
      (define start (scheme-listener-completion-token-start text))
      (define prefix (substring text start (string-length text)))
      (when (not (string=? prefix ""))
        (define candidates (scheme-listener-completions prefix))
        (cond ((null? candidates)
               (show-completion-popup "No completions" :enabled #f (lambda () #f)))
              ((null? (cdr candidates))
               (set-input-text (<-> (substring text 0 start)
                                    (car candidates)
                                    " ")))
              (else
               (define common (scheme-listener-common-prefix-of-list candidates))
               (when (and (not (scheme-listener-substring-search? prefix))
                          (> (string-length common) (string-length prefix)))
                 (set-input-text (<-> (substring text 0 start) common)))
               (define before-token (substring text 0 start))
               (apply show-completion-popup
                      (apply append
                             (map (lambda (candidate)
                                    (list candidate
                                          (lambda ()
                                            (set-input-text (<-> before-token
                                                                 candidate
                                                                 " ")))))
                                  candidates)))))))

    (define (add-history text)
      ;; History is stored with the most recent command first.
      ;; history-index -1 means that we are not browsing the history.
      (when (not (string=? text ""))
        (when (or (null? history)
                  (not (string=? (car history) text)))
          (set! history (cons text history))))
      (set! history-index -1)
      (set! history-draft ""))

    (define (history-up)
      (when (pair? history)
        (cond ((= history-index -1)
               (set! history-draft (<gui> :get-value input-gui))
               (set! history-index 0))
              ((< history-index (1- (length history)))
               (set! history-index (1+ history-index))))
        (set-input-text (list-ref history history-index))))

    (define (history-down)
      (cond ((= history-index -1)
             #f)
            ((> history-index 0)
             (set! history-index (1- history-index))
             (set-input-text (list-ref history history-index)))
            (else
             (set! history-index -1)
             (set-input-text history-draft))))

    (define (update-prompt)
      (<gui> :set-text prompt-label
             (if in-continuation "...> " "radium> ")
             "green"))

    (define (focus-input)
      ;; Setting focus with obtainKeyboardFocus also increments Radium's
      ;; "someone else has keyboard focus" counter. Release it immediately;
      ;; the line edit's FocusSniffer handles the actual focus tracking. If we
      ;; leave the counted focus held, the editor can never get keyboard focus
      ;; back after the listener has been used.
      (<ra> :obtain-keyboard-focus input-gui)
      (<ra> :release-keyboard-focus))

    (define (clear-input-focus)
      ;; The line edit's FocusSnifferQLineEdit clears Qt focus when it sees
      ;; Escape. Posting Escape is the only exposed way to move keyboard focus
      ;; away from the line without focusing another widget in this window.
      (when (<gui> :has-keyboard-focus input-gui)
        (<ra> :gui_post-key-event input-gui 16777216 0 #t) ;; Qt::Key_Escape
        (<ra> :gui_post-key-event input-gui 16777216 0 #f)))

    (define (mouse-over-listener?)
      (let ((x (<gui> :get-mouse-x gui))
            (y (<gui> :get-mouse-y gui)))
        (and (>= x 0)
             (>= y 0)
             (< x (<gui> :width gui))
             (< y (<gui> :height gui)))))

    (define (finish-input)
      (<gui> :set-value input-gui "")
      (update-prompt))

    (define (handle-complete-input prompt text full-input forms)
      (append-output (<-> prompt text "\n"))
      (set! pending-input "")
      (set! in-continuation #f)
      (add-history full-input)
      (define eval-values (scheme-listener-eval-forms forms))
      (define output (car eval-values))
      (define result (cadr eval-values))
      (define error-info (caddr eval-values))
      (when (not (string=? output ""))
        (append-output (if (string-ends-with? output "\n")
                           output
                           (<-> output "\n"))))
      (cond (error-info
             (append-output (<-> "error: "
                                 (scheme-listener-error->string error-info)
                                 "\n")))
            ((not (eq? result #<unspecified>))
             (append-output (<-> "result: "
                                 (to-displayable-string result)
                                 "\n"))))
      (finish-input))

    (define (handle-input text)
      (when (and (string? text)
                 (not (string=? text "")))
        (define prompt (if in-continuation "...> " "radium> "))
        (define full-input (if (string=? pending-input "")
                               text
                               (<-> pending-input "\n" text)))
        (define parse-result (scheme-listener-try-parse full-input))
        (if (eq? (car parse-result) 'ok)
            (handle-complete-input prompt text full-input (cadr parse-result))
            (if (scheme-listener-incomplete-input? (cadr parse-result))
                (begin
                  (append-output (<-> prompt text "\n"))
                  (set! pending-input full-input)
                  (set! in-continuation #t)
                  (finish-input))
                (begin
                  (append-output (<-> prompt text "\n"))
                  (add-history full-input)
                  (append-output (<-> "error: "
                                      (scheme-listener-error->string (cadr parse-result))
                                      "\n"))
                  (set! pending-input "")
                  (set! in-continuation #f)
                  (finish-input))))))

    (<gui> :set-window-title gui "Scheme Listener")
    (<gui> :set-name gui "scheme-listener")
    (<gui> :set-name output-gui "scheme-listener-output")
    (<gui> :set-name prompt-label "scheme-listener-prompt")
    (<gui> :set-name input-gui "scheme-listener-input")
    (<gui> :set-size gui 700 450)
    (<gui> :set-layout-spacing gui 4 4 4 4 4)

    (define (submit-input)
      (catch #t
        (lambda ()
          (handle-input (<gui> :get-value input-gui)))
        (lambda error-info
          (append-output (<-> "error: "
                              (scheme-listener-error->string error-info)
                              "\n"))
          (finish-input))))

    (<gui> :add-key-callback input-gui
           (lambda (presstype key)
             (if (= presstype 0) ;; Key press.
                 (cond ((or (string=? key "Tab")
                            (string=? key "\t"))
                        (complete-input)
                        #t)
                       ((string=? key "Up")
                        (history-up)
                        #t)
                       ((string=? key "Down")
                        (history-down)
                        #t)
                       ((or (string=? key "Return")
                            (string=? key "Enter")
                            (string=? key "\r")
                            (string=? key "\n"))
                        (submit-input)
                        #t)
                       (else
                        #f))
                 #f)))

    (<gui> :add-mouse-callback gui
           (lambda (button state x y)
             (cond ((= state *is-entering*)
                    (set! mouse-inside? #t)
                    (when (not in-completion-popup?)
                      (focus-input)))
                   ((= state *is-leaving*)
                    (set! mouse-inside? #f)
                    (clear-input-focus))
                   (else
                    #f))
             #f))

    (<gui> :show gui)
    (set! mouse-inside? (mouse-over-listener?))
    ;; Wait until Qt has assigned focus to the new window, then make the
    ;; line edit focus follow the mouse pointer.
    (<ra> :schedule 0
          (lambda ()
            (set! mouse-inside? (mouse-over-listener?))
            (if mouse-inside?
                (focus-input)
                (clear-input-focus))
            #f))
    gui))
