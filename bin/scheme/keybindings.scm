(provide 'keybindings.scm)

(define *reload-keybindings-callbacks* '())

(define (add-reload-keybindings-callback func)
  (push! *reload-keybindings-callbacks* func))

(define (remove-reload-keybindings-callback func)
  (set! *reload-keybindings-callbacks*
        (let loop ((callbacks *reload-keybindings-callbacks*))
          (cond ((null? callbacks)
                 (if (not (<ra> :release-mode))
                     (assert #f))
                 '())
                ((eqv? (car callbacks) func)
                 callbacks)
                (else
                 (cons (car callbacks)
                       (loop (cdr callbacks))))))))

(define (FROM_C-keybindings-have-been-reloaded)
  (for-each (lambda (func)
              (func))
            (copy *reload-keybindings-callbacks*))) ;; need to make a copy in case the callback removes itself.

(define (get-keybindings-command rafuncname args)
  (define (get-arg-string arg)
    (let ((arg2 (to-displayable-string arg)))
      (if (string? arg)
          (<-> "\"" arg2 "\"")
          arg2)))
  (<-> rafuncname
       (apply <-> (map (lambda (arg)
                         (<-> " " (get-arg-string arg)))
                       args))))
#!!
(get-keybindings-command "" '())
!!#

(define (get-keybindings-line keys rafuncname args)
  (<-> (string-join keys " ")
       " : "
       (get-keybindings-command rafuncname args)))


(define (remove-mouse-from-keybinding keybinding)
  (<-> (string-join (remove (lambda (key)
                              (string-starts-with? key "MOUSE_"))
                            (string-split keybinding #\ ))
                    " ")))


(define (get-displayable-keybinding-from-keybinding keybinding)
  ;;(c-display "KEYBINDING" keybinding)
  (string-join (map (lambda (qualifier)
                      (let ((readable (<ra> :get-qualifier-name qualifier)))
                        (if (string=? "" readable)
                            qualifier
                            readable)))
                    (let* ((keys (string-split keybinding #\ ))
                           (qualifiers (cdr keys))
                           (key (car keys)))
                      (append (remove (lambda (qualifier)
                                        (string-starts-with? qualifier "MOUSE"))
                                      qualifiers)
                              (list key))))
               " + "))

(define (get-displayable-keybinding rafuncname args)
  (if (< (string-length rafuncname) 3)
      ""
      (let* ((command (get-keybindings-command (get-python-ra-funcname rafuncname) args))
             (keybinding (<ra> :get-keybinding-from-command command)))
        ;;(c-display "command:" command)
        ;;(c-display "keybinding:" keybinding)
        (if (string=? "" keybinding)
            ""
            (get-displayable-keybinding-from-keybinding keybinding)))))


(define (FROM_C-get-displayable-keybinding rafuncname args)
  (get-displayable-keybinding rafuncname (to-list args)))

(***assert*** (get-displayable-keybinding "w" '())
              "")
(***assert*** (get-displayable-keybinding "we" '())
              "")

#!!
(get-displayable-keybinding "e" '())
(get-displayable-keybinding "ra:copy-tracke" '())
(get-displayable-keybinding "ra:paste-seqblocks" '())
(get-displayable-keybinding "ra:copy-paste-seqblocks" '())
(get-displayable-keybinding "ra:copy-selected-mixer-objects" '())

(get-displayable-keybinding "ra:eval-scheme" '("(moduloskew-track -1)"))
  
(let ((gakk (get-displayable-keybinding "ra:transpose-block" (list 1))))
  (c-display "gakk:" gakk)
  gakk)
(pretty-print (<ra> :get-keybindings-from-commands))
!!#


(delafina (add-keybinding-configuration-to-gui :gui
                                               :ra-funcname
                                               :args '())

  (define (get-arg-string arg)
    (let ((arg2 (to-displayable-string arg)))
      (if (string? arg)
          (<-> "\"" arg2 "\"")
          arg2)))
  
  (define command (<-> (get-python-ra-funcname ra-funcname)
                       (apply <-> (map (lambda (arg)
                                         (<-> " " (get-arg-string arg)))
                                       args))))
  
  (define (changeit)
    (define grab-gui (<gui> :vertical-layout))

    #||
    (define all (<gui> :radiobutton "All" #t))
    (define mixerstrips (<gui> :radiobutton "Mixer strips" #f))
    (define sequencer (<gui> :radiobutton "Mixer strips" #f))
    (define mixer (<gui> :radiobutton "Mixer" #f))
    (define editor (<gui> :radiobutton "Editor" #f))
    
    (define buttons (<gui> :horizontal-layout all mixerstrips sequencer mixer editor))
    (<gui> :add grab-gui buttons)
    ||#
    
    (define wait-text (mid-horizontal-layout (<gui> :text "Waiting for key....")))
    (<gui> :add grab-gui wait-text)

    
    (<gui> :set-parent grab-gui -1)
    (<gui> :show grab-gui)
    (<gui> :set-modal grab-gui #t)

    (<ra> :grab-keybinding
          (lambda (keybinding)
            (set! keybinding (remove-mouse-from-keybinding keybinding))
            (<gui> :close grab-gui)
            (define (setit!)
              (<ra> :set-keybinding keybinding (get-python-ra-funcname ra-funcname) (map get-arg-string args)))
            (define existing-command (<ra> :get-keybinding-from-keys keybinding))
            (if (string=? "" existing-command)
                (setit!)
                (show-async-message :text (<-> "Keybinding \"" (get-displayable-keybinding-from-keybinding keybinding) "\" already bound to \"" existing-command "\". Override?")
                                    :buttons '("Yes" "No")
                                    :callback (lambda (answer)
                                                (if (string=? "Yes" answer)
                                                    (setit!)))))
                                                    
            (c-display "hello1"))))
    
  (define (keybinding-mouse-handler button state x y)
    (define keybinding (get-displayable-keybinding ra-funcname args))
    (define has-keybinding (not (string=? keybinding "")))
    
    (if (and (= button *right-button*)
             (= state *is-pressing*))
        (popup-menu (list (if has-keybinding
                              (<-> "Change keybinding for \"" command "\"")
                              (<-> "Add keybinding for \"" command "\""))
                          changeit)
                    (if has-keybinding
                        (list 
                         "Remove the keybinding"
                         (lambda ()
                           (define python-ra-funcname (get-python-ra-funcname ra-funcname))
                           (<ra> :remove-keybinding
                                 (<ra> :get-keybinding-from-command (get-keybindings-command python-ra-funcname args))
                                 python-ra-funcname
                                 (map get-arg-string args))
                           (c-display "removing")))
                        '())))
    #f)

  (<gui> :add-mouse-callback gui keybinding-mouse-handler))

    
  
(delafina (create-keybinding-button :name
                                    :ra-funcname
                                    :arguments '())
  (define requires-range (string-case-insensitive-contains? (<-> ra-funcname name) "range"))
  (define ra-func (eval-string ra-funcname))
  (define (func)
    (if (and requires-range
             (not (<ra> :has-range)))
        (show-async-message :text "No range in block. Select range by using Left Meta + b")
        (apply ra-func arguments)))

  (define button (<gui> :button name func))  

  (define keybinding #f)

  (define (set-keybinding!)
    (set! keybinding (get-displayable-keybinding ra-funcname arguments)))

  (set-keybinding!)
  
  (define gui (<gui> :horizontal-layout
                     button))

  (define text (<gui> :text ""))
  (<gui> :add gui text)
  
  (define (set-text-gui)    
    (define texttext (if (string=? "" keybinding)
                         " "
                         (<-> " (" keybinding ")")))
    (<gui> :set-text text texttext))
  
  (set-text-gui)

  (define (reloaded-keybinding-callback)
    (if (not (<gui> :is-open gui))
        (remove-reload-keybindings-callback reloaded-keybinding-callback)
        (begin
          (set-keybinding!)
          (set-text-gui))))
  
  (add-reload-keybindings-callback reloaded-keybinding-callback)
  
  (add-keybinding-configuration-to-gui gui ra-funcname arguments)
  
  gui)

