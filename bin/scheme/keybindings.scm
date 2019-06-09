(provide 'keybindings.scm)

(define (maybe-merge-two-keybindings-2 keybinding1 keybinding2 left right qualifier)
  (define (removeleftright keybinding)
    (delete-maybe right
                  (delete-maybe left keybinding string=?)
                  string=?))
  (define without1 (removeleftright keybinding1))
  (define without2 (removeleftright keybinding2))
  (if (or (equal? without1 keybinding1)
          (equal? without2 keybinding2)
          (not (equal? without1 without2)))
      (list keybinding1 keybinding2)
      (list (append without1 (list qualifier)))))

(***assert*** (maybe-merge-two-keybindings-2 '("Z" "CTRL_R") '("Z" "CTRL_L")
                                             "CTRL_L" "CTRL_R"
                                             "CTRL")
              '(("Z" "CTRL")))

(***assert*** (maybe-merge-two-keybindings-2 '("Z" "ALT_R") '("Z" "ALT_L")
                                             "ALT_L" "ALT_R"
                                             "ALT")
              '(("Z" "ALT")))

(***assert*** (maybe-merge-two-keybindings-2 '("Z" "ALT_L" "CTRL_R") '("Z" "CTRL_L")
                                             "CTRL_L" "CTRL_R"
                                             "CTRL")
              '(("Z" "ALT_L" "CTRL_R") ("Z" "CTRL_L")))



(define *mergable-qualifiers* (list (hash-table :left "CTRL_L"
                                                :right "CTRL_R"
                                                :to "CTRL")
                                    (hash-table :left "ALT_L"
                                                :right "ALT_R"
                                                :to "ALT")
                                    (hash-table :left "SHIFT_L"
                                                :right "SHIFT_R"
                                                :to "SHIFT")))
;;                                    (hash-table :left "EXTRA_L"
;;                                                :right "EXTRA_R"
;;-                                                :to "EXTRA")))

(define (maybe-merge-two-keybindings keybinding1 keybinding2)
  (define thesame (list keybinding1 keybinding2))
  (let loop ((mergables *mergable-qualifiers*))
    (if (null? mergables)
        thesame
        (let* ((mergable (car mergables))
               (maybe (maybe-merge-two-keybindings-2 keybinding1 keybinding2
                                                     (mergable :left) (mergable :right)
                                                     (mergable :to))))
          ;;(c-display "MAHBE:" maybe (equal? maybe thesame))
          (if (equal? maybe thesame)
              (loop (cdr mergables))
              maybe)))))

(***assert*** (maybe-merge-two-keybindings '("Z" "ALT_R") '("Z" "ALT_L"))
              '(("Z" "ALT")))

(***assert*** (maybe-merge-two-keybindings '("Z" "ALT_L" "CTRL_R") '("Z" "CTRL_L"))
              '(("Z" "ALT_L" "CTRL_R") ("Z" "CTRL_L")))

;; keeps order
(define (replace-merged-keybinding-in-keybindings keybindings keybindingA keybindingB merged-keybinding)
  ;;(c-display "MERGED:" merged-keybinding)
  (let loop ((keybindings keybindings)
             (A keybindingA)
             (B keybindingB)
             (merged-keybinding merged-keybinding))
    (if (null? keybindings)
        '()
        (let ((keybinding (car keybindings)))
          (cond ((equal? keybinding A)
                 (let ((rest (loop (cdr keybindings)
                                   "---"
                                   B
                                   #f)))
                   (if merged-keybinding
                       (cons merged-keybinding
                             rest)
                       rest)))
                ((equal? keybinding B)
                 (let ((rest (loop (cdr keybindings)
                                   A
                                   "---"
                                   #f)))
                   (if merged-keybinding
                       (cons merged-keybinding
                             rest)
                       rest)))
                (else
                 (cons keybinding
                       (loop (cdr keybindings)
                             A
                             B
                             merged-keybinding))))))))


(***assert*** (replace-merged-keybinding-in-keybindings '(a b c) 'a 'b 'ab)
              '(ab c))
                
(***assert*** (replace-merged-keybinding-in-keybindings '(a b c) 'a 'c 'ac)
              '(ac b ))
                
                       
                
              
                          
        
(define (merge-keybindings keybindings)
  ;;(c-display "KEYBINDINGS:" keybindings)
  (let loopA ((keybindingsA keybindings))
    (define keybindingA (cl-car keybindingsA))
    (if (not keybindingA)
        keybindings
        (let loopB ((keybindingsB (cdr keybindingsA)))
          (define keybindingB (cl-car keybindingsB))
          (if (not keybindingB)
              (loopA (cdr keybindingsA))
              (let ((org (list keybindingA keybindingB))
                    (maybe (maybe-merge-two-keybindings keybindingA keybindingB)))
                (if (equal? org maybe)
                    (loopB (cdr keybindingsB))
                    (begin
                      (assert (= (length maybe) 1))
                      (merge-keybindings (replace-merged-keybinding-in-keybindings keybindings
                                                                                   keybindingA
                                                                                   keybindingB
                                                                                   (car maybe)))))))))))
                                       
(***assert*** (merge-keybindings '(("Z" "ALT_R") ("Z" "ALT_L")))
              '(("Z" "ALT")))

(***assert*** (merge-keybindings '(("Z" "ALT_L" "CTRL_R")
                                   ("Z" "CTRL_L")))
              '(("Z" "ALT_L" "CTRL_R")
                ("Z" "CTRL_L")))

(***assert*** (merge-keybindings '(("Z" "ALT_R")
                                   ("Z" "ALT_L")
                                   ("Z" "CTRL_L")
                                   ("Z" "CTRL_R")
                                   ))
              '(("Z" "ALT")
                ("Z" "CTRL")))

(***assert*** (merge-keybindings '(("Z" "ALT_R")
                                   ("X" "CTRL_L")
                                   ))
              '(("Z" "ALT_R")
                ("X" "CTRL_L")))

(***assert*** (merge-keybindings '(("Z" "ALT_R")
                                   ("Z" "ALT_L")
                                   ("Y")
                                   ("X" "CTRL_L")
                                   ("X" "CTRL_R")
                                   ))
              '(("Z" "ALT")
                ("Y")
                ("X" "CTRL")))

(***assert*** (merge-keybindings '(("Z" "ALT_R" "CTRL_L")
                                   ("Z" "ALT_L" "CTRL_L")
                                   ("Z" "ALT_R" "CTRL_R")
                                   ("Z" "ALT_L" "CTRL_R")
                                   ))
              '(("Z" "ALT" "CTRL")))




(define (get-displayable-keybindings1 command)
  (define keybindings (get-keybindings-from-command-without-focus-and-mouse command))
  ;;(c-display "keybindings2:" keybindings2)
  ;;(c-display "keybindings3:" (merge-keybindings keybindings2))
  (remove-duplicates-in-sorted-list equal?  ;; call remove-duplicates again since merge-keybindings may have merged into several equal keybindings
                                    (merge-keybindings keybindings)))

#!!
(get-displayable-keybindings1 "ra.quantitizeRange")
(get-displayable-keybindings1 "ra.setEditorKeyboardFocus")
(get-displayable-keybindings1 "ra.evalScheme \"(replace-with-random-velocities-in-track)\"")
(for-each c-display (<ra> :get-keybindings-from-command "ra.setEditorKeyboardFocus"))
(<ra> :get-keybindings-from-command "ra.undo")
!!#

(define *key-display*
  (hash-table
   "DOWNARROW" "Down"
   "UPARROW" "Up"
   "RIGHTARROW" "Right"
   "LEFTARROW" "Left"
   
   "KP_DIV" "Keypad /"
   "KP_MUL" "Keypad *"
   "KP_SUB" "Keypad -"
   "KP_ADD" "Keypad +"
   
   "KP_0"   "Keypad 0"
   "KP_DOT" "Keypad ."
   "KP_ENTER" "Keypad Enter"
   
   "KP_1" "Keypad 1"
   "KP_2" "Keypad 2"
   "KP_3" "Keypad 3"
   
   "KP_4" "Keypad 4"
   "KP_5" "Keypad 5"
   "KP_6" "Keypad 6"
   
   "KP_7" "Keypad 7"
   "KP_8" "Keypad 8"
   "KP_9" "Keypad 9"

   "SPACE" "Space"
   "RETURN" "Return"
   "ENTER" "Enter"
   "BACKSPACE" "Backspace"
   "ESC" "Esc"
   "TAB" "Tab"
   "MENU" "Menu"
   
   "PAGE_UP" "Page Up"
   "PAGE_DOWN" "Page Down"
   "INSERT" "Insert"
   "HOME" "Home"
   "DEL" "Delete"
   "END" "End"
   "HOME" "Home"
   ))

(define (get-key-name key)
  (or (*key-display* key)      
      key))

(define (get-displayable-keybinding2 keybinding)
  (let loop ((qualifiers (cdr keybinding))
             (is-first #t))
    (<-> (if is-first "" " + ")
         (if (null? qualifiers)
             (get-key-name (car keybinding))
             (<-> (<ra> :get-qualifier-name (car qualifiers))
                  (loop (cdr qualifiers)
                        #f))))))

(define (get-displayable-keybindings command)
  (map get-displayable-keybinding2
       (get-displayable-keybindings1 command)))

(define (remove-focus-and-mouse-from-keybinding keybinding)
  (let* ((keys (if (list? keybinding)
                   keybinding
                   (string-split keybinding #\space)))
         (key (car keys))
         (qualifiers (cdr keys)))

    (cons key
          (remove (lambda (keybinding)
                    (or (string-starts-with? keybinding "FOCUS_")
                        (string-starts-with? keybinding "MOUSE_")))
                  qualifiers))))
     
(define (get-displayable-keybinding-from-keybinding keybinding)
  (get-displayable-keybinding2 (remove-focus-and-mouse-from-keybinding keybinding)))

  
#!!
(get-displayable-keybindings "ra.quantitizeRange")
(get-displayable-keybinding2 (list "F2" "FOCUS_EDITOR" "MOUSE_EDITOR"))
(get-displayable-keybindings "ra.undo")
(get-displayable-keybindings "ra.setEditorKeyboardFocus")
(for-each c-display (<ra> :get-keybindings-from-command "ra.setEditorKeyboardFocus"))
(<ra> :get-keybindings-from-command "ra.undo")
!!#


(define (get-keybindings-from-command-without-focus-and-mouse command)
  (remove-duplicates-in-sorted-list equal?
                                    (map (lambda (keybindings)
                                           (remove (lambda (keybinding)
                                                     (or (string-starts-with? keybinding "FOCUS_")
                                                         (string-starts-with? keybinding "MOUSE_")))
                                                   (string-split keybindings #\space)))
                                         (to-list (<ra> :get-keybindings-from-command command)))))

#!!
(get-keybindings-from-command-without-focus-and-mouse "ra.pasteSeqblocks")
(<ra> :get-keybindings-from-command "ra.pasteSeqblocks")
(get-menu-keybindings "ra.pasteSeqblocks")
(<ra> :get-qualifier-name "CTRL")
!!#

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


(define (get-displayable-keybinding rafuncname args)
  (if (< (string-length rafuncname) 3)
      ""
      (let* ((command (get-keybindings-command (get-python-ra-funcname rafuncname) args))
             (keybindings (get-displayable-keybindings command)))
        ;;(c-display "command:" command)
        ;;(c-display "keybindings:" keybindings)
        (if (null? keybindings)
            ""
            (last keybindings)))))


(define (FROM_C-get-displayable-keybinding rafuncname args)
  (get-displayable-keybinding rafuncname (to-list args)))

(***assert*** (get-displayable-keybinding "w" '())
              "")
(***assert*** (get-displayable-keybinding "we" '())
              "")

#!!
(get-displayable-keybinding "e" '())
(get-displayable-keybinding "ra:copy-track" '())
(get-displayable-keybinding "ra:paste-seqblocks" '())
(get-displayable-keybinding "ra:copy-paste-seqblocks" '())
(get-displayable-keybinding "ra:copy-selected-mixer-objects" '())

(<ra> :get-keybinding-from-command "ra.copyEditorTrackOnOffToSeqblock")

(get-displayable-keybinding "ra:eval-scheme" '("(moduloskew-track -1)"))
  
(let ((gakk (get-displayable-keybinding "ra:transpose-block" (list 1))))
  (c-display "gakk:" gakk)
  gakk)
(pretty-print (<ra> :get-keybindings-from-commands))
!!#


(define (remove-mouse-from-keybinding keybinding)
  (<-> (string-join (remove (lambda (key)
                              (string-starts-with? key "MOUSE_"))
                            (string-split keybinding #\ ))
                    " ")))

(define (add-qualifier-to-keybinding keybinding qualifier)
  (let* ((keys (if (list? keybinding)
                   keybinding
                   (string-split keybinding #\space)))
         (key (car keys))
         (qualifiers (cdr keys)))    
    (string-join (cons key (sort (cons qualifier
                                       qualifiers)
                                 string<?))
                 " ")))

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
              (<ra> :add-keybinding-to-conf-file keybinding (get-python-ra-funcname ra-funcname) (map get-arg-string args)))
            (define existing-command (<ra> :get-keybinding-from-keys (add-qualifier-to-keybinding keybinding "MOUSE_EDITOR")))
            (c-display "KEYBINDING:" keybinding ". EXISTING:" existing-command)
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
                           (<ra> :remove-keybinding-from-conf-file
                                 (let ((keybinding (last (get-keybindings-from-command-without-focus-and-mouse (get-keybindings-command python-ra-funcname args)))))
                                   (define ret (add-qualifier-to-keybinding keybinding "FOCUS_EDITOR"))
                                   (c-display "RET:" ret)
                                   ret)
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

