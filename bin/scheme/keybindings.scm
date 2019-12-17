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

;; "shortcut"s are used in popup menues and things.
#!!
Examples:
:shortcut (list func arg1 arg2)
:shortcut ra:show-hide-swingtext
:shortcut "SHIFT + A"
!!!#
(define (get-displayable-keybinding-from-shortcut shortcut)
  (if (or (list? shortcut)
          (procedure? shortcut))
      (let ((keybinding (if (list? shortcut)
                            (get-displayable-keybinding (get-procedure-name (car shortcut)) (cdr shortcut))
                            (get-displayable-keybinding (get-procedure-name shortcut) '()))))
        (if (not (string=? keybinding ""))
            keybinding
            (begin
              ;;(c-display "Note: No keybinding found for procedure \"" (get-procedure-name shortcut) "\"")
              #f)))
      (<-> shortcut)))


(define (remove-mouse-from-keybinding keybinding)
  (<-> (string-join (remove (lambda (key)
                              (string-starts-with? key "MOUSE_"))
                            (string-split keybinding #\ ))
                    " ")))

(define (remove-focus-from-keybinding keybinding)
  (<-> (string-join (remove (lambda (key)
                              (string-starts-with? key "FOCUS_"))
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

(define (get-keyboard-focus-from-gui gui)
  (define parent-window (<gui> :get-parent-window gui))
  (let loop ((gui gui))
    (cond ((< gui 0)
           "")
          ((= gui (<gui> :get-editor-gui))
           "FOCUS_EDITOR")
          ((= gui (FROM-C-get-edit-gui))
           "FOCUS_EDITOR")          
          ((= gui (<gui> :get-sequencer-gui))
           "FOCUS_SEQUENCER")
          ((= gui (<gui> :get-main-mixer-gui))
           "FOCUS_MIXER")
          ((= gui (<gui> :get-main-mixer-strips-gui))
           "FOCUS_MIXER")
          ((= gui parent-window)
           "")
          (else
           (let ((parent (<gui> :get-parent-gui gui)))
             (if (= parent gui)
                 ""
                 (loop parent)))))))
         
#!!
(get-keyboard-focus-from-gui 327)
(<gui> :get-main-mixer-strips-gui)
!!#

(define (get-mouse-keybinding-from-focus-keybinding focus-keybinding)
  (string-join (let loop ((keybindings (string-split focus-keybinding #\space))
                          (gotit #f))
                 (if (null? keybindings)
                     '()
                     (let ((keybinding (car keybindings)))
                       (if gotit
                           (cons keybinding
                                 (loop (cdr keybindings)
                                       gotit))
                           (cond ((string=? keybinding "FOCUS_EDITOR")
                                  (cons "MOUSE_EDITOR"
                                        (loop (cdr keybindings)
                                              #t)))
                                 ((string=? keybinding "FOCUS_MIXER")
                                  (cons "MOUSE_MIXER"
                                        (loop (cdr keybindings)
                                              #t)))
                                 ((string=? keybinding "FOCUS_SEQUENCER")
                                  (cons "MOUSE_SEQUENCER"
                                        (loop (cdr keybindings)
                                              #t)))
                                 (else
                                  (cons keybinding
                                        (loop (cdr keybindings)
                                              #f))))))))
                                
               " "))

(define (get-mouse-binding-from-first-focus-binding focus-keybinding)
  (let loop ((keybindings (string-split focus-keybinding #\space)))
    (if (null? keybindings)
        ""
        (let ((keybinding (car keybindings)))
          (cond ((string=? keybinding "FOCUS_EDITOR")
                 "MOUSE_EDITOR")
                ((string=? keybinding "FOCUS_MIXER")
                 "MOUSE_MIXER")
                ((string=? keybinding "FOCUS_SEQUENCER")
                 "MOUSE_SEQUENCER")
                (else
                 (loop (cdr keybindings))))))))
  
#!!
(get-mouse-keybinding-from-focus-keybinding "FOCUS_SEQUENCER FOCUS_EDITOR FOCUS_EDITOR")
(get-mouse-keybinding-from-focus-keybinding "")
(get-mouse-keybinding-from-focus-keybinding "a b c")
(string-join (list "a" "b" "c") " ")
(string-join (list) " ")
(string-contains? "FOCUS_SEQUENCER FOCUS_EDITOR" "FOCUS_EDITOR")
!!#

(delafina (add-keybinding-configuration-to-gui :gui-or-area
                                               :ra-funcname
                                               :args '()
                                               :focus-keybinding ;; Can be either FOCUS_EDITOR, FOCUS_MIXER, FOCUS_SEQUENCER, a combination of those (separate them with space), or an empty string (all). If not set, it will be set automatically from where gui-or-area is placed. It will be set automatically when the user right-clicks so you don't have to set parent gui before calling this function.
                                               )

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

    (define focus-editor (string-contains? focus-keybinding "FOCUS_EDITOR"))
    (define focus-mixer (string-contains? focus-keybinding "FOCUS_MIXER"))
    (define focus-sequencer (string-contains? focus-keybinding "FOCUS_SEQUENCER"))
      
    (define focus-layout (<gui> :horizontal-layout))
    (<gui> :add focus-layout (<gui> :checkbox "Focus Editor" focus-editor (lambda (x) (set! focus-editor x))))
    (<gui> :add focus-layout (<gui> :checkbox "Focus Mixer" focus-mixer (lambda (x) (set! focus-mixer x))))
    (<gui> :add focus-layout (<gui> :checkbox "Focus Sequencer" focus-sequencer (lambda (x) (set! focus-sequencer x))))
    (<gui> :add grab-gui focus-layout)

    (<gui> :add-deleted-callback grab-gui (lambda _
                                            (<ra> :cancel-grab-keybinding)))
    
    (<gui> :add grab-gui (<gui> :button "Cancel" (lambda ()
                                                   (<gui> :close grab-gui))))

    (<gui> :set-parent grab-gui -1)
    (<gui> :show grab-gui)
    (<gui> :set-modal grab-gui #t)

    (<ra> :grab-keybinding
          (lambda (keybinding)
            (define selected-focus-keybinding (string-join (append (if focus-editor (list "FOCUS_EDITOR") '())
                                                                   (if focus-mixer (list "FOCUS_MIXER") '())
                                                                   (if focus-sequencer (list "FOCUS_SEQUENCER") '()))
                                                           " "))
            
            (c-display "KEYBINDING1:" keybinding)
            
            (set! keybinding (remove-mouse-from-keybinding keybinding))
            (set! keybinding (remove-focus-from-keybinding keybinding))
            
            (if (not (string=? "" selected-focus-keybinding))
                (set! keybinding (<-> keybinding " " selected-focus-keybinding)))
            
            (<gui> :close grab-gui)
            (define (setit!)
              (<ra> :add-keybinding-to-conf-file keybinding (get-python-ra-funcname ra-funcname) (map get-arg-string args)))
            (define existing-command (<ra> :get-keybinding-from-keys (add-qualifier-to-keybinding keybinding (get-mouse-binding-from-first-focus-binding focus-keybinding))))
            (c-display "KEYBINDING2:" keybinding ". Existing:" existing-command ". focus-keybinding:" focus-keybinding ". mouse:" (get-mouse-keybinding-from-focus-keybinding focus-keybinding))
            (if (string=? "" existing-command)
                (setit!)
                (show-async-message :text (<-> "Keybinding \"" (get-displayable-keybinding-from-keybinding keybinding) "\" already bound to \"" existing-command "\". Override?")
                                    :buttons '("Yes" "No")
                                    :callback (lambda (answer)
                                                (if (string=? "Yes" answer)
                                                    (setit!)))))
                                                    
            (c-display "hello1................. GUI:" (if (integer? gui-or-area)
                                                          gui-or-area
                                                          (gui-or-area :get-gui))))))
    
  (define (keybinding-mouse-handler button state x y)
    (if (not focus-keybinding)
        (set! focus-keybinding (let ((gui (if (integer? gui-or-area)
                                              gui-or-area
                                              (gui-or-area :get-gui))))
                                 (get-keyboard-focus-from-gui gui))))
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
                                   (define ret (add-qualifier-to-keybinding keybinding focus-keybinding))
                                   (c-display "RET:" ret)
                                   ret)
                                 python-ra-funcname
                                 (map get-arg-string args))
                           (c-display "removing")))
                        '())))
    #f)

  (if (integer? gui-or-area)
      (<gui> :add-mouse-callback gui-or-area keybinding-mouse-handler)
      (gui-or-area :add-mouse-cycle-prepend!
                   (lambda (button x* y*)
                     (c-display "HEPP")
                     (if (= button *right-button*)
                         (begin
                           (keybinding-mouse-handler button *is-pressing* x* y*)
                           'eat-mouse-cycle)
                         #f))))
  )

#!!
(for-each c-display (<ra> :get-keybindings-from-keys))

(get-keybindings-from-command-without-focus-and-mouse
 (get-keybindings-command (get-python-ra-funcname "ra:eval-scheme")
                          (list (<-> "(c-display " 0 ")"))))
(<ra> :get-keybindings-from-command
 (get-keybindings-command (get-python-ra-funcname "ra:eval-scheme")
                          (list (<-> "(c-display " 0 ")"))))
!!#
  
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

