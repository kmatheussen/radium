(provide 'keybindings.scm)

(my-require 'area.scm)

(<declare-variable> FROM-C-get-edit-gui) ;; in main_layout.scm
(<declare-variable> show-keybinding-help-window) ;; in various.scm
(<declare-variable> ensure-range-from-selection!) ;; in notem.scm



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

(***assert*** (maybe-merge-two-keybindings '("Z" "ALT_R")
                                           '("Z" "ALT_L"))
              '(("Z" "ALT")))

(***assert*** (maybe-merge-two-keybindings '("Z" "ALT_L" "CTRL_R")
                                           '("Z" "CTRL_L"))
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

#!!
(merge-keybindings '(("Z" "ALT_L" "SHIFT_L" "EDITOR")
                     ("Z" "ALT_L" "SHIFT_R" "EDITOR")
                     ("Z" "ALT_R" "SHIFT_L" "EDITOR")
                     ("Z" "ALT_R" "SHIFT_R" "EDITOR")
                     ))
!!#

#!!
(define (merge-mouse-and-focus-keybindings keybindings)
  (for-each (lambda (to-remove)
              (define new (remove-duplicates (remove to-remove keybindings)))
              (when (not (= (length new) (length keybindings)))
                (set! keybindings ...)))
            (list "FOCUS_EDITOR" "FOCUS_MIXER" "MOUSE_" ...))
  #t)
!!#

#!!
(merge-mouse-and-focus-keybindings
 '(("Q" "EXTRA_L" "FOCUS_EDITOR" "MOUSE_EDITOR")
   ("Q" "EXTRA_L" "FOCUS_EDITOR" "MOUSE_MIXER")
   ("Q" "EXTRA_L" "FOCUS_EDITOR" "MOUSE_MIXERSTRIPS")
   ("Q" "EXTRA_L" "FOCUS_EDITOR" "MOUSE_SEQUENCER")
   ("Q" "EXTRA_L" "FOCUS_MIXER" "MOUSE_SEQUENCER")
   ))
->
'(("Q" "EXTRAL_L" "FOCUS_EDITOR")
  ("Q" "EXTRA_L" "FOCUS_MIXER" "MOUSE_SEQUENCER"))
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
(get-keybindings-from-command-without-focus-and-mouse "keybinding.space_play_keyboard_handler")
(<ra> :get-keybindings-from-command "ra.pasteSeqblocks")
(get-menu-keybindings "ra.pasteSeqblocks")
(<ra> :get-qualifier-name "CTRL")

(get-keybindings-from-command-without-focus-and-mouse "ra.transposeTrack 12")
!!#


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
(get-displayable-keybindings1 "ra.undo")
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

   "PLAY" "Play"
   "STOP" "Stop"
   "CALCULATOR" "Calculator"
   "MAIL" "Mail"
   "HOMEPAGE" "Home page"
   "VOLUME_DOWN" "Volume Down"
   "VOLUME_UP" "Volume Up"
   "MUTE" "Mute"
   
   "QWERTY_1" "Qwerty 1"
   "QWERTY_2" "Qwerty 2"
   "QWERTY_3" "Qwerty 3"
   "QWERTY_4" "Qwerty 4"
   "QWERTY_5" "Qwerty 5"
   "QWERTY_6" "Qwerty 6"
   "QWERTY_7" "Qwerty 7"
   "QWERTY_8" "Qwerty 8"
   "QWERTY_9" "Qwerty 9"
   "QWERTY_0" "Qwerty 0"
   
   "QWERTY_Q" "Qwerty Q"
   "QWERTY_W" "Qwerty W"
   "QWERTY_E" "Qwerty E"
   "QWERTY_R" "Qwerty R"
   "QWERTY_T" "Qwerty T"
   "QWERTY_Y" "Qwerty Y"
   "QWERTY_U" "Qwerty U"
   "QWERTY_I" "Qwerty I"
   "QWERTY_O" "Qwerty O"
   "QWERTY_P" "Qwerty P"
   
   "QWERTY_A" "Qwerty A"
   "QWERTY_S" "Qwerty S"
   "QWERTY_D" "Qwerty D"
   "QWERTY_F" "Qwerty F"
   "QWERTY_G" "Qwerty G"
   "QWERTY_H" "Qwerty H"
   "QWERTY_J" "Qwerty J"
   "QWERTY_K" "Qwerty K"
   "QWERTY_L" "Qwerty L"
   
   "QWERTY_Z" "Qwerty Z"
   "QWERTY_X" "Qwerty X"
   "QWERTY_C" "Qwerty C"
   "QWERTY_V" "Qwerty V"
   "QWERTY_B" "Qwerty B"
   "QWERTY_N" "Qwerty N"
   "QWERTY_M" "Qwerty M"
   ))

(define (get-key-name key)
  (or (*key-display* key)      
      key))

(define (remove-focus-and-mouse-from-keys keys)
  (remove (lambda (key)
            (or (string-starts-with? key "FOCUS_")
                (string-starts-with? key "MOUSE_")))
          keys))
  
(define (get-displayable-keybinding2 keys)
  ;;(c-display "get-displayable-keybinding2. keys:" keys)
  (define (getit pre)
    (keep identity
          (map (lambda (key)
                 (cond ((string=? (<-> pre "EDITOR") key)
                        "Editor")
                       ((string=? (<-> pre "MIXER") key)
                        "Mixer")
                       ((string=? (<-> pre "SEQUENCER") key)
                        "Sequencer")
                       (else
                        #f)))
               keys)))
                                 
  (define focus-keys (getit "FOCUS_"))
  (define mouse-keys (getit "MOUSE_"))

  (string-join (append (if (null? focus-keys)
                           '()
                           (list (<-> (string-join focus-keys " or ")
                                      " has keyboard focus")))
                       (if (null? mouse-keys)
                           '()
                           (list (<-> "Mouse pointer inside the "
                                      (string-join mouse-keys " or the "))))
                       (map ra:get-qualifier-name (remove-focus-and-mouse-from-keys (cdr keys)))
                       (list (get-key-name (car keys))))
               " + "))

#!!
(get-displayable-keybinding2 (list "F2" "CTRL_L" "FOCUS_EDITOR" "FOCUS_MIXER"))
(get-displayable-keybinding2 (list "F2" "MOUSE_EDITOR" "MOUSE_MIXER"))
(get-displayable-keybinding2 (list "F2" "MOUSE_EDITOR" "MOUSE_MIXER" "FOCUS_EDITOR" "FOCUS_MIXER"))
(get-displayable-keybinding2 (list "F2" "CTRL_L" "CTRL_R" "MOUSE_EDITOR" "FOCUS_EDITOR"))
!!#

(define (remove-focus-and-mouse-from-keybinding-list keybinding)
  (let* ((keys (if (list? keybinding)
                   keybinding
                   (string-split keybinding #\space)))
         (key (car keys))
         (qualifiers (cdr keys)))
    (cons key
          (remove-focus-and-mouse-from-keys qualifiers))))
     
(define (get-displayable-keybindings command)
  (map get-displayable-keybinding2
       (get-displayable-keybindings1 command)))

(define (get-displayable-keybinding-from-keybinding keybinding)
  (get-displayable-keybinding2 (remove-focus-and-mouse-from-keybinding keybinding)))

  
#!!
(get-displayable-keybinding "ra.selectPrevSeqblock" '())
(get-displayable-keybinding "ra:select-prev-seqblock" '())
(get-displayable-keybindings "ra.selectPrevSeqblock")
(get-displayable-keybindings "ra.quantitizeRange")
(get-displayable-keybinding2 (list "F2" "FOCUS_EDITOR" "MOUSE_EDITOR"))
(get-displayable-keybindings "ra.undo")
(get-displayable-keybindings "ra.setEditorKeyboardFocus")
(for-each c-display (<ra> :get-keybindings-from-command "ra.setEditorKeyboardFocus"))
(<ra> :get-keybindings-from-command "ra.undo")
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


(define (get-python-arg-string arg)
  (cond ((eq? #f arg)
         "False")
        ((eq? #t arg)
         "True")
        (else
         (let ((arg2 (to-displayable-string arg)))
           (if (string? arg)
               (<-> "\"" arg2 "\"")
               arg2)))))

(define (get-keybindings-command rafuncname args)
  (<-> rafuncname
       (apply <-> (map (lambda (arg)
                         (<-> " " (get-python-arg-string arg)))
                       args))))
#!!
(get-keybindings-command "" '())
!!#

(define (get-keybindings-line keys rafuncname args)
  (<-> (string-join keys " ")
       " : "
       (get-keybindings-command rafuncname args)))


(delafina (get-displayable-keybinding :rafuncname
                                      :args
                                      :ra-funcname-is-in-python-format #f)
  ;;(c-display "a/b:" rafuncname args)
  (cond ((string=? "" rafuncname)
         "")
        (ra-funcname-is-in-python-format
         (let* ((command (get-keybindings-command rafuncname args))
                (keybindings (get-displayable-keybindings command)))
           ;;(c-display "command:" command)
           ;;(c-display "keybindings:" keybindings)
           (if (null? keybindings)
               ""
               (last keybindings))))
        ((string-starts-with? rafuncname "ra:")
         ;;(c-display "stuff:" (get-python-ra-funcname rafuncname) args)
         (let* ((command (get-keybindings-command (get-python-ra-funcname rafuncname) args))
                (keybindings (get-displayable-keybindings command)))
           ;;(c-display "command:" command)
           ;;(c-display "keybindings:" keybindings)
           (if (null? keybindings)
               ""
               (last keybindings))))
        ((not (defined? (string->symbol rafuncname)))
         "")
        (else
         (get-displayable-keybinding "ra:eval-scheme" (list (<-> "(" rafuncname
                                                                 (apply <-> (map (lambda (arg)
                                                                                   (<-> " " arg))
                                                                                 args))
                                                                 ")"))))))
             


(define (FROM_C-get-displayable-keybinding rafuncname args)
  (get-displayable-keybinding rafuncname (to-list args)))

(***assert*** (get-displayable-keybinding "w" '())
              "")
(***assert*** (get-displayable-keybinding "we" '())
              "")

#!!
(get-displayable-keybinding "ra:general-transpose-track-up" (list #t))
(get-displayable-keybinding "ra:general-transpose-track-up" '())
(get-displayable-keybinding "e" '())
(get-displayable-keybinding "ra:copy-track" '())
(get-displayable-keybinding "ra:paste-seqblocks" '())
(get-displayable-keybinding "ra:copy-paste-seqblocks" '())
(get-displayable-keybinding "ra:copy-selected-mixer-objects" '())

(<ra> :get-keybindings-from-command "ra.copyEditorTrackOnOffToSeqblock")

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
      (and shortcut
           (<-> shortcut))))


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

(define (remove-focus-and-mouse-from-keybinding keybinding)
  (remove-mouse-from-keybinding
   (remove-focus-from-keybinding
    keybinding)))

(define (add-qualifier-to-keybinding keybinding qualifier)
  (let* ((keys (if (list? keybinding)
                   keybinding
                   (string-split keybinding #\space)))
         (key (car keys))
         (qualifiers (cdr keys)))    
    (string-join (cons key
                       (sort (cons qualifier
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


(define (group-keybindings-by-command keybindings)
  (map (lambda (group)
         (define first (car group))
         (define command (cdr first))
         (hash-table :command command
                     :keybindings (map car group)))
       (group-list keybindings
                   (lambda (keybinding1 keybinding2)
                     (equal? (cdr keybinding1)
                             (cdr keybinding2))))))

#!!
(group-keybindings-by-command (get-existing-keybindings-from-keys "2"))
(for-each pretty-print (group-keybindings-by-command (get-existing-keybindings-from-keys "2")))
(length (group-keybindings-by-command (get-existing-keybindings-from-keys "2")))
!!#

(define (merge-keybindings-by-mouse-and-focus command-keybindings)
  (define (remove-if-all pre keys)
    (if (and (member (<-> pre "_SEQUENCER") keys)
             (member (<-> pre "_MIXER") keys)
             (member (<-> pre "_EDITOR") keys))
        (delete (<-> pre "_SEQUENCER")
                (delete (<-> pre "_MIXER")
                        (delete (<-> pre "_EDITOR")
                                keys
                                string=?)
                        string=?)
                string=?)
        keys))
  (copy-hash command-keybindings
             :keybindings
             (map (lambda (grouped)
                    (string-join (remove-if-all "MOUSE"
                                                (remove-if-all "FOCUS"
                                                               (remove-duplicates string<? string=?
                                                                                  (apply append (map (lambda (s)
                                                                                                       (string-split s #\space))
                                                                                                     grouped)))))
                                 " "))
                  (group-list (command-keybindings :keybindings)
                              (lambda (keybinding1 keybinding2)    
                                (string=? (remove-focus-and-mouse-from-keybinding keybinding1)
                                          (remove-focus-and-mouse-from-keybinding keybinding2)))))))

#!!
(for-each (lambda (a)
            (pretty-print a)(newline))
          (map merge-keybindings-by-mouse-and-focus
               (group-keybindings-by-command
                (get-existing-keybindings-from-keys "2 FOCUS_SEQUENCER FOCUS_EDITOR"))))


(delete 5 (list 2 3 4 5) =)
(map group-keybindings-by-non-mouse-and-non-focus-keys
     (group-keybindings-by-command (get-existing-keybindings-from-keys "2")))

(car (map group-keybindings-by-non-mouse-and-non-focus-keys
          (group-keybindings-by-command (get-existing-keybindings-from-keys "2"))))

(group-keybindings-by-command (get-existing-keybindings-from-keys "2"))

(((("2 FOCUS_SEQUENCER MOUSE_SEQUENCER" . "ra.setCurrSeqtrackConfigNum 1")
   ("2 FOCUS_SEQUENCER MOUSE_EDITOR" . "ra.setCurrSeqtrackConfigNum 1")
   ("2 FOCUS_SEQUENCER MOUSE_MIXER" . "ra.setCurrSeqtrackConfigNum 1")))
 ((("2 FOCUS_EDITOR MOUSE_SEQUENCER" . "ra.evalScheme \"(c-display 50)\"")
   ("2 FOCUS_EDITOR MOUSE_EDITOR" . "ra.evalScheme \"(c-display 50)\"")
   ("2 FOCUS_EDITOR MOUSE_MIXER" . "ra.evalScheme \"(c-display 50)\"")
   ("2 FOCUS_MIXER MOUSE_SEQUENCER" . "ra.evalScheme \"(c-display 50)\"")
   ("2 FOCUS_MIXER MOUSE_EDITOR" . "ra.evalScheme \"(c-display 50)\"")
   ("2 FOCUS_MIXER MOUSE_MIXER" . "ra.evalScheme \"(c-display 50)\""))))

(for-each pretty-print (group-keybindings-by-command (get-existing-keybindings-from-keys "2")))
(length (group-keybindings-by-command (get-existing-keybindings-from-keys "2")))
!!#

(define (get-existing-keybindings-from-keys keys-string)
  (define keys (string-split keys-string #\space))
  
  (define focus-keys (let ((ret (keep (lambda (key) (string-starts-with? key "FOCUS_")) keys)))
                       (if (null? ret)
                           (list "FOCUS_SEQUENCER" "FOCUS_EDITOR" "FOCUS_MIXER")
                           ret)))
  
  (define mouse-keys (let ((ret (keep (lambda (key) (string-starts-with? key "MOUSE_")) keys)))
                       (if (null? ret)
                           (list "MOUSE_SEQUENCER" "MOUSE_EDITOR" "MOUSE_MIXER")
                           ret)))

  (define key (car keys))

  (define qualifiers (remove (lambda (key)
                               (or (member? key focus-keys)
                                   (member? key mouse-keys)))
                             (cdr keys)))

  (define ret '())
  
  (let loop1 ((focus-keys focus-keys))
    (when (not (null? focus-keys))
      (let loop2 ((mouse-keys mouse-keys))
        (if (null? mouse-keys)
            (loop1 (cdr focus-keys))
            (let ((keys-string (string-join (cons key
                                                  (sort (cons (car focus-keys)
                                                              (cons (car mouse-keys)
                                                                    qualifiers))
                                                        string<=?))
                                            " ")))
              (define command (<ra> :get-keybinding-from-keys keys-string))
              ;;(c-display "command:" command ". keybinding: " keybinding)
              (if (not (string=? "" command))
                  (push-back! ret (cons keys-string command)))
              (loop2 (cdr mouse-keys)))))))

  ret
  )
#!!
(for-each c-display (get-existing-keybindings-from-keys "2 FOCUS_EDITOR FOCUS_SEQUENCER"))
(for-each c-display (get-existing-keybindings-from-keys "SPACE FOCUS_EDITOR"))
(get-existing-keybindings-from-keys "SPACE FOCUS_EDITOR")
(<ra> :get-keybinding-from-keys "SPACE FOCUS_EDITOR MOUSE_EDITOR")
(for-each c-display ((<ra> :get-keybindings-from-commands) 'keybinding.space_play_keyboard_handler))
(for-each c-display ((<ra> :get-keybindings-from-commands) 'ra.resetMixerZoom))
(for-each c-display ((<ra> :get-keybindings-from-commands) 'keybinding.switch_metronome))
(<ra> :get-keybinding-from-keys "FOCUS_EDITOR MOUSE_EDITOR SPACE")

(member? "a" (list "b" "a"))
(delete "a" (list "a" "b") string=?)
(string-join (list) " ")
(<ra> :get-keybinding-from-keys " ")
!!#

(define (get-prepared-existing-keybindings-from-keys keys) ;; keys is a string separated by spaces
  (map merge-keybindings-by-mouse-and-focus
       (group-keybindings-by-command
        (get-existing-keybindings-from-keys keys))))

#!!
(for-each (lambda (c)
            (pretty-print c)(newline))
          (get-prepared-existing-keybindings-from-keys "2"))
!!#

(define (get-html-from-prepared-existing-keybindings keybindings)
  (<-> "<table bgcolor=\"black\"> <tr>  <font color=\"white\">  <th><u>Keys</u></th>    <th><u>Function</u></th>    <th><u>Argument</u></th> </font> </tr>\n"
       ;;"<tr><td><hr></td><td><hr></td><td><hr></td></tr>\n"
       
       (apply <-> (map (lambda (prepared)
                         ;;(c-display "KEYBINDINGS:" (prepared :keybindings))
                         (apply <-> (map (lambda (keybinding)
                                           (define splitted (string-split (prepared :command) #\space))
                                           (<-> "<tr>\n"
                                                "  <td> <font color=\"green\"> " keybinding " </font> </td>\n"
                                                "  <td> <font color=\"green\">" (car splitted) "</font> </td>\n"
                                                (if (= (length splitted) 1)
                                                    "  <td></td>\n"
                                                    (<-> "  <td> <font color=\"green\">" (string-join (cdr splitted) " ") " </font> </td>\n"))                                                
                                                "</tr>"))
                                         (prepared :keybindings))))
                       keybindings))
       
       "</table>\n"))

#!!
(get-html-from-prepared-existing-keybindings (get-prepared-existing-keybindings-from-keys "2"))
(get-prepared-existing-keybindings-from-keys "2")
(map c-display "asdf")
!!#

(delafina (FROM_C-request-grab-keybinding :ra-funcname
                                          :args
                                          :focus-editor     ;; is ignored if command already has keybinding. Then it will use those keybindings.
                                          :focus-mixer      ;; (same)
                                          :focus-sequencer  ;; (same)
                                          :ra-funcname-is-in-python-format)
  
  (define python-ra-funcname (if ra-funcname-is-in-python-format
                                 ra-funcname
                                 (get-python-ra-funcname ra-funcname)))
  
  (define command (<-> python-ra-funcname
                       (apply <-> (map (lambda (arg)
                                         (<-> " " (get-python-arg-string arg)))
                                       args))))

  (let ((keybindings (to-list (<ra> :get-keybindings-from-command command))))
    (when (not (null? keybindings))
      (let ((oboy (apply <-> (keep (lambda (key)
                                     (string-starts-with? key "FOCUS_"))
                                   (string-split (apply append keybindings)
                                                 #\space)))))
        (set! focus-editor (string-contains? oboy "FOCUS_EDITOR"))
        (set! focus-mixer (string-contains? oboy "FOCUS_MIXER"))
        (set! focus-sequencer (string-contains? oboy "FOCUS_SEQUENCER"))

        ;;(c-display "========================= " focus-editor focus-mixer focus-sequencer oboy)
        )))

  (define (get-a-mouse-key)
    (cond (focus-editor
           "MOUSE_EDITOR")
          (focus-mixer
           "MOUSE_MIXER")
          (focus-sequencer
           "MOUSE_SEQUENCER")
          (else
           "MOUSE_EDITOR")))
           
  (define grab-gui (<gui> :vertical-layout))
  
  (define wait-text (mid-horizontal-layout (<gui> :text "Waiting for key....")))
  (<gui> :add grab-gui wait-text)
  
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

  (define (grabbed keybinding)
    (c-display "KEYBINDING1:" keybinding)
    
    (set! keybinding (remove-focus-and-mouse-from-keybinding keybinding))
    
    (define selected-focus-keybinding (string-join (append (if focus-editor (list "FOCUS_EDITOR") '())
                                                           (if focus-mixer (list "FOCUS_MIXER") '())
                                                           (if focus-sequencer (list "FOCUS_SEQUENCER") '()))
                                                   " "))
    
    (if (not (string=? "" selected-focus-keybinding))
        (set! keybinding (<-> keybinding " " selected-focus-keybinding)))
    
    (<gui> :close grab-gui) ;; This causes ra:cancel-grab-keybinding to be called, but that doesn't matter now.

    (define existing-commands (get-prepared-existing-keybindings-from-keys keybinding))
    ;;(define existing-command (<ra> :get-keybinding-from-keys (add-qualifier-to-keybinding keybinding (get-a-mouse-key))))
    
    (c-display "KEYBINDING2:" keybinding ". Existing:" existing-commands ". mouse:" (get-a-mouse-key))
    
    (define (setit!)
      (<ra> :add-keybinding-to-conf-file keybinding python-ra-funcname (map get-python-arg-string args)))
    
    (if (null? existing-commands)
        (setit!)
        (show-async-message :text (<-> "<style>table, th, td {  border: 2px solid white; background-color:black; border-collapse: collapse;}th, td {  padding: 5px;  text-align: left;    }</style>"
                                       ;;"Keybinding \""
                                       "<center>Keybinding already bound.</center>\n"
                                       "<p>\n"
                                       "\"" (get-displayable-keybinding2 (string-split keybinding #\space)) "\" is currently bound to:<br>"
                                       (get-html-from-prepared-existing-keybindings existing-commands)
                                       "<br>\n"
                                       "<p>Override?</p>")
                            :buttons '("Yes" "No")
                            :callback (lambda (answer)
                                        (if (string=? "Yes" answer)
                                            (setit!)))))
    )

  (<ra> :grab-keybinding grabbed))


#!!
(<ra> :schedule 100
      (lambda ()
        (FROM_C-request-grab-keybinding "ra.evalScheme" (list "(c-display 50)") #t #t #f)
        #f))

(<ra> :schedule 100
      (lambda ()
        (FROM_C-request-grab-keybinding "ra.load" '() #t #t #f)
        #f))

(apply <-> (keep (lambda (key)
                   (string-starts-with? key "FOCUS_"))
                 (string-split (apply append
                                      (to-list (<ra> :get-keybindings-from-command "ra.copyEditorTrackOnOffToSeqblock")))
                               #\space)))

(<ra> :get-keybindings-from-command "ra.copyEditorTrackOnOffToSeqblock2")

(apply append (to-list (<ra> :get-keybindings-from-command "ra.copyEditorTrackOnOffToSeqblock")))
(apply append (to-list (<ra> :get-keybindings-from-command "ra.showHideSequencerInWindow")))

(<ra> :get-keybindings-from-command "ra.showHideSequencerInWindow")


(<ra> :get-keybinding-from-keys "R FOCUS_EDITOR FOCUS_MIXER")
(<ra> :get-keybinding-from-keys "Q MOUSE_EDITOR UP")
(<ra> :get-keybinding-from-keys "2 FOCUS_EDITOR MOUSE_EDITOR")
(<ra> :get-keybinding-from-keys "LEFTARROW CTRL_L FOCUS_EDITOR MOUSE_MIXER")
(for-each (lambda (something)
            (define keystring (<-> (car something)))
            (if (string-starts-with? keystring "2 ")
                (c-display something)))
          (<ra> :get-keybindings-from-keys))



!!#

(delafina (get-keybinding-configuration-popup-menu-entries :ra-funcname
                                                           :args '()
                                                           :focus-keybinding ;; Can be either FOCUS_EDITOR, FOCUS_MIXER, FOCUS_SEQUENCER, a combination of those (separate them with space), or an empty string (all). If not set, it will be set automatically from where gui-or-area is placed. It will be set automatically when the user right-clicks so you don't have to set parent gui before calling this function. If gui-or-area is not set, current focus will be used.
                                                           :gui-or-area ;; if this one is set, and focus-keybinding is #f, focus-keybinding will be set automatically from gui-or-area.
                                                           :ra-funcname-is-in-python-format
                                                           )

  (when (and (not ra-funcname-is-in-python-format)
             (not (string-starts-with? ra-funcname "ra:"))
             (defined? (string->symbol ra-funcname)))
    (set! args (list (<-> "(" ra-funcname
                          (apply <-> (map (lambda (arg)
                                            (<-> " " arg))
                                          args))
                          ")")))
    (set! ra-funcname "ra:eval-scheme"))
   
  (define python-ra-funcname (if ra-funcname-is-in-python-format
                                 ra-funcname
                                 (get-python-ra-funcname ra-funcname)))
  
  (define command (<-> python-ra-funcname
                       (apply <-> (map (lambda (arg)
                                         (<-> " " (get-python-arg-string arg)))
                                       args))))

  (when (not (string? focus-keybinding))
    (if (not (<ra> :release-mode))
        (assert (eq? #f focus-keybinding)))
    (set! focus-keybinding (cond (gui-or-area
                                  (let ((gui (if (integer? gui-or-area)
                                                 gui-or-area
                                                 (gui-or-area :get-gui))))
                                    (get-keyboard-focus-from-gui gui)))
                                 ((<ra> :editor-has-keyboard-focus)
                                  "FOCUS_EDITOR")
                                 ((<ra> :mixer-has-keyboard-focus)
                                  "FOCUS_MIXER")
                                 ((<ra> :sequencer-has-keyboard-focus)
                                  "FOCUS_SEQUENCER")
                                 (else
                                  "FOCUS_EDITOR FOCUS_MIXER FOCUS_SEQUENCER"))))
                                 
  (define keybinding (get-displayable-keybinding ra-funcname args ra-funcname-is-in-python-format))
  (define has-keybinding (not (string=? keybinding "")))

  ;;(c-display "------------KEYBINDING:" keybinding)
  
  (list (if has-keybinding
            (<-> "Change keybinding for \"" command "\". (Now: " keybinding ")")
            (<-> "Add keybinding for \"" command "\""))
        (lambda ()

          (c-display "========================  focus-keybindign: " focus-keybinding)
  

          (define focus-editor (string-contains? focus-keybinding "FOCUS_EDITOR"))
          (define focus-mixer (string-contains? focus-keybinding "FOCUS_MIXER"))
          (define focus-sequencer (string-contains? focus-keybinding "FOCUS_SEQUENCER"))
          (FROM_C-request-grab-keybinding ra-funcname args focus-editor focus-mixer focus-sequencer ra-funcname-is-in-python-format))
        (if has-keybinding
            (list 
             "Remove the keybinding"
             (lambda ()
               (<ra> :remove-keybinding-from-conf-file
                     (let ((keybinding (last (get-keybindings-from-command-without-focus-and-mouse (get-keybindings-command python-ra-funcname args)))))
                       (define ret (add-qualifier-to-keybinding keybinding focus-keybinding))
                       (c-display "RET:" ret)
                       ret)
                     python-ra-funcname
                     (map get-python-arg-string args))
               (c-display "removing")))
            '())))

(delafina (add-keybinding-configuration-to-gui :gui-or-area
                                               :ra-funcname
                                               :args '()
                                               :focus-keybinding ;; Can be either FOCUS_EDITOR, FOCUS_MIXER, FOCUS_SEQUENCER, a combination of those (separate them with space), or an empty string (all). If not set, it will be set automatically from where gui-or-area is placed. It will be set automatically when the user right-clicks so you don't have to set parent gui before calling this function.
                                               )

  (define (keybinding-mouse-handler button state x y)
    (if (and (= button *right-button*)
             (= state *is-pressing*))
        (popup-menu (get-keybinding-configuration-popup-menu-entries :ra-funcname ra-funcname
                                                                     :args args
                                                                     :focus-keybinding focus-keybinding
                                                                     :gui-or-area gui-or-area)
                    "-------------"
                    "Help keybindings" show-keybinding-help-window
                    ))
    #f)

  (if (integer? gui-or-area)
      (<gui> :add-mouse-callback gui-or-area keybinding-mouse-handler)
      (gui-or-area :add-mouse-cycle-prepend!
                   (lambda (button x* y*)
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

(ensure-range-from-selection!)
!!#
  
(delafina (create-keybinding-button :name
                                    :ra-funcname
                                    :arguments '()
                                    :ensure-range-from-selection #f)
  (define requires-range (string-case-insensitive-contains? (<-> ra-funcname name) "range"))
  (define ra-func (eval-string ra-funcname))
  (define (func)
    (if (and requires-range
             ensure-range-from-selection)
        (ensure-range-from-selection!))
    (if (and requires-range
             (not (has-range)))
        (show-missing-range-message)
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
    (<gui> :set-text text texttext "menu_keybinding_text"))
  
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





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keybindings preferences GUI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (create-keybindings-editor)
  (let ((text-editor (<new> :text-editor :filename (<ra> :append-file-paths
                                                         (<ra> :get-home-path)
                                                         (<ra> :append-file-paths
                                                               (<ra> :get-path ".radium")
                                                               (<ra> :get-path "keybindings.conf")))
                            :include-load-button #f
                            :include-save-button #f
                            :include-save-as-button #f)))
    
    (define save+reload-button-text "Save + Reload keybindings (F5)")
    
    (define save+reload-button #f)
    
    (define (save+reload)
      (<gui> :set-text save+reload-button "Please wait...")
      (<ra> :schedule 30
            (lambda ()
              (<gui> :editor-save (text-editor :editor))
              (<ra> :reload-keybindings)
              (<gui> :set-text save+reload-button save+reload-button-text)
              #f)))
    
    (set! save+reload-button (<gui> :button save+reload-button-text save+reload))
    
    (<gui> :add (text-editor :vertical-widget) save+reload-button
           2)
    
    (<gui> :add-key-callback (text-editor :editor)
           (lambda (presstype key)
             ;;(c-display "press:" presstype "key: " key)
             (if (string=? key "F5")
                 (begin
                   (if (= presstype 0)
                       (save+reload))
                   #t)
                 #f)))

    (<gui> :editor-add-text-changed-callback
           (text-editor :editor)
           (lambda ()
             ;;(c-display "HEPP")
             (<gui> :set-text save+reload-button (<-> "*" save+reload-button-text))))
    
    (<gui> :set-size (text-editor :gui) (round (/ (<gui> :width -1) 1.3)) (round (/ (<gui> :height -1) 1.3)))
    
    (text-editor :show-window)

    text-editor))

(define *keybindings-editor* #f)

(define (FROM_C-show-keybindings-editor)
  (if (or (not *keybindings-editor*)
          (not (<gui> :is-open (*keybindings-editor* :gui))))
      (set! *keybindings-editor* (create-keybindings-editor))
      (<gui> :show (*keybindings-editor* :gui))))



;; stuff below not finished.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-area-subclass (<keybinding-config-entry> :gui :x1 :y1 :x2 :y2
                                              :keys
                                              :function
                                              :args)
;  (define border 2)
;  (define key-x2 (* 1.5 (<gui> :text-width "Keybinding" "CTRL + EDITOR + MIXER + HOME")))
;  (define command-x1 (+ key-x2 border))
;  (define command-x2 (+ command-x1 (* 1.5 (<gui> :text-width "ra.DosomethingEvil!"))))
;  (define args-x1 (+ command-x2 border))
;  (define args-x2 (+ args-x1 (* 1.5 (<gui> :text-width "True"))))

  (define-override (paint)
    (<gui> :draw-text gui "black" (<-> keys function args)
           x1
           y1
           x2
          y2
           #f ;;wrap-lines
           #f ;;align-top
           #t ;;align-left
           0  ;;rotate
           #t ;;cut-text-to-fit
           #f ;;scale-font-size
           #f ;;text-is-base64
           ))

  )


(define (create-keybindings-table-gui)

  (define gui (<gui> :ui (<ra> :append-file-paths
                               (<ra> :get-program-path)
                              (<ra> :get-path "keybindings.ui"))))
  (<gui> :set-window-title gui "Keybindings")

  (define doit #f)

 (define search-string "")

  (define search-text (<gui> :child gui "search_text"))
  (<gui> :hide (<gui> :child gui "search_button"))

 (<gui> :add-realtime-callback search-text
         (lambda (new-text)
           (when doit
             (set! search-string new-text)
             ;;(c-display "SETTING search to" new-text)
             (update-rows!))))

 (<gui> :set-layout-spacing (<gui> :child gui "searchWidget") 0 0 0 0 2)
  
  (define width (floor (* 3 (<gui> :text-width "CTRL + EDITOR + MIXER + HOME ra.DosomethingEvil! True Delete"))))
  ;;(define height 
  (<gui> :set-size gui width (floor (* width 0.7)))

  (define curr-data (vector))

  (define fontheight (get-fontheight))
  (define entry-height (round (* 1.2 fontheight)))
  
  (define (recreate-table-gui gui width height state)
    (c-display "RECREATING TABLE-GUI " gui width height curr-data entry-height)
    (<new> :vertical-list-area2 gui 0 0 width height
           :num-sub-areas (vector-length curr-data)
           :get-sub-area-height entry-height
           :create-sub-area
           (lambda (num x1 x2)
             (c-display "RECREATING NUM" num)
             (define binding (vector-ref curr-data num))
             (<new> :keybinding-config-entry gui x1 0 x2 (- entry-height 1)
                    (binding :keys)
                    (binding :function)
                    (binding :args)))))
  
  (define table (make-qtarea :width width :height (floor (* width 0.6))
                             :sub-area-creation-callback recreate-table-gui))
  
 (define table-gui (table :get-gui))


  '(define table (create-table-gui (list (make-table-row "Keybinding" "CTRL + EDITOR + MIXER + HOME" #f)
                                        (make-table-row "Function" "ra.DosomethingEvil!" #f)
                                        (make-table-row "Argument" "True" #t)
                                        (make-table-row "Delete" #f #f))
                                  :hide-callback (lambda (table)
                                                   (<gui> :close table))
                                  :curr-selected-row-changed-callback (lambda (table row-num row-content)
                                                                        (when doit
                                                                          (c-display "Keybinding GUI: Selected row changed:" row-content row-num)
                                                                          ))))
                                                                        
  
  (let ((table-parent (<gui> :child gui "tableParent")))
    (<gui> :set-layout-spacing table-parent 0 0 0 0 2)
    (<gui> :set-layout-spacing gui 0 2 2 2 2)
    (<gui> :add table-parent table-gui)
    ;;(<gui> :show table-gui)
    )

  (define (create-keybinding-entry keybinding rownum)
    (<gui> :button keybinding
           (lambda ()
             (c-display "Change keybinding. Fix")
             (update-rows!))))
  
  (define (get-data search-string)
    ;;(set! search-string "zoom")
    (list->vector
     (keep identity
           (map (lambda (keybinding)
                  (define keys (to-string (car keybinding)))
                  ;;(c-display "KEYBINDING:" keys (string? keys))
                  ;;(c-display "KEYBINDING2:" (cdr keybinding) (string? (cdr keybinding)))
                  (define func-and-arg (string-split (cdr keybinding) #\space))
                  (define func (car func-and-arg))
                  (define arg (string-join (cdr func-and-arg) " "))
                  
                  (and (or (string=? search-string "")
                           (string-case-insensitive-contains? keys search-string)
                           (string-case-insensitive-contains? func search-string)
                           (string-case-insensitive-contains? arg search-string))
                       (hash-table :keys keys
                                   :function func
                                   :argument arg)))
                (map identity (<ra> :get-keybindings-from-keys))))))
    
  (define (update-rows!)
    (define data (get-data search-string))
    (when (or #t (not (morally-equal? data curr-data)))
      (set! curr-data data)))

  (update-rows!)

  '(<ra> :schedule 1100
        (lambda ()
          (cond ((not (<gui> :is-open gui))
                 #f)
                (else
                 (update-rows!)
                 200))))

  (define close-button (<gui> :child gui "close_button"))
  (<gui> :add-callback close-button (lambda ()
                                      (<gui> :close gui)))

  (<gui> :set-takes-keyboard-focus gui #f)

  (<gui> :set-parent gui -1) ;; Set parent to the main window.

  (<gui> :show gui)

  gui)


(if (not *is-initializing*)
    (create-keybindings-table-gui))


(define *keybindings-table-gui* #f)  

(define (FROM_C-create-keybindings-table-gui)
  (if (or (not *keybindings-table-gui*)
          (not (<gui> :is-open *keybindings-table-gui*)))
      (set! *keybindings-table-gui* (create-keybindings-table-gui))
      (<gui> :raise *keybindings-table-gui*)))

  
#!!
(FROM_C-create-keybindings-table-gui)
(car (map identity (<ra> :get-keybindings-from-keys)))
(<ra> :get-keybindings-from-keys)

;;(map identity (<ra> :get-keybindings-from-commands))

(define (get-data search-string)
  (map (lambda (keybinding)
         (define keys (to-string (car keybinding)))
         ;;(c-display "KEYBINDING:" keys (string? keys))
         ;;(c-display "KEYBINDING2:" (cdr keybinding) (string? (cdr keybinding)))
         (define func-and-arg (string-split (cdr keybinding) #\space))
        (define func (car func-and-arg))
         (define arg (string-join (cdr func-and-arg) " "))
         
         (and (or (string=? search-string "")
                  (string-case-insensitive-contains? keybinding search-string)
                  (string-case-insensitive-contains? func-and-arg search-string))
              (hash-table :keys keys
                          :function func
                          :argument arg)))
       (map identity (<ra> :get-keybindings-from-keys))))

(car (get-data ""))

!!#
