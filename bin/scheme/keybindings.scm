(provide 'keybindings.scm)

(<declare-variable> FROM-C-get-edit-gui) ;; in main_layout.scm


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
                       (list (car keys)))
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

(define (get-existing-keybindings-from-keys keybinding)
  (define keys (string-split keybinding #\space))
  
  (define focus-keys (let ((ret (keep (lambda (key) (string-starts-with? key "FOCUS_")) keys)))
                       (if (null? ret)
                           (list "FOCUS_SEQUENCER" "FOCUS_EDITOR" "FOCUS_MIXER")
                           ret)))
  
  (define mouse-keys (let ((ret (keep (lambda (key) (string-starts-with? key "MOUSE_")) keys)))
                       (if (null? ret)
                           (list "MOUSE_SEQUENCER" "MOUSE_EDITOR" "MOUSE_MIXER")
                           ret)))
                       
  (define other-keys (remove (lambda (key)
                               (or (member? key focus-keys)
                                   (member? key mouse-keys)))
                             keys))

  (define ret '())
  
  (let loop1 ((focus-keys focus-keys))
    (when (not (null? focus-keys))
      (let loop2 ((mouse-keys mouse-keys))
        (if (null? mouse-keys)
            (loop1 (cdr focus-keys))
            (let ((keybinding (string-join (sort (cons (car focus-keys)
                                                       (cons (car mouse-keys)
                                                             other-keys))
                                                 string<=?)
                                           " ")))
              (define command (<ra> :get-keybinding-from-keys keybinding))
              (if (not (string=? "" command))
                  (push-back! ret (cons keybinding command)))
              (loop2 (cdr mouse-keys)))))))

  ret
  )
#!!
(for-each c-display (get-existing-keybindings-from-keys "2 FOCUS_EDITOR FOCUS_SEQUENCER"))
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

(define (FROM_C-request-grab-keybinding ra-funcname args focus-editor focus-mixer focus-sequencer)
  (define (get-arg-string arg)
    (let ((arg2 (to-displayable-string arg)))
      (if (string? arg)
          (<-> "\"" arg2 "\"")
          arg2)))
  
  (define command (<-> (get-python-ra-funcname ra-funcname)
                       (apply <-> (map (lambda (arg)
                                         (<-> " " (get-arg-string arg)))
                                       args))))

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
      (<ra> :add-keybinding-to-conf-file keybinding (get-python-ra-funcname ra-funcname) (map get-arg-string args)))

    (if (null? existing-commands)
        (setit!)
        (show-async-message :text (<-> "<style>table, th, td {  border: 2px solid white;  border-collapse: collapse;}th, td {  padding: 5px;  text-align: left;    }</style>"
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
                                                           :focus-keybinding ;; Can be either FOCUS_EDITOR, FOCUS_MIXER, FOCUS_SEQUENCER, a combination of those (separate them with space), or an empty string (all). If not set, it will be set automatically from where gui-or-area is placed. It will be set automatically when the user right-clicks so you don't have to set parent gui before calling this function.
                                                           :gui-or-area ;; if this one is set, and focus-keybinding is #f, focus-keybinding will be set automatically from gui-or-area.
                                                           )

  (assert (or focus-keybinding gui-or-area))
  
  (define (get-arg-string arg)
    (let ((arg2 (to-displayable-string arg)))
      (if (string? arg)
          (<-> "\"" arg2 "\"")
          arg2)))
  
  (define command (<-> (get-python-ra-funcname ra-funcname)
                       (apply <-> (map (lambda (arg)
                                         (<-> " " (get-arg-string arg)))
                                       args))))

  (if (not focus-keybinding)
      (set! focus-keybinding (let ((gui (if (integer? gui-or-area)
                                            gui-or-area
                                            (gui-or-area :get-gui))))
                               (get-keyboard-focus-from-gui gui))))
  (define keybinding (get-displayable-keybinding ra-funcname args))
  (define has-keybinding (not (string=? keybinding "")))
    
  (list (if has-keybinding
            (<-> "Change keybinding for \"" command "\". (Now: " keybinding ")")
            (<-> "Add keybinding for \"" command "\""))
        (lambda ()
          (define focus-editor (string-contains? focus-keybinding "FOCUS_EDITOR"))
          (define focus-mixer (string-contains? focus-keybinding "FOCUS_MIXER"))
          (define focus-sequencer (string-contains? focus-keybinding "FOCUS_SEQUENCER"))
          (FROM_C-request-grab-keybinding ra-funcname args focus-editor focus-mixer focus-sequencer))
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
                                                                     :gui-or-area gui-or-area)))
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

