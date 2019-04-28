(provide 'main_menus.scm)

(my-require 'keybindings.scm)


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
                      (merge-keybindings (replace-merged-keybinding-in-keybindings keybindings keybindingA keybindingB (car maybe)))))))))))
                                       
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




(define (get-all-lines-in-file wfilename)
  (let ((file (<ra> :open-file-for-reading wfilename)))
    (let loop ((ret '()))
      (if (not (<ra> :file-at-end file))
          (loop (cons (<ra> :read-line-from-file file)
                      ret))
          (begin
            (<ra> :close-file file)
            (reverse ret))))))


(define (get-menu-indent-level line)
  (let loop ((level 0)
             (chars (string->list line)))
    (if (or (null? chars)
            (not (char=? #\tab (car chars))))
        level
        (loop (1+ level)
              (cdr chars)))))

(define-struct menu-line
  :indentation
  :is-separator
  :text
  :command
  :args
  :keybindings)

(define (get-menu-keybindings command)
  (define keybindings (get-keybindings-from-command-without-focus-and-mouse command))
  ;;(c-display "keybindings2:" keybindings2)
  ;;(c-display "keybindings3:" (merge-keybindings keybindings2))
  (remove-duplicates-in-sorted-list equal?  ;; call remove-duplicates again since merge-keybindings may have merged into several equal keybindings
                                    (merge-keybindings keybindings)))

#!!
(get-menu-keybindings "ra.quantitizeRange")
(get-menu-keybindings "ra.setEditorKeyboardFocus")
(for-each c-display (<ra> :get-keybindings-from-command "ra.setEditorKeyboardFocus"))
(<ra> :get-keybindings-from-command "ra.undo")
!!#

(define (create-menu-line-from-line line)
  (define parts (map string-strip (string-split line #\|)))
  (define command (cl-cadr parts))
  ;;(c-display "command:" command (and command (to-list (<ra> :get-keybindings-from-command command))))
  (define keybindings (and command
                           (get-menu-keybindings command)))
  
  ;;(c-display "Keybindings:" keybindings)
  (make-menu-line :indentation (get-menu-indent-level line)
                  :is-separator (string-starts-with? (car parts) "--")
                  :text (car parts)
                  :command command
                  :args (cl-cddr parts)
                  :keybindings keybindings))

#!!
(string-split "# a b c d #" #\#)
(string-split " " #\#)
!!#


(define (get-menu-lines wfilename)
  (map create-menu-line-from-line
       (remove (lambda (line)
                 (string=? "" (string-strip line))) ;; remove empty lines
               (map (lambda (line)
                      (if (or (string=? "" line)
                              (string-starts-with? line "#"))
                          ""
                          ((string-split line #\#) 0))) ;; remove comments
                    (get-all-lines-in-file wfilename)))))

#!!
(get-menu-lines (<ra> :to-base64 "/home/kjetil/radium/bin/menues.conf"))
!!#

(define (generate-menu-item-text text keybinding)
  (string-rightjustify text
                       40
                       (let loop ((qualifiers (cdr keybinding))
                                  (is-first #t))
                         (<-> (if is-first "" " + ")
                              (if (null? qualifiers)
                                  (car keybinding)
                                  (<-> (<ra> :get-qualifier-name (car qualifiers))
                                       (loop (cdr qualifiers)
                                             #f)))))))

(define (generate-menu-item-python-command command)
  (define pos (string-position " " command))
  (if (not pos)
      (<-> command "()")
      (<-> (string-take command pos)
           "("
           (string-drop command (+ pos 1))
           ")")))

(***assert*** (generate-menu-item-python-command "gakk 1")
              "gakk(1)")
(***assert*** (generate-menu-item-python-command "gakk")
              "gakk()")
(***assert*** (generate-menu-item-python-command "evalScheme '(list 0 1 2 3)'")
              "evalScheme('(list 0 1 2 3)')")


(define (add-menu-items menu-line)
  (define (printit line)
    (define command (and (menu-line :command) (generate-menu-item-python-command (menu-line :command))))
    (c-display (make-list (1+ (menu-line :indentation)) " ") "ra:add-menu-item" line command)
    (<ra> :add-menu-item line (or command "")))

  ;;(c-display "menuline:" menu-line)
  (let loop ((keybindings (menu-line :keybindings))
             (is-first #t))
    (if (or (not keybindings)
            (null? keybindings))
        (if is-first
            (printit (menu-line :text)))
        (let ((keybinding (car keybindings)))
          (printit (generate-menu-item-text (if is-first
                                                (menu-line :text)
                                                ".")
                                            keybinding))
          (loop (cdr keybindings)
                #f)))))

(define (generate-main-menus)
  (let loop ((menu-lines (get-menu-lines (<ra> :append-file-paths
                                               (<ra> :get-program-path)
                                               (<ra> :to-base64 "menues.conf"))))
             (last-indentation -1))
    (if (null? menu-lines)
        #t
        (let ((menu-line (car menu-lines))
              (next-menu-line (cl-cadr menu-lines)))
          ;;(c-display "menu-line:" menu-line)
          (define indentation (menu-line :indentation))
          (let loop ((indentation indentation))
            (when (< indentation last-indentation)
              (c-display (make-list (1+ indentation) " ") "ra:go-previous-menu-level")
              (<ra> :go-previous-menu-level)
              (loop (1+ indentation))))
          (cond ((and next-menu-line
                      (> (next-menu-line :indentation) indentation))
                 (c-display (make-list (1+ indentation) " ") "ra:add-menu-menu" (menu-line :text) (menu-line :command))
                 (<ra> :add-menu-menu (menu-line :text) "")
                 )
                ((menu-line :is-separator)
                 (c-display (make-list (1+ indentation) " ") "ra:add-menu-separator")
                 (<ra> :add-menu-separator)
                 )
                (else
                 (add-menu-items menu-line))
                )
          (loop (cdr menu-lines)
                indentation)))))

#!!
(generate-main-menus)

(<ra> :get-keybindings-from-command "ra.switchSoloTrack")
!!#
                     
