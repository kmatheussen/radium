(provide 'main_menus.scm)

(my-require 'keybindings.scm)


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

(define (create-menu-line-from-line line)
  (define parts (map string-strip (string-split line #\|)))
  (define parts2 (map string-strip (string-split (car parts) #\^)))
  (define command (cl-cadr parts))
  ;;(c-display "command:" command (and command (to-list (<ra> :get-keybindings-from-command command))))
  (define keybindings (if (cl-cadr parts2)
                          (list (list (cl-cadr parts2)))
                          (and command
                               (get-displayable-keybindings1 command))))
  
  ;;(c-display "Keybindings:" keybindings)
  (make-menu-line :indentation (get-menu-indent-level line)
                  :is-separator (string-starts-with? (car parts2) "--")
                  :text (car parts2)
                  :command command
                  :args (cl-cddr parts)
                  :keybindings keybindings))

#!!
(pretty-print (create-menu-line-from-line "	Seqblock Delete                         ^ Shift + Right mouse button | ra.deleteSelectedSeqblocks"))
(pretty-print (create-menu-line-from-line "	Open 		| ra.load"))



(string-split "# a b c d #" #\#)
(string-split " " #\#)
!!#


(delafina (get-menu-items :wfilename (<ra> :append-file-paths
                                           (<ra> :get-program-path)
                                           (<ra> :get-path "menues.conf")))
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
(get-menu-lines (<ra> :get-path "/home/kjetil/radium/bin/menues.conf"))
(get-all-lines-in-file (<ra> :get-path "/home/kjetil/radium/bin/menues.conf"))

(define wfilename (<ra> :get-path "/home/kjetil/radium/bin/menues.conf"))

(map create-menu-line-from-line
     (remove (lambda (line)
               (string=? "" (string-strip line))) ;; remove empty lines
             (map (lambda (line)
                    (if (or (string=? "" line)
                            (string-starts-with? line "#"))
                        ""
                        ((string-split line #\#) 0))) ;; remove comments
                  (get-all-lines-in-file wfilename))))


!!#

(define (generate-menu-item-text text keybinding)
  (string-rightjustify text
                       40
                       (get-displayable-keybinding2 keybinding)))

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
                                               (<ra> :get-path "menues.conf"))))
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
                     
