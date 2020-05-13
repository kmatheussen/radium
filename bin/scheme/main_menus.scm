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
  :keybindings
  :sub-menu #f)


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
                  :text (let ((text (car parts2)))
                          (if (string-starts-with? text "[")
                              (string-drop text
                                           (+ 1 (string-position "]" text)))
                              text))
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
       (keep (lambda (line)
               (set! line (string-strip line))
               ;;(c-display "LINE:" line " - " (string? line) (string=? "" line))
               (cond ((string=? "" line) ;; remove empty lines
                      #f)
                     ((and (string-starts-with? line "[linux]")
                           (not (string=? (<ra> :get-os-name) "linux")))
                      #f)
                     ((and (string-starts-with? line "[windows]")
                           (not (string=? (<ra> :get-os-name) "windows")))
                      #f)
                     ((and (string-starts-with? line "[macosx]")
                           (not (string=? (<ra> :get-os-name) "macosx")))
                      #f)
                     ((and (string-starts-with? line "[NSM]")
                           (not (<ra> :nsm-is-active)))
                      #f)
                     ((and (string-starts-with? line "[non-NSM]")
                           (<ra> :nsm-is-active))
                      #f)
                     (else
                      #t)))
             (map (lambda (line)
                    (if (or (string=? "" line)
                            (string-starts-with? line "#"))
                        ""
                        ((string-split line #\#) 0))) ;; remove comments
                  (get-all-lines-in-file wfilename)))))

#!!
(get-menu-items)
(generate-main-menus)
(pretty-print (get-menu-items))

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

(define (get-menu-items2)
  (let loop ((lines (get-menu-items))
             (indentation 0)
             (result '())
             (finished (lambda (result rest)
                         result)))
    (if (null? lines)
        (finished result '())
        (let ((line (car lines))
              (next-line (cl-cadr lines)))
          (cond ((and next-line
                      (> (next-line :indentation)
                         indentation))
                 (assert (= (next-line :indentation) (+ 1 indentation)))
                 (loop (cdr lines)
                       (next-line :indentation)
                       '()
                       (lambda (sub-result rest)
                         (loop rest
                               indentation
                               (append result (list (hash-table :text (line :text)
                                                                :sub-menu sub-result)))
                               finished))))
                ((= (line :indentation) indentation)
                 (loop (cdr lines)
                       indentation
                       (append result (list line))
                       finished))
                ((< (line :indentation) indentation)
                 (finished result lines))
                (else
                 (assert #f)))))))
    

#!!
(length (get-menu-items))

(pretty-print (last (get-menu-items)))
!!#

(define (get-correct-python-arg-type arg)
  (cond ((string-starts-with? arg "0")
         (string->number arg))
        ((string-starts-with? arg "1")
         (string->number arg))
        ((string-starts-with? arg "2")
         (string->number arg))
        ((string-starts-with? arg "3")
         (string->number arg))
        ((string-starts-with? arg "4")
         (string->number arg))
        ((string-starts-with? arg "5")
         (string->number arg))
        ((string-starts-with? arg "6")
         (string->number arg))
        ((string-starts-with? arg "7")
         (string->number arg))
        ((string-starts-with? arg "8")
         (string->number arg))
        ((string-starts-with? arg "9")
         (string->number arg))
        ((string-starts-with? arg "-1")
         (string->number arg))
        ((string-starts-with? arg "-2")
         (string->number arg))
        ((string-starts-with? arg "-3")
         (string->number arg))
        ((string-starts-with? arg "-4")
         (string->number arg))
        ((string-starts-with? arg "-5")
         (string->number arg))
        ((string-starts-with? arg "-6")
         (string->number arg))
        ((string-starts-with? arg "-7")
         (string->number arg))
        ((string-starts-with? arg "-8")
         (string->number arg))
        ((string-starts-with? arg "-9")
         (string->number arg))
        ((string=? arg "True")
         #t)
        ((string=? arg "False")
         #f)
        ((or (string-starts-with? arg "\"")
             (string-starts-with? arg "'"))
         (let ((arg (string-drop-right (string-drop arg 1) 1)))
           ;;(c-display "=======================AARGG: -" arg "-. After:"           (string-replace (string-replace arg "\"" "\\\"")
           ;;                                                                                       "'" "\\\""))
           (string-replace (string-replace arg "\"" "\\\\\"")
                           "'" "\\\\\"")))
        (else
         (c-display "=========================Unknown python arg:" arg)
         (assert #f))))
        
#!!
(get-correct-python-arg-type "23")
(string-replace "gakkgakk\"aiai" "\"" "\\\"")
(get-correct-python-arg-type "gakkgakk\"ai'ai")
(ra:eval-scheme "(ra:load-song (ra:get-path \"sounds/Radium_Care.rad\"))")
!!#

(define (get-popup-menu-items-from-menu-items items)
  (let loop ((items items))
    ;;(c-display "LOOPING. ITEMS:" (pp items) (null? items))
    (if (null? items)
        '()
        (let ((item (car items)))
          ;;(c-display "ITEM:" item)
          (cond ((item :is-separator)
                 ;;(c-display "AAAAAA")
                 (cons (item :text)
                       (loop (cdr items))))
                ((item :sub-menu)
                 ;;(c-display "SUB-MENU:" (item :sub-menu))
                 (cons (list (item :text)
                             (loop (item :sub-menu)))
                       (begin
                         ;;(c-display "DDDDDDDDDDDD:" items)
                         (loop (cdr items)))))
                (else
                 ;;(c-display "CCCCCCCC")
                 (define is-first #t)
                 (append (map (lambda (shortcut)
                                ;;(c-display "SHORTCUT:" shortcut)
                                (define text (if is-first
                                                 (item :text)
                                                 "."))
                                (set! is-first #f)
                                (split-menu-item-python-command
                                 (item :command)
                                 (lambda (a b)
                                   (list text
                                         :python-ra-command a (if (and (string? b) (string=? "" b))
                                                                  '()
                                                                  (if (not b)
                                                                      b
                                                                      (list (get-correct-python-arg-type b))))
                                         :shortcut (and shortcut (get-displayable-keybinding2 shortcut))
                                         (let ((command (and (item :command)
                                                             (generate-menu-item-python-command (item :command)))))
                                           (lambda ()
                                             ;;(c-display "Executing: -" command)
                                             (when command
                                               ;;(<ra> :add-to-program-log (<-> "menu: " command)) ;; not necessary. Already logged twice. both eval-python and popup menu are logged.
                                               (<ra> :eval-python command))))))))
                              (or (and (item :keybindings)
                                       (not (null? (item :keybindings)))
                                       (item :keybindings))
                                  (list #f)))
                         (loop (cdr items)))))))))

(define (popup-menu-from-menu-items items)
  (popup-menu (get-popup-menu-items-from-menu-items items)))

(define (generate-main-menus)
  (<ra> :wait-until-nsm-has-inited)
  (for-each (lambda (menu)
              (apply ra:add-menu-menu2 
                     (cons (menu :text)
                           (get-popup-menu-args
                            (get-popup-menu-items-from-menu-items
                             (menu :sub-menu)))))
              (<ra> :go-previous-menu-level))
            (get-menu-items2)))

#!!

(generate-main-menus)

(apply ra:add-menu-menu2 
       (append (list "hepp4")
               (get-popup-menu-args
                (get-popup-menu-items-from-menu-items
                 ((get-menu-items2) 5 :sub-menu)))))

(<ra> :go-previous-menu-level)

(for-each c-display
          (keep (lambda (keybinding)
                  (and (string-contains? (<-> (car keybinding)) "B")
                       (string-starts-with? (cdr keybinding) "ra.evalScheme")))
                (hash-table->alist (<ra> :get-keybindings-from-keys))))

(apply ra:add-menu-menu2 
       (append (list "hepp")
               (get-popup-menu-args
                (get-popup-menu-items-from-menu-items
                 ((get-menu-items2) 1 :sub-menu)))))

(pretty-print (append (list "hepp2")
                      (get-popup-menu-args
                       (get-popup-menu-items-from-menu-items
                        ((get-menu-items2) 5 :sub-menu)))))

(pretty-print (car (get-popup-menu-items-from-menu-items
                    ((get-menu-items2) 5 :sub-menu))))


(popup-menu-from-menu-items ((get-menu-items2) 0 :sub-menu))
(popup-menu-from-menu-items (get-menu-items2))

(pretty-print ((get-menu-items2) 0 :sub-menu))

(pretty-print ((get-menu-items2))

(<ra> :eval-python "ra.evalScheme('(ra:load-song (ra:get-path \"sounds/Radium_Care.rad\"))')")

(and #f (get-displayable-keybinding2 #f))

(popup-menu "hello"
            :shortcut #f
            (lambda ()
              (c-display "gakk")))

(pretty-print ((get-menu-items) 0 :sub-menu))
!!#

(define (generate-menu-item-text text keybinding)
  (string-rightjustify text
                       40
                       (get-displayable-keybinding2 keybinding)))

(define (split-menu-item-python-command command kont)
  (if (not command)
      (kont #f #f)
      (let ((pos (string-position " " command)))
        (if (not pos)
            (kont command "")
            (kont (string-take command pos)
                  (string-drop command (+ pos 1)))))))

(define (generate-menu-item-python-command command)
  (split-menu-item-python-command command
                                  (lambda (a b)
                                    (<-> a "(" b ")"))))

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

(define (generate-main-menus-old)
  (let loop ((menu-lines (get-menu-items))
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
                     
