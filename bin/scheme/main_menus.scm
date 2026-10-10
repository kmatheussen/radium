(provide 'main_menus.scm)

(my-require 'keybindings.scm)
(my-require 'popupmenu.scm)


(define (include-menu-item? line)
  (set! line (string-strip line))
  (cond ((and (string-starts-with? line "[Linux]")
              (not (string=? (<ra> :get-os-name) "linux")))
         #f)
        ((and (string-starts-with? line "[Windows]")
              (not (string=? (<ra> :get-os-name) "windows")))
         #f)
        ((and (string-starts-with? line "[Darwin]")
              (not (string=? (<ra> :get-os-name) "macosx")))
         #f)
        ((and (string-starts-with? line "[non-32bit]")
              (string=? (<ra> :get-architecture-name) "i686"))
         #f)
        ((and (string-starts-with? line "[NSM]")
              (not (<ra> :nsm-is-active)))
         #f)
        ((and (string-starts-with? line "[non-NSM]")
              (<ra> :nsm-is-active))
         #f)
        (else
         #t)))


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
  :include?
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

  (define text (let ((text (car parts2)))
                 (if (string-starts-with? text "[")
                     (string-drop text
                                  (+ 1 (string-position "]" text)))
                     text)))
  
  (make-menu-line :indentation (get-menu-indent-level line)
                  :is-separator (string-starts-with? text "--")
                  :text text
                  :include? (include-menu-item? (car parts2))
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
               (not (string=? "" line))) ;; remove empty lines
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
                               (if (line :include?)
                                   (append result
                                           (list (hash-table :text (line :text)
                                                             :sub-menu sub-result)))
                                   result)
                               finished))))
                ((= (line :indentation) indentation)
                 (loop (cdr lines)
                       indentation
                       (if (line :include?)
                           (append result (list line))
                           result)
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

(define (get-recently-opened-song-filenames)
  (let loop ((i 0)
             (result '()))
    (if (= i 30)
        (reverse result)
        (let ((filename (<ra> :get-settings (<-> "recent_song_" i) "")))
          (if (string=? filename "")
              (reverse result)
              (loop (1+ i)
                    (cons filename result)))))))

(define (get-recently-opened-songs-popup-items)
  (define filenames (get-recently-opened-song-filenames))
  (if (null? filenames)
      (list "No recently opened songs" :enabled #f (lambda () #f))
      (map (lambda (filename)
             (list filename
                   (lambda ()
                     (<ra> :load-song (<ra> :get-path filename)))))
           filenames)))

;; Pure function so it can be tested with ***assert*** below.
(define (replace-recent-menu items recent-items)
  (map (lambda (item)
         (cond ((and (list? item)
                     (not (null? item))
                     (string? (car item))
                     (string=? (car item) "Recent-main-menu-99992222"))
                (if recent-items
                    (list "Recently-opened-songs" recent-items)
                    '()))
               ((and (list? item)
                     (not (null? item))
                     (string? (car item))
                     (not (null? (cdr item)))
                     (list? (cadr item)))
                (list (car item)
                      (replace-recent-menu (cadr item) recent-items)))
               (else
                item)))
       items))

(***assert*** (replace-recent-menu (list "separator"
                                         (list "Recent-main-menu-99992222"
                                               (list "x"))
                                         (list "Sub"
                                               (list (list "Recent-main-menu-99992222"
                                                           (list "y")))))
                                   (list "recent"))
              (list "separator"
                    (list "Recently-opened-songs"
                          (list "recent"))
                    (list "Sub"
                          (list (list "Recently-opened-songs"
                                      (list "recent"))))))

(define *main-menu-search-popup-args* #f)

(define *main-menu-search-options* #f)

(define *main-menu-items* #f)

;; Shortcuts are part of the cached data, so it must be regenerated when keybindings change.
(add-reload-keybindings-callback (lambda ()
                                   (set! *main-menu-search-popup-args* #f)
                                   (set! *main-menu-search-options* #f)
                                   (set! *main-menu-items* #f)))

(define (get-main-menu-items)
  (when (not *main-menu-items*)
    (set! *main-menu-items* (get-menu-items2)))
  *main-menu-items*)

;; Removes the "Recent-main-menu-99992222" submenu from a parsed popup menu options list.
;; (Temporarily disabled while investigating why the popup takes long time to appear.)
(define (remove-recent-from-options options)
  (let loop ((options options)
             (skip-depth 0)
             (result '()))
    (if (null? options)
        (reverse result)
        (let ((text (car options))
              (callback (cadr options))
              (rest (cddr options)))
          (cond ((> skip-depth 0)
                 (if (string? text)
                     (cond ((string-starts-with? text "[submenu start]")
                            (loop rest (1+ skip-depth) result))
                           ((string-starts-with? text "[submenu end]")
                            (loop rest (1- skip-depth) result))
                           (else
                            (loop rest skip-depth result)))
                     (loop rest skip-depth result)))
                ((and (string? text)
                      (string=? text "[submenu start]Recent-main-menu-99992222"))
                 (loop rest 1 result))
                (else
                 (loop rest 0 (cons callback (cons text result)))))))))

(define (assemble-main-menu-search-options menus-options)
  (apply append
         (map (lambda (menu-options)
                (append (list (<-> "[submenu start]" (car menu-options))
                              (lambda () #t))
                        (cdr menu-options)
                        (list "[submenu end]"
                              (lambda () #t))))
              menus-options)))

(define *main-menu-popup-generation* 0)

;; Called from C++ when the hamburger popup is closed, to cancel any scheduled opening of it.
(define (FROM_C-cancel-main-menu-popup)
  (set! *main-menu-popup-generation* (+ *main-menu-popup-generation* 1)))

;; A generation of #f means the popup is not the hamburger popup, and can't be cancelled.
(define (main-menu-popup-generation-is-current? generation)
  (or (not generation)
      (= generation *main-menu-popup-generation*)))

(define (open-main-menu-search-popup args generation)
  (<ra> :schedule 0
        (lambda ()
          (when (main-menu-popup-generation-is-current? generation)
            (popup-menu-from-args args))
          #f)))

(define (open-main-menu-search-popup-from-options menus-options generation)
  (when (main-menu-popup-generation-is-current? generation)
    (c-display "SEARCHPOPUP open-from-options" (<ra> :get-ms))
    (define args (get-popup-menu-args-from-options (assemble-main-menu-search-options menus-options)))
    (set! *main-menu-search-popup-args* args)
    (open-main-menu-search-popup args generation)))

;; Builds the options one top-level menu at a time, scheduling a new step between
;; each menu. Only used if generate-main-menus didn't already build them.
(define (build-main-menu-search-popup menus chunks generation)
  (when (main-menu-popup-generation-is-current? generation)
    (if (null? menus)
        (let ((menus-options (reverse chunks)))
          (set! *main-menu-search-options* menus-options)
          (open-main-menu-search-popup-from-options menus-options generation))
        ;; Schedule the processing of each menu, so that the gui gets a chance
        ;; to repaint the wait popup between each top-level menu.
        (<ra> :schedule 0
              (lambda ()
                (when (main-menu-popup-generation-is-current? generation)
                  (let* ((menu (car menus))
                         (menu-options (remove-recent-from-options
                                        (parse-popup-menu-options
                                         (get-popup-menu-items-from-menu-items (menu :sub-menu))))))
                    (c-display "SEARCHPOPUP chunk" (menu :text) (<ra> :get-ms))
                    (build-main-menu-search-popup (cdr menus)
                                                  (cons (cons (menu :text) menu-options) chunks)
                                                  generation)))
                #f)))))

(define* (popup-search-all-menus (generation #f))
  (if *main-menu-search-popup-args*
      (open-main-menu-search-popup *main-menu-search-popup-args* generation)
      (when (main-menu-popup-generation-is-current? generation)
        (c-display "SEARCHPOPUP wait-screen-start" (<ra> :get-ms))
        (<ra> :show-popup-search-wait-screen)
        (c-display "SEARCHPOPUP wait-screen-shown" (<ra> :get-ms))
        (<ra> :schedule 1
              (lambda ()
                (when (main-menu-popup-generation-is-current? generation)
                  (c-display "SEARCHPOPUP build-start" (<ra> :get-ms))
                  (if *main-menu-search-options*
                      (open-main-menu-search-popup-from-options *main-menu-search-options* generation)
                      (build-main-menu-search-popup (get-main-menu-items) '() generation)))
                #f)))))

;; Called from the hamburger button in the bottom bar and from the left alt key.
(define (FROM_C-popup-main-menus)
  (set! *main-menu-popup-generation* (+ *main-menu-popup-generation* 1))
  (define generation *main-menu-popup-generation*)
  ;; Must be scheduled since this function can be called from within a native event filter.
  (<ra> :schedule 0
        (lambda ()
          (when (main-menu-popup-generation-is-current? generation)
            (popup-search-all-menus generation))
          #f)))

;; Called when right-clicking the hamburger button in the bottom bar.
(define (FROM_C-show-hamburger-keybinding-popup)
  (popup-menu (get-keybinding-configuration-popup-menu-entries :ra-funcname "ra:open-main-menu-popup"
                                                               :args '()
                                                               :focus-keybinding "FOCUS_EDITOR FOCUS_MIXER FOCUS_SEQUENCER")
              "-------------"
              "Help keybindings" show-keybinding-help-window))

(define (generate-main-menus)
  (<ra> :wait-until-nsm-has-inited)
  (define menus-options '())
  (for-each (lambda (menu)
              (define menu-items (get-popup-menu-items-from-menu-items (menu :sub-menu)))
              (define menu-options (parse-popup-menu-options menu-items))
              (apply ra:add-menu-menu2
                     (cons (menu :text)
                           (get-popup-menu-args-from-options menu-options)))
              ;; Cache the options for the search popup, without the Recent menu.
              (set! menus-options
                    (append menus-options
                            (list (cons (menu :text)
                                        (remove-recent-from-options menu-options)))))
              (<ra> :go-previous-menu-level))
            (get-main-menu-items))
  (set! *main-menu-search-options* menus-options))

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

(<ra> :get-keybindings-from-command "ra.toggleCurrWindowFullScreen")
(<ra> :get-keybindings-from-command "ra.toggleFullScreen")
!!#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Help menu: "List of included Pd externals in Pd2"
;;;
;;; Scans <program-dir>/pd/externals when the menu entry is clicked, and shows
;;; the result in the message window. The rules mirror the Pd2 loader
;;; (ensure_externals_host() in audio/Pd_plugin2.cpp): class binaries
;;; (.pd_linux/.pd_darwin/.pd_freebsd) and abstractions (.pd, except
;;; "*-help.pd" and "*-meta.pd"). Alias symlinks (cyclone's Append.pd_linux,
;;; etc.) are skipped, as the loader does. The monolithic <lib>/<lib>.pd_linux
;;; binary is reported in its group's header instead of as a class.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pd-external-classname filename)
  ;; Mirrors external_class_name() in audio/Pd_plugin2.cpp.
  (cond ((string-ends-with? filename ".pd_linux")   (string-drop-right filename (string-length ".pd_linux")))
        ((string-ends-with? filename ".pd_darwin")  (string-drop-right filename (string-length ".pd_darwin")))
        ((string-ends-with? filename ".pd_freebsd") (string-drop-right filename (string-length ".pd_freebsd")))
        ((string-ends-with? filename ".pd")         (string-drop-right filename (string-length ".pd")))
        (else #f)))

(***assert*** (pd-external-classname "accum.pd_linux") "accum")
(***assert*** (pd-external-classname "acosh~.pd_darwin") "acosh~")
(***assert*** (pd-external-classname "0x3c0x7e.pd") "0x3c0x7e")
(***assert*** (pd-external-classname "zexy.pd_freebsd") "zexy")
(***assert*** (pd-external-classname "libiemnet.pd_linux.so") #f)
(***assert*** (pd-external-classname "README.md") #f)

;; Returns (list class-binaries abstractions library-binary) for one library dir.
(define (get-pd-externals-in-libdir libname libpath)
  (define class-binaries '())
  (define abstractions '())
  (define library-binary #f)

  ;; Sync iteration: ~1400 directory entries in total, so a few milliseconds.
  (<ra> :iterate-directory libpath #f
        (lambda (is-finished file-info)
          ;; file_info is uninitialized when is-finished is #t.
          (when (not is-finished)
            (define filename (<ra> :get-path-string (file-info :filename)))
            (define classname (and (not (file-info :is-dir))
                                   (not (file-info :is-sym-link))
                                   (pd-external-classname filename)))
            (when (and classname
                       (not (string-ends-with? filename "-help.pd"))
                       (not (string-ends-with? filename "-meta.pd")))
              (cond ((string=? classname libname) ;; the monolithic <lib>/<lib>.pd_linux
                     (set! library-binary filename))
                    ((string-ends-with? filename ".pd")
                     (set! abstractions (cons classname abstractions)))
                    (else
                     (set! class-binaries (cons classname class-binaries))))))
          #t)) ;; Must return non-#f, otherwise the iteration stops.
  (list (sort class-binaries string<?)
        (sort abstractions string<?)
        library-binary))

;; Returns the whole listing as plain text. Can also be called from the scheme
;; listener to dump the list to a terminal.
(define (get-pd2-externals-text)
  (define externals-path (<ra> :append-file-paths (<ra> :get-program-path)
                               (<ra> :get-path "pd/externals")))
  (if (not (<ra> :dir-exists externals-path))
      (<-> "Could not find the Pd2 externals directory: \""
           (<ra> :get-path-string externals-path) "\".")
      (let ((text (<-> "Pd2 externals in \"" (<ra> :get-path-string externals-path) "\":\n"))
            (libs '()))
        ;; First pass: just collect the library directories. They must not be
        ;; iterated from inside the callback below: ra:iterate-directory aborts
        ;; if it is called again while it is still running.
        (<ra> :iterate-directory externals-path #f
              (lambda (is-finished file-info)
                (when (and (not is-finished)
                           (file-info :is-dir))
                  (set! libs (cons (cons (<ra> :get-path-string (file-info :filename))
                                         (file-info :path))
                                   libs)))
                #t))
        ;; Second pass: one directory per library.
        (for-each (lambda (lib)
                    (define libname (car lib))
                    (define data (get-pd-externals-in-libdir libname (cdr lib)))
                    (define class-binaries (car data))
                    (define abstractions (cadr data))
                    (define library-binary (caddr data))
                    (set! text (<-> text "\n" libname ": "
                                    (length class-binaries) " class binaries, "
                                    (length abstractions) " abstractions"
                                    (if library-binary
                                        (<-> " (library binary: " library-binary ")")
                                        "")
                                    "\n"))
                    (for-each (lambda (name)
                                (set! text (<-> text "  " name "\n")))
                              (append class-binaries abstractions)))
                  (sort libs (lambda (a b)
                               (string<? (car a) (car b)))))
        (<-> text "\n" (length libs) " libraries.\n"))))

(define (show-pd2-externals-list)
  ;; Called from the Help menu, "List of included Pd externals in Pd2".
  (ra:add-message (ra:get-html-from-text (get-pd2-externals-text))))
                     
