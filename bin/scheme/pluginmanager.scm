(provide 'pluginmanager.scm)

(when (and (defined? '*pluginmanager-gui*)
           (<gui> :is-open *pluginmanager-gui*))
  (when (not *pmg-has-keyboard-focus*)
    (<ra> :obtain-keyboard-focus *pmg-search-text-button*) ;; hack. (all of this is just fallback code in case something goes wrong)
    (set! *pmg-has-keyboard-focus* #f))
  (<gui> :set-static-toplevel-widget *pluginmanager-gui* #f)
  (<gui> :close *pluginmanager-gui*))

(define *pmg-has-keyboard-focus* #f)

(define *pluginmanager-gui* (<gui> :ui "pluginmanager.ui")) ;; Must use relative path. Haven't gotten absolute paths to work in windows when using char* instead of wchar_t*. And s7 uses char*.
;;(<gui> :ui (<ra> :append-paths (<ra> :get-program-path) "pluginmanager.ui"))
(<gui> :set-static-toplevel-widget *pluginmanager-gui* #t)

(<gui> :set-modal *pluginmanager-gui* #t)
(let ((width (floor (* 3 (<gui> :text-width "Usage  Name  Type    Category    Creator        Path              Inputs Outputs")))))
  (<gui> :set-size *pluginmanager-gui* width (floor (/ width 1.5))))

(define *pmg-table* (<gui> :table (list "Usage" "Name" "Type" "Category" "Creator" "Path" "Inputs" "Outputs")))

(define-constant *pmg-use-x* 0)
(define-constant *pmg-name-x* 1)
(define-constant *pmg-type-x* 2)
(define-constant *pmg-category-x* 3)
(define-constant *pmg-creator-x* 4)
(define-constant *pmg-path-x* 5)
(define-constant *pmg-inputs-x* 6)
(define-constant *pmg-outputs-x* 7)

(let ((table-parent (<gui> :child *pluginmanager-gui* "tableParent")))
  (<gui> :set-layout-spacing table-parent 2 0 2 0 2)
  (<gui> :set-layout-spacing *pluginmanager-gui* 2 2 2 2 2)
  (<gui> :add table-parent *pmg-table*))

(define *pmg-progress-label* (<gui> :child *pluginmanager-gui* "progress"))

(define *pmg-instrconf* #f)
(define *pmg-callback* #f)

#!
(begin *pmg-instrconf*)
(<gui> :is-open *pluginmanager-gui*)
(<gui> :is-visible *pluginmanager-gui*)
(<gui> :hide *pluginmanager-gui*)
!#

(define (pmg-hide)
  (when (pmg-open?)
    (<ra> :release-keyboard-focus)
    (set! *pmg-has-keyboard-focus* #f)
    (<gui> :hide *pluginmanager-gui*)
    (<gui> :set-parent *pluginmanager-gui* -3) ;; Set parent to NULL. If not, the plugin manager window is deleted when the parent is deleted.
    (set! *pmg-callback* #f)
    (set! *pmg-instrconf* #f)))

(define (pmg-show instrconf callback)
  (if (not (pmg-open?)) ;; Not supposed to happen, but if it for some reason should happen, it might make the plugin manager work again.
      (load "scheme/pluginmanager.scm"))
  (set! *pmg-instrconf* instrconf)
  (set! *pmg-callback* callback)
  (<gui> :set-parent *pluginmanager-gui* (instrconf :parentgui))
  (if callback
      (<gui> :set-enabled *pmg-add-button* #t)
      (<gui> :set-enabled *pmg-add-button* #f))
  (if callback
      (<gui> :set-text *pmg-cancel-button* "Cancel")
      (<gui> :set-text *pmg-cancel-button* "Close"))
  (<gui> :show *pluginmanager-gui*)
  (<ra> :obtain-keyboard-focus *pmg-search-text-button*)
  (set! *pmg-has-keyboard-focus* #t))



(define (pmg-open?)
  (<gui> :is-open *pluginmanager-gui*))

#||
(begin *pluginmanager-gui*)
(pmg-open?)
(pmg-visible?)
||#

(define (pmg-visible?)
  (<gui> :is-visible *pluginmanager-gui*))

;; Just hide window when closing it.
(<gui> :add-close-callback *pluginmanager-gui*
       (lambda ()
         (catch #t ;; We don't want to risk not returning #f (if that happens, the plugin manager can't be opened again)
                pmg-hide
                (lambda args
                  (c-display (ow!))))
         #f))

;; init table stuff
(let ((table *pmg-table*))
  (<gui> :stretch-table table *pmg-use-x* #f (floor (* 1.5 (<gui> :text-width "Usage"))))
  (<gui> :stretch-table table *pmg-name-x* #f (floor (* 1.5 (<gui> :text-width "Multi-band compressor"))))
  (<gui> :stretch-table table *pmg-type-x* #f (floor (* 1.5 (<gui> :text-width "Sample Player"))))
  (<gui> :stretch-table table *pmg-category-x* #f (floor (* 1.5 (<gui> :text-width "Category"))))
  (<gui> :stretch-table table *pmg-creator-x* #f (floor (* 1.5 (<gui> :text-width "Joern Nettingsmeier"))))
  (<gui> :stretch-table table *pmg-path-x* #t (floor (* 1.5 (<gui> :text-width "VST / plugin type / plugin name"))))
  (<gui> :stretch-table table *pmg-inputs-x* #f  (floor (* 1.5 (<gui> :text-width "Outputs")))) ;; same width for input and output
  (<gui> :stretch-table table *pmg-outputs-x* #f (floor (* 1.5 (<gui> :text-width "Outputs"))))

  (<gui> :sort-table-by table *pmg-path-x* #t)
  )


;; Set up button and search field callbacks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (pmg-ask-are-you-sure yes-callback)
  (<ra> :show-async-message *pluginmanager-gui* "Make sure you haved saved all your work.\n\nThe program can crash now.\n\nPlugins that crash will be blacklisted. Already blacklisted plugins will not be scanned.\n\nAre you ready?" (list "Yes" "No") #t
        (lambda (res)
          (if (string=? "Yes" res)
              (yes-callback)))))
  
(define (pmg-scan-all-remaining)
  (<ra> :schedule 0
        (lambda ()
          (if (null? *pmg-populate-funcs*)
              (begin
                (<gui> :set-value *pmg-progress-label* "")
                (<ra> :add-message "Finished"))))
                #f)
              (begin                                   
                ((car *pmg-populate-funcs*))
                ;;(<gui> :update table-parent)
                50)))))

(define *pmg-scan-all-button* (<gui> :child *pluginmanager-gui* "scan_all_button"))
(<gui> :add-callback *pmg-scan-all-button* (lambda ()
                                             (pmg-ask-are-you-sure pmg-scan-all-remaining)))


(define *pmg-rescan-all-button* (<gui> :child *pluginmanager-gui* "rescan_all_button"))
(<gui> :add-callback *pmg-rescan-all-button*
       (lambda ()
         (pmg-ask-are-you-sure
          (lambda ()
            (let ((message "Please wait. Clearing saved plugin info and rescanning directories for plugins..."))
              (<gui> :set-value *pmg-progress-label* message)
              (<ra> :add-message message))
            (<ra> :schedule 50
                  (lambda ()
                    (<ra> :clear-sound-plugin-registry)
                    (pmg-search "" #f)
                    (<ra> :schedule 50
                          (lambda ()
                            (if (pmg-finished-searching?)
                                (begin
                                  (if (not (null? *pmg-populate-funcs*))
                                      (pmg-scan-all-remaining)
                                      (<ra> :add-message "Finished"))
                                  #f)
                                50)))
                    #f))))))

(define *pmg-search-text-button* (<gui> :child *pluginmanager-gui* "search_text"))
(<gui> :set-value *pmg-search-text-button* "")

(<gui> :add-realtime-callback *pmg-search-text-button*
       (lambda (val)
         (if (pmg-visible?)
             (pmg-search val #t))))


(define *pmg-add-button* (<gui> :child *pluginmanager-gui* "add_button"))
(define *pmg-cancel-button* (<gui> :child *pluginmanager-gui* "cancel_button"))


(let ()
  
  (define search-button (<gui> :child *pluginmanager-gui* "search_button"))
  (<gui> :close search-button)
  ;;(<gui> :add-callback search-button
  ;;       (lambda ()
  ;;         (if (pmg-open?)
  ;;             (pmg-search (<gui> :get-value search-text) #t))))

  (define (made-selection)
    (when *pmg-callback*
      (assert *pmg-instrconf*)
      (let ((entry (pmg-find-entry-from-row (<gui> :get-value *pmg-table*)))
            (instrconf *pmg-instrconf*)
            (callback *pmg-callback*))
        (when entry
          (c-display (pp (<gui> :get-value *pmg-table*)))
          (c-display (pp entry))
          (spr-entry->instrument-description entry
                                             instrconf
                                             callback)
          (pmg-hide)))))

  (<gui> :add-key-callback *pluginmanager-gui*
         (lambda (presstype key)
           (c-display "GOT KEY" presstype key (string=? key "\n"))           
           (cond ((string=? key "HOME")
                  (c-display "HOME")
                  (<gui> :set-value *pmg-table* 0)
                  #t)
                 ((string=? key "END")
                  (c-display "END")
                  (<gui> :set-value *pmg-table* (1- (<gui> :get-num-table-rows *pmg-table*)))
                  #t)
                 ((= 1 presstype)
                  #f)
                 ((string=? key "\n")
                  (made-selection)
                  #t)
                 ((string=? key "ESC")
                  (pmg-hide)
                  #t)
                 (else
                  #f))))

  (<gui> :add-callback *pmg-add-button* made-selection)
  
  (<gui> :add-double-click-callback *pmg-table*
         (lambda (x y)
           (made-selection)))

  (<gui> :add-callback *pmg-cancel-button* pmg-hide)
  )


(define *pmg-curr-entries* '())

(define *pmg-populate-funcs* '())
(define *pmg-populate-buttons* '())

(define *pmg-curr-search-string* "------------")

(define *pmg-curr-fill-table-coroutine* #f)
  
(define (pmg-find-entry-from-row row)
  (if (= 0 (length row))
      #f
      (let ((path (row *pmg-path-x*)))
        (find-first *pmg-curr-entries*
                    (lambda (entry)
                      (string=? (entry :path) path))))))
  
(define (pmg-clear-table!)
  (<gui> :add-table-rows *pmg-table* 0 (- (<gui> :get-num-table-rows *pmg-table*))) ;; TODO: Check if this one also closes the cell GUIs.
  (set! *pmg-populate-funcs* '())
  (set! *pmg-populate-buttons* '())
  (set! *pmg-curr-entries* '()))
  

(define (pmg-add-to-table! table entries instrconf y finished-callback do-update-progress)
  (set! *pmg-curr-entries* (append *pmg-curr-entries* entries))
  
  (define total-num-entries (length entries))
  (<gui> :set-enabled *pmg-scan-all-button* #f)
  (<gui> :enable-table-sorting table #f)
  (define start-time (time))

  (define (update-progress entries)
    (if do-update-progress
        (<gui> :set-value *pmg-progress-label* (<-> (- total-num-entries (length entries)) "/" total-num-entries))))
    
  (update-progress entries)
  
  (let loop ((entries entries)
             (y y))    
    (if (null? entries)
        (begin
          (if do-update-progress
              (<gui> :set-value *pmg-progress-label* ""))
          (<gui> :enable-table-sorting table #t)
          (<gui> :set-enabled *pmg-scan-all-button* (not (null? *pmg-populate-funcs*)))
          (for-each (lambda (populate-button)
                      (<gui> :set-enabled populate-button #t))
                    *pmg-populate-buttons*)
          (if finished-callback
              (finished-callback)))
        (begin
          (define entry (car entries))
          (define is-normal (string=? (entry :type) "NORMAL"))
          (define is-container (string=? (entry :type) "CONTAINER"))
          ;;(define is-favourite (string=? (entry :type) "NUM_USED_PLUGIN"))
          ;;(c-display "entry:" entry)

          (define enabled (or (not is-normal)
                              (can-spr-entry-be-used? entry instrconf)))

          (when (or is-normal is-container)
            (let ((n (entry :num-uses)))
              (if (> n 0)
                  (<gui> :add-table-int-cell table n *pmg-use-x* y enabled)
                  (<gui> :add-table-string-cell table "" *pmg-use-x* y enabled)))
            (define name-gui (<gui> :add-table-string-cell table (entry :name) *pmg-name-x* y enabled))
            (<gui> :add-table-string-cell table (entry :type-name) *pmg-type-x* y enabled)
            (<gui> :add-table-string-cell table (entry :path) *pmg-path-x* y enabled)
            (cond (is-normal
                  
                   (<gui> :add-table-string-cell table (<-> (entry :category)) *pmg-category-x* y enabled)
                   (<gui> :add-table-string-cell table (<-> (entry :creator)) *pmg-creator-x* y enabled)
                   (<gui> :add-table-int-cell table (entry :num-inputs) *pmg-inputs-x* y enabled)
                   (<gui> :add-table-int-cell table (entry :num-outputs) *pmg-outputs-x* y) enabled)
                  
                  (is-container

                   (define is-blacklisted (entry :is-blacklisted))

                   (define pop1 #f)
                   (define pop2 #f)
                   
                   (define (populate)
                     (c-display "       POPULATE" entry)
                     (<gui> :set-value *pmg-progress-label* (<-> "Finished Scanning (" (length *pmg-populate-funcs*) "): " (entry :name)))
                     (<gui> :update *pmg-progress-label*)
                     ;;(<gui> :enable-table-sorting table #f)
                     (let ((new-entries (<ra> :populate-plugin-container entry))
                           (y (<gui> :get-table-row-num table pop1)))
                       (c-display "    Y" y pop1 "length:" (length new-entries))
                       (assert (>= y 0))
                       (<gui> :add-table-rows table y (1- (length new-entries)))
                       (c-display (pp new-entries))
                       (pmg-add-to-table! table (to-list new-entries) instrconf y #f #f))
                     ;;(<gui> :enable-table-sorting table #t)
                     (if (not is-blacklisted)
                         (set! *pmg-populate-funcs* (delete-from2 *pmg-populate-funcs* populate)))
                     (set! *pmg-populate-buttons* (delete-from2 *pmg-populate-buttons* pop1))
                     (set! *pmg-populate-buttons* (delete-from2 *pmg-populate-buttons* pop2))
                     (<gui> :close pop1)
                     (<gui> :close pop2))
                   
                   (set! pop1 (<gui> :button "Scan" populate))
                   (set! pop2 (<gui> :button "Scan" populate))

                   (if (not is-blacklisted)
                       (push! *pmg-populate-funcs* populate))
                   (<gui> :set-enabled pop1 #f)
                   (<gui> :set-enabled pop2 #f)
                   (push! *pmg-populate-buttons* pop1)
                   (push! *pmg-populate-buttons* pop2)
                   (<gui> :add-table-string-cell table (entry :category) *pmg-category-x* y)
                   (<gui> :add-table-string-cell table "" *pmg-creator-x* y)
                   (<gui> :add-table-gui-cell table pop1 *pmg-inputs-x* y)
                   (<gui> :add-table-gui-cell table pop2 *pmg-outputs-x* y)
                   )
                  ))

          (define time-now (time))
          
          (if (> (- time-now start-time)
                 0.05)
              (begin
                (update-progress entries)
                (set! *pmg-curr-fill-table-coroutine* (lambda ()                                                        
                                                        (set! *pmg-curr-fill-table-coroutine* #f)
                                                        (set! start-time (time))
                                                        ;;(<gui> :enable-table-sorting table #f)
                                                        (loop (cdr entries)
                                                              (1+ y))
                                                        #f))
                ;;(<gui> :enable-table-sorting table #t)
                (<ra> :schedule 10 *pmg-curr-fill-table-coroutine*))
              (loop (cdr entries)
                    (1+ y)))))))




(define (pmg-stop-search)
  (when *pmg-curr-fill-table-coroutine*
    (<ra> :remove-schedule *pmg-curr-fill-table-coroutine*)
    (set! *pmg-curr-fill-table-coroutine* #f)))



(define (pmg-finished-searching?)
  (not *pmg-curr-fill-table-coroutine*)) ;; yepp. If searching, either the PC can not be here (there's just one thread), or *pmg-curr-fill-table-coroutine* has a value.


(define *pmg-cached-entries* #f)
(define *pmg-cached-entries-generation* -1)
(define (pmg-get-entries)
  (let ((curr-generation (<ra> :get-sound-plugin-registry-generation)))
    (when (not (= curr-generation *pmg-cached-entries-generation*))
      (set! *pmg-cached-entries* (to-list (<ra> :get-sound-plugin-registry #t)))
      (set! *pmg-cached-entries-generation* curr-generation))
    *pmg-cached-entries*))
          

(define (pmg-search search-string check-same-search)

  (define table *pmg-table*)
  
  (define (filter-entries entries search-string)
    (if (string=? "" search-string)
        entries
        (keep (lambda (entry)
                (any? (lambda (str)
                        (and (string? str)
                             (string-case-insensitive-contains? str search-string)))
                      (map cdr (map values entry))))
              entries)))

  (set! search-string (string-upcase search-string))

  (<ra> :schedule 10 ;; Feels somewhat better for interactivity to schedule it.
        (lambda ()
          (when (or (not check-same-search)
                    (not (string=? *pmg-curr-search-string* search-string)))

            (define t (time))
            (define (get-time)
              (let* ((now (time))
                     (ret (- now t)))                
                (set! t (time))
                ret))
            
            ;;(c-display "    SEARCHING FOR" search-string (get-time))
            (set! *pmg-curr-search-string* search-string)
            (pmg-stop-search)
            ;;(c-display " REMOVING..." (get-time))
            ;;      (<gui> :enable-table-sorting table #f)
            (pmg-clear-table!)
            ;;(c-display "  REQUESTING" (get-time))
            (define raw-entries (pmg-get-entries))
            ;;(c-display "  FILTERING" (get-time))
            (define filtered-entries (filter-entries raw-entries search-string))
            ;;(c-display "  ADDING TABLE ROWS..." (get-time))
            (<gui> :add-table-rows table 0 (length filtered-entries))
            ;;(c-display "  ADDING ENTRIES..." (length entries) (get-time))
            (pmg-add-to-table! *pmg-table* filtered-entries *pmg-instrconf* 0 #f #t))
          #f)))


;; Start search and show gui
(define (pmg-start instrconf callback)
  (pmg-show instrconf callback)
  (pmg-search (<gui> :get-value *pmg-search-text-button*) #f))

  
#||
(pmg-open?)
(pmg-visible?)

(begin
  (<gui> :hide *pluginmanager-gui*)
  (<gui> :show *pluginmanager-gui*))
||#
