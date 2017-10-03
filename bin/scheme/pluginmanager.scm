(provide 'pluginmanager.scm)

(when (and (defined? '*pluginmanager-gui*)
           (<gui> :is-open *pluginmanager-gui*))
  (pmg-stop-search!)
  (pmg-stop-scanning!)
  (when (not *pmg-has-keyboard-focus*)
    (<ra> :obtain-keyboard-focus *pmg-search-text-field*) ;; hack. (all of this is just fallback code in case something goes wrong)
    (set! *pmg-has-keyboard-focus* #f))
  (<gui> :set-static-toplevel-widget *pluginmanager-gui* #f)
  (if *message-gui*
      (<gui> :set-parent *message-gui* -1)) ;; Change parent of message gui. Don't want to delete it. (Note that we are in DEV mode here. This code should never be run by user.) TODO: We could perhaps, somehow, do this automatically in the setStaticToplevelWidget function. We would probably have less semi-weird crashes during development then.
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

(define *pmg-search-coroutine* (make-coroutine))
(define *pmg-scanner-coroutine* (make-coroutine))
  

#!
(begin *pmg-instrconf*)
(<gui> :is-open *pluginmanager-gui*)
(<gui> :is-visible *pluginmanager-gui*)
(<gui> :hide *pluginmanager-gui*)
!#

(define (pmg-hide)
  (when (pmg-open?)
    (when *pmg-has-keyboard-focus*
      (<ra> :release-keyboard-focus)
      (set! *pmg-has-keyboard-focus* #f))
    (pmg-stop-search!)
    (pmg-stop-scanning!)
    (<gui> :hide *pluginmanager-gui*)
    (<gui> :set-parent *pluginmanager-gui* -1) ;; Set parent to the main window. If not, the plugin manager window is deleted when the parent is deleted. (the plugin manager window is modal)
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
  (when (not *pmg-has-keyboard-focus*)
    (<ra> :obtain-keyboard-focus *pmg-search-text-field*)
    (set! *pmg-has-keyboard-focus* #t)))



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
       (lambda (radium-runs-custom-exec)
         (if (not radium-runs-custom-exec)
             (try-finally :try pmg-hide)) ;; We don't want to risk not returning #f. If that happens, the plugin manager can't be opened again.
         #f))

;; init table stuff
(let ((table *pmg-table*))
  (<gui> :stretch-table table *pmg-use-x* #f (floor (* 1.5 (<gui> :text-width "Usage"))))  ;; TODO: Does ':text-width' consistently return a too small value?
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
  (if #t ;; we scan in separate process now
      (yes-callback)     
      (show-async-message *pluginmanager-gui* "Make sure you haved saved all your work.\n\nThe program can crash now.\n\nPlugins that crash will be blacklisted. Already blacklisted plugins will not be scanned.\n\nAre you ready?" (list "Yes" "No") #t
                          (lambda (res)
                            (if (string=? "Yes" res)
                                (yes-callback))))))


(define (pmg-scan-all-remaining)

  (when (pmg-finished-scanning?)
    
    (define (all-plugins-are-scanned)
      (<gui> :set-value *pmg-progress-label* "")
      (<ra> :add-message "Finished.")
      (<gui> :set-enabled *pmg-scan-all-remaining-button* #f)
      (define org-search-string *pmg-curr-search-string*)
      (pmg-search "" ;; Make sure all entries are updated. Even though entries are filled in during update, entries that share the same container are not updated.
                  #f
                  (lambda ()
                    (pmg-search org-search-string #t)))) ;; set back to original search, if necessary

    (run-coroutine *pmg-scanner-coroutine*
                   '()
                   (lambda ()
                     (if (null? *pmg-populate-funcs*)
                         (begin
                           (all-plugins-are-scanned)
                           #f)
                         (begin
                           ((car *pmg-populate-funcs*))
                           (list 50)))))))
        

(define *pmg-scan-all-remaining-button* (<gui> :child *pluginmanager-gui* "scan_all_remaining_button"))
(if #f
    (<gui> :hide *pmg-scan-all-remaining-button*)    
    (<gui> :add-callback *pmg-scan-all-remaining-button* (lambda ()
                                                 (when (pmg-finished-scanning?)
                                                   (pmg-ask-are-you-sure pmg-scan-all-remaining)))))



(define *pmg-rescan-all-button* (<gui> :child *pluginmanager-gui* "rescan_all_button"))
(<gui> :add-callback *pmg-rescan-all-button*
       (lambda ()
         (if (pmg-finished-scanning?)
             (pmg-ask-are-you-sure
              (lambda ()
                (let ((message "Please wait. Clearing saved plugin info and rescanning directories for plugins..."))
                  (<gui> :set-value *pmg-progress-label* message)
                  (<ra> :add-message message))
                (<ra> :schedule 50 ;; Give the "Please wait" message a little bit of time to display.
                      (lambda ()
                        (<ra> :clear-sound-plugin-registry)
                        (pmg-search "" #f pmg-scan-all-remaining)
                        #f)))))))

(<gui> :add-mouse-callback *pmg-table*
       (lambda (button state x y)
         (if (and (= button *right-button*)
                  (= state *is-pressing*))
             (popup-menu "Show Info" (lambda ()
                                       (let* ((row (<gui> :get-value *pmg-table*))
                                              (entry (pmg-find-entry-from-row row))
                                              (instrconf *pmg-instrconf*))
                                         (when entry
                                           (spr-entry->instrument-description entry
                                                                              instrconf
                                                                              (lambda (descr)
                                                                                (<ra> :show-instrument-info descr *pmg-table*))))))))
         #f))


(define *pmg-search-text-field* (<gui> :child *pluginmanager-gui* "search_text"))
(<gui> :set-value *pmg-search-text-field* "")


(<gui> :add-realtime-callback *pmg-search-text-field*
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
      (let* ((row (<gui> :get-value *pmg-table*))
             (entry (pmg-find-entry-from-row row))
             (instrconf *pmg-instrconf*)
             (callback *pmg-callback*))
        (cond (entry
               (c-display (pp (<gui> :get-value *pmg-table*)))
               (c-display (pp entry))
               (spr-entry->instrument-description entry
                                                  instrconf
                                                  callback)
               (pmg-hide))
              ((> (length row) 0)
               (<ra> :add-message (<-> "Error. Unable to find instrument description for row " (pp (<gui> :get-value *pmg-table*)))))))))

  (define last-time-we-pressed-return-in-search-field 0)
  
  (<gui> :add-callback *pmg-search-text-field*
         (lambda (val)           
           (set! last-time-we-pressed-return-in-search-field (time))))
  
  (<gui> :add-key-callback *pluginmanager-gui*
         (lambda (presstype key)
           ;;(c-display "GOT KEY" presstype key (string=? key "\n"))
           (cond ((< (- (time) last-time-we-pressed-return-in-search-field) ;; Another hack: TODO sniff native keyboard events from Qt_Main.cpp instead.
                     0.2)
                  #f)
                 ;;((= 1 presstype) ;; Qt eats a lot of key down events, so we can't ignore key up. TODO: Let :add-key-callback sniff native keyboard events from Qt_Main.cpp instead.
                 ;; #f)
                 ((string=? key "HOME")
                  (<gui> :set-value *pmg-table* 0)
                  #t)
                 ((string=? key "END")
                  (<gui> :set-value *pmg-table* (1- (<gui> :get-num-table-rows *pmg-table*)))
                  #t)
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

#||
(let ((i 100))
  (define (func)
    (if (> i 0)
        (begin
          (c-display "hasit:" (<gui> :has-keyboard-focus *pmg-search-text-field*))
          (set! i (1- i))
          1000)
        #f))
  (<ra> :schedule 0 func))
||#

(define *pmg-curr-entries* '())

(define *pmg-populate-funcs* '())
(define *pmg-populate-buttons* '())

(define *pmg-curr-search-string* "------------")

(define (pmg-find-entry-from-row row)
  (if (= 0 (length row))
      #f
      (let ((path (row *pmg-path-x*)))
        (find-first *pmg-curr-entries*
                    (lambda (entry)
                      (string=? (entry :path) path))))))
  
(define (pmg-initialize-table! table new-num-entries)
  ;;(c-display "                pmg-clear-table! SEARCH-CLEAR")
  (<gui> :enable-table-sorting table #f)  

  (let* ((old-num-entries (<gui> :get-num-table-rows table))
         (num-new-entries (- new-num-entries old-num-entries)))
    (cond ((> num-new-entries 0)
           (<gui> :add-table-rows table old-num-entries num-new-entries))
          ((< num-new-entries 0)
           (<gui> :add-table-rows table (+ old-num-entries num-new-entries) num-new-entries))))

  (set! *pmg-populate-funcs* '())
  (set! *pmg-populate-buttons* '())
  (set! *pmg-curr-entries* '())

  (<gui> :set-enabled *pmg-scan-all-remaining-button* #f))


(define (pmg-add-entry-to-table! table entry instrconf y)
  (define is-normal (string=? (entry :type) "NORMAL"))
  (define is-container (string=? (entry :type) "CONTAINER"))
  ;;(define is-favourite (string=? (entry :type) "NUM_USED_PLUGIN"))
  ;;(c-display "entry:" entry)

  (define enabled (or (not is-normal)
                      (can-spr-entry-be-used? entry instrconf)))
  
  (push! *pmg-curr-entries* entry)

  (disable-gui-updates-block ;; The "scan" buttons sometimes pop up temporarily in a position where they are not supposed to be, unless we turn off updates.
   table
   (lambda ()

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
            (<gui> :add-table-int-cell table (entry :num-outputs) *pmg-outputs-x* y enabled)
            )
           
           (is-container
            
            (define is-blacklisted (entry :is-blacklisted))
            
            (define pop1 #f)
            (define pop2 #f)
            
            (define (populate)
              (<gui> :set-value *pmg-progress-label* (<-> "Scanning (" (length *pmg-populate-funcs*) "): " (entry :name)))
              (<gui> :update *pmg-progress-label*)
              (<gui> :enable-table-sorting table #f) ;; Must disable sorting when modifying table (QTableWidget sorts immediately when changing the content of a cell).
              (let* ((new-entries (<ra> :populate-plugin-container entry))
                     (y (<gui> :get-table-row-num table pop1)))
                (<gui> :add-table-rows table y (1- (length new-entries)))
                ;;(c-display (pp new-entries))
                (pmg-add-entries-to-table! table (to-list new-entries) instrconf y)
                (<gui> :set-value *pmg-progress-label* (<-> "Finished Scanning (" (length *pmg-populate-funcs*) "): " (entry :name)))
                )
              (<gui> :enable-table-sorting table #t)
              (if (not is-blacklisted)
                  (set! *pmg-populate-funcs* (delete-from2 *pmg-populate-funcs* populate)))
              (set! *pmg-populate-buttons* (delete-from2 *pmg-populate-buttons* pop1))
              (set! *pmg-populate-buttons* (delete-from2 *pmg-populate-buttons* pop2))
              ;;(<gui> :close pop1) ;; Not needed, and not even possible because Qt crashes as a (not so nice) nice way to tell the user that this is unnecessary.
              ;;(<gui> :close pop2) ;; Not needed, and not even possible because Qt crashes as a (not so nice) nice way to tell the user that this is unnecessary.
              )
            
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
           
           (else
            (<ra> :addMessage (<-> "Don't know how to handle " entry)))))))

(define (pmg-add-entries-to-table! table entries instrconf y)
  (let loop ((entries entries)
             (y y))
    (when (not (null? entries))
      (pmg-add-entry-to-table! table (car entries) instrconf y)
      (loop (cdr entries)
            (1+ y)))))


(define (pmg-schedule-adding-entries-to-table! table entries instrconf finished-callback)
  
  (define total-num-entries (length entries))

  (define (update-progress entries)
    ;;(c-display "PROGRESS:" (<-> (- total-num-entries (length entries)) "/" total-num-entries))
    (<gui> :set-value *pmg-progress-label* (<-> (- total-num-entries (length entries)) "/" total-num-entries)))
  
  (define (finalize-search)
    (<gui> :set-value *pmg-progress-label* (<-> "Num plugins: " (<gui> :get-num-table-rows *pmg-table*)))
    (<gui> :enable-table-sorting table #t)
    (<gui> :set-enabled *pmg-scan-all-remaining-button* (not (null? *pmg-populate-funcs*)))
    (for-each (lambda (populate-button)
                (<gui> :set-enabled populate-button #t))
              *pmg-populate-buttons*)
    (finished-callback)
    #f)

  (pmg-stop-search!)

  (update-progress entries)

  (pmg-initialize-table! table (length entries))

  (run-coroutine   
   *pmg-search-coroutine*
   (list entries 0 #f)
   
   (lambda (entries y start-time) ;; Start-time needs to be set lazily. Probably not necessary anymore tough since the scheduler resolution has now been reduced from 90ms to 5ms.
     (if (null? entries)

         (begin
           (finalize-search)
           #f)
         
         (begin
           (pmg-add-entry-to-table! table (car entries) instrconf y)
           
           (assert (= (<gui> :get-num-table-rows *pmg-table*)
                      total-num-entries))
           
           ;;(c-display "dur: " (and start-time (- (time) start-time)))

           (if (and start-time
                    (> (- (time) start-time)
                       0.1)) ;; seconds
               (begin
                 (update-progress entries) ;; Not very CPU hungry. We could have updated all the time. But it looks better only updating every 0.1 seconds.
                 (list 10 ;; milliseconds
                       (cdr entries)
                       (1+ y)
                       #f))
               (begin
                 (list 0
                       (cdr entries)
                       (1+ y)
                       (or start-time (time))))))))))


(define (pmg-stop-search!)
  (stop-coroutine! *pmg-search-coroutine*))

(define (pmg-stop-scanning!)
  (stop-coroutine! *pmg-scanner-coroutine*))

(define (pmg-finished-scanning?)
  (not (coroutine-alive? *pmg-scanner-coroutine*)))

#||
(begin *pmg-scanner-coroutine*)
||#

(define *pmg-cached-entries* #f)
(define *pmg-cached-entries-generation* -1)
(define (pmg-get-entries)
  (let ((curr-generation (<ra> :get-sound-plugin-registry-generation)))
    (when (not (= curr-generation *pmg-cached-entries-generation*))
      (set! *pmg-cached-entries* (to-list (<ra> :get-sound-plugin-registry #t)))
      (set! *pmg-cached-entries-generation* curr-generation))
    *pmg-cached-entries*))
          

(delafina (pmg-search :search-string
                      :check-same-search
                      :search-finished-callback #f)

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

  (define (finished-callback)
    (if search-finished-callback
        (<ra> :schedule 0 ;; To avoid starting the search coroutine from the search coroutine.
              (lambda ()
                (search-finished-callback)
                #f))))
  
  (set! search-string (string-upcase search-string))

  (if (or (not check-same-search)
          (not (string=? *pmg-curr-search-string* search-string)))
      (let* ((raw-entries (pmg-get-entries))
             (filtered-entries (filter-entries raw-entries search-string)))
        (set! *pmg-curr-search-string* search-string)
        (pmg-schedule-adding-entries-to-table! *pmg-table* filtered-entries *pmg-instrconf* finished-callback)
        )
      (finished-callback)))


;; Start search and show gui
(define (pmg-start instrconf callback)
  (pmg-show instrconf callback)
  (pmg-search (<gui> :get-value *pmg-search-text-field*) #f))

  
#||
(define hash-test (make-hash-table))
(hash-test :test)

(pmg-open?)
(pmg-visible?)

(begin
  (<gui> :hide *pluginmanager-gui*)
  (<gui> :show *pluginmanager-gui*))
||#
