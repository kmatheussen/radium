(provide 'pluginmanager.scm)

(if (and (defined? '*pluginmanager-gui*)
         (<gui> :is-open *pluginmanager-gui*))
    (<gui> :close *pluginmanager-gui*))

(define *pluginmanager-gui* (<gui> :ui "pluginmanager.ui")) ;; Must use relative path. Haven't gotten absolute paths to work in windows when using char* instead of wchar_t*. And s7 uses char*.
;;(<gui> :ui (<ra> :append-paths (<ra> :get-program-path) "pluginmanager.ui"))

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

(define (pmg-hide)
  (<ra> :release-keyboard-focus)
  (<gui> :hide *pluginmanager-gui*)
  (set! *pmg-callback* #f)
  (set! *pmg-instrconf* #f))

(define (pmg-show instrconf callback)
  (set! *pmg-instrconf* instrconf)
  (set! *pmg-callback* callback)
  (<gui> :set-always-on-top *pluginmanager-gui* -2)
  (<gui> :show *pluginmanager-gui*)
  (<ra> :obtain-keyboard-focus *pmg-search-text-button*))



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
(define *pmg-scan-all-button* (<gui> :child *pluginmanager-gui* "scan_all_button"))
(<gui> :add-callback *pmg-scan-all-button*
       (lambda ()
         (<ra> :show-async-message "Are you sure? The program could crash if you have unstable plugins." (list "Yes" "No") #t
               (lambda (res)
                 (if (string=? "Yes" res)
                     (<ra> :schedule 0
                           (lambda ()
                             (if (null? *pmg-populate-funcs*)
                                 (begin
                                   #f)
                                 (begin
                                   ((car *pmg-populate-funcs*))
                                   ;;(<gui> :update table-parent)
                                   50)))))))))

(define *pmg-search-text-button* (<gui> :child *pluginmanager-gui* "search_text"))
(<gui> :set-value *pmg-search-text-button* "")

(<gui> :add-realtime-callback *pmg-search-text-button*
       (lambda (val)
         (if (pmg-visible?)
             (pmg-search val #t))))


(let ()
  
  (define search-button (<gui> :child *pluginmanager-gui* "search_button"))
  (<gui> :close search-button)
  ;;(<gui> :add-callback search-button
  ;;       (lambda ()
  ;;         (if (pmg-open?)
  ;;             (pmg-search (<gui> :get-value search-text) #t))))

  (define (made-selection)
    (assert *pmg-instrconf*)
    (assert *pmg-callback*)
    (let ((entry (pmg-find-entry-from-row (<gui> :get-value *pmg-table*)))
          (instrconf *pmg-instrconf*)
          (callback *pmg-callback*))
      (when entry
        (c-display (pp (<gui> :get-value *pmg-table*)))
        (c-display (pp entry))
        (spr-entry->instrument-description entry
                                           instrconf
                                           callback)
        (pmg-hide))))
    
  (define add-button (<gui> :child *pluginmanager-gui* "add_button"))
  (<gui> :add-callback add-button made-selection)

  (<gui> :add-double-click-callback *pmg-table*
         (lambda (x y)
           (made-selection)))

  (define cancel-button (<gui> :child *pluginmanager-gui* "cancel_button"))
  (<gui> :add-callback cancel-button pmg-hide)
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
  

(define (pmg-add-to-table! table entries instrconf y finished-callback)
  (set! *pmg-curr-entries* (append *pmg-curr-entries* entries))
  
  (define total-num-entries (length entries))
  (<gui> :set-enabled *pmg-scan-all-button* #f)
  (<gui> :enable-table-sorting table #f)
  (define start-time (time))

  (define (update-progress entries)
    (<gui> :set-value *pmg-progress-label* (<-> (- total-num-entries (length entries)) "/" total-num-entries)))

  (update-progress entries)
  
  (let loop ((entries entries)
             (y y))    
    (if (null? entries)
        (begin
          (<gui> :set-value *pmg-progress-label* "")
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
                   
                   (define pop1 #f)
                   (define pop2 #f)
                   
                   (define (populate)
                     ;;(<gui> :enable-table-sorting table #f)
                     (let ((new-entries (<ra> :populate-plugin-container entry))
                           (y (<gui> :get-table-row-num table pop1)))
                       (c-display "    Y" y pop1 "length:" (length new-entries))
                       (assert (>= y 0))
                       (<gui> :add-table-rows table y (1- (length new-entries)))
                       (c-display (pp new-entries))
                       (pmg-add-to-table! table (to-list new-entries) instrconf y #f))
                     ;;(<gui> :enable-table-sorting table #t)
                     (set! *pmg-populate-funcs* (delete-from2 *pmg-populate-funcs* populate))
                     (set! *pmg-populate-buttons* (delete-from2 *pmg-populate-buttons* pop1))
                     (set! *pmg-populate-buttons* (delete-from2 *pmg-populate-buttons* pop2))
                     (<gui> :close pop1)
                     (<gui> :close pop2))
                   
                   (set! pop1 (<gui> :button "Scan" populate))
                   (set! pop2 (<gui> :button "Scan" populate))
                   
                   (push! *pmg-populate-funcs* populate)
                   (<gui> :set-enabled pop1 #f)
                   (<gui> :set-enabled pop2 #f)
                   (push! *pmg-populate-buttons* pop1)
                   (push! *pmg-populate-buttons* pop2)
                   (<gui> :add-table-string-cell table "" *pmg-category-x* y)
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

  (when (or (not check-same-search)
            (not (string=? *pmg-curr-search-string* search-string)))

    (c-display "    SEARCHING FOR" search-string)

    (set! *pmg-curr-search-string* search-string)
    (pmg-stop-search)
    ;;(c-display " REMOVING...")
    ;;      (<gui> :enable-table-sorting table #f)
    (pmg-clear-table!)
    ;;(c-display "  ADDING...")
    (define entries (filter-entries (to-list (<ra> :get-sound-plugin-registry #t))
                                    search-string))
    (<gui> :add-table-rows table 0 (length entries))
    ;;(c-display "  ADDING2..." (length entries))
    (pmg-add-to-table! *pmg-table* entries *pmg-instrconf* 0 #f)))



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
