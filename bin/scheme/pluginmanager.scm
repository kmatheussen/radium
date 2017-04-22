
(let ((table (<gui> :table (list "Name" "Type" "Category" "Creator" "Path" "Inputs" "Outputs"))))
  (define name-x 0)
  (define type-x 1)
  (define category-x 2)
  (define creator-x 3)
  (define path-x 4)
  (define inputs-x 5)
  (define outputs-x 6)
  (define populate-funcs '())
  (define populate-buttons '())

  (define progress-label #f)
  
  (define (remove-all-rows)
    (<gui> :add-table-rows table 0 (- (<gui> :get-num-table-rows table))) ;; TODO: Check if this one also closes the cell GUIs.
    (set! populate-funcs '())
    (set! populate-buttons '()))

  (define curr-search-string "")

  (define (filter-entries entries search-string)
    (if (string=? "" search-string)
        entries
        (keep (lambda (entry)
                (any? (lambda (str)
                        (and (string? str)
                             (string-case-insensitive-contains? str search-string)))
                      (map cdr (map values entry))))
              entries)))

  (define curr-add-rows-callback #f)
  
  (define (add-rows entries y kont)
    (define total-entries (length entries))
    (<gui> :set-enabled scan-button #f)
    (<gui> :enable-table-sorting table #f)
    (define start-time (time))
    (let loop ((entries entries)
               (y y)
               (downcount 10))
      (<gui> :set-value progress-label (<-> (- total-entries (length entries)) "/" total-entries))
      (if (null? entries)
          (begin
            (<gui> :set-value progress-label "")
            (<gui> :enable-table-sorting table #t)
            (<gui> :set-enabled scan-button (not (null? populate-funcs)))
            (for-each (lambda (populate-button)
                        (<gui> :set-enabled populate-button #t))
                      populate-buttons)
            (if kont
                (kont)))
          (begin
            (define entry (car entries))
            (define name-gui (<gui> :add-table-cell table (entry :name) name-x y))
            (<gui> :add-table-cell table (entry :type-name) type-x y)
            (<gui> :add-table-cell table (entry :path) path-x y)
            (cond ((string=? (entry :type) "NORMAL")
                   
                   (<gui> :add-table-cell table (<-> (entry :category)) category-x y)
                   (<gui> :add-table-cell table (<-> (entry :creator)) creator-x y)
                   (<gui> :add-table-cell table (<-> (entry :num-inputs)) inputs-x y)
                   (<gui> :add-table-cell table (<-> (entry :num-outputs)) outputs-x y))
                  
                  ((string=? (entry :type) "CONTAINER")

                   (define pop1 #f)
                   (define pop2 #f)
                   
                   (define (populate)
                     ;;(<gui> :enable-table-sorting table #f)
                     (let ((new-entries (<ra> :populate-plugin-container entry))
                           (y (<gui> :get-table-row-num table name-gui)))
                       (assert (>= y 0))
                       (<gui> :add-table-rows table y (1- (length new-entries)))
                       (c-display (pp new-entries))
                       (add-rows (to-list new-entries) y #f))
                     ;;(<gui> :enable-table-sorting table #t)
                     (set! populate-funcs (delete-from2 populate-funcs populate))
                     (set! populate-buttons (delete-from2 populate-buttons pop1))
                     (set! populate-buttons (delete-from2 populate-buttons pop2)))

                   (set! pop1 (<gui> :button "Scan" populate))
                   (set! pop2 (<gui> :button "Scan" populate))

                   (push! populate-funcs populate)
                   (<gui> :set-enabled pop1 #f)
                   (<gui> :set-enabled pop2 #f)
                   (push! populate-buttons pop1)
                   (push! populate-buttons pop2)
                   (<gui> :add-table-cell table "" category-x y)
                   (<gui> :add-table-cell table "" creator-x y)
                   (<gui> :add-table-cell table pop1 inputs-x y)
                   (<gui> :add-table-cell table pop2 outputs-x y)
                   )
                  )
            (if (> (- (time) start-time)
                   0.1)
                (begin
                  (set! curr-add-rows-callback (lambda ()
                                                 (set! curr-add-rows-callback #f)
                                                 (set! start-time (time))
                                                 ;;(<gui> :enable-table-sorting table #f)
                                                 (loop (cdr entries)
                                                       (1+ y)
                                                       50)
                                                 #f))
                  ;;(<gui> :enable-table-sorting table #t)
                  (<ra> :schedule 20 curr-add-rows-callback))
                (loop (cdr entries)
                      (1+ y)
                      (1- downcount)))))))

  (define (stop-search)
    (if curr-add-rows-callback
        (<ra> :remove-schedule curr-add-rows-callback)))
  
  (define (search search-string)
    (set! search-string (string-upcase search-string))
    (when (and (string? search-string)
               (not (string=? curr-search-string search-string)))
      (set! curr-search-string search-string)
      (stop-search)
      (c-display " REMOVING...")
;      (<gui> :enable-table-sorting table #f)
      (remove-all-rows)
      (c-display "  ADDING...")
      (define entries (filter-entries (to-list (<ra> :get-sound-plugin-registry))
                                      search-string))
      (<gui> :add-table-rows table 0 (length entries))
      (c-display "  ADDING2..." (length entries))
      (add-rows entries 0 #f)))
                  ;(<gui> :enable-table-sorting table #t)
  ;;(set! curr-search-string search-string)))))


  ;;(<gui> :enable-table-sorting table #t)
  (<gui> :sort-table-by table path-x #t)

  ;;(<gui> :stretch-table table name-x #f 3)
  ;;(<gui> :stretch-table table type-x #f 2)
  ;;(<gui> :stretch-table table category-x #f 2)
  ;;(<gui> :stretch-table table creator-x #f 2)
  (<gui> :stretch-table table path-x #t 2)
  ;;(<gui> :stretch-table table inputs-x #f 1)
  ;;(<gui> :stretch-table table outputs-x #f 1)
  
  
  (define ui (<gui> :ui (<ra> :append-paths (<ra> :get-program-path) "pluginmanager.ui")))
  (define table-parent (<gui> :child ui "tableParent"))
  (<gui> :set-layout-spacing table-parent 2 0 2 0 2)
  (<gui> :set-layout-spacing ui 2 2 2 2 2)                                                                                        
  (<gui> :add table-parent table)

  (set! progress-label (<gui> :child ui "progress"))
  
  (define scan-button (<gui> :child ui "scan_all_button"))
  (<gui> :add-callback scan-button
         (lambda ()
           (define (again)
             (<ra> :schedule 50
                   (lambda ()
                     (if (null? populate-funcs)
                         (begin
                           #f)
                         (begin
                           ((car populate-funcs))
                           (<gui> :update table-parent)
                           (again)
                           50)))))
           (again)))

  (define search-text (<gui> :child ui "search_text"))
  (<gui> :add-callback search-text
         (lambda (val)           
           (search val)))
  
  (define search-button (<gui> :child ui "search_button"))
  (<gui> :add-callback search-button
         (lambda ()
           (search (<gui> :get-value search-text))))  

  (let ((entries (to-list (<ra> :get-sound-plugin-registry))))
    (<gui> :add-table-rows table 0 (length entries))
    (add-rows entries 0 #f))
  
  (<gui> :show ui))

(time)
  

