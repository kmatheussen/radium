(provide 'sequencer_right_part.scm)

(my-require 'gui.scm)
(my-require 'area.scm)


;; Note: Playlist/blocklist is also here.


(define *block/audio/list-recreate-callbacks* '())

(define (FROM_C-recreate-block/audio-list-guis)
  ;;(c-display "         CALLING FROM_C-recreate-block/audio-list-guis")
  (let ((callbacks *block/audio/list-recreate-callbacks*))
    (set! *block/audio/list-recreate-callbacks* '())
    (define new-callbacks
      (let loop ((callbacks callbacks))
        (if (null? callbacks)
            '()
            (let ((callback (car callbacks)))
              (if (callback)
                  (cons callback
                        (loop (cdr callbacks)))
                  (loop (cdr callbacks)))))))
    (set! *block/audio/list-recreate-callbacks*
          (append *block/audio/list-recreate-callbacks*
                  new-callbacks))))

#!!
(FROM_C-recreate-block/audio-list-guis)
(begin *curr-block/audio-list-type*)
(FROM_C-recreate-playlist-area)
(recreate-block/audio-list)
!!#

(define *curr-audiofile-num* 0)

(define (create-audio-files-browser-area gui x1 y1 x2 y2 draggable state)
  (c-display "CRETING")
  
  (define curr-audiofile #f)
  
  (define (recreate x1 y1 x2 y2 state)
    (define entry-height (round (if draggable
                                    (* 1.2 (get-fontheight))
                                    (* 0.8 (get-fontheight)))))
    (define audiofiles (<ra> :get-audio-files))

    (define (create-entry i x1 x2)
      (define audiofile (audiofiles i))
      (define color (<ra> :get-audiofile-color audiofile))
      (define col1 color)
      ;;(set! color (<gui> :make-color-lighter color 1.5))
      (set! color (<gui> :set-alpha-for-color color 0.5))
      ;;(c-display "With alpha:" col1 ". without:" color)
      (define file-info (<ra> :get-file-info audiofile))
      (define (mouse-callback button x y . rest)
        (set! curr-audiofile audiofile)
        (set! *curr-audiofile-num* i)
        ;;(c-display "HEPP:" (<ra> :from-base64 audiofile) i *curr-audiofile-num*)
        (area :update-me!)
        (cond ((= button *right-button*)
               (show-audiolist-popup-menu audiofile)
               #t)
              ((and (= button *left-button*)
                    (<gui> :is-double-clicking gui))
               (undo-block
                (lambda ()
                  (let ((pos (<ra> :get-playlist-pos-time)))
                    (insert-pause-in-seqtrack! -1 pos (<ra> :get-sample-length audiofile))
                    (<ra> :create-sample-seqblock -1 audiofile pos))))
               #t)
              (else
               #f)))
      (define (is-current?)
        ;;(c-display "curr:" curr-audiofile i *curr-audiofile-num*)
        ;;(and curr-audiofile
        ;;     (string=? audiofile curr-audiofile))
        (and *curr-audiofile-num*
             (= i *curr-audiofile-num*)))
      
      (if draggable
          (<new> :seqblock-table-entry-area gui x1 0 x2 entry-height
                 :is-current is-current?
                 :entry-num i
                 :file-info file-info
                 :allow-dragging #t
                 :background-color color
                 :callback mouse-callback
                 )
          (let ((text-area (<new> :text-area gui x1 0 x2 entry-height
                                  (<ra> :append-base64-strings
                                        (<ra> :to-base64 (<-> (if (< i 10) " " "") i ": "))
                                        (file-info :filename))
                                  :background-color (lambda ()
                                                      (let ((base (<gui> :set-alpha-for-color color 0.05)))
                                                        (if (is-current?)
                                                            (<gui> :mix-colors "color11" base 0.95)
                                                            base)))
                                  :text-color (lambda ()
                                                (if (is-current?)
                                                    *text-color*
                                                    "black"))
                                  :align-left #t
                                  :paint-border #f
                                  :cut-text-to-fit #t
                                  :text-is-base64 #t
                                  )))
            (text-area :add-mouse-cycle! mouse-callback)
            text-area)))
    
    (define area
      (<new> :vertical-list-area2 gui x1 y1 x2 y2
             :num-sub-areas (length audiofiles)
             :get-sub-area-height entry-height
             :create-sub-area create-entry
             :sub-areas-can-be-cached #t
             ))
    (if state
        (area :apply-state! state))
    area)

  
  (define area (<new> :use-first-subarea-state-as-state-area gui x1 y1 x2 y2))
  (area :add-sub-area-plain! (recreate x1 y1 x2 y2 state))

  (area :add-mouse-cycle! (lambda (button x* y*)
                            (when curr-audiofile
                              (set! curr-audiofile #f)
                              (area :update-me!))
                            (and (= button *right-button*)
                                 (begin
                                   (show-audiolist-popup-menu #f) ;;curr-audiofile)
                                   #t))))


  (push-back! *block/audio/list-recreate-callbacks*
              (lambda ()
                (if (or (not (<gui> :is-open gui))
                        (not (area :is-alive)))
                    #f
                    (begin
                      (define state (area :get-state))
                      (area :remove-sub-areas!)
                      (area :get-position
                            (lambda (x1 y1 x2 y2 width height)
                              (area :add-sub-area-plain! (recreate x1 y1 x2 y2 state))))
                      #t))))
  area
  )

#!!
(let ()
  (define testarea (make-qtarea :width 450 :height 750
                                :sub-area-creation-callback (lambda (gui width height state)
                                                              (create-audio-files-browser-area gui 0 0 width height #t state))))
  (<gui> :show (testarea :get-gui)))
!!#




(define (doubleclick-or-shift-rightclick? gui button)
  (or (and (= button *left-button*)
           (<gui> :is-double-clicking gui))
      (and (= button *right-button*)
           (<ra> :shift-pressed))))

(define (create-blocks-browser-area gui x1 y1 x2 y2 draggable state)

  (define (recreate x1 y1 x2 y2 state)
    (define area
      (<new> :vertical-list-area gui x1 y1 x2 y2
             (lambda (x1 x2)
               (map (lambda (blocknum)
                      (define color (<ra> :get-block-color blocknum))
                  ;;;(set! color (<gui> :make-color-lighter color 1.5))
                      (set! color (<gui> :set-alpha-for-color color 0.5))
                      (define is-current (= (<ra> :current-block) blocknum))
                      (define (mouse-callback button x y . rest)
                        (cond ((and (= button *right-button*)
                                    (<ra> :shift-pressed))
                               (<ra> :delete-block blocknum))
                              ((and (= button *left-button*)
                                    (<gui> :is-double-clicking gui))
                               (<ra> :create-seqblock -1 blocknum))
                              ((not (<ra> :is-playing-song))
                               (<ra> :select-block blocknum)))
                        (update)
                        #f)
                      
                      (if draggable
                          (<new> :seqblock-table-entry-area gui 10 0 100 (round (* 1.2 (get-fontheight)))
                                 :is-current is-current
                                 :entry-num blocknum
                                 :blocknum blocknum
                                 :allow-dragging #t
                                 :background-color color ;(if (= (<ra> :current-block) blocknum)
                                        ;(<gui> :mix-colors color "green" 0.1)
                                        ;color)
                                 :callback mouse-callback)
                          (let ()
                            (let ((text-area (<new> :text-area gui 10 0 100 (round (* 0.8 (get-fontheight)))
                                                    (<-> (if (< blocknum 10) " " "") blocknum ": " (<ra> :get-block-name blocknum))
                                                    :background-color (let ((base (<gui> :set-alpha-for-color color 0.05)))
                                                                        (if is-current
                                                                            (<gui> :mix-colors "color11" base 0.95)
                                                                            base))
                                                    :text-color (if is-current
                                                                    *text-color*
                                                                    "black")
                                                    :align-left #t
                                                    :paint-border #f
                                                    :cut-text-to-fit #t
                                                    )))
                              (text-area :add-mouse-cycle! mouse-callback)
                              text-area))))
                    (iota (<ra> :get-num-blocks))))))
    (if state
        (area :apply-state! state))
    area)
  
  (define area (<new> :use-first-subarea-state-as-state-area gui x1 y1 x2 y2))
  (area :add-sub-area-plain! (recreate x1 y1 x2 y2 state))

  (area :add-mouse-cycle! (lambda (button x* y*)
                            (and (not (<ra> :shift-pressed))
                                 (= button *right-button*)
                                 (begin
                                   (show-blocklist-popup-menu)
                                   #t))))
  
  ;;(c-display "state:" state)

  (define (update)
    (define state (area :get-state))
    (area :remove-sub-areas!)
    (area :get-position
          (lambda (x1 y1 x2 y2 width height)
            (area :add-sub-area-plain! (recreate x1 y1 x2 y2 state)))))
  
  (push-back! *block/audio/list-recreate-callbacks*
              (lambda ()
                (if (or (not (<gui> :is-open gui))
                        (not (area :is-alive)))
                    #f
                    (begin
                      (update)
                      #t))))
    
  area
  )

#!!
(let ()
  (define testarea (make-qtarea :width 450 :height 750
                                :sub-area-creation-callback (lambda (gui width height state)
                                                              (create-blocks-browser-area gui 0 0 width height #f state))))
  (<gui> :show (testarea :get-gui)))

!!#

(define-struct playlist-entry
  :type ;; 'block, 'audiofile, 'pause, or 'last
  :start-time #f
  :duration #f
  :blocknum #f
  :seqblock-name #f
  :num #f
  :seqblockid #f
  :audiofile #f)

(define (get-playlist-entry-text entry)
  (cond ((eq? (entry :type) 'pause)
         (<ra> :to-base64 (<-> "  P: " (<ra> :get-time-string-from-frames (entry :duration)))))
        ((or (eq? (entry :type) 'block)
             (eq? (entry :type) 'audiofile))
         (<ra> :append-base64-strings
               (<ra> :to-base64
                     (<-> (entry :num)
                          ": "
                          (if (entry :blocknum)
                              (<-> (entry :blocknum) "/")
                              "")))
               (entry :seqblock-name)))
        ((eq? (entry :type) 'last)
         "")
        (else
         (c-display "      ENTRY:" entry)
         (assert #f))))

(define (get-playlist-entries seqtracknum)
  (let loop ((last-time 0)
             (seqblocks (to-list (<ra> :get-gfx-seqblocks-state seqtracknum)))
             (num 0))
    (if (null? seqblocks)
        (list (make-playlist-entry 'last
                                   :start-time last-time))
        (let* ((seqblock (car seqblocks))
               (time (seqblock :start-time)))
          (define rest (cons (make-playlist-entry (if (seqblock :blocknum)
                                                      'block
                                                      'audiofile)
                                                  :start-time time
                                                  :duration (- (seqblock :end-time) time)
                                                  :blocknum (seqblock :blocknum)
                                                  :seqblock-name (<ra> :get-seqblock-name (seqblock :id))
                                                  :num num
                                                  :seqblockid (seqblock :id)
                                                  :audiofile (seqblock :sample-base64))
                             (loop (seqblock :end-time)
                                   (cdr seqblocks)
                                   (+ num 1))))
          (if (> time last-time)
              (cons (make-playlist-entry 'pause
                                         :start-time last-time
                                         :duration (- time last-time))
                    rest)
              rest)))))
#!!
(pp (get-playlist-entries -1))
(<ra> :get-curr-playlist-pos)
!!#

(define (get-legal-playlist-pos entries)
  (define maybe (<ra> :get-curr-playlist-pos))
  (if (< maybe 0)
      0
      (if (>= maybe (length entries))
          (- (length entries) 1)
          maybe)))

(define (get-curr-playlist-entry)
  (let ((entries (get-playlist-entries -1)))
    (entries (get-legal-playlist-pos entries))))

#!!
(<ra> :get-curr-playlist-pos)
!!#

(define (get-playlist-entry-area gui entry playlist-pos)
  
  (define (is-current?)
    (if #t
        (= (<ra> :get-curr-playlist-pos)
           playlist-pos)
        (if (<ra> :is-playing-song)
            (and (>= (<ra> :get-song-pos)
                     (entry :start-time))
                 (or (not (entry :duration))
                     (< (<ra> :get-song-pos)
                        (+ (entry :start-time) (entry :duration)))))
            (equal? (entry :seqblockid)
                    (<ra> :get-curr-seqblock-id))))
    )
  
  
  ;  (equal? (entry :seqblockid)
  ;          (<ra> :get-curr-seqblock-id)))

  (define area (<new> :text-area gui 10 0 100 (round (* 0.8 (get-fontheight)))
                      (get-playlist-entry-text entry)
                      :background-color (lambda ()
                                          (define color (cond ((eq? (entry :type) 'block)
                                                               (<ra> :get-block-color (entry :blocknum)))
                                                              ((eq? (entry :type) 'audiofile)
                                                               (<ra> :get-audiofile-color (entry :audiofile)))
                                                              (else
                                                               #f)))
                                          (if color
                                              (set! color (<gui> :set-alpha-for-color color 0.1)))
                                          (cond ;((eq? (entry :type) 'pause)
                                        ; "color9");;#f) ;;"#666666")
                                           ((is-current?)
                                            (if color
                                                (<gui> :mix-colors "color11" color 0.95)
                                                "color11"))
                                           (else
                                            color)))
                      :text-color (lambda ()
                                    (cond ((is-current?)
                                           *text-color*)
                                          ((eq? (entry :type) 'pause)
                                           "black") ;;*text-color*)
                                          (else
                                           "black")))
                      :align-left #t
                      :paint-border #f
                      :cut-text-to-fit #t
                      :text-is-base64 #t
                      ))

  (area :add-method! :is-current? is-current?)
                                    
  (area :add-mouse-cycle! (lambda (button x* y*)
                            ;;(if (entry :seqblockid)
                            ;;    (<ra> :set-curr-seqblock (entry :seqblockid)))
                            (c-display "Setting curr to" playlist-pos)
                            (<ra> :set-curr-playlist-pos playlist-pos)
                            ;;(<ra> :set-song-pos (entry :start-time))
                            ;;(let ((parent-area (area :get-parent-area)))
                            ;;  (if parent-area
                            ;;      (parent-area :update-me!)
                            ;;      (area :update-me!)))
                            (if (doubleclick-or-shift-rightclick? gui button)
                                (cond ((eq? (entry :type) 'pause)
                                           (delete-pause-in-seqtrack -1 (entry :start-time)))
                                          ((or (eq? (entry :type) 'block)
                                               (eq? (entry :type) 'audiofile))
                                           (<ra> :delete-seqblock (entry :seqblockid)))
                                          ((eq? (entry :type) 'last)
                                           #t)
                                          (else
                                           (assert #f)))
                                (if (= button *right-button*)                                    
                                    (FROM_C-show-playlist-popup-menu)))
                            #t))
  area
  )


(define (create-playlist-area gui x1 y1 x2 y2 state)

  (define entry-areas '())
  
  (define (recreate x1 y1 x2 y2 state)
    (define curr-entry-area #f)
    (set! entry-areas (let ((entries (get-playlist-entries -1)))
                        ;;(c-display "RECREATING. len:" (length entries))
                        (map (lambda (entry playlist-pos)
                               (define entry-area (get-playlist-entry-area gui entry playlist-pos))
                               (if (entry-area :is-current?)
                                   (set! curr-entry-area entry-area))
                               entry-area)                             
                             entries
                             (iota (length entries)))))
    (define area (<new> :vertical-list-area gui x1 y1 x2 y2
                        
                        (lambda (x1 x2)
                          entry-areas)))
    ;;(c-display "STASTE:" state)
    (if state
        (area :apply-state! state))

    (if curr-entry-area
        (ensure-entry-area-visible-in-playlist area curr-entry-area))
    
    area)
  
  (define (ensure-entry-area-visible-in-playlist playlist-area curr-entry-area)
    (playlist-area :ensure-area-is-visible curr-entry-area))

  (define area (<new> :use-first-subarea-state-as-state-area gui x1 y1 x2 y2))

  (area :add-sub-area-plain! (recreate x1 y1 x2 y2 state))

  (define (get-vertical-list-area)
    (car (area :get-sub-areas)))
  
  (area :add-mouse-cycle! (lambda (button x* y*)
                            (and (not (<ra> :shift-pressed))
                                 (= button *right-button*)
                                 (begin
                                   (FROM_C-show-playlist-popup-menu)
                                   #t))))

  (area :add-method! :recreate (lambda ()
                                 (define state (area :get-state))
                                 ;;(c-display "        EHP " state)
                                 (area :remove-sub-areas!)
                                 (area :get-position
                                       (lambda (x1 y1 x2 y2 width height)
                                         (area :add-sub-area-plain! (recreate x1 y1 x2 y2 state)))))
        )

  (area :add-method! :ensure-curr-entry-is-visible (lambda ()
                                                     (let loop ((entry-areas entry-areas))
                                                       (when (not (null? entry-areas))
                                                         (define entry-area (car entry-areas))
                                                         (if (entry-area :is-current?)
                                                             (ensure-entry-area-visible-in-playlist (get-vertical-list-area) entry-area)
                                                             (loop (cdr entry-areas)))))))
                                                     
  area
  )

(define *playlist-area* (if (defined? '*playlist-area*)
                            *playlist-area*
                            #f))
(define *playlist-area-gui* (if (defined? '*playlist-area-gui*)
                                *playlist-area-gui*
                                #f))

(define (FROM_C-update-playlist-area)
  ;;o(c-display "HEPP. update playlist")
  (when *playlist-area*
    (*playlist-area* :ensure-curr-entry-is-visible)
    (*playlist-area* :update-me!)))

;;(define (FROM_C-set-curr-playlist-pos pos)
;;  (when (not (= *curr-playlist-pos* pos))
;;    (set! *curr-playlist-pos* pos)
;;    (FROM_C-update-playlist-area)))

(define (FROM_C-recreate-playlist-area)
  (if *playlist-area*
      (*playlist-area* :recreate)))

#!!
(let ()
  (define testarea (make-qtarea :width 450 :height 750
                                :sub-area-creation-callback (lambda (gui width height state)
                                                              (set! *playlist-area* (create-playlist-area gui 0 0 width height state)))))
  (when (and *playlist-area-gui*
             (<gui> :is-open *playlist-area-gui*))
    (define gui (testarea :get-gui))
    (<gui> :set-size gui (<gui> :width *playlist-area-gui*) (<gui> :height *playlist-area-gui*))
    (<gui> :show gui)
    (<gui> :set-pos gui (<gui> :get-x *playlist-area-gui*) (<gui> :get-y *playlist-area-gui*))
    (<gui> :close *playlist-area-gui*))
  (set! *playlist-area-gui* (testarea :get-gui))
  (<gui> :set-background-color *playlist-area-gui* "color9")
  (<gui> :show *playlist-area-gui*))
!!#


(define *curr-block/audio-list-type* 'unknown)

(define (get-curr-block/audio-list-type)
  (if (<ra> :seqtrack-for-audiofiles -1)
      'audio
      'block))


(define (FROM_C-playlist-insert!)
  (define pos (<ra> :get-curr-playlist-pos))
  (c-display "POS:" pos)
  (define entry (get-curr-playlist-entry))
  (if (<ra> :seqtrack-for-audiofiles -1)
      (let* ((pos (entry :start-time))
             (filename (let ((v (<ra> :get-audio-files)))
                         (v *curr-audiofile-num*))) 
             (duration (<ra> :get-sample-length filename)))
        (undo-block
         (lambda ()
           (insert-pause-in-seqtrack! -1 pos duration)
           (<ra> :create-sample-seqblock
                 -1
                 filename
                 (entry :start-time)
                 )))
        )
      (<ra> :create-seqblock -1 -1 (entry :start-time)))
  (<ra> :set-curr-playlist-pos (+ pos 1) #f #t))


(define (FROM_C-playlist-remove!)
  (define entry (get-curr-playlist-entry))
  (when (not (eq? (entry :type) 'last))
    (undo-block
     (lambda ()
       (if (or (eq? (entry :type) 'block)
               (eq? (entry :type) 'audiofile))                                                            
           (<ra> :delete-seqblock (entry :seqblockid)))
       (delete-pause-in-seqtrack -1 (entry :start-time) (entry :duration)))))
  (c-display "Remove"))


(def-area-subclass (<block-and-playlist-area> :gui :x1 :y1 :x2 :y2 :state)

  (define button-height (round (min (/ height 4)
                                    (* 0.8 (get-fontheight)))))

  (define button-mid (average x1 x2))
  
  (define list-height (/ (- height
                            (* button-height 2))
                         2))
  
  (define blocklist-y1 y1)
  (define blocklist-y2 (+ blocklist-y1 list-height))
  
  (define insert/remove-y1 blocklist-y2)
  (define insert/remove-y2 (+ blocklist-y2 button-height))

  (define playlist-y1 insert/remove-y2)
  (define playlist-y2 (+ playlist-y1 list-height))

  (define up/down-y1 playlist-y2)
  (define up/down-y2 y2)

  (set! *curr-block/audio-list-type* (get-curr-block/audio-list-type))
  
  (define blocklist-for-audiofiles (<ra> :seqtrack-for-audiofiles -1))
  
  (define blocklist (if blocklist-for-audiofiles
                        (create-audio-files-browser-area gui x1 blocklist-y1 x2 blocklist-y2 #f (and state
                                                                                                     (state :blocklist-for-audiofiles)
                                                                                                     (state :blocklist)))
                        (create-blocks-browser-area gui x1 blocklist-y1 x2 blocklist-y2 #f (and state
                                                                                                (not (state :blocklist-for-audiofiles))
                                                                                                (state :blocklist)))))

  (define playlist (create-playlist-area gui x1 playlist-y1 x2 playlist-y2 #f))
  (set! *playlist-area* playlist)

  ;;(c-display blocklist-y1 blocklist-y2 playlist-y1 playlist-y2 "button-height:" button-height ". list-height:" list-height ". y1/y2:" y1 y2)
  
  (add-sub-area-plain! (<new> :button gui x1 insert/remove-y1 button-mid insert/remove-y2
                              :text "Insert"
                              :callback-release FROM_C-playlist-insert!))
                                                  
  
  (add-sub-area-plain! (<new> :button gui button-mid insert/remove-y1 x2 insert/remove-y2
                              :text "Remove"
                              :callback-release FROM_C-playlist-remove!))

  (define (swap-entries entry1 entry2)
    (cond ((eq? (entry1 :type) 'pause)
           (move-seqblock! (entry2 :seqblockid) (entry1 :start-time)))
          ((eq? (entry2 :type) 'pause)
           (move-seqblock! (entry1 :seqblockid) (- (+ (entry1 :start-time)
                                                     (entry1 :duration)
                                                     (entry2 :duration))
                                                  (entry1 :duration))))
          (else
           (swap-seqblock-with-next! (entry1 :seqblockid)))))

  (add-sub-area-plain! (<new> :button gui x1 up/down-y1 button-mid up/down-y2
                              :text "Up"
                              :callback-release (lambda ()
                                                  (define entries (get-playlist-entries -1))
                                                  (define curr-playlist-pos (get-legal-playlist-pos entries))
                                                  (c-display "OLD POS:" (<ra> :get-curr-playlist-pos) curr-playlist-pos)
                                                  (when (and (> curr-playlist-pos 0)
                                                             (< curr-playlist-pos (- (length entries) 1)))
                                                    (define entry1 (entries (- curr-playlist-pos 1)))
                                                    (define entry2 (entries curr-playlist-pos))
                                                    (define new-pos (- curr-playlist-pos 1))
                                                    (swap-entries entry1 entry2)
                                                    
                                                    (c-display "NEW POS:" new-pos ", old:" curr-playlist-pos)
                                                    ( ;;<ra> :schedule 0
                                                          (lambda ()
                                                            (FROM_C-recreate-playlist-area)
                                                            (<ra> :set-curr-playlist-pos new-pos)
                                                            #f))
                                                    ))))
  (add-sub-area-plain! (<new> :button gui button-mid up/down-y1 x2 up/down-y2
                              :text "Down"
                              :callback-release (lambda ()
                                                  (define entries (get-playlist-entries -1))
                                                  (define curr-playlist-pos (get-legal-playlist-pos entries))
                                                  (when (and (>= curr-playlist-pos 0)
                                                             (< curr-playlist-pos (- (length entries) 2)))
                                                    (define entry1 (entries curr-playlist-pos))
                                                    (define entry2 (entries (+ 1 curr-playlist-pos)))
                                                    (define new-pos (+ curr-playlist-pos 1))
                                                    (swap-entries entry1 entry2)
                                                    ( ;;<ra> :schedule 0
                                                     (lambda ()
                                                       (FROM_C-recreate-playlist-area)
                                                       (<ra> :set-curr-playlist-pos new-pos)
                                                       #f))))))

  
  (add-sub-area-plain! blocklist)
  (add-sub-area-plain! playlist)

  ;;(<gui> :set-background-color gui "color9") doesn't work on osx. (really strange)
  (define-override (paint) ;; workaround for osx.
    (<gui> :filled-box gui "color9" x1 y1 x2 y2 0 0 #t))

  (define-override (get-state)
    (hash-table :blocklist-class-name (blocklist :class-name)
                :blocklist (blocklist :get-state)
                :playlist (playlist :get-state)))
  
  (define-override (apply-state! state)
    (if (eq? (state :blocklist-for-audiofiles)
             blocklist-for-audiofiles)
        (blocklist :apply-state! (state :blocklist)))
    (playlist :apply-state! (state :playlist)))

  ;;(if state
  ;;    (apply-state! state))
  )



(define (FROM_C-create-bock-and-playlist-gui)
  (define (recreate gui width height state)
    (<new> :block-and-playlist-area gui 0 0 width height state))
  
  (define thatthing (make-qtarea :width 450 :height 750
                                 :sub-area-creation-callback recreate))
  

  (set! *block-and-playlist-area* thatthing)

  (define gui (thatthing :get-gui))

  (define width (<gui> :text-width "0: 0/Pretty long name"))
  (set-fixed-width gui (ceiling width))
  
  gui)


(define (recreate-block/audio-list)
  (if (not (eq? *curr-block/audio-list-type*
                (get-curr-block/audio-list-type)))
      (*block-and-playlist-area* :recreate))
  #t)

(push-back! *block/audio/list-recreate-callbacks* recreate-block/audio-list)


(if (defined? 'FROM_C-reconfigure-sequencer-right-part)
    (let ((gui (FROM_C-create-bock-and-playlist-gui)))
      (<gui> :show gui)))


#!!


(pp (<ra> :get-seqblocks-state -1))

(<ra> :get-seqblock-name 0 -1)
!!#



(define (create-sequencer-right-part-area gui x1 y1 x2 y2 state)
  ;;(c-display "    CREATE SEQUENCER RIGHT AREA. State:" (pp state))
  (define (recreate gui x1 y1 x2 y2)
    (define list-area (<new> :tabs gui x1 y1 x2 y2
                             :is-horizontal #f
                             :curr-tab-num 0
                             :tab-names '("Hide" "Blocks" "Sounds" "Files" "Tracks")
                             :state state
                             :get-tab-area-func
                             (lambda (tab-num x1 y1 x2 y2 state)
                               ;;(<gui> :disable-updates gui) <-- Caused flickering for all of the sequencer when resizing.
                               (<ra> :schedule 0 ;; Run in next event cycle to avoid set-sequencer-right-part-empty calling a reconfigure and so forth.
                                     (lambda ()
                                       ;;(<gui> :enable-updates gui)
                                       (<ra> :set-sequencer-right-part-empty (= tab-num 0))
                                       #f))
                               (cond ((= tab-num 0)
                                      ;;(c-display "EMPTY")
                                      (<new> :area gui x1 y1 x2 y2))
                                     ((= tab-num 1)
                                      (create-blocks-browser-area gui x1 y1 x2 y2 #t state))
                                     ((= tab-num 2)
                                      (create-audio-files-browser-area gui x1 y1 x2 y2 #t state))
                                     ((= tab-num 3)
                                      ;;(c-display "    CREATING FILEBROWSER. state:" state)
                                      (<new> :file-browser gui x1 y1 x2 y2
                                             :path (<ra> :get-home-path)
                                             :id-text "sequencer-right-part"
                                             :only-audio-files #t
                                             :state state
                                             ))
                                     ((= tab-num 4)
                                      (create-seqtracks-config-area gui x1 y1 x2 y2 state))))
                             ))

    '(list-area :add-raw-mouse-cycle!
               (lambda (button x* y)
                 (set-mouse-pointer ra:set-normal-mouse-pointer gui)
                 #f))
    
    list-area)
  (recreate gui x1 y1 x2 y2))

#!!
(let ()
  (define testarea (make-qtarea :width 450 :height 750
                                :sub-area-creation-callback (lambda (gui width height state)
                                                               (create-sequencer-right-part-area gui 0 0 width height state))))
  (<gui> :show (testarea :get-gui)))

(load "area.scm")

(stacktrace)
(signature ra:release-mode)
!!#

(define (get-sequencer-right-part-position kont)
  (define header-box (<ra> :get-box sequencer-right-part))
  (kont (header-box :x1) (header-box :y1)
        (header-box :x2) (header-box :y2)))

(define (get-sequencer-right-part-area)
  (when (not *sequencer-right-part-area*)
    (set! *sequencer-right-part-area* (if *use-testgui*
                                         *testarea*
                                         (get-sequencer-right-part-position
                                          (lambda (x1 y1 x2 y2)
                                            (<new> :area (<gui> :get-sequencer-gui)
                                                   x1 y1 x2 y2))))))
  *sequencer-right-part-area*)

(define (FROM_C-reconfigure-sequencer-right-part)
  ;;(c-display "   Scheme: Reconfiguring right part")

  (get-sequencer-right-part-area)

  (define gui (if *use-testgui*
                  *testgui*
                  (<gui> :get-sequencer-gui)))

  (get-sequencer-right-part-position
   (lambda (x1 y1 x2 y2)
     (define sub-areas (*sequencer-right-part-area* :get-sub-areas))
     (define state (and (not (null? sub-areas))
                        ((car sub-areas) :get-state)))
     ;;(c-display "das state:" state)
     (*sequencer-right-part-area* :reset! x1 y1 x2 y2)
     (*sequencer-right-part-area* :add-sub-area-plain! (create-sequencer-right-part-area gui x1 y1 x2 y2 state))
     ))

  ;;(if (and (defined? 'recreate-seqtracks-config-area)
  ;;         recreate-seqtracks-config-area)
  ;;    (recreate-seqtracks-config-area))
  )

