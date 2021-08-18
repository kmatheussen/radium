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

(define (create-audio-files-browser-area gui x1 y1 x2 y2 is-draggable state)
  (c-display "CRETING")
  
  (define curr-audiofile #f)
  
  (define (recreate x1 y1 x2 y2 state)
    (define entry-height (round (if is-draggable
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
               (if is-draggable
                   (show-audiolist-popup-menu audiofile)
                   (FROM_C-show-blocklist-popup-menu))
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

      (define entry
        (if is-draggable
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
                                          (<ra> :get-base64-from-filepath (file-info :filename)))
                                    :background-color (lambda ()
                                                        (let ((base (<gui> :set-alpha-for-color color 0.5)))
                                                          (if (and #f (is-current?))
                                                              (<gui> :mix-colors "high_background" base 0.95)
                                                              base)))
                                    :text-color (lambda ()
                                                  (if (is-current?)
                                                      *text-color*
                                                      "black"))
                                    :align-left #t
                                    :paint-border #t
                                    :border-rounding 0
                                    :border-width (lambda ()
                                                    (if (is-current?)
                                                        2.9
                                                        0.5))
                                    :border-color (lambda ()
                                                    (if (is-current?)
                                                        "sequencer_text_current_block_color"
                                                        "high_background"))
                                    :cut-text-to-fit #t
                                    :text-is-base64 #t
                                    :light-up-when-hovered #t
                                    )))
              (text-area :add-mouse-cycle! mouse-callback)
              text-area)))

      (entry :add-hover-callback! (lambda (is-above)
                                    (c-display is-above audiofile)
                                    (<ra> :set-curr-sample-under-mouse-for-sequencer (if is-above
                                                                                         audiofile
                                                                                         (<ra> :create-illegal-filepath)))))

      entry)
    
    (define area
      (<new> :vertical-list-area2 gui x1 y1 x2 y2
             :num-sub-areas (length audiofiles)
             :get-sub-area-height entry-height
             :create-sub-area create-entry
             :sub-areas-can-be-cached #f
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
                                   (if is-draggable
                                       (show-audiolist-popup-menu #f) ;;curr-audiofile)
                                       (FROM_C-show-blocklist-popup-menu))
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

(define *blocklist-has-been-overridden* (defined? '*blocklist-areas*))
(define *blocklist-areas* (<new> :container '() eq?))

(define (create-blocks-browser-area gui x1 y1 x2 y2 is-draggable state)

  (define entry-areas '())
  
  (define (recreate x1 y1 x2 y2 state)
    (set! entry-areas
          (map (lambda (blocknum)
                 (define color (<ra> :get-block-color blocknum))
                  ;;;(set! color (<gui> :make-color-lighter color 1.5))
                 (set! color (<gui> :set-alpha-for-color color 0.5))
                 
                 (define is-current-block (= (<ra> :current-block) blocknum))
                 
                 (define is-current-blocklist-pos (= (<ra> :get-curr-blocklist-pos) blocknum))
                 
                 (define (mouse-callback button x y . rest)
                   (<ra> :set-curr-blocklist-pos blocknum)
                   (cond ((and (= button *right-button*)
                               (<ra> :shift-pressed))
                          (<ra> :delete-block blocknum))
                         ((and (= button *left-button*)
                               (<gui> :is-double-clicking gui))
                          (<ra> :playlist-insert)
                          ;;(<ra> :create-seqblock -1 blocknum)
                          )
                         ((not (<ra> :is-playing-song))
                          ;;(<ra> :playlist-insert)
                          (<ra> :select-block blocknum -1 #f)
                          ))
                   (update)
                   #f)
                 
                 (define entry
                   (if is-draggable
                       (<new> :seqblock-table-entry-area gui 10 0 100 (round (* 1.2 (get-fontheight)))
                              :is-current is-current-block
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
                                                 :background-color (if #t
                                                                       color
                                                                       (let ((base color)) ;;(<gui> :set-alpha-for-color color 0.5)))
                                                                         (if is-current-block
                                                                             (<gui> :mix-colors "high_background" base 0.95)
                                                                             base)))
                                                 :text-color (lambda ()
                                                               (if is-current-block
                                                                   "sequencer_text_current_block_color"
                                                                   "sequencer_text_color"))
                                                 :align-left #t
                                                 :paint-border #t ;;is-current
                                                 :border-rounding 0
                                                 ;;:border-width 2.9
                                                 ;;:border-color "sequencer_text_current_block_color"
                                                 :border-width (lambda ()
                                                                 (if is-current-blocklist-pos
                                                                     2.9
                                                                     0.5))
                                                 :border-color (lambda ()
                                                                 (if is-current-blocklist-pos
                                                                     "playlist_current_entries_border"
                                                                     "high_background"))
                                                 :cut-text-to-fit #t
                                                 :light-up-when-hovered #t
                                                 )))
                           (text-area :add-mouse-cycle! mouse-callback)
                           text-area))))
                 
                 (entry :add-hover-callback! (lambda (is-above)
                                               ;;(c-display is-above blocknum)
                                               (<ra> :set-curr-editor-block-under-mouse-for-sequencer (if is-above
                                                                                                          blocknum
                                                                                                          -1))))
                 (entry :add-method! :is-current-blocklist-pos? (lambda ()
                                                                  is-current-blocklist-pos))
                 entry)
               
               (iota (<ra> :get-num-blocks))))
    
    (define area (<new> :vertical-list-area gui x1 y1 x2 y2
                        (lambda (x1 x2)
                          entry-areas)))
    
    (if state
        (area :apply-state! state))
    
    area)
  
  (define area (<new> :use-first-subarea-state-as-state-area gui x1 y1 x2 y2))
  (area :add-sub-area-plain! (recreate x1 y1 x2 y2 state))

  (area :add-mouse-cycle! (lambda (button x* y*)
                            (and (not (<ra> :shift-pressed))
                                 (= button *right-button*)
                                 (begin
                                   (if is-draggable
                                       (show-blocklist-popup-menu)
                                       (FROM_C-show-blocklist-popup-menu))
                                   #t))))
  
  ;;(c-display "state:" state)

  (define (ensure-entry-area-visible vertical-area entry-area)
    (vertical-area :ensure-area-is-visible entry-area))

  (define (get-vertical-list-area)
    (car (area :get-sub-areas)))
  
  (area :add-method! :ensure-curr-block-is-visible (lambda ()
                                                     (let loop ((entry-areas entry-areas))
                                                       (when (not (null? entry-areas))
                                                         (define entry-area (car entry-areas))
                                                         (if (entry-area :is-current-blocklist-pos?)
                                                             (ensure-entry-area-visible (get-vertical-list-area) entry-area)
                                                             (loop (cdr entry-areas)))))))

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


  (*blocklist-areas* :add! area)

  (area :override-method! :about-to-be-removed-callback
        (lambda ()
          (define num-removed (*blocklist-areas* :remove! area))
          (when (not (= 1 num-removed))
            (define may-have-been-a-good-reason-for-it (and *blocklist-has-been-overridden*
                                                            (= num-removed 0)))
            ;;(c-display "*gakk-num-removed*:" *gakk-num-removed* ". *gakk-num-added*:" *gakk-num-added* ". *blocklist-areas*:" (*blocklist-areas* :list) ". Before:" list-before)
            (c-display "\n\n\nERROR: Blocklist: Removed wrong number of areas:" num-removed ". Expected 1. May have been a good reason for it:" may-have-been-a-good-reason-for-it)
            (if (and (not (<ra> :release-mode))
                     (not may-have-been-a-good-reason-for-it))
                (assert #f)))))
  
  area
  )

(define (FROM_C-ensure-curr-block-is-visible-in-blocklist)
  (for-each (lambda (blocklist)
              (blocklist :ensure-curr-block-is-visible))
            (*blocklist-areas* :list)))


#!!
(*blocklist-areas* :list)


(let ()
  (define testarea (make-qtarea :width 450 :height 750
                                :sub-area-creation-callback (lambda (gui width height state)
                                                              (create-blocks-browser-area gui 0 0 width height #t state))))
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
                                                  :audiofile (seqblock :sample))
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

(<declare-variable> show-playlist-popup-menu-for-seqblock) ;; in mouse.scm

(define (get-playlist-entry-area gui entry playlist-pos)

  ;(define (is-current-block?)
  ;  (if (eq? (entry :type) 'block)
  ;      (= (<ra> :current-block) (entry :blocknum))))
  ;      (is-current?)))
  
  (define (is-current-playlist-pos?)
    (= (<ra> :get-curr-playlist-pos)
       playlist-pos))

  (define (is-current-seqblock?)
    (equal? (entry :seqblockid)
            (<ra> :get-curr-seqblock-id)))
  
  '(if (<ra> :is-playing-song)
       (and (>= (<ra> :get-song-pos)
                (entry :start-time))
            (or (not (entry :duration))
                (< (<ra> :get-song-pos)
                   (+ (entry :start-time) (entry :duration)))))
       (equal? (entry :seqblockid)
               (<ra> :get-curr-seqblock-id)))
  
  
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
                                              (set! color (<gui> :set-alpha-for-color color 0.5)))
                                          (cond ;((eq? (entry :type) 'pause)
                                        ; "low_background");;#f) ;;"#666666")
                                           ((is-current-seqblock?)
                                            (if color
                                                color ;;(<gui> :mix-colors "high_background" color 0.95)
                                                "high_background"))
                                           (else
                                            color)))
                      :text-color (lambda ()
                                    (cond ((is-current-seqblock?)
                                           "sequencer_text_current_block_color")
                                          (else
                                           "sequencer_text_color")))
                                           ;;*text-color*)
                                          ;;((eq? (entry :type) 'pause)
                                          ;; "black") ;;*text-color*)
                                          ;;(else
                                          ;; "black")))
                      :align-left #t
                      :paint-border #t
                      :border-rounding 0
                      :border-width (lambda ()
                                      (cond ((is-current-playlist-pos?)
                                             2.9)
                                            ((is-current-seqblock?)
                                             1.2)
                                            (else
                                             0.5)))
                      :border-color (lambda ()
                                      (cond ((is-current-playlist-pos?)
                                             "playlist_current_entries_border")
                                            ((is-current-seqblock?)
                                             "sequencer_curr_seqblock_border_color" ;; "sequencer_cursor_color" "sequencer_text_current_block_color"
                                             )
                                            (else
                                             "high_background")))
                      :cut-text-to-fit #t
                      :text-is-base64 #t
                      :light-up-when-hovered #t
                      ))

  (area :add-hover-callback! (lambda (is-hovering)
                               (if (and is-hovering
                                        (entry :seqblockid))
                                   (<ra> :set-curr-seqblock-under-mouse (entry :seqblockid)))))
  
  (area :add-method! :is-current-playlist-pos? is-current-playlist-pos?)

  (area :add-mouse-cycle! (lambda (button x* y*)
                            ;;(if (entry :seqblockid)
                            ;;    (<ra> :set-curr-seqblock (entry :seqblockid)))
                            (c-display "Setting curr to" playlist-pos)
                            (if (and (eq? (entry :type) 'block)
                                     (not (<ra> :is-playing-song)))
                                (<ra> :select-block (entry :blocknum)))
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
                                (when (= button *right-button*)
                                  (if (entry :seqblockid)
                                      (show-playlist-popup-menu-for-seqblock (entry :seqblockid) x* y*)
                                      (FROM_C-show-playlist-popup-menu))
                                  ))
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
                               (if (entry-area :is-current-playlist-pos?)
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
                                                         (if (entry-area :is-current-playlist-pos?)
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
  (<gui> :set-background-color *playlist-area-gui* "low_background")
  (<gui> :show *playlist-area-gui*))
!!#


(define *curr-block/audio-list-type* 'unknown)

(define (get-curr-block/audio-list-type)
  (if (<ra> :seqtrack-for-audiofiles -1)
      'audio
      'block))


(define (FROM_C-playlist-insert!)
  (define pos (<ra> :get-curr-playlist-pos))
  (define entry (get-curr-playlist-entry))
  (if (not (<ra> :seqtrack-for-audiofiles -1))
      (<ra> :create-seqblock -1 (<ra> :get-curr-blocklist-pos) (entry :start-time))
      (let* ((pos (entry :start-time))
             (filename (let ((v (<ra> :get-audio-files)))
                         (if (not *curr-audiofile-num*)
                             (set! *curr-audiofile-num* 0))
                         (if (>= *curr-audiofile-num*
                                 (length v))
                             (set! *curr-audiofile-num* (- (length v) 1)))
                         (and (>= *curr-audiofile-num* 0)
                              (v *curr-audiofile-num*))))
             (duration (and filename
                            (<ra> :get-sample-length filename))))
        (if filename
            (undo-block
             (lambda ()
               (insert-pause-in-seqtrack! -1 pos duration)
               (<ra> :create-sample-seqblock
                     -1
                     filename
                     (entry :start-time)
                     ))))
        ))
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

(define (playlist-swap-entries entry1 entry2)
  (cond ((eq? (entry1 :type) 'pause)
         (move-seqblock! (entry2 :seqblockid) (entry1 :start-time)))
        ((eq? (entry2 :type) 'pause)
         (move-seqblock! (entry1 :seqblockid) (- (+ (entry1 :start-time)
                                                    (entry1 :duration)
                                                    (entry2 :duration))
                                                 (entry1 :duration))))
        (else
         (swap-seqblock-with-next! (entry1 :seqblockid)))))

(define (FROM_C-playlist-up!)

  (define entries (get-playlist-entries -1))
  (define curr-playlist-pos (get-legal-playlist-pos entries))
  (c-display "OLD POS:" (<ra> :get-curr-playlist-pos) curr-playlist-pos)
  (when (and (> curr-playlist-pos 0)
             (< curr-playlist-pos (- (length entries) 1)))
    (define entry1 (entries (- curr-playlist-pos 1)))
    (define entry2 (entries curr-playlist-pos))
    (define new-pos (- curr-playlist-pos 1))
    (playlist-swap-entries entry1 entry2)
    
    (c-display "NEW POS:" new-pos ", old:" curr-playlist-pos)
    ( ;;<ra> :schedule 0
     (lambda ()
       (FROM_C-recreate-playlist-area)
       (<ra> :set-curr-playlist-pos new-pos)
       #f))
    ))

(define (FROM_C-playlist-down!)
  (define entries (get-playlist-entries -1))
  (define curr-playlist-pos (get-legal-playlist-pos entries))
  (when (and (>= curr-playlist-pos 0)
             (< curr-playlist-pos (- (length entries) 2)))
    (define entry1 (entries curr-playlist-pos))
    (define entry2 (entries (+ 1 curr-playlist-pos)))
    (define new-pos (+ curr-playlist-pos 1))
    (playlist-swap-entries entry1 entry2)
    ( ;;<ra> :schedule 0
     (lambda ()
       (FROM_C-recreate-playlist-area)
       (<ra> :set-curr-playlist-pos new-pos)
       #f))))

  
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

  (define playlist-y1 (+ insert/remove-y2 1))
  (define playlist-y2 (+ playlist-y1 list-height))

  (define up/down-y1 playlist-y2)
  (define up/down-y2 (- y2 1))

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

  (define insert-button (<new> :button gui (+ x1 1) insert/remove-y1 (- button-mid 1) insert/remove-y2
                               :text "Insert"
                               :callback-release ra:playlist-insert))
  
  (add-keybinding-configuration-to-gui insert-button
                                       "ra:playlist-insert"
                                       '()
                                       "FOCUS_EDITOR FOCUS_SEQUENCER")
  
  (add-sub-area-plain! insert-button)
                                                  

  (define remove-button (<new> :button gui button-mid insert/remove-y1 (- x2 1) insert/remove-y2
                               :text "Remove"
                               :callback-release ra:playlist-remove))
  
  (add-keybinding-configuration-to-gui remove-button
                                       "ra:playlist-remove"
                                       '()
                                       "FOCUS_EDITOR FOCUS_SEQUENCER")
  (add-sub-area-plain! remove-button)

  (define up-button (<new> :button gui (+ x1 1) up/down-y1 (- button-mid 1) up/down-y2
                           :text "Up"
                           :callback-release FROM_C-playlist-up!))

  (add-keybinding-configuration-to-gui up-button
                                       "ra:playlist-up"
                                       '()
                                       "FOCUS_EDITOR FOCUS_SEQUENCER")
  (add-sub-area-plain! up-button)

  (define down-button (<new> :button gui button-mid up/down-y1 (- x2 1) up/down-y2
                             :text "Down"
                             :callback-release FROM_C-playlist-down!))
  
  (add-keybinding-configuration-to-gui down-button
                                       "ra:playlist-down"
                                       '()
                                       "FOCUS_EDITOR FOCUS_SEQUENCER")
  (add-sub-area-plain! down-button)

  
  (add-sub-area-plain! blocklist)
  (add-sub-area-plain! playlist)

  ;;(<gui> :set-background-color gui "low_background") doesn't work on osx. (really strange)
  (define-override (paint) ;; workaround for osx.
    (<gui> :filled-box gui "low_background" x1 y1 x2 y2 0 0))


  (add-raw-mouse-cycle!
   :enter-func (lambda (button x y)
                 (<ra> :set-sequencer-keyboard-focus)
                 #f))
  
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

