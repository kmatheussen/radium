(provide 'seqblock_editor.scm)

;;(my-require 'area.scm)
(my-require 'gui.scm)
(my-require 'instruments.scm)
(my-require 'area.scm)


(define-class (<seqblock-track-on-off-configuration>)

  (define seqblockid #f)
  
  (define window (<gui> :vertical-layout))
  (<gui> :set-size window 100 200)

  (define area (make-qtarea))
  
  (define gui (area :get-gui))
  
  (<gui> :add window gui)
  
  (define close-button (<gui> :button "Close" (lambda ()
                                                (<gui> :hide window))))

  (<gui> :add window close-button)

  (<gui> :set-takes-keyboard-focus window #f)
  (<gui> :set-parent window (<gui> :get-sequencer-gui))

  (<ra> :schedule 100
        (lambda ()
          (if (not (<gui> :is-open window))
              #f
              (begin
                (this->reorganize seqblockid)
                190))))
  
  (<gui> :add-close-callback window
         (lambda (radium-runs-custom-exec)
           (<gui> :hide window)
           #f))
  
  (<gui> :add-resize-callback gui
         (lambda (width height)
           ;;(c-display "resized:" width height seqblockid)
           (this->reorganize seqblockid)))

  (<ra> :add-undo-redo-callback
        (lambda ()
          (if (not (<gui> :is-open window))
              #f
              (begin
                (this->reorganize seqblockid)
                #t))))

  (<ra> :add-seqblock-deleted-callback
        (lambda (id)
          (cond ((not (<gui> :is-open window))
                 #f)
                ((or (not seqblockid)
                     (not (= id seqblockid)))
                 #t)
                (else
                 (this->reorganize #f)
                 (<gui> :close window)
                 #f))))
                  
  :reorganize (new-seqblockid)
  (let ((old-seqblockid seqblockid))
    
    ;;(c-display "reorganize:" new-seqblockid)
    (set! seqblockid (and new-seqblockid
                          (<ra> :seqblock-is-alive new-seqblockid)
                          new-seqblockid))

    (if (<gui> :is-open gui)
        (area :reset! 0 0 (<gui> :width gui) (<gui> :height gui))
        (area :reset! 0 0 10 10))

    (when (and seqblockid
               (<gui> :is-open window))
      (define seqtracknum (<ra> :get-seqblock-seqtrack-num seqblockid))
      (define seqblocknum (<ra> :get-seqblock-seqblock-num seqblockid))
      (define blocknum (<ra> :get-seqblock-blocknum seqblocknum seqtracknum))

      (if (or (not old-seqblockid)
              (not (= old-seqblockid seqblockid)))
          (<gui> :set-window-title window (<ra> :get-seqblock-name seqblocknum seqtracknum)))

      (area :get-position
            (lambda (x1 y1 x2 y2 width height)
              
              (vertically-layout-areas x1 y1 x2 y2
                                       (iota (<ra> :get-num-tracks blocknum))
                                       :callback
                                       (lambda (tracknum x1 y1 x2 y2)
                                         (define instrument-id (<ra> :get-instrument-for-track tracknum blocknum))
                                         (define box (<new> :checkbox gui x1 y1 x2 y2
                                                            (lambda ()
                                                              (<ra> :is-seqblock-track-enabled tracknum seqblocknum seqtracknum))
                                                            (lambda (enabled)
                                                              (<ra> :undo-seqblock seqblocknum seqtracknum)
                                                              (<ra> :set-seqblock-track-enabled enabled tracknum seqblocknum seqtracknum))
                                                            :text
                                                            (<-> tracknum ": " 
                                                                 (if (< instrument-id 0)
                                                                     ""
                                                                     (<ra> :get-instrument-name instrument-id)))
                                                            :selected-color
                                                            (and (>= instrument-id 0)
                                                                 (<ra> :get-instrument-color instrument-id))
                                                            ))
                                         (area :add-sub-area-plain! box)))))))

  :is-alive? ()
  (<gui> :is-open window)
  
  :show ()
  (<gui> :show window)

  :hide ()
  (<gui> :hide window)
  
  )

#!!
(define onoffgui (<new> :seqblock-track-on-off-configuration))

(onoffgui :reorganize (<ra> :get-seqblock-id 0 0))
(onoffgui :show)


(onoffgui :reorganize (<ra> :get-seqblock-id 1 0))


(define testqtarea (make-qtarea))

(<gui> :show (testqtarea :get-gui))

(testqtarea :add-sub-area-plain! (<new> :button (testqtarea :get-gui) 0 0 100 100
                                        :text "hello"))

!!#


(define *curr-seqblock-track-on-off-window* #f)

(define (show-seqblock-track-on-off-configuration seqblockid blocknum)
  (if (or (not *curr-seqblock-track-on-off-window*)
          (not (*curr-seqblock-track-on-off-window* :is-alive?)))
      (set! *curr-seqblock-track-on-off-window* (<new> :seqblock-track-on-off-configuration)))
  (*curr-seqblock-track-on-off-window* :reorganize seqblockid)
  (*curr-seqblock-track-on-off-window* :show))


#!!
(show-seqblock-track-on-off-configuration 0 0 0)
!!#


