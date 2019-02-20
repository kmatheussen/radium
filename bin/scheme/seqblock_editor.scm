(provide 'seqblock_editor.scm)

;;(my-require 'area.scm)
(my-require 'gui.scm)
(my-require 'instruments.scm)
(my-require 'area.scm)

(delafina (FROM_C-copy-editor-track-on/off-to-seqblock :seqblocknum -1
                                                       :seqtracknum -1)
  
  (when (= -1 seqblocknum)
    (define id (<ra> :get-curr-seqblock-id))
    (when (>= id 0)
      (set! seqtracknum (<ra> :get-seqblock-seqtrack-num id))
      (set! seqblocknum (<ra> :get-seqblock-seqblock-num id))))
                            
  (cond ((not seqtracknum)
         (show-async-message (<gui> :get-sequencer-gui)
                             "No seqtrack selected"))
        ((not seqblocknum)
         (show-async-message (<gui> :get-sequencer-gui)
                             "No seqblock selected"))         
        ((<ra> :seqtrack-for-audiofiles seqtracknum)
         (show-async-message (<gui> :get-sequencer-gui)
                             "Current seqtrack is for audio files, not editor blocks"))
        ((not (<ra> :seqblock-holds-block seqblocknum seqtracknum))
         (show-async-message (<gui> :get-sequencer-gui)
                             "The selected seqblock does not hold an editor block"))
        (else
         (define blocknum (<ra> :get-seqblock-blocknum seqblocknum seqtracknum))
         (<ra> :undo-seqblock seqblocknum seqtracknum)
         (for-each (lambda (tracknum)
                     (<ra> :set-seqblock-track-enabled
                           (<ra> :track-on tracknum blocknum)
                           tracknum seqblocknum seqtracknum))
                   (iota (<ra> :get-num-tracks blocknum))))))

  
(define-class (<seqblock-track-on-off-configuration>)

  (define has-started #f)
  
  (define is-alive #t)
  (define seqblockid #f)
  
  (define window (<gui> :vertical-layout))
  (<gui> :set-size window 100 200)

  (define area (make-qtarea))
  
  (define gui (area :get-gui))
  
  (<gui> :add window gui)
  
  (define close-button (<gui> :button "Close" (lambda ()
                                                (if has-started
                                                    (<gui> :hide window)))))

  (<gui> :add window close-button)

  (<gui> :set-takes-keyboard-focus window #f)
  (<gui> :set-parent window (<gui> :get-sequencer-gui))

  (<ra> :schedule 100
        (lambda ()
          (if (not is-alive)
              #f
              (begin
                (this->reorganize seqblockid)
                190))))
  
  (<gui> :add-close-callback window
         (lambda (radium-runs-custom-exec)
           (if is-alive
               (begin
                 (<gui> :hide window)
                 #f)
               #t)))
  
  (<gui> :add-resize-callback gui
         (lambda (width height)
           ;;(c-display "resized:" width height seqblockid)
           (if has-started
               (this->reorganize seqblockid))))

  (<ra> :add-undo-redo-callback
        (lambda ()
          (if (not is-alive)
              #f
              (begin
                (this->reorganize seqblockid)
                #t))))

  (set! has-started #t)
  
  :reorganize (new-seqblockid)
  (let ((old-seqblockid seqblockid))
    
    ;;(c-display "\n\n  ======================     reorganize:" new-seqblockid " ================ \n\n\n")

    (if (and new-seqblockid
             (not (<ra> :release-mode)))
        (assert (<ra> :seqblock-is-alive new-seqblockid)))
        
    (set! seqblockid (and new-seqblockid
                          (<ra> :seqblock-is-alive new-seqblockid)
                          new-seqblockid))

    (if (not seqblockid)
        (begin
          (area :reset! 0 0 10 10)
          (if (<gui> :is-visible window)
              (<gui> :hide window)))
        (area :reset! 0 0 (<gui> :width gui) (<gui> :height gui)))

    (when (and seqblockid
               is-alive
               (<gui> :is-open window)
               (<gui> :is-visible window))
      
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
                                       :spacing 0
                                       :callback
                                       (lambda (tracknum x1 y1 x2 y2)
                                         (define instrument-id (<ra> :get-instrument-for-track tracknum blocknum))
                                         (define box (<new> :checkbox gui x1 y1 x2 y2
                                                            (lambda ()
                                                              (define seqtracknum (<ra> :get-seqblock-seqtrack-num seqblockid))
                                                              (define seqblocknum (<ra> :get-seqblock-seqblock-num seqblockid))
                                                              (<ra> :is-seqblock-track-enabled tracknum seqblocknum seqtracknum))
                                                            (lambda (enabled)
                                                              (define seqtracknum (<ra> :get-seqblock-seqtrack-num seqblockid))
                                                              (define seqblocknum (<ra> :get-seqblock-seqblock-num seqblockid))
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
                                                            :box-rounding 2.5
                                                            :border-width 1
                                                            ))
                                         (area :add-sub-area-plain! box)))))
      
      (area :update-me!)))

  :get-seqblock-id ()
  seqblockid
  
  :visible? ()
  (and is-alive
       (begin
         (if (not (<ra> :release-mode))
             (assert (<gui> :is-open window)))
         #t)
       (<gui> :is-visible window))

  :alive? ()
  is-alive
  
  :close! ()
  (begin
    (set! is-alive #f)
    (<gui> :close window))
  
  :show ()
  (<gui> :show window)

  :hide ()
  (<gui> :hide window)
  
  )


(define has-been-evaluated-earlier (defined? '*curr-seqblock-track-on-off-window*))

(if has-been-evaluated-earlier
    (if (and *curr-seqblock-track-on-off-window*
             (*curr-seqblock-track-on-off-window* :alive?))
        (*curr-seqblock-track-on-off-window* :close!)))

(if (not has-been-evaluated-earlier)
    (<ra> :add-seqblock-deleted-callback
          (lambda (id)
            (if (and *curr-seqblock-track-on-off-window*
                     (morally-equal? (*curr-seqblock-track-on-off-window* :get-seqblock-id) id))
                (*curr-seqblock-track-on-off-window* :reorganize #f))
            #t)))
                  
(define *curr-seqblock-track-on-off-window* (if has-been-evaluated-earlier
                                                *curr-seqblock-track-on-off-window*
                                                #f))

(define (seqblock-track-on-off-configuration-alive?)
  (and *curr-seqblock-track-on-off-window*
       (*curr-seqblock-track-on-off-window* :alive?)))

(define (seqblock-track-on-off-configuration-visible?)
  (and (seqblock-track-on-off-configuration-alive?)
       (*curr-seqblock-track-on-off-window* :visible?)))

(define (show-seqblock-track-on-off-configuration seqblockid)
  (c-display "show" seqblockid)
  (if (not (seqblock-track-on-off-configuration-alive?))
      (set! *curr-seqblock-track-on-off-window* (<new> :seqblock-track-on-off-configuration)))
  (*curr-seqblock-track-on-off-window* :reorganize seqblockid)
  (*curr-seqblock-track-on-off-window* :show))


#!!
(seqblock-track-on-off-configuration-visible?)
(seqblock-track-on-off-configuration-alive?)
(show-seqblock-track-on-off-configuration (<ra> :get-seqblock-id 0 0) 0)
(show-seqblock-track-on-off-configuration (<ra> :get-seqblock-id 1 0) 0)
(show-seqblock-track-on-off-configuration 500 0)
!!#


