(provide 'instruments.scm)

(define (undo-block block)
  (<ra> :open-undo)
  (catch #t
         block
         (lambda args ;; Catch exceptions to ensure (<ra> :cose-undo) will be called
           (display "args")(display args)(newline)
           (apply format #t (cadr args))
           (display (ow!))))
  (<ra> :close-undo))


(define (for-all-tracks func)
  (for-each (lambda (blocknum)
              (for-each (lambda (tracknum)
                          (func blocknum tracknum))
                        (iota (<ra> :get-num-tracks blocknum))))
            (iota (<ra> :get-num-blocks))))

(define (get-instruments-connecting-to-instrument id-instrument)
  (map (lambda (in-connection)
         (<ra> :get-audio-connection-source-instrument in-connection id-instrument))
       (iota (<ra> :get-num-in-audio-connections id-instrument))))

(define (get-instruments-connecting-from-instrument id-instrument)
  (map (lambda (in-connection)
         (<ra> :get-audio-connection-dest-instrument in-connection id-instrument))
       (iota (<ra> :get-num-out-audio-connections id-instrument))))

(define (get-instruments-econnecting-to-instrument id-instrument)
  (map (lambda (in-connection)
         (<ra> :get-event-connection-source-instrument in-connection id-instrument))
       (iota (<ra> :get-num-in-event-connections id-instrument))))

(define (get-instruments-econnecting-from-instrument id-instrument)
  (map (lambda (in-connection)
         (<ra> :get-event-connection-dest-instrument in-connection id-instrument))
       (iota (<ra> :get-num-out-event-connections id-instrument))))

(define (duplicate-connections id-old-instrument id-new-instrument)
  (for-each (lambda (from-instrument)
              (<ra> :create-audio-connection
                    from-instrument
                    id-new-instrument))
            (get-instruments-connecting-to-instrument id-old-instrument))
  (for-each (lambda (to-instrument)
              (<ra> :create-audio-connection
                    id-new-instrument
                    to-instrument))
            (get-instruments-connecting-from-instrument id-old-instrument))
  (for-each (lambda (from-instrument)
              (<ra> :create-event-connection
                    from-instrument
                    id-new-instrument))
            (get-instruments-econnecting-to-instrument id-old-instrument))
  (for-each (lambda (to-instrument)
              (<ra> :create-event-connection
                    id-new-instrument
                    to-instrument))
            (get-instruments-econnecting-from-instrument id-old-instrument)))

(define (replace-instrument-in-all-tracks! id-old-instrument id-new-instrument)
  (for-all-tracks
   (lambda (blocknum tracknum)
     (if (= id-old-instrument (<ra> :get-instrument-for-track tracknum blocknum))
         (<ra> :set-instrument-for-track id-new-instrument tracknum blocknum)))))

(define (replace-instrument-in-mixer id-old-instrument id-new-instrument)
  (define x (<ra> :get-instrument-x id-old-instrument))
  (define y (<ra> :get-instrument-y id-old-instrument))
  (<ra> :delete-instrument id-old-instrument)
  (<ra> :set-instrument-position x y id-new-instrument)
  )

;; Called from the outside. 'instrument-description' can be false.
(define (replace-instrument id-old-instrument instrument-description)
  (let ((instrument-description (or instrument-description
                                    (<ra> :instrument-description-popup-menu))))
    (when (not (string=? "" instrument-description))
      (undo-block
       (lambda ()
         (define id-new-instrument (<ra> :create-audio-instrument-from-description instrument-description))
         (c-display "hist1: " (<ra> :get-undo-history))
         (when (not (= -1 id-new-instrument))
           (duplicate-connections id-old-instrument id-new-instrument)
           (replace-instrument-in-all-tracks! id-old-instrument id-new-instrument)
           (c-display "hist2: " (<ra> :get-undo-history))
           (replace-instrument-in-mixer id-old-instrument id-new-instrument)
           (c-display "hist3: " (<ra> :get-undo-history))
           ))))))


#!!
(<ra> :get-num-audio-instruments)
(define id (<ra> :get-audio-instrument-id 5))
(<ra> :get-instrument-x 17)
(<ra> :get-instrument-y 17)

(<ra> :set-instrument-position -80 106 17)
(<ra> :connect-audio-instrument-to-main-pipe 17)
(<ra> :delete-instrument 22)

;; fix load preset
(let ((descr (<ra> :instrument-description-popup-menu)))
  (define id (<ra> :create-audio-instrument-from-description descr))
  ;;(<ra> :delete-instrument id)
  id
  )

(let ((id-old-instrument (<ra> :get-instrument-for-track))
      (descr (<ra> :instrument-description-popup-menu)))
  (replace-instrument id-old-instrument descr)
  ;;(define id-new-instrument (<ra> :create-audio-instrument-from-description descr))
  ;;(<ra> :set-instrument-for-track id-new-instrument)
  id-old-instrument)

(<ra> :get-undo-history)

todo:
* Fix load preset
* Check if cancelUndo is called automatically if no undo has been added.
* Check if instrument widget for deleted patch is hidden.
!!#





;; Called from the outside. if 'do-autoconnect' is true, instruments sending sound to id-instrument and instruments getting sound from id-instrument will be connected.
(define (delete-instrument id-instrument do-autoconnect)
  ...)


