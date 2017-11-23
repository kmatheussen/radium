(provide 'randomize-note-durations.scm)


(define (get-randomly-distributed-units num-notes num-units)
  (if (= 0 num-notes)
      '()
      (let ((ret (make-vector num-notes 0)))
        (let loop ((i 0))
          (when (< i num-units)
            (define note-num (integer-myrand 0 (1- num-notes)))
            (set! (ret note-num) (1+ (ret note-num)))
            (loop (1+ i))))
        (to-list ret))))

#!!
(get-randomly-distributed-units 0 7)
(get-randomly-distributed-units 5 7)
!!#


(define (get-min-duration-of-note note)
  (max ((second-last (note :pitches)) :place)
       ((second-last (note :velocities)) :place)))


(define (change-pitches/velocities-duration pitches new-duration)
  (let loop ((pitches pitches))
    ;;(c-display "pitches:" pitches)
    (if (null? (cdr pitches))
        (list (copy-hash (car pitches)
                         :place new-duration))
        (cons (car pitches)
              (loop (cdr pitches))))))

(***assert*** (change-pitches/velocities-duration (list (make-pitch :place 0
                                                                    :value 5
                                                                    :logtype 0
                                                                    :chance 1)
                                                        (make-pitch :place 20
                                                                    :value 5
                                                                    :logtype 0
                                                                    :chance 1))
                                                  10)
              (list (make-pitch :place 0
                                :value 5
                                :logtype 0
                                :chance 1)
                    (make-pitch :place 10
                                :value 5
                                :logtype 0
                                :chance 1)))


(define (set-new-note-durations notes new-durations new-pauses)
  (assert (not (null? notes)))
  (assert (>= (length notes)
              (length new-durations)))
  (let loop ((notes notes)
             (new-start-pos ((car notes) :place))
             (new-pauses new-pauses)
             (new-durations new-durations))
    (cond ((null? notes)
           (assert (null? new-durations))
           '())
          ;;((null? new-durations)
          ;; notes)
          (else
           (let ((note (car notes))
                 (new-pause (or (cl-car new-pauses) 0))
                 (new-duration (car new-durations)))
             (cons (copy-note note
                              :place new-start-pos
                              :pitches (change-pitches/velocities-duration (note :pitches) new-duration)
                              :velocities (change-pitches/velocities-duration (note :velocities) new-duration))
                   (loop (cdr notes)
                         (+ new-start-pos new-duration new-pause)
                         (cl-cdr new-pauses)
                         (cl-cdr new-durations))))))))
                              

(define (set-last-note-to-end-at notes last-endpos)
  (let loop ((notes notes))
    (if (null? (cdr notes))
        (let* ((note (car notes))
               (place (note :place))
               (new-duration (- last-endpos place)))
          (assert (> new-duration 0))
          (list (copy-note note
                           :pitches (change-pitches/velocities-duration (note :pitches) new-duration)
                           :velocities (change-pitches/velocities-duration (note :velocities) new-duration))))
        (cons (car notes)
              (loop (cdr notes))))))

(define (find-pause-duration notes last-endpos)
  (let loop ((notes notes))
    (if (null? notes)
        0
        (let ((note1 (car notes)))
          (if (null? (cdr notes))
              (max 0
                   (- last-endpos (get-note-end note1)))
              (let ((note2 (cadr notes)))
                (+ (max 0
                        (- (note2 :place)
                           (get-note-end note1)))
                   (loop (cdr notes)))))))))

(***assert*** (find-pause-duration (list (make-simple-note 2 4))
                                   4)
              0)

(***assert*** (find-pause-duration (list (make-simple-note 2 4))
                                   5)
              1)

(***assert*** (find-pause-duration (list (make-simple-note 2 4)
                                         (make-simple-note 4 6))
                                   6)
              0)

(***assert*** (find-pause-duration (list (make-simple-note 2 4)
                                         (make-simple-note 5 6))
                                   6)
              1)

(***assert*** (find-pause-duration (list (make-simple-note 2 4)
                                         (make-simple-note 5 6)
                                         (make-simple-note 7 8))
                                   9)
              3)

            
;; Husk at siste note skal slutte på samme sted som før.
;; Burde det legges inn tilfeldige pauser også? (Ja, og jo flere pauser inn, jo flere pauser ut)
;; Om første note ikke ligger på unit, så gjør ikke de påfølgende notene det heller. (kanskje ikke så farlig)
;; Om første note ligger på start-place, så blir den liggende der. Hvis ikke, så flyttes den tilfeldig.
(define (randomize-note-durations notes end-place unit) ;; 'end-place' is the last position (plus one unit) that the last note can be moved to.
  (assert (> unit 0))
  ;;(assert (>= end-place (+ ((last notes) :place) (get-note-duration (last notes)))))
  
  (call-with-exit
   (lambda (return)

     (if (null? notes)
         (return '()))
     
     (define num-notes (length notes))

     (define min-durations (map (lambda (note)
                                  (define min-duration (get-min-duration-of-note note))
                                  (if (< (note :place) 0) ;; For notes starting before the area, ensure that we don't cut notes before the area starts.
                                      (set! min-duration (max (- (note :place))
                                                              min-duration)))
                                  (max unit
                                       (unit-ceiling min-duration
                                                     unit)))
                                notes))
     
     (define num-extra-units (floor (/ (let ((total-duration (- end-place
                                                                ((car notes) :place)))
                                             (total-min-duration (apply + min-durations)))
                                         (- total-duration
                                            total-min-duration))
                                       unit)))
     
     (when (<= num-extra-units 0)
       (c-display "min-durations:" min-durations ". num-extra-units:" num-extra-units ". total-duration:" (- end-place ((car notes) :place)) ". unit:" unit)
       (<ra> :show-async-message "Not enough room to randomize note positions. Try to increase LZ value")
       (return notes))

     (define num-old-pause-units (unit-ceiling (/ (find-pause-duration notes end-place)
                                                  unit)
                                               unit))
     (define num-new-pause-units (let ()
                                   (define random-num-pause-units (round (* (+ 0.9 (* 3 num-old-pause-units))
                                                                            (- 1 (sqrt (myrand 0 1))))))
                                   (min num-extra-units
                                        random-num-pause-units)))
     (define num-new-duration-units (- num-extra-units num-new-pause-units))
     
     (define new-durations (map (lambda (min-duration randomly-added-durations)
                                  (+ min-duration (* unit randomly-added-durations)))
                                min-durations
                                (get-randomly-distributed-units num-notes num-new-duration-units)))
                                

     (define new-pauses (map (lambda (num)
                               (* num unit))
                             (get-randomly-distributed-units (1- num-notes) num-new-pause-units)))
     
     '(c-display ((car notes) :place) "->" end-place
                ". min-durations:" min-durations
                ". new-durations:" new-durations
                ". new-pauses:" new-pauses
                ". pause old/new:" num-old-pause-units num-new-pause-units
                ". durations new:" num-new-duration-units
                ". num-extra-units:" num-extra-units (<-> "(dur: " (* num-extra-units unit) ")"))
     
     (define new-notes (set-new-note-durations notes
                                               new-durations
                                               new-pauses))

     ;; Check if last note should end the same place as before. (if it ended after end-place, it should)
     (let* ((last-note (last notes))
            (last-duration (get-note-duration last-note))
            (last-endplace (+ (last-note :place)
                              last-duration)))
       (if (> last-endplace end-place)
           (set-last-note-to-end-at new-notes last-endplace)
           new-notes)))))
     

(pp
 (randomize-note-durations (list (make-simple-note 0 3 'a)
                               (make-simple-note 5 8 'b)
                               (make-simple-note 9 10 'b))
                         11 1))

(pp
 (randomize-note-durations (list (make-note :place 0
                                          :pitches (list (make-pitch :place 0
                                                                     :value 5
                                                                     :logtype 0
                                                                     :chance 1)
                                                         (make-pitch :place 20
                                                                     :value 5
                                                                     :logtype 0
                                                                     :chance 1))
                                          :velocities (list (make-velocity :place 0
                                                                           :value 5
                                                                           :logtype 0)
                                                            (make-velocity :place 2
                                                                           :value 1
                                                                           :logtype 0)
                                                            (make-velocity :place 20
                                                                           :value 5
                                                                           :logtype 0))
                                          :continues-next-block #t
                                          :id -1)
                               (make-note :place 5
                                          :pitches (list (make-pitch :place 0
                                                                     :value 5
                                                                     :logtype 0
                                                                     :chance 1)
                                                        (make-pitch :place 20
                                                                    :value 5
                                                                    :logtype 0
                                                                    :chance 1))
                                          :velocities (list (make-velocity :place 0
                                                                           :value 5
                                                                          :logtype 0)
                                                            (make-velocity :place 20
                                                                           :value 5
                                                                           :logtype 0))
                                         :continues-next-block #t
                                         :id -1))
                         8
                         1
                         )
 )


(define (randomize-note-durations! area)
  (undo-editor-area area)
  (define start-place (area :start-place))
  (define end-place (area :end-place))
  (define duration (- end-place start-place))
  (c-display "duration:" duration)
  (define unit (/ 1
                  (<ra> :get-line-zoom-block-ratio)))
  (replace-notes! (map (lambda (tracknum track-notes)
                         (let ((new-notes (randomize-note-durations track-notes duration unit)))
                           ;;(pretty-print new-notes)
                           new-notes))
                       (integer-range (area :start-track) (1- (area :end-track)))
                       (get-area-notes area
                                       :include-starting-before #t))
                  area
                  :include-starting-before #t))



  
