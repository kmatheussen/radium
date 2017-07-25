
(define-struct velocity
  :place ;; relative to note start
  :value
  :logtype)

(define-struct pitch
  :place ;; relative to note start
  :value
  :logtype
  :chance)

;; Tips:
;; -----
;; * To get start pitch of a note: "(note :pitches 0 :value)"
;; * To get start velocity of a note: "(note :velocities 0 :value)"
;; * To get chance value of note: "(note :pitches 0 :chance)"
(define-struct note
  :place
  :pitches
  :velocities
  :continues-next-block
  )

(define (get-note-duration note)
  ((last (note :pitches)) :place))

(define (get-note-end note)
  (+ (note :place)
     (get-note-duration note)))

(define-struct editor-area
  :start-place
  :end-place
  :start-track
  :end-track
  :blocknum)

(delafina (in-editor-area :start-place
                          :end-place #f
                          :area)
  (if (not end-place)
      (and (>= start-place (area :start-place))
           (< start-place (area :end-place)))
      (begin
        (assert (> end-place start-place))
        (and (< start-place (area :end-place))
             (>= end-place (area :start-place))))))
          
(delafina (get-block-editor-area :blocknum -1)
  (make-editor-area :start-place 0
                  :end-place (<ra> :get-num-lines blocknum)
                  :start-track 0
                  :end-track (<ra> :get-num-tracks blocknum)
                  :blocknum blocknum))

(delafina (get-track-editor-area :tracknum -1
                               :blocknum -1)
  (define start-track (if (= -1 tracknum)
                          (<ra> :current-track blocknum)
                          tracknum))
  (assert (< start-track (<ra> :get-num-tracks)))
  (assert (>= start-track 0))
          
  (make-editor-area :start-place 0
                  :end-place (<ra> :get-num-lines blocknum)
                  :start-track start-track
                  :end-track (1+ start-track)
                  :blocknum blocknum))

(delafina (get-ranged-editor-area :blocknum -1)
  (assert (<ra> :has-range blocknum))
  (make-editor-area :start-place (<ra> :get-range-start-place blocknum)
                  :end-place (<ra> :get-range-end-place blocknum)
                  :start-track (<ra> :get-range-start-track blocknum)
                  :end-track (<ra> :get-range-end-track blocknum)
                  :blocknum blocknum))

(define (undo-editor-area area)
  (undo-block
   (lambda ()
     (let loop ((tracknum (area :start-track)))
       (when (< tracknum (area :end-track))
         (c-display "Creating undo for track " tracknum)
         (<ra> :undo-notes tracknum (area :blocknum))
         (loop (1+ tracknum)))))))
             
;;;;;;;;; GET NOTES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-note-velocities blocknum tracknum notenum notestart)
  (c-display "num velocities:" blocknum tracknum notenum (<ra> :get-num-velocities notenum tracknum blocknum))
  (map (lambda (velocitynum)
         (make-velocity :place (- (<ra> :get-velocity-place velocitynum notenum tracknum blocknum) notestart)
                        :value (<ra> :get-velocity-value velocitynum notenum tracknum blocknum)
                        :logtype (<ra> :get-velocity-logtype velocitynum notenum tracknum blocknum)))
       (iota (<ra> :get-num-velocities notenum tracknum blocknum))))

(define (get-note-pitches blocknum tracknum notenum notestart)
  (map (lambda (pitchnum)
         (make-pitch :place (- (<ra> :get-pitch-place pitchnum notenum tracknum blocknum) notestart)
                     :value (<ra> :get-pitch-value pitchnum notenum tracknum blocknum)
                     :logtype (<ra> :get-pitch-logtype pitchnum notenum tracknum blocknum)
                     :chance (<ra> :get-pitch-chance pitchnum notenum tracknum blocknum)))
       (iota (<ra> :get-num-pitches notenum tracknum blocknum))))

(define (get-note blocknum tracknum notenum startplace)
  (let* ((note-start (<ra> :get-note-start notenum tracknum blocknum))
         (velocities (get-note-velocities blocknum tracknum notenum note-start))
         (pitches (get-note-pitches blocknum tracknum notenum note-start)))
    (make-note :place (- note-start startplace)
               :pitches pitches
               :velocities velocities
               :continues-next-block (<ra> :note-continues-next-block notenum tracknum blocknum))))

(delafina (get-area-notes :area
                          :include-all #t) ;; if include-all is #t, we also include all notes starting to play before the area and either ends inside the area or after.
  (define blocknum (area :blocknum))
  (define startplace (area :start-place))
  (define endplace (area :end-place))
  (map (lambda (tracknum)
         (if (>= tracknum (<ra> :get-num-tracks blocknum))
             '()
             (let ((num-notes (<ra> :get-num-notes tracknum blocknum)))
               (let loop ((notenum 0))
                 (if (>= notenum num-notes)
                     '()
                     (let ((note-start (<ra> :get-note-start notenum tracknum blocknum)))
                       (cond ((>= note-start endplace)
                              '())
                             ((in-editor-area note-start
                                              (and include-all (<ra> :get-note-end notenum tracknum blocknum))
                                              area)
                              (cons (get-note blocknum tracknum notenum startplace)
                                    (loop (1+ notenum))))
                             (else
                              (loop (1+ notenum))))))))))
       (integer-range (area :start-track)
                      (1- (area :end-track)))))

(delafina (get-ranged-notes :blocknum -1)
  (get-area-notes (get-ranged-editor-area blocknum)))


(define (map-area-notes area-notes func)
  (map (lambda (track-notes)
         (map func track-notes))
       area-notes))

(define (for-each-area-note area-notes func)
  (for-each (lambda (track-notes)
              (for-each func track-notes))
            area-notes))
  

#||
(pp (map (lambda (track)
           (map (lambda (note)
                  (<-> (note :dir) "\n"))
                track))
         (get-notes 0 0)))

(c-display ((get-notes) 0 0 :pitch))

(get-notes :startplace 7)

(pretty-print (get-notes))



(pretty-print (get-notes :startplace 4))

(pp (hash-table* 'b 2 'c 3))
(map values (hash-table* 'b 2 'c 3))

(equal? (hash-table* :c 8 :b 9) (hash-table* :b 9 :c 8))

(begin
  (<ra> :get-num-velocities 0 -1 -1 5)
  (c-display "THSI IS NOT SUPPOSED TO HAPPEN"))

||#


(define (cut-note note place)
  (assert (> place (note :place)))
  note)

(define (split-notes notes place)
  (let loop ((bef '())
             (notes notes))
    (if (null? notes)
        (list (reverse bef) '())
        (let* ((note (car notes))
               (start (note :place))
               (end (get-note-end note))
               )
          (cond ((>= start place)
                 (list (reverse bef) notes))
                (else
                 (loop (cons (cut-note note place)
                             bef)
                       (cdr notes))))))))
               

;; Returns notes1, where the notes in notes1 that spans the range of notes2, has been replaced by notes2.
;; 'notes1' and 'notes2' are linked lists of notes. I.e. just one track.
(define (merge-notes notes1 notes2)
  (cond ((null? notes1)
         notes2)
        ((null? notes2)
         notes1)
        (else
         (define firstnote2 (car notes2))
         (define lastnote2 (last notes2))
         (define start2 (firstnote2 :place))
         (define end2 (get-note-end lastnote2))

         (append (car (split-notes notes1 start2))
                 notes2
                 (cadr (split-notes notes1 end2))))))

#!
(pretty-print
 (merge-notes (list (make-note :place 0
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
                              :continues-next-block #t))
             (list (make-note :place 5
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
                                                (make-velocity :place 3
                                                               :value 5
                                                               :logtype 0))
                              :continues-next-block #t))))
             

                        
                                       
!#

;;;;;;;;; SET NOTES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-note! note tracknum blocknum)

  (c-display "note:")
  (pretty-print note)

  (define num-lines (<ra> :get-num-lines blocknum))
  (assert (< (note :place) num-lines))
           
  ;; add note
  (define notenum (<ra> :add-note
                        (note :pitches 0 :value)
                        (note :velocities 0 :value)
                        (note :place)
                        (min num-lines (get-note-end note))
                        tracknum
                        blocknum))

  ;; add pitches
  ;;
  ;; logtype of first pitch
  (<ra> :set-pitch-logtype ((car (note :pitches)) :logtype) 0 notenum tracknum blocknum)

  ;; end-pitch. TODO: end-pitch is not correct if end-place is >= num_lines.
  (let ((pitch (last (note :pitches))))
    (if (not (= 0 (pitch :value)))
        (<ra> :set-pitch
              (pitch :value)
              'same-place
              1  ;; (1==current end-pitch num)
              notenum tracknum blocknum)))

  (for-each (lambda (pitch)
              (define place (+ (pitch :place)
                               (note :place)))
              (if (< place num-lines)
                  (let ((pitchnum (<ra> :add-pitch
                                        (pitch :value)
                                        place
                                        notenum tracknum blocknum)))
                    (<ra> :set-pitch-logtype (pitch :logtype) pitchnum notenum tracknum blocknum))))
            (cdr (butlast (note :pitches))))

  
  ;; add velocities
  ;;
  ;; logtype of first velocity.
  (<ra> :set-velocity-logtype ((car (note :velocities)) :logtype) 0 notenum tracknum blocknum)

  ;; end-velocity. TODO: end-velocity is not correct if end-place is >= num_lines.
  (let ((velocity (last (note :velocities))))
    (<ra> :set-velocity
          (velocity :value)
          'same-place
          1 ;; (1==current end-velocity num)
          notenum tracknum blocknum))

  (for-each (lambda (velocity)
              (define place (+ (velocity :place)
                               (note :place)))
              (if (< place num-lines)
                  (let ((velocitynum (<ra> :add-velocity
                                           (velocity :value)
                                           place
                                           notenum tracknum blocknum)))
                    (<ra> :set-velocity-logtype (velocity :logtype) velocitynum notenum tracknum blocknum))))
            (cdr (butlast (note :velocities))))
  
  notenum)
  

(define (add-notes! area-notes area)
  (define blocknum (area :blocknum))
  (define startplace (area :start-place))
  (define endplace (area :end-place))
  (define starttrack (area :start-track))
  (for-each (lambda (addtracknum track-notes)
              (let ((tracknum (+ starttrack addtracknum)))
                (if (< tracknum (<ra> :get-num-tracks blocknum)) ;; Need this test since the 'area-notes' doesn't have to be created from 'area'.
                    (for-each (lambda (note)
                                (let ((place (+ startplace (note :place))))
                                  (if (< place endplace)
                                      (add-note! (copy-note note
                                                            :place place)
                                                 tracknum
                                                 blocknum))))
                              track-notes))))
            (iota (length area-notes))
            area-notes))

(delafina (remove-notes! :area
                         :include-all #t) ;; if include-all is #t, we also include all notes starting to play before the area and either ends inside the area or after.
  (define blocknum (area :blocknum))
  (define startplace (area :start-place))
  (define endplace (area :end-place))
  (assert (<= (area :end-track)
              (<ra> :get-num-tracks blocknum)))
  
  (let loop ((tracknum (area :start-track))
             (notenum 0))
    (if (< tracknum (area :end-track))
        (let ((num-notes (<ra> :get-num-notes tracknum blocknum)))
          (if (>= notenum num-notes)
              (loop (1+ tracknum)
                    0)
              (let ((note-start (<ra> :get-note-start notenum tracknum blocknum)))
                (cond ((>= note-start endplace)
                       (loop (1+ tracknum)
                             0))
                      ((in-editor-area note-start
                                       (and include-all (<ra> :get-note-end notenum tracknum blocknum))
                                       area)
                       (<ra> :delete-note notenum tracknum blocknum)
                       (loop tracknum
                             notenum))
                      (else
                       (loop tracknum
                             (1+ notenum))))))))))
#||
(remove-notes! 7 9 0 1)

||#


(define (replace-notes! area-notes area)
  (remove-notes! area)
  (add-notes! area-notes area))

#||
(let ((notes (get-notes :starttracknum 0)))
  (c-display (length ((caar notes) :pitches)))
  (for-each c-display ((caar notes) :pitches))
  (replace-notes! notes :starttrack 1))

||#


;;;;;;;;; TESTING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reverse-velocity velocity length)
  (<copy-velocity> velocity
                   :place (scale (velocity :place) 0 length length 0)))

(define (reverse-velocities velocities length)
  (map (lambda (velocity)
         (reverse-velocity velocity length))
       (reverse velocities)))

(define (reverse-pitch pitch length)
  (<copy-pitch> pitch
                :place (scale (pitch :place) 0 length length 0)))

(define (reverse-pitches pitches length)
  (define first-pitch (reverse-pitch (first pitches) length))
  (define last-pitch (reverse-pitch (last pitches) length))
  
  ;;(c-display "bef first-pitch:" first-pitch length)
  ;;(c-display "bef last-pitch:" last-pitch length)

  (when (= 0 (last-pitch :value))
    (set! last-pitch  (<copy-pitch> last-pitch  :value (first-pitch :value)))
    (set! first-pitch (<copy-pitch> first-pitch :value 0)))

  ;;(c-display "aft first-pitch:" first-pitch)
  ;;(c-display "aft last-pitch:" last-pitch)

  (append (list last-pitch)
          (map (lambda (pitch)
                 (reverse-pitch pitch length))
               (reverse (cdr (butlast pitches))))
          (list first-pitch)))

(define (reverse-note note length)
  (let ((duration (get-note-duration note)))
    (<copy-note> note
                 :place (scale (get-note-end note) 0 length length 0)
                 :pitches (reverse-pitches (note :pitches) duration)
                 :velocities (reverse-velocities (note :velocities) duration)
                 )))

(define (get-reversed-notes area)
  (define area-length (- (area :end-place)
                         (area :start-place)))
  (map (lambda (tracknotes)
         (map (lambda (note)
                (reverse-note note area-length))
              (reverse tracknotes)))
       (get-area-notes area)))
              

(delafina (reverse-block! :blocknum -1)
  (define area (get-block-editor-area blocknum))
  (replace-notes! (get-reversed-notes area)
                  area))
              
(delafina (reverse-track! :tracknum -1
                          :blocknum -1)
  (define area (get-track-editor-area tracknum blocknum))
  (replace-notes! (get-reversed-notes area)
                  area))
              
(delafina (reverse-range! :blocknum -1)
  (define area (get-ranged-editor-area blocknum))
  (replace-notes! (get-reversed-notes area)
                  area))

#||

(reverse-range!)

(reverse-track!)

(pretty-print (get-reversed-range))
(pretty-print (get-reversed-track -1 0))


(let ((tracknum 0))
  (set-notes! (reverse-track 0 tracknum) 0 tracknum))


(load "notes.scm")

(begin *load-path*)

(<ra> :delete-note 1)

||#


