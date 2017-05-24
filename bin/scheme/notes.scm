
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
                   
(delafina (get-notes :startplace 0
                     :endplace (<ra> :get-num-lines blocknum)
                     :starttracknum -1
                     :endtracknum -1
                     :blocknum -1
                     )
  (if (= -1 starttracknum)
      (set! starttracknum (<ra> :current-track blocknum)))
  
  (if (<= endtracknum starttracknum)
      (set! endtracknum (1+ starttracknum)))

  (c-display "start/end" starttracknum endtracknum)
  
  (map (lambda (tracknum)
         (if (>= tracknum (<ra> :get-num-tracks blocknum))
             '()
             (let ((num-notes (<ra> :get-num-notes tracknum blocknum)))
               (let loop ((notenum 0))
                 (if (>= notenum num-notes)
                     '()
                     (let ((note-start (<ra> :get-note-start notenum tracknum blocknum)))
                       (cond ((< note-start startplace)
                              (loop (1+ notenum)))
                             ((>= note-start endplace)
                              '())
                             (else
                              (cons (get-note blocknum tracknum notenum startplace)
                                    (loop (1+ notenum)))))))))))
       (map (lambda (i) (+ i starttracknum))
            (iota (- endtracknum starttracknum)))))

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


(delafina (delete-notes! :startplace 0
                         :endplace (<ra> :get-num-lines blocknum)
                         :starttracknum -1
                         :endtracknum -1
                         :blocknum -1)
  (let loop ((tracknum starttracknum)
             (notenum 0))
    (when (and (< tracknum endtracknum)
               (< tracknum (<ra> :get-num-tracks blocknum)))
      (let ((num-notes (<ra> :get-num-notes tracknum blocknum)))
        (if (>= notenum num-notes)
            (loop (1+ tracknum)
                  0)
            (let ((note-start (<ra> :get-note-start notenum tracknum blocknum)))
              (cond ((<= note-start startplace)
                     (loop tracknum
                           (1+ notenum)))
                    ((>= note-start endplace)
                     (loop (1+ tracknum)
                           0))
                    (else
                     (<ra> :delete-note notenum tracknum blocknum)
                     (loop tracknum
                           notenum)))))))))
#||
(delete-notes! 7 9 0 1)

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
  
  ;; add note
  (define notenum (<ra> :add-note
                        (note :pitches 0 :value)
                        (note :velocities 0 :value)
                        (note :place)
                        tracknum
                        blocknum))
  
  ;; add pitches
  ;;
  (<ra> :set-pitch-logtype ((car (note :pitches)) :logtype) 0 notenum tracknum blocknum)
  
  (let ((pitch (last (note :pitches))))
    (if (not (= 0 (pitch :value)))
        (<ra> :set-pitch (pitch :value) -1 1 notenum tracknum blocknum)))

  (for-each (lambda (pitch)
              (define pitchnum (<ra> :add-pitch (pitch :value) (+ (pitch :place) (note :place)) notenum tracknum blocknum))
              (<ra> :set-pitch-logtype (pitch :logtype) pitchnum notenum tracknum blocknum))
            (cdr (butlast (note :pitches))))

  
  ;; add velocities
  ;;
  (<ra> :set-velocity-logtype ((car (note :velocities)) :logtype) 0 notenum tracknum blocknum)

  (let ((velocity (last (note :velocities))))
    (<ra> :set-velocity
          (velocity :value)
          -1
          1 notenum tracknum blocknum))

  (for-each (lambda (velocity)
              (define velocitynum (<ra> :add-velocity (velocity :value) (+ (velocity :place) (note :place)) notenum tracknum blocknum))
              (<ra> :set-velocity-logtype (velocity :logtype) velocitynum notenum tracknum blocknum))
            (cdr (butlast (note :velocities))))
  
  notenum)

(delafina (set-notes! :notes
                      :startplace 0
                      :starttrack -1
                      :blocknum -1)
  (if (= -1 starttrack)
      (set! starttrack (<ra> :get-cursor-track)))
  (for-each (lambda (addtracknum notes)
              (let ((tracknum (+ starttrack addtracknum)))
                (when (< tracknum (<ra> :get-num-tracks blocknum))
                  (<ra> :delete-all-notes-in-track tracknum blocknum)
                  (for-each (lambda (note)
                              (add-note! note tracknum blocknum))
                            notes))))
            (iota (length notes))
            notes)
  )

#||
(let ((notes (get-notes :starttracknum 0)))
  (c-display (length ((caar notes) :pitches)))
  (for-each c-display ((caar notes) :pitches))
  (set-notes! notes :starttrack 1))

||#


;;;;;;;;; TESTING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reverse-velocity velocity length)
  (copy-velocity velocity
                 :place (scale (velocity :place) 0 length length 0)))

(define (reverse-velocities velocities length)
  (map (lambda (velocity)
         (reverse-velocity velocity length))
       (reverse velocities)))

(define (reverse-pitch pitch length)
  (copy-pitch pitch
              :place (scale (pitch :place) 0 length length 0)))

(define (reverse-pitches pitches length)
  (define first-pitch (reverse-pitch (first pitches) length))
  (define last-pitch (reverse-pitch (last pitches) length))
  
  ;;(c-display "bef first-pitch:" first-pitch length)
  ;;(c-display "bef last-pitch:" last-pitch length)

  (when (= 0 (last-pitch :value))
    (set! last-pitch  (copy-pitch last-pitch  :value (first-pitch :value)))
    (set! first-pitch (copy-pitch first-pitch :value 0)))

  ;;(c-display "aft first-pitch:" first-pitch)
  ;;(c-display "aft last-pitch:" last-pitch)

  (append (list last-pitch)
          (map (lambda (pitch)
                 (reverse-pitch pitch length))
               (reverse (cdr (butlast pitches))))
          (list first-pitch)))

(define (reverse-note note length)
  (let ((duration (get-note-duration note)))
    (copy-note note
               :place (scale (get-note-end note) 0 length length 0)
               :pitches (reverse-pitches (note :pitches) duration)
               :velocities (reverse-velocities (note :velocities) duration)
               )))

(delafina (get-reversed-notes :blocknum -1
                              :starttracknum -1
                              :endtracknum (1+ starttracknum)
                              :startplace 0
                              :endplace (<ra> :get-num-lines blocknum)
                              )
  (map (lambda (tracknotes)
         (map (lambda (note)
                (reverse-note note (- endplace startplace)))
              (reverse tracknotes)))
       (get-notes :blocknum blocknum
                  :starttracknum starttracknum
                  :endtracknum endtracknum
                  :startplace startplace
                  :endplace endplace)))
              

(delafina (get-reversed-block :blocknum -1)
  (get-reversed-notes :blocknum blocknum
                      :starttracknum 0
                      :endtracknum (<ra> :get-num-tracks blocknum)))
                      
(delafina (reverse-block! :blocknum -1)
  (set-notes! :notes (get-reversed-notes :blocknum blocknum)
              :startplace 0
              :starttrack 0
              :blocknum blocknum))
              
(delafina (get-reversed-track :tracknum -1
                              :blocknum -1)
  (get-reversed-notes blocknum tracknum))

(delafina (reverse-track! :tracknum -1
                          :blocknum -1)
  (set-notes! :notes (get-reversed-track :tracknum -1
                                         :blocknum blocknum)
              :startplace 0
              :starttrack tracknum
              :blocknum blocknum))
              
(delafina (get-reversed-range :blocknum -1)
  (get-reversed-notes :blocknum blocknum
                      :starttracknum (<ra> :get-range-start-track)
                      :endtracknum (<ra> :get-range-end-track)
                      :startplace (<ra> :get-range-start-place)
                      :endplace (<ra> :get-range-end-place)))
       
(delafina (reverse-range! :blocknum -1)
  (set-notes! :notes (get-reversed-range :blocknum blocknum)
              :startplace (<ra> :get-range-start-place)
              :starttrack (<ra> :get-range-start-track)
              :blocknum blocknum))
              
  
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


