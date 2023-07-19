
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
  :is-selected #f
  :id -1
  )

#||
(define-struct notes
  :notes
  :subtracknum
  :tracknum
  :blocknum)
||#

(delafina (make-simple-note :place
                            :end-place
                            :pitch 60)                          
  (make-note :place place
             :pitches (list (make-pitch :place 0
                                        :value pitch
                                        :logtype 0
                                        :chance 1)
                            (make-pitch :place (- end-place place)
                                        :value pitch
                                        :logtype 0
                                        :chance 1))
             :velocities (list (make-velocity :place 0
                                              :value 1
                                              :logtype 0)
                               (make-velocity :place (- end-place place)
                                              :value 1
                                              :logtype 0))
             :continues-next-block #t
             :is-selected #f
             :id -1))

             
  
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
  :blocknum
  :use-selection #f
  :selected-notes '())

(delafina (in-editor-area :start-place
                          :end-place #f
                          :area
                          :include-ending-after #t ;; only has meaning if end-place is #t
                          :include-starting-before #t)
  (cond ((area :use-selection)
         #t)
        
        ((and end-place
              (not include-ending-after)
              (> end-place (area :end-place)))
         #f)
        ((and (not include-starting-before)
              (< start-place (area :start-place)))
         #f)
        ((not end-place)
         (and (>= start-place (area :start-place))
              (< start-place (area :end-place))))
        (else
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
  (define start-track (max 0
                           (if (= -1 tracknum)
                               (<ra> :current-track blocknum)
                               tracknum)))
  (assert (< start-track (<ra> :get-num-tracks)))
  (assert (>= start-track 0))
  
  (make-editor-area :start-place 0
                    :end-place (<ra> :get-num-lines blocknum)
                    :start-track start-track
                    :end-track (1+ start-track)
                    :blocknum blocknum))

(define (get-last-note-end-from-notes notes tracknum blocknum)
  (let loop ((last #f)
             (notes notes))
    (if (null? notes)
        last
        (let* ((note (car notes))
               (note-end (<ra> :get-note-end note tracknum blocknum)))
          (loop (if (or (not last)
                        (> note-end last))
                    note-end
                    last)
                (cdr notes))))))
              
         
(delafina (get-ranged-editor-area :blocknum -1)
  (if (<ra> :has-range blocknum)
      (make-editor-area :start-place (<ra> :get-range-start-place blocknum)
                        :end-place (<ra> :get-range-end-place blocknum)
                        :start-track (<ra> :get-range-start-track blocknum)
                        :end-track (<ra> :get-range-end-track blocknum)
                        :blocknum blocknum)
      (let* ((tracknum (<ra> :current-track blocknum))
             (notes (to-list (<ra> :get-selected-notes tracknum blocknum))))
        (make-editor-area :start-place (if (null? notes)
                                           0
                                           (<ra> :get-note-start (car notes) tracknum blocknum))
                          :end-place (if (null? notes)
                                         (<ra> :get-num-lines blocknum)
                                         (get-last-note-end-from-notes notes tracknum blocknum))
                          :start-track tracknum
                          :end-track (+ tracknum 1)
                          :blocknum blocknum
                          :use-selection #t
                          :selected-notes notes))))
      

(delafina (ensure-range-from-selection! :blocknum -1)
  (if (not (<ra> :has-range blocknum))
      (let* ((tracknum (<ra> :current-track blocknum))
             (notes (to-list (<ra> :get-selected-notes tracknum blocknum))))
        ;;(c-display "notes:" notes)
        (if (not (null? notes))
            (<ra> :set-range
                  (<ra> :get-note-start (car notes) tracknum blocknum)
                  (get-last-note-end-from-notes notes tracknum blocknum)
                  tracknum
                  (+ 1 tracknum)
                  blocknum)))))
  
(define (undo-editor-area area)
  (if (area :use-selection)
      (<ra> :undo-notes (area :start-track))
      (undo-block
       (lambda ()
         (for-each (lambda (tracknum)
                     (c-display "Creating undo for track " tracknum)
                     (<ra> :undo-notes tracknum (area :blocknum)))
                   (integer-range (area :start-track) (1- (area :end-track))))))))
             

;;;;;;;;; GET NOTES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-note-velocities blocknum tracknum notenum notestart)
  ;;(c-display "num velocities:" blocknum tracknum notenum (<ra> :get-num-velocities notenum tracknum blocknum))
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

#!!
(<ra> :get-pitch-logtype 0 0 0 0)
(get-note-pitches 0 0 0 0)
(<ra> :get-pianonote-logtype 0 0 0)
(<ra> :get-num-pianonotes 0 0)
!!#

(define (get-note blocknum tracknum noteid startplace)
  (let* ((note-start (<ra> :get-note-start noteid tracknum blocknum))
         (velocities (get-note-velocities blocknum tracknum noteid note-start))
         (pitches (get-note-pitches blocknum tracknum noteid note-start)))
    (make-note :place (- note-start startplace)
               :pitches pitches
               :velocities velocities
               :continues-next-block (<ra> :note-continues-next-block noteid tracknum blocknum)
               :is-selected (<ra> :note-is-selected noteid tracknum blocknum)
               :id (if (integer? noteid)
                       (<ra> :get-note-id noteid tracknum blocknum)
                       noteid))))

(define (transpose-pitch pitch pitch-delta)
  (<copy-pitch> pitch
                :value (if (< (pitch :value) 0.001)
                           0
                           (between 0.002 (+ (pitch :value) pitch-delta) 127))))

(define (transpose-note note pitch-delta place-delta)
  (<copy-note> note
               :place (+ (note :place) place-delta)
               :pitches (map (lambda (pitch)
                               (transpose-pitch pitch pitch-delta))
                             (note :pitches))))
  
(delafina (get-selected-pianonotes :tracknum -1
                                   :blocknum -1
                                   :note-ids #f) 
  (if (not note-ids)
      (set! note-ids (<ra> :get-selected-notes tracknum blocknum)))
 
  (set! note-ids (to-list note-ids))

  (if (null? note-ids)
      '()
      (let ((startplace (<ra> :get-note-start (car note-ids) tracknum blocknum)))
        (map (lambda (note-id)
               (get-note blocknum tracknum note-id startplace))
             note-ids))))
  

(delafina (get-area-notes :area
                          :include-ending-after #t
                          :include-starting-before #t
                          )  
  (define blocknum (area :blocknum))
  (define startplace (area :start-place))
  (define endplace (area :end-place))
  
  (if (area :use-selection)
      (list
       (get-selected-pianonotes (area :start-track) blocknum (area :selected-notes)))
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
                                                  (<ra> :get-note-end notenum tracknum blocknum)
                                                  :area area
                                                  :include-ending-after include-ending-after
                                                  :include-starting-before include-starting-before)
                                  (cons (get-note blocknum tracknum notenum startplace)
                                        (loop (1+ notenum))))
                                 (else
                                  (loop (1+ notenum))))))))))
           (integer-range (area :start-track)
                          (1- (area :end-track))))))
           

(delafina (get-ranged-notes :blocknum -1)
  (get-area-notes (get-ranged-editor-area blocknum)))


(define (map-area-notes area-notes func)
  ;;(c-display "AREA NOTES:")
  ;;(pretty-print area-notes)
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


(define (cut-pitchvelocity-keep-end pv place portamento-enabled)
  ;;(c-display "place:" place)
  (let loop ((pv pv))
    ;;(c-display "pv:" (pp pv))
    (if (or (null? pv)
            (null? (cdr pv)))
        '()
        (let* ((pv1 (car pv))
               (pv2 (cadr pv))
               (place1 (pv1 :place))
               (place2 (pv2 :place)))
          (cond ((= place place1)
                 pv)
                ((= place place2)
                 (if (null? (cddr pv))
                     '()
                     (cdr pv)))
                ((< place place2)
                 (let ((logtype (pv1 :logtype))
                       (value1 (pv1 :value))
                       (value2 (pv2 :value)))
                   (let ((value (if (or (logtype-holding? logtype)
                                        (not portamento-enabled))
                                    value1
                                    (scale place place1 place2 value1 value2))))
                     (cons (copy-hash pv1
                                      :value value
                                      :place place)
                           (cdr pv)))))
                (else
                 (loop (cdr pv))))))))


(***assert*** (cut-pitchvelocity-keep-end (list (make-velocity :place 0 :value 0 :logtype (<ra> :get-logtype-hold))
                                                (make-velocity :place 1 :value 0 :logtype (<ra> :get-logtype-hold)))
                                          0 #t)
              (list (make-velocity :place 0 :value 0 :logtype (<ra> :get-logtype-hold))
                    (make-velocity :place 1 :value 0 :logtype (<ra> :get-logtype-hold))))

(***assert*** (cut-pitchvelocity-keep-end (list (make-velocity :place 0 :value 0 :logtype (<ra> :get-logtype-hold))
                                                (make-velocity :place 1 :value 1 :logtype (<ra> :get-logtype-hold)))
                                          1 #t)
              '())

(***assert*** (cut-pitchvelocity-keep-end (list (make-velocity :place 0 :value 0 :logtype (<ra> :get-logtype-hold))
                                                (make-velocity :place 2 :value 2 :logtype (<ra> :get-logtype-hold)))
                                          1 #t)
              (list (make-velocity :place 1 :value 0 :logtype (<ra> :get-logtype-hold))
                    (make-velocity :place 2 :value 2 :logtype (<ra> :get-logtype-hold))))

(***assert*** (cut-pitchvelocity-keep-end (list (make-velocity :place 0 :value 0 :logtype (<ra> :get-logtype-linear))
                                                (make-velocity :place 2 :value 2 :logtype (<ra> :get-logtype-linear)))
                                          1 #t)
              (list (make-velocity :place 1 :value 1 :logtype (<ra> :get-logtype-linear))
                    (make-velocity :place 2 :value 2 :logtype (<ra> :get-logtype-linear))))

(***assert*** (cut-pitchvelocity-keep-end (list (make-velocity :place 0 :value 0 :logtype (<ra> :get-logtype-linear))
                                                (make-velocity :place 2 :value 2 :logtype (<ra> :get-logtype-linear))
                                                (make-velocity :place 4 :value 9 :logtype (<ra> :get-logtype-linear)))
                                          1 #t)
              (list (make-velocity :place 1 :value 1 :logtype (<ra> :get-logtype-linear))
                    (make-velocity :place 2 :value 2 :logtype (<ra> :get-logtype-linear))
                    (make-velocity :place 4 :value 9 :logtype (<ra> :get-logtype-linear))))

(***assert*** (cut-pitchvelocity-keep-end (list (make-velocity :place 0 :value 0 :logtype (<ra> :get-logtype-linear))
                                                (make-velocity :place 2 :value 2 :logtype (<ra> :get-logtype-linear))
                                                (make-velocity :place 4 :value 9 :logtype (<ra> :get-logtype-linear)))
                                          2.5 #t)
              (list (make-velocity :place 2.5 :value (scale 2.5 2 4 2 9) :logtype (<ra> :get-logtype-linear))
                    (make-velocity :place 4 :value 9 :logtype (<ra> :get-logtype-linear))))


#!!
(pretty-print (cut-pitchvelocity-keep-end (list (make-velocity :place 0 :value 0 :logtype (<ra> :get-logtype-linear))
                                                (make-velocity :place 2 :value 2 :logtype (<ra> :get-logtype-linear))
                                                (make-velocity :place 4 :value 9 :logtype (<ra> :get-logtype-linear)))
                                          2.5 #t))
!!#


;; Note: this function might return a note without pitches or velocities.
(define (cut-note-keep-end note place)
  ;;(c-display "Note place2:" (note :place) ". place:" place)
  (if (>= (note :place) place)
      (<copy-note> note
                   :pitches '()
                   :velocities '())
      (begin
        (define pitches (note :pitches))
        (define velocities (note :velocities))
        (define cut-place (- place (note :place)))
        
        (define portamento-enabled (or (> (length pitches) 2)
                                       (> (pitches 1 :value) 0)))
        
        (define (skew-pvs pvs)
          (map (lambda (cutted-pv)
                 (copy-hash cutted-pv :place (- (cutted-pv :place) cut-place)))
               pvs))
        
        ;;(c-display "PLACE: " place ". Note-place:" (note :place) ". Cut-place:" cut-place ". Portamento-enabled:" portamento-enabled)
        ;;(c-display "velocities:\n" (pp velocities))
        ;;(c-display "\nvelocities cut:\n" (pp (cut-pitchvelocity-keep-end velocities cut-place #t)))
        ;;(c-display "\nvelocities skew:\n" (pp (skew-pvs (cut-pitchvelocity-keep-end velocities cut-place #t))))
        
        (<copy-note> note
                     :place place
                     :pitches (skew-pvs (cut-pitchvelocity-keep-end pitches cut-place portamento-enabled))
                     :velocities (skew-pvs (cut-pitchvelocity-keep-end velocities cut-place #t))
                     :continues-next-block #f))))



(define (cut-pitchvelocity-keep-start pv place portamento-enabled)
  (let loop ((pv pv))
    (if (or (null? pv)
            (null? (cdr pv)))
        pv
        (let* ((pv1 (car pv))
               (pv2 (cadr pv))
               (place1 (pv1 :place))
               (place2 (pv2 :place)))
          (cond ((>= place1 place)
                 '())
                ((< place2 place)
                 (cons pv1
                       (loop (cdr pv))))
                ((= place2 place)
                 (list pv1 pv2))
                ((> place2 place)
                 (let ((logtype (pv1 :logtype))
                       (value1 (pv1 :value))
                       (value2 (pv2 :value)))
                   (let ((value (cond ((not portamento-enabled)
                                       0)
                                      ((logtype-holding? logtype)
                                       value1)
                                      (else
                                       (scale place place1 place2 value1 value2)))))
                     (list pv1
                           (copy-hash pv2
                                      :value value
                                      :place place)))))
                (else
                 (error 'internal-error-in-cut-pitchvelocity-keep-start (<-displayable-> "pv1:" pv1 ", pv2: " pv2 ", place1: " place1 ", place2: " place2 ", place: "place))))))))

(***assert*** (cut-pitchvelocity-keep-start (list (make-velocity :place 1 :value 0 :logtype (<ra> :get-logtype-hold))
                                                  (make-velocity :place 2 :value 0 :logtype (<ra> :get-logtype-hold)))
                                            0 #t)
              '())
(***assert*** (cut-pitchvelocity-keep-start (list (make-velocity :place 0 :value 0 :logtype (<ra> :get-logtype-hold))
                                                  (make-velocity :place 1 :value 0 :logtype (<ra> :get-logtype-hold)))
                                            0 #t)
              '())
(let ((l (list (make-velocity :place 0 :value 0 :logtype (<ra> :get-logtype-hold))
               (make-velocity :place 1 :value 1 :logtype (<ra> :get-logtype-hold)))))
  (***assert*** (cut-pitchvelocity-keep-start l
                                              1 #t)
                l))
(***assert*** (cut-pitchvelocity-keep-start (list (make-velocity :place 0 :value 0 :logtype (<ra> :get-logtype-hold))
                                                  (make-velocity :place 1 :value 2 :logtype (<ra> :get-logtype-hold)))
                                            1 #t)
              (list (make-velocity :place 0 :value 0 :logtype (<ra> :get-logtype-hold))
                    (make-velocity :place 1 :value 2 :logtype (<ra> :get-logtype-hold))))

(***assert*** (cut-pitchvelocity-keep-start (list (make-velocity :place 0 :value 0 :logtype (<ra> :get-logtype-linear))
                                                  (make-velocity :place 2 :value 2 :logtype (<ra> :get-logtype-linear)))
                                            1 #t)
              (list (make-velocity :place 0 :value 0 :logtype (<ra> :get-logtype-linear))
                    (make-velocity :place 1 :value 1 :logtype (<ra> :get-logtype-linear))))


;; Note: this function might return a note without pitches or velocities.
(define (cut-note-keep-start note place)
  ;;(c-display "Note place:" (note :place) ". place:" place)
  (if (>= (note :place) place)
      (<copy-note> note
                   :pitches '()
                   :velocities '())
      (begin
        (define pitches (note :pitches))
        (define velocities (note :velocities))
        (define dplace (- place (note :place)))
        
        (define portamento-enabled (or (> (length pitches) 2)
                                       (> (pitches 1 :value) 0)))
        
        (<copy-note> note
                     :pitches (cut-pitchvelocity-keep-start pitches dplace portamento-enabled)
                     :velocities (cut-pitchvelocity-keep-start velocities dplace #t)
                     :continues-next-block #f))))

#||
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
                 (loop (cons (cut-note-end-at note place)
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

||#

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

(define (set-new-note-end note new-length)
  ;;(c-display "Pitches:" (length (note :pitches)) (length (note :velocities)))
  (define old-length ((last (note :velocities)) :place))
  ;;(c-display "start:" (* 1.0 (note :place)) ". old-length: " (* 1.0 old-length) ". new-length:" (* 1.0 new-length))
  (cond (#f
         note)
        ((< new-length old-length)
         (cut-note-keep-start note (+ (note :place) new-length)))
        ((> new-length old-length)
         (<copy-note> note
                      :pitches (append (butlast (note :pitches))
                                       (list
                                        (<copy-pitch> (last (note :pitches))
                                                      :place new-length)))
                      :velocities (append (butlast (note :velocities))
                                          (list
                                           (<copy-velocity> (last (note :velocities))
                                                            :place new-length)))))
        (else
         note)))


;;;;;;;;; SET NOTES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; returns #f if note was not added, if not returns note-id.
(define (add-note! note tracknum blocknum)

  ;;(pretty-print note)

  (define num-lines (<ra> :get-num-lines blocknum))

  (if (>= (note :place) num-lines)
      #f
      (update-notes-after-block
       (lambda ()  
         
         ;; add note
         (define note-id (<ra> :add-note
                               (note :pitches 0 :value)
                               (note :velocities 0 :value)
                               (note :place)
                               (i-min num-lines (get-note-end note))
                               tracknum
                               blocknum))
         
         (if (note :is-selected)
             (<ra> :select-note note-id tracknum blocknum))
         
         ;; add pitches
         ;;
         ;; logtype of first pitch
         (<ra> :set-pitch-logtype ((car (note :pitches)) :logtype) 0 note-id tracknum blocknum)
         
         ;; end-pitch. TODO: end-pitch is not correct if end-place is >= num_lines.
         (let ((pitch (last (note :pitches))))
           (if (not (= 0 (pitch :value)))
               (<ra> :set-pitch
                     (pitch :value)
                     'same-place
                     1  ;; (1==current end-pitch num)
                     note-id tracknum blocknum)))
         
         (for-each (lambda (pitch)
                     (define place (+ (pitch :place)
                                      (note :place)))
                     ;;(c-display "PLACE:" place ". note-place:" (note :place) ". pitch-place:" (pitch :place) ". note-id:" note-id ". note-pitches:" (pp (note :pitches)))
                     (if (< place num-lines)
                         (let ((pitchnum (<ra> :add-pitch
                                               (pitch :value)
                                               place
                                               note-id tracknum blocknum)))
                           ;;(c-display "PITCHNUM:" pitchnum ". place:" place ". note-id:" note-id ". num notes:" (<ra> :get-num-notes tracknum blocknum))
                           (<ra> :set-pitch-logtype (pitch :logtype) pitchnum note-id tracknum blocknum))))
                   (cdr (butlast (note :pitches))))
         
         ;; add velocities
         ;;
         ;; logtype of first velocity.
         (<ra> :set-velocity-logtype ((car (note :velocities)) :logtype) 0 note-id tracknum blocknum)
         
         ;; end-velocity. TODO: end-velocity is not correct if end-place is >= num_lines.
         (let ((velocity (last (note :velocities))))
           (<ra> :set-velocity
                 (velocity :value)
                 'same-place
                 1 ;; (1==current end-velocity num)
                 note-id tracknum blocknum))
         
         (for-each (lambda (velocity)
                     (define place (+ (velocity :place)
                                      (note :place)))
                     (if (< place num-lines)
                         (let ((velocitynum (<ra> :add-velocity
                                                  (velocity :value)
                                                  place
                                                  note-id tracknum blocknum)))
                           ;;(c-display "place/value:" place (velocity :value) velocitynum)
                           (<ra> :set-velocity-logtype (velocity :logtype) velocitynum note-id tracknum blocknum))))
                   (cdr (butlast (note :velocities))))
         
         note-id))))
  

(define (add-notes! area-notes area)
  (define blocknum (area :blocknum))
  (define startplace (area :start-place))
  (define endplace (area :end-place))
  (define starttrack (area :start-track))
  (update-notes-after-block
   (lambda ()  
     (for-each (lambda (addtracknum track-notes)
                 (let ((tracknum (+ starttrack addtracknum)))
                   (if (< tracknum (<ra> :get-num-tracks blocknum)) ;; Need this test since the 'area-notes' doesn't have to be created from 'area'.
                       (for-each (lambda (note)
                                   (let ((place (+ startplace (note :place))))
                                     (if (or (area :use-selection)
                                             (< place endplace))
                                         (add-note! (<copy-note> note
                                                                 :place place)
                                                    tracknum
                                                    blocknum))))
                                 track-notes))))
               (iota (length area-notes))
               area-notes))))

(delafina (remove-notes! :area
                         :include-ending-after #t
                         :include-starting-before #t)
  (define blocknum (area :blocknum))
  (define startplace (area :start-place))
  (define endplace (area :end-place))
  (update-notes-after-block
   (lambda ()
     (if (area :use-selection)
         (for-each (lambda (note-id)
                     (<ra> :delete-note note-id (area :start-track) blocknum))
                   (area :selected-notes))
         (let loop ((tracknum (area :start-track))
                    (notenum 0))
           (assert (<= (area :end-track)
              (<ra> :get-num-tracks blocknum)))
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
                                              (<ra> :get-note-end notenum tracknum blocknum)
                                              :area area
                                              :include-ending-after include-ending-after
                                              :include-starting-before include-starting-before)
                              (<ra> :delete-note notenum tracknum blocknum)
                              (loop tracknum
                                    notenum))
                             (else
                              (loop tracknum
                                    (1+ notenum)))))))))))))
#||
(remove-notes! 7 9 0 1)

||#


(delafina (replace-notes! :area-notes
                          :area
                          :include-ending-after #t
                          :include-starting-before #t)
  (update-notes-after-block
   (lambda ()
     (remove-notes! area
                    :include-ending-after include-ending-after
                    :include-starting-before include-starting-before)
     (add-notes! area-notes
                 area))))

#||
(let ((notes (get-notes :starttracknum 0)))
  (c-display (length ((caar notes) :pitches)))
  (for-each c-display ((caar notes) :pitches))
  (replace-notes! notes :starttrack 1))

||#


;;;;;;;;; PB ERASE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#!!
(define (get-all-note-pitch-ranges note)
  (let loop ((curr-pitch (car (note :pitches)))
             (pitches (cdr (note :pitches))))
    (if (null? pitches)
        '()
        (let ((next-pitch (car pitches)))
          (cons (list (curr-pitch :value)
                      (next-pitch :value))
                (loop next-pitch
                      (cdr pitches)))))))
!!#


(define (is-note-inside-pitch-range? note eraser_place1 eraser_place2 eraser_pitch1 eraser_pitch2)
  (define pitches (note :pitches))
  (define portamento-enabled (or (> (length pitches) 2)
                                 (> (pitches 1 :value) 0)))
  (define note-start (note :place))
  (let loop ((pitches pitches))
    (if (or (null? pitches)
            (null? (cdr pitches)))
        #f
        (let* ((pitch1 (car pitches))
               (pitch2 (cadr pitches))
               (a_place1 (+ note-start (pitch1 :place)))
               (a_place2 (+ note-start (pitch2 :place))))
          (cond ((>= a_place1 eraser_place2)
                 #f)
                ((< a_place2 eraser_place1)
                 (loop (cdr pitches)))
                (else
                 (let* ((a_pitch1 (pitch1 :value))
                        (a_pitch2 (if (or (logtype-holding? (pitch1 :logtype))
                                          (not portamento-enabled))
                                      a_pitch1
                                      (pitch2 :value))))
                   ;;(c-display "hold?" (logtype-holding? (pitch1 :logtype)) (not portamento-enabled) ". note pitch 1/2:" a_pitch1 a_pitch2 ". endpitch:" (pitch2 :value))
                   (rectangle-intersects-with-parallelogram eraser_pitch1 eraser_place1
                                                            eraser_pitch2 eraser_place2
                                                            
                                                            a_pitch1 a_place1
                                                            a_pitch2 a_place2
                                                            1))))))))
                                                            
#||                                      
  (any? (lambda (hepp)
          (define pitch1 (car hepp))
          (define pitch2 (cadr hepp))
          (or (and (>= pitch1 startnote) ;; pitch1 in range
                   (< pitch1 endnote))
              (and (>= pitch2 startnote) ;; pitch2 in range
                   (< pitch2 endnote))
              (and (> pitch2 0)
                   (<= pitch1 startnote) ;; A point in pitch1->pitch2 is in range
                   (>= pitch2 endnote))
              (and (> pitch2 0)
                   (<= pitch2 startnote) ;; A point in pitch2->pitch1 is in range
                   (>= pitch1 endnote))))
        (get-all-note-pitch-ranges note)))
||#

(define (is-note-inside-place-range? note place1 place2)
  (cond ((< (get-note-end note) place1)
         #f)
        ((>= (note :place) place2)
         #f)
        (else
         #t)))

#||
(define (remove-notes-outside-pitch-range notes startpitch endpitch)
  (keep (lambda (note)
          (is-note-inside-pitch-range? note startpitch endpitch))
        notes))
||#
             
(define (pr-erase-split-note note startsplit endsplit)
  ;;(c-display "START/END-split:" startsplit endsplit)
  (define note1 (cut-note-keep-start note startsplit))
  (define note2 (cut-note-keep-end note endsplit))
  (keep (lambda (note)
          (define has-pitches (not (null? (note :pitches))))
          (define has-velocities (not (null? (note :velocities))))
          (assert (or (and has-pitches has-velocities)
                      (and (not has-pitches) (not has-velocities))))
          has-pitches)
        (list note1 note2)))
        

(define (pr-erase! blocknum tracknum startnote endnote startsplit endsplit make-undo)
  (define area (get-track-editor-area tracknum blocknum))
  (define notes (car (get-area-notes :area area)))
  ;;(c-display " old notes:\n" (pp notes))

  (define do-erase-something #f)
  
  (define new-notes
    (let loop ((notes notes))
      (if (null? notes)
          '()
          (let ((note (car notes)))
            (if (and (is-note-inside-place-range? note startsplit endsplit)
                     (is-note-inside-pitch-range? note startsplit endsplit startnote endnote))
                (begin
                  (set! do-erase-something #t)
                  (append (pr-erase-split-note note startsplit endsplit)
                          (loop (cdr notes))))
                (cons note
                      (loop (cdr notes))))))))

  (define (apply-changes)
    (<ra> :undo-notes tracknum)
    (replace-notes! :area-notes (list new-notes)
                    :area area))

  (if do-erase-something
      (if make-undo
          (undo-block apply-changes)
          (ignore-undo-block apply-changes)))
           
  do-erase-something)


#!!
(pr-erase! 0 0 0 128 5 6)
!!#



;;;;;;;;; TESTING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reverse-velocity velocity length*)
  (<copy-velocity> velocity
                   :place (scale (velocity :place) 0 length* length* 0)))

(define (reverse-velocities velocities length*)
  (map (lambda (velocity)
         (reverse-velocity velocity length*))
       (reverse velocities)))

(define (reverse-pitch pitch length*)
  (<copy-pitch> pitch
                :place (scale (pitch :place) 0 length* length* 0)))

(define (reverse-pitches pitches length*)
  (define first-pitch (reverse-pitch (first pitches) length*))
  (define last-pitch (reverse-pitch (last pitches) length*))
  
  ;;(c-display "bef first-pitch:" first-pitch length)
  ;;(c-display "bef last-pitch:" last-pitch length)

  (when (= 0 (last-pitch :value))
    (set! last-pitch  (<copy-pitch> last-pitch  :value (first-pitch :value)))
    (set! first-pitch (<copy-pitch> first-pitch :value 0)))

  ;;(c-display "aft first-pitch:" first-pitch)
  ;;(c-display "aft last-pitch:" last-pitch)

  (append (list last-pitch)
          (map (lambda (pitch)
                 (reverse-pitch pitch length*))
               (reverse (cdr (butlast pitches))))
          (list first-pitch)))

(define (reverse-note uncut-note length*)
  (let* ((note (if (> (+ (uncut-note :place)
                         (get-note-duration uncut-note))
                      length*)
                   (cut-note-keep-start uncut-note length*)
                   uncut-note))
         (duration (get-note-duration note)))
    ;;(pretty-print (list ":dur" duration
    ;;                    ":note" note))
    (<copy-note> note
                 :place (scale (get-note-end note) 0 length* length* 0)
                 :pitches (reverse-pitches (note :pitches) duration)
                 :velocities (reverse-velocities (note :velocities) duration)
                 )))

(define (get-reversed-notes area)
  (define area-length (- (area :end-place)
                         (area :start-place)))
  (map (lambda (tracknotes)
         (map (lambda (note)
                (if (>= (note :place) 0)
                    (reverse-note note area-length)
                    note))
              (reverse tracknotes)))
       (get-area-notes area)))
              

(define (reverse-area! area)
  (undo-editor-area area)
  (replace-notes! (get-reversed-notes area)
                  area))
  
(delafina (FROM_C-reverse-block! :blocknum -1)
  (reverse-area! (get-block-editor-area blocknum)))
              
(delafina (FROM_C-reverse-track! :tracknum -1
                          :blocknum -1)
  (reverse-area! (get-track-editor-area tracknum blocknum)))

(delafina (FROM_C-reverse-range! :blocknum -1)
  (reverse-area! (get-ranged-editor-area blocknum)))

#!!

(c-display "GAKK")

(pretty-print (get-area-notes (get-ranged-editor-area)))

(FROM_C-reverse-range!)

(pretty-print (cut-note-keep-start (caar (get-area-notes (get-ranged-editor-area -1)))
                                   6))

(pretty-print (get-area-notes (get-ranged-editor-area -1)))

(pretty-print (get-reversed-notes (get-ranged-editor-area -1)))


(pretty-print (get-reversed-track -1 0))

(FROM_C-reverse-track!)
(FROM_C-reverse-block!)

(let ((tracknum 0))
  (set-notes! (reverse-track 0 tracknum) 0 tracknum))


(load "notes.scm")

(begin *load-path*)

(<ra> :delete-note 1)

!!#



;;;;;;;;; Piano roll selected notes clipboard ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *selected-pianonotes* '())

(delafina (get-note-ids-for-mouse-to-operate-on :tracknum -1
                                                :blocknum -1)
  (let ((current-note (<ra> :get-current-pianonote tracknum)))
    '(c-display "curr-note/selected:"
                current-note
                (string? current-note)
                (integer? current-note)
                (and #f current-note
                     (<ra> :note-is-selected current-note tracknum)))
    (if (and current-note
             (not (<ra> :note-is-selected current-note tracknum)))
        (list current-note)
        (to-list (<ra> :get-selected-notes tracknum blocknum)))))

(delafina (FROM_C-copy-selected-pianonotes :tracknum -1
                                           :blocknum -1
                                           :note-ids #f)
  (set! note-ids (or note-ids
                     (get-note-ids-for-mouse-to-operate-on tracknum blocknum)))

  (when (and note-ids
             (not (null? note-ids)))
    (set! *selected-pianonotes* (get-selected-pianonotes tracknum
                                                         blocknum
                                                         note-ids))))


(delafina (FROM_C-delete-selected-pianonotes! :tracknum -1
                                              :blocknum -1
                                              :note-ids #f)
  (set! note-ids (or note-ids
                     (get-note-ids-for-mouse-to-operate-on tracknum blocknum)))
  (when (and note-ids
             (not (null? note-ids)))
    (<ra> :undo-notes)
    (for-each (lambda (note-id)
                (<ra> :delete-note note-id tracknum blocknum))
              note-ids)))

(delafina (FROM_C-cut-selected-pianonotes! :tracknum -1
                                           :blocknum -1
                                           :note-ids #f)
  (set! note-ids (or note-ids
                     (get-note-ids-for-mouse-to-operate-on tracknum blocknum)))
  (FROM_C-copy-selected-pianonotes tracknum blocknum note-ids)
  (FROM_C-delete-selected-pianonotes! tracknum blocknum note-ids))


(delafina (FROM_C-paste-pianonotes! :pitch (<ra> :get-current-piano-ghost-note-value)
                                    :startplace (<ra> :get-current-piano-ghost-note-start)
                                    :tracknum (<ra> :get-current-piano-ghost-note-tracknum)
                                    :blocknum -1)
  (when (and (not (null? *selected-pianonotes*))
             (>= tracknum 0))
    (<ra> :undo-notes tracknum blocknum)
    (<ra> :unselect-all-notes tracknum blocknum)
    (define pitch1 (*selected-pianonotes* 0 :pitches 0 :value))
    (define pitch-delta (- pitch pitch1))
    (define gotit #f)
    (update-notes-after-block
     (lambda ()  
       (for-each (lambda (note)
                   (if (add-note! (transpose-note note
                                                  pitch-delta
                                                  startplace)
                                  tracknum
                                  blocknum)
                       (set! gotit #t)))
                 *selected-pianonotes*)))
    
    (<declare-variable> highlight-piano-note-under-mouse) ;; in mouse.scm
    
    (if (not gotit)
        (<ra> :cancel-last-undo)
        (if (highlight-piano-note-under-mouse (<ra> :get-mouse-pointer-x)
                                              (<ra> :get-mouse-pointer-y))
            (<ra> :cancel-current-piano-ghost-note)))))


#!!
(FROM_C-copy-selected-pianonotes)
(pretty-print *selected-pianonotes*)
(FROM_C-paste-pianonotes! 2 16 0)
(FROM_C-cut-selected-pianonotes!)
(<ra> :get-selected-notes -1 -1)
!!#
