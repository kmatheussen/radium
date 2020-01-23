(provide 'fxrange.scm)


;; fxs format:
'((fxname1 ((place1 value1 logtype) ;; Note that the fxname might clash when merging, different type of fx with the same name. This should not be a big problem though (it could even be a feature), since all values are between 0 and 1.
            (place2 value2 logtype)
            ...))
  (fxname2 ...)
  ...)

(define (logtype-holding? logtype)
  (= (<ra> :get-logtype-hold) logtype))

(define (scale-logtype logtype x x1 x2 y1 y2)
  (if (logtype-holding? logtype)
      y1
      (scale x x1 x2 y1 y2)))

(define-struct fx
  :name
  :instrument-id
  :nodes)


(define-struct fxnode
  :place
  :value
  :logtype)

#||
(define (create-fxnode place value logtype)
  (list place value logtype))

(define fxnode-place car)
(define fxnode-value cadr)
(define fxnode-logtype caddr)

(define (fxnode-replace-place fxnode new-place)
  (make-fxnode new-place
               (fxnode-value fxnode)
               (fxnode-logtype fxnode)))
  
(define (fxnode-replace-logtype fxnode new-logtype)
  (make-fxnode (fxnode-place fxnode)
                 (fxnode-value fxnode)
                 new-logtype))
  

||#

(define (fxnode-is-holding? fxnode)
  (logtype-holding? (fxnode :logtype)))


(define (lists->fxnodes lists)
  (map (lambda (l)
         (apply make-fxnode l))
       lists))
(define (fxnodes->lists fxnodes)
  (map (lambda (fxnode)
         (list (fxnode :place) (fxnode :value) (fxnode :logtype)))
       fxnodes))


(define (find-fx fxs fxname)
  (let loop ((fxs fxs))
    (if (null? fxs)
        #f
        (let ((fx (car fxs)))
          (if (eq? fxname (fx :name))
              fx
              (loop (cdr fxs)))))))

(assq 'b '((a 9)))

(define (remove-fx fxs name)
  (if (null? fxs)
      '()
      (let ((fx (car fxs)))
        (if (eq? (fx :name) name)
            (cdr fxs)
            (cons fx
                  (remove-fx (cdr fxs) name))))))

;; A list of fxs, one fxs for each track
(define *clipboard-fxs* '())

      

(define (scissor-fxnodes-keep-outside fxnodes startplace endplace)
  (assert (> endplace startplace))
  
  (let loop ((fxnodes fxnodes)
             (last-fxnode (<optional-hash-table>)))
    ;;(c-display "fxnodes:" fxnodes)
    ;;(c-display "last:" last-fxnode)
    ;;(c-display)
    (if (null? fxnodes)
        '()
        (let* ((fxnode (let ((a (car fxnodes)))
                         (if (list? a) ;; Happens in testing. Code is much clearer when we can write '((1 10 0)(2 11 0)...) instead of the alternative.
                             (apply make-fxnode a)
                             a)))
               (place (fxnode :place))
               (last-place (and last-fxnode
                                (last-fxnode :place))))
          
          (if last-place
              (assert (< last-place place)))
          
          (cond ((< place startplace)
                 (cons fxnode
                       (loop (cdr fxnodes)
                             fxnode)))
                
                ((= place startplace)
                 (cons (<copy-fxnode> fxnode :logtype (<ra> :get-logtype-hold))
                       (loop (cdr fxnodes)
                             fxnode)))

                ((and last-place
                      (> place endplace)
                      (< last-place startplace))
                 (let ((first (make-fxnode startplace
                                           (scale-logtype (last-fxnode :logtype)
                                                          startplace
                                                          last-place place
                                                          (last-fxnode :value) (fxnode :value))
                                           (<ra> :get-logtype-hold)))
                       (second (make-fxnode endplace
                                            (scale-logtype (last-fxnode :logtype)
                                                           endplace
                                                           last-place place
                                                           (last-fxnode :value) (fxnode :value))
                                            (last-fxnode :logtype))))
                   (append (list first second)
                           (loop fxnodes
                                 second))))
                
                ((and last-place
                      (> place      startplace)
                      (< last-place startplace))
                 (let ((startplace-fxnode (make-fxnode startplace
                                                       (scale-logtype (last-fxnode :logtype)
                                                                      startplace
                                                                      last-place place
                                                                      (last-fxnode :value) (fxnode :value))
                                                       (<ra> :get-logtype-hold))))
                   (cons startplace-fxnode
                         (loop fxnodes
                               startplace-fxnode))))

                ((and (> place startplace)
                      (< place endplace))
                 (loop (cdr fxnodes)
                       fxnode))

                ((and last-place
                      (> place endplace)
                      (< last-place endplace))
                 (let ((endplace-fxnode (make-fxnode endplace
                                                     (scale-logtype (last-fxnode :logtype)
                                                                    endplace
                                                                    last-place place
                                                                    (last-fxnode :value) (fxnode :value))
                                                     (last-fxnode :logtype))))
                   (cons endplace-fxnode
                         (loop fxnodes
                               endplace-fxnode))))

                (else
                 (cons fxnode
                       (loop (cdr fxnodes)
                             fxnode))))))))

;; nothing
(***assert*** (scissor-fxnodes-keep-outside (lists->fxnodes '())
                                            5 8)
              (lists->fxnodes '()))

;; only before
(***assert*** (scissor-fxnodes-keep-outside (lists->fxnodes '((1 10 0)
                                                              (2 11 0)))
                                            5 8)
              (lists->fxnodes '((1 10 0)
                                (2 11 0))))

;; only after
(***assert*** (scissor-fxnodes-keep-outside (lists->fxnodes '((10 10 0)
                                                             (20 11 0)))
                                            5 8)
              (lists->fxnodes '((10 10 0)
                               (20 11 0))))

;; On first line
(***assert*** (scissor-fxnodes-keep-outside (lists->fxnodes '((1 10 0)
                                                             (2 11 0)
                                                             (5 12 0)))
                                            5 8)
              (lists->fxnodes `((1 10 0)
                               (2 11 0)
                               (5 12 ,(<ra> :get-logtype-hold)))))

;; On last line
(***assert*** (scissor-fxnodes-keep-outside (lists->fxnodes '((8 10 0)
                                                             (9 11 0)
                                                             (10 12 0)))
                                            5 8)
              (lists->fxnodes '((8 10 0)
                               (9 11 0)
                               (10 12 0))))

;; Before and inside, and on first line
(***assert*** (scissor-fxnodes-keep-outside (lists->fxnodes '((1 10 0)
                                                             (2 11 0)
                                                             (5 12 0)
                                                             (6 13 0)))
                                            5 8)
              (lists->fxnodes `((1 10 0)
                               (2 11 0)
                               (5 12 ,(<ra> :get-logtype-hold)))))


;; Before and inside
(***assert*** (scissor-fxnodes-keep-outside (lists->fxnodes '((1 10 0)
                                                             (2 11 0)
                                                             (6 13 0)))
                                            5 8)
              (lists->fxnodes `((1 10 0)
                               (2 11 0)
                               (5 ,(scale 5 2 6 11 13) ,(<ra> :get-logtype-hold)))))


;; Before(hold) and inside
(***assert*** (scissor-fxnodes-keep-outside (lists->fxnodes `((2 11 ,(<ra> :get-logtype-hold))
                                                             (6 13 0)))
                                            5 8)
              (lists->fxnodes `((2 11 ,(<ra> :get-logtype-hold))
                               (5 11 ,(<ra> :get-logtype-hold)))))

;; On first line and inside
(***assert*** (scissor-fxnodes-keep-outside (lists->fxnodes '((5 11 0)
                                                             (6 13 0)))
                                            5 8)
              (lists->fxnodes `((5 11 ,(<ra> :get-logtype-hold)))))

;; Inside
(***assert*** (scissor-fxnodes-keep-outside (lists->fxnodes '((6 11 0)
                                                             (7 13 0)))
                                            5 8)
              '())

;; Inside and last line
(***assert*** (scissor-fxnodes-keep-outside (lists->fxnodes '((6 11 0)
                                                             (8 13 0)))
                                            5 8)
              (lists->fxnodes '((8 13 0))))


;; Inside and last line and after
(***assert*** (scissor-fxnodes-keep-outside (lists->fxnodes '((6 11 0)
                                                             (8 13 0)
                                                             (9 14 0)))
                                            5 8)
              (lists->fxnodes '((8 13 0)
                               (9 14 0))))

;; Inside and after
(***assert*** (scissor-fxnodes-keep-outside (lists->fxnodes '((6 11 0)
                                                             (9 14 0)))
                                            5 8)
              (lists->fxnodes `((8 ,(scale 8 6 9 11 14) 0)
                               (9 14 0))))

;; Inside(hold) and after
(***assert*** (scissor-fxnodes-keep-outside (lists->fxnodes `((6 11 ,(<ra> :get-logtype-hold))
                                                             (9 14 0)))
                                            5 8)
              (lists->fxnodes `((8 11 ,(<ra> :get-logtype-hold))
                               (9 14 0))))

;; Before, on first line, inside, on last line, after
(***assert*** (scissor-fxnodes-keep-outside (lists->fxnodes '((4 11 0)
                                                             (5 12 0)
                                                             (6 13 0)
                                                             (8 14 0)
                                                             (9 15 0)))
                                            5 8)
              (lists->fxnodes `((4 11 0)
                               (5 12 ,(<ra> :get-logtype-hold))
                               (8 14 0)
                               (9 15 0))))


;; Before, inside, after
(***assert*** (scissor-fxnodes-keep-outside (lists->fxnodes '((4 11 0)
                                                             (6 13 0)
                                                             (9 15 0)))
                                            5 8)
              (lists->fxnodes `((4 11 0)
                               (5 ,(scale 5 4 6 11 13) ,(<ra> :get-logtype-hold))
                               (8 ,(scale 8 6 9 13 15) 0)
                               (9 15 0))))

;; Before and after
(***assert*** (scissor-fxnodes-keep-outside (lists->fxnodes '((4 11 0)
                                                             (9 15 0)))
                                            5 8)
              (lists->fxnodes `((4 11 0)
                               (5 ,(scale 5 4 9 11 15) ,(<ra> :get-logtype-hold))
                               (8 ,(scale 8 4 9 11 15) 0)
                               (9 15 0))))


;; Before(hold) and after
(***assert*** (scissor-fxnodes-keep-outside (lists->fxnodes `((4 11 ,(<ra> :get-logtype-hold))
                                                             (9 15 0)))
                                            5 8)
              (lists->fxnodes `((4 11 ,(<ra> :get-logtype-hold))
                               (5 11 ,(<ra> :get-logtype-hold))
                               (8 11 ,(<ra> :get-logtype-hold))
                               (9 15 0))))


(define (scissor-fxs-keep-outside fxs startplace endplace)
  (map (lambda (fx)
         (<copy-fx> fx
                    :nodes (scissor-fxnodes-keep-outside (fx :nodes)
                                                         startplace
                                                         endplace)))
       fxs))




(define (scissor-fxnodes-keep-inside fxnodes startplace endplace)
  (assert (> endplace startplace))
  (let loop ((fxnodes fxnodes)
             (last-fxnode (<optional-hash-table>)))
    ;;(c-display "fxnodes:" fxnodes)
    ;;(c-display "last:" last-fxnode)
    ;;(c-display)
    (if (null? fxnodes)
        '()
        (let* ((fxnode (car fxnodes))
               (place (fxnode :place))
               (last-place (and last-fxnode
                                (last-fxnode :place))))
          
          (if last-place
              (assert (< last-place place)))
          
          (cond ((< place startplace)
                 (loop (cdr fxnodes)
                       fxnode))
                
                ((= place startplace)
                 (cons fxnode
                       (loop (cdr fxnodes)
                             fxnode)))
                
                ((and last-place
                      (> place      startplace)
                      (< last-place startplace))
                 (let ((startplace-fxnode (make-fxnode startplace
                                                       (scale-logtype (last-fxnode :logtype)
                                                                      startplace
                                                                      last-place place
                                                                      (last-fxnode :value) (fxnode :value))
                                                       (last-fxnode :logtype))))
                   (cons startplace-fxnode
                         (loop fxnodes
                               startplace-fxnode))))

                ((and (> place startplace)
                      (<= place endplace))
                 (cons fxnode
                       (loop (cdr fxnodes)
                             fxnode)))

                ((and last-place
                      (> place endplace)
                      (< last-place endplace))
                 (list (make-fxnode endplace
                                    (scale-logtype (last-fxnode :logtype)
                                                   endplace
                                                   last-place place
                                                   (last-fxnode :value) (fxnode :value))
                                    (last-fxnode :logtype))))

                (else
                 '()))))))
                
;; nothing
(***assert*** (scissor-fxnodes-keep-inside '()
                               5 8)
              '())

;; only before
(***assert*** (scissor-fxnodes-keep-inside (lists->fxnodes '((1 10 0)
                                                            (2 11 0)))
                                           5 8)
              '())

;; only after
(***assert*** (scissor-fxnodes-keep-inside (lists->fxnodes '((10 10 0)
                                                            (20 11 0)))
                                           5 8)
              '())

;; On first line
(***assert*** (scissor-fxnodes-keep-inside (lists->fxnodes '((1 10 0)
                                                            (2 11 0)
                                                            (5 12 0)))
                                           5 8)
              (lists->fxnodes '((5 12 0))))

;; On last line
(***assert*** (scissor-fxnodes-keep-inside (lists->fxnodes '((8 10 0)
                                                            (9 11 0)
                                                            (10 12 0)))
                                           5 8)
              (lists->fxnodes '((8 10 0))))

;; Before and inside, and on first line
(***assert*** (scissor-fxnodes-keep-inside (lists->fxnodes '((1 10 0)
                                                            (2 11 0)
                                                            (5 12 0)
                                                            (6 13 0)))
                                           5 8)
              (lists->fxnodes '((5 12 0)
                               (6 13 0))))


;; Before and inside
(***assert*** (scissor-fxnodes-keep-inside (lists->fxnodes '((1 10 0)
                                                            (2 11 0)
                                                            (6 13 0)))
                                           5 8)
              (lists->fxnodes `((5 ,(scale 5 2 6 11 13) 0)
                               (6 13 0))))

;; Before(hold) and inside
(***assert*** (scissor-fxnodes-keep-inside (lists->fxnodes `((2 11 ,(<ra> :get-logtype-hold))
                                                            (6 13 0)))
                                           5 8)
              (lists->fxnodes `((5 11 ,(<ra> :get-logtype-hold))
                               (6 13 0))))

;; On first line and inside
(***assert*** (scissor-fxnodes-keep-inside (lists->fxnodes '((5 11 0)
                                                            (6 13 0)))
                                           5 8)
              (lists->fxnodes '((5 11 0)
                               (6 13 0))))

;; Inside
(***assert*** (scissor-fxnodes-keep-inside (lists->fxnodes '((6 11 0)
                                                            (7 13 0)))
                                           5 8)
              (lists->fxnodes '((6 11 0)
                               (7 13 0))))

;; Inside and last line
(***assert*** (scissor-fxnodes-keep-inside (lists->fxnodes '((6 11 0)
                                                            (8 13 0)))
                                           5 8)
              (lists->fxnodes '((6 11 0)
                               (8 13 0))))


;; Inside and last line and after
(***assert*** (scissor-fxnodes-keep-inside (lists->fxnodes '((6 11 0)
                                                            (8 13 0)
                                                            (9 14 0)))
                                           5 8)
              (lists->fxnodes '((6 11 0)
                               (8 13 0))))

;; Inside and after
(***assert*** (scissor-fxnodes-keep-inside (lists->fxnodes '((6 11 0)
                                                            (9 14 0)))
                                           5 8)
              (lists->fxnodes `((6 11 0)
                               (8 ,(scale 8 6 9 11 14) 0))))

;; Inside(hold and after
(***assert*** (scissor-fxnodes-keep-inside (lists->fxnodes `((6 11 ,(<ra> :get-logtype-hold))
                                                             (9 14 0)))
                                           5 8)
              (lists->fxnodes `((6 11 ,(<ra> :get-logtype-hold))
                               (8 11 ,(<ra> :get-logtype-hold)))))

;; Before, on first line, inside, on last line, after
(***assert*** (scissor-fxnodes-keep-inside (lists->fxnodes '((4 11 0)
                                                            (5 12 0)
                                                            (6 13 0)
                                                            (8 14 0)
                                                            (9 15 0)))
                                           5 8)
              (lists->fxnodes `((5 12 0)
                               (6 13 0)
                               (8 14 0))))


;; Before, inside, after
(***assert*** (scissor-fxnodes-keep-inside (lists->fxnodes '((4 11 0)
                                                            (6 13 0)
                                                            (9 15 0)))
                                           5 8)
              (lists->fxnodes `((5 ,(scale 5 4 6 11 13) 0)
                               (6 13 0)
                               (8 ,(scale 8 6 9 13 15) 0))))

;; Before and after
(***assert*** (scissor-fxnodes-keep-inside (lists->fxnodes '((4 11 0)
                                                            (9 15 0)))
                                           5 8)
              (lists->fxnodes `((5 ,(scale 5 4 9 11 15) 0)
                               (8 ,(scale 8 4 9 11 15) 0))))


;; Before(hold) and after
(***assert*** (scissor-fxnodes-keep-inside (lists->fxnodes `((4 11 ,(<ra> :get-logtype-hold))
                                                            (9 15 0)))
                                           5 8)
              (lists->fxnodes `((5 11 ,(<ra> :get-logtype-hold))
                               (8 11 ,(<ra> :get-logtype-hold)))))



                
(define (scissor-fxs-keep-inside fxs startplace endplace)
  (map (lambda (fx)
         (<copy-fx> fx
                    :nodes (scissor-fxnodes-keep-inside (fx :nodes)
                                                        startplace
                                                        endplace)))
       fxs))


;; Returns the range fx for a track, skewed into 'startplace'. All fx after 'endplace' is not included.

;; Used when track fx was stored in C
(define (get-range-fxs rangetracknum startplace endplace)
  (map (lambda (fxnum)
         (define fxname (<_> (<ra> :get-fxrange-name fxnum rangetracknum)))
         (define num-fxnodes (<ra> :get-num-fxrangenodes fxnum rangetracknum))
         (define fxnodes (let loop ((fxnodenum 0)
                                    (last-fxnode (<optional-hash-table>)))
                           (if (= fxnodenum num-fxnodes)
                               '()
                               (begin
                                 (define place (+ startplace
                                                  (<ra> :get-fxrangenode-place fxnodenum fxnum rangetracknum)))
                                 (define value (<ra> :get-fxrangenode-value fxnodenum fxnum rangetracknum))
                                 (define logtype (<ra> :get-fxrangenode-logtype fxnodenum fxnum rangetracknum))
                                 (define fxnode (make-fxnode place
                                                             value
                                                             logtype))
                                 (if (>= place endplace)
                                     (if last-fxnode
                                         (list (make-fxnode endplace
                                                            (scale-logtype (last-fxnode :logtype)
                                                                           endplace
                                                                           (last-fxnode :place) (fxnode :place)
                                                                           (last-fxnode :value) (fxnode :value))
                                                            (last-fxnode :logtype)))
                                         '())
                                     (cons fxnode
                                           (loop (1+ fxnodenum)
                                                 fxnode)))))))
         (list fxname
               (remove (lambda (fxnode)
                         (eqv? #f fxnode))
                       fxnodes)))
       (iota (<ra> :get-num-fxs-in-range rangetracknum))))



#||
(string->symbol (<ra> :get-fxrange-name 0 0))
(string->symbol "Vibrato Speed")
(c-display (list (string->symbol "asdf")))
||#

(define (get-track-fxs blocknum tracknum)
  (map (lambda (fxnum)
         (define track-instrument-id (<ra> :get-instrument-for-track tracknum))
         (define fxname (<_> (<ra> :get-fx-name fxnum tracknum blocknum)))
         (define fxinstrument (let ((fx-instrument-id (<ra> :get-fx-instrument fxnum tracknum blocknum)))
                                (if (equal? fx-instrument-id track-instrument-id)
                                    (<ra> :create-illegal-instrument) ;; We want to copy effect from other instruments with the same type without modifying the other instrument. (maybe not though)
                                    fx-instrument-id)))
         (define num-fxnodes (<ra> :get-num-fxnodes fxnum tracknum blocknum))
         (define fxnodes (map (lambda (fxnodenum)
                                (define place (<ra> :get-fxnode-place fxnodenum fxnum tracknum blocknum))
                                (define value (<ra> :get-fxnode-value fxnodenum fxnum tracknum blocknum))
                                (define logtype (<ra> :get-fxnode-logtype fxnodenum fxnum tracknum blocknum))
                                (make-fxnode place value logtype))
                              (iota num-fxnodes)))
         (make-fx fxname
                  fxinstrument
                  fxnodes))
       (iota (<ra> :get-num-fxs tracknum blocknum))))

#||
(get-track-fxs -1 -1)
||#



(define (merge-fx-nodes track-nodes range-nodes)
  (cond ((null? track-nodes)
         range-nodes)
        ((null? range-nodes)
         track-nodes)
        (else
         (assert (>= (length track-nodes) 2))
         (assert (>= (length range-nodes) 2))

         (define first-track-node (car track-nodes))
         (define first-track-place (first-track-node :place))
         (define last-track-node (last track-nodes))
         (define last-track-place (last-track-node :place))
         
         (define first-range-node (car range-nodes))
         (define first-range-place (first-range-node :place))
         (define last-range-node (last range-nodes))
         (define last-range-place (last-range-node :place))
         
         (define before-track-nodes (take-while track-nodes
                                                (lambda (track-node)
                                                  (< (track-node :place)
                                                     first-range-place))))
         (define after-track-nodes (remove-while track-nodes
                                                 (lambda (track-node)
                                                   (< (track-node :place)
                                                      last-range-place))))

         (cond ((and (<= first-range-place
                         first-track-place)
                     (>= last-range-place
                         last-track-place))
                range-nodes)

               ((= last-track-place first-range-place)
                (append (butlast track-nodes)
                        (list (<copy-fxnode> last-track-node :place (-line last-track-place)))
                        range-nodes))

               ((= last-range-place first-track-place)
                (append (butlast range-nodes)
                        (list (<copy-fxnode> last-range-node :place (-line last-range-place)))
                        track-nodes))

               ((< last-track-place first-range-place)
                (append (butlast track-nodes)
                        (list (<copy-fxnode> last-track-node :logtype (<ra> :get-logtype-hold)))
                        range-nodes))

               ((> first-track-place last-range-place)
                (append (butlast range-nodes)
                        (list (<copy-fxnode> last-range-node :logtype (<ra> :get-logtype-hold)))
                        track-nodes))

               (else

                (define before-nodes (if (< first-track-place first-range-place)
                                         (begin
                                           (define bef-node (last before-track-nodes))
                                           (define aft-node (list-ref track-nodes (length before-track-nodes)))
                                           (define interfere-place (-line first-range-place))
                                           (append before-track-nodes
                                                   (list (make-fxnode interfere-place
                                                                      (scale-logtype (bef-node :logtype)
                                                                                     interfere-place
                                                                                     (bef-node :place) (aft-node :place)
                                                                                     (bef-node :value) (aft-node :value))
                                                                      (bef-node :logtype)))))
                                         '()))

                (if (> last-track-place last-range-place)
                    (append before-nodes
                            (begin
                              (define bef-node (find-last track-nodes
                                                          (lambda (track-node)
                                                            (< (track-node :place) last-range-place))))
                              (define aft-node (car after-track-nodes))
                              (define interfere-place last-range-place)
                              (append (butlast range-nodes)
                                      (list (<copy-fxnode> last-range-node :place (-line last-range-place)))
                                      (if (= ((car after-track-nodes) :place)
                                             interfere-place)
                                          '()
                                          (list (make-fxnode interfere-place
                                                             (scale-logtype (bef-node :logtype)
                                                                            interfere-place
                                                                            (bef-node :place) (aft-node :place)
                                                                            (bef-node :value) (aft-node :value))
                                                             (bef-node :logtype))))
                                      after-track-nodes)))
                    (append before-nodes
                            range-nodes)))))))

               

(***assert*** (merge-fx-nodes (lists->fxnodes '((2 11 0)
                                               (3 12 0)))
                              (lists->fxnodes '((13 20 0)
                                               (16 22 0))))
              (lists->fxnodes `((2 11 0)
                               (3 12 ,(<ra> :get-logtype-hold))
                               (13 20 0)
                               (16 22 0))))

(***assert*** (merge-fx-nodes (lists->fxnodes '((2 11 0)
                                               (3 12 0)))
                              (lists->fxnodes '((3 20 0)
                                               (6 22 0))))
              (lists->fxnodes `((2 11 0)
                               (,(-line 3) 12 0)
                               (3 20 0)
                               (6 22 0))))

(***assert*** (merge-fx-nodes (lists->fxnodes '((2 11 0)
                                               (3 12 0)))
                              (lists->fxnodes '((5 20 0)
                                               (6 22 0))))
              (lists->fxnodes `((2 11 0)
                               (3 12 ,(<ra> :get-logtype-hold))
                               (5 20 0)
                               (6 22 0))))

(***assert*** (merge-fx-nodes (lists->fxnodes '((2 11 0)
                                               (3 12 0)
                                               (8 13 0)))
                              (lists->fxnodes '((5 20 0)
                                               (6 22 0))))
              (lists->fxnodes `((2 11 0)
                               (3 12 0)
                               
                               (,(-line 5) ,(scale (-line 5) 3 8 12 13) 0)
                               
                               (5 20 0)
                               (,(-line 6) 22 0)
                               
                               (6 ,(scale 6 3 8 12 13) 0)                
                               (8 13 0))))


(***assert*** (merge-fx-nodes (lists->fxnodes '((6 12 0)
                                               (8 13 0)))
                              (lists->fxnodes '((5 20 0)
                                               (7 22 0))))
              (lists->fxnodes `((5 20 0)
                               (,(-line 7) 22 0)
                               
                               (7 ,(scale 7 6 8 12 13) 0)                
                               (8 13 0))))

(***assert*** (merge-fx-nodes (lists->fxnodes '((2 11 0)
                                               (3 12 0)))
                              (lists->fxnodes '((5 20 0)
                                               (6 22 0)
                                               (7 24 0))))
              (lists->fxnodes `((2 11 0)
                               (3 12 ,(<ra> :get-logtype-hold))
                               (5 20 0)
                               (6 22 0)
                               (7 24 0))))

(***assert*** (merge-fx-nodes (lists->fxnodes '((2 11 0)
                                               (5 12 0)))
                              (lists->fxnodes '((5 20 0)
                                               (6 22 0)
                                               (7 24 0))))
              (lists->fxnodes `((2 11 0)
                               (,(-line 5) 12 0)
                               (5 20 0)
                               (6 22 0)
                               (7 24 0))))

(***assert*** (merge-fx-nodes (lists->fxnodes '((2 11 0)
                                               (3 12 0)))
                              (lists->fxnodes '((1 20 0)
                                               (2 22 0)
                                               (3 24 0))))
              (lists->fxnodes '((1 20 0)
                               (2 22 0)
                               (3 24 0))))

(***assert*** (merge-fx-nodes (lists->fxnodes '((1 10 0)
                                               (2 11 0)
                                               (3 12 0)))
                              (lists->fxnodes '((2 22 0)
                                               (3 24 0))))
              (lists->fxnodes `((1 10 0)
                               (,(-line 2) ,(scale (-line 2) 2 3 11 12) 0)
                               (2 22 0)
                               (3 24 0))))

(***assert*** (merge-fx-nodes (lists->fxnodes '((1 10 0)
                                               (2 11 0)
                                               (3 12 0)
                                               (4 13 0)))
                              (lists->fxnodes '((2 22 0)
                                               (3 24 0))))
              (lists->fxnodes `((1 10 0)
                               (,(-line 2) ,(scale (-line 2) 2 3 11 12) 0)
                               (2 22 0)
                               (,(-line 3) 24 0)
                               (3 12 0)
                               (4 13 0))))

  
(define (merge-fxs track-fxs range-fxs)
  (cond ((null? track-fxs)
         range-fxs)
        ((null? range-fxs)
         track-fxs)
        (else
         (let* ((range-fx (car range-fxs))
                (track-fx (find-fx track-fxs (range-fx :name))))
           ;;(c-display "track-fx" track-fx)
           (if track-fx
               (cons (<copy-fx> range-fx
                                :nodes (merge-fx-nodes (track-fx :nodes) (range-fx :nodes)))
                     (merge-fxs (remove-fx track-fxs (range-fx :name))
                                (cdr range-fxs)))
               (cons range-fx
                     (merge-fxs track-fxs
                                (cdr range-fxs))))))))

(define (skew-fxnodes fxnodes how-much)
  (map (lambda (fxnode)
         (<copy-fxnode> fxnode :place (+ (fxnode :place)
                                         how-much)))
       fxnodes))

(define (skew-fxs fxs how-much)
  (if (null? fxs)
      '()
      (let ((fx (car fxs)))
        (cons (<copy-fx> fx
                         :nodes (skew-fxnodes (fx :nodes) how-much))
              (skew-fxs (cdr fxs)
                        how-much)))))


(define (get-fxnames instrument)
  (map (lambda (effect-num)
         (<_> (<ra> :get-instrument-effect-name effect-num instrument)))
       (iota (<ra> :get-num-instrument-effects instrument))))
  
(define (paste-track-fxs! blocknum tracknum fxs)
  (define instrument-id (<ra> :get-instrument-for-track tracknum blocknum))
  ;;(c-display "blocknum/tracknum/fxs/instrument" blocknum tracknum fxs instrument)
  (if (not (<ra> :is-legal-instrument instrument-id))
      #t
      (let ((effect-names  (get-fxnames instrument-id)))
        (ignore-undo-block
         (lambda ()
           (<ra> :clear-track-fx tracknum blocknum)))
        ;;(c-display "effect-names" effect-names)

        (define num-lines (<ra> :get-num-lines blocknum))
        (define (legal-place pos)
          (if #t
              pos ;; No, those should have been removed before calling paste-track-fxs. TODO: Add assertion.
              (min (-line num-lines)
                   pos)))
        
        (for-each (lambda (fx)
                    (define name (fx :name))
                    (define is-legal-effect (or (not (equal? (fx :instrument-id)
                                                             instrument-id))
                                                (memq name effect-names)))
                    (define fx-nodes (fx :nodes))
                    (c-display "got" name "? "
                               is-legal-effect
                               (>= (length fx-nodes) 2))
                    (when (and is-legal-effect
                               (>= (length fx-nodes) 2))
                          (define fx-node (car fx-nodes))
                          (define fxnum (<ra> :add-fx
                                              (fx-node :value)
                                              (legal-place (fx-node :place))
                                              (<-> name)
                                              tracknum
                                              (fx :instrument-id)
                                              blocknum))
                          ;;(c-display "fxnum" fxnum (fx-node :place))
                          (when (>= fxnum 0)
                                (<ra> :set-fxnode-logtype (fx-node :logtype) 0 fxnum tracknum blocknum)
                                
                                (define fx-node2 (cadr fx-nodes))
                                (<ra> :set-fxnode  ;; Need a better API for creating fx
                                      1
                                      (fx-node2 :value)
                                      (legal-place (fx-node2 :place))
                                      fxnum
                                      tracknum
                                      blocknum)
                                (<ra> :set-fxnode-logtype (fx-node2 :logtype) 1 fxnum tracknum blocknum)
                                
                                (for-each (lambda (fxnode)
                                            (define nodenum (<ra> :add-fxnode
                                                                  (fxnode :value)
                                                                  (legal-place (fxnode :place))
                                                                  fxnum
                                                                  tracknum
                                                                  blocknum))
                                            ;;(c-display "Creating node" nodenum " at place" (* 1.0 (fxnode :place)))
                                            (if (>= nodenum 0)
                                                (<ra> :set-fxnode-logtype (fxnode :logtype) nodenum fxnum tracknum blocknum))
                                            )
                                          (cddr fx-nodes)))))
                  fxs))))


(define (copy-fx-range! blocknum starttrack endtrack startplace endplace)
  (c-display "COPY " startplace endplace)
  (set! *clipboard-fxs*
        (map (lambda (tracknum)
               (skew-fxs (scissor-fxs-keep-inside (get-track-fxs blocknum tracknum)
                                                  startplace
                                                  endplace)
                         (- startplace)))
             (integer-range starttrack endtrack))))


(define (cut-fx-range! blocknum starttrack endtrack startplace endplace)
  ;; (copy-fx-range! blocknum starttrack endtrack startplace endplace) ;; copy-fx-range! is called manually before cut-fx-range!
  (for-each (lambda (tracknum)
              ;;(c-display "   2. calling paste-track-fxs from cut-fx-range" tracknum)
              (paste-track-fxs! blocknum
                                tracknum
                                (scissor-fxs-keep-outside (get-track-fxs blocknum tracknum)
                                                          startplace
                                                          endplace)))
            (integer-range starttrack endtrack)))
  


(define (paste-fx-range! blocknum starttrack startplace)
  (assert (>= starttrack 0))
  
  (define endplace (-line (<ra> :get-num-lines blocknum)))

  (for-each (lambda (range-fxs tracknum)
              (when (< tracknum (<ra> :get-num-tracks blocknum))
                (let ((track-fxs (get-track-fxs blocknum tracknum))
                      (scissored-range-fxs (scissor-fxs-keep-inside (skew-fxs range-fxs startplace)
                                                                    0
                                                                    endplace)))
                  ;;(c-display "   1. calling paste-track-fxs from paste-fx-range" tracknum)
                  ;;(c-display "track-fx:\n" (pp track-fxs) "\n")
                  ;;(c-display "scissored-fx:\n" (pp scissored-range-fxs) "\n")
                  ;;(c-display "merged:\n" (pp (merge-fxs track-fxs scissored-range-fxs)) "\n")
                  (paste-track-fxs! blocknum
                                    tracknum
                                    (merge-fxs track-fxs scissored-range-fxs)))))
            *clipboard-fxs*
            (map (lambda (tracknum)
                   (+ starttrack tracknum))
                 (iota (length *clipboard-fxs*)))))
              

#||
(paste-fx-range! -1 2 14)
||#
  

(define (quantitize-down-place place quant)
  (* quant (floor (/ place quant))))

(***assert*** (quantitize-down-place 5 2)
              4)
(***assert*** (quantitize-down-place 4 2)
              4)


(define (simple-quantitize-fxnodes fxnodes quant)
  (let loop ((fxnodes fxnodes)
             (curr-place 0)
             (last-val #f)
             (last-logtype #f))
    (if (null? fxnodes)
        (if (not last-val)
            '()
            (list (make-fxnode curr-place last-val last-logtype)))
        (let* ((fxnode (car fxnodes))
               (place (quantitize-down-place (fxnode :place) quant))
               (rest (loop (cdr fxnodes)
                           place
                           (fxnode :value)
                           (fxnode :logtype))))
          (if (and (> place curr-place)
                   last-val)
              (cons (make-fxnode curr-place
                                 last-val
                                 last-logtype)
                    rest)
              rest)))))

(***assert*** (simple-quantitize-fxnodes (list) 1)
              '())

(***assert*** (simple-quantitize-fxnodes (list
                                          (make-fxnode 8.2 9 'a))
                                         1)
              (list (make-fxnode 8 9 'a)))

(***assert*** (simple-quantitize-fxnodes (list
                                          (make-fxnode 5   1 'a)
                                          (make-fxnode 5.2 2 'b)
                                          (make-fxnode 6   3 'c)
                                          (make-fxnode 7.6 4 'd)
                                          (make-fxnode 8   5 'e))
                                         1)
              (list (make-fxnode 5 2 'b)
                    (make-fxnode 6 3 'c)
                    (make-fxnode 7 4 'd)
                    (make-fxnode 8 5 'e)))
                                           
(***assert*** (simple-quantitize-fxnodes (list
                                          (make-fxnode 5   1 'a)
                                          (make-fxnode 5.2 2 'b)
                                          (make-fxnode 6   3 'c)
                                          (make-fxnode 7.6 4 'd)
                                          (make-fxnode 8   5 'e))
                                         1/2)
              (list (make-fxnode 5 2 'b)
                    (make-fxnode 6 3 'c)
                    (make-fxnode 7.5 4 'd)
                    (make-fxnode 8 5 'e)))
                                           

#||
(define (simple-quantitize-fxs-internal fxs)
  (map (lambda (fx)
         (make-fx (fx-name fx)
                    (fx-instrument fx)
                    (simple-quantitize-fxnodes (fx-nodes fx))))
       fxs))
||#

(define (simple-quantitize-fxs! blocknum tracknum fxnum quant)
  (define old-fxs (get-track-fxs blocknum tracknum))
  (define new-fxs (map (lambda (fx fxnum2)
                         (if (= fxnum fxnum2)
                             (<copy-fx> fx
                                        :nodes (simple-quantitize-fxnodes (fx :nodes) quant))
                             fx))
                       old-fxs
                       (iota (length old-fxs))))
  (paste-track-fxs! blocknum
                    tracknum
                    new-fxs))


#||
(simple-quantitize-fxs! 0 1 0 1)
||#
