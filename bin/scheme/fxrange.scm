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

(define (create-fx fxname fxnodes)
  (list fxname fxnodes))

(define (fx-name fx)
  (car fx))

(define (fx-nodes fx)
  (cadr fx))

(define (create-fxnode place value logtype)
  (list place value logtype))

(define fxnode-place car)
(define fxnode-value cadr)
(define fxnode-logtype caddr)
(define (fxnode-is-holding? fxnode)
  (logtype-holding? (fxnode-logtype fxnode)))
  
(define (fxnode-replace-place fxnode new-place)
  (create-fxnode new-place
                 (fxnode-value fxnode)
                 (fxnode-logtype fxnode)))
  
(define (fxnode-replace-logtype fxnode new-logtype)
  (create-fxnode (fxnode-place fxnode)
                 (fxnode-value fxnode)
                 new-logtype))
  
(define (find-fx fxs name)
  (assq name fxs))

(define (remove-fx fxs name)
  (if (null? fxs)
      '()
      (let ((fx (car fxs)))
        (if (eq? (fx-name fx) name)
            (cdr fxs)
            (cons fx
                  (remove-fx (cdr fxs) name))))))


;; A list of fxs, one fxs for each track
(define *clipboard-fxs* '())



(define (scissor-fxnodes-keep-outside fxnodes startplace endplace)
  (assert (> endplace startplace))
  
  (let loop ((fxnodes fxnodes)
             (last-fxnode #f))
    (c-display "fxnodes:" fxnodes)
    (c-display "last:" last-fxnode)
    (c-display)
    (if (null? fxnodes)
        '()
        (let* ((fxnode (car fxnodes))
               (place (fxnode-place fxnode))
               (last-place (and last-fxnode
                                (fxnode-place last-fxnode))))
          
          (if last-place
              (assert (< last-place place)))
          
          (cond ((< place startplace)
                 (cons fxnode
                       (loop (cdr fxnodes)
                             fxnode)))
                
                ((= place startplace)
                 (cons (fxnode-replace-logtype fxnode (<ra> :get-logtype-hold))
                       (loop (cdr fxnodes)
                             fxnode)))

                ((and last-place
                      (> place endplace)
                      (< last-place startplace))
                 (let ((first (create-fxnode startplace
                                              (scale-logtype (fxnode-logtype last-fxnode)
                                                             startplace
                                                             last-place place
                                                             (fxnode-value last-fxnode) (fxnode-value fxnode))
                                              (<ra> :get-logtype-hold)))
                       (second (create-fxnode endplace
                                              (scale-logtype (fxnode-logtype last-fxnode)
                                                             endplace
                                                             last-place place
                                                             (fxnode-value last-fxnode) (fxnode-value fxnode))
                                              (fxnode-logtype last-fxnode))))
                   (append (list first second)
                           (loop fxnodes
                                 second))))
                
                ((and last-place
                      (> place      startplace)
                      (< last-place startplace))
                 (let ((startplace-fxnode (create-fxnode startplace
                                                         (scale-logtype (fxnode-logtype last-fxnode)
                                                                        startplace
                                                                        last-place place
                                                                        (fxnode-value last-fxnode) (fxnode-value fxnode))
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
                 (let ((endplace-fxnode (create-fxnode endplace
                                                       (scale-logtype (fxnode-logtype last-fxnode)
                                                                      endplace
                                                                      last-place place
                                                                      (fxnode-value last-fxnode) (fxnode-value fxnode))
                                                       (fxnode-logtype last-fxnode))))
                   (cons endplace-fxnode
                         (loop fxnodes
                               endplace-fxnode))))

                (else
                 (cons fxnode
                       (loop (cdr fxnodes)
                             fxnode))))))))


;; nothing
(***assert*** (scissor-fxnodes-keep-outside '()
                                            5 8)
              '())

;; only before
(***assert*** (scissor-fxnodes-keep-outside '((1 10 0)
                                              (2 11 0))
                                            5 8)
              '((1 10 0)
                (2 11 0)))

;; only after
(***assert*** (scissor-fxnodes-keep-outside '((10 10 0)
                                              (20 11 0))
                                            5 8)
              '((10 10 0)
                (20 11 0)))

;; On first line
(***assert*** (scissor-fxnodes-keep-outside '((1 10 0)
                                              (2 11 0)
                                              (5 12 0))
                                            5 8)
              `((1 10 0)
                (2 11 0)
                (5 12 ,(<ra> :get-logtype-hold))))

;; On last line
(***assert*** (scissor-fxnodes-keep-outside '((8 10 0)
                                              (9 11 0)
                                              (10 12 0))
                                            5 8)
              '((8 10 0)
                (9 11 0)
                (10 12 0)))

;; Before and inside, and on first line
(***assert*** (scissor-fxnodes-keep-outside '((1 10 0)
                                              (2 11 0)
                                              (5 12 0)
                                              (6 13 0))
                                            5 8)
              `((1 10 0)
                (2 11 0)
                (5 12 ,(<ra> :get-logtype-hold))))


;; Before and inside
(***assert*** (scissor-fxnodes-keep-outside '((1 10 0)
                                              (2 11 0)
                                              (6 13 0))
                                            5 8)
              `((1 10 0)
                (2 11 0)
                (5 ,(scale 5 2 6 11 13) ,(<ra> :get-logtype-hold))))


;; Before(hold) and inside
(***assert*** (scissor-fxnodes-keep-outside `((2 11 ,(<ra> :get-logtype-hold))
                                              (6 13 0))
                                            5 8)
              `((2 11 ,(<ra> :get-logtype-hold))
                (5 11 ,(<ra> :get-logtype-hold))))

;; On first line and inside
(***assert*** (scissor-fxnodes-keep-outside '((5 11 0)
                                              (6 13 0))
                                            5 8)
              `((5 11 ,(<ra> :get-logtype-hold))))

;; Inside
(***assert*** (scissor-fxnodes-keep-outside '((6 11 0)
                                              (7 13 0))
                                            5 8)
              '())

;; Inside and last line
(***assert*** (scissor-fxnodes-keep-outside '((6 11 0)
                                              (8 13 0))
                                            5 8)
              '((8 13 0)))


;; Inside and last line and after
(***assert*** (scissor-fxnodes-keep-outside '((6 11 0)
                                              (8 13 0)
                                              (9 14 0))
                                            5 8)
              '((8 13 0)
                (9 14 0)))

;; Inside and after
(***assert*** (scissor-fxnodes-keep-outside '((6 11 0)
                                              (9 14 0))
                                            5 8)
              `((8 ,(scale 8 6 9 11 14) 0)
                (9 14 0)))

;; Inside(hold) and after
(***assert*** (scissor-fxnodes-keep-outside `((6 11 ,(<ra> :get-logtype-hold))
                                              (9 14 0))
                                            5 8)
              `((8 11 ,(<ra> :get-logtype-hold))
                (9 14 0)))

;; Before, on first line, inside, on last line, after
(***assert*** (scissor-fxnodes-keep-outside '((4 11 0)
                                              (5 12 0)
                                              (6 13 0)
                                              (8 14 0)
                                              (9 15 0))
                                            5 8)
              `((4 11 0)
                (5 12 ,(<ra> :get-logtype-hold))
                (8 14 0)
                (9 15 0)))


;; Before, inside, after
(***assert*** (scissor-fxnodes-keep-outside '((4 11 0)
                                              (6 13 0)
                                              (9 15 0))
                                            5 8)
              `((4 11 0)
                (5 ,(scale 5 4 6 11 13) ,(<ra> :get-logtype-hold))
                (8 ,(scale 8 6 9 13 15) 0)
                (9 15 0)))

;; Before and after
(***assert*** (scissor-fxnodes-keep-outside '((4 11 0)
                                              (9 15 0))
                                            5 8)
              `((4 11 0)
                (5 ,(scale 5 4 9 11 15) ,(<ra> :get-logtype-hold))
                (8 ,(scale 8 4 9 11 15) 0)
                (9 15 0)))


;; Before(hold) and after
(***assert*** (scissor-fxnodes-keep-outside `((4 11 ,(<ra> :get-logtype-hold))
                                              (9 15 0))
                                            5 8)
              `((4 11 ,(<ra> :get-logtype-hold))
                (5 11 ,(<ra> :get-logtype-hold))
                (8 11 ,(<ra> :get-logtype-hold))
                (9 15 0)))


(define (scissor-fxs-keep-outside fxs startplace endplace)
  (map (lambda (fx)
         (create-fx (fx-name fx)
                    (scissor-fxnodes-keep-outside (fx-nodes fx)
                                                  startplace
                                                  endplace)))
       fxs))




(define (scissor-fxnodes-keep-inside fxnodes startplace endplace)
  (assert (> endplace startplace))
  (let loop ((fxnodes fxnodes)
             (last-fxnode #f))
    ;;(c-display "fxnodes:" fxnodes)
    ;;(c-display "last:" last-fxnode)
    ;;(c-display)
    (if (null? fxnodes)
        '()
        (let* ((fxnode (car fxnodes))
               (place (fxnode-place fxnode))
               (last-place (and last-fxnode
                                (fxnode-place last-fxnode))))
          
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
                 (let ((startplace-fxnode (create-fxnode startplace
                                                         (scale-logtype (fxnode-logtype last-fxnode)
                                                                        startplace
                                                                        last-place place
                                                                        (fxnode-value last-fxnode) (fxnode-value fxnode))
                                                         (fxnode-logtype last-fxnode))))
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
                 (list (create-fxnode endplace
                                      (scale-logtype (fxnode-logtype last-fxnode)
                                                     endplace
                                                     last-place place
                                                     (fxnode-value last-fxnode) (fxnode-value fxnode))
                                      (fxnode-logtype last-fxnode))))

                (else
                 '()))))))
                
;; nothing
(***assert*** (scissor-fxnodes-keep-inside '()
                               5 8)
              '())

;; only before
(***assert*** (scissor-fxnodes-keep-inside '((1 10 0)
                                             (2 11 0))
                                           5 8)
              '())

;; only after
(***assert*** (scissor-fxnodes-keep-inside '((10 10 0)
                                             (20 11 0))
                                           5 8)
              '())

;; On first line
(***assert*** (scissor-fxnodes-keep-inside '((1 10 0)
                                             (2 11 0)
                                             (5 12 0))
                                           5 8)
              '((5 12 0)))

;; On last line
(***assert*** (scissor-fxnodes-keep-inside '((8 10 0)
                                             (9 11 0)
                                             (10 12 0))
                                           5 8)
              '((8 10 0)))

;; Before and inside, and on first line
(***assert*** (scissor-fxnodes-keep-inside '((1 10 0)
                                             (2 11 0)
                                             (5 12 0)
                                             (6 13 0))
                                           5 8)
              '((5 12 0)
                (6 13 0)))


;; Before and inside
(***assert*** (scissor-fxnodes-keep-inside '((1 10 0)
                                             (2 11 0)
                                             (6 13 0))
                                           5 8)
              `((5 ,(scale 5 2 6 11 13) 0)
                (6 13 0)))

;; Before(hold) and inside
(***assert*** (scissor-fxnodes-keep-inside `((2 11 ,(<ra> :get-logtype-hold))
                                             (6 13 0))
                                           5 8)
              `((5 11 ,(<ra> :get-logtype-hold))
                (6 13 0)))

;; On first line and inside
(***assert*** (scissor-fxnodes-keep-inside '((5 11 0)
                                             (6 13 0))
                                           5 8)
              '((5 11 0)
                (6 13 0)))

;; Inside
(***assert*** (scissor-fxnodes-keep-inside '((6 11 0)
                                             (7 13 0))
                                           5 8)
              '((6 11 0)
                (7 13 0)))

;; Inside and last line
(***assert*** (scissor-fxnodes-keep-inside '((6 11 0)
                                             (8 13 0))
                                           5 8)
              '((6 11 0)
                (8 13 0)))


;; Inside and last line and after
(***assert*** (scissor-fxnodes-keep-inside '((6 11 0)
                                             (8 13 0)
                                             (9 14 0))
                                           5 8)
              '((6 11 0)
                (8 13 0)))

;; Inside and after
(***assert*** (scissor-fxnodes-keep-inside '((6 11 0)
                                             (9 14 0))
                                           5 8)
              `((6 11 0)
                (8 ,(scale 8 6 9 11 14) 0)))

;; Inside(hold and after
(***assert*** (scissor-fxnodes-keep-inside `((6 11 ,(<ra> :get-logtype-hold))
                                 (9 14 0))
                               5 8)
              `((6 11 ,(<ra> :get-logtype-hold))
                (8 11 ,(<ra> :get-logtype-hold))))

;; Before, on first line, inside, on last line, after
(***assert*** (scissor-fxnodes-keep-inside '((4 11 0)
                                             (5 12 0)
                                             (6 13 0)
                                             (8 14 0)
                                             (9 15 0))
                                           5 8)
              `((5 12 0)
                (6 13 0)
                (8 14 0)))


;; Before, inside, after
(***assert*** (scissor-fxnodes-keep-inside '((4 11 0)
                                             (6 13 0)
                                             (9 15 0))
                                           5 8)
              `((5 ,(scale 5 4 6 11 13) 0)
                (6 13 0)
                (8 ,(scale 8 6 9 13 15) 0)))

;; Before and after
(***assert*** (scissor-fxnodes-keep-inside '((4 11 0)
                                             (9 15 0))
                                           5 8)
              `((5 ,(scale 5 4 9 11 15) 0)
                (8 ,(scale 8 4 9 11 15) 0)))


;; Before(hold) and after
(***assert*** (scissor-fxnodes-keep-inside `((4 11 ,(<ra> :get-logtype-hold))
                                             (9 15 0))
                                           5 8)
              `((5 11 ,(<ra> :get-logtype-hold))
                (8 11 ,(<ra> :get-logtype-hold))))



                
(define (scissor-fxs-keep-inside fxs startplace endplace)
  (map (lambda (fx)
         (create-fx (fx-name fx)
                    (scissor-fxnodes-keep-inside (fx-nodes fx)
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
                                    (last-fxnode #f))
                           (if (= fxnodenum num-fxnodes)
                               '()
                               (begin
                                 (define place (+ startplace
                                                  (<ra> :get-fxrangenode-place fxnodenum fxnum rangetracknum)))
                                 (define value (<ra> :get-fxrangenode-value fxnodenum fxnum rangetracknum))
                                 (define logtype (<ra> :get-fxrangenode-logtype fxnodenum fxnum rangetracknum))
                                 (define fxnode (create-fxnode place
                                                               value
                                                               logtype))
                                 (if (>= place endplace)
                                     (if last-fxnode
                                         (list (create-fxnode endplace
                                                              (scale-logtype (fxnode-logtype last-fxnode)
                                                                             endplace
                                                                             (fxnode-place last-fxnode) (fxnode-place fxnode)
                                                                             (fxnode-value last-fxnode) (fxnode-value fxnode))
                                                              (fxnode-logtype last-fxnode)))
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
         (define fxname (<_> (<ra> :get-fx-name fxnum tracknum blocknum)))
         (define num-fxnodes (<ra> :get-num-fxnodes fxnum tracknum blocknum))
         (define fxnodes (map (lambda (fxnodenum)
                                (define place (<ra> :get-fxnode-place fxnodenum fxnum tracknum blocknum))
                                (define value (<ra> :get-fxnode-value fxnodenum fxnum tracknum blocknum))
                                (define logtype (<ra> :get-fxnode-logtype fxnodenum fxnum tracknum blocknum))
                                (list place value logtype))
                              (iota num-fxnodes)))
         (create-fx fxname
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
         (define first-track-place (fxnode-place first-track-node))
         (define last-track-node (last track-nodes))
         (define last-track-place (fxnode-place last-track-node))
         
         (define first-range-node (car range-nodes))
         (define first-range-place (fxnode-place first-range-node))
         (define last-range-node (last range-nodes))
         (define last-range-place (fxnode-place last-range-node))
         
         (define before-track-nodes (take-while track-nodes
                                                (lambda (track-node)
                                                  (< (fxnode-place track-node)
                                                     first-range-place))))
         (define after-track-nodes (remove-while track-nodes
                                                 (lambda (track-node)
                                                   (< (fxnode-place track-node)
                                                      last-range-place))))

         (cond ((and (<= first-range-place
                         first-track-place)
                     (>= last-range-place
                         last-track-place))
                range-nodes)

               ((= last-track-place first-range-place)
                (append (butlast track-nodes)
                        (list (fxnode-replace-place last-track-node (-line last-track-place)))
                        range-nodes))

               ((= last-range-place first-track-place)
                (append (butlast range-nodes)
                        (list (fxnode-replace-place last-range-node (-line last-range-place)))
                        track-nodes))

               ((< last-track-place first-range-place)
                (append (butlast track-nodes)
                        (list (fxnode-replace-logtype last-track-node (<ra> :get-logtype-hold)))
                        range-nodes))

               ((> first-track-place last-range-place)
                (append (butlast range-nodes)
                        (list (fxnode-replace-logtype last-range-node (<ra> :get-logtype-hold)))
                        track-nodes))

               (else

                (define before-nodes (if (< first-track-place first-range-place)
                                         (begin
                                           (define bef-node (last before-track-nodes))
                                           (define aft-node (list-ref track-nodes (length before-track-nodes)))
                                           (define interfere-place (-line first-range-place))
                                           (append before-track-nodes
                                                   (list (create-fxnode interfere-place
                                                                        (scale-logtype (fxnode-logtype bef-node)
                                                                                       interfere-place
                                                                                       (fxnode-place bef-node) (fxnode-place aft-node)
                                                                                       (fxnode-value bef-node) (fxnode-value aft-node))
                                                                        (fxnode-logtype bef-node)))))
                                         '()))

                (if (> last-track-place last-range-place)
                    (append before-nodes
                            (begin
                              (define bef-node (find-last track-nodes
                                                          (lambda (track-node)
                                                            (< (fxnode-place track-node) last-range-place))))
                              (define aft-node (car after-track-nodes))
                              (define interfere-place last-range-place)
                              (append (butlast range-nodes)
                                      (list (fxnode-replace-place last-range-node (-line last-range-place)))
                                      (if (= (fxnode-place (car after-track-nodes))
                                             interfere-place)
                                          '()
                                          (list (create-fxnode interfere-place
                                                               (scale-logtype (fxnode-logtype bef-node)
                                                                              interfere-place
                                                                              (fxnode-place bef-node) (fxnode-place aft-node)
                                                                              (fxnode-value bef-node) (fxnode-value aft-node))
                                                               (fxnode-logtype bef-node))))
                                      after-track-nodes)))
                    (append before-nodes
                            range-nodes)))))))

               

(***assert*** (merge-fx-nodes '((2 11 0)
                                (3 12 0))
                              '((13 20 0)
                                (16 22 0)))
              `((2 11 0)
                (3 12 ,(<ra> :get-logtype-hold))
                (13 20 0)
                (16 22 0)))

(***assert*** (merge-fx-nodes '((2 11 0)
                                (3 12 0))
                              '((3 20 0)
                                (6 22 0)))
              `((2 11 0)
                (,(-line 3) 12 0)
                (3 20 0)
                (6 22 0)))

(***assert*** (merge-fx-nodes '((2 11 0)
                                (3 12 0))
                              '((5 20 0)
                                (6 22 0)))
              `((2 11 0)
                (3 12 ,(<ra> :get-logtype-hold))
                (5 20 0)
                (6 22 0)))

(***assert*** (merge-fx-nodes '((2 11 0)
                                (3 12 0)
                                (8 13 0))
                              '((5 20 0)
                                (6 22 0)))
              `((2 11 0)
                (3 12 0)
                
                (,(-line 5) ,(scale (-line 5) 3 8 12 13) 0)
                    
                (5 20 0)
                (,(-line 6) 22 0)
                
                (6 ,(scale 6 3 8 12 13) 0)                
                (8 13 0)))


(***assert*** (merge-fx-nodes '((6 12 0)
                                (8 13 0))
                              '((5 20 0)
                                (7 22 0)))
              `((5 20 0)
                (,(-line 7) 22 0)

                (7 ,(scale 7 6 8 12 13) 0)                
                (8 13 0)))

(***assert*** (merge-fx-nodes '((2 11 0)
                                (3 12 0))
                              '((5 20 0)
                                (6 22 0)
                                (7 24 0)))
              `((2 11 0)
                (3 12 ,(<ra> :get-logtype-hold))
                (5 20 0)
                (6 22 0)
                (7 24 0)))

(***assert*** (merge-fx-nodes '((2 11 0)
                                (5 12 0))
                              '((5 20 0)
                                (6 22 0)
                                (7 24 0)))
              `((2 11 0)
                (,(-line 5) 12 0)
                (5 20 0)
                (6 22 0)
                (7 24 0)))

(***assert*** (merge-fx-nodes '((2 11 0)
                                (3 12 0))
                              '((1 20 0)
                                (2 22 0)
                                (3 24 0)))
              '((1 20 0)
                (2 22 0)
                (3 24 0)))

(***assert*** (merge-fx-nodes '((1 10 0)
                                (2 11 0)
                                (3 12 0))
                              '((2 22 0)
                                (3 24 0)))
              `((1 10 0)
                (,(-line 2) ,(scale (-line 2) 2 3 11 12) 0)
                (2 22 0)
                (3 24 0)))

(***assert*** (merge-fx-nodes '((1 10 0)
                                (2 11 0)
                                (3 12 0)
                                (4 13 0))
                              '((2 22 0)
                                (3 24 0)))
              `((1 10 0)
                (,(-line 2) ,(scale (-line 2) 2 3 11 12) 0)
                (2 22 0)
                (,(-line 3) 24 0)
                (3 12 0)
                (4 13 0)))

  
(define (merge-fxs track-fxs range-fxs)
  (cond ((null? track-fxs)
         range-fxs)
        ((null? range-fxs)
         track-fxs)
        (else
         (let* ((range-fx (car range-fxs))
                (name (fx-name range-fx))
                (track-fx (find-fx track-fxs name)))
           ;;(c-display "track-fx" track-fx)
           (if track-fx
               (cons (create-fx name
                                (merge-fx-nodes (cadr track-fx) (cadr range-fx)))
                     (merge-fxs (remove-fx track-fxs name)
                                (cdr range-fxs)))
               (cons range-fx
                     (merge-fxs track-fxs
                                (cdr range-fxs))))))))

(define (skew-fxnodes fxnodes how-much)
  (map (lambda (fxnode)
         (fxnode-replace-place fxnode (+ (fxnode-place fxnode)
                                         how-much)))
       fxnodes))

(define (skew-fxs fxs how-much)
  (if (null? fxs)
      '()
      (let ((fx (car fxs)))
        (cons (create-fx (fx-name fx)
                         (skew-fxnodes (fx-nodes fx) how-much))
              (skew-fxs (cdr fxs)
                        how-much)))))


(define (get-fxnames instrument)
  (map (lambda (effect-num)
         (<_> (<ra> :get-instrument-effect-name effect-num instrument)))
       (iota (<ra> :get-num-instrument-effects instrument))))
  
(define (paste-track-fxs! blocknum tracknum fxs)
  (define instrument (<ra> :get-instrument-for-track tracknum blocknum))
  (c-display "blocknum/tracknum/fxs/instrument" blocknum tracknum fxs instrument)
  (if (< instrument 0)
      #t
      (let ((effect-names  (get-fxnames instrument)))  
        (<ra> :clear-track-fx tracknum blocknum)
        (c-display "effect-names" effect-names)

        (define num-lines (<ra> :get-num-lines blocknum))
        (define (legal-place pos)
          (if #t
              pos ;; No, those should have been removed before calling paste-track-fxs. TODO: Add assertion.
              (min (-line num-lines)
                   pos)))

        (for-each (lambda (fx)
                    (define name (fx-name fx))
                    (define fx-nodes (fx-nodes fx))
                    (c-display "got" name "? "
                               (and (memq name effect-names) #t)
                               (>= (length fx-nodes) 2))
                    (when (and (memq name effect-names)
                               (>= (length fx-nodes) 2))
                          (define fx-node (car fx-nodes))
                          (define fxnum (<ra> :create-fx
                                              (fxnode-value fx-node)
                                              (legal-place (fxnode-place fx-node))
                                              (<-> name)
                                              tracknum
                                              blocknum))
                          (c-display "fxnum" fxnum (fxnode-place fx-node))
                          (when (>= fxnum 0)
                                (<ra> :set-fxnode-logtype (fxnode-logtype fx-node) 0 fxnum tracknum blocknum)
                                
                                (define fx-node2 (cadr fx-nodes))
                                (<ra> :set-fxnode  ;; Need a better API for creating fx
                                      1
                                      (fxnode-value fx-node2)
                                      (legal-place (fxnode-place fx-node2))
                                      fxnum
                                      tracknum
                                      blocknum)
                                (<ra> :set-fxnode-logtype (fxnode-logtype fx-node2) 1 fxnum tracknum blocknum)
                                
                                (for-each (lambda (fxnode)
                                            (define nodenum (<ra> :create-fxnode
                                                                  (fxnode-value fxnode)
                                                                  (legal-place (fxnode-place fxnode))
                                                                  fxnum
                                                                  tracknum
                                                                  blocknum))
                                            ;;(c-display "Creating node" nodenum " at place" (* 1.0 (fxnode-place fxnode)))
                                            (if (>= nodenum 0)
                                                (<ra> :set-fxnode-logtype (fxnode-logtype fxnode) nodenum fxnum tracknum blocknum))
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
  
