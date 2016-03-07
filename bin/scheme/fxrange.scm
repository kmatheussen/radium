(provide 'fxrange.scm)


;; fxs format:
'((fxname1 ((place1 value1 logtype) ;; Note that the fxname might clash when merging, different type of fx with the same name. This should not be a big problem though (it could even be a feature), since all values are between 0 and 1.
            (place2 value2 logtype)
            ...))
  (fxname2 ...)
  ...)

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
  

;; Returns the range fx for a track, skewed into 'startplace'. All fx after 'endplace' is not included.
(define (get-range-fxs rangetracknum startplace endplace)
  (map (lambda (fxnum)
         (define fxname (string->symbol (<ra> :get-fxrange-name fxnum rangetracknum)))
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
                                                              (if (= (<ra> :get-logtype-hold)
                                                                     (fxnode-logtype last-fxnode))
                                                                  (fxnode-value last-fxnode)
                                                                  (scale endplace
                                                                         (fxnode-place last-fxnode) (fxnode-place fxnode)
                                                                         (fxnode-value last-fxnode) (fxnode-value fxnode)))
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
         (define fxname (string->symbol (<ra> :get-fx-name fxnum tracknum blocknum)))
         (define num-fxnodes (<ra> :get-num-fxnodes fxnum tracknum blocknum))
         (define fxnodes (map (lambda (fxnodenum)
                                (define place (<ra> :get-fxnode-place fxnodenum fxnum tracknum blocknum))
                                (define value (<ra> :get-fxnode-value fxnodenum fxnum tracknum blocknum))
                                (define logtype (<ra> :get-fxnode-logtype fxnodenum fxnum tracknum blocknum))
                                (list place value logtype))
                              (iota num-fxnodes)))
         (list fxname
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
                                                                        (scale interfere-place
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
                              (define bef-holding? (= (<ra> :get-logtype-hold)
                                                      (fxnode-logtype bef-node)))
                              (define aft-node (car after-track-nodes))
                              (define interfere-place last-range-place)
                              (append (butlast range-nodes)
                                      (list (fxnode-replace-place last-range-node (-line last-range-place)))
                                      (if (= (fxnode-place (car after-track-nodes))
                                             interfere-place)
                                          '()
                                          (list (create-fxnode interfere-place
                                                               (if bef-holding?
                                                                   (fxnode-value bef-node)
                                                                   (scale interfere-place
                                                                          (fxnode-place bef-node) (fxnode-place aft-node)
                                                                          (fxnode-value bef-node) (fxnode-value aft-node)))
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
                (name (car range-fx))               
                (track-fx (find-fx track-fxs name)))
           (c-display "track-fx" track-fx)
           (if track-fx
               (cons (create-fx name
                                (merge-fx-nodes (cadr track-fx) (cadr range-fx)))
                     (merge-fxs (remove-fx track-fxs name)
                                (cdr range-fxs)))
               (cons range-fx
                     (merge-fxs track-fxs
                                (cdr range-fxs))))))))


(define (get-fxnames instrument)
  (map (lambda (effect-num)
         (string->symbol (<ra> :get-instrument-effect-name effect-num instrument)))
       (iota (<ra> :get-num-instrument-effects instrument))))
  
(define (paste-track-fxs blocknum tracknum fxs)
  (define instrument (<ra> :get-instrument-for-track tracknum blocknum))
  (c-display "blocknum/tracknum/fxs/instrument" blocknum tracknum fxs instrument)
  (if (= -1 instrument)
      #t
      (let ((effect-names  (get-fxnames instrument)))  
        (<ra> :clear-track-fx tracknum blocknum)
        (c-display "effect-names" effect-names)
        (for-each (lambda (fx)
                    (define name (fx-name fx))
                    (define fx-nodes (fx-nodes fx))
                    (c-display "got" name "? "
                               (and (memq name effect-names) #t)
                               (>= (length fx-nodes) 2))
                    (when (and (memq name effect-names)
                               (>= (length fx-nodes) 2))
                          (define fx-node (car fx-nodes))
                          (define fxnum (<ra> :create-fx3
                                              (fxnode-value fx-node)
                                              (fxnode-place fx-node)
                                              (<-> name)
                                              tracknum
                                              blocknum))
                          (when (>= fxnum 0)
                                (<ra> :set-fxnode-logtype (fxnode-logtype fx-node) 0 fxnum tracknum blocknum)
                                
                                (define fx-node2 (cadr fx-nodes))
                                (<ra> :set-fxnode3  ;; Need a better API for creating fx
                                      1
                                      (fxnode-value fx-node2)
                                      (fxnode-place fx-node2)
                                      fxnum
                                      tracknum
                                      blocknum)
                                (<ra> :set-fxnode-logtype (fxnode-logtype fx-node2) 1 fxnum tracknum blocknum)
                                
                                (for-each (lambda (fxnode)
                                            (define nodenum (<ra> :create-fxnode3
                                                                  (fxnode-value fxnode)
                                                                  (fxnode-place fxnode)
                                                                  fxnum
                                                                  tracknum
                                                                  blocknum))
                                            (<ra> :set-fxnode-logtype (fxnode-logtype fxnode) nodenum fxnum tracknum blocknum))
                                          (cddr fx-nodes)))))
                  fxs))))

(define (paste-range blocknum starttrack startplace)
  (assert (>= starttrack 0))
  
  (define endplace (-line (<ra> :get-num-lines blocknum)))
  
  (let loop ((rangetracknum 0))
    (define tracknum (+ rangetracknum starttrack))
    (if (or (>= rangetracknum (<ra> :get-num-tracks-in-range))
            (>= tracknum (<ra> :get-num-tracks blocknum)))
        #t
        (let* ((range-fxs (get-range-fxs rangetracknum startplace endplace))
               (track-fxs (get-track-fxs blocknum tracknum)))
          (if (or (not range-fxs)
                  (not track-fxs))
              #t
              (begin
                (paste-track-fxs blocknum
                                 tracknum
                                 (merge-fxs track-fxs range-fxs))
                (loop (1+ rangetracknum))))))))

#||
(paste-range -1 2 14)
||#
  
