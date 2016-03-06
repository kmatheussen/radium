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

(define (find-fx-nodes fxs name)
  (let ((fx (assq name fxs)))
    (and fx
         (fx-nodes fx))))

(define (remove-fx fxs name)
  (if (null? fxs)
      '()
      (let ((fx (car fxs)))
        (if (eq? (fx-name fx))
            (cdr fxs)
            (cons fx
                  (remove-fx (cdr fxs) name))))))
  

;; Returns the range fx for a track, skewed into 'startplace'. All fx after 'endplace' is not included.
(define (get-range-fxs rangetracknum startplace endplace)
  (map (lambda (fxnum)
         (define fxname (<ra> :get-fxrange-name fxnum rangetracknum))
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
                                         (if (= (<ra> :get-logtype-hold)
                                                (fxnode-logtype last-fxnode))
                                             (fxnode-value last-fxnode)
                                             (create-fxnode endplace
                                                            (scale endplace
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
(get-range-fxs 0 10 14)
||#

(define (get-track-fxs blocknum tracknum)
  (map (lambda (fxnum)
         (define fxname (<ra> :get-fx-name fxnum tracknum blocknum))
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
  (cond ((null? range-nodes)
         track-nodes)
        ((null? track-nodes)
         range-nodes)
        (else
         (let* ((a (car track-nodes))
                (b (car range-nodes))
                (pos-a (car a))
                (pos-b (car b)))
           (if (< pos-a pos-b)
               (cons a
                     (merge-fx-nodes (cdr track-nodes) range-nodes)) ;; need to add a HOLD node.
               (let ((last-range-pos (car (last range-nodes))))
                 (append range-nodes
                         (remove-while track-nodes
                                       (lambda (track-node)
                                         (<= (car track-node)
                                             last-range-pos))))))))))

(***assert*** (merge-fx-nodes '((2 11 0)
                                (3 12 0))
                              '((5 20 0)
                                (6 22 0)
                                (7 24 0)))
              '((2 11 0)
                (3 12 0)
                (5 20 0)
                (6 22 0)
                (7 24 0)))

(***assert*** (merge-fx-nodes '((2 11 0)
                                (5 12 0))
                              '((5 20 0)
                                (6 22 0)
                                (7 24 0)))
              '((2 11 0)
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
              '((1 10 0)
                (2 22 0)
                (3 24 0)))

(***assert*** (merge-fx-nodes '((1 10 0)
                                (2 11 0)
                                (3 12 0)
                                (4 13 0))
                              '((2 22 0)
                                (3 24 0)))
              '((1 10 0)
                (2 22 0)
                (3 24 0)
                (4 13 0)))

  
  
(define (merge-fxs track-fxs range-fxs)
  (cond ((null? track-fxs)
         range-fxs)
        ((null? range-fxs)
         track-fxs)
        (else
         (let* ((range-fx (car range-fxs))
                (name (car range-fx))               
                (track-fx (find-fx-nodes track-fxs name)))
           (if track-fx
               (cons (create-fx name
                                (merge-fx-nodes (cadr track-fx) (cadr range-fxs)))
                     (merge-fxs (remove-fx track-fxs name)
                                (cdr range-fxs)))
               (cons range-fx
                     (merge-fxs track-fxs
                                (cdr range-fxs))))))))

        
;; Remember: Check that number of nodes >= 2.
(define (paste-track-fxs blocknum tracknum fxs)
  (<ra> :clear-track-fx tracknum blocknum)
  (for-each (lambda (fx)
              (define name (fx-name fx))  ;; Need to check if this fxname is available for the track.
              (define fx-nodes (fx-nodes fx))              
              (when (>= (length fx-nodes) 2)
                (define fx-node (car fx-nodes))
                (define fxnum (<ra> :create-fx3
                                    (fxnode-value fx-node)
                                    (fxnode-place fx-node)
                                    name
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
            fxs))

(define (paste-range blocknum starttrack startplace)
  (assert (>= starttrack 0))
  
  (define endplace (- (<ra> :get-num-lines blocknum)
                      (/ 1
                         (<ra> :get-highest-legal-place-denominator))))
  (let loop ((rangetracknum 0))
    (if (>= rangetracknum (<ra> :get-num-tracks-in-range))
        #t
        (let* ((tracknum (+ rangetracknum starttrack))
               (range-fxs (get-range-fxs rangetracknum startplace endplace))
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
  
