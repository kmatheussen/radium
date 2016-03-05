

;; fxs format:
'((fxname1 ((pos1 value2 logtype) ;; Note that the fxname might clash when merging, different type of fx with the same name. This should not be a big problem though (it could even be a feature), since all values are between 0 and 1.
            (pos2 value2 logtype)
            ...))
  (fxname2 ...)
  ...)

(define (create-fx fxname fxnodes)
  (list fxname fxnodes))

(define (fx-name fx)
  (car fx))

(define (fx-nodes fx)
  (cadr fxs))

(define (find-fx-nodes fxs name)
  (let ((fx assq name fxs))
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
  
;; Returns the range fx for a track, skewed into 'startline'. All fx after 'endline' is not included.
(define (get-range-fxs rangetracknum startline endline)
  ...)

(define (get-track-fxs blocknum tracknum)
  ...)



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
                     (merge-fx-nodes (cdr track-nodes) range-nodes))
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
        (let* ((range-fx (car range-fxs))
               (name (car range-fx))               
               (track-fx (find-fx track-fxs name)))
          (if track-fx
              (cons (create-fx name
                               (merge-fx-nodes (cadr track-fx) (cadr range-fxs)))
                    (merge-fxs (remove-fx track-fxs name)
                               (cdr range-fxs)))
              (cons range-fx
                    (merge-fxs track-fxs
                               (cdr range-fxs)))))))

        

(define (paste-track-fxs blocknum tracknum fxs)
  ...)

(define (paste-range blocknum starttrack)
  (let loop ((rangetracknum 0))
    (define tracknum (+ rangetracknum starttrack))  
    (let ((range-fx (get-range-fx rangetracknum))
          (track-fx (get-track-fx blocknum tracknum)))
      (if (or (not range-fx)
              (not track-fx))
          #t
          (begin
            (paste-track-fx blocknum tracknum (merge-fx track-fx range-fx)))
            (loop (1+ rangetracknum)))))))
