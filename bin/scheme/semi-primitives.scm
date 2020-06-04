(define (remove func list*)
  (if (null? list*)
      '()
      (if (func (car list*))
          (remove func (cdr list*))
          (cons (car list*)
                (remove func (cdr list*))))))

(define (sort sequence less?)
  (sort! (copy sequence) less?))

(define (list-position list* is-this-it?)
  (let loop ((n 0)
             (l list*))
    (cond ((null? l)
           -1)
          ((is-this-it? (car l))
           n)
          (else
           (loop (1+ n)
                 (cdr l))))))
  
(define (vector-position list* is-this-it?)
  (let loop ((n 0))
    (cond ((= n (vector-length list*))
           -1)
          ((is-this-it? (list* n))
           n)
          (else
           (loop (1+ n))))))
  
(define (get-bool something)
  (if something
      #t
      #f))

(define (scale x x1 x2 y1 y2)
  (+ y1 (/ (* (- x x1)
              (- y2 y1))
           (- x2 x1))))

(define (X/Y a b x y)
  (+ a (/ (* x (- b a))
          y)))

(define (average . numbers)
  (/ (apply + numbers)
     (length numbers)))

(define (onethird a b)
  (X/Y a b 1 3))
(define (twothirds a b)
  (X/Y a b 2 3))
         
(define (twofifths a b)
  (X/Y a b 2 5))
(define (threefifths a b)
  (X/Y a b 3 5))

(define (between Min Try-it Max)
  (cond ((< Try-it Min)
         Min)
        ((> Try-it Max)
         Max)
        (else
         Try-it)))     

(define (myrand low high)
  (scale (random 100000) 0 100000 low high))

(define (integer-myrand low high)
  (+ low (random (1+ (- high low)))))

#!!
(integer-myrand 0 1)
!!#



(c-define-expansion (*inc!* var how-much)
  `(set! ,var (+ ,var ,how-much)))

(c-define-expansion (*push-back!* list* . elements)
  `(set! ,list* (append ,list* (list ,@elements))))


(define (delete-from das-list element)
  (assert (not (null? das-list)))
  (if (equal? (car das-list) element)
      (cdr das-list)
      (cons (car das-list)
            (delete-from (cdr das-list) element))))

(define (delete-from2 das-list element)
  (assert (not (null? das-list)))
  (if (equal? (car das-list) element)
      (cdr das-list)
      (cons (car das-list)
            (delete-from (cdr das-list) element))))

(define (delete-list-from das-list elements)
  (if (null? elements)
      das-list
      (delete-list-from (delete-from das-list (car elements))
                        (cdr elements))))


