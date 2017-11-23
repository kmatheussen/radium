
;; Standard binary search. Used in common/SeqAutomation.hpp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (binsearch vector value low high)
  (c-display low high)
  (if (< high low)
      low
      (let ((mid (floor (/ (+ low high) 2))))
        (if (>= (vector mid) value)
            (binsearch vector value low (1- mid))
            (binsearch vector value (1+ mid) high)))))

(binsearch (vector 0.5 2.3 4.5) 4.5 0 2))





;; Algorithm used to store peaks. Lookup is O(log N).
;; Used in audio/Peaks.hpp.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct arraysum
  :div ;; Contains the factor we need to multiply the current div with to get the granularity of up.
  :up
  :values)


(define (get-array-sum array start end)
  (define len (vector-length array))
  (assert (< start len))
  (set! end (min len end))
  (let loop ((i start)
             (sum 0))
    (if (= i end)
        sum
        (loop (1+ i)
              (+ sum
                 (vector-ref array i))))))
               
(define (make-arraysum2 div values1)
  (define len1 (vector-length values1))
  (assert (>= len1 1))

  (if (< len1 div)
      (make-arraysum :div div
                     :up #f
                     :values values1)
      (begin
        (define len2 (floor (/ len1 div)))
        (define values2 (make-vector len2))
        
        (let loop ((i2 0))
          (when (< i2 len2)
            (vector-set! values2 i2 (get-array-sum values1
                                                   (* i2 div)
                                                   (+ (* i2 div)
                                                      div)))
            (loop (1+ i2))))
        
        (make-arraysum :div div
                       :up (make-arraysum2 div values2)
                       :values values1))))

(pp (make-arraysum2 4 (vector 1 2 3 4 5 6 7 8 9 10)))


#!!
->
(make-arraysum :div 2
               :up #f
               :values 3 7 18)

!!#

(define (arraysum-sum arraysum start end)
  (let find-sum ((arraysum arraysum)
                 (start start)
                 (end end))
    
    (define div (arraysum :div))

    (define next-array-start (unit-ceiling start div))
    (define next-array-end (unit-floor end div))

    (c-display "div:" div "start - array-start - array-end - end:" start next-array-start next-array-end end "arraysum:" (pp arraysum))
    
    (if (and (> next-array-end next-array-start)
             (arraysum :up))
        (append (sublist (to-list (arraysum :values))
                         start
                         next-array-start)
                (find-sum (arraysum :up)
                          (/ next-array-start div)
                          (/ next-array-end div))
                (sublist (to-list (arraysum :values))
                         next-array-end
                         end))
        (sublist (to-list (arraysum :values))
                 start
                 end))))

            


                       
(define testarraysum (make-arraysum2 2 (vector 1 2 3 4 5 6 7 8 9 10 11)))
(pretty-print testarraysum)
(arraysum-sum testarraysum
              0 4)
(arraysum-sum testarraysum
              1 10)
(arraysum-sum testarraysum
              1 11)

(***assert*** (arraysum-sum testarraysum
                            0 1)
              (list 1))

(***assert*** (arraysum-sum testarraysum
                            0 2)
              (list 3))

(***assert*** (arraysum-sum testarraysum
                            0 3)
              (list 3 3))

(***assert*** (arraysum-sum testarraysum
                            0 4)
              (list 10))

(***assert*** (arraysum-sum testarraysum
                            1 2)
              (list 2))

(***assert*** (arraysum-sum testarraysum
                            1 3)
              (list 2 3))

(***assert*** (arraysum-sum testarraysum
                            1 4)
              (list 2 7))

(***assert*** (arraysum-sum testarraysum
                            1 7)
              (list 2 7 11 7))

(***assert*** (arraysum-sum testarraysum
                            1 10)
              (list 2 7 26 19))

(***assert*** (arraysum-sum testarraysum
                            1 11)
              (list 2 7 26 19 11))


  

