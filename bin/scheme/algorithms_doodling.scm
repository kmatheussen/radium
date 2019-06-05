;; Standard binary search. Used in common/SeqAutomation.hpp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#||


(define (binsearch vector value low high)
  (c-display low high)
  (if (< high low)
      low
      (let ((mid (floor (/ (+ low high) 2))))
        (if (>= (vector mid) value)
            (binsearch vector value low (1- mid))
            (binsearch vector value (1+ mid) high)))))

(binsearch (vector 1 3 8 12 22)
           -1
           0
           4))




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


||#


;; Algorithm to find constant value to multiply stretch with when a seqblock is automating stretch values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define-struct granresampler
  :pos 0
  :read-pos 0
  :curr-sample 0
  :compensation 1
  :get-next-sample)

#!!
(define granulator (make-granulator))
(set! (granulator :pos) 5)
(granulator :pos)
!!#

(define (granresample granresampler get-ratio outsample-pos)
  (define ratio (* (granresampler :compensation)
                   (get-ratio outsample-pos))) ;;(granresampler :read-pos)))
  (set! (granresampler :read-pos) (+ 1 (granresampler :read-pos)))
  
  (while (>= (granresampler :pos) 0)
    (let* ((value ((granresampler :get-next-sample) outsample-pos)))
      (set! (granresampler :curr-sample) value)
      (set! (granresampler :pos) (- (granresampler :pos) 1))))
  
  (set! (granresampler :pos) (+ (granresampler :pos) ratio))
  (granresampler :curr-sample))

(define (make-get-ratio nodes)
  (lambda (time)
    (let loop ((nodes nodes))
      (define node1 (car nodes))
      (define node2 (cl-cadr nodes))
      (define time1 (car node1))
      (define time2 (cl-car node2))
      (define value1 (cadr node1))
      (define value2 (cl-cadr node2))
      (cond ((<= time time1)
             value1) ;; before start
            ((not time2)
             value1) ;; after end
            ((> time time2)
             (loop (cdr nodes)))
            (else
             (scale time time1 time2 value1 value2))))))

(define (make-get-sample-for-resampler)
  (define insample-pos -1)
  (lambda (outsample-pos)
    (set! insample-pos (1+ insample-pos))
    insample-pos))
;;    (list "insample" insample-pos)))

(define (make-get-sample-for-granulator resampler get-resampler-ratio)
  (lambda (n)
    (granresample resampler get-resampler-ratio n)))

(define (calculate-duration1 input-duration resampler-nodes granulate-nodes)
  (define get-resampler-ratio (make-get-ratio resampler-nodes))
  (define get-granulator-ratio (make-get-ratio granulate-nodes))
  (let loop ((in-time 0)
             (granulation-time 0)
             (resampler-time 0)
             (out-time 0))
    (if (>= in-time input-duration)
        (list out-time
              granulation-time
              resampler-time)
        (begin
          (define granulation-ratio (get-granulator-ratio in-time))
          (define resampler-ratio (get-resampler-ratio in-time))
          (loop (+ 1 in-time)
                (+ granulation-time (/ 1
                                       granulation-ratio))
                (+ resampler-time (/ 1
                                     resampler-ratio))
                (+ out-time (/ 1
                               (* granulation-ratio
                                  resampler-ratio))))))))
        
(define (calculate-duration2 input-duration resampler-nodes granulate-nodes)
  (define get-resampler-ratio (make-get-ratio resampler-nodes))
  (define get-granulator-ratio (make-get-ratio granulate-nodes))
  (let loop ((in-time 0)
             (out-time 0))
    (if (>= in-time input-duration)
        out-time
        (begin
          (define granulation-ratio (get-granulator-ratio in-time))
          (define resampler-ratio (get-resampler-ratio in-time))
          (loop (+ 1 in-time)
                (+ out-time (/ 1
                               (* granulation-ratio
                                  resampler-ratio))))))))
        
(define (calculate-duration3 input-duration resampler-nodes granulate-nodes)
  (define get-resampler-ratio (make-get-ratio resampler-nodes))
  (define get-granulator-ratio (make-get-ratio granulate-nodes))

  (define gran-pos 0)
  (define resample-pos 0)
  
  (define sample-read-pos 0)

  (define out-pos 0)
  
  (while (< out-pos input-duration)
    (define ratio-pos out-pos)
    (define granulation-ratio (get-granulator-ratio ratio-pos))
    (define resampler-ratio (get-resampler-ratio ratio-pos))

    (while (>= gran-pos 0)
    
      (while (>= resample-pos 0)
        ;;(c-display gran-pos resample-pos granulation-ratio resampler-ratio)
        (inc! resample-pos (- resampler-ratio))
        (inc! sample-read-pos 1))
      
      (inc! resample-pos 1)
      
      (inc! gran-pos (- granulation-ratio)))
  
    (inc! gran-pos 1)
    
    (inc! out-pos 1))

  sample-read-pos)
        

(let* ((len 500)
       ;(resampler-nodes `((0 15)(,len 1.5)))  ;`((0 1.1)
       ;                  ; (,(/ len 2) 100)
       ;                  ; (,len 10.8)))
       ;(granulate-nodes `((0 0.8)
       ;                   (,(/ len 2) 0.1)
       ;                   (,len 0.2)))
       ;;(resampler-nodes `((0 1.0)
       ;;                   (,(/ len 3) 0.02)
       ;;                   (,len 1.0)))
       ;;(granulate-nodes `((0 50.0)
       ;;                   (,(/ len 4) 1.0)
       ;;                   (,len 50.0)))
       (resampler-nodes `((0 50.0)
                          (,len 1.0)))
       (granulate-nodes `((0 1.0)
                          (,len 0.02)))
       (get-resampler-ratio (make-get-ratio resampler-nodes))
       (get-granulator-ratio (make-get-ratio granulate-nodes))
       (resampler (make-granresampler :get-next-sample (make-get-sample-for-resampler)))
       (granulator (make-granresampler :get-next-sample (make-get-sample-for-granulator resampler get-resampler-ratio))))

  (define temp (calculate-duration1 len resampler-nodes granulate-nodes))
  (define output-duration1 (car temp))
  (define granulation-time (cadr temp))
  (define resampler-time (caddr temp))
  (define granulation-compensation (/ granulation-time len))
  (define resampler-compensation (/ output-duration1 granulation-time)) ;;(/ resampler-time len))
  (define output-duration2 (calculate-duration2 len resampler-nodes granulate-nodes))
  (define output-duration3 (calculate-duration3 len resampler-nodes granulate-nodes))

  ;;(set! (granulator :compensation) granulation-compensation)
  ;;(set! (resampler :compensation) resampler-compensation)
  
  (define (calculate resampler granulator)
    (newline)
    (let loop ((n 0)
               (insample-pos 0))
      (set! insample-pos (granresample granulator get-granulator-ratio insample-pos))
      (c-display "Calculated duration:" (two-decimal-string output-duration1)
                 ;;"Gr c:" (two-decimal-string granulation-compensation)
                 ;;"Re c:" (two-decimal-string resampler-compensation)
                 "Dur3:" (two-decimal-string output-duration3)
                 "Counted duration:" n (list "insample" insample-pos)
                 granulator
                 )
      (if (< insample-pos len)
          (loop (+ n 1)
                insample-pos))))
  
  (calculate resampler granulator))


  


  

