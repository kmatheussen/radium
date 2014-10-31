;; run like this: GUILE_AUTO_COMPILE=0 guile --fresh-auto-compile test.scm
;;
;; If something goes wrong, Some "ERROR: ..." lines should be printed when the script is finished.


(define is-testing #t)
(load "common.scm")
(load "define-match.scm")


(define-macro (define-match funcname . matchers)
  (let ((ret (create-matcher-func funcname matchers)))
    (pretty-print ret)
    ret))


;;;;;;; Fibonacci

(define-match fib
  0 :> 0
  N :> 1 :where (= N 1)
  N :> (+ (fib (- N 1))
          (fib (- N 2))))

(test (fib 1) 1)
(test (fib 12) 144)
;;(test (fib 32) 2178309)


;;;;;;; Permutations

(define-match insert
  L        0 E :> (cons E L)
  (L . Ls) N E :> (cons L (insert Ls (- N 1) E)))
 
(test (insert '(2 3 4) 2 'a)
      '(2 3 a 4))

(define-match seq
  Same  Same  :> (list Same)
  Start End   :> (cons Start (seq (+ Start 1) End)))

(test (seq 5 8)
      '(5 6 7 8))
 
(define-match append-lists
  ()      :> '()
  (A . B) :> (append A (append-lists B)))

(test (append-lists '((2 3)(4 5)))
      '(2 3 4 5))
 
(define-match permutate
  (     ) :> '(())
  (H . T) :> (append-lists (map (lambda (P)
                                  (map (lambda (N)
                                           (insert P N H))
                                       (seq 0 (length P))))
                              (permutate T))))

(test (permutate '(2 3 4))
      '((2 3 4) (3 2 4) (3 4 2) (2 4 3) (4 2 3) (4 3 2)))


;;;;;;;; Quick sort

(define-match keep
  (        )  ____ :> '()
  (A . Rest) Pred :> (cons A (keep Rest Pred)) :where (Pred A)
  (_ . Rest) Pred :> (keep Rest Pred))
 
(define-match quicksort
  (     ) :> '()
  (A . R) :> (append (quicksort (keep R (lambda (B) (>= A B))))
                     (list A)
                     (quicksort (keep R (lambda (B) (< A B))))))
 
(test (quicksort '(6 8 5 9 3 2 2 1 4 7))
      '(1 2 2 3 4 5 6 7 8 9))



;;;;;;;;;; Pascal triangle

(define-match iterate
  _ _ 0 :> '()
  F V N :> (cons V (iterate F (F V) (1- N))))
 
(define-match next-row
  R :> (map + (cons 0 R) (append R '(0))))
 
(define-match pascal
  N :> (iterate next-row '(1) N))

(test (pascal 1)
      '((1)))

(test (pascal 4)
      '((1) (1 1) (1 2 1) (1 3 3 1)))




;;;;;;;;;;; Bubble sort

(define-match fix
  Func Value :> (let ((Result (Func Value)))
                  (if (equal? Value Result)
                      Value
                      (fix Func Result))))

(define-match bubble-shot
  (A      ) :> (cons A '())
  (A B . R) :> (cons B (bubble-shot (cons A R))) :where (> A B)
  (A   . R) :> (cons A (bubble-shot R)))
 
(define-match bubble-sort
  X :> (fix bubble-shot X))
 
(test (bubble-sort '(6 8 5 9 3 2 2 1 4 7))
      '(1 2 2 3 4 5 6 7 8 9))



;;;;;;;;;;; Selection sort

(define-match select-r
  Small (      ) Output :> (cons Small (selection-sort Output))
  Small (X . Xs) Output :> (select-r X Xs (cons Small Output)) :where (< X Small)
  Small (X . Xs) Output :> (select-r Small Xs (cons X  Output)))
 
(define-match selection-sort
  (           ) :> '()
  (First . Lst) :> (select-r First Lst '()))
 
(test (selection-sort '(8 7 7 4 3 2 0 9 1 5 6))
      '(0 1 2 3 4 5 6 7 7 8 9))


;;;;;;;;;;; Insertion sort

(define-match insert
  X (      ) :> (list X)
  X (Y . Ys) :> `(,X ,Y ,@Ys) :where (<= X Y)
  X (Y . Ys) :> `(,Y ,@(insert X Ys)))
 
(define-match insertion-sort
  (      ) :> '()
  (X . Xs) :> (insert X (insertion-sort Xs)))
 
(test (insertion-sort '(6 8 5 5 9 3 2 1 4 7))
      '(1 2 3 4 5 5 6 7 8 9))


;;;;;;;;;;; Merge sort

(define-match merge
  (      ) Ys       :> Ys
  Xs       (      ) :> Xs
  (X . Xt) (Y . Yt) :> (cons X (merge Xt (cons Y Yt))) :where (<= X Y)
  (X . Xt) (Y . Yt) :> (cons Y (merge (cons X Xt) Yt)))

#!
(track merge)
!#

(test (merge '(5 2) '(4 3))
      '(4 2 3 5))

(test (merge '(5 2 3) '(1 8 4))
      '(1 5 2 8 3 4))

(define-match split
  (        ) K :> (K `() '())
  (X       ) K :> (K `(,X) '())
  (X Y . Zs) K :> (split Zs (lambda (Xs Ys)
                            (K (cons X Xs)
                               (cons Y Ys)))))

(test (split '(2 1 3 5) list)
      '((2 3) (1 5)))

(define-match merge-sort
  ()  :> '(  )
  (X) :> `(,X)
  Xs  :> (split Xs (lambda (Xs Ys)
                     (merge (merge-sort Xs)
                            (merge-sort Ys)))))

(test (merge-sort '(3 2 1))
      '(1 2 3))

(test (merge-sort '(6 8 5 5 9 3 2 1 4 7))
      '(1 2 3 4 5 5 6 7 8 9))



;; Finally run the tests in the converter itself. We do this last so we don't override the original functions

;;(load "converter.scm")
