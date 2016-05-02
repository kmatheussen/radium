(provide 'nodes.scm)

(define (create-node place value)
  (list place value))

(define (node-place node)
  (car node))

(define (node-value node)
  (cadr node))

(define (nodelist-add-same-value-at-place nodelist place default-value)
  (let loop ((last-value default-value)
             (nodelist nodelist))
    (if (null? nodelist)
        (list (create-node place last-value))
        (let ((node (car nodelist)))
          (cond ((> (node-place node)
                    place)
                 (cons (create-node place last-value)
                       nodelist))
                ((= (node-place node)
                    place)
                 nodelist)
                (else
                 (cons node
                       (loop (node-value node)
                             (cdr nodelist)))))))))

(***assert*** (nodelist-add-same-value-at-place '() 5 2)
              (list (create-node 5 2)))
              
(***assert*** (nodelist-add-same-value-at-place (list (create-node 10 5))
                                                7 2)
              (list (create-node 7 2)
                    (create-node 10 5)))

(***assert*** (nodelist-add-same-value-at-place (list (create-node 10 5))
                                                70 2)
              (list (create-node 10 5)
                    (create-node 70 5)))

(***assert*** (nodelist-add-same-value-at-place (list (create-node 10 5))
                                                10 2)
              (list (create-node 10 5)))

(***assert*** (nodelist-add-same-value-at-place (list (create-node 10 5)
                                                      (create-node 20 7))
                                                10 2)
              (list (create-node 10 5)
                    (create-node 20 7)))

(***assert*** (nodelist-add-same-value-at-place (list (create-node 10 5)
                                                      (create-node 20 7))
                                                13 2)
              (list (create-node 10 5)
                    (create-node 13 5)
                    (create-node 20 7)))

