#lang rosette


(define (first-contains-second l1 l2)
    (define contained
        (map (lambda (v) (member v l2)) l1))
    (foldl (lambda (x y) (and x y)) #t contained))

(define (queries-equal? q1 q2)
    (if (= (length q1) (length q1))
        (let ([list-equalities
                (map (lambda (l1 l2)
                        (and ; (= (length l1) (length l2))
                             (first-contains-second l1 l2)
                             (first-contains-second l2 l1))) q1 q2)])
            (foldl (lambda (x y) (and x y)) #t list-equalities))
        #f))
(provide queries-equal?)


