#lang rosette

(define (make-symbolic-relation-from-pairs pairs)
    (if (null? pairs)
        null
        (begin
            (define-symbolic* p boolean?)
            ;; Adding both orders makes the relation symmetric.
            (let* ([forward (car pairs)]
                   [backward (list (list-ref forward 1) (list-ref forward 0))])
                (append
                    (list (cons forward p) (cons backward p))
                    (make-symbolic-relation-from-pairs (cdr pairs)))))))
(provide make-symbolic-relation-from-pairs)
