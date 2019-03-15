#lang rosette

(provide (all-defined-out))

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

;; For pretty-printing the relations and the variables
;; they map to for quick reference.
;; Courtesy of https://stackoverflow.com/a/32582903,
(define (nth-places n lst [i 0])
  (cond
    [(null? lst) null]
    [(= i 0) (cons (car lst)
                   (nth-places n (cdr lst) (+ i 1)))]
    [(= i n) (nth-places n (cdr lst) 0)]
    [else (nth-places n (cdr lst) (+ i 1))]))