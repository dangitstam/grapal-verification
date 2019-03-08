
#lang rosette 

(require "../src/language.rkt")
(require "../src/interpreter.rkt")
(require "universe-1.rkt")

(define interpreter (make-interpreter all-elements all-relations))
(define MATCH (make-query-matcher interpreter))


(define (test-constrain-author-via-constrained-paper)
    (define query
        (MATCH (list
            (authors (author "a") (paper "p")) 
            (mentions (paper "p") (entity "e" #:constrain (name 4))))
            #:RETURN "a"))
    (define answer (make-hash))
    (hash-set! answer "a"
        (set (list-ref all-authors 5) (list-ref all-authors 9)))
    (assert (equal? query answer))
    (displayln "test-constrain-author-via-constrained-paper ✓"))

(displayln "Constrain via constrained dependent --------------------------------------------")
(test-constrain-author-via-constrained-paper)
