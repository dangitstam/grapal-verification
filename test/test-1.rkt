
#lang rosette 

(require "../src/language.rkt")
(require "../src/interpreter.rkt")
(require "universe-1.rkt")

(define interpreter (make-interpreter all-elements all-relations))
(define MATCH (make-query-matcher interpreter))

(define (test-all-authors-that-have-authored-papers)
    (define query
        (MATCH (list
            (authors (author "a") (paper "p")))
            #:RETURN "a"))
    (define answer (make-hash))
    (hash-set! answer "a"
        (set (list-ref all-authors 0) (list-ref all-authors 1)
             (list-ref all-authors 3) (list-ref all-authors 4)
             (list-ref all-authors 5) (list-ref all-authors 9)))
    (assert (equal? query answer))
    (displayln "test-all-authors-that-have-authored-papers ✓"))

(define (test-all-papers-that-have-mentioned-entities)
    (define query
        (MATCH (list
            (mentions (paper "p") (entity "e")))
            #:RETURN "p"))
    (define answer (make-hash))
    (hash-set! answer "p"
        (set (list-ref all-papers 0) (list-ref all-papers 1) (list-ref all-papers 3)))
    (assert (equal? query answer))
    (displayln "test-all-papers-that-have-mentioned-entities ✓"))

(define (test-all-entities-that-have-mentioned)
    (define query
        (MATCH (list
            (mentions (paper "p") (entity "e")))
            #:RETURN "e"))
    (define answer (make-hash))
    (hash-set! answer "e"
        (set (list-ref all-entities 0) (list-ref all-entities 1) (list-ref all-entities 4)))
    (assert (equal? query answer))
    (displayln "test-all-entities-that-have-mentioned ✓"))

(test-all-authors-that-have-authored-papers)
(test-all-papers-that-have-mentioned-entities)
(test-all-entities-that-have-mentioned)
