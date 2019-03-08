
#lang rosette 

(require "../src/language.rkt")
(require "../src/interpreter.rkt")
(require "universe-1.rkt")

(define interpreter (make-interpreter all-elements all-relations))
(define MATCH (make-query-matcher interpreter))

(define (test-all-authors)
    (define query
        (MATCH (list (author "a"))
            #:RETURN "a"))
    (define answer (make-hash))
    (hash-set! answer "a"
        (list->set all-authors))
    (assert (equal? query answer))
    (displayln "test-all-authors ✓"))

(define (test-all-papers)
    (define query
        (MATCH (list (paper "a"))
            #:RETURN "a"))
    (define answer (make-hash))
    (hash-set! answer "a"
        (list->set all-papers))
    (assert (equal? query answer))
    (displayln "test-all-papers ✓"))

(define (test-all-entities)
    (define query
        (MATCH (list (entity "a"))
            #:RETURN "a"))
    (define answer (make-hash))
    (hash-set! answer "a"
        (list->set all-entities))
    (assert (equal? query answer))
    (displayln "test-all-entities ✓"))

(define (test-all-affiliations)
    (define query
        (MATCH (list (affiliation "a"))
            #:RETURN "a"))
    (define answer (make-hash))
    (hash-set! answer "a"
        (list->set all-affiliations))
    (assert (equal? query answer))
    (displayln "test-all-affiliations ✓"))

(define (test-all-venues)
    (define query
        (MATCH (list (venue "a"))
            #:RETURN "a"))
    (define answer (make-hash))
    (hash-set! answer "a"
        (list->set all-venues))
    (assert (equal? query answer))
    (displayln "test-all-venues ✓"))

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
        (set (list-ref all-papers 0)
             (list-ref all-papers 1)
             (list-ref all-papers 3)))
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

(displayln "Single-constraint unit tests ---------------------------------------------------")
(test-all-authors)
(test-all-papers)
(test-all-entities)
(test-all-affiliations)
(test-all-venues)
(test-all-authors-that-have-authored-papers)
(test-all-papers-that-have-mentioned-entities)
(test-all-entities-that-have-mentioned)
