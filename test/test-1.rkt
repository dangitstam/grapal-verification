
#lang rosette 

(require "../src/language.rkt")
(require "../src/interpreter.rkt")
(require "fixtures/example_database.rkt")
(require "util.rkt")

(define interpreter (make-interpreter all-elements all-relations))
(define MATCH (make-query-matcher interpreter))

(define (test-all-authors)
    (define query
        (MATCH (list (author "a"))
            #:RETURN "a"))
    (define answer
        (list all-authors))
    (assert (queries-equal? query answer))
    (displayln "test-all-authors ✓"))

(define (test-all-papers)
    (define query
        (MATCH (list (paper "a"))
            #:RETURN "a"))
    (define answer
        (list all-papers))
    (assert (queries-equal? query answer))
    (displayln "test-all-papers ✓"))

(define (test-all-entities)
    (define query
        (MATCH (list (entity "a"))
            #:RETURN "a"))
    (define answer
        (list all-entities))
    (assert (queries-equal? query answer))
    (displayln "test-all-entities ✓"))

(define (test-all-affiliations)
    (define query
        (MATCH (list (affiliation "a"))
            #:RETURN "a"))
    (define answer
        (list all-affiliations))
    (assert (queries-equal? query answer))
    (displayln "test-all-affiliations ✓"))

(define (test-all-venues)
    (define query
        (MATCH (list (venue "a"))
            #:RETURN "a"))
    (define answer
        (list all-venues))
    (assert (queries-equal? query answer))
    (displayln "test-all-venues ✓"))

(define (test-all-authors-that-have-authored-papers)
    (define query
        (MATCH (list
            (authors (author "a") (paper "p")))
            #:RETURN (list "a")))
    (define answer 
        (list (list (list-ref all-authors 0) (list-ref all-authors 1)
                    (list-ref all-authors 3) (list-ref all-authors 4)
                    (list-ref all-authors 5) (list-ref all-authors 9))))
    (assert (queries-equal? query answer))
    (displayln "test-all-authors-that-have-authored-papers ✓"))

(define (test-all-papers-that-have-mentioned-entities)
    (define query
        (MATCH (list
            (mentions (paper "p") (entity "e")))
            #:RETURN "p"))
    (define answer
        (list (list (list-ref all-papers 0)
                    (list-ref all-papers 1)
                    (list-ref all-papers 3))))
    (assert (queries-equal? query answer))
    (displayln "test-all-papers-that-have-mentioned-entities ✓"))

(define (test-all-entities-that-have-mentioned)
    (define query
        (MATCH (list
            (mentions (paper "p") (entity "e")))
            #:RETURN "e"))
    (define answer
        (list (list (list-ref all-entities 0)
                    (list-ref all-entities 1)
                    (list-ref all-entities 4))))
    (assert (queries-equal? query answer))
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
