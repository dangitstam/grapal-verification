
#lang rosette 

(require "../src/language.rkt")
(require "../src/interpreter.rkt")
(require "fixtures/symbolic_database.rkt")
(require "../common/util.rkt")

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


(define ())


(displayln "Single-constraint unit tests ---------------------------------------------------")
(test-all-authors)
(test-all-papers)
(test-all-entities)
(test-all-affiliations)
(test-all-venues)

