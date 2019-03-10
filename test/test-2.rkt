
#lang rosette 

(require "../src/language.rkt")
(require "../src/interpreter.rkt")
(require "fixtures/example_database.rkt")
(require "util.rkt")

(define interpreter (make-interpreter all-elements all-relations))
(define MATCH (make-query-matcher interpreter))


(define (test-constrain-author-via-constrained-paper)
    (define query
        (MATCH (list
            (authors (author "a") (paper "p")) 
            (mentions (paper "p") (entity "e" #:constrain (name 4))))
            #:RETURN "a"))
    (define answer
        (list (list (list-ref all-authors 5) (list-ref all-authors 9))))
    (assert (queries-equal? query answer))
    (displayln "test-constrain-author-via-constrained-paper ✓"))

(define (test-constrain-author-via-paper-constrained-via-venue)
    (define query
        (MATCH (list
            (authors (author "a") (paper "p")) 
            (appears-in (paper "p") (venue "v" #:constrain (text 0))))
            #:RETURN "a"))
    (define answer
        (list (list (list-ref all-authors 4)
                    (list-ref all-authors 5)
                    (list-ref all-authors 9))))
    (assert (queries-equal? query answer))
    (displayln "test-constrain-author-via-paper-constrained-via-venue ✓"))

(define (test-constrain-author-via-paper-constrained-via-paper)
    (define query
        (MATCH (list
            (authors (author "a") (paper "p1")) 
            (cites (paper "p1") (paper "p2" #:constrain (title 1))))
            #:RETURN "a"))
    (define answer
        (list (list (list-ref all-authors 0)
                    (list-ref all-authors 1)
                    (list-ref all-authors 4))))
    (assert (queries-equal? query answer))
    (displayln "test-constrain-author-via-paper-constrained-via-paper ✓"))

(define (test-constrain-author-via-constrained-affiliation)
    (define query
        (MATCH (list
            (affiliated-with (author "a1") (affiliation "a2" #:constrain (text 2))))
            #:RETURN "a1"))
    (define answer
        (list
            (list (list-ref all-authors 0)
                 (list-ref all-authors 1)
                 (list-ref all-authors 3))))
    (assert (queries-equal? query answer))
    (displayln "test-constrain-author-via-constrained-affiliation ✓"))

;; Constrain papers
(define (test-constrain-paper-via-constrained-author)
    (define query
        (MATCH (list
            (authors (author "a" #:constrain (first 3)) (paper "p")))
            #:RETURN "p"))
    (define answer
        (list (list (list-ref all-papers 1))))
    (assert (queries-equal? query answer))
    (displayln "test-constrain-paper-via-constrained-author ✓"))


(displayln "Constrain via constrained dependent --------------------------------------------")
(test-constrain-author-via-constrained-paper)
(test-constrain-author-via-constrained-affiliation)
(test-constrain-author-via-paper-constrained-via-venue)
(test-constrain-author-via-paper-constrained-via-paper)
(test-constrain-paper-via-constrained-author)