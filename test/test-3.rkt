
#lang rosette 

(require "../src/language.rkt")
(require "../src/interpreter.rkt")
(require "fixtures/example_database.rkt")
(require "../common/util.rkt")

(define interpreter (make-interpreter all-elements all-relations))
(define MATCH (make-query-matcher interpreter))

#|

Papers 0 - 5 have years 1 - 5 respectively. Restricting year to be > 2 should have the same
effect as restricting to papers 3, 4, and 5.
|#

(define cite-equivalent-1
    (MATCH (list
        (authors (author "a1") (paper "p1" #:constrain (year-greater-than 2))))
        #:RETURN "a1"))

(define cite-equivalent-2
    (MATCH (list
        (authors (author "a1") (paper "p1" #:constrain (title 2)))
        (authors (author "a2") (paper "p2" #:constrain (title 3)))
        (authors (author "a3") (paper "p3" #:constrain (title 4))))
        #:RETURN (list "a1" "a2" "a3")))

(println cite-equivalent-1)
(println cite-equivalent-2)