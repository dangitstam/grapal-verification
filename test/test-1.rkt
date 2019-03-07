
#lang rosette 

(require "../src/language.rkt")
(require "../src/interpreter.rkt")
(require "universe-1.rkt")

(define interpreter (make-interpreter all-elements all-relations))
(define MATCH (make-query-matcher interpreter))

(define test
    (MATCH (list
        (authors (author "a") (paper "p")) 
        (mentions (paper "p") (entity "e" #:constrain (name 1))))
        #:RETURN "p"))

(println test)