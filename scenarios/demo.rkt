
#lang rosette 

(require racket/pretty)

(require "../src/language.rkt")
(require "../src/interpreter.rkt")
(require "../src/graph.rkt")
(require "fixtures/symbolic_database.rkt")
(require "../common/util.rkt")

(define interpreter (make-interpreter all-elements all-relations))
(define MATCH (make-query-matcher interpreter))

#|

Papers 0 - 5 have years 1 - 5 respectively. Restricting year to be > 2 should have the same
effect as restricting to papers 3, 4, and 5.
|#

; (define cite-equivalent-1
;     (MATCH (list
;         (authors (author "a1") (paper "p1" #:constrain (year-greater-than 2))))
;         #:RETURN "a1"))

; (define cite-equivalent-2
;     (MATCH (list
;         (authors (author "a1") (paper "p1" #:constrain (title 2)))
;         (authors (author "a2") (paper "p2" #:constrain (title 3)))
;         (authors (author "a3") (paper "p3" #:constrain (title 4))))
;         #:RETURN (list "a1" "a2" "a3")))

; (println cite-equivalent-1)
; (println cite-equivalent-2)

; (assert (equal? (list->set (flatten cite-equivalent-1)))
;                 (list->set (flatten cite-equivalent-2)))

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

(pretty-print all-elements)

(displayln "The Authors Relation ------------------------------------------")
(pretty-print
    (nth-places 1 (relations-authors all-relations)))
(displayln "The Mentions Relation ------------------------------------------")
(pretty-print
    (nth-places 1 (relations-mentions all-relations)))
(displayln "The Cites Relation ------------------------------------------")
(pretty-print
    (nth-places 1 (relations-cites all-relations)))
    (displayln "The Affiliated-With Relation -------------------------------------")
(pretty-print
    (nth-places 1 (relations-affiliated-with all-relations)))
    (displayln "The Appears-In Relation ------------------------------------------")
(pretty-print
    (nth-places 1 (relations-appears-in all-relations)))

;; Two queries that look similar but are not equivalent, why?
;; - The first query restricts authors that have authored papers after 1997.
;; - The second query restricts authors that have authored papers in both 2001
;;   and 2018.
(verify (assert (equal?
    (MATCH (list
        (authors (author "a1") (paper "p1" #:constrain (year-greater-than 1997))))
        #:RETURN "a1")

    (MATCH (list
        (authors (author "a1") (paper "p1")))
        #:RETURN "a1"))))

; (println 
;     (MATCH (list
;         (authors (author "a1") (paper "p1"))
;         (authors (author "a2") (paper "p1")))
;         #:RETURN "a1"))

    #|
        Turn this into a binary tree such that the root is the return node (simplify to single return).

        Once the tree is built, 
    
    |#

(verify (equal?
    (MATCH (list
        (authors (author "a1") (paper "p1"))
        (cites (paper "a2") (paper "p1")))
        #:RETURN "a1")

    (MATCH (list
        (authors (author "a1") (paper "p1"))
        (cites (paper "a2") (paper "p1")))
        #:RETURN "a1")
    )
)