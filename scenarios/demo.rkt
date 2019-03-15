
#lang rosette 

(require racket/pretty)

(require "../src/language.rkt")
(require "../src/interpreter.rkt")
(require "../src/graph.rkt")
(require "fixtures/symbolic_database.rkt")
(require "../common/util.rkt")
(require "util.rkt")

(define interpreter (make-interpreter all-elements all-relations))
(define MATCH (make-query-matcher interpreter))


;; The universe and all relations will be printed for easy reference.
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

;; -------------------
;; Demos begin here!
;; -------------------

;; Expected output: unsat (a2 and a3 do not constrain a1 at all)
(define (demo-1)
  (verify (assert (equal?
    (MATCH (list
      (authors (author "a1") (paper "p1"))
      (cites (paper "p2") (paper "p1")))
      #:RETURN "a1")

    (MATCH (list
      (authors (author "a1") (paper "p1"))
      (authors (author "a2") (paper "p2"))
      (authors (author "a3") (paper "p3"))
      (affiliated-with (author "a3") (affiliation "aff1"))
      (cites (paper "p2") (paper "p1")))
      #:RETURN "a1")))))

;; These are not equivalent; there is a path from a2 to a1 via p2 and p1.
(define (demo-1-2)
  (verify (assert (equal?
    (MATCH (list
      (authors (author "a1") (paper "p1"))
      (cites (paper "p2") (paper "p1")))
      #:RETURN "a1")

    (MATCH (list
      (authors (author "a1") (paper "p1"))
      (authors (author "a2") (paper "p2"))
      (authors (author "a3") (paper "p3"))
      (affiliated-with (author "a2") (affiliation "aff1"))
      (cites (paper "p2") (paper "p1")))
      #:RETURN "a1")))))

;; Expected output: A single assignment of true to add an author/paper
;; relation containing a paper newer than 1997.
;;
;; If you search for `[p$9 #t]` in the printed relations before the demo
;; query outputs, you will find that it properly adds a paper from 1997
;; in order to prove the queries are not equivalent given the fixed
;; universe!
(define (demo-2)
  (verify (assert (equal?
    (MATCH (list
      (authors (author "a1") 
               (paper "p1" #:constrain (year-greater-than 1997))))
      #:RETURN "a1")

    (MATCH (list
      (authors (author "a1") (paper "p1")))
      #:RETURN "a1")))))

;; It seems that these queries would do the same thing but they don't.
;; In the second query, a1 is restricted to authors that have authored papers
;; from both 1975 and 2001.
;;
;; The counterexample will allow two authors to have written a paper: one will
;; publish in 1975 and the other will publish in both 1975 and 2001. Note that
;; this results in two different authors!
(define (demo-3)
  (verify (assert (equal?
    (MATCH (list
      (authors (author "a1") 
               (paper "p1" #:constrain (year-less-than 2018))))
      #:RETURN "a1")

    (MATCH (list
      (authors (author "a1") 
               (paper "p1" #:constrain (year-equal-to 1975)))
      (authors (author "a1") 
               (paper "p2" #:constrain (year-equal-to 2001))))
      #:RETURN "a1")))))


(displayln "Demo 1 ----------------------------------------------------------")
(demo-1)

(displayln "Demo 1.2 ----------------------------------------------------------")
(demo-1-2)

(displayln "Demo 2 ----------------------------------------------------------")
(demo-2)

(displayln "Demo 3 ----------------------------------------------------------")
(demo-3)
