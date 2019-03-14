
#lang rosette 

(require racket/pretty)

(require "../src/language.rkt")
(require "../src/interpreter.rkt")
(require "../src/graph.rkt")
(require "fixtures/symbolic_database.rkt")
(require "../common/util.rkt")

(define interpreter (make-interpreter all-elements all-relations))
(define MATCH (make-query-matcher interpreter))


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

;; Given a query to Rosette, if the result is a model,
;; print only the true booleans.
(define (display-positive-switches cex)
  (if (unsat? cex)
    cex
    (let* ([pairs (hash->list (model cex))]
         [positive-switches (filter (lambda (x) (cdr x)) pairs)])
      (if (null? positive-switches)
        cex
        positive-switches))))

; (displayln "The Authors Relation ------------------------------------------")
; (pretty-print
;   (nth-places 1 (relations-authors all-relations)))
; (displayln "The Mentions Relation ------------------------------------------")
; (pretty-print
;   (nth-places 1 (relations-mentions all-relations)))
; (displayln "The Cites Relation ------------------------------------------")
; (pretty-print
;   (nth-places 1 (relations-cites all-relations)))
;   (displayln "The Affiliated-With Relation -------------------------------------")
; (pretty-print
;   (nth-places 1 (relations-affiliated-with all-relations)))
;   (displayln "The Appears-In Relation ------------------------------------------")
; (pretty-print
;   (nth-places 1 (relations-appears-in all-relations)))
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
      ; (affiliated-with (author "a2") (affiliation "aff1"))
      (cites (paper "p2") (paper "p1")))
      #:RETURN "a1")))))

(define (demo-2)
  (verify (assert (equal?
    (MATCH (list
      (authors (author "a1") 
               (paper "p1" #:constrain (year-greater-than 1997))))
      #:RETURN "a1")

    (MATCH (list
      (authors (author "a1") (paper "p1")))
      #:RETURN "a1")))))

(define (demo-3)
  (verify (assert (equal?
    (MATCH (list
      (authors (author "a1") (paper "p1" #:constrain (year-less-than 2018)))
      #:RETURN "a1")

    (MATCH (list
      (authors (author "a1") (paper "p1" #:constrain (year-equal-to 1975)))
      (authors (author "a1") (paper "p1" #:constrain (year-equal-to 2001))))
      #:RETURN "a1"))))))


(define (demo-4)
  (verify (assert (equal?
    ; (MATCH (list
    ;   (affiliated-with (author "a1") (affiliation "aff1"))
    ;   (authors (author "a1") (paper "p1"))
    ;   (mentions (paper "p1") (entity #:constrain (name 0))))
    ;   #:RETURN "aff1")

    (MATCH (list
      (affiliated-with (author "a1") (affiliation "aff1"))
      (mentions (paper "p1") (entity #:constrain (name 4))))
      #:RETURN "aff1")

    (MATCH (list
      (affiliated-with (author "a1") (affiliation "aff1"))
      (mentions (paper "p1") (entity "e" #:constrain (name 4))))
      #:RETURN "aff1")))))

; ;; TODO: paper affiliation vs author?

(displayln "Demo 1 ----------------------------------------------------------")
(display-positive-switches (demo-1))

(displayln "Demo 2 ----------------------------------------------------------")
(display-positive-switches (demo-2))

(displayln "Demo 3 ----------------------------------------------------------")
(display-positive-switches (demo-3))

(displayln "Demo 4 ----------------------------------------------------------")
(display-positive-switches (demo-4))
