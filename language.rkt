#lang rosette

(provide (all-defined-out))

#|
    Goal syntax.
    (MATCH (list
        (authors (author "a") (paper "p")) 
        (mentions (paper "p") (entity "e" #:constrain (name "Relationship Extraction"))))
        #:WHERE (list
            (greater (year "p") 1200))
        #:RETURN "p")
|#

;; Variable AST structs for the interpreter.
(struct author-node      (variable constrain) #:transparent)
(struct affiliation-node (variable constrain) #:transparent)
(struct entity-node      (variable constrain) #:transparent)
(struct paper-node       (variable constrain) #:transparent)
(struct venue-node       (variable constrain) #:transparent)

;; Possible constraints.
(struct first            (value)              #:transparent)
(struct last             (value)              #:transparent)
(struct id               (value)              #:transparent)
(struct title            (value)              #:transparent)
(struct name             (value)              #:transparent)
(struct year             (value)              #:transparent)

;; Supported relations.
;; For now, allow "edge" to be the representation between two nodes.
(struct edge (v1 v2))

(struct authors     (author paper)       #:transparent)
(struct mentions    (paper entity)       #:transparent)

;; Racket embedding of GrapAL. This allows a cleaner syntax so that
;; the constraint can be optional.
(define (author variable #:constrain [constrain null])
  (author-node variable constrain))
(define (affiliation variable #:constrain [constrain null])
  (affiliation-node variable constrain))
(define (entity variable #:constrain [constrain null])
  (entity-node variable constrain))
(define (paper variable #:constrain [constrain null])
  (paper-node variable constrain))
(define (venue variable #:constrain [constrain null])
  (venue-node variable constrain))
