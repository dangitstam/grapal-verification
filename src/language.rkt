#lang rosette

(provide (all-defined-out))

;; Variable AST structs for the interpreter.
(struct author-node      (variable constrain) #:transparent)
(struct affiliation-node (variable constrain) #:transparent)
(struct entity-node      (variable constrain) #:transparent)
(struct paper-node       (variable constrain) #:transparent)
(struct venue-node       (variable constrain) #:transparent)

;; Possible constraints.
(struct first              (value) #:transparent)
(struct id                 (value) #:transparent)
(struct last               (value) #:transparent)
(struct name               (value) #:transparent)
(struct title              (value) #:transparent)
(struct text               (value) #:transparent)
(struct year-equal-to      (value) #:transparent)
(struct year-less-than     (value) #:transparent)
(struct year-greater-than  (value) #:transparent)


;; Supported edge types.
(struct authors         (author paper) #:transparent)
(struct mentions        (paper entity) #:transparent)
(struct cites           (p1 p2)        #:transparent)
(struct affiliated-with (e1 e2)        #:transparent)
(struct appears-in      (paper venue)        #:transparent)

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
