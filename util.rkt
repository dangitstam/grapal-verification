
#lang rosette

(provide (all-defined-out))
(require "graph.rkt")
(require "language.rkt")

;; For convenience, reverses a pair.
(define (reverse-pair pr)
    (list (car (cdr pr)) (car pr)))
