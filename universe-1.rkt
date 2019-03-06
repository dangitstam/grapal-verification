#lang rosette

(provide (all-defined-out))
(require "graph.rkt")

;; An example universe containing an instantiated set of nodes and edges
;; defined in GrapAL. For simplicity, intergers are used in place of strings.

(define (set-of-all-authors n)
    (define xs null)
    (for ([i n])
        (set! xs (cons (author-data (- n i) (- n i) (- n i)) xs)))
    xs)

(define (set-of-all-papers n)
    (define xs null)
    (for ([i n])
        (set! xs (cons (paper-data (- n i) (- n i) (- n i)) xs)))
    xs)

(define (set-of-all-entities n)
    (define xs null)
    (for ([i n])
        (set! xs (cons (entity-data (- n i) (- n i)) xs)))
    xs)

(define all-authors (set-of-all-authors 5))

(define all-entities (set-of-all-entities 3))

(define all-papers (set-of-all-papers 2))

;; TODO: All relations are symmetric. A helper function that checks both orders should be implemented.

(define all-authors-relation
    ;; Make it so all-authors 0 & 1 all-authors paper-data 0, and all-authors 3 & 4 author-data paper-data 1.
    (list
        (cons (cons (list-ref all-authors 0) (list-ref all-papers 0)) 1)
        (cons (cons (list-ref all-authors 0) (list-ref all-papers 1)) 0)
        (cons (cons (list-ref all-authors 1) (list-ref all-papers 0)) 1)
        (cons (cons (list-ref all-authors 1) (list-ref all-papers 1)) 0)
        (cons (cons (list-ref all-authors 2) (list-ref all-papers 0)) 0)
        (cons (cons (list-ref all-authors 2) (list-ref all-papers 1)) 0)
        (cons (cons (list-ref all-authors 3) (list-ref all-papers 0)) 0)
        (cons (cons (list-ref all-authors 3) (list-ref all-papers 1)) 1)
        (cons (cons (list-ref all-authors 4) (list-ref all-papers 0)) 0)
        (cons (cons (list-ref all-authors 4) (list-ref all-papers 1)) 1)
    )     
)

(define mentions-relation
    ;; Make it so paper-data 1 mentions enities 1 and 2, paper-data 2 mentions entity-data 3.
    ;; TODO: This representation is wrong, it should be a list of tuples with 1 or 0 as the second element.
    (list
        (cons (list-ref all-papers 0) (list-ref all-entities 0))
        (cons (list-ref all-papers 0) (list-ref all-entities 1))
        (cons (list-ref all-papers 1) (list-ref all-entities 2))
    )
)

(define univ (universe all-authors all-entities all-papers null null))
(define rel (relations null null all-authors-relation null mentions-relation))
