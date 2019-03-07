#lang rosette

(provide (all-defined-out))
(require "../src/graph.rkt")

;; An example universe containing an instantiated set of nodes and edges
;; defined in GrapAL. For simplicity, intergers are used in place of strings.

(define (set-of-all-authors n)
    (define xs null)
    (for ([i n])
        (set! xs (cons (author-data i i i) xs)))
    xs)

(define (set-of-all-papers n)
    (define xs null)
    (for ([i n])
        (set! xs (cons (paper-data i i i) xs)))
    xs)

(define (set-of-all-entities n)
    (define xs null)
    (for ([i n])
        (set! xs (cons (entity-data i i) xs)))
    xs)

(define all-authors (reverse (set-of-all-authors 5)))

(define all-entities (reverse (set-of-all-entities 3)))

(define all-papers (reverse (set-of-all-papers 2)))

;; TODO: All relations are symmetric. A helper function that checks both orders should be implemented.

(define all-authors-relation
    (list
        ;; Make it so all-authors 0 & 1 all-authors paper-data 0
        (cons (list (list-ref all-authors 0) (list-ref all-papers 0)) 1)
        (cons (list (list-ref all-authors 0) (list-ref all-papers 1)) 0)
        (cons (list (list-ref all-authors 1) (list-ref all-papers 0)) 1)
        (cons (list (list-ref all-authors 1) (list-ref all-papers 1)) 0)
        (cons (list (list-ref all-authors 2) (list-ref all-papers 0)) 0)
        (cons (list (list-ref all-authors 2) (list-ref all-papers 1)) 0)
        (cons (list (list-ref all-authors 3) (list-ref all-papers 0)) 0)

        ;; and authors 3 & 4 author-data paper-data 1.
        (cons (list (list-ref all-authors 3) (list-ref all-papers 1)) 1)
        (cons (list (list-ref all-authors 4) (list-ref all-papers 0)) 0)
        (cons (list (list-ref all-authors 4) (list-ref all-papers 1)) 1)
    )     
)

(define mentions-relation
    ;; Make it so paper-data 0 mentions enities 0 and 1, paper-data 1 mentions entity-data 1.
    ;; TODO: This representation is wrong, it should be a list of tuples with 1 or 0 as the second element.
    (list
        (cons (list (list-ref all-papers 0) (list-ref all-entities 0)) 1)
        (cons (list (list-ref all-papers 0) (list-ref all-entities 1)) 1)
        (cons (list (list-ref all-papers 1) (list-ref all-entities 0)) 0)
        (cons (list (list-ref all-papers 1) (list-ref all-entities 1)) 1)
    )
)

(define all-elements (universe all-authors all-entities all-papers null null))
(define all-relations (relations null null all-authors-relation null mentions-relation))
