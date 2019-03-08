#lang rosette

(provide (all-defined-out))
(require "../src/graph.rkt")

;; An example universe containing an instantiated set of nodes and edges
;; defined in GrapAL. For simplicity, intergers are used in place of strings.

(define (make-n-authors n)
    (build-list n
        (lambda (x)
            (author-data x (+ 1 x) (+ 2 x)))))

(define (make-n-papers n)
    (build-list n
        (lambda (x)
            (paper-data x (+ 1 x) (+ 2 x)))))

(define (make-n-entities n)
    (build-list n
        (lambda (x)
            (entity-data x (+ 1 x)))))

(define all-authors
    (make-n-authors 10))

(define all-papers
    (make-n-papers 5))

(define all-entities
    (make-n-entities 5))

;; TODO: All relations are symmetric. A helper function that checks both orders should be implemented.

(define (authors-relation)
    (define pairs (cartesian-product all-authors all-papers))

    ;; TODO: (ask) is this sufficient? Do we need to include all of the 0 pairs?
    ;; What will be symbolic, the values?
    (define relation
        (list
            ;; Make it so all-authors 0 & 1 all-authors paper-data 0
            (cons (list (list-ref all-authors 0) (list-ref all-papers 0)) 1)
            (cons (list (list-ref all-authors 1) (list-ref all-papers 0)) 1)

            ;; and authors 3 & 4 author-data paper-data 1.
            (cons (list (list-ref all-authors 3) (list-ref all-papers 1)) 1)
            (cons (list (list-ref all-authors 4) (list-ref all-papers 1)) 1)

            ;; Authors 5 and 9 author paper 2, 3
            (cons (list (list-ref all-authors 5) (list-ref all-papers 2)) 1)
            (cons (list (list-ref all-authors 5) (list-ref all-papers 2)) 1)
            (cons (list (list-ref all-authors 9) (list-ref all-papers 2)) 1)
            (cons (list (list-ref all-authors 9) (list-ref all-papers 2)) 1)
        )
    )

    (begin
        (map (lambda (pr)
            (if (not (assoc (car pairs) relation))
                (set! relation (cons pr 0))
                (void)))
            pairs)

        ;; Yield the relation.
        relation))

(define (mentions-relation)
    (define pairs (cartesian-product all-papers all-entities))
    (define relation
        (list
            ;; Make it so paper-data 0 mentions enities 0 and 1,
            ;; paper-data 1 mentions entity-data 1.
            (cons (list (list-ref all-papers 0) (list-ref all-entities 0)) 1)
            (cons (list (list-ref all-papers 0) (list-ref all-entities 1)) 1)
            (cons (list (list-ref all-papers 1) (list-ref all-entities 0)) 0)
            (cons (list (list-ref all-papers 1) (list-ref all-entities 1)) 1)

            ;; Paper 3 mentions entity 4.
            (cons (list (list-ref all-papers 3) (list-ref all-entities 4)) 1)
        )
    )

    (begin
        (map (lambda (pr)
            (if (not (assoc (car pairs) relation))
                (set! relation (cons pr 0))
                (void)))
            pairs)

        ;; Yield the relation.
        relation))

(define all-elements (universe all-authors all-entities all-papers null null))
(define all-relations (relations null null (authors-relation) null (mentions-relation)))
