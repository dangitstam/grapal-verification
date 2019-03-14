#lang rosette

(provide (all-defined-out))
(require "../../src/graph.rkt")

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

(define (make-n-venues n)
    (build-list n
        (lambda (x)
            (venue-data x (+ 1 x)))))

(define (make-n-affiliations n)
    (build-list n
        (lambda (x)
            (affiliation-data x (+ 1 x)))))

(define (make-symmetric relation)
    (define (make-symmetric-helper relation result)
        (if (null? relation)
            result
            (let* ([pr (car relation)]
                   [value (cdr pr)]
                   [forward-pr (car pr)]
                   [backward-pr (list (list-ref forward-pr 1) (list-ref forward-pr 0))]
                   [forward (cons forward-pr value)]
                   [backward (cons backward-pr value)])
                (make-symmetric-helper (cdr relation) (cons backward (cons forward result))))))
    (make-symmetric-helper relation null))

(define (complete-pairs pairs relation)
    (if (null? pairs)
        (make-symmetric relation)
        (let* ([pr (car pairs)]
               [lookup (assoc pr relation)])
            (if lookup
                (complete-pairs (cdr pairs) relation)
                (complete-pairs (cdr pairs) (cons (cons pr #f) relation))))))



(define all-authors
    (make-n-authors 10))

(define all-papers
    (make-n-papers 5))

(define all-entities
    (make-n-entities 5))

(define all-venues
    (make-n-venues 5))

(define all-affiliations
    (make-n-affiliations 3))

;; TODO: All relations are symmetric. A helper function that checks both orders should be implemented.

(define (authors-relation)
    (define pairs (cartesian-product all-authors all-papers))

    ;; TODO: (ask) is this sufficient? Do we need to include all of the 0 pairs?
    ;; What will be symbolic, the values?
    (define relation
        (list
            ;; Make it so all-authors 0 & 1 all-authors paper-data 0
            (cons (list (list-ref all-authors 0) (list-ref all-papers 0)) #t)
            (cons (list (list-ref all-authors 1) (list-ref all-papers 0)) #t)

            ;; and authors 3 & 4 author-data paper-data 1.
            (cons (list (list-ref all-authors 3) (list-ref all-papers 1)) #t)
            (cons (list (list-ref all-authors 4) (list-ref all-papers 1)) #t)

            ;; Authors 5 and 9 author paper 2, 3
            (cons (list (list-ref all-authors 5) (list-ref all-papers 2)) #t)
            (cons (list (list-ref all-authors 5) (list-ref all-papers 3)) #t)
            (cons (list (list-ref all-authors 9) (list-ref all-papers 2)) #t)
            (cons (list (list-ref all-authors 9) (list-ref all-papers 3)) #t)

            ;; Author 4 authors paper 4.
            (cons (list (list-ref all-authors 4) (list-ref all-papers 4)) #t)
        )
    )

    (complete-pairs pairs relation))

(define (mentions-relation)
    (define pairs (cartesian-product all-papers all-entities))
    (define relation
        (list
            ;; Make it so paper-data 0 mentions enities 0 and 1,
            ;; paper-data 1 mentions entity-data 1.
            (cons (list (list-ref all-papers 0) (list-ref all-entities 0)) #t)
            (cons (list (list-ref all-papers 0) (list-ref all-entities 1)) #t)
            (cons (list (list-ref all-papers 1) (list-ref all-entities 0)) #f)
            (cons (list (list-ref all-papers 1) (list-ref all-entities 1)) #t)

            ;; Paper 3 mentions entity 4.
            (cons (list (list-ref all-papers 3) (list-ref all-entities 4)) #t)
        )
    )

    (complete-pairs pairs relation))

(define (cites-relation)
    (define pairs (cartesian-product all-papers all-papers))
    (define relation
        (list
            ;; All papers cite paper 4 except 4 itself.
            (cons (list (list-ref all-papers 0) (list-ref all-papers 4)) #t)
            (cons (list (list-ref all-papers 1) (list-ref all-papers 4)) #t)
            (cons (list (list-ref all-papers 2) (list-ref all-papers 4)) #t)
            (cons (list (list-ref all-papers 3) (list-ref all-papers 4)) #t)

            ;; Paper 0, 4 cites paper 1.
            (cons (list (list-ref all-papers 0) (list-ref all-papers 1)) #t)
            (cons (list (list-ref all-papers 4) (list-ref all-papers 1)) #t)
        )
    )

    (complete-pairs pairs relation))

(define (appears-in-relation)
    (define pairs (cartesian-product all-papers all-venues))
    (define relation
        (list
            ;; Make it so paper 0 appears in venue 2.
            (cons (list (list-ref all-papers 0) (list-ref all-venues 2)) #t)

            ;; All papers appear in venue 0.
            (cons (list (list-ref all-papers 0) (list-ref all-venues 1)) #t)
            (cons (list (list-ref all-papers 1) (list-ref all-venues 1)) #t)
            (cons (list (list-ref all-papers 2) (list-ref all-venues 1)) #t)
            (cons (list (list-ref all-papers 3) (list-ref all-venues 1)) #t)
            (cons (list (list-ref all-papers 4) (list-ref all-venues 1)) #t)

            ;; Paper 2, 4 appears in venue 0.
            (cons (list (list-ref all-papers 2) (list-ref all-venues 0)) #t)
            (cons (list (list-ref all-papers 4) (list-ref all-venues 0)) #t)
        )
    )

    (complete-pairs pairs relation))

(define (affiliated-with-relation)
    (define author-affiliation-pairs (cartesian-product all-authors all-affiliations))
    (define paper-affiliation-pairs  (cartesian-product all-papers all-affiliations))
    (define relation
        (list
            ;; Authors 4, 6, 8 are affiliated with affiliation 0.
            ;; paper-data 1 mentions entity-data 1.
            (cons (list (list-ref all-authors 4) (list-ref all-affiliations 0)) #t)
            (cons (list (list-ref all-authors 6) (list-ref all-affiliations 0)) #t)
            (cons (list (list-ref all-authors 8) (list-ref all-affiliations 0)) #t)

            ;; Authors 0, 3, 4, 9 are affiliated with affiliation 0.
            ;; paper-data 1 mentions entity-data 1.
            (cons (list (list-ref all-authors 0) (list-ref all-affiliations 1)) #t)
            (cons (list (list-ref all-authors 3) (list-ref all-affiliations 1)) #t)
            (cons (list (list-ref all-authors 4) (list-ref all-affiliations 1)) #t)
            (cons (list (list-ref all-authors 9) (list-ref all-affiliations 1)) #t)

            ;; Authors 0 1 3 affiliated with affiliation 2.
            (cons (list (list-ref all-authors 0) (list-ref all-affiliations 2)) #t)
            (cons (list (list-ref all-authors 1) (list-ref all-affiliations 2)) #t)
            (cons (list (list-ref all-authors 3) (list-ref all-affiliations 2)) #t)

            ;; Papers 0 3 4 affiliated with affiliation 0.
            (cons (list (list-ref all-papers 0) (list-ref all-affiliations 0)) #t)
            (cons (list (list-ref all-papers 3) (list-ref all-affiliations 0)) #t)
            (cons (list (list-ref all-papers 4) (list-ref all-affiliations 0)) #t)

            ;; Papers 1 2 affiliated with affiliation 1.
            (cons (list (list-ref all-papers 1) (list-ref all-affiliations 1)) #t)
            (cons (list (list-ref all-papers 2) (list-ref all-affiliations 1)) #t)

            ;; All papers affiliated with affiliation 2
            (cons (list (list-ref all-papers 0) (list-ref all-affiliations 2)) #t)
            (cons (list (list-ref all-papers 1) (list-ref all-affiliations 2)) #t)
            (cons (list (list-ref all-papers 2) (list-ref all-affiliations 2)) #t)
            (cons (list (list-ref all-papers 3) (list-ref all-affiliations 2)) #t)
            (cons (list (list-ref all-papers 4) (list-ref all-affiliations 2)) #t)
        )
    )

    (complete-pairs author-affiliation-pairs (complete-pairs paper-affiliation-pairs relation)))

(define all-elements (universe all-authors all-entities all-papers all-affiliations all-venues))
(define all-relations (relations (authors-relation) (mentions-relation) (cites-relation)
                                 (affiliated-with-relation) (appears-in-relation)))
