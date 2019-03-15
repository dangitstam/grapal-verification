#lang rosette

(provide (all-defined-out))
(require "../../src/graph.rkt")
(require "../util.rkt")

;; A symbolic dataset containing an instantiated set of nodes and edges
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

(define (complete-pairs pairs relation)
    (begin
        (map (lambda (pr)
            (if (not (assoc (car pairs) relation))
                (set! relation (cons (cons pr 0) relation))
                (void)))
            pairs)

        ;; Yield the relation.
        relation))

(define all-authors
    (make-n-authors 4))

(define all-papers
    (list (paper-data 0 1975 3)
          (paper-data 1 2001 4)
          (paper-data 2 2018 5)))

(define all-entities
    (make-n-entities 3))

(define all-venues
    (make-n-venues 2))

(define all-affiliations
    (make-n-affiliations 2))

(define (authors-relation)
    (make-symbolic-relation-from-pairs (cartesian-product all-authors all-papers)))

(define (mentions-relation)
    (make-symbolic-relation-from-pairs  (cartesian-product all-papers all-entities)))

(define (cites-relation)
    (make-symbolic-relation-from-pairs (cartesian-product all-papers all-papers)))

(define (appears-in-relation)
    (make-symbolic-relation-from-pairs (cartesian-product all-papers all-venues)))

(define (affiliated-with-relation)
    (make-symbolic-relation-from-pairs (cartesian-product all-authors all-affiliations)))

(define all-elements (universe all-authors all-entities all-papers all-affiliations all-venues))
(define all-relations (relations (authors-relation) (mentions-relation) (cites-relation)
                                 (affiliated-with-relation) (appears-in-relation)))
