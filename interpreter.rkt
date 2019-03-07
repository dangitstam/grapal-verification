#lang rosette

(provide (all-defined-out))
(require "language.rkt")
(require "graph.rkt")
(require "util.rkt")
(require "universe-1.rkt")

;; Given two strings representing variables, establish a dependency by
;; creating mappings for each string in `dependencies` such that
;; the values are sets. Each set will contain the other variable.
;;
;; All edges in the DSL will result in a dependency of nodes that
;; share an edge.
(define (establish-dependence n1 n2 dependencies)
    (begin (hash-set! dependencies n1 (set-add (hash-ref! dependencies n1 (set)) n2))
           (hash-set! dependencies n2 (set-add (hash-ref! dependencies n2 (set)) n1))))


;; Given two nodes, finds the appropriate relation between their types.
;; Returns null if no such relation exists.
;;
;; TODO: This accepts a node struct; should it instead take two variables and lookup
;; the type from them? Should the IR include mappings from variables to their types?
(define (get-relation n1 n2 types all-relations)
    (define t1 (hash-ref! types n1 #f))
    (define t2 (hash-ref! types n2 #f))
    (cond 
          ;; Affiliations are between an affiliations and either an author or paper.
          [(or (and (or (equal? author-node? t1) (equal? paper-node? t1)) (equal? affiliation-node? t2))
               (and (equal? affiliation-node? t1) (or (equal? author-node? t2) (equal? paper-node? t2)))) 
            (relations-affiliated-with all-relations)]
          ;; Venues are only associated with papers via appears-in.
          [(or (and (equal? paper-node? t1) (equal? venue-node? t2))
               (and (equal? venue-node? t1) (equal? paper-node? t2)))
            (relations-appears-in all-relations)]
          ;; Only authors can write papers.
          [(or (and (equal? author-node? t1) (equal? paper-node? t2))
               (and (equal? paper-node? t1)  (equal? author-node? t2)))
            (relations-authors all-relations)]
          ;; Only papers can cite each other.
          [(and (equal? paper-node? t1) (equal? paper-node? t2))
            (relations-cites all-relations)]
          ;; Only papers can mention entities.
          [(or (and (equal? paper-node? t1) (equal? entity-node? t2))
               (and (equal? entity-node? t1) (equal? paper-node? t2)))
            (relations-mentions all-relations)]
          [#f null]))


;; Given a node, collects all possible nodes that satisfy the node's given
;; constraint.
;; E.g. a node may be an author node that restricts to all authors with a
;; particular first name.
;;
;; If a node has no constraints, returns the set of all nodes in the universe
;; of the type specified by the input.
(define (consume-node node univ)
    (cond 
        [(author-node? node)
            (let ([constraint (author-node-constrain node)]
                  [author-nodes    (universe-authors univ)])
                (cond  ;; Filter via constraints.
                    [(first? constraint)
                        (let ([v (first-value constraint)])
                            (filter (lambda (a) (= v (author-data-first a))) author-nodes))]
                    [(last? constraint)
                        (let ([v (last-value constraint)])
                            (filter (lambda (a) (= v (author-data-last a))) author-nodes))]
                    [(id? constraint)
                        (let ([v (id-value constraint)])
                            (filter (lambda (a) (= v (author-data-id a))) author-nodes))]
                    ;; No constraint specified.
                    [(null? constraint) author-nodes]))]
        [(paper-node? node)
            (let ([constraint (paper-node-constrain node)]
                  [paper-nodes    (universe-papers univ)])
                (cond  ;; Filter via constraints.
                    [(title? constraint)
                        (let ([v (title-value constraint)])
                            (filter (lambda (p) (= v (paper-data-title p))) paper-nodes))]
                    [(year? constraint)
                        (let ([v (year-value constraint)])
                            (filter (lambda (p) (= v (paper-data-year p))) paper-nodes))]
                    [(id? constraint)
                        (let ([v (id-value constraint)])
                            (filter (lambda (p) (= v (paper-data-id p))) paper-nodes))]
                    [(null? constraint) paper-nodes]))]
        [(entity-node? node)
            (let ([constraint (entity-node-constrain node)]
                  [entity-nodes   (universe-entities univ)])
                (cond  ;; Filter via constraints.
                    [(name? constraint)
                        (let ([v (name-value constraint)])
                            (filter (lambda (e) (= v (entity-data-name e))) entity-nodes))]
                    [(id? constraint)
                        (let ([v (last-value constraint)])
                            (filter (lambda (e) (= v (entity-data-id e))) entity-nodes))]
                    [(null? constraint) entity-nodes]))]))


;; Given a pair of variables and a relation, constraint the nodes that the
;; variables map to.
;; Meaning, for the given relation, for a node n1 in the set v1 maps to,
;; if there exists at least one node n2 in the set of nodes v2 maps to such
;; that (n1, n2) -> 1 in the relation, then keep n1 in the set v1 maps to.
(define (constrain-by-relation v1 v2 environment relation)
    (define pairs (cartesian-product (hash-ref! environment v1 null) (hash-ref! environment v2 null)))
    (define new-v1 null)
    (define new-v2 null)
    (define (check-pairs ps)
        (if (null? ps)
            (void)
            (begin
                (let* ([pr (car ps)]
                       [forward-pair (assoc pr relation)]
                       [backward-pair (assoc (reverse-pair pr) relation)])
                    ;; Checks both orders (relations in GrapAL are symmetric).
                    (if (or (and forward-pair (= (cdr forward-pair) 1))
                            (and backward-pair (= (cdr backward-pair) 1)))
                        (begin (set! new-v1 (cons (car pr) new-v1))
                               ;; pr is actually a list of two elements.
                               (set! new-v2 (cons (car (cdr pr)) new-v2)))
                        (void)))
                (check-pairs (cdr ps)))))
    (begin
        (check-pairs pairs)
        (hash-set! environment v1 new-v1)
        (hash-set! environment v2 new-v2)))


;; Given a node and a set of nodes that were already visited, recursively
;; explores and updates all dependencies.
;;
;; For now, we assume the query forms a DAG. As a precaution, we do
;; not recursively explore nodes that do not change.
;;
;; TODO: How will bounded model checking be introduced here?
(define (update-dependencies var visited environment types dependencies)
    ;; Collect all dependent nodes and exclude ones that have been visited.
    ;; Operating with sets of strings prevents duplicates.
    (define unvisited-dependent-nodes
        (filter (lambda (v) (not (set-member? visited v)))
                (set->list (hash-ref! dependencies var (set)))))
    (define update-pairs
        (cartesian-product (list var) unvisited-dependent-nodes))

    (begin
        ;; First, map over all pairs of var and its dependencies
        ;; to update them w.r.t var.
        (map 
            (lambda (vs)
                (constrain-by-relation (car vs) (car (cdr vs)) environment (get-relation (car vs) (car (cdr vs)) types rel)))
            update-pairs)

        ;; Next, recursively the dependencies of var's dependencies.
        ;; All nodes updated in this frame do not need to be visited again.
        (define updated-visted (set-union visited (list->set unvisited-dependent-nodes)))
        (map 
            (lambda (v)
                (update-dependencies v updated-visted
                    environment types dependencies))
             unvisited-dependent-nodes)))


;; Given an edge type, establishes a dependence between the nodes that share
;; the edge and initialize any new node in the environment.
;;
;; TODO: can this be polymorphic?
(define (consume-edge edge environment types dependencies univ)
    ;; Resolve constraints and map variables appropriately.
    ;; Then, within edges, remove nodes in respective mappings that do not
    ;; exist in the relation specified by the edge.

    ;; Some parts of this are generalizable, we just need the right relation.
    ;; Everything else outside of this helps in implementing a type system.
    (define (consume-edge-helper v1 v2 n1 n2 relation)
        (begin
            (establish-dependence v1 v2 dependencies)
            ;; Resolve constraints that nodes come with.
            (hash-set! environment v1 (consume-node n1 univ))
            (hash-set! environment v2 (consume-node n2 univ))

            ;; Given two sets, the final constraint is to constraint
            ;; w.r.t the relation.
            (constrain-by-relation v1 v2 environment relation)

            ;; Recursively update dependencies without revisiting nodes
            ;; that have been seen.
            (update-dependencies v1 (set v1 v2) environment types dependencies)
            (update-dependencies v2 (set v1 v2) environment types dependencies)))

    ;; TODO: if any of the below break, the query was not well-formed.
    ;; Implement custom exceptions to communicate this to the user
    ;; in the future.
    ;; Select the correct relation to constrain with.
    (cond 
        [(authors? edge)
            (let* ([a (authors-author edge)]
                   [p (authors-paper edge)]
                   [a-var (author-node-variable a)]
                   [p-var (paper-node-variable p)])
                ;; Extraction of variable names is necessary because the helper is not
                ;; smart enough to do this.
                (begin
                    ;; Map variables to their types for easy lookups of relations later.
                    (hash-set! types a-var author-node?)
                    (hash-set! types p-var paper-node?)
                    (consume-edge-helper a-var p-var a p (relations-authors rel))))]
        [(mentions? edge)
            (let* ([p (mentions-paper edge)]
                   [e (mentions-entity edge)]
                   [p-var (paper-node-variable p)]
                   [e-var (entity-node-variable e)])
                   (begin
                    ;; Map variables to their types for easy lookups of relations later.
                    (hash-set! types p-var paper-node?)
                    (hash-set! types e-var entity-node?)
                    (consume-edge-helper p-var e-var p e (relations-mentions rel))))]
        
        ;; TODO: Support this and other edges after smoke-testing the above.
        ; [(cites? edge)
        ;     (let ([p1 (cites-p1 edge)]
        ;           [p2 (cites-p2 edge)]
        ;           [p1-var (paper-node-variable p1)]
        ;           [p1-var (paper-node-variable p2)])
        ;         (consume-edge-helper p1-var p2-var p1 p2 (relations-cites rel)))]
                    
))

(define (interpreter edges where ret)
    (define environment (make-hash))   ;; Variable => Candidate Nodes.
    (define types (make-hash))         ;; Variable => Type
    (define dependencies (make-hash))  ;; Variable => Dependent Variables.

    ;; Given every provided edge
    ;;   i. resolves constraints within nodes.
    ;;  ii. resolves constraints between the nodes that share the edge.
    ;; iii. resolves constraints between the nodes that share the edge and all
    ;;      of their respective dependencies.
    (map (lambda (edge) (consume-edge edge environment types dependencies univ)) edges)

    ;; Return the specified node.
    (hash-ref! environment ret null)
)

(define (MATCH edges #:WHERE [where null] #:RETURN [ret null])
    (interpreter edges where ret))

(define test
    (MATCH (list
        (authors (author "a") (paper "p")) 
        (mentions (paper "p") (entity "e" #:constrain (name 1))))
        #:RETURN "p"))

; (define test2
;     (MATCH (list
;         (authors (author "a") (paper "p" #:constrain (title 1))))
;         #:RETURN "a"))

(println test)