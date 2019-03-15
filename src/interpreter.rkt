#lang rosette

(require racket/pretty)

(require "language.rkt")
(require "graph.rkt")
(require "util.rkt")


;; Given two strings representing variables, establish a dependency by
;; creating mappings in an association list. Elements are tuples of
;; variables to a list of their dependencies.
;;
;; All edges in the DSL will result in a dependency of nodes that
;; share an edge.
;;
;; Returns the new association list.
(define (establish-dependence v1 v2 dependencies)
    (define v1-dependencies (assoc v1 dependencies))
    (define v2-dependencies (assoc v2 dependencies))

    (define updated-v1-entry
        (list v1 (if v1-dependencies
                    ;; The dependences are the second element of the pair.
                    ;; cdr yields the list of list.
                    (cons v2 (car (cdr v1-dependencies)))
                    (list v2))))
    (define updated-v2-entry
        (list v2 (if v2-dependencies
                    (cons v1 (car (cdr v2-dependencies)))
                    (list v1))))

    ;; Construct new list with cons, dead values are okay.
    (cons updated-v1-entry (cons updated-v2-entry dependencies)))


;; Given two nodes, finds the appropriate relation between their types.
;; Returns null if no such relation exists.
;;
;; TODO: This accepts a node struct; should it instead take two variables and lookup
;; the type from them? Should the IR include mappings from variables to their types?
(define (get-relation v1 v2 types all-relations)
    (define t1-lookup (assoc v1 types))
    (define t2-lookup (assoc v2 types))

    (if (not (and t1-lookup t2-lookup))
        (error "get-relation: type lookup failed.")
        (void))

    (define t1 (cdr t1-lookup))
    (define t2 (cdr t2-lookup))

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


;; Remember to deal with null constraints
(define (consume-node node all-elements)
    ;; A simple lambda that yields true on any input.
    (define true-factory (lambda (x) #t))
    (cond 
        [(author-node? node)
            (let ([constraint (author-node-constrain node)]
                  [author-nodes    (universe-authors all-elements)])
                (cond  ;; Filter via constraints.
                    [(first? constraint)
                        (let ([v (first-value constraint)])
                            (map cons author-nodes (map (lambda (a) (= v (author-data-first a))) author-nodes)))]
                    [(last? constraint)
                        (let ([v (last-value constraint)])
                            (map cons author-nodes (map (lambda (a) (= v (author-data-last a))) author-nodes)))]
                    [(id? constraint)
                        (let ([v (id-value constraint)])
                            (map cons author-nodes (map (lambda (a) (= v (author-data-id a))) author-nodes)))]
                    ;; No constraint specified.
                    [(null? constraint)
                        (map cons author-nodes (build-list (length author-nodes) true-factory))]))]
        [(paper-node? node)
            (let ([constraint (paper-node-constrain node)]
                  [paper-nodes    (universe-papers all-elements)])
                (cond  ;; Filter via constraints.
                    [(title? constraint)
                        (let ([v (title-value constraint)])
                            (map cons paper-nodes (map (lambda (p) (= v (paper-data-title p))) paper-nodes)))]
                    [(year-equal-to? constraint)
                        (let ([v (year-equal-to-value constraint)])
                            (map cons paper-nodes (map (lambda (p) (= (paper-data-year p) v)) paper-nodes)))]
                    [(year-less-than? constraint)
                        (let ([v (year-less-than-value constraint)])
                            (map cons paper-nodes (map (lambda (p) (< (paper-data-year p) v)) paper-nodes)))]
                    [(year-greater-than? constraint)
                        (let ([v (year-greater-than-value constraint)])
                            (map cons paper-nodes (map (lambda (p) (> (paper-data-year p) v)) paper-nodes)))]
                    [(id? constraint)
                        (let ([v (id-value constraint)])
                            (map cons paper-nodes (map (lambda (p) (= (paper-data-id p) v)) paper-nodes)))]
                    [(null? constraint)
                        (map cons paper-nodes (build-list (length paper-nodes) true-factory))]))]
        [(entity-node? node)
            (let ([constraint (entity-node-constrain node)]
                  [entity-nodes   (universe-entities all-elements)])
                (cond  ;; Filter via constraints.
                    [(name? constraint)
                        (let ([v (name-value constraint)])
                            (map cons entity-nodes (map (lambda (e) (= v (entity-data-name e))) entity-nodes)))]
                    [(id? constraint)
                        (let ([v (last-value constraint)])
                            (map cons entity-nodes (map (lambda (e) (= v (entity-data-id e))) entity-nodes)))]
                    [(null? constraint)
                        (map cons entity-nodes (build-list (length entity-nodes) true-factory))]))]
        [(affiliation-node? node)
            (let ([constraint        (affiliation-node-constrain node)]
                  [affiliation-nodes (universe-affiliations all-elements)])
                (cond  ;; Filter via constraints.
                    [(text? constraint)
                        (let ([v (text-value constraint)])
                            (map cons affiliation-nodes (map (lambda (a) (= v (affiliation-data-text a))) affiliation-nodes)))]
                    [(id? constraint)
                        (let ([v (last-value constraint)])
                            (map cons affiliation-nodes (map (lambda (a) (= v (affiliation-data-id a))) affiliation-nodes)))]
                    [(null? constraint)
                        (map cons affiliation-nodes (build-list (length affiliation-nodes) true-factory))]))]
        [(venue-node? node)
            (let ([constraint        (venue-node-constrain node)]
                  [venue-nodes (universe-venues all-elements)])
                (cond  ;; Filter via constraints.
                    [(text? constraint)
                        (let ([v (text-value constraint)])
                            (map cons venue-nodes (map (lambda (w) (= v (venue-data-text w))) venue-nodes)))]
                    [(id? constraint)
                        (let ([v (last-value constraint)])
                            (map cons venue-nodes (map (lambda (w) (= v (venue-data-id w))) venue-nodes)))]
                    [(null? constraint)
                        (map cons venue-nodes (build-list (length venue-nodes) true-factory))]))]))


;; Given a pair of variables and a relation, constraint the nodes that the
;; variables map to.
;; Meaning, for the given relation, for a node n1 in the set v1 maps to,
;; if there exists at least one node n2 in the set of nodes v2 maps to such
;; that (n1, n2) -> 1 in the relation, then keep n1 in the set v1 maps to.
(define (constrain-by-relation v1 v2 environment relation)
    (define (sweep-through-relation datum others relation)
        ;; Fold through the relevant pairs in a relation, accumulating booleans with \/.
        ;; This is then /\'ed with the entry for v.
        ;; Relations are assumed to be symmetric. Without loss of generality, check
        ;; the first element of every pair.
        (foldl
            (lambda (x acc)
                (or acc 
                    ;; This needs to include a lookup
                    (let* ([other (car (cdr (car x)))]
                           [other-boolean (cdr (assoc other others))]
                           [relation-existence (cdr x)])
                        (and relation-existence other-boolean))))
            #f
            (filter (lambda (pr) (equal? datum (car (car pr)))) relation)))

    (define (accumulate-membership-disjunctions values others relation)
        ;; Build a new association list with the aggregate relation boolean disjunctions
        ;; /\'ed onto whatever each value_i had previously.    
        (if (null? values)
            null
            (let* ([pr (car values)]
                   [datum (car pr)]
                   [aggregate-boolean (cdr pr)]
                   [next-aggregate-boolean
                    (and aggregate-boolean (sweep-through-relation datum others relation))])
                (cons 
                    (cons datum next-aggregate-boolean)
                    (accumulate-membership-disjunctions (cdr values) others relation)))))

    (define v1-lookup (assoc v1 environment))
    (define v2-lookup (assoc v2 environment))

    (define updated-v1-values
        (accumulate-membership-disjunctions (car (cdr v1-lookup)) (car (cdr v2-lookup)) relation))

    (define updated-v2-values
        (accumulate-membership-disjunctions (car (cdr v2-lookup)) (car (cdr v1-lookup)) relation))

    ;; Overwrite existing values.
    (cons (list v1 updated-v1-values)
          (cons (list v2 updated-v2-values) environment)))


;; Given a node and a set of nodes that were already visited, recursively
;; explores and updates all dependencies.
;;
;; For now, we assume the query forms a DAG. As a precaution, we do
;; not recursively explore nodes that do not change.
;;
;; TODO: How will bounded model checking be introduced here?
(define (update-dependencies var visited environment types dependencies all-relations)
    ;; Collect all dependent nodes and exclude ones that have been visited.
    ;; Operating with sets of strings prevents duplicates.
    (define dependencies-of-var
        (let ([lookup (assoc var dependencies)])
            ;; Lookup is of the form (list var (list vars))
            (if lookup (car (cdr lookup)) null)))

    (define unvisited-dependent-nodes
        (filter (lambda (v) (not (member v visited))) dependencies-of-var))

    (define pairs-to-update
        (cartesian-product (list var) unvisited-dependent-nodes))

    ;; Helper function to overwrite the environment by constraining on
    ;; all pairs.
    (define (constrain-all-pairs-by-relation pairs environment)
        (if (null? pairs)
            environment
            ;; Each pair is a two-element list. Use car x2 to get the first element out,
            ;; and car cdr car to get the second.
            (let* ([pr (car pairs)]
                   [v1 (car pr)]
                   [v2 (car (cdr pr))]
                   [current-relation (get-relation v1 v2 types all-relations)]
                   [new-environment (constrain-by-relation v1 v2 environment current-relation)])
                (constrain-all-pairs-by-relation (cdr pairs) new-environment))))

    (define environment-with-constrained-pairs
        (constrain-all-pairs-by-relation pairs-to-update environment))

    (define all-visited-nodes (append visited unvisited-dependent-nodes))

    (define (update-all-dependencies nodes-to-visit visited environment types dependencies all-relations)
        (if (null? nodes-to-visit)
            environment
            (let* ([v (car nodes-to-visit)]
                   [new-environment (update-dependencies v visited environment types dependencies all-relations)])
                (update-all-dependencies
                    (cdr nodes-to-visit)
                    visited
                    new-environment
                    types dependencies all-relations))))

    (update-all-dependencies
        unvisited-dependent-nodes
        all-visited-nodes
        environment-with-constrained-pairs
        types dependencies all-relations))


;; Given an edge type, establishes a dependence between the nodes that share
;; the edge and initialize any new node in the environment.
;;
;; Returns the new environment, types, and dependencies.
(define (consume-edge edge state all-elements all-relations)
    ;; Resolve constraints and map variables appropriately.
    ;; Then, within edges, remove nodes in respective mappings that do not
    ;; exist in the relation specified by the edge.

    (define environment (list-ref state 0))
    (define types (list-ref state 1))
    (define dependencies (list-ref state 2))

    ;; Some parts of this are generalizable, we just need the right relation.
    ;; Everything else outside of this helps in implementing a type system.
    (define (consume-edge-helper v1 v2 n1 n2 types dependencies relation)
        ; (define constrained-n1 (consume-node n1 all-elements))
        ; (define constrained-n2 (consume-node n2 all-elements))

        ;; Dependencies updated so that v1 and v2 depend on each other.
        (define latest-dependencies (establish-dependence v1 v2 dependencies))

        (begin
            ; (establish-dependence v1 v2 dependencies)
            ;; Resolve constraints that nodes come with.
            (define environment-with-v1-v2
                (cons (list v1 (consume-node n1 all-elements))
                      (cons (list v2 (consume-node n2 all-elements))
                            environment)))

            ;; Given two sets, the final constraint is to constraint
            ;; w.r.t the relation.
            (define environment-after-relation-constraints
                (constrain-by-relation v1 v2 environment-with-v1-v2 relation))

            ;; Recursively update dependencies without revisiting nodes
            ;; that have been seen.
            (define environment-after-updating-v1-dependencies  ;; TODO: issue here.
                (update-dependencies v1 (list v1 v2) environment-after-relation-constraints
                                     types latest-dependencies all-relations))

            (define environment-after-updating-v2-dependencies
                (update-dependencies v2 (list v1 v2) environment-after-updating-v1-dependencies
                                     types latest-dependencies all-relations))
            
            (list environment-after-updating-v2-dependencies types latest-dependencies)
        ))

    ;; TODO(Tam): if any of the below break, the query was not well-formed.
    ;; Implement custom exceptions to communicate this to the user
    ;; in the future.
    ;; Select the correct relation to constrain with.
    (cond 
        ;; TODO: Support single-node queries.
        [(author-node? edge)
            (list     ;; `list` needed to prevent variable from being cons'ed onto the value.
                (cons (list (author-node-variable edge) (consume-node edge all-elements)) environment)
                (cons (cons (author-node-variable edge) author-node?) types)
                dependencies)]
        [(paper-node? edge)
            (list
                (cons (list (paper-node-variable edge) (consume-node edge all-elements)) environment)
                (cons (cons (paper-node-variable edge) paper-node?) types)
                dependencies)]
        [(entity-node? edge)
            (list
                (cons (list (entity-node-variable edge) (consume-node edge all-elements)) environment)
                (cons (cons (entity-node-variable edge) entity-node?) types)
                dependencies)]
        [(affiliation-node? edge)
            (list
                (cons (list (affiliation-node-variable edge) (consume-node edge all-elements)) environment)
                (cons (cons (affiliation-node-variable edge) affiliation-node?) types)
                dependencies)]
        [(venue-node? edge)
            (list
                (cons (list (venue-node-variable edge) (consume-node edge all-elements)) environment)
                (cons (cons (venue-node-variable edge) venue-node?) types)
                dependencies)]
        ;; Edges require establishing a dependency in addition to adding both variables to the environment.
        [(authors? edge)
            (let* ([a (authors-author edge)]
                   [p (authors-paper edge)]
                   [a-var (author-node-variable a)]
                   [p-var (paper-node-variable p)])
                ;; Extraction of variable names is necessary because the helper is not
                ;; smart enough to do this.
                (consume-edge-helper a-var p-var a p
                    ;; Updates the type association list.
                    ;; Map variables to their types for easy lookups of relations later.
                    (cons (cons a-var author-node?) 
                          (cons (cons p-var paper-node?) types))
                    dependencies (relations-authors all-relations)))]
        [(mentions? edge)
            (let* ([p (mentions-paper edge)]
                   [e (mentions-entity edge)]
                   [p-var (paper-node-variable p)]
                   [e-var (entity-node-variable e)])
                    (consume-edge-helper p-var e-var p e
                        (cons (cons p-var paper-node?) 
                              (cons (cons e-var entity-node?) types))
                        dependencies (relations-mentions all-relations)))]
        [(cites? edge)
            (let* ([p1 (cites-p1 edge)]
                   [p2 (cites-p2 edge)]
                   [p1-var (paper-node-variable p1)]
                   [p2-var (paper-node-variable p2)])
                (consume-edge-helper p1-var p2-var p1 p2
                    (cons (cons p1-var paper-node?) 
                          (cons (cons p2-var paper-node?) types))
                    dependencies (relations-cites all-relations)))]
        [(affiliated-with? edge)
            (let* ([e1 (affiliated-with-e1 edge)]
                   [e2 (affiliated-with-e2 edge)]
                   [e1-var 
                        (cond [(author-node? e1) (author-node-variable e1)]
                              [(paper-node? e1) (paper-node-variable e1)]
                              [#t (error "Affiliation is only between authors/papers and affilations.")])]

                   ;; Second argument is assumed to be an affiliation (TODO: type checking here?)
                   [e2-var (affiliation-node-variable e2)])
                (consume-edge-helper e1-var e2-var e1 e2
                    (cons (cons e1-var (if (author-node? e1) author-node? paper-node?))
                          (cons (cons e2-var paper-node?) types))
                    dependencies (relations-affiliated-with all-relations)))]
        [(appears-in? edge)
            (let* ([p (appears-in-paper edge)]
                   [v (appears-in-venue edge)]
                   [p-var (paper-node-variable p)]
                   [v-var (venue-node-variable v)])
                (consume-edge-helper p-var v-var p v
                    (cons (cons p-var paper-node?) 
                          (cons (cons v-var venue-node?) types))
                    dependencies (relations-appears-in all-relations)))]))

(define (consume-edges edges state all-elements all-relations)
    (if (null? edges)
        state
        (consume-edges (cdr edges)
            (consume-edge (car edges) state all-elements all-relations)
            all-elements all-relations)))

;; Wrapper for creating an interpreter that works over the given universe
;; and relations.
(define (make-interpreter all-elements all-relations)
    (lambda (edges where return-stmt)
        ;; Given every provided edge
        ;;   i. resolves constraints within nodes.
        ;;  ii. resolves constraints between the nodes that share the edge.
        ;; iii. resolves constraints between the nodes that share the edge and all
        ;;      of their respective dependencies.
        (define final-execution-state
            (consume-edges edges (list null null null) all-elements all-relations))

        (define environment (list-ref final-execution-state 0))  ;; Variable => Candidate Nodes.
        (define types (list-ref final-execution-state 1))  ;; Variable => Type
        (define dependencies (list-ref final-execution-state 2))  ;; Variable => Dependent Variables.

        ;; TODO: Handle the where statement.

        ;; TODO: Limit 1 and other perks
        (define (filter-return-values var environment)
            (map car (filter (lambda (x) (cdr x)) (car (cdr (assoc var environment))))))
    
        ;; Return the specified node(s).
        (cond [(string? return-stmt) (list (filter-return-values return-stmt environment))]
              [(list? return-stmt)
                (let* ([collect-elements (lambda (v) (filter-return-values v environment))])
                    (map collect-elements return-stmt))])))
(provide make-interpreter)


;; Given an interpreter, yields a function capable of fulfilling queries.
(define (make-query-matcher interpreter)
    (lambda (edges #:WHERE [where null] #:RETURN [return-stmt null])
        (interpreter edges where return-stmt)))
(provide make-query-matcher)
