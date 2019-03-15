# grapal-verification
A model of GraphAL embedded in Rosette for query verification.

## Getting Started

To use the language, both a **universe** and a set of **relations** must be provided. To do so, instantiate the `universe` and `relation` structs in `src/graph.rkt`.

An example universe/set-of-relations pair is contained in `test/fixtures/example_database.rkt`.

To instantiate an `interpreter` and a `MATCH` function, the two constructs needed for making queries given a universe and set of relations, include in your source file

```
(define interpreter (make-interpreter all-elements all-relations))
(define MATCH (make-query-matcher interpreter))
```

where `all-elements` and `all-relations` are your universe and set of relations, respectively. Note that once `MATCH` is defined, `interpreter` is no longer needed.

An example instantiation of an interpreter and a MATCH function can be found in `test/test-1.rkt` and `test/test-2.rkt` as well as `scenarios/demo.rkt`.

## Running the In-Class Demo

To run the demonstration from project presentations, do

```
racket scenarios/demo.rkt
```

Within this demonstration, each relation uses symbolic booleans to represent membership in any relation. Verification that takes place then enumerates over these symbolic values, yielding counterexamples in the form of bindings for these variables.

Please read the comments and go through the bindings produced in counterexamples (wherever applicable) and convince yourself the solver is correct!

# Implementation Details

The `src` folder contains the implementation of the GrapAL embedded DSL.
* `language.rkt` contains functions that serve as abstractions for a pleasant DSL experience (e.g. making it so optional constraints are possible) as well as containing the structs that will allow the interpreter to reason about the query.
* `graph.rkt` contains structs that represent node metadata (e.g. `author-data` has `first`, `last`, and `id` fields) as well as the `universe` and `relations` structs that make up the graph database
* `interpreter.rkt` contains the interpreter implementation. Specifically, it provides `make-interpreter`:  a function that given a `universe` and `relations` produces an interpreter that can reason about queries. It also provides `make-query-matcher`, which when given an interpreter, provides the function that allows users to make queries in the DSL.

As described above, properly instantiating an `interpreter` and `MATCH` function at the top of your file allows you to make queries on the given graph database:

```
(define interpreter (make-interpreter all-elements all-relations))
(define MATCH (make-query-matcher interpreter))

(define query
    (MATCH (list
      (authors (author "a1") 
               (paper "p1" #:constrain (year-greater-than 1997))))
      #:RETURN "a1")
```

`MATCH` takes a list of edges (edge functions and metadata structs are found in `language.rkt`), a named `RETURN` value, and you also have the option to **constrain** nodes based on the sets of constraints provided in `language.rkt`. For example, in the above query, `year-greater-than` enforces that all candidate papers `p1` are published after 1997.

An example symbolic database can be found in `scenarios/fixtures/symbolic_database.rkt`. `all-authors`, `all-papers`, `all-entities`, `all-venues`, `all-affiliations` are bound to the fixed collections of their respective node types. Note that all bindings are bound to calls to factory functions that generate instances, with the exception of `all-papers` having hardcoded instances for presentation purposes (i.e. they use years that are easy to recognize to showcase the effectiveness of the verifier).



