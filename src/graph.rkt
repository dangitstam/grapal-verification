#lang rosette

(provide (all-defined-out))

;; Definition of GrapAL constructs to represent the database.
;;
;; Node and edge types found here: http://grapal.allenai.org/
(struct author-data (first last id) #:transparent)
(struct entity-data (name id) #:transparent)
(struct paper-data (title year id) #:transparent)
(struct affiliation-data (text id) #:transparent)
(struct venue-data (text id) #:transparent)

;; A construct containing all relations.
(struct universe (authors entities papers affiliations venues) #:transparent)
(struct relations (authors mentions cites affiliated-with appears-in) #:transparent)
