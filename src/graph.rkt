#lang rosette

(provide (all-defined-out))

;; Definition of GrapAL constructs.
;;
;; Node and edge types found here: http://grapal.allenai.org/
(struct author-data (first last id) #:transparent)
(struct affiliation-data (text) #:transparent)
(struct entity-data (name id) #:transparent)
(struct paper-data (title year id) #:transparent)
(struct venue-data (text) #:transparent)

;; A construct containing all relations.
(struct universe (authors entities papers affiliations venues) #:transparent)
(struct relations (affiliated-with appears-in authors cites mentions) #:transparent)

#| Useful functions while using the above. |#

