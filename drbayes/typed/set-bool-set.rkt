#lang typed/racket/base

(provide
 empty-bool-set
 bool-set-intersect
 (struct-out Base-Bool-Set)
 (struct-out Empty-Bool-Set)
 (struct-out Full-Bool-Set)
 (struct-out True-Bool-Set)
 (struct-out False-Bool-Set))

;; -----------------------------------------------------------------------------

(require
  "set-types-adapted.rkt")

;; =============================================================================

(struct Base-Bool-Set Base-Bot-Basic ())
(struct Empty-Bool-Set Base-Bool-Set ())
(struct Full-Bool-Set Base-Bool-Set ())
(struct True-Bool-Set Base-Bool-Set ())
(struct False-Bool-Set Base-Bool-Set ())

(define-type Plain-Bool-Set (U True-Bool-Set False-Bool-Set))
(define-type Nonfull-Bool-Set (U Plain-Bool-Set Empty-Bool-Set))
(define-type Nonempty-Bool-Set (U Plain-Bool-Set Full-Bool-Set))
(define-type Bool-Set (U Plain-Bool-Set Empty-Bool-Set Full-Bool-Set))

(define empty-bool-set (Empty-Bool-Set))

(: bool-set-intersect (case-> (-> Bool-Set Nonfull-Bool-Set Nonfull-Bool-Set)
                              (-> Nonfull-Bool-Set Bool-Set Nonfull-Bool-Set)
                              (-> Bool-Set Bool-Set Bool-Set)))
(define (bool-set-intersect A B)
  (cond [(Full-Bool-Set? A)  B]
        [(Full-Bool-Set? B)  A]
        [(Empty-Bool-Set? A)  A]
        [(Empty-Bool-Set? B)  B]
        [(eq? A B)  A]
        [else  empty-bool-set]))
