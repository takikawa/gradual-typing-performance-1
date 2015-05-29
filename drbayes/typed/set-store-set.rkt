#lang typed/racket/base

(provide
  (struct-out Base-Store-Set)
  (struct-out Full-Store-Set)
  (struct-out Empty-Store-Set)
  (struct-out Plain-Store-Set)
  stores
  store-set-realize
  store-set-random-measure
  store-set-random-proj
  store-set-branch-proj
  store-set-random-unproj
  store-set-branch-unproj
  store-set-intersect
)

;; -----------------------------------------------------------------------------

(require
  benchmark-util
  racket/match
  racket/promise
  "set-bool-set-adapted.rkt"
  "set-types-adapted.rkt"
  "set-bottom-adapted.rkt"
  "set-store-adapted.rkt"
  "flonum-probability-adapted.rkt"
  ;; -- TODO adapt
  "set-prob-set-adapted.rkt"
)
;(require/typed/check
;  [empty-prob-set Prob-Set]
;  [probs? (-> Any Boolean)]
;  [prob-set-sample-point (-> Nonempty-Prob-Set (U Bad-Prob Prob))]
;  [prob-set-measure (-> Prob-Set Prob)]
;)
(require/typed/check "set-store-index.rkt"
  [j0 (Listof Boolean)]
  [left (-> (Listof Boolean) (Listof Boolean))]
  [right (-> (Listof Boolean) (Listof Boolean))])

;; =============================================================================

(define-type Store-Index (Listof Boolean))
(define-type Store-Set (U Plain-Store-Set Empty-Store-Set Full-Store-Set))

(define-type Nonfull-Store-Set (U Plain-Store-Set Empty-Store-Set))

(struct Base-Store-Set Base-Bot-Basic () #:transparent)
(struct Full-Store-Set Base-Store-Set ())
(struct Empty-Store-Set Base-Store-Set ())

(define empty-store-set (Empty-Store-Set))
(define empty-store-set? Empty-Store-Set?)

(define stores (Full-Store-Set))
(define stores? Full-Store-Set?)

(define-type Nonempty-Store-Set (U Plain-Store-Set Full-Store-Set))

(struct Plain-Store-Set Base-Store-Set
  ([random : Nonempty-Prob-Set]
   [branch : Nonempty-Bool-Set]
   [left   : Nonempty-Store-Set]
   [right  : Nonempty-Store-Set])
  #:transparent)

(: make-bottom-trace-value (-> Store-Index Bottom))
(define (make-bottom-trace-value j)
  (bottom (delay (format "no branch decision at index ~a" j))))

(: stores-realize (-> Store-Index Store))
(define (stores-realize j)
  (let loop ([j j])
    (Store (delay (prob-random prob-0 prob-1))
           (delay (make-bottom-trace-value j))
           (delay (loop (left j)))
           (delay (loop (right j))))))

(: store-set-realize (-> Nonempty-Store-Set Store))
;; Sample each real axis, take infimum of each boolean axis
(define (store-set-realize S)
  (let loop : Store ([S S] [j j0])
    (cond [(stores? S)  (stores-realize j)]
          [(Plain-Store-Set? S)
           (match-define (Plain-Store-Set X B L R) S)
           (Store (if (probs? X)
                      (delay (prob-random prob-0 prob-1))
                      (prob-set-sample-point X))
                  (if (bools? B)
                      (delay (make-bottom-trace-value j))
                      (trues? B))
                  (loop L (left j))
                  (loop R (right j)))])))

(: make-store-set-random-measure (-> (-> Nonempty-Prob-Set Prob) (-> Prob Prob Prob)
                                     (-> Store-Set Prob)))
(define ((make-store-set-random-measure prob-set-measure prob*) S)
  (if (empty-store-set? S)
      prob-0
      (let loop ([S S])
        (if (stores? S)
            prob-1
            (match-let ([(Plain-Store-Set X B L R)  S])
              (prob* (prob-set-measure X)
                     (prob* (loop L) (loop R))))))))

(define store-set-random-measure
  (make-store-set-random-measure prob-set-measure prob*))

;; TODO can I narrow the case->???
(: store-set-random-proj (case-> (-> Empty-Store-Set Store-Index Empty-Prob-Set)
                                 (-> Nonempty-Store-Set Store-Index Nonempty-Prob-Set)
                                 (-> Store-Set Store-Index Prob-Set)))
(define (store-set-random-proj S j)
  (if (empty-store-set? S)
      empty-prob-set
      (let loop ([S S] [j  (reverse j)])
        (cond [(stores? S)  probs]
              [(eq? '() j)  (Plain-Store-Set-random S)]
              [(car j)  (loop (Plain-Store-Set-left  S) (cdr j))]
              [else       (loop (Plain-Store-Set-right S) (cdr j))]))))

(: store-set-branch-proj (case-> (-> Empty-Store-Set Store-Index Empty-Bool-Set)
                                 (-> Nonempty-Store-Set Store-Index Nonempty-Bool-Set)
                                 (-> Store-Set Store-Index Bool-Set)))
(define (store-set-branch-proj S j)
  (if (empty-store-set? S)
      empty-bool-set
      (let loop ([S S] [j  (reverse j)])
        (cond [(stores? S)  bools]
              [(eq? '() j)  (Plain-Store-Set-branch S)]
              [(car j)  (loop (Plain-Store-Set-left  S) (cdr j))]
              [else       (loop (Plain-Store-Set-right S) (cdr j))]))))

;; TODO kill the case->
(: store-set-projs (case-> (-> Empty-Store-Set (Values Empty-Prob-Set
                                                       Empty-Bool-Set
                                                       Empty-Store-Set
                                                       Empty-Store-Set))
                           (-> Nonempty-Store-Set (Values Nonempty-Prob-Set
                                                          Nonempty-Bool-Set
                                                          Nonempty-Store-Set
                                                          Nonempty-Store-Set))
                           (-> Store-Set (Values Prob-Set
                                                 Bool-Set
                                                 Store-Set
                                                 Store-Set))))
(define (store-set-projs S)
  (cond [(empty-store-set? S)  (values empty-prob-set empty-bool-set empty-store-set empty-store-set)]
        [(stores? S)  (values probs bools stores stores)]
        [else  (values (Plain-Store-Set-random S)
                       (Plain-Store-Set-branch S)
                       (Plain-Store-Set-left   S)
                       (Plain-Store-Set-right  S))]))

(: store-set-unrandom (-> Store-Set Prob-Set Store-Set))
(define (store-set-unrandom S X)
  (define-values (X* B L R) (store-set-projs S))
  (let ([X  (prob-set-intersect X* X)])
    (if (eq? X* X) S (store-set X B L R))))

;; TODO
(: store-set (case-> (-> Nonempty-Prob-Set
                         Nonempty-Bool-Set
                         Nonempty-Store-Set
                         Nonempty-Store-Set
                         Nonempty-Store-Set)
                     (-> Prob-Set Bool-Set Store-Set Store-Set Store-Set)))
(define (store-set X B L R)
  (cond [(or (empty-prob-set? X) (empty-bool-set? B) (empty-store-set? L) (empty-store-set? R))
         empty-store-set]
        [(and (probs? X) (bools? B) (stores? L) (stores? R))
         stores]
        [else
         (Plain-Store-Set X B L R)]))

(: store-set-random-unproj (-> Store-Set Store-Index Prob-Set Store-Set))
(define (store-set-random-unproj S j X)
  (if (empty-store-set? S)
      empty-store-set
      (let loop ([S S] [j  (reverse j)])
        (cond [(eq? '() j)  (store-set-unrandom S X)]
              [(car j)
               (define-values (X B L* R) (store-set-projs S))
               (define L (loop L* (cdr j)))
               (if (eq? L* L) S (store-set X B L R))]
              [else
               (define-values (X B L R*) (store-set-projs S))
               (define R (loop R* (cdr j)))
               (if (eq? R* R) S (store-set X B L R))]))))

(: store-set-unbranch (-> Store-Set Bool-Set Store-Set))
(define (store-set-unbranch S B)
  (define-values (X B* L R) (store-set-projs S))
  (let ([B  (bool-set-intersect B* B)])
    (if (eq? B* B) S (store-set X B L R))))

(: store-set-branch-unproj (-> Store-Set Store-Index Bool-Set Store-Set))
(define (store-set-branch-unproj S j B)
  (if (or (empty-store-set? S) (empty-bool-set? B))
      empty-store-set
      (let loop ([S S] [j  (reverse j)])
        (cond [(eq? '() j)  (store-set-unbranch S B)]
              [(car j)
               (define-values (X B L* R) (store-set-projs S))
               (define L (loop L* (cdr j)))
               (if (eq? L* L) S (store-set X B L R))]
              [else
               (define-values (X B L R*) (store-set-projs S))
               (define R (loop R* (cdr j)))
               (if (eq? R* R) S (store-set X B L R))]))))

(: store-set-intersect (case-> (-> Store-Set Nonfull-Store-Set Nonfull-Store-Set)
                               (-> Nonfull-Store-Set Store-Set Nonfull-Store-Set)
                               (-> Store-Set Store-Set Store-Set)))
(define (store-set-intersect S1 S2)
  (cond [(stores? S1)  S2]
        [(stores? S2)  S1]
        [(eq? S1 S2)  S1]
        [(empty-store-set? S1)  S1]
        [(empty-store-set? S2)  S2]
        [else
         (match-define (Plain-Store-Set X1 B1 L1 R1) S1)
         (match-define (Plain-Store-Set X2 B2 L2 R2) S2)
         (let ([B  (bool-set-intersect B1 B2)])
           (if (empty-bool-set? B)
               empty-store-set
               (let ([X  (prob-set-intersect X1 X2)])
                 (if (empty-prob-set? X)
                     empty-store-set
                     (let ([L  (store-set-intersect L1 L2)])
                       (if (empty-store-set? L)
                           empty-store-set
                           (let ([R  (store-set-intersect R1 R2)])
                             (if (empty-store-set? R)
                                 empty-store-set
                                 (cond [(and (eq? X X1) (eq? B B1) (eq? L L1) (eq? R R1))  S1]
                                       [(and (eq? X X2) (eq? B B2) (eq? L L2) (eq? R R2))  S2]
                                       [else  (Plain-Store-Set X B L R)])))))))))]))
