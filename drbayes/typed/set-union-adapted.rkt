#lang typed/racket/base

;; TODO
;; make this first, then move things to a non-adaptor

(provide
 Nonempty-Set
 Set
 set-pair
 set-projs
 bot-tagged?
 set-take-stores
 set-intersect
 pair-set-intersect
 different
 different?
 )

;; -----------------------------------------------------------------------------

(require
 benchmark-util
 racket/list
 "set-bool-set-adapted.rkt"
 "set-null-set-adapted.rkt"
 "set-store-set-adapted.rkt"
 "set-extremal-set-adapted.rkt"
 "set-types-adapted.rkt"
 "set-real-set-adapted.rkt"
 "set-prob-set-adapted.rkt"
 )

;; ===================================================================================================
;; structs

(struct Bot-Union Base-Bot-Set ([hash : Bot-Union-Hash]) #:transparent)
(struct Top-Union Base-Top-Set ([hash : Top-Union-Hash]) #:transparent)

(struct Base-Pair-Set Base-Bot-Basic () #:transparent)
(define pair-set? Base-Pair-Set?)

(struct Empty-Pair-Set Base-Pair-Set ());empty-pair-set)
(define empty-pair-set (Empty-Pair-Set))
(define empty-pair-set? Empty-Pair-Set?)

(struct Full-Pair-Set Base-Pair-Set ());pairs)
(define pairs (Full-Pair-Set))
(define pairs? Full-Pair-Set?)

(struct Plain-Pair-Set Base-Pair-Set ([fst : Nonempty-Set] [snd : Nonempty-Set]) #:transparent)

(struct Bot-Tagged Base-Bot-Entry ([tag : Tag] [set : Nonempty-Set]) #:transparent)
(define bot-tagged? Bot-Tagged?)
(struct Top-Tagged Base-Top-Entry ([tag : Tag] [set : Nonfull-Set]) #:transparent)
(define top-tagged? Top-Tagged?)
(define top-tagged-set Top-Tagged-set)

(struct Top-Basic Base-Top-Entry ([set : Nonfull-Basic]) #:transparent)
(define top-basic? Top-Basic?)
(define top-basic-set Top-Basic-set)

(struct Different ())
(define different (Different))
(define different? Different?)

;; ===================================================================================================
;; types

(define-type Bot-Basic Nonempty-Basic)

(define-type Bot-Entry (U Bot-Basic Bot-Tagged))
(define bot-tagged-tag Bot-Tagged-tag)
(define bot-tagged-set Bot-Tagged-set)
(define-type Top-Entry (U Top-Basic Top-Tagged))

(define-type Plain-Set
  (U Bot-Basic Top-Basic
     Bot-Tagged Top-Tagged
     Bot-Union Top-Union))

(define-type Nonempty-Set (U Plain-Set Universe))
(define-type  Nonfull-Set (U Plain-Set Empty-Set))
(define-type          Set (U Plain-Set Universe Empty-Set))

(define-type Nonfull-Pair-Set (U Plain-Pair-Set Empty-Pair-Set))
(define-type Nonempty-Pair-Set (U Plain-Pair-Set Full-Pair-Set))
(define-type Pair-Set (U Plain-Pair-Set Full-Pair-Set Empty-Pair-Set))

(define-type Full-Basic
  (U Full-Real-Set
     Full-Prob-Set
     Full-Bool-Set
     Full-Null-Set
     Full-Pair-Set
     Full-Store-Set))

(define-type Empty-Basic
  (U Empty-Real-Set
     Empty-Prob-Set
     Empty-Bool-Set
     Empty-Null-Set
     Empty-Pair-Set
     Empty-Store-Set))

(define-type Plain-Basic
  (U Plain-Real-Set
     Plain-Prob-Set
     Plain-Bool-Set
     ;Plain-Null-Set  ; there aren't any plain null sets
     Plain-Pair-Set
     Plain-Store-Set))

(define-type Nonempty-Basic (U Plain-Basic Full-Basic))
(define-type  Nonfull-Basic (U Plain-Basic Empty-Basic))
(define-type          Basic (U Plain-Basic Full-Basic Empty-Basic))

(define-type Bot-Union-Hash (HashTable Tag Bot-Entry))
(define-type Top-Union-Hash (HashTable Tag Top-Entry))

;; =============================================================================
;; defines

(define bot-union? Bot-Union?)
(define top-union? Top-Union?)

(: top-tagged (case-> (-> Tag Nonfull-Set Top-Tagged)
                      (-> Tag Set (U Top-Tagged Universe))))
(define (top-tagged tag A)
  (if (universe? A) A (Top-Tagged tag A)))

(: bot-tagged (case-> (-> Tag Nonempty-Set Bot-Tagged)
                      (-> Tag Set (U Bot-Tagged Empty-Set))))
(define (bot-tagged tag A)
  (if (empty-set? A) A (Bot-Tagged tag A)))


(: top-union (-> Top-Entry Top-Entry Top-Union))
(define (top-union A0 A1)
  ;; (Top-Union ((inst hasheq2 Tag Top-Entry) (top-tag A0) A0 (top-tag A1) A1)))
  (Top-Union (make-immutable-hasheq (list (cons (top-tag A0) A0) (cons (top-tag A1) A1)))))

(: set-pair (case-> (-> Nonempty-Set Nonempty-Set Nonempty-Pair-Set)
                    (-> Set Set (U Empty-Set Nonempty-Pair-Set))))
(define (set-pair A B)
  (define C (pair-set A B))
  (if (empty-pair-set? C) empty-set C))

(: pair-set (case-> (-> Nonempty-Set Plain-Set Plain-Pair-Set)
                    (-> Plain-Set Nonempty-Set Plain-Pair-Set)
                    (-> Nonempty-Set Nonempty-Set Nonempty-Pair-Set)
                    (-> Nonfull-Set Set Nonfull-Pair-Set)
                    (-> Set Nonfull-Set Nonfull-Pair-Set)
                    (-> Set Set Pair-Set)))
(define (pair-set A1 A2)
  (cond [(and (universe? A1) (universe? A2))  pairs]
        [(empty-set? A1)  empty-pair-set]
        [(empty-set? A2)  empty-pair-set]
        [else  (Plain-Pair-Set A1 A2)]))

(: bot-tag (-> Bot-Entry Tag))
(define (bot-tag A)
  (if (bot-basic? A) (basic-tag A) (bot-tagged-tag A)))

(define real-tag 'real)
(define prob-tag 'prob)
(define bool-tag 'bool)
(define null-tag 'null)
(define pair-tag 'pair)
(define store-tag 'store)

(define top-tagged-tag Top-Tagged-tag)

(: basic-intersect (case-> (-> Nonfull-Basic Basic (U Different Nonfull-Basic))
                           (-> Basic Nonfull-Basic (U Different Nonfull-Basic))
                           (-> Basic Basic (U Different Basic))))
(define (basic-intersect A B)
  (cond [(and (real-set? A) (real-set? B))  (real-set-intersect A B)]
        [(and (prob-set? A) (prob-set? B))  (prob-set-intersect A B)]
        [(and (null-set? A) (null-set? B))  (null-set-intersect A B)]
        [(and (pair-set? A) (pair-set? B))  (pair-set-intersect A B)]
        [(and (bool-set? A) (bool-set? B))  (bool-set-intersect A B)]
        [(and (store-set? A) (store-set? B))  (store-set-intersect A B)]
        [else  different]))

(: empty-basic? (-> Any Boolean : Empty-Basic))
(define (empty-basic? A)
  (or (empty-real-set? A)
      (empty-prob-set? A)
      (empty-bool-set? A)
      (empty-null-set? A)
      (empty-pair-set? A)
      (empty-store-set? A)))

(: full-basic? (-> Any Boolean : Full-Basic))
(define (full-basic? A)
  (or (reals? A)
      (probs? A)
      (bools? A)
      (nulls? A)
      (pairs? A)
      (stores? A)))

(: top-basic (case-> (-> Nonfull-Basic Top-Basic)
                     (-> Basic (U Top-Basic Universe))))
(define (top-basic A)
  (if (full-basic? A) universe (Top-Basic A)))

(: bot-basic (case-> (-> Bot-Basic Bot-Basic)
                     (-> Basic (U Bot-Basic Empty-Set))))
(define (bot-basic A)
  (if (empty-basic? A) empty-set A))

(: top-basic-tag (-> Top-Basic Tag))
(define (top-basic-tag A) (basic-tag (top-basic-set A)))

(: top-tag (-> Top-Entry Tag))
(define (top-tag A)
  (if (top-basic? A) (top-basic-tag A) (top-tagged-tag A)))

(: basic-tag (-> Basic Tag))
(define (basic-tag A)
  (cond [(real-set? A)  real-tag]
        [(prob-set? A)  prob-tag]
        [(bool-set? A)  bool-tag]
        [(null-set? A)  null-tag]
        [(pair-set? A)  pair-tag]
        [(store-set? A) store-tag]))

(: top-union-ref (-> (U Universe Top-Entry Top-Union) Tag (U Universe Top-Entry)))
(define (top-union-ref A tag)
  (cond [(universe? A)   A]
        [(top-entry? A)  (if (eq? tag (top-tag A)) A universe)]
        [else  (hash-ref (Top-Union-hash A) tag (λ () universe))]))

(: bot-union-add (-> (U Empty-Set Bot-Entry Bot-Union) (U Bot-Entry Bot-Union)
                     (U Bot-Entry Bot-Union)))
(define (bot-union-add A C)
  (cond [(empty-set? A)  C]
        [(bot-union? C)  (for/fold ([A A]) ([C  (in-list (bot-union-sets C))])
                           (bot-union-add A C))]
        [(bot-entry? A)
         (define a-tag (bot-tag A))
         (define c-tag (bot-tag C))
         (cond [(and (bot-entry? C) (eq? c-tag a-tag))  C]
               ;; [else  (Bot-Union (hasheq2 a-tag A c-tag C))])]
               [else  (Bot-Union (make-immutable-hasheq (list (cons a-tag A) (cons c-tag C))))])]
        [else
         (Bot-Union (hash-set (Bot-Union-hash A) (bot-tag C) C))]))

(: bot-union-remove (-> (U Empty-Set Bot-Entry Bot-Union) Tag (U Empty-Set Bot-Entry Bot-Union)))
(define (bot-union-remove A tag)
  (cond [(empty-set? A)  A]
        [(bot-entry? A)  (if (eq? (bot-tag A) tag) empty-set A)]
        [else  (define h (hash-remove (Bot-Union-hash A) tag))
               (define n (hash-count h))
               (cond [(= n 0)  empty-set]
                     [(= n 1)  (first (hash-values h))]
                     [else  (Bot-Union h)])]))

(: top-union-sets (-> (U Universe Top-Entry Top-Union) (Listof Top-Entry)))
(define (top-union-sets A)
  (cond [(universe? A)  empty]
        [(or (top-basic? A) (top-tagged? A))  (list A)]
        [else  (hash-values (Top-Union-hash A))]))

(: top-union-add (-> (U Universe Top-Entry Top-Union) (U Top-Entry Top-Union)
                     (U Top-Entry Top-Union)))
(define (top-union-add A C)
  (cond [(universe? A)  C]
        [(top-union? C)  (for/fold ([A A]) ([C  (in-list (top-union-sets C))])
                           (top-union-add A C))]
        [(top-entry? A)
         (define a-tag (top-tag A))
         (define c-tag (top-tag C))
         (cond [(and (top-entry? C) (eq? c-tag a-tag))  C]
               ;; [else  (Top-Union (hasheq2 a-tag A c-tag C))])]
               [else  (Top-Union (make-immutable-hasheq (list (cons a-tag A) (cons c-tag C))))])]
        [else
         (Top-Union (hash-set (Top-Union-hash A) (top-tag C) C))]))

(: bot-union-ref (-> (U Empty-Set Bot-Entry Bot-Union) Tag (U Empty-Set Bot-Entry)))
(define (bot-union-ref A tag)
  (cond [(empty-set? A)  A]
        [(bot-entry? A)  (if (eq? tag (bot-tag A)) A empty-set)]
        [else  (hash-ref (Bot-Union-hash A) tag (λ () empty-set))]))

(define-syntax-rule (make-set-take-basic pred? tag empty full)
  (λ (A)
    (let loop ([A A])
      (cond [(empty-set? A)  empty]
            [(universe? A)   full]
            [(bot-set? A)
             (cond [(bot-basic? A)   (if (pred? A) A empty)]
                   [(bot-tagged? A)  empty]
                   [else             (loop (bot-union-ref A tag))])]
            [else
             (cond [(top-basic? A)   (define Asub (top-basic-set A))
                                     (if (pred? Asub) Asub full)]
                   [(top-tagged? A)  full]
                   [else             (loop (top-union-ref A tag))])]))))

(: set-take-pairs (-> Set Pair-Set))
(define set-take-pairs (make-set-take-basic pair-set? pair-tag empty-pair-set pairs))

(: set-take-stores (-> Set Store-Set))
(define set-take-stores (make-set-take-basic store-set? store-tag empty-store-set stores))

(: pair-set-projs (case-> (-> Empty-Pair-Set (Values Empty-Set Empty-Set))
                          (-> Nonempty-Pair-Set (Values Nonempty-Set Nonempty-Set))
                          (-> Pair-Set (Values Set Set))))
(define (pair-set-projs A)
  (cond [(empty-pair-set? A)  (values empty-set empty-set)]
        [(pairs? A)  (values universe universe)]
        [else  (values (Plain-Pair-Set-fst A)
                       (Plain-Pair-Set-snd A))]))

(: set-projs (-> Set (Values Set Set)))
(define (set-projs A)
  (pair-set-projs (set-take-pairs A)))

(: bot-union-sets (-> (U Empty-Set Bot-Entry Bot-Union) (Listof Bot-Entry)))
(define (bot-union-sets A)
  (cond [(empty-set? A)  empty]
        [(or (bot-basic? A) (bot-tagged? A))  (list A)]
        [else  (hash-values (Bot-Union-hash A))]))

;; -----------------------------------------------------------------------------
;; Intersection

(: set-intersect (case-> (-> Set Nonfull-Set Nonfull-Set)
                         (-> Nonfull-Set Set Nonfull-Set)
                         (-> Set Set Set)))
(define (set-intersect A B)
  (cond [(universe? A)   B]
        [(universe? B)   A]
        [(eq? A B)  B]
        [(empty-set? A)  A]
        [(empty-set? B)  B]
        [(bot-set? A)
         (if (bot-set? B)
             (cond [(and (bot-entry? A) (bot-entry? B))  (bot-bot-entry-intersect A B)]
                   [else  (bot-bot-intersect A B)])
             (cond [(and (bot-entry? A) (top-entry? B))  (bot-top-entry-intersect A B)]
                   [else  (bot-top-intersect A B)]))]
        [else
         (if (bot-set? B)
             (cond [(and (top-entry? A) (bot-entry? B))  (bot-top-entry-intersect B A)]
                   [else  (bot-top-intersect B A)])
             (cond [(and (top-entry? A) (top-entry? B))  (top-top-entry-intersect A B)]
                   [else  (top-top-intersect A B)]))]))

(: bot-bot-intersect (-> (U Bot-Entry Bot-Union) (U Bot-Entry Bot-Union)
                         (U Empty-Set Bot-Entry Bot-Union)))
(define (bot-bot-intersect A B)
  (for/fold: ([C : (U Empty-Set Bot-Entry Bot-Union)  empty-set]
              ) ([Bsub  (in-list (bot-union-sets B))])
    (define b-tag (bot-tag Bsub))
    (define Asub (bot-union-ref A b-tag))
    (define Dsub (if (empty-set? Asub) empty-set (bot-bot-entry-intersect Asub Bsub)))
    (if (empty-set? Dsub) C (bot-union-add C Dsub))))

(: bot-top-intersect (-> (U Bot-Entry Bot-Union) (U Top-Entry Top-Union)
                         (U Bot-Entry Bot-Union Empty-Set)))
(define (bot-top-intersect A B)
  (for/fold: ([A : (U Empty-Set Bot-Entry Bot-Union)  A]) ([Asub  (in-list (bot-union-sets A))])
    (define a-tag (bot-tag Asub))
    (define Bsub (top-union-ref B a-tag))
    (define Dsub (if (universe? Bsub) Asub (bot-top-entry-intersect Asub Bsub)))
    (if (empty-set? Dsub) (bot-union-remove A a-tag) (bot-union-add A Dsub))))

(: top-top-intersect (-> (U Top-Entry Top-Union) (U Top-Entry Top-Union) (U Top-Entry Top-Union)))
(define (top-top-intersect A B)
  (for/fold ([A A]) ([Bsub  (in-list (top-union-sets B))])
    (define b-tag (top-tag Bsub))
    (define Asub (top-union-ref A b-tag))
    (define Dsub (if (universe? Asub) Bsub (top-top-entry-intersect Asub Bsub)))
    (top-union-add A Dsub)))

(: bot-bot-entry-intersect (-> Bot-Entry Bot-Entry (U Bot-Entry Empty-Set)))
(define (bot-bot-entry-intersect A B)
  (cond [(and (bot-basic? A)  (bot-basic? B))   (bot-bot-basic-intersect A B)]
        [(and (bot-tagged? A) (bot-tagged? B))  (bot-bot-tagged-intersect A B)]
        [else  empty-set]))

(: bot-top-entry-intersect (-> Bot-Entry Top-Entry (U Bot-Entry Empty-Set)))
(define (bot-top-entry-intersect A B)
  (cond [(and (bot-basic? A)  (top-basic? B))   (bot-top-basic-intersect A B)]
        [(and (bot-tagged? A) (top-tagged? B))  (bot-top-tagged-intersect A B)]
        [else  A]))

(: top-top-entry-intersect (-> Top-Entry Top-Entry (U Top-Entry Top-Union)))
(define (top-top-entry-intersect A B)
  (cond [(and (top-basic? A)  (top-basic? B))   (top-top-basic-intersect A B)]
        [(and (top-tagged? A) (top-tagged? B))  (top-top-tagged-intersect A B)]
        [else  (top-union A B)]))

(: bot-bot-basic-intersect (-> Bot-Basic Bot-Basic (U Bot-Basic Empty-Set)))
(define (bot-bot-basic-intersect A B)
  (define C (basic-intersect A B))
  (cond [(different? C)  empty-set]
        [else  (bot-basic C)]))

(: bot-top-basic-intersect (-> Bot-Basic Top-Basic (U Bot-Basic Empty-Set)))
(define (bot-top-basic-intersect A B)
  (define Bsub (top-basic-set B))
  (define Csub (basic-intersect A Bsub))
  (cond [(different? Csub)  A]
        [else  (bot-basic Csub)]))

(: top-top-basic-intersect (-> Top-Basic Top-Basic (U Top-Basic Top-Union)))
(define (top-top-basic-intersect A B)
  (define Asub (top-basic-set A))
  (define Bsub (top-basic-set B))
  (define Csub (basic-intersect Asub Bsub))
  (cond [(different? Csub)  (top-union A B)]
        [(eq? Csub Asub)  A]
        [(eq? Csub Bsub)  B]
        [else  (top-basic Csub)]))

(: bot-bot-tagged-intersect (-> Bot-Tagged Bot-Tagged (U Bot-Tagged Empty-Set)))
(define (bot-bot-tagged-intersect A B)
  (define a-tag (bot-tagged-tag A))
  (cond [(eq? a-tag (bot-tagged-tag B))
         (define Asub (bot-tagged-set A))
         (define Bsub (bot-tagged-set B))
         (define Csub (set-intersect Asub Bsub))
         (cond [(eq? Csub Asub)  A]
               [(eq? Csub Bsub)  B]
               [else  (bot-tagged a-tag Csub)])]
        [else  empty-set]))

(: bot-top-tagged-intersect (-> Bot-Tagged Top-Tagged (U Bot-Tagged Empty-Set)))
(define (bot-top-tagged-intersect A B)
  (define a-tag (bot-tagged-tag A))
  (cond [(eq? a-tag (top-tagged-tag B))
         (define Asub (bot-tagged-set A))
         (define Bsub (top-tagged-set B))
         (define Csub (set-intersect Asub Bsub))
         (cond [(eq? Csub Asub)  A]
               [else  (bot-tagged a-tag Csub)])]
        [else  A]))

(: top-top-tagged-intersect (-> Top-Tagged Top-Tagged (U Top-Tagged Top-Union)))
(define (top-top-tagged-intersect A B)
  (define a-tag (top-tagged-tag A))
  (cond [(eq? a-tag (top-tagged-tag B))
         (define Asub (top-tagged-set A))
         (define Bsub (top-tagged-set B))
         (define Csub (set-intersect Asub Bsub))
         (cond [(eq? Csub Asub)  A]
               [(eq? Csub Bsub)  B]
               [else  (top-tagged a-tag Csub)])]
        [else  (top-union A B)]))

;; Intersection
;; -----------------------------------------------------------------------------

(: pair-set-intersect (case-> (-> Pair-Set Nonfull-Pair-Set Nonfull-Pair-Set)
                              (-> Nonfull-Pair-Set Pair-Set Nonfull-Pair-Set)
                              (-> Pair-Set Pair-Set Pair-Set)))
(define (pair-set-intersect A B)
  (cond [(pairs? A)  B]
        [(pairs? B)  A]
        [(eq? A B)  A]
        [(empty-pair-set? A)  A]
        [(empty-pair-set? B)  B]
        [else
         (let* ([A1  (Plain-Pair-Set-fst A)]
                [B1  (Plain-Pair-Set-fst B)]
                [C1  (set-intersect A1 B1)])
           (if (empty-set? C1)
               empty-pair-set
               (let* ([A2  (Plain-Pair-Set-snd A)]
                      [B2  (Plain-Pair-Set-snd B)]
                      [C2  (set-intersect A2 B2)])
                 (if (empty-set? C2)
                     empty-pair-set
                     (cond [(and (eq? A1 C1) (eq? A2 C2))  A]
                           [(and (eq? B1 C1) (eq? B2 C2))  B]
                           [else  (Plain-Pair-Set C1 C2)])))))]))
