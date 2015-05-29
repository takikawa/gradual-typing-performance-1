#lang typed/racket/base

;; Represent probabilities using symmetric logs

(provide
  flprob-random
  flprob-midpoint
  flprob1-
  flprob+
  flprob-
  flprob*
  flprob/
  flprob=
  flprob<
  flprob<=
  flprob-boolean
  flonum->flprob
  flprob->flonum)

;; -----------------------------------------------------------------------------

(require
  benchmark-util
  (only-in racket/flonum
    fl<
    fl<=
    fl=
    fllog)
  math/flonum
)
(require/typed/check "flonum-utils.rkt"
 [flexpm0.5/error (-> Flonum (Values Flonum Flonum))])
(require/typed/check "flonum-flops.rkt"
  [fllog-random (-> Flonum Flonum Flonum)]
  [fllog/error (-> Flonum (Values Flonum Flonum))]
  [fllog1p/error (-> Flonum (Values Flonum Flonum))])

;; =============================================================================

(: flprob-boolean (-> Flonum Boolean))
(define (flprob-boolean p)
  (if (< p 0.0)
      (< (random) (flexp p))
      (< (flexp (- p)) (random))))

(: flprob? (-> Flonum Boolean))
(define (flprob? x)
  (or (<= x (fllog 0.5))
      (< (fllog 2.0) x)))

(define-syntax-rule (make-flprob-comp-fun name flop)
  (λ ([x : Flonum] [y : Flonum])
    (cond [(not (flprob? x))  (raise-argument-error name "flprob?" 0 x y)]
          [(not (flprob? y))  (raise-argument-error name "flprob?" 1 x y)]
          [else  (flop x y)])))

(define flprob=  (make-flprob-comp-fun 'flprob=  fl=))
(define flprob<  (make-flprob-comp-fun 'flprob<  fl<))
(define flprob<=  (make-flprob-comp-fun 'flprob<=  fl<=))

;; -----------------------------------------------------------------------------

;(: flprob-fast-canonicalize (-> Flonum Flonum))
;; Turns a nearly symmetric log probability into a symmetric log probability
(define-syntax-rule (flprob-fast-canonicalize x-stx)
  (let ([x : Flonum  x-stx])
    (cond [(<= x (fllog 0.5))  x]
          [(< x 0.0)  (+ (fllog 2.0) (- x (fllog 0.5)))]
          [(<= x (fllog 2.0))  (+ (fllog 0.5) (- x (fllog 2.0)))]
          [else  x])))

(: flonum->flprob/error (-> Flonum (Values Flonum Flonum)))
(define (flonum->flprob/error x)
  (if (<= x 0.5)
      (fllog/error x)
      (let-values ([(y.hi y.lo)  (fllog1p/error (- x))])
        (values (- y.hi) (- y.lo)))))

(: flonum->flprob (-> Flonum Flonum))
(define (flonum->flprob x)
  (define-values (y.hi y.lo) (flonum->flprob/error x))
  (flprob-fast-canonicalize (+ y.hi y.lo)))

(: make-flprob-2d-fun (-> (-> Flonum Flonum (Values Flonum Flonum))
                          (-> Flonum Flonum Flonum)))
(define ((make-flprob-2d-fun flop/error) x y)
  (if (and (flprob? x) (flprob? y))
      (let-values ([(z.hi z.lo)  (flop/error x y)])
        (flprob-fast-canonicalize (+ z.hi z.lo)))
      +nan.0))

(: flprob+/error (-> Flonum Flonum (Values Flonum Flonum)))
;; Assumes x and y are in canonical form
(define (flprob+/error x y)
  (cond [(> x (- y))  (values +nan.0 0.0)]  ; Result > 1
        [(= x (- y))  (values +inf.0 0.0)]  ; Result = 1
        [else
         (let ([x  (max x y)]
               [y  (min x y)])
           (cond [(= x -inf.0)  (values -inf.0 0.0)]  ; Result 0 + 0 = 0
                 ;; Quadrant III
                 [(< x 0.0)
                  (define-values (z.hi z.lo)
                    (let*-values ([(z.hi z.lo)  (fl-/error y x)]
                                  [(z.hi z.lo)  (fl2exp z.hi z.lo)]
                                  [(z.hi z.lo)  (fl2log1p z.hi z.lo)])
                      (fl2+ z.hi z.lo x)))
                  (if (fl2<= z.hi z.lo (fllog 0.5) 0.0)
                      (values z.hi z.lo)
                      (let*-values ([(a.hi a.lo)  (flexpm0.5/error x)]
                                    [(b.hi b.lo)  (flexpm0.5/error y)]
                                    [(z.hi z.lo)  (fl2+ a.hi a.lo b.hi b.lo)]
                                    [(z.hi z.lo)  (fl2log (- z.hi) (- z.lo))])
                        (values (- z.hi) (- z.lo))))]
                 ;; Quadrants II and IV
                 [else
                  (let*-values ([(z.hi z.lo)  (fl+/error x y)]
                                [(z.hi z.lo)  (fl2exp z.hi z.lo)]
                                [(z.hi z.lo)  (fl2log1p (- z.hi) (- z.lo))])
                    (fl2+ (- z.hi) (- z.lo) x))]))]))

(define flprob+ (make-flprob-2d-fun flprob+/error))
(: flprob1- (-> Flonum Flonum))
(define (flprob1- x)
  (cond [(or (< x (fllog 0.5)) (< (fllog 2.0) x))  (- x)]
        [(= x (fllog 0.5))  x]
        [else  +nan.0]))

(: flprob-/error (-> Flonum Flonum (Values Flonum Flonum)))
;; Assumes x and y are in canonical form
(define (flprob-/error x y)
  (cond [(< x y)  (values +nan.0 0.0)]             ; Result would be negative
        [(= x y)  (values -inf.0 0.0)]             ; Subtracting same number
        [(= x +inf.0)  (values (flprob1- y) 0.0)]  ; Subtracting from 1
        [(= y -inf.0)  (values x 0.0)]             ; Subtracting 0
        ;; Quadrant III
        [(< x 0.0)
         (let*-values ([(z.hi z.lo)  (fl-/error y x)]
                       [(z.hi z.lo)  (fl2expm1 z.hi z.lo)]
                       [(z.hi z.lo)  (fl2log (- z.hi) (- z.lo))])
           (fl2+ z.hi z.lo x))]
        ;; Quadrant IV
        [(< y 0.0)
         (define-values (z.hi z.lo)
           (let*-values ([(a.hi a.lo)  (flexpm0.5/error (- x))]
                         [(b.hi b.lo)  (flexpm0.5/error y)]
                         [(z.hi z.lo)  (fl2+ a.hi a.lo b.hi b.lo)]
                         [(z.hi z.lo)  (cond [(fl2positive? z.hi z.lo)  (values 0.0 0.0)]
                                             [else                      (values z.hi z.lo)])])
             (fl2log (- z.hi) (- z.lo))))
         (if (fl2<= z.hi z.lo (fllog 0.5) 0.0)
             (values z.hi z.lo)
             (let ([x  (max (- x) y)]
                   [y  (min (- x) y)])
               (let*-values ([(z.hi z.lo)  (fl-/error y x)]
                             [(z.hi z.lo)  (fl2exp z.hi z.lo)]
                             [(z.hi z.lo)  (fl2log1p z.hi z.lo)]
                             [(z.hi z.lo)  (fl2+ z.hi z.lo x)])
                 (values (- z.hi) (- z.lo)))))]
        ;; Quadrant I
        [else
         (let*-values ([(z.hi z.lo)  (fl-/error y x)]
                       [(z.hi z.lo)  (fl2expm1 z.hi z.lo)]
                       [(z.hi z.lo)  (fl2log (- z.hi) (- z.lo))])
           (fl2- z.hi z.lo y))]))

(define flprob- (make-flprob-2d-fun flprob-/error))
(: flprob*/error (-> Flonum Flonum (Values Flonum Flonum)))
;; Assumes x and y are in canonical form
(define (flprob*/error x y)
  (let ([x  (max x y)]
        [y  (min x y)])
    (cond [(= x +inf.0)  (values y 0.0)]  ; Multiplying by 1.0
          [(= y -inf.0)  (values y 0.0)]  ; Multiplying by 0.0
          ;; Quadrant III
          [(< x 0.0)  (fl+/error x y)]
          ;; Quadrant II/IV
          [(< y 0.0)
           (let*-values ([(z.hi z.lo)  (flexp/error (- x))]
                         [(z.hi z.lo)  (fl2log1p (- z.hi) (- z.lo))])
             (fl2+ z.hi z.lo y))]
          ;; Quadrant I
          [else
           (let*-values ([(a.hi a.lo)  (flexpm1/error (- x))]
                         [(a.hi a.lo)  (fl2log (- a.hi) (- a.lo))]
                         [(b.hi b.lo)  (flexpm1/error (- y))]
                         [(b.hi b.lo)  (fl2log (- b.hi) (- b.lo))]
                         [(z.hi z.lo)  (fl2+ a.hi a.lo b.hi b.lo)])
             (cond [(fl2<= z.hi z.lo (fllog 0.5) 0.0)
                    (values z.hi z.lo)]
                   [(< x 80.0)
                    (let*-values ([(a.hi a.lo)  (flexpm1/error x)]
                                  [(b.hi b.lo)  (flexp/error y)]
                                  [(z.hi z.lo)  (fl2+ a.hi a.lo b.hi b.lo)]
                                  [(z.hi z.lo)  (fl2log z.hi z.lo)]
                                  [(z.hi z.lo)  (fl2- z.hi z.lo x)]
                                  [(z.hi z.lo)  (fl2- z.hi z.lo y)])
                      (values (- z.hi) (- z.lo)))]
                   [else
                    ;; At this point, (flexpm1 x) = (flexp x).
                    ;; The following code is derived from the case above using that fact, to
                    ;; avoid premature overflow.
                    (let*-values ([(z.hi z.lo)  (fl-/error y x)]
                                  [(z.hi z.lo)  (fl2exp z.hi z.lo)]
                                  [(z.hi z.lo)  (fl2log1p z.hi z.lo)]
                                  [(z.hi z.lo)  (fl2- z.hi z.lo y)])
                      (values (- z.hi) (- z.lo)))]))])))

(define flprob* (make-flprob-2d-fun flprob*/error))

(: flprob//error (-> Flonum Flonum (Values Flonum Flonum)))
(define (flprob//error x y)
  (cond [(< y x)  (values +nan.0 0.0)]       ; Result is > 1
        [(= y -inf.0)  (values +nan.0 0.0)]  ; Dividing by 0
        [(= x -inf.0)  (values x 0.0)]       ; Dividing 0
        [(= x y)  (values +inf.0 0.0)]       ; Dividing equal numbers
        ;; Quadrant III
        [(< y 0.0)
         (let-values ([(z.hi z.lo)  (fl-/error x y)])
           (if (fl2<= z.hi z.lo (fllog 0.5) 0.0)
               (values z.hi z.lo)
               (let*-values ([(z.hi z.lo)  (fl2expm1 z.hi z.lo)]
                             [(z.hi z.lo)  (fl2log (- z.hi) (- z.lo))])
                 (values (- z.hi) (- z.lo)))))]
        ;; Quadrant II
        [(< x 0.0)
         (let*-values ([(z.hi z.lo)  (flexp/error (- y))]
                       [(z.hi z.lo)  (fl2log1p (- z.hi) (- z.lo))]
                       [(z.hi z.lo)  (fl2+ (- z.hi) (- z.lo) x)])
           (if (fl2<= z.hi z.lo (fllog 0.5) 0.0)
               (values z.hi z.lo)
               (let*-values ([(z.hi z.lo)  (fl2exp z.hi z.lo)]
                             [(z.hi z.lo)  (fl2log1p (- z.hi) (- z.lo))])
                 (values (- z.hi) (- z.lo)))))]
        ;; Quadrant I
        [(or (< y 80.0) (> (- x y) -80.0))
         (define-values (a.hi a.lo) (fl-/error x y))
         (let*-values ([(b.hi b.lo)  (flexpm1/error y)]
                       [(c.hi c.lo)  (fl2expm1 a.hi a.lo)]
                       [(b.hi b.lo)  (fl2/ b.hi b.lo c.hi c.lo)]
                       [(b.hi b.lo)  (fl2log (- b.hi) (- b.lo))])
           (fl2+ a.hi a.lo b.hi b.lo))]
        [else
         ;; At this point, (flexpm1/error y) = (flexp/error y) and (flexpm1 (- x y)) ≈ -1.
         ;; The following code is derived from the case above using those facts, to avoid
         ;; premature overflow.
         (values x 0.0)]))

(define flprob/ (make-flprob-2d-fun flprob//error))

(: flprob->flonum/error (-> Flonum (Values Flonum Flonum)))
(define (flprob->flonum/error x)
  (cond [(not (flprob? x))  (values +nan.0 0.0)]
        [(< x 0.0)  (flexp/error x)]
        [else  (let-values ([(y.hi y.lo)  (flexpm1/error (- x))])
                 (values (- y.hi) (- y.lo)))]))

(: flprob->flonum (-> Flonum Nonnegative-Flonum))
(define (flprob->flonum x)
  (define-values (y.hi y.lo) (flprob->flonum/error x))
  (max 0.0 (min 1.0 (+ y.hi y.lo))))

(: flprob-random (-> Flonum Flonum Flonum))
(define (flprob-random a b)
  (if (not (and (flprob? a) (flprob? b)))
      +nan.0
      (flprob-fast-canonicalize
       (let ([a  (min a b)] [b  (max a b)])
         (cond [(<= b (fllog 0.5))  (fllog-random a b)]
               [(<= (fllog 2.0) a)  (- (fllog-random (- b) (- a)))]
               [else
                (define pa (flexpm1 (- a (fllog 0.5))))
                (define pb (flexpm1 (- (fllog 2.0) b)))
                (define r (random))
                (cond [(cond [(< pa pb)  (< r (/ pa (+ pa pb)))]
                             [else       (> r (/ pb (+ pa pb)))])
                       (fllog-random a (fllog 0.5))]
                      [else
                       (- (fllog-random (- b) (fllog 0.5)))])])))))

(: flprob-midpoint (-> Flonum Flonum Flonum))
;; Greatest observed error is 1.5252 ulps
(define (flprob-midpoint x y)
  (if (not (and (flprob? x) (flprob? y)))
      +nan.0
      (flprob-fast-canonicalize
       (let loop ([x x] [y y])
         (cond [(> x (- y))  (- (loop (- x) (- y)))]
               [(= x y)  x]
               [(= x (- y))  (fllog 0.5)]
               [else
                (let ([x  (max x y)]
                      [y  (min x y)])
                  (cond [(< x 0.0)
                         (+ (fllog 0.5) (+ x (fllog1p (flexp (fl- y x)))))]
                        [else
                         (+ (fllog 0.5) (fllog1p (- (flexp y) (flexp (- x)))))]))])))))
