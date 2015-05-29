#lang typed/racket

;; ray-tracing.rkt

(require
  benchmark-util
  racket/flonum
  "language-functions.rkt"
  "language-macros.rkt"
  ;; "arrow-proc-arrow.rkt" ;; outside the experiment
)
(require/typed/check "search-parameters.rkt"
  [drbayes-refinement-search? (Parameterof Boolean)]
  [drbayes-refinement-sample? (Parameterof Boolean)]
  [drbayes-sample-max-splits (Parameterof Natural)]
)
(require/typed/check "search-sample.rkt"
  (drbayes-sample (-> meaning Natural (Values (Listof Value) (Listof Nonnegative-Flonum)))))

;; =============================================================================
;; Actual benchmark

(: main (-> Index Void))
(define (main n)
  ;; Set up parameters
  (drbayes-refinement-search? #t)
  (drbayes-refinement-sample? #t)
  (drbayes-sample-max-splits 0)
  ;; Define some vectors
  (define/drbayes (vec+ lst1 lst2)
    (list (+ (list-ref lst1 0) (list-ref lst2 0))
          (+ (list-ref lst1 1) (list-ref lst2 1))
          (+ (list-ref lst1 2) (list-ref lst2 2))))
  (define/drbayes (vec- lst1 lst2)
    (list (- (list-ref lst1 0) (list-ref lst2 0))
          (- (list-ref lst1 1) (list-ref lst2 1))
          (- (list-ref lst1 2) (list-ref lst2 2))))
  (define/drbayes (vec-neg lst)
    (list (- (list-ref lst 0))
          (- (list-ref lst 1))
          (- (list-ref lst 2))))
  (define/drbayes (vec-mag^2 lst)
    (+ (+ (sqr (list-ref lst 0))
          (sqr (list-ref lst 1)))
       (sqr (list-ref lst 2))))
  (define/drbayes (vec-dot lst1 lst2)
    (+ (+ (* (list-ref lst1 0) (list-ref lst2 0))
          (* (list-ref lst1 1) (list-ref lst2 1)))
       (* (list-ref lst1 2) (list-ref lst2 2))))
  (define/drbayes (vec-norm lst)
    (let ([z  (sqrt (vec-mag^2 lst))])
      (list (/ (list-ref lst 0) z)
            (/ (list-ref lst 1) z)
            (/ (list-ref lst 2) z))))
  (define/drbayes (vec-scale lst s)
    (list (* (list-ref lst 0) s)
          (* (list-ref lst 1) s)
          (* (list-ref lst 2) s)))
  (define/drbayes (min a b)
    (strict-if (a . < . b) a b))
  (define/drbayes (max a b)
    (strict-if (a . > . b) a b))
  (define/drbayes (triangle-inv-cdf p)
    (strict-if (< p 0.5)
               (- (sqrt (* 2 p)) 1)
               (- 1 (sqrt (* 2 (- 1 p))))))

  ;; Unnormalized uniform direction
  (define/drbayes (uniform-vec)
    (list (triangle-inv-cdf (random))
          (triangle-inv-cdf (random))
          (triangle-inv-cdf (random))))

  (define/drbayes (uniform-vec/dir n)
    (let ([v  (uniform-vec)])
      (strict-if (positive? (vec-dot n v)) v (vec-neg v))))

  (struct/drbayes collision (time point normal))

  (define/drbayes (closer-collision c1 c2)
    (if (and (collision? c1) (collision? c2))
        (if ((collision-time c2) . < . (collision-time c1)) c2 c1)
        (if (collision? c1) c1 c2)))

  (define/drbayes (ray-reflect d n)
    (vec- d (vec-scale n (* 2.0 (vec-dot d n)))))

  (define/drbayes (ray-sphere-intersect p v c r)
    (let* ([dp  (vec- c p)]
           [-b/2  (vec-dot dp v)]
           [disc  (- (sqr -b/2) (- (vec-mag^2 dp) (sqr r)))])
      (if (positive? disc)
          (let ([t  (- -b/2 (sqrt disc))])
            (if (positive? t)
                (let* ([p1  (vec+ p (vec-scale v t))]
                       [n   (vec-scale (vec- p1 c) (/ r))])
                  (collision t p1 n))
                #f))
          #f)))

  (define/drbayes (ray-plane-intersect p0 v n d)
    (let ([denom  (- (vec-dot v n))])
      (if (positive? denom)
          (let ([t  (/ (+ d (vec-dot p0 n)) denom)])
            (if (positive? t)
                (collision t (vec+ p0 (vec-scale v t)) n)
                #f))
          #f)))

  (define plane1-n (list 0.0 1.0 0.0))
  (define plane1-d 0.0)
  (define plane2-n (list 0.0 -1.0 0.0))
  (define plane2-d 1.0)
  (define plane3-n (list 1.0 0.0 0.0))
  (define plane3-d 0.0)
  (define plane4-n (list -1.0 0.0 0.0))
  (define plane4-d 1.0)
  (define plane5-n (list 0.0 0.0 1.0))
  (define plane5-d 0.0)
  (define plane6-n (list 0.0 0.0 -1.0))
  (define plane6-d 1.0)

  (define sphere0-pc (list 0.4 0.6 0.4))
  (define sphere0-r 0.25)

  (define/drbayes (box-intersect p0 d)
    (closer-collision
     (closer-collision
       (ray-plane-intersect p0 d (const plane3-n) (const plane3-d))
       (ray-plane-intersect p0 d (const plane4-n) (const plane4-d)))
     (closer-collision
      (closer-collision
       (ray-plane-intersect p0 d (const plane1-n) (const plane1-d))
       (ray-plane-intersect p0 d (const plane2-n) (const plane2-d)))
      (closer-collision
       (ray-plane-intersect p0 d (const plane5-n) (const plane5-d))
       (ray-plane-intersect p0 d (const plane6-n) (const plane6-d))))))

  (define/drbayes (trace-light ps d)
    (let* ([p0  (car ps)]
           [c   (box-intersect p0 d)])
      (if (collision? c)
          ;(cons (collision-point c) ps)
          (let* ([p0  (collision-point c)]
                 [n  (collision-normal c)]
                 [d  (uniform-vec/dir n)]
                 [ps  (cons p0 ps)]
                 [c   (ray-plane-intersect p0 d (const plane1-n) (const plane1-d))])
            (if (collision? c)
                (cons (collision-point c) ps)
                ps))
          ps)))
  (define p0 (list 0.9 0.25 0.9))

  (define/drbayes (start-p)
    (const p0))

  (define/drbayes (e)
    (let* ([ps  (trace-light (list (start-p)) (uniform-vec))]
           [p   (list-ref ps 0)])
      (cond [(< (list-ref p 0) 0.49)  (fail)]
            [(> (list-ref p 0) 0.51)  (fail)]
            [(< (list-ref p 1) -0.001)  (fail)]
            [(> (list-ref p 1) 0.001)  (fail)]
            [(< (list-ref p 2) 0.49)  (fail)]
            [(> (list-ref p 2) 0.51)  (fail)]
            [else  ps])))

  (drbayes-sample (drbayes (e)) n)
  (void)
)

;; (time (main 1000)) ;; 9.6 seconds
(time (main 100)) ;; 0.3 seconds
