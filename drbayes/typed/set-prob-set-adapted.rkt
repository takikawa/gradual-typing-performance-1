#lang typed/racket/base

;; TODO wtf are we gonna do about un-typing this file?
;; (replace define-singleton with struct, add adaptor)

(provide
  Prob-Set
  Empty-Prob-Set
  Nonempty-Prob-Set
  Nonempty-Prob-Interval
  Plain-Prob-Set
  Full-Prob-Set
  (struct-out Prob-Interval-List)
  ;; ---
  empty-prob-set
  empty-prob-set?
  ;; ---
  prob-set?
  ;; --- (struct-out prob-set)
  prob-set-intersect
  prob-set-union
  prob-set-measure
  prob-set-sample-point
  prob-set-member?
  prob-set-measure
  ;; --- (struct-out prob-interval)
  prob-interval-fields
  prob-interval-can-sample?
  prob-interval
  ;; ---
  prob->singleton
  probs
  probs?
  prob-set-sample-point ;;(-> Nonempty-Prob-Set (U Bad-Prob Prob))
  )
;; -----------------------------------------------------------------------------

(require
  benchmark-util
  racket/match
  (only-in racket/list first rest empty?)
  (only-in math/flonum
    fllog
    flnext*)
  (for-syntax racket/base racket/syntax)
  "flonum-probability-adapted.rkt"
  (only-in "set-types-adapted.rkt"
    Base-Bot-Basic)
  (only-in "untyped-utils.rkt"
    define-singleton-type) ;; TODO keep or trash?
)

(: prob-interval-fields (-> Nonempty-Prob-Interval (Values Prob Prob Boolean Boolean)))
(define (prob-interval-fields I)
  (if (probs? I)
      (values prob-0 prob-1 #t #t)
      (values (Plain-Prob-Interval-min I)
              (Plain-Prob-Interval-max I)
              (Plain-Prob-Interval-min? I)
              (Plain-Prob-Interval-max? I))))

(: prob-next (-> Prob Prob))
(define (prob-next x)
  (Prob (flprob-fast-canonicalize (flnext* (Prob-value x)))))
;;bg hacks
(define-syntax-rule (flprob-fast-canonicalize x-stx)
  (let ([x : Flonum  x-stx])
    (cond [(<= x (fllog 0.5))  x]
          [(< x 0.0)  (+ (fllog 2.0) (- x (fllog 0.5)))]
          [(<= x (fllog 2.0))  (+ (fllog 0.5) (- x (fllog 2.0)))]
          [else  x])))

(: prob-interval-can-sample? (-> Nonempty-Prob-Interval Boolean))
(define (prob-interval-can-sample? I)
  (define-values (a b a? b?) (prob-interval-fields I))
  (not (and (not a?) (not b?) (prob= (prob-next a) b))))

(: prob-interval-sample-point (-> Nonempty-Prob-Interval (U Bad-Prob Prob)))
(define (prob-interval-sample-point I)
  (define-values (a b a? b?) (prob-interval-fields I))
  (let loop ()
    (define x (prob-random a b))
    (cond [(not (or (and (not a?) (prob= x a)) (and (not b?) (prob= x b))))  x]
          [(prob-interval-can-sample? I)  (loop)]
          [else  (Bad-Prob)])))

(: make-prob-interval-measure (-> (-> Prob Prob (U Prob Bad-Prob)) (-> Prob-Interval Prob)))
(define ((make-prob-interval-measure prob-) I)
  (cond [(empty-prob-set? I)  prob-0]
        [(probs? I)   prob-1]
        [else
         (define p (prob- (Plain-Prob-Interval-max I) (Plain-Prob-Interval-min I)))
         (cond [(prob? p)  p]
               [else  (error 'prob-interval-measure "result is not a probability; given ~e" I)])]))

(define prob-interval-measure (make-prob-interval-measure prob-))

(: make-prob-interval-list-measure (-> (-> Nonempty-Prob-Interval Prob)
                                       (-> Prob Prob (U Prob Bad-Prob))
                                       (-> (Pairof Nonempty-Prob-Interval (Pairof Nonempty-Prob-Interval (Listof Nonempty-Prob-Interval))) Prob)))
(define ((make-prob-interval-list-measure prob-interval-measure prob+) Is)
  (for/fold ([q : Prob  prob-0]) ([I  (in-list Is)])
    (let ([q  (prob+ q (prob-interval-measure I))])
      (if (prob? q) q prob-1))))

(define prob-interval-list-measure
  (make-prob-interval-list-measure prob-interval-measure prob+))

(: make-prob-set-measure (-> (-> Plain-Prob-Interval Prob)
                             (-> (Pairof Plain-Prob-Interval (Pairof Plain-Prob-Interval (Listof Plain-Prob-Interval))) Prob)
                             (-> Prob-Set Prob)))
(define ((make-prob-set-measure prob-interval-measure prob-interval-list-measure) I)
  (cond [(empty-prob-set? I)  prob-0]
        [(probs? I)  prob-1]
        [(Plain-Prob-Interval? I)   (prob-interval-measure I)]
        [else  (prob-interval-list-measure (Prob-Interval-List-elements I))]))

(define prob-set-measure
  (make-prob-set-measure prob-interval-measure prob-interval-list-measure))

(: prob-set-sample-point (-> Nonempty-Prob-Set (U Bad-Prob Prob)))
(define (prob-set-sample-point I)
  (cond [(or (probs? I) (Plain-Prob-Interval? I))  (prob-interval-sample-point I)]
        [else
         (define q (prob-set-measure I))
         (define Is
           (cast (filter prob-interval-can-sample? (Prob-Interval-List-elements I))
                 (Pairof Nonempty-Prob-Interval (Pairof Nonempty-Prob-Interval (Listof Nonempty-Prob-Interval)))))
         (cond [(eq? '() Is)  (Bad-Prob)]
               [(eq? '() (cdr Is))  (prob-interval-sample-point (car Is))]
               [else
                (define ps ;;bg inlined 'prob-interval-list-probs'
                  (let ([q (prob-interval-list-measure Is)])
                      (cond [(prob-0? q)  #f]
                            [else
                             (map (λ ([I : Nonempty-Prob-Interval])
                                       (define p (prob/ (prob-interval-measure I) q))
                                       (if (prob? p) p prob-1))
                                     Is)])))
                (cond [ps  (define i (prob-random-index ps))
                           (prob-interval-sample-point (list-ref Is i))]
                      [else  (Bad-Prob)])])]))

(: prob->singleton (-> Prob Plain-Prob-Set))
(define (prob->singleton x)
  (plain-prob-interval x x #t #t))

;; =============================================================================
;; from ordered-set.rkt

(struct Base-Ordered-Set Base-Bot-Basic () #:transparent)

(define-syntax (define-ordered-set stx)
  (syntax-case stx ()
    [(_ #:names Name name
        #:types Value
        #:predicates value? contained-value? -inf-value? +inf-value?
        #:comparisons = <
        #:guards empty? full? guard
        )
     (with-syntax ([Base-Set    (format-id #'Name "Base-~a-Set" #'Name)]
                   [Base-Set?   (format-id #'Name "Base-~a-Set?" #'Name)]
                   [base-set?   (format-id #'name "~a-set?" #'name)]
                   [Full-Set    (format-id #'Name "Full-~a-Set" #'Name)]
                   [full-set    (format-id #'name "~as" #'name)]
                   [full-set?   (format-id #'name "~as?" #'name)]
                   [Empty-Set   (format-id #'Name "Empty-~a-Set" #'Name)]
                   [empty-set   (format-id #'name "empty-~a-set" #'name)]
                   [empty-set?  (format-id #'name "empty-~a-set?" #'name)]
                   ;[plain-interval-hash  (format-id #'Name "plain-~a-interval-hash" #'name)]
                   [plain-interval       (format-id #'Name "plain-~a-interval" #'name)]
                   [Plain-Interval       (format-id #'Name "Plain-~a-Interval" #'Name)]
                   [Plain-Interval?      (format-id #'Name "Plain-~a-Interval?" #'Name)]
                   [Plain-Interval-min   (format-id #'Name "Plain-~a-Interval-min" #'Name)]
                   [Plain-Interval-max   (format-id #'Name "Plain-~a-Interval-max" #'Name)]
                   [Plain-Interval-min?  (format-id #'Name "Plain-~a-Interval-min?" #'Name)]
                   [Plain-Interval-max?  (format-id #'Name "Plain-~a-Interval-max?" #'Name)]
                   [Nonempty-Interval  (format-id #'Name "Nonempty-~a-Interval" #'Name)]
                   [Nonfull-Interval   (format-id #'Name "Nonfull-~a-Interval" #'Name)]
                   [Interval  (format-id #'Name "~a-Interval" #'Name)]
                   [interval  (format-id #'name "~a-interval" #'name)]
                   [interval-join       (format-id #'name "~a-interval-join" #'name)]
                   [interval-intersect  (format-id #'name "~a-interval-intersect" #'name)]
                   [interval-subseteq?  (format-id #'name "~a-interval-subseteq?" #'name)]
                   [interval-member?  (format-id #'name "~a-interval-member?" #'name)]
                   ;[interval-list-hash  (format-id #'Name "~a-interval-list-hash" #'name)]
                   [interval-list       (format-id #'Name "~a-interval-list" #'name)]
                   [Interval-List       (format-id #'Name "~a-Interval-List" #'Name)]
                   [Interval-List?      (format-id #'Name "~a-Interval-List?" #'Name)]
                   [Interval-List-elements  (format-id #'Name "~a-Interval-List-elements" #'Name)]
                   [Plain-Set     (format-id #'Name "Plain-~a-Set" #'Name)]
                   [Nonempty-Set  (format-id #'Name "Nonempty-~a-Set" #'Name)]
                   [Nonfull-Set   (format-id #'Name "Nonfull-~a-Set" #'Name)]
                   [Set           (format-id #'Name "~a-Set" #'Name)]
                   [set-union       (format-id #'name "~a-set-union" #'name)]
                   [set-join        (format-id #'name "~a-set-join" #'name)]
                   [set-intersect   (format-id #'name "~a-set-intersect" #'name)]
                   [set-subseteq?   (format-id #'name "~a-set-subseteq?" #'name)]
                   [set-member?     (format-id #'name "~a-set-member?" #'name)]
                   [set-singleton?  (format-id #'name "~a-set-singleton?" #'name)]
                   [set-map         (format-id #'name "~a-set-map" #'name)]
                   )
       (syntax/loc stx
         (begin
           (struct Base-Set Base-Ordered-Set () #:transparent)

           (define base-set? Base-Set?)

           (define-singleton-type Full-Set Base-Set full-set)
           (define-singleton-type Empty-Set Base-Set empty-set)

           ;; ========================================================================================
           ;; Interval type

           (struct Plain-Interval Base-Set
             ([min : Value] [max : Value] [min? : Boolean] [max? : Boolean])
             #:transparent)

           (define-type Nonempty-Interval (U Plain-Interval Full-Set))
           (define-type Nonfull-Interval (U Plain-Interval Empty-Set))
           (define-type Interval (U Plain-Interval Full-Set Empty-Set))

           #;;(: plain-interval-hash (HashTable Plain-Interval (Weak-Boxof Plain-Interval)))
           (define plain-interval-hash (make-weak-hash))

           (: plain-interval (-> Value Value Boolean Boolean Plain-Interval))
           (define (plain-interval a b a? b?)
             (Plain-Interval a b a? b?))

           (: interval (-> Value Value Boolean Boolean Interval))
           (define (interval a b a? b?)
             (cond [(not (value? a))
                    (raise-argument-error 'interval (symbol->string 'Value) 0 a b a? b?)]
                   [(not (value? b))
                    (raise-argument-error 'interval (symbol->string 'Value) 1 a b a? b?)]
                   [(or (< b a) (and (= a b) (not (and a? b?))))  empty-set]
                   [(and a? b? (-inf-value? a) (+inf-value? b))  full-set]
                   [(empty? a b a? b?)  empty-set]
                   [(full? a b a? b?)  full-set]
                   [else  (let-values ([(a b a? b?)  (guard a b a? b?)])
                            (plain-interval a b a? b?))]))

           (: interval-join (case-> (-> Interval Nonempty-Interval (Values Nonempty-Interval Boolean))
                                    (-> Nonempty-Interval Interval (Values Nonempty-Interval Boolean))
                                    (-> Interval Interval (Values Interval Boolean))))
           (define (interval-join I1 I2)
             (cond [(empty-set? I1)  (values I2 #t)]
                   [(empty-set? I2)  (values I1 #t)]
                   [(eq? I1 I2)  (values I1 #t)]
                   [(full-set? I1)  (values I1 #t)]
                   [(full-set? I2)  (values I2 #t)]
                   [else
                    (match-define (Plain-Interval a1 b1 a1? b1?) I1)
                    (match-define (Plain-Interval a2 b2 a2? b2?) I2)
                    (define-values (a a?)
                      (cond [(< a1 a2)  (values a1 a1?)]
                            [(< a2 a1)  (values a2 a2?)]
                            [else       (values a1 (or a1? a2?))]))
                    (define-values (b b?)
                      (cond [(< b1 b2)  (values b2 b2?)]
                            [(< b2 b1)  (values b1 b1?)]
                            [else       (values b1 (or b1? b2?))]))
                    (cond [(and (= a a1) (= b b1) (eq? a? a1?) (eq? b? b1?))  (values I1 #t)]
                          [(and (= a a2) (= b b2) (eq? a? a2?) (eq? b? b2?))  (values I2 #t)]
                          [else
                           (define I (interval a b a? b?))
                           (cond [(empty-set? I)
                                  (raise-result-error 'interval-join
                                                      (symbol->string 'Nonempty-Interval)
                                                      I)]
                                 [else
                                  (define inexact?
                                    (or (or (< b1 a2) (and (= b1 a2) (and (not b1?) (not a2?))))
                                        (or (< b2 a1) (and (= b2 a1) (and (not b2?) (not a1?))))))
                                  (values I (not inexact?))])])]))

           (: interval-intersect (case-> (-> Interval Nonfull-Interval Nonfull-Interval)
                                         (-> Nonfull-Interval Interval Nonfull-Interval)
                                         (-> Interval Interval Interval)))
           (define (interval-intersect I1 I2)
             (cond [(full-set? I1)  I2]
                   [(full-set? I2)  I1]
                   [(eq? I1 I2)  I1]
                   [(empty-set? I1)  I1]
                   [(empty-set? I2)  I2]
                   [else
                    (match-define (Plain-Interval a1 b1 a1? b1?) I1)
                    (match-define (Plain-Interval a2 b2 a2? b2?) I2)
                    (define-values (a a?)
                      (cond [(< a2 a1)  (values a1 a1?)]
                            [(< a1 a2)  (values a2 a2?)]
                            [else       (values a1 (and a1? a2?))]))
                    (define-values (b b?)
                      (cond [(< b2 b1)  (values b2 b2?)]
                            [(< b1 b2)  (values b1 b1?)]
                            [else       (values b1 (and b1? b2?))]))
                    (cond [(and (eq? a a1) (eq? b b1) (eq? a? a1?) (eq? b? b1?))  I1]
                          [(and (eq? a a2) (eq? b b2) (eq? a? a2?) (eq? b? b2?))  I2]
                          [else
                           (define I (interval a b a? b?))
                           (cond [(full-set? I)
                                  (raise-result-error 'interval-intersect
                                                      (symbol->string 'Nonfull-Interval)
                                                      I)]
                                 [else  I])])]))

           (: interval-subseteq? (-> Interval Interval Boolean))
           (define (interval-subseteq? I1 I2)
             (cond [(empty-set? I1)  #t]
                   [(empty-set? I2)  #f]
                   [(eq? I1 I2)           #t]
                   [(full-set? I2)   #t]
                   [(full-set? I1)   #f]
                   [else
                    (match-define (Plain-Interval a1 b1 a1? b1?) I1)
                    (match-define (Plain-Interval a2 b2 a2? b2?) I2)
                    (and (or (< a2 a1) (and (= a1 a2) (or (not a1?) a2?)))
                         (or (< b1 b2) (and (= b1 b2) (or (not b1?) b2?))))]))

           (: interval-member? (-> Interval Value Boolean))
           (define (interval-member? I x)
             (cond [(not (contained-value? x))  #f]
                   [(empty-set? I)  #f]
                   [(full-set? I)   #t]
                   [else
                    (match-define (Plain-Interval a b a? b?) I)
                    (cond [(and (< a x) (< x b))  #t]
                          [(< x a)  #f]
                          [(< b x)  #f]
                          [(and (= x a) a?)  #t]
                          [(and (= x b) b?)  #t]
                          [else  #f])]))

           ;; ========================================================================================
           ;; Sorted, disjoint interval union type

           (struct: Interval-List Base-Set
             ([elements : (Pairof Plain-Interval (Pairof Plain-Interval (Listof Plain-Interval)))])
             #:transparent)

           #;;(: interval-list-hash (HashTable Interval-List (Weak-Boxof Interval-List)))
           (define interval-list-hash (make-weak-hash))

           (: interval-list (-> (Pairof Plain-Interval (Pairof Plain-Interval (Listof Plain-Interval))) Interval-List))
           (define (interval-list Is)
             (Interval-List Is))

           (define-type Plain-Set (U Plain-Interval Interval-List))
           (define-type Nonfull-Set (U Plain-Set Empty-Set))
           (define-type Nonempty-Set (U Plain-Set Full-Set))
           (define-type Set (U Plain-Set Full-Set Empty-Set))

           (: min<? (-> Value Boolean Value Boolean Boolean))
           (define (min<? a1 a1? a2 a2?)
             (or (< a1 a2) (and (= a1 a2) a1? (not a2?))))

           (: max<? (-> Value Boolean Value Boolean Boolean))
           (define (max<? b1 b1? b2 b2?)
             (or (< b1 b2) (and (= b1 b2) (not b1?) b2?)))

           ;;bg;;(: set->list (-> Plain-Set (Listof+1 Plain-Interval)))
           (: set->list (-> Plain-Set (Pairof Plain-Interval (Listof Plain-Interval))))
           (define (set->list I)
             (if (Plain-Interval? I) (list I) (Interval-List-elements I)))

           (: list->set (case-> (-> (Pairof Plain-Interval (Listof Plain-Interval)) Plain-Set) ;;bg
                                (-> (Listof Plain-Interval) Nonfull-Set)))
           (define (list->set Is)
             (cond [(null? Is)  empty-set]
                   [(null? (cdr Is))  (car Is)]
                   [else  (interval-list Is)]))

           ;; ========================================================================================
           ;; Union

           (: interval-list-union (-> (Listof Plain-Interval)
                                      (Listof Plain-Interval)
                                      (U Full-Set (Listof Plain-Interval))))
           (define (interval-list-union I1 I2)
             (cond
               [(null? I1)  I2]
               [(null? I2)  I1]
               [else
                (match-define (Plain-Interval a1 b1 a1? b1?) (car I1))
                (match-define (Plain-Interval a2 b2 a2? b2?) (car I2))
                (cond
                  [(or (< b1 a2) (and (= b1 a2) (not b1?) (not a2?)))
                   ;; ------
                   ;;        ------
                   (let ([I  (interval-list-union (cdr I1) I2)])
                     (if (full-set? I) I (cons (car I1) I)))]
                  [(or (< b2 a1) (and (= b2 a1) (not b2?) (not a1?)))
                   ;;        ------
                   ;; ------
                   (let ([I  (interval-list-union I1 (cdr I2))])
                     (if (full-set? I) I (cons (car I2) I)))]
                  [(min<? a1 a1? a2 a2?)
                   (cond [(max<? b2 b2? b1 b1?)
                          ;; ------
                          ;;   --
                          (interval-list-union I1 (cdr I2))]
                         [else
                          ;; ------           ------
                          ;;    ------   or      ---
                          (define I (interval a1 b2 a1? b2?))
                          (cond [(full-set? I)  I]
                                [else  (let ([I  (assert I Plain-Interval?)])
                                         (interval-list-union (cdr I1) (cons I (cdr I2))))])])]
                  [else
                   (cond [(max<? b2 b2? b1 b1?)
                          ;;    ------        ------
                          ;; ------      or   ---
                          (define I (interval a2 b1 a2? b1?))
                          (cond [(full-set? I)  I]
                                [else  (let ([I  (assert I Plain-Interval?)])
                                         (interval-list-union (cons I (cdr I1)) (cdr I2)))])]
                         [else
                          ;;   --             ---        ---           ------
                          ;; ------   or   ------   or   ------   or   ------
                          (interval-list-union (cdr I1) I2)])])]))


           (: set-union (case-> (-> Set Nonempty-Set Nonempty-Set)
                                (-> Nonempty-Set Set Nonempty-Set)
                                (-> Set Set Set)))
           (define (set-union I1 I2)
             (cond [(empty-set? I1)  I2]
                   [(empty-set? I2)  I1]
                   [(eq? I1 I2)   I1]
                   [(full-set? I1)   I1]
                   [(full-set? I2)   I2]
                   [else
                    (define Is (interval-list-union (set->list I1) (set->list I2)))
                    (cond [(full-set? Is)  Is]
                          [else  (list->set (assert Is pair?))])]))

           (: set-join (case-> (-> Set Nonempty-Set (Values Nonempty-Set #t))
                               (-> Nonempty-Set Set (Values Nonempty-Set #t))
                               (-> Set Set (Values Set #t))))
           (define (set-join I1 I2)
             (values (set-union I1 I2) #t))

           ;; ========================================================================================
           ;; Intersection

           (: interval-list-intersect (-> (Listof Plain-Interval)
                                          (Listof Plain-Interval)
                                          (Listof Plain-Interval)))
           (define (interval-list-intersect I1 I2)
             (cond
               [(null? I1)  I1]
               [(null? I2)  I2]
               [else
                (match-define (Plain-Interval a1 b1 a1? b1?) (car I1))
                (match-define (Plain-Interval a2 b2 a2? b2?) (car I2))
                (cond
                  [(or (< b1 a2) (and (= b1 a2) (or (not b1?) (not a2?))))
                   ;; ------
                   ;;       ------
                   (interval-list-intersect (cdr I1) I2)]
                  [(or (< b2 a1) (and (= b2 a1) (or (not b2?) (not a1?))))
                   ;;       ------
                   ;; ------
                   (interval-list-intersect I1 (cdr I2))]
                  [(min<? a1 a1? a2 a2?)
                   (cond [(max<? b2 b2? b1 b1?)
                          ;; ------
                          ;;   --
                          (cons (car I2) (interval-list-intersect I1 (cdr I2)))]
                         [else
                          ;; ------           ------
                          ;;    ------   or      ---
                          (define I (interval a2 b1 a2? b1?))
                          (cond [(empty-set? I)  (interval-list-intersect (cdr I1) I2)]
                                [else  (let ([I  (assert I Plain-Interval?)])
                                         (cons I (interval-list-intersect (cdr I1) I2)))])])]
                  [else
                   (cond [(max<? b2 b2? b1 b1?)
                          ;;    ------        ------
                          ;; ------      or   ---
                          (define I (interval a1 b2 a1? b2?))
                          (cond [(empty-set? I)  (interval-list-intersect I1 (cdr I2))]
                                [else  (let ([I  (assert I Plain-Interval?)])
                                         (cons I (interval-list-intersect I1 (cdr I2))))])]
                         [else
                          ;;   --             ---        ---           ------
                          ;; ------   or   ------   or   ------   or   ------
                          (cons (car I1) (interval-list-intersect (cdr I1) I2))])])]))

           (: set-intersect (case-> (-> Set Nonfull-Set Nonfull-Set)
                                    (-> Nonfull-Set Set Nonfull-Set)
                                    (-> Set Set Set)))
           (define (set-intersect I1 I2)
             (cond [(full-set? I1)  I2]
                   [(full-set? I2)  I1]
                   [(eq? I1 I2)  I1]
                   [(empty-set? I1)  I1]
                   [(empty-set? I2)  I2]
                   [(and (Plain-Interval? I1) (Plain-Interval? I2))
                    (interval-intersect I1 I2)]
                   [else
                    (list->set (interval-list-intersect (set->list I1) (set->list I2)))]))

           ;; ========================================================================================
           ;; Subseteq

           (: interval-list-subseteq? (-> (Listof Plain-Interval)
                                          (Listof Plain-Interval)
                                          Boolean))
           (define (interval-list-subseteq? I1 I2)
             (cond [(null? I1)  #t]
                   [(null? I2)  #f]
                   [else
                    (match-define (Plain-Interval a1 b1 a1? b1?) (car I1))
                    (match-define (Plain-Interval a2 b2 a2? b2?) (car I2))
                    (cond
                      [(or (< b1 a2) (and (= b1 a2) (or (not b1?) (not a2?))))
                       ;; ------
                       ;;       ------
                       #f]
                      [(or (< b2 a1) (and (= b2 a1) (or (not b2?) (not a1?))))
                       ;;       ------
                       ;; ------
                       (interval-list-subseteq? I1 (cdr I2))]
                      [(min<? a1 a1? a2 a2?)
                       ;; ------        ------           ------
                       ;;   --     or      ------   or      ---
                       #f]
                      [(max<? b2 b2? b1 b1?)
                       ;;    ------        ------
                       ;; ------      or   ---
                       #f]
                      [else
                       ;;   --             ---        ---           ------
                       ;; ------   or   ------   or   ------   or   ------
                       (interval-list-subseteq? (cdr I1) I2)])]))

           (: set-subseteq? (-> Set Set Boolean))
           (define (set-subseteq? I1 I2)
             (cond [(empty-set? I1)  #t]
                   [(empty-set? I2)  #f]
                   [(eq? I1 I2)  #t]
                   [(full-set? I2)  #t]
                   [(full-set? I1)  #f]
                   [(and (Plain-Interval? I1) (Plain-Interval? I2))
                    (interval-subseteq? I1 I2)]
                   [else
                    (interval-list-subseteq? (set->list I1) (set->list I2))]))

           ;; ========================================================================================
           ;; Other functions

           (: set-member? (-> Set Value Boolean))
           (define (set-member? I x)
             (cond [(not (contained-value? x))  #f]
                   [(empty-set? I)  #f]
                   [(full-set? I)   #t]
                   [(Plain-Interval? I)   (interval-member? I x)]
                   [else  (ormap (λ: ([I : Plain-Interval]) (interval-member? I x))
                                 (Interval-List-elements I))]))

           (: set-singleton? (-> Set Boolean))
           (define (set-singleton? I)
             (and (Plain-Interval? I)
                  (= (Plain-Interval-min I)
                     (Plain-Interval-max I))))
           (: set-map (-> (-> Nonempty-Interval (Values Set Boolean)) Set (Values Set Boolean)))
           (define (set-map f I)
             (cond [(empty-set? I)  (values empty-set #t)]
                   [(or (full-set? I) (Plain-Interval? I))  (f I)]
                   [else
                    (for/fold ([B : Set  empty-set]
                               [exact? : Boolean  #t])
                              ([I  (in-list (Interval-List-elements I))])
                      (let*-values ([(C e1?)  (f I)]
                                    [(B e2?)  (set-join B C)])
                        (values B (and exact? e1? e2?))))]))
           )))]))

;; -----------------------------------------------------------------------------

(define-ordered-set
  #:names Prob prob
  #:types Prob
  #:predicates
  (λ (x) #t)
  (λ (x) #t)
  prob-0?
  prob-1?
  #:comparisons prob= prob<
  #:guards
  (λ (a b a? b?) #f)
  (λ (a b a? b?) #f)
  (λ (a b a? b?) (values a b a? b?))
  )
