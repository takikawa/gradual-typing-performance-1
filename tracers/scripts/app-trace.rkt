#lang racket/base

;; Proposal for the benchmark-util library
;; - Add a (provide/trace id ...) form to
;;    register `id` into a set.
;; - Override #%app to expand into a print&apply.
;;    Print which identifiers from the global set are being accessed.

(provide
 provide/trace
 (rename-out [APP #%app])
)

;; -----------------------------------------------------------------------------

(require
 (for-syntax racket/base syntax/parse racket/string)
 )

;; =============================================================================

(define-syntax (provide/trace stx)
  (syntax-parse stx
    [(_ x*:id ...)
     (register-all (syntax->list #'(x* ...)))
     #'(provide x* ...)]
    [_
     (error 'provide/trace "Bad syntax, can only provide/trace identifiers")]))

(define-syntax (APP stx)
  (syntax-parse stx
    [(_ e* ...)
     ;; At compile-time, collect the names of traced identifiers
     (define pos+id+src* (for/list ([e (in-list (syntax->list #'(e* ...)))]
                                [i (in-naturals)]
                                #:when (and (identifier? e) (lookup? e)))
                       (list i (syntax->datum e) (binding-module (id->list e)))))
     (define msg (and (not (null? pos+id+src*)) (format "[TRACE:APPLY]\t~a\t~a" (syntax-source stx) pos+id+src*)))
     ;; At runtime, print a message before doing the application
     #`(begin (when #,msg (#%app displayln #,msg)) (#%app e* ...))]
    [_
     (error 'provide/trace (format "Bad syntax, no idea what might've happened here. Original was '~a'" stx))]))

;; -----------------------------------------------------------------------------
(define-for-syntax GLOBAL_LOGFILE "/home/ben/Downloads/trace.log")

;; Search the global provide/trace identifier table for `id`.
;; Use the symbol & syntax properties of id to do the lookup.
(define-for-syntax (lookup? id)
  (with-input-from-file GLOBAL_LOGFILE
    (lambda ()
      (define id-list (id->list id))
      (for/or ([ln (in-lines)])
        (idlog=? id-list (line->list ln))))))

(define-for-syntax (line->list ln)
  (string-split ln "\t"))

;; Register a list of variables
;; Save anything needed to later identify each var, along with a message.
(define-for-syntax (register-all id*)
  (with-output-to-file GLOBAL_LOGFILE #:exists 'append
    (lambda ()
      (for ([id (in-list id*)])
        (displayln (string-join (id->list id) "\t"))))))

(define-for-syntax (id->list id)
  (list (format "~a" (syntax->datum id))
        (format "~a" (syntax-source id))
        (format "~a" (identifier-binding id))))

;; Compare two list versions identifiers
(define-for-syntax (idlog=? a b)
  (and (string=? (car a) (car b))
       (string=? (binding-module a)
                 (binding-module b))))

;; (: binding-module (-> (List String String String) String))
(define-for-syntax (binding-module x)
  (or (mpi->mod (caddr x))
      (rsplit #\/ (cadr x))))

(define-for-syntax mpi-pat (regexp "^\\(#<module-path-index:\\(\"(.*?)\"\\)>"))
(define-for-syntax (mpi->mod x)
  (let ([m (regexp-match mpi-pat x)])
    (and m
         (let ([s (cadr m)])
           (and (not (string=? "" s))
                s)))))

(define-for-syntax (last x*)
  (if (null? (cdr x*))
      (car x*)
      (last (cdr x*))))

(define-for-syntax (rindex c str)
  (for/fold ([acc #f])
      ([c2 (in-string str)]
       [i (in-naturals)])
    (if (char=? c c2)
        i
        acc)))

(define-for-syntax (rsplit c str)
  (substring str (add1 (or (rindex c str) -1))))
