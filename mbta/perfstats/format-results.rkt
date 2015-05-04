#lang racket/base

;; Script to read files containing string representations of vectors
;;  and format the 4th element.
;;
;; Input is a directory.
;; We scrape each .log file in the directory as having a vector on each line.
;; We read the 4th entry of each vector and print their average
;;  along with the filename and names of the typed modules.

(require (only-in racket/string string-split string-join)
         (only-in racket/list last)
         (only-in racket/format ~r)
         (only-in math/statistics mean)
         (only-in racket/port call-with-input-string)
         (only-in racket/file file->lines))

(define (index->name i)
  (case i
    [(0) "main.rkt"]
    [(1) "run-t.rkt"]
    [(2) "t-graph.rkt"]
    [(3) "t-view.rkt"]))

(define (typed-mbta-modules config)
  (define names
    (for/list ([i (in-range (string-length config))]
               #:when (eq? #\1 (string-ref config i))) ;;UGH, eq? not =
      (index->name i)))
  (string-join names ","))

(define (parse-file file)
  (define config  (last (string-split (car (string-split file ".")) "/")))
  (define ctx-switches
    (for/list ([ln (in-list (file->lines file))])
      ;;UGH so much work to read a damn string
      (vector-ref (eval (call-with-input-string ln read) (make-base-namespace)) 4)))
  (list config (~r (mean ctx-switches) #:precision 2) (typed-mbta-modules config)))

(define (print-results titles row*)
  (displayln (string-join titles "\t"))
  (for ([r row*]) (displayln (string-join r "\t"))))

(define (parse-context-switches file*)
  (define titles '("Config" "Avg. Context Switches" "Typed Modules"))
  (define rows    (for/list ([file (in-list file*)]) (parse-file file)))
  (print-results titles rows))

(module+ main
  (require racket/cmdline glob)
  (command-line #:args (dirname)
                (parse-context-switches (glob (format "~a/*.log" dirname)))))
