#lang racket

;;;; plan: run run.js n times
;;;;       each time, extract the time for each bench
;;;;       then, craft a benchmark-result struct and do plotting

(require benchmark benchmark/types
         plot
         racket/runtime-path
         unstable/sequence)

(define-runtime-path here ".")

(define (run-once)
  (with-output-to-string
    (lambda () (system (format "js ~a/run.js" (path->string here))))))

;; takes results from all the runs
;; returns a list of benchmark-result structs
(define (parse-results ss)
  (define sums (make-hash)) ; maps (bench version) to sum of time
  (define ((add inc) stored) (cons (string->number inc) stored))

  (for ([s ss])
    (match-define (list _
                        richards1 richards2 richards3)
      (regexp-match
       (string-append "^"
                      "Richards: ([0-9]+)\n"
                      "Richards[^:]+: ([0-9]+)\n"
                      "Richards[^:]+: ([0-9]+)\n")
       s))
    ;; TODO oops, can't use full version name for each bench, because versions
    ;;   are different for each bench. just use number for now, and provide a
    ;;   mapping somewhere
    (dict-update! sums '("Richards" 1) (add richards1) '())
    (dict-update! sums '("Richards" 2) (add richards2) '())
    (dict-update! sums '("Richards" 3) (add richards3) '())
    )

  ;; make order more predictable than hashing order
  (define sorted
    (sort (dict-map sums cons)
          < #:key cadar)) ; e.g. 1 in Richards 1

  (for/list ([(bench+version times) (in-pairs sorted)])
    (benchmark-result (first bench+version)
                      (rest bench+version)
                      times)))

(module+ main

  (define n 3)

  (define out (for/list ([i n]) (printf "running set ~a\n" i) (run-once)))
  (define results (parse-results out))

  [plot-x-ticks no-ticks]
  [plot-y-ticks (linear-ticks #:divisors '(5 2))]
  [plot-y-far-ticks no-ticks]
  [benchmark-show-legend? #f]
  ;; [current-benchmark-color-scheme black-white-color-scheme-short]
  [error-bar-color "black"]
  [error-bar-alpha 1]
  [error-bar-line-width 1]
  [error-bar-width 10]
  [rectangle-line-color "black"]
  [plot-font-family 'swiss]

  (define renderer
    (render-benchmark-alts
     '(1) ; normalize to first version
     results))

  (plot-file renderer "plot.pdf"
             #:x-label #f #:y-label "Score (higher is better)")

  )