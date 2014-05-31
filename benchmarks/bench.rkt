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

  ;; TODO oops, can't use full version name for each bench, because versions
  ;;   are different for each bench. just use number for now, and provide a
  ;;   mapping somewhere
  (for ([s ss])
    (for ([bench '("Richards" "DeltaBlue" "RayTrace")]
          #:when #t ; nest loop
          [line (regexp-match* (string-append bench "[^:]*: [0-9]+\n") s)]
          [i    (in-naturals)])
      (match-define (list _ time) (regexp-match ": ([0-9]+)\n$" line))
      (dict-update! sums (list bench i) (add time) '())))

  ;; not all benchmarks have the same # of versions
  ;; pad with 0s for those who have fewer than the max, and sort in a
  ;; sensible order to avoid screwing up plot grouping
  (define max-n-versions (apply max (map second (dict-keys sums))))
  (define benchs (remove-duplicates (map first (dict-keys sums))))
  (for*/list ([b benchs]
              [i max-n-versions])
    (benchmark-result b
                      (list i)
                      (dict-ref! sums (list b i) '(0)))))

(module+ main

  (define n 10)

  (define out (for/list ([i n]) (printf "running set ~a\n" i) (run-once)))
  (define results (parse-results out))

  [plot-x-ticks no-ticks]
  [plot-y-ticks (linear-ticks #:divisors '(5 2))]
  [plot-y-far-ticks no-ticks]
  [benchmark-show-legend? #f]
  [error-bar-color "black"]
  [error-bar-alpha 1]
  [error-bar-line-width 1]
  [error-bar-width 10]
  [rectangle-line-color "black"]
  [plot-font-family 'swiss]

  (define renderer
    (render-benchmark-alts
     '(0) ; normalize to first version
     results))

  (plot-file (list renderer (y-tick-lines)) "plot.pdf"
             #:x-label #f #:y-label "Normalized score (higher is better)")

  )