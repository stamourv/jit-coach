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
                        richards1 richards2 richards3
                        deltablue1 deltablue2 deltablue3 deltablue4
                        raytrace1 raytrace2 raytrace3 raytrace4 raytrace5)
      (regexp-match
       (string-append "^"
                      "Richards: ([0-9]+)\n"
                      "Richards[^:]+: ([0-9]+)\n"
                      "Richards[^:]+: ([0-9]+)\n"
                      "DeltaBlue: ([0-9]+)\n"
                      "DeltaBlue[^:]+: ([0-9]+)\n"
                      "DeltaBlue[^:]+: ([0-9]+)\n"
                      "DeltaBlue[^:]+: ([0-9]+)\n"
                      "RayTrace: ([0-9]+)\n"
                      "RayTrace[^:]+: ([0-9]+)\n"
                      "RayTrace[^:]+: ([0-9]+)\n"
                      "RayTrace[^:]+: ([0-9]+)\n"
                      "RayTrace[^:]+: ([0-9]+)\n")
       s))
    ;; TODO oops, can't use full version name for each bench, because versions
    ;;   are different for each bench. just use number for now, and provide a
    ;;   mapping somewhere
    (dict-update! sums '("Richards" 1) (add richards1) '())
    (dict-update! sums '("Richards" 2) (add richards2) '())
    (dict-update! sums '("Richards" 3) (add richards3) '())
    (dict-update! sums '("DeltaBlue" 1) (add deltablue1) '())
    (dict-update! sums '("DeltaBlue" 2) (add deltablue2) '())
    (dict-update! sums '("DeltaBlue" 3) (add deltablue3) '())
    (dict-update! sums '("DeltaBlue" 4) (add deltablue4) '())
    (dict-update! sums '("RayTrace" 1) (add raytrace1) '())
    (dict-update! sums '("RayTrace" 2) (add raytrace2) '())
    (dict-update! sums '("RayTrace" 3) (add raytrace3) '())
    (dict-update! sums '("RayTrace" 4) (add raytrace4) '())
    (dict-update! sums '("RayTrace" 5) (add raytrace5) '())
    )

  ;; not all benchmarks have the same # of versions
  ;; pad with 0s for those who have fewer than the max, and sort in a
  ;; sensible order to avoid screwing up plot grouping
  (define max-n-versions (apply max (map second (dict-keys sums))))
  (define benchs (remove-duplicates (map first (dict-keys sums))))
  (for*/list ([b benchs]
                [i max-n-versions])
      (benchmark-result b
                        (list (add1 i))
                        (dict-ref! sums (list b (add1 i)) '(0)))))

(module+ main

  (define n 10)

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

  (plot-file (list renderer (y-tick-lines)) "plot.pdf"
             #:x-label #f #:y-label "Normalized score (higher is better)")

  )