#lang racket

(require benchmark benchmark/types
         plot
         racket/runtime-path
         unstable/sequence)

(define-runtime-path here ".")

(define (run-one filename+bench v*)
  (match-define `(,filename ,bench) filename+bench)
  (define output
    (with-output-to-string
      (lambda ()
        (parameterize ([current-directory here])
          ;; first version has no suffix
          (define v    (if (= v* 1) "" (number->string v*)))
          (define file (format "run-~a~a.js" filename v))
          (when (file-exists? file) ; different number of versions for each
            (system (format "js ~a/~a" (path->string here) file)))))))
  (define matched
    (regexp-match (string-append bench "[^:]*: [0-9]+\n") output))
  (displayln ; benchmarking lib reads from stdout
   (cond [matched
          (define bench-time (first matched))
          (match-define (list _ time) (regexp-match ": ([0-9]+)\n$" bench-time))
          (string->number time)]
         [else ; version didn't exist
          0])))

(define (run)
  (run-benchmarks
   ;; filenames and benchmark names are slightly different. need both
   '(("richards"      "Richards")
     ("deltablue"     "DeltaBlue")
     ("raytrace"      "RayTrace")
     ("splay"         "Splay")
     ("navier-stokes" "NavierStokes")
     ("pdfjs"         "PdfJS")
     ("crypto"        "Crypto")
     ("box2d"         "Box2D"))
   (list (range 1 11)) ; 10 is current max
   run-one
   #:num-trials 10
   ;; run-one does the actual parsing
   #:extract-time (compose string->number string-trim)
   #:make-name second))


(module+ main

  (define results (run))

  (record-results results (build-path here "results"))

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
     '(1) ; normalize to first version
     results))

  (plot-file (list renderer (y-tick-lines)) (build-path here "plot.pdf")
             #:x-label #f #:y-label "Normalized score (higher is better)")

  )
