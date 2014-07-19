#lang racket

(require "recommendations.rkt" "structs.rkt" "utils.rkt")

(provide (all-defined-out))

;; Data structures to represent near miss reports

;; General structure representing a coaching report.
;; Includes one of the failures that originated the report (this is post
;; merging, so this report may summarize multiple failures). This failure
;; is considered representative of those that got merged, and should be
;; identical to the others, except along dimensions that were merged (and
;; thus should not be presented in the report, so we're good).
(struct report
  (object-typeset ; (listof <constructor>) ; all the object types affected
   failure        ; failure?
   badness)       ; number?
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc r port _)
     (parameterize ([current-output-port port])
       (print-report r)))])

;; Report structure for near misses that we think would be solved by changing
;; a/some constructor(s).
;; Does not keep track of failure locations, because (we think) the solution
;; is not likely to be found there.
(struct constructor-report report
  (properties+badnesses) ; (listof (list string? number?))
  ;; We keep track of badness for each property individually to help
  ;; programmers prioritize which properties to fix.
  ;; When reporting near misses at constructors, we report all the common
  ;; fields of a group of types together. Because they're in the same group,
  ;; these fields are (or should) be defined together, and so issues about
  ;; them would also be solved together.
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc r port _)
     (parameterize ([current-output-port port])
       (print-report
        r
        (lambda ()
          (printf "affected properties:\n")
          (for ([p (sort (constructor-report-properties+badnesses r)
                         > #:key second)])
            (printf "  ~a (badness: ~a)\n" (first p) (second p)))))))])

;; Report structure for near misses that we think should be solved directly
;; at the failure site (or at least, for which the failure site is informative
;; to find a solution).
(struct in-situ-report report
  ;; can get affected property from failure, so not stored directly
  (locations+badnesses)
  ;; We keep track of badness for each location (this is post temporal and
  ;; locality merging).
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc r port _)
     (parameterize ([current-output-port port])
       (print-report
        r
        (lambda ()
          (printf "affected property: ~a\n\n"
                  (attempt-property (report-failure r)))
          (printf "locations:\n")
          (for ([l (sort (in-situ-report-locations+badnesses r)
                         > #:key second)])
            (printf "  ~a (badness: ~a)\n" (first l) (second l)))))))])


;; Paper: the division above is worth discussing. Some failures are non-local
;; (fail at use site, fixed at constructor), but not all of them. Heuristics
;; to distinguish them, and different info when reporting them.


(define (print-report r [print-substruct-info void])
  (match-define (report typeset failure badness) r)
  (printf "badness: ~a\n\nfor object types: ~a\n\n"
          badness typeset)
  (printf "chosen strategy: ~a\nfailed strategy: ~a\nreason: ~a\n\n"
          (event-strategy (attempt-event failure))
          (attempt-strategy failure)
          (failure-reason failure))
  (print-substruct-info)
  (printf "\n~a" (explain-failure failure))
  (print-separator))
