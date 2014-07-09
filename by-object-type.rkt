#lang racket

(require unstable/list)

(require "structs.rkt" "recommendations.rkt" "utils.rkt")

;;;; Produces reports related to specific object types.
;;;; These provide a more concise summary of failures than by-location
;;;; reports, and groups issues that are likely to have a common solution
;;;; (i.e. if you fix a type, that may resolve multiple related issues).

;;;; Unfortunately, those kinds of reports really only make sense for
;;;; monomorphic operations, where the layout/etc. of one specific object
;;;; type causes failures, as opposed to the presence of 0 or multiple object
;;;; types. The latter will keep failing regardless of whether we change the
;;;; individual types involved.

;;;; Also, this will, of course, only consider operations (or failures)
;;;; that we *observed* were related to that type. We won't see places
;;;; where failure to see that type was the problem (e.g. non-executed
;;;; code, that would have seen that type, but didn't so didn't know how
;;;; to optimize).

;; research: this is a new kind of merging, worth discussing in paper/thesis
;;   that's a kind of analysis that an OO coach benefits from that a functional
;;   coach doesn't as much (I think)

(provide report-by-object-type)


;; group-by-object-type : (listof optimization-event?)
;;                          -> (listof (listof optimization-event?)
(define (group-by-object-type opt-events)
  (group-by event-object-type (filter single-object-type? opt-events)))

;; report-by-object-type : (listof optimization-event?) -> void?
;; takes a list of ungrouped events, and prints a by-object-type view
;; of optimization failures
(define (report-by-object-type all-events)
  (define by-object-type (group-by-object-type all-events))
  (for ([group by-object-type])
    (define common-type (event-object-typeset (first group)))

    ;; secondary grouping by failure type (currently counts both attempted
    ;; strategy and cause of failure)
    (define all-failures (append-map event-failures group))
    (unless (empty? all-failures) ; only successes for that object type

      (printf "failures for object type: ~a\n\n" common-type)
      ;; TODO would really be nice to be able to print constructor location, or sth

      (define by-failure-type
        (group-by (lambda (f) (cons (attempt-strategy f)
                                    (failure-reason f)))
                  all-failures))
      (for ([group by-failure-type])
        (printf "strategy: ~a\nreason: ~a\n\n~aat:\n"
                (attempt-strategy (first group))
                (failure-reason (first group))
                (explain-failure (first group)))
        (define by-location
          (group-by (lambda (f) (optimization-event-location (attempt-event f)))
                    group))
        ;; TODO also check whether that failure happens across all compiles
        ;;   -> for that, events would need to know their siblings by location
        (for ([loc (sort by-location > #:key length)])
          (printf "~a x ~a\n"
                  (length loc)
                  (optimization-event-location (attempt-event (first loc)))))
        (newline))

      (print-separator))))
