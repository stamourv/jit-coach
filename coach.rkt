#lang racket

(require unstable/list)

(require "structs.rkt" "parsing.rkt" "recommendations.rkt" "utils.rkt"
         "by-location.rkt")


;;;; reporting by object type

;; Grouping reports by object types should allow a lot of merging
;; (a lot of reports are related to the same object type).
;; But, this only really makes sense for monomorphic operations
;; (operations that fail because of polymorphism will still fail
;; regardless of the changes we make to one of the types).
;; Also, this will, of course, only consider operations (or failures)
;; that we *observed* were related to that type. We won't see places
;; where failure to see that type was the problem (e.g. non-executed
;; code, that would have seen that type, but didn't so didn't know how
;; to optimize).

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
    (define common-type (event-object-type (first group)))

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


(module+ main
  (define log-file (vector-ref (current-command-line-arguments) 0))
  (define log (file->lines log-file))
  (define parsed-events (log->events log))

  (report-flip-flops parsed-events) ; one found in richards, 3 in deltablue

  ;; (for-each displayln (optimization-event-attempts (first parsed-events)))
  (report-regressions parsed-events) ; one found in paper-example-poly3

  (report-consistently-bad parsed-events)

  (report-by-object-type parsed-events)

  )
