#lang racket

(require unstable/list json)

(require "structs.rkt" "parsing.rkt" "reports.rkt" "property-reports.rkt")


;; generate-reports : (listof optimization-event?) -> (listof report?)
;; Takes a list of ungrouped events, and produces a list of near misses to be
;; shown to the user, sorted by badness.
(define (generate-reports all-events)
  ;; Only consider optimization events in code that was sampled.
  (define live-events
    (filter (lambda (e) (> (optimization-event-profile-weight e) 0))
            all-events))
  (define all-reports ;; TODO do element reports too
    (property-events->reports (filter property-event? live-events)))
  (sort all-reports > #:key report-badness))


(module+ main
  (define log-file (vector-ref (current-command-line-arguments) 0))
  (define profile (with-input-from-file log-file read-json))
  (define compiles (profile->compiles profile))
  (define all-events (append-map compile-events compiles))
  (define sorted-reports (generate-reports all-events))

  ;; do pruning based on badness (profile weight + merging)
  ;; keep only top N
  ;; TODO could prune differently. e.g. take up to X% of the total badness
  ;;   or take reports until we reach a cutoff point (e.g. next is less than
  ;;   10% of the badness of the previous one)
  (define hot-reports (take sorted-reports (min 5 (length sorted-reports))))

  (for-each display hot-reports)
  )
