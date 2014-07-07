#lang racket

(require unstable/list json)

(require "structs.rkt" "parsing.rkt"
         "by-location.rkt" "by-object-type.rkt")

(module+ main
  (define log-file (vector-ref (current-command-line-arguments) 0))
  (define profile (with-input-from-file log-file read-json))
  (define compiles (profile->compiles profile))

  (define sorted-compiles (sort compiles > #:key compile-self-time))
  (define top-compiles (take sorted-compiles 5)) ; arbitrary
  ;; (for ([e top-compiles])
  ;;   (printf "~a - ~a self - ~a total - ~a events\n\n\n"
  ;;           (compile-location-string e)
  ;;           (compile-self-time e)
  ;;           (compile-total-time e)
  ;;           (length (compile-events e)))) ;; TODO

  ;; TODO crude adapter to work with old analyses. doesn't take advantage
  ;;   of grouping in compiles (beyond pruning, that is)
  ;; TODO makes no sense to use that with analyses that look for temporal
  ;;   patterns, though, since we destroyed ordering, and now have holes
  ;;   in the timeline
  (define hot-events (append-map compile-events top-compiles))
  (report-consistently-bad hot-events)
  (report-by-object-type hot-events)

  )
