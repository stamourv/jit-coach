#lang racket

(require unstable/list)

(require "structs.rkt" "parsing.rkt"
         "by-location.rkt" "by-object-type.rkt")

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
