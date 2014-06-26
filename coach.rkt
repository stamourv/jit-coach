#lang racket

(require unstable/list)

(require "structs.rkt" "parsing.rkt"
         "by-location.rkt" "by-object-type.rkt")

(module+ main
  (define log-file (vector-ref (current-command-line-arguments) 0))
  (define log (file->lines log-file))
  (define parsed-events (log->events log))

  (report-flip-flops parsed-events)
  (report-regressions parsed-events)
  (report-consistently-bad parsed-events)
  (report-by-object-type parsed-events)

  ;; (define bailouts (log->bailouts log)) ;; TODO disabled for now, parsing is incomplete
  ;; (for-each displayln bailouts) ;; TODO better reporting

  )
