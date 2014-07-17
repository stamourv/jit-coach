#lang racket

(require unstable/list json)

(require "structs.rkt" "parsing.rkt" "by-object-type.rkt")

(module+ main
  (define log-file (vector-ref (current-command-line-arguments) 0))
  (define profile (with-input-from-file log-file read-json))
  (define compiles (profile->compiles profile))

  ;; (define sorted-compiles (sort compiles > #:key compile-self-time))
  ;; (define top-compiles (take sorted-compiles 5)) ; arbitrary
  ;; ;; (for ([e top-compiles])
  ;; ;;   (printf "~a - ~a self - ~a total - ~a events\n\n\n"
  ;; ;;           (compile-location-string e)
  ;; ;;           (compile-self-time e)
  ;; ;;           (compile-total-time e)
  ;; ;;           (length (compile-events e)))) ;; TODO

  ;; using pruning at the report level, instead of at the compile level
  (define all-events (append-map compile-events compiles))
  (report-by-object-type all-events)

  )
