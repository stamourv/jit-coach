#lang racket

(require unstable/list json)

(require "structs.rkt" "parsing.rkt" "by-object-type.rkt")

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
