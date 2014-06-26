#lang racket

;;;; log parsing

(require "structs.rkt")

(provide log->events log->bailouts)


;; will get replaced when we have a proper API to IonMonkey

(define (log-line-bailout? line)
  (regexp-match "^COACH: bailout:" line))

;; log->events : (listof string?) -> (listof optimization-event?)
(define (log->events log)
  (map parse-event
       (log->optimization-events
        ;; handle bailouts separately
        (filter (negate log-line-bailout?) log))))

;; first, split into optimization events
;; ASSUMPTION: all the logs that come after a "COACH: optimizing ..."
;;   message (but before another are related to that event.
;; log->optimization-events : (listof string?) -> (listof (listof string?))
(define (log->optimization-events l)
  (define-values (rev-events rev-current-event)
    (for/fold ([rev-events '()]
               [rev-current-event (list (first l))]) ; start first event
        ([line (in-list (rest l))])
      (cond [(regexp-match "^COACH: optimizing " line) ; starting a new event
             (values (cons (reverse rev-current-event)
                           rev-events)
                     (list line))]
            [else ; continue the current event
             (values rev-events
                     (cons line rev-current-event))])))
  (when (empty? rev-current-event)
    (error "empty log"))
  (reverse (cons (reverse rev-current-event)
                 rev-events)))

(define location-regexp "([^:]+):([0-9]+):([0-9]+) #([0-9]+):([0-9]+)")

;; second, parse location (and maybe some general info)
;; parse-event : (listof string?) -> optimization-event?
(define (parse-event e)

  ;; first line is of the form:
  ;; "COACH: optimizing <operation> <property>: <file>:<line>:<column> #<script>:<offset>"
  (match-define (list _ operation property file line column script offset)
    (regexp-match
     ;; note: will choke on unusual file / property names
     (string-append "^COACH: optimizing ([^ ]+) ([^: ]+): "
                    location-regexp
                    "$")
     (first e)))
  (unless (and operation property file line column script offset)
    (error "invalid log entry" (first e)))

  ;; type info is of the form:
  ;;   for getprop:
  ;;     "COACH:    types: <typeinfo>"
  ;;   for setprop:
  ;;     "COACH:    obj types: <typeinfo>"
  ;;     "COACH:    value types: <int>"
  ;; for now, we just consider typeinfo to be a string TODO exploit structure
  (define type-dict
    (with-handlers
        ([exn:misc:match? (lambda (_) (error "ill-formed event" e))])
      (cond [(equal? operation "getprop")
             (match-define (list _ obj-types)
               (regexp-match "^COACH:    types: ?(.*)$" (second e)))
             (hash "obj" obj-types)]
            [(equal? operation "setprop")
             (match-define (list _ obj-types)
               (regexp-match "^COACH:    obj types: ?(.*)$" (second e)))
             (match-define (list _ property-types) ; from the heap typeset
               ;; there's one set of types per possible object type
               ;; TODO eventually have a separator for those
               (regexp-match "^COACH:    property types: ?(.*)$" (third e)))
             (match-define (list _ value-types)
               (regexp-match "^COACH:    value types: ?(.*)$" (fourth e)))
             (hash "obj"      obj-types
                   "property" property-types
                   "value"    value-types)]
            [else
             (error "unknown operation" operation)])))
  (define attempts-log
    (if (equal? operation "getprop")
        (drop e 2) ; single line of type info + first line with general info
        (drop e 4))) ; three lines of type info

  (define event
    (optimization-event (location file
                                  (string->number line)
                                  (string->number column)
                                  (string->number script)
                                  (string->number offset)
                                  operation
                                  property)
                        operation
                        property
                        type-dict
                        #f)) ; filled below
  (set-optimization-event-attempts! event (parse-attempts attempts-log event))
  event)


;; given a list of lines that describe attempts at different implementation
;; strategies for an operation, parse what succeeded, what failed, and why
;; parse-attempts : (listof string?) -> (values (listof failure?) success?)
(define (parse-attempts lines event)
  (when (empty? lines) ; shouldn't happen
    (error "no attempts were made"))

  ;; each attempt log start with "trying <strategy>", then a line describing
  ;; success / failure
  ;; Note: there may be cases where there's more than one failure / success
  ;;   line. There are either leftovers from reporting multiple failures for
  ;;   the same attempt when possible, or bugs. Either way, if that happens,
  ;;   we should fix the logging. Reporting multiple causes for failures
  ;;   *could* be useful, but let's not worry for now (plus that makes this
  ;;   code much more complex)

  ;; simple state machine, match a strategy line, then a result line, ad inf.
  (define (parse-strategy ls)
    (if (empty? ls)
        '() ; done
        (match (regexp-match "^COACH:    trying (.+)$" (first ls))
          [(list _ strategy)
           (parse-result strategy (rest ls))]
          [_
           (error "attempt log does not have the right structure" lines)])))
  (define (parse-result strategy ls)
    (cond [(regexp-match "^COACH:        success(, )?(.*)$" (first ls))
           => (match-lambda [(list _ _ details)
                             (cons (success strategy
                                            event
                                            (and (not (equal? details ""))
                                                 details))
                                   (parse-strategy (rest ls)))])]
          [(regexp-match "^COACH:        failure, (.+)$" (first ls))
           => (match-lambda [(list _ reason)
                             (cons (failure strategy event reason)
                                   (parse-strategy (rest ls)))])]
          [else
           (error "unexpected result line" (first ls))]))
  (parse-strategy lines))


;; log->events : (listof string?) -> (listof bailout?)
(define (log->bailouts log)
  (map parse-bailout (filter log-line-bailout? log)))

;; parse-bailout : string? -> bailout?
(define (parse-bailout l)
  (printf "trying to parse: ~s\n" l) ;; TODO
  (match l
    [(list _ at/after file line column script offset kind) (void)]
    [_ (error "can't parse" l)]) ;; TODO
  (match-define (list _ at/after file line column script offset kind)
    (regexp-match
     ;; note: will choke on unusual file / property names
     (string-append
      "^COACH: bailout: resuming ([a-z]+) " ;; TODO there's now 2 different kinds of bailout lines
      location-regexp ;; TODO better reuse than that
      " kind: (.*)$")
     l))
  (bailout (location file
                     (string->number line)
                     (string->number column)
                     (string->number script)
                     (string->number offset)
                     #f ; these don't make as much sense for bailouts
                     #f)
           kind))
;; TODO can we get any more info from a bailout?
;;   maybe whether it caused invalidation?
