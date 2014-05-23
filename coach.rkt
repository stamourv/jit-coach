#lang racket

(require unstable/list)

;;;; parsing

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

;; second, parse location (and maybe some general info)
;; location : location?
;; operation : "getprop" | "setprop"
;; type-dict : (dictof string? string?)
;;   mapping "operand name" to their possible types
;; attempts : (listof attempt?)
(struct optimization-event (location operation property type-dict attempts)
        #:transparent)
;; file + line + column are not enough to disambiguate. script + offset is
;; also includes operation + property, for printing
(struct location (file line column script offset operation property)
        #:transparent
        #:methods gen:custom-write
        [(define (write-proc location port _)
           (fprintf port "~a:~a:~a (~a ~a, offset: ~a)"
                    (location-file location)
                    (location-line location)
                    (location-column location)
                    (location-operation location)
                    (location-property location)
                    ;; because line+column info seems to point at the
                    ;; *statement*, there may be multiple, e.g., getprop x
                    ;; at the same location (e.g. v.x + w.x).
                    ;; bytecode offset allows us to disambiguate, but
                    ;; doesn't really point precisely, which is bad.
                    ;; TODO is there a way to get more precise info?
                    (location-offset location)))])

;; parse-event : (listof string?) -> optimization-event?
(define (parse-event e)

  ;; first line is of the form:
  ;; "COACH: optimizing <operation> <property>: <file>:<line>:<column> #<script>:<offset>"
  (match-define (list _ operation property file line column script offset)
    (regexp-match
     ;; note: will choke on unusual file / property names
     "^COACH: optimizing ([^ ]+) ([^: ]+): ([^: ]+):([0-9]+):([0-9]+) #([0-9]+):([0-9]+)$"
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
                      (parse-attempts attempts-log)))


;; strategy : string?
;; TODO grammar is a bit inconsistent in logs. sometimes lists the operation
;;   too, sometimes is a verb phrase, sometimes a noun. clean up
(struct attempt (strategy) #:transparent)
;; reason : string?
(struct failure attempt (reason) #:transparent)
;; details: string? ; e.g. what sub-strategy succeeded
(struct success attempt (details) #:transparent)
;; TODO maybe not have sub-strategies, and consider each as a top-level
;;  success/failure. currently, e.g., inlining a poly getprop is a success,
;;  but it's less good than inlining a mono one. but that doesn't show up as
;;  a failure

;; given a list of lines that describe attempts at different implementation
;; strategies for an operation, parse what succeeded, what failed, and why
;; parse-attempts : (listof string?) -> (values (listof failure?) success?)
(define (parse-attempts lines)
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
                                            (and (not (equal? details ""))
                                                 details))
                                   (parse-strategy (rest ls)))])]
          [(regexp-match "^COACH:        failure, (.+)$" (first ls))
           => (match-lambda [(list _ reason)
                             (cons (failure strategy reason)
                                   (parse-strategy (rest ls)))])]
          [else
           (error "unexpected result line" (first ls))]))
  (parse-strategy lines))


;;;; reporting

;; group-by-location : (listof optimization-event?)
;;                       -> (listof (listof optimization-event?)
(define (group-by-location opt-events)
  (group-by optimization-event-location opt-events))

;; detect-flip-flop : (listof optimization-event?)
;;                      -> (or/c (listof typeinfo?) #f)
;; takes a list of events that affect a given location, and returns
;;  - a list of typeinfos, if this location causes flip-flopping
;;    (i.e. we compile over and over, making an assumption and then
;;    invalidating it, repeatedly). returns the typesets that we keep
;;    flip-flopping between.
;;  - #f, if this site doesn't cause flip-flopping.
;; Niko's explanation for why flip-flopping happens at all:
;;   on major GCs, compiled code, baseline ICs, typesets, etc. all get
;;   GCed (hard to tell what's still hot and/or useful), so need to
;;   start gathering info and compiling from scratch every time.
;;   (not the case for ||js code, though, but not relevant for us)
(define (detect-flip-flop event-group)

  ;; we only care about type sets for `obj`, since it's the one
  ;; decisions are made on
  (define all-typesets
    (remove-duplicates (for/list ([e event-group])
                         (dict-ref (optimization-event-type-dict e) "obj"))))

  ;; for now, do crude matching over traces with regexps
  (when (> (length all-typesets) 10)
    (error "too many typesets (need less stupid matching)" all-typesets))
  (define typeset->char
    (for/hash ([t all-typesets]
               [s (in-string "0123456789")])
      (values t s)))
  (define trace
    (list->string (for/list ([e event-group])
                    (dict-ref typeset->char
                              (dict-ref (optimization-event-type-dict e)
                                        "obj")))))
  ;; try all the combinations of typesets to flip-flop between
  (for*/first ([a all-typesets]
               [b (remove a all-typesets)] ; the two need to be distinct
               #:when (regexp-match
                       (pregexp (format "(~a+~a+){2}"
                                        (dict-ref typeset->char a)
                                        (dict-ref typeset->char b)))
                       trace))
    (list a b)))

;; report-flip-flops : (listof optimization-event?) -> void?
;; takes a list of (ungrouped) events, and prints flip-flop reports
;; TODO eventually return a report struct, or sth, instead of printing
(define (report-flip-flops all-events)
  (define by-location (group-by-location all-events))
  (for ([es by-location])
    (when (empty? es) ; can't happen
      (error "can't detect flip-flopping of an empty group"))
    (define flip-flop? (detect-flip-flop es))
    (when flip-flop?
      (printf "flip-flopping detected at ~a\n"
              (optimization-event-location (first es)))
      (printf "  between: ~a\n  and: ~a\n\n"
              (first flip-flop?) (second flip-flop?)))))

;; TODO try a simpler one, that just checks for monotonicity (never see an old one again)


(define (event-failures event)
  (filter failure? (optimization-event-attempts event)))
(define (event-strategy event)
  (for/first ([a (optimization-event-attempts event)]
              #:when (success? a))
    (match-define (success strategy details) a)
    (if details (format "~a (~a)" strategy details) strategy)))

(struct regression (from-event to-event) #:transparent
        #:methods gen:custom-write
        [(define (write-proc regression port _)
           (fprintf port "from ~a to ~a\n"
                    (regression-from-strategy regression)
                    (regression-to-strategy regression))
           (fprintf port "causes:\n\n")
           ;; TODO call the failure explanation logic, when it exists
           (for ([f (regression-new-failures regression)])
             ;; TODO put explain-failure in the printer for failures?
             ;;   actually, can't. also requires the enclosing event
             (fprintf port "~a"
                      (explain-failure f (regression-to-event regression)))))])

(define (regression-from-strategy regression)
  (event-strategy (regression-from-event regression)))
(define (regression-to-strategy regression)
  (event-strategy (regression-to-event regression)))
(define (regression-new-failures regression)
  (define old-failures (event-failures (regression-from-event regression)))
  (define new-failures (event-failures (regression-to-event regression)))
  ;; compute the delta
  ;; ASSUMPTION: shared prefix, same strategies tried in same order
  ;; INVARIANT: new-failures has more failures than old-failures
  ;;   (o/w wouldn't be a regression)
  (drop new-failures (length old-failures)))

;; detect-regression : (listof optimization-event?)
;;                       -> (or/c (listof success?) #f)
;; takes a list of events that affect a given location, and returns #f
;; if the number of failures does not increase, or the list of strategies
;; that were picked over time (in case worse strategies get picked over time)
;; ASSUMPTION: more failures = worse strategy is picked
;;   this relies on, for a given operation, the same strategies being attempted
;;   in the same order every time.
(define (detect-regression event-group)
  (define-values (max-n-failures max-n-failures-event rev-regressions)
    (for/fold ([max-n-failures (length (event-failures (first event-group)))]
               [max-n-failures-event (first event-group)]
               [rev-regressions '()])
        ([e (rest event-group)])
      (define n-failures (length (event-failures e)))
      (cond [(> n-failures max-n-failures) ; regression
             (values n-failures
                     e
                     (cons (regression max-n-failures-event e)
                           rev-regressions))]
            [else
             (values max-n-failures
                     max-n-failures-event
                     rev-regressions)])))
  (and (not (empty? rev-regressions)) ; we actually did get worse
       (reverse rev-regressions)))

;; report-regressions : (listof optimization-event?) -> void?
;; takes a list of (ungrouped) events, and prints regression reports
;; TODO eventually return a report struct, or sth, instead of printing
;; TODO abstract with flip-flop reporting
(define (report-regressions all-events)
  (define by-location (group-by-location all-events))
  (for ([es by-location])
    (when (empty? es) ; can't happen
      (error "can't detect regressions on an empty group"))
    (define regression? (detect-regression es))
    (when regression?
      (printf "implementation regressed at ~a\n"
              (optimization-event-location (first es)))
      (for-each displayln regression?))))


;;;; failure explanation

;; explain-failure : failure? optimization-event? -> string?
;; given a failure that happened during the given event, try to sketch
;; an explanation for the user
;; done on a case-by-case basis, work in progress.
(define (explain-failure failure event)
  (match (failure-reason failure)
    ["would require a barrier"
     (unless (equal? "setprop" (optimization-event-operation event))
       (error "should only happen for getprop"))
     (define expected-types
       (dict-ref (optimization-event-type-dict event) "property"))
     (define unexpected-type
       (dict-ref (optimization-event-type-dict event) "value"))
     (string-append
      (format "so far, this property has held values of type:\n    ~a\n"
              expected-types)
      (format "but this operation would assign a value of type:\n    ~a\n"
              unexpected-type)
      "which causes a type barrier to be added, which disables some "
      "optimizations\n\n")]
    [reason ;; TODO implement more
     (format "~a (no explanation implemented yet!)\n\n" reason)]))


(module+ main
  (define log-file (vector-ref (current-command-line-arguments) 0))
  (define log (file->lines log-file))
  (define opt-events (log->optimization-events log))
  ;; (for ([l opt-events]) (printf "~s\n\n" l))
  (define parsed-events (map parse-event opt-events))
  ;; (displayln (first parsed-events))
  ;; (displayln (for/first ([e parsed-events]
  ;;                        #:when (equal? (optimization-event-operation e)
  ;;                                       "setprop"))
  ;;              e))

  (define by-location (group-by-location parsed-events))
  ;; just out of curiosity, how often are things reported about
  ;; (displayln
  ;;  (sort (map length by-location) >))
  ;; ;; A: long tail distribution, at least in richards
  ;; (for-each displayln ;; most "popular" one in richards is line 527, bytecode 54 (that's the next.link)
  ;;           ;; TODO that one actually looks like a case of flip flopping: type info keeps going from "object" to "null + object", back and forth
  ;;           ;; TODO so, it seems like using null as a sentinel for a linked list may be a bad idea. should use arrays instead, maybe? or use an object of the same type with a flag?
  ;;           ;; TODO if that helps, that's a good recommendation, because it's not obvious at all (to me at least), at first
  ;;           ;; TODO despite the flip flopping, always succeeds at accessing a fixed slot anyway
  ;;           (first (sort by-location > #:key length)))


  ;; (detect-flip-flop (first (sort by-location > #:key length)))
  (report-flip-flops parsed-events) ; one found in richards

  ;; (for-each displayln (optimization-event-attempts (first parsed-events)))
  (report-regressions parsed-events) ; one found in paper-example-poly3

  )
