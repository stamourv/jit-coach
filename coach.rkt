#lang racket

(require unstable/list)

(require "structs.rkt" "parsing.rkt")

(define separator (make-string 80 #\-))
(define (print-separator) (displayln separator))




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
;;   TODO will that always be the case, though? will compiles eventually
;;     survive across GC? if so, may not want to assume that.
;; TODO flip-flopping is a problem at the *function* level. that is, the
;;   whole function gets recompiled. *but* it will get reported for all
;;   operations in the function (which is redundant) regardless of whether
;;   that operation was actually responsible (which is BAD).
;;   e.g.: deltablue, BinaryConstraint.prototype.output flip-flops
;;     going back and forth beween 1 and 2 obj types, but we don't know
;;     which property access is for these objects (can guess, based on code,
;;     but at least one of the operations that are reported is a total red
;;     herring)
;;   TODO maybe that's something that bailout logging could help with
;;     we could get the cause for a recompilation, rather than inferring it
;;     from the newly disabled optimizations
;;     (first, figure out if fixing flip-flopping is actually worth it)
(define (detect-flip-flop event-group)

  ;; we only care about type sets for `obj`, since it's the one
  ;; decisions are made on
  (define all-typesets
    (remove-duplicates (map event-object-type event-group)))

  ;; for now, do crude matching over traces with regexps
  (when (> (length all-typesets) 10)
    (error "too many typesets (need less stupid matching)" all-typesets))
  (define typeset->char
    (for/hash ([t all-typesets]
               [s (in-string "0123456789")])
      (values t s)))
  (define trace
    (list->string (for/list ([e event-group])
                    (dict-ref typeset->char (event-object-type e)))))
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
              (first flip-flop?) (second flip-flop?))
      (print-separator))))

;; TODO try a simpler one, that just checks for monotonicity (never see an old one again)


;; from-event, to-event : optimization-event? ; before and after regression
;; got-better? : (or/c optimization-event? #f) ; event at which we got better
(struct regression (from-event to-event got-better?) #:transparent
        #:methods gen:custom-write
        [(define (write-proc regression port _)
           (fprintf port "from ~a to ~a~a\n"
                    (event-strategy (regression-from-event regression))
                    (event-strategy (regression-to-event regression))
                    (if (regression-got-better? regression)
                        (format " (but eventually got back to ~a)"
                                (event-strategy
                                 (regression-got-better? regression)))
                        ""))
           (fprintf port "causes:\n\n")
           (for ([f (regression-new-failures regression)])
             (fprintf port "~a" (explain-failure f))))])

(define (regression-new-failures regression)
  (define old-failures (event-failures (regression-from-event regression)))
  (define new-failures (event-failures (regression-to-event regression)))
  ;; compute the delta
  ;; ASSUMPTION: shared prefix, same strategies tried in same order
  ;; INVARIANT: new-failures has more failures than old-failures
  ;;   (o/w wouldn't be a regression)
  (drop new-failures (length old-failures)))

;; detect-regression : (listof optimization-event?)
;;                       -> (or/c (listof regression?) #f)
;; takes a list of events that affect a given location, and returns #f
;; if the number of failures does not increase, or a list of regression events.
;; also reports "local" regressions, that get fixed by subsequent compilations,
;; but marks them specially
;; ASSUMPTION: more failures = worse strategy is picked
;;   this relies on, for a given operation, the same strategies being attempted
;;   in the same order every time.
(define (detect-regression event-group)
  (define-values (prev-n-failures prev-event just-regressed? rev-regressions)
    (for/fold ([prev-n-failures (length (event-failures (first event-group)))]
               [prev-event (first event-group)]
               [just-regressed? #f]
               [rev-regressions '()])
        ([e (rest event-group)])
      (define n-failures (length (event-failures e)))
      (cond [(> n-failures prev-n-failures) ; regression
             (values n-failures
                     e
                     #t
                     (cons (regression prev-event e #f)
                           rev-regressions))]
            [just-regressed?
             (if (< n-failures prev-n-failures) ; are we getting better?
                 (values n-failures ; yes, make note of it
                         e
                         #f
                         (cons (match-let ([(regression from to _)
                                            (first rev-regressions)])
                                 (regression from to e)) ; we got better
                               (rest rev-regressions)))
                 (values n-failures ; no, just as bad
                         e
                         #t ; keep looking if we get better
                         rev-regressions))]
            [else
             (values prev-n-failures
                     e
                     #f
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
      (for-each displayln regression?)
      (newline)
      (print-separator))))

(define (all-the-same? l)
  (cond [(empty? l) #t]
        [else
         (define head (first l))
         (andmap (lambda (x) (equal? x head)) (rest l))]))

;; detect-consistently-bad : (listof optimization-event?)
;;                           [#:monomorphic? any/c #f]
;;                             -> (or/c consistently-bad #f)
;; takes a list of events that affect the same location, and returns a
;; list of failures if the same failure pattern happens every compilation
;; (i.e. we have found a consistent failure), or #f otherwise
;; if the #:monomorphic? argument is non-#f, also detect failures for
;; monomorphic operations (otherwise don't, and let some other pass report them)
(struct consistently-bad (failures n-times))
(define (detect-consistently-bad events #:monomorphic? [monomorphic? #f])
  (when (empty? events)
    (error "no events for a location"))
  (define failuress (map event-failures events))
  (define representative (first failuress))
  (and (all-the-same? failuress)
       (not (empty? representative)) ; actually has failures
       ;; operation is not monomorphic.
       ;; those failures are worth reporting, but they're reported better
       ;; by the by-object-type view.
       ;; other operations are not so lucky, and must be reported here.
       (or monomorphic? ; show them anyway
           (not (single-object-type? (attempt-event (first representative)))))
       (consistently-bad representative (length failuress))))

;; report-consistently-bad : (listof optimization-event?) -> void?
;; takes a list of (ungrouped) events, and prints report of consistent issues
;; TODO abstract with other reporting functions
(define (report-consistently-bad all-events)
  (define by-location (group-by-location all-events))
  (for ([es by-location])
    (when (empty? es)
      (error "no optimization events for a location"))
    (define consistently-bad? (detect-consistently-bad es))
    (when consistently-bad?
      (printf "always picking a sub-optimal implementation (~a times):\n"
              (consistently-bad-n-times consistently-bad?))
      (printf "strategy: ~a\nat: ~a\n"
              (event-strategy (first es))
              (optimization-event-location (first es)))
      (printf "failures:\n\n")
      (for ([failure (consistently-bad-failures consistently-bad?)])
        (printf "~a: ~a" (attempt-strategy failure) (explain-failure failure)))
      (print-separator))))


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


;;;; failure explanation

;; explain-failure : failure? -> string?
;; given a failure, try to sketch an explanation for the user
;; done on a case-by-case basis, work in progress.
(define (explain-failure failure)
  (define event (attempt-event failure))
  (match (failure-reason failure)

    ["would require a barrier"
     (unless (equal? "setprop" (optimization-event-operation event))
       (error "should only happen for getprop"))
     (define expected-types
       (dict-ref (optimization-event-type-dict event) "property"))
     (define unexpected-type
       (dict-ref (optimization-event-type-dict event) "value"))
     (string-append
      (format "So far, this property has held values of type:\n    ~a\n"
              expected-types)
      (format "but this operation would assign a value of type:\n    ~a\n"
              unexpected-type)
      "which causes a type barrier to be added, which disables some "
      "optimizations.\n\n")]

    ["property not in a fixed slot"
     (string-append
      "This property is not guaranteed to always be in the same location.\n"
      "Are you initializing fields in a different order in different places?\n"
      "  If so, try to stick to the same order.\n"
      "Are you initializing it in multiple places?\n"
      "  If so, try initializing it always in the same place.\n\n")]

    [(? (lambda (x) (regexp-match "^([0-9]+) possible object types$" x)) _)
     (string-append
      "This operation is polymorphic. Specifically, it sees these types:\n"
      (format "    ~a\n" (event-object-type event))
      "It would be optimized better if it was monomorphic.\n\n")]

    [reason ;; TODO implement more
     (format "~a (no explanation implemented yet!)\n\n" reason)]))


(module+ main
  (define log-file (vector-ref (current-command-line-arguments) 0))
  (define log (file->lines log-file))
  (define parsed-events (log->events log))

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
  (report-flip-flops parsed-events) ; one found in richards, 3 in deltablue

  ;; (for-each displayln (optimization-event-attempts (first parsed-events)))
  (report-regressions parsed-events) ; one found in paper-example-poly3

  (report-consistently-bad parsed-events)

  (report-by-object-type parsed-events)

  )
