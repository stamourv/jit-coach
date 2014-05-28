#lang racket

(require unstable/list)

(require "structs.rkt" "recommendations.rkt" "utils.rkt")

;;;; produces reports related to individual locations (for now, specific
;;;; getprop/setprop operations). includes detection of temporal patterns

(provide report-flip-flops
         report-regressions
         report-consistently-bad)

;; group-by-location : (listof optimization-event?)
;;                       -> (listof (listof optimization-event?)
(define (group-by-location opt-events)
  (group-by optimization-event-location opt-events))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; TODO try a simpler one, that just checks for monotonicity
;;   (never see an old typeset again)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
