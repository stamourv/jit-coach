#lang racket

(require "structs.rkt" "reports.rkt")

(require unstable/list)

;;;; Produces reports related to specific object types or groups of types.
;;;; Currently only does basic analysis (locality/temporal merging, some
;;;; irrelevant failure pruning).


(provide element-events->reports)

;; element-events->reports : (listof optimization-event?) -> (listof report?)
(define (element-events->reports events)
  (define reports
    (events->reports
     (temporal+locality-merging
      (filter counts-as-near-miss?
              (map irrelevant-failure-pruning events)))))
  reports)

;; -----------------------------------------------------------------------------


;; irrelevant-failure-pruning : optimization-event? -> optimization-event?
;; Prunes failures that are "expected" to happen, and are not indicative of
;; problems.
;; Works at the level of individual failures, removing them from events.
;; Note: if we eventually use # of failures when computing badness, pruning
;; at that level is useful.
;; TODO do the same for property events, instead of dooming the whole event
(define (irrelevant-failure-pruning event)
  (match-define (optimization-event location operation argument type-dict
                                    attempts profile-weight)
    event)
  (optimization-event location operation argument type-dict
                      (filter (negate irrelevant-failure?) attempts)
                      profile-weight))

;; irrelevant-failure? : attempt? -> boolean?
(define (irrelevant-failure? a)
  (and (failure? a) ; successes are trivially not irrelevant failures
       (let ()
         (define strategy (attempt-strategy a))
         (define reason   (failure-reason   a))
         (or
          ;; That will happen most times when we try that strategy. (Most things
          ;; aren't strings.) Other kinds of failures for that strategy may be
          ;; worth reporting, though.
          (and (equal? strategy "string")
               (equal? reason   "not a string"))
          ;; TODO more heuristics
          ))))

;; counts-as-near-miss? : optimization-event? -> boolean?
;; See comment in property-reports.rkt for general explanations.
(define (counts-as-near-miss? event)
  (define failures (event-failures event))
  (cond [(empty? failures) ; success
         #f]
        ;; TODO more heuristics
        [else
         #t]))


;; temporal+locality-merging : (listof optimization-event?)
;;                               -> (listof optimization-event?)
;; Merges events that affect the same location (possibly at different times),
;; as long as they have the same failures (for the same reasons), etc.
;; TODO also done during property report generation, but folded into the rest
;;   of the analysis. try to abstract the two.
(define (temporal+locality-merging events)
  (define by-location
    (group-by (lambda (e) (list (optimization-event-location e)
                                ;; these also need to be identical
                                (optimization-event-attempts e)
                                ;; TODO do we need to have identical types?
                                ;;   could maybe just merge the type dicts?
                                (optimization-event-type-dict e)))
              events))
  (for/list ([g by-location])
    (match-define (optimization-event location operation argument type-dict
                                      attempts profile-weight)
        (first g))
    (optimization-event location ; same for all
                        operation ; same for all
                        argument ; same for all
                        type-dict ; same for all
                        attempts ; same for all
                        (events->total-badness g)))) ; add up badness


;; events->reports : (listof optimization-event?) -> (listof report?)
;;
;; Generates element reports from element optimization events.
;;
;; Performs a second pass of locality merging, which merges events within
;; a script that affect the same array (using the address of the array
;; argument's MDefinition, from the optimization logs to distinguish arrays),
;; as long as they have the same failure mode.
;;
;; This second merging pass is useful because (1) near misses related to the
;; same array may be easy to solve together (hypothesis), and so would benefit
;; from being reported together, (2) it naturally increases the badness of
;; element reports, which prevent them from being pushed down in the ranking by
;; property reports (which are the subject of a lot of merging, and thus
;; generally have a high badness), and (3) it reduces the total number of
;; reports, and thus the amount of info shown to the user.
;;
;; We limit this merging to within a script to keep changes local, so that users
;; don't have to jump all over their program to follow a recommendation.
;;
;; TODO maybe just merge all within the same script, regardless of array?
;;   try both and see

(define (events->reports all-events)
  (define by-script+array
    (group-by (lambda (e)
                ;; same script, array and failure mode
                (list (location-script (optimization-event-location e))
                      (optimization-event-argument e)
                      (optimization-event-attempts e)))
              all-events))
  ;; generate one report per group
  (for/list ([g by-script+array])
    (define representative (first g))
    (match-define
        (optimization-event location operation argument type-dict
                            attempts profile-weight)
      representative)
    ;; TODO again, this pruning / merging (keeping only the last failure) is
    ;;   also done for property reports, but done with the rest of the analysis.
    ;;   abstract both pieces of code?
    (element-report (event-last-failure representative)
                    (events->total-badness g)
                    ;; should be unique after 1st locality merging pass
                    (map optimization-event-location g))))
