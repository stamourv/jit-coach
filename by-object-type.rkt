#lang racket

(require unstable/list)

(require "structs.rkt" "recommendations.rkt" "utils.rkt")

;;;; Produces reports related to specific object types or groups of types.
;;;; This groups issues that are likely to have a common solution (i.e. if you
;;;; fix a constructor, you may make operations on multiple fields (operations
;;;; that could themselves be all over the program) easier to optimize at the
;;;; same time.

;;;; Also, this will, of course, only consider operations (or failures)
;;;; that we *observed* were related to that type. We won't see places
;;;; where failure to see that type was the problem (e.g. non-executed
;;;; code, that would have seen that type, but didn't so didn't know how
;;;; to optimize).

;; research: this is a new kind of merging, worth discussing in paper/thesis
;;   that's a kind of analysis that an OO coach benefits from that a functional
;;   coach doesn't as much (I think)

(provide report-by-object-type)


(define (sets-overlap? ts1 ts2)
  (or (equal? ts1 ts2) ; to count two empty sets as overlapping
      ;; This is desirable to merge reports about fields that have the same
      ;; name and unknown constructors.
      ;; TODO try to avoid unknown constructors for singletons. would avoid
      ;;   the issue and give more precise reports
      (not (empty? (set-intersect ts1 ts2)))))

;; group-by-object-type-poly : (listof optimization-event?)
;;                               -> (listof (cons (listof <object-type-string>)
;;                                                (listof optimization-event?)))
;; For polymorphic operations, we use the following heuristic: if two operations
;; on properties of the same name have overlapping type sets, then chances are
;; they're talking about the same property (either from a common ancestor, or
;; from an ancestor's "virtual" property).
;; This assumes that no location will call both, e.g. `gun.draw` and
;; `canvas.draw`, otherwise all operations involving draw will conflate firearms
;; and art supplies.
;; Monomorphic operations are handled gracefully. If a property is only ever
;; used in monomorphic contexts, will be kept separate from polymorphic
;; properties on the same class(es).
(define (group-by-object-type-poly opt-events)

  ;; Step 1, group operations by property name.
  (define by-name (group-by optimization-event-property opt-events))

  ;; Step 2, for each property name, find equivalence classes, i.e. find
  ;; properties that are likely to be the "same", not accidental name clashes.
  ;; E.g., For `draw`, distinguish between firearms and art supplies.
  ;; TODO this is important at least for richards, where both TaskControlBlock
  ;;   and tasks have a `run` method, and the different tasks' `scheduler`
  ;;   fields are all used monomorphically, it looks like (same for v1, v2)
  (define names->equivalence-classes
    (for/hash ([g by-name])
      (define name (optimization-event-property (first g)))
      (define classes
        (for/fold ([classes '()])
            ([evt g])
          (let loop ([to-match (event-object-types evt)]
                     [classes  classes])
            (cond [(empty? classes)
                   ;; didn't find an equivalence class our typeset overlaps with
                   ;; start a new one
                   (cons to-match classes)]
                  [(sets-overlap? to-match (first classes))
                   ;; overlap, merge
                   (define new-class (set-union to-match (first classes)))
                   ;; look for overlap between this new class and the rest of
                   ;; the existing classes
                   (loop new-class
                         (rest classes))]
                  [else
                   ;; keep going
                   (cons (first classes)
                         (loop to-match (rest classes)))]))))
      (values name classes)))

  ;; (for ([(k v) (in-hash names->equivalence-classes)])
  ;;   (printf "~a -> ~s\n" k v))

  ;; Step 3, merge properties that appear on the same typesets, so they are
  ;; reported together.
  (define (find-class event)
    (define name (optimization-event-property event))
    (define classes (dict-ref names->equivalence-classes name))
    ;; find the equivalence class our typeset belongs to
    (define evt-types (event-object-types event))
    (findf (lambda (c) (sets-overlap? c evt-types))
           classes))
  (define by-typesets
    (group-by find-class opt-events))

  ;; (for ([g by-typesets])
  ;;   (for ([e g])
  ;;     (printf "~s -- ~a\n"
  ;;             (event-object-types e)
  ;;             (optimization-event-property e)))
  ;;   (newline) (newline) (newline))

  ;; For reporting, attach the equivalence class to each group.
  (for/list ([g by-typesets])
    (cons (find-class (first g)) g)))


;; For now, allowing multiple reports for the same object-typeset, one per
;; cause for failure. Makes sense, since those may have different solutions.
;; TODO probably want a general `report` struct, of which this is a substruct
(struct by-object-type-report
  (object-typeset       ; (listof <constructor>) ; all the object types affected
   failure              ; failure?
   ;;                     one of the failures from the group. all of them should
   ;;                     be equivalent, except for location, which we don't
   ;;                     care about at this point.
   badness              ; number?
   affected-properties  ; (listof (list string? number?))
   ;;                     keeps track of badness for each property individually
   ;;                     to help programmers prioritize which properties to fix
   ;;                     should probably keep reports (and thus pruning) at the
   ;;                     level of type sets, since it's a logical unit when
   ;;                     explaining
   locations            ; (listof (list location? number?))
   ;;                     keeps track of badness for each location
   ;;                     (post temporal + locality merging)
   )
  #:transparent)


;; counts-as-near-miss? : optimization-event? -> boolean?
;; Determines whether the given event should be considered a near miss.
;; If the event has no optimization failures, then it is a success, and not
;; a near miss.
;; Some failures do not count as near misses. For example, polymorphic getprops
;; / setprops that get compiled using the best polymorphic optimization strategy
;; are irrelevant failures (the code was probably *meant* to be polymorphic) and
;; should be pruned. Sure, the code may be faster if the operation was
;; monomorphic, but that recommendation is likely to be rejected by the user.
;; Note: this includes heuristics that are specific to getprop / setprop.
(define (counts-as-near-miss? event)
  (define failures (event-failures event))
  (cond [(empty? failures) ; success
         #f]
        [(regexp-match "inlining polymorphic" (event-strategy event))
         ;; best polymorphic strategy. irrelevant failure
         #f]
        [(and (regexp-match "inlining monomorphic" (event-strategy event))
              (> (length (event-object-types event)) 1))
         ;; we can do even better, if all types have the same shape.
         ;; only consider "inlining monomorphic" to be a failure for actual
         ;; monomorphic operations (which could potentially use definite slot)
         #f]
        [(regexp-match "shape in dictionary mode" (event-strategy event))
         ;; not actionable
         ;; dictionary mode is decided at run-time, based on a number of
         ;; factors, so we don't know why we ended up in dictionary mode, and
         ;; it's not clear how to avoid it, esp. since it may very well be the
         ;; intended semantics
         #f]
        [else
         #t]))

;; by-object-type-group->reports : (cons (listof <object-type-string>)
;;                                       (listof optimization-event?))
;;                                   -> (listof by-object-type-report?)
;; Generates the list of near miss reports for the given object-type-group.
;; Performs temporal merging: merges identical failures that affect the same
;;   operation but come from different compiles. Adds up their badness.
;; Also performs by-property merging.
(define (by-object-type-group->reports types+group)
  (define common-types     (first types+group))
  (define group            (rest types+group))
  (define near-miss-events (filter counts-as-near-miss? group))

  ;; Secondary grouping by failure type.
  ;; We only consider the last failure for each event. That way, we avoid
  ;; producing multiple reports about the same operations (which is not helpful)
  ;; and instead report the worst problem. If that problem is fixed, the user
  ;; can re-run the coach and see the other failures (assuming they weren't also
  ;; fixed at the same time).
  (define by-failure-type
    (group-by event-last-failure near-miss-events))
  (for/list ([group by-failure-type])
    (define failure (event-last-failure (first group)))
    (define (total-badness group)
      (for/sum ([e group])
        (optimization-event-profile-weight e)))
    (define by-property
      (group-by optimization-event-property group))
    (define affected-properties
      (for/list ([g by-property])
        (list (optimization-event-property (first g))
              (total-badness g))))
    (define by-location
      (group-by optimization-event-location group))
    (define affected-locations
      (for/list ([g by-location])
        (list (optimization-event-location (first g))
              (total-badness g))))
    (by-object-type-report common-types
                           failure
                           (total-badness group)
                           affected-properties
                           affected-locations)))

;; report-by-object-type : (listof optimization-event?) -> void?
;; takes a list of ungrouped events, and prints a by-object-type view
;; of optimization failures
(define (report-by-object-type all-events)
  (define by-object-type (group-by-object-type-poly all-events))
  (define all-reports (append-map by-object-type-group->reports by-object-type))

  ;; do pruning based on badness (profile weight, really)
  ;; keep only top N
  ;; TODO could prune differently. e.g. take up to X% of the total badness
  ;;   or take reports until we reach a cutoff point (e.g. next is less than
  ;;   10% of the badness of the previous one)
  (define live-reports
    (filter (lambda (r) (> (by-object-type-report-badness r) 0))
            all-reports))
  (define hot-reports
    (take (sort live-reports > #:key by-object-type-report-badness)
          (min 5 (length live-reports))))

  (for ([report hot-reports])
    (match-define (by-object-type-report typeset failure badness properties locations)
      report)
    (printf "badness: ~a\n\nfor object types: ~a\n\n"
            badness typeset)
    (printf "chosen strategy: ~a\nfailed strategy: ~a\nreason: ~a\n\n"
            (event-strategy (attempt-event failure))
            (attempt-strategy failure)
            (failure-reason failure))
    (printf "affected properties:\n")
    (for ([p (sort properties > #:key second)])
      (printf "  ~a (badness: ~a)\n" (first p) (second p)))
    (printf "\n~a" (explain-failure failure))
    (printf "locations:\n")
    (for ([l (sort locations > #:key second)])
      (printf "  ~a (badness: ~a)\n" (first l) (second l)))
    (newline)
    (print-separator)))
