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


;; General structure representing a coaching report.
;; Includes one of the failures that originated the report (this is post
;; merging, so this report may summarize multiple failures). This failure
;; is considered representative of those that got merged, and should be
;; identical to the others, except along dimensions that were merged (and
;; thus should not be presented in the report, so we're good).
(struct report
  (object-typeset ; (listof <constructor>) ; all the object types affected
   failure        ; failure?
   badness)       ; number?
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc r port _)
     (parameterize ([current-output-port port])
       (print-report r)))])

;; Report structure for near misses that we think would be solved by changing
;; a/some constructor(s).
;; Does not keep track of failure locations, because (we think) the solution
;; is not likely to be found there.
(struct constructor-report report
  (properties+badnesses) ; (listof (list string? number?))
  ;; We keep track of badness for each property individually to help
  ;; programmers prioritize which properties to fix.
  ;; When reporting near misses at constructors, we report all the common
  ;; fields of a group of types together. Because they're in the same group,
  ;; these fields are (or should) be defined together, and so issues about
  ;; them would also be solved together.
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc r port _)
     (parameterize ([current-output-port port])
       (print-report
        r
        (lambda ()
          (printf "affected properties:\n")
          (for ([p (sort (constructor-report-properties+badnesses r)
                         > #:key second)])
            (printf "  ~a (badness: ~a)\n" (first p) (second p)))))))])

;; Report structure for near misses that we think should be solved directly
;; at the failure site (or at least, for which the failure site is informative
;; to find a solution).
(struct in-situ-report report
  ;; can get affected property from failure, so not stored directly
  (locations+badnesses)
  ;; We keep track of badness for each location (this is post temporal and
  ;; locality merging).
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc r port _)
     (parameterize ([current-output-port port])
       (print-report
        r
        (lambda ()
          (printf "affected property: ~a\n\n"
                  (attempt-property (report-failure r)))
          (printf "locations:\n")
          (for ([l (sort (in-situ-report-locations+badnesses r)
                         > #:key second)])
            (printf "  ~a (badness: ~a)\n" (first l) (second l)))))))])


;; Paper: the division above is worth discussing. Some failures are non-local
;; (fail at use site, fixed at constructor), but not all of them. Heuristics
;; to distinguish them, and different info when reporting them.


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


;; report-in-situ? : failure? -> boolean?
;; Determines whether the given failure should be reported at the failure site
;; itself (#t) or at the constructor (#f).
;; Some failures are solved by changing the constructor, and others by changing
;; the operation's context. To be actionable, we should report failures where
;; they would be solved.
;; This is a set of heuristics to determine where a failure would be solved.
(define (report-in-situ? failure)
  (define reason (failure-reason failure))
  (cond
   [(regexp-match "needs to add field" reason)
    ;; in this case, *where* the field is added matters, so need to show
    ;; location
    #t]
   [(regexp-match "access needs to go through the prototype" reason)
    ;; again, that's operation-specific
    #t]
   [(= (length (event-object-types (attempt-event failure))) 1)
    ;; failures affect a single type, likely to be fixed at constructor
    #f]
   ;; TODO more heuristics
   [else
    ;; default: show failure site. more information can't hurt
    #t]))


;; helpers for below
(define (events->total-badness group)
  (for/sum ([e group])
    (optimization-event-profile-weight e)))
(define (events->affected-properties group)
  (for/list ([g (group-by optimization-event-property group)])
    (list (optimization-event-property (first g))
          (events->total-badness g))))
(define (events->affected-locations group)
  (for/list ([g (group-by optimization-event-location group)])
    (list (optimization-event-location (first g))
          (events->total-badness g))))
;; Not all the types in a group may be relevant for a specific report.
;; The group includes all types that share the fields we're currently
;; emitting reports about. The failures that a specific report is about may
;; only involve some of those types, so compute the relevant types from the
;; failures themselves.
;; Note: computing those type groups is still necessary, to find out which
;; fields are actually the same.
(define (events->relevant-types group)
  (for/fold ([ts '()])
      ([e group])
    (set-union ts (event-object-types e))))

;; by-object-type-group->reports : (cons (listof <object-type-string>)
;;                                       (listof optimization-event?))
;;                                   -> (listof by-object-type-report?)
;; Generates the list of near miss reports for the given object-type-group.
;; Performs temporal merging: merges identical failures that affect the same
;;   operation but come from different compiles. Adds up their badness.
;; Also performs same-property merging when failure kinds are identical.
;; Multiple reports can originate from the same type group. Type groups are
;; mostly used for identifying which properties should be considered the same
;; across classes, and we don't always merge all failures corresponding to a
;; group of types.
(define (by-object-type-group->reports types+group)
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
  (flatten
   (for/list ([group by-failure-type])
     (define failure     (event-last-failure (first group)))
     (cond
      [(report-in-situ? failure)
       ;; Report at the failure site. One report per property.
       ;; Reasoning: since solution is not at the constructor, each field
       ;; should be considered separately, and hence be in a separate report.
       ;; Still perform same-field merging, though.
       (define by-property (group-by optimization-event-property group))
       (for/list ([g by-property])
         (define failure (first g))
         (in-situ-report (events->relevant-types g)
                         (event-last-failure (first g))
                         (events->total-badness g)
                         (events->affected-locations g)))]
      [else
       ;; Report at the constructor. Emit a single report, and perform
       ;; by-object-type merging.
       (constructor-report (events->relevant-types group)
                           failure
                           (events->total-badness group)
                           (events->affected-properties group))]))))


;; report-by-object-type : (listof optimization-event?) -> void?
;; takes a list of ungrouped events, and prints a by-object-type view
;; of optimization failures
(define (report-by-object-type all-events)
  ;; Only consider optimization events in code that was sampled.
  (define live-events
    (filter (lambda (e) (> (optimization-event-profile-weight e) 0))
            all-events))
  (define by-object-type (group-by-object-type-poly live-events))
  (define all-reports (append-map by-object-type-group->reports by-object-type))

  ;; do pruning based on badness (profile weight, really)
  ;; keep only top N
  ;; TODO could prune differently. e.g. take up to X% of the total badness
  ;;   or take reports until we reach a cutoff point (e.g. next is less than
  ;;   10% of the badness of the previous one)
  (define hot-reports
    (take (sort all-reports > #:key report-badness)
          (min 5 (length all-reports))))

  (for-each display hot-reports))

(define (print-report r [print-substruct-info void])
  (match-define (report typeset failure badness) r)
  (printf "badness: ~a\n\nfor object types: ~a\n\n"
          badness typeset)
  (printf "chosen strategy: ~a\nfailed strategy: ~a\nreason: ~a\n\n"
          (event-strategy (attempt-event failure))
          (attempt-strategy failure)
          (failure-reason failure))
  (print-substruct-info)
  (printf "\n~a" (explain-failure failure))
  (print-separator))
