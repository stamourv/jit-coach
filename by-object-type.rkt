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
  (object-typeset   ; (listof <constructor>) ; all the object types affected
   failure          ; failure?
   badness          ; number?
   affected-fields) ; (listof string?)
  #:transparent)

;; by-object-type-group->reports : (cons (listof <object-type-string>)
;;                                       (listof optimization-event?))
;;                                   -> (listof by-object-type-report?)
;; Generates the list of near miss reports for the given object-type-group.
;; Performs temporal merging: merges identical failures that affect the same
;;   operation but come from different compiles. Adds up their badness.
;; Also performs by-field merging.
(define (by-object-type-group->reports types+group)
  (define common-types (first types+group))
  (define group        (rest types+group))

  ;; secondary grouping by failure type (currently counts both attempted
  ;; strategy and cause of failure)
  (define all-failures (append-map event-failures group))
  (define by-failure-type
    (group-by (lambda (f) (cons (attempt-strategy f)
                                (failure-reason f)))
              all-failures))
  (for/list ([group by-failure-type])
    (define failure (first group))
    (define total-badness
      (for/sum ([a group])
        (optimization-event-profile-weight
         (attempt-event a))))
    (define affected-fields
      (remove-duplicates
       (for/list ([a group])
         (optimization-event-property (attempt-event a)))))
    (by-object-type-report common-types
                           failure
                           total-badness
                           affected-fields)))

;; report-by-object-type : (listof optimization-event?) -> void?
;; takes a list of ungrouped events, and prints a by-object-type view
;; of optimization failures
(define (report-by-object-type all-events)
  (define by-object-type (group-by-object-type-poly all-events))
  (define all-reports (append-map by-object-type-group->reports by-object-type))

  ;; do pruning based on badness (profile weight, really)
  ;; keep only top N
  ;; TODO could prune differently. e.g. take up to X% of the total badness
  (define hot-reports
    (take (sort all-reports > #:key by-object-type-report-badness)
          (min 5 (length all-reports))))

  (for ([report hot-reports])
    (match-define (by-object-type-report typeset failure badness fields)
      report)
    (printf
     "badness: ~a\n\nfor object types: ~a\n\nstrategy: ~a\nreason: ~a\n\n"
     badness typeset (attempt-strategy failure) (failure-reason failure))
    (printf "affected fields:\n")
    (for ([f fields]) (printf "  ~a\n" f))
    (printf "\n~a\n" (explain-failure failure))
    (print-separator)))
