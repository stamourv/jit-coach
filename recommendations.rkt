#lang racket

;;;; recommendation generation

(require "structs.rkt")

(provide explain-failure)

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
      "  If so, try initializing it always in the same place.\n"
      "Is not sometimes but not always an own property?\n"
      "  If so, try using it consistently.\n\n")]

    [(? (lambda (x) (regexp-match "^([0-9]+) possible object types$" x)))
     (string-append
      "This operation is polymorphic. Specifically, it sees these types:\n"
      (format "    ~a\n" (event-object-typeset event))
      "It would be optimized better if it was monomorphic.\n\n")]

    [(app (lambda (x)
            (regexp-match "^([0-9]+) possible shapes, ([0-9]+) max$" x))
          (list _ obs max))
     ;; TODO use for promixity
     (string-append
      (format "This operation observed different ~a shapes so far.\n" obs)
      (format "~s is the maximum number for inline property access.\n" max)
      "Try restricting the number of shapes, either by reducing the number\n"
      "of types that flow here, or making each type use shapes uniformily\n"
      "(e.g. by always initializing fields in the same order).\n\n")]

    ["singleton"
     (string-append
      "This object is a singleton.\n"
      "Singletons are not guaranteed to have properties in a fixed slot.\n"
      "Try making the object's field globals.\n\n")]

    ["shape in dictionary mode"
     ;; not very actionable
     ;; dictionary mode is decided at run-time, based on a number of factors,
     ;; so we don't know why we ended up in dictionary mode, and it's not clear
     ;; how to avoid it, esp. since it may very well be the intended semantics.
     ;; TODO prune these reports altogether?
     (string-append
      "This operation saw (an) object(s) in dictionary mode.\n"
      "This typically means objects with a large number of properties, or\n"
      "dynamically added or removed properties.\n\n")]

    ["fallback had unoptimizable access"
     ;; TODO under what circumstances does this happen?
     "Fallback had unoptimizable access.\n\n"]

    [reason ;; TODO implement more
     (format "~a (no explanation implemented yet!)\n\n" reason)]))
