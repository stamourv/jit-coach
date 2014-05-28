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
      "  If so, try initializing it always in the same place.\n\n")]

    [(? (lambda (x) (regexp-match "^([0-9]+) possible object types$" x)) _)
     (string-append
      "This operation is polymorphic. Specifically, it sees these types:\n"
      (format "    ~a\n" (event-object-type event))
      "It would be optimized better if it was monomorphic.\n\n")]

    [reason ;; TODO implement more
     (format "~a (no explanation implemented yet!)\n\n" reason)]))
