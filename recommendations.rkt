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

    ;; Failures for property events

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
      "This property is not guaranteed to always be in the same location.\n\n"
      "Are properties initialized in different orders in different places?\n"
      "  If so, try to stick to the same order.\n"
      "Is this property initialized in multiple places?\n"
      "  If so, try initializing it always in the same place.\n"
      "Is it sometimes on instances and sometimes on the prototype?\n"
      "  If so, try always putting it in the same location.\n\n")]

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
      (format "~a is the maximum number for inline property access.\n" max)
      "Try restricting the number of shapes, either by reducing the number\n"
      "of types that flow here, or making each type use shapes uniformily\n"
      "(e.g. by always initializing fields in the same order).\n\n")]

    ["singleton"
     (string-append
      "This object is a singleton.\n"
      "Singletons are not guaranteed to have properties in a fixed slot.\n"
      "Try making the object's field globals.\n\n")]

    ["fallback had unoptimizable access"
     ;; TODO under what circumstances does this happen?
     "Fallback had unoptimizable access.\n\n"]

    ["access needs to go through the prototype"
     (string-append
      "This operation needs to walk the prototype chain to find the property.\n"
      "Try putting the property in the same location for all objects.\n"
      "For example, try always putting it directly on the object, or always\n"
      "on its direct prototype.\n\n")]

    ["no known shapes"
     no-type-info-message]
    ["no type info"
     no-type-info-message]
    ;; TODO is that actionable? or should we just prune?

    ["needs to add field"
     (string-append
      "This operation may need to add the property it's assigning to to the\n"
      "object. Initializing it to a default value (of the right type) in the\n"
      "constructor may enable optimizations.\n"
      "This failure may also be due to a subclass calling a parent class's\n"
      "constructor. If so, you may want to inline the parent constructor, or\n"
      "initialize the property in the subclass's constructor.\n\n")]


    ;; Failures for element events

    ["index not an integer, string or symbol"
     (unexpected-type-message event)]
    ["index is not a number"
     (unexpected-type-message event #t)]

    [(or "not an object"
         ;; TODO have instrumentation emit the same message in both cases
         "input may not be an object") ; used by getprop / setprop
     (define typeset (dict-ref (optimization-event-type-dict event) "obj"))
     (if (equal? (typeset-primitive-types typeset) '("missing"))
         no-type-info-message
         (string-append ;; TODO not very actionable
          "The JIT is not certain that the accessed object is actually an\n"
          "object. Try warming up the code to give the JIT more type info.\n"))]


    [reason ;; TODO implement more
     (format "~a (no explanation implemented yet!)\n\n" reason)]))


(define no-type-info-message
  (string-append
   "The JIT had no type information available for this operation when it\n"
   "compiled the surrounding method, which prevented it from optimizing.\n"
   "Try executing that code (with objects of the right type) during\n"
   "initialization to provide typo information to the JIT.\n\n"))

(define (unexpected-type-message event [expected-int? #f])
  (define unexpected-typeset ; MIR type, so will have a single value type
    (dict-ref (optimization-event-type-dict event) "index"))
  (define explained-type
    (match (first (typeset-primitive-types unexpected-typeset))
      ["Value"
       "unknown kind of value"]
      [t
       t]))
  (define expected-msg
    (if expected-int?
        "an integer, and instead was a(n) "
        "either an integer, a string, or a symbol, and instead was a(n)\n"))
  (string-append
   "This array operation saw an index that was not guaranteed to be\n"
   expected-msg
   explained-type ".\n"
   "Try using an integer"
   (if expected-int? "" ", a string or a symbol")
   " instead.\n"
   "If you're already using an integer, trying doing `index|0` to help the\n"
   "JIT recognize its type.\n\n"))
