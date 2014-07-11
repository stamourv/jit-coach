#lang racket

;;;; struct definitions and struct-specific helpers

(provide (all-defined-out))

;; location : location?
;; operation : "getprop" | "setprop"
;; type-dict : (dictof string? typeset?)
;;   mapping "operand name" to their possible types
;; attempts : (listof attempt?)
;; profile-weight : number?
(struct optimization-event
  (location operation property type-dict attempts profile-weight)
  #:transparent
  #:mutable) ; so attempts can refer back to the event

(define (event-object-typeset event)
  (dict-ref (optimization-event-type-dict event) "obj"))
(define (event-object-types event) ; only object types, not primitive types
  (typeset-object-types (event-object-typeset event)))
(define (monomorphic-event? event)
  (single-object-type? (event-object-typeset event)))

;; representation of SpiderMonkey's TI's type sets
;; mostly about keeping object types separate from primitive types in our case
(struct typeset (primitive-types ; (listof string?)
                 object-types)   ; (listof string?)
        #:transparent
        #:guard (lambda (p o _)
                  (unless (and ((listof string?) p) ((listof string?) o))
                    (error "typesets expect lists of types" p o))
                  (values p o)))

(define (single-object-type? typeset)
  (= (length (typeset-object-types typeset)) 1))

(define (merge-typesets tss)
  (define pss (append-map typeset-primitive-types tss))
  (define oss (append-map typeset-object-types    tss))
  (typeset (remove-duplicates pss)
           (remove-duplicates oss)))


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


;; strategy : string?
;; event : optimization-event? ; the event during which this attempt was made
(struct attempt (strategy event) #:transparent
        #:methods gen:equal+hash
        [(define (equal-proc x y =?)
           ;; Don't look at the event. We want to equate attempts from
           ;; different events.
           ;; TODO does it make sense to store the event, then? as long as
           ;;   they're identical, I guess? (which should be the case for
           ;;   compile merging)
           ;; TODO or maybe could use =? for that? would it do cycle detection?
           (=?  (attempt-strategy x)
                (attempt-strategy y)))
         (define (hash-proc x h)
           (+ (h (attempt-strategy x))
              (h (attempt-event    x))))
         (define (hash2-proc x h)
           (* (h (attempt-strategy x))
              (h (attempt-event    x))))])

;; reason : string?
(struct failure attempt (reason) #:transparent
        #:methods gen:equal+hash
        [(define (equal-proc x y =?)
           (and (=?  (attempt-strategy x)
                     (attempt-strategy y))
                (=?  (failure-reason x)
                     (failure-reason y))))
         (define (hash-proc x h)
           (+ (h (attempt-strategy x))
              (h (attempt-event    x))
              (h (failure-reason   x))))
         (define (hash2-proc x h)
           (* (h (attempt-strategy x))
              (h (attempt-event    x))
              (h (failure-reason   x))))])

;; details: string? ; e.g. what sub-strategy succeeded
(struct success attempt (details) #:transparent
        #:methods gen:equal+hash
        [(define (equal-proc x y =?)
           (and (=?  (attempt-strategy x)
                     (attempt-strategy y))
                (=?  (success-details x)
                     (success-details y))))
         (define (hash-proc x h)
           (+ (h (attempt-strategy x))
              (h (attempt-event    x))
              (h (success-details  x))))
         (define (hash2-proc x h)
           (* (h (attempt-strategy x))
              (h (attempt-event    x))
              (h (success-details  x))))])
;; TODO maybe not have sub-strategies, and consider each as a top-level
;;  success/failure. currently, e.g., inlining a poly getprop is a success,
;;  but it's less good than inlining a mono one. but that doesn't show up as
;;  a failure


(define (event-failures event)
  (filter failure? (optimization-event-attempts event)))

;; strategy that was ultimately picked
(define (event-strategy event)
  (for/first ([a (optimization-event-attempts event)]
              #:when (success? a))
    (match-define (success strategy event details) a)
    (if details (format "~a (~a)" strategy details) strategy)))


;; Specific compiled version of a script, or set of identical compiled versions.
;; Includes the time spent executing it (both self and total time) and the
;; optimization events from that compile.
(struct compile (location-string ; string? ; from samples. unknown format
                 self-time ; flonum?
                 total-time ; flonum?
                 events) ; (listof optimization-event?)
        #:transparent)

