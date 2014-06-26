#lang racket

;;;; struct definitions and struct-specific helpers

(provide (all-defined-out))

;; location : location?
;; operation : "getprop" | "setprop"
;; type-dict : (dictof string? string?)
;;   mapping "operand name" to their possible types
;; attempts : (listof attempt?)
(struct optimization-event (location operation property type-dict attempts)
        #:transparent
        #:mutable) ; so attempts can refer back to the event

(define (event-object-type event)
  (dict-ref (optimization-event-type-dict event) "obj"))
(define (single-object-type? event)
  (regexp-match "^object\\[1\\]" (event-object-type event)))


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
(struct attempt (strategy event) #:transparent)

;; reason : string?
(struct failure attempt (reason) #:transparent)

;; details: string? ; e.g. what sub-strategy succeeded
(struct success attempt (details) #:transparent)
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


;; location : location?
;; kind : string?
(struct bailout (location kind) #:transparent)
