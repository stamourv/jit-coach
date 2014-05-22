#lang racket

(require unstable/list)

;;;; parsing

;; first, split into optimization events
;; ASSUMPTION: all the logs that come after a "COACH: optimizing ..."
;;   message (but before another are related to that event.
;; log->optimization-events : (listof string?) -> (listof (listof string?))
(define (log->optimization-events l)
  (define-values (rev-events rev-current-event)
    (for/fold ([rev-events '()]
               [rev-current-event (list (first l))]) ; start first event
        ([line (in-list (rest l))])
      (cond [(regexp-match "^COACH: optimizing " line) ; starting a new event
             (values (cons (reverse rev-current-event)
                           rev-events)
                     (list line))]
            [else ; continue the current event
             (values rev-events
                     (cons line rev-current-event))])))
  (when (empty? rev-current-event)
    (error "empty log"))
  (reverse (cons (reverse rev-current-event)
                 rev-events)))

;; second, parse location (and maybe some general info)
;; location : location?
;; operation : "getprop" | "setprop"
;; type-dict : (dictof string? string?)
;;   mapping "operand name" to their possible types
;; attempts : (listof string?) ;; TODO for now
(struct optimization-event (location operation type-dict attempts) #:transparent)
(struct location (file line column script offset) #:transparent)

;; parse-event : (listof string?) -> optimization-event?
(define (parse-event e)

  ;; first line is of the form:
  ;; "COACH: optimizing <operation>: <file>:<line>:<column> #<script>:<offset>"
  (match-define (list _ operation file line column script offset)
    (regexp-match
     "^COACH: optimizing ([^ ]+): ([^: ]+):([0-9]+):([0-9]+) #([0-9]+):([0-9]+)$"
     (first e)))
  (unless (and operation file line column script offset)
    (error "invalid log entry" (first e)))

  ;; type info is of the form:
  ;;   for getprop:
  ;;     "COACH:    types: <typeinfo>"
  ;;   for setprop:
  ;;     "COACH:    obj types: <typeinfo>"
  ;;     "COACH:    value types: <int>"
  ;; for now, we just consider typeinfo to be a string TODO exploit structure
  (define type-dict
    (cond [(equal? operation "getprop")
           (match-define (list _ obj-types)
             (regexp-match "^COACH:    types: ?(.*)$" (second e)))
           (hash "obj" obj-types)]
          [(equal? operation "setprop")
           (match-define (list _ obj-types)
             (regexp-match "^COACH:    obj types: ?(.*)$" (second e)))
           (match-define (list _ value-types)
             (regexp-match "^COACH:    value types: ?(.*)$" (third e)))
           (hash "obj" obj-types "value" value-types)]
          [else
           (error "unknown operation" operation)]))
  (define attempts-log
    (if (equal? operation "getprop")
        (drop e 2) ; single line of type info + first line with general info
        (drop e 3))) ; two lines of type info

  (optimization-event (location file
                                (string->number line)
                                (string->number column)
                                (string->number script)
                                (string->number offset))
                      operation
                      type-dict
                      attempts-log)) ;; TODO prune and parse that later

;; group-by-location : (listof optimization-event?)
;;                       -> (listof (listof optimization-event?)
(define (group-by-location opt-events)
  (group-by optimization-event-location opt-events))

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
(define (detect-flip-flop event-group)

  ;; we only care about type sets for `obj`, since it's the one
  ;; decisions are made on
  (define all-typesets
    (remove-duplicates (for/list ([e event-group])
                         (dict-ref (optimization-event-type-dict e) "obj"))))

  ;; for now, do crude matching over traces with regexps
  (when (> (length all-typesets) 10)
    (error "too many typesets (need less stupid matching)" all-typesets))
  (define typeset->char
    (for/hash ([t all-typesets]
               [s (in-string "0123456789")])
      (values t s)))
  (define trace
    (list->string (for/list ([e event-group])
                    (dict-ref typeset->char
                              (dict-ref (optimization-event-type-dict e)
                                        "obj")))))
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
      (printf "flip-flopping detected at ~a\n  between: ~a\n  and: ~a\n\n"
              (optimization-event-location (first es))
              (first flip-flop?) (second flip-flop?)))))

;; TODO try a simpler one, that just checks for monotonicity (never see an old one again)


(module+ main
  (define log-file (vector-ref (current-command-line-arguments) 0))
  (define log (file->lines log-file))
  (define opt-events (log->optimization-events log))
  ;; (for ([l opt-events]) (printf "~s\n\n" l))
  (define parsed-events (map parse-event opt-events))
  ;; (displayln (first parsed-events))
  ;; (displayln (for/first ([e parsed-events]
  ;;                        #:when (equal? (optimization-event-operation e)
  ;;                                       "setprop"))
  ;;              e))

  (define by-location (group-by-location parsed-events))
  ;; just out of curiosity, how often are things reported about
  ;; (displayln
  ;;  (sort (map length by-location) >))
  ;; ;; A: long tail distribution, at least in richards
  ;; (for-each displayln ;; most "popular" one in richards is line 527, bytecode 54 (that's the next.link)
  ;;           ;; TODO that one actually looks like a case of flip flopping: type info keeps going from "object" to "null + object", back and forth
  ;;           ;; TODO so, it seems like using null as a sentinel for a linked list may be a bad idea. should use arrays instead, maybe? or use an object of the same type with a flag?
  ;;           ;; TODO if that helps, that's a good recommendation, because it's not obvious at all (to me at least), at first
  ;;           (first (sort by-location > #:key length)))


  ;; (detect-flip-flop (first (sort by-location > #:key length)))
  (report-flip-flops parsed-events)

  )
