#lang racket

;;;; log parsing

(require "structs.rkt")

(define compile-id-regexp "^optimization info for compile #")

;; The gecko profiler emits profiles as JSON.
;; The two properties of the profile that we're interested in are 'marker and
;; 'threads. The former contains a list of "profile events" that are objects
;; containing a string describing the event and a timestamp. Optimization info
;; is stored in profile events (in the string portion), with one event per
;; compile. We don't care about the timestamps. Optimization info strings
;; include a "compile id", which is a sequence number that uniquely identifies
;; compiles.
;; The latter contains the samples taken by the profiler. Samples include stack
;; traces, timestamps and for ion-compiled code, compile ids.
;; We correlate which samples correspond to which compiles by matching compile
;; ids.

;; <json profile from gecko profiler> -> (listof compile?)
;; Takes a json profile dump (as a string) and returns a list of compiles.
(provide profile->compiles)
(define (profile->compiles profile*)
  ;; The xpcshell and dev tools profiler entry points don't output exactly the
  ;; same format. Find out which we have.
  (define profile
    (if (dict-has-key? profile* 'threads)
        profile*
        (dict-ref (dict-ref profile* 'profilerData) 'profile)))
  ;; We only handle single-threaded stuff for now.
  (define first-thread (first (dict-ref profile 'threads)))
  (define samples      (dict-ref first-thread 'samples))
  (define markers      (dict-ref first-thread 'markers))
  (define all-opt-info
    (for*/list ([m (dict-ref first-thread 'markers)]
                [n (in-value (dict-ref m 'name))]
                #:when (regexp-match compile-id-regexp n))
      n))

  (define-values (self-times total-times)
    (samples/time-spent->compile-ids/times
     (samples/timestamps->samples/time-spent samples)))

  (define compile-ids->locs
    (samples->compile-ids/locations samples))

  (define compile-id->events
    (for/hash ([logs all-opt-info])
      (define by-operation* (string-split logs "|"))
      (define prefix (first by-operation*))
      (match-define (list _ id-string)
        (regexp-match (format "~a([0-9]+)$"
                              compile-id-regexp)
                      prefix))
      (define id (string->number id-string))
      (define by-operation (rest by-operation*))
      (define by-line (append-map (lambda (o) (string-split o ";"))
                                  by-operation))
      (values id (log->events by-line (dict-ref self-times id 0.0)))))

  ;; generate `compile` structs
  (for/list ([id (dict-keys compile-id->events)]) ; superset of the others
    (compile
     ;; If we don't have a location, that's because we never sampled that
     ;; compile. #f is as good as anything, that compile will get pruned
     ;; for being cold anyway.
     (dict-ref compile-ids->locs id #f)
     (dict-ref self-times id 0.0)
     (dict-ref total-times id 0.0)
     (dict-ref compile-id->events id))))


;; (listof {frames: any/c, time: flonum?}) -> same
;; Converts samples to include time "spent" in each sample, instead of
;; a timestamp. Time spent is computed as half the sum of the two time ranges
;; that touch the sample (before and after).
;; E.g. with timestamps 10, 20 and 60, the middle sample would have
;; (/ (+ (- 20 10) (- 60 20)) 2) = 25 spent in it.
;; For the first and last samples, the time is twice the half of the single
;; adjacent range.
;; Drops single samples.
;; Note: adapted from the Racket profiler.
(define (samples/timestamps->samples/time-spent samples*)
  (cond
   [(or (empty? samples*) (empty? (rest samples*)))
    '()] ; no interval, can't compute anything
   [else
    ;; for some reason, the last sample doesn't have a timestamp
    (define (sample-time s)        (dict-ref s 'time #f))
    (define samples (filter sample-time samples*))
    (for/list ([prev (cons #f samples)]
               [cur  samples]
               [next (append (rest samples) '(#f))])
      (define (update-sample-time t) (dict-set cur 'time t))
      (define pre-interval
        (and prev (- (sample-time cur) (sample-time prev))))
      (define post-interval
        (and next (- (sample-time next) (sample-time cur))))
      (cond [(not prev) ; first sample
             (update-sample-time post-interval)]
            [(not next) ; last sample
             (update-sample-time pre-interval)]
            [else ; somewhere in the middle
             (update-sample-time (/ (+ pre-interval post-interval) 2))]))]))

;; (listof {frames: (listof {optional compile-id: integer?}), time: flonum?})
;;   -> (values (dictof compile-id self-time)
;;              (dictof compile-id total-time))
;; Traverses samples and compute how much self and total time is spent in each
;; compile.
(define (samples/time-spent->compile-ids/times samples)
  ;; map compile-ids (integers) to self time / total time
  (define self-times  (make-hash))
  (define total-times (make-hash))
  (define ((update-time table) frame time)
    (define id (dict-ref frame 'compile-id #f))
    (when id ; frame was ion compiled. otherwise, no compile-id to assign to
      (hash-update! table id (lambda (t) (+ t time)) 0.0)))
  (define update-self-time  (update-time self-times))
  (define update-total-time (update-time total-times))
  (for ([s samples])
    (define frames (dict-ref s 'frames))
    (define time   (dict-ref s 'time))
    (for ([f frames])
      (update-total-time f time))
    ;; update self time for leaves
    (unless (empty? frames)
      (update-self-time (last frames) time)))
  (values self-times total-times))


;;(listof {frames: (listof {optional compile-id: integer?}), location: string?})
;;   -> (dictof compile-id location)
;; Finds the location of the script corresponding to each (sampled) compile.
;; Note: we *could* include that info with opt info profile events. Last time I
;;   tried, I got weird memory corruption and gave up. May not be worth the
;;   trouble, since the current technique works just as well for sampled
;;   scripts, and those are the only ones that matter, really. Also, I'm not
;;   even sure script location is useful info for the coach.
(define (samples->compile-ids/locations samples)
  (for*/hash ([s   samples]
              [f   (dict-ref s 'frames)]
              [id  (in-value (dict-ref f 'compile-id #f))]
              [loc (in-value (dict-ref f 'location #f))]
              #:when (and id loc))
    (values id loc)))


;; log->events : (listof string?) number? -> (listof optimization-event?)
(define (log->events log profile-weight)
  (map (parse-event profile-weight) (log->optimization-events log)))

;; first, split into optimization events
;; ASSUMPTION: all the logs that come after a "optimizing ..."
;;   message (but before another) are related to that event.
;; log->optimization-events : (listof string?) -> (listof (listof string?))
(define (log->optimization-events l)
  (define-values (rev-events rev-current-event)
    (for/fold ([rev-events '()]
               [rev-current-event (list (first l))]) ; start first event
        ([line (in-list (rest l))])
      (cond [(regexp-match "^optimizing " line) ; starting a new event
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
;; parse-event : number? -> (listof string?) -> optimization-event?
(define ((parse-event profile-weight) e)

  ;; first line is of the form:
  ;; "optimizing <operation> [<argument>] <file>:<line>:<column> #<script>:<offset>"
  ;; Because "file" can actually refer to an eval'ed script (I think), in which
  ;; case the "filename" can include spaces and other stuff, and because there
  ;; is no argument field for non-get/setprop log entries, parse those after.
  (match-define (list _ operation argument+file line column script offset)
     (regexp-match
      "^optimizing ([^ ]+) (.+):([0-9]+):([0-9]+) #([0-9]+):([0-9]+)$"
      (first e)))
  (define-values (argument file)
    ;; note: will choke on unusual file / property names
    (cond [(member operation '("getprop" "setprop")) ; match a property name
           (match-define (list _ property file)
             (regexp-match "^([^: ]+) (.+)$" argument+file))
           (values property file)]
          [else ; it's all the filename
           (values #f argument+file)]))
  (unless (and operation file line column script offset)
    (error "invalid log entry" (first e)))

  ;; type info is of the form: "<part> types: <typeset>"
  (define type-log-regexp "^([^ ]+) types: ?(.*)$")
  (define-values (type-dict n-lines-consumed)
    (for/fold ([type-dict        (hash)]
               [n-lines-consumed 0])
        ([line (rest e)]
         ;; stop when we reach the end of the type info
         #:break (not (regexp-match type-log-regexp line)))
      (match-define (list _ part types)
        (regexp-match type-log-regexp line))
      (values (dict-set type-dict
                        part
                        (cond [(equal? part "property")
                               ;; Special case: In TI, properties can have
                               ;; multiple typesets (one per possible object
                               ;; type), which we log comma-separated. For
                               ;; simplicity, we merge the sets.
                               ;; TODO may be simpler for instrumentation to
                               ;;   allow multiple "messages" for property types
                               ;;   (to use addOptInfoTypeset) instead of a
                               ;;   comma-separated list
                               (merge-typesets
                                (map parse-typeset
                                     (string-split types ",")))]
                              [else (parse-typeset types)]))
              (add1 n-lines-consumed))))

  (define attempts-log
    ;; +1 for the initial line (with location and co)
    (drop e (add1 n-lines-consumed)))

  (define event
    (optimization-event (location file
                                  (string->number line)
                                  (string->number column)
                                  (string->number script)
                                  (string->number offset)
                                  operation
                                  argument)
                        operation
                        argument
                        type-dict
                        #f ; attempts, filled below
                        profile-weight))
  (set-optimization-event-attempts! event (parse-attempts attempts-log event))
  event)


;; parse-typeset : string? -> typeset?
;; Takes a printed representation of a typeset and returns a list of individual
;; types (still represented as strings).
(define object-marker "object\\[([0-9]+)\\] ")
(define (parse-typeset type-string)
  (define (parse-primitive-types s)
    ;; Remove info that doesn't really correspond to types.
    ;; E.g. the fact that a property is in a definite slot.
    (define pruning-regexps
      '("^\\[non-data\\]$"
        "^\\[non-writable\\]$"
        "^\\[definite:[0-9]+\\]$"))
    (for/list ([t (string-split s " ")]
               #:when (for/and ([r pruning-regexps])
                        (not (regexp-match r t))))
      t))
  (define (parse-object-types s)
    ;; Object types are printed space-separated.
    ;; Currently, object types' printed representation include:
    ;; constructor name, constructor location and address of object type.
    ;; The first two are for user consumption. The latter is to disambiguate.
    ;; For example in raytrace, because of the `Class` "class", all objects end
    ;; up with the same constructor, which ruins any kind of analysis on object
    ;; types. Disambiguating using the address solves the problem. Still not
    ;; great UI-wise thought, since addresses are meaningless to users. Bleh.
    (string-split s " "))
  ;; object types are last. there may be primitives before that
  (match (regexp-split object-marker type-string)
    [(list primitive-types) ; no object types
     (typeset (parse-primitive-types primitive-types) '())]
    [(list "" object-types) ; no primitive types
     (typeset '() (parse-object-types object-types))]
    [(list primitive-types object-types)
     (typeset (parse-primitive-types primitive-types)
              (parse-object-types    object-types))]))


;; given a list of lines that describe attempts at different implementation
;; strategies for an operation, parse what succeeded, what failed, and why
;; parse-attempts : (listof string?) -> (values (listof failure?) success?)
(define (parse-attempts lines event)
  (when (empty? lines) ; shouldn't happen
    (error "no attempts were made"))

  ;; each attempt log start with "trying <strategy>", then a line describing
  ;; success / failure
  ;; Note: there may be cases where there's more than one failure / success
  ;;   line. There are either leftovers from reporting multiple failures for
  ;;   the same attempt when possible, or bugs. Either way, if that happens,
  ;;   we should fix the logging. Reporting multiple causes for failures
  ;;   *could* be useful, but let's not worry for now (plus that makes this
  ;;   code much more complex)

  ;; simple state machine, match a strategy line, then a result line, ad inf.
  (define (parse-strategy ls)
    (if (empty? ls)
        '() ; done
        (match (regexp-match "^trying (.+)$" (first ls))
          [(list _ strategy)
           (parse-result strategy (rest ls))]
          [_
           (error "attempt log does not have the right structure" lines)])))
  (define (parse-result strategy ls)
    (cond [(regexp-match "^success(, )?(.*)$" (first ls))
           => (match-lambda [(list _ _ details)
                             (cons (success strategy
                                            event
                                            (and (not (equal? details ""))
                                                 details))
                                   (parse-strategy (rest ls)))])]
          [(regexp-match "^failure, (.+)$" (first ls))
           => (match-lambda [(list _ reason)
                             (cons (failure strategy event reason)
                                   (parse-strategy (rest ls)))])]
          [else
           (error "unexpected result line" (first ls))]))
  (parse-strategy lines))
