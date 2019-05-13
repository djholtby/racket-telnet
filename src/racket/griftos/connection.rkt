#lang racket/base
(require racket/class racket/bool racket/list json racket/contract racket/set racket/hash racket/match json)

(provide telnet-message? telnet-message?/c conn<%> terminal<%> terminal%)

(define (telnet-message? v)
  (or (string? v)
      (bytes? v)
      (symbol? v)
      (false? v)
      (eof-object? v)
      (and
       (cons? v)
       (symbol? (car v)))))


(define telnet-message?/c
  (or/c string? bytes? symbol? #f eof-object? (cons/c symbol? any/c)))

;; A connection (conn<%>) is something that can send and receive network layer messages
(define conn<%>
  (interface ()
    [receive  (->m telnet-message?/c void?)]      ; called when the server receives a message from the client
    [transmit (->m telnet-message?/c ... void?)]  ; you can call this to send a message back to the client.
    [connected? (->m boolean?)]
    ))

(define terminal<%>
  (interface (conn<%>)
    [dimensions (->m (cons/c exact-positive-integer? exact-positive-integer?))]
    [supports? (->m symbol? boolean?)]
    [supports-union! (->m (set/c symbol? #:kind 'dont-care) ... void?)]
    [get-supports (->m (set/c symbol? #:kind 'mutable))]
    [get-encoding (->m symbol?)]
    [set-encoding! (->m symbol? boolean?)]
    ))

(define terminal%
  (class* object% (terminal<%>)
    (super-new)
    (init-field ip [secure? #f] [markup-settings (make-hasheq)])
    
    (abstract dimensions get-encoding set-encoding! receive transmit connected?)
    (define supports (mutable-seteq 'color)) ; by default everything supports color!
    (define supports:override:on (mutable-seteq))     ; overrides 
    (define supports:override:off (mutable-seteq))
    
    (define/public (supports? option)
      (and (or (set-member? supports option)
               (set-member? supports:override:on option))
           (not (set-member? supports:override:off option))))

    (define/public (supports-union! . support-sets)
      (apply set-union! supports (map (λ (s)
                                        (if (cons? s) (apply mutable-seteq s) s))
                                      support-sets)))

    (define/public (set-support! option [on? #t] #:override [override? #f])
      (if override?
          (if on?
              (begin (set-add! supports:override:on option)
                     (set-remove! supports:override:off option))
              (begin (set-add! supports:override:off option)
                     (set-remove supports:override:on option)))
          (if on?
              (set-add! supports option)
              (set-remove! supports option))))

    (define/public (get-supports)
      (set-copy supports))))


#|

(define terminal%/abstract
  (class* object% (terminal<%>)
    (super-new)
    (init raw-connection add-message sema) ; c pointer
    (init-field [ip #f])
    
    (define connection-semaphore sema)
    
    (define p:conn raw-connection)
    (define p:add-message add-message)
    (define p:connected #t)
   
    (define/public (connected?) p:connected)
    
    
    (define dimensions:naws #f)
    (define dimensions:default '(80 . 24))
    (define dimensions:override #f)
    (define/public (dimensions)
      (or dimensions:override dimensions:naws dimensions:default))

    (define gmcp (make-hasheq))
    
    (define/public (get-gmcp key)
      (hash-ref gmcp key)) 

    ;; for telnet, these are the env vars
    (define vars (make-hasheq))
    (define/public (get-var var)
      (hash-ref vars var void))

    (define/public (transform-message message)
      (if (string? message)
          (pinkfishx message
                     (cond [(supports? '256-color) '256-color]
                           [(supports? 'color) 'color]
                           [else #f]))
          message))
    
    (define/public (transmit . messages)
      (semaphore-wait connection-semaphore)
      (when p:connected
        (with-handlers ([(λ (e) #t)
                         (λ (e)
                           (semaphore-post connection-semaphore)
                           (displayln "ERROR DURING CONNECTION TRANSMIT" (current-error-port))
                           (displayln e (current-error-port))
                           (raise e #t))])
          (apply p:add-message p:conn (map (λ (m) (transform-message m)) messages))))
      (semaphore-post connection-semaphore))
      
    
    (define/pubment (receive message)
      ;; default message handler for any telnet port
      (match message
        [(list 'naws width height) (set! dimensions:naws (cons width height))]
        [(list 'gmcp key val) (hash-set! gmcp (string->symbol key) (string->gmcp val))]
        [(cons 'new-environ env-vars)
         (hash-union! vars env-vars #:combine/key (λ (k a b) b))]
;        [(list 'telnet:options charset primary-id secondary-id options)
;         (supports-union! options)]
        [(or (? eof-object?) #f) (set! p:connected #f) (transmit eof)]
        [else (void)])
      (void (inner #f receive message)))
    ))
|#
