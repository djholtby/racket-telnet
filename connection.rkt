#lang racket/base
(require racket/class racket/bool racket/list racket/contract racket/set)

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
    [receive  (->m telnet-message?/c void?)]      ; called when the connection receives a message 
    [transmit (->m telnet-message?/c ... void?)]  ; you can call this to send a message back
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
