#lang racket
(require racket/class json)
(require "objects.rkt")
(require "msdp.rkt")

(provide telnet-message?/c telnet-message? telnet-send telnet-send/gmcp telnet-send/msdp telnet-set-echo
         register-print-callback add-message-to-griftos)

(provide telnet telnet? telnet?/c telnet-cptr telnet-encoding set-telnet-encoding! telnet-client set-telnet-client! telnet-term
         set-telnet-term! telnet-supports set-telnet-supports! telnet-supports? telnet-connected? set-telnet-connected?!
         telnet-user-data set-telnet-user-data!)

;; a telnet message is one of
;; * Bytes
;; * (list 'GMCP Bytes)
;; * (list 'MSSP Bytes)
;; * EOF -- indicates a server-side disconnect request

(define telnet-message?/c (or/c bytes?
                                string?
                                (cons/c symbol? (or/c bytes? (cons/c exact-nonnegative-integer? exact-nonnegative-integer?)))
                                eof-object?))

(define (telnet-message? m)
  (or (bytes? m)
      (string? m)
      (and (cons? m)
           (symbol? (car m))
           (or (bytes? (cdr m))
               (and (cons? cdr m)
                    (exact-nonnegative-integer? (cadr m))
                    (exact-nonnegative-integer? (cddr m)))))
               
      (eof-object? m)))

; (send-to-user t message) sends message to the C backend telnet struct cptr
; send-to-user: Telnet TelnetMessage -> Void

(define telnet-send void)


; (register-print-callback cb) registers native function cb as the send-to-user function
; register-print-callback: (CPTR ByteStr -> Void) -> Void
(define (register-print-callback cb)
  (unless (procedure? cb)
    (raise-argument-error 'register-print-callback "procedure?" cb))
  (unless (procedure-arity-includes? cb 2)
    (raise-argument-error 'register-print-callback "procedure-arity-includes? 2" cb))
  (set! telnet-send
        (lambda (t msg)
          (cb (telnet-cptr t) msg))))

(define (telnet-send/gmcp t key data)
  (unless (telnet? t)
    (raise-argument-error 'telnet-send/gmcp "telnet?" t))
  (unless (string? key)
    (raise-argument-error 'telnet-send/gmcp "string?" key))
  (unless (jsexpr? data)
    (raise-argument-error 'telnet-send/gmcp "jsexpr?" data))
  (telnet-send t (cons 'gmcp (format "~a ~a" key (jsexpr->string data)))))

(define (telnet-send/msdp t msdp-pair)
  (unless (and (cons? msdp-pair)
               (symbol? (car msdp-pair))
               (jsexpr? (cdr msdp-pair)))
    (raise-argument-error 'telnet-send/msdp "msdp?" msdp-pair))
  (telnet-send t (cons 'msdp (msdp->bytes msdp-pair))))

(define (telnet-set-echo t on?)
(unless (telnet? t)
    (raise-argument-error 'telnet-set-echo "telnet?" t))
  (telnet-send t (if on? 'echo 'noecho)))


(struct telnet (cptr [on-message #:mutable] [client #:mutable] [term #:mutable] [encoding #:mutable] [supports #:mutable] [connected? #:mutable] [user-data #:mutable]))
(define telnet?/c (struct/c telnet exact-positive-integer? (-> telnet-message?/c void?)  string? string? symbol? (listof symbol?) boolean? any/c))

(define (telnet-supports? t option)
  (unless (telnet? t)
    (raise-argument-error 'telnet-supports? "telnet?" t))
  (and (memq option (telnet-supports t)) #t))

;(define (add-message-to-racket msg tn)
;  (unless (telnet? tn)
;    (raise-argument-error 'add-message-to-racket "telnet?" tn))
;  (unless (telnet-message? msg)
;    (raise-argument-error 'add-message-to-racket "telnet-message?" msg))
;  (define enc (telnet-encoding tn))
;  ((telnet-on-message tn) (transcode-telnet #t msg enc)))


(define connection-map (make-hasheqv))

(define (telnet-find-object cptr)
  (unless (exact-nonnegative-integer? cptr)
    (raise-argument-error 'telnet-find-object "#<CPTR:user_info_t>" cptr))
  (hash-ref connection-map cptr (λ () #f)))

(define (telnet-establish-connection tn)
  (unless (telnet? tn)
    (raise-argument-error 'establish-connection "telnet?" tn))
  (define cptr (telnet-cptr tn))
  (hash-set! connection-map cptr tn))

(define (telnet-drop-connection tn)
  (unless (telnet? tn)
    (raise-argument-error 'establish-connection "telnet?" tn))
  (telnet-send tn eof)
  (define cptr (telnet-cptr tn))
  (hash-remove! connection-map cptr))

(define (add-message-to-griftos tn msg)
  (unless (telnet? tn)
    (raise-argument-error 'add-message-to-racket "telnet?" tn))
  ((telnet-on-message tn) tn msg))