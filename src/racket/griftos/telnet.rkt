#lang racket/base

(require racket/class racket/list racket/set json ffi/unsafe/atomic)
(require "objects.rkt")
(require "msdp.rkt")
(require "pinkfish.rkt")

(provide telnet-send telnet-send/gmcp telnet-send/msdp telnet-send/prompt telnet-set-echo
         register-print-callback add-message-to-griftos)

(provide telnet telnet? telnet-cptr telnet-encoding set-telnet-encoding! telnet-client set-telnet-client! telnet-term
         set-telnet-term! telnet-supports set-telnet-supports! telnet-supports-union! telnet-supports? telnet-connected? set-telnet-connected?!
         telnet-user-data set-telnet-user-data! telnet-language set-telnet-language! telnet-vars)

;; a TelnetMessage is one of
;; * String
;; * Symbol
;; * (list 'GMCP Bytes)
;; * (list 'MSSP Bytes)
;; * false - disconnect event/request
;; * EOF   - acknowledged disconnect (means telnet object is disposed of)

               
; (send-to-user t message) sends message to the C backend telnet struct cptr
; send-to-user: Telnet TelnetMessage -> Void

(define telnet-send void)

; (register-print-callback cb) registers native function cb as the send-to-user function
; register-print-callback: (CPTR ByteStr -> Void) -> Void
(define (register-print-callback cb)
  (unless (procedure? cb)
    (raise-argument-error 'register-print-callback "procedure?" cb))
  (unless (procedure-arity-includes? cb 2)
    (raise-argument-error 'register-print-callback "(procedure-arity-includes?/c 2)" cb))
  (set! telnet-send
        (lambda (t . msgs)
          (define msgs/ansi (map (λ (msg) (if (string? msg) (pinkfishx msg (telnet-supports t)) msg)) msgs))
          (start-atomic)
          (when (telnet-connected? t)
            (for ([msg/ansi (in-list msgs/ansi)])
              (cb (telnet-cptr t) msg/ansi)))
          (end-atomic))))


(define (telnet-send/prompt t . values)
  (apply telnet-send t values)
  (telnet-send t 'go-ahead))

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


(struct telnet (cptr [on-message #:mutable] [client #:mutable] [term #:mutable] [encoding #:mutable]
                     [supports #:mutable] [connected? #:mutable] [language #:mutable] [user-data #:mutable] [vars #:auto])
  #:auto-value (make-hasheq))

(define (telnet-supports? t option)
  (unless (telnet? t)
    (raise-argument-error 'telnet-supports? "telnet?" t))
  (set-member? (telnet-supports t) option))

(define (telnet-supports-union! t . lol)
  (start-atomic)
  (for* ([settings (in-list lol)]
         [setting (in-list settings)])
    (set-add! (telnet-supports t) setting))
  (end-atomic))

;(define (add-message-to-racket msg tn)
;  (unless (telnet? tn)
;    (raise-argument-error 'add-message-to-racket "telnet?" tn))
;  (unless (telnet-message? msg)
;    (raise-argument-error 'add-message-to-racket "telnet-message?" msg))
;  (define enc (telnet-encoding tn))
;  ((telnet-on-message tn) (transcode-telnet #t msg enc)))




(define (add-message-to-griftos tn msg)
  (unless (telnet? tn)
    (raise-argument-error 'add-message-to-racket "telnet?" tn))
  ((telnet-on-message tn) tn msg))