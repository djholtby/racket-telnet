#lang racket
(require racket/class json)
(require "transcode.rkt")

(provide telnet-user telnet-user? telnet-user-cptr telnet-user-settings telnet-user-th set-telnet-user-th! telnet-user-out telnet-user-conn? set-telnet-user-conn?!
         new-connection send-to-user register-print-callback)

; (send-to-user cptr bytes) sends bytes to the C backend telnet struct cptr
; send-to-user: CPTR ByteStr -> Void
(define send-to-user #f)


; (register-print-callback cb) registers native function cb as the send-to-user function
; register-print-callback: (CPTR ByteStr -> Void) -> Void
(define (register-print-callback cb)
  (unless (procedure? cb)
    (raise-argument-error 'register-print-callback "procedure?" cb))
  (unless (procedure-arity-includes? cb 2)
    (raise-argument-error 'register-print-callback "procedure-arity-includes? 2" cb))
  (set! send-to-user cb))

;; supports: (listof Sym)
;; encoding: Sym
;; to-utf8 : Bytes-Converter
;; from-utf8 : Bytes-Converter

(struct telnet-settings (supports encoding to-utf8 from-utf8) #:mutable)

(struct telnet-user
  ( cptr     ; int?              - cptr cast as uintptr_t for speed reasons
    [th #:mutable]      ; 
    settings; telnet-settings?
    out     ; output-port?      - printing to here wraps it up and sends it to C backend
    [conn? #:mutable]    ; boolean?          - is currently connected
    )
  #:property prop:output-port (struct-field-index out))



(define (new-connection cptr)
  (define default-encoding 'ISO-8859-1)
  (define default-settings
    (telnet-settings '() default-encoding (get-converter #f default-encoding) (get-converter #t default-encoding)))
  (telnet-user cptr
               #f
               default-settings
               (make-output-port 'telnet-writer ; name
                                 always-evt ; evt
                                 (lambda (bytes start end non-block? breakable?) ; write-out, never blocks because the channel is non-blocking
                                   (send-to-user cptr (bytes-convert/complete (subbytes bytes start end) (telnet-settings-from-utf8 default-settings)))
                                   (- end start))
                                 void)
               #t
               ))

