#lang racket
(require racket/class
         json)

#|-----------------------------------------------------------------------------
 | Racket Telnet interface module
 | 
 |
 |
 |
 |-----------------------------------------------------------------------------|#


;(require native/telnet)
#|-----------------------------------------------------------------------------|
 | Native interface                                                            |
 | A Telnet is a C-Pointer (telnet *)                                          |
 | (native-register telnet socket) registers the native telnet* with the Racket|
 |   socket object (hooks the callbacks so telnet messages are relayed properly|
 |                                                                             |
 | (native-send telnet bytes) sends a bytestring to the given telnet socket    |
 |                                                                             |
 | (native-gmcp telnet key bytes) sends a gmcp message over the given telnet   |
 |    socket, using the bytestring key and JSON message content bytes          |
 |                                                                             |
 | (native-tel-printf telnet fmt ...) sends a C style printf message to telnet |
 |                                                                             |
 | (native-tel-rprintf telnet fmt ...) sends a Racket style printf message     |
 |-----------------------------------------------------------------------------|#




(define telnet-socket% (class object%
  (super-new)
  (init-field native-socket)
;  (link-native-socket native-socket this)
  (field [gmcp (make-hash)]
         [on-gmcp void]
         [on-text void])
  
  (define/public (gmcp-send key value)
    (unless (jsexpr? value) (raise-argument-error 'telnet-socket:gmcp-send "jsexpr?" value))
    (native-gmcp-send native-socket key (jsexpr->bytes value)))
  
  (define/public (gmcp-receive key value)
    (hash-set! gmcp key (bytes->jsexpr value))
    (on-gmcp gmcp))

  (define/public (send msg)
    (unless (or (string? msg) (bytes? msg))
      (raise-argument-error 'telnet-socket:send "(or string? bytes?)" value))
    (native-send native-socket (if (string? msg) (string->bytes/utf-8 msg) msg)))

  (define/public (rec msg)
    (on-msg (bytes->string/utf-8 msg)))

  
  ))
