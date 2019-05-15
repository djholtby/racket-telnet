#lang racket/base

(require racket/class racket/port racket/bytes racket/list racket/match racket/string racket/set json)
(require "tags.rkt" "connection.rkt" "compressed-ports.rkt" "defconst.rkt" "charset.rkt" "transcode.rkt")

;; todo: split each manager into its own module?
;; idiosynchratic racket style is to jam everything into a monolithic file ;)

(provide telnet-conn% telnet-option-manager% mccp2-manager% gmcp-manager% msdp-manager%
         naws-manager% ttype-manager% charset-manager% mssp-manager%
         encodings->charset-req-sequence) 

(define-constants telnet #:provide
  [iac 255]
  [dont 254]
  [do 253]
  [wont 252]
  [will 251]
  [sb 250]
  [ga 249]
  [el 248]
  [ec 247]
  [ayt 246]
  [ao 245]
  [ip 244]
  [break 243]
  [dm 242]
  [nop 241]
  [se 240]
  [eor 239]
  [abort 238]
  [susp 237]
  [eof 236])

(define (byte->cmd b)
  (or (constant-name 'telnet b) 'invalid))

(define (cmd->byte s)
  (constant-value 'telnet s))

;; Telopt codes
(define-constants telopt #:provide #:define-alias
  [binary 0]
  [echo 1]
  [rcp 2]
  [sga 3]
  [nams 4]
  [status 5]
  [tm 6]
  [rcte 7]
  [naol 8]
  [naop 9]
  [naocrd 10]
  [naohts 11]
  [naohtd 12]
  [naoffd 13]
  [xascii 17]
  [logout 18]
  [bm 19]
  [def 20]
  [supdup 21]
  [supdupoutput 22]
  [sndloc 23]
  [ttype 24]
  [eor 25]
  [outmrk 27]
  [ttyloc 28]
  [3270regime 29]
  [x3-pad 30]
  [naws 31]
  [tspeed 32]
  [lflow 33]
  [linemode 34]
  [xdisploc 35]
  [environ 36]
  [authentication 37]
  [encrypt 38]
  [new-environ 39]
  [starttls 46]
  [mssp 70]
  [compress mccp 85]
  [compress2 mccp2 86]
  [zmp 93]
  [exopl 255]
  [charset 42]
  [mxp 91]
  [gmcp 201]
  [msdp 69])
          
(define telopt-list
  (apply seteq (get-constant-names 'telopt)))

(define (telopt? sym)
  (set-member? telopt-list sym))

; Protocol specific bytes

(define-constants ttype
  [is 0]
  [send 1])

(define-constants charset
  [request 1]
  [accepted 2]
  [rejected 3]
  [ttable-is 4]
  [ttable-rejected 5]
  [ttable-ack 6]
  [ttable-nak 7])


(define-constants environ-command
  [is 0]
  [send 1]
  [info 2])

(define-constants environ-value
  [var 0]
  [value 1]
  [esc 2]
  [uservar 3])

(define-constants mssp
  [var 1]
  [val 2])


(define-constants msdp
  [var 1]
  [val 2]
  [table-open 3]
  [table-close 4]
  [array-open 5]
  [array-close 6])

#|
(define FLAG-PROXY 1)

(define PFLAG-DEFLATE 128)

(define ENCRYPT-IS 0)
(define ENCRYPT-SUPPORT 1)
(define ENCRYPT-REPLY 2)
(define ENCRYPT-START 3)
(define ENCRYPT-END 4)
(define ENCRYPT-REQUEST-START 5)
(define ENCRYPT-REQUEST-END 6)
(define ENCRYPT-ENC-KEYID 7)
(define ENCRYPT-DEC-KEYID 8)

(define ENCRYPT-SUP-NULL 0)
(define ENCRYPT-SUP-DES-CFB64 1)
(define ENCRYPT-SUP-DES-OFB64 2)
(define ENCRYPT-SUP-DES3-CFB64 3)
(define ENCRYPT-SUP-DES3-OFB64 4)
(define ENCRYPT-SUP-CAST5-40-CFB64 8)
(define ENCRYPT-SUP-CAST5-40-OFB64 9)
(define ENCRYPT-SUP-CAST128-CFB64 10)
(define ENCRYPT-SUP-CAST128-OFB64 11)
|#

(struct wrapped-input-port (in old-port) #:property prop:input-port (struct-field-index in))
(struct wrapped-output-port (out old-port) #:property prop:output-port (struct-field-index out))

(define (telopt-q-state? s)
  (memq s (list 'no 'want-no 'want-no/opp 'yes 'want-yes 'want-yes/opp)))






(define telnet-option-manager%
  (class object%
    (super-new)
    (init-field owner)

    (define to -1) 
    (define telopt-name 'undefined)

    (define/public (set-telopt telopt)
      (set! to telopt)
      (set! telopt-name (constant-name 'telopt telopt)))

    (define/public (telopt/byte) to)
    (define/public (telopt/symbol) telopt-name) 
    
    (define/public (send-subneg . args)
      (if (and (cons? args) (bytes? (car args)))
          (car args)
          (error 'telnet-option-manager%:send-subneg "telnet-option-manager does not support this type of subnegotiation: ~v" args)))

    ;; default way to translate a subnegotation buffer (tb) to a Racket message 
    (define/public (receive-subneg tb)
      (list telopt-name tb))

    ;; override if the telopt has a state that needs to be reset when its activated, or has a startup procedure
    ;;  (e.g. WILL compress2 should be followed by a compression marker and then a switch to compressed stream,
    ;;        DO   compress2 does nothing (it needs to wait on the marker).
    (define/public (on-enable where) (void))

    ;; override if the telopt has a teardown procedure 
    (define/public (on-disable where) (void))))

(define mccp2-manager%
  (class telnet-option-manager%

    (super-new)
    (inherit-field owner)
    (send this set-telopt telopt:compress2)
    

    (define/override (on-enable where)
      (when (eq? where 'local) ; we've been asked to compress
        (send owner start-compress-output!))
      ; if the remote side wants to compress, we need to wait for the marker
      )

    (define/override (receive-subneg bs)
      ;; This is the "compression start" marker
      (send owner start-compress-input!)
      'compress-start)
    
    (define/override (on-disable where)
      ;; about all we can do for remote is send them the dont and wait for them to send Z-STREAM-DONE
      (when (eq? where 'local)
        (send owner stop-compress-output!))) ; this makes US send THEM Z-STREAM-DONE
    ))


(define (string->gmcp s) ; some clients send invalid JSON because they're jerks
  (if s (with-handlers ([exn:fail:read? (λ (e) s)]) (string->jsexpr s)) #f))

(define gmcp-manager%
  (class telnet-option-manager%
    (super-new)
    (send this set-telopt telopt:gmcp)

    (define/override (receive-subneg bs)
      (define s (bytes->string/utf-8 bs))
      (define parts (regexp-match #px"^([^ ]+)( (.*))?$" s))
      (define key (second parts))
      (define value (fourth parts))
      (if value
          (list 'gmcp key (string->gmcp value))
          (list 'gmcp key)))

    (define/override (send-subneg key [value #f])
      (string->bytes/utf-8
       (if value (string-append key " " (jsexpr->string value)) key)))))


(define (read-msdp i)
  ; (get-var) reads a variable name from port i
  ; get-var:  -> Sym
  ; requires: next byte in i is MSDP_VAR followed by valid MSDP data
  (define (get-var)
    (define bv (read-byte i))
    (unless (and (not (eof-object? bv)) (= bv msdp:var)) (error 'read-msdp "Expected MSDP_VAR but saw ~v isntead" bv))
    (let loop ([acc (open-output-bytes)])
      (define b (peek-byte i))
      (cond [(eof-object? b) (error 'read-msdp "unexpected EOM reading VAR name")]
            [(memv b (list msdp:table-open msdp:table-close msdp:array-open msdp:array-close msdp:var ))
             (error 'read-msdp "Unexpected value: ~v, acc=~a" b (get-output-string acc))]
            [(= b msdp:val) (string->symbol (bytes->string/utf-8 (get-output-bytes acc)))]
            [else (write-byte (read-byte i) acc) (loop acc)])))
  ; (get-val) reads a MSDP value from port i
  ; get-val: -> MSDP-Value
  ; requires: next byte in i is MSDP_VAL followed by valid MSDP data
  (define (get-val)
    (define bv (read-byte i))
    (unless (and (not (eof-object? bv)) (= bv msdp:val)) (error 'read-msdp "Expected MSDP_VAL but saw ~v instead" bv))
    (define b (peek-byte i))
    (cond [(eof-object? b) (error 'read-msdp "unexpected EOM reading VAL")]
          [(= b msdp:table-open) (get-table)]
          [(= b msdp:array-open) (get-array)]
          [(memv b (list msdp:table-close msdp:array-close msdp:var msdp:val))
           (error 'read-msdp "Expected value, MSDP_TABLE_OPEN, or MSSDP_ARRAY_OPEN, but saw ~v" b)]
          [else (get-atomic)]))
  ; (get-table) reads a MSDP table from port i
  ; get-table: -> (hashof Sym MSDP-Value)
  ; requires: next byte in i is MSDP_OPEN_TABLE followed by valid MSDP data
  (define (get-table)
    (define bv (read-byte i))
    (unless (and (not (eof-object? bv)) (= bv msdp:table-open)) (error 'read-msdp "Expected MSDP_TABLE_OPEN but saw ~v instead" bv))
    (let loop ([acc empty])
      (define b (peek-byte i))
      (cond [(eof-object? b) (error 'read-msdp "unexpected EOM reading table")]
            [(= b msdp:var) (loop (cons (read-msdp i) acc))]
            [(= b msdp:table-close)
             (read-byte i) ; chomp the table_close
             (make-immutable-hasheq acc)] 
            [else (error 'read-msdp "Expected additional MSDP_VAR or MSDP_TABLE_CLOSE while reading table, but saw ~v, acc = ~v" b acc)])))
  ; (get-array) reads a MSDP array from port i
  ; get-array: -> (listof MSDP-Value)
  ; requires: next byte in i is MSDP_OPEN_ARRAY followed by valid MSDP data
  (define (get-array)
    (define bv (read-byte i))
    (unless (and (not (eof-object? bv)) (= bv msdp:array-open)) (error 'read-msdp "Expected MSDP_ARRAY_OPEN but saw ~v instead" bv))
    (let loop ([acc empty])
      (define b (peek-byte i))
      (cond [(eof-object? b) (error 'read-msdp "unexpected EOM reading array")]
            [(= b msdp:val) (loop (cons (get-val) acc))]
            [(= b msdp:array-close)
             (read-byte i) ; chomp the array_close
             (reverse acc)]
            [else (error 'read-msdp "Expected additional MSDP_VAL or MSDP_ARRAY_CLOSE while reading array, but saw ~v" b)])))
  ; (get-atomic) reads an atomic MSDP value
  ; get-atomic: -> (anyof Num Bool Str)
  ; requires: next byte in i is NOT MSDP_ARRAY_OPEN or MSDP_TABLE_OPEN
  (define (get-atomic)
    (let loop ([acc (open-output-bytes)])
      (define b (peek-byte i))
      (cond [(memv b (list eof msdp:var msdp:val msdp:array-close msdp:table-close)) (string->value (get-output-string acc))]
            [(memv b (list msdp:array-open msdp:table-open))
             (error 'read-msdp "Unexpected MSDP_~a_OPEN token while reading string" (if (= b msdp:array-open) "ARRAY" "TABLE"))]
            [else (write-byte (read-byte i) acc) (loop acc)])))

  (cons (get-var) (get-val)))


(define (string->value s)
  (define n (string->number s))
  (cond [n n]
        [(string=? s "true") #t]
        [(string=? s "false") #f]
        [else s]))
        
    

(define (bytes->msdp bytes)
  (define i (open-input-bytes bytes))
  (define result (read-msdp i))
  (close-input-port i)
  result)


(define (write-msdp* msdp-pair o)
  (define (write-msdp*/val val)
    (write-byte msdp:val o)
    (cond [(hash? val)
           (write-byte msdp:table-open o)
           (hash-for-each val (λ (var val) (write-msdp* (cons var val) o)))
           (write-byte msdp:table-close o)]
          [(list? val)
           (write-byte msdp:array-open o)
           (for-each (λ (val) (write-msdp*/val val)) val)
           (write-byte msdp:array-close o)]
          [(boolean? val)
           (write-bytes (string->bytes/utf-8 (if val "true" "false")) o)]
          [(string? val)
           (write-bytes (string->bytes/utf-8 val) o)]
          [(number? val)
           (write-bytes (string->bytes/utf-8 (number->string val)) o)]))
  (define val (cdr msdp-pair))
  (write-byte msdp:var o)
  (write-bytes (string->bytes/utf-8 (symbol->string (car msdp-pair))) o)
  (write-msdp*/val val))

(define (write-msdp msdp-pair o)
  (unless (and (cons? msdp-pair)
               (symbol? (car msdp-pair))
               (jsexpr? (cdr msdp-pair)))
    (raise-argument-error 'msdp->bytes "msdp-pair?" msdp-pair))
  (write-msdp* msdp-pair o))

(define (msdp->bytes msdp-pair)
  (unless (and (cons? msdp-pair)
               (symbol? (car msdp-pair))
               (jsexpr? (cdr msdp-pair)))
    (raise-argument-error 'msdp->bytes "msdp-pair?" msdp-pair))
  (define result (open-output-bytes))
  (write-msdp* msdp-pair result)
  (get-output-bytes result))


(define msdp-manager%
  (class telnet-option-manager%
    (super-new)

    (send this set-telopt telopt:msdp)

    (define/override (receive-subneg bs)
      (list 'msdp
            (bytes->msdp bs)))

    (define/override (send-subneg jsexpr)
      (msdp->bytes jsexpr))))

(define naws-manager%
  (class telnet-option-manager%
    (super-new)
    (send this set-telopt telopt:naws)
    (inherit-field owner)
    
    (define/override (receive-subneg bs)
      (define width (integer-bytes->integer bs #f #t 0 2))
      (define height (integer-bytes->integer bs #f #t 2 4))
        (send owner set-dimensions! width height)
        (list 'naws width height))

    (define/override (send-subneg width height)
      (define resp (make-bytes 4))
      (integer->integer-bytes width 2 #f #t resp 0)
      (integer->integer-bytes height 2 #f #t resp 2)
      resp)))

(define ttype-manager%
  (class telnet-option-manager%
    (init wrapped-owner)
    (super-new [owner wrapped-owner])
    (init [terminal-types '("ANSI")])
    (inherit-field owner)
    (send this set-telopt telopt:ttype)
    
    (define local-types
      (if (vector? terminal-types) terminal-types (list->vector terminal-types)))
    (define remote-types '())
    (define local-index 0)

    (define (next-terminal!)
      (when (< local-index (vector-length local-types))
        (set! local-index (add1 local-index)))
      (vector-ref local-types (sub1 local-index)))
        
    (define/override (on-enable where)
      (when (eq? where 'remote)
        (send owner send-subnegotiate telopt:ttype ttype:send #:raw? #t)))

    (define/override (receive-subneg bs)
      (if (= ttype:send (bytes-ref bs 0))
          (begin
            (send owner send-subnegotiate telopt:ttype (bytes-append (list->bytes (list ttype:is))
                                                                     (string->bytes/utf-8 (next-terminal!))) #:raw? #t)
            #f)
          (let ([terminal-name (bytes->string/utf-8 bs #\? 1)])
            (if (or (null? remote-types)
                     (not (string=? (first remote-types) terminal-name)))
                (begin
                  (set! remote-types (cons terminal-name remote-types))
                  (send owner send-subnegotiate telopt:ttype ttype:send #:raw? #t)
                  #f)
                (list 'ttype (reverse remote-types))))))))

(define (encodings->charset-req-sequence loe)
  (define out (open-output-bytes))
  (write-byte charset:request out)
  (write-bytes (string->bytes/utf-8 (string-join (map symbol->string loe) ":")) out)
  (get-output-bytes out #t))


;; (pick-first-common our-prefs their-options) returns the first item from our-prefs that is a member of their-options (according to equal?)
;;    or #f if none of our-prefs is a member.
;; pick-first-common: (listof Sym) (listof Str) -> (U (cons Sym Str) #f)

(define (pick-first-common our-prefs their-options)
  (define options (make-hasheq (map (lambda (str)
                                      (cons (encoding->symbol str) str))
                                    their-options)))
  (let loop ([lst our-prefs])
    (cond [(empty? lst) #f]
          [(hash-has-key? options (first lst))
           (cons (first lst) (hash-ref options (first lst)))]
          [else (loop (rest lst))])))

(define charset-manager%
  (class telnet-option-manager%
    (init wrapped-owner)
    (super-new [owner wrapped-owner])
    (send this set-telopt telopt:charset)
    (init encodings [request-sequence #f])
    (define enc encodings)
    (define rs (if request-sequence request-sequence (encodings->charset-req-sequence encodings)))
    (inherit-field owner)
    (define/override (on-enable where)
      (when (eq? where 'local)
        (send owner send-subnegotiate telopt:charset rs)))

    ;; todo: double check that local and remote aren't backwards!!!
    (define/override (receive-subneg bs)
      (define option (bytes-ref bs 0))
      (cond [(= option charset:rejected)
             (send owner set-remote-charset "us-ascii")]
            [(= option charset:accepted)
             (send owner set-remote-charset (bytes->string/utf-8 bs #\? 1))]
            [(= option charset:request)
             (define remote-encodings (string-split (bytes->string/utf-8 bs #\? 1) ":"))
             ;(displayln (format "Remote repports supprt for these charsets : ~a" remote-encodings) (current-error-port))
             (define chosen-one (pick-first-common enc remote-encodings))
             (when chosen-one
               (send owner set-output-encoding! (car chosen-one))
               (send owner set-input-encoding! (car chosen-one)))
             (send owner send-subnegotiate telopt:charset chosen-one)
             (list telopt:charset chosen-one)]
            [else (send owner send-subnegotiate telopt:charset #f)
                  (list 'charset #f)] ; reject
            ;; todo: handle translation tables?
            ))

    (define/override (send-subneg enc)
      (define out (open-output-bytes))
      (if enc
          (begin (write-byte charset:accepted out)
                 (write-string (cdr enc) out))
          (write-byte charset:rejected out))
      (get-output-bytes out #t))))
               
(define mssp-manager%
  (class telnet-option-manager%
    (super-new)
    (inherit set-telopt)
    (set-telopt telopt:mssp)
    
    (define/override (send-subneg variables)
      (define out (open-output-bytes))
      (for ([kvp (in-list (cdr variables))])
        (let ([var (car kvp)]
              [vlst (cdr kvp)])
          (write-byte mssp:var out)
          (write-string (if (string? var) var (symbol->string var)) out)
          (for ([v (in-list vlst)])
            (write-byte mssp:val out)
            (write v out))))
      (get-output-bytes out #f))))

#| 
Managers to do
--------------
NEWENVIRON
LINEMODE

Managers to not do (they don't do any subnegs)
--------------
MXP
ECHO (debatable)
BINARY
EOR

|#

(define (escape-iac-and-cr bs)
  (define out (open-output-bytes))
  (for ([b (in-bytes bs)])
    (case b
      [(255) (write-byte 255 out) (write-byte 255 out)]
      [(10) (write-byte 13 out) (write-byte 10 out)]
      [(13) (write-byte 13 out) (write-byte 0 out)]
      [else (write-byte b out)]))
  (get-output-bytes out #t))
 
(define (escape-iac bs)
  (define out (open-output-bytes))
  (for ([b (if (bytes? bs) (in-bytes bs) (in-list (list bs)))])
    (when (= b telnet:iac)
      (write-byte b out))
    (write-byte b out))
  (get-output-bytes out #t))



(define-struct telopt-state (allow-local? allow-remote? [us #:mutable] [them #:mutable]) #:transparent)
;; A TQ is a (U 'yes 'no 'want-yes 'want-yes/opposite 'want-no 'want-no/opposite)
;; A Telopt-State (TS) is a (telopt-state Bool Bool TQ TQ)

(define (set-telopt-settings! tab telopt allow-local? allow-remote? us them)
  (hash-set! tab (if (byte? telopt) telopt (constant-value 'telopt telopt))
             (telopt-state allow-local? allow-remote? us them)))

(define (list->telopt-settings lots)
  (foldl (lambda (ts tab)
           (match ts
             [(list telopt allow-local? allow-remote? us them)
              (set-telopt-settings! tab telopt allow-local? allow-remote? us them)]
             [(list telopt allow? us them)
              (set-telopt-settings! tab telopt allow? allow? us them)]
             [(list telopt allow-local? allow-remote?)
              (set-telopt-settings! tab telopt allow-local? allow-remote? 'no 'no)]
             [(list telopt allow?)
              (set-telopt-settings! tab telopt allow? allow? 'no 'no)]
             [(list telopt)
              (set-telopt-settings! tab telopt #t #t 'no 'no)]
             [telopt
              (set-telopt-settings! tab telopt #t #t 'no 'no)])
           tab)
         (make-hasheqv) lots))

(define (list->managers loom owner)
  (foldl (lambda (man tab)
           (if (is-a? man telnet-option-manager%)
               (hash-set! tab (send man telopt/byte) man)
               (if (and (cons? man)
                        (subclass? (car man) telnet-option-manager%))
                   (let ([mgr (apply make-object (car man) owner (cdr man))])
                     (hash-set! tab (send mgr telopt/byte) mgr))
                   (error 'list->managers "invalid entry in telopt manager init list" man)))
           tab)
         (make-hasheqv) loom))

(define (try-close-input-port port)
  (with-handlers ([exn? (λ (e) (void))])
    (close-input-port port)))

(define (try-close-output-port port)
  (with-handlers ([exn? (λ (e) (void))])
    (close-output-port port)))

(define telnet-conn%
  (class* terminal% (terminal<%>)
    (super-new)
    
    (init-field in out)
    
    (init [telopts '()])
    (init [option-managers '()])
    (init [buffer-size 1024])

    (define input-encoding 'ASCII)
    (define output-encoding 'ASCII)
    (define connected #t)

    (define/override (connected?) connected)
    
    (define/override (get-encoding) input-encoding)
    (define/override (set-encoding! enc)
      (set! input-encoding enc)
      (set! output-encoding enc))

    ;; in theory we can use different encodings, in practice why would a client want to receive a different encoding than it sends?
    
    (define/public (get-input-encoding)
      input-encoding)

    (define/public (get-output-encoding)
      output-encoding)

    
    (define/public (set-input-encoding! enc)
      (set! input-encoding enc))

    (define/public (set-output-encoding! enc)
      (set! output-encoding enc))

    (define (transcode-input bs)
      (bytes->string/name bs (if (telopt-enabled? telopt:binary) input-encoding 'ASCII)))

    (define (transcode-output s)
      (if (string? s) (string->bytes/name s (if (telopt-enabled? telopt:binary) output-encoding 'ASCII)) s))
    
    (define telopt-managers (list->managers option-managers this))
    (define telopt-settings (list->telopt-settings telopts))
    (define bs buffer-size)
    (define sema (make-semaphore 1))
    (define sb-telopt #f)
    (define state 'data)
    (define saw/r? #f)
    (define input-buffer  (open-output-bytes))
    (define output-buffer (open-output-bytes))
    (define subneg-buffer (open-output-bytes))  
    (inherit receive)
    (inherit-field markup-settings)
    
    (define/public (flush-in)
      (receive (transcode-input (get-output-bytes input-buffer #t))))

    (define/public (flush-out)
      (write-bytes (get-output-bytes output-buffer #t) out)
      (flush-output out))

    
    (define (send-bytes #:flush? [flush? #t] . args )
      (with-handlers ([exn:fail:network? (λ (e)
                                           (on-close))])
        (for ([b (in-list args)])
          (if (byte? b)
              (write-byte b output-buffer)
              (write-bytes b output-buffer)))
        (when flush?
          (flush-out))))
    
    (define/public (start-compress-input!)
      (if (zstream-input-port? in)
          (log-warning "compression already enabled")
          (if (wrapped-input-port? in)
              (set! in (open-zstream-input-port (wrapped-input-port-old-port in) bs #:remnant (port->bytes in)))
              (set! in (open-zstream-input-port in bs)))))

    (define/public (start-compress-output!)
      (if (zstream-output-port? out)
          (log-warning "output compression already enabled")
          (begin
            ;(displayln "Enabling output compression" (current-error-port))
            (send-subnegotiate telopt:compress2 #"" #:raw? #t)
            (set! out (open-zstream-output-port out bs)))))

    (define/public (stop-compress-output!)
      (if (zstream-output-port? out)
          (begin
            ((zstream-output-port-stop out))
            (close-output-port out)
            (set! out (zstream-output-port-old-port out)))
          (log-warning "output compression not currently active")))
    
#|    (define input-byte-buffer (make-bytes buffer-length))
    (define input-byte-count 0)

    (define (refill-buffer)

      (define amnt (with-handlers ([exn:fail? (λ (e) eof)])
                     (read-bytes-avail! input-byte-buffer in)))
      (cond [(and (eof-object? amnt)
                  (zstream-input-port? in)
                  (bytes? (get-zsteam-input-port-remains in)))
             (close-input-port in)
             (bytes-copy! input-byte-buffer 0 (get-zstream-input-port-remains in) 0)
             (set! input-byte-count (bytes-length (bytes-length (get-zstream-input-port-remains in))))]
            [(eof-object? amnt) amnt]
            [else (set! input-byte-count amnt) amnt]))
    |#

    (define terminal-dimensions '(80 . 24))
    (define/override (dimensions)
      terminal-dimensions)

    (define/public (set-dimensions! width height)
      (set! terminal-dimensions (cons width height)))
    
    (define (next-byte)
      (define b (with-handlers ([exn? (λ (e) eof)])
                  (read-byte in)))
      (cond [(and (eof-object? b)
                  (zstream-input-port? in)
                  (bytes? (get-zstream-input-port-remains in)))
             (close-input-port in)
             (set! in (wrapped-input-port (open-input-bytes (get-zstream-input-port-remains in))
                                          (zstream-input-port-old-port in)))
             (next-byte)]
            [(and (eof-object? b)
                  (wrapped-input-port? in))
             (set! in (wrapped-input-port-old-port in))
             (next-byte)]
            [else b]))

    (define (accumulate b)
      (if saw/r?
          (case b
            [(0)
             (write-byte 13 input-buffer)
             (set! saw/r? #f)]
            [(10)
             ;(write-byte 10 input-buffer)
             (set! saw/r? #f)
             (flush-in)]
            [else
             (write-byte 13 input-buffer)
             (write-byte b input-buffer)
             (log-info (format "malformed telnet byte sequence \\r\\u~a" b))
             (set! saw/r? #f)])
          (if (= b 13) (set! saw/r? #t) (write-byte b input-buffer))))

    (define (receive-negotiate telopt)
      (define ts (hash-ref! telopt-settings telopt (lambda () (telopt-state #f #f 'no 'no))))
      (define tm (hash-ref telopt-managers telopt #f))
      (case state
        [(do)
         (case (telopt-state-us ts)
           [(no) (if (telopt-state-allow-local? ts)
                     (begin
                       (send-negotiate telnet:will telopt)
                       (set-telopt-state-us! ts 'yes)
                       (when tm (send tm on-enable 'local))
                       (receive `(enable local ,(constant-name 'telopt telopt))))
                     (send-negotiate telnet:wont telopt))]
           [(yes) (void)]
           [(want-no)
            (when tm (send tm on-disable 'local))
            (receive `(disable local ,(constant-name 'telopt telopt)))
            (set-telopt-state-us! ts 'no)]
           [(want-no/opposite want-yes)
            (receive `(enable local ,(constant-name 'telopt telopt)))
            (set-telopt-state-us! ts 'yes)
            (when tm (send tm on-enable 'local))]
           [(want-yes/opposite) (set-telopt-state-us! ts 'want-no) (send-negotiate telnet:wont telopt)])]
        [(dont)
         (case (telopt-state-us ts)
           [(no) (void)]
           [(yes)
            (set-telopt-state-us! ts 'no)
            (when tm (send tm on-disable 'local))
            (receive `(disable local ,(constant-name 'telopt telopt)))
            (send-negotiate telnet:wont telopt)]
           [(want-no)
            (set-telopt-state-us! ts 'no)
            (when tm (send tm on-disable 'local))
            (receive  `(disable local ,(constant-name 'telopt telopt)))]
           [(want-no/opposite)
            (set-telopt-state-us! ts 'want-yes)
            (send-negotiate telnet:will telopt)]
           [(want-yes want-yes/opposite)
            (set-telopt-state-us! ts 'no)])]
        [(will)
         (case (telopt-state-them ts)
           [(no) (if (telopt-state-allow-remote? ts)
                     (begin
                       (set-telopt-state-them! ts 'yes)
                       (when tm (send tm on-enable 'remote))
                       (receive  `(enable remote ,(constant-name 'telopt telopt)))
                       (send-negotiate telnet:do telopt))
                     (begin
                       (send-negotiate telnet:dont telopt)))]
           [(yes) (void)]
           [(want-no)
            (set-telopt-state-them! 'no)
            (when tm (send tm on-disable 'remote))
            (receive `(disable remote ,(constant-name 'telopt telopt)))]
           [(want-no/opposite)
            (set-telopt-state-them! ts 'yes)
            (receive `(enable remote ,(constant-name 'telopt telopt)))
            (when tm (send tm on-enable 'remote))]
           [(want-yes)
            (set-telopt-state-them! ts 'yes)
            (receive `(enable remote ,(constant-name 'telopt telopt)))
            (when tm (send tm on-enable 'remote))]
           [(want-yes/opposite)
            (set-telopt-state-them! ts 'want-no)
            (send-negotiate telnet:dont telopt)])]
        [(wont)
         (case (telopt-state-them ts)
           [(no) (void)]
           [(yes)
            (set-telopt-state-them! ts 'no)
            (when tm (send tm on-disable 'remote))
            (receive `(disable remote ,(constant-name 'telopt telopt)))
            (send-negotiate telnet:dont telopt)]
           [(want-no)
            (set-telopt-state-them! ts 'no)
            (receive `(disable remote ,(constant-name 'telopt telopt)))
            (when tm (send tm on-disable 'remote))]
           [(want-no/opposite)
            (set-telopt-state-them! ts 'want-yes)
            (send-negotiate telnet:do telopt)]
           [(want-yes want-yes/opposite)
            (set-telopt-state-them! ts 'no)])]))
  
    (define (send-negotiate neg telopt #:flush? [flush? #t])
      (define neg-byte (if (byte? neg) neg
                           (constant-value 'telnet neg)))
      (define telopt-byte (if (byte? telopt) telopt
                              (constant-value 'telopt telopt)))
;      (displayln (format "Sending negotiate sequence ~a ~a" (constant-name 'telnet neg-byte) (constant-name 'telopt telopt-byte))
;                 (current-error-port))
      (if (and neg-byte telopt-byte)
          (send-bytes telnet:iac neg-byte telopt-byte #:flush? flush?)
          (error 'telnet:send-negotiate "invalid negotiate sequence ~v ~v" neg telopt)))
  
    (define/public (telopt-enabled? telopt [where 'either])
      (define ts (hash-ref! telopt-settings telopt (lambda () (telopt-state #f #f 'no 'no))))
      (define us (telopt-state-us ts))
      (define them (telopt-state-them ts))
      (case where
        [(local) (eq? 'yes us)]
        [(remote) (eq? 'yes them)]
        [else (or (eq? 'yes us)
                  (eq? 'yes them))]))

    (define/public (telopt-disabled? telopt [where 'either])
      (define ts (hash-ref! telopt-settings telopt (lambda () (telopt-state #f #f 'no 'no))))
      (define us  (telopt-state-us ts))
      (define them (telopt-state-them ts))
      (case where
        [(local) (eq? 'no us)]
        [(remote) (eq? 'no them)]
        [else (or (eq? 'no us)
                  (eq? 'no them))]))
    
    (define/public (enable-telopt telopt where)
      (define remote? (eq? where 'remote))
      (define ts (hash-ref! telopt-settings telopt (lambda () (telopt-state #f #f 'no 'no))))
      (when (if remote? (telopt-state-allow-remote? ts) (telopt-state-allow-local? ts))
        (if remote?
            (case (telopt-state-them ts)
              [(no)
               (set-telopt-state-them! ts 'want-yes)
               (send-negotiate telnet:do telopt)]
              [(want-no)
               (set-telopt-state-them! ts 'want-no/opposite)]
              [(yes want-yes want-no/opposite) (void)]
              [(want-yes/opposite)
               (set-telopt-state-them! ts 'want-yes)])
            (case (telopt-state-us ts)
              [(no)
               (set-telopt-state-us! ts 'want-yes)
               (send-negotiate telnet:will telopt)]
              [(want-no)
               (set-telopt-state-us! ts 'want-no/opposite)]
              [(yes want-yes want-no/opposite) (void)]
              [(want-yes/opposite)
               (set-telopt-state-us! ts 'want-yes)]))))
    
    (define/public (disable-telopt telopt where)
      (define remote? (eq? where 'remote))
      (define ts (hash-ref! telopt-settings telopt (lambda () (telopt-state #f #f 'no 'no))))
      (if remote?
          (case (telopt-state-them ts)
            [(yes)
             (set-telopt-state-them! ts 'want-no)
             (send-negotiate telnet:dont telopt)]
            [(want-yes)
             (set-telopt-state-them! ts 'want-yes/opposite)]
            [(no want-no want-yes/opposite) (void)]
            [(want-no/opposite)
             (set-telopt-state-them! ts 'want-no)])
          (case (telopt-state-us ts)
            [(yes)
             (set-telopt-state-us! ts 'want-no)
             (send-negotiate telnet:wont telopt)]
            [(want-yes)
             (set-telopt-state-us! ts 'want-yes/opposite)]
            [(no want-no want-yes/opposite) (void)]
            [(want-no/opposite)
             (set-telopt-state-us! ts 'want-no)])))
    
    (define/public (send-subnegotiate telopt #:flush? [flush? #t] #:raw? [raw? #f]. args)
      (if (telopt-enabled? telopt)
          (let* ([manager (hash-ref telopt-managers telopt #f)]
                 [bmsg (escape-iac (if (and (not raw?) manager)
                                       (send manager send-subneg . args)
                                       (first args)))])
            ;(displayln (format "send subnegotiate ~a (~a) - ~v (bytes=~v)" telopt (constant-name 'telopt telopt) args bmsg))
            (send-bytes
             telnet:iac
             telnet:sb
             telopt
             bmsg
             telnet:iac
             telnet:se
             #:flush? flush?))
          (begin
            (displayln (format "cannot subnegotiate with disabled telopt ~a (~a) ~a" telopt (constant-name 'telopt telopt) args) (current-error-port))
            (log-warning 'telnet:send-subnegotiate
                         (format "attempt to subnegotiate with disabled protocol ~a (~a)" telopt (constant-name 'telopt telopt))))))


    
    (define (receive-subnegotiate telopt bytes)
      (define manager (hash-ref telopt-managers telopt #f))
      ;(displayln (format "subneg buffer received ~a: ~v" telopt bytes) (current-error-port))
      (if manager
          (let ([msg (send manager receive-subneg bytes)])
            (when msg (receive msg)))
          (receive (list (constant-name 'telopt telopt) bytes))))

    (define (send-message msg)
      (match msg
        [(? bytes?) (send-bytes (escape-iac-and-cr msg)) #t]
        [(? string?) (send-bytes (escape-iac-and-cr (transcode-output msg))) #t]
        [(list 'text contents ...)
         (send-bytes (escape-iac-and-cr (transcode-output (xexpr->telnet msg markup-settings)))) #t]
        [(list (and telopt (? telopt?)) args ...)
         (send this send-subnegotiate (constant-value 'telopt telopt) #:flush? #t . args) #t]
        [(or #f (? eof-object?)) (on-close) (receive eof) #f]
        ['echo
         (disable-telopt telopt:echo 'local) #t]
        ['noecho
         (enable-telopt telopt:echo 'local) #t]
        ['prompt
         (cond
           [(telopt-enabled? telopt:eor)
            (send-bytes telnet:iac telnet:eor)]
           [(telopt-disabled? telopt:sga)
            (send-bytes telnet:iac telnet:ga)]) #t]
        ; todo - other iac commands?
        [else (log-warning (format "msg not handled yet ~v" msg)) #t]))

    (define (on-close)
      (set! connected #f)
      (when (zstream-input-port? in)
        (try-close-input-port (zstream-input-port-old-port in)))
      (when (wrapped-input-port? in)
        (try-close-input-port (zstream-output-port-old-port in)))
      (when (zstream-output-port? out)
        (try-close-output-port (zstream-output-port-old-port out)))
      (try-close-input-port in)
      (try-close-output-port out)
      (define old-thread connection-thread)
      (set! connection-thread #f)
      
      (when old-thread (kill-thread old-thread))
      )
    
    (define connection-thread
      (thread
       (lambda ()
         (let loop ()
           (if (port? (sync (thread-receive-evt) in))
               (let ([b (next-byte)])
                 (if (eof-object? b)
                     (begin
                       (receive b)
                       (on-close))
                     (begin
                       (case state
                         [(data) (if (= b telnet:iac) (set! state 'iac) (accumulate b))]
                         [(iac)
                          (define c (byte->cmd b))
                          (case c
                            [(sb will wont do dont) (set! state c)]
                            [(iac) (set! state 'data) (accumulate b)]
                            [else (set! state 'data) (receive c)])]
                         [(will wont do dont)
                          (receive-negotiate b)
                          (set! state 'data)
                          ]
                         [(sb)
                          (set! sb-telopt b)
                          (set! state 'sb+telopt)]
                         [(sb+telopt)
                          (if (= b telnet:iac) (set! state 'sb+telopt+iac) (write-byte b subneg-buffer))]
                         [(sb+telopt+iac)
                          (if (= b telnet:iac)
                              (begin (set! state 'sb+telopt) (write-byte b subneg-buffer))
                              (if (= b telnet:se)
                                  (begin
                                    (receive-subnegotiate sb-telopt (get-output-bytes subneg-buffer #t))
                                    (set! state 'data))
                                  (begin
                                    (log-warning 'telnet "invalid iac during subnegotiation")
                                    (receive-subnegotiate sb-telopt (get-output-bytes subneg-buffer #t))
                                    (set! state 'data))))])
                       (loop))))
               (if (send-message (thread-receive))
                   (loop)
                   (void))
                ))
         (on-close))))
       
    
    (define/override (transmit . args)
      (for ([msg (in-list args)])
          (when (and connected connection-thread (thread-running? connection-thread))
            (thread-send connection-thread msg))))))
              

