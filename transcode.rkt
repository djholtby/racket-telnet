#lang racket

(provide bytes->string/name string->bytes/name transcode-close get-converter bytes-convert/complete string-convert/complete)

(define converter-table (make-hasheq))

(define (transcode-close)
  (hash-for-each converter-table (lambda (s convs)
                                   (bytes-close-converter (car convs))
                                   (bytes-close-converter (cdr convs))))
  (hash-clear! converter-table))

(define (get-converter from? enc)
  (unless (hash-has-key? converter-table enc)
    (define enc/str (symbol->string enc))
    (define convs (cons (bytes-open-converter enc/str "UTF-8//TRANSLIT")
                        (bytes-open-converter "UTF-8" (string-append enc/str "//TRANSLIT"))))
    (unless (and (bytes-converter? (car convs))
                 (bytes-converter? (cdr convs)))
      (raise-arguments-error 'get-converter "given encoding not supported on this platform" "encoding" enc))
    (hash-set! converter-table enc convs))
  ((if from? car cdr) (hash-ref converter-table enc)))


(define (bytes-convert/complete bytes converter)
  (let loop ([index 0]
             [acc (open-output-bytes)])
      (define-values (new-bytes read state) (bytes-convert converter bytes index))
      (case state
        [(complete)
         (write-bytes new-bytes acc)
         (get-output-bytes acc #t)]
        [(continues aborts)
         (write-bytes new-bytes acc)
         (write-bytes (bytes-convert-end converter) acc)
         (get-output-bytes acc #t)]
        [(error)
         (write-bytes #"?" acc)
         (loop (+ index read 1) acc)])))



(define (string-convert/complete string converter)
  (bytes-convert/complete (string->bytes/utf-8 string) converter))
  


(define (bytes->string/name bytes enc)
  (case enc
    [(UTF-8 ASCII) (bytes->string/utf-8 bytes)] ; ASCII bytes are also UTF-8 bytes 
    [else 
     (define converter (get-converter #t enc))
     (bytes->string/utf-8 (bytes-convert/complete bytes converter))]))
#|  (define
    new-bytes
    (let loop ([index 0]
               [acc #""])
      (define-values (new-bytes read state) (bytes-convert converter bytes index))
      (case state
        [(complete) (bytes-append acc new-bytes)]
        [(continues aborts) (bytes-append acc new-bytes (bytes-convert-end converter))]
        [(error) (loop (+ index read 1) (bytes-append acc new-bytes #"?"))])))
  (bytes->string/utf-8 new-bytes)]))|#

(define (string->bytes/name str enc)
  (case enc
    [(UTF-8) (string->bytes/utf-8 str)]
    [else 
     (define converter (get-converter #f enc))
     (string-convert/complete str converter)]))
  #|  (define bytes (string->bytes/utf-8 str))
  (let loop ([index 0]
             [acc #""])
    (define-values (new-bytes read state) (bytes-convert converter bytes index))
    (case state
      [(complete) (bytes-append acc new-bytes)]
      [(continues aborts) (bytes-append acc new-bytes (bytes-convert-end converter))]
      [(error) (loop (+ index read 1) (bytes-append acc new-bytes #"?"))]))]))
|#


;(module+ test
;  (require "charset.rkt")
;  (require "defconst.rkt")
;  (define supported (mutable-seteq))
;  (define not-supported (mutable-seteq))
;  (for ([cs-sym (in-list (get-constant-names 'CHARSET #t))])
;    (with-handlers ([exn? (λ (e)
;                            (set-add! not-supported cs-sym))])
;      (let ([converter (get-converter #t cs-sym)])
;        (set-add! supported cs-sym))))
;  not-supported
;  (set-count supported)
;  (set-count not-supported)
;)
      
;(string->bytes/name "Testing testing 123" 'SHIFT-JIS)
