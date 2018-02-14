#lang racket

(provide bytes->string/name string->bytes/name transcode-close get-converter bytes-convert/complete)

(define converter-table (make-hasheq))

(define (transcode-close)
  (hash-for-each converter-table (lambda (s convs)
                                   (bytes-close-converter (car convs))
                                   (bytes-close-converter (cdr convs))))
  (hash-clear! converter-table))

(define (get-converter from? enc)
  (unless (hash-has-key? converter-table enc)
    (define enc/str (symbol->string enc))
    (define convs (cons (bytes-open-converter enc/str "UTF-8")
                     (bytes-open-converter "UTF-8" enc/str)))
    (unless (and (bytes-converter? (car convs))
                 (bytes-converter? (cdr convs)))
      (raise-arguments-error 'get-converter "given encoding not supported on this platform" "encoding" enc))
    (hash-set! converter-table enc convs))
  ((if from? car cdr) (hash-ref converter-table enc)))


(define (bytes-convert/complete bytes converter)
  (let loop ([index 0]
               [acc #""])
      (define-values (new-bytes read state) (bytes-convert converter bytes index))
      (case state
        [(complete) (bytes-append acc new-bytes)]
        [(continues aborts) (bytes-append acc new-bytes (bytes-convert-end converter))]
        [(error) (loop (+ index read 1) (bytes-append acc new-bytes #"?"))])))

(define (bytes->string/name bytes enc)
  (case enc
    [(UTF-8) (bytes->string/utf-8 bytes)]
    [(ASCII ISO-8859-1 LATIN1) (bytes->string/latin-1 bytes)]
    [else 
  (define converter (get-converter #t enc))
  (define
    new-bytes
    (let loop ([index 0]
               [acc #""])
      (define-values (new-bytes read state) (bytes-convert converter bytes index))
      (case state
        [(complete) (bytes-append acc new-bytes)]
        [(continues aborts) (bytes-append acc new-bytes (bytes-convert-end converter))]
        [(error) (loop (+ index read 1) (bytes-append acc new-bytes #"?"))])))
  (bytes->string/utf-8 new-bytes)]))

(define (string->bytes/name str enc)
  (case enc
    [(UTF-8) (string->bytes/utf-8 str)]
    [(ISO-8859-1 LATIN1) (string->bytes/latin-1 str 63)]
    [else 
  (define converter (get-converter #f enc))
  (define bytes (string->bytes/utf-8 str))
  (let loop ([index 0]
             [acc #""])
    (define-values (new-bytes read state) (bytes-convert converter bytes index))
    (case state
      [(complete) (bytes-append acc new-bytes)]
      [(continues aborts) (bytes-append acc new-bytes (bytes-convert-end converter))]
      [(error) (loop (+ index read 1) (bytes-append acc new-bytes #"?"))]))]))
