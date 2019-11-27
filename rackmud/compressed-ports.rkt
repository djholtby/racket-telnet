#lang racket/base
(require ffi/unsafe ffi/unsafe/define racket/port racket/file (except-in racket/contract ->) (prefix-in c: (only-in racket/contract ->)))

(provide
 (contract-out (open-zstream-input-port (->* (input-port?) (exact-positive-integer? #:remnants (or/c #f bytes?)) input-port?))
               (open-zstream-output-port (->* (output-port?) (exact-positive-integer?) output-port?)))
 zstream-input-port? zstream-output-port? get-zstream-input-port-remains zstream-input-port-old-port zstream-output-port-old-port)

(define-ffi-definer define-zlib (ffi-lib "libz" '("1.2" "1" #f)))

(define-cstruct _z_stream
  ([next_in _bytes]
   [avail_in _uint]
   [total_in _ulong]
   [next_out _bytes]
   [avail_out _uint]
   [total_out _ulong]
   [msg _bytes]
   [state _pointer]
   [alloc_func _pointer]
   [free_func _pointer]
   [opaque _pointer]
   [data_type _int]
   [adler _ulong]
   [reserved _ulong]))
                 

(define-zlib zlibVersion (_fun -> _string/utf-8))

(define ZLIB-VERSION (zlibVersion))
(define Z-DEFAULT-COMPRESSION -1)
(define Z-SYNC-FLUSH 2)
(define Z-FINISH 4)
(define Z-STREAM-END 1)
(define Z-STREAM-OK 0)
;ZLIB-VERSION


(define-zlib deflateInit_ (_fun _z_stream-pointer _int _string/utf-8 _int -> _int ))
(define (deflateInit zstream level)
  (deflateInit_ zstream level ZLIB-VERSION (ctype-sizeof _z_stream)))

(define-zlib deflate (_fun _z_stream-pointer _int -> _int))
(define-zlib deflateEnd (_fun _z_stream-pointer -> _int))

(define-zlib inflateInit_ (_fun _z_stream-pointer _string/utf-8 _int -> _int))
(define (inflateInit zstream)
  (inflateInit_ zstream ZLIB-VERSION (ctype-sizeof _z_stream)))
(define-zlib inflate (_fun _z_stream-pointer _int -> _int))
(define-zlib inflateEnd (_fun _z_stream-pointer -> _int))


;(define-struct zstream-input-port
;  (in
;   zs
;   buffer
;   ))

;; 

(struct zstream-input-port (port old-port unused-buffer raw-ptr) #:property prop:input-port (struct-field-index port))
(define (get-zstream-input-port-remains z)
  (unbox (zstream-input-port-unused-buffer z)))

(define (open-zstream-input-port in [buffer-length 1024] #:remnants [remnants #f])
  (define buffer/raw (malloc _byte buffer-length 'atomic-interior))
  (define buffer (cast buffer/raw _pointer (_bytes o buffer-length)))
  (define p (malloc _z_stream 'atomic-interior))
  (define z (cast p _pointer _z_stream-pointer))
  (define dest-retainer #f)
  (define dest-buffer #f)
  (define rem-ret (if remnants (malloc _byte (bytes-length remnants) remnants 'atomic-interior) #f))
  (define eof? #f)
  (define done-box (box #f))
  
  (set-z_stream-avail_in! z (if remnants (bytes-length remnants) 0))
  (set-z_stream-next_in! z (if remnants (cast rem-ret _pointer _bytes) #f))
  (inflateInit z)
  (zstream-input-port
   (make-input-port 'zstream-input-port
                   (lambda (dest)
                     (set! dest-retainer (malloc _byte (bytes-length dest) 'atomic-interior))
                     (set! dest-buffer (cast dest-retainer _pointer (_bytes o (bytes-length dest))))
                     (set-z_stream-next_out! z dest-buffer)
                     (set-z_stream-avail_out! z (bytes-length dest))
                     (define code (if (positive? (z_stream-avail_in z)) (inflate z Z-SYNC-FLUSH) Z-STREAM-OK)) 
                     (unless (negative? code)
                       (bytes-copy! dest 0 dest-buffer
                                    0 (- (bytes-length dest) (z_stream-avail_out z))))
                     (cond
                       [(negative? code) 
                        (error 'zstream-input-port "zlib:inflate error ~v" code)]
                       [(and (= code Z-STREAM-END)
                             (> (bytes-length dest) (z_stream-avail_out z)))
                        (- (bytes-length dest) (z_stream-avail_out z))]
                       [(= code Z-STREAM-END)
                        (set-box! done-box (subbytes (z_stream-next_in z) 0 (z_stream-avail_in z)))
                        eof]
                       [(zero? (z_stream-avail_out z))
                        ; dest has been filled, but input buffer not exhausted yet.
                        (bytes-length dest)]
                       [eof? eof]
                       [else
                        ; dest wants more bytes, need to refill the input buffer
                        (define amnt (read-bytes-avail!* buffer in))
                        (cond
                          [(eof-object? amnt) (set! eof? #t) (- (bytes-length dest) (z_stream-avail_out z))]
                          [(procedure? amnt) (error 'zstream-input-port:read "unexpected special value")]
                          [(positive? amnt)
                           (set-z_stream-next_in! z buffer)
                           (set-z_stream-avail_in! z amnt)
                           (- (bytes-length dest) (z_stream-avail_out z))]
                          [else (wrap-evt in (λ (x) 0))])]))
                   #f
                   (λ () (inflateEnd z))) in done-box p))
(struct zstream-output-port (port old-port raw-ptr) #:property prop:output-port (struct-field-index port))

(define (open-zstream-output-port out [buffer-length 8192])
  (define buffer/raw (malloc _byte buffer-length 'atomic-interior))
  (define buffer (cast buffer/raw _pointer (_bytes o buffer-length)))
  (define ibuffer/raw #f)
  (define p (malloc _z_stream 'atomic-interior))
  (define z (cast p _pointer _z_stream-pointer))
  (deflateInit z Z-DEFAULT-COMPRESSION)

  (define (write-deflated-bytes src start end)
    (set! ibuffer/raw (malloc _byte (- end start) (ptr-add src start) 'atomic-interior))
    (set-z_stream-next_in! z (cast ibuffer/raw _pointer (_bytes o (- end start))))
    (set-z_stream-avail_in! z (- end start))
    (let loop ()
      (set-z_stream-next_out! z buffer)
      (set-z_stream-avail_out! z buffer-length)
      (deflate z Z-SYNC-FLUSH)
      (write-bytes buffer out 0 (- buffer-length (z_stream-avail_out z)))
      (when (positive? (z_stream-avail_in z)) (loop)))
    (flush-output out)
    (set-z_stream-next_in! z #f)
    (set! ibuffer/raw #f)
    (- end start))

  (define (get-write-evt src start end) ; get-write-evt
    (wrap-evt out
              (λ () (write-deflated-bytes src start end))))
  
  (zstream-output-port
   (make-output-port 'zstream-output-port
                    out ; ready event
                    (λ (src start end non-blocking? enable-breaks?) ; write-out
                      (if non-blocking? (get-write-evt src start end) (write-deflated-bytes src start end)))
                    (λ () ; close-port
                       (let loop ()
                         (set-z_stream-next_out! z buffer)
                         (set-z_stream-avail_out! z buffer-length)
                         (let ([r (deflate z Z-FINISH)])
                           (write-bytes buffer out 0 (- buffer-length (z_stream-avail_out z)))
                           (when (= r Z-STREAM-OK) (loop))))
                      (deflateEnd z)
                      (set! z #f)
                      (set! p #f)
                      (set! buffer #f)
                      (set! buffer/raw #f)
                      (set! ibuffer/raw #f))
                    #f ; write-out-special
                    get-write-evt)
   out
   p))


;; the following test indicates that this code can
;; compress a 3MB text file in 450ms, compared to gzip's 400
;; and decompress it back again in 50ms, compared to gunzip's 50

;(module+ test
;(define my-out (open-output-bytes))
;(define my-zout (open-zstream-output-port my-out))
;(define input (file->bytes "english-cmu.txt"))
;(time (write-bytes-avail* input my-zout))
;(bytes-length (get-output-bytes my-out #t))

;(define input-stream (open-input-bytes input))
;(set! my-zout (open-zstream-output-port my-out 1024))
;(define input-buffer (make-bytes 1024))
;(time (let loop ()
;        (define amnt (read-bytes! input-buffer input-stream))
;        (when (number? amnt)
;          (write-bytes input-buffer my-zout 0 amnt)
;          (loop))))
;(bytes-length (get-output-bytes my-out))

;(define my-in (open-input-bytes (get-output-bytes my-out)))
;(define my-zin (open-zstream-input-port my-in))
;(define my-text (time (port->bytes my-zin)))
;(bytes-length my-text)
;(for ([i (in-range 100)])
;  (displayln (read-line my-zin)))
;  (with-output-to-file "foo.txt" (lambda () (display my-text))))
