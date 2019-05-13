#lang typed/racket
(require typed/racket/class typed/racket/draw)

(provide ansi-color-table)


(: ansi-color-table (HashTable Byte (Instance Color%)))
(define ansi-color-table (make-hasheqv (list
                                        (cons  0 (make-object color% 0 0 0))
                                        (cons  1 (make-object color% 170 0 0))
                                        (cons  2 (make-object color% 0 170 0))
                                        (cons  3 (make-object color% 170 85 0))
                                        (cons  4 (make-object color% 0 0 170))
                                        (cons  5 (make-object color% 170 0 170))
                                        (cons  6 (make-object color% 0 170 170))
                                        (cons  7 (make-object color% 170 170 170))

                                        (cons  8 (make-object color% 255 85 85))
                                        (cons  9 (make-object color% 85 255 85))
                                        (cons 10 (make-object color% 255 255 85))
                                        (cons 11 (make-object color% 85 85 255))
                                        (cons 12 (make-object color% 255 255 85))
                                        (cons 13 (make-object color% 255 85 255))
                                        (cons 14 (make-object color% 85 255 255))
                                        (cons 15 (make-object color% 255 255 255)))))

(for* ([r : Exact-Nonnegative-Integer (in-range 6)]
       [g : Exact-Nonnegative-Integer (in-range 6)]
       [b : Exact-Nonnegative-Integer (in-range 6)])
  (let ([indx : Byte (min 255 (+ 16 (* r 36) (* g 6) b))]
        [rv : Byte (min 255 (* 51 r))]
        [gv : Byte (min 255 (* 51 g))]
        [bv : Byte (min 255 (* 51 b))])
    (when (and (< rv 256) (< gv 256) (< bv 256) (< indx 256))
      (hash-set! ansi-color-table indx (make-object color% rv gv bv)))))

(for ([g : Exact-Nonnegative-Integer (in-range 24)])
  (let ([indx : Byte (min 255 (+ 232 g))])
    (hash-set! ansi-color-table indx
               (let ([shade : Byte (min 255 (exact-round (* (add1 g) (/ 255 24))))])
                 (make-object color% shade shade shade)))))
