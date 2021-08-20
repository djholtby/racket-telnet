#lang racket/base

(require racket/class)
(provide (all-defined-out))

(define start-compress-input!/k (generate-member-key))
(define start-compress-output!/k (generate-member-key))
(define stop-compress-input!/k (generate-member-key))
(define stop-compress-output!/k (generate-member-key))
