#lang racket/base

(provide telnet-logger log-telnet-fatal log-telnet-error log-telnet-warning log-telnet-info
         log-telnet-debug)

(define-logger telnet)