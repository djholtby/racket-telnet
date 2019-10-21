#lang info

(define collection 'multi)
(define pkg-name "racket-mud")
(define pkg-authors '(djholtby))
(define version "0.01")
(define deps '("base"
               "data-lib"
               "db-lib"
               "gregor-lib"
               "libuuid"
               "parser-tools-lib"
               "rfc6455"
               "versioned-box"
               "web-server-lib"))
(define build-deps '("racket-doc"
                     "scribble-lib"))
