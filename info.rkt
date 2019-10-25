#lang info

(define collection 'multi)
(define pkg-name "rackmud")
(define pkg-authors '(djholtby))
(define version "0.01")
(define deps '("uuid"
               "base"
               "data-lib"
               "db-lib"
               "gregor-lib"
               "parser-tools-lib"
               "rfc6455"
               "versioned-box"
               "web-server-lib"))
(define build-deps '("racket-doc"
                     "scribble-lib"))
