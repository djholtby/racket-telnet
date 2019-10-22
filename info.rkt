#lang info

(define collection 'multi)
(define pkg-name "rackmud")
(define pkg-authors '(djholtby))
(define scribblings '(("rackmud/scribblings/rackmud.scrbl")))
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
