#lang info

(define collection 'use-pkg-name)
(define version "0.3")
(define deps (list "plot-lib"
                   "base"
                   "math-lib"
                   "plot-gui-lib"))
(define build-deps (list "racket-doc"
                         "scribble-lib"))
(define scribblings '(("doc/main.scrbl" ())))