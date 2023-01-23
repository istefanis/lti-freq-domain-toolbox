#lang info

(define collection 'use-pkg-name)
(define license 'GPL-3.0-only)
(define version "0.5")
(define deps (list "plot-lib"
                   "base"
                   "math-lib"
                   "plot-gui-lib"))
(define build-deps (list "racket-doc"
                         "scribble-lib"))
(define scribblings '(("doc/lti-freq-domain-toolbox.scrbl" ())))