#|
lti-freq-domain-toolbox | Functions for studying LTI (linear time-invariant) dynamical systems 
Copyright (C) 2014-2023  Ioannis Stefanis

This file is part of lti-freq-domain-toolbox.

lti-freq-domain-toolbox is free software: you can redistribute it and/or modify it under the terms of 
the GNU General Public License Version 3 as published by the Free Software Foundation.

lti-freq-domain-toolbox is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
See the GNU General Public License Version 3 for more details.

You should have received a copy of the GNU General Public License Version 3 along with lti-freq-domain-toolbox. 
If not, see <https://www.gnu.org/licenses/>.
|#




#|
Some of the functions in this file have been implemented as demonstrated in:

Structure and Interpretation of Computer Programs second edition. 
The MIT Press with the McGraw-Hill Book Company, 1996
Harold Abelson and Gerald Jay Sussman with Julie Sussman, foreword by Alan J. Perlis. 

or are modifications of code presented there.

The book is available under a Creative Commons Attribution-ShareAlike 4.0 International License 
(https://creativecommons.org/licenses/by-sa/4.0/). See: <http://mitpress.mit.edu/sicp>.
|#






#lang racket

(provide (all-defined-out))







; //////////  General math functions  //////////



; SHORTCUT FUNCTIONS

(define (average a b) (/ (+ a b) 2))

(define (square x) (* x x)) 

(define (cube x) (* x x x))

(define (inc n) (+ n 1))

(define (inc-by-2 n) (+ n 2))

(define (double2 a) (* a 2))

(define (halve2 a) (* 0.5 a))

(define (identity x) x)

(define (add-dx x dx) (+ x dx))

;the trig functions cot(phi) and csc(phi)
(define (cot phi) (/ 1.0 (tan phi)))
(define (csc phi) (/ 1.0 (sin phi)))

;list of integers from 0 to x
(define (integers x)
  (if (= x 0)
      (cons 0 '())
      (cons x
            (integers (- x 1)))))





; ROUND DECIMAL

(define (round-decimal x digits)
  
  ;chebyshev:
  (if (not (real? x))
      (make-rectangular (/ (round (* (expt 10 digits) (real-part x))) (expt 10 digits))
                        (/ (round (* (expt 10 digits) (imag-part x))) (expt 10 digits)))
      (let ((number (/ (round (* (expt 10 digits) x)) (expt 10 digits))))
        (if (integer? number)
            (inexact->exact number)
            (exact->inexact number)))))





; EXPONENTATION

#|
(define (expt b n)
  
  (define (expt-iter b counter product)
    (if (= counter 0)
        product
        (expt-iter b
                   (- counter 1)
                   (* b product))))
  
  (expt-iter b n 1))
|#





; SUM

#|
(define (sum-iter term a next b)
  
  (define (sum-iter2 term a next b totsum)
    (if (> a b) 
        totsum
        (sum-iter2 term (next a) next b (+ totsum (term a)))))
  
  (sum-iter2 term a next b 0))
|#





; PRODUCT

#|
(define (product-iter function a next b) 
  (define (product-iter2 a totprod)
    (if (> a b) totprod
        (product-iter2 (next a) (* totprod (function a)))
        )
    )
  (product-iter2 a 1))


(define (pi-approx-iter n)
  (* 8.0 (/ (product-iter square 4 inc-by-2 n) (* n (product-iter square 3 inc-by-2 n)))))

(define (factorial n)
  (product-iter identity 1 inc n))
|#


