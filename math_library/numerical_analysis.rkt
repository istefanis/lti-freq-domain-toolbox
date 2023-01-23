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
Parts of this library have been implemented as demonstrated in:

Structure and Interpretation of Computer Programs second edition. 
The MIT Press with the McGraw-Hill Book Company, 1996
Harold Abelson and Gerald Jay Sussman with Julie Sussman, foreword by Alan J. Perlis. 

or are modifications of code presented there.

The book is available under a Creative Commons Attribution-ShareAlike 4.0 International License 
(https://creativecommons.org/licenses/by-sa/4.0/). See: <http://mitpress.mit.edu/sicp>.
|#






#lang racket

;(require "general.rkt")
(provide (all-defined-out))







; //////////  Numerical analysis library  //////////



; ROOT-FINDING METHODS FOR NON-LINEAR FUNCTIONS OF ONE VARIABLE

(define max-loop-counter 0)



; 1. Newton's method (input: a first guess)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  
  (define (try guess)
    (if (eq? f #f)
        #f
        (let ((next (f guess)))
          ;(display next)
          ;(newline)
          (if (or (close-enough? guess next)
                  (> max-loop-counter 5000))
              next
              (if (eqv? +nan.0 next)
                  #f
                  (begin (set! max-loop-counter (+ max-loop-counter 1))
                         (try next)))))))

  (set! max-loop-counter 0)
  (try first-guess))


;(define (average-damp f) (lambda (x) (average x (f x))))

;(define (cube-root x) (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))
;(define (square-root x) (fixed-point (average-damp (lambda (y) (/ x y))) 1.0)) 


(define dx 0.01)

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-function-f g)
  (lambda (x) (if (eq? 0 ((deriv g) x))
                  #f
                  (- x (/ (g x) ((deriv g) x))))))

(define (newton-meth-for-solv-eq g guess)
  (fixed-point (newton-function-f g) guess))




; 2. Half interval or bisection method (input: an interval)

(define e 0.0001)
(define M 10000000)

(define (half-interval-method f a b)
  
  (define (loop an bn N)
    (let ((m (/ (+ an bn) 2)))
      (if (or (> N M) (< (abs (- bn an)) e))
          m
          (if (< (* (f an) (f m)) 0)
              (loop an m (+ N 1))
              (loop m bn (+ N 1))))))
  
  (if (negative? (* (f a) (f b)))
      (begin (set! max-loop-counter 0)
             (loop a b 1))
      (if (> max-loop-counter 10000)
          #f
          (begin (set! max-loop-counter (+ max-loop-counter 1))
                 (half-interval-method f (* 1.01 a) (* 0.99 b))))
      ;(error "Values are not of opposite sign" a b (f a) (f b))
      ))




; 3. Chord method (input: an interval)

#|
(define (chord-method f a b) ;rather slow convergence when f'(root) = 0 
  
  (define (loop an bn N)
    (cond ((= (f an) 0) an)
          ((= (f bn) 0) bn)
          (else 
           (let ((c (/ (- (* an (f bn)) (* bn (f an)))
                       (- (f bn) (f an)))))
             (if (or (> N M) (< (abs (- bn an)) e))
                 c
                 (if (< (* (f an) (f c)) 0)
                     (loop an c (+ N 1))
                     (loop c bn (+ N 1))))))))
  
  (if (negative? (* (f a) (f b)))
      (loop a b 1)
      #f
      ;(error "Values are not of opposite sign" (f a) (f b))
      ))



(define (improved-chord-method f a b)
  
  (define (loop an fa bn fb cn-1 N)
    (cond ((= (f an) 0) an)
          ((= (f bn) 0) bn)
          (else 
           (let ((c (/ (- (* an fb) (* bn fa))
                       (- fb fa))))
             (if (or (> N M) (< (abs (- bn an)) e))
                 c
                 (if (< (* fa (f c)) 0)
                     (if (> (* (f c) (f cn-1)) 0)
                         
                         ; the divisor (here 2) can be lowered for better approximations:
                         (loop an (/ fa 2) c fb c (+ N 1))
                         (loop an fa c fb c (+ N 1)))
                     
                     (if (> (* (f c) (f cn-1)) 0)
                         (loop c fa bn fb c (+ N 1))
                         (loop an fa c (/ fb 2) c (+ N 1)))))))))
  
  (if (negative? (* (f a) (f b)))
      (loop a (f a) b (f b) a 1)
      (error "Values are not of opposite sign" (f a) (f b))))
|#




; Interval methods comparison tests: half-interval-method is chosen

#|
(half-interval-method (λ (x) (* x x x)) -1 1.1)
(chord-method (λ (x) (* x x x)) -1 1.1)
(improved-chord-method (λ (x) (* x x x)) -1 1.1)

(half-interval-method (λ (x) (- x (exp (- 0 x)))) -1 1.1)
|#





; INTEGRATION

#|
(define (integral f a b dx)
  (* (sum-iter f (+ a (/ dx 2.0)) (lambda (x) (+ x dx)) b)
     dx))



(define (integral-simpson f a b n)
  
  (define (sum-iter-custom term a next b totsum n h k)
    (if (> a b) 
        
        (/ (* totsum h) 3.0)
        
        (cond ((or (= k 0) (= k n)) (sum-iter-custom term (next a h) next b (+ totsum (term a)) n h (+ k 1) )) 
              ((= (remainder k 2) 0) (sum-iter-custom term (next a h) next b (+ totsum (* 2(term a))) n h (+ k 1) ))
              (else (sum-iter-custom term (next a h) next b (+ totsum (* 4(term a))) n h (+ k 1) )))))
  
  (sum-iter-custom f a add-dx b 0 n (/ (- b a) n) 0))  
|#



