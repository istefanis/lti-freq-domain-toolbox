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


#|
The implementation of the Talbot algorithm for Laplace inversion
was originally demonstrated in Python by F.D. Nieuwveldt and D. Kadelka. See: 
<https://code.activestate.com/recipes/576934/>
<https://code.activestate.com/recipes/576938/>

Reference:
L.N. Trefethen, J.A.C. Weideman, and T. Schmelzer. Talbot quadratures and rational 
approximations. BIT. Numerical Mathematics, 46(3):653 670, 2006.

The original implementations are licensed under the MIT License,
and their licences are included in respective 'math_library\assets' subdirectories.
|#






#lang racket

(require "general.rkt")
(require "assets/complex-polynomial-roots.rkt")
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




; APPROXIMATE ROOT LOCALIZATION

;Receives as input a list of curve points (with each point represented as a list (x f(x)) itself),
;and returns a list containing the intervals in which a curve's roots are to be found
;(with each interval represented as a list itself)
(define (find-curve-root-intervals curve-points) 
  (define (loop points root-intervals)
    (if (eq? (cdr points) '())
        root-intervals
        (if (<= (* (cadr (cadr points)) (cadr (car points))) 0)
            (loop (cdr points) (append root-intervals
                                       (list (list (car (car points)) (car (cadr points))))))
            (loop (cdr points) root-intervals))))

  (loop curve-points '()))




; COMPLEX UNIVARIATE POLYNOMIAL ROOTS COMPUTATION


;Using an implementation of the Weierstrass / Durand-Kerner method,
;compute the complex roots of a polynomial and return them inside a list
(define (find-complex-roots-of-polynomial terms)

  (define (zero-roots-from-polynomial-terms-array terms)
    (let ((zero-roots-counter 0)
          (i 0))
      (for-each (λ(x)
                  (cond ((and (eq? i zero-roots-counter) (eq? x 0))
                         (set! zero-roots-counter (+ zero-roots-counter 1))))
                  (set! i (+ i 1)))
                (reverse terms))   
      zero-roots-counter))

  (let ((complex-roots '())
        (r 0)) 
    (if (eq? (- (length terms) 1) (zero-roots-from-polynomial-terms-array terms))
        ;if all roots are zero, provide a tailored initial guess
        (set! r (find-roots (reverse terms) #f #f #f (build-list (- (length terms) 1) (lambda (x) 0)) #f))
        (set! r (find-roots (reverse terms) #f #f #f #f #f)))
  
    (set! complex-roots (map (λ(x) (make-rectangular (round-decimal (real-part x) 3)
                                                     (round-decimal (imag-part x) 3))) r))
    
    (sort complex-roots (λ(x1 x2) (< (real-part x1) (real-part x2))))))




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




; LAPLACE INVERSION


; Talbot numerical method
(define (talbot-method F t N)
  (when (= t 0) (display "error - t must be positive - TALBOT METHOD"))
  
  (let ((h (/ (* 2 pi) N))
        (shift 0.0) ;contour should be shifted if positive real poles exist
        (c1 0.5017) ;parameters by Weideman (see reference above)
        (c2 0.6407)
        (c3 0.6122)
        (c4 0.2645))
    
    ;evaluation at theta
    (define (loop k ans)
      (if (> k N)
          
          (* (/ h (make-rectangular 0 (* 2 pi))) ans)
          
          (let* ((theta (+ (* -1 pi) (* (+ k 0.5) h)))
                 
                 (z (+ shift (* (/ N t) (+ (* c1 theta (cot (* c2 theta)))
                                           (- c3) 
                                           (make-rectangular 0 (* c4 theta))))))
                 
                 (dz-dtheta (* (/ N t) (+ (* (- c1) c2 theta (expt (csc (* c2 theta)) 2))
                                          (* c1 (cot (* c2 theta)))
                                          (make-rectangular 0 c4)))))
            
            (loop (+ k 1) (+ ans (* (exp (* z t)) (F z) dz-dtheta))))))
    
    (real-part (loop 0 0))))



