

#|
lti-freq-domain-toolbox
Copyright (C) 2014 Ioannis Stefanis <iastefan@outlook.com>

lti-freq-domain-toolbox is distributed under the GNU Lesser General Public License Version 3 (LGPLv3). 
The LGPLv3 license text is included in the file "LICENSE_LESSER.txt".

See http://www.gnu.org/licenses/lgpl-3.0.txt for more information.
|#







#lang racket

(provide (all-defined-out))



(define e 0.0001)
(define M 10000000)







; precision

(define (akriveia)
  
  (define (loop e e-prev)
    (if (not (eq? 1.0 (+ e 1.0)))
        (loop (/ e (* 1000 M)) e)
        e-prev))
  
  (loop 1.0 1.0))





; ROOT-FINDING METHODS

(define max-loop-counter 0)

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




; rather slow convergence when f'(root) = 0 

(define (chord-method f a b)
  
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





; tests:

;(half-interval-method (λ (x) (* x x x)) -1 1.1)
;(chord-method (λ (x) (* x x x)) -1 1.1)
;(improved-chord-method (λ (x) (* x x x)) -1 1.1)


;(half-interval-method (λ (x) (- x (exp (- 0 x)))) -1 1.1)