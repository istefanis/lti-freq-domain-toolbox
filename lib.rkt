

#|
lti-freq-domain-toolbox
Copyright (C) 2014 Ioannis Stefanis <iastefan@outlook.com>

lti-freq-domain-toolbox is distributed under the GNU Lesser General Public License Version 3 (LGPLv3). 
The LGPLv3 license text is included in the file "LICENSE_LESSER.txt".

See http://www.gnu.org/licenses/lgpl-3.0.txt for more information.
|#







#lang racket

(provide (all-defined-out))




(define tolerance 0.00001)


(define (fixed-point f first-guess)
  
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  
  (define (try guess)
    (let ((next (f guess)))
      ;(display next)
      ;(newline)
      (if (close-enough? guess next)
          next
          (if (eqv? +nan.0 next)
              #f
              (try next)))))
  (try first-guess))



;///////

(define (average-damp f)
  (lambda (x) (average x (f x))))



(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))

(define (square-root x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0)) 



;///////

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))  

(define dx 0.01)



;///////

(define (newton-function-f g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newton-meth-for-solv-eq g guess)
  (fixed-point (newton-function-f g) guess))



;///////

(define (fixed-point-abs g transform guess)
  (fixed-point (transform g) guess))



;///////

(define (average a b) (/ (+ a b) 2))


(define (square x) (* x x)) 


(define (cube x) (* x x x))


(define (inc n) (+ n 1))


(define (inc-by-2 n) (+ n 2))


(define (double2 a) (* a 2))


(define (halve2 a) (* 0.5 a))


(define (identity x) x)


(define (pi-approx-iter n)
  (* 8.0 (/ (product-iter square 4 inc-by-2 n) (* n (product-iter square 3 inc-by-2 n)))))


(define (factorial n)
  (product-iter identity 1 inc n))



;///////

; integers from 0 to x:
(define (integers x)
  (if (= x 0)
      (cons 0 '())
      (cons x
            (integers (- x 1)))))



;///////

(define (product-iter function a next b) 
  (define (product-iter2 a totprod)
    (if (> a b) totprod
        (product-iter2 (next a) (* totprod (function a)))
        )
    )
  (product-iter2 a 1))



;///////

(define (prime? n)
  (= n (smallest-divisor n)))



(define (smallest-divisor n)
  (find-divisor n 2))



(define (find-divisor n test-divisor)
  
  (define (next input)
    (if (= input 2) 3 (+ input 2)))
  
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))



(define (divides? a b)
  (= (remainder b a) 0))



(define (relative-prime? a b)
  (= 1 (gcd a b)))



(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))




;///////

(define (expt b n)
  
  (define (expt-iter b counter product)
    (if (= counter 0)
        product
        (expt-iter b
                   (- counter 1)
                   (* b product))))
  
  (expt-iter b n 1))



;///////

;cheb:
(define (sum1 term a next b)
  (if (> a b) 
      0
      (+ (term a) (sum1 term (next a) next b))))



(define (sum-iter term a next b)
  
  (define (sum-iter2 term a next b totsum)
    (if (> a b) 
        totsum
        (sum-iter2 term (next a) next b (+ totsum (term a)))))
  
  (sum-iter2 term a next b 0))



;///////

(define (integral f a b dx)
  
  (define (sum term a next)
    (if (> a b) 
        0
        (+ (term a) (sum term (next a dx) next))))
  
  (* (sum f (+ a (/ dx 2)) add-dx) dx))


(define (integral2 f a b dx)
  
  (define (sum a)
    (if (> a b) 
        0
        (+ (f a) (sum (+ a dx)))))
  
  (* (sum (+ a (/ dx 2))) dx))


(define (integral-λ f a b dx)
  (* (sum1 f (+ a (/ dx 2.0)) (lambda (x) (+ x dx)) b)
     dx))



;///////

(define (integral-simpson f a b n)
  
  (define (sum-iter-custom term a next b totsum n h k)
    (if (> a b) 
        
        (/ (* totsum h) 3.0)
        
        (cond ((or (= k 0) (= k n)) (sum-iter-custom term (next a h) next b (+ totsum (term a)) n h (+ k 1) )) 
              ((= (remainder k 2) 0) (sum-iter-custom term (next a h) next b (+ totsum (* 2(term a))) n h (+ k 1) ))
              (else (sum-iter-custom term (next a h) next b (+ totsum (* 4(term a))) n h (+ k 1) )))))
  
  (sum-iter-custom f a add-h b 0 n (/ (- b a) n) 0))  



(define (add-h x h)
  (+ x h))



;///////

(define (add-dx x dx)
  (+ x dx))


