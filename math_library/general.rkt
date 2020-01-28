#lang racket

(provide (all-defined-out))







; //////////  General math functions  //////////

#|
Some of these functions have been implemented as demonstrated in:

Structure and Interpretation of Computer Programs second edition. 
The MIT Press with the McGraw-Hill Book Company, 1996
Harold Abelson and Gerald Jay Sussman with Julie Sussman, foreword by Alan J. Perlis. 

or are modifications of code presented there.
|#




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

(define (integers x) ; list of integers from 0 to x
  (if (= x 0)
      (cons 0 '())
      (cons x
            (integers (- x 1)))))





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


