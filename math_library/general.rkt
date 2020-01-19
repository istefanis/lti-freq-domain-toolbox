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




(define (average a b) (/ (+ a b) 2))

(define (square x) (* x x)) 

(define (cube x) (* x x x))

(define (inc n) (+ n 1))

(define (inc-by-2 n) (+ n 2))

(define (double2 a) (* a 2))

(define (halve2 a) (* 0.5 a))

(define (identity x) x)

(define (add-dx x dx) (+ x dx))


(define (pi-approx-iter n)
  (* 8.0 (/ (product-iter square 4 inc-by-2 n) (* n (product-iter square 3 inc-by-2 n)))))

(define (factorial n)
  (product-iter identity 1 inc n))

(define (product-iter function a next b) 
  (define (product-iter2 a totprod)
    (if (> a b) totprod
        (product-iter2 (next a) (* totprod (function a)))
        )
    )
  (product-iter2 a 1))


; list of integers from 0 to x:
(define (integers x)
  (if (= x 0)
      (cons 0 '())
      (cons x
            (integers (- x 1)))))




#|
; PRIMES

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




; 1-D TABLES:

(define (lookup key table)
  (let ((record (m-assoc key (mcdr table))))
    (if record
        (mcdr record)
        #f)))

(define (m-assoc key records)
  (cond ((null? records) #f)
        ((equal? (mcar (mcar records)) key) (mcar records))
        (else (m-assoc key (mcdr records)))))

(define (insert! key value table)
  (let ((record (m-assoc key (mcdr table))))
    (if record
        (set-mcdr! record value)
        (set-mcdr! table (mcons (mcons key value) (mcdr table))))))

(define (make-table)
  (mcons '*table* '()))
|#




; EXPONENTATION

(define (expt b n)
  
  (define (expt-iter b counter product)
    (if (= counter 0)
        product
        (expt-iter b
                   (- counter 1)
                   (* b product))))
  
  (expt-iter b n 1))




; SUMMATION

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



